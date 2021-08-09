/**
 * @module PvsioProcess
 * @author Paolo Masci
 * @date 2020.04.02
 * @copyright 
 * Copyright 2019 United States Government as represented by the Administrator 
 * of the National Aeronautics and Space Administration. All Rights Reserved.
 *
 * Disclaimers
 *
 * No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY
 * WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY,
 * INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE
 * WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM
 * INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR
 * FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO
 * THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER,
 * CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT
 * OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY
 * OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.
 * FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES
 * REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE,
 * AND DISTRIBUTES IT "AS IS."
 *
 * Waiver and Indemnity: RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS
 * AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND
 * SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF
 * THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES,
 * EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM
 * PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT
 * SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED
 * STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY
 * PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE
 * REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL
 * TERMINATION OF THIS AGREEMENT.
 **/

import { spawn, ChildProcess, execSync } from 'child_process';
// note: ./common is a symbolic link. if vscode does not find it, try to restart TS server: CTRL + SHIFT + P to show command palette, and then search for Typescript: Restart TS Server
import { PvsVersionDescriptor, SimpleConnection, PvsTheory, EvalExpressionRequest, PvsioEvaluatorCommand, PvsIoMode } from './common/serverInterface'
import * as path from 'path';
import * as fsUtils from './common/fsUtils';
import * as languageUtils from './common/languageUtils';
import { PvsResponse } from './common/pvs-gui';
import { forceLocale, isQuitCommand } from './common/languageUtils';
import { WorkspaceFolder } from 'vscode-languageserver';

export const pvsioResultRegExp: RegExp = /(\s*==>)?([\w\W\s]+)/g;

/**
 * Wrapper class for PVSio: spawns a PVSio process, and exposes the PVSio REPL interface as an asyncronous server.
 */
class PvsIoProcess {
	protected pvsioProcess: ChildProcess = null;
	protected pvsVersionInfo: PvsVersionDescriptor;

	protected pvsPath: string;
	protected pvsLibPath: string;
	protected pvsLibraryPath: string;
	protected nasalibPath: string;

	protected patchesLoaded: boolean = false;
	protected ready: boolean = false;
	protected data: string = "";
	protected cb: (data: string, readyPrompt?: boolean) => void;

	protected errorFlag: boolean = false;

	protected externalHandlers: {
		onExit: () => void,
		onError: (err?: Error) => void
	} = null;

	// pvs theory associated to this pvsio process
	protected desc: PvsTheory;

	/**
	 * Connection to the client
	 */
	protected connection: SimpleConnection;

	/**
	 * utility function for sending error messages over the connection (if any connection is available)
	 * @param msg message to be sent
	 */
	protected error(msg: string): void {
		if (msg) {
			if (this.connection) {
				this.connection.sendNotification('pvs-error', msg);
			}
			console.log('[pvsio-process] pvs-error', msg);
		}
	}

	/**
	 * Internal function, checks if the evaluation result indicates error
	 * @param data 
	 */
	protected checkError (data: string): void {
		this.errorFlag = data?.includes("Expecting an expression")
			|| data?.includes("does not uniquely resolve")
			|| data?.includes("Parser error")
			|| data?.includes("Error:") 
			|| data?.includes("<pvserror msg=")
			|| data?.includes("Type mismatch in application")
			|| data?.includes("can't be translated to PVS syntax")
			|| data?.includes("#<Function");
	}

	/**
	 * Returns the error flag, useful to check if the evaluation returned an error
	 * TODO: improve the APIs
	 */
	getErrorFlag (): boolean {
		return this.errorFlag;
	}

	/**
	 * @constructor
	 * @param desc Information on the PVS execution environment.
	 * @param connection Connection with the language client
	 */
	constructor (desc: { pvsPath: string }, opt?: { pvsLibraryPath?: string }, connection?: SimpleConnection) {
		this.pvsPath = (desc && desc.pvsPath) ? fsUtils.tildeExpansion(desc.pvsPath) : __dirname
		this.pvsLibPath = path.join(this.pvsPath, "lib");
		this.nasalibPath = path.join(this.pvsPath, "nasalib");
		opt = opt || {};
		this.pvsLibraryPath = opt.pvsLibraryPath || "";
		this.connection = connection;
	}

	getData (): string { return this.data; }
	resetData (): void { this.data = ""; }

	/**
	 * Internal function, loads pvs patches. 
	 * This command is not exposed in the APIs because it will produce an addional PVSio prompt 
	 * that can be handled correctly only within the activate method.
	 */
	protected async loadPvsPatches (): Promise<void> {
		await this.sendText(`(load-pvs-patches)!`);
		this.patchesLoaded = true;
	}


	//----------------------------------------------------------------------------------------------------
	//--------------------- The following functions are the main APIs provided by PvsIoProcess
	//----------------------------------------------------------------------------------------------------

	/**
	 * Sends an expression at the PVSio prompt
	 * @param data Expression to be evaluated at the PVSio prompt 
	 */
	async sendText (data: string, cb?: (data: string) => void): Promise<string> {
		return new Promise((resolve, reject) => {
			this.cb = (data: string) => {
				if (cb) { cb(data); } 
				resolve(data);
			};
			this.resetData();
			this.pvsioProcess?.stdin.write(data + "\n");
		});
	}
	/**
	 * Sends the quit command (followed by a confirmation) to PVSio
	 */
	quit (): void {
		this.pvsioProcess?.stdin.write("exit; Y");
	}

	/**
	 * Creates a new pvsio process.
	 * @param desc Descriptor indicating the pvs file and theory for this pvsio process
	 * @returns true if the process has been created; false if the process could not be created.
	 */
	async activate (desc: PvsTheory, opt?: {
		showBanner?: boolean,
		workspaceFolders?: WorkspaceFolder[],
		onExit?: () => void, 
		onError?: (err?: Error) => void
	}): Promise<boolean> {
		opt = opt || {};
		this.desc = desc;
		this.resetData();
		forceLocale();
		this.externalHandlers = {
			onError: opt?.onError,
			onExit: opt?.onExit
		};
		return new Promise (async (resolve, reject) => {
			if (this.pvsioProcess) {
				// process already running, nothing to do
				return resolve(true);
			}
			const pvsioExecutable = path.join(this.pvsPath, "pvsio");
			const fname: string = fsUtils.desc2fname(desc);
			const args: string[] = [ `${fname}@${desc.theoryName}` ];
			// pvsio args
			let libraries: string[] = [];
			let external: string[] = this.pvsLibraryPath?.split(":") || [];
			if (opt?.workspaceFolders?.length) {
				external = external.concat(opt.workspaceFolders.map(folder => {
					return fsUtils.normalizeContextFolder(folder.uri);
				}));
			}
			if (process.env["PVS_LIBRARY_PATH"]) {
				external = external.concat(process.env["PVS_LIBRARY_PATH"].split(":"));
			}
			for (let i = 0; i < external.length; i++) {
				let lib: string = external[i].trim();
				if (lib) {
					lib = lib.endsWith("/") ? lib : `${lib}/`;
					if (fsUtils.folderExists(lib)) {
						libraries.push(fsUtils.tildeExpansion(lib));
					}
				}
			}
			if (this.nasalibPath && fsUtils.folderExists(this.nasalibPath)) {
				let lib: string = this.nasalibPath.endsWith("/") ? this.nasalibPath : `${this.nasalibPath}/`;
				libraries.push(fsUtils.tildeExpansion(lib));
			}
			process.env["PVS_LIBRARY_PATH"] = (libraries?.length) ? libraries.join(":") : "";
			console.log(`\nPVS_LIBRARY_PATH=${process.env["PVS_LIBRARY_PATH"]}\n`);
			const fileExists: boolean = fsUtils.fileExists(pvsioExecutable);
			let bootData: string = "";
			if (fileExists && !this.pvsioProcess) {
				console.log(pvsioExecutable + " " + args.join(" "));
				this.pvsioProcess = spawn(pvsioExecutable, args);
				// console.dir(this.pvsProcess, { depth: null });
				this.pvsioProcess.stdout.setEncoding("utf8");
				this.pvsioProcess.stderr.setEncoding("utf8");
				this.pvsioProcess.stdout.on("data", (data: string) => {
					// data = data.trim();
					if (this.connection) {
						this.connection.console.log(data);
					} else {
						console.log(data);
					}

					this.data += data;
					this.checkError(data);

					const readyPrompt: boolean = data.trim().endsWith(languageUtils.pvsioPrompt);
					if (this.cb && typeof this.cb === "function") {
						// the prompt will be added by CLI
						if (readyPrompt) {
							this.data = this.data.replace(languageUtils.pvsioPrompt, "").trim();
							if (this.data.includes("+----") && !opt.showBanner) {
								this.cb("", readyPrompt);								
							} else {
								this.cb(this.data, readyPrompt);
							}
						}
					}
					// console.dir({ 
					// 	type: "memory usage",
					// 	data: process.memoryUsage()
					// }, { depth: null });
					// console.log(data);

					// wait for the pvs prompt, to make sure pvs-server is operational
					// const match: RegExpMatchArray = /\s*<PVSio>\s*/g.exec(data);
					if (readyPrompt) {
						if (!this.patchesLoaded) {
							this.loadPvsPatches();
						} else {
							if (!this.ready) {
								bootData = data;
								this.ready = true;
								resolve(true);
							}
						}
						// if (this.cb && typeof this.cb === "function") {
						// 	this.cb(this.data);
						// }
					}
				});
				this.pvsioProcess?.stdin.on("data", (data: string) => {
					console.log("stdin", data);
				});
				this.pvsioProcess.stderr.on("data", (data: string) => {
					console.log("[pvsio-process] Error: " + data);
					this.error(data);
					// resolve(false);
				});
				this.pvsioProcess.on("error", (err: Error) => {
					console.log("[pvsio-process] Process error", err);
					// console.dir(err, { depth: null });
					if (this.externalHandlers.onError && typeof this.externalHandlers.onError === "function") {
						this.externalHandlers.onError(err);
					}		
				});
				this.pvsioProcess.on("exit", async (code: number, signal: string) => {
					this.pvsioProcess = null;
					this.patchesLoaded = false;
					if (!this.ready) {
						this.error(bootData);
						console.log("[pvsio-process] Process exited with code ", code);
						resolve(false);
						return;
					}
					if (this.errorFlag) {
						this.cb(this.data, true);
						console.error("[pvsio-process] Evaluation error ", this.data);
						// re-activate pvsio process
						console.log(`[pvsio-process] Re-activating pvsio...`);
						this.resetData();
						this.ready = false;
						const success: boolean = await this.activate(this.desc, { showBanner: false });
						console.log(success);
						if (this.externalHandlers?.onExit && typeof this.externalHandlers.onExit === "function") {
							this.externalHandlers.onExit();
						}
						return;
					}
					// console.dir({ code, signal });
				});
				this.pvsioProcess.on("message", (message: any) => {
					console.log("[pvsio-process] Process message");
					// console.dir(message, { depth: null });
				});
			} else {
				console.log(`\n>>> PVSio executable not found at ${pvsioExecutable} <<<\n`);
				resolve(false);
			}
		});
	}
	/**
	 * Utility function. Returns a string representing the ID of the pvs process.
	 * @returns String representation of the pvs process ID.
	 */
	protected getProcessID (): string {
		if (this.pvsioProcess && !isNaN(this.pvsioProcess.pid)) {
			return this.pvsioProcess.pid.toString();
		}
		return null;
	}
	/**
	 * Kills the pvsio process.
	 * @returns True if the process was killed, false otherwise.
	 */
	async kill (): Promise<boolean> {
		return new Promise((resolve, reject) => {
			if (this.pvsioProcess) {
				const pvs_shell: string = this.getProcessID();
				// before killing the process, we need to close & drain the streams, otherwisae an ERR_STREAM_DESTROYED error will be triggered
				// because the destruction of the process is immediate but previous calls to write() may not have drained
				// see also nodejs doc for writable.destroy([error]) https://nodejs.org/api/stream.html
				this.pvsioProcess?.stdin.destroy();
				// try {
				// 	execSync(`kill -9 ${pid}`);
				// } finally {
				// 	setTimeout(() => {
				// 		resolve(true);
				// 	}, 1000);
				// }
				try {
					execSync(`kill -9 ${pvs_shell}`);
					this.pvsioProcess.on("close", (code: number, signal: string) => {
						console.log("[pvs-process] Process terminated");
						if (this.externalHandlers?.onExit && typeof this.externalHandlers.onExit === "function") {
							this.externalHandlers.onExit();
						}
						resolve(true);
						// console.dir({ code, signal }, { depth: null });
					});
				} catch (kill_error) {
					console.log(`[pvsProcess] Warning: Could not kill process id ${pvs_shell}.`);
					resolve(false);
				} finally {
					this.pvsioProcess = null;
				}
			} else {
				resolve(true);
			}
		});
	}
}

export class PvsIoProxy {
	/**
	 * pvs path
	 */
	protected pvsPath: string;
	/**
	 * path to the internal pvs libraries
	 */
	protected pvsLibPath: string;
	/**
	 * path to external pvs libraries
	 */
	protected pvsLibraryPath: string;
	/**
	 * connection to the client
	 */
	protected connection: SimpleConnection;
	/**
	 * pvsio process map, key is given by utils.desc2id(theory), see also startEvaluator
	 */
	protected processRegistry: { [key: string]: PvsIoProcess } = {};

	/**
	 * queue for incoming command requests
	 */
	protected queue: Promise<void> = Promise.resolve(null);

	/**
	 * attributes used when in "state-machine" mode
	 */
	protected initialState: string = ""; // initial machine state
	protected lastState: string = ""; // last machine state

	/**
	 * Constructor
	 */
	constructor (pvsPath: string, opt?: { connection?: SimpleConnection, pvsLibraryPath?: string }) {
		opt = opt || {};
		this.pvsPath = pvsPath;
		this.pvsLibraryPath = opt.pvsLibraryPath || "";
		this.pvsLibPath = path.join(pvsPath, "lib");
		this.connection = opt.connection;
	}

	/**
	 * Utility function, clears the process cache
	 */
	clearCache (): void {
		for (let i in this.processRegistry) {
			this.processRegistry[i].kill();
		}
		this.processRegistry = {};
	}

	/**
	 * start pvsio programmatically
	 */
	async startEvaluator (desc: PvsTheory, opt?: { 
		pvsLibraryPath?: string, 
		reuseProcess?: boolean,
		workspaceFolders?: WorkspaceFolder[]
	}): Promise<PvsResponse> {
		const processId: string = languageUtils.desc2id(desc);
		if (processId) {
			opt = opt || {};
			// try to re-use existing processes
			const pvsioProcess: PvsIoProcess = opt?.reuseProcess  && this.processRegistry[processId] ?
				this.processRegistry[processId] 
					: new PvsIoProcess({ pvsPath: this.pvsPath }, opt, this.connection);
			const success: boolean = await pvsioProcess.activate({
				fileName: desc.fileName, 
				fileExtension: desc.fileExtension,
				contextFolder: desc.contextFolder,
				theoryName: desc.theoryName
			}, {
				onExit: () => {
					console.error("[pvsio-proxy] PVSio process exited.");
					if (this.processRegistry[processId]) {
						delete this.processRegistry[processId];
					}
				},
				onError: (err?: Error) => {
					console.error("[pvsio-proxy] PVSio process exited with error", err);
					if (this.processRegistry[processId]) {
						delete this.processRegistry[processId];
					}
				},
				...opt
			});
			if (success) {
				this.processRegistry[processId] = pvsioProcess;
				const data: string = pvsioProcess.getData();
				return {
					jsonrpc: "2.0",
					id: processId,
					result: data
				};	
			}
		}
		return {
			jsonrpc: "2.0",
			id: processId,
			error: "Failed to start PVSio"
		};
	}
	/**
	 * Set the initial state
	 */
	setInitialState (state: string): void {
		this.initialState = state || this.initialState;
	}
	/**
	 * Terminates the pvsio session for the given theory
	 */
	async quitEvaluator (desc: PvsTheory): Promise<void> {
		const processId: string = languageUtils.desc2id(desc);
		const pvsio: PvsIoProcess = this.processRegistry ? this.processRegistry[processId] : null;
		await pvsio?.kill();
		this.lastState = this.initialState;
	}
	/**
	 * Evaluate a pvs command in a pvsio evaluator session (unbuffered version)
	 * Requires an evaluator session already started.
	 */
	protected async evalCmd (desc: PvsioEvaluatorCommand, opt?: {
		cb?: (data: string, state: string) => void
	}): Promise<PvsResponse> {
		opt = opt || {};
		const processId: string = languageUtils.desc2id(desc);
		const res: PvsResponse = {
			jsonrpc: "2.0",
			id: processId,
			result: null
		};
		const pvsio: PvsIoProcess = this.processRegistry ? this.processRegistry[processId] : null;
		if (pvsio && desc && desc.contextFolder && desc.fileName && desc.fileExtension) {
			if (desc.cmd) {
				const cmdSeq: string[] = desc.cmd?.split(";").filter(cmd => {
					return cmd.trim() !== "";
				}).map(cmd => {
					return cmd + ";";
				});
				let data: string = "";
				for (let i = 0; i < cmdSeq.length; i++) {
					let cmd: string = cmdSeq[i];
					// console.log(cmd);
					// check if this is a quit command
					if (isQuitCommand(cmd)) {
						await pvsio.kill();
						if (opt?.cb && i === cmdSeq.length - 1) {
							opt.cb("bye!", null);
						}
						return res;
					}
					// initialize state machine if field desc.initialState is provided
					if (desc.initialState) {
						this.initialState = desc.initialState;
						this.lastState = this.initialState;
					}
					// in state-machine mode, replay the last state as function parameter
					if (desc.mode === "state-machine" && this.lastState && this.initialState) {
						// remove semicolor at the end
						cmd = cmd.substring(0, cmd.length - 1 );
						// replay last state
						cmd = `LET st = ${this.lastState} IN ${cmd}(st);`;
					}
					// evaluate the command
					res.result = res.result || "";
					res.result += "\n" + await pvsio.sendText(cmd, (state: string) => {
						// clean up output, there's a ==> symbol at the start
						const match: RegExpMatchArray = new RegExp(pvsioResultRegExp).exec(state);
						if (match && match.length > 2) {
							state = match[2]?.trim(); // remove all unnencessary spaces
						}
						if (!pvsio.getErrorFlag()) {
							// set the initial state
							this.initialState = this.initialState || state;
							// update last state
							this.lastState = state;
						}
						data = data !== "" ? data + "\n" : data;
						data += desc.showCommandInTerminal ? `${cmd}\n==>\n${state}` 
							: `==>\n${state}`
						// execute callback, if any, on the last command
						if (opt?.cb && i === cmdSeq.length - 1) {
							opt.cb(data, state);
						}	
					});
				}
			}
			return res;
		}
		res.result = "Error: PVSio could not be started. Please check pvs-server log for details.";
		return res;
	}
	/**
	 * Evaluate a pvs command in a pvsio evaluator session. Requires an evaluator session already started.
	 */
	async evalCommand (desc: PvsioEvaluatorCommand, opt?: {
		cb?: (data: string, state: string) => void
	}): Promise<PvsResponse> {
		return new Promise ((resolve, reject) => {
			this.queue = this.queue.then(async () => {
				const res: PvsResponse = await this.evalCmd(desc, opt);
				resolve(res);
			});
		});
	}

	/**
	 * Evaluate a pvs expression
	 * @param req 
	 */
	async evalExpression (req: EvalExpressionRequest, opt?: {
		workspaceFolders?: WorkspaceFolder[],
		pvsLibraryPath?: string
	}): Promise<PvsResponse> {
		opt = opt || {};
		let response: PvsResponse = await this.startEvaluator(req, { ...opt, reuseProcess: true });
		if (response && !response.error) {
			response = await this.evalCommand({
				contextFolder: req.contextFolder,
				fileName: req.fileName,
				fileExtension: req.fileExtension,
				theoryName: req.theoryName,
				cmd: req.expr + ";"
			});
			if (response?.result?.startsWith("\n==>\n")) {
				response.result = response.result.replace("\n==>\n", "");
			}
		}
		return response;
	}
}