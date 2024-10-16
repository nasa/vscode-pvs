/**
 * @module PvsProcess
 * @author Paolo Masci
 * @date 2019.06.18
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

import { spawn, ChildProcess } from 'child_process';
// note: ./common is a symbolic link. if vscode does not find it, try to restart TS server: CTRL + SHIFT + P to show command palette, and then search for Typescript: Restart TS Server
import { PvsVersionDescriptor, SimpleConnection } from './common/serverInterface'
import * as path from 'path';
import * as fsUtils from './common/fsUtils';
import { PvsErrorManager } from './pvsErrorManager';
import { forceLocale } from './common/languageUtils';

import { PvsPackageManager } from './providers/pvsPackageManager';

import consoleStamp from 'console-stamp';
consoleStamp(console, { format: ':date(yyyy/mm/dd HH:MM:ss.l)' });

const MAX_LOG_CHUNK: number = 600;


export enum ProcessCode {
	SUCCESS = 0, PVS_NOT_FOUND = -1, ADDR_IN_USE = -2, COMM_FAILURE = -3, PVS_START_FAIL = -4, PVS_ERROR = -5, UNSUPPORTED_PLATFORM = -6,
	TERMINATED = -7
};
/**
 * Wrapper class for PVS: spawns a PVS process, and exposes the PVS Lisp interface as an asynchronous JSON/web-socket server.
 */
export class PvsProcess {
	protected pvsProcess: ChildProcess = null;
	protected pvsVersionInfo: PvsVersionDescriptor;

	protected pvsPath: string = null;
	protected pvsLibraryPath: string = null;

	/**
	 * Current status of the PVS process; `undefined` indicates that the current status cannot be determined (for example, because the process is starting but not yet ready).
	 */
	protected currentStatus: ProcessCode | undefined = undefined; 
	protected data: string = "";
	protected cb: (data: string) => void;
	protected buffer: Promise<string> = Promise.resolve("");

	protected connection: SimpleConnection;
	protected enableNotifications: boolean;

	protected pvsErrorManager: PvsErrorManager;

	protected serverPort: number = 23456;
	protected verbose: boolean = false;
	
	protected progressInfoEnabled: boolean = false;

	protected verboseLog: boolean = false;
	protected reportedServerPort: number | undefined;
	
	public getReportedServerPort(): number | undefined {
		return this.reportedServerPort;
	}
	/**
	 * utility function for sending error messages over the connection (if any connection is available)
	 * @param msg message to be sent
	 */
	protected error(msg: string): void {
		if (msg) {
			if (this.connection && this.enableNotifications) {
				this.connection.sendNotification('pvs-error', msg);
			} else {
				console.log(msg);
			}
		}
	}
	protected warn(msg: string): void {
		if (msg) {
			if (this.connection && this.connection.console) {
				this.connection.console.warn(msg);
			} else {
				console.warn(msg);
			}
		}
	}
	/**
	 * Internal function, logs debug messages in vscode-pvs output or in the console if a connection to the front-end is not available
	 */
	protected log (msg: string, opt?: { force?: boolean}): void {
		opt = opt || {};
		if (msg && (this.verbose || opt.force)) {
			if (!msg.startsWith("127.0.0.1") && !msg.startsWith("emacs does not support X;")) {
				if (this.connection && this.connection.console) {
					// msg = msg.replace(/(\r\n|\n|\r)/gm, "");
					this.connection.console.log(msg);
					if (this.progressInfoEnabled) {
						const ln: string[] = msg.trim().split("\n").filter(line => {
							return line.trim() && !line.includes("PVS(") && !(line.trim() === "NIL");
						});
						this.sendProgressInfo(ln[ln.length - 1]);
					}
				} else {
					console.log(msg);
				}
			}
		}
	}
	protected sendProgressInfo (msg: string): void {
		if (msg) {
			this.connection?.sendNotification("pvs.progress-info", msg);
		}
	}

	/**
	 * @constructor
	 * @param desc Information on the PVS execution environment.
	 * @param connection Connection with the language client
	 */
	constructor (pvsPath: string, opt?: { connection?: SimpleConnection, pvsErrorManager?: PvsErrorManager, pvsLibraryPath?: string }) {
		opt = opt || {};
		this.pvsPath = pvsPath ? fsUtils.tildeExpansion(pvsPath) : __dirname
		this.pvsLibraryPath = opt.pvsLibraryPath || '';
		this.connection = opt.connection;
		this.pvsErrorManager = opt.pvsErrorManager;
	}

	/**
	 * Utility function, sets the log level
	 */
	logLevel (l: "verbose" | "standard"): void {
		this.verboseLog = l === "verbose";
	}

	/**
	 * Internal function. Runs the relocate script necessary for starting pvs.
	 */
	// protected async relocate(): Promise<boolean> {
	// 	let relocate: string = null;
	// 	if (await fsUtils.fileExists(path.join(`${this.pvsPath}`, "install-sh"))) {
	// 		relocate = `cd ${this.pvsPath} && ./install-sh` // pvs 7 has this new script
	// 	} else if (await fsUtils.fileExists(path.join(`${this.pvsPath}`, "bin/relocate"))) {
	// 		relocate = `cd ${this.pvsPath} && bin/relocate`; // this is for backwards compatibility
	// 	}
	// 	if (relocate) {
	// 		try {
	// 			const output: Buffer = execSync(relocate);
	// 			// console.log(output.toString());
	// 		} catch (relocateError) {
	// 			console.log(relocateError);
	// 			return false;
	// 		}
	// 		return true;
	// 	}
	// 	return false;
	// }

	//----------------------------------------------------------------------------------------------------
	//--------------------- The following functions are the main APIs provided by PvsProcess
	//----------------------------------------------------------------------------------------------------

	/**
	 * Creates a new pvs process.
	 * @param opt Options: enableNotifications, transmits the output of the pvs process over the client connection (if any is available)
	 * @returns true if the process has been created; false if the process could not be created.
	 */
	async activate (opt?: { 
		enableNotifications?: boolean, 
		webSocketPort?: number,
		serverPort?: number,
		verbose?: boolean
	}): Promise<ProcessCode> {
		console.info("[pvsProcess.activate] start... ");
		if (this.pvsProcess) {
			// process already running, nothing to do
			console.info("[pvsProcess.activate] process already running, nothing to do ");
			return ProcessCode.SUCCESS;
		}
		if (!this.pvsPath) {
			return ProcessCode.PVS_NOT_FOUND;
		}
		console.info("[pvsProcess.activate] pvs is not already running, path seems right ");
		opt = opt || {};
		this.enableNotifications = !!opt.enableNotifications;
		this.verbose = !!opt.verbose;
		this.serverPort = opt.serverPort || this.serverPort;
		// force locale settings
		forceLocale();
		console.log(`[PvsProcess.activate] ACL_LOCALE=${process.env["ACL_LOCALE"]}, LC_ALL=${process.env["LC_ALL"]}, LANG=${process.env["LANG"]}`);
		// pvs args
		const pvs: string = path.join(this.pvsPath, "pvs");
		
		// Manually loads PVS patches for VSCode-PVS
		var loadAdditionalPatchesCode: string = PvsPackageManager.generateLoadOwnPatchesCode();
		if (loadAdditionalPatchesCode.length > 0)
			loadAdditionalPatchesCode = `-E '${loadAdditionalPatchesCode}'`;

		const args: string[] = [ "-raw" , "-port", `${this.serverPort}`, loadAdditionalPatchesCode];

		console.info(`[PvsProcess.activate] shell command  ${pvs} ${args.join(" ")}`);
		console.info(`[PvsProcess.activate] pvsLibraryPath ${this.pvsLibraryPath}`);
		const fileExists: boolean = fsUtils.fileExists(pvs);
		if (fileExists) {
			// Start the PVS process
			if (!this.pvsProcess) {
				this.currentStatus = undefined;
				console.info('[PvsProcess.activate] about to spawn PVS');
				this.pvsProcess = spawn(pvs, args, { detached: true, env: { ...process.env, PVS_LIBRARY_PATH: this.pvsLibraryPath} });
				console.info(`[PvsProcess.activate] PVS spawned (PID: ${this.pvsProcess.pid})`);
				// console.dir(this.pvsProcess, { depth: null });
				this.pvsProcess.stdout.setEncoding("utf8");
				this.pvsProcess.stderr.setEncoding("utf8");
				
				// data logged for the command sent to PVS, this is used to limit the amount of data shown in the vscode-pvs output panel
				let logData: string = "";
				let maxLogLimitReached: boolean = false;
				const resetLocalLog = () => {
					logData = "";
					maxLogLimitReached = false;
				};

				function logOutputToConsole(output: string, tag?: string) {
					for(let line of output.split('\n')){
						console.log(`${tag}${line}`);
					}
				}

				this.pvsProcess.stdout.on("data", async (data: string) => {
					logOutputToConsole(data, "[pvsProcess.stdout] ");

					const dataNoLineBreaks = data.replace('\n',' ');
					this.data += dataNoLineBreaks;
					logData += data;

					// if PVS reports entering to the debugger, an unrecoverable error has occurred.
					// We try to restart the server.
					const matchLispDebugger: RegExpMatchArray = /Welcome to LDB, a low-level debugger for the Lisp runtime environment/gi.exec(dataNoLineBreaks);
					if (matchLispDebugger || 
						  /debugger invoked on/gi.exec(dataNoLineBreaks)
					) {
						this.pvsErrorManager.notifyPvsFailure({msg: "PVS entered the debugger. Please use M-x reboot-pvs to restart the process.", src: "pvs"});
					}

					if(this.currentStatus !== ProcessCode.SUCCESS){
						const matchNoExecutable: RegExpMatchArray = /No executable available in (.+)/gi.exec(dataNoLineBreaks);
						if (matchNoExecutable) {
							this.pvsProcess = null;
							resetLocalLog();
							this.currentStatus = ProcessCode.UNSUPPORTED_PLATFORM;
						}
						const matchUsedPort: RegExpMatchArray = /\bListening +on +127\.0\.0\.1:(\d+)/g.exec(this.data);
						if(matchUsedPort){
							this.reportedServerPort = +matchUsedPort[1];
							// @M3 Now it's not necessary to wait for the pvs-REPL prompt to 
							//     decide that the process is ready; it suffices with checking
							//     that the 'Listening on' message is printed on stdout.
							this.currentStatus = ProcessCode.SUCCESS;
						}
					}

					const matchPvsPrompt: RegExpMatchArray = /(?:\[\d+\w*\])?\s+pvs\(\d+\)\s*:/ig.exec(dataNoLineBreaks);
					const matchProverPrompt: RegExpMatchArray = /\bRule\?/g.exec(dataNoLineBreaks); // @M3 we should stop checking for this prompt #TODO 
					if (matchPvsPrompt || matchProverPrompt) {
						// @M3 If the pvs-REPL or prover prompt is found, it is assumed that the process is active and ready to answer.
						this.currentStatus = ProcessCode.SUCCESS;
						if (!this.currentStatus) {
							if (maxLogLimitReached) {
								// log pvs prompt to provide better feedback
								const logPrompt: string = matchPvsPrompt?.length ? matchPvsPrompt[0]
									: matchProverPrompt?.length ? matchProverPrompt[0]
									: "";
								this.log(logPrompt);
							}
							resetLocalLog();
						}
						if (this.cb && typeof this.cb === "function") { // #TODO old. check @M3
							let res: string = this.data.replace(/(?:\[\d+\w*\])?\s+PVS\(\d+\)\s*:/g, "").replace(/\bRule\?/g, "");
							// clean up pvs output by removing unnecessary text
							res = res.replace("[Current process: Initial Lisp Listener]", "");
							this.cb(res.trim());
						}
					} 
				});
				this.pvsProcess.stderr.on("data", (data: string) => {
					const dataNoLineBreaks = data.replace('\n',' ');
					if (/debugger invoked on/gi.exec(dataNoLineBreaks)
					) {
						this.pvsErrorManager.notifyPvsFailure({msg: "PVS entered the debugger. Please use M-x reboot-pvs to restart the process.", src: "pvs"});
					}
					logOutputToConsole(data, "[pvsProcess.stderr] ");
				});
				this.pvsProcess.on("error", (err: Error) => {
					this.error(`[pvsProcess] Process error \n>>> ${err} <<< `);
				});
				this.pvsProcess.on("exit", (code: number, signal: string) => {
					resetLocalLog();
					console.log(`[pvsProcess] Process exited, code: ${code}, signal: ${signal}, this.ready: ${this.currentStatus}`);
					// if PVS fails before communicating its 'ready' state, it's a starting-up fail.
					if(!this.currentStatus)
						this.currentStatus = ProcessCode.PVS_START_FAIL;
					else
						this.currentStatus = undefined;
				});
				this.pvsProcess.on("message", (message: any) => {
					resetLocalLog();
					this.log(`[pvsProcess] Process message \n>>> ${message} <<< `);
				});
			}
			// Wait for the PVS process to notify it started listening at the given port			
			return await new Promise((resolveWait,rejectWait) => {
				const maxNumberOfAttempts = 100; 
				const intervalTime = 200; //ms
	
				let currentAttempt = 0
				console.log("[pvsProcess.activate] Waiting for PVS confirmation ")
				const interval = setInterval(() => {
					console.log(`[pvsProcess.activate] -> polling on currentStatus (check ${currentAttempt+1} of ${maxNumberOfAttempts})...`);
					if (this.currentStatus === ProcessCode.SUCCESS) {
						clearInterval(interval);
						console.log("[pvsProcess.activate] DONE: PVS is active and waiting for requests");
						resolveWait(ProcessCode.SUCCESS); 
					} else if (currentAttempt > maxNumberOfAttempts - 1) {
						clearInterval(interval)
						console.log("[pvsProcess.activate] FAIL: reached max number of attempts");
						resolveWait(ProcessCode.PVS_START_FAIL);
					} else if (this.currentStatus){
						clearInterval(interval)
						console.log(`[pvsProcess.activate] FAIL: status code ${this.currentStatus}`);
						resolveWait(this.currentStatus);
					}
					currentAttempt++
				}, intervalTime);
			});

		} else {
			this.error(`\n>>> PVS executable not found at ${pvs} <<<\n`);
			return ProcessCode.PVS_NOT_FOUND;
		}
	}
	/**
	 * Kills the pvs process.
	 * @returns The ID of the process that was killed. Null if no process was killed.
	 */
	async kill (): Promise<boolean> {
		this.currentStatus = undefined;
		return new Promise(async (resolve, reject) => {
			if (this.pvsProcess) {
				// Only try to kill the process if it's not already dead
				if (this.pvsProcess.exitCode === null) {
					let done: boolean = false;
					const pid: number = this.getProcessID();
					// before killing the process, we need to close & drain the streams, otherwise an ERR_STREAM_DESTROYED error will be triggered
					// because the destruction of the process is immediate but previous calls to write() may not have drained
					// see also nodejs doc for writable.destroy([error]) https://nodejs.org/api/stream.html
					this.pvsProcess.on("close", (code: number, signal: string) => {
						console.log("[pvsProcess] Process terminated");
						this.currentStatus = ProcessCode.TERMINATED;
						this.pvsProcess = null;
						done = true;
						resolve(true);
					});
					this.pvsProcess.on("error", (code: number, signal: string) => {
						console.log("[pvsProcess] Process terminated");
						this.currentStatus = ProcessCode.TERMINATED;
						resolve(true);
					});
					try {
						if (this.pvsProcess) {
							// try to exit the process gracefully
							await new Promise((resolveExit, rejectExit) => {
								if (!this.pvsProcess?.stdin?.destroyed) {
									this.pvsProcess?.stdin?.write("(lisp (bye))\n");
								}
								if(!this.pvsProcess.killed)
									this.pvsProcess.kill('SIGKILL');
								// process?.kill(pid, "SIGKILL");
								// give pvs some time to quit before resolving the promise
								setTimeout(() => {
									if (!done) {
										resolveExit(true);
									}
								}, 400);
							});
						} else {
							// execSync(`kill -9 ${pid}`);
							process?.kill(pid, "SIGKILL");
						} 
					} finally {
						this.pvsProcess = null;
						if (!done) {
							resolve(true);
						}
					}
				} else {
					this.pvsProcess = null;
					resolve(true);
				}
			} else {
				resolve(true);
			}
		});
	}
	/**
	 * Utility function. Returns the ID of the pvs process.
	 * @returns pvs process ID.
	 */
	protected getProcessID (): number {
		if (this.pvsProcess && !isNaN(this.pvsProcess.pid)) {
			return this.pvsProcess.pid;
		}
		return null;
	}

	public async clearContext (contextFolder?: string): Promise<void> {
		// const currentContext: string = contextFolder;// || this.contextFolder;
		// if (currentContext) {
		// 	// console.info(`** clearing pvs cache for context ${currentContext} **`)
		// 	await fsUtils.deletePvsCache(currentContext);
		// }
	}

	//----------------------------------------------------------------------------------------------------
	//--------------------- The following functions are used for the Emacs REPL
	//----------------------------------------------------------------------------------------------------

	/**
	 * Sends a command the Emacs REPL of PVS
	 * The buffer is used to queue the messages and make sure one message at a time is sent to the process
	 * @param cmd Command to be sent 
	 */
	async sendText (cmd: string): Promise<string> {
		this.buffer = this.buffer.then(() => {
			return new Promise((resolve, reject) => {
				this.cb = (data: string) => {
					resolve(data);
				};
				this.data = "";
				if (this.pvsProcess && this.pvsProcess.stdin && !this.pvsProcess.stdin.destroyed) {
					this.progressInfoEnabled = cmd.includes("typecheck-file");
					this.pvsProcess?.stdin?.write(cmd);
					this.log(cmd + "\n");
				}
			});
		});
		return this.buffer;
	}
	/**
	 * Sends the quit command (followed by a confirmation) to the Emacs REPL of PVS
	 */
	quit (): void {
		this.pvsProcess?.stdin?.write("exit; Y");
	}
	/**
	 * returns the current output of the process
	 */
	getLispInterfaceOutput (): string {
		return this.data || "";
	}

	/**
	 * Clears the current lisp output
	 */
	clearLispInterfaceOutput (): void {
		this.data = "";
	}
}