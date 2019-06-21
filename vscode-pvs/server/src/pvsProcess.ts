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

import { spawn, ChildProcess, execSync } from 'child_process';
// note: ./common is a symbolic link. if vscode does not find it, try to restart TS server: CTRL + SHIFT + P to show command palette, and then search for Typescript: Restart TS Server
import { 
	PvsResponseType, PvsParserResponse, PvsDeclarationDescriptor,
	PRELUDE_FILE, PvsDeclarationType, PrettyPrintRegionRequest,
	PrettyPrintRegionResult, ExpressionDescriptor, EvaluationResult, PvsListDeclarationsRequest,
	PvsTypecheckerResponse, PvsChangeContextResponseType, PvsCurrentContextResponseType,
	SimpleConnection, TheoryList, FileList, TheoryMap, TheoryStatus, PvsVersionDescriptor, PvsVersionInfoResponseType, 
	JsonType, ProofNodeType, ProofDescriptor, ProofObjectType, PvsListProofStrategiesResponseType, FindDeclarationResponseType, PvsFindDeclarationResponseType
} from './common/serverInterface'
import { Connection } from 'vscode-languageserver';
import * as path from 'path';
import { PVS_TRUE_FALSE_REGEXP_SOURCE, PVS_STRING_REGEXP_SOURCE } from "./common/languageKeywords";
import * as fsUtils from './common/fsUtils';
import { PvsLispReader, TccDescriptor } from './pvsLisp';
import * as utils from './common/languageUtils';
// import * as xmlrpcProvider from './common/xmlrpcProvider';


export interface ContextDiagnostics {
	[fileName: string]: PvsParserResponse
};

export class PvsProgressInfo {
	private progressLevel: number = 0;
	showProgress (cmd: string, data?: string): string {
		this.progressLevel++;
		if (this.progressLevel > 4) {
			this.progressLevel = 0;
		}
		return `Executing ${cmd}${".".repeat(this.progressLevel)}`;
	}
}

/**
 * Wrapper class for PVS: spawns a PVS process, and exposes the PVS Lisp interface as an asyncronous JSON/RPC server.
 */
export class PvsProcess {
	private pvsProcess: ChildProcess = null;
	private pvsProcessBusy: boolean = false;

	private processType: string; // "typechecker" or "parser"

	private pvsVersionInfo: PvsVersionDescriptor;

	private progressInfo: PvsProgressInfo;
	// private serverProxy: xmlrpcProvider.XmlRpcProxy;

	private pvsCmdQueue: Promise<PvsResponseType> = Promise.resolve({ res: null, error: null, raw: null });

	private pvsPath: string = null;
	private pvsLibraryPath: string = null;
	private pvsContextFolder: string = null;

	private connection: SimpleConnection;
	private enableNotifications: boolean;

	private _disableGC: boolean = false;

	/**
	 * utility function for sending notifications over the connection (if any connection is available)
	 * @param msg message to be sent
	 */
	private info(msg: string) {
		if (this.enableNotifications && this.connection && msg && msg.length > 10 && !msg.startsWith(";;;")) {
			this.connection.sendNotification('server.status.update', msg.trim());
		}
	}
	/**
	 * utility function for sending error messages over the connection (if any connection is available)
	 * @param msg message to be sent
	 */
	private error(msg: string) {
		if (this.enableNotifications) {
			this.connection.sendNotification('pvs-error', msg);
		}
	}
	/**
	 * utility function for notifying the client that the pvs process ready
	 */
	private ready() {
		if (this.enableNotifications) {
			this.connection.sendNotification('pvs-ready');
		}
	}
	/**
	 * Path to the current context folder (i.e., the working directory)
	 * @returns The current pvs context path
	 */
	getContextFolder (): string {
		return this.pvsContextFolder;
	}
	/**
	 * Path to the pvs library folder
	 * @returns Path of the prelude library
	 */
	getPvsLibraryPath (): string {
		return path.join(this.pvsPath, "lib");
	}
	/**
	 * Path to the pvs executable
	 */
	getPvsPath (): string {
		return this.pvsPath;
	}
	/**
	 * @constructor
	 * @param desc Information on the PVS execution environment.
	 * @param connection Connection with the language client
	 */
	constructor (desc: { pvsPath: string, pvsContextFolder?: string, processType?: string }, connection?: Connection) {
		this.pvsPath = (desc && desc.pvsPath) ? fsUtils.tildeExpansion(desc.pvsPath) : __dirname
		this.pvsLibraryPath = path.join(this.pvsPath, "lib");
		this.pvsContextFolder = (desc && desc.pvsContextFolder) ? fsUtils.tildeExpansion(desc.pvsContextFolder) : __dirname;
		this.processType = (desc && desc.processType) ? desc.processType : "typechecker"; // this is used only for debugging
		this.progressInfo = new PvsProgressInfo();
		// this.serverProxy = new xmlrpcProvider.XmlRpcProxy();
		this.connection = connection;
	}
	/**
	 * Internal function, used to communicate that the process is busy and cmd cannot be executed.
	 * @param cmd 
	 */
	private cannotExecute (msg: string): Promise<PvsResponseType> {
		// if (this.connection) { this.connection.console.error(msg); }
		return Promise.resolve({
			error: {
				msg: msg,
				parserError: null,
				restartOption: null
			},
			res: null,
			raw: null
		});
	}
	/**
	 * Internal function. Executes pvs Lisp commands.
	 * @param cmd pvs Lisp command
	 * @param desc information on the pvs file and pvs theory to be used for the execution of the pvs command.
	 */
	private pvsExecAux (cmd: string, desc?: { fileName: string, fileExtension: string, theoryName: string }): Promise<PvsResponseType> {
		if (this.pvsProcessBusy) {
			const msg: string = "PVS busy, cannot execute " + cmd + " :/";
			return this.cannotExecute(msg);
		}
		const pvsLispReader: PvsLispReader = new PvsLispReader(this.connection);
		const match: RegExpMatchArray = /\(\b([\w\-]+)\b.*\)/.exec(cmd);
		if (match && match[1]) {
			const commandId: string = match[1];
			if (this.connection) { this.connection.console.log(cmd); }
			return new Promise((resolve, reject) => {
				const listener = (data: string) => {
					if (this.connection) {
						this.connection.console.log(data); // this is the crude pvs lisp output, useful for debugging
						this.info(this.progressInfo.showProgress(commandId, data));
					}
					pvsLispReader.read(data, async (pvsOut: string) => {
						if (this.pvsProcess) {
							this.pvsProcess.stdout.removeListener("data", listener); // remove listener otherwise this will capture the output of other commands
						}
						this.pvsProcessBusy = false;
						const ans: PvsResponseType = pvsLispReader.parse(commandId, pvsOut, desc);
						this.ready();
						resolve(ans);
					});
				};
				if (this.pvsProcess) {
					try {
						if (this.pvsProcess.stdout) { this.pvsProcess.stdout.on("data", listener); }
						if (this.pvsProcess.stdin) { this.pvsProcess.stdin.write(cmd + "\n"); }
					} catch (pvsProcessWriteError) {
						console.warn(pvsProcessWriteError);
					}
				}
			});
		}
		if (this.connection) { this.connection.console.error(`Unrecognised command ${cmd}`); }
		Promise.reject({
			res: null,
			error: `Unrecognised command ${cmd}`,
			raw: null
		});
	}
	/**
	 * Internal function. Executes pvs Lisp commands. Uses a buffer to queue messages if pvs process is busy.
	 * This function is used by the APIs provided by PvsProcess. Requires correct syntax for commands, cannot be used for CLI Interfaces
	 * as commands typed with wrong syntax may miss parentheses and therefore pvs Lisp would not return control.
	 * @param cmd pvs Lisp command to be executed
	 * @param desc information on the pvs file and pvs theory to be used for the execution of the pvs command.
	 */
	private pvsExec (cmd: string, desc?: { fileName: string, fileExtension: string, theoryName: string }): Promise<PvsResponseType> {
		this.pvsCmdQueue = new Promise((resolve, reject) => {
			this.pvsCmdQueue.then(() => {
				this.pvsExecAux(cmd, desc).then((ans: PvsResponseType) => {
					resolve(ans);
				}).catch(execError => {
                    console.error(execError);
                });
			});
		});
		return this.pvsCmdQueue;
	}
	/**
	 * Utility function. Can be used to pipe the output of the pvs process to a listener function. Currently used by PVS CLI. 
	 * @param listener the listener to be added
	 */
	startCli (listener: (data: string) => void): void {
		this.pvsProcess.stdout.on("data", listener);
	}
	/**
	 * Utility function. Removes a given listener from the output channel of the pvs process.
	 * @param listener the listener to be removed
	 */
	endCli (listener: (data: string) => void): void {
		this.pvsProcess.stdout.removeListener("data", listener);
	}
	/**
	 * Executes a pvs Lisp command. Unbuffered version. Should only be used in CLI interfaces.
	 * @param cmd 
	 */
	execCmd (cmd: string): void {
		this.pvsProcess.stdin.write(cmd);
	}
	/**
	 * Internal function. Returns the current context.
	 * @returns Path to the current context
	 */
	private async currentContext(): Promise<PvsResponseType> {
		const cmd: string = '(pvs-current-directory)';
		return await this.pvsExec(cmd);
	}
	/**
	 * Internal function. Disables garbage collector messages.
	 */
	private async disableGcPrintout(): Promise<PvsResponseType> {
		this._disableGC = true;
		return await this.pvsExec('(setq *disable-gc-printout* t)');
	}
	/**
	 * Internal function. Runs the relocate script necessary for starting pvs.
	 */
	private async relocate(): Promise<boolean> {
		const relocateScript: string = `cd ${this.pvsPath} && bin/relocate`;
		try {
			const output: Buffer = execSync(relocateScript);
			// console.log(output.toString());
		} catch (relocateError) {
			console.error(relocateError);
			return false;
		}
		return true;
	}
	/**
	 * Internal function. Creates a new pvs process.
	 * @param opt Options: enableNotifications, transmits the output of the pvs process over the client connection (if any is available)
	 * @returns true if the process has been created; false if the process could not be created.
	 */
	private async _pvs(opt?: { enableNotifications?: boolean }): Promise<boolean> {
		opt = opt || {};
		await this.relocate();
		this.enableNotifications = opt.enableNotifications;
		// await this.clearContext();
		if (!this.pvsProcessBusy) {
			this.pvsProcessBusy = true;
			const pvsLispReader = new PvsLispReader(this.connection);
			const pvs: string = path.join(this.pvsPath, "pvs");
			const args: string[] = [ "-raw"];//, "-port", "22334" ];
			// if (this.connection) { this.connection.console.info(`Spawning pvs process ${pvs} ${args.join(" ")}`); }
			return new Promise(async (resolve, reject) => {
				const fileExists: boolean = await fsUtils.fileExists(pvs);
				if (fileExists) {
					this.pvsProcess = spawn(pvs, args);
					this.pvsProcess.stdout.setEncoding("utf8");
					this.pvsProcess.stderr.setEncoding("utf8");
					let listener = (data: string) => {
						if (this.connection) { this.connection.console.log(data); } // this is the crude pvs lisp output, useful for debugging
						// pvslispParser.parse(data, (res: string) => {
						pvsLispReader.read(data, (res: string) => {
							// connection.console.info(res);
							this.pvsProcess.stdout.removeListener("data", listener); // remove listener otherwise this will capture the output of other commands
							this.pvsProcessBusy = false;
							resolve(true);
						});
					};
					this.pvsProcess.stdout.on("data", listener);
					this.pvsProcess.stderr.on("data", (data: string) => {
						if (this.connection) { this.connection.console.log(data); }
					});
					// if (this.connection) { this.connection.console.info("PVS process ready!"); }
					if (this.connection) {
						this.connection.sendNotification(`server.new-pvs-${this.processType}`); // used for debugging
					}
				} else {
					console.error(`\n>>> PVS executable not found at ${pvs} <<<\n`);
					this.pvsProcessBusy = false;
					resolve(false)
				}
			});
		}
	}

	//----------------------------------------------------------------------------------------------------
	//--------------------- The following functions are the main APIs provided by PvsProcess
	//----------------------------------------------------------------------------------------------------

	/**
	 * Creates a new pvs process.
	 * @param opt Options: enableNotifications, transmits the output of the pvs process over the client connection (if any is available)
	 * @returns true if the process has been created; false if the process could not be created.
	 */
	async pvs(opt?: { enableNotifications?: boolean}): Promise<boolean> {
		const res: boolean = await this._pvs(opt);
		await this.disableGcPrintout();
		await this.changeContext(this.pvsContextFolder);
		const ans: PvsCurrentContextResponseType = await this.currentContext();
		// refresh this.currentContext, to make sure the string is identical to that used by pvs -- useful for deciding whether a future change context is actually changing the context
		if (ans && ans.res) {
			this.pvsContextFolder = ans.res;
		} else {
			console.error(`Unexpected value for context folder`, ans);
		}
		return res;
	}
	/**
	 * Kills the pvs process.
	 * @returns The ID of the process that was killed. Null if no process was killed.
	 */
	kill (): string {
		if (this.pvsProcess) {
			const id: string = this.getProcessID();
            // before killing the process, we need to close & drain the streams, otherwisae an ERR_STREAM_DESTROYED error will be triggered
            // because the destruction of the process is immediate but previous calls to write() may not have drained
            // see also nodejs doc for writable.destroy([error]) https://nodejs.org/api/stream.html
            this.pvsProcess.stdin.end(() => {});
            this.pvsProcess.kill();
            if (this.connection) {
                this.connection.sendNotification(`server.delete-pvs-${this.processType}`); // used for debugging & stats
			}
			return id;
		}
		return null;
	}
	/**
	 * Restarts the pvs process.
	 */
	async restart (): Promise<void> {
		this.kill();
		await this.pvs();
		if (this._disableGC) {
			await this.disableGcPrintout();
		}
	}
	/**
	 * Utility function. Returns a string representing the ID of the pvs process.
	 * @returns String representation of the pvs process ID.
	 */
	getProcessID (): string {
		if (this.pvsProcess && this.pvsProcess.pid) {
			return this.pvsProcess.pid.toString();
		}
		return null;
	}
	/**
	 * Changes the current context. When the context is changed, all symbol information are erased and the parser/typechecker needs to be re-run.
	 * @param contextFolder Path of the context folder 
	 */
	async changeContext (contextFolder: string): Promise<PvsChangeContextResponseType> {
		// await this.serverProxy.changeContext(contextFolder);
		const folderExists: boolean = await fsUtils.dirExists(contextFolder);
		if (folderExists) {
			const cmd: string = `(change-context "${contextFolder}" nil)`;
			const ans: PvsChangeContextResponseType = await this.pvsExec(cmd);
			if (ans && ans.res && ans.res.context) {
				this.pvsContextFolder = ans.res.context;
			}
		}
		return {
			res: null,
			error: { msg: `Error: Could not change context to ${contextFolder} (folder does not exist or cannot be read).` },
			raw: null
		};
	}
	/**
	 * Enables the pvs emacs interface.
	 * This is used to overcome a limitation of the raw mode, which does not provide detailed info for errors.
	 * ATT: Use this command carefully. The parser works fine, but the prover cannot be started as it expects commands from emacs. 
	 *
	 * pvs output in raw mode:
	 * pvs(7): (typecheck-file "test" nil nil nil)
	 * Parsing test
	 * <pvserror msg="Parser error">
	 * "Found '-' when expecting 'END'"
	 * </pvserror>
	 * 
	 * pvs output in emacs mode
	 * pvs(8): (setq *pvs-emacs-interface* t)
	 * pvs(9): (typecheck-file "test" nil nil nil)
	 * Parsing test
	 * Found '-' when expecting 'END'
	 * In file test (line 11, col 9)
	 * Error: Parse error
	 * Restart actions (select using :continue):
	 * 0: Return to Top Level (an "abort" restart).
	 * 1: Abort entirely from this (lisp) process.
	 * pvs(10):
	 */
	async emacsInterface (): Promise<void> {
		const cmd: string = '(setq *pvs-emacs-interface* t)';
		await this.pvsExec(cmd);
	}
	/**
	 * Returns the list of commands accepted by the theorem prover
	 */
	async listProofStrategies(): Promise<PvsListProofStrategiesResponseType> {
		return await this.pvsExec('(collect-strategy-names)');
	}
	/**
	 * Finds a symbol declaration. Requires parsing. May return a list of results when the symbol is overloaded.
	 * @param symbolName Name of the symbol
	 */
	async findDeclaration(symbolName: string): Promise<PvsFindDeclarationResponseType> {
		if (new RegExp(PVS_TRUE_FALSE_REGEXP_SOURCE).test(symbolName)) {
			// find-declaration is unable to handle boolean constants if they are not spelled with capital letters
			symbolName = symbolName.toUpperCase();
		} else if (new RegExp(PVS_STRING_REGEXP_SOURCE).test(symbolName)) {
			// string constant, nothing to do
			return Promise.resolve({
				res: null,
				error: null,
				raw: null
			});
		}
		return await this.pvsExec(`(find-declaration "${symbolName}")`);
	}
	/**
	 * List all declarations in a given theory. The theory should parse correctly, otherwise the list of declarations cannot be computed.
	 * @param desc Theory descriptor TODO: use the standard format { fileName, fileExtension, theoryName, line, character }
	 */
	async listDeclarations (desc: PvsListDeclarationsRequest): Promise<PvsDeclarationDescriptor[]> {
		let response: PvsDeclarationDescriptor[] = [];
		const path = desc.file.trim().split("/");
		const fileName = path[path.length - 1].split(".pvs")[0];
		if (fileName !== PRELUDE_FILE) {
			// find-declaration works even if a pvs file does not parse correctly 
			let ans: PvsResponseType = await this.pvsExec(`(list-declarations "${desc.theoryName}")`);
			const allDeclarations: FindDeclarationResponseType = ans.res;
			response = Object.keys(allDeclarations).map(function (key) {
				const info: PvsDeclarationType = allDeclarations[key];
				const ans: PvsDeclarationDescriptor = {
					line: desc.line,
					character: desc.character,
					file: desc.file,
					symbolName: info.symbolName,
					symbolTheory: info.symbolTheory,
					symbolDeclaration: (info) ? info.symbolDeclaration : null,
					symbolDeclarationRange: (info) ? info.symbolDeclarationRange : null,
					symbolDeclarationFile: (info) ? info.symbolDeclarationFile : null,
					symbolDoc: null,
					comment: null,
					error: null
				}
				return ans;
			});
		}
		return response;
	}
	/**
	 * Parse a file
	 * @param fileName File to be parsed, must be in the current pvs context
	 * @returns Parser result, can be either a message (parse successful), or list of syntax errors
	 */
	async parseFile (fileName: string): Promise<PvsParserResponse> {
		fileName = fsUtils.getFilename(fileName, { removeFileExtension: true });
		let response: PvsParserResponse = {
			fileName: fileName,
			res: null,
			error: null
		};
		if (this.pvsContextFolder !== this.pvsPath && this.pvsContextFolder !== this.pvsLibraryPath) {
			const parserInfo: PvsResponseType = await this.pvsExec(`(parse-file "${fileName}" nil nil)`); // (defmethod parse-file ((filename string) &optional forced? no-message?)
			if (parserInfo.error) {
				response.error = parserInfo.error.parserError;
			} else {
				response.res = parserInfo.res
			}
		} else {
			if (this.connection) {
				this.info(`Reading library file ${fileName}...`);
			}
		}
		return response;
	}
	/**
	 * Parse all files in the current context folder
	 * @returns Parser result for each file, can be either a message (parse successful), or list of syntax errors
	 */
	async parseCurrentContext (): Promise<ContextDiagnostics> {
		const result: { [ fileName: string ] : PvsParserResponse } = {};
		const contextFiles: FileList = await fsUtils.listPvsFiles(this.pvsContextFolder);
		if (contextFiles && contextFiles.fileNames) {
			for (const i in contextFiles.fileNames) {
				result[contextFiles.fileNames[i]] = await this.parseFile(contextFiles.fileNames[i]);
			}
		}
		this.ready();
		return result;
	}
	/**
	 * Shows the Type Check Conditions (TCCs) for the selected theory.
	 * This command triggers typechecking and creates a .tccs file on disk.
	 * The .tccs file name is the name of the selected theory.
	 * @returns TODO: change return type to an array of TCC descriptors
	 */
	async showTccs (file: { fileName: string, fileExtension: string }, theoryName: string): Promise<PvsResponseType> {	
		const ans: PvsResponseType = await this.pvsExec(`(show-tccs "${theoryName}" nil)`);
		// const res = await this.serverProxy.changeContext(this.pvsContextFolder);
		// const res = await this.serverProxy.typecheck(fileName);
		// const res = await this.serverProxy.lisp(cmd);
		// create a new file with the tccs. The file name corresponds to the theory name.
		const tccsFileName: string = path.join(this.pvsContextFolder, `${theoryName}.tccs`);
		const tccsFileNameJSON: string = path.join(this.pvsContextFolder, `${theoryName}.tccs.json`);
		if (ans && ans.res) {
			const tccsFileContent: string = ans.raw;
			fsUtils.writeFile(tccsFileName, tccsFileContent).then(() => {
				const tccDescriptors: TccDescriptor[] = ans.res;
				if (tccDescriptors) {
					const json = tccDescriptors.map((desc: TccDescriptor) => {
						return {
							formulaName: desc.formulaName,
							line: desc.symbolLine,
							character: desc.symbolCharacter
						};
					});
					fsUtils.writeFile(tccsFileNameJSON, JSON.stringify(json, null, " "));
				}	
			});
		} else {
			const tccsFileContent: string = `% ${ans.raw}`;
			fsUtils.writeFile(tccsFileName, tccsFileContent).then(() => {
				const tccsFileNameJSON: string = path.join(this.pvsContextFolder, `${theoryName}.tccs.json`);
				fsUtils.writeFile(tccsFileNameJSON, "[]");	
			});
		}
		return ans;
	}

	/**
	 * DEPRECATED Animates a pvs expression
	 * @param desc Expression descriptor
	 */
	// async runit (desc: ExpressionDescriptor): Promise<EvaluationResult> {
	// 	// start pvsio process
	// 	await this.pvsio();
	// 	let cmd: string = '(setq *disable-gc-printout* t)';
	// 	// disable garbage collector printout
	// 	await this.pvsioExec("disable-gc-printout");
	// 	// // enable emacs interface
	// 	// cmd = '(setq *pvs-emacs-interface* t)';
	// 	// await this.pvsioExec("emacs-interface", cmd);
	// 	// make sure we are in the correct context
	// 	cmd = `(change-context "${this.pvsContextFolder}" t)`;
	// 	await this.pvsioExec("change-context");
	// 	// typecheck
	// 	let fileName = fsUtils.getFilename(desc.fileName, { removeFileExtension: true });
	// 	cmd = `(typecheck-file "${fileName}" nil nil nil)`;
	// 	await this.pvsioExec("typecheck-file");
	// 	// load semantic attachments
	// 	cmd = "(load-pvs-attachments)";
	// 	await this.pvsioExec("load-pvs-attachments");
	// 	// enter pvsio mode
	// 	cmd = `(evaluation-mode-pvsio "${desc.theoryName}" nil nil nil)`; // the fourth argument removes the pvsio 	banner
	// 	await this.pvsioExec("evaluation-mode-pvsio");
	// 	// send expression to be evaluated
	// 	cmd = `${desc.expression};`;
	// 	let ans = await this.pvsioExec("eval-expr");
	// 	// await this.pvsioExec("quit-pvsio", "quit;");
	// 	this.pvsioProcess.kill();
	// 	return {
	// 		fileName: desc.fileName,
	// 		theoryName: desc.theoryName,
	// 		msg: "%-- animation result for " + desc.expression,
	// 		result: ans.res
	// 	};
	// }

	/**
	 * Internal function, proves a theorem stored in a .pvs file
	 * @param desc Formula descriptor
	 */
	private async proveTheorem(desc: { fileName: string, theoryName: string, formulaName: string, line: number }): Promise<void> {
		if (desc) {			
			// await this.pvsExec(`(prove-formula "${desc.theoryName}" "${desc.formulaName}" t)`);
			await this.pvsExec(`(prove-file-at "${desc.theoryName}" ${desc.line} nil nil)`);
		}
	}
	/**
	 * Internal function, proves a theorem stored in a .tccs file
	 * @param desc Formula descriptor
	 */
	private async proveTcc(desc: { fileName: string, theoryName: string, formulaName: string, line: number }): Promise<void> {
		if (desc) {			
			// await this.pvsExec(`(prove-formula "${desc.theoryName}" "${desc.formulaName}" t)`);
			await this.pvsExec(`(prove-file-at "${desc.theoryName}" ${desc.line} nil nil)`);
		}
	}
	/**
	 * Proves a formula
	 * @param desc Formula descriptor
	 */
	async proveFormula(desc: { fileName: string, fileExtension: string, theoryName: string, formulaName: string, line: number }): Promise<void> {
		if (desc) {
			if (desc.fileExtension === ".pvs") {
				return await this.proveTheorem(desc);
			} else if (desc.fileExtension === ".tccs") {
				return await this.proveTcc(desc);
			} else {
				if (this.connection) {
					this.connection.console.error(`Could not prove formula ${desc.formulaName} (formula is not in a pvs file).`);
				}
			}
		}
	}
	/**
	 * Returns the status of theorems and tccs in a given theory
	 * @param desc Theory descriptor, defines the theoryName
	 */
	async getTheoryStatus(desc: { fileName: string, fileExtension: string, theoryName: string }): Promise<TheoryStatus> {
		if (desc && desc.theoryName) {
			const statusInfo: PvsResponseType = await this.pvsExec(`(status-proof-theory "${desc.theoryName}")`, desc);
			// const statusInfo: PvsResponseType = await this.pvsExec(`(prove-tccs-theory "${theoryName}" nil "${fileName}" nil)`);
			if (statusInfo && statusInfo.res) {
				// status-proof-theories theoryname
				const theoryStatus: TheoryStatus = statusInfo.res;
				return theoryStatus;
			}
		}
		return null;
	}
	/**
	 * Typechecks a file
	 * @param uri The uri of the file to be typechecked
	 * @param attemptProof Tries to discharge all tccs (default is no)
	 */
	async typecheckFile (desc: { fileName: string, fileExtension: string }, attemptProof?: boolean): Promise<PvsTypecheckerResponse> {
		const context: string = this.pvsContextFolder;
		let response: PvsTypecheckerResponse = {
			fileName: desc.fileName,
			res: null,
			error: null
		};
		if (context !== this.pvsPath && context !== path.join(this.pvsPath, "lib")) {
			const info: PvsResponseType = (attemptProof) ? await this.pvsExec(`(typecheck-file "${desc.fileName}" nil t nil)`)
															: await this.pvsExec(`(typecheck-file "${desc.fileName}" nil nil nil)`);
			if (info && info.error) {
				response.error = info.error.parserError;
			} else {
				// load status info for each theory
				const theoryMap: TheoryMap = await utils.listTheoriesInFile(path.join(this.pvsContextFolder, `${desc.fileName}.pvs`));
				if (theoryMap) {
					response.res = {};
					const theoryNames: string[] = Object.keys(theoryMap);
					for (const i in theoryNames) {
						const theoryName: string = theoryNames[i];
						const theoryStatus: TheoryStatus = await this.getTheoryStatus({ fileName: desc.fileName, fileExtension: ".pvs", theoryName });	
						if (theoryStatus) {
							theoryStatus.theorems = theoryStatus.theorems || {};
							const pvsResponse: PvsResponseType = await this.showTccs(desc, theoryName);
							const tccArray: TccDescriptor[] = (pvsResponse && pvsResponse.res) ? pvsResponse.res : null;
							if (tccArray) {
								// set tcc flags in theoryStatus and update filename and position
								for (const i in tccArray) {
									const formulaName: string = tccArray[i].formulaName;
									if (theoryStatus.theorems[formulaName]) {
										theoryStatus.theorems[formulaName].isTcc = true;
										theoryStatus.theorems[formulaName].fileName = `${theoryName}.tccs`;
										theoryStatus.theorems[formulaName].position = {
											line: tccArray[i].line,
											character: 0
										}
									} else {
										console.error("ooops");
										theoryStatus.theorems[formulaName] = {
											isTcc: true,
											fileName: `${theoryName}.tccs`,
											position: {
												line: tccArray[i].line,
												character: 0
											},
											theoryName: theoryName,
											formulaName: formulaName,
											status: "not available"
										};
									}
								}
							}
							response.res[theoryName] = theoryStatus;
						}
					}
				}
				// await this.saveContext();
			}
		} else {
			if (this.connection) {
				this.connection.console.info(`PVS library file ${desc.fileName} already typechecked.`);
			}
		}
		return response;
	}
	/**
	 * Typechecks a file and tries to discharge all tccs
	 */
	async typecheckProve (desc: { fileName: string, fileExtension: string }): Promise<PvsTypecheckerResponse> {
		return this.typecheckFile(desc, true)
	}
	private async saveContext(): Promise<PvsResponseType> {
		return await this.pvsExec('(save-context)');
	}
	private async clearContext (): Promise<void> {
		const currentContext: string = this.pvsContextFolder;
		if (currentContext) {
			await fsUtils.deletePvsCache(currentContext);
		}
	}
	/**
	 * Typechecks a theory
	 * @param theoryName The theory to be typechecked
	 */
	async typecheckTheory(theoryName: string): Promise<PvsParserResponse> {
		if (theoryName) {
			const pvsContextFolder: string = this.getContextFolder();
			const list: TheoryList = await utils.listTheories(pvsContextFolder);
			if (list && list.theories && list.theories[theoryName]) {
				const fileName = list.theories[theoryName].fileName;
				return (await this.pvsExec(`(typecheck-file "${fileName}" nil nil nil)`)).res;
			}
		}
		return null;
	}
	/**
	 * Provides pvs version information
	 */
	async getPvsVersionInfo(): Promise<PvsVersionInfoResponseType> {
		const cmd: string = '(get-pvs-version-information)';
		return await this.pvsExec(cmd);
	}

	async prettyprintRegion (desc: PrettyPrintRegionRequest): Promise<PrettyPrintRegionResult> {
		// TODO
		return null;
	}

	// TODO: make a single function out of stepTcc and stepProof
	async stepProof(data: { fileName: string, theoryName: string, formulaName: string, line: number }): Promise<PvsResponseType> {
		// const tactics = await PvsProcess.test();
		// const res = await PvsProcess.prf2json(tactics[1], "test");
		// return {
		// 	res: JSON.stringify(res),
		// 	raw: null,
		// 	error: null
		// };
		const cmd: string = `(edit-proof-at "${data.fileName}" nil ${data.line} "pvs" "${data.fileName}.pvs" 0 nil)`;
		const response: PvsResponseType = await this.pvsExec(cmd);
		if (response && response.res) {
			const proof: { [key: string]: any } = PvsProcess.prf2json(response.res, data.formulaName);
			proof['desc'] = data; // append descriptor that identifies file, formula, and line
			response.res = JSON.stringify(proof);
		}
		return response;
	}
	async stepTcc(data: { fileName: string, theoryName: string, formulaName: string, line: number }): Promise<PvsResponseType> {
		const cmd: string = `(edit-proof-at "${data.theoryName}" nil ${data.line} "tccs" "${data.theoryName}.tccs" 0 nil)`;
		const response: PvsResponseType = await this.pvsExec(cmd);
		if (response && response.res) {
			const proof: { [key: string]: any } = PvsProcess.prf2json(response.res, data.formulaName);
			proof['desc'] = data; // append descriptor that identifies file, formula, and line
			response.res = JSON.stringify(proof);
		}
		return response;
	}

	//--- utility functions
	static getExpression(prf: string): string {
		let par: number = 0;
		let match: RegExpMatchArray = null;
		const regexp: RegExp = new RegExp(/([\(\)])/g);
		while(match = regexp.exec(prf)) {
			switch (match[1]) {
				case "(": { par++; break; }
				case ")": { par--; break; }
				default: {}
			}
			if (par === 0) {
				return prf.substr(0, match.index + 1);
			}
		}
		return "";
	}

	/**
	 * Utility function, transforms a .prf (proof file) into a json object
	 * @param prf proof file file content
	 * @param formulaName name of the theorem or tcc associated to the proof
	 * @param parent argument not used in regular invocations --- it is used by the function itself during recursive calls, to keep track of the current parent in the proof tree
	 */
	static prf2json(prf: string, formulaName: string, parent?: ProofNodeType): ProofObjectType {
		if (prf) {
			prf = prf.trim();
			const res: ProofObjectType = {
				proof: {
					id: formulaName,
					children: [],
					type: "root"
				}
			};
			while (prf && prf.length) {
				if (prf.startsWith(`(""`)) {
					// root node
					const match: RegExpMatchArray = /\(\"\"([\w\W\s]+)\s*\)/.exec(prf);
					prf = match[1].trim();
					parent = res["proof"];
				} else {
					// series of proof branches or a proof commands
					const expr: string = PvsProcess.getExpression(prf);
					if (expr && expr.length) {
						if (expr.startsWith("((")) {
							// series of proof branches
							// remove a pair of parentheses and iterate
							const match: RegExpMatchArray = /\(([\w\W\s]+)\s*\)/.exec(prf);
							const subexpr: string = match[1];
							const currentParent: ProofNodeType = parent.children[parent.children.length - 1];
							PvsProcess.prf2json(subexpr, formulaName, currentParent);
						} else if (expr.startsWith(`("`)) {
							// proof command from a labelled branch -- remove the label and iterate
							const match: RegExpMatchArray = /\(\"(\d+)\"\s*([\w\W\s]+)/.exec(expr);
							const subexpr: string = match[2].replace(/\n/g, ""); // remove all \n introduced by pvs in the expression
							const currentBranch: ProofNodeType = { id: match[1], children:[], type: "proof-branch" };
							parent.children.push(currentBranch);
							PvsProcess.prf2json(subexpr, formulaName, currentBranch);
						} else {
							// proof command
							parent.children.push({
								id: expr.replace(/\n/g, ""), // remove all \n introduced by pvs in the expression
								children: [],
								type: "proof-command"
							});
						}
						prf = prf.substr(expr.length).trim();
					} else {
						// ) parentheses comes before (, from parsing a series labelled branches, just ignore them and iterate
						const match: RegExpMatchArray = /\)+([\w\W\s]*)/.exec(prf);
						prf = match[1].trim(); // remove all \n introduced by pvs in the expression
						if (prf && prf.length) {
							PvsProcess.prf2json(prf, formulaName, parent);
						}
					}
				}
			}
			return res;
		}
		return null;
	}
}