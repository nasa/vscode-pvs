/**
 * @module PvsProcess
 * @version 2019.02.07
 * PVS process wrapper
 * @author Paolo Masci
 * @date 2019.02.07
 * @copyright 
 * Copyright 2016 United States Government as represented by the
 * Administrator of the National Aeronautics and Space Administration. No
 * copyright is claimed in the United States under Title 17, 
 * U.S. Code. All Other Rights Reserved.
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
import { PvsExecutionContext } from './common/pvsExecutionContextInterface';
import * as language from "./common/languageKeywords";
// note: ./common is a symbolic link. if vscode does not find it, try to restart TS server: CTRL + SHIFT + P to show command palette, and then search for Typescript: Restart TS Server
import { 
	PvsResponseType, PvsParserResponse, PvsDeclarationDescriptor,
	PRELUDE_FILE, PvsDeclarationType, FormulaDescriptor, ProofResult, PrettyPrintRegionRequest,
	PrettyPrintRegionResult, ExpressionDescriptor, EvaluationResult, PvsListDeclarationsRequest,
	PvsFindDeclarationRequest, PvsDefinition, PvsTheoryListDescriptor,
	TccDescriptorArray, TccDescriptor, PvsFileListDescriptor, PvsTypecheckerResponse
} from './common/serverInterface'
import { Connection, TextDocument } from 'vscode-languageserver';
import * as path from 'path';
import { PVS_TRUE_FALSE_REGEXP_SOURCE, PVS_STRING_REGEXP_SOURCE } from "./common/languageKeywords";
import * as fs from './common/fsUtils';
import { PvsFindDeclarationInterface, PvsLispReader } from './pvsLisp';
// import * as xmlrpcProvider from './common/xmlrpcProvider';


export interface ContextDiagnostics {
	[fileName: string]: PvsParserResponse
};

/**
 * Wrapper class for PVS: spawns a PVS process, and exposes the PVS Lisp interface as an asyncronous JSON/RPC server.
 */
export class PvsProcess {
	private pvsProcess: ChildProcess = null;
	private pvsProcessBusy: boolean = false;

	private pvsioProcess: ChildProcess = null;
	private proverProcess: ChildProcess = null;
	private pvsioProcessBusy: boolean = false;
	private proverProcessBusy: boolean = false;

	// private serverProxy: xmlrpcProvider.XmlRpcProxy;

	private pvsServerProcess: ChildProcess = null;

	private pvsCmdQueue: Promise<PvsResponseType> = Promise.resolve({ res: null, error: null, raw: null });

	private pvsPath: string = null;
	private pvsContextFolder: string = null;
	private pvsServerPath: string = null;

	private connection: Connection;
	private enableNotifications: boolean;
	private readyString: string = "PVS ready!";
	// utility functions for showing notifications on the status bar
	private info(msg: string) {
		if (this.enableNotifications && this.connection && msg && msg.length > 10 && !msg.startsWith(";;;")) {
			this.connection.sendNotification('server.status.update', msg.trim());
		}
	}
	private ready() {
		if (this.enableNotifications) {
			this.connection.sendNotification('pvs-ready', this.readyString);
		}
	}
	private error(msg: string) {
		if (this.enableNotifications) {
			this.connection.sendNotification('pvs-error', msg);
		}
	}


	/**
	 * @returns The current pvs context path
	 */
	getContextFolder(): string {
		return this.pvsContextFolder;
	}

	/**
	 * @returns Path of the prelude library
	 */
	getLibrariesPath(): string {
		return path.join(this.pvsPath, "lib");
	}

	/**
	 * @constructor
	 * @param pvsExecutionContext PVS context 
	 * @param connection Connection with the language client
	 */
	constructor (pvsExecutionContext: PvsExecutionContext, connection?: Connection) {
		this.pvsPath = pvsExecutionContext.pvsPath || __dirname;
		this.pvsContextFolder = pvsExecutionContext.pvsContextFolder || __dirname;
		this.pvsServerPath = pvsExecutionContext.pvsServerPath || __dirname;

		// this.serverProxy = new xmlrpcProvider.XmlRpcProxy();

		this.connection = connection;
	}
	/**
	 * Internal function, used to communicate that the process is busy and cmd cannot be executed
	 * @param cmd 
	 */
	private cannotExecute (msg: string): Promise<PvsResponseType> {
		if (this.connection) { this.connection.console.error(msg); }
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

	private pvsExecAux(cmd: string): Promise<PvsResponseType> {
		if (this.pvsProcessBusy) {
			const msg: string = "PVS busy, cannot execute " + cmd + " :/";
			return this.cannotExecute(msg);
		}
		const pvsLispReader: PvsLispReader = new PvsLispReader(this.connection);
		const match: RegExpMatchArray = /\(\b([\w-]+)\b.*\)/.exec(cmd);
		if (match && match[1]) {
			const commandId: string = match[1];
			if (this.connection) { this.connection.console.log(cmd); }
			return new Promise((resolve, reject) => {
				const listener = (data: string) => {
					if (this.connection) {
						this.connection.console.log(data); // this is the crude pvs lisp output, useful for debugging
						this.info(data);
					}
					pvsLispReader.read(data, async (pvsOut: string) => {
						this.pvsProcess.stdout.removeListener("data", listener); // remove listener otherwise this will capture the output of other commands
						this.pvsProcessBusy = false;
						const ans: PvsResponseType = pvsLispReader.parse(commandId, pvsOut);
						resolve(ans);
					});
				};
				this.pvsProcess.stdout.on("data", listener);
				this.pvsProcess.stdin.write(cmd + "\n");
			});
		}
		if (this.connection) { this.connection.console.error("Unrecognised command " + cmd); }
		Promise.reject({
			res: null,
			error: "Unrecognised command " + cmd,
			raw: null
		});
	}

	// /**
	//  * Executes a pvs lisp command using the pvs process
	//  * @param commandId Command name (e.g,. parse-file), see list of commands in PvsLisp
	//  * @param cmd The pvs lisp command, e.g., (parse-file "main" nil nil)
	//  */
	// private __pvsExecAux(commandId: string, cmd: string): Promise<PvsResponseType> {
	// 	const _this = this;
	// 	// utility function, automatically responds to lisp interactive commands, such as when pvs crashes into lisp
	// 	async function getResult(pvsLispResponse: string): Promise<PvsResponseType> {
	// 		const ans: PvsResponseType = JSON.parse(pvsLispResponse);
	// 		// the following :continue creates problems with the parser -- sometimes the entire process quits
	// 		// if (ans.error) {
	// 		// 	let option: number = +ans.error.restartOption;
	// 		// 	_this.continueLisp(option);
	// 		// }
	// 		return ans; 
	// 	}
	// 	if (this.pvsProcessBusy) {
	// 		const msg: string = "PVS busy, cannot execute " + cmd + " :/";
	// 		return this.cannotExecute(msg);
	// 	}	
	// 	this.pvsProcessBusy = true;
	// 	const pvslispParser = new PvsLisp(commandId, this.connection);
	// 	// connection.console.info("Executing command " + cmd);
	// 	if (this.connection) { this.connection.console.log(cmd); }
	// 	return new Promise(async function (resolve, reject) {
	// 		const listener = function (data: string) {
	// 			if (_this.connection) { _this.connection.console.log(data); }// this is the crude pvs lisp output, useful for debugging
	// 			pvslispParser.parse(data, async (res: string) => {
	// 				_this.pvsProcess.stdout.removeListener("data", listener); // remove listener otherwise this will capture the output of other commands
	// 				const ans: PvsResponseType = await getResult(res);
	// 				_this.pvsProcessBusy = false;
	// 				resolve(ans);
	// 			});
	// 		};
	// 		_this.pvsProcess.stdout.on("data", listener);
	// 		_this.pvsProcess.stdin.write(cmd + "\n");
	// 	});
	// }
	private pvsExec(cmd: string): Promise<PvsResponseType> {
		this.pvsCmdQueue = new Promise((resolve, reject) => {
			this.pvsCmdQueue.then(() => {
				// this.pvsExecAux(commandId, cmd).then((ans: PvsResponseType) => {
				// 	resolve(ans);
				// });
				this.pvsExecAux(cmd).then((ans: PvsResponseType) => {
					resolve(ans);
				});
			});
		});
		return this.pvsCmdQueue;
	}

	

	/**
	 * Executes a pvs lisp command using the pvsio process
	 * @param commandId Command name (e.g,. parse-file), see list of commands in PvsLisp
	 * @param cmd The pvs lisp command, e.g., (parse-file "main" nil nil)
	 */
	private pvsioExec(cmd: string): Promise<PvsResponseType> {
		const _this = this;
		// utility function, automatically responds to lisp interactive commands, such as when pvs crashes into lisp
		function getResult(pvsLispResponse: string): PvsResponseType {
			const ans: PvsResponseType = JSON.parse(pvsLispResponse);
			if (/.*==>\s*(.*)\s*<PVSio>/.test(ans.res)) {
				let match: RegExpMatchArray = /.*==>\s*(.*)\s*<PVSio>/.exec(ans.res);
				ans.res = match[1];
			}
			return ans; 
		}
		if (this.pvsioProcessBusy) {
			const msg: string = "PVSio busy, cannot execute " + cmd + ":/";
			return this.cannotExecute(msg);
		}	
		this.pvsioProcessBusy = true;
		const pvsLispReader: PvsLispReader = new PvsLispReader(this.connection);
		// const pvslispParser = new PvsLisp(commandId, this.connection);
		// if (this.connection) { this.connection.console.log(cmd); }
		return new Promise(function (resolve, reject) {
			let listener = function (data: string) {
				if (_this.connection) { _this.connection.console.log(data); }// this is the crude pvs lisp output, useful for debugging
				pvsLispReader.read(data, async (res: string) => {
					_this.pvsProcess.stdout.removeListener("data", listener); // remove listener otherwise this will capture the output of other commands
					_this.pvsioProcessBusy = false;
					resolve(getResult(res));
				});
			};
			_this.pvsioProcess.stdout.on("data", listener);
			_this.pvsioProcess.stdin.write(cmd + "\n");
		});
	}
	// /**
	//  * Executes a pvs lisp command using the prover process
	//  * @param commandId Command name (e.g,. parse-file), see list of commands in PvsLisp
	//  * @param cmd The pvs lisp command, e.g., (parse-file "main" nil nil)
	//  */
	// private proverExec(commandId: string, cmd: string): Promise<PvsResponseType> {
	// 	const _this = this;
	// 	// utility function, automatically responds to lisp interactive commands, such as when pvs crashes into lisp
	// 	async function getResult(pvsLispResponse: string): Promise<PvsResponseType> {
	// 		const ans: PvsResponseType = JSON.parse(pvsLispResponse);
	// 		if (ans.error) {
	// 			let option: number = +ans.error.restartOption;
	// 			_this.continueLisp(option);
	// 		}
	// 		return ans; 
	// 	}
	// 	if (this.proverProcessBusy) {
	// 		const msg: string = "Prover busy, cannot execute " + cmd + ":/";
	// 		return this.cannotExecute(msg);
	// 	}	
	// 	this.proverProcessBusy = true;
	// 	const pvslispParser = new PvsLisp(commandId, this.connection);
	// 	// connection.console.info("Executing command " + cmd);
	// 	if (this.connection) { this.connection.console.log(cmd); }
	// 	return new Promise(async (resolve, reject) => {
	// 		let listener = function (data: string) {
	// 			if (_this.connection) { _this.connection.console.log(data); }// this is the crude pvs lisp output, useful for debugging
	// 			pvslispParser.parse(data, async (res: string) => {
	// 				_this.proverProcess.stdout.removeListener("data", listener); // remove listener otherwise this will capture the output of other commands
	// 				_this.proverProcessBusy = false;
	// 				resolve(await getResult(res));
	// 			});
	// 		};
	// 		_this.proverProcess.stdout.on("data", listener);
	// 		_this.proverProcess.stdin.write(cmd + "\n");
	// 	});
	// }

	async pvs(opt?: {
		enableNotifications?: boolean
	}): Promise<boolean> {
		opt = opt || {};
		this.enableNotifications = opt.enableNotifications;
		if (!this.pvsProcessBusy) {
			this.pvsProcessBusy = true;
			// const pvslispParser = new PvsLisp("pvs-init", this.connection);
			const pvsLispReader = new PvsLispReader(this.connection);
			let pvs: string = path.join(this.pvsPath, "pvs");
			const args: string[] = [ "-raw"];//, "-port", "22334" ];
			if (this.connection) { this.connection.console.info(`Spawning pvs process ${pvs} ${args.join(" ")}`); }
			return new Promise(async (resolve, reject) => {
				const fileExists: boolean = await fs.fileExists(pvs);
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
					if (this.connection) {
						this.connection.console.info("PVS process ready!");
					}
				} else {
					console.error(`\n>>> PVS executable not found at ${pvs} <<<\n`);
					resolve(false)
				}
			});
		}
	}

	// /**
	//  * Starts the pvs process
	//  */
	// async __pvs (): Promise<{}> {
	// 	if (!this.pvsProcessBusy) {
	// 		this.pvsProcessBusy = true;
	// 		const pvslispParser = new PvsLisp("pvs-init", this.connection);	
	// 		let cmd: string = path.join(this.pvsPath, "pvs");
	// 		if (this.connection) { this.connection.console.info("Spawning pvs process " + cmd); }
	// 		return new Promise((resolve, reject) => {
	// 			this.pvsProcess = spawn(cmd, ["-raw"]);
	// 			this.pvsProcess.stdout.setEncoding("utf8");
	// 			this.pvsProcess.stderr.setEncoding("utf8");
	// 			const _this = this;
	// 			let listener = function (data: string) {
	// 				if (_this.connection) { _this.connection.console.log(data); } // this is the crude pvs lisp output, useful for debugging
	// 				pvslispParser.parse(data, (res: string) => {
	// 					// connection.console.info(res);
	// 					_this.pvsProcess.stdout.removeListener("data", listener); // remove listener otherwise this will capture the output of other commands
	// 					_this.pvsProcessBusy = false;
	// 					resolve();
	// 				});
	// 			};
	// 			this.pvsProcess.stdout.on("data", listener);
	// 			this.pvsProcess.stderr.on("data", (data: string) => {
	// 				if (this.connection) { this.connection.console.log(data); }
	// 			});
	// 			if (this.connection) { this.connection.console.info("PVS process ready!"); }
	// 		});
	// 	}
	// }
	/**
	 * Starts the pvsio process
	 */
	private async pvsio (): Promise<{}> {
		if (!this.pvsioProcessBusy) {
			this.pvsioProcessBusy = true;
			// const pvslispParser = new PvsLisp("pvs-init", this.connection);	
			const pvsLispReader = new PvsLispReader(this.connection);
			let cmd: string = path.join(this.pvsPath, "pvs");
			if (this.connection) { this.connection.console.info("Spawning pvsio process " + cmd); }
			const _this = this;
			return new Promise(function (resolve, reject) {
				_this.pvsioProcess = spawn(cmd, ["-raw"]);
				_this.pvsioProcess.stdout.setEncoding("utf8");
				_this.pvsioProcess.stderr.setEncoding("utf8");
				let listener = function (data: string) {
					if (this.connection) { this.connection.console.log(data); } // this is the crude pvs lisp output, useful for debugging
					pvsLispReader.read(data, (res: string) => {
						// connection.console.info(res);
						_this.pvsioProcess.stdout.removeListener("data", listener); // remove listener otherwise this will capture the output of other commands
						_this.pvsioProcessBusy = false;
						resolve();
					});
				};
				_this.pvsioProcess.stdout.on("data", listener);
				_this.pvsioProcess.stderr.on("data", function (data: string) {
					if (_this.connection) { _this.connection.console.log(data); }
				});
			});
		}
	}
	// /**
	//  * Starts the prover process
	//  */
	// private async prover (): Promise<{}> {
	// 	if (!this.proverProcessBusy) {
	// 		this.proverProcessBusy = true;
	// 		const pvslispParser = new PvsLisp("pvs-init", this.connection);	
	// 		let cmd: string = path.join(this.pvsPath, "pvs");
	// 		if (this.connection) { this.connection.console.info("Spawning prover process " + cmd); }
	// 		const _this = this;
	// 		return new Promise(function (resolve, reject) {
	// 			_this.proverProcess = spawn(cmd, ["-raw"]);
	// 			_this.proverProcess.stdout.setEncoding("utf8");
	// 			_this.proverProcess.stderr.setEncoding("utf8");
	// 			let listener = function (data: string) {
	// 				if (this.connection) { this.connection.console.log(data); } // this is the crude pvs lisp output, useful for debugging
	// 				pvslispParser.parse(data, function (res: string) {
	// 					// connection.console.info(res);
	// 					_this.proverProcess.stdout.removeListener("data", listener); // remove listener otherwise this will capture the output of other commands
	// 					_this.proverProcessBusy = false;
	// 					resolve();
	// 				});
	// 			};
	// 			_this.proverProcess.stdout.on("data", listener);
	// 			_this.proverProcess.stderr.on("data", function (data: string) {
	// 				if (_this.connection) { _this.connection.console.log(data); }
	// 			});
	// 		});
	// 	}
	// }

	/**
	 * Changes the current context. When the context is changed, all symbol information are erased and the parser/typechecker needs to be re-run.
	 * @param contextFolder Path of the context folder 
	 */
	async changeContext(contextFolder: string): Promise<PvsResponseType> {
		// await this.serverProxy.changeContext(contextFolder);
		const folderExists: boolean = await fs.dirExists(contextFolder);
		if (folderExists) {
			// clearing the context is necessary at the moment because the binary files generated by pvs are broken
			// await this.clearContext();
			if (contextFolder !== this.pvsContextFolder) {
				const cmd: string = '(change-context "' + contextFolder + '" nil)';
				this.pvsContextFolder = contextFolder;
				return await this.pvsExec(cmd);
			}
			return {
				res: { context: contextFolder },
				error: null,
				raw: `Context folder unchanged: ${contextFolder}`
			};
		}
		return {
			res: null,
			error: {
				msg: `Error: Could not change context to ${contextFolder} (folder does not exist or cannot be read).`
			},
			raw: null
		};
	}
	/**
	 * Returns the current context
	 * @returns Path of the current context
	 */
	async currentContext(): Promise<PvsResponseType> {
		const cmd: string = '(pvs-current-directory)';
		return await this.pvsExec(cmd);
	}
	/**
	 * FIXME: remove this command in favour of listPvsFiles?
	 * Identifies the set of theories loaded in the current context
	 * FIXME: it is not clear when theories are loaded in the context? how to find the importchain??
	 * @returns A descriptor providing the list of theories in the current context.
	 * 			The descriptor contains two fields:
	 * 			- files: list of theories grouped by filename (type: { [fileName: string]: string })
	 * 			- theories: list of theories ordered by name (type: string[] )
	 */
	private async listTheories(): Promise<PvsResponseType> {
		const cmd: string = '(context-files-and-theories "'+ this.pvsContextFolder +'")';
		return await this.pvsExec(cmd);
	}
	/**
	 * Returns the pvs files in the current context, i.e., all pvs files in the current context folder
	 * TODO: create a separate module for file system operations?
	 * @returns A descriptor providing the list of pvs files and the name of the context folder
	 */
	async listPvsFiles(): Promise<PvsFileListDescriptor> {
		let files: string[] = await fs.readDir(this.pvsContextFolder);
		let pvsFiles: string[] = files.filter(fileName => {
			return fileName.endsWith(".pvs");
		});
		return {
			fileNames: pvsFiles,
			folder: this.pvsContextFolder
		};
	}
	async writeFile(fileName: string, content: string): Promise<void> {
		await fs.writeFileSync(path.join(this.pvsContextFolder,fileName), content);
	}
	// /**
	//  * Utility function, restores the pvs prompt if pvs crashes into lisp
	//  * @param option The restart option for restoring the pvs prompt
	//  * @private 
	//  */
	// private async continueLisp(option: number): Promise<PvsResponseType> {
	// 	const cmd = ':continue ' + option + "\n";
	// 	return this.pvsExec("pvs-continue", cmd);
	// }
	/**
	 * Disables garbage collector messages
	 */
	async disableGcPrintout(): Promise<PvsResponseType> {
		const cmd: string = '(setq *disable-gc-printout* t)';
		return await this.pvsExec(cmd);
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
	async emacsInterface(): Promise<PvsResponseType> {
		const cmd: string = '(setq *pvs-emacs-interface* t)';
		return await this.pvsExec(cmd);
	}
	/**
	 * Finds a symbol declaration. Requires parsing. May return a list of results when the symbol is overloaded.
	 * @param symbolName Name of the symbol
	 */
	async findDeclaration(symbolName: string): Promise<PvsResponseType> {
		if (new RegExp(PVS_TRUE_FALSE_REGEXP_SOURCE).test(symbolName)) {
			// find-declaration is unable to handle boolean constants if they are not spelled with capital letters
			symbolName = symbolName.toUpperCase();
		} else if (new RegExp(PVS_STRING_REGEXP_SOURCE).test(symbolName)) {
			return Promise.resolve({
				res: null,
				error: null,
				raw: null
			});
		}
		const cmd: string = '(find-declaration "' + symbolName + '")';
		return await this.pvsExec(cmd);
	}
	/**
	 * List all declaration in a given theory. Requires parsing.
	 * @param theoryName Name of the theory 
	 */
	private async _listDeclarations(theoryName: string): Promise<PvsResponseType> {
		const cmd: string = '(list-declarations "' + theoryName + '")';
		return await this.pvsExec(cmd);
	}
	/**
	 * Provides the list of symbols declared in a given theory
	 * @param desc 
	 */
	async listDeclarations (desc: PvsListDeclarationsRequest): Promise<PvsDeclarationDescriptor[]> {
		let response: PvsDeclarationDescriptor[] = [];
		const path = desc.file.trim().split("/");
		const fileName = path[path.length - 1].split(".pvs")[0];
		if (fileName !== PRELUDE_FILE) {
			// find-declaration works even if a pvs file does not parse correctly 
			let ans: PvsResponseType = await this._listDeclarations(desc.theoryName);
			const allDeclarations: PvsFindDeclarationInterface = ans.res;
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
	// /**
	//  * Parse the expression passed as argument
	//  * @param expression The expression to be parsed
	//  */
	// async parseExpression(expression: string): Promise<PvsResponseType> {
	// 	const cmd: string = '(describe (pc-parse "' + expression + '" `expr))';
	// 	return await this.pvsExec("parse-expression", cmd);
	// }
	/**
	 * Parse a file. This is the original API provided by PVS Lisp.
	 * @param fileName File to be parsed, must be in the current pvs context
	 * @private
	 */
	private async _parseFile(fileName: string): Promise<PvsResponseType> {
		const cmd: string = '(parse-file "' + fileName + '" nil nil)'; // is there a way to force parsing of importchain?
		return await this.pvsExec(cmd);
	}
	/**
	 * Parse a file
	 * @param fileName File to be parsed, must be in the current pvs context
	 * @returns Parser result, can be either a message (parse successful), or list of syntax errors
	 */
	async parseFile (fileName: string): Promise<PvsParserResponse> {
		fileName = fs.getFilename(fileName, { removeFileExtension: true });
		// const filePath: string = getPathname(uri);
		let response: PvsParserResponse = {
			fileName: fileName,
			res: null,
			error: null
		};
		if (!this.pvsContextFolder.startsWith(this.pvsPath)) {
			// await _this.pvsProcess.changeContext(filePath);
			const parserInfo: PvsResponseType = await this._parseFile(fileName);
			if (parserInfo.error) {
				response.error = parserInfo.error.parserError;
			} else {
				response.res = parserInfo.res
			}
		} else {
			if (this.connection) {
				this.connection.console.info("PVS library file " + fileName + " already parsed.");
			}
		}
		return response;
	}

	/**
	 * Parse all files in the current context
	 * @returns Parser result for each file, can be either a message (parse successful), or list of syntax errors
	 */
	async parseCurrentContext (): Promise<ContextDiagnostics> {
		const result: { [ fileName: string ] : PvsParserResponse } = {};
		const contextFiles: PvsFileListDescriptor = await this.listPvsFiles();
		if (contextFiles && contextFiles.fileNames) {
			const context: string = contextFiles.folder;
			if (this.pvsContextFolder !== context) {
				await this.changeContext(context);
			}
			for (const i in contextFiles.fileNames) {
				result[contextFiles.fileNames[i]] = await this.parseFile(contextFiles.fileNames[i]);
			}
		}
		return result;
	}

	// /**
	//  * Creates the prover process, if the process has not been created already.
	//  */
	// private async initTypeChecker () {
	// 	if (this.proverProcess === null) {
	// 		// start prover process
	// 		await this.prover();
	// 		let cmd: string = '(setq *disable-gc-printout* t)';
	// 		// disable garbage collector printout
	// 		await this.proverExec("disable-gc-printout", cmd);
	// 	}
	// } 

	/**
	 * Shows the Type Check Conditions (TCCs) for the selected theory.
	 * This command triggers typechecking and creates a .tccs file on disk. The .tccs file name corresponds to the theory name.
	 * @returns An array of TCC descriptors
	 */
	async showTccs (fileName: string, theoryName: string): Promise<PvsResponseType> {	
		const cmd: string = '(show-tccs "' + theoryName + '" nil)';
		const ans: PvsResponseType = await this.pvsExec(cmd);
		// const importChain: PvsTheoryListDescriptor = await this.listTheories();
		// const fileName: string = importChain.theories[theoryName][0]; // this is broken because list-theories is broken -- come files may not have been loaded yet 

		// const res = await this.serverProxy.changeContext(this.pvsContextFolder);

		// const res = await this.serverProxy.typecheck(fileName);

		// const res = await this.serverProxy.lisp(cmd);

		// create a new file with the tccs. The file name corresponds to the theory name.
		if (ans && ans.res) {
			const tccs: TccDescriptor[] = ans.res;
			let tccsFileContent: string = "";
			tccs.forEach((tcc) => {
				tccsFileContent += tcc.content;
			});
			await this.writeFile(theoryName + ".tccs", tccsFileContent);
		}

		// send results back to the client
		return Promise.resolve(ans);
	}

	/**
	 * Animates a pvs expression
	 * @param desc Expression descriptor
	 */
	async runit (desc: ExpressionDescriptor): Promise<EvaluationResult> {
		// start pvsio process
		await this.pvsio();
		let cmd: string = '(setq *disable-gc-printout* t)';
		// disable garbage collector printout
		await this.pvsioExec("disable-gc-printout");
		// // enable emacs interface
		// cmd = '(setq *pvs-emacs-interface* t)';
		// await this.pvsioExec("emacs-interface", cmd);
		// make sure we are in the correct context
		cmd = '(change-context "' + this.pvsContextFolder + '" t)';
		await this.pvsioExec("change-context");
		// typecheck
		let fileName = fs.getFilename(desc.fileName, { removeFileExtension: true });
		cmd = '(typecheck-file "' + fileName + '" nil nil nil)';
		await this.pvsioExec("typecheck-file");
		// load semantic attachments
		cmd = "(load-pvs-attachments)";
		await this.pvsioExec("load-pvs-attachments");
		// enter pvsio mode
		cmd = '(evaluation-mode-pvsio "' + desc.theoryName + '" nil nil nil)'; // the fourth argument removes the pvsio 	banner
		await this.pvsioExec("evaluation-mode-pvsio");
		// send expression to be evaluated
		cmd = desc.expression + ";";
		let ans = await this.pvsioExec("eval-expr");
		// await this.pvsioExec("quit-pvsio", "quit;");
		this.pvsioProcess.kill();
		return {
			fileName: desc.fileName,
			theoryName: desc.theoryName,
			msg: "%-- animation result for " + desc.expression,
			result: ans.res
		};
	}

	// /**
	//  * Proves a formula
	//  * @param desc Formula descriptor
	//  */
	// async proveit (desc: FormulaDescriptor): Promise<ProofResult> {
	// 	await this.initTypeChecker();
	// 	let cmd = '(change-context "' + this.pvsContextFolder + '" t)';
	// 	await this.proverExec("change-context", cmd);
	// 	// typecheck
	// 	let fileName = getFilename(desc.fileName, { removeFileExtension: true });
	// 	cmd = '(typecheck-file "' + fileName + '" nil nil nil)';
	// 	await this.proverExec("typecheck-file", cmd);

	// 	// // enable proof editing
	// 	// cmd = '(edit-proof-at "' + fileName + '" nil ' + desc.line + ' "pvs" "' + fileName + '.pvs" 0 nil)';
	// 	// await this.proverExec("edit-proof-at", cmd);
	// 	// // edit proof
	// 	// const strategyFolder: string = path.join(this.pvsServerPath, "strategies");
	// 	// const grindStrategy: string = path.join(strategyFolder, "grind.lisp");
	// 	// cmd = '(install-proof `' + grindStrategy + ' "' + fileName + '" "' + desc.formula + '" 1 t "Proof" 0)';
	// 	// await this.proverExec("install-proof", cmd);
		
	// 	cmd = '(prove-formula "' + desc.theoryName + '" "'+ desc.formulaName +'" nil)'; 
	// 	let ans: string = (await this.proverExec("prove-formula", cmd)).res;
	// 	// (install-prooflite-scripts "test" "test" 0 t)
	// 	//'(install-prooflite-scripts "' + desc.fileName + '" "' + desc.theoryName +  '" ' + desc.line + ' t)';
	// 	return {
	// 		fileName: desc.fileName,
	// 		theoryName: desc.theoryName,
	// 		msg: "%-- proof state for " + desc.formulaName,
	// 		result: ans
	// 	};
	// }

	async proveFormula(theoryName: string, formulaName: string) {
		// await this.initTypeChecker();
		const cmd = '(prove-formula "' + theoryName + '" "'+ formulaName +'" t)'; 
		await this.pvsExec(cmd);
	}

	// /**
	//  * Shows the declaration of a symbol. Requires typechecking.
	//  * @param fileName 
	//  * @param line 
	//  * @param character 
	//  */
	// async showDeclaration(fileName: string, line: number, character: number): Promise<PvsResponseType> {
	// 	const cmd: string = '(show-declaration "' + fileName + '" "pvs" ' + "'(" + line + " " + character + ")" + ')';
	// 	return await this.pvsExec(cmd);
	// }
	// /**
	//  * Identifies the importchain for a given theory.
	//  * @param theoryName Theory name for which the importchain should be computed
	//  */
	// async showImportChain(theoryName: string): Promise<PvsResponseType> {
	// 	if (theoryName) {
	// 		const cmd: string = '(show-importchain "' + theoryName + '")';
	// 		return await this.pvsExec(cmd);
	// 	}
	// 	return Promise.resolve({
	// 		res: { theories: [] },
	// 		error: null,
	// 		raw: null
	// 	});
	// }
	/**
	 * Internal function, typechecks a file
	 * @param uri The uri of the file to be typechecked
	 * @param tcpFlag Optional flag, triggers automatic proof of tccs
	 */
	private async _typecheckFile(uri: string, tcpFlag?: boolean): Promise<PvsResponseType> {
		let fileName: string = fs.getFilename(uri, { removeFileExtension: true });

		// await this.serverProxy.changeContext(this.pvsContextFolder);
		// await this.serverProxy.typecheck(fileName);

		const cmd: string = (tcpFlag) ? 
			'(typecheck-file "' + fileName + '" nil t nil)'
				: '(typecheck-file "' + fileName + '" nil nil nil)';
		// await this.initTypeChecker();
		return await this.pvsExec(cmd);
		// const cmd: string = '(json-typecheck-file "' + fileName + '")'; /// what is the difference between json-xxx and xxx?
		// return (await this.pvsExec("json-typecheck-file", cmd)).res;
	}

	/**
	 * Typechecks a file
	 * @param uri The uri of the file to be typechecked
	 * @param tcp Tries to discharge tccs
	 */
	async typecheckFile (uri: string, tcp?: boolean): Promise<PvsTypecheckerResponse> {
		const fileName: string = fs.getFilename(uri, { removeFileExtension: true });
		const filePath: string = fs.getPathname(uri);
		let response: PvsTypecheckerResponse = {
			fileName: fileName,
			res: null,
			error: null
		};
		if (filePath !== this.pvsPath && filePath !== path.join(this.pvsPath, "lib")) {
			// await _this.pvsProcess.changeContext(filePath);
			const info: PvsResponseType = await this._typecheckFile(fileName, tcp);
			if (info.error) {
				response.error = info.error.parserError;
			} else {
				response.res = info.res;
				await this.saveContext();
			}
		} else {
			if (this.connection) {
				this.connection.console.info("PVS library file " + fileName + " already typechecked.");
			}
		}
		return response;
	}
	/**
	 * Typechecks a file and tries to discharge all tccs
	 * @param uri The uri of the file to be typechecked
	 */
	async typecheckProve (uri: string): Promise<PvsTypecheckerResponse> {
		return this.typecheckFile(uri, true)
	}

	private async saveContext(): Promise<PvsResponseType> {
		const cmd: string = '(save-context)';
		return await this.pvsExec(cmd);
	}

	// private async clearContext(): Promise<PvsResponseType> {
	// 	if (this.pvsContextFolder.startsWith(this.pvsPath)) {
	// 		const msg: string = `Library files - no need to clear context cache`;
	// 		if (this.connection) { this.connection.console.info(msg); }
	// 		return Promise.resolve({
	// 			error: null,
	// 			res: null,
	// 			raw: msg
	// 		});	
	// 	} else {
	// 		const binFolder: string = path.join(this.pvsContextFolder, "pvsbin");
	// 		const folderExists: boolean = fs.dirExists(binFolder);
	// 		if (folderExists) {
	// 			await fs.rmDir(binFolder);
	// 		}
	// 		const msg: string = `Context cache ${binFolder} cleared`;
	// 		return Promise.resolve({
	// 			error: null,
	// 			res: null,
	// 			raw: msg
	// 		});
	// 	}
	// }

	/**
	 * Typechecks a theory
	 * @param theoryName The theory to be typechecked
	 */
	async typecheckTheory(theoryName: string): Promise<PvsParserResponse> {
		const pvsResponse: PvsResponseType = await this.listTheories();
		if (pvsResponse && pvsResponse.res) {
			const importChain: PvsTheoryListDescriptor = pvsResponse.res;
			if (importChain && importChain.theories) {
				const fileName = importChain.theories[theoryName];
				const cmd: string = '(typecheck-file "' + fileName + '" nil nil nil)';
				// await this.initTypeChecker();
				return (await this.pvsExec(cmd)).res;
			}
		}
		return Promise.resolve(null);
	}
	/**
	 * Provides pvs version information
	 */
	async pvsVersionInformation(): Promise<PvsResponseType> {
		const cmd: string = '(get-pvs-version-information)';
		return await this.pvsExec(cmd);
	}

	async prettyprintRegion (desc: PrettyPrintRegionRequest): Promise<PrettyPrintRegionResult> {
		// TODO
		return null;
	}

	// async continueProof(cmd: string): Promise<string> {
	// 	// await this.initTypeChecker();
	// 	let ans: string = (await this.pvsExec("prove-formula", cmd)).res;
	// 	// (install-prooflite-scripts "test" "test" 0 t)
	// 	//'(install-prooflite-scripts "' + desc.fileName + '" "' + desc.theoryName +  '" ' + desc.line + ' t)';
	// 	return ans;
	// }

	async stepProof(data: { fileName: string, formulaName: string, line: number }): Promise<PvsResponseType> {
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
			response.res = JSON.stringify(PvsProcess.prf2json(response.res, data.formulaName));
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
	// static prf2json(prf: string, prefix?: string): { [key: string]: any } {
	// 	if (prf) {
	// 		prf = prf.trim();
	// 		const res: { [key: string]: any } = {};
	// 		let counter: number = 1;
	// 		while (prf && prf.length) {
	// 			if (prf.startsWith(`(""`)) {
	// 				// proof start
	// 				const match: RegExpMatchArray = /\(\"\"([\w\W\s]+)\s*\)/.exec(prf);
	// 				prf = match[1].trim();
	// 			} else {
	// 				// series of proof branches or a proof commands
	// 				const expr: string = PvsProcess.getExpression(prf);
	// 				if (expr && expr.length) {
	// 					if (expr.startsWith("((")) {
	// 						// series of proof branches
	// 						// remove a pair of parentheses and iterate
	// 						const match: RegExpMatchArray = /\(([\w\W\s]+)\s*\)/.exec(prf);
	// 						const subexpr: string = match[1];
	// 						res[counter++] = PvsProcess.prf2json(subexpr);
	// 					} else if (expr.startsWith(`("`)) {
	// 						// proof command from a labelled branch -- remove the label and iterate
	// 						const match: RegExpMatchArray = /\(\"\d+\"\s*([\w\W\s]+)/.exec(expr);
	// 						const subexpr: string = match[1].replace(/\n/g, ""); // remove all \n introduced by pvs in the expression
	// 						res[counter++] = PvsProcess.prf2json(subexpr);
	// 					} else {
	// 						res[counter++] = expr.replace(/\n/g, ""); // remove all \n introduced by pvs in the expression
	// 					}
	// 					prf = prf.substr(expr.length).trim();
	// 				} else {
	// 					// ) parentheses comes before (, from parsing labelled branches, just ignore them and iterate
	// 					const match: RegExpMatchArray = /\)+([\w\W\s]*)/.exec(prf);
	// 					prf = match[1].trim(); // remove all \n introduced by pvs in the expression
	// 					if (prf && prf.length) {
	// 						PvsProcess.prf2json(prf);
	// 					}
	// 				}
	// 			}
	// 		}
	// 		return res;
	// 	}
	// 	return null;
	// }

	static prf2json(prf: string, formulaName: string, parent?: { id: string, children: any[]}): { [key: string]: any } {
		if (prf) {
			prf = prf.trim();
			const res: { [key: string]: any } = {};
			while (prf && prf.length) {
				if (prf.startsWith(`(""`)) {
					// root node
					const match: RegExpMatchArray = /\(\"\"([\w\W\s]+)\s*\)/.exec(prf);
					prf = match[1].trim();
					res["proof"] = {
						id: formulaName,
						children: []
					};
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
							const currentParent: { id: string, children: any[] } = parent.children[parent.children.length - 1];
							PvsProcess.prf2json(subexpr, formulaName, currentParent);
						} else if (expr.startsWith(`("`)) {
							// proof command from a labelled branch -- remove the label and iterate
							const match: RegExpMatchArray = /\(\"(\d+)\"\s*([\w\W\s]+)/.exec(expr);
							const subexpr: string = match[2].replace(/\n/g, ""); // remove all \n introduced by pvs in the expression
							const currentBranch: { id: string, children: any[] } = { id: match[1], children: [] };
							parent.children.push(currentBranch);
							PvsProcess.prf2json(subexpr, formulaName, currentBranch);
						} else {
							// proof command
							parent.children.push({
								id: expr.replace(/\n/g, ""), // remove all \n introduced by pvs in the expression
								children: []
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

	static test(): string[] {
		const tactics: string[] = [
			`(""
			(skeep)
			(lemma "sin2_cos2")
			(inst?)
			(both-sides-f 1 "sq")
			(("1"
			  (both-sides "-" "sq(cos(acos(sig)))" -1)
			  (("1"
				(assert)
				(replaces -1)
				(rewrite "cos_acos")
				(rewrite "sq_sqrt"))
			   ("2" (iff) (ground))))
			 ("2"
			  (case "FORALL (zz,qq:nnreal): sq(zz) = sq(qq) IMPLIES zz=qq")
			  (("1"
				(invoke (inst - "%1" "%2") (! 1 1) (! 1 2))
				(("1" (assert) (assert)) ("2" (assert))
				 ("3" (assert) (lemma "sin_ge_0") (inst?) (assert))))
			   ("2"
				(hide-all-but 1)
				(skeep)
				(case "sqrt(sq(zz)) = sqrt(sq(qq))")
				(("1" (assert)) ("2" (replaces -1))))))))`,

			`(""
			(case "FORALL (zz,qq:nnreal): sq(zz) = sq(qq) IMPLIES zz=qq")
			(("1"
			  (label "igz" -1)
			  (hide "igz")
			  (skeep)
			  (skoletin :var "AA")
			  (assert)
			  (expand "xyz2spherical")
			  (assert)
			  (lift-if)
			  (split -)
			  (("1"
				(flatten)
				(assert)
				(case "x = 0 AND y=0")
				(("1"
				  (flatten)
				  (assert)
				  (flatten)
				  (replaces -5)
				  (assert)
				  (replace -1)
				  (replace -2)
				  (assert)
				  (expand "^" + 1)
				  (expand "expt")
				  (assert)
				  (lemma "sqrt_sq")
				  (inst - "z")
				  (split -)
				  (("1"
					(expand "expt")
					(expand "expt")
					(expand "sq")
					(replaces -1)
					(assert)
					(expand "spherical2xyz")
					(assert)
					(rewrite "cos_pi")
					(rewrite "sin_0")
					(rewrite "sin_pi")
					(rewrite "cos_0")
					(assert))
				   ("2" (propax))))
				 ("2"
				  (hide-all-but (-1 1))
				  (case "NOT x^2+y^2=0")
				  (("1" (assert))
				   ("2"
					(case "NOT sq(x)+sq(y)=0")
					(("1" (grind))
					 ("2"
					  (lemma "sq_eq_0")
					  (inst-cp - "x")
					  (inst - "y")
					  (grind))))))))
			   ("2"
				(flatten)
				(split -1)
				(("1"
				  (flatten)
				  (assert)
				  (hide -3)
				  (lift-if)
				  (split -)
				  (("1" (ground))
				   ("2"
					(flatten)
					(hide 1)
					(replaces -1)
					(assert)
					(case "x = 0 AND y = 0")
					(("1"
					  (flatten)
					  (replaces -1)
					  (replaces -1)
					  (assert)
					  (case-replace "sqrt(z^2) = -z")
					  (("1"
						(expand "spherical2xyz")
						(rewrite "cos_pi")
						(assert)
						(rewrite "sin_pi")
						(assert))
					   ("2"
						(hide-all-but (1 2))
						(rewrite "expt_x2")
						(rewrite "sq" :dir rl)
						(assert))
					   ("3" (hide-all-but 1) (grind))))
					 ("2"
					  (hide (2 3))
					  (case "NOT x^2+y^2=0")
					  (("1" (assert))
					   ("2"
						(case "NOT sq(x)+sq(y)=0")
						(("1" (grind))
						 ("2"
						  (lemma "sq_eq_0")
						  (inst-cp - "x")
						  (inst - "y")
						  (ground))))))))))
				 ("2"
				  (flatten)
				  (hide 2)
				  (assert)
				  (replaces -1)
				  (assert)
				  (expand "spherical2xyz")
				  (name "R" "sqrt(x^2+y^2+z^2)")
				  (("1"
					(replace -1)
					(case "R > 0")
					(("1"
					  (case "x/=0 OR y/=0")
					  (("1"
						(hide -4)
						(assert)
						(split +)
						(("1"
						  (rewrite "cos_atan2")
						  (assert)
						  (lift-if)
						  (assert)
						  (split +)
						  (("1" (propax))
						   ("2"
							(flatten)
							(rewrite "sin_acos_ecef")
							(("1"
							  (case "sqrt(1 - sq(z / R)) * (1 / sqrt(1 + sq(y / x))) * R = abs(x)")
							  (("1"
								(split +)
								(("1"
								  (flatten)
								  (expand "abs")
								  (expand "sq")
								  (lift-if)
								  (ground))
								 ("2"
								  (flatten)
								  (expand "abs")
								  (expand "sq")
								  (lift-if)
								  (ground))))
							   ("2"
								(hide 3)
								(reveal "igz")
								(invoke (inst - "%1" "%2") (! 1 1) (! 1 2))
								(assert)
								(hide 2)
								(rewrite "sq_times")
								(rewrite "sq_times")
								(rewrite "sq_div")
								(rewrite "sq_div")
								(("1"
								  (rewrite "sq_div")
								  (rewrite "sq_sqrt")
								  (("1"
									(field)
									(replaces -2 1 :dir rl)
									(rewrite "sq_sqrt")
									(("1" (grind))
									 ("2"
									  (typepred "sq(x)+sq(y)+sq(z)")
									  (grind))))
								   ("2"
									(assert)
									(lemma "nnreal_div_posreal_is_nnreal")
									(inst?)
									(("1" (assert))
									 ("2" (lemma "sq_eq_0") (inst?) (assert))))))
								 ("2"
								  (flatten)
								  (lemma "sqrt_eq_0")
								  (inst - "1+sq(y)/sq(x)")
								  (assert))
								 ("3"
								  (lemma "nnreal_div_posreal_is_nnreal")
								  (inst?)
								  (("1" (assert))
								   ("2" (lemma "sq_eq_0") (inst?) (assert))))))
							   ("3" (assert))))
							 ("2"
							  (rewrite "abs_div")
							  (expand "abs" + 2)
							  (cross-mult 1)
							  (lemma "sq_le")
							  (inst?)
							  (assert)
							  (hide 2)
							  (case "FORALL (rr:real): 1*rr = rr")
							  (("1"
								(rewrite -1)
								(hide -1)
								(replaces -2 :dir rl)
								(rewrite "sq_sqrt")
								(("1" (typepred "sq(x)+sq(y)") (grind))
								 ("2" (typepred "sq(x)+sq(y)+sq(z)") (grind))))
							   ("2" (assert))))))))
						 ("2"
						  (rewrite "sin_acos_ecef")
						  (("1"
							(rewrite "sin_atan2")
							(lift-if)
							(assert)
							(split +)
							(("1"
							  (flatten)
							  (assert)
							  (case "sqrt(1 - sq(z / R)) * R = abs(y)")
							  (("1" (expand "abs") (lift-if) (ground))
							   ("2"
								(hide 2)
								(reveal "igz")
								(invoke (inst - "%1" "%2") (! 1 1) (! 1 2))
								(assert)
								(hide 2)
								(delabel "igz")
								(rewrite "sq_times")
								(rewrite "sq_div")
								(replace -1)
								(assert)
								(field)
								(replaces -4 1 :dir rl)
								(rewrite "sq_sqrt")
								(("1" (assert) (grind))
								 ("2" (typepred "sq(y)+sq(z)") (grind))))))
							 ("2"
							  (flatten)
							  (case "sqrt(1 - sq(z / R)) * (y / abs(x) / sqrt(1 + sq(y / x))) * R = y")
							  (("1"
								(split +)
								(("1" (flatten) (expand "abs") (lift-if) (assert))
								 ("2" (flatten) (expand "abs") (assert))))
							   ("2"
								(hide 3)
								(reveal "igz")
								(case "sqrt(1 - sq(z / R)) * (abs(y) / abs(x) / sqrt(1 + sq(y / x))) * R = abs(y)")
								(("1"
								  (name "d" "abs(y)")
								  (replace -1)
								  (expand "abs" -1)
								  (replaces -1 :dir rl)
								  (lift-if)
								  (ground))
								 ("2"
								  (hide 2)
								  (invoke (inst - "%1" "%2") (! 1 1) (! 1 2))
								  (("1"
									(assert)
									(hide 1)
									(delabel "igz")
									(rewrite "sq_times")
									(rewrite "sq_div")
									(rewrite "sq_times")
									(rewrite "sq_div")
									(rewrite "sq_div")
									(("1"
									  (rewrite "sq_div")
									  (rewrite "sq_sqrt")
									  (rewrite "sq_sqrt")
									  (("1"
										(field)
										(case "sq(R) = sq(x)+sq(y)+sq(z)")
										(("1" (replaces -1) (grind))
										 ("2"
										  (replace -2 1 :dir rl)
										  (rewrite "sq_sqrt")
										  (("1" (grind))
										   ("2"
											(typepred "sq(x)+sq(y)+sq(z)")
											(grind))))))
									   ("2"
										(lemma "nnreal_div_posreal_is_nnreal")
										(inst?)
										(("1" (assert))
										 ("2"
										  (lemma "sq_eq_0")
										  (inst?)
										  (assert))))))
									 ("2"
									  (flatten)
									  (lemma "sqrt_eq_0")
									  (inst - "1+sq(y)/sq(x)")
									  (assert))
									 ("3"
									  (lemma "nnreal_div_posreal_is_nnreal")
									  (inst?)
									  (("1" (assert))
									   ("2"
										(lemma "sq_eq_0")
										(inst?)
										(assert))))))
								   ("2"
									(rewrite "nnreal_times_nnreal_is_nnreal")
									(rewrite "nnreal_times_nnreal_is_nnreal")
									(cross-mult 1))))))
							   ("3" (assert)) ("4" (assert))))))
						   ("2"
							(rewrite "abs_div")
							(cross-mult 1)
							(expand "abs" 1 2)
							(assert)
							(lemma "sq_le")
							(inst?)
							(assert)
							(replace -3 1 :dir rl)
							(rewrite "sq_sqrt")
							(("1" (typepred "sq(x)+sq(y)") (grind))
							 ("2" (typepred "sq(x)+sq(y)+sq(z)") (grind))))))
						 ("3" (rewrite "cos_acos") (assert))))
					   ("2"
						(flatten)
						(assert)
						(replace -1)
						(replace -2)
						(assert))))
					 ("2"
					  (assert)
					  (lemma "sqrt_pos")
					  (assert)
					  (lemma "sq_eq_0")
					  (typepred "sq(x)")
					  (typepred "sq(y)")
					  (typepred "sq(z)")
					  (split -)
					  (("1" (flatten) (inst - "x") (grind))
					   ("2" (inst - "y") (grind)) ("3" (inst - "z") (grind))))))
				   ("2" (hide 3) (typepred "sq(x)+sq(y)+sq(z)") (grind))))))))
			 ("2" (hide 2) (skeep) (both-sides-f -1 "sqrt") (replaces -1))))`
		];
		// tactics.forEach(tactic => {
		// 	const res = PvsProcess.prf2json(tactic, "test");
		// 	console.log(res);
		// });
		return tactics;
	}



}