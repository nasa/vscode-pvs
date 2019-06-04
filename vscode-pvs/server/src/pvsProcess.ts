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
import * as language from "./common/languageKeywords";
// note: ./common is a symbolic link. if vscode does not find it, try to restart TS server: CTRL + SHIFT + P to show command palette, and then search for Typescript: Restart TS Server
import { 
	PvsResponseType, PvsParserResponse, PvsDeclarationDescriptor,
	PRELUDE_FILE, PvsDeclarationType, FormulaDescriptor, ProofResult, PrettyPrintRegionRequest,
	PrettyPrintRegionResult, ExpressionDescriptor, EvaluationResult, PvsListDeclarationsRequest,
	PvsFindDeclarationRequest, PvsDefinition,
	TccDescriptorArray, TccDescriptor, PvsFileListDescriptor, PvsTypecheckerResponse,
	PvsExecutionContext, SimpleConnection, TheoryList, FileList, TheoremsMap, TheoryMap, TheoryStatus
} from './common/serverInterface'
import { Connection, TextDocument } from 'vscode-languageserver';
import * as path from 'path';
import { PVS_TRUE_FALSE_REGEXP_SOURCE, PVS_STRING_REGEXP_SOURCE } from "./common/languageKeywords";
import * as fsUtils from './common/fsUtils';
import { PvsFindDeclarationInterface, PvsLispReader } from './pvsLisp';
import * as utils from './common/languageUtils';
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

	private connection: SimpleConnection;
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

	// public setConnection (connection: SimpleConnection) {
	// 	this.connection = connection;
	// }

	// public removeConnection () {
	// 	this.connection = null;
	// }

	private pvsExecAux(cmd: string): Promise<PvsResponseType> {
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
		if (this.connection) { this.connection.console.error(`Unrecognised command ${cmd}`); }
		Promise.reject({
			res: null,
			error: `Unrecognised command ${cmd}`,
			raw: null
		});
	}

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


	startCli(listener: (data: string) => void) {
		this.pvsProcess.stdout.on("data", listener);
	}

	endCli(listener: (data: string) => void) {
		this.pvsProcess.stdout.removeListener("data", listener);
	}

	execCmd(cmd: string): void {
		this.pvsProcess.stdin.write(cmd);
	}
	

	/**
	 * Executes a pvs lisp command using the pvsio process
	 * @param commandId Command name (e.g,. parse-file), see list of commands in PvsLisp
	 * @param cmd The pvs lisp command, e.g., (parse-file "main" nil nil)
	 */
	private pvsioExec(cmd: string): Promise<PvsResponseType> {
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
			const msg: string = `PVSio busy, cannot execute ${cmd} :/`;
			return this.cannotExecute(msg);
		}	
		this.pvsioProcessBusy = true;
		const pvsLispReader: PvsLispReader = new PvsLispReader(this.connection);
		// const pvslispParser = new PvsLisp(commandId, this.connection);
		// if (this.connection) { this.connection.console.log(cmd); }
		return new Promise((resolve, reject) => {
			const listener = (data: string) => {
				if (this.connection) { this.connection.console.log(data); }// this is the crude pvs lisp output, useful for debugging
				pvsLispReader.read(data, async (res: string) => {
					this.pvsProcess.stdout.removeListener("data", listener); // remove listener otherwise this will capture the output of other commands
					this.pvsioProcessBusy = false;
					resolve(getResult(res));
				});
			};
			this.pvsioProcess.stdout.on("data", listener);
			this.pvsioProcess.stdin.write(cmd + "\n");
		});
	}


	async pvs(opt?: {
		enableNotifications?: boolean
	}): Promise<boolean> {
		opt = opt || {};
		this.enableNotifications = opt.enableNotifications;
		await this.clearContext();
		if (!this.pvsProcessBusy) {
			this.pvsProcessBusy = true;
			// const pvslispParser = new PvsLisp("pvs-init", this.connection);
			const pvsLispReader = new PvsLispReader(this.connection);
			let pvs: string = path.join(this.pvsPath, "pvs");
			const args: string[] = [ "-raw"];//, "-port", "22334" ];
			if (this.connection) { this.connection.console.info(`Spawning pvs process ${pvs} ${args.join(" ")}`); }
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

	/**
	 * Starts the pvsio process
	 */
	private async pvsio (): Promise<{}> {
		if (!this.pvsioProcessBusy) {
			this.pvsioProcessBusy = true;
			// const pvslispParser = new PvsLisp("pvs-init", this.connection);	
			const pvsLispReader = new PvsLispReader(this.connection);
			let cmd: string = path.join(this.pvsPath, "pvs");
			if (this.connection) { this.connection.console.info(`Spawning PVSio process ${cmd}`); }
			return new Promise((resolve, reject) => {
				this.pvsioProcess = spawn(cmd, ["-raw"]);
				this.pvsioProcess.stdout.setEncoding("utf8");
				this.pvsioProcess.stderr.setEncoding("utf8");
				const listener = (data: string) => {
					if (this.connection) { this.connection.console.log(data); } // this is the crude pvs lisp output, useful for debugging
					pvsLispReader.read(data, (res: string) => {
						// connection.console.info(res);
						this.pvsioProcess.stdout.removeListener("data", listener); // remove listener otherwise this will capture the output of other commands
						this.pvsioProcessBusy = false;
						resolve();
					});
				};
				this.pvsioProcess.stdout.on("data", listener);
				this.pvsioProcess.stderr.on("data", function (data: string) {
					if (this.connection) { this.connection.console.log(data); }
				});
			});
		}
	}

	/**
	 * Changes the current context. When the context is changed, all symbol information are erased and the parser/typechecker needs to be re-run.
	 * @param contextFolder Path of the context folder 
	 */
	async changeContext(contextFolder: string): Promise<PvsResponseType> {
		// await this.serverProxy.changeContext(contextFolder);
		const folderExists: boolean = await fsUtils.dirExists(contextFolder);
		if (folderExists) {
			if (contextFolder !== this.pvsContextFolder) {
				const cmd: string = `(change-context "${contextFolder}" nil)`;
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
	// /**
	//  * FIXME: remove this command in favour of listPvsFiles?
	//  * Identifies the set of theories loaded in the current context
	//  * FIXME: it is not clear when theories are loaded in the context? how to find the importchain??
	//  * @returns A descriptor providing the list of theories in the current context.
	//  * 			The descriptor contains two fields:
	//  * 			- files: list of theories grouped by filename (type: { [fileName: string]: string })
	//  * 			- theories: list of theories ordered by name (type: string[] )
	//  */
	// private async listTheories(): Promise<PvsResponseType> {
	// 	const cmd: string = '(context-files-and-theories "'+ this.pvsContextFolder +'")';
	// 	return await this.pvsExec(cmd);
	// }
	/**
	 * Returns the pvs files in the current context, i.e., all pvs files in the current context folder
	 * @returns A descriptor providing the list of pvs files and the name of the context folder
	 */
	// async listPvsFiles(): Promise<PvsFileListDescriptor> {
	// 	let files: string[] = await fsUtils.readDir(this.pvsContextFolder);
	// 	let pvsFiles: string[] = files.filter(fileName => {
	// 		return fileName.endsWith(".pvs") && !fileName.startsWith(".");
	// 	});
	// 	return {
	// 		fileNames: pvsFiles,
	// 		folder: this.pvsContextFolder
	// 	};
	// }
	/**
	 * Disables garbage collector messages
	 */
	async disableGcPrintout(): Promise<PvsResponseType> {
		return await this.pvsExec('(setq *disable-gc-printout* t)');
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
	 * Returns the list of commands accepted by the theorem prover
	 */
	async listProofStrategies(): Promise<PvsResponseType> {
		return await this.pvsExec('(collect-strategy-names)');
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
		if (!this.pvsContextFolder.startsWith(this.pvsPath)) {
			const parserInfo: PvsResponseType = await this.pvsExec(`(parse-file "${fileName}" nil nil)`);
			if (parserInfo.error) {
				response.error = parserInfo.error.parserError;
			} else {
				response.res = parserInfo.res
			}
		} else {
			if (this.connection) {
				this.connection.console.info(`PVS library file ${fileName} already parsed.`);
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
		const contextFolder: string = this.getContextFolder();
		const contextFiles: FileList = await fsUtils.listPvsFiles(contextFolder);
		if (contextFiles && contextFiles.fileNames) {
			for (const i in contextFiles.fileNames) {
				result[contextFiles.fileNames[i]] = await this.parseFile(contextFiles.fileNames[i]);
			}
		}
		return result;
	}
	/**
	 * Shows the Type Check Conditions (TCCs) for the selected theory.
	 * This command triggers typechecking and creates a .tccs file on disk.
	 * The .tccs file name is the name of the selected theory.
	 * @returns TODO: change return type to an array of TCC descriptors
	 */
	async showTccs (fileName: string, theoryName: string): Promise<PvsResponseType> {	
		const ans: PvsResponseType = await this.pvsExec(`(show-tccs "${theoryName}" nil)`);
		// const res = await this.serverProxy.changeContext(this.pvsContextFolder);
		// const res = await this.serverProxy.typecheck(fileName);
		// const res = await this.serverProxy.lisp(cmd);
		// create a new file with the tccs. The file name corresponds to the theory name.
		if (ans && ans.res) {
			let tccsFileContent: string = ans.raw;
			const fileName: string = path.join(this.pvsContextFolder, `${theoryName}.tccs`)
			await fsUtils.writeFile(fileName, tccsFileContent);
		}
		return ans;
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
		cmd = `(change-context "${this.pvsContextFolder}" t)`;
		await this.pvsioExec("change-context");
		// typecheck
		let fileName = fsUtils.getFilename(desc.fileName, { removeFileExtension: true });
		cmd = `(typecheck-file "${fileName}" nil nil nil)`;
		await this.pvsioExec("typecheck-file");
		// load semantic attachments
		cmd = "(load-pvs-attachments)";
		await this.pvsioExec("load-pvs-attachments");
		// enter pvsio mode
		cmd = `(evaluation-mode-pvsio "${desc.theoryName}" nil nil nil)`; // the fourth argument removes the pvsio 	banner
		await this.pvsioExec("evaluation-mode-pvsio");
		// send expression to be evaluated
		cmd = `${desc.expression};`;
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
	 * Typechecks a file
	 * @param uri The uri of the file to be typechecked
	 * @param attemptProof Tries to discharge all tccs (default is no)
	 */
	async typecheckFile (uri: string, attemptProof?: boolean): Promise<PvsTypecheckerResponse> {
		const fileName: string = fsUtils.getFilename(uri, { removeFileExtension: true });
		const filePath: string = fsUtils.getPathname(uri);
		let response: PvsTypecheckerResponse = {
			fileName: fileName,
			res: null,
			error: null
		};
		if (filePath !== this.pvsPath && filePath !== path.join(this.pvsPath, "lib")) {
			const info: PvsResponseType = (attemptProof) ? 
				await this.pvsExec(`(typecheck-file "${fileName}" nil t nil)`)
				: await this.pvsExec(`(typecheck-file "${fileName}" nil nil nil)`);
			if (info && info.error) {
				response.error = info.error.parserError;
			} else {
				// load status info for each theory
				const theoryMap: TheoryMap = await utils.listTheoriesInFile(uri);
				if (theoryMap) {
					response.res = {};
					const theoryNames: string[] = Object.keys(theoryMap);
					for (const i in theoryNames) {
						const theoryName: string = theoryNames[i];
						const statusInfo: PvsResponseType = await this.pvsExec(`(prove-tccs-theory "${theoryName}" nil "${fileName}" nil)`);
						if (statusInfo && statusInfo.res) {
							const theoryStatus: TheoryStatus = statusInfo.res;
							response.res[theoryName] = theoryStatus;
						}							
					}
				}
				await this.saveContext();
			}
		} else {
			if (this.connection) {
				this.connection.console.info(`PVS library file ${fileName} already typechecked.`);
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

	static prf2json(prf: string, formulaName: string, parent?: { id: string, children: any[], type: string }): { [key: string]: any } {
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
						children: [],
						type: "root"
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
							const currentParent: { id: string, children: any[], type: string } = parent.children[parent.children.length - 1];
							PvsProcess.prf2json(subexpr, formulaName, currentParent);
						} else if (expr.startsWith(`("`)) {
							// proof command from a labelled branch -- remove the label and iterate
							const match: RegExpMatchArray = /\(\"(\d+)\"\s*([\w\W\s]+)/.exec(expr);
							const subexpr: string = match[2].replace(/\n/g, ""); // remove all \n introduced by pvs in the expression
							const currentBranch: { id: string, children: any[], type: string } = { id: match[1], children:[], type: "proof-branch" };
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