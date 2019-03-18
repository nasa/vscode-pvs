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
import { 
	PvsResponseType, PvsParserResponse, getFilename, PvsDeclarationDescriptor, getPathname,
	PRELUDE_FILE, PvsDeclarationType, FormulaDescriptor, ProofResult, PrettyPrintRegionRequest,
	PrettyPrintRegionResult, ExpressionDescriptor, EvaluationResult, PvsListDeclarationsRequest,
	PvsFindDeclarationRequest, PvsFindDeclarationResponse, PvsTheoryListDescriptor,
	TccDescriptorArray, TccDescriptor, PvsFileListDescriptor, PvsTypecheckerResponse
} from './common/serverInterface'
import { Connection, TextDocument } from 'vscode-languageserver';
import * as path from 'path';
import { PVS_TRUE_FALSE_REGEXP_SOURCE, PVS_STRING_REGEXP_SOURCE } from "./common/languageKeywords";
import * as fs from './common/fsUtils';

/**
 * Wrapper class for PVS: spawns a PVS process, and exposes the PVS Lisp interface as an asyncronous JSON/RPC server.
 */
export class PvsProcess {
	private pvsProcess: ChildProcess = null;
	private pvsioProcess: ChildProcess = null;
	private proverProcess: ChildProcess = null;
	private pvsProcessBusy: boolean = false;
	private pvsioProcessBusy: boolean = false;
	private proverProcessBusy: boolean = false;

	private pvsCmdQueue: Promise<PvsResponseType> = Promise.resolve({ res: null, error: null });

	private pvsPath: string = null;
	private pvsContextPath: string = null;
	private pvsServerPath: string = null;

	private connection: Connection;

	/**
	 * @returns The current pvs context path
	 */
	getContextPath(): string {
		return this.pvsContextPath;
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
	constructor (pvsExecutionContext: PvsExecutionContext, connection: Connection) {
		this.pvsPath = pvsExecutionContext.pvsPath || __dirname;
		this.pvsContextPath = pvsExecutionContext.pvsContextPath || __dirname;
		this.pvsServerPath = pvsExecutionContext.pvsServerPath || __dirname;
		this.connection = connection;
	}
	/**
	 * Internal function, used to communicate that the process is busy and cmd cannot be executed
	 * @param cmd 
	 */
	private cannotExecute (msg: string): Promise<PvsResponseType> {
		this.connection.console.error(msg)
		return Promise.resolve({
			error: {
				msg: msg,
				parserError: null,
				restartOption: null
			},
			res: null
		});
	}
	/**
	 * Executes a pvs lisp command using the pvs process
	 * @param commandId Command name (e.g,. parse-file), see list of commands in PvsLisp
	 * @param cmd The pvs lisp command, e.g., (parse-file "main" nil nil)
	 */
	private pvsExecAux(commandId: string, cmd: string): Promise<PvsResponseType> {
		const _this = this;
		// utility function, automatically responds to lisp interactive commands, such as when pvs crashes into lisp
		async function getResult(pvsLispResponse: string): Promise<PvsResponseType> {
			const ans: PvsResponseType = JSON.parse(pvsLispResponse);
			if (ans.error) {
				let option: number = +ans.error.restartOption;
				// NB: do not
				_this.continueLisp(option);
			}
			return ans; 
		}
		if (this.pvsProcessBusy) {
			const msg: string = "PVS busy, cannot execute " + cmd + " :/";
			return this.cannotExecute(msg);
		}	
		this.pvsProcessBusy = true;
		const pvslispParser = new PvsLisp(commandId, this.connection);
		// connection.console.info("Executing command " + cmd);
		if (this.connection) { this.connection.console.log(cmd); }
		return new Promise(async function (resolve, reject) {
			const listener = function (data: string) {
				if (_this.connection) { _this.connection.console.log(data); }// this is the crude pvs lisp output, useful for debugging
				pvslispParser.parse(data, async function (res: string) {
					_this.pvsProcess.stdout.removeListener("data", listener); // remove listener otherwise this will capture the output of other commands
					const ans: PvsResponseType = await getResult(res);
					_this.pvsProcessBusy = false;
					resolve(ans);
				});
			};
			_this.pvsProcess.stdout.on("data", listener);
			_this.pvsProcess.stdin.write(cmd + "\n");
		});
	}
	private pvsExec(commandId: string, cmd: string): Promise<PvsResponseType> {
		this.pvsCmdQueue = new Promise((resolve, reject) => {
			this.pvsCmdQueue.then(() => {
				this.pvsExecAux(commandId, cmd).then((ans: PvsResponseType) => {
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
	private pvsioExec(commandId: string, cmd: string): Promise<PvsResponseType> {
		const _this = this;
		// utility function, automatically responds to lisp interactive commands, such as when pvs crashes into lisp
		async function getResult(pvsLispResponse: string): Promise<PvsResponseType> {
			const ans: PvsResponseType = JSON.parse(pvsLispResponse);
			if (ans.error) {
				let option: number = +ans.error.restartOption;
				_this.continueLisp(option);
			}
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
		const pvslispParser = new PvsLisp(commandId, this.connection);
		// connection.console.info("Executing command " + cmd);
		if (this.connection) { this.connection.console.log(cmd); }
		return new Promise(async function (resolve, reject) {
			let listener = function (data: string) {
				if (_this.connection) { _this.connection.console.log(data); }// this is the crude pvs lisp output, useful for debugging
				pvslispParser.parse(data, async function (res: string) {
					_this.pvsioProcess.stdout.removeListener("data", listener); // remove listener otherwise this will capture the output of other commands
					_this.pvsioProcessBusy = false;
					resolve(await getResult(res));
				});
			};
			_this.pvsioProcess.stdout.on("data", listener);
			_this.pvsioProcess.stdin.write(cmd + "\n");
		});
	}
	/**
	 * Executes a pvs lisp command using the prover process
	 * @param commandId Command name (e.g,. parse-file), see list of commands in PvsLisp
	 * @param cmd The pvs lisp command, e.g., (parse-file "main" nil nil)
	 */
	private proverExec(commandId: string, cmd: string): Promise<PvsResponseType> {
		const _this = this;
		// utility function, automatically responds to lisp interactive commands, such as when pvs crashes into lisp
		async function getResult(pvsLispResponse: string): Promise<PvsResponseType> {
			const ans: PvsResponseType = JSON.parse(pvsLispResponse);
			if (ans.error) {
				let option: number = +ans.error.restartOption;
				_this.continueLisp(option);
			}
			return ans; 
		}
		if (this.proverProcessBusy) {
			const msg: string = "Prover busy, cannot execute " + cmd + ":/";
			return this.cannotExecute(msg);
		}	
		this.proverProcessBusy = true;
		const pvslispParser = new PvsLisp(commandId, this.connection);
		// connection.console.info("Executing command " + cmd);
		if (this.connection) { this.connection.console.log(cmd); }
		return new Promise(async (resolve, reject) => {
			let listener = function (data: string) {
				if (_this.connection) { _this.connection.console.log(data); }// this is the crude pvs lisp output, useful for debugging
				pvslispParser.parse(data, async (res: string) => {
					_this.proverProcess.stdout.removeListener("data", listener); // remove listener otherwise this will capture the output of other commands
					_this.proverProcessBusy = false;
					resolve(await getResult(res));
				});
			};
			_this.proverProcess.stdout.on("data", listener);
			_this.proverProcess.stdin.write(cmd + "\n");
		});
	}
	/**
	 * Starts the pvs process
	 */
	async pvs (): Promise<{}> {
		if (!this.pvsProcessBusy) {
			this.pvsProcessBusy = true;
			const pvslispParser = new PvsLisp("pvs-init", this.connection);	
			let cmd: string = path.join(this.pvsPath, "pvs");
			if (this.connection) { this.connection.console.info("Spawning pvs process " + cmd); }
			return new Promise((resolve, reject) => {
				this.pvsProcess = spawn(cmd, ["-raw"]);
				this.pvsProcess.stdout.setEncoding("utf8");
				this.pvsProcess.stderr.setEncoding("utf8");
				const _this = this;
				let listener = function (data: string) {
					if (_this.connection) { _this.connection.console.log(data); } // this is the crude pvs lisp output, useful for debugging
					pvslispParser.parse(data, (res: string) => {
						// connection.console.info(res);
						_this.pvsProcess.stdout.removeListener("data", listener); // remove listener otherwise this will capture the output of other commands
						_this.pvsProcessBusy = false;
						resolve();
					});
				};
				this.pvsProcess.stdout.on("data", listener);
				this.pvsProcess.stderr.on("data", (data: string) => {
					if (this.connection) { this.connection.console.log(data); }
				});
				if (this.connection) { this.connection.console.info("PVS process ready!"); }
			});
		}
	}
	/**
	 * Starts the pvsio process
	 */
	private async pvsio (): Promise<{}> {
		if (!this.pvsioProcessBusy) {
			this.pvsioProcessBusy = true;
			const pvslispParser = new PvsLisp("pvs-init", this.connection);	
			let cmd: string = path.join(this.pvsPath, "pvs");
			if (this.connection) { this.connection.console.info("Spawning pvsio process " + cmd); }
			const _this = this;
			return new Promise(function (resolve, reject) {
				_this.pvsioProcess = spawn(cmd, ["-raw"]);
				_this.pvsioProcess.stdout.setEncoding("utf8");
				_this.pvsioProcess.stderr.setEncoding("utf8");
				let listener = function (data: string) {
					if (this.connection) { this.connection.console.log(data); } // this is the crude pvs lisp output, useful for debugging
					pvslispParser.parse(data, function (res: string) {
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
	/**
	 * Starts the prover process
	 */
	private async prover (): Promise<{}> {
		if (!this.proverProcessBusy) {
			this.proverProcessBusy = true;
			const pvslispParser = new PvsLisp("pvs-init", this.connection);	
			let cmd: string = path.join(this.pvsPath, "pvs");
			if (this.connection) { this.connection.console.info("Spawning prover process " + cmd); }
			const _this = this;
			return new Promise(function (resolve, reject) {
				_this.proverProcess = spawn(cmd, ["-raw"]);
				_this.proverProcess.stdout.setEncoding("utf8");
				_this.proverProcess.stderr.setEncoding("utf8");
				let listener = function (data: string) {
					if (this.connection) { this.connection.console.log(data); } // this is the crude pvs lisp output, useful for debugging
					pvslispParser.parse(data, function (res: string) {
						// connection.console.info(res);
						_this.proverProcess.stdout.removeListener("data", listener); // remove listener otherwise this will capture the output of other commands
						_this.proverProcessBusy = false;
						resolve();
					});
				};
				_this.proverProcess.stdout.on("data", listener);
				_this.proverProcess.stderr.on("data", function (data: string) {
					if (_this.connection) { _this.connection.console.log(data); }
				});
			});
		}
	}

	/**
	 * Changes the current context. When the context is changed, all symbol information are erased and the parser/typechecker needs to be re-run.
	 * @param contextPath Path of the new context 
	 */
	async changeContext(contextPath: string): Promise<PvsResponseType> {
		this.pvsContextPath = contextPath;
		const cmd: string = '(change-context "' + contextPath + '" t)';
		return await this.pvsExec("change-context", cmd);
	}
	/**
	 * Returns the current context
	 * @returns Path of the current context
	 */
	async currentContext(): Promise<string> {
		const cmd: string = '(pvs-current-directory)';
		return (await this.pvsExec("current-context", cmd)).res;
	}
	/**
	 * FIXME: remove this command, it's useless, it has been re-implemented on the languageserver.
	 * Identifies the set of theories loaded in the current context
	 * FIXME: it is not clear when theories are loaded in the context? how to find the importchain??
	 * @returns A descriptor providing the list of theories in the current context.
	 * 			The descriptor contains two fields:
	 * 			- files: list of theories grouped by filename (type: { [fileName: string]: string })
	 * 			- theories: list of theories ordered by name (type: string[] )
	 */
	private async listTheories(): Promise<PvsTheoryListDescriptor> {
		const cmd: string = '(context-files-and-theories "'+ this.pvsContextPath +'")';
		return (await this.pvsExec("list-theories", cmd)).res;
	}


	/**
	 * Returns the pvs files in the current context, i.e., all pvs files in the current context folder
	 * @returns A descriptor providing the list of pvs files and the name of the context folder
	 */
	async listPvsFiles(): Promise<PvsFileListDescriptor> {
		let files: string[] = await fs.readDir(this.pvsContextPath);
		let pvsFiles: string[] = files.filter(function (fileName) {
			return fileName.endsWith(".pvs");
		});
		return {
			fileNames: pvsFiles,
			folder: this.pvsContextPath
		};
	}
	/**
	 * Utility function, restores the pvs prompt if pvs crashes into lisp
	 * @param option The restart option for restoring the pvs prompt
	 * @private 
	 */
	private async continueLisp(option: number): Promise<PvsResponseType> {
		const cmd = ':continue ' + option + "\n";
		return this.pvsExec("pvs-continue", cmd);
	}
	/**
	 * Disables garbage collector messages
	 */
	async disableGcPrintout(): Promise<PvsResponseType> {
		const cmd: string = '(setq *disable-gc-printout* t)';
		return await this.pvsExec("disable-gc-printout", cmd);
	}
	/**
	 * Enables the pvs emacs interface.
	 * This is used to overcome a limitation of the raw mode, which does not provide detailed info for errors.

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
		return await this.pvsExec("emacs-interface", cmd);
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
				error: null
			});
		}
		const cmd: string = '(find-declaration "' + symbolName + '")';
		return await this.pvsExec("find-declaration", cmd);
	}
	// /**
	//	* OBSOLETE -- reimplemented in providers/pvsDefinitionProvider
	//  * Finds the definition of a symbol
	//  * @param desc symbol name, and, optionally, symbol position and file where the symbol is defined. The optional fields are used to narrow down the list of definitions (symbols can be overloaded).
	//  * @returns Promise<PvsFindDeclarationResponse> A symbol descriptor. 
	//  * 			Attributes symbolDeclaration, symbolRange and symbolFile are non-null only if a unique declaration is identified.  
	//  */
	// async findDefinition (desc: PvsFindDeclarationRequest): Promise<PvsFindDeclarationResponse> {
	// 	let response: PvsFindDeclarationResponse = {
	// 		file: desc.file,
	// 		theory: desc.theory,
	// 		line: desc.line,
	// 		character: desc.character,
	// 		symbolName: desc.symbolName,
	// 		symbolTheory: null,
	// 		symbolDeclaration: null,
	// 		symbolDeclarationRange: null,
	// 		symbolDeclarationFile: null,
	// 		symbolDoc: null,
	// 		comment: null,
	// 		error: {
	// 			msg: null,
	// 			fileName: null,
	// 			line: null,
	// 			character: null
	// 		}
	// 	};
	// 	let currentTheory: string = desc.theory;
	// 	let isNumber: boolean = new RegExp(language.PVS_NUMBER_REGEXP_SOURCE).test(desc.symbolName);
	// 	let isString: boolean = new RegExp(language.PVS_STRING_REGEXP_SOURCE).test(desc.symbolName);
	// 	if (isNumber) {
	// 		this.connection.console.info("Number " + desc.symbolName);
	// 	} else if (isString) {
	// 		this.connection.console.info("String " + desc.symbolName);
	// 	}
	// 	const path = desc.file.trim().split("/");
	// 	const fileName = path[path.length - 1].split(".pvs")[0];
	// 	if (fileName !== PRELUDE_FILE) {
	// 		// find-declaration works even if a pvs file does not parse correctly 
	// 		let ans: PvsResponseType = await this.findDeclaration(desc.symbolName);
	// 		const allDeclarations: PvsFindDeclarationInterface = ans.res;
	// 		// first, check if the declaration is in the current theory
	// 		if (currentTheory && allDeclarations[currentTheory + "." + desc.symbolName]) {
	// 			response = allDeclarations[currentTheory + "." + desc.symbolName];
	// 		} else {
	// 			// otherwise check the importchain
	// 			const candidates: PvsDeclarationDescriptor[] = Object.keys(allDeclarations).map(function (key) {
	// 				const info: PvsDeclarationType = allDeclarations[key];
	// 				const ans: PvsDeclarationDescriptor = {
	// 					line: desc.line,
	// 					theory: desc.theory,
	// 					character: desc.character,
	// 					file: desc.file,
	// 					symbolName: desc.symbolName,
	// 					symbolTheory: info.symbolTheory,
	// 					symbolDeclaration: (info) ? info.symbolDeclaration : null,
	// 					symbolDeclarationRange: (info) ? info.symbolDeclarationRange : null,
	// 					symbolDeclarationFile: (info) ? info.symbolDeclarationFile : null,
	// 					symbolDoc: null,
	// 					comment: null,
	// 					error: null
	// 				}
	// 				return ans;
	// 			});
	// 			const filteredResults: PvsDeclarationDescriptor[] = candidates.filter(function (desc) {
	// 				return !(desc.symbolDeclarationFile == PRELUDE_FILE && 
	// 							/^\w+\s*:\s*VAR\s+\w+/gi.test(desc.symbolDeclaration)); // VAR declarations from the prelude
	// 			});
	// 			const withoutObsoletePrelude: PvsDeclarationDescriptor[] = filteredResults.filter(function (desc) {
	// 				return !(desc.symbolDeclarationFile == PRELUDE_FILE && 
	// 							new RegExp(language.PVS_PRELUDE_OBSOLETE_THEORIES_REGEXP_SOURCE, "g").test(desc.symbolTheory));
	// 			});
	// 			let fromImportChain: PvsDeclarationDescriptor[] = [];
	// 			const resp: PvsResponseType = await this.showImportChain(desc.theory);
	// 			const importChain: PvsShowImportChain = resp.res;
	// 			if (importChain.theories.length > 0) {
	// 				fromImportChain = filteredResults.filter(function (desc) {
	// 					return importChain.theories.indexOf(desc.symbolTheory) >= 0;
	// 				});
	// 			}
	// 			// send response back to the client
	// 			if (candidates.length === 0) {
	// 				this.connection.console.warn("Could not find declaration :/");
	// 			} else if (candidates.length === 1) {
	// 				response = candidates[0];
	// 			} else if (filteredResults.length === 1) {
	// 				response = filteredResults[0];
	// 			} else if (fromImportChain.length >= 1) {
	// 				response = fromImportChain[0];
	// 			} else if (withoutObsoletePrelude.length === 1) {
	// 				response = withoutObsoletePrelude[0];
	// 			} else {
	// 				this.connection.console.warn("Found several candidate declarations :/");
	// 			}
	// 		}
	// 	}
	// 	return response;
	// }
	/**
	 * List all declaration in a given theory. Requires parsing.
	 * @param theoryName Name of the theory 
	 */
	private async _listDeclarations(theoryName: string): Promise<PvsResponseType> {
		const cmd: string = '(list-declarations "' + theoryName + '")';
		return await this.pvsExec("list-declarations", cmd);
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
	/**
	 * Parse the expression passed as argument
	 * @param expression The expression to be parsed
	 */
	async parseExpression(expression: string): Promise<PvsResponseType> {
		const cmd: string = '(describe (pc-parse "' + expression + '" `expr))';
		return await this.pvsExec("parse-expression", cmd);
	}
	/**
	 * Parse a file. This is the original API provided by PVS Lisp.
	 * @param fileName File to be parsed, must be in the current pvs context
	 * @private
	 */
	private async _parseFile(fileName: string): Promise<PvsResponseType> {
		const cmd: string = '(parse-file "' + fileName + '" nil nil)'; // is there a way to force parsing of importchain?
		return await this.pvsExec("parse-file", cmd);
	}
	/**
	 * Parse a file
	 * @param uri File to be parsed, must be in the current pvs context
	 * @returns Parser result, can be either a message (parse successful), or list of syntax errors
	 */
	async parseFile (uri: string): Promise<PvsParserResponse> {
		const fileName: string = getFilename(uri, { removeFileExtension: true });
		const filePath: string = getPathname(uri);
		let response: PvsParserResponse = {
			fileName: fileName,
			res: null,
			error: null
		};
		if (filePath !== path.join(this.pvsPath, "lib")) {
			// await _this.pvsProcess.changeContext(filePath);
			const parserInfo: PvsResponseType = await this._parseFile(fileName);
			if (parserInfo.error) {
				response.error = parserInfo.error.parserError;
			} else {
				response.res = parserInfo.res
			}
		} else {
			this.connection.console.info("PVS library file " + fileName + " already parsed.");
		}
		return response;
	}

	/**
	 * Parse all files in the current context
	 * @returns Parser result for each file, can be either a message (parse successful), or list of syntax errors
	 */
	async parseAll (): Promise<{ [fileName: string]: PvsParserResponse }> {
		let result: { [ fileName: string ] : PvsParserResponse } = {};
		let contextFiles: PvsFileListDescriptor = await this.listPvsFiles();
		if (contextFiles && contextFiles.fileNames) {
			for (let i in contextFiles.fileNames) {
				result[contextFiles.fileNames[i]] = await this.parseFile(contextFiles.fileNames[i]);
			}
		}
		return result;
	}

	/**
	 * Creates the prover process, if the process has not been created already.
	 */
	private async initTypeChecker () {
		if (this.proverProcess === null) {
			// start prover process
			await this.prover();
			let cmd: string = '(setq *disable-gc-printout* t)';
			// disable garbage collector printout
			await this.proverExec("disable-gc-printout", cmd);
		}
	} 

	/**
	 * Shows the Type Check Conditions (TCCs) for the selected theory. This command triggers typechecking.
	 * @returns An array of TCC descriptors
	 */
	async showTccs (theoryName: string): Promise<TccDescriptorArray> {
		const cmd: string = '(show-tccs "' + theoryName + '" nil)';
		await this.initTypeChecker();
		const tccs: TccDescriptor[] = (await this.proverExec("show-tccs", cmd)).res;
		const importChain: PvsTheoryListDescriptor = await this.listTheories();
		const fileName: string = importChain.theories[theoryName][0]; // FIXME: can we have the same theory defined in different files? I don't think so
		return Promise.resolve({
			theoryName: theoryName,
			fileName: fileName,
			tccs: tccs
		});
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
		await this.pvsioExec("disable-gc-printout", cmd);
		// // enable emacs interface
		// cmd = '(setq *pvs-emacs-interface* t)';
		// await this.pvsioExec("emacs-interface", cmd);
		// make sure we are in the correct context
		cmd = '(change-context "' + this.pvsContextPath + '" t)';
		await this.pvsioExec("change-context", cmd);
		// typecheck
		let fileName = getFilename(desc.fileName, { removeFileExtension: true });
		cmd = '(typecheck-file "' + fileName + '" nil nil nil)';
		await this.pvsioExec("typecheck-file", cmd);
		// load semantic attachments
		cmd = "(load-pvs-attachments)";
		await this.pvsioExec("load-pvs-attachments", cmd);
		// enter pvsio mode
		cmd = '(evaluation-mode-pvsio "' + desc.theoryName + '" nil nil nil)'; // the fourth argument removes the pvsio 	banner
		await this.pvsioExec("evaluation-mode-pvsio", cmd);
		// send expression to be evaluated
		cmd = desc.expression + ";";
		let ans = await this.pvsioExec("eval-expr", cmd);
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
	 * Proves a formula
	 * @param desc Formula descriptor
	 */
	async proveit (desc: FormulaDescriptor): Promise<ProofResult> {
		await this.initTypeChecker();
		let cmd = '(change-context "' + this.pvsContextPath + '" t)';
		await this.proverExec("change-context", cmd);
		// typecheck
		let fileName = getFilename(desc.fileName, { removeFileExtension: true });
		cmd = '(typecheck-file "' + fileName + '" nil nil nil)';
		await this.proverExec("typecheck-file", cmd);

		// // enable proof editing
		// cmd = '(edit-proof-at "' + fileName + '" nil ' + desc.line + ' "pvs" "' + fileName + '.pvs" 0 nil)';
		// await this.proverExec("edit-proof-at", cmd);
		// // edit proof
		// const strategyFolder: string = path.join(this.pvsServerPath, "strategies");
		// const grindStrategy: string = path.join(strategyFolder, "grind.lisp");
		// cmd = '(install-proof `' + grindStrategy + ' "' + fileName + '" "' + desc.formula + '" 1 t "Proof" 0)';
		// await this.proverExec("install-proof", cmd);
		
		cmd = '(json-prove-formula "' + desc.theoryName + '" "'+ desc.formula +'")';
		let ans: string = (await this.proverExec("json-prove-formula", cmd)).res;
		// (install-prooflite-scripts "test" "test" 0 t)
		//'(install-prooflite-scripts "' + desc.fileName + '" "' + desc.theoryName +  '" ' + desc.line + ' t)';
		return {
			fileName: desc.fileName,
			theoryName: desc.theoryName,
			msg: "%-- proof state for " + desc.formula,
			result: ans
		};
	}	
	/**
	 * Shows the declaration of a symbol. Requires typechecking.
	 * @param fileName 
	 * @param line 
	 * @param character 
	 */
	async showDeclaration(fileName: string, line: number, character: number): Promise<PvsResponseType> {
		const cmd: string = '(show-declaration "' + fileName + '" "pvs" ' + "'(" + line + " " + character + ")" + ')';
		return await this.pvsExec("show-declaration", cmd);
	}
	/**
	 * Identifies the importchain for a given theory.
	 * @param theoryName Theory name for which the importchain should be computed
	 */
	async showImportChain(theoryName: string): Promise<PvsResponseType> {
		if (theoryName) {
			const cmd: string = '(show-importchain "' + theoryName + '")';
			return await this.pvsExec("show-importchain", cmd);
		}
		return Promise.resolve({
			res: {
				theories: []
			},
			error: null
		});
	}
	/**
	 * Internal function, typechecks a file
	 * @param uri The uri of the file to be typechecked
	 * @param tcpFlag Optional flag, triggers automatic proof of tccs
	 */
	private async _typecheckFile(uri: string, tcpFlag?: boolean): Promise<PvsResponseType> {
		let fileName: string = getFilename(uri, { removeFileExtension: true });
		const cmd: string = (tcpFlag) ? 
			'(typecheck-file "' + fileName + '" nil t nil)'
				: '(typecheck-file "' + fileName + '" nil nil nil)';
		await this.initTypeChecker();
		return await this.proverExec("typecheck-file", cmd);
		// const cmd: string = '(json-typecheck-file "' + fileName + '")'; /// what is the difference between json-xxx and xxx?
		// return (await this.pvsExec("json-typecheck-file", cmd)).res;
	}

	/**
	 * Typechecks a file and tries to discharge all tccs
	 * @param uri The uri of the file to be typechecked
	 * @param tcp Triggers automatic proof of tccs
	 */
	async typecheckFile (uri: string, tcpFlag?: boolean): Promise<PvsTypecheckerResponse> {
		const fileName: string = getFilename(uri, { removeFileExtension: true });
		const filePath: string = getPathname(uri);
		let response: PvsTypecheckerResponse = {
			fileName: fileName,
			res: null,
			error: null
		};
		if (filePath !== path.join(this.pvsPath, "lib")) {
			// await _this.pvsProcess.changeContext(filePath);
			const info: PvsResponseType = await this._typecheckFile(fileName, tcpFlag);
			if (info.error) {
				response.error = info.error.parserError;
			} else {
				response.res = info.res
			}
		} else {
			this.connection.console.info("PVS library file " + fileName + " already typechecked.");
		}
		return response;
	}

	/**
	 * Typechecks a theory
	 * @param theoryName The theory to be typechecked
	 */
	async typecheckTheory(theoryName: string): Promise<PvsParserResponse> {
		const importChain: PvsTheoryListDescriptor = await this.listTheories();
		if (importChain && importChain.theories) {
			const fileName = importChain.theories[theoryName];
			const cmd: string = '(typecheck-file "' + fileName + '" nil nil nil)';
			await this.initTypeChecker();
			return (await this.proverExec("typecheck-file", cmd)).res;
		}
		return Promise.resolve(null);
	}
	/**
	 * Provides pvs version information
	 */
	async pvsVersionInformation(): Promise<PvsResponseType> {
		const cmd: string = '(get-pvs-version-information)';
		return await this.pvsExec("pvs-version-information", cmd);
	}

	async prettyprintRegion (desc: PrettyPrintRegionRequest): Promise<PrettyPrintRegionResult> {
		// TODO
		return null;
	}

	async continueProof(cmd: string): Promise<string> {
		await this.initTypeChecker();
		let ans: string = (await this.proverExec("json-prove-formula", cmd)).res;
		// (install-prooflite-scripts "test" "test" 0 t)
		//'(install-prooflite-scripts "' + desc.fileName + '" "' + desc.theoryName +  '" ' + desc.line + ' t)';
		return ans;
	}

}


interface ParseExpressionDescriptor {
	kind: string
};
interface PvsShowDeclarationInterface {
	theoryName: string,
	declaration: string,
	comment: string
}
export interface PvsFindDeclarationInterface {
    [ key: string ] : PvsDeclarationType;
}
export interface PvsShowImportChain {
	theories: string[] // list of theories, ordered by import
}

// utility functions and constants
const PVS_LISP_ERROR_RETURN_TO_TOP_LEVEL: string = 'Return to Top Level (an "abort" restart)';
function getKey(theoryName: string, symbolName: string): string {
	return theoryName + "." + symbolName;
}

/**
 * Class used by PvsProcess to detect pvs ready prompt and transform pvs lisp output to JSON objects.
 * TODO: use visitor pattern
 */
class PvsLisp {
	private cmd: string = null;
	private pvsOut: string = "";
	private connection: Connection = null;
	/**
	 * Constructor
	 * @param cmd {string} The command sent to pvs lisp.
	 */
	constructor (cmd: string, connection: Connection) {
		this.cmd = cmd;
		this.connection = connection;
	}
	/**
	 * Parses pvs lisp output and transforms it into a JSON object. The function collects the pvs lisp output until the pvs ready prompt is detected.
	 * @param data {string} The PVS output to be parsed. 
	 * @param cb {function} Function given by the caller, invoked when the output is ready.
	 */
	parse(data: string, cb: (out: string) => void) {
		this.pvsOut += data;
		// see also regexp from emacs-src/ilisp/ilisp-acl.el
		const PVS_COMINT_PROMPT_REGEXP: RegExp = /\s*pvs\(\d+\):|([\w\W\s]*)\spvs\(\d+\):/g;
		const PVSIO_PROMPT: RegExp = /<PVSio>/g;
		const PROVER_PROMPT: RegExp = /\bRule\?/g;
		const QUERY_YES_NO: RegExp = /\?\s*\(Y or N\)|\?\s*\(Yes or No\)/gi;
		let ready: boolean = PVS_COMINT_PROMPT_REGEXP.test(data)
								|| PVSIO_PROMPT.test(data)
								|| PROVER_PROMPT.test(data)
								|| QUERY_YES_NO.test(data);
		PVS_COMINT_PROMPT_REGEXP.lastIndex = 0;
		PVSIO_PROMPT.lastIndex = 0;
		PROVER_PROMPT.lastIndex = 0;
		QUERY_YES_NO.lastIndex = 0;
		if (ready && cb) {
			// if (this.connection) { this.connection.console.info("Server ready!"); }
			const pvsOut: string = this.pvsOut;
			this.pvsOut = "";
			let ans: PvsResponseType = {
				error: null,
				res: null
			};
			if (pvsOut.includes(PVS_LISP_ERROR_RETURN_TO_TOP_LEVEL)) {
				const PVS_LISP_ERROR_REGEXP = /[\w\W\s]*(\d+):\s*Return to Top Level[\w\W\s]*/gim;
				const tmp = PVS_LISP_ERROR_REGEXP.exec(pvsOut);
				if (tmp && tmp.length === 2) {
					ans.error = {
						msg: pvsOut,
						restartOption: +tmp[1]
					};
				} else {
					if (this.connection) {
						this.connection.console.error("PVS responded with error message \n" + pvsOut);
					}
				}
			}
			if (PROVER_PROMPT.test(pvsOut)) {
				let match: RegExpMatchArray =  /\w+\s*:([\w\W\s]+)\nRule\?/gi.exec(pvsOut);
				ans.res = match[1];
			} else if (QUERY_YES_NO.test(pvsOut)) {
				ans.res = pvsOut;
			} else {
				switch (this.cmd) {
					case "pvs-version-information": {
						const PVS_VERSION_INFO_REGEXP: { [s:string]: RegExp } = {
							brief: /"(\d+.?\d*)"[\s|nil]*"([^"]*)"/g,
							full: /"(\d+.?\d*)"[\s|nil]*"([^"]*)"\s*"(\d+.?\d*)([^"]*)/g
						};
						let info = PVS_VERSION_INFO_REGEXP.brief.exec(pvsOut);
						if (info && info.length > 2) {
							const pvsVersion: string = info[1];
							const lispVersion: string = info.slice(2).join(" ");
							ans.res = {
								pvsVersion: "PVS " + pvsVersion,
								lispVersion: lispVersion
							};
						} else {
							if (this.connection) { this.connection.console.warn("Unexpected pvs version response\n" + pvsOut); }
							ans.res = {
								pvsVersion: pvsOut,
								lispVersion: ""
							};
						}
						break;
					}
					case "pvs-init": {
						ans.res = "pvs process ready!";
						break;
					}
					case "parser-init": {
						ans.res = "parser ready!";
						break;
					}
					case "parse-file":
					case "json-typecheck-file":
					case "typecheck-file": {
						if (pvsOut.includes(PVS_LISP_ERROR_RETURN_TO_TOP_LEVEL)) {
							const PVS_PARSE_FILE_ERROR_REGEXP = /Parsing (.*)([\w\W\n]+)In file (\w*)\s*\(line (\d+), col (\d+)\)/gim;
							const tmp = PVS_PARSE_FILE_ERROR_REGEXP.exec(pvsOut);
							if (tmp && tmp.length === 6) {
								ans.error. parserError = {
									msg: tmp[2],
									fileName: tmp[3],
									line: +tmp[4],
									character: +tmp[5]
								};
							} else {
								if (this.connection) { this.connection.console.error("Unexpected parser error\n" + pvsOut); }
							}
						} else {
							ans.res = pvsOut;
						}
						break;
					}
					case "parse-expression": {
						if (pvsOut.includes(PVS_LISP_ERROR_RETURN_TO_TOP_LEVEL)) {
							const PVS_PARSE_EXPRESSION_ERROR_REGEXP = /[\w\W\s]*(\d+):\s*Return to Top Level[\w\W\s]*/gim;
							const tmp = PVS_PARSE_EXPRESSION_ERROR_REGEXP.exec(pvsOut);
							if (tmp && tmp.length === 2) {
								const error = {
									restartOption: tmp[1]
								};
								return cb(JSON.stringify({ error: error }));
							} else {
								if (this.connection) { this.connection.console.error("Unexpected expression parser response\n" + pvsOut); }
							}
						} else {
							const PVS_PARSE_EXPRESSION_REGEXP = /\s(id|number|string-value)\s*([^\s]*)\s/gim;
							let kind = "";
							if (pvsOut.includes(":instance allocation:")) {
								let tmp = pvsOut.split(":instance allocation:")[1];
								let info = PVS_PARSE_EXPRESSION_REGEXP.exec(tmp);
								if (info && info.length > 1) {
									kind = info[1];
								}
							}
							const res: ParseExpressionDescriptor = {
								kind: kind
							};
							ans.res = res;
						}
						break;
					}
					case "show-declaration": {
						// command defined in pvs-browser.el, line 82
						let res: PvsShowDeclarationInterface = {
							theoryName: null,
							declaration: null,
							comment: null
						}
						if (pvsOut.includes("The cursor is at the declaration")) {
							res.comment = "The cursor is already at the declaration for this identifier";
						} else if (pvsOut.includes("is a theory; to see it use C-c C-f")) {
							res.comment = "Theory"; 
						} else {
							const PVS_SHOW_DECLARATION_REGEXP: RegExp = /% From theory ([^:]*):([\s\w\W]*)nil/g;
							const info: string[] = PVS_SHOW_DECLARATION_REGEXP.exec(pvsOut);
							if (info && info.length > 2) {
								res.theoryName = info[1];
								res.declaration = info[2].trim();
							}
						}
						ans.res = res;
						break;
					}
					case "list-declarations":
					case "find-declaration": {
						// regexp for parsing the pvs lisp response
						const PVS_FIND_DECLARATION_REGEXP = {
							prelude: /\("[^"]*\s*"\s*"([^\s"]*)"\s*"([^\s"]*)"\s*([nil]*)\s*\((\d+\s*\d+\s*\d+\s*\d+)\)\s*"([^"]*)"\)/g,
							other: /\("[^"]*\s*"\s*"([^\s"]*)"\s*"([^\s"]*)"\s*"([^"]*)"*\s*\((\d+\s*\d+\s*\d+\s*\d+)\)\s*"([^"]*)"\)/g
						}
						// key is symbolFile.symbolTheory.symbolName, value is { symbolKind: string, symbolDeclaration: string, symbolDeclarationRange: vscode.Range }
						let declarations: PvsFindDeclarationInterface = {};
						let info = null;
						while (info = PVS_FIND_DECLARATION_REGEXP.prelude.exec(pvsOut)) {
							const symbolName: string = info[1];
							const symbolTheory: string = info[2];
							const symbolDeclarationFile: string = PRELUDE_FILE;
							const symbolDeclarationPosition = info[4].split(" ");
							const symbolDeclarationRange = {
								start: {
									line: +symbolDeclarationPosition[0],
									character: +symbolDeclarationPosition[1]
								},
								end: {
									line: +symbolDeclarationPosition[2],
									character: +symbolDeclarationPosition[3]
								}
							};
							const symbolDeclaration: string = info[5];
							declarations[getKey(symbolTheory, symbolName)] = {
								symbolName: symbolName,
								symbolTheory: symbolTheory,
								symbolDeclaration: symbolDeclaration,
								symbolDeclarationRange: symbolDeclarationRange,
								symbolDeclarationFile: symbolDeclarationFile
							};
						}
						while (info = PVS_FIND_DECLARATION_REGEXP.other.exec(pvsOut)) {
							const symbolName: string = info[1];
							const symbolTheory: string = info[2];
							const symbolDeclarationFile: string = info[3];
							const symbolDeclarationPosition = info[4].split(" ");
							const symbolDeclarationRange = {
								start: {
									line: +symbolDeclarationPosition[0],
									character: +symbolDeclarationPosition[1]
								},
								end: {
									line: +symbolDeclarationPosition[2],
									character: +symbolDeclarationPosition[3]
								}
							};
							const symbolDeclaration: string = info[5];
							declarations[getKey(symbolTheory, symbolName)] = {
								symbolName: symbolName,
								symbolTheory: symbolTheory,
								symbolDeclaration: symbolDeclaration,
								symbolDeclarationRange: symbolDeclarationRange,
								symbolDeclarationFile: symbolDeclarationFile
							};
						}
						ans.res = declarations;
						break;
					}
					case "list-theories": {
						// returns the list of theories in the current context
						// theories are grouped by filename
						let res: PvsTheoryListDescriptor = {
							folder: "",
							files: {},
							theories: {}
						};
						let collect_theories_regexp: RegExp = new RegExp(/\("([^"]+)"\s+"([^"]+)"\)/g); // capture group 1 is theory name, capture group 2 is filename
						let match: RegExpMatchArray = [];
						while(match = collect_theories_regexp.exec(pvsOut)) {
							let theoryName: string = match[1];
							let fileName: string = match[2];
							if (fileName.endsWith(".pvs")) {
								fileName = fileName.substr(0, fileName.length - 4);
							}
							res.files[fileName] = res.files[fileName] || [];
							res.files[fileName].push(match[1]);
							res.theories[theoryName] = res.theories[theoryName] || [];
							res.theories[theoryName].push(fileName);
						}
						collect_theories_regexp.lastIndex = 0;
						ans.res = res;
						break;
					}
					case "show-importchain": {
						let res: PvsShowImportChain = {
							theories: []
						};
						let show_importchain_regexp: RegExp = new RegExp(/Theory\s+([^\s]+) is /gi);
						let match: RegExpMatchArray = [];
						while(match = show_importchain_regexp.exec(pvsOut)) {
							res.theories.push(match[1]);
						}
						show_importchain_regexp.lastIndex = 0;
						ans.res = res;
						break;
					}
					case "show-tccs": {
						let res: TccDescriptor[] = [];
						// capture group 1: tcc one-liner message
						// capture group 2: tcc type
						// capture group 3: position (line) of the symbol that has triggered the tcc
						// capture group 4: position (column) of the symbol that has triggered the tcc
						// capture group 5: symbol that has triggered the tcc
						// capture group 6: tcc message (e.g., expected type list[int])
						// capture group 7: tcc status (e.g., unfinished)
						// capture group 8: tcc ID (name of the pvs theorem to be proved)
						// capture group 9: tcc formula (body of the pvs theorem to be proved)
						const regexp: RegExp = new RegExp(/((.+) generated \(at line (\d+), column (\d+)\) for\s+%?(.+))\s+%\s*(.*)\s+%\s*(.*)\s+(\w+):\s*OBLIGATION\s+([^%]+)/gi);
						let match: RegExpMatchArray = null;
						while (match = regexp.exec(pvsOut)) {
							if (match[3] && match[4]) {
								res.push({
									line: +match[3],
									character: +match[4],
									symbol: match[5],
									msg: match[6],
									status: match[7],
									id: match[8],
									formula: match[9]
								});
							}
						}
						ans.res = res;
						break;
					}
					case "change-context":
					case "current-context":
					case "disable-gc-printout":
					case "emacs-interface":
					case "pvs-continue":
					case "load-pvs-attachments":
					case "evaluation-mode-pvsio":
					case "install-prooflite-scripts":
					case "edit-proof-at":
					case "install-proof":
					case "json-prove-formula":
					default: {
						ans.res = pvsOut;
					}
				}
			}
			cb(JSON.stringify(ans));
		}
	}
}

