/**
 * @module PvsProxy
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
// - the PVS server accepts XML-RPC messages; the only method that can be invoked is pvs.request.
// - the parameter passed to pvs.request is a string encoding of a JSON-RPC message (see
// - https://www.jsonrpc.org/specification) the JSON-RPC message indicates the command to be
// - executed by pvs, e.g., reset, typecheck, etc.

// methods supported by pvs-server:
// 'change-context',      'change-workspace',
// 'find-declaration',    'help',
// 'interrupt',           'lisp',
// 'list-client-methods', 'list-methods',
// 'names-info',          'parse',
// 'proof-command',       'proof-script',
// 'proof-status',        'prove-formula',
// 'prove-tccs',          'prover-status',
// 'reset',               'show-tccs',
// 'term-at',             'typecheck'

import { Client, Server, createClient, createServer } from 'xmlrpc';
import { PvsProcess } from "./pvsProcess";
import { PvsResponse, ParseResult } from "./common/pvs-gui.d";
import * as fsUtils from './common/fsUtils';
import * as path from 'path';
import * as net from 'net';
import * as crypto from 'crypto';
import { SimpleConnection, serverEvent, PvsVersionDescriptor } from './common/serverInterface';
import { Parser } from './core/Parser';
import * as languageserver from 'vscode-languageserver';
import { ParserDiagnostics } from './core/pvs-parser/javaTarget/pvsParser';
import { getErrorRange } from './common/languageUtils';
import * as utils from './common/languageUtils';


//----------------------------
// constants introduced for dev purposes, while waiting for the new pvs snapshot
// export const show_tccs_result: any =
//     [ { id: 'sq_TCC1',
// 	theory: 'sq',
// 	comment: [ '% Subtype TCC generated (at line 10, column 23) for  a * a\n    % expected type  nonneg_real' ],
// 	'from-decl': 'sq',
// 	definition: 'FORALL (a: real): a * a >= 0',
// 	proved: true },
//       { id: 'sq_div_TCC1',
// 	theory: 'sq',
// 	comment: [ '% Subtype TCC generated (at line 32, column 54) for  sq(d)\n    % expected type  nznum' ],
// 	'from-decl': 'sq_div',
// 	definition: 'FORALL (d: real): d /= 0 IMPLIES sq(d) /= 0',
//   proved: true } ] ;

// export const proof_script_result: string =
//   ';;; Proof sq_ge-1 for formula sq.sq_ge\n(""\n (skosimp)\n (ground)\n (("1"\n   (expand "sq")\n   (case "forall (x,y:real): x>=y iff not x<y")\n   (("1" (flatten) (assert)) ("2" (skosimp) (ground))))\n  ("2" (assert))))'


// const proof_script_check_chev_fup_permission: string = `;;; Proof check_chev_fup_permission-1 for formula alaris_th.check_chev_fup_permission
// (""
//  (skosimp*)
//  (expand "per_release_fup")
//  (split)
//  (("1" (postpone))
//   ("2"
//    (expand "fup")
//    (expand "decrement")
//    (lift-if)
//    (split)
//    (("1" (postpone)) ("2" (flatten) (postpone))))
//   ("3" (expand "per_release_chevron") (flatten) (postpone))))`;  
//----------------------------


const ENABLE_NEW_PARSER: boolean = false;

export class PvsProgressInfo {
	protected progressLevel: number = 0;
	showProgress (cmd: string, data?: string): string {
		this.progressLevel++;
		if (this.progressLevel > 4) {
			this.progressLevel = 0;
		}
		return `Executing ${cmd}${".".repeat(this.progressLevel)}`;
	}
}

export declare interface FileList {
  contextFolder: string;
  fileNames: string[]; // TODO: FileDescriptor[]
}

export declare interface ContextDiagnostics {
  [fileName: string]: PvsResponse
};


// type _PvsResponse_ = _PvsError_ | _PvsResult_
// interface _PvsError_ {
// 	jsonrpc: "2.0";
// 	id: string;
// 	error: string;
// }
// interface _PvsResult_ {
// 	jsonrpc: "2.0";
// 	id: string;
// 	result?: string;
// }


export class PvsProxy {
	protected parserCache: { [ filename: string ]: { hash: string, diags: ParserDiagnostics, isTypecheckError?: boolean } } = {};
	protected activeParsers: { [ filename: string ]: boolean } = {};
	// mathObjetcsCache stores identifiers known to the parser, grouped by kind (lemma, types, definitions). 
	// The utility of matchObjectsCache if for autocompletion functions.
	// The actual definition can be searched with declarationProvider.
	protected mathObjectsCache: { lemmas: string[], types: string[], definitions: string[] } = {
		lemmas: [],
		types: [],
		definitions: []
	};

	protected showBanner: boolean = true;
	protected debugMode: boolean = false;

	// protected isActive: boolean = false;
	readonly MAXTIME: number = 2000; // millis
	readonly MAX_PORT_ATTEMPTS: number = 200;
	readonly client_methods: string[] = ['info', 'warning', 'debug', 'buffer', 'yes-no', 'dialog'];
	protected banner: string;
	protected handlers: { [mth: string]: (params: string[]) => string[] } = {};
	protected serverAddress: string = "0.0.0.0"; // using "0.0.0.0" instead of "localhost" because the client seems to have troubles connecting when indicating "localhost"
	protected serverPort: number;
	protected clientAddress: string = "0.0.0.0";
	protected clientPort: number;
	protected client: Client;
	protected guiServer: Server; // GUI server, needed to receive responses sent back by pvs-server
	protected pvsPath: string;
	protected pvsLibraryPath: string;
	protected pvsServer: PvsProcess;
	protected connection: SimpleConnection; // connection to the client
	protected externalServer: boolean;
	protected cliListener: (data: string) => void; // useful to show progress feedback
	protected progressInfo: PvsProgressInfo; // sends progress feedback to the front-end, in a form that can be rendered in the status bar

	// protected parserQueue: Promise<PvsResponse> = Promise.resolve(null);
	/**
	 * Parser
	 */
	protected parser: Parser;


	protected nReboots: number = 0;
	readonly MAX_REBOOTS: number = 4;

	/** The constructor simply sets various properties in the PvsProxy class. */
	constructor(pvsPath: string,
		opt?: {
			serverPort?: number,
			clientPort?: number, 
			connection?: SimpleConnection,
			externalServer?: boolean
		}) {
		opt = opt || {};
		this.pvsPath = pvsPath;
		this.pvsLibraryPath = path.join(pvsPath, "lib");
		this.serverPort = (!!opt.serverPort) ? opt.serverPort : 22334;
		this.clientPort = (!!opt.clientPort) ? opt.clientPort : 9092;
		this.client_methods.forEach(mth => {
			this.handlers[mth] = (params: string[]): string[] => {
				// console.info("info", params);
				return params;
			};
		});
		this.banner = `XML-RPC GUI Server active at http://${this.clientAddress}:${this.clientPort}`;
		this.connection = opt.connection;
		this.externalServer = !!opt.externalServer;
		this.cliListener = null;
		this.progressInfo = new PvsProgressInfo();

		// create antlr parser for pvs
		this.parser = new Parser();
	}

	async enableExternalServer (): Promise<void> {
		console.info("[pvs-proxy] Enabling external server...");
		await this.killPvsServer();
		this.externalServer = true;
	}

	async disableExternalServer (): Promise<void> {
		console.info("[pvs-proxy] External server disabled");
		this.externalServer = false;
		await this.restartPvsServer();
	}

	//--------------------------------------------------
	//         json-rpc methods
	//--------------------------------------------------
	pvsRequest(method: string, params?: string[]): Promise<PvsResponse> {
		params = params || [];
		const req = { method: method, params: params, jsonrpc: "2.0", id: this.get_fresh_id() };
		return new Promise((resolve, reject) => {
			if (this.client) {
				this.progressInfo.showProgress(method);
				const jsonReq: string = JSON.stringify(req);
				// console.log(jsonReq);
				return this.client.methodCall("pvs.request", [jsonReq, `http://${this.clientAddress}:${this.clientPort}`], (error: Error, value: string) => {
					if (error) {
						console.error("[pvs-proxy] Error returned by pvs-server: "); 
						console.dir(error, { depth: null }); 
					}
					// console.log("[pvs-proxy] Value returned by pvs-server: ");
					// console.dir(value);
					if (value) {
						this.nReboots = 0;
						this.progressInfo.showProgress(method);
						try {
							const resp: PvsResponse = JSON.parse(value);
							// console.dir(resp, { depth: null });
							if (resp && (resp["result"] === "null" || resp["result"] === "nil")) {
								// sometimes pvs returns a string "null" or "nil" as result -- here the string is transformed into a proper null value
								resp["result"] = null;
							}
							resolve(resp);
						} catch (jsonError) {
							console.error(`[pvs-proxy] Unable to parse pvs-server response :/`, value);
							resolve(null);
						}
					} else {
						if (error && error['code'] === 'ECONNREFUSED') {
							// if the server refuses the connection, try to reboot
							if (this.nReboots < this.MAX_REBOOTS) {
								this.nReboots++;
								console.log(`[pvs-proxy] Connection refused, trying to reboot pvs from ${this.pvsPath} (attempt #${this.nReboots})`);
								setTimeout(() => {
									this.restartPvsServer().then(() => {
										resolve({
											jsonrpc: "2.0",
											id: req.id,
											error
										});
									});
								}, 1000);
							} else {
								console.log(`[pvs-proxy] Connection refused when launching pvs from ${this.pvsPath}`);
								resolve({
									jsonrpc: "2.0",
									id: req.id,
									error
								});
							}
						} else {
							if (error) {
								console.log(`[pvs-proxy] pvs-server returned error`, error);
							} else {
								console.info(`[pvs-proxy] pvs-server returned null`);
							}
							resolve({
								jsonrpc: "2.0",
								id: req.id,
								error: {
									code: -1,
									message: (error && error.message) ? error.message : "pvs-server returned null"
								}
							});
						}
					}
				});
			} else {
				console.log(`[pvs-proxy] Warning: could not invoke method ${method} (client is null)`);
				resolve({
					jsonrpc: "2.0",
					id: req.id,
					error: {
						code: -2,
						message: "pvs-proxy failed to initialize gui server"
					}
				});
			}
		});
	}
	async listMethodsRequest(): Promise<PvsResponse> {
		return await this.pvsRequest('list-methods');
	}
	async listClientMethods(): Promise<PvsResponse> {
		return await this.pvsRequest('list-client-methods');
	}

	/**
	 * Utility function, creates a PvsResponse out of parse/typecheck diagnostic messages
	 * @param diags Diagnostic messages from the parser
	 */
	protected makeDiags (diags: ParserDiagnostics, opt?: { id?: string, isTypecheckError?: boolean }): PvsResponse {
		opt = opt || {};
		const id: string = opt.id || this.get_fresh_id();
		let ans: PvsResponse = {
			jsonrpc: "2.0",
			id
		}
		if (diags) {
			ans["math-objects"] = diags["math-objects"];
			ans.contextFolder = diags.contextFolder;
			ans.fileName = diags.fileName;
			ans.fileExtension = diags.fileExtension;
			if (diags.errors && diags.errors.length > 0) {
				ans.error = {
					code: 1,
					message: 'Error',
					data: diags.errors
				};
			}
		}
		return ans;
	}
	/**
	 * Utility function, returns the list of known math objects
	 */
	listMathObjects (): { lemmas: string[], types: string[], definitions: string[] } {
		return this.mathObjectsCache;
	} 
	/**
	 * Utility function, returns the list of known lemmas
	 */
	listLemmas (): string[] {
		return this.mathObjectsCache.lemmas;
	} 
	/**
	 * Utility function, returns the list of known definitions
	 */
	listDefinitions (): string[] {
		return this.mathObjectsCache.definitions;
	} 
	/**
	 * Utility function, returns the list of known types
	 */
	listTypes (): string[] {
		return this.mathObjectsCache.types;
	} 
	/**
	 * Parse a given pvs file
	 * @param desc pvs file descriptor: context folder, file name, file extension
	 */
	async parseFile(desc: { contextFolder: string, fileName: string, fileExtension: string }, opt?: { test?: boolean }): Promise<PvsResponse | null> {
		opt = opt || {};
		if (desc) {
			const fname: string = path.join(desc.contextFolder, `${desc.fileName}${desc.fileExtension}`);
			const content: string = await fsUtils.readFile(fname);
			if (content) {
				const hash: string = crypto.createHash('sha256').update(content.replace(/\s/g, "")).digest('hex'); // do not consider white spaces when creating the hash
				if (!opt.test && this.parserCache[fname] && hash === this.parserCache[fname].hash) {
					// console.log("[pvs-proxy] Parser diagnostics loaded from cache.");
					const diags: ParserDiagnostics = this.parserCache[fname].diags;
					const isTypecheckError: boolean = this.parserCache[fname].isTypecheckError;
					if (diags) {
						const stats: string = JSON.stringify(this.parserCache[fname].diags["math-objects"]);
						if (diags.errors && diags.errors.length > 0) {
							const msg: string = `${desc.fileName}${desc.fileExtension} contains errors.`;
							// console.log(`[pvs-proxy] ${msg}`);
						} else {
							const msg: string = `${desc.fileName}${desc.fileExtension} parsed successfully!`;
							// console.log(`[pvs-proxy] ${msg}`);
						}
						// console.log(`[pvs-proxy] ${desc.fileName}${desc.fileExtension} | ${stats}`);
					}
					return this.makeDiags(diags, { isTypecheckError });
				} else {
					if (desc.fileExtension === ".hpvs") {
						if (!this.activeParsers[fname]) {
							// using new parser
							// console.log("[pvs-proxy] Updating parser cache.");
							this.activeParsers[fname] = true;
							const diags: ParserDiagnostics = await this.parser.parseFile(desc);
							this.parserCache[fname] = { hash, diags };
							delete this.activeParsers[fname];
							return this.makeDiags(diags);
						}
					} else {
						// TODO: create a separate module ParserWrapper
						if (this.isProtectedFolder(desc.contextFolder)) {
							this.info(`${desc.contextFolder} is already parsed`);
							// return resolve(null);
							return null;
						}
						const startTime: number = Date.now();
						const res: PvsResponse = await this.pvsRequest('parse', [fname]);
						if (opt.test) { return res; }
						if (res) {
							let range: languageserver.Range = null;
							let message: string = `File ${desc.fileName} parsed successfully`;
							let mathObjects: { types: number, definitions: number, lemmas: number } = { types: 0, definitions: 0, lemmas: 0 };
							if (res.result) {
								const result: ParseResult = res.result;
								if (result && result.length) {
									for (let i = 0; i < result.length; i++) {
										const theoryStats = result[i];
										if (theoryStats && theoryStats.decls && theoryStats.decls.length) {
											theoryStats.decls.forEach(decl => {
												switch (decl.kind) {
													case "type":
													case "datatype":
														mathObjects.types++;
														this.mathObjectsCache.types.push(decl.id);
														break;
													case "formula":
													case "judgement":
														mathObjects.lemmas++;
														this.mathObjectsCache.lemmas.push(decl.id);
														break;
													case "expr":
													case "conversion":
													case "auto-rewrite":
														mathObjects.definitions++;
														this.mathObjectsCache.definitions.push(decl.id);
														break;
													default: // do nothing
												}
											});
										}
									}
								}
							}
							const diags: ParserDiagnostics = {
								fileName: desc.fileName,
								fileExtension: desc.fileExtension,
								contextFolder: desc.contextFolder,
								"math-objects": mathObjects,
								"parse-time": { ms: Date.now() - startTime },
								range,
								message
							};
							if (res.error && res.error.data) {
								const errorStart: languageserver.Position = { 
									line: res.error.data.place[0], 
									character: res.error.data.place[1]
								};
								const errorEnd: languageserver.Position = (res.error.data.place.length > 3) ? { 
									line: res.error.data.place[2], 
									character: res.error.data.place[3]
								} : null;	
								const txt: string = await fsUtils.readFile(fname);
								const error_range: languageserver.Range = getErrorRange(txt, errorStart, errorEnd);
								const error: languageserver.Diagnostic = {
									range: error_range,
									message: res.error.data.error_string,
									severity: languageserver.DiagnosticSeverity.Error
								};
								diags.message = `File ${desc.fileName} contains errors`;
								diags.errors = [ error ];

								// send also a request to the antlr parser, as it may report more errors (the standard pvs parser stops at the first error)
								// const antlrdiags: ParserDiagnostics = await this.parser.parseFile(desc);
								// diags.errors = diags.errors.concat(antlrdiags.errors);
							} 
							this.parserCache[fname] = { hash, diags };
							// console.info('parseFile:');
							// console.dir(res, { depth: null });
							// return resolve(res);
							return this.makeDiags(diags, { id: res.id });
						} else {
							console.error(`[pvs-proxy] Error: received pvs-server error while parsing file ${desc.fileName}${desc.fileExtension}`, res);
							this.notifyError(JSON.stringify(res, null, " "));
							// return resolve(null);
							return null;
						}
					}
				}
			} else {
				// empty content
				return this.makeDiags({
					fileName: desc.fileName,
					fileExtension: desc.fileExtension,
					contextFolder: desc.contextFolder,
					"math-objects": { types: 0, definitions: 0, lemmas: 0 },
					"parse-time": { ms: 0 },
					range: null,
					message: `File ${desc.fileName} is empty`
				});
			}
		}
		return null;
	}

	async prettyPrintDdl(desc: { fileName: string, fileExtension: string, contextFolder: string, expr: string }): Promise<string> {
		if (desc && desc.expr) {
			return await this.parser.prettyPrintDdl(desc);
		}
		return null;
	}
	/**
	 * Translates a hybrid program into a standard pvs file
	 * @param desc File descriptor for the hybrid program
	 */
	async hp2pvs(desc: { contextFolder: string, fileName: string, fileExtension: string }): Promise<PvsResponse> {
		if (desc) {
			this.notifyStartExecution(`Generating PVS file ${desc.fileName}.hpvs`);
			const id: string = this.get_fresh_id();

			let ans: PvsResponse = {
				jsonrpc: "2.0",
				id
			}
			const diags: ParserDiagnostics = await this.parser.hp2pvs(desc);
			if (diags) {
				ans["math-objects"] = diags["math-objects"];
				ans.contextFolder = diags.contextFolder;
				ans.fileName = diags.fileName;
				ans.fileExtension = diags.fileExtension;
				if (diags.errors && diags.errors.length > 0) {
					const msg: string = `PVS file could not be generated (${desc.fileName}.hpvs contains errors)`;
					this.reportError(msg);
					ans.error = {
						code: 1,
						message: msg,
						data: diags.errors
					};
				} else {
					this.notifyEndExecution(`${desc.fileName}.pvs generated successfully!`);
				}
			}
			return ans;
		}
		return null;
	}

	/**
	 * Typechecks a given pvs file
	 * @param desc pvs file descriptor: context folder, file name, file extension
	 * @param opt 
	 */
  	async typecheckFile (desc: { contextFolder: string, fileName: string, fileExtension: string }): Promise<PvsResponse> {
		if (desc) {
			let fname: string = fsUtils.desc2fname(desc);
			const taskId: string = `typecheck-${fname}`;
			this.notifyStartImportantTask(taskId, `Typechecking file ${desc.fileName}${desc.fileExtension}`);
			if (this.isProtectedFolder(desc.contextFolder)) {
				this.info(`${desc.fileName}${desc.fileExtension} is already typechecked`);
				return null;
			}
			if (desc.fileExtension === ".hpvs") {
				// translate file to .pvs and then typecheck
				await this.hp2pvs(desc);
				fname = path.join(desc.contextFolder, `${desc.fileName}.pvs`);
			}
			const res: PvsResponse = await this.pvsRequest('typecheck', [ fname ]);
			if (res && (res.error && res.error.data) || res.result) {
				if (res.result) {
					this.notifyEndImportantTask(taskId, `Typechecking successful for ${desc.fileName}${desc.fileExtension}`);
				} else {
					// the typecheck error might be generated from an imported file --- we need to check res.error.file_name
					fname = (res.error && res.error.data && res.error.data.file_name) ? res.error.data.file_name : fname;
					if (res.error && res.error.data && this.parserCache[fname]) {
						const errorStart: languageserver.Position = { 
							line: res.error.data.place[0], 
							character: res.error.data.place[1]
						};
						const errorEnd: languageserver.Position = (res.error.data.place.length > 3) ? { 
							line: res.error.data.place[2], 
							character: res.error.data.place[3]
						} : null;
						const txt: string = await fsUtils.readFile(fname);
						const error_range: languageserver.Range = getErrorRange(txt, errorStart, errorEnd);
						const error: languageserver.Diagnostic = {
							range: error_range,
							message: res.error.data.error_string,
							severity: languageserver.DiagnosticSeverity.Error
						};
						// update parser stats in cache
						this.parserCache[fname].diags.errors = [ error ];
						if (desc.fileExtension === ".hpvs") {
							const hpvs_fname: string = fsUtils.desc2fname(desc);
							if (this.parserCache[hpvs_fname]) {
								this.parserCache[hpvs_fname].diags.errors = [ error ]
								this.parserCache[hpvs_fname].isTypecheckError = true;
							}
						}
					}
					this.notifyEndImportantTaskWithErrors(taskId, `Typecheck error in file ${desc.fileName}${desc.fileExtension}: ${res.error.data.error_string}`);
				}
			} else {
				const msg: string = `Typechecker was unable to process file ${desc.fileName}${desc.fileExtension}: ${res}`;
				console.error(`[pvs-proxy] Error: ${msg}`);
				this.notifyEndImportantTaskWithErrors(taskId, msg);
				if (typeof res === "string") {
					const error: languageserver.Diagnostic = {
						range: { start: { line: 1, character: 0 }, end: { line: 1, character: 100 } },
						message: res,
						severity: languageserver.DiagnosticSeverity.Error
					};
					// update parser cache
					this.parserCache[fname].diags.errors = [ error ];
					this.parserCache[fname].isTypecheckError = true;
					// return error
					return {
						jsonrpc: "2.0",
						id: this.get_fresh_id(),
						error
					};
				}
			}
			return res;
		}
		return null;
	}

	/**
	 * Re-runs the proofs for all theorems and tccs in the given pvs file
	 * @param desc 
	 */
	async proveFile (desc: { contextFolder: string, fileName: string, fileExtension: string,  }): Promise<PvsResponse> {
		if (desc) {
			this.notifyStartExecution(`Proving ${desc.fileName}`);
			if (this.isProtectedFolder(desc.contextFolder)) {
				this.info(`${desc.contextFolder} is already proved`);
				return null;
			}
			const fname: string = path.join(desc.contextFolder, `${desc.fileName}${desc.fileExtension}`);
			const res: PvsResponse = await this.pvsRequest('lisp', [ `(prove-pvs-file "${fname}" nil)` ]);
			this.notifyEndExecution();
			return res;
		}
		return null;
	}

	/**
	 * Starts an interactive prover session for the given formula
	 * @param desc 
	 */
	async proveFormula(desc: { contextFolder: string, fileName: string, fileExtension: string, theoryName: string, formulaName: string }): Promise<PvsResponse> {
		if (desc) {
			this.notifyStartExecution(`Typechecking files necessary to prove formula ${desc.formulaName}`);
			await this.changeContext(desc.contextFolder);
			const fullName: string = desc.fileName + ".pvs" + "#" + desc.theoryName; // file extension is always .pvs, regardless of whether this is a pvs file or a tcc file
			const ans: PvsResponse = await this.pvsRequest("prove-formula", [ desc.formulaName, fullName ]);		
			this.notifyEndExecution();
			return ans;
		}
		return null;
	}


	/**
	 * FIXME: THIS IS NOT SUPPORTED YET BY PVS-SERVER
	 * Starts an interactive pvsio evaluator session for the given theory
	 * @param desc 
	 */
	protected async startEvaluator (desc: { contextFolder: string, fileName: string, fileExtension: string, theoryName: string }): Promise<PvsResponse> {
		if (desc) {
			this.notifyStartExecution(`Evaluation environment for ${desc.theoryName}`);
			if (this.isProtectedFolder(desc.contextFolder)) {
				this.info(`${desc.contextFolder} cannot be evaluated`);
				return null;
			}
			// const res: PvsResponse = await this.lisp(`(pvsio "${fname}" t)`);
			// make sure file typechecks correctly
			let ans: PvsResponse = await this.typecheckFile(desc);
			if (ans && !ans.error) {
				// make sure we are in the correct context
				ans = await this.changeContext(desc.contextFolder);
				// disable garbage collector printout
				ans = await this.lisp('(setq *disable-gc-printout* t)');
				// load semantic attachments
				ans = await this.lisp('(load-pvs-attachments)');
				// enter pvsio mode -- do not wait, pvs won't return control
				this.lisp(`(evaluation-mode-pvsio "${desc.theoryName}" nil nil)`); // the fourth argument removes the pvsio banner
			}
			this.notifyEndExecution();
			return ans;
		}
		return null;
	}

	/**
	 * FIXME: THIS IS NOT SUPPORTED YET BY PVS-SERVER
	 * Evaluates a pvs/lisp expression
	 * @param desc Descriptor of the expression
	 */
	protected async evaluateExpression (desc: { contextFolder: string, fileName: string, fileExtension: string, theoryName: string, cmd: string }): Promise<PvsResponse> {
		if (desc) {
			const cmd = (desc.cmd.endsWith("!") || desc.cmd.endsWith(";")) ? desc.cmd
							: `${desc.cmd};`;
			this.notifyStartExecution(`Executing ${cmd}`);
			// console.dir(desc, { depth: null });
			const res: PvsResponse =  await this.pvsRequest('proof-command', [ desc.cmd ]); //await this.lisp(cmd);
			this.notifyEndExecution();
			return res;
		}
		return null;
	}

	/**
	 * Returns the status of all proofs in a given theory
	 * @param desc Descriptor specifying context folder and theory name
	 */
	async statusProofTheory (desc: { contextFolder: string, fileName: string, fileExtension: string, theoryName: string }): Promise<PvsResponse> {
		if (desc) {
			await this.changeContext(desc.contextFolder);
			const res: PvsResponse = await this.pvsRequest('lisp', [ `(status-proof-theory "${desc.theoryName}")` ]);
			return res;
		}
		return null;
	}

	/**
	 * Changes context
	 * @param contextFolder 
	 */
	async changeContext (desc: string | { contextFolder: string }): Promise<PvsResponse> {
		if (desc) {
			const ctx: string = (typeof desc === "string") ? desc : desc.contextFolder;
			return await this.pvsRequest('change-context', [ ctx ]);
		}
		return null;
	}

	/**
	 * Returns the current context
	 */
	async currentContext (): Promise<PvsResponse> {
		return await this.pvsRequest('lisp', [ '(pvs-current-directory)' ]);
	}

	/**
	 * Executes a lisp command in pvs
	 * @param cmd 
	 */
	async lisp(cmd: string): Promise<PvsResponse> {
		return await this.pvsRequest('lisp', [ cmd ]);
	}

	/**
	 * Finds a symbol declaration
	 * @param symbolName Symbol name 
	 */
	async findDeclaration (symbolName: string): Promise<PvsResponse> {
		const ans: PvsResponse = await this.pvsRequest('find-declaration', [ symbolName ]);
		if (ans && ans.result) {
			if (typeof ans.result !== "object") {
				console.error(`[pvs-proxy] Warning: pvs-server returned malformed result for find-declaration (expecting object found ${typeof ans.result})`);
			}
			if (typeof ans.result === "string") {
				ans.result = JSON.parse(ans.result);
			}
		}
		return ans;
	}

	async termAt (desc: { fileName: string, fileExtension: string, contextFolder: string, line: number, character: number }): Promise<PvsResponse> {
		const fname: string = fsUtils.desc2fname(desc);
		const ans: PvsResponse = await this.pvsRequest("term-at", [ fname, `(${desc.line} ${desc.character})`, 't' ]); 
		return ans;
	}

	/**
	 * Executes a proof command. The command is always adorned with round parentheses, e.g., (skosimp*)
	 * @param desc Descriptor of the proof command
	 */
	async proofCommand(desc: { cmd: string }): Promise<PvsResponse> {
		if (desc) {
			this.notifyStartExecution(`Executing ${desc.cmd}`);
			// console.dir(desc, { depth: null });
			const showHidden: boolean = utils.isShowHiddenCommand(desc.cmd);
			// the following additional logic is a workaround necessary because pvs-server does not know the command show-hidden. 
			// the front-end will handle the command, and reveal the hidden sequents.
			const cmd: string = showHidden ? "(skip)" : desc.cmd; 
			const res: PvsResponse =  await this.pvsRequest('proof-command', [ cmd ]);
			if (showHidden) {
				if (res.result) {
					res.result.action = "Showing list of hidden sequents";
					if (res.result.commentary && res.result.commentary.length) {
						res.result.commentary[0] = "No change on: (show-hidden)";
					} 
				}
			}
			// --- to invoke more commands, just do as follows (no need to use then, just use await)
			// const stat: PvsResponse = await this.pvsRequest('prover-status', []);
			// console.dir(stat);
			this.notifyEndExecution();
			return res;
		}
		return null;
	}

	/**
	 * Returns the prover status
	 */
	async proverStatus(): Promise<PvsResponse> {
		const res: PvsResponse = await this.pvsRequest('prover-status');
		return res;
	}

	/**
	 * Returns the proof script for the given formula
	 */
	async proofScript(desc: { contextFolder: string, fileName: string, fileExtension: string, formulaName: string, theoryName: string }): Promise<PvsResponse> {
		if (desc) {
			// need to change extension, in the case this request comes from a .tccs file
			const fname: string = fsUtils.desc2fname({ contextFolder: desc.contextFolder, fileName: desc.fileName, fileExtension: ".pvs" });
			const result: PvsResponse = await this.pvsRequest('proof-script', [ fname, desc.formulaName ]);
			return result;
			// return { 
			// 	result: proof_script_check_chev_fup_permission, //proof_script_result, 
			// 	jsonrpc: "2.0", 
			// 	id: "testing-proof-script" 
			// };
		}
		return null;
	}

	/**
	 * Shows all tccs generated for the given theory
	 * @param desc 
	 */
	async showTccs(desc: { contextFolder: string, fileName: string, fileExtension: string, theoryName: string }): Promise<PvsResponse> {
		if (desc) {
			// @TODO @SAM: show-tccs should include fullName in the call, as in prove-formula
			// const res: PvsResponse = await this.pvsRequest('show-tccs', [ desc.theoryName, fullName ]);
			await this.changeContext(desc.contextFolder);
			const res: PvsResponse = await this.pvsRequest('show-tccs', [ desc.theoryName ]);
			return res;
		}
		return null;
	}

	/**
	 * Discharge all tccs generated for the given theory
	 * @param desc 
	 */
	async proveTccs(desc: { contextFolder: string, fileName: string, fileExtension: string }): Promise<PvsResponse> {
		if (desc) {
			const fname: string = path.join(desc.contextFolder, `${desc.fileName}${desc.fileExtension}`);
			const res: PvsResponse = await this.pvsRequest('prove-tccs', [ fname ]);
			return res;
		}
		return null;
	}

	/**
	 * Returns pvs version information
	 */
	async getPvsVersionInfo(): Promise<{ "pvs-version": string, "lisp-version": string }> {
		const res: PvsResponse = await this.lisp(`(get-pvs-version-information)`);
		if (res && res.result) {
			const regexp: RegExp = /\((\d+(?:.?\d+)*)[\s|nil]*([\w\s\d\.]*)/g; // group 1 is pvs version, group 2 is lisp version
			const info: RegExpMatchArray = regexp.exec(res.result);
			if (info && info.length > 2) {
				return {
					"pvs-version": info[1].trim(),
					"lisp-version": info[2].trim()
				}
			}
		}
		return null;
	}

	
	//--------------------------------------------------
	//         internal functions
	//--------------------------------------------------
	protected info (...data: any): void {
		if (this.debugMode) {
			console.info(data);
		}
		if (this.cliListener) {
			this.cliListener(data);
		}
	}
	// @TODO: move all notifications to pvsLanguageServer
	protected notifyStartExecution (msg: string): void {
		if (this.connection) {
			this.connection.sendNotification("server.status.progress", msg);
		}
	}
	protected notifyEndExecution (msg?: string): void {
		if (this.connection) {
			this.connection.sendNotification("server.important-notification", msg);
		}
	}
	protected notifyStartImportantTask (id: string, msg: string): void {
		if (this.connection) {
			this.connection.sendNotification(`server.status.start-important-task`, msg);
		}
	}
	protected notifyProgressImportantTask (id: string, msg: string): void {
		if (this.connection) {
			this.connection.sendNotification(`server.status.progress-important-task-${id}`, msg);
		}
	}
	protected notifyEndImportantTask (id: string, msg: string): void {
		if (this.connection) {
			this.connection.sendNotification(`server.status.end-important-task-${id}`, msg);
		}
	}
	protected notifyEndImportantTaskWithErrors (id: string, msg: string) {
		if (this.connection) {
			this.connection.sendNotification(`server.status.end-important-task-${id}-with-errors`, msg);
		}
	}
	protected reportError (msg: string): void {
		// error shown in a dialogue box
		if (this.connection) {
			this.connection.sendNotification("server.status.report-error", msg);
		}
	}
	protected notifyError (msg: string): void {
		// error shown in the status bar
		if (this.connection) {
			this.connection.sendNotification("server.status.error", msg);
		}
	}
	/**
	 * Checks availability of the port. Returns true if the port is available, false otherwise.
	 * The check works as follows. A dummy server is created at port p; if the creation of the server succeeds, an event 'listening' is triggered, otherwise an event 'error' is triggered.
	 * The server is turned off as soon as an answer is available.
	 */
	protected checkPort (p: number, retry: boolean): Promise<boolean> {
		// console.info(`checking port ${p}`);
		return new Promise((resolve, reject) => {
			if (this.showBanner) {
				console.log(`[pvs-proxy] Checking port ${p}...`);
			}
			const server: net.Server = net.createServer();
			const timeout: number = 1000; // msec
			server.once('error', (error: Error) => {
				if (error["code"] === 'EADDRINUSE' && retry) {
					console.log(`[pvs-proxy] port ${p} busy, retrying after timeout of ${timeout} msec`);
					retry = false; // retry just once on the same port
					setTimeout(() => {
						this.checkPort(p, false);
					}, timeout);
				} else {
					console.log(`[pvs-proxy] port ${p} is not available :/`);
					resolve(false);
				}
			});
			server.once('listening', () => {
				// console.error(`port ${p} is available :)`);
				server.once('close', () => {
					resolve(true);
				});
				server.close();
			});
			server.listen(p);
		});
	}

	/**
	 * Checks if pvs-server accepts connection
	 */
	async testServerConnectivity(): Promise<boolean> {
		const client: Client = createClient({
			host: "0.0.0.0", port: this.clientPort, path: "/RPC2"
		});
		return new Promise((resolve, reject) => {
			client.methodCall('request', [], (error, value) => {
				if (error) {
					console.log("[pvs-proxy] server-test error:", error);
					return resolve(false);
				}
				// console.log("server-test value:", value);
				return resolve(true);
			});
		});
	}

	/**
	 * Callback function called from activate, when gui-server is ready 
	 */
	protected serverReadyCallBack() {
		this.guiServer.on('request', (error: any, params: string[], callback: (error: any, value: any) => void) => {
			if (error) {
				console.log("[pvs-proxy] Error", error);
			}
			// console.log("params", params);
			if (params && params.length > 0) {
				try {
					for (let i = 0; i < params.length; i++) {
						const json: { method: string, params: string[] } = JSON.parse(params[i]);
						// console.info(`\n json: `, json);
						if (json && json.method && this.handlers[json.method]) {
							this.handlers[json.method](json.params);
						}
					}
				} catch (jsonError) {
					console.log("[pvs-proxy] Error", jsonError);
				}
			}
			callback(error, params);
		});
		this.guiServer.on("error", (error: Error) => {
			console.log(`[pvs-proxy] GUI-Server Error`, error);
		});
		this.guiServer.on("connect", (error: any, params: string, callback: (error: any, value: any) => void) => {
			if (error) {
				console.log("[pvs-proxy] connection error", error);
			}
			if (params) {
				console.info("[pvs-proxy] connection params", params);
			}
		});
		// this.isActive = true;
		// console.info(this.server.httpServer);
	}
	/**
	 * Utility function, creates a new pvs-server
	 */
	protected async createPvsServer(opt?: { enableNotifications?: boolean }): Promise<PvsProcess | null> {
		opt = opt || {};
		const connection: SimpleConnection = (opt.enableNotifications) ? this.connection : null;
		const proc: PvsProcess = new PvsProcess({ pvsPath: this.pvsPath, contextFolder: this.pvsPath }, connection);
		const success: boolean = await proc.activate({
			enableNotifications: opt.enableNotifications,
			xmlRpcServer: { port: this.serverPort }
		});
		if (success) {
			if (connection) {
				connection.console.info(`[pvs-proxy] pvs-server active at http://${this.serverAddress}:${this.serverPort}`);
			} else {
				console.info(`[pvs-proxy] pvs-server active at http://${this.serverAddress}:${this.serverPort}`);
			}
			return proc;
		}
		console.log(`[pvs-proxy] Failed to activate pvs-server at http://${this.serverAddress}:${this.serverPort}`);
		this.notifyError("Failed to start pvs-server");
		return null;
	}
	killParser(): void {
		if (this.parser) {
			this.parser.killParser();
		}
	}
	/**
	 * Kill pvs process
	 */
	async killPvsServer(): Promise<void> {
		if (this.pvsServer && !this.externalServer) {
			await this.pvsServer.kill();
			console.info("[pvs-proxy] Killed pvs-server");
		}
	}
	/**
	 * Kill pvs-gui server
	 */
	async killPvsProxy (): Promise<void> {
		return new Promise((resolve, reject) => {
			if (this.guiServer) {
				// console.dir(this.guiServer, { depth: null });
				this.guiServer.httpServer.once("close", () => {
					if (this.showBanner) {
						console.log("[pvs-proxy] Closed pvs-proxy");
					}
					this.guiServer = null;
					this.client = null;
					resolve();
				});
				this.guiServer.httpServer.close();
			} else {
				resolve();
			}
		});
	}

	async listSystemMethods (): Promise<{ error: { code: string, message: string, stack: string }, result: string[] }> {
		return new Promise((resolve, reject) => {
			if (this.client) {
				this.client.methodCall("system.listMethods", [ ], (error: { code: string, message: string, stack: string }, value: string[]) => {
					resolve({ error, result: value });
				});
			} else {
				console.log('[pvs-proxy] Warning: client is null :/');
				resolve(null);
			}
		});
	}

	async killAndRestartPvsServer (desc?: { pvsPath?: string }): Promise<void> {
		if (desc && desc.pvsPath) {
			this.pvsPath = desc.pvsPath;
			console.log(`[pvs-proxy] New pvs path: ${this.pvsPath}`);
		}
		await this.killPvsServer();
		// an explicit invocation of restartServer is not necessary, pvs server will automatically be rebooted --- see pvsRequest method
		// await this.restartPvsServer();
	}

	protected async sendPvsVersionInfo () : Promise<void> {
		const desc: PvsVersionDescriptor = await this.getPvsVersionInfo();
		if (desc) {
			if (this.externalServer) {
				desc["lisp-version"] += " [EXTERNAL SERVER]";
			}
			this.connection.sendRequest(serverEvent.pvsVersionInfo, desc);
		}
	}

	async restartPvsServer (desc?: { pvsPath?: string }): Promise<void> {
		if (desc && desc.pvsPath) {
			this.pvsPath = desc.pvsPath;
			console.log(`[pvs-proxy] New pvs path: ${this.pvsPath}`);
		}
		if (!this.externalServer) {
			console.info("[pvs-proxy] Restarting pvs-server...");
			this.pvsServer = await this.createPvsServer({ enableNotifications: true });
			console.info("[pvs-proxy] Restart complete!");
		}
		// if pvs server has been created, then create the client
		if (this.externalServer || this.pvsServer) {
			await this.createClient();
		}
		this.sendPvsVersionInfo(); // async call
	}

  // async xmlrpcMethodHelp (methodName: string): Promise<XmlRpcResponse> {
  // 	return new Promise((resolve, reject) => {
  // 	    this.client.methodCall("system.methodHelp", [ methodName ], (error: XmlRpcError, value: string) => {
  // 		const res: RpcPvsResponse = JSON.parse(value);
  // 		resolve({ error, res });
  // 	    });
  // 	});
  // }

	/**
	 * Utility methods
	*/
	get_fresh_id(): string {
		// getTime can be used, but does potentially lead to duplicate ids
		// return new Date().getTime();
		// This may be overkill, a simple call to random is probably good enough.
		return crypto.createHash('sha256').update(Math.random().toString(36)).digest('hex');
	}
	isProtectedFolder(contextFolder: string): boolean {
		return !(contextFolder !== this.pvsPath && contextFolder !== this.pvsLibraryPath);
	}

	/**
	 * Other internal methods
	 */
	protected async disableGcPrintout (): Promise <PvsResponse> {
		return await this.pvsRequest('lisp', [ '(setq *disable-gc-printout* t)' ]);
	}
	protected async namesInfo(fileName: string): Promise<PvsResponse> {
		return await this.pvsRequest('names-info', [ fileName ]);
	}
	protected async xmlrcpReset(): Promise<PvsResponse> {
		return await this.pvsRequest('reset');
	}



	/**
	 * pvs-proxy activation function
	 * @param opt 
	 */
	async activate(opt?: { debugMode?: boolean, showBanner?: boolean }): Promise<boolean> {
		opt = opt || {};
		this.showBanner = (opt.showBanner === undefined) ? true : opt.showBanner;
		this.debugMode = !!opt.debugMode;
		if (this.pvsServer) {
			return Promise.resolve(true);
		}
		// try to create pvs server
		this.notifyStartExecution("Activating pvs language features...");
		if (!this.externalServer) {
			this.pvsServer = await this.createPvsServer({ enableNotifications: true });
		}
		// if pvs server has been created, then create the client
		if (this.externalServer || this.pvsServer) {
			const success: boolean = await this.createClient(opt);
			if (success) {
				this.notifyEndExecution("VSCode-PVS ready!");
			} else {
				this.notifyError("Error while creating pvs language client :/");
			}
			return success;
		}
		return false;
	}

	async createClient (opt?: { debugMode?: boolean, showBanner?: boolean }): Promise<boolean> {
		opt = opt || {};
		if (this.client) { Promise.resolve(true); }
		return new Promise(async (resolve, reject) => {
			this.notifyStartExecution(`Loading user interface components...`);
			try {
				let portIsAvailable: boolean = (this.guiServer) ? true : false;
				for (let i = 0; !portIsAvailable && i < this.MAX_PORT_ATTEMPTS; i++) {
					portIsAvailable = await this.checkPort(this.clientPort, true);
					if (portIsAvailable === false) {
						this.clientPort++;
					}
				}
				if (portIsAvailable) {
					this.banner = `XML-RPC GUI Server active at http://${this.clientAddress}:${this.clientPort}`;
					this.client = createClient({
						host: this.serverAddress, port: this.serverPort, path: "/RPC2"
					});
					if (!this.client) {
						console.log(`[pvs-proxy] Error: could not create client necessary to connect to pvs-server`);
						reject(false);
					}
					if (!this.guiServer) {
						this.guiServer = createServer({
							host: this.clientAddress, port: this.clientPort, path: "/RPC2"
						}, () => {
							this.serverReadyCallBack();
							if (opt.showBanner) {
								console.log("[pvs-proxy] " + this.banner);
							}
							this.notifyEndExecution();
							resolve(true);
						});
					}
				} else {
					console.error(`[pvs-proxy] Error: could not start GUI-server`);
					reject(false);
				}
			} catch (err) {
				console.error("[pvs-proxy] Error while activating XmlRpcProvider", JSON.stringify(err));
				reject(false);
			}
		});
	}

}
