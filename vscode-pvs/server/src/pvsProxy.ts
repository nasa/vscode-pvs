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

import * as xmlrpc from 'xmlrpc';
import { PvsProcess, ProcessCode } from "./pvsProcess";
import { PvsResponse, ParseResult, ShowTCCsResult, PvsError } from "./common/pvs-gui.d";
import * as fsUtils from './common/fsUtils';
import * as path from 'path';
import * as net from 'net';
import * as crypto from 'crypto';
import { SimpleConnection, serverEvent, PvsVersionDescriptor, ProofStatus, ProofDescriptor, ProofFile, PvsFormula, ServerMode, TheoryDescriptor, PvsTheory, FormulaDescriptor, PvsFile, PvsContextDescriptor, FileDescriptor, SequentDescriptor, MathObjects, ProofOrigin, VSCodePvsVersionDescriptor, PvsFileDescriptor, DumpFileDescriptor } from './common/serverInterface';
import { Parser } from './core/Parser';
import * as languageserver from 'vscode-languageserver';
import { ParserDiagnostics } from './core/pvs-parser/javaTarget/pvsParser';
import { checkPar, CheckParResult, getErrorRange, isQuitCommand, isQuitDontSaveCommand, isSaveThenQuitCommand, isShowExpandedSequentCommand, isShowFullyExpandedSequentCommand, isShowHiddenFormulas } from './common/languageUtils';
import * as languageUtils from './common/languageUtils';
import { PvsProxyLegacy } from './legacy/pvsProxyLegacy';
import { PvsErrorManager } from './pvsErrorManager';
import * as os from 'os';
import { DUMP_FILE_EXTENSION } from './common/fsUtils';

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

export class PvsProxy {
	// protected parserCache: { [ filename: string ]: { hash: string, diags: ParserDiagnostics, isTypecheckError?: boolean } } = {};
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
	protected verbose: boolean = false;

	readonly MAXTIME: number = 2000; // millis
	readonly MAX_PORT_ATTEMPTS: number = 200;
	readonly client_methods: string[] = ['info', 'warning', 'debug', 'buffer', 'yes-no', 'dialog'];
	protected banner: string;
	protected handlers: { [mth: string]: (params: string[]) => string[] } = {};
	protected serverAddress: string = "0.0.0.0"; // using "0.0.0.0" instead of "localhost" because the client seems to have troubles connecting when indicating "localhost"
	protected serverPort: number;
	protected clientAddress: string = "0.0.0.0";
	protected clientPort: number;
	protected client: xmlrpc.Client;
	protected guiServer: xmlrpc.Server; // GUI server, needed to receive responses sent back by pvs-server
	protected pvsPath: string;
	protected pvsLibPath: string; // these are internal libraries
	protected nasalibPath: string;
	protected pvsLibraryPath: string; // these are external libraries
	protected pvsServer: PvsProcess;
	protected connection: SimpleConnection; // connection to the client
	protected cliListener: (data: string) => void; // useful to show progress feedback

	protected pvsErrorManager: PvsErrorManager;
	// protected buffer: Promise<PvsResponse> = Promise.resolve(null); // this is a queue used to serialize the requests sent to the server

	protected pvsVersionInfo: PvsVersionDescriptor = null;

	protected legacy: PvsProxyLegacy;
	protected useLegacy: boolean = true;
	protected useNasalib: boolean = false;
	protected externalServer: boolean;

	protected mode: ServerMode = "lisp";
	protected proverBusy: boolean = false;

	/**
	 * Parser
	 */
	parser: Parser;

	/** The constructor simply sets various properties in the PvsProxy class. */
	constructor(pvsPath: string,
		opt?: {
			serverPort?: number,
			clientPort?: number, 
			connection?: SimpleConnection,
			externalServer?: boolean,
			pvsLibraryPath?: string
		}) {
		opt = opt || {};
		this.pvsPath = pvsPath;
		// this.pvsLibPath = path.join(pvsPath, "lib");
		// this.nasalibPath = path.join(pvsPath, "nasalib");
		this.pvsLibraryPath = opt.pvsLibraryPath || "";
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

		// create antlr parser for pvs
		this.parser = new Parser();

		this.legacy = //this.externalServer ? null : 
			new PvsProxyLegacy(pvsPath, opt); // this is needed for find-declaration, which breaks the server.
	}

	setClientPort (p: number): boolean {
		if (p) {
			this.clientPort = p;
			return true;
		}
		return false;
	}
	setClientAddress (addr: string): boolean {
		if (addr) {
			this.clientAddress = addr;
			return true;
		}
		return false;
	}

	async getMode (): Promise<ServerMode> {
		// return await this.getServerMode(); // pvs is not always able to return the prover status -- the workaround is to use this.mode to keep track of the prover status, and ask pvs the status only when stricly necessary
		return this.mode;
	}

	async enableExternalServer (opt?: { enabled?: boolean }): Promise<void> {
		opt = opt || {};
		if (opt.enabled === false) {
			await this.disableExternalServer();
		} else {
			console.info("[pvs-proxy] Enabling external server...");
			await this.killPvsServer();
			this.externalServer = true;
		}
	}

	async disableExternalServer (): Promise<void> {
		console.info("[pvs-proxy] External server disabled");
		this.externalServer = false;
		// await this.restartPvsServer();
	}

	protected normalizeSequent (res: SequentDescriptor | { result: string }, cmd: string): SequentDescriptor {
		// NOTE: this is a temporary fix while waiting the server APIs to be fixed
		if (res) {
			const sequent: SequentDescriptor = {
				path: res["path"],
				label: (res["result"]) ? "Q.E.D." : res["label"],
				commentary: (res["result"]) ? [ res["result"] ] 
								: res["prover-session-status"] ? [ "Q.E.D." ] 
								: res["commentary"],
				"num-subgoals": (res["result"]) ? 0 : res["num-subgoals"],
				sequent: (res["result"]) ? {} : res["sequent"],
				comment: res["comment"] || "",
				"prev-cmd": (res["prev-cmd"] && typeof res["prev-cmd"] === "string") ? res["prev-cmd"].replace(/\s+/g, " ") : res["prev-cmd"]
			}
			sequent["commentary"] = sequent["commentary"] || [];
			if (typeof sequent["commentary"] === "string") { sequent["commentary"] = [ sequent["commentary"] ]; }
			if (res["action"]) {
				sequent["commentary"].push(res["action"]);
			}
			if (res["num-subgoals"] && +res["num-subgoals"] > 1) {
				sequent["commentary"].push(`this yields ${res["num-subgoals"]} subgoals:`);
			}
			return sequent;
		}
		return null;
	}

	//--------------------------------------------------
	//         json-rpc methods
	//--------------------------------------------------
	async pvsRequest (method: string, params?: string[]): Promise<PvsResponse> {
		params = params || [];
		const req = { method: method, params: params, jsonrpc: "2.0", id: this.get_fresh_id() };
		// this.buffer = this.buffer.then(() => {
			return new Promise(async (resolve, reject) => {
				if (this.proverBusy && req && req.method === "proof-command") {
					console.warn(`[pvs-proxy] Warning: prover busy, ignoring command ${req.method}`);
					return resolve({
						jsonrpc: "2.0", 
						id: req.id
					});
				}
				if (this.client) {
					const jsonReq: string = JSON.stringify(req, null, " ");
					// console.log(jsonReq);

					if (!this.externalServer && req && req.method !== "prover-status") {
						// console.dir(jsonReq);
						// const msg: string = (req.params) ? req.method + " " + JSON.stringify(req.params)
						// 	: req.method;
						// console.log(msg);
					}
					this.proverBusy = true;
					this.client.methodCall("pvs.request", [jsonReq, `http://${this.clientAddress}:${this.clientPort}`], (error: Error, value: string) => {
						// console.log(error);
						// console.log(value);
						this.proverBusy = false;

						if (error) {
							console.error("[pvs-proxy] Error returned by pvs-server: "); 
							console.error(error); 
							if (error['code'] === 'ECONNREFUSED') {
								// if the server refuses the connection, try to reboot
								console.log(`[pvs-proxy] Connection refused when launching pvs from ${this.pvsPath}`);
								resolve({
									jsonrpc: "2.0", 
									id: req.id,
									error
								});
								if (this.pvsErrorManager) {
									const pvs: string = path.join(this.pvsPath, "pvs");
									const fileExists: boolean = fsUtils.fileExists(pvs);
									if (!fileExists) {
										this.pvsErrorManager.notifyPvsNotFound(this.pvsPath);
									} else {
										this.pvsErrorManager.notifyPvsFailure({ 
											msg: `Error: unable to connect to pvs-server at http://${this.clientAddress}:${this.clientPort}`,
											src: "pvs-server"
										});
									}
								}
							} else {
							// if (error['code'] === 'ECONNRESET') {
							// 	// do nothing -- this usually occurs when the user reboots pvs-server
							// } else {
								if (this.pvsErrorManager) {
									this.pvsErrorManager.notifyPvsFailure({ method: jsonReq, msg: error.message, src: "pvs-server" });
								}
								resolve({
									jsonrpc: "2.0", 
									id: req.id,
									error: {
										error_string: `xmlrpc method ${method} failed with error ${JSON.stringify(error, null, " ")}`
									}
								});
							}
						} else if (value) {
							// console.log("[pvs-proxy] Value returned by pvs-server: ");
							// console.dir(value);
							try {
								const resp: PvsResponse = JSON.parse(value);

								// console.dir(resp, { depth: null });
								if (resp && (resp["result"] === "null" || resp["result"] === "nil")) {
									// sometimes pvs returns a string "null" or "nil" as result -- here the string is transformed into a proper null value
									resp["result"] = null;
								}

								// console.log("---------------- in-checker --------------");
								// console.dir(req);
								// console.dir(resp);
								// console.log("------------------------------------------\n");
								if ((method === "proof-command" || method === "prove-formula") && resp.result) {
									if (resp.result) {
										if (resp.result.length) {
											for (let i = 0; i < resp.result.length; i++) {
												resp.result[i] = this.normalizeSequent(resp.result[i], params[0]);
											}
										} else {
											resp.result = [ this.normalizeSequent(resp.result, params[0]) ];
										}
									}
								}
								resolve(resp);
							} catch (jsonError) {
								console.error(`[pvs-proxy] Unable to parse pvs-server response :/`, value);
								resolve(null);
							}
						} else {
							console.error(`[pvs-proxy] pvs-server returned error`, error);
							resolve({
								jsonrpc: "2.0",
								id: req.id,
								error: {
									code: 'NO_CONTENT',
									message: "pvs-server returned null response"
								}
							});
						}
					});
				} else {
					// this error typically occurs at the very beginning if pvs is not installed
					console.log(`[pvs-proxy] Warning: could not invoke method ${method} (pvs is not installed)`);
					this.proverBusy = false;
					resolve({
						jsonrpc: "2.0",
						id: req.id,
						error: {
							code: 'PVS_NOT_INSTALLED',
							message: "pvs-proxy failed to initialize gui server"
						}
					});
				}
			});
		// });
		// return this.buffer;
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
		let ans: PvsResponse = { jsonrpc: "2.0", id };
		if (diags) {
			ans["math-objects"] = diags["math-objects"];
			ans.contextFolder = diags.contextFolder;
			ans.fileName = diags.fileName;
			ans.fileExtension = diags.fileExtension;
			if (diags.errors && diags.errors.length > 0) {
				ans.error = {
					code: 1,
					message: 'Errors',
					data: diags.errors
				};
			}
		}
		return ans;
	}
	/**
	 * Utility function, returns the list of known math objects
	 */
	listMathObjects (): MathObjects {
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
	async parseFile(desc: { 
		contextFolder: string, 
		fileName: string, 
		fileExtension: string 
	}, opt?: { 
		test?: boolean,
		externalServer?: boolean,
		enableEParser?: boolean
	}): Promise<PvsResponse | null> {
		opt = opt || {};
		// console.log(desc);
		if (desc && desc.fileName && desc.fileExtension && desc.contextFolder) {
			const fname: string = path.join(desc.contextFolder, `${desc.fileName}${desc.fileExtension}`);
			const fileExists: boolean = fsUtils.fileExists(fname);
			if (fileExists) {
				await this.changeContext(desc);
				const content: string = await fsUtils.readFile(fname);
				if (content) {
					if (desc.fileExtension === ".hpvs") {
						if (!this.activeParsers[fname]) {
							// using new parser
							this.activeParsers[fname] = true;
							const diags: ParserDiagnostics = await this.parser.parseFile(desc);
							delete this.activeParsers[fname];
							return this.makeDiags(diags);
						}
					} else {
						// TODO: create a separate module ParserWrapper?
						if (this.isProtectedFolder(desc.contextFolder)) {
							this.info(`${desc.contextFolder} is already parsed`);
							return {
								jsonrpc: "2.0",
								id: `parse-${desc.contextFolder}`,
								result: `Skipping parsing of ${desc.contextFolder} (protected pvs folder)`
							};
						}
						const startTime: number = Date.now();
						// const externalServer: boolean = opt.externalServer === undefined ? this.externalServer : !!opt.externalServer;
						const res: PvsResponse = (this.useLegacy) ? await this.legacy?.parseFile(fname)
							: await this.pvsRequest('parse', [fname]);
						if (opt.test) { return res; }

						// testing antlr parser
						// this.parser.parseFile(desc); // async call
						
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

								if (opt.enableEParser) {
									// send also a request to the antlr parser, as it may report more errors (the standard pvs parser stops at the first error)
									const antlrdiags: ParserDiagnostics = await this.parser.parseFile(desc);
									if (antlrdiags && antlrdiags.errors) {
										diags.errors = diags.errors.concat(antlrdiags.errors);
										if (antlrdiags["parse-time"]) {
											console.log(`[pvs-parser] antlr completed parsing in ${antlrdiags["parse-time"].ms}ms`);
										}
									}
								}
							} 
							return this.makeDiags(diags, { id: res.id });
						} else {
							console.error(`[pvs-proxy] Error: received pvs-server error while parsing file ${desc.fileName}${desc.fileExtension}`, res);
							return null;
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
			} else {
				const message: string = `File ${fname} does not exist or is not readable`;
				const error: languageserver.Diagnostic = {
					range: { start: { line: 0, character: 0}, end: { line: 0, character: 0 }},
					message,
					severity: languageserver.DiagnosticSeverity.Error
				};
				return this.makeDiags({
					fileName: desc.fileName,
					fileExtension: desc.fileExtension,
					contextFolder: desc.contextFolder,
					"math-objects": { types: 0, definitions: 0, lemmas: 0 },
					"parse-time": { ms: 0 },
					range: null,
					message,
					errors: [ error ]
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
			const id: string = this.get_fresh_id();
			let ans: PvsResponse = { jsonrpc: "2.0", id };
			const diags: ParserDiagnostics = await this.parser.hp2pvs(desc);
			if (diags) {
				ans["math-objects"] = diags["math-objects"];
				ans.contextFolder = diags.contextFolder;
				ans.fileName = diags.fileName;
				ans.fileExtension = diags.fileExtension;
				if (diags.errors && diags.errors.length > 0) {
					const msg: string = `PVS file could not be generated (${desc.fileName}.hpvs contains errors)`;
					ans.error = {
						code: 1,
						message: msg,
						data: diags.errors
					};
				}
			}
			return ans;
		}
		return null;
	}

	// async getProofStatus (formula: PvsFormula): Promise<ProofStatus | null> {
	// 	const fname: string = fsUtils.desc2fname(formula);
	// 	const response: PvsResponse = await this.lisp(`(proof-status-at "${fname}" nil 12 "pvs")`);
	// 	if (response && response.result) {
	// 		switch (response.result) {
	// 			case "subsumed":
	// 			case "simplified":
	// 			// case "proved - incomplete":
	// 			// case "proved - by mapping":
	// 			// case "proved - complete": 
	// 			case "proved":
	// 				return "proved";
	// 			case "unfinished": // proof attempted but failed
	// 				return "unfinished";
	// 			case "unchecked":  // proof was successful, but needs to be checked again because of changes in the theories
	// 				return "unchecked";
	// 			case "unproved":
	// 			case "untried": // proof has not been attempted yet
	// 				return "untried";
	// 		}
	// 	}
	// 	return null;
	// }

	/**
	 * Creates a new file that contains all the specifications and associated proofs for the import chain of the specified PVS file (M-x dump-pvs-files).
	 * This function reflects the behavior of the emacs function dump-pvs-files-to-current-buffer
	 */
	async dumpPvsFiles (desc: PvsFile): Promise<DumpFileDescriptor> {
		if (desc?.fileName) {
			const contextFolder: string = fsUtils.tildeExpansion(desc.contextFolder);
			const pvsFiles: PvsFile[] = await this.getPvsFileDependencies(desc);
			if (pvsFiles?.length) {
				const dependencies: { fname: string, tag: string }[] = [];
				// create a list that associates filenames to tags that will be used in the .dump file
				for (let i = 0; i < pvsFiles?.length; i++) {
					const fileName: string = `${pvsFiles[i].fileName}${pvsFiles[i].fileExtension}`;
					const fname: string = path.join(pvsFiles[i].contextFolder, fileName);
					dependencies.push({
						fname,
						tag: `$$$${fileName}`
					});
					const prf_fileName: string = `${pvsFiles[i].fileName}.prf`;
					const prf_fname: string = path.join(pvsFiles[i].contextFolder, prf_fileName);
					dependencies.push({
						fname: prf_fname, 
						tag: `$$$${prf_fileName}`
					});
				}
				// predefined files included in the dump file
				const predefined: { fname: string, tag: string }[] = [
					{ fname: path.join(contextFolder, "pvs-strategies"), tag: "$$$pvs-strategies" },
					{ fname: path.join("~", "pvs-strategies"), tag: "$$$PVSHOME/pvs-strategies" },
					{ fname: path.join("~", ".pvs.lisp"), tag: "$$$PVSHOME/.pvs.lisp" },
					{ fname: path.join("~", ".pvsemacs"), tag: "$$$PVSHOME/.pvsemacs" },
				];
				// generated the content of the dump file
				// for each file in dumpFiles, writing the tag followed by the file content
				let dumpFileContent: string = "";
				for (let i = 0; i < predefined.length; i++) {
					if (fsUtils.fileExists(predefined[i].fname)) {
						const fileContent: string = await fsUtils.readFile(predefined[i].fname);
						dumpFileContent += `\n${predefined[i].tag}\n${fileContent}`;
					}
				}
				const files: FileDescriptor[] = [];
				for (let i = 0; i < dependencies.length; i++) {
					if (fsUtils.fileExists(dependencies[i].fname)) {
						const fileContent: string = await fsUtils.readFile(dependencies[i].fname);
						files.push({
							fileContent,
							fileName: fsUtils.getFileName(dependencies[i].fname),
							fileExtension: fsUtils.getFileExtension(dependencies[i].fname),
							contextFolder: fsUtils.getContextFolder(dependencies[i].fname)
						});
						dumpFileContent += `\n${dependencies[i].tag}\n${fileContent}`;
					}
				}
				// get pvs version
				const pvsVersion: PvsVersionDescriptor = this.getPvsVersionInfo();
				dumpFileContent = `
%% PVS Version ${this.pvsVersionInfo?.version}${this.pvsVersionInfo?.['nasalib-version'] ? " + NASALib" : ""}
%% ${os.version()}
` + dumpFileContent;
				// write dump file
				const dumpFileName: string = `${desc.fileName}`;
				const dumpFile: string = path.join(contextFolder, `${dumpFileName}${DUMP_FILE_EXTENSION}`);
				const success: boolean = await fsUtils.writeFile(dumpFile, dumpFileContent);
				if (success) {
					return {
						dmpFile: {
							contextFolder,
							fileName: dumpFileName,
							fileExtension: DUMP_FILE_EXTENSION,
							fileContent: dumpFileContent
						},
						files,
						folder: contextFolder
					};
				}
			}
		}
		return null;
	}

	/**
	 * Utility function, undumps .dmp files
	 * TODO: add support for libraries
	 */
	async undumpPvsFiles (desc: FileDescriptor): Promise<DumpFileDescriptor> {
		if (desc?.fileName && desc?.contextFolder) {
			const dmpFile: string = fsUtils.desc2fname(desc);
			if (fsUtils.fileExists(dmpFile)) {
				const undump_folder: string = path.join(desc.contextFolder, fsUtils.getUndumpFolderName(desc));
				const dmpFileContent: string = await fsUtils.readFile(dmpFile);
				// group 1 is the file name
				// group 2 is the file content
				const regex: RegExp = new RegExp(/\$\$\$(.+\.pvs)\s*\n([^\$]+)/g);
				let match: RegExpMatchArray = null;
				const undumpedFiles: PvsFile[] = [];
				while (match = regex.exec(dmpFileContent)) {
					const fname: string = match[1]?.trim();
					const fileContent: string = match[2]?.trim();
					if (fname && fileContent) {
						undumpedFiles.push({
							fileName: fsUtils.getFileName(fname),
							fileContent,
							fileExtension: fsUtils.getFileExtension(fname),
							contextFolder: undump_folder
						});
					}
				}
				if (undumpedFiles.length) {
					// create subdir and write files to disk
					await fsUtils.createFolder(undump_folder);
					for (let i = 0; i < undumpedFiles.length; i++) {
						const fname: string = fsUtils.desc2fname(undumpedFiles[i]);
						await fsUtils.writeFile(fname, undumpedFiles[i].fileContent);
					}
					return {
						dmpFile: desc,
						files: undumpedFiles,
						folder: undump_folder
					};	
				}
			}
		}
		return null;
	}


	/**
	 * Returns pvs file dependencies
	 * TODO: add support for libraries
	 */
	async getPvsFileDependencies (desc: PvsFile): Promise<PvsFile[]> {
		if (desc?.fileName) {
			// make sure the file is typechecked first
			let res: PvsResponse = await this.typecheckFile(desc);
			if (res?.result) {
				const contextFolder: string = fsUtils.tildeExpansion(desc.contextFolder);
				const lispCmd: string = `(get-pvs-file-dependencies \"${path.join(contextFolder, `${desc.fileName}${desc.fileExtension}`)}\")`;
				res = await this.legacy?.lisp(lispCmd);
				if (res?.result) {
					const fileNames: string[] = res.result.substring(1, res.result.length - 1 ).replace(/\n/g, "").split(/\s+/g);
					const pvsFiles: PvsFile[] = [];
					for (let i = 0; i < fileNames?.length; i++) {
						// filenames are reported between double quotes -- remove the double quotes and trim spaces
						const fileName: string = fileNames[i].replace(/\"/g, "").trim();
						if (fileName) {
							pvsFiles.push({
								contextFolder,
								fileName,
								fileExtension: ".pvs"
							});
						}
					}
					return pvsFiles;
				}		
			}
		}
		return [];
	}

	/**
	 * Returns the import chain for a given theory
	 * @param desc theory descriptor: context folder, file name, file extension, theory name
	 */
	async getImportChain (desc: PvsTheory): Promise<PvsTheory[]> {
		const importChain: PvsTheory[] = [];
		if (desc?.theoryName) {
			// change context
			await this.changeContext(desc);

			// const res: PvsResponse = await this.legacy?.lisp(`(prog2 (show-tccs "${fullName}")
			// new lisp command for importing proof chain
			const lispCmd: string = `
(let ((theoryname "${desc.theoryName}"))
		(let ((usings (remove-if #'(lambda (th)
		(or (from-prelude? th)
			(lib-datatype-or-theory? th)))
	(collect-theory-usings theoryname nil))))
		(mapcar (lambda (theory) (cons (format nil "|~a|" (id theory)) (shortname (make-specpath (filename theory))))) (reverse usings))))`;
			const res: PvsResponse = await this.legacy?.lisp(lispCmd);
			// )`);
			console.log(res?.result);
			if (res?.result) {
				const regex: RegExp = /\"\|(\w+)\|\"\s*\.\s*\"([^\|]+)\"\)/g;
				let match: RegExpMatchArray = null;
				while (match = regex.exec(res.result)) {
					if (match && match.length > 2) {
						const fname: string = match[2].trim();
						if (fname) {
							const pvsTheory: PvsTheory = {
								theoryName: match[1].trim(),
								fileName: fsUtils.getFileName(fname),
								contextFolder: fsUtils.getContextFolder(fname),
								fileExtension: fsUtils.getFileExtension(fname)
							};
							importChain.push(pvsTheory);
						}
					}
				}
				// console.dir(importChain);
			}
		}
		return importChain;
	}
	/**
	 * Resolves all theorems, tccs and import chain theorems for the given theory
	 * @param desc 
	 */
	async getTheorems (desc: PvsTheory, opt?: { includeImportChain?: boolean, tccsOnly?: boolean }): Promise<PvsFormula[]> {
		opt = opt || {};
		let ans: PvsFormula[] = [];
		if (desc) {
			let theories: PvsTheory[] = (opt.includeImportChain) ? await this.getImportChain(desc) : [ desc ];
			if (theories && theories.length) {
				for (let i = 0; i < theories.length; i++) {
					if (!opt.tccsOnly) {
						const formulaDescriptors: FormulaDescriptor[] = await fsUtils.listTheoremsInFile(fsUtils.desc2fname(theories[i]));
						if (formulaDescriptors && formulaDescriptors.length) {
							const theorems: FormulaDescriptor[] = formulaDescriptors.filter(formula => {
								return formula.theoryName === desc.theoryName;
							});
							if (theorems && theorems.length) {
								ans = ans.concat(theorems);
							}
						}
					}
					// generate tccs for the given theory
					const tccsResponse: PvsResponse = await this.generateTccsFile(theories[i]);
					if (tccsResponse && tccsResponse.result) {
						const tccsResult: ShowTCCsResult = <ShowTCCsResult> tccsResponse.result;
						for (let j = 0; j < tccsResult.length; j++) {
							if (tccsResult[j].id) {
								ans.push({
									formulaName: tccsResult[j].id,
									theoryName: theories[i].theoryName,
									fileName: theories[i].fileName,
									contextFolder: theories[i].contextFolder,
									fileExtension: ".tccs" //theories[i].fileExtension
								});
							}
						}
					}
				}
			}
		}
		return ans;
	}
	/**
	 * Generates a .tccs file for each theory in a given pvs file
	 * @param desc Handler arguments: filename, file extension, context folder
	 */
	async generateTccs (desc: PvsFile): Promise<PvsContextDescriptor> {
		desc = fsUtils.decodeURIComponents(desc);
		if (desc && desc.contextFolder && desc.fileName && desc.fileExtension) {
			try {
				const fname: string = fsUtils.desc2fname({
					contextFolder: desc.contextFolder,
					fileName: desc.fileName,
					fileExtension: ".pvs"
				});
				const res: PvsContextDescriptor = {
					contextFolder: desc.contextFolder,
					fileDescriptors: {}
				};
				res.fileDescriptors[fname] = {
					fileName: desc.fileName,
					fileExtension: ".pvs",
					contextFolder: desc.contextFolder,
					theories: []
				};
				// delete old .tccs file
				fsUtils.deleteFile(fsUtils.desc2fname({
					fileName: desc.fileName,
					fileExtension: ".tccs",
					contextFolder: desc.contextFolder
				}));
				// fetch theory names
				const theories: TheoryDescriptor[] = await fsUtils.listTheoriesInFile(fname);
				const TCC_START_OFFSET: number = 5; // this depends on the size of the header and theory information added before the tccs returned by pvs-server, see this.showTccs
				if (theories) {
					for (let i = 0; i < theories.length; i++) {
						const theoryName: string = theories[i].theoryName;
						// generate .tccs file
						const response: PvsResponse = await this.generateTccsFile({
							fileName: desc.fileName, fileExtension: ".pvs", contextFolder: desc.contextFolder, theoryName
						});
						if (response && !response.error) {
							if (response.result) {
								// console.log(`-- ${desc.fileName}.tccs --`);
								// console.log(response.result);
								// console.log("---------------------------");
								const tccResult: ShowTCCsResult = <ShowTCCsResult> response.result;
								let line: number = TCC_START_OFFSET;
								const tccs: FormulaDescriptor[] = (tccResult) ? tccResult.map(tcc => {
									line += (tcc.comment && tcc.comment.length) ? tcc.comment[0].split("\n").length + 1 
												: tcc["subsumed-tccs"] ? tcc["subsumed-tccs"].split("\n").length + 1
												: 1;
									// const content: string = tcc.definition || "";
									const res: FormulaDescriptor = {
										fileName: desc.fileName,
										fileExtension: ".tccs",
										contextFolder: desc.contextFolder,
										theoryName,
										formulaName: tcc.id,
										position: { line, character: 0 },
										status: tcc["status"], //(tcc.proved) ? "proved" : "untried",
										isTcc: true//,
										// shasum: fsUtils.shasum(content)
									};
									line += (tcc.definition) ? tcc.definition.split("\n").length + 2 : 2;
									return res;
								}): null;
								res.fileDescriptors[fname].theories.push({
									fileName: desc.fileName,
									fileExtension: ".pvs",
									contextFolder: desc.contextFolder,
									theoryName,
									position: null, // tccs are not part of the .pvs file
									theorems: tccs
								});
							} else {
								console.info(`[pvs-language-server.showTccs] No TCCs generated`, response);	
							}
						} else {
							this.pvsErrorManager.handleShowTccsError({ response: <PvsError> response });
						}
					}
				}
				return res;
			} catch (ex) {
				console.error('[pvs-language-server.showTccs] Error: pvsProxy has thrown an exception', ex);
				return null;
			}
		}
		return null;
	}
	/**
	 * Typechecks a given pvs file
	 * @param desc pvs file descriptor: context folder, file name, file extension
	 * @returns response.result !== null indicates file typechecks successfully
	 *          response.error !== null or response == null indicates error
	 */
  	async typecheckFile (desc: PvsFile, opt?: { externalServer?: boolean }): Promise<PvsResponse> {
		opt = opt || {};
		if (desc && desc.fileName && desc.fileExtension && desc.contextFolder) {
			let fname: string = fsUtils.desc2fname(desc);
			if (this.isProtectedFolder(desc.contextFolder)) {
				this.info(`${desc.fileName}${desc.fileExtension} is already typechecked`);
				return {
					jsonrpc: "2.0",
					id: `parse-${desc.contextFolder}`,
					result: `Skipping typechecking of ${desc.contextFolder} (protected pvs folder)`
				};
			}
			if (desc.fileExtension === ".hpvs") {
				// translate file to .pvs and then typecheck
				await this.hp2pvs(desc);
				fname = path.join(desc.contextFolder, `${desc.fileName}.pvs`);
			}

			// show library path and temporary folders, it's useful when debugging problems with importings
			await this.getPvsLibraryPath();
			await this.getPvsTemporaryFolder();
			// typecheck file
			await this.changeContext(desc);
			// NOTE: we need to use the lisp interface because typecheck-file breaks the server
			const res: PvsResponse = await this.legacy?.typecheckFile(fname);
			// const externalServer: boolean = opt.externalServer === undefined ? this.externalServer : !!opt.externalServer;
			// const res: PvsResponse = externalServer ? await this.pvsRequest('typecheck', [ fname ])
			// 	: await this.legacy?.typecheckFile(fname);
			if (res && (res.error && res.error.data) || res.result) {
				if (res.error) {
					// the typecheck error might be generated from an imported file --- we need to check res.error.file_name
					fname = (res.error && res.error.data && res.error.data.file_name) ? res.error.data.file_name : fname;
					if (res.error && res.error.data) {
						if (res.error.data?.error_string?.includes("Error: the assertion")) {
							this.pvsErrorManager.notifyPvsFailure({
								src: "pvs-proxy.typecheck-file",
								msg: res.error.data.error_string
							});
						}
						return {
							jsonrpc: "2.0",
							id: this.get_fresh_id(),
							error: { data: res.error.data }
						};	
					}
				}
			} else {
				const msg: string = `Typechecker was unable to process file ${desc.fileName}${desc.fileExtension}: ${JSON.stringify(res, null, " ")}`;
				console.error(`[pvs-proxy] Error: ${msg}`);
				if (typeof res === "string") {
					const error: languageserver.Diagnostic = {
						range: { start: { line: 1, character: 0 }, end: { line: 1, character: 100 } },
						message: res,
						severity: languageserver.DiagnosticSeverity.Error
					};
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
	 * @deprecated
	 */
	protected async proveFile (desc: { contextFolder: string, fileName: string, fileExtension: string,  }): Promise<PvsResponse> {
		if (desc && desc.fileName && desc.fileExtension && desc.contextFolder) {
			if (this.isProtectedFolder(desc.contextFolder)) {
				this.info(`${desc.contextFolder} is already proved`);
				return null;
			}
			const fname: string = path.join(desc.contextFolder, `${desc.fileName}${desc.fileExtension}`);
			const res: PvsResponse = await this.pvsRequest('lisp', [ `(prove-pvs-file "${fname}" nil)` ]);
			return res;
		}
		return null;
	}
    /**
     * The status-proofchain command provides a proof chain analysis of the
     * formula at the cursor and displays it in the PVS Status buffer.  The
     * proof chain analysis indicates whether the formula has been proved, and
     * analyses the formulas used in the proof to insure that the proof is
     * complete; lemmas used in the proof are proved, and sound, i.e. there are
     * no circularities.
     * Example invocation: (proofchain-status-at "/test/helloworld/helloworld.pvs" nil 12 "pvs")
     */
	async statusProofChain (desc: PvsFormula): Promise<PvsResponse> {
		const res: PvsResponse = await this.legacy.statusProofChain(desc);
        return res;
    }
	/**
	 * Internal function used by getProofStatus
	 * @param formula 
	 */
	protected async proofStatus(formula: PvsFormula): Promise<PvsResponse> {
		if (formula && formula.fileName && formula.fileExtension && formula.contextFolder && formula.theoryName && formula.formulaName) {
			await this.changeContext(formula.contextFolder);
			const fullName: string = path.join(formula.contextFolder, formula.fileName + ".pvs" + "#" + formula.theoryName); // file extension is always .pvs, regardless of whether this is a pvs file or a tcc file
			const ans: PvsResponse = await this.pvsRequest("proof-status", [ fullName, formula.formulaName ]);
			return ans;
		}
		return null;
	}
	/**
	 * Returns the proof status (proved, unfinished, untried, ...) for the given formula
	 * @param formula 
	 */
	async getProofStatus (formula: PvsFormula): Promise<ProofStatus> {
		if (formula) {
			const response: PvsResponse = await this.proofStatus(formula);
			if (response && response.result) {
				switch (response.result) {
					case "subsumed":
					case "simplified":
					// case "proved - incomplete":
					// case "proved - by mapping":
					// case "proved - complete": 
					case "proved":
						return "proved";
					case "unfinished": // proof attempted but failed
						return "unfinished";
					case "unchecked":  // proof was successful, but needs to be checked again because of changes in the theories
						return "unchecked";
					case "unproved":
					case "untried": // proof has not been attempted yet
						return "untried";
				}
			}
		}
		return "untried";
	}
	/**
	 * Starts an interactive prover session for the given formula
	 * @param formula 
	 */
	async proveFormula(formula: PvsFormula, opt?: {
		useLispInterface?: boolean,
		verbose?: boolean
	}): Promise<PvsResponse> {
		if (formula && formula.fileName && formula.fileExtension && formula.contextFolder && formula.theoryName && formula.formulaName) {
			opt = opt || {};
			await this.changeContext(formula.contextFolder);
			const fullName: string = path.join(formula.contextFolder, formula.fileName + ".pvs" + "#" + formula.theoryName); // file extension is always .pvs, regardless of whether this is a pvs file or a tcc file
			if (this.verbose || opt?.verbose) { console.log(`[pvs-proxy] prove-formula [${formula.formulaName}, ${fullName}]`); }
			let ans: PvsResponse = await this.pvsRequest("prove-formula", [ formula.formulaName, fullName ]);
			// if pvs reports that the prover was still open, try to force exit and retry prove-formula
			if (ans?.error?.data?.error_string === "Must exit the prover first") {
				await this.quitProof({ force: true });
				ans = await this.pvsRequest("prove-formula", [ formula.formulaName, fullName ]);
			}
			if (this.verbose || opt?.verbose) { console.dir(ans); }
			if (ans && ans.result && ans.result["length"] === undefined) {
				ans.result = [ ans.result ]; // the prover should return an array of proof states
			}
			if (ans && !ans.error) {
				this.mode = "in-checker";
			}
			return ans;
		}
		return null;
	}

	/**
	 * Clears the list of theories and theorems known to pvs
	 */
	async clearTheories (): Promise<void> {
		console.log(`[pvs-proxy] Resetting cache of theory names`);
		await this.lisp("(clear-theories t)");
	}


	/**
	 * THIS IS NOT SUPPORTED BY PVS-SERVER
	 * Starts an interactive pvsio evaluator session for the given theory
	 * @param desc 
	 */
	// protected async startEvaluator (desc: { contextFolder: string, fileName: string, fileExtension: string, theoryName: string }): Promise<PvsResponse> {
	// 	if (desc) {
	// 		this.notifyStartExecution(`Evaluation environment for ${desc.theoryName}`);
	// 		if (this.isProtectedFolder(desc.contextFolder)) {
	// 			this.info(`${desc.contextFolder} cannot be evaluated`);
	// 			return null;
	// 		}
	// 		// const res: PvsResponse = await this.lisp(`(pvsio "${fname}" t)`);
	// 		// make sure file typechecks correctly
	// 		let ans: PvsResponse = await this.typecheckFile(desc);
	// 		if (ans && !ans.error) {
	// 			// make sure we are in the correct context
	// 			ans = await this.changeContext(desc.contextFolder);
	// 			// disable garbage collector printout
	// 			ans = await this.lisp('(setq *disable-gc-printout* t)');
	// 			// load semantic attachments
	// 			ans = await this.lisp('(load-pvs-attachments)');
	// 			// enter pvsio mode -- do not wait, pvs won't return control
	// 			this.lisp(`(evaluation-mode-pvsio "${desc.theoryName}" nil nil)`); // the fourth argument removes the pvsio banner
	// 		}
	// 		this.notifyEndExecution();
	// 		return ans;
	// 	}
	// 	return null;
	// }

	/**
	 * THIS IS NOT SUPPORTED BY PVS-SERVER
	 * Evaluates a pvs/lisp expression
	 * @param desc Descriptor of the expression
	 */
	// protected async evaluateExpression (desc: { contextFolder: string, fileName: string, fileExtension: string, theoryName: string, cmd: string }): Promise<PvsResponse> {
	// 	if (desc) {
	// 		const cmd = (desc.cmd.endsWith("!") || desc.cmd.endsWith(";")) ? desc.cmd
	// 						: `${desc.cmd};`;
	// 		this.notifyStartExecution(`Executing ${cmd}`);
	// 		// console.dir(desc, { depth: null });
	// 		const res: PvsResponse =  await this.pvsRequest('proof-command', [ desc.cmd ]); //await this.lisp(cmd);
	// 		this.notifyEndExecution();
	// 		return res;
	// 	}
	// 	return null;
	// }

	/**
	 * Returns the status of all proofs in a given theory
	 * @param desc Descriptor specifying context folder and theory name
	 */
	async statusProofTheory (desc: { contextFolder: string, fileName: string, fileExtension: string, theoryName: string }): Promise<PvsResponse> {
		if (desc && desc.fileName && desc.fileExtension && desc.contextFolder && desc.theoryName) {
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
	async changeContext (desc: string | { contextFolder: string }, opt?: { externalServer?: boolean }): Promise<PvsResponse> {
		if (desc) {
			opt = opt || {};
			const ctx: string = (typeof desc === "string") ? desc : desc.contextFolder;
			const externalServer: boolean = opt.externalServer === undefined ? this.externalServer : !!opt.externalServer;
			if (externalServer) {
				return await this.pvsRequest('change-context', [ ctx ]);
			}
			return await this.legacy?.changeContext(ctx);
		}
		return null;
	}

	/**
	 * Returns the current context
	 */
	async currentContext (): Promise<PvsResponse> {
		// const res: PvsResponse = await this.lisp('(pvs-current-directory)');
		const res: PvsResponse = await this.legacy?.lisp('(pvs-current-directory)');
		if (res && res.result && typeof res.result === "string") {
			res.result = res.result.replace(/"/g, ""); // pvs returns the folder name adorned with double quotes
		}
		return res;
	}

	/**
	 * Executes a lisp command in pvs
	 * @param cmd 
	 */
	async lisp(cmd: string, opt?: { externalServer?: boolean }): Promise<PvsResponse> {
		opt = opt || {};
		// const externalServer: boolean = opt.externalServer === undefined ? this.externalServer : !!opt.externalServer;
		// if (externalServer) {
		// 	return await this.pvsRequest('lisp', [ cmd ]);
		// }
		// return await this.legacy?.lisp(cmd);
		// we need to use the lisp interface because the server breaks
		if (this.useLegacy && !opt.externalServer) {
			return await this.legacy?.lisp(cmd);
		}
		return await this.pvsRequest('lisp', [ cmd ]);
	}

	async getServerMode (): Promise<ServerMode> {
		const proverStatus: PvsResponse = await this.getProverStatus();
		// console.log("Prover status: ", proverStatus);
		if (proverStatus === undefined) {
			console.warn(`[pvs-language-server] Warning: prover status is undefined`);
			// const pvsResponse: PvsResponse = await this.pvsProxy.proofCommand({ cmd: "(skip)" });
			// if (pvsResponse && pvsResponse.result && pvsResponse.result.length 
			// 		&& pvsResponse.result[0].commentary && pvsResponse.result[0].commentary.length) {
			// 	return "in-checker";
			// }
		}
		const mode: ServerMode = (proverStatus && proverStatus.result !== "inactive") ?
			"in-checker" : "lisp";
		return mode;
	} 

	// async quitProverIfActive (): Promise<void> {
	// 	// quit prover if prover status is active
	// 	const proverStatus: PvsResult = await this.getProverStatus();
	// 	expect(proverStatus.result).toBeDefined();
	// 	expect(proverStatus.error).not.toBeDefined();
	// 	console.log(proverStatus);
	// 	if (proverStatus && proverStatus.result !== "inactive") {
	// 		await this.proofCommand({ cmd: 'quit' });
	// 	}
	// }

	/**
	 * Quits the prover
	 */
	async quitProof (opt?: { force?: boolean }): Promise<void> {
		// await this.interrupt();
		const mode: string = await this.getMode(); //await this.getServerMode(); //await this.getMode();
		if (mode === "in-checker" || opt?.force) {
			const useLispInterface: boolean = true;
			const response: PvsResponse = await this.proofCommand({ cmd: "(quit)" }, { useLispInterface });
			if (response && response.error) {
				if (response.error.data && !response.error.data.error_string.includes("Prover is not running")) {
					console.warn(`[pvs-proxy] Warning: Unable to quit proof`, response.error.data.error_string);
				}
			}
		}
		this.mode = "lisp";
	}

	/**
	 * Finds a symbol declaration
	 * The result is an object in the form
	 * {
		decl-ppstring:"posnat: TYPE+ = posint"
		declname:"posnat"
		filename:"/Users/usr1/pvs-snapshots/pvs-7.1.0/lib/prelude.pvs"
		place: [2194, 2, 2194, 4]
		theoryid:"integers"
		type:"type"
	 }
	 * @param symbolName Symbol name 
	 */
	// protected findDeclarationQueue: Promise<PvsResponse> = null; // we are using this queue to reduce the strain on the server --- pvs-server tends to crash on MacOS if too many requests are sent in a short time frame
	async findDeclaration (symbolName: string, opt?: { externalServer?: boolean }): Promise<PvsResponse> {
		opt = opt || {};
		// find-declaration breaks the server, we need to use the lisp interface for now
		// const externalServer: boolean = opt.externalServer === undefined ? this.externalServer : !!opt.externalServer;
		// if (externalServer) {
		// 	return await this.pvsRequest('find-declaration', [ symbolName ]);
		// }
		return await this.legacy?.findDeclaration(symbolName);
	}

	async findTheory (theoryName: string): Promise<PvsResponse> {
		const ans: PvsResponse = await this.lisp(`(let ((th (get-theory "${theoryName}")))
(when th
(format nil "~a~a.pvs" (context-path th) (filename th))))`);
		return ans;
	}

	/**
	 * @SAM: FIXME: this function should not automatically invoke the typechecker
	 * @param desc 
	 */
	async termAt (desc: { fileName: string, fileExtension: string, contextFolder: string, line: number, character: number }): Promise<PvsResponse> {
		const fname: string = fsUtils.desc2fname(desc);
		const ans: PvsResponse = await this.pvsRequest("term-at", [ fname, `(${desc.line} ${desc.character})`, 't' ]); 
		return ans;
	}

	/**
     * Returns the help message for a given command
     */
	async showHelpBang (cmd: string): Promise<PvsResponse> {
		const match: RegExpMatchArray = new RegExp(languageUtils.helpBangCommandRegexp).exec(cmd);
		if (match && match.length > 1 && match[1]) {
			this.pvsServer.clearLispInterfaceOutput();
			const helpCmd: string = `(help ${match[1]})`;
			const ans: PvsResponse = await this.pvsRequest('proof-command', [ helpCmd ]);
			if (ans && ans.result && ans.result.length) {
				let help: string = this.pvsServer.getLispInterfaceOutput();
				this.pvsServer.clearLispInterfaceOutput();
				if (languageUtils.isInvalidCommand({ commentary: help })) {
					// check if there's some help string we can provide
					const metaHelp: string = languageUtils.PROOF_COMMANDS[match[1]] ? 
						`(${match[1]})    ${languageUtils.PROOF_COMMANDS[match[1]].description}`
							: null;
					help = metaHelp || `Help not available for ${match[1]}`;
				} else {
					help = help.substring(help.indexOf(`(${match[1]}`), help.indexOf("No change on"));
				}
				ans.result[ans.result.length - 1].action = "";
				ans.result[ans.result.length - 1].commentary = [ help.trim() ];
			}
			return ans;
		}
		return null;
	}

	/**
	 * Executes a proof command. The command is always adorned with round parentheses, e.g., (skosimp*)
	 * @param desc Descriptor of the proof command
	 */
	async proofCommand(desc: { cmd: string }, opt?: { useLispInterface?: boolean }): Promise<PvsResponse> {
		if (desc) {
			opt = opt || {};

			const test: CheckParResult = checkPar(desc.cmd);
			let res: PvsResponse = {
				jsonrpc: "2.0", 
				id: ""
			};
			if (test.success) {
				// console.dir(desc, { depth: null });
				const showHidden: boolean = isShowHiddenFormulas(desc.cmd);
				const showExpandedSequent: boolean = isShowExpandedSequentCommand(desc.cmd);
				// const isGrind: boolean = utils.isGrindCommand(desc.cmd);
				// the following additional logic is a workaround necessary because pvs-server does not know the command show-hidden. 
				// the front-end will handle the command, and reveal the hidden sequents.
				const cmd: string = (showHidden || showExpandedSequent) ? "(skip)" : desc.cmd;
				if (!this.externalServer) { console.log(cmd); }
				res = languageUtils.isHelpBangCommand(cmd) ? await this.showHelpBang(cmd) :
					await this.pvsRequest('proof-command', [ cmd ]);
				if (res && res.result) {
					const proofStates: SequentDescriptor[] = res.result;
					if (showHidden) {
						for (let i = 0; i < proofStates.length; i++) {
							const result: SequentDescriptor = proofStates[i];
							if (result) {
								result.label = "hidden formulas in " + result.label;
								result.action = "Showing list of hidden formulas";
								result.commentary = "Showing list of hidden formulas";
								result.sequent = {
									antecedents: result.sequent ? result.sequent['hidden-antecedents'] : null,
									succedents: result.sequent ? result.sequent['hidden-succedents'] : null
								};
								// if (result.commentary) {
								// 	const hiddenFormulas: string[] = [ languageUtils.formatHiddenFormulas(result) ];					
								// 	result.commentary = hiddenFormulas.concat([ "No change on: (show-hidden-formulas)" ]);
								// }
							}
						}
					} else if (showExpandedSequent) {
						const expanded: PvsResponse = await this.lisp(`(show-expanded-sequent${isShowFullyExpandedSequentCommand(desc.cmd) ? " t" : ""})`);
						if (expanded?.result) {
							for (let i = 0; i < proofStates.length; i++) {
								const result: SequentDescriptor = proofStates[i];
								if (result) {
									result.commentary = isShowFullyExpandedSequentCommand(desc.cmd) ?
										`;;; Fully expanded sequent\n${expanded.result}`
										: expanded.result.replace("; C-u M-x show-expanded-sequent fully expands", "");
								}
							}
						}
						// console.log(res);
					}
					// if (isGrind) {
					// 	for (let i = 0; i < proofStates.length; i++) {
					// 		const result: SequentDescriptor = proofStates[i];
					// 		if (opt.timeout) {
					// 			if (result && result.commentary && typeof result.commentary === "object" 
					// 					&& result.commentary.length && result.commentary[result.commentary.length - 1].startsWith("No change on")) {
					// 				result.action = `No change on: ${desc.cmd}`;
					// 				result.commentary = result.commentary.slice(0, result.commentary.length - 1).concat(`No change on: ${desc.cmd}`);
					// 			}
					// 		}
					// 		result["prev-cmd"] = desc.cmd; // this will remove the timeout applied to grind
					// 	}
					// }
					for (let i = 0; i < proofStates.length; i++) {
						const result: SequentDescriptor = proofStates[i];
						if (languageUtils.QED(result)) {
							this.mode = "lisp";
						}
					}
					if (isQuitCommand(desc.cmd) 
							|| isQuitDontSaveCommand(desc.cmd) 
							|| isSaveThenQuitCommand(desc.cmd)) {
						this.mode = "lisp";
					}
				}
			}
			// FIXME: pvs needs to return errors in a standard way! 
			// e.g., when the prover is active, pvs should always return the sequents, and present errors as commentary.
			// currently, errors are presented in many (incompatible) different ways
			if (res.error || !test.success) {
				res.result = res.result || [{
					"prev-cmd": desc.cmd
				}];
				const error_msg: string = (test.success) ? 
					(res.error && res.error.message) ? res.error.message.replace(/\\\\"/g, "") : null
						: test.msg;
				const i: number = res.result.length - 1;
				res.result[i].commentary = res.result[i].commentary || [];
				res.result[i].commentary.push(`No change on: ${desc.cmd}`);
				res.result[i].commentary.push("Error: " + error_msg);
				// console.error(desc.cmd, error_msg);
			}
			return res;
		}
		return null;
	}

	/**
	 * Returns the prover status
	 */
	async getProverStatus(): Promise<PvsResponse> {
		const ans1: PvsResponse = await this.pvsRequest('prover-status'); // this uses info provided by the xmlrpc server
		const ans2: PvsResponse = await this.legacy?.getProverStatus();  // this other uses the lisp interface to test flag *in-checker*
		// sanity check
		if (ans1 && ans1.result && ans2 && ans2.result) {
			return ans1;
		} else {
			console.warn(`[pvs-proxy] Warning: pvs is not returning a correct prover status`);
			console.dir(ans1);
			console.dir(ans2);
		}
		return (ans1 && ans1.result) ? ans1 : ans2;
	}

	/**
	 * Returns the pvs proof script for a given formula
	 */
	async getDefaultProofScript (formula: PvsFormula, opt?: { externalServer?: boolean }): Promise<PvsResponse> {
		if (formula) {
			opt = opt || {};
			formula = fsUtils.decodeURIComponents(formula);
			// extension is forced to .pvs, this is necessary as the request may come for a .tccs file
			const fname: string = fsUtils.desc2fname({ contextFolder: formula.contextFolder, fileName: formula.fileName, fileExtension: ".pvs" });
			// const externalServer: boolean = opt.externalServer === undefined ? this.externalServer : !!opt.externalServer;
			// if (externalServer) {
			// 	return await this.pvsRequest('proof-script', [ fname, formula.formulaName ]);
			// }
			// return await this.legacy?.getDefaultProofScript(formula);
			if (this.useLegacy && !opt.externalServer) {
				return await this.legacy?.getDefaultProofScript(formula);
			}
			return await this.pvsRequest('proof-script', [ fname, formula.formulaName ]);
		}
		return null;
	}

	/**
	 * Opens a proof file
	 * @param desc Proof file descriptor (name, extension, folder)
	 * @param formula Formula descriptor
	 * @returns Proof descriptor
	 */
	async openProofFile (desc: FileDescriptor, formula: PvsFormula, opt?: { quiet?: boolean }): Promise<ProofDescriptor> {
		let origin: ProofOrigin = ".prf";
		if (desc && desc.fileName && desc.fileExtension && desc.contextFolder 
				&& formula && formula.fileName && formula.fileExtension 
				&& formula.theoryName && formula.formulaName) {
			let pdesc: ProofDescriptor = null;
			try {
				// create proof descriptor
				const isTcc: boolean = languageUtils.isTccFormula(formula);
				switch (desc.fileExtension) {
					case ".prf": {
						const response: PvsResponse = await this.getDefaultProofScript(formula);
						if (response && response.result) {
							const pvsVersionDescriptor: PvsVersionDescriptor = this.getPvsVersionInfo();
							const shasum: string = await fsUtils.shasumFile(formula);
							const jpdesc: ProofDescriptor = await fsUtils.getProofDescriptor(formula);
							// const status: ProofStatus = await this.getProofStatus(formula); <<< we can't use this because pvs doesn't know the proof status until the proof has been re-run
							pdesc = languageUtils.prf2jprf({
								prf: response.result,
								theoryName: formula.theoryName, 
								formulaName: formula.formulaName, 
								version: pvsVersionDescriptor,
								shasum,
								status: jpdesc?.info?.status || "untried"
							});
							// console.log(pdesc);
						}
						if (!languageUtils.isEmptyProof(pdesc)) {
							break;
						}
						// else, try to load from .jprf
					}
					case ".jprf": {
						const fname: string = fsUtils.desc2fname({
							fileName: desc.fileName,
							fileExtension: ".jprf",
							contextFolder: desc.contextFolder
						});
						const proofFile: ProofFile = await fsUtils.readJprfProofFile(fname, opt);
						const key: string = `${formula.theoryName}.${formula.formulaName}`;
						// try to load the proof from the .jprf file
						if (proofFile && proofFile[key] && proofFile[key].length > 0) {
							origin = ".jprf";
							pdesc = new ProofDescriptor (proofFile[key][0].info, origin, proofFile[key][0].proofTree);
						}
						break;
					}
					case ".prl": {
						const fname: string = fsUtils.desc2fname(desc);
						const prl: string = await fsUtils.readProoflite(fname, formula.formulaName);
						if (prl) {
							// FIXME: this does not seem to be working!
							const pvsResponse: PvsResponse = await this.installProofliteScript(formula, prl); // this will load the prl in pvs and save the proof as .prf
							if (pvsResponse && pvsResponse.result) {
								const response: PvsResponse = await this.getDefaultProofScript(formula);
								if (response && response.result) {
									const pvsVersionDescriptor = this.getPvsVersionInfo();
									const shasum: string = await fsUtils.shasumFile(formula);
									pdesc = languageUtils.prf2jprf({
										prf: response.result,
										theoryName: formula.theoryName, 
										formulaName: formula.formulaName, 
										version: pvsVersionDescriptor,
										shasum
									});
									origin = ".prl";
								} else {
									let msg: string = `Error: Unable to associate prooflite script to ${formula.formulaName}`;
									const error_msg: string = (response && response.error && response.error.data && response.error.data.error_string) ?
										response.error.data.error_string : "";
									if (error_msg) {
										msg += ` (${error_msg})`;
									}
									this.connection.sendNotification("server.status.error", msg);
								}
							}
						}
						break;
					}
					default: {
						console.warn(`[proof-explorer] Warning: trying to load unrecognized proof format ${desc.fileExtension}`);
						break;
					}
				}
			} catch (err) {
				console.error(`[pvs-server] Error while fetching proof information.`, err);
			} finally {
				if (!pdesc) {
					const shasum: string = await fsUtils.shasumFile(formula);
					const pvsVersionDescriptor = this.getPvsVersionInfo();		
					pdesc = new ProofDescriptor ({
						theory: formula.theoryName,
						formula: formula.formulaName,
						status: "untried",
						prover: languageUtils.pvsVersionToString(pvsVersionDescriptor) || "PVS 7.x",
						shasum
					}, origin);
				}
				return pdesc;
			}
		}
		return null;
	}

	async newProof (formula: PvsFormula): Promise<ProofDescriptor> {
		formula = fsUtils.decodeURIComponents(formula);
		const shasum: string = await fsUtils.shasumFile(formula);
		const pvsVersionDescriptor = this.getPvsVersionInfo();
		const status: ProofStatus = await this.getProofStatus(formula);
		const empty_pdesc: ProofDescriptor = new ProofDescriptor ({
			theory: formula.theoryName,
			formula: formula.formulaName,
			status,
			prover: languageUtils.pvsVersionToString(pvsVersionDescriptor) || "PVS 7.x",
			shasum
		}, ".prf");
		return empty_pdesc;
	}

	/**
	 * Opens the proof script for the formula indicated in the request
	 * The function looks for the proof in the .jprf file first.
	 * If the proof is not in the .jprf file, then checks the .prf file and updates the jprf file with the obtained information
	 * @param formula 
	 */
	// async openProof (formula: PvsFormula, opt?: {
	// 	quiet?: boolean,
	// 	newProof?: boolean
	// }): Promise<ProofDescriptor> {
	// 	if (formula) {
	// 		opt = opt || {};
	// 		formula = fsUtils.decodeURIComponents(formula);
	// 		const shasum: string = await fsUtils.shasumFile(formula);
	// 		const pvsVersionDescriptor = this.getPvsVersionInfo();
	// 		const status: ProofStatus = await this.getProofStatus(formula);

	// 		// to begin with, create an empty proof
	// 		const empty_pdesc: ProofDescriptor = new ProofDescriptor ({
	// 			theory: formula.theoryName,
	// 			formula: formula.formulaName,
	// 			status,
	// 			prover: utils.pvsVersionToString(pvsVersionDescriptor) || "PVS 7.x",
	// 			shasum
	// 		});

	// 		if (opt.newProof) {
	// 			return empty_pdesc;
	// 		} else {
	// 			// console.log(`[pvs-proxy] Trying to load proof for formula ${formula.formulaName} from .jprf file`);
	// 			let pdesc: ProofDescriptor = await this.openProofFile({
	// 				fileName: formula.fileName,
	// 				fileExtension: ".prf",
	// 				contextFolder: formula.contextFolder
	// 			}, formula, opt);
	// 			if (!pdesc || pdesc.isEmpty()) {
	// 				// try to load from the .prf legacy file
	// 				// console.log(`[pvs-proxy] Trying to load proof for formula ${formula.formulaName} from legacy .prf file`);
	// 				pdesc = await this.openProofFile({
	// 					fileName: formula.fileName,
	// 					fileExtension: ".jprf",
	// 					contextFolder: formula.contextFolder
	// 				}, formula, opt);
	// 			}
	// 			return (!pdesc || pdesc.isEmpty()) ? empty_pdesc : pdesc;
	// 		}
	// 	} else {
	// 		console.warn(`[pvs-server] Warning: load-proof received null request`);
	// 	}
	// 	return null;
	// }

	/**
	 * Saves the last proof executed by the prover in a .prf file
	 * NOTE: this function can be used only after quitting the current proof
	 * @param formula Proof descriptor
	 */
	async storeLastAttemptedProof (formula: PvsFormula): Promise<PvsResponse> {
		if (formula && formula.fileName && formula.contextFolder && formula.formulaName && formula.theoryName) {
			// desc = fsUtils.decodeURIComponents(desc);
			// const fname: string = path.join(desc.contextFolder, `${desc.fileName}.jprf`);
			// let proofFile: ProofFile = await utils.readProofFile(fname);
			// proofFile = proofFile || {};
			// const key: string = `${desc.theoryName}.${desc.formulaName}`;
			// // update date in proof descriptor
			// desc.proofDescriptor.info.date = new Date().toISOString();
			// // TODO: implement mechanism to save a specific proof?
			// proofFile[key] = [ desc.proofDescriptor ];
			// const success: boolean = await fsUtils.writeFile(fname, JSON.stringify(proofFile, null, " "));
			// await this.proofCommand({ cmd: "(quit)" });
			const fullTheoryName: string = path.join(formula.contextFolder, formula.fileName + ".pvs" + "#" + formula.theoryName);
			const response: PvsResponse = await this.pvsRequest("store-last-attempted-proof", [ formula.formulaName, fullTheoryName, 't' ]);
			if (response && response.result) {
				await this.pvsRequest("change-context", [ formula.contextFolder ]);
				await this.pvsRequest("save-all-proofs", [ fullTheoryName ]); // it's pointless to return the result of 'save-all-proofs' because the result is null all the times
			}
			return response;
		}
		// else
		console.error("[pvs-language-server] Warning: save-proof invoked with null or incomplete descriptor", formula);
		return null;
	}
	/**
	 * Saves the given proof script in prooflite (.prl) format
	 * @param desc Proof descriptor
	 */
	async saveProoflite (desc: { 
		fileName: string, 
		fileExtension: ".prl", 
		theoryName: string, 
		formulaName: string, 
		contextFolder: string, 
		proofDescriptor: ProofDescriptor
	}, opt?: { 
		usePvsBinFolder?: boolean
	}): Promise<FileDescriptor> {
		const prlFile: FileDescriptor = fsUtils.getProofliteFileName(desc, opt);
		// opt = opt || {};
		// // by default, save under pvsbin
		// opt.usePvsBinFolder = opt.usePvsBinFolder === undefined ? true : !!opt.usePvsBinFolder;
		// const contextFolder: string = opt.usePvsBinFolder ? path.join(desc.contextFolder, "pvsbin") : desc.contextFolder;
		// // save prooflite
		// const prlFile: FileDescriptor = {
		// 	contextFolder,
		// 	fileName: desc.theoryName,
		// 	fileExtension: ".prl"
		// };
		const content: string[] = languageUtils.proofDescriptor2ProofLite(desc.proofDescriptor);
		if (content && content.length) {
			const header: string = languageUtils.makeProofliteHeader(
				`${desc.fileName}.pvs`,
				desc.formulaName, 
				desc.theoryName, 
				desc.proofDescriptor.info.status
			);
			const proofLite: string = content.join("\n");
			const success: boolean = await fsUtils.saveProoflite(fsUtils.desc2fname(prlFile), desc.formulaName, header + proofLite);
			// try to save into .prf -- disabled for now
			// const prl: string = utils.proofTree2Prl(desc.proofDescriptor);
			// await this.saveProofWithFormula(desc, prl);
			if (success) {
				prlFile.fileContent = proofLite;
				return prlFile;
			}
		}
		return null;
	}
	/**
	 * Saves the given proof script in .prf format via prooflite
	 * @param desc Proof descriptor
	 */
	// async saveProofliteAsPrf (desc: { 
	// 	fileName: string, 
	// 	fileExtension: ".prf", 
	// 	theoryName: string, 
	// 	formulaName: string, 
	// 	contextFolder: string, 
	// 	proofDescriptor: ProofDescriptor
	// }): Promise <PvsResponse> {
	// 	const prl: string = utils.proofTree2Prl(desc.proofDescriptor);
	// 	const pvsResponse: PvsResponse = await this.installProofliteScript(desc, prl); // this will load the prl in pvs ans save the proof as .prf
	// 	return pvsResponse;
	// }
	/**
	 * Saves the given proof script in .prf (legacy) format
	 * @param desc Proof descriptor
	 */
	// async saveProofAsPrf (desc: { 
	// 	fileName: string, 
	// 	fileExtension: ".prf", 
	// 	theoryName: string, 
	// 	formulaName: string, 
	// 	contextFolder: string, 
	// 	proofDescriptor: ProofDescriptor
	// }): Promise <PvsResponse> {
	// 	// TODO -- rerun entire proof and use 'save-all-proofs'
	// 	return this.saveProofliteAsPrf(desc);
	// }
	/**
	 * Returns the prooflite script for the given formula -- FIXME: display-prooflite-script is not working, we need to use languageUtils.proofTree2ProofLite()
	 */
	async proofLiteScript(desc: { contextFolder: string, fileName: string, fileExtension: string, theoryName: string, formulaName: string }): Promise<PvsResponse> {
		if (desc) {
			// extension is forced to .pvs, this is necessary as the request may come for a .tccs file
			const fname: string = fsUtils.desc2fname({ contextFolder: desc.contextFolder, fileName: desc.fileName, fileExtension: ".pvs" });
			const res: PvsResponse = await this.lisp(`(display-prooflite-script "${fname}#${desc.theoryName}" "${desc.formulaName}")`);
			return res;
		}
		return null;
	}

	/**
	 * Generate a .tccs file for the given theory
	 * @param desc 
	 */
	async generateTccsFile (desc: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string }): Promise<PvsResponse> {
		desc.fileExtension = ".pvs"; // tccs can be generated only for .pvs files
		desc = fsUtils.decodeURIComponents(desc);
		const res: PvsResponse = await this.showTccs(desc);
		// create tccs files
		try {
			if (res && res.result) {
				const result: ShowTCCsResult = res.result;
				const keys: string[] = Object.keys(result);
				let content: string = "";
				for (let i = 0; i < keys.length; i++) {
					if (result[i]["subsumed-tccs"]) {
						content += result[i]["subsumed-tccs"] + "\n\n";
					} else {
						content += result[i].comment;
						if (result[i].id) {
							const formulaName: string = result[i].id;
							// try to fetch the last know status from  the .jprf file
							const lastKnownStatus: ProofStatus = await fsUtils.getProofStatus({
								fileName: desc.fileName,
								fileExtension: ".tccs",
								contextFolder: desc.contextFolder,
								theoryName: desc.theoryName,
								formulaName
							});
							result[i]["status"] = result[i].proved ? "proved"
													: lastKnownStatus || "unfinished"; // we are artificially adding this field because pvs-server does not provide it
							const status: string = result[i]["status"]; //lastKnownStatus || (result[i].proved) ? "proved" : "untried";
							content += `\n  % ${status}\n`;
							content += `${formulaName}: OBLIGATION\n${result[i].definition}\n\n`;
						}
					}
				}
				if (content.trim()) {
					// indent lines
					content = "\n\t" + content.split("\n").map(line => { return `\t${line}`; }).join("\n").trim() + "\n";
				} else {
					content = `%-- No TCC was generated for theory ${desc.theoryName}`;
				}
				// add theory information
				content = `${desc.theoryName}_TCCS: THEORY BEGIN\n` + content + `\nEND ${desc.theoryName}_TCCS`;
				// add header
				const header: string = `%% TCCs associated with theory ${desc.theoryName}\n%% This file was automatically generated by PVS, please **do not modify** by hand.\n`;
				// write .tccs file
				const fname: string = path.join(desc.contextFolder, `${desc.fileName}.tccs`);
				await fsUtils.writeFile(fname, header + content, { append: true });
			}
		} catch (error) {
			console.warn(`[pvs-language-server] Warning: something when wrong while writing the TCCs file`, error);
		} finally {
			return res;
		}
	}
	protected async showTccs(desc: PvsTheory, opt?: { externalServer?: boolean }): Promise<PvsResponse> {
		if (desc) {
			opt = opt || {};

			await this.changeContext(desc.contextFolder);

			const fullName: string = path.join(desc.contextFolder, desc.fileName + ".pvs" + "#" + desc.theoryName); // file extension is always .pvs, regardless of whether this is a pvs file or a tcc file
			// const externalServer: boolean = opt.externalServer === undefined ? this.externalServer : !!opt.externalServer;
			// if (externalServer) {
			// 	return await this.pvsRequest('show-tccs', [ fullName ]);
			// }
			// return await this.legacy?.showTccs(fsUtils.desc2fname(desc), desc.theoryName);
			if (this.useLegacy && !opt.externalServer) {
				return await this.legacy?.showTccs(fsUtils.desc2fname(desc), desc.theoryName);
			}
			return await this.pvsRequest('show-tccs', [ fullName ]);
		}
		return null;
	}

	/**
	 * Discharge all tccs generated for the given theory
	 * @param desc 
	 */
	// async proveTccs(desc: { contextFolder: string, fileName: string, fileExtension: string }): Promise<PvsResponse> {
	// 	if (desc) {
	// 		const fname: string = path.join(desc.contextFolder, `${desc.fileName}${desc.fileExtension}`);
	// 		const res: PvsResponse = await this.pvsRequest('prove-tccs', [ fname ]);
	// 		return res;
	// 	}
	// 	return null;
	// }

	/**
	 * Utility function, returns pvs version information
	 */
	getPvsVersionInfo(): PvsVersionDescriptor {
		return this.pvsVersionInfo;
	}

	/**
	 * Utility function, loads library paths and patches
	 */
	async loadPvsLibraryPathAndPatches(): Promise<void> {
		await this.setNasalibPath();
		await this.pushPvsLibraryPath({ useLisp: true });
		await this.loadPvsPatches();
	}

	/**
	 * Loads pvs version information
	 */
	async loadPvsVersionInfo(): Promise<PvsVersionDescriptor | null> {
		let res: PvsResponse = null;
		if (this.externalServer) {
			res = await this.lisp(`(get-pvs-version-information)`);
		} else {
			// try to load nasalib and patches
			await this.loadPvsLibraryPathAndPatches();
			res = await this.legacy?.lisp(`(get-pvs-version-information)`);
		}
		const nasalib: string = await this.getNasalibVersionInfo();
		if (res?.result) {
			const regexp: RegExp = /\(\"?(\d+(?:.?\d+)*)\"?[\s|nil]*\"?([\w\s\d\.]*)\"?/g; // group 1 is pvs version, group 2 is lisp version
			const info: RegExpMatchArray = regexp.exec(res.result);
			// clean up version info
			const version: string = res.result.replace(/\bnil\b/g, "").replace(/\n*/g, "").replace(/\"/g, "").replace(/\s+/g, " ");
			if (info?.length > 2) {
				this.pvsVersionInfo = {
					"pvs-version": info[1].trim(),
					"lisp-version": info[2].replace("International", "").trim(),
					"nasalib-version": nasalib ? "NASALib" : null,
					version: version.substring(1, version.length - 1)
				};
				return this.pvsVersionInfo;
			}
		}
		return null;
	}

	/**
	 * Returns pvs version information
	 */
	async getNasalibVersionInfo (): Promise<string | null> {
		const nasalibPresent: boolean = await this.NasalibPresent();
		if (nasalibPresent) {
			const nasalibVersion: PvsResponse = this.externalServer ? await this.lisp(`*nasalib-version*`)
				: await this.legacy?.lisp(`*nasalib-version*`);
			const regexp: RegExp = /(\d+(?:.?\d+)*)/g; // group 1 is nasalib
			const info: RegExpMatchArray = regexp.exec(nasalibVersion.result);
			if (info && info.length > 1) {
				this.useNasalib = true;
				// check if vscode-output is enabled -- this is disabled for now
				if (this.connection) {
					this.connection.sendNotification(serverEvent.profilerData, `Profiling: xmlrpc-server\n`);
				}
				return info[1];
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

	/**
	 * Checks availability of the port. Returns true if the port is available, false otherwise.
	 * The check works as follows. A dummy server is created at port p; if the creation of the server succeeds, an event 'listening' is triggered, otherwise an event 'error' is triggered.
	 * The server is turned off as soon as an answer is available.
	 */
	protected checkPort (port: number, retry: boolean): Promise<boolean> {
		return new Promise((resolve, reject) => {
			if (this.showBanner) {
				console.log(`[pvs-proxy] Checking port ${port}...`);
			}
			const socket: net.Socket = net.createConnection({ host: this.clientAddress, port });
			socket.once('error', (error: Error) => {
				// noone is serving on the given port
				console.log(`[pvs-proxy] port ${port} is available :)`);
				resolve(true);
			});
			socket.once('connect', () => {
				// sombody is using the port
				console.error(`[pvs-proxy] port ${port} is not available :/`);
				resolve(false);
			});
		});
	}

	/**
	 * Checks if pvs-server accepts connection
	 */
	async testServerConnectivity(): Promise<boolean> {
		const client: xmlrpc.Client = xmlrpc.createClient({
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
	protected async createPvsServer(opt?: { 
		enableNotifications?: boolean, 
		externalServer?: boolean,
		verbose?: boolean
	}): Promise<ProcessCode> {
		opt = opt || {};
		const externalServer: boolean = opt.externalServer === undefined ? this.externalServer : !!opt.externalServer;
		if (externalServer) {
			console.log(`[pvs-proxy] +-- EXTERNAL SERVER CONFIGURATION --`);
			console.log(`[pvs-proxy] |  Address: ${this.serverAddress}`);
			console.log(`[pvs-proxy] |  Port: ${this.serverPort}`);
			console.log(`[pvs-proxy] +-----------------------------------`);
			return ProcessCode.SUCCESS;
		}

		const connection: SimpleConnection = (opt.enableNotifications) ? this.connection : null;
		const proc: PvsProcess = new PvsProcess(this.pvsPath, { connection, pvsErrorManager: this.pvsErrorManager });

		let portIsAvailable: boolean = false;
		for (let i = 0; !portIsAvailable && i < this.MAX_PORT_ATTEMPTS; i++) {
			const success: ProcessCode = await proc.activate({
				enableNotifications: opt.enableNotifications,
				serverPort: this.serverPort,
				externalServer: opt.externalServer,
				verbose: opt.verbose
			});
			if (success === ProcessCode.PVSNOTFOUND) {
				return ProcessCode.PVSNOTFOUND;
			}
			if (success === ProcessCode.UNSUPPORTEDPLATFORM) {
				return ProcessCode.UNSUPPORTEDPLATFORM;
			}
			portIsAvailable = success === ProcessCode.SUCCESS;
			if (portIsAvailable === false) {
				this.serverPort++;
				await proc.kill();
			}
		}
		if (portIsAvailable) {
			if (!opt.externalServer) {
				if (connection) {
					connection.console.info(`[pvs-proxy] pvs-server active at http://${this.serverAddress}:${this.serverPort}`);
				} else {
					console.info(`[pvs-proxy] pvs-server active at http://${this.serverAddress}:${this.serverPort}`);
				}
			} else {
				console.info(`[pvs-proxy] using external pvs-server on port ${this.serverPort}`);
			}
			this.pvsServer = proc;
			return ProcessCode.SUCCESS;
		}
		console.log(`[pvs-proxy] Failed to activate pvs-server at http://${this.serverAddress}:${this.serverPort}`);
		return ProcessCode.ADDRINUSE;
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
		if (this.pvsServer) {// && !this.externalServer) {
			await this.interruptProver();
			await this.pvsServer.kill();
			if (this.debugMode) {
				console.info("[pvs-proxy] Killed pvs-server");
			}
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
					if (this.debugMode) {
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

	async rebootPvsServer (desc: { pvsPath?: string }): Promise<void> {
		this.pvsPath = desc?.pvsPath || "";
		console.log(`[pvs-proxy] New PVS path: ${this.pvsPath}`);
		await this.killPvsServer();
		await this.restartPvsServer();
	}

	protected async sendWorkspaceInfo (): Promise<void> {
		const res: PvsResponse = await this.currentContext();
		if (res && res.result) {
			const contextFolder: string = res.result;
			const contextFiles: FileList = await fsUtils.listPvsFiles(contextFolder);
			const nfiles: number = contextFiles.fileNames.length;
			this.connection.sendRequest(serverEvent.workspaceStats, { contextFolder, files: nfiles });
		}
	}

	/**
	 * Utility function, sets the current mode to 'lisp'
	 */
	lispMode (): void {
		this.mode = "lisp";
	}

	/**
	 * Utility function, restarts the xml-rpc server
	 * @param opt 
	 */
	async restartPvsServer (opt?: { pvsPath?: string, pvsLibraryPath?: string, externalServer?: boolean }): Promise<boolean> {
		opt = opt || {};

		// set server mode to "lisp"
		this.mode = "lisp";

		// update pvsPath and pvsLibraryPath with the provided info
		if (opt) {
			if (opt.pvsPath) {
				this.pvsPath = opt.pvsPath;
				console.log(`[pvs-proxy] PVS path: ${this.pvsPath}`);
			}
			if (opt.pvsLibraryPath) {
				this.pvsLibraryPath = opt.pvsLibraryPath;
				console.log(`[pvs-proxy] PVS library path: ${this.pvsLibraryPath}`);
			}
		}

		console.info("[pvs-proxy] Rebooting pvs-server...");
		const serverPort: number = this.serverPort;
		const serverAddress: string = this.serverAddress;

		// create xml-rpc server
		// if externalServer === true then method createPvsServer will create only 
		// the pvs process necessary to parse/typecheck files.
		// Otherwise, createPvsServer creates also the xml-rpc server.
		const pvsProcessActive: ProcessCode = await this.createPvsServer({
			enableNotifications: true,
			externalServer: this.externalServer,
			verbose: this.verbose
		});
		if (pvsProcessActive !== ProcessCode.SUCCESS) {
			this.pvsErrorManager.handleStartPvsServerError(pvsProcessActive);
		}
		// activate proxy for Lisp interface
		await this.legacy?.activate(this.pvsServer, {
			pvsErrorManager: this.pvsErrorManager
		});

		// create GUI server necessary for interacting with the xml-rpc server
		if (this.client && (this.serverPort !== serverPort || this.serverAddress !== serverAddress)) {
			// port has changed, we need to update the client
			this.client = xmlrpc.createClient({
				host: this.serverAddress, port: this.serverPort, path: "/RPC2"
			});
			if (!this.client) {
				console.error(`[pvs-proxy] Error: could not create client necessary to connect to pvs-server`);
				this.pvsErrorManager.handleStartPvsServerError(ProcessCode.COMMFAILURE);
			}
		}
		
		const success: boolean = await this.createGuiServer();
		if (success) { console.info("[pvs-proxy] Reboot complete!"); }
		return success;
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
	 * Internal function, returns a unique ID
	*/
	protected get_fresh_id(): string {
		return crypto.createHash('sha256').update(Math.random().toString(36)).digest('hex');
	}
	/**
	 * Utility function, checks if the context folder is the pvs installation folder (pvsPath) or the internal library (pvsLibPath)
	 * @param contextFolder 
	 */
	isProtectedFolder(contextFolder: string): boolean {
		return contextFolder === this.pvsPath || contextFolder === path.join(this.pvsPath, "lib");
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

	async getPvsTemporaryFolder (opt?: { externalServer?: boolean }): Promise<string[]> {
		const ans: string[] = [];
		let response: PvsResponse = await this.lisp(`uiop:*temporary-directory*`);
		ans.push(response?.result);
		response = await this.lisp(`(uiop:default-temporary-directory)`);
		ans.push(response?.result);
		response = await this.lisp(`(uiop:temporary-directory)`);
		ans.push(response?.result);
		return ans;
	}
	async getPvsLibraryPath (opt?: { externalServer?: boolean }): Promise<string[]> {
		const response: PvsResponse = await this.lisp(`*pvs-library-path*`, opt);
		if (response && response.result) {
			const match: RegExpMatchArray = /\(([\w\W\s]+)\)/g.exec(response.result);
			if (match && match.length > 1 && match[1]) {
				const pvsLibraries: string[] = match[1].replace(/\s+/g, " ").split(`"`).filter((elem: string) => {
					return elem.trim();
				});
				// console.log(pvsLibraries);
				return pvsLibraries;
			}
		}
		return [];
	}
	getNasalibPath (): string {
		return fsUtils.tildeExpansion(path.join(this.pvsPath, "nasalib/"));
	}

	async setNasalibPath (opt?: { externalServer?: boolean }): Promise<PvsResponse | null> {
		const present: boolean = await this.NasalibPresent();
		if (!present) {
			const pvsLibraries: string[] = await this.getPvsLibraryPath(opt);
			const nasalibPath: string = this.getNasalibPath();
			if (!pvsLibraries.includes(nasalibPath) && fsUtils.folderExists(nasalibPath)) {
				console.log(`[pvs-proxy] Setting nasalib path to ${nasalibPath}`);
				// return await this.lisp(`(push "${nasalibPath}" *pvs-library-path*)`, opt);
				return await this.pushPvsLibraryPath({ pvsLibraryPath: nasalibPath, useLisp: true });
			}
		}
		return null;
	}

	async NasalibPresent (): Promise<boolean> {
		const response: PvsResponse = this.externalServer ? await this.lisp(`(boundp '*nasalib-version*)`)
			: await this.legacy?.lisp(`(boundp '*nasalib-version*)`);
		return response && response.result === "t";
	}

	async installProofliteScript (desc: PvsFormula, proofLiteScript: string): Promise<PvsResponse> {
		const nasalibPresent: boolean = await this.NasalibPresent();
		if (nasalibPresent) {
			if (this.externalServer) {
				// TODO: investigate if this can be done when using the server
				return null;
			}
			return await this.legacy?.installProofliteScript(desc, proofLiteScript);
		}
		return null;
	}

	/**
	 * Updates pvs library path
	 * @param pvsLibraryPath colon-separated list of folders
	 */
	async pushPvsLibraryPath (opt?: { pvsLibraryPath?: string, useLisp?: boolean, externalServer?: boolean }): Promise<PvsResponse | null> {
		opt = opt || {};
		const lp: string = opt.pvsLibraryPath || this.pvsLibraryPath;
		const libs: string[] = (lp) ? lp.split(":").map((elem: string) => {
			elem = elem.trim();
			elem = fsUtils.tildeExpansion(elem);
			return elem.endsWith("/") ? elem : `${elem}/`
		}) : [];
		if (libs && libs.length) {
			const pvsLibraries: string[] = await this.getPvsLibraryPath(opt);
			let res: PvsResponse = null;
			for (let i = 0; i < libs.length; i++) {
				let path: string = libs[i].endsWith("/") ? libs[i] : `${libs[i]}/`;
				path = fsUtils.tildeExpansion(path);
				// console.log({ path, pvsLibraries });
				if (!pvsLibraries.includes(path)) {
					// console.log(`[pvs-proxy] Adding library path ${path}`);
					res = (opt.useLisp) ? await this.legacy?.lisp(`(push "${path}" *pvs-library-path*)`)
						: await this.pvsRequest('add-pvs-library', [ path ]);
				}
			}
			return res;
		}
		return null;
	}

	/**
	 * Utility function, returns true if any pvs patch has been loaded
	 */
	async patchesLoaded (): Promise<boolean> {
		const response: PvsResponse = await this.legacy?.lisp(`(boundp '*pvs-patches*)`);
		return response && response.result === "t";
	}

	/**
	 * Utility function, loads pvs patches stored in the default location (folder 'pvs-patches')
	 * @param opt 
	 * @returns 
	 */
	async loadPvsPatches (opt?: { externalServer?: boolean }): Promise<PvsResponse> {
		const alreadyLoaded: boolean = await this.patchesLoaded();
		if (!alreadyLoaded) {
			return await this.lisp(`(load-pvs-patches)`, opt);
		}
		return null;
	}

	/**
	 * pvs-proxy activation function
	 * @param opt 
	 */
	async activate(opt?: { debugMode?: boolean, showBanner?: boolean, verbose?: boolean, pvsErrorManager?: PvsErrorManager }): Promise<boolean> {
		opt = opt || {};
		console.log(`[pvs-proxy] Starting pvs-proxy...`);
		this.showBanner = (opt.showBanner === undefined) ? true : opt.showBanner;
		this.debugMode = !!opt.debugMode;
		this.verbose = !!opt.verbose;
		this.pvsErrorManager = opt.pvsErrorManager;
		if (this.pvsServer) {
			return Promise.resolve(true);
		}
		const success: boolean = await this.restartPvsServer();
		if (success) {
			console.log(`[pvs-proxy] Ready!`);
		} else {
			console.error(`[pvs-proxy] Failed to start proxy :/`);
		}
		return success;
	}

	async createGuiServer (opt?: { debugMode?: boolean, showBanner?: boolean }): Promise<boolean> {
		opt = opt || {};
		if (this.client) {
			return Promise.resolve(true);
		}
		return new Promise(async (resolve, reject) => {
			let portIsAvailable: boolean = (this.guiServer) ? true : false;
			for (let i = 0; !portIsAvailable && i < this.MAX_PORT_ATTEMPTS; i++) {
				portIsAvailable = await this.checkPort(this.clientPort, true);
				if (portIsAvailable === false) {
					this.clientPort++;
				}
			}
			if (portIsAvailable) {
				this.banner = `GUI Server active at http://${this.clientAddress}:${this.clientPort}`;
				console.log(`[pvs-proxy] Starting GUI Server on http://${this.clientAddress}:${this.clientPort}`);
				this.client = xmlrpc.createClient({
					host: this.serverAddress, port: this.serverPort, path: "/RPC2"
				});
				if (!this.client) {
					console.error(`[pvs-proxy] Error: could not create client necessary to connect to pvs-server`);
					resolve(false);
				}
				if (!this.guiServer) {
					try {
						this.guiServer = xmlrpc.createServer({
							host: this.clientAddress, port: this.clientPort, path: "/RPC2"
						}, () => {
							this.serverReadyCallBack();
							if (opt.showBanner) {
								console.log("[pvs-proxy] " + this.banner);
							}
							resolve(true);
						});
						this.guiServer.once('error', (error: Error) => {
							console.error(error);
							if (error["code"] === 'EADDRINUSE') {
								console.log(`[pvs-proxy] port ${this.clientPort} busy`);
							}
						});
					} catch (gui_server_error) {
						console.error(`[pvs-proxy]`, gui_server_error);
						resolve(false);
					}
				}
			} else {
				console.error(`[pvs-proxy] Error: could not start GUI-server`);
				resolve(false);
			}
		});
	}

	/**
	 * interrupts the prover
	 */
	async interruptProver (): Promise<PvsResponse | null> {
		return await this.pvsRequest('interrupt');
	}

}
