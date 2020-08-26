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
import { SimpleConnection, serverEvent, PvsVersionDescriptor, ProofStatus, ProofDescriptor, ProofFile, PvsFormula, ServerMode, TheoryDescriptor, PvsTheory, FormulaDescriptor, PvsFile, PvsContextDescriptor } from './common/serverInterface';
import { Parser } from './core/Parser';
import * as languageserver from 'vscode-languageserver';
import { ParserDiagnostics } from './core/pvs-parser/javaTarget/pvsParser';
import { getErrorRange, SequentDescriptor } from './common/languageUtils';
import * as utils from './common/languageUtils';
import { PvsProxyLegacy } from './legacy/pvsProxyLegacy';
import * as os from 'os';
import { PvsErrorManager } from './pvsErrorManager';

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
	protected externalServer: boolean;
	protected cliListener: (data: string) => void; // useful to show progress feedback

	protected pvsErrorManager: PvsErrorManager;
	// protected buffer: Promise<PvsResponse> = Promise.resolve(null); // this is a queue used to serialize the requests sent to the server

	protected pvsVersionInfo: PvsVersionDescriptor = null;

	protected legacy: PvsProxyLegacy;
	protected useLegacy: boolean = true;
	protected useNasalib: boolean = false;
	protected jsonOutputAvailable: boolean = false;

	protected mode: ServerMode = "lisp";

	/**
	 * Parser
	 */
	protected parser: Parser;

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
		this.pvsLibPath = path.join(pvsPath, "lib");
		this.nasalibPath = path.join(pvsPath, "nasalib");
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

		this.legacy = new PvsProxyLegacy(pvsPath, opt);
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
		return this.mode;
	}

	async enableExternalServer (): Promise<void> {
		console.info("[pvs-proxy] Enabling external server...");
		await this.killPvsServer();
		this.externalServer = true;
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
				label: (res["result"]) ? "QED" : res["label"], // this should always be the formula name....
				commentary: (res["result"]) ? [ res["result"] ] : res["commentary"],
				"num-subgoals": (res["result"]) ? 0 : res["num-subgoals"],
				sequent: (res["result"]) ? {} : res["sequent"],
				"prev-cmd": res["prev-cmd"],
				"last-cmd": (res["prev-cmd"] && res["prev-cmd"].length === 1) ? res["prev-cmd"][0] : cmd
			}
			return sequent;
		}
		return null;
	}

	//--------------------------------------------------
	//         json-rpc methods
	//--------------------------------------------------
	pvsRequest(method: string, params?: string[]): Promise<PvsResponse> {
		params = params || [];
		const req = { method: method, params: params, jsonrpc: "2.0", id: this.get_fresh_id() };
		// this.buffer = this.buffer.then(() => {
			return new Promise((resolve, reject) => {
				if (this.client) {
					const jsonReq: string = JSON.stringify(req, null, " ");

					if (this.connection) {
						this.connection.sendNotification(serverEvent.proverData, jsonReq);
					}
					// console.dir(jsonReq, { depth: null });

					if (this.verbose && req && req.method !== "prover-status") {
						// console.dir(jsonReq);
						const msg: string = (req.params) ? req.method + " " + JSON.stringify(req.params)
							: req.method;
						console.log(msg);
					}
					this.client.methodCall("pvs.request", [jsonReq, `http://${this.clientAddress}:${this.clientPort}`], (error: Error, value: string) => {
						// console.log(error);
						// console.log(value);

						if (error) {
							if (this.connection) {
								this.connection.sendNotification(serverEvent.proverData, JSON.stringify(error, null, " "));
							}
		
							console.error("[pvs-proxy] Error returned by pvs-server: "); 
							console.dir(error, { depth: null }); 
							if (error['code'] === 'ECONNREFUSED') {
								// if the server refuses the connection, try to reboot
								console.log(`[pvs-proxy] Connection refused when launching pvs from ${this.pvsPath}`);
								resolve({
									jsonrpc: "2.0", 
									id: req.id,
									error
								});
								if (this.pvsErrorManager) {
									this.pvsErrorManager.notifyPvsFailure({ msg: `Error: unable to connect to pvs-server at http://${this.clientAddress}:${this.clientPort}` });
								}
							} else if (error['code'] === 'ECONNRESET') {
								// do nothing -- this usually occurs when the user reboots pvs-server
							} else {
								if (this.pvsErrorManager) {
									this.pvsErrorManager.notifyPvsFailure({ method });
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

								if (this.connection) {
									this.connection.sendNotification(serverEvent.proverData, JSON.stringify(resp, null, " "));
								}

								// console.dir(resp, { depth: null });
								if (resp && (resp["result"] === "null" || resp["result"] === "nil")) {
									// sometimes pvs returns a string "null" or "nil" as result -- here the string is transformed into a proper null value
									resp["result"] = null;
								}

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

								if (this.connection) {
									this.connection.sendNotification(serverEvent.proverData, value);
								}
	
								resolve(null);
							}
						} else {
							if (this.connection) {
								this.connection.sendNotification(serverEvent.proverData, JSON.stringify(error, null, " "));
							}
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
	async parseFile(desc: { 
		contextFolder: string, 
		fileName: string, 
		fileExtension: string 
	}, opt?: { 
		test?: boolean,
		enableEParser?: boolean
	}): Promise<PvsResponse | null> {
		opt = opt || {};
		if (desc && desc.fileName && desc.fileExtension && desc.contextFolder) {
			const fname: string = path.join(desc.contextFolder, `${desc.fileName}${desc.fileExtension}`);
			const fileExists: boolean = await fsUtils.fileExists(fname);
			if (fileExists) {
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
						const res: PvsResponse = (this.useLegacy) ? await this.legacy.parseFile(fname)
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
											console.log(`[pvs-parser] antlr completed parsing in ${antlrdiags["parse-time"].ms}ms`)
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

	/**
	 * Returns the import chain for a given theory
	 * @param desc theory descriptor: context folder, file name, file extension, theory name
	 */
	async getImportChain (desc: PvsTheory): Promise<PvsTheory[]> {
		let importChain: PvsTheory[] = [];
		if (desc && desc.theoryName) {
			// change context
			await this.changeContext(desc);

			// const res: PvsResponse = await this.legacy.lisp(`(prog2 (show-tccs "${fullName}") 
			const res: PvsResponse = await this.legacy.lisp(`
		(let ((theoryname "${desc.theoryName}"))
				(let ((usings (remove-if #'(lambda (th)
				(or (from-prelude? th)
					(lib-datatype-or-theory? th)))
			(collect-theory-usings theoryname nil))))
				(mapcar (lambda (theory) (cons (format nil "|~a|" (id theory)) (shortname (make-specpath (filename theory))))) (reverse usings))))
			`);
			// )`);
			console.log(res.result);
			if (res && res.result) {
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
						const formulaDescriptors: FormulaDescriptor[] = await utils.listTheoremsInFile(fsUtils.desc2fname(theories[i]));
						if (formulaDescriptors && formulaDescriptors.length) {
							ans = ans.concat(formulaDescriptors);
						}
					}
					// generate tccs for the given theory
					const tccsResponse: PvsResponse = await this.tccs(theories[i]);
					if (tccsResponse && tccsResponse.result) {
						const tccsResult: ShowTCCsResult = <ShowTCCsResult> tccsResponse.result;
						for (let j = 0; j < tccsResult.length; j++) {
							if (tccsResult[j].id) {
								ans.push({
									formulaName: tccsResult[j].id,
									theoryName: theories[i].theoryName,
									fileName: theories[i].fileName,
									contextFolder: theories[i].contextFolder,
									fileExtension: theories[i].fileExtension
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
	 * @param args Handler arguments: filename, file extension, context folder
	 */
	async generateTccs (args: PvsFile): Promise<PvsContextDescriptor> {
		args = fsUtils.decodeURIComponents(args);
		if (args && args.contextFolder && args.fileName && args.fileExtension) {
			try {
				const fname: string = fsUtils.desc2fname(args);
				const res: PvsContextDescriptor = {
					contextFolder: args.contextFolder,
					fileDescriptors: {}
				};
				res.fileDescriptors[fname] = {
					fileName: args.fileName,
					fileExtension: args.fileExtension,
					contextFolder: args.contextFolder,
					theories: []
				};
				// fetch theory names
				const theories: TheoryDescriptor[] = await utils.listTheoriesInFile(fname);
				const TCC_START_OFFSET: number = 5; // this depends on the size of the header and theory information added before the tccs returned by pvs-server, see this.showTccs
				if (theories) {
					for (let i = 0; i < theories.length; i++) {
						const theoryName: string = theories[i].theoryName;
						const response: PvsResponse = await this.tccs({
							fileName: args.fileName, fileExtension: args.fileExtension, contextFolder: args.contextFolder, theoryName
						});
						if (response && !response.error) {
							if (response.result) {
								const tccResult: ShowTCCsResult = <ShowTCCsResult> response.result;
								let line: number = TCC_START_OFFSET;
								res.fileDescriptors[fname].theories.push({
									fileName: args.fileName,
									fileExtension: args.fileExtension,
									contextFolder: args.contextFolder,
									theoryName,
									position: null,
									theorems: (tccResult) ? tccResult.map(tcc => {
										line += (tcc.comment && tcc.comment.length) ? tcc.comment[0].split("\n").length + 1 : 1;
										// const content: string = tcc.definition || "";
										const res: FormulaDescriptor = {
											fileName: args.fileName,
											fileExtension: ".tccs",
											contextFolder: args.contextFolder,
											theoryName,
											formulaName: tcc.id,
											position: { line, character: 0 },
											status: tcc["status"], //(tcc.proved) ? "proved" : "untried",
											isTcc: true//,
											// shasum: fsUtils.shasum(content)
										};
										line += (tcc.definition) ? tcc.definition.split("\n").length + 2 : 2;
										return res;
									}): null
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
	 */
  	async typecheckFile (desc: { contextFolder: string, fileName: string, fileExtension: string }): Promise<PvsResponse> {
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

			const res: PvsResponse = (this.useLegacy) ? await this.legacy.typecheckFile(fname)
					: await this.pvsRequest('typecheck', [ fname ]);
			if (res && (res.error && res.error.data) || res.result) {
				if (res.error) {
					// the typecheck error might be generated from an imported file --- we need to check res.error.file_name
					fname = (res.error && res.error.data && res.error.data.file_name) ? res.error.data.file_name : fname;
					if (res.error && res.error.data) {
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
	 * Starts an interactive prover session for the given formula
	 * @param desc 
	 */
	async proveFormula(desc: PvsFormula, opt?: {
		useLispInterface?: boolean
	}): Promise<PvsResponse> {
		if (desc && desc.fileName && desc.fileExtension && desc.contextFolder && desc.theoryName && desc.formulaName) {
			opt = opt || {};
			await this.changeContext(desc.contextFolder);
			if (this.useNasalib && this.jsonOutputAvailable && opt.useLispInterface) {
				const res: PvsResponse = await this.legacy.proveFormula(desc);
				// console.dir(res);
				return res;
			} else {
				const fullName: string = path.join(desc.contextFolder, desc.fileName + ".pvs" + "#" + desc.theoryName); // file extension is always .pvs, regardless of whether this is a pvs file or a tcc file
				const ans: PvsResponse = await this.pvsRequest("prove-formula", [ desc.formulaName, fullName ]);
				// if (this.verbose) { console.dir(ans); }
				if (ans && ans.result && ans.result["length"] === undefined) {
					ans.result = [ ans.result ]; // the prover should return an array of proof states
				}
				if (ans && !ans.error) {
					this.mode = "in-checker";
				}
				return ans;
			}
		}
		return null;
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
	async changeContext (desc: string | { contextFolder: string }): Promise<PvsResponse> {
		if (desc) {
			const ctx: string = (typeof desc === "string") ? desc : desc.contextFolder;
			if (this.useLegacy) {
				return await this.legacy.changeContext(ctx);
			}
			return await this.pvsRequest('change-context', [ ctx ]);
		}
		return null;
	}

	/**
	 * Returns the current context
	 */
	async currentContext (): Promise<PvsResponse> {
		// const res: PvsResponse = await this.lisp('(pvs-current-directory)');
		const res: PvsResponse = await this.legacy.lisp('(pvs-current-directory)');
		if (res && res.result && typeof res.result === "string") {
			res.result = res.result.replace(/"/g, ""); // pvs returns the folder name adorned with double quotes
		}
		return res;
	}

	/**
	 * Executes a lisp command in pvs
	 * @param cmd 
	 */
	async lisp(cmd: string, opt?: { useXmlrpc?: boolean }): Promise<PvsResponse> {
		opt = opt || {};
		// don't use legacy apis here --- this command might be used during a prover session, and pvs process is unable to process requests during prover sessions
		if (this.useLegacy && !opt.useXmlrpc) {
			return await this.legacy.lisp(cmd);
		}
		// else
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

	async quitProofIfInProver (): Promise<void> {
		if (await this.getMode() === "in-checker") {
			const useLispInterface: boolean = true;
			const response: PvsResponse = await this.proofCommand({ cmd: "(quit)" }, { useLispInterface });
			if (response && response.error && this.pvsErrorManager) {
				this.pvsErrorManager.handleProofCommandError({ cmd: "(quit)", response: <PvsError> response });
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
	async findDeclaration (symbolName: string): Promise<PvsResponse> {
		// we want to use the server, otherwise find-declaration won't work while in a prover session
		const ans: PvsResponse = (this.useLegacy) ? await this.legacy.findDeclaration(symbolName)
			: await this.pvsRequest('find-declaration', [ symbolName ]);

		// const promiseFindDeclaration: Promise<PvsResponse> = new Promise ((resolve, reject) => {
		// 	setTimeout(async () => {
		// 		const ans: PvsResponse = await this.pvsRequest('find-declaration', [ symbolName ]);
		// 		// console.log(ans);
		// 		if (ans && ans.result) {
		// 			if (typeof ans.result !== "object") {
		// 				// disabling this warning for now, as we know how to recover from this bug
		// 				// console.error(`[pvs-proxy] Warning: pvs-server returned malformed result for find-declaration (expecting object found ${typeof ans.result})`);
		// 			}
		// 			if (typeof ans.result === "string") {
		// 				ans.result = JSON.parse(ans.result);
		// 			}
		// 		}
		// 		resolve(ans);
		// 	}, 200);
		// })
		// this.findDeclarationQueue = (this.findDeclarationQueue) ? this.findDeclarationQueue.then(() => {
		// 	return promiseFindDeclaration;
		// }): promiseFindDeclaration;
		// return this.findDeclarationQueue;

		// const ans: PvsResponse = await this.pvsRequest('find-declaration', [ symbolName ]);
		// if (ans && ans.result) {
		// 	if (typeof ans.result !== "object") {
		// 		// disabling this warning for now, as we know how to recover from this bug
		// 		// console.error(`[pvs-proxy] Warning: pvs-server returned malformed result for find-declaration (expecting object found ${typeof ans.result})`);
		// 	}
		// 	if (typeof ans.result === "string") {
		// 		ans.result = JSON.parse(ans.result);
		// 	}
		// }
		return ans;
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
	 * Executes a proof command. The command is always adorned with round parentheses, e.g., (skosimp*)
	 * @param desc Descriptor of the proof command
	 */
	async proofCommand(desc: { cmd: string }, opt?: { timeout?: number, useLispInterface?: boolean }): Promise<PvsResponse> {
		if (desc) {
			opt = opt || {};

			const test: { success: boolean, msg: string } = utils.parCheck(desc.cmd);
			let res: PvsResponse = {
				jsonrpc: "2.0", 
				id: ""
			};
			if (test.success) {
				// console.dir(desc, { depth: null });
				const showHidden: boolean = utils.isShowHiddenCommand(desc.cmd);
				const isGrind: boolean = utils.isGrindCommand(desc.cmd);
				// the following additional logic is a workaround necessary because pvs-server does not know the command show-hidden. 
				// the front-end will handle the command, and reveal the hidden sequents.
				const cmd: string = showHidden ? "(skip)"
					: isGrind ? utils.applyTimeout(desc.cmd, opt.timeout)
						: desc.cmd;
				res = (this.useNasalib && this.jsonOutputAvailable && opt.useLispInterface) ? await this.legacy.proofCommand(cmd) 
					: await this.pvsRequest('proof-command', [ cmd ]);
				if (res && res.result) {
					const proofStates: SequentDescriptor[] = res.result;
					if (showHidden) {
						for (let i = 0; i < proofStates.length; i++) {
							const result: SequentDescriptor = proofStates[i];
							if (result) {
								result.action = "Showing list of hidden sequents";
								if (result.commentary && result.commentary.length) {
									result.commentary[0] = "No change on: (show-hidden)";
								}
							}
						}
					}
					if (isGrind) {
						for (let i = 0; i < proofStates.length; i++) {
							const result: SequentDescriptor = proofStates[i];
							if (opt.timeout) {
								if (result && result.commentary 
										&& result.commentary.length 
										&& result.commentary[result.commentary.length - 1].startsWith("No change on")) {
									result.action = `No change on: ${desc.cmd}`;
									result.commentary = result.commentary.slice(0, result.commentary.length - 1).concat(`No change on: ${desc.cmd}`);
								}
							}
							result["last-cmd"] = desc.cmd; // this will remove the timeout applied to grind
						}
					}
					for (let i = 0; i < proofStates.length; i++) {
						const result: SequentDescriptor = proofStates[i];
						if (utils.QED(result)) {
							this.mode = "lisp";
						}
					}
					if (utils.isQuitCommand(desc.cmd) 
							|| utils.isQuitDontSaveCommand(desc.cmd) 
							|| utils.isSaveThenQuitCommand(desc.cmd)) {
						this.mode = "lisp";
					}
				}
			}
			// FIXME: pvs needs to return errors in a standard way! 
			// e.g., when the prover is active, pvs should always return the sequents, and present errors as commentary.
			// currently, errors are presented in many (incompatible) different ways
			if (res.error || !test.success) {
				res.result = res.result || [{
					"last-cmd": desc.cmd
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
		const ans1: PvsResponse =  await this.pvsRequest('prover-status'); // this uses info provided by the xmlrpc server
		const ans2: PvsResponse = await this.legacy.getProverStatus();  // this other uses the lisp interface to test flag *in-checker*
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
	async proofScript(desc: { contextFolder: string, fileName: string, fileExtension: string, formulaName: string, theoryName: string }): Promise<PvsResponse> {
		if (desc) {
			desc = fsUtils.decodeURIComponents(desc);
			// extension is forced to .pvs, this is necessary as the request may come for a .tccs file
			const fname: string = fsUtils.desc2fname({ contextFolder: desc.contextFolder, fileName: desc.fileName, fileExtension: ".pvs" });
			if (this.useLegacy) {
				return await this.legacy.proofScript(desc);
			} else {
				return await this.pvsRequest('proof-script', [ fname, desc.formulaName ]);
			}
		}
		return null;
	}

	/**
	 * Loads the proof script for the formula indicated in the request
	 * The function looks for the proof in the .jprf file first.
	 * If the proof is not in the .jprf file, then checks the .prf file and updates the jprf file with the obtained information
	 * @param formula 
	 */
	async loadProof (formula: PvsFormula, opt?: {
		quiet?: boolean
	}): Promise<ProofDescriptor> {
		if (formula) {
			formula = fsUtils.decodeURIComponents(formula);
			const shasum: string = await fsUtils.shasumFile(formula);
			const pvsVersionDescriptor = this.getPvsVersionInfo();

			// to begin with, create an empty proof
			let proofDescriptor: ProofDescriptor = {
				info: {
					theory: formula.theoryName,
					formula: formula.formulaName,
					status: "untried",
					prover: utils.pvsVersionToString(pvsVersionDescriptor) || "PVS 7.x",
					shasum
				}
			};

			try {
				const fname: string = path.join(formula.contextFolder, `${formula.fileName}.jprf`);
				let proofFile: ProofFile = await utils.readProofFile(fname, opt);
				const key: string = `${formula.theoryName}.${formula.formulaName}`;
				// try to load the proof from the .jprf file
				if (proofFile && proofFile[key] && proofFile[key].length > 0) {
					proofDescriptor = proofFile[key][0];
					if (formula.fileExtension === ".pvs") {
						const shasum: string = await fsUtils.shasumFile(formula);
						if (shasum !== proofDescriptor.info.shasum) {
							proofDescriptor.info.status = utils.getActualProofStatus(proofDescriptor, shasum);
						}
					} else if (formula.fileExtension === ".tccs") {
						if (proofDescriptor && (!proofDescriptor.proofTree || !proofDescriptor.proofTree.rules || proofDescriptor.proofTree.rules.length === 0)) {
							const response: PvsResponse = await this.proofScript(formula);
							if (response && response.result) {
								proofDescriptor = utils.prf2jprf({
									prf: response.result,
									theoryName: formula.theoryName, 
									formulaName: formula.formulaName, 
									version: pvsVersionDescriptor,
									shasum
								});
								// save proof in the jprf file
								await this.saveProof({
									fileName: formula.fileName,
									fileExtension: formula.fileExtension,
									theoryName: formula.theoryName,
									formulaName: formula.formulaName,
									contextFolder: formula.contextFolder,
									proofDescriptor
								});
							}
						}
					}
				} else {
					// if the proof is not stored in the .jprf file, then try to load the proof from the .prf and then update jprf
					// the APIs of pvs are ugly -- an error is returned if the formula does not have a proof, as opposed to simply returning an empty proof
					const response: PvsResponse = await this.proofScript(formula);
					if (response && response.result) {
						proofDescriptor = utils.prf2jprf({
							prf: response.result,
							theoryName: formula.theoryName, 
							formulaName: formula.formulaName, 
							version: pvsVersionDescriptor,
							shasum
						});
					}
					// save proof in the jprf file
					await this.saveProof({
						fileName: formula.fileName,
						fileExtension: formula.fileExtension,
						theoryName: formula.theoryName,
						formulaName: formula.formulaName,
						contextFolder: formula.contextFolder,
						proofDescriptor
					});
				}
			} catch (err) {
				console.error(`[pvs-server] Error while fetching proof information.`, err);
			} finally {
				// return the requested proof
				return proofDescriptor;
			}
		} else {
			console.warn(`[pvs-server] Warning: load-proof received null request`);
		}
		return null;
	}
	/**
	 * Saves the proof script for the formula indicated in the request
	 * @param desc 
	 */
	async saveProof (desc: { 
		fileName: string, 
		fileExtension: string, 
		theoryName: string, 
		formulaName: string, 
		contextFolder: string, 
		proofDescriptor: ProofDescriptor
	}): Promise<boolean> {
		if (desc) {
			desc = fsUtils.decodeURIComponents(desc);
			const fname: string = path.join(desc.contextFolder, `${desc.fileName}.jprf`);
			let proofFile: ProofFile = await utils.readProofFile(fname);
			proofFile = proofFile || {};
			const key: string = `${desc.theoryName}.${desc.formulaName}`;
			proofFile[key] = [ desc.proofDescriptor ]; // TODO: implement mechanism to save a specific proof?
			const success: boolean = await fsUtils.writeFile(fname, JSON.stringify(proofFile, null, " "));
			return success;
		}
		// else
		console.error("[pvs-language-server] Warning: save-proof invoked with null or incomplete descriptor", desc);
		return false;
	}

	async saveProoflite (desc: { 
		fileName: string, 
		fileExtension: string, 
		theoryName: string, 
		formulaName: string, 
		contextFolder: string, 
		proofDescriptor: ProofDescriptor
	}): Promise<boolean> {
		// save prooflite
		const fname: string = fsUtils.desc2fname({
			contextFolder: desc.contextFolder,
			fileName: desc.theoryName,
			fileExtension: ".prl"
		});
		const content: string[] = utils.proofTree2ProofLite(desc.proofDescriptor);
		if (content && content.length) {
			const header: string = utils.makeProofliteHeader(desc.formulaName, desc.theoryName, desc.proofDescriptor.info.status);
			const proofLite: string = content.join("\n");
			const success: boolean = await utils.saveProoflite(fname, desc.formulaName, header + proofLite);
			// try to save into .prf -- disabled for now
			// const prl: string = utils.proofTree2Prl(desc.proofDescriptor);
			// await this.saveProofWithFormula(desc, prl);
			return success;
		}
		return false;
	}

	async saveAsPrj (desc: { 
		fileName: string, 
		fileExtension: string, 
		theoryName: string, 
		formulaName: string, 
		contextFolder: string, 
		proofDescriptor: ProofDescriptor
	}): Promise <boolean> {
		const prl: string = utils.proofTree2Prl(desc.proofDescriptor);
		const pvsResponse: PvsResponse = await this.saveProofWithFormula(desc, prl);
		return pvsResponse && pvsResponse.result;
	}

	/**
	 * Returns the prooflite script for the given formula -- FIXME: display-prooflite-script is not working, we need to use languageUtils.proofTree2ProofLite()
	 */
	async proofLiteScript(desc: { contextFolder: string, fileName: string, fileExtension: string, theoryName: string, formulaName: string }): Promise<PvsResponse> {
		if (desc) {
			// extension is forced to .pvs, this is necessary as the request may come for a .tccs file
			const fname: string = fsUtils.desc2fname({ contextFolder: desc.contextFolder, fileName: desc.fileName, fileExtension: ".pvs" });
			const res: PvsResponse = await this.lisp(`(display-prooflite-script "${fname}#${desc.theoryName}" "${desc.formulaName}")`);
		}
		return null;
	}

	/**
	 * Generate a .tccs file for the given theory
	 * @param desc 
	 */
	async tccs (desc: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string }): Promise<PvsResponse> {
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
							const lastKnownStatus: ProofStatus = await utils.getProofStatus({
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
					content = "\n" + content.split("\n").map(line => { return `\t${line}`; }).join("\n").trim() + "\n";
				} else {
					content = `%-- No TCC was generated for theory ${desc.theoryName}`;
				}
				// add theory information
				content = `${desc.theoryName}: THEORY BEGIN\n` + content + `\nEND ${desc.theoryName}`;
				// add header
				const header: string = `%% TCCs associated with theory ${desc.theoryName}\n%% This file was automatically generated by PVS, please **do not modify** by hand.\n`;
				// write .tccs file
				const fname: string = path.join(desc.contextFolder, `${desc.fileName}.tccs`);
				fsUtils.writeFile(fname, header + content);
			}
		} catch (error) {
			console.warn(`[pvs-language-server] Warning: something when wrong while writing the TCCs file`, error);
		} finally {
			return res;
		}
	}
	protected async showTccs(desc: { contextFolder: string, fileName: string, fileExtension: string, theoryName: string }): Promise<PvsResponse> {
		if (desc) {
			await this.changeContext(desc.contextFolder);
			if (this.useLegacy) {
				return await this.legacy.showTccs(fsUtils.desc2fname(desc), desc.theoryName);
			}
			const fullName: string = path.join(desc.contextFolder, desc.fileName + ".pvs" + "#" + desc.theoryName); // file extension is always .pvs, regardless of whether this is a pvs file or a tcc file
			return await this.pvsRequest('show-tccs', [ fullName ]);
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
	getPvsVersionInfo(): PvsVersionDescriptor {
		return this.pvsVersionInfo;
	}

	async loadPatchesAndLibraries(): Promise<void> {
		await this.setNasalibPath();
		await this.loadPvsPatches();
		await this.loadPvsLibraryPath();
	}

	/**
	 * Loads pvs version information
	 */
	async loadPvsVersionInfo(): Promise<PvsVersionDescriptor | null> {
		// try to load nasalib
		await this.setNasalibPath();
		await this.loadPvsPatches();
		await this.loadPvsLibraryPath();

		// const res: PvsResponse = await this.lisp(`(get-pvs-version-information)`);
		const res: PvsResponse = await this.legacy.lisp(`(get-pvs-version-information)`);
		const nasalib: string = await this.getNasalibVersionInfo();
		if (res && res.result) {
			const regexp: RegExp = /\(\"?(\d+(?:.?\d+)*)\"?[\s|nil]*\"?([\w\s\d\.]*)\"?/g; // group 1 is pvs version, group 2 is lisp version
			const info: RegExpMatchArray = regexp.exec(res.result);
			if (info && info.length > 2) {
				this.pvsVersionInfo = {
					"pvs-version": info[1].trim(),
					"lisp-version": info[2].replace("International", "").trim(),
					"nasalib-version": nasalib ? "NASALib" : null
				}
				return this.pvsVersionInfo;
			}
		}
		return null;
	}

	/**
	 * Returns pvs version information
	 */
	async getNasalibVersionInfo(): Promise<string | null> {
		const nasalibPresent: boolean = await this.isNasalibPresent();
		if (nasalibPresent) {
			const nasalibVersion: PvsResponse = await this.legacy.lisp(`*nasalib-version*`);
			const regexp: RegExp = /(\d+(?:.?\d+)*)/g; // group 1 is nasalib
			const info: RegExpMatchArray = regexp.exec(nasalibVersion.result);
			if (info && info.length > 1) {
				this.useNasalib = true;
				// check if vscode-output is enabled -- this is disabled for now
				const jsonOutput: PvsResponse = await this.legacy.lisp(`(boundp '*vscode-output*)`);
				if (jsonOutput && jsonOutput.result === "t") {
					if (this.connection) {
						this.connection.sendNotification(serverEvent.profilerData, `Profiling: vscode-output\n`);
					}
					this.jsonOutputAvailable = true;
				} else {
					if (this.connection) {
						this.connection.sendNotification(serverEvent.profilerData, `Profiling: xmlrpc-server\n`);
					}
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

	async rebootPvsServer (desc?: { pvsPath?: string }): Promise<void> {
		if (desc && desc.pvsPath) {
			this.pvsPath = desc.pvsPath;
			console.log(`[pvs-proxy] New PVS path: ${this.pvsPath}`);
		}
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
	 * Utility function, restarts the xml-rpc server
	 * @param opt 
	 */
	async restartPvsServer (opt?: { pvsPath?: string, pvsLibraryPath?: string }): Promise<boolean> {
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
		await this.legacy.activate(this.pvsServer, {
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
		return !(contextFolder !== this.pvsPath && contextFolder !== this.pvsLibPath);
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

	async getPvsLibraryPath (opt?: { useXmlrpc?: boolean }): Promise<string[]> {
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

	async setNasalibPath (opt?: { useXmlrpc?: boolean }): Promise<PvsResponse | null> {
		const pvsLibraries: string[] = await this.getPvsLibraryPath(opt);
		let path: string = this.nasalibPath.endsWith("/") ? this.nasalibPath : `${this.nasalibPath}/`;
		path = fsUtils.tildeExpansion(path);
		if (!pvsLibraries.includes(path) && await fsUtils.folderExists(path)) {
			return await this.lisp(`(push "${path}" *pvs-library-path*)`, opt);
		}
		return null;
	}

	getNasaLibPath (): string {
		return this.nasalibPath;
	}

	async isNasalibPresent (): Promise<boolean> {
		const response: PvsResponse = await this.legacy.lisp(`(boundp '*nasalib-version*)`);
		return response && response.result === "t";
	}

	async saveProofWithFormula (desc: PvsFormula, proofLiteScript: string): Promise<PvsResponse> {
		const nasalibPresent: boolean = await this.isNasalibPresent();
		if (nasalibPresent) {
			return await this.legacy.saveProofWithFormula(desc, proofLiteScript);
		}
		return null;
	}

	/**
	 * Updates pvs library path
	 * @param pvsLibraryPath colon-separated list of folders
	 */
	async loadPvsLibraryPath (pvsLibraryPath?: string, opt?: { useXmlrpc?: boolean }): Promise<PvsResponse | null> {
		opt = opt || {};
		const lp: string = pvsLibraryPath || this.pvsLibraryPath;
		const libs: string[] = (lp) ? lp.split(":").map((elem: string) => {
			return elem.trim();
		}) : [];
		if (libs && libs.length) {
			const pvsLibraries: string[] = await this.getPvsLibraryPath(opt);
			for (let i = 0; i < libs.length; i++) {
				const path: string = libs[i].endsWith("/") ? libs[i] : `${libs[i]}/`;
				if (!pvsLibraries.includes(path)) {
					return await this.legacy.lisp(`(push "${path}" *pvs-library-path*)`);
				}
			}
		}
		return null;
	}

	async loadPvsPatches (opt?: { useXmlrpc?: boolean }): Promise<PvsResponse> {
		return await this.lisp(`(load-pvs-patches)`, opt);
	}


	/**
	 * pvs-proxy activation function
	 * @param opt 
	 */
	async activate(opt?: { debugMode?: boolean, showBanner?: boolean, verbose?: boolean, pvsErrorManager?: PvsErrorManager }): Promise<boolean> {
		opt = opt || {};
		console.log(`[pvs-proxy] Activating pvs-proxy...`)
		this.showBanner = (opt.showBanner === undefined) ? true : opt.showBanner;
		this.debugMode = !!opt.debugMode;
		this.verbose = !!opt.verbose;
		this.pvsErrorManager = opt.pvsErrorManager;
		if (this.pvsServer) {
			return Promise.resolve(true);
		}
		const success: boolean = await this.restartPvsServer();
		if (success) {
			// await this.loadPvsVersionInfo();
			console.log(`[pvs-proxy] Activation complete!`);
		} else {
			console.error(`[pvs-proxy] Activation failed :/`);
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
				console.log(`[pvs-proxy] Activating GUI Server on http://${this.clientAddress}:${this.clientPort}`)
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

}
