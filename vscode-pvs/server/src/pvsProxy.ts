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

//import * as xmlrpc from 'xmlrpc';
import * as WebSocket from 'ws';
import { PvsProcess, ProcessCode } from "./pvsProcess";
import { PvsResponse, ParseResult, ShowTCCsResult, PvsError } from "./common/pvs-gui.d";
import * as fsUtils from './common/fsUtils';
import * as path from 'path';
import * as net from 'net';
import * as crypto from 'crypto';
import { SimpleConnection, serverEvent, PvsVersionDescriptor, ProofStatus, ProofDescriptor, ProofFile, PvsFormula, ServerMode, TheoryDescriptor, PvsTheory, FormulaDescriptor, PvsFile, PvsContextDescriptor, FileDescriptor, PvsProofState, MathObjects, ProofOrigin } from './common/serverInterface';
import { Parser } from './core/Parser';
import * as languageserver from 'vscode-languageserver';
import { ParserDiagnostics } from './core/pvs-parser/javaTarget/pvsParser';
import { checkPar, CheckParResult, getErrorRange, isQuitCommand, isQuitDontSaveCommand, isSaveThenQuitCommand, isShowHiddenFormulas } from './common/languageUtils';
import * as languageUtils from './common/languageUtils';
// import { PvsProxyLegacy } from './legacy/pvsProxyLegacy';
import { PvsErrorManager } from './pvsErrorManager';

export class PvsProgressInfo {
	protected progressLevel: number = 0;
	showProgress(cmd: string, data?: string): string {
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
	protected activeParsers: { [filename: string]: boolean } = {};
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
	readonly SOCKET_CLIENT_ATTEMPTS: number = 100; // used to allow a small delay in the socket opening on the PVS side 

	/** Number of retries for unsuccessful pvs-process starts */
	readonly MAX_PVS_START_ATTEMPTS: number = 3;
	readonly client_methods: string[] = ['info', 'warning', 'debug', 'buffer', 'yes-no', 'dialog'];
	protected banner: string;
	protected handlers: { [mth: string]: (params: string[]) => string[] } = {};
	protected webSocket: WebSocket;
	protected interruptConn: WebSocket;
	protected webSocketAddress: string = "localhost";
	protected webSocketPort: number;

	protected pendingRequests: object = []
	protected activeProgressReporters: object = [];

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

	// protected legacy: PvsProxyLegacy;
	// protected useLegacy: boolean = false;
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
			webSocketPort?: number,
			connection?: SimpleConnection,
			externalServer?: boolean,
			pvsLibraryPath?: string,
			messageReporter?: (data:string) => void
		}) {
		opt = opt || {};
		this.pvsPath = pvsPath;
		// this.pvsLibPath = path.join(pvsPath, "lib");
		// this.nasalibPath = path.join(pvsPath, "nasalib");
		this.pvsLibraryPath = opt.pvsLibraryPath || "";
		this.webSocketPort = (!!opt.webSocketPort) ? opt.webSocketPort : 23456; // 22334; // 23456;
		this.client_methods.forEach(mth => {
			this.handlers[mth] = (params: string[]): string[] => {
				// console.info("info", params);
				return params;
			};
		});
		this.banner = `Websocket GUI Server active at ws://${this.webSocketAddress}:${this.webSocketPort}`;
		this.connection = opt.connection;
		this.externalServer = !!opt.externalServer;
		this.cliListener = opt.messageReporter;

		// create antlr parser for pvs
		this.parser = new Parser();
	}

	async getMode(): Promise<ServerMode> {
		// return await this.getServerMode(); // pvs is not always able to return the prover status -- the workaround is to use this.mode to keep track of the prover status, and ask pvs the status only when stricly necessary
		return this.mode;
	}

	async enableExternalServer(opt?: { enabled?: boolean }): Promise<void> {
		opt = opt || {};
		if (opt.enabled === false) {
			await this.disableExternalServer();
		} else {
			console.info("[pvs-proxy] Enabling external server...");
			await this.killPvsServer();
			this.externalServer = true;
		}
	}

	async disableExternalServer(): Promise<void> {
		console.info("[pvs-proxy] External server disabled");
		this.externalServer = false;
		// await this.restartPvsServer();
	}

	//--------------------------------------------------
	//         json-rpc methods
	//--------------------------------------------------
	async pvsRequest(method: string, params?: string[], progressReporter?: (msg: string) => void): Promise<PvsResponse> {
		params = params || [];
		if (this.pvsServerProcessStatus === ProcessCode.SUCCESS && this.webSocket) {
			return new Promise(async (resolve, reject) => {
				const id = this.get_fresh_id();
				const req = { method: method, params: params, jsonrpc: "2.0", id: id };
				const jsonReq: string = JSON.stringify(req, null, " ");
				// console.log('pvsRequest send: ', jsonReq);
				this.webSocket.send(jsonReq)
				// This is the function called when a response comes through the
				// message handler of activate.
				const me = this;
				this.activeProgressReporters[id] = progressReporter;				
				this.pendingRequests[id] = function (obj: PvsResponse) {
					// console.log("pvsRequest response: ", obj);
					if ("result" in obj) {
						resolve(obj);
					} else if ("error" in obj) {
						//reject(obj);
						resolve(obj);
					} else if ("method" in obj) {
						switch (obj.method) {
							case 'pvsMessage': 
								if (me.activeProgressReporters[id])
									me.activeProgressReporters[id](obj.message);
								else
									console.log(`[pvsProxy.pvsRequest pendingRequest] message received from PVS but cliListener not set. msg: ${obj.message}`);
								break;
							default:
								console.log('[pvsProxy.pvsRequest pendingRequest] unknown');
								console.dir(obj, { depth: null });
						}
					} else {
						console.log('Bad obj:');
						console.dir(obj);
					}
				};
			});
		} else {
			let errorMsg: string = `[pvs-proxy] Warning: could not invoke method ${method} ` +
			(this.pvsServerProcessStatus === ProcessCode.PVS_NOT_FOUND ? 
				"`(PVS not installed)"
				: (this.pvsServerProcessStatus === ProcessCode.PVS_START_FAIL ? 
					"(PVS not responding)" : 
					"(Unknown error)"));
			console.log(errorMsg);
			return new Promise(async (resolve, reject) => {
				resolve({
					jsonrpc: "2.0",
					id: "00",
					error: {
						code: 
							(this.pvsServerProcessStatus===ProcessCode.PVS_NOT_FOUND? 'PVS_NOT_INSTALLED' : 
							(this.pvsServerProcessStatus===ProcessCode.PVS_START_FAIL? 'PVS_NOT_RESPONDING' : 
							(this.pvsServerProcessStatus===ProcessCode.ADDR_IN_USE? 'ADDR_IN_USE' : 
							(this.pvsServerProcessStatus===ProcessCode.COMM_FAILURE? 'COMM_FAILURE' : 
							(this.pvsServerProcessStatus===ProcessCode.PVSERROR? 'PVS_ERROR' : 
							(this.pvsServerProcessStatus===ProcessCode.UNSUPPORTED_PLATFORM? 'UNSUPPORTED_PLATFORM' :
							 'UNKNOWN_ERROR')))))),
						message: "pvs-proxy failed to initialize PVS server"
					}
				});
			})
		}
	}

	async pvsInterrupt(proofId: string): Promise<PvsResponse> {
		return this.sendRequestOnSecondaryConn("interrupt-proof", [proofId]);
	}

	public secondaryConnReady(): boolean {
		return this.interruptConn && this.interruptConn.readyState === WebSocket.OPEN;
	}

	public primaryConnReady(): boolean {
		return this.webSocket && this.webSocket.readyState === WebSocket.OPEN;
	}

	async sendRequestOnSecondaryConn(method: string, params?: string[]): Promise<PvsResponse> {
		if (this.interruptConn) {
			return new Promise(async (resolve, reject) => {
				const id = this.get_fresh_id();
				const req = { method: method, params: params, jsonrpc: "2.0", id: id };			
				const jsonReq: string = JSON.stringify(req, null, " ");
				this.interruptConn.send(jsonReq);
				this.pendingRequests[id] = function (obj: PvsResponse) {
					//console.log("pvsInterrupt response: ", obj);
					if ("result" in obj) {
						resolve(obj);
					} else if ("error" in obj) {
						//reject(obj);
						resolve(obj);
					} else {
						console.log('Bad obj:');
						console.dir(obj);
					}
				};
			});
		} else {
			console.log(`[pvs-proxy] Warning: could not invoke pvsInterrupt (pvs is not installed)`);
			return new Promise(async (resolve, reject) => {
				resolve({
					jsonrpc: "2.0",
					id: "00",
					error: {
						code: 'PVS_NOT_INSTALLED',
						message: "pvs-proxy failed to initialize gui server"
					}
				});
			})
		}
	}

	async startWebSocket(): Promise<void> {
		console.log("[pvsProxy.startWebSocket] Creating web socket client connection...");
		this.webSocket = new WebSocket(`ws://${this.webSocketAddress}:${this.webSocketPort}`);
		this.webSocket.on('redirect', () => {
			console.log("[pvsProxy.startWebSocket] redirect");
		});
		this.webSocket.on('upgrade', (response) => {
			console.log("[pvsProxy.startWebSocket] upgrade");
		});
		this.webSocket.on('pong', () => {
			console.log("[pvsProxy.startWebSocket] PONG");
		});
		this.webSocket.on('unexpected-response', () => {
			console.log("[pvsProxy.startWebSocket] unexpected-response");
		});
		this.webSocket.on('ping', () => {
			console.log("[pvsProxy.startWebSocket] PING");
		});
		this.webSocket.on('error', (error) => {
			console.log(`[pvsProxy.startWebSocket] Error on starting: ${error}`);
			// If the connection was refused, we retry, in case PVS is still starting
			
		});
		this.webSocket.on('open', () => {
			console.log("[pvsProxy.startWebSocket] OPEN");
		});
		this.webSocket.on('close', () => {
			console.log("[pvsProxy.startWebSocket] CLOSE");
			setTimeout(() => {
				if (this.pvsServerProcessStatus === ProcessCode.SUCCESS) this.startWebSocket(); 
			}, 5000);
		});
		this.webSocket.on('message', (msg: string) => {
			const obj = JSON.parse(msg);
			// Should check for valid JSON-RPC,
			//console.log('webSocket.on: ', this.pendingRequests);
			//console.log('  obj = ', obj);
			if (obj.id) {
				if ("result" in obj || "error" in obj || "method" in obj) {
					if (this.pendingRequests[obj.id]) {
						// console.log(`[pvsRequest] msg = ${msg}`)
						this.pendingRequests[obj.id](obj) // resolve(obj);
						if ("result" in obj || "error" in obj) {
							delete this.pendingRequests[obj.id];
							delete this.activeProgressReporters[obj.id];
						}
					} else {
						console.log('[pvsProxy.startWebSocket.on message] id not found');
					}
				} else {
					console.log('[pvsProxy.startWebSocket.on message] Unknown kind of object BEGIN');
					console.dir(obj, { depth: null });
					console.log('[pvsProxy.startWebSocket.on message] Unknown kind of object END');
				}
			}
		});		
	}

	async startInterruptionWebSocket(): Promise<void> {
		console.log("[pvsProxy.startInterruptionWebSocket] Creating web socket client connection...");
		this.interruptConn = new WebSocket(`ws://${this.webSocketAddress}:${this.webSocketPort}`);
		this.interruptConn.on('redirect', () => {
			console.log("[pvsProxy.startInterruptionWebSocket]  redirect");
		});
		this.interruptConn.on('upgrade', (response) => {
			console.log("[pvsProxy.startInterruptionWebSocket]  upgrade");
		});
		this.interruptConn.on('pong', () => {
			console.log("[pvsProxy.startInterruptionWebSocket]  PONG");
		});
		this.interruptConn.on('unexpected-response', () => {
			console.log("[pvsProxy.startInterruptionWebSocket]  unexpected-response");
		});
		this.interruptConn.on('ping', () => {
			console.log("[pvsProxy.startInterruptionWebSocket]  PING");
		});
		this.interruptConn.on('error', (error) => {
			console.log(`ERROR: ${error}`);
			// If the connection was refused, we retry, in case PVS is still starting
			
		});
		this.interruptConn.on('open', () => {
			console.log("[pvsProxy.startInterruptionWebSocket]  OPEN");
		});
		this.interruptConn.on('close', () => {
			console.log("[pvsProxy.startInterruptionWebSocket]  CLOSE");
			setTimeout(() => {
				if (this.pvsServerProcessStatus === ProcessCode.SUCCESS) this.startInterruptionWebSocket();
			}, 5000);
		});
		this.interruptConn.on('message', (msg: string) => {
			const obj = JSON.parse(msg);
			if (obj.id) {
				if ("result" in obj || "error" in obj) {
					if (this.pendingRequests[obj.id]) {
						// console.log(`[pvsRequest] msg = ${msg}`)
						this.pendingRequests[obj.id](obj) // resolve(obj);
						delete this.pendingRequests[obj.id]
					} else {
						console.log('[on message] id not found');
					}
				} else {
					switch (obj.method) {
						case 'pvsMessage':
							// A notification
							// console.log(`[pvsRequest]  ${method} = ${msg}`);
							console.log(`[pvsMessage] ${obj.params}`);
							break;
						default:
							console.log('[on message] unknown');
							console.dir(obj, { depth: null });
					}
				}
			}
		});		
	}

	/**
	 * Indicates whether the given web socket is considered to be in an operational state.
	 * @param ws 
	 * @returns 
	 */
	protected isOperational(ws: WebSocket): boolean {
		return ws && !(ws.readyState === WebSocket.CLOSED);
	}

	protected pvsServerProcessStatus: ProcessCode;
	/**
	 * pvs-proxy activation function
	 * @param opt 
	 */
	async activate(opt?: { debugMode?: boolean, showBanner?: boolean, verbose?: boolean, pvsErrorManager?: PvsErrorManager }): Promise<ProcessCode> {
		opt = opt || {};
		console.log(`[pvs-proxy.activate] Starting pvs-proxy...`);
		console.log(`[pvs-proxy.activate] this.webSocket ${this.webSocket}`);

		this.pvsErrorManager = opt.pvsErrorManager;
		this.showBanner = (opt.showBanner === undefined) ? true : opt.showBanner;
		this.debugMode = !!opt.debugMode;
		this.verbose = !!opt.verbose;

		this.pvsServerProcessStatus = await this.restartPvsServer();
		if (this.pvsServerProcessStatus === ProcessCode.SUCCESS) {
			let currentConnectionAttempt: number = 0;

			if (this.webSocket == undefined) {
				do {
					currentConnectionAttempt++;
					try {
						await this.startWebSocket();
						console.log("[pvsProxy.activate] Waiting for webSocket to be operational");
						await this.waitForOpenConnection(this.webSocket);
					} catch (jsonError) {
						// Start up PVS 
						console.log("[pvsProxy.activate] Error", jsonError);
					}
				} while (currentConnectionAttempt < this.SOCKET_CLIENT_ATTEMPTS && !this.isOperational(this.webSocket)); 
			}
	
			if (currentConnectionAttempt < this.SOCKET_CLIENT_ATTEMPTS && this.interruptConn == undefined) {
				do {
					currentConnectionAttempt++;
					try {
						await this.startInterruptionWebSocket();
						console.log("[pvsProxy.activate] Waiting for interruptConn to be operational");
						await this.waitForOpenConnection(this.interruptConn);
					} catch (jsonError) {
						// Start up PVS 
						console.log("[pvsProxy.activate] Error creating interruptConn", jsonError);
					}
				} while (currentConnectionAttempt < this.SOCKET_CLIENT_ATTEMPTS && !this.isOperational(this.interruptConn));
			}
	
			if(this.isOperational(this.webSocket) && this.isOperational(this.interruptConn))
				console.log(`[pvs-proxy.activate] Restart PVS Server done. PvsProxy is Ready ✅`);
			else {
				console.error(`[pvs-proxy.activate] Failed to open socket client ❌`);					
				this.pvsServerProcessStatus = ProcessCode.COMM_FAILURE; 
			}

		} else {
			console.error(`[pvs-proxy.activate] Failed to start proxy ❌`);
		}
		return this.pvsServerProcessStatus;
	}
	/**
	 * 
	 * @returns 
	 */
	async listMethodsRequest(): Promise<PvsResponse> {
		return await this.pvsRequest('list-methods');
	}
	/**
	 * 
	 * @returns 
	 */
	async listClientMethods(): Promise<PvsResponse> {
		return await this.pvsRequest('list-client-methods');
	}

	/**
	 * Utility function, creates a PvsResponse out of parse/typecheck diagnostic messages
	 * @param diags Diagnostic messages from the parser
	 */
	protected makeDiags(diags: ParserDiagnostics, opt?: { id?: string, isTypecheckError?: boolean }): PvsResponse {
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
	listMathObjects(): MathObjects {
		return this.mathObjectsCache;
	}
	/**
	 * Utility function, returns the list of known lemmas
	 */
	listLemmas(): string[] {
		return this.mathObjectsCache.lemmas;
	}
	/**
	 * Utility function, returns the list of known definitions
	 */
	listDefinitions(): string[] {
		return this.mathObjectsCache.definitions;
	}
	/**
	 * Utility function, returns the list of known types
	 */
	listTypes(): string[] {
		return this.mathObjectsCache.types;
	}

	fileRef(desc: PvsFile): string {
		return path.join(desc.contextFolder, desc.fileName + ".pvs");
	}

	theoryRef(desc: PvsTheory): string {
		return this.fileRef(desc) + "#" + desc.theoryName;
	}

	formulaRef(fm: PvsFormula): string {
		return this.theoryRef(fm) + "#" + fm.formulaName;
	}

	/**
	 * Parse a given pvs file
	 * @param desc pvs file descriptor: context folder, file name, file extension
	 */
	async parseFile(desc: PvsFile, opt?: {
		test?: boolean,
		externalServer?: boolean,
		enableEParser?: boolean
	}): Promise<PvsResponse | null> {
		opt = opt || {};
		// console.log('[parseFile] desc = ', desc);
		if (desc) {
			const fname: string = this.fileRef(desc);
			const fileExists: boolean = fsUtils.fileExists(fname);
			if (fileExists) {
				// await this.changeContext(desc);
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
						const res: PvsResponse = await this.pvsRequest('parse', [fname]);
						if (opt.test) { return res; }

						// testing antlr parser
						// this.parser.parseFile(desc); // async call

						if (res) {
							let range: languageserver.Range = null;
							let message: string = `File ${desc.fileName} parsed successfully`;
							let mathObjects: { types: number, definitions: number, lemmas: number } = { types: 0, definitions: 0, lemmas: 0 };
							// console.log('[parseFile] res = ', res);
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
								diags.errors = [error];

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
					range: { start: { line: 0, character: 0 }, end: { line: 0, character: 0 } },
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
					errors: [error]
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
	 * Returns the import chain for a given theory
	 * @param desc theory descriptor: context folder, file name, file extension, theory name
	 */
	async getImportChain(desc: PvsTheory): Promise<PvsTheory[]> {
		let importChain: PvsTheory[] = [];
		if (desc && desc.theoryName) {
			// change context
			await this.changeContext(desc);

			const res: PvsResponse = await this.lisp(`
	(let ((theoryname "${desc.theoryName}"))
	(let ((usings (remove-if #'(lambda (th)
	(or (from-prelude? th)
	(lib-datatype-or-theory? th)))
	(collect-theory-usings theoryname nil))))
	(mapcar (lambda (theory) (cons (format nil "|~a|" (id theory)) (shortname (make-specpath (filename theory))))) (reverse usings))))
	`);
			// console.log(res.result);
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
	async getTheorems(desc: PvsTheory, opt?: { includeImportChain?: boolean, tccsOnly?: boolean }): Promise<PvsFormula[]> {
		opt = opt || {};
		let ans: PvsFormula[] = [];
		if (desc) {
			let theories: PvsTheory[] = (opt.includeImportChain) ? await this.getImportChain(desc) : [desc];
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
						const tccsResult: ShowTCCsResult = <ShowTCCsResult>tccsResponse.result;
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
	async generateTccs(desc: PvsFile): Promise<PvsContextDescriptor> {
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
								const tccResult: ShowTCCsResult = <ShowTCCsResult>response.result;
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
								}) : null
								res.fileDescriptors[fname].theories.push({
									fileName: desc.fileName,
									fileExtension: ".pvs",
									contextFolder: desc.contextFolder,
									theoryName,
									position: null, // tccs are not part of the .pvs file
									theorems: tccs
								});
							} else {
								console.log(`[pvsProxy.generateTccs] No TCCs generated`);
							}
						} else {
							this.pvsErrorManager.handleShowTccsError({ response: <PvsError>response });
						}
					}
				}
				return res;
			} catch (ex) {
				console.error('[pvsProxy.generateTccs] Error: pvsProxy has thrown an exception', ex);
				return null;
			}
		}
		return null;
	}
	/**
	 * Typechecks a given pvs file
	 * @param desc pvs file descriptor:
	 *   contextFolder, fileName, fileExtension, optional fileContent
	 */
	async typecheckFile(desc: PvsFile, opt?: { externalServer?: boolean, progressReporter?: (msg: string) => void }): Promise<PvsResponse> {
		opt = opt || {};
		if (desc && desc.fileName && desc.fileExtension && desc.contextFolder) {
			let fname: string = this.fileRef(desc);
			//console.log('[typecheckFile] fname = ', fname);
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
			// console.log('Typechecking ', fname);
			const res: PvsResponse = await this.pvsRequest('typecheck', [fname, desc.fileContent], opt.progressReporter );
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
	protected async proveFile(desc: { contextFolder: string, fileName: string, fileExtension: string, }): Promise<PvsResponse> {
		if (desc && desc.fileName && desc.fileExtension && desc.contextFolder) {
			if (this.isProtectedFolder(desc.contextFolder)) {
				this.info(`${desc.contextFolder} is already proved`);
				return null;
			}
			const fname: string = path.join(desc.contextFolder, `${desc.fileName}${desc.fileExtension}`);
			const res: PvsResponse = await this.lisp(`(prove-pvs-file "${fname}" nil)`);
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
	async statusProofChain(desc: PvsFormula): Promise<PvsResponse> {
		const formref: string = this.formulaRef(desc);
		const cmd: string = `(proofchain-status "${formref}")`;
		const res: PvsResponse = await this.lisp(cmd);
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
			const ans: PvsResponse = await this.pvsRequest("proof-status", [fullName, formula.formulaName]);
			return ans;
		}
		return null;
	}
	/**
	 * Returns the proof status (proved, unfinished, untried, ...) for the given formula
	 * @param formula 
	 */
	async getProofStatus(formula: PvsFormula): Promise<ProofStatus> {
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
	async proveFormula(formula: PvsFormula): Promise<PvsResponse> {
		if (formula) {
			// const fullName: string = path.join(formula.contextFolder, formula.fileName + ".pvs"); // file extension is always .pvs, regardless of whether this is a pvs file or a tcc file
			// const formRef = { directory: formula.contextFolder,
			// 		       file: formula.fileName,
			// 		       theory: formula.theoryName,
			// 		       formula: formula.formulaName };
			const formRef: string = formula.contextFolder + '/' + formula.fileName + '#' + formula.theoryName + '#' + formula.formulaName;
			// const jsonRef: string = JSON.stringify(formRef, null, " ");
			// const jsonRef: string = `${formRef}`;
			let ans: PvsResponse = await this.pvsRequest("prove-formula", [formRef]);
			// let ans: PvsResponse = await this.pvsRequest("prove-formula", [ jsonRef ]);
			// if pvs reports that the prover was still open, try to force exit and retry prove-formula
			// if (ans?.error?.data?.error_string === "Must exit the prover first") {
			// 	await this.quitProof({ force: true });
			// 	ans = await this.pvsRequest("prove-formula", [ formula.formulaName, fullName ]);
			// }
			// if (this.verbose) { console.dir(ans); }
			// if (ans && ans.result && ans.result["length"] === undefined) {
			// 	ans.result = [ ans.result ]; // the prover should return an array of proof states
			// }
			// if (ans && !ans.error) {
			// 	this.mode = "in-checker";
			// }
			return ans;
		}
		return null;
	}

	/**
	 * clearWorkspace by default will save the current pvs-context, and then
	 * reset to the state as if PVS had been restarted in the specified workspaces.
	 * The ws string can be a library reference, a directory, or ":all"
	 *   emptyPvsContext - loses theory dependencies, invalidates binfiles
	 *   deleteBinfiles - removes the binfiles
	 *   dontLoadPreludeLibraries - loaded by default, unless emptied
	 **/
	async clearWorkspace(ws: string = "./",
		opt?: {
			emptyPvsContext?: boolean,
			deleteBinfiles?: boolean,
			dontLoadPreludeLibraries?: boolean
		}): Promise<void> {
		// console.dir(`[pvs-proxy] Resetting workspace ${ws}: ${opt.emptyPvsContext}, ${opt.deleteBinfiles}, ${opt.dontLoadPreludeLibraries}`);
		await this.pvsRequest('clear-workspace',
			[ws,
				opt.emptyPvsContext ? "t" : "nil",
				opt.deleteBinfiles ? "t" : "nil",
				opt.dontLoadPreludeLibraries ? "t" : "nil"]);
	}

	async emptyAllWorkspaces(): Promise<void> {
		// console.log(`[pvs-proxy] Emptying all workspaces`);
		this.clearWorkspace(":all", { emptyPvsContext: true });
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
	async statusProofTheory(desc: { contextFolder: string, fileName: string, fileExtension: string, theoryName: string }): Promise<PvsResponse> {
		if (desc && desc.fileName && desc.fileExtension && desc.contextFolder && desc.theoryName) {
			await this.changeContext(desc.contextFolder);
			const res: PvsResponse = await this.pvsRequest('lisp', [`(status-proof-theory "${desc.theoryName}")`]);
			return res;
		}
		return null;
	}

	/**
	 * Changes context
	 * @param contextFolder 
	 */
	async changeContext(desc: string | { contextFolder: string }): Promise<PvsResponse> {
		if (desc) {
			const ctx: string = (typeof desc === "string") ? desc : desc.contextFolder;
			return await this.pvsRequest('change-context', [ctx]);
		}
		return null;
	}

	/**
	 * Returns the current context
	 */
	async currentContext(): Promise<PvsResponse> {
		const res: PvsResponse = await this.lisp('(pvs-current-directory)');
		if (res && res.result && typeof res.result === "string") {
			res.result = res.result.replace(/"/g, ""); // pvs returns the folder name adorned with double quotes
		}
		return res;
	}

	/**
	 * Executes a lisp command in pvs
	 * @param cmd 
	 */
	async lisp(cmd: string): Promise<PvsResponse> {
		return await this.pvsRequest('lisp', [cmd]);
	}

	async getServerMode(): Promise<ServerMode> {
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
	async quitProof(proofId: string, opt?: { force?: boolean }): Promise<void> {
		// await this.interrupt();
		const mode: string = await this.getMode(); //await this.getServerMode(); //await this.getMode();
		if (mode === "in-checker" || opt?.force) {
			const useLispInterface: boolean = true;
			const response: PvsResponse = await this.proofCommand({ proofId: proofId, cmd: "(quit)" });
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
	async findDeclaration(symbolName: string, opt?: { externalServer?: boolean }): Promise<PvsResponse> {
		opt = opt || {};
		// find-declaration breaks the server, we need to use the lisp interface for now
		// const externalServer: boolean = opt.externalServer === undefined ? this.externalServer : !!opt.externalServer;
		// if (externalServer) {
		return await this.pvsRequest('find-declaration', [symbolName]);
		// }
	}

	async findTheory(theoryName: string): Promise<PvsResponse> {
		const ans: PvsResponse = await this.lisp(`(let ((th (get-theory "${theoryName}")))
	(when th
	(format nil "~a~a.pvs" (context-path th) (filename th))))`);
		return ans;
	}

	/**
	 * @SAM: FIXME: this function should not automatically invoke the typechecker
	 * @param desc 
	 */
	async termAt(desc: { fileName: string, fileExtension: string, contextFolder: string, line: number, character: number }): Promise<PvsResponse> {
		const fname: string = fsUtils.desc2fname(desc);
		const ans: PvsResponse = await this.pvsRequest("term-at", [fname, `(${desc.line} ${desc.character})`, 't']);
		return ans;
	}

	/**
	 * Returns the help message for a given command
	 */
	async showHelpBang(desc: { cmd: string }): Promise<PvsResponse> {
		const match: RegExpMatchArray = new RegExp(languageUtils.helpBangCommandRegexp).exec(desc.cmd);
		if (match && match.length > 1 && match[1]) {
			this.pvsServer.clearLispInterfaceOutput();
			const helpCmd: string = `(help ${match[1]})`;
			const ans: PvsResponse = await this.pvsRequest('proof-help', [helpCmd]);
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
				ans.result[ans.result.length - 1].commentary = [help.trim()];
			}
			return ans;
		}
		return null;
	}

	/**
	 * Executes a proof command. The command is always adorned with round parentheses, e.g., (skosimp*)
	 * @param desc Descriptor of the proof command
	 */
	async proofCommand(desc: { proofId: string, cmd: string }): Promise<PvsResponse> {
		if (desc) {
			const test: CheckParResult = checkPar(desc.cmd);
			let res: PvsResponse = { jsonrpc: "2.0", id: "" };
			// console.log('[proofCommand] test = ', test);
			if (test.success) {
				// console.dir(desc, { depth: null });
				const showHidden: boolean = isShowHiddenFormulas(desc.cmd);
				// const isGrind: boolean = utils.isGrindCommand(desc.cmd);
				// the following additional logic is a workaround necessary because pvs-server does not know the command show-hidden. 
				// the front-end will handle the command, and reveal the hidden sequents.
				const cmd: string = showHidden ? "(skip)" : desc.cmd;
				const pid: string = desc.proofId;
				res = languageUtils.isHelpBangCommand(cmd)
					? await this.showHelpBang({ cmd: cmd })
					: await this.pvsRequest('proof-command', [pid, cmd]);
				if (res && res.result) {
					const proofStates: PvsProofState[] = res.result;
					if (showHidden) {
						for (let i = 0; i < proofStates.length; i++) {
							const result: PvsProofState = proofStates[i];
							if (result) {
								result.label = "hidden formulas in " + result.label;
								result.action = "Showing list of hidden formulas";
								result.commentary = "Showing list of hidden formulas";
								result.sequent = {
									antecedents: result.sequent ? result.sequent['hidden-antecedents'] : null,
									succedents: result.sequent ? result.sequent['hidden-succedents'] : null
								}
							}
						}
					}
					for (let i = 0; i < proofStates.length; i++) {
						const result: PvsProofState = proofStates[i];
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
					(res.error && res.error.message) ? 
						res.error.message.replace(/\\\\"/g, "") + (res.error.data) ? (" " + res.data) : "" 
						: null
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
	async getProverStatus(proofId?: string): Promise<PvsResponse> {
		let prfid: string = (typeof proofId == 'undefined') ? "" : proofId;
		const ans: PvsResponse = await this.pvsRequest('prover-status', [prfid]);
		return ans;
	}

	/**
	 * Returns the pvs proof script for a given formula
	 */
	async getDefaultProofScript(formula: PvsFormula, opt?: { externalServer?: boolean }): Promise<PvsResponse> {
		if (formula) {
			opt = opt || {};
			formula = fsUtils.decodeURIComponents(formula);
			// extension is forced to .pvs, this is necessary as the request may come for a .tccs file
			const fname: string = fsUtils.desc2fname({ contextFolder: formula.contextFolder, fileName: formula.fileName, fileExtension: ".pvs" });
			const formName: string = fname + "#" + formula.formulaName;
			return await this.pvsRequest('proof-script', [formName]);
		}
		return null;
	}

	/**
	 * Returns all the proofs associated with the given formula.
	 * @author M3
	 */
	async getAllProofScripts(formula: PvsFormula, opt?: { externalServer?: boolean }): Promise<PvsResponse> {
		if (formula) {
			opt = opt || {};
			formula = fsUtils.decodeURIComponents(formula);
			// extension is forced to .pvs, this is necessary as the request may come for a .tccs file
			const fname: string = fsUtils.desc2fname({ contextFolder: formula.contextFolder, fileName: formula.fileName, fileExtension: ".pvs" });
			const formName: string = fname + "#" + formula.formulaName;
			return await this.pvsRequest('all-proofs-of-formula', [formName]);
		}
		return null;
	}

	/**
	 * 
	 * @param pvsFileName a string, which may include an absolute or relative (to 
	 *   current workspace) directory, and may or may not include the .pvs extension.
	 * @returns the proof scripts extracted from the .prf file associated with the given filename.
	 * @author M3
	 */
	async getProofScriptsInPrfFile(pvsFileName: string): Promise<PvsResponse> {
		const ans: PvsResponse = await this.pvsRequest("get-proof-scripts", [pvsFileName]);
		return ans;
	}

	
	/**
	 * Opens a proof file
	 * @param desc Proof file descriptor (name, extension, folder)
	 * @param formula Formula descriptor
	 * @returns Proof descriptor
	 */
	async openProofFile(desc: FileDescriptor, formula: PvsFormula, opt?: { quiet?: boolean }): Promise<ProofDescriptor> {
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
							pdesc = new ProofDescriptor(proofFile[key][0].info, origin, proofFile[key][0].proofTree);
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
					pdesc = new ProofDescriptor({
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

	async newProof(formula: PvsFormula): Promise<ProofDescriptor> {
		formula = fsUtils.decodeURIComponents(formula);
		const shasum: string = await fsUtils.shasumFile(formula);
		const pvsVersionDescriptor = this.getPvsVersionInfo();
		const status: ProofStatus = await this.getProofStatus(formula);
		const empty_pdesc: ProofDescriptor = new ProofDescriptor({
			theory: formula.theoryName,
			formula: formula.formulaName,
			status,
			prover: languageUtils.pvsVersionToString(pvsVersionDescriptor) || "PVS 8.x",
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
	async storeLastAttemptedProof(formula: PvsFormula): Promise<PvsResponse> {
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
			const response: PvsResponse = await this.pvsRequest("save-all-proofs", [fullTheoryName]);
			return response;
		}
		// else
		console.error("[pvs-language-server] Warning: save-proof invoked with null or incomplete descriptor", formula);
		return null;
	}
	/**
	 * Instructs the server to save to a prf file all the modified proofs in the theory theoryRef.
	 * @param theoryRef a URI of the form [pvsfile '#'] thname
	 * @returns true on success
	 */
	async saveAllModifiedProofsIntoPrfFile(theoryRef: string): Promise<PvsResponse> {
		const response: PvsResponse = await this.pvsRequest("save-all-proofs", [theoryRef]);
		return response;
	}
	/**
	 * Marks the proof identified by proofId as the default proof for the given formula.
	 * proofId is assumed to be the identifier of one of the proofs for the formula.
	 * @param formula to be modified
	 * @param proofId identifier of the proof to be marked as default
	 * @returns 
	 */
	async markProofAsDefault(formula: PvsFormula, proofId: string): Promise<PvsResponse> {
		if(formula){
			const formRef: string = formula.contextFolder + '/' + formula.fileName + '#' + formula.theoryName + '#' + formula.formulaName;
			let ans: PvsResponse = await this.pvsRequest("mark-proof-as-default", [formRef, proofId]);
			return ans;
		} else
			return null;
	}
	/**
	 * Discards the proof identified by proofId from the collection of proofs for the given formula.
	 * proofId is assumed to be the identifier of one of the proofs for the formula.
	 * @param formula to be modified
	 * @param proofId identifier of the proof to be discarded
	 * @returns 
	 */
	async discardProofFromFormula(formula: PvsFormula, proofId: string): Promise<PvsResponse> {
		if(formula){
			const formRef: string = this.formulaRef(formula); // formula.contextFolder + '/' + formula.fileName + '#' + formula.theoryName + '#' + formula.formulaName;
			let ans: PvsResponse = await this.pvsRequest("delete-proof-of-formula", [formRef, proofId]);
			return ans;
		} else
			return null;
	}
	/**
	 * Saves the given proof script in prooflite (.prl) format
	 * @param desc Proof descriptor
	 */
	async saveProoflite(desc: {
		fileName: string,
		fileExtension: ".prl",
		theoryName: string,
		formulaName: string,
		contextFolder: string,
		proofDescriptor: ProofDescriptor
	}, opt?: {
		usePvsBinFolder?: boolean
	}): Promise<FileDescriptor> {
		opt = opt || {};
		// by default, save under pvsbin
		opt.usePvsBinFolder = opt.usePvsBinFolder === undefined ? true : !!opt.usePvsBinFolder;
		const contextFolder: string = opt.usePvsBinFolder ? path.join(desc.contextFolder, "pvsbin") : desc.contextFolder;
		// save prooflite
		const prlFile: FileDescriptor = {
			contextFolder,
			fileName: desc.theoryName,
			fileExtension: ".prl"
		};
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
	async generateTccsFile(desc: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string }): Promise<PvsResponse> {
		desc.fileExtension = ".pvs"; // tccs can be generated only for .pvs files
		desc = fsUtils.decodeURIComponents(desc);
		const res: PvsResponse = await this.showTccs(desc);
		// create tccs files
		try {
			if (res && res.result) {
				//console.log('[generateTccsFile] res = ', res);
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

			// await this.changeContext(desc.contextFolder);

			const fullName: string = path.join(desc.contextFolder, desc.fileName + "#" + desc.theoryName);
			// file extension is not needed, command will figure it out.
			const response: PvsResponse = await this.pvsRequest('show-tccs', [fullName]);
			return response;
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
	 * Returns pvs version information
	 */
	getPvsVersionInfo(): PvsVersionDescriptor {
		return this.pvsVersionInfo;
	}

	async loadPvsLibraryPathAndPatches(): Promise<void> {
		await this.setNasalibPath();
		await this.pushPvsLibraryPath({ useLisp: true });
		await this.loadPvsPatches();
	}

	/**
	 * Loads pvs version information
	 */
	async loadPvsVersionInfo(): Promise<PvsVersionDescriptor | null> {
		let res: PvsResponse = await this.lisp(`(get-pvs-version-information)`);
		const nasalib: string = await this.getNasalibVersionInfo();
		if (res && res.result) {
			const regexp: RegExp = /\(\"?(\d+(?:.?\d+)*)\"?[\s|NIL]*\"?([\w\s\d\.]*)\"?/g; // group 1 is pvs version, group 2 is lisp version
			const info: RegExpMatchArray = regexp.exec(res.result);
			if (info && info.length > 2) {
				this.pvsVersionInfo = {
					"pvs-version": info[1].trim(),
					"lisp-version": info[2].replace("International", "").trim(),
					"nasalib-version": nasalib ? "NASALib" : null,
					version: res.result
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
		const nasalibPresent: boolean = await this.NasalibPresent();
		if (nasalibPresent) {
			const nasalibVersion: PvsResponse = await this.lisp(`(when (fboundp 'extra-pvslib-keyval) (extra-pvslib-keyval "NASALib" "version"))`);
			this.useNasalib = (nasalibVersion?.result !== 'NIL' && nasalibVersion?.result !== 'nil');
			return this.useNasalib? nasalibVersion.result : null;
		}
		return null;
	}

	//--------------------------------------------------
	//         internal functions
	//--------------------------------------------------
	protected info(...data: any): void {
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
	protected checkPort(port: number, retry: boolean): Promise<boolean> {
		return new Promise((resolve, reject) => {
			if (this.showBanner) {
				console.log(`[pvs-proxy] Checking port ${port}...`);
			}
			const socket: net.Socket = net.createConnection({ host: this.webSocketAddress, port });
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

	async waitForOpenConnection(ws: WebSocket): Promise<void> {
		return new Promise((resolve, reject) => {
			const maxNumberOfAttempts = 100 // debug
			const intervalTime = 200 //ms

			let currentAttempt = 0
			const intervalId = setInterval(() => {
				console.log(`[waitForOpenConnection] waiting for ${ws}`);
				if (ws.readyState === WebSocket.OPEN) {
					clearInterval(intervalId)
					resolve()
				} else if (ws.readyState === WebSocket.CLOSED) {
					clearInterval(intervalId)
					reject(new Error('Web socket in CLOSE state'))
				} else if (currentAttempt > maxNumberOfAttempts - 1) {
					clearInterval(intervalId)
					reject(new Error('Maximum number of attempts exceeded'))
				} else if (ws.readyState === WebSocket.CLOSING) {
					console.log(`[waitForOpenConnection] closing`);
				}
				currentAttempt++;
			}, intervalTime);
		})
	}

	/**
	 * Checks if pvs-server accepts connection
	 */
	async testServerConnectivity(): Promise<boolean> {
		//console.log(`opening ws://${this.webSocketAddress}:${this.webSocketPort}`);
		//const ws = new WebSocket(`ws://${this.webSocketAddress}:${this.webSocketPort}`);
		return new Promise((resolve, reject) => {
			const id = 'test1'
			const req = { method: 'list-methods', params: [], jsonrpc: "2.0", id: id };
			const jsonReq = JSON.stringify(req, null, " ");
			this.webSocket.send(jsonReq);
			this.webSocket.once('message', (msg: string) => {
				//console.log("on %s", msg);
				const resp = JSON.parse(msg);
				//console.log("got %s", resp);
				if (resp.result) {
					console.log("server-test value:", resp.result);
					resolve(true);
				} else {
					console.log("[pvs-proxy] server-test error:", resp.error);
					reject(resp.error)
				}
			});
		});
	}
	/**
	 * Internal function, returns a unique ID
	 */
	protected get_fresh_id(): string {
		return crypto.createHash('sha256').update(Math.random().toString(36)).digest('hex');
	}

	/**
	 * Callback function called from activate, when gui-server is ready 
	 */
	// protected serverReadyCallBack() {
	// 	this.guiServer.on('request', (error: any, params: string[], callback: (error: any, value: any) => void) => {
	// 	    if (error) {
	// 		console.log("[pvs-proxy] Error", error);
	// 	    }
	// 	    // console.log("params", params);
	// 	    if (params && params.length > 0) {
	// 		try {
	// 		    for (let i = 0; i < params.length; i++) {
	// 			const json: { method: string, params: string[] } = JSON.parse(params[i]);
	// 			// console.info(`\n json: `, json);
	// 			if (json && json.method && this.handlers[json.method]) {
	// 			    this.handlers[json.method](json.params);
	// 			}
	// 		    }
	// 		} catch (jsonError) {
	// 		    console.log("[pvs-proxy] Error", jsonError);
	// 		}
	// 	    }
	// 	    callback(error, params);
	// 	});
	// 	this.guiServer.on("error", (error: Error) => {
	// 	    console.log(`[pvs-proxy] GUI-Server Error`, error);
	// 	});
	// 	this.guiServer.on("connect", (error: any, params: string, callback: (error: any, value: any) => void) => {
	// 	    if (error) {
	// 		console.log("[pvs-proxy] connection error", error);
	// 	    }
	// 	    if (params) {
	// 		console.info("[pvs-proxy] connection params", params);
	// 	    }
	// 	});
	// 	// this.isActive = true;
	// 	// console.info(this.server.httpServer);
	// }

	/**
	 * Utility function, creates a new pvs-server
	 */
	protected async createPvsServer(opt?: {
		enableNotifications?: boolean,
		externalServer?: boolean,
		verbose?: boolean
	}): Promise<void> {
		opt = opt || {};
		const externalServer: boolean = opt.externalServer === undefined ? this.externalServer : !!opt.externalServer;
		if (externalServer) {
			console.log(`[pvs-proxy.createPvsServer] +-- EXTERNAL SERVER CONFIGURATION --`);
			console.log(`[pvs-proxy.createPvsServer] |  Address: ${this.webSocketAddress}`);
			console.log(`[pvs-proxy.createPvsServer] |  Port: ${this.webSocketPort}`);
			console.log(`[pvs-proxy.createPvsServer] +-----------------------------------`)
			this.pvsServerProcessStatus = ProcessCode.SUCCESS;
		} else {
			const connection: SimpleConnection = (opt.enableNotifications) ? this.connection : null;
			const proc: PvsProcess = new PvsProcess(this.pvsPath, { connection, pvsErrorManager: this.pvsErrorManager, pvsLibraryPath: this.pvsLibraryPath });
	
			let serverIsReady: boolean = false;
			let currentPortAttemptNumber: number = 0;
			let currentPvsStartAttemptNumber: number = 0;		
			while ( !serverIsReady && currentPortAttemptNumber < this.MAX_PORT_ATTEMPTS && currentPvsStartAttemptNumber < this.MAX_PVS_START_ATTEMPTS) {
				const success: ProcessCode = await proc.activate({
					enableNotifications: opt.enableNotifications,
					webSocketPort: this.webSocketPort,
					externalServer: opt.externalServer,
					verbose: opt.verbose
				});
				console.log(`[pvsProxy] pvs Process activate returned ${success}`);
				if (success === ProcessCode.PVS_NOT_FOUND) {
					this.pvsServerProcessStatus =  ProcessCode.PVS_NOT_FOUND;
				} else if (success === ProcessCode.UNSUPPORTED_PLATFORM) {
					this.pvsServerProcessStatus = ProcessCode.UNSUPPORTED_PLATFORM;
				} else if (success === ProcessCode.PVS_START_FAIL) {
					// try again
					console.log(`[pvsProxy.createPvsServer] Didn't get feedback from PVS (attempt #${currentPortAttemptNumber+1})`);
					currentPvsStartAttemptNumber++;
					await proc.kill(); 
				} else {
					serverIsReady = (success === ProcessCode.SUCCESS && proc.getReportedServerPort() !== undefined);
					if (success !== ProcessCode.SUCCESS) {
						// this.webSocketPort++;
						await proc.kill();
					} 
				}
				currentPortAttemptNumber++;
			};
			if (serverIsReady) {
				this.webSocketPort = proc.getReportedServerPort();
				if (!opt.externalServer) {
					if (connection) {
						connection.console.info(`[pvsProxy.createPvsServer] pvs-server active at ws://${this.webSocketAddress}:${this.webSocketPort}`);
					} else {
						console.info(`[pvsProxy.createPvsServer] pvs-server active at ws://${this.webSocketAddress}:${this.webSocketPort}`);
					}
				} else {
					console.info(`[pvsProxy.createPvsServer] using external pvs-server on port ${this.webSocketPort}`);
				}
				this.pvsServer = proc;
				this.pvsServerProcessStatus = ProcessCode.SUCCESS;
			} else if (currentPortAttemptNumber >= this.MAX_PORT_ATTEMPTS) {
				console.log(`[pvsProxy.createPvsServer] Failed to activate pvs-server at ws://${this.webSocketAddress}:${this.webSocketPort}`);
				this.pvsServerProcessStatus = ProcessCode.ADDR_IN_USE;
			} else { // currentPvsStartAttemptNumber >= this.MAX_PVS_START_ATTEMPTS
				console.log(`[pvsProxy.createPvsServer] Failed to start PVS process`);
				this.pvsServerProcessStatus = ProcessCode.PVS_START_FAIL;
			}	
		}
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
		if(this.secondaryConnReady())
			await this.sendRequestOnSecondaryConn('quit-all-proof-sessions');
		if(this.webSocket){
			if(this.webSocket.readyState === WebSocket.OPEN) {
				await this.webSocket.close();
			}
			await this.webSocket.terminate();	
			this.webSocket = null;
		}
		if(this.interruptConn){
			if(this.interruptConn.readyState === WebSocket.OPEN) {
				await this.interruptConn.close();
			}
			await this.interruptConn.terminate();
			this.interruptConn = null;
		}
		const serverKilled: boolean = await this.pvsServer.kill();		
		if (serverKilled){
			this.pvsServerProcessStatus = ProcessCode.TERMINATED;
			console.log("[pvs-proxy] Killed pvs-server");
		}
		else
			console.log("[pvs-proxy] Couldn't kill pvs-server");
	}
	/**
	 * Kill pvs-gui server
	 */
	async killPvsProxy(): Promise<void> {
		return new Promise((resolve, reject) => {
			if (this.webSocket) {
				// console.dir(this.guiServer, { depth: null });
				this.webSocket.close();
				this.interruptConn.close();
				if (this.debugMode) {
					console.log("[pvs-proxy] Closed pvs-proxy");
				}
				this.webSocket = null;
				this.interruptConn = null;
				resolve();
			} else {
				resolve();
			}
		});
	}

	async listSystemMethods(): Promise<string[]> {
		return new Promise((resolve, reject) => {
			if (this.webSocket) {
				const id = 'test1'
				const req = { method: 'list-methods', params: [], jsonrpc: "2.0", id: id };
				const jsonReq = JSON.stringify(req, null, " ");

				this.webSocket.send(jsonReq)
				this.webSocket.once('message', (value: string[]) => {
					console.log('[listSystemMethods]: have value = %s: %s', value, typeof (value));
					resolve(value);
				});
			} else {
				console.log('[pvs-proxy] Warning: socket is null :/');
				reject();
			}
		});
	}

	async rebootPvsServer(desc: { pvsPath?: string }): Promise<void> {
		this.pvsPath = desc?.pvsPath || "";
		console.log(`[pvs-proxy] New PVS path: ${this.pvsPath}`);
		await this.killPvsServer();
		await this.restartPvsServer();
	}

	protected async sendWorkspaceInfo(): Promise<void> {
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
	lispMode(): void {
		this.mode = "lisp";
	}

	/**
	 * Utility function, checks if the context folder is the pvs installation folder (pvsPath) or the internal library (pvsLibPath)
	 * @param contextFolder 
	 */
	isProtectedFolder(contextFolder: string): boolean {
		return contextFolder === this.pvsPath || contextFolder === path.join(this.pvsPath, "lib");
	}

	/**
	 * Utility function, restarts the pvs server
	 * @param opt 
	 */
	async restartPvsServer(opt?: { pvsPath?: string, pvsLibraryPath?: string, externalServer?: boolean }): Promise<ProcessCode> {
		opt = opt || {};

		console.log('[pvs-proxy.restartPvsServer] called');

		// set server mode to "lisp"
		this.mode = "lisp";

		// update pvsPath and pvsLibraryPath with the provided info
		if (opt) {
			if (opt.pvsPath) {
				this.pvsPath = opt.pvsPath;
				console.log(`[pvs-proxy.restartPvsServer] PVS path: ${this.pvsPath}`);
			}
			if (opt.pvsLibraryPath) {
				this.pvsLibraryPath = opt.pvsLibraryPath;
				console.log(`[pvs-proxy.restartPvsServer] PVS library path: ${this.pvsLibraryPath}`);
			}
		}

		console.info("[pvs-proxy.restartPvsServer] Rebooting pvs-server...");
		console.info(`[pvs-proxy.restartPvsServer] External Server: ${this.externalServer}`);

		// create PVS websocket server
		// if externalServer === true then method createPvsServer will create only 
		// the pvs process necessary to parse/typecheck files (via stdin/stdout).
		// Otherwise, createPvsServer starts pvs in server mode.
		await this.createPvsServer({
			enableNotifications: true,
			externalServer: this.externalServer,
			verbose: this.verbose
		});
		return this.pvsServerProcessStatus;
	}

	/**
	 * Other internal methods
	 */
	protected async disableGcPrintout(): Promise<PvsResponse> {
		return await this.pvsRequest('lisp', ['(setq *disable-gc-printout* t)']);
	}
	protected async namesInfo(fileName: string): Promise<PvsResponse> {
		return await this.pvsRequest('names-info', [fileName]);
	}
	protected async xmlrcpReset(): Promise<PvsResponse> {
		return await this.pvsRequest('reset');
	}

	async getPvsTemporaryFolder(opt?: { externalServer?: boolean }): Promise<string[]> {
		const ans: string[] = [];
		let response: PvsResponse = await this.lisp(`uiop:*temporary-directory*`);
		ans.push(response?.result);
		//response = await this.lisp(`(uiop:default-temporary-directory)`);
		//ans.push(response?.result);
		//response = await this.lisp(`(uiop:temporary-directory)`);
		//ans.push(response?.result);
		return ans;
	}
	async getPvsLibraryPath(opt?: { externalServer?: boolean }): Promise<string[]> {
		const response: PvsResponse = await this.lisp(`(format nil "~{~s~}" *pvs-library-path*)`);
		if (response && response.result) {
			const match: RegExpMatchArray = /"([\w\W]+)"/g.exec(response.result);
			if (match && match.length > 1 && match[1]) {
				const pvsLibraries: string[] = match[1].split(`""`);
				console.log(`[pvs-proxy.pvsLibraryPath] reported *pvs-library-path* : ${pvsLibraries}`);
				return pvsLibraries;
			}
		}
		return [];
	}
	getNasalibPath(): string {
		return fsUtils.tildeExpansion(path.join(this.pvsPath, "nasalib/"));
	}

	async setNasalibPath(opt?: { externalServer?: boolean }): Promise<PvsResponse | null> {
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

	async NasalibPresent(): Promise<boolean> {
		const response: PvsResponse = await this.pvsRequest('lisp', [`(fboundp 'nasalib-path)`]);
		return response && response.result !== "nil" && response.result !== "NIL";
	}

	async installProofliteScript(desc: PvsFormula, proofLiteScript: string): Promise<PvsResponse> {
		const escaped: string = proofLiteScript.replace(/"/g, "\\\"");
		const cmd: string = `(install-script "${desc.theoryName}" "${escaped}" (list "${desc.formulaName}") t)`;
		const data: PvsResponse = await this.pvsRequest('lisp', [cmd]);
		if (data && data.error) {
			console.error(data.error);
		} else if (data && data.result && typeof data.result === "string" && data.result.startsWith("Error:")) {
			console.error(data.result);
		}
		return null;
	}

	/**
	 * Updates pvs library path
	 * @param pvsLibraryPath colon-separated list of folders
	 */
	async pushPvsLibraryPath(opt?: { pvsLibraryPath?: string, useLisp?: boolean, externalServer?: boolean }): Promise<PvsResponse | null> {
		opt = opt || {};
		const lp: string = opt.pvsLibraryPath || this.pvsLibraryPath;
		const libs: string[] = (lp) ? lp.split(":").map((elem: string) => {
			elem = elem.trim();
			elem = fsUtils.tildeExpansion(elem);
			return elem.endsWith("/") ? elem : `${elem}/`
		}) : [];
		if (libs && libs.length) {
			const pvsLibraries: string[] = await this.getPvsLibraryPath(opt);
			for (let i = 0; i < libs.length; i++) {
				let path: string = libs[i].endsWith("/") ? libs[i] : `${libs[i]}/`;
				path = fsUtils.tildeExpansion(path);
				if (!pvsLibraries.includes(path)) {
					return await this.pvsRequest('add-pvs-library', [path]);
				}
			}
		}
		return null;
	}

	async patchesLoaded(): Promise<boolean> {
		const response: PvsResponse = await this.lisp(`(boundp '*pvs-patches*)`);
		return response && response.result === "t";
	}

	async loadPvsPatches(opt?: { externalServer?: boolean }): Promise<PvsResponse> {
		const alreadyLoaded: boolean = await this.patchesLoaded();
		if (!alreadyLoaded) {
			return await this.lisp(`(load-pvs-patches)`);
		}
		return null;
	}


	// async createGuiServer (opt?: { debugMode?: boolean, showBanner?: boolean }): Promise<boolean> {
	// 	opt = opt || {};
	// 	if (this.client) {
	// 	    return Promise.resolve(true);
	// 	}
	// 	return new Promise(async (resolve, reject) => {
	// 	    let portIsAvailable: boolean = (this.guiServer) ? true : false;
	// 	    for (let i = 0; !portIsAvailable && i < this.MAX_PORT_ATTEMPTS; i++) {
	// 		portIsAvailable = await this.checkPort(this.clientPort, true);
	// 		if (portIsAvailable === false) {
	// 		    this.clientPort++;
	// 		}
	// 	    }
	// 	    if (portIsAvailable) {
	// 		this.banner = `GUI Server active at http://${this.clientAddress}:${this.clientPort}`;
	// 		console.log(`[pvs-proxy] Starting GUI Server on http://${this.clientAddress}:${this.clientPort}`)
	// 		this.client = xmlrpc.createClient({
	// 		    host: this.webSocketAddress, port: this.webSocketPort, path: "/RPC2"
	// 		});
	// 		if (!this.client) {
	// 		    console.error(`[pvs-proxy] Error: could not create client necessary to connect to pvs-server`);
	// 		    resolve(false);
	// 		}
	// 		if (!this.guiServer) {
	// 		    try {
	// 			this.guiServer = xmlrpc.createServer({
	// 			    host: this.clientAddress, port: this.clientPort, path: "/RPC2"
	// 			}, () => {
	// 			    this.serverReadyCallBack();
	// 			    if (opt.showBanner) {
	// 				console.log("[pvs-proxy] " + this.banner);
	// 			    }
	// 			    resolve(true);
	// 			});
	// 			this.guiServer.once('error', (error: Error) => {
	// 			    console.error(error);
	// 			    if (error["code"] === 'EADDRINUSE') {
	// 				console.log(`[pvs-proxy] port ${this.clientPort} busy`);
	// 			    }
	// 			});
	// 		    } catch (gui_server_error) {
	// 			console.error(`[pvs-proxy]`, gui_server_error);
	// 			resolve(false);
	// 		    }
	// 		}
	// 	    } else {
	// 		console.error(`[pvs-proxy] Error: could not start GUI-server`);
	// 		resolve(false);
	// 	    }
	// 	});
	// }

	/**
	 * interrupts the prover
	 */
	async interruptProver(proofId: string): Promise<PvsResponse | null> {
		return await this.pvsRequest('interrupt-proof', [proofId]);
	}

}
