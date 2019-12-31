/**
 * @module PvsLanguageServer
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

import { 
	Connection, TextDocuments, TextDocument, CompletionItem, createConnection, ProposedFeatures, InitializeParams, 
	TextDocumentPositionParams, Hover, CodeLens, CodeLensParams,
	Diagnostic, Position, Range, DiagnosticSeverity, Definition,
	Location, TextDocumentChangeEvent, TextDocumentSyncKind
} from 'vscode-languageserver';
import { 
	PvsDefinition,
	FileList, TheoryDescriptor,
	ContextDescriptor,
	serverEvent,
	serverCommand,
	cliSessionType,
	PvsDownloadDescriptor
} from './common/serverInterface'
import { PvsCompletionProvider } from './providers/pvsCompletionProvider';
import { PvsDefinitionProvider } from './providers/pvsDefinitionProvider';
import { PvsHoverProvider } from './providers/pvsHoverProvider';
import { PvsCodeLensProvider } from './providers/pvsCodeLensProvider';
import { PvsLinter } from './providers/pvsLinter';
import { PvsCliGateway } from './pvsCliGateway';
import { getErrorRange } from './common/languageUtils';
import * as utils from './common/languageUtils';
import * as fsUtils from './common/fsUtils';
import * as path from 'path';
import { PvsProxy, ContextDiagnostics } from './pvsProxy';
import { ParseResult, PvsResponse, PvsError, PvsResult, ImportingDecl, TypedDecl, FormulaDecl } from './common/pvs-gui';
import { PvsPackageManager } from './providers/pvsPackageManager';

export interface PvsTheoryDescriptor {
	id?: string;
	/**
	 * an array of declarations in the given theory
	 */
	decls?: (ImportingDecl | TypedDecl | FormulaDecl)[];
}

// Example server settings, this is not used at the moment
interface Settings {
	maxNumberOfProblems: number;
}

const DEBUG_MODE: boolean = true;


export class PvsLanguageServer {
	// pvs path, context folder, server path
	protected pvsPath: string;
	// protected pvsVersion: { "pvs-version": string, "lisp-version": string };

	// constants
	protected logFolder: string = "pvslog";
	protected logFileExtension: string = ".pr";
	// timers
	protected timers: { [ key:string ]: NodeJS.Timer } = {};
	// pvs server & proxy
	protected pvsProxy: PvsProxy;
	// connection to the client
	protected connection: Connection;
	// list of documents opened in the editor
	protected documents: TextDocuments;
	// client capabilities
	protected clientCapabilities: {
		hasConfigurationCapability: boolean,
		hasWorkspaceFolderCapability: boolean,
		hasDiagnosticRelatedInformationCapability: boolean
	}
	// client settings
	protected settings: {
		global: Settings,
		documents: Map<string, Thenable<Settings>>
	}
	/**
	 * Service providers necessary to support the language server APIs
	 */
	protected completionProvider: PvsCompletionProvider;
	protected definitionProvider: PvsDefinitionProvider;
	protected hoverProvider: PvsHoverProvider;
	protected codeLensProvider: PvsCodeLensProvider;
	protected linter: PvsLinter;
	protected cliGateway: PvsCliGateway;

	/**
	 * Data structures used for performance improvements
	 */
	protected lastParsedContext: string; // this is used to avoid re-parsing a context

	/**
	 * @constructor
	 */
	constructor () {
		this.settings = {
			global: { maxNumberOfProblems: 1000 },
			documents: new Map()
        };
		this.clientCapabilities = {
			hasConfigurationCapability: false,
			hasWorkspaceFolderCapability: false,
			hasDiagnosticRelatedInformationCapability: false
		};
		// Create a connection channel to allow clients to connect.
		// The connection uses Node's IPC as a transport. Includes all proposed LSP features.
		this.connection = createConnection(ProposedFeatures.all);
		this.setupConnectionManager();

		// Create a simple text document manager. The text document manager supports full document sync only
		this.setupDocumentsManager(this.connection);

		// Listen on the connection
		this.connection.listen();
	}
	
	/**
	 * Internal function, checks that pvsProxy and desc are defined
	 * @param desc 
	 */
	protected checkArgs (methodID: string, desc: { fileName?: string, fileExtension?: string, contextFolder: string, theoryName?: string, formulaName?: string, cmd?: string }): boolean {
		if (desc && desc.contextFolder) {
			if (this.pvsProxy) {
				if (methodID === "proveFormula") {
					return desc.formulaName !== null && desc.formulaName !== undefined && desc.formulaName !== "";
				}
				return true;
			} else {
				console.error(`[pvs-language-server.${methodID}] Error: pvs proxy is null`);
			}
		} else {
			console.error(`[pvs-language-server.${methodID}] Error: descriptor is null or malformed`);
		}
		return false;
	}


	//--------------------------------------------------------------------
	//                                APIs
	//--------------------------------------------------------------------

	/**
	 * Send proof command
	 * @param args Handler arguments: filename, file extension, context folder, theory name, formula name, prover command
	 */
	async proofCommand (args: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string, cmd: string }): Promise<PvsResponse> {
		if (args) {
			const response: PvsResponse = await this.pvsProxy.proofCommand(args);
			const status: PvsResponse = await this.pvsProxy.proverStatus();
			// ATTN: when quitting the proof, pvs-server returns an object { result: string }, where the string indicates the proof status (completed, unfinished, ...)
			if (response && response.result) {
				return response;
			}
		} else {
			console.error('[pvs-language-server] Error: proofCommand invoked with null descriptor');
		}
		return null;
	}
	async proofCommandRequest (request: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string, cmd: string }): Promise<void> {
		const response: PvsResponse = await this.proofCommand(request);

		if (response) {
			if (response.result) {
				// the following additional logic is necessary to create the log file and start up the interactive cli session
				const proofLogPath: string = path.join(request.contextFolder, this.logFolder);
				const channelID: string = utils.desc2id(request);
				const pvsLogFile: string = path.join(proofLogPath, `${channelID}${this.logFileExtension}`);
				await fsUtils.createFolder(proofLogPath);
				await fsUtils.writeFile(pvsLogFile, utils.formatProofState(response.result));

				this.connection.sendRequest(serverEvent.proofCommandResponse, { response, args: request });
				this.cliGateway.publish({ type: serverEvent.proofStateUpdate, channelID, data: response });
				this.connection.sendRequest(serverEvent.proofStateUpdate, { response, args: request, pvsLogFile });
			} else {
				console.error("[pvs-langauge-server.proofCommandRequest] Warning: proof-command returned error", response);
			}
		} else {
			console.error("[pvs-language-server.proofCommandRequest] Warning: proof-command returned null");
		}
	}
	/**
	 * Prove formula
	 * @param args Handler arguments: filename, file extension, context folder, theory name, formula name
	 */
	async proveFormula (args: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string }): Promise<PvsResponse> {
		if (this.checkArgs("proveFormula", args)) {
			try {
				const response: PvsResponse = await this.pvsProxy.proveFormula(args);
				return response;
			} catch (ex) {
				console.error('[pvs-language-server.proveFormula] Error: pvsProxy has thrown an exception', ex);
				return null;
			}
		}
		return null;
	}
	async proveFormulaRequest (request: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string }): Promise<void> {
		const response: PvsResponse = await this.proveFormula(request);
		if (response) {
			if (response.result) {
				// the following additional logic is necessary to create the log file and start up the interactive cli session
				const proofLogPath: string = path.join(request.contextFolder, this.logFolder);
				const channelID: string = utils.desc2id(request);
				const pvsLogFile: string = path.join(proofLogPath, `${channelID}${this.logFileExtension}`);
				await fsUtils.createFolder(proofLogPath);
				await fsUtils.writeFile(pvsLogFile, utils.formatProofState(response.result));

				this.cliGateway.publish({ type: serverEvent.proofStateUpdate, channelID, data: response });
				this.connection.sendRequest(serverEvent.proveFormulaResponse, { response, args: request, pvsLogFile });
				this.connection.sendRequest(serverEvent.proofStateUpdate, { response, args: request, pvsLogFile });
			} else {
				console.error("[pvs-langauge-server.proveFormulaRequest] Warning: prove-formula returned error", response);
			}
		} else {
			console.error("[pvs-language-server] Warning: prove-formula returned null");
		}
	}
	/**
	 * CURRENTLY NOT SUPPORTED BY PVS-SERVER
	 * PVSio evaluator prompt
	 * @param args Handler arguments: filename, file extension, context folder, theory name, formula name
	 */
	// async pvsioEvaluator (args: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string }): Promise<PvsResponse> {
	// 	if (this.checkArgs("pvsioEvaluator", args)) {
	// 		try {
	// 			const response: PvsResponse = await this.pvsProxy.pvsioEvaluator(args);
	// 			return response;
	// 		} catch (ex) {
	// 			console.error('[pvs-language-server.proveFormula] Error: pvsProxy has thrown an exception', ex);
	// 			return null;
	// 		}
	// 	}
	// 	return null;
	// }
	// async pvsioEvaluatorRequest (request: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string }): Promise<void> {
	// 	const response: PvsResponse = await this.pvsioEvaluator(request);
	// 	if (response) {
	// 		// the following additional logic is necessary to create the log file and start up the interactive cli session
	// 		const channelID: string = utils.desc2id(request);
	// 		this.cliGateway.publish({ type: serverEvent.pvsioEvaluatorResponse, channelID, data: response });
	// 		this.connection.sendRequest(serverEvent.pvsioEvaluatorResponse, { response, args: request });
	// 	} else {
	// 		console.error("[pvs-language-server] Warning: prove-formula returned null");
	// 	}
	// }
	/**
	 * Show proof script
	 * @param args Handler arguments: filename, file extension, context folder, theory name, formula name
	 */
	async proofScript (args: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string }): Promise<PvsResponse> {
		if (this.checkArgs("showProof", args)) {
			try {
				const response: PvsResponse = await this.pvsProxy.proofScript(args);
				return response;
			} catch (ex) {
				console.error('[pvs-language-server.showProof] Error: pvsProxy has thrown an exception', ex);
				return null;
			}
		}
		return null;
	}
	async proofScriptRequest (request: { fileName: string, fileExtension: string, theoryName: string, formulaName: string, contextFolder: string }): Promise<void> {
		const response: PvsResponse = await this.proofScript(request);
		if (response) {
			this.connection.sendRequest(serverEvent.proofScriptResponse, { response, args: request });
		} else {
			console.error("[pvs-language-server] Warning: show-proof returned null");
		}
	}

	/**
	 * Typecheck file
	 * @param args Handler arguments: filename, file extension, context folder
	 */
	async typecheckFile (args: { fileName: string, fileExtension: string, contextFolder: string }): Promise<PvsResponse> {
		if (this.checkArgs("typecheckFile", args)) {
			try {
				const response: PvsResponse = await this.pvsProxy.typecheckFile(args);
				
				// proof status
				// const proofStatus: { [fname: string ]: PvsResponse } = await this.statusProof(args);
				//...

				return response;
			} catch (ex) {
				console.error('[pvs-language-server.typecheckFile] Error: pvsProxy has thrown an exception', ex);
				return null;
			}
		}
		return null;
	}
	async typecheckFileRequest (request: string | { fileName: string, fileExtension: string, contextFolder: string }): Promise<void> {
		if (request) {
			const desc: {
				fileName: string, 
				fileExtension: string, 
				contextFolder: string 
			} = (typeof request === "string") ? fsUtils.fname2desc(request) : request;
			if (desc) {
				const response: PvsResponse = await this.typecheckFile(desc);
				this.connection.sendRequest(serverEvent.typecheckFileResponse, { response, args: request });
				// send diagnostics
				const diags: ContextDiagnostics = {};
				if (response) {
					if (response.error && response.error.data) {
						const fname: string = response.error.data.file_name;
						diags[fname] = response;
						this.sendDiagnostics(diags, desc.contextFolder, "Typecheck");
					} else {
						const fname: string = fsUtils.desc2fname(desc);
						diags[fname] = response;
						this.sendDiagnostics(diags, desc.contextFolder, "Typecheck");
					}
				} else {
					// clear diagnostics, as the typecheck error may have gone and we don't know because pvs-server failed to execute typecheckFile
					const fname: string = fsUtils.desc2fname(desc);
					diags[fname] = null;
				}
			} else {
				console.error("[pvs-language-server] Warning: pvs.typecheck-file is unable to identify filename for ", request);
			}
		} else {
			console.error("[pvs-language-server] Warning: pvs.typecheck-file invoked with null request");
		}
	}
	/**
	 * Returns all tccs for a given theory
	 * @param args Handler arguments: filename, file extension, context folder
	 */
	async showTccsTheory (args: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string }): Promise<PvsResponse> {
		return await this.pvsProxy.showTccs(args);
	}
	/**
	 * Returns all tccs for a given file
	 * @param args Handler arguments: filename, file extension, context folder
	 */
	async showTccs (args: { fileName: string, fileExtension: string, contextFolder: string }): Promise<{ [ theoryName: string ]: PvsResponse }> {
		if (this.checkArgs("showTccs", args)) {
			try {
				const res: { [ theoryName: string ]: PvsResponse } = {};
				// fetch theory names
				const fname: string = fsUtils.desc2fname(args);
				const theories: TheoryDescriptor[] = await utils.listTheoriesInFile(fname);
				for (let i = 0; i < theories.length; i++) {
					const theoryName: string = theories[i].theoryName;
					const response: PvsResponse = await this.showTccsTheory({
						fileName: args.fileName, fileExtension: args.fileExtension, contextFolder: args.contextFolder, theoryName
					});
					if (response && response.result) {
						res[theoryName] = response; 
					} else {
						console.error(`[pvs-language-server.showTccs] Warning: tccs could not be generated ${response}`);
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
	async showTccsRequest (request: string | { fileName: string, fileExtension: string, contextFolder: string }): Promise<void> {
		if (request) {
			const desc: {
				fileName: string, 
				fileExtension: string, 
				contextFolder: string 
			} = (typeof request === "string") ? fsUtils.fname2desc(request) : request;
			if (desc) {
				const response: { [ theoryName: string ]: PvsResponse } = await this.showTccs(desc);
				this.connection.sendRequest(serverEvent.showTccsResponse, { response, args: request });
			} else {
				console.error("[pvs-language-server] Warning: pvs.typecheck-file is unable to identify filename for ", request);
			}
		} else {
			console.error("[pvs-language-server] Warning: pvs.typecheck-file invoked with null request");
		}
	}
	/**
	 * Typecheck file and prove all tccs
	 * @param args Handler arguments: filename, file extension, context folder
	 */
	async proveFile (args: { fileName: string, fileExtension: string, contextFolder: string }): Promise<PvsResponse> {
		if (this.checkArgs("proveFile", args)) {
			try {
				return await this.pvsProxy.proveFile(args);
			} catch (ex) {
				console.error('[pvs-language-server.proveFile] Error: pvsProxy has thrown an exception', ex);
				return null;
			}
		}
		return null;
	}
	/**
	 * Parse file
	 * @param args Handler arguments: filename, file extension, context folder
	 */
	async parseFile (args: { fileName: string, fileExtension: string, contextFolder: string }): Promise<PvsResponse> {
		if (this.checkArgs("parseFile", args)) {
			try {
				return await this.pvsProxy.parseFile(args);
			} catch (ex) {
				console.error('[pvs-language-server.parseFile] Error: pvsProxy has thrown an exception', ex);
				return null;
			}
		}
		return null;
	}
	async parseFileRequest (request: string | { fileName: string, fileExtension: string, contextFolder: string }): Promise<void> {
		if (request) {
			const desc: {
				fileName: string, 
				fileExtension: string, 
				contextFolder: string 
			} = (typeof request === "string") ? fsUtils.fname2desc(request) : request;
			if (desc) {
				// const response: PvsResponse = await this.parseFile(desc);
				// const diags: ContextDiagnostics = {};
				const response: PvsResponse = await this.parseFile(desc);
				const diags: ContextDiagnostics = {};
				const fname: string = fsUtils.desc2fname(desc);
				if (response) {
					// send parser response
					this.connection.sendRequest(serverEvent.parseFileResponse, response);
					// collect diagnostics
					diags[fname] = response;
				} else {
					// clear diagnostics, as the parse error may have gone and we don't know because pvs-server failed to execute parseFile
					diags[fname] = null;
				}
				// send diagnostics
				this.sendDiagnostics(diags, desc.contextFolder, "Parse");
			} else {
				console.error("[pvs-language-server] Warning: pvs.parse-file is unable to identify filename for ", request);
			}
		} else {
			console.error("[pvs-language-server] Warning: pvs.parse-file invoked with null request");
		}
	}
	/**
	 * Parse all files in a given context
	 * @param args Handler arguments: context folder
	 */
	async parseContext (args: { contextFolder: string }): Promise<ContextDiagnostics> {
		if (this.checkArgs("parseContext", args)) {
			if (args.contextFolder !== this.lastParsedContext || args.contextFolder !== path.join(this.lastParsedContext, "pvslog")) {
				this.lastParsedContext = args.contextFolder;
				try {
					return await this.pvsProxy.parseContext(args.contextFolder);
				} catch (ex) {
					console.error('[pvs-language-server.parseContext] Error: pvsProxy has thrown an exception', ex);
					return null;
				}
			}
		}
		return null;
	}
	async parseContextRequest (request: string | { contextFolder: string }): Promise<void> {
		if (request) {
			const desc: { contextFolder: string } = (typeof request === "string") ? { contextFolder: request } : request;
			// send context update first -- so the front-end can be updated promptly
			const cdesc: ContextDescriptor = await this.getContextDescriptor(desc);
			this.connection.sendRequest(serverEvent.contextUpdate, cdesc);
			// parse context
			const diags: ContextDiagnostics = await this.parseContext(desc);
			this.sendDiagnostics(diags, desc.contextFolder, "Parse error");
		} else {
			console.error("[pvs-language-server] Warning: pvs.parse-context invoked with null request");
		}
	}
	/**
	 * Returns the list of pvs files in a given context folder
	 */
	async listContextFiles (args: { contextFolder: string }): Promise<FileList> {
		if (args && args.contextFolder) {
			return await fsUtils.listPvsFiles(args.contextFolder);
		}
		return null;
	}
	async listContextFilesRequest (request: string | { contextFolder: string }): Promise<void> {
		const args = (typeof request === "string") ? { contextFolder: request } : request;
		if (args && args.contextFolder) {
			const response: FileList = await this.listContextFiles(args);
			this.connection.sendRequest(serverEvent.listContextResponse, response);
		}
	}
	/**
	 * Returns the proof status for all theorems in a given theory
	 */
	async statusProofTheory (args: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string }): Promise<PvsResponse> {
		return await this.pvsProxy.statusProofTheory(args);
	}
	/**
	 * Returns the proof status for all theorems in a given file
	 */
	async statusProof (args: { fileName: string, fileExtension: string, contextFolder: string }): Promise<{ [ fname: string ]: PvsResponse }> {
		let res: { [ fname: string ]: PvsResponse } = {};
		// fetch theory names
		const fname: string = fsUtils.desc2fname(args);
		const theories: TheoryDescriptor[] = await utils.listTheoriesInFile(fname);
		for (let i = 0; i < theories.length; i++) {
			const proofStatus: PvsResponse = await this.statusProofTheory({
				contextFolder: args.contextFolder,
				fileName: args.fileName,
				fileExtension: args.fileExtension,
				theoryName: theories[i].theoryName
			});
			if (proofStatus && proofStatus.response) {
				res[fname] = proofStatus;
			} else {
				console.error("[pvs-language-server] Warning: status proof indicates error for theory ", theories[i].theoryName);
			}
		}
		return res;
	}
	/**
	 * Returns a descriptor containing information about theories in the prelude
	 */
	async getPreludeDescriptor (): Promise<ContextDescriptor> {
		return await this.cachePreludeDescriptor();
	}
	/**
	 * Returns a descriptor with information on all theories in a given context folder
	 * @param contextFolder Context folder
	 */
	async getContextDescriptor (args: { contextFolder: string }): Promise<ContextDescriptor> {
		if (args) {
			if (this.pvsProxy) {
				if (this.pvsProxy.isProtectedFolder(args.contextFolder)) {
					return await this.getPreludeDescriptor();
				} // else
				return await utils.getContextDescriptor(args.contextFolder, this.connection);
			} else {
				console.error('[pvs-language-server.listTheories] Error: pvs proxy is null');
			}
		} else {
			console.error('[pvs-language-server.listTheories] Error: getContextDescriptor invoked with null descriptor');
		}
		return null;
	}



	// async typecheckFileAndShowTccs (desc: { fileName: string, fileExtension: string, contextFolder: string }): Promise<ContextDescriptor> {
	// 	if (desc) {
	// 		const ans: PvsResponse = await this.typecheckFile(desc);
	// 		if (ans) {
	// 			if (ans.error) {
	// 				// send diagnostics
	// 				this.sendDiagnostics(desc.contextFolder, ans.error, "Typecheck error");
	// 			} else if (ans.result) {
	// 				const fileName: string = desc.fileName;
	// 				const fileExtension: string = desc.fileExtension;
	// 				const contextFolder: string = desc.contextFolder;
	// 				const fname: string = path.join(desc.contextFolder, `${desc.fileName}${desc.fileExtension}`);
	// 				const response: ContextDescriptor = {
	// 					contextFolder,
	// 					theories: []
	// 				};
	// 				if (ans.result.length) {
	// 					const theories: { [ key: string ]: TheoryDescriptor } = await utils.mapTheoriesInFile(fname);
	// 					for (let i = 0; i < ans.result.length; i++) {
	// 						const desc: PvsTheoryDescriptor = ans.result[i];
	// 						const theoryName: string = desc.id;
	// 						const theoryPosition: Position = (theories) ? theories[theoryName].position : null;
	// 						if (theoryPosition === null) {
	// 							console.error(`[pvs-language-server] Warning: could not find theory ${theoryName} in file ${fname}`);
	// 						}

	// 						// get tccs
	// 						const tcc: PvsResult = await this.pvsProxy.showTccs({ fileName, fileExtension, contextFolder, theoryName });

	// 						// get theorems
	// 						const proofStatus: PvsResult = await this.pvsProxy.statusProofTheory({ contextFolder, theoryName });

	// 						const formulaDeclarations: FormulaDecl[] = (desc.decls) ? <FormulaDecl[]> desc.decls.filter((elem: ImportingDecl | TypedDecl | FormulaDecl) => {
	// 							return elem.kind === "formula";
	// 						}) : null;
	// 						if (formulaDeclarations && formulaDeclarations.length) {
	// 							const theorems = formulaDeclarations.map((elem: FormulaDecl) => {
	// 								if (elem.place && elem.place.length > 1) {
	// 									// theorem
	// 									return {
	// 										fileName, fileExtension, contextFolder, theoryName,
	// 										formulaName: elem.id,
	// 										position: { line: elem.place[0], character: elem.place[1] },
	// 										status: "proved", // ????? get this info from proofstatus, when the functionality is fixed in pvs-server
	// 										isTcc: elem.place === null
	// 									};
	// 								} else {
	// 									// tcc
	// 									return {
	// 										fileName, fileExtension, contextFolder, theoryName,
	// 										formulaName: elem.id,
	// 										position: { line: 0, character: 0 }, // FIXME
	// 										status: "proved", // ????? get this info from proofstatus, when the functionality is fixed in pvs-server
	// 										isTcc: elem.place === null
	// 									};
	// 								}
	// 							});
								
	// 							response.theories.push({
	// 								fileName, fileExtension, contextFolder, theoryName,
	// 								position: theoryPosition || { line: 0, character: 0 },
	// 								theorems // this includes the status of theorems and tccs
	// 							});
	// 							// note: showTccs automatically trigger typechecking. However, we are still calling typecheckFile because we want to save the binary files
	// 							// const pvsResponse: PvsResponseType = await typechecker.showTccs(fileName, theoryName);
	// 							// const tccArray: TccDescriptor[] = (pvsResponse && pvsResponse.res) ? pvsResponse.res : [];
	// 						}
	// 					}
	// 					// report updated list/status of theorems
	// 					this.connection.sendNotification('pvs.context.theories-status.update', response);
	// 				}
	// 				return response;
	// 			}
	// 		}
	// 	} else {
	// 		console.error('[pvs-language-server] Error: pvs.typecheck-file request invoked with null descriptor');
	// 	}
	// 	return null;
	// }

	//------------------------------------------------------------------------------------------
	//                          INTERNAL FUNCTIONS
	//------------------------------------------------------------------------------------------

	/**
	 * Internal function, install document managers
	 * @param connection 
	 */
	protected setupDocumentsManager (connection: Connection) {
		this.documents = new TextDocuments();

		// -- Close the file only if it has been deleted from the context?
		this.documents.onDidClose((close: TextDocumentChangeEvent) => {
			if (close && close.document && close.document.uri) {
				const uri: string = close.document.uri;
				// remove file descriptor
				this.settings.documents.delete(close.document.uri);
				// update diagnostics, if this is a pvs file
				if (fsUtils.isPvsFile(uri)) {
					const diags: ContextDiagnostics = {};
					const fname: string = uri.replace("file://", "");
					const contextFolder: string = fsUtils.getContextFolder(fname);
					diags[fname] = null;
					this.sendDiagnostics(diags, contextFolder, "** deleted file **");
				}
			}
		});

		// -- This event fires when the text document is first opened in the editor or when then content of the opened document has changed.
		// this.documents.onDidChangeContent(change => {
		// 	console.log(change);
		// });

		// onDidOpen fires when a document is opened in the editor
		// this.documents.onDidOpen(async (open: TextDocumentChangeEvent) => {
		// 	if (this.pvsParser) {
		// 		const fileName: string = fsUtils.getFilename(open.document.uri);
		// 		const contextFolder: string = fsUtils.getContextFolder(open.document.uri);
		// 		if (this.contextFolder !== contextFolder) {
		// 			// parse all files in the context -- this is necessary in pvs to create the data structures for resolving names
		// 			await this.changeContextAndParseFiles(contextFolder);
		// 		}
		// 	}
		// });

		// onDidSave fires when a document is saved on the editor
		this.documents.onDidSave(async (save: TextDocumentChangeEvent) => {
			if (save && save.document && save.document.uri) {
				// parse file
				const contextFolder: string = fsUtils.getContextFolder(save.document.uri);
				const fileName: string = fsUtils.getFileName(save.document.uri);
				const fileExtension: string = fsUtils.getFileExtension(save.document.uri);
				this.parseFileRequest({ fileName, fileExtension, contextFolder }); // async call, will automatically send diags to the client
			}
		});

		// Listen to document events triggered by the editor
		this.documents.listen(connection);
	}
	/**
	 * Internal function, used by restartPvs
	 */
	protected createServiceProviders(): PvsLanguageServer {
		// Create service providers
		this.definitionProvider = new PvsDefinitionProvider(this.pvsProxy, this.documents);
		this.completionProvider = new PvsCompletionProvider(this.definitionProvider);
		this.codeLensProvider = new PvsCodeLensProvider(this.definitionProvider);
		this.hoverProvider = new PvsHoverProvider(this.definitionProvider);
		this.linter = new PvsLinter();
		this.cliGateway = new PvsCliGateway(this);
		return this;
	}
	/**
	 * Internal function, used by typecheckFile
	 */
	// protected async makeContextDescriptor (response: PvsResponse, desc: { fileName: string, fileExtension: string, contextFolder: string }): Promise<ContextDescriptor> {
	// 	if (response && response.result) {
	// 		const result: ParseResult = <ParseResult> response.result;
	// 		// fetch theory names
	// 		const fname: string = this.desc2fname(desc);
	// 		const theories: { [ key: string ]: TheoryDescriptor } = await utils.mapTheoriesInFile(fname);
	// 		// prepare response
	// 		const ctxDesc: ContextDescriptor = {
	// 			contextFolder: desc.contextFolder,
	// 			theories: []
	// 		};
	// 		// get info for each theory
	// 		for (let i = 0; i < result.length; i++) {
	// 			const tdesc: PvsTheoryDescriptor = result[i];
	// 			const theoryName: string = tdesc.id;
	// 			const theoryPosition: Position = (theories) ? theories[theoryName].position : null;
	// 			if (theoryPosition === null) {
	// 				console.error(`[pvs-language-server] Warning: could not find theory ${theoryName} in file ${fname}`);
	// 			}
	// 			// get tccs
	// 			const tcc: PvsResult = await this.pvsProxy.showTccs({
	// 				fileName: desc.fileName, fileExtension: desc.fileExtension, contextFolder: desc.contextFolder, theoryName
	// 			});
	// 			// get theorem status
	// 			const proofStatus: PvsResult = await this.pvsProxy.statusProofTheory({
	// 				contextFolder: desc.contextFolder,
	// 				fileName: desc.fileName,
	// 				fileExtension: desc.fileExtension,
	// 				theoryName
	// 			});

	// 			const formulaDeclarations: FormulaDecl[] = (tdesc.decls) ? <FormulaDecl[]> tdesc.decls.filter((elem: ImportingDecl | TypedDecl | FormulaDecl) => {
	// 				return elem.kind === "formula";
	// 			}) : null;
	// 			if (formulaDeclarations && formulaDeclarations.length) {
	// 				const theorems = formulaDeclarations.map((elem: FormulaDecl) => {
	// 					if (elem.place && elem.place.length > 1) {
	// 						// theorem
	// 						return {
	// 							fileName: desc.fileName,
	// 							fileExtension: desc.fileExtension,
	// 							contextFolder: desc.contextFolder, 
	// 							theoryName,
	// 							formulaName: elem.id,
	// 							position: { line: elem.place[0], character: elem.place[1] },
	// 							status: "proved", // ????? get this info from proofstatus, when the functionality is fixed in pvs-server
	// 							isTcc: elem.place === null
	// 						};
	// 					} else {
	// 						// tcc
	// 						return {
	// 							fileName: desc.fileName, 
	// 							fileExtension: desc.fileExtension, 
	// 							contextFolder: desc.contextFolder, 
	// 							theoryName,
	// 							formulaName: elem.id,
	// 							position: { line: 0, character: 0 }, // FIXME
	// 							status: "proved", // ????? get this info from proofstatus, when the functionality is fixed in pvs-server
	// 							isTcc: elem.place === null
	// 						};
	// 					}
	// 				});
	// 				ctxDesc.theories.push({
	// 					fileName: desc.fileName, 
	// 					fileExtension: desc.fileExtension, 
	// 					contextFolder: desc.contextFolder, 
	// 					theoryName,
	// 					position: theoryPosition || { line: 0, character: 0 },
	// 					theorems // this includes the status of theorems and tccs
	// 				});
	// 				// note: showTccs automatically trigger typechecking. However, we are still calling typecheckFile because we want to save the binary files
	// 				// const pvsResponse: PvsResponseType = await typechecker.showTccs(fileName, theoryName);
	// 				// const tccArray: TccDescriptor[] = (pvsResponse && pvsResponse.res) ? pvsResponse.res : [];
	// 			}
	// 		}
	// 		return ctxDesc;
	// 	}
	// 	return null;
	// }
	/**
	 * Internal function, sends diagnostics to the client
	 * @param data Diagnostics data
	 */
	protected async sendDiagnostics (diags: ContextDiagnostics, contextFolder: string, source: string): Promise<void> {
		if (diags) {
			const fnames: string[] = Object.keys(diags);
			if (fnames && fnames.length > 0) {
				for (let i = 0; i < fnames.length; i++) {
					let fname: string = fnames[i];
					const response: PvsResponse = diags[fname];
					if (response && response["error"]) {
						const info: PvsError = <PvsError> response;

						// old parser
						if (info.error && info.error.data && info.error.data.place && info.error.data.place.length >= 2) {
							const errorPosition: Position = { line: info.error.data.place[0], character: info.error.data.place[1] };
							fname = fname.includes("/") ? fname : path.join(contextFolder, fname); // FIXME: pvs-server does not include path when the file is in the current context -- this inconsistency be fixed
							const txt: string = await fsUtils.readFile(fname);
							if (txt) {
								const errorRange: Range = getErrorRange(txt, errorPosition);
								const diag: Diagnostic = {
									severity: DiagnosticSeverity.Error,
									range: {
										start: { line: errorRange.start.line - 1, character: errorRange.start.character },
										end: { line: errorRange.end.line - 1, character: errorRange.end.character },
									},
									message: info.error.data.error_string,
									source: `\n${source} error`
								};
								this.connection.sendDiagnostics({ uri: `file://${fname}`, diagnostics: [ diag ] });
							} else {
								console.error(`[pvs-language-server] Warning: unable to send error diagnostics for file ${fname}`);
							}
						}
						// new parser
						else if (info.error && info.error.data && info.error.data.length > 0) {
							const diagnostics: Diagnostic[] = <Diagnostic[]> info.error.data.map(diag => {
								return {
									range: {
										start: { line: diag.range.start.line - 1, character: diag.range.start.character }, // lines in the editor start from 0
										end: { line: diag.range.end.line - 1, character: diag.range.end.character }
									},
									message: diag.message,
									severity: diag.severity
								}
							});
							this.connection.sendDiagnostics({ uri: `file://${fname}`, diagnostics });
						}
					} else {
						// send clean diagnostics
						this.connection.sendDiagnostics({ uri: `file://${fname}`, diagnostics: [ ] });
					}
				}
			}
		}
	}
	/**
	 * Internal function reads the content of a file
	 * @param fileName Path to the filename
	 * @returns Promise<string> Promise that resolves to the content of the file
	 */
	protected async readFile (fileName: string): Promise<string> {
		if (fileName) {
			fileName = fileName.startsWith("file://") ? fileName = fileName.replace("file://", "") : fileName;
			const doc: TextDocument = this.documents.get("file://" + fileName);
			if (doc) {
				return doc.getText();
			}
			try {
				return fsUtils.readFile(fileName);
			} catch (readError) {
				console.error(`[pvs-language-server] Warning: Error while reading file ${fileName} (${readError.message})`)
				this.connection.sendNotification("pvs-error", `Error while reading file ${fileName} (${readError.message})`);
			}
		} else {
			console.error("[pvs-language-server] Warning: trying to read null filename");
		}
		return null;
	}


	// protected async parallelTypeCheckAllAndShowTccs(contextFolder: string): Promise<void> {
	// 	// const contextFolder: string = this.contextFolder;
	// 	const pvsFiles: FileList = await fsUtils.listPvsFiles(contextFolder);
	// 	const promises = [];
	// 	this.connection.sendNotification('server.status.info', "Allocating pool of typecheckers...");
	// 	for (const i in pvsFiles.fileNames) {
	// 		promises.push(new Promise (async (resolve, reject) => {
	// 			const fname: string = pvsFiles.fileNames[i];
	// 			this.connection.sendNotification('server.status.info', "Typechecking " + fname);
	// 			const proc: PvsProcess = await this.createPvsProcess({
	// 				enableNotifications: true
	// 			});
	// 			if (proc) {
	// 				this.dynamicPool[proc.getProcessID()] = proc;
	// 				const response: TheoriesMap = await this.typecheckFileAndShowTccs(fname);
	// 				if (this.dynamicPool[proc.getProcessID()]) {
	// 					this.dynamicPool[proc.getProcessID()].kill();
	// 				}
	// 				// the list of proof obligations is provided incrementally to the client so feedback can be shown as soon as available
	// 				this.connection.sendRequest("server.response.typecheck-file-and-show-tccs", response);
	// 			}
	// 			resolve();	
	// 		}));
	// 	}
	// 	// parallel typechecking
	// 	await Promise.all(promises);
	// 	this.connection.sendNotification('server.status.info', "Typechecking complete!");
	// 	this.pvsReady();
	// }
	// protected async serialTypeCheckAllAndShowTccs(contextFolder: string): Promise<void> {
	// 	// const contextFolder: string = this.contextFolder;
	// 	const pvsFiles: FileList = await fsUtils.listPvsFiles(contextFolder);
	// 	for (const i in pvsFiles.fileNames) {
	// 		const fname: string = pvsFiles.fileNames[i];
	// 		const fileName: string = fsUtils.getFileName(fname);
	// 		const fileExtension: string = fsUtils.getFileExtension(fname);
	// 		const contextFolder: string = fsUtils.getContextFolder(fname);
	// 		const response: PvsResponse = await this.typecheckFile({ fileName, fileExtension, contextFolder });
	// 		if (response && response.result) {
	// 			const contextDescriptor: ContextDescriptor = await this.makeContextDescriptor(response, { fileName, fileExtension, contextFolder });
	// 			// the list of proof obligations is provided incrementally to the client so feedback can be shown as soon as available
	// 			this.connection.sendRequest(serverEvent.contextUpdate, contextDescriptor);
	// 		}
	// 	}
	// }

	// an important difference between parallelTCP and serialTCP is that serialTCP reuses the same pvs process
	protected async serialTCP(fname: string, theoryNames: string[], contextFolder: string): Promise<ContextDescriptor> {
		if (this.pvsProxy) {
			const fileName: string = fsUtils.getFileName(fname);
			const fileExtension: string = fsUtils.getFileExtension(fname);
			// const contextFolder: string = this.contextFolder;
			const typecheckerResponse: PvsResult = await this.pvsProxy.typecheckFile({ fileName, fileExtension, contextFolder }); //this.pvsProxy.typecheckProve({ fileName, fileExtension, contextFolder });
			// const tccs: TheoriesStatusMap = {};
			// for (const i in theoryNames) {
			// 	const theoryName: string = theoryNames[i];
			// 	// const pvsResponse: PvsResponseType = await proc.showTccs(fileName, theoryNames[i]);
			// 	// const tccArray: TccDescriptor[] = (pvsResponse && pvsResponse.res) ? pvsResponse.res : [];
			// 	tccs[theoryNames[i]] = {
			// 		theoryName: theoryName,
			// 		fileName: fname,
			// 		theorems: null//typecheckerResponse.res[theoryName].theorems
			// 	};
			// }
			const response: ContextDescriptor = {
				contextFolder: contextFolder,
				theories: []//tccs
			};
			return response;
		}
		return null;
	}
	/**
	 * Internal function, creates a cache to speed up the creation of the prelude descriptor
	 */
	protected async cachePreludeDescriptor (): Promise<ContextDescriptor> {
		// cache prelude libraries
		const preludeCache: string = path.join(this.pvsPath, "prelude.cache.json");
		const cache: string = await fsUtils.readFile(preludeCache);
		if (!cache) {
			// const theories: ContextDescriptor = await utils.listTheories(contextFolder, this.connection);
			const libPath: string = path.join(this.pvsPath, "lib");
			const cdesc: ContextDescriptor = await utils.getContextDescriptor(libPath, this.connection, true);
			await fsUtils.writeFile(preludeCache, JSON.stringify(cdesc, null, " "));
			return cdesc;
		}
		try {
			return JSON.parse(cache);
		} catch (jsonError) {
			console.error("[pvs-language-server] Error: unable to parse prelude cache");
			return null;
		}
	}

	/**
	 * Internal function, restarts pvs-server
	 * FIXME: create separate functions for starting pvs-server and pvs-proxy
	 * @param desc 
	 */
	protected async startPvsServerRequest (desc: { pvsPath: string, contextFolder?: string }): Promise<boolean> {
		if (desc) {
			this.pvsPath = desc.pvsPath || this.pvsPath;
			if (this.pvsPath) {
				console.log(`[pvs-language-server] Rebooting pvs (boot script in folder ${this.pvsPath})`);
				const contextFolder: string = desc.contextFolder || this.lastParsedContext || this.pvsPath;
				if (this.pvsProxy) {
					await this.pvsProxy.restartPvsServer({ pvsPath: this.pvsPath });
				} else {
					this.pvsProxy = new PvsProxy(this.pvsPath, { connection: this.connection });
					this.createServiceProviders();
					const success: boolean = await this.pvsProxy.activate({ debugMode: false });
					if (!success) {
						console.error("[pvs-language-server] Error: failed to activate pvs-proxy");
						this.connection.sendRequest(serverEvent.pvsNotPresent);
						return false;
					}
				}
				// send information to the client, to populate theory explorer on the front-end
				this.getContextDescriptor({ contextFolder }).then((cdesc: ContextDescriptor) => {
					this.connection.sendRequest(serverEvent.contextUpdate, cdesc);
				});
				// activate cli gateway
				await this.cliGateway.activate();
				// send version info to the front-end
				this.pvsProxy.getPvsVersionInfo().then((desc: { "pvs-version": string, "lisp-version": string }) => {
					if (desc) {
						const majorReleaseNumber: number = parseInt(desc["pvs-version"]);
						if (majorReleaseNumber >= 7) {
							this.connection.sendRequest(serverEvent.pvsServerReady, desc);
							this.connection.sendRequest(serverEvent.pvsVersionInfo, desc);
							// parse context folder after a timeout and send diagnostics to the client
							// disabling parseContext for now as the parser still lacks performance
							// setTimeout(async () => {
							// 	const diags: ContextDiagnostics = await this.parseContext({ contextFolder });
							// 	this.sendDiagnostics(diags, contextFolder, "Parse");	
							// }, 4000);
						} else {
							console.error(`[pvs-language-server] Error: incompatible pvs version ${desc["pvs-version"]}`);
							this.connection.sendRequest(serverEvent.pvsIncorrectVersion, `Incorrect PVS version ${desc["pvs-version"]} (vscode-pvs requires pvs ver >= 7)`);
						}
					} else {
						const msg: string = `PVS 7.x not found at ${this.pvsPath}`;
						console.error(msg);
						this.connection.sendRequest(serverEvent.pvsIncorrectVersion, msg);
					}
				});
				return true;
			} else {
				console.error("[pvs-language-server] Error: failed to identify pvs path");
				this.connection.sendRequest(serverEvent.pvsNotPresent);
				return false;
			}
		}
		return false;
	}

	/**
	 * Internal function, used to setup LSP event listeners
	 */
	protected setupConnectionManager () {
		this.connection.onInitialize((params: InitializeParams) => {
			// console.log(`--------- Client capabilities ---------\n`, params.capabilities);
			const capabilities = params.capabilities;
			this.clientCapabilities = {
				hasConfigurationCapability: capabilities.workspace && !!capabilities.workspace.configuration,
				hasWorkspaceFolderCapability: capabilities.workspace && !!capabilities.workspace.workspaceFolders,
				hasDiagnosticRelatedInformationCapability: capabilities.textDocument && capabilities.textDocument.publishDiagnostics && capabilities.textDocument.publishDiagnostics.relatedInformation
			}
			return {
				capabilities: {
					textDocumentSync: TextDocumentSyncKind.Full, //Incremental,//this.documents.syncKind,
					// The completion provider returns a list of completion items to the editor.
					completionProvider: {
						resolveProvider: true, // code completion
						triggerCharacters: [ '`', '#', `\\`, ' ' ] //  ` # space are for records; \ is for math symbols 
					},
					// Hover provider
					hoverProvider: true,
					// CodeLens provider returns commands that can be shown inline along with the specification, e.g,. to animate an expression or prove a theorem
					codeLensProvider: {
						resolveProvider: true
					},
					// definition provider
					definitionProvider: true//,
					// executeCommandProvider: {
					// 	commands: SERVER_COMMANDS // what is this for??
					// },
					// ,
					// renameProvider: true,
					// ,
					// documentOnTypeFormattingProvider: {
					// 	firstTriggerCharacter: "}",
					// 	moreTriggerCharacter: [";", ","]
					// }
				}
			};
		});
		// this.connection.onDidChangeConfiguration(async change => {
		// 	const pvsPath: string = await this.getPvsPath();
		// 	if (pvsPath !== this.pvsPath) {
		// 		this.info(`Restarting PVS (pvs path changed to ${pvsPath})`);
		// 		this.pvsPath = pvsPath;
		// 		await this.startPvs();
		// 	}
		// });
		this.connection.onInitialized(async () => {
			// register handlers
			// if (this.clientCapabilities.hasConfigurationCapability) {
			// 	this.connection.client.register(DidChangeConfigurationNotification.type, undefined);
			// }
			// if (this.clientCapabilities.hasWorkspaceFolderCapability) {
			// 	this.connection.workspace.onDidChangeWorkspaceFolders((evt: WorkspaceFoldersChangeEvent) => {
			// 		// this.connection.console.info('Workspace folder change event received.');
			// 	});
			// }

			this.connection.onRequest(serverCommand.startPvsLanguageServer, async (request: { pvsPath: string, contextFolder?: string }) => {
				// this should be called just once at the beginning
				this.startPvsServerRequest(request); // async call
			});
			this.connection.onRequest(serverCommand.rebootPvsServer, async () => {
				this.pvsProxy.killAndRestartPvsServer(); // async call
			});
			this.connection.onRequest(serverCommand.parseFile, async (request: string | { fileName: string, fileExtension: string, contextFolder: string }) => {
				this.parseFileRequest(request); // async call
			});
			this.connection.onRequest(serverCommand.typecheckFile, async (request: string | { fileName: string, fileExtension: string, contextFolder: string }) => {
				this.typecheckFileRequest(request); // async call
			});
			this.connection.onRequest(serverCommand.showTccs, async (args: { fileName: string, fileExtension: string, contextFolder: string }) => {
				this.showTccsRequest(args); // async call
			});
			this.connection.onRequest(serverCommand.listContext, async (request: string | { contextFolder: string }) => {
				this.listContextFilesRequest(request); // async call
			});
			this.connection.onRequest(serverCommand.proveFormula, async (args: { fileName: string, fileExtension: string, theoryName: string, formulaName: string, contextFolder: string }) => {
				this.proveFormulaRequest(args); // async call
			});
			// this.connection.onRequest(serverCommand.pvsioEvaluator, async (args: { fileName: string, fileExtension: string, theoryName: string, formulaName: string, contextFolder: string }) => {
			// 	this.pvsioEvaluatorRequest(args); // async call
			// });
			this.connection.onRequest(serverCommand.proofScript, async (args: { fileName: string, fileExtension: string, theoryName: string, formulaName: string, contextFolder: string }) => {
				this.proofScriptRequest(args); // async call
			});
			this.connection.onRequest(serverCommand.proofCommand, async (args: { fileName: string, fileExtension: string, theoryName: string, formulaName: string, contextFolder: string, cmd: string }) => {
				this.proofCommandRequest(args); // async call
			});
			this.connection.onRequest(serverCommand.parseContext, async (request: string | { contextFolder: string }) => {
				this.parseContextRequest(request); // async call
			});

			this.connection.onRequest(serverCommand.listDownloadableVersions, async () => {
				const versions: PvsDownloadDescriptor[] = await PvsPackageManager.listDownloadableVersions();
				this.connection.sendRequest(serverEvent.listDownloadableVersionsResponse, { response: versions });
			});
			this.connection.onRequest(serverCommand.downloadPvs, async (desc: PvsDownloadDescriptor) => {
				const fname: string = await PvsPackageManager.downloadPvsExecutable(desc);
				this.connection.sendRequest(serverEvent.downloadPvsResponse, { response: fname });
			});
			this.connection.onRequest(serverCommand.downloadLicensePage, async () => {
				const lpage: string = await PvsPackageManager.downloadPvsLicensePage();
				this.connection.sendRequest(serverEvent.downloadLicensePageResponse, { response: lpage });
			});


			// pvs.prove-tccs handler
			this.connection.onRequest('pvs.prove-tccs', async (request: string | { fileName: string, fileExtension: string, contextFolder: string }) => {
				if (request) {
					const desc: {
						fileName: string, 
						fileExtension: string, 
						contextFolder: string 
					} = (typeof request === "string") ? fsUtils.fname2desc(request) : request;
					const response: PvsResponse = await this.proveFile(desc); // FIXME: this is not the right function, as this will try to prove theorems & tccs 
					this.connection.sendRequest("pvs.response.prove-tccs", response);
					// send diagnostics
					const diags: ContextDiagnostics = {};
					const fname: string = fsUtils.desc2fname(desc);
					diags[fname] = response;
					this.sendDiagnostics(diags, desc.contextFolder, "Typecheck error");
				} else {
					console.error("[pvs-language-server] Warning: pvs.typecheck-prove invoked with null request");
				}
			});

			// pvs.get-context-descriptor handler
			// this.connection.onRequest("pvs.get-context-descriptor", async (request: string | { contextFolder: string } | undefined) => {
			// 	if (request === undefined) {
			// 		const res: PvsResponse = await this.pvsProxy.currentContext();
			// 		request = res["response"];
			// 	}
			// 	if (request) {
			// 		const desc: { contextFolder: string } = (typeof request === "string") ? { contextFolder: request } : request;
			// 		// async send, info necessary to populate theory explorer on the front-end
			// 		const cdesc: ContextDescriptor = await this.getContextDescriptor(desc);
			// 		this.connection.sendRequest("pvs.response.get-context-descriptor", cdesc);
			// 	} else {
			// 		console.error("[pvs-language-server] Warning: pvs.get-context-descriptor invoked with null request");
			// 	}
			// });

			// this.connection.onRequest('pvs.prove-tccs', async (desc: { fileName: string, fileExtension: string, contextFolder: string }) => {
			// 	if (desc) {
			// 		// this.info(`Discharging proof obligations for ${fsUtils.getFilename(fileName)}`);
			// 		// execute tcp
			// 		// TODO: run tcp in parallel -- to do this, create n processes, each proving one tcc (tcp can only perform sequential execution)
			// 		// await this.pvsTypeChecker.typecheckProve(fileName);
			// 		// gather updated information about tccs
			// 		const txt: string = await this.readFile(fname);
			// 		if (txt) {
			// 			const theoryNames: string[] = listTheoryNames(txt);
			// 			const contextFolder: string = fsUtils.getContextFolder(fname);
			// 			// TODO: move this function to pvsProxy
			// 			const response: ContextDescriptor = await this.serialTCP(fname, theoryNames, contextFolder);
			// 			// const response: TccList = await this.parallelTCP(fileName, theoryNames);
			// 			this.connection.sendRequest("server.response.typecheck-prove-and-show-tccs", response);
			// 		}
			// 	} else {
			// 		console.error("[pvs-language-server] Warning: pvs.prove-tccs invoked with null descriptor");
			// 	}
			// });

			// this.connection.onRequest("pvs.list-declarations", async (desc: PvsListDeclarationsRequest) => {
			// 	if (this.pvsServer) {
			// 		const ans: PvsListDeclarationsResponseType = await this.serverProxy.listDeclarations(desc);
			// 		if (ans && ans.res) {
			// 			this.connection.sendRequest("server.response.list-declarations", ans.res);
			// 		}
			// 	}
			// });
			// this.connection.onRequest("pvs.typecheck-all-and-show-tccs", (desc: { contextFolder: string }) => {
			// 	if (desc && desc.contextFolder && this.pvsProxy) {
			// 		// this.parallelTypeCheckAllAndShowTccs();
			// 		this.serialTypeCheckAllAndShowTccs(desc.contextFolder);
			// 	}
			// });

			// this.connection.onRequest('pvs.show-proof', async (desc: { fileName: string, fileExtension: string, theoryName: string, formulaName: string, contextFolder: string, line: number }) => {
			// 	if (desc) {
			// 		// await this.pvsProxy.typecheckFile(desc);
			// 		const response: PvsResponse = await this.pvsProxy.showProof(desc);
			// 		this.connection.sendRequest("pvs.response.show-proof", { response, args: desc });
			// 	} else {
			// 		console.error('[pvs-language-server] Warning: received pvs.show-proof request with null descriptor');
			// 	}
			// });
			// this.connection.onRequest('pvs.step-tcc', async (desc: ProofDescriptor) => {
			// 	if (desc) {
			// 		if (DEBUG_MODE) {
			// 			console.log(`[DEBUG] pvs.step-tcc ${desc}`)
			// 		}
			// 		// const proc: PvsProcess = this.pvsServer; //this.pvsTypeChecker;
			// 		// if (proc) {
			// 			// this.info("Initialising step-proof for tcc...");
			// 			await this.pvsProxy.typecheckFile(desc);
			// 			await this.pvsProxy.showTccs(desc, desc.theoryName);
			// 			const response: PvsResponseType = await this.pvsProxy.stepProof(desc);
			// 			if (response) {
			// 				if (response.res) {
			// 					this.connection.sendRequest("server.response.step-tcc", response.res);
			// 				} else if (response.error) {
			// 					const msg: string = JSON.stringify(response).replace("\n", "").replace("\"","");
			// 					this.connection.sendNotification('server.status.error', `Error while executing step-tcc: ${msg}`);
			// 				} else {
			// 					// formula does not have a proof
			// 					this.connection.sendNotification('server.status.error', response.raw);
			// 				}
			// 			}
			// 			// this.pvsReady();
			// 			// feed symbols to the parser
			// 			// this.pvsServer.typecheckFile(desc);
			// 		// }
			// 	} else {
			// 		this.connection.sendNotification('server.status.error', `Malformed pvs.step-proof request received by the server: ${JSON.stringify(desc)}`);
			// 	}
			// });
		});


		//-------------------------------
		//    LSP event handlers
		//-------------------------------

		this.connection.onCompletion(async (tpp: TextDocumentPositionParams): Promise<CompletionItem[]> => {
			if (this.pvsProxy && this.completionProvider) {
				// const isEnabled = await this.connection.workspace.getConfiguration("pvs").settings.completionProvider;
				const uri: string = tpp.textDocument.uri;
				if (uri.endsWith(".pvs") && this.completionProvider) {
					const txt: string = await this.readFile(uri);
					const completionItems: CompletionItem[] = await this.completionProvider.provideCompletionItems({ txt, uri }, tpp.position);
					return completionItems;
				}
			}
			return null;
		});
		this.connection.onCompletionResolve(async (item: CompletionItem): Promise<CompletionItem> => {
			// example item:
			//{ label: 'COROLLARY',
			// 	insertTextFormat: 1,
			// 	insertText: 'COROLLARY',
			// 	kind: 14,
			// 	commitCharacters: [ '\n' ] }
			// console.log(item);
			return item;
		});
		this.connection.onHover(async (tpp: TextDocumentPositionParams): Promise<Hover> => {
			if (this.pvsProxy && this.hoverProvider) {
				// const isEnabled = await this.connection.workspace.getConfiguration("pvs").settings.hoverProvider;
				const uri: string = tpp.textDocument.uri;
				if (fsUtils.isPvsFile(uri) && this.hoverProvider) {
					// const document: TextDocument = this.documents.get(tpp.textDocument.uri);
					const txt: string = await this.readFile(uri);
					const position: Position = tpp.position;
					let hover: Hover = await this.hoverProvider.provideHover({ txt, uri, position });
					return hover;
				}
			}
			return null;
		});
		this.connection.onCodeLens(async (tpp: CodeLensParams): Promise<CodeLens[]> => {
			if (this.codeLensProvider) {
				// const isEnabled = await this.connection.workspace.getConfiguration("pvs").settings.codelensProvider;
				const uri: string = tpp.textDocument.uri;
				if (fsUtils.isPvsFile(uri) && this.codeLensProvider) {
					// const doc: TextDocument = this.documents.get(tpp.textDocument.uri);
					const txt: string = await this.readFile(uri);
					let codelens: CodeLens[] = await this.codeLensProvider.provideCodeLens({ txt, uri });
					return codelens;
				}
			}
			return null;
		});
		this.connection.onCodeLensResolve(async (codeLens: CodeLens): Promise<CodeLens> => {
			if (this.codeLensProvider) {
				return this.codeLensProvider.resolveCodeLens(codeLens);
			}
			return null;
		});
		// this provider enables peek definition in the editor
		this.connection.onDefinition(async (tpp: TextDocumentPositionParams): Promise<Definition> => {
			if (this.pvsProxy && this.definitionProvider) {
				const uri: string = tpp.textDocument.uri;
				if (fsUtils.isPvsFile(uri) && this.definitionProvider) {
					const txt: string = await this.readFile(uri);
					if (txt) {
						const position: Position = tpp.position;
						const info: { symbolName: string, definitions: PvsDefinition[] } = await this.definitionProvider.provideDefinition({ txt, uri, position });
						if (info) {
							const pvsDefinitions: PvsDefinition[] = info.definitions;
							if (pvsDefinitions) {
								const ans: Location[] = [];
								for (const i in pvsDefinitions) {
									const def: PvsDefinition = pvsDefinitions[i];
									const uri: string = def.symbolDeclarationFile;
									const range: Range = {
										start: {
											line: def.symbolDeclarationRange.start.line - 1,
											character: def.symbolDeclarationRange.start.character
										},
										end: {
											line: def.symbolDeclarationRange.end.line - 1,
											character: def.symbolDeclarationRange.end.character
										}
									}
									const location: Location = {
										uri: "file://" + uri,
										range: range
									}
									ans.push(location);
								}
								return ans.reverse();
							}
						}
					}
				}
			}
			return null;
		});
	}
}

// instantiate the language server
new PvsLanguageServer();
