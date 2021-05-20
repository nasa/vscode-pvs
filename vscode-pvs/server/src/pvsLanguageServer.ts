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
	Location, TextDocumentChangeEvent, TextDocumentSyncKind, RenameParams, WorkspaceEdit
} from 'vscode-languageserver';
import { 
	PvsDefinition, FileList, TheoryDescriptor, PvsContextDescriptor, serverEvent, serverRequest,
	PvsDownloadDescriptor, PvsFileDescriptor, PvsVersionDescriptor, ProofDescriptor, ServerMode,
	PvsFormula, ProofEditCommand, ProofExecCommand, PvsFile, ContextFolder, PvsTheory,
	PvsProofCommand, FormulaDescriptor, FileDescriptor, PvsioEvaluatorCommand, EvalExpressionRequest, 
	SearchRequest, SearchResponse, SearchResult, FindSymbolDeclarationRequest, FindSymbolDeclarationResponse, 
	ProveFormulaResponse, ProveFormulaRequest, EvaluatorCommandResponse, SequentDescriptor, 
	DownloadWithProgressRequest, DownloadWithProgressResponse, InstallWithProgressRequest, InstallWithProgressResponse, RebootPvsServerRequest, NASALibDownloader, NASALibDownloaderRequest, NASALibDownloaderResponse, ListVersionsWithProgressRequest, ListVersionsWithProgressResponse
} from './common/serverInterface'
import { PvsCompletionProvider } from './providers/pvsCompletionProvider';
import { PvsDefinitionProvider } from './providers/pvsDefinitionProvider';
import { PvsHoverProvider } from './providers/pvsHoverProvider';
import { PvsCodeLensProvider } from './providers/pvsCodeLensProvider';
import { PvsLinter } from './providers/pvsLinter';
// import { PvsCliGateway } from './pvsCliGateway';
import { getErrorRange } from './common/languageUtils';
import * as utils from './common/languageUtils';
import * as fsUtils from './common/fsUtils';
import * as path from 'path';
import { PvsProxy } from './pvsProxy';
import { 
	PvsResponse, PvsError, ImportingDecl, TypedDecl, 
	FormulaDecl, PvsResult
} from './common/pvs-gui';
import { PvsPackageManager } from './providers/pvsPackageManager';
import { PvsIoProxy } from './pvsioProxy';
import { PvsErrorManager } from './pvsErrorManager';
import { ProcessCode } from './pvsProcess';
import { PvsProofExplorer } from './providers/pvsProofExplorer';
import { PvsRenameProvider } from './providers/pvsRenameProvider';
import { PvsSearchEngine } from './providers/pvsSearchEngine';
import { getOs } from './common/fsUtils';

export declare interface PvsTheoryDescriptor {
	id?: string;
	/**
	 * an array of declarations in the given theory
	 */
	decls?: (ImportingDecl | TypedDecl | FormulaDecl)[];
}

export declare interface ContextDiagnostics {
	[fileName: string]: { pvsResponse: PvsResponse, isTypecheckError: boolean }
};
  
  

// Example server settings, this is not used at the moment
interface Settings {
	maxNumberOfProblems: number;
}

export class PvsLanguageServer {
	protected MAX_PARALLEL_PROCESSES: number = 1; // pvs 7.1 currently does not support parallel processes
	readonly MIN_PVS_VERSION: number = 7.1;

	protected diags: ContextDiagnostics = {}; 

	// pvs path, context folder, server path
	protected pvsPath: string;
	protected pvsLibraryPath: string;
	protected pvsVersionDescriptor: PvsVersionDescriptor;

	// timers
	protected timers: { [ key:string ]: NodeJS.Timer } = {};
	// proxy servers
	protected pvsProxy: PvsProxy;
	protected pvsioProxy: PvsIoProxy; // this is necessary for the moment because pvs-server does not have APIs for pvsio
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
	protected renameProvider: PvsRenameProvider;
	protected linter: PvsLinter;
	protected proofExplorer: PvsProofExplorer;
	protected pvsSearchEngine: PvsSearchEngine;

	// cliGateway: PvsCliGateway;
	pvsErrorManager: PvsErrorManager;

	/**
	 * Data structures used for performance improvements
	 */
	protected lastParsedContext: string = ""; // this is used to avoid re-parsing a context

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

		try {
			// Create a connection channel to allow clients to connect.
			// The connection uses Node's IPC as a transport. Includes all proposed LSP features.
			this.connection = createConnection(ProposedFeatures.all);
			// Create connection manager
			this.setupConnectionManager();
			// Create a simple text document manager. The text document manager supports full document sync only
			this.setupDocumentsManager(this.connection);
			// Listen on the connection
			this.connection?.listen();
		} catch (connectionError) {
			console.warn(`[pvs-server] Warning: Unable to create LSP connection with client front-end`);
		} finally {
			// Create error manager
			this.pvsErrorManager = new PvsErrorManager(this.connection);
		}
	}

	getPvsProxy (): PvsProxy {
		return this.pvsProxy;
	}
	getPvsPath (): string {
		return this.pvsPath ? `${this.pvsPath}` : "";
	}
	getNasalibPath (): string {
		return this.pvsPath ? path.join(this.pvsPath, "nasalib") : "";
	}
	
	//-- utility functions for notifiying the client
	protected notifyStartExecution (desc: { msg: string }): void {
		this.connection?.sendNotification("server.status.progress", desc);
	}
	protected notifyEndExecution (desc?: { msg?: string }): void {
		this.connection?.sendNotification("server.status.info", desc);
	}
	protected notifyStartImportantTask (desc: { id: string, msg: string }): void {
		this.connection?.sendNotification(`server.status.start-important-task`, desc);
	}
	protected notifyProgressImportantTask (desc: { id: string, msg: string, increment: number }): void {
		this.connection?.sendNotification(`server.status.progress-important-task-${desc.id}`, desc);
	}
	protected notifyEndImportantTask (desc: { id: string, msg?: string }): void {
		this.connection?.sendNotification(`server.status.end-important-task-${desc.id}`, desc);
	}
	notifyEndImportantTaskWithErrors (desc: { id: string, msg: string }) {
        this.connection?.sendNotification(`server.status.end-important-task-${desc.id}-with-errors`, desc);
	}
	protected notifyMessage (desc: { msg: string }): void {
		this.connection?.sendNotification("server.status.warning", desc);
	}

	/**
	 * Internal function, checks if the context folder has changed
	 * @param contextFolder 
	 */
	protected isSameWorkspace (contextFolder: string): boolean {
		if (this.lastParsedContext) {
			return contextFolder === this.lastParsedContext
				|| contextFolder === path.join(this.lastParsedContext, "pvsbin");
		}
		return false;
	}


	//--------------------------------------------------------------------
	//                                APIs
	//--------------------------------------------------------------------
	getConnection (): Connection {
		return this.connection;
	}

	/**
	 * Prove formula
	 * @param args Handler arguments: filename, file extension, context folder, theory name, formula name
	 */
	async startProof (args: PvsFormula): Promise<PvsResponse | null> {
		return await this.proveFormula(args);
	}
	async proveFormula (formula: PvsFormula): Promise<PvsResponse | null> {
		if (formula && formula.fileName && formula.formulaName && formula.fileExtension && formula.contextFolder && formula.theoryName) {
			const mode: string = await this.getMode();
			if (mode !== "lisp") {
				await this.quitProof();
			}
			try {
				formula = fsUtils.decodeURIComponents(formula);
				const useLispInterface: boolean = true;//!!(this.connection && await this.connection?.workspace.getConfiguration("pvs.xperimental.developer.lispInterface"));
				const response: PvsResponse = await this.pvsProxy?.proveFormula(formula, { useLispInterface });
				this.connection?.sendNotification(serverEvent.profilerData, `(prove-formula "${path.join(formula.contextFolder, formula.fileName + ".pvs" + "#" + formula.theoryName)}")`);
				return response;
			} catch (ex) {
				console.error('[pvs-language-server.proveFormula] Error: pvsProxy has thrown an exception', ex);
				return null;
			}
		}
		return null;
	}
	async getMode (): Promise<ServerMode | null> {
		return await this.pvsProxy?.getMode();
	}
	async getImportChainTheoremsRequest (theory: PvsTheory): Promise<void> {
		if (this.pvsProxy) {
			const res: PvsResponse = await this.typecheckFile(theory);
			if (res && !res.error) {
				const theorems: PvsFormula[] = await this.pvsProxy?.getTheorems(theory, { includeImportChain: true });
				this.connection?.sendRequest(serverEvent.getImportChainTheoremsResponse, { theorems });
			}
			else {
				this.connection?.sendRequest(serverEvent.getImportChainTheoremsResponse, null);
			}
		}
	}
	async getTheoremsRequest (theory: PvsTheory): Promise<void> {
		if (this.pvsProxy) {
			const res: PvsResponse = await this.typecheckFile(theory);
			if (res && !res.error) {
				const theorems: PvsFormula[] = await this.pvsProxy?.getTheorems(theory);
				this.connection?.sendRequest(serverEvent.getTheoremsResponse, { theorems });
			} else {
				if (res && res.error && res.error.data) {
					const fname: string = (res.error.data.file_name) ? res.error.data.file_name : fsUtils.desc2fname(theory);
					const msg: string = `Typecheck errors in ${fname}${res.error.data.error_string ? ":" + res.error.data.error_string : ""}`;
					this.connection?.sendRequest(serverEvent.getTheoremsResponse, { error: msg });
				}
			}
		}
	}
	async getTccsRequest (theory: PvsTheory): Promise<void> {
		if (this.pvsProxy) {
			const res: PvsResponse = await this.typecheckFile(theory);
			if (res && !res.error) {
				const theorems: PvsFormula[] = await this.pvsProxy?.getTheorems(theory, { tccsOnly: true });
				this.connection?.sendRequest(serverEvent.getTccsResponse, { theorems });
			} else {
				if (res && res.error && res.error.data) {
					const fname: string = (res.error.data.file_name) ? res.error.data.file_name : fsUtils.desc2fname(theory);
					const msg: string = `Typecheck errors in ${fname}${res.error.data.error_string ? ":" + res.error.data.error_string : ""}`;
					this.connection?.sendRequest(serverEvent.getTccsResponse, { error: msg });
				}
			}
		}
	}
	/**
	 * Handler for prove formula requests
	 */
	async proveFormulaRequest (desc: ProveFormulaRequest, opt?: { 
		autorun?: boolean, 
		newProof?: boolean, 
		useJprf?: boolean, 
		skipSave?: boolean,
		externalServer?: boolean
	}): Promise<void> {
		opt = opt || {};
		if (desc && desc.formulaName && desc.theoryName && desc.fileName && desc.contextFolder) {
			desc = fsUtils.decodeURIComponents(desc);
			const mode: string = await this.getMode();
			if (mode !== "lisp") {
				// save then quit current proof
				if (this.proofExplorer?.proofIsDirty() && !opt.autorun && !opt.skipSave) {
					// ask if the proof needs to be saved
					await this.proofExplorer?.querySaveProof(desc);
				}
				await this.quitProof();
			}

			// make sure file exists
			const fname: string = fsUtils.desc2fname(desc);
			if (desc.fileExtension === ".pvs" && desc.origin !== "prooflite" && !fsUtils.fileExists(fname)) {
				this.notifyMessage({ msg: `Warning: file ${fname} does not exist. (source: Prover)` });
				return;
			}
			
			// make sure the file typechecks correctly before starting a proof attempt
			// send feedback to the front-end
			const taskId: string = `typecheck-${desc.formulaName}`;
			if (!opt.autorun) {
				this.proofExplorer?.resetFlags();
				this.notifyStartImportantTask({ id: taskId, msg: `Starting prover session for formula '${desc.formulaName}'` });
			}

			// make sure pvs files are typechecked before starting a proof attempt
			if (desc.fileExtension) {
				const response: PvsResponse = await this.typecheckFile({
					contextFolder: desc.contextFolder,
					fileName: desc.fileName,
					fileExtension: ".pvs" // this allows to check the pvs file for .tccs
				}, opt);
				if (response && response.error) {
					const fname: string = (response.error.data.file_name) ? response.error.data.file_name : fsUtils.desc2fname(desc);
					this.diags[fname] = {
						pvsResponse: response,
						isTypecheckError: true
					};
					this.sendDiagnostics("Typecheck");
					if (this.pvsErrorManager) {
						this.pvsErrorManager?.handleTypecheckError({ request: desc, response: <PvsError> response, taskId });
						this.pvsErrorManager?.handleProveFormulaError({ request: desc, response: <PvsError> response, taskId });
					}
					return;
				}
			}
			// load proof -- this needs to be done before starting the prover session
			await this.proofExplorer?.loadProofRequest(desc, opt);

			const response: PvsResponse = await this.proveFormula(desc);
			if (response?.result) {
				const channelID: string = utils.desc2id(desc);
				// the initial response should include only one sequent descriptor
				const result: SequentDescriptor[] = response.result;
				if (result && result.length) {
					// notify the client that the server is in prover mode
					this.notifyServerMode("in-checker");

					// publish the sequent on the cligateway, so it is visible in the prover terminal
					// this.cliGateway?.publish({ type: "pvs.event.proof-state", channelID, data: result[0] });
					// this.cliGateway?.publish({ type: "pvs.event.prover-ready", channelID });
					// this.cliGateway?.publish({ type: "gateway.publish.math-objects", channelID, data: this.pvsProxy?.listMathObjects() });

					// load initial sequent in proof explorer
					this.proofExplorer?.loadInitialSequent(result[0]);

					// start proof in proof explorer
					this.proofExplorer?.startProof({ autorun: !!opt.autorun });

					if (result.length > 1) {
						for (let i = 1; i < response.result.length; i++) {
							const proofState: SequentDescriptor = response.result[i]; // process proof commands
							await this.proofExplorer?.onStepExecutedNew({ proofState, lastSequent: i === response.result.length - 1 }, { feedbackToTerminal: true });
						}
					}

					if (!opt.autorun) { // TODO: always send notifications to the client, and let the client decide whether they should be displayed
						this.notifyEndImportantTask({ id: taskId });
						const ans: ProveFormulaResponse = { res: result[0], req: desc, mathObjects: this.pvsProxy?.listMathObjects() };
						this.connection?.sendRequest(serverEvent.proveFormulaResponse, ans);
					} else {
						const msg: string = opt.useJprf ? `Re-running J-PRF proof for ${desc.formulaName}` : `Re-running proof for ${desc.formulaName}`
						this.connection?.sendNotification("pvs.progress-info", msg);
					}
				}
			} else {
				// there was an error
				const channelID: string = utils.desc2id(desc);
				if (this.pvsErrorManager) {
					this.pvsErrorManager?.handleProveFormulaError({ request: desc, response: <PvsError> response, taskId, autorun: opt.autorun });
				} else {
					console.error(response);
				}
				// this.cliGateway?.publish({ type: "pvs.event.quit", channelID });
			}
		} else {
			if (this.pvsErrorManager) {
				this.pvsErrorManager?.handleProveFormulaError({ request: desc, response: null, taskId: null, autorun: opt.autorun });
			} else {
				console.error(`[pvs-language-server] Warning: Unable to prove formula`, desc);
			}
		}
	}
	async evalExpressionRequest (req: EvalExpressionRequest): Promise<void> {
		req = fsUtils.decodeURIComponents(req);
		const response: PvsResponse = await this.pvsioProxy?.evalExpression(req);
		this.connection?.sendRequest(serverEvent.evalExpressionResponse, { request: req, response });
	}
	/**
	 * Evaluator command handler
	 */
	async pvsioEvaluatorCommandRequest (req: PvsioEvaluatorCommand): Promise<void> {
		req = fsUtils.decodeURIComponents(req);
		const channelID: string = utils.desc2id(req);
		const response: PvsResponse = await this.pvsioProxy?.evalCommand(req, {
			cb: (res: string | "bye!", state: string) => {
				const result: PvsResult = {
					jsonrpc: "2.0",
					id: channelID,
					result: res
				};
				// this.cliGateway?.publish({ type: "pvs.event.evaluator-state", channelID, data: result });
				const data: EvaluatorCommandResponse = {
					req,
					res,
					state
				};
				this.connection.sendRequest(serverEvent.evaluatorCommandResponse, data);
			}
		});
		if (response && response.error) {
			this.pvsErrorManager?.handleEvaluationError({ request: req, response: <PvsError> response });
		}
	}
	async quitEvaluatorRequest (theory: PvsTheory): Promise<void> {
		await this.pvsioProxy?.quitEvaluator(theory);
	}
	async startEvaluatorRequest (theory: PvsTheory): Promise<void> {
		theory.fileExtension = ".pvs";
		const mode: string = await this.getMode();
		if (mode !== "lisp") {
			return;
		}
		theory = fsUtils.decodeURIComponents(theory);
		// send feedback to the front-end
		const taskId: string = `pvsio-${theory.fileName}@${theory.theoryName}`;
		const channelID: string = utils.desc2id(theory);
		this.notifyStartImportantTask({ id: taskId, msg: `Loading files necessary to evaluate theory ${theory.theoryName}` });
		// make sure the theory typechecks before starting the evaluator
		const response: PvsResponse = await this.typecheckFile(theory);
		if (response && response.result) {
			// start pvsio evaluator
			let pvsioResponse: PvsResponse = await this.pvsioProxy?.startEvaluator(theory, {
				pvsLibraryPath: this.pvsLibraryPath
			});
			// replace standard banner
			// const banner: string = utils.colorText(utils.pvsioBanner, utils.vscodeColor.green);// + "\n\n" + utils.pvsioPrompt;
			// this.cliGateway?.publish({ type: "pvs.event.evaluator-ready", channelID, banner });
			this.connection?.sendRequest(serverEvent.startEvaluatorResponse, { response: pvsioResponse, args: theory });
			this.notifyEndImportantTask({ id: taskId, msg: "PVSio evaluator session ready!" });
			this.notifyServerMode("pvsio");
		} else {
			this.pvsErrorManager?.handleEvaluationError({ request: theory, response: <PvsError> response, taskId });
			// this.cliGateway?.publish({ type: "pvs.event.quit", channelID });
		}
	}

	/**
	 * Sends to the client the prooflite script associated with the formula indicated in the request
	 * @param formula 
	 */
	async showProofLiteRequest (formula: PvsFormula): Promise<void> {
		if (formula && this.connection) {
			formula = fsUtils.decodeURIComponents(formula);
			const pdesc: ProofDescriptor = await this.openProof(formula);

			const fname: string = await this.pvsProxy?.saveProoflite({ 
				fileName: formula.fileName,
				fileExtension: ".prl",
				contextFolder: formula.contextFolder,
				theoryName: formula.theoryName,
				formulaName: formula.formulaName,
				proofDescriptor: pdesc
			});

			// const header: string = utils.makeProofliteHeader(formula.formulaName, formula.theoryName, pdesc.info.status);
			// const proof: string[] = utils.proofDescriptor2ProofLite(pdesc);
			// const proofScript: string = (proof && proof.length) ? header + proof.join("\n") : header;
			// send response to the client
			const proofFile: FileDescriptor = fsUtils.fname2desc(fname)
			this.connection?.sendRequest(serverEvent.showProofLiteResponse, { response: { proofFile }, args: formula });
		}
	}

	/**
	 * Internal function, loads the proof script for the formula indicated in the request
	 * @param formula 
	 */
	protected async openProof (formula: PvsFormula): Promise<ProofDescriptor> {
		if (this.pvsProxy) {
			return await this.pvsProxy?.openProofFile({
				fileName: formula.fileName,
				fileExtension: ".prf",
				contextFolder: formula.contextFolder
			}, formula);
		}
		// else
		console.error(`[pvs-language-server] Error: Could not load proof script (pvs-proxy is null)`);
		return null;
	}

	/**
	 * Typecheck file
	 * @param args Handler arguments: filename, file extension, context folder
	 */
	async typecheckFile (args: PvsFile, opt?: { externalServer?: boolean }): Promise<PvsResponse | null> {
		opt = opt || {};
		if (args && args.fileName && args.fileExtension && args.contextFolder) {
			try {
				args = fsUtils.decodeURIComponents(args);
				const response: PvsResponse = await this.pvsProxy?.typecheckFile(args, opt);
				// send diagnostics
				if (response) {
					if (response.result) {
						const fname: string = fsUtils.desc2fname(args);
						this.diags[fname] = {
							pvsResponse: response,
							isTypecheckError: true
						};
						this.sendDiagnostics("Typecheck");
					} else {
						if (response.error?.data) {
							const fname: string = (response.error.data.file_name) ? response.error.data.file_name : fsUtils.desc2fname(args);
							this.diags[fname] = {
								pvsResponse: response,
								isTypecheckError: true
							};
							this.sendDiagnostics("Typecheck");
						}
					}
				}
				return response;
			} catch (ex) {
				console.error('[pvs-language-server.typecheckFile] Error: pvsProxy has thrown an exception', ex);
				return null;
			}
		}
		return null;
	}
	async typecheckFileRequest (request: PvsFile): Promise<void> {
		const mode: string = await this.getMode();
		if (mode !== "lisp") {
			return;
		}
		if (request) {
			request = fsUtils.decodeURIComponents(request);
			if (request) {
				request.fileExtension = ".pvs"; // only .pvs files should be typechecked
				const fname: string = fsUtils.desc2fname(request);
				// make sure file exists
				if (!fsUtils.fileExists(fname)) {
					this.notifyMessage({ msg: `Warning: file ${fname} does not exist. (source: Typechecker)` });
					return;
				}
				const taskId: string = `typecheck-${fname}`;
				// send feedback to the front-end
				this.notifyStartImportantTask({ id: taskId, msg: `Typechecking ${fname}` });
				// parse workspace first, so the front-end is updated with statistics
				// await this.parseWorkspaceRequest(request); // this could be done in parallel with typechecking -- pvs-server is not able for now tho.
				// proceed with typechecking
				const response: PvsResponse = await this.typecheckFile(request);
				this.connection?.sendRequest(serverEvent.typecheckFileResponse, { response, args: request });
				// // send diagnostics
				if (response) {
					if (response.result) {
						// this.diags[fname] = {
						// 	pvsResponse: response,
						// 	isTypecheckError: true
						// };
						// this.sendDiagnostics("Typecheck");
						this.notifyEndImportantTask({ id: taskId, msg: `${request.fileName}${request.fileExtension} typechecks successfully!` });
					} else {
						this.pvsErrorManager?.handleTypecheckError({ response: <PvsError> response, taskId, request });
						// send diagnostics
						// if (response.error.data) {
						// 	const fname: string = (response.error.data.file_name) ? response.error.data.file_name : fsUtils.desc2fname(request);
						// 	const msg: string = response.error.data.error_string || "";
						// 	this.diags[fname] = {
						// 		pvsResponse: response,
						// 		isTypecheckError: true
						// 	};
						// 	this.sendDiagnostics("Typecheck");
						// }
					}
				}
			} else {
				console.error("[pvs-language-server] Warning: pvs.typecheck-file is unable to identify filename for ", request);
			}
		} else {
			console.error("[pvs-language-server] Warning: pvs.typecheck-file invoked with null request");
		}
	}
	async generateTheorySummaryRequest (request: PvsTheory, opt?: { showSummaryRequest?: boolean }): Promise<void> {
		opt = opt || {};
		if (request) {
			const fileContent: string = request.fileContent || "";
			const contextFolder: string = request.contextFolder;
			const fileName: string = request.fileName;
			const fileExtension: string = ".summary";

			// remove old file
			await fsUtils.deleteFile(fsUtils.desc2fname({
				contextFolder,
				fileName,
				fileExtension
			}));
			// save new content
			await fsUtils.saveSummary(fsUtils.desc2fname({
				contextFolder,
				fileName,
				fileExtension
			}), request.theoryName, fileContent);

			if (opt.showSummaryRequest) {
				this.connection?.sendRequest(serverEvent.showTheorySummaryResponse, {
					response: {
						contextFolder,
						fileName,
						fileExtension,
						fileContent
					}, 
					args: request 
				});
			}
		}
	}
	async generateWorkspaceSummaryRequest (req: FileDescriptor, opt?: { showSummaryRequest?: boolean }): Promise<void> {
		opt = opt || {};
		if (req) {
			const fileContent: string = req.fileContent || "";
			const contextFolder: string = req.contextFolder;
			const fileName: string = req.fileName;
			const fileExtension: string = ".workspace.summary";

			// save content
			await fsUtils.writeFile(fsUtils.desc2fname({
				contextFolder,
				fileName,
				fileExtension
			}), fileContent);

			if (opt.showSummaryRequest) {
				const res: FileDescriptor = {
					contextFolder,
					fileName,
					fileExtension,
					fileContent
				};
				this.connection?.sendNotification(serverRequest.showWorkspaceSummary, {
					res, 
					req
				});
			}
		}
	}
	async generateTccsRequest (request: { fileName: string, fileExtension: string, contextFolder: string }, opt?: { quiet?: boolean, showTccsRequest?: boolean }): Promise<void> {
		request = fsUtils.decodeURIComponents(request);
		if (request && this.pvsProxy) {
			opt = opt || {};
			const desc: {
				fileName: string, 
				fileExtension: string, 
				contextFolder: string 
			} = (typeof request === "string") ? fsUtils.fname2desc(request) : request;
			if (desc) {
				const fname: string = fsUtils.desc2fname({
					contextFolder: desc.contextFolder,
					fileName: desc.fileName,
					fileExtension: ".pvs"
				});
				const shortName: string = `${desc.fileName}.pvs`;
				const taskId: string = `generate-tcc-for-${fname}`;
				if (!opt.quiet) {
					this.notifyStartImportantTask({ id: taskId, msg: `Generating typecheck conditions for ${shortName}`});
				}
				// parse files first, so front-end is updated with stats
				// await this.parseWorkspaceRequest(request); // this could be done in parallel with typechecking, pvs-server is not able to do this tho.
				// then generate tccs
				const response: PvsContextDescriptor = await this.pvsProxy?.generateTccs(desc);
				this.connection?.sendRequest((opt.showTccsRequest) ? serverEvent.showTccsResponse : serverEvent.generateTccsResponse, { response, args: request });

				let nTccs: number = 0;
				let nProved: number = 0;
				if (response && response.fileDescriptors && response.fileDescriptors[fname] && response.fileDescriptors[fname].theories) {
					const theories: TheoryDescriptor[] = response.fileDescriptors[fname].theories;
					for (let i = 0; i < theories.length; i++) {
						if (theories[i].theorems && theories[i].theorems.length) {
							// count only tccs with id --- the others are subsumed
							nTccs += theories[i].theorems.filter((elem: FormulaDescriptor) => {
								return elem.formulaName;
							}).length;
							for (let j = 0; j < theories[i].theorems.length; j++) {
								if (utils.isProved(theories[i].theorems[j].status)) {
									nProved++;
								}
							}
						}
					}
				}
				if (!opt.quiet) {
					const msg: string = `${nTccs} tccs generated for ${shortName} (${nProved} proved, ${nTccs - nProved} to be proved)`;
					this.notifyEndImportantTask({ id: taskId, msg });
				}
			} else {
				console.error("[pvs-language-server] Warning: pvs.generate-tccs is unable to identify filename for ", request);
			}
		} else {
			console.error("[pvs-language-server] Warning: pvs.generate-tccs invoked with null request");
		}
	}
	/**
	 * Parse file
	 * @param args Handler arguments: filename, file extension, context folder
	 */
	async parseFile (args: PvsFile): Promise<PvsResponse> {
		args = fsUtils.decodeURIComponents(args);
		if (args && args.fileName && args.fileExtension && args.contextFolder) {			
			const enableEParser: boolean = !!(this.connection && await this.connection?.workspace.getConfiguration("pvs.xtras.enableAntlrParser"));
			try {
				return await this.pvsProxy?.parseFile(args, { enableEParser });
			} catch (ex) {
				console.error('[pvs-language-server.parseFile] Error: pvsProxy has thrown an exception', ex);
				return null;
			}
		}
		return null;
	}
	async parseFileRequest (request: PvsFile, opt?: { withFeedback?: boolean }): Promise<void> {
		const mode: string = await this.getMode();
		if (mode !== "lisp") {
			return;
		}
		request = fsUtils.decodeURIComponents(request);
		if (request) {
			opt = opt || {};
			if (request) {
				if (fsUtils.isPvsFile(request) && !fsUtils.isSummaryFile(request) && !fsUtils.isProofliteFile(request) && !this.isPreludeFile(request)) {
					if (request.contextFolder === path.join(this.lastParsedContext, "pvsbin")) {
						// nothing to do
						return;
					}
					// only .pvs files should be parsed
					request.fileExtension = ".pvs";
					// send information to the client, to populate theory explorer on the front-end
					this.lastParsedContext = request.contextFolder;
					const cdesc: PvsContextDescriptor = await this.getContextDescriptor({ contextFolder: request.contextFolder });
					this.connection?.sendRequest(serverEvent.contextUpdate, cdesc);
					
					// we should only parse .pvs files
					const fname: string = fsUtils.desc2fname(request);
					// make sure file exists
					if (!fsUtils.fileExists(fname)) {
						this.notifyMessage({ msg: `Warning: file ${fname} does not exist. (source: Parser)` });
						return;
					}
					const taskId: string = `parse-${fname}`;
					// send feedback to the front-end
					if (opt.withFeedback) {
						this.notifyStartImportantTask({ id: taskId, msg: `Parsing file ${fname}` });
					}

					// send workspace stats
					const contextFiles: FileList = await fsUtils.listPvsFiles(request.contextFolder);
					if (contextFiles && contextFiles.fileNames) {
						const nfiles: number = contextFiles.fileNames.length;
						this.connection?.sendRequest(serverEvent.workspaceStats, { contextFolder: request.contextFolder, files: nfiles });
					}
					
					// parse file, as requested
					const response: PvsResponse = await this.parseFile(request);
					let source: string = "Parse";
					if (response) {
						// send parser response
						this.connection?.sendRequest(serverEvent.parseFileResponse, response);
						// collect diagnostics
						if (this.diags[fname] && this.diags[fname].isTypecheckError) {
							// keep typecheck diags
							source = "Typecheck";
						} else {
							this.diags[fname] = {
								pvsResponse: response,
								isTypecheckError: false
							};
						}
						// send feedback to the front-end
						if (opt.withFeedback) {
							if (response.error) {
								this.notifyEndImportantTaskWithErrors({ id: taskId, msg: `${source} errors in ${request.fileName}${request.fileExtension}` });
							} else {
								this.notifyEndImportantTask({ id: taskId, msg: `${request.fileName}${request.fileExtension} parsed successfully!` });
							}
						}
					} 
					// send diagnostics
					this.sendDiagnostics(source);
				}
			} else {
				console.error("[pvs-language-server] Warning: pvs.parse-file is unable to identify filename for ", request);
			}
		} else {
			console.error("[pvs-language-server] Warning: pvs.parse-file invoked with null request");
		}
	}
	protected async workspaceActionRequest (
		action: "parse-workspace" | "typecheck-workspace", 
		request: { contextFolder: string }, 
		opt?: { 
			withFeedback?: boolean, 
			suppressFinalMessage?: boolean, 
			suppressDialogCreation?: boolean,
			keepDialogOpen?: boolean,
			msg?: string,
			generateTCCs?: boolean,
			actionId?: string }
	): Promise<boolean> {
		opt = opt || {};
		return new Promise (async (resolve, reject) => {
			request = fsUtils.decodeURIComponents(request);
			// reset diagnostics for the workspace if this is a typecheck-workspace request
			if (action === "typecheck-workspace") {
				this.diags = {};
			}
			if (request) {
				const actionId: string = opt.actionId || action;
				const contextFolder: string = (typeof request === "string") ? request : request.contextFolder;
				let success: boolean = true;

				// send feedback to the front-end
				const taskId: string = `${actionId}-${contextFolder}`;
				const workspaceName: string = fsUtils.getContextFolderName(contextFolder);

				// constants useful for feedback
				const actionFriendlyName: string = action === "parse-workspace" ? "Parse" : "Typecheck";
				const actionFriendlyNamePast: string = action === "parse-workspace" ? "Parsed" : "Typechecked";
				const actionFriendlyNameContinuous: string = action === "parse-workspace" ? "Parsing" : "Typechecking";

				if (opt.withFeedback) {
					const msg: string = opt.msg	|| `${actionFriendlyNameContinuous} workspace ${workspaceName}`;
					if (!opt.suppressDialogCreation) {
						// this means that another task has already created the dialog
						this.notifyStartImportantTask({ id: taskId, msg });
					} else {
						this.notifyProgressImportantTask ({ id: taskId, msg, increment: -1 }); 
					}
				}

				if (contextFolder) {
					// send information to the client, to populate theory explorer on the front-end
					if (!this.isSameWorkspace(contextFolder)) {
						this.lastParsedContext = contextFolder;
					}
					const cdesc: PvsContextDescriptor = await this.getContextDescriptor({ contextFolder });
					this.connection?.sendRequest(serverEvent.contextUpdate, cdesc);

					// parse/typecheck files, as requested
					const contextFiles: FileList = await fsUtils.listPvsFiles(contextFolder);
					if (contextFiles && contextFiles.fileNames && contextFiles.fileNames.length) {
						const nfiles: number = contextFiles.fileNames.length;
						let completed: number = 0;

						const msg: string = opt.msg	|| `${actionFriendlyNameContinuous} workspace ${workspaceName} (0/${nfiles} files processed)`;
						this.notifyProgressImportantTask ({ id: taskId, msg, increment: -1 }); 
						console.log(msg);

						this.connection?.sendRequest(serverEvent.workspaceStats, { contextFolder, files: nfiles });
						// run the promises & cap the concurrent function execution
						let next_file_index: number = this.MAX_PARALLEL_PROCESSES;
						let completed_tasks: number = 0;
						let errors: string[] = [];
						
						const actionFunction: "parseFile" | "typecheckFile" = (action === "parse-workspace") ? "parseFile" : "typecheckFile";
						// worker
						const workspaceActionAux = async (
							actionName: "parseFile" | "typecheckFile", 
							desc: PvsFile
						) => {
							const response: PvsResponse = await this[actionName](desc);
							completed_tasks++;

							// TODO: investigate why pvs is using two types of error messages to communicate errors
							if (response && response.error) {
								if (response.error.message) {
									errors.push(`${actionFriendlyName} error in file ${desc.fileName}${desc.fileExtension}: ${response.error.message}`);
								} else if (response.error.data && response.error.data.error_string) {
									let error_msg: string = `${actionFriendlyName} error in file ${desc.fileName}${desc.fileExtension}`;
									if (response.error.data.place && response.error.data.place.length > 1) { error_msg += ` (Ln ${response.error.data.place[0]}, Col ${response.error.data.place[1]})`; }
									error_msg += ": " + response.error.data.error_string;
									errors.push(error_msg);
								}
							}
							
							// update feedback every time a file has been processed
							if (response) {
								if (opt.generateTCCs) {
									await this.pvsProxy.generateTccs(desc)
								}
								completed++;
								if (opt.withFeedback) {
									const msg: string = opt.msg	|| `${actionFriendlyNameContinuous} workspace ${workspaceName} (${completed}/${nfiles} files processed)`;
									console.log(msg);
									if (opt.keepDialogOpen) {
										// this is a pre-task, just show spinning bar
										this.notifyProgressImportantTask ({ id: taskId, msg, increment: -1 }); 
									} else {
										this.notifyProgressImportantTask ({ id: taskId, msg, increment: 1 / nfiles * 100 }); 
									}
								}
								this.connection?.sendRequest(serverEvent.workspaceStats, {
									contextFolder: response.contextFolder,
									fileName: response.fileName,
									fileExtension: response.fileExtension,
									files: nfiles, 
									"math-objects": response["math-objects"]
								});
							}

							const fname: string = fsUtils.desc2fname(desc);

							// send diagnostics TODO: cleanup pvs response for error.data, sometimes is an object others is an array of objects, and file_name is sometimes not included
							const diag_fname: string = (response && response.error && response.error.data && response.error.data.file_name) ? response.error.data.file_name : fname;
							let source: string = actionFriendlyName;
							if (action === "typecheck-workspace") {
								this.diags[diag_fname] = {
									pvsResponse: response,
									isTypecheckError: true
								};
							} else if (action === "parse-workspace") {
								if (this.diags[diag_fname] && this.diags[fname].isTypecheckError) {
									// keep typecheck diags
									source = "Typecheck";
								} else {
									this.diags[diag_fname] = {
										pvsResponse: response,
										isTypecheckError: false
									};	
								}
							}
							this.sendDiagnostics(source);

							// check if there are more files that need to be parsed
							if (completed_tasks >= contextFiles.fileNames.length) {
								if (opt.withFeedback) {
									if (errors.length) {
										success = false;
										let msg: string = (opt.suppressFinalMessage) ? ""
											: (opt.msg) ? opt.msg
												: (errors.length === 1) ? errors[0] 
													: `Workspace ${workspaceName} contains ${actionFriendlyName.toLocaleLowerCase()} errors`;
										this.notifyEndImportantTaskWithErrors({ id: taskId, msg });
									} else {
										if (!opt.keepDialogOpen) {
											const msg: string = (opt.suppressFinalMessage) ? ""
												: opt.msg || `Workspace ${workspaceName} ${actionFriendlyNamePast.toLocaleLowerCase()} successfully (${completed} files processed)`;
											this.notifyEndImportantTask({ id: taskId, msg });
										}
									}
								}
								resolve(success);
							} else {
								if (next_file_index < contextFiles.fileNames.length) {
									const fname: string = path.join(contextFolder, contextFiles.fileNames[next_file_index++]);
									const fileName: string = fsUtils.getFileName(fname);
									const fileExtension: string = fsUtils.getFileExtension(fname);
									// send feedback
									// this.notifyProgressImportantTask (`Parsing workspace ${contextFolder} (${fileName}${fileExtension})`);
									// start new task
									await workspaceActionAux(actionName, { fileName, fileExtension, contextFolder });
								}
							}
						}
						if (contextFiles.fileNames.length) {
							for (let i = 0; i < this.MAX_PARALLEL_PROCESSES && i < contextFiles.fileNames.length; i++) {
								const fname: string = path.join(contextFolder, contextFiles.fileNames[i]);
								const fileName: string = fsUtils.getFileName(fname);
								const fileExtension: string = fsUtils.getFileExtension(fname);
								workspaceActionAux(actionFunction, { fileName, fileExtension, contextFolder });
							}
						} else {
							resolve(success);
						}
					} else {
						console.log(`[pvs-language-server] Workspace ${workspaceName} does not contain pvs files`);
						if (!opt.keepDialogOpen) {
							const msg: string = (opt.suppressFinalMessage) ? ""
								: (opt.msg) ? opt.msg : `Workspace ${workspaceName} ${actionFriendlyNamePast.toLocaleLowerCase()} (0 files processed)`;
							this.notifyEndImportantTask({ id: taskId, msg });
						}
						resolve(success);
					}
				} else {
					console.warn(`[pvs-language-server] Warning: workspace name is null`);
					resolve(false);
				}
			} else {
				console.error(`[pvs-language-server] Warning: pvs.${action} invoked with null request`);
				resolve(false);
			}
		});
	}
	async parseWorkspaceRequest (request: ContextFolder, opt?: {
		withFeedback?: boolean, 
		suppressFinalMessage?: boolean,
		keepDialogOpen?: boolean,
		actionId?: string,
		msg?: string 
	}): Promise<void> {
		request = fsUtils.decodeURIComponents(request);
		await this.workspaceActionRequest("parse-workspace", request, opt);
	}
	async typecheckWorkspaceRequest (request: ContextFolder, opt?: { 
		withFeedback?: boolean,
		suppressDialogCreation?: boolean,
		generateTCCs?: boolean
	}): Promise<void> {
		request = fsUtils.decodeURIComponents(request);
		const success: boolean = await this.workspaceActionRequest("typecheck-workspace", request, opt);
		this.connection.sendRequest(serverEvent.typecheckWorkspaceResponse, { args: request, success });
	}
	/**
	 * Returns the list of pvs files in a given context folder
	 */
	async listContextFiles (args: ContextFolder): Promise<FileList> {
		args = fsUtils.decodeURIComponents(args);
		if (args && args.contextFolder) {
			return await fsUtils.listPvsFiles(args.contextFolder);
		}
		return null;
	}
	async listContextFilesRequest (request: string | { contextFolder: string }): Promise<void> {
		request = fsUtils.decodeURIComponents(request);
		const args = (typeof request === "string") ? { contextFolder: request } : request;
		if (args && args.contextFolder) {
			const response: FileList = await this.listContextFiles(args);
			this.connection?.sendRequest(serverEvent.listContextResponse, response);
		}
	}
	/**
	 * Returns the proof status for all theorems in a given theory
	 */
	async statusProofTheory (args: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string }): Promise<PvsResponse> {
		args = fsUtils.decodeURIComponents(args);
		return await this.pvsProxy?.statusProofTheory(args);
	}
	/**
	 * Returns the proof status for all theorems in a given file
	 */
	async statusProof (args: { fileName: string, fileExtension: string, contextFolder: string }): Promise<{ [ fname: string ]: PvsResponse }> {
		args = fsUtils.decodeURIComponents(args);
		let res: { [ fname: string ]: PvsResponse } = {};
		// fetch theory names
		const fname: string = fsUtils.desc2fname(args);
		const theories: TheoryDescriptor[] = await fsUtils.listTheoriesInFile(fname);
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
	async getPreludeDescriptor (): Promise<PvsContextDescriptor> {
		return await this.cachePreludeDescriptor();
	}
	/**
	 * Returns a descriptor with information on all theories in a given context folder
	 * @param contextFolder Context folder
	 */
	async getContextDescriptor (args: { contextFolder: string }): Promise<PvsContextDescriptor> {
		if (args && args.contextFolder) {
			args = fsUtils.decodeURIComponents(args);
			if (this.pvsProxy) {
				if (this.pvsProxy?.isProtectedFolder(args.contextFolder)) {
					return await this.getPreludeDescriptor();
				} // else
				const contextFolder: string = (args.contextFolder.endsWith("pvsbin") || args.contextFolder.endsWith("pvsbin/")) ? path.join(args.contextFolder, "..") : args.contextFolder;
				return await fsUtils.getContextDescriptor(contextFolder, { listTheorems: true, includeTccs: true });
			} else {
				console.error('[pvs-language-server.listTheories] Error: pvs proxy is null');
			}
		} else {
			console.warn('[pvs-language-server.listTheories] Warning: getContextDescriptor invoked with null descriptor');
		}
		return null;
	}

	/**
	 * Returns a descriptor with information on all theories in a given file
	 * @param contextFolder Context folder
	 * @param fileName Name of the file (without extension)
	 * @param fileExtension File extension
	 */
	async getFileDescriptor (args: { contextFolder: string, fileName: string, fileExtension: string }, opt?: { listTheorems?: boolean, includeTccs?: boolean }): Promise<PvsFileDescriptor> {
		if (args) {
			args = fsUtils.decodeURIComponents(args);
			opt = opt || {};
			if (this.pvsProxy) {
				if (this.pvsProxy?.isProtectedFolder(args.contextFolder)) {
					const cdesc: PvsContextDescriptor = await this.getPreludeDescriptor();
					if (cdesc) {
						return cdesc[fsUtils.desc2fname(args)];
					} else {
						console.error('[pvs-language-server.listTheories] Error: could not read file descriptor for protected file ', args);
						return null;
					}
				} // else
				const fname: string = fsUtils.desc2fname(args);
				return await fsUtils.getFileDescriptor(fname, {
					...opt,
					prelude: this.isPreludeFile(fname)
				});
			} else {
				console.error('[pvs-language-server.listTheories] Error: pvs proxy is null');
			}
		} else {
			console.error('[pvs-language-server.listTheories] Error: getContextDescriptor invoked with null descriptor');
		}
		return null;
	}

	// DDL -- to be moved to a new tool vscode-ddl
	async hp2pvs (args: PvsFile): Promise<PvsResponse> {
		args = fsUtils.decodeURIComponents(args);
		if (args && args.fileName && args.fileExtension && args.contextFolder) {
			try {
				return await this.pvsProxy?.hp2pvs(args);
			} catch (ex) {
				console.error('[pvs-language-server.generatePvsFile] Error: pvsProxy has thrown an exception', ex);
				return null;
			}
		}
		return null;
	}
	async prettyPrintDdlRequest (args: { fileName: string, fileExtension: string, contextFolder: string, expr: string }): Promise<void> {
		args = fsUtils.decodeURIComponents(args);
		if (args && args.expr) {
			const res: string = await this.pvsProxy?.prettyPrintDdl(args);
		}
	}
	async hp2pvsRequest (request: string | PvsFile): Promise<void> {
		request = fsUtils.decodeURIComponents(request);
		if (request) {
			const desc: {
				fileName: string, 
				fileExtension: string, 
				contextFolder: string 
			} = (typeof request === "string") ? fsUtils.fname2desc(request) : request;
			if (desc) {
				const taskId: string = `typecheck-${desc.fileName}${desc.fileExtension}`;
				this.notifyStartImportantTask({ id: taskId, msg: `Translating hybrid program ${desc.fileName}${desc.fileExtension} to PVS` });

				const response: PvsResponse = await this.hp2pvs(desc);
				const fname: string = fsUtils.desc2fname(desc);
				if (response) {
					// send parser response
					this.connection?.sendRequest(serverEvent.parseFileResponse, response);
					// collect diagnostics
					this.diags[fname] = {
						pvsResponse: response,
						isTypecheckError: false
					};
				} 
				// else {
				// 	// clear diagnostics, as the parse error may have gone and we don't know because pvs-server failed to execute parseFile
				// 	this.diags[fname] = null;
				// }
				// send diagnostics
				this.sendDiagnostics("Parse");
				if (response && response.error) {
					this.pvsErrorManager?.notifyEndImportantTaskWithErrors({ id: taskId, msg: `Error: ${desc.fileName}.pvs could not be generated -- please check pvs-server output to view errors.` });
				} else {
					this.notifyEndImportantTask({ id: taskId, msg: `PVS file ${desc.fileName}.pvs generated successfully!` });
				}
			} else {
				console.error("[pvs-language-server] Warning: pvs.parse-file is unable to identify filename for ", request);
			}
		} else {
			console.error("[pvs-language-server] Warning: pvs.parse-file invoked with null request");
		}
	}


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
			// if (close && close.document && close.document.uri) {
			// 	const uri: string = close.document.uri;
			// 	// remove file descriptor
			// 	this.settings.documents.delete(close.document.uri);
			// 	// update diagnostics, if this is a pvs file
			// 	if (fsUtils.isPvsFile(uri)) {
			// 		const fname: string = uri.replace("file://", "");
			// 		const contextFolder: string = fsUtils.getContextFolder(fname);
			// 		this.diags[fname] = null;
			// 		this.sendDiagnostics(this.diags, contextFolder, "** deleted file **");
			// 	}
			// }
		});

		// -- This event fires when the text document is first opened in the editor or when then content of the opened document has changed.
		// this.documents.onDidChangeContent(change => {
		// 	console.log(change);
		// });

		// onDidOpen fires when a document is opened in the editor
		// this.documents.onDidOpen(async (open: TextDocumentChangeEvent) => {
		// });

		// onDidSave fires when a document is saved on the editor
		this.documents.onDidSave(async (save: TextDocumentChangeEvent) => {
			if (save && save.document && save.document.uri && fsUtils.isPvsFile(save.document.uri)) {
				// parse file
				const contextFolder: string = fsUtils.getContextFolder(save.document.uri);
				const fileName: string = fsUtils.getFileName(save.document.uri);
				const fileExtension: string = fsUtils.getFileExtension(save.document.uri);
				// reset diagnostics for this file
				const fname: string = fsUtils.desc2fname({ contextFolder, fileName, fileExtension });
				delete this.diags[fname];
				// trigger parsing if this is a .pvs file
				await this.parseFileRequest({ fileName, fileExtension, contextFolder }); // this call will automatically send new diags to the client
			}
		});

		// Listen to document events triggered by the editor
		this.documents.listen(connection);
	}
	/**
	 * Internal function, used by restartPvs
	 */
	protected createServiceProviders(): void {
		// Create service providers
		this.definitionProvider = new PvsDefinitionProvider(this.pvsProxy, this.documents);
		this.completionProvider = new PvsCompletionProvider(this.definitionProvider);
		this.codeLensProvider = new PvsCodeLensProvider();
		this.renameProvider = new PvsRenameProvider(this.connection);
		this.hoverProvider = new PvsHoverProvider(this.definitionProvider);
		this.linter = new PvsLinter();
		// this.cliGateway = new PvsCliGateway(this);
		this.proofExplorer = new PvsProofExplorer(this.connection, this.pvsProxy, this);
		this.pvsSearchEngine = new PvsSearchEngine(this.connection, this);
	}

	getProofExplorer (): PvsProofExplorer {
		return this.proofExplorer;
	}

	// getGatewayPort(): number {
	// 	if (this.cliGateway) {
	// 		return this.cliGateway?.getPort();
	// 	}
	// 	return 0;
	// }
	/**
	 * Internal function, sends diagnostics to the client
	 * @param data Diagnostics data
	 */
	protected async sendDiagnostics (source?: string): Promise<void> {
		source = source || "";
		const fnames: string[] = Object.keys(this.diags);
		if (fnames && fnames.length > 0) {
			for (let i = 0; i < fnames.length; i++) {
				let fname: string = fnames[i];
				const response: PvsResponse = (this.diags && this.diags[fname]) ? this.diags[fname].pvsResponse : null;
				if (response && response["error"]) {
					const info: PvsError = <PvsError> response;

					// old parser
					if (info.error && info.error.data && info.error.data.place && info.error.data.place.length >= 2) {
						const errorStart: Position = {
							line: info.error.data.place[0], 
							character: info.error.data.place[1]
						};
						const errorEnd: Position = (info.error.data.place.length > 3) ? { 
							line: info.error.data.place[2], 
							character: info.error.data.place[3]
						} : errorStart;
						const txt: string = await fsUtils.readFile(fname);
						if (txt) {
							const errorRange: Range = getErrorRange(txt, errorStart, errorEnd);
							const diag: Diagnostic = {
								severity: DiagnosticSeverity.Error,
								range: {
									start: { line: errorRange.start.line - 1, character: errorRange.start.character },
									end: { line: errorRange.end.line - 1, character: errorRange.end.character },
								},
								message: info.error.data.error_string,
								source: `\n${source} error`
							};
							this.connection?.sendDiagnostics({ uri: `file://${fname}`, diagnostics: [ diag ] });
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
						this.connection?.sendDiagnostics({ uri: `file://${fname}`, diagnostics });
					}
				} else {
					// send clean diagnostics
					this.connection?.sendDiagnostics({ uri: `file://${fname}`, diagnostics: [ ] });
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
			fileName = decodeURIComponent(fileName);
			fileName = fileName.startsWith("file://") ? fileName = fileName.replace("file://", "") : fileName;
			const doc: TextDocument = this.documents.get("file://" + fileName);
			if (doc) {
				return doc.getText();
			}
			try {
				return fsUtils.readFile(fileName);
			} catch (readError) {
				console.error(`[pvs-language-server] Warning: Error while reading file ${fileName} (${readError.message})`);
				this.connection?.sendNotification("pvs-error", `Error while reading file ${fileName} (${readError.message})`);
			}
		} else {
			console.error("[pvs-language-server] Warning: trying to read null filename");
		}
		return null;
	}

	/**
	 * Internal function, creates a cache to speed up the creation of the prelude descriptor
	 */
	protected async cachePreludeDescriptor (): Promise<PvsContextDescriptor> {
		const libPath: string = path.join(this.pvsPath, "lib");
		// cache prelude libraries
		const preludeCache: string = path.join(libPath, "prelude.cache.json");
		const cache: string = await fsUtils.readFile(preludeCache);
		if (cache) {
			try {
				const res: PvsContextDescriptor = JSON.parse(cache);
				if (res && res.fileDescriptors) {
					// make sure all file descriptors indicate theorems proved
					for (let i in res.fileDescriptors) {
						const theories: TheoryDescriptor[] = res.fileDescriptors[i].theories;
						for (let t = 0; t < theories?.length; t++) {
							const theorems: FormulaDescriptor[] = theories[t].theorems;
							for (let tt = 0; tt < theorems?.length; tt++) {
								theorems[tt].status = "proved";
							}
						}
					}
					return res;
				}
			} catch (jsonError) {
				console.warn("[pvs-language-server] Warning: unable to parse prelude cache, trying to re-create the file.");
				// return null;
			}	
		}
		// else, cache file not present or in wrong format, create it again
		const cdesc: PvsContextDescriptor = await fsUtils.getContextDescriptor(libPath, { prelude: true });
		await fsUtils.writeFile(preludeCache, JSON.stringify(cdesc, null, " "));
		return cdesc;
	}

	protected async sendWorkspaceInfo (): Promise<void> {
		const res: PvsResponse = await this.pvsProxy?.currentContext();
		if (res && res.result) {
			const contextFolder: string = res.result;
			const contextFiles: FileList = await fsUtils.listPvsFiles(contextFolder);
			const nfiles: number = contextFiles.fileNames.length;
			this.connection?.sendRequest(serverEvent.workspaceStats, { contextFolder, files: nfiles });
		}
	}

	protected async sendPvsServerReadyEvent (): Promise<boolean> {
		// load pvs version info
		const desc: PvsVersionDescriptor = await this.pvsProxy?.loadPvsVersionInfo();
		if (desc) {
			const pvsVersion: number = parseFloat(desc["pvs-version"]);
			if (pvsVersion >= this.MIN_PVS_VERSION) {
				await this.sendWorkspaceInfo();
				this.pvsVersionDescriptor = desc;
				this.connection?.sendRequest(serverEvent.pvsServerReady, desc);
				this.connection?.sendRequest(serverEvent.pvsVersionInfo, desc);
				return true;
			} else {
				console.error(`[pvs-language-server] Error: incompatible pvs version ${desc["pvs-version"]}`);
				this.connection?.sendRequest(serverEvent.pvsIncorrectVersion, `Incorrect PVS version ${desc["pvs-version"]} (vscode-pvs requires pvs ver >= ${this.MIN_PVS_VERSION})`);
			}
		} else {
			const msg: string = `PVS executable not found at ${this.pvsPath}`;
			console.error(msg);
			this.connection?.sendRequest(serverEvent.pvsNotFound, msg);
		}
		return false;
	}

	async checkDependencies (): Promise<boolean> {
		console.log(`[pvs-server] Checking dependencies...`);
		const osVersion: { version?: string, error?: string } = getOs();
		if (osVersion && (osVersion.version !== "Linux" && osVersion.version !== "MacOSX")) {
			let msg: string = `VSCode-PVS currently runs only under Linux or MacOSX.\nPlease use a virtual machine to run VSCode-PVS under ${osVersion.version}.`;
			console.error(msg);
			this.pvsErrorManager?.notifyPvsFailure({
				msg,
				error_type: "dependency",
				src: "pvs-language-server"
			});
			return false;
		}
		if (!osVersion || osVersion.error) {
			console.error(osVersion.error);
			this.pvsErrorManager?.notifyPvsFailure({
				msg: osVersion.error,
				error_type: "dependency",
				src: "pvs-language-server"
			});
			return false;
		}
		// const nodejs: { version?: string, error?: string } = await fsUtils.getNodeJsVersion();
		// if (!nodejs || nodejs.error) {
		// 	let msg: string = (nodejs && nodejs.error) ? nodejs.error : "";
		// 	msg += "\n" + "Required dependency 'node' is not installed. Please download 'node' from https://nodejs.org/";
		// 	console.error(msg);
		// 	this.pvsErrorManager?.notifyPvsFailure({
		// 		msg,
		// 		error_type: "dependency"
		// 	});
		// 	return false;
		// }
		// console.log("[pvs-server] node: " + nodejs?.version);
		return true;
	}

	async startPvsServer (desc: { pvsPath: string, pvsLibraryPath?: string, contextFolder?: string, externalServer?: boolean }, opt?: { verbose?: boolean, debugMode?: boolean }): Promise<boolean> {
		if (desc) {
			opt = opt || {};
			desc = fsUtils.decodeURIComponents(desc);
			if (desc.pvsPath !== this.pvsPath && this.pvsProxy) {
				// the server was already running, the user must have selected a different pvs path. Kill the existing server.
				await this.pvsProxy?.killPvsServer();
			}
			this.pvsPath = desc.pvsPath || this.pvsPath;
			this.pvsLibraryPath = desc.pvsLibraryPath || this.pvsLibraryPath;
			const externalServer: boolean = !!desc.externalServer;
			if (this.pvsPath) {
				console.log(`[pvs-language-server] Rebooting PVS (installation folder is ${this.pvsPath})`);
				if (this.pvsProxy) {
					await this.pvsProxy?.enableExternalServer({ enabled: externalServer });
					await this.pvsProxy?.restartPvsServer({ pvsPath: this.pvsPath, pvsLibraryPath: this.pvsLibraryPath, externalServer });
				} else {
					this.pvsProxy = new PvsProxy(this.pvsPath, { connection: this.connection, pvsLibraryPath: this.pvsLibraryPath, externalServer });
					this.pvsioProxy = new PvsIoProxy(this.pvsPath, { connection: this.connection, pvsLibraryPath: this.pvsLibraryPath });
					this.createServiceProviders();
					const success: boolean = await this.pvsProxy?.activate({
						debugMode: opt.debugMode, 
						verbose: opt.debugMode !== false,
						pvsErrorManager: this.pvsErrorManager
					});
					if (!success) {
						console.error("[pvs-language-server] Error: failed to activate pvs-proxy");
						this.connection?.sendRequest(serverEvent.pvsNotFound);
						return false;
					}
				}
				// activate cli gateway
				// await this.cliGateway?.activate();
				this.notifyServerMode("lisp");
				return true;
			} else {
				console.error("[pvs-language-server] Error: failed to identify PVS path");
				this.connection?.sendRequest(serverEvent.pvsNotFound);
			}
		}
		return false;
	}

	/**
	 * Internal function, restarts pvs-server
	 * FIXME: create separate functions for starting pvs-server and pvs-proxy
	 * @param desc 
	 */
	protected async startPvsServerRequest (desc: { pvsPath: string, pvsLibraryPath: string, contextFolder?: string, externalServer?: boolean }): Promise<boolean> {
		// make sure that all dependencies are installed; an error will be shown to the user if some dependencies are missing
		this.checkDependencies(); // async call
		// start pvs
		const success: boolean = await this.startPvsServer(desc);
		if (success) {
			// send version info to the front-end
			await this.sendPvsServerReadyEvent();
			return true;
		}
		return false;
	}

	protected async rebootPvsServer (desc?: RebootPvsServerRequest): Promise<boolean> {
		desc = desc || {};
		desc.pvsPath = desc.pvsPath || this.pvsPath;
		// make sure that all dependencies are installed; an error will be shown to the user if some dependencies are missing
		this.checkDependencies(); // async call
		// await fsUtils.cleanBin(this.lastParsedContext, { keepTccs: true, recursive: fsUtils.MAX_RECURSION }); // this will remove .pvscontext and pvsbin
		// if (desc.cleanFolder && desc.cleanFolder !== this.lastParsedContext) {
		// 	await fsUtils.cleanBin(desc.cleanFolder, { keepTccs: true, recursive: fsUtils.MAX_RECURSION }); // this will remove .pvscontext and pvsbin
		// }
		if (this.pvsProxy) {
			await this.pvsProxy.rebootPvsServer(desc);
			this.notifyServerMode("lisp");
			// send version info
			await this.sendPvsServerReadyEvent();
			return true;
		} else {
			console.error("[pvs-language-server] Error: pvs-proxy is null");
			this.connection?.sendRequest(serverEvent.pvsNotFound);
		}
		return false;
	}

	/**
	 * Quits the prover
	 * @param opt 
	 */
	async quitProof (): Promise<void> {
		if (this.pvsProxy) {
			await this.pvsProxy?.quitProof();
		}
	}

	notifyServerMode (mode: ServerMode): void {
		this.connection?.sendRequest(serverEvent.serverModeUpdateEvent, { mode });
	}

	async quitProofRequest (): Promise<void> {
		await this.quitProof();
		this.connection?.sendRequest(serverEvent.quitProofResponse);
	}

	/**
	 * Utility function, return the file name of the prelude
	 */
	getPreludeFileName (): FileDescriptor {
		return {
			contextFolder: path.join(this.pvsPath, "lib"),
			fileName: "prelude",
			fileExtension: ".pvs"
		};
	}

	/**
	 * Utility function, checks if the given file name is the prelude file
	 */
	isPreludeFile (desc: string | FileDescriptor): boolean {
		if (desc) {
			const fname: string = typeof desc === "string" ? desc : fsUtils.desc2fname(desc);
			const prelude: string = fsUtils.desc2fname(this.getPreludeFileName());
			return fname === prelude;
		}
		return false;
	}

	/**
	 * Internal function, used to setup LSP event listeners
	 */
	protected setupConnectionManager () {
		this.connection?.onInitialize((params: InitializeParams) => {
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
					definitionProvider: true,
					workspaceSymbolProvider: true,
					// documentSymbolProvider: true
					// executeCommandProvider: {
					// 	commands: SERVER_COMMANDS // what is this for??
					// },
					// ,
					renameProvider: true,
					// ,
					// documentOnTypeFormattingProvider: {
					// 	firstTriggerCharacter: "}",
					// 	moreTriggerCharacter: [";", ","]
					// }
				}
			};
		});
		// this.connection?.onDidChangeConfiguration(async change => {
		// 	const pvsPath: string = await this.getPvsPath();
		// 	if (pvsPath !== this.pvsPath) {
		// 		this.info(`Restarting PVS (pvs path changed to ${pvsPath})`);
		// 		this.pvsPath = pvsPath;
		// 		await this.startPvs();
		// 	}
		// });
		this.connection?.onInitialized(async () => {
			// register handlers
			// if (this.clientCapabilities.hasConfigurationCapability) {
			// 	this.connection?.client.register(DidChangeConfigurationNotification.type, undefined);
			// }
			// if (this.clientCapabilities.hasWorkspaceFolderCapability) {
			// 	this.connection?.workspace.onDidChangeWorkspaceFolders((evt: WorkspaceFoldersChangeEvent) => {
			// 		// this.connection?.console.info('Workspace folder change event received.');
			// 	});
			// }
			this.connection?.onRequest(serverRequest.cancelOperation, () => {
				console.log(`[pvs-server] Operation cancelled by the user`);
				this.proofExplorer?.interruptProofCommand();
				// which pvs-server command should I invoke to stop the operation??
			});
			this.connection?.onRequest(serverRequest.getContextDescriptor, (request: { contextFolder: string }) => {
				if (request && request.contextFolder) {
					// send information to the client, to populate theory explorer on the front-end
					this.getContextDescriptor(request).then((cdesc: PvsContextDescriptor) => {
						this.connection?.sendRequest(serverEvent.getContextDescriptorResponse, cdesc);
					});
				}
			});
			this.connection?.onRequest(serverRequest.getFileDescriptor, (request: { contextFolder: string, fileName: string, fileExtension: string }) => {
				if (request && request.contextFolder) {
					// send information to the client, to populate theory explorer on the front-end
					this.getFileDescriptor(request).then((fdesc: PvsFileDescriptor) => {
						this.connection?.sendRequest(serverEvent.getFileDescriptorResponse, fdesc);
					});
				}
			});
			this.connection?.onRequest(serverRequest.stopPvsServer, async () => {
				if (this.pvsProxy) {
					await this.pvsProxy?.killPvsServer();
				}
			});
			this.connection?.onRequest(serverRequest.startPvsServer, async (request: { 
				pvsPath: string, 
				pvsLibraryPath: string, 
				contextFolder?: string, 
				externalServer?: boolean
			}) => {
				// this should be called just once at the beginning
				const success: boolean = await this.startPvsServerRequest(request);
				if (success) {
					// send information to the client, to populate theory explorer on the front-end
					const contextFolder: string = request.contextFolder;
					if (contextFolder) {
						if (!this.isSameWorkspace(contextFolder)) {
							this.lastParsedContext = contextFolder;
						}
						const cdesc: PvsContextDescriptor = await this.getContextDescriptor({ contextFolder });
						this.connection?.sendRequest(serverEvent.contextUpdate, cdesc);
					}
				} else {
					console.error(`[pvs-server] Error: failed to start pvs-server`);
					this.pvsErrorManager?.handleStartPvsServerError(ProcessCode.PVSSTARTFAIL);
				}
			});
			this.connection?.onRequest(serverRequest.rebootPvsServer, async (req?: RebootPvsServerRequest) => {
				this.rebootPvsServer(req);
			});
			this.connection?.onRequest(serverRequest.clearTheories, async () => {
				await this.pvsProxy?.clearTheories();
			});
			this.connection?.onRequest(serverRequest.parseFile, async (request: { fileName: string, fileExtension: string, contextFolder: string }) => {
				this.parseFileRequest(request); // async call
			});
			this.connection?.onRequest(serverRequest.parseFileWithFeedback, async (request: { fileName: string, fileExtension: string, contextFolder: string }) => {
				this.parseFileRequest(request, { withFeedback: true }); // async call
			});
			this.connection?.onRequest(serverRequest.parseWorkspace, async (request:{ contextFolder: string }) => {
				this.parseWorkspaceRequest(request); // async call
			});
			this.connection?.onRequest(serverRequest.parseWorkspaceWithFeedback, async (request: { contextFolder: string }) => {
				this.parseWorkspaceRequest(request, { withFeedback: true }); // async call
			});
			this.connection?.onRequest(serverRequest.typecheckWorkspaceWithFeedback, async (request: { contextFolder: string }) => {
				this.typecheckWorkspaceRequest(request, { withFeedback: true, generateTCCs: true }); // async call
			});
			this.connection?.onRequest(serverRequest.typecheckWorkspace, async (request: { contextFolder: string }) => {
				this.typecheckWorkspaceRequest(request, { generateTCCs: true }); // async call
			});
			this.connection?.onRequest(serverRequest.hp2pvs, async (request: PvsFile) => {
				this.hp2pvsRequest(request); // async call
			});
			this.connection?.onRequest(serverRequest.typecheckFile, async (request: PvsFile) => {
				this.typecheckFileRequest(request); // async call
			});
			this.connection?.onRequest(serverRequest.showTccs, async (request: { fileName: string, fileExtension: string, contextFolder: string, quiet?: boolean }) => {
				this.generateTccsRequest(request, { quiet: request && request.quiet, showTccsRequest: true }); // async call
			});
			this.connection?.onRequest(serverRequest.generateTccs, async (request: { fileName: string, fileExtension: string, contextFolder: string, quiet?: boolean }) => {
				this.generateTccsRequest(request, { quiet: request && request.quiet }); // async call
			});
			this.connection?.onRequest(serverRequest.generateTheorySummary, async (request: PvsTheory) => {
				this.generateTheorySummaryRequest(request); // async call
			});
			this.connection?.onRequest(serverRequest.showTheorySummary, async (request: PvsTheory) => {
				this.generateTheorySummaryRequest(request, { showSummaryRequest: true }); // async call
			});
			this.connection?.onRequest(serverRequest.generateWorkspaceSummary, async (request: FileDescriptor) => {
				this.generateWorkspaceSummaryRequest(request); // async call
			});
			this.connection?.onRequest(serverRequest.showWorkspaceSummary, async (request: FileDescriptor) => {
				this.generateWorkspaceSummaryRequest(request, { showSummaryRequest: true }); // async call
			});
			this.connection?.onRequest(serverRequest.listContext, async (request: ContextFolder) => {
				await this.listContextFilesRequest(request);
			});
			this.connection?.onRequest(serverRequest.proveFormula, async (req: ProveFormulaRequest) => {
				await this.proveFormulaRequest(req);
			});
			this.connection?.onRequest(serverRequest.getImportChainTheorems, async (args: PvsTheory) => {
				await this.getImportChainTheoremsRequest(args); // async call
			});
			this.connection?.onRequest(serverRequest.getTheorems, async (args: PvsTheory) => {
				await this.getTheoremsRequest(args); // async call
			});
			this.connection?.onRequest(serverRequest.getTccs, async (args: PvsTheory) => {
				await this.getTccsRequest(args); // async call
			});
			this.connection?.onRequest(serverRequest.autorunFormula, async (req: PvsFormula) => {
				await this.proveFormulaRequest(req, { autorun: true }); // async call
			});
			this.connection?.onRequest(serverRequest.autorunFormulaFromJprf, async (req: PvsFormula) => {
				await this.proveFormulaRequest(req, { autorun: true, useJprf: true }); // async call
			});
			this.connection?.onRequest(serverRequest.showProofLite, async (args: PvsFormula) => {
				await this.showProofLiteRequest(args); // async call
			});
			this.connection?.onRequest(serverRequest.proofCommand, async (args: PvsProofCommand) => {
				await this.proofExplorer?.proofCommandRequest(args); // async call
			});
			this.connection?.onRequest(serverRequest.getGatewayConfig, async () => {
				// const port: number = this.getGatewayPort();
				this.connection?.sendRequest(serverEvent.getGatewayConfigResponse, { port: 0 });
			});
			this.connection?.onRequest(serverRequest.viewPreludeFile, async () => {
				this.connection?.sendRequest(serverEvent.viewPreludeFileResponse, this.getPreludeFileName());
			});
			this.connection?.onRequest(serverRequest.quitProof, async () => {
				this.quitProofRequest(); // this method will send a quitProofResponse to the client
			});

			this.connection?.onRequest(serverRequest.getNasalibDownloader, async (req: NASALibDownloaderRequest) => {
				const downloader: NASALibDownloader = await PvsPackageManager.getNasalibDownloader(req);
				const res: NASALibDownloaderResponse = { downloader };
				this.connection?.sendNotification(serverRequest.getNasalibDownloader, { req, res });
			});
			this.connection?.onRequest(serverRequest.listVersionsWithProgress, async (req: ListVersionsWithProgressRequest) => {
				const res: ListVersionsWithProgressResponse = await PvsPackageManager.listDownloadableVersionsWithProgress(this.connection, req);
				this.connection?.sendNotification(serverRequest.listVersionsWithProgress, { req, res });
			});
			this.connection?.onRequest(serverRequest.installWithProgress, async (req: InstallWithProgressRequest) => {
				const res: InstallWithProgressResponse = await PvsPackageManager.installWithProgress(this.connection, req);
				this.connection?.sendNotification(serverRequest.installWithProgress, { req, res });
			});
			this.connection?.onRequest(serverRequest.downloadWithProgress, async (req: DownloadWithProgressRequest) => {
				const res: DownloadWithProgressResponse = await PvsPackageManager.downloadWithProgress(this.connection, req);
				this.connection?.sendNotification(serverRequest.downloadWithProgress, { req, res });
			});
			// this.connection?.onRequest(serverRequest.downloadPvs, async (desc: PvsDownloadDescriptor) => {
			// 	const fname: string = await PvsPackageManager.downloadPvsExecutable(desc);
			// 	this.connection?.sendRequest(serverEvent.downloadPvsResponse, { response: fname });
			// });
			this.connection?.onRequest(serverRequest.downloadLicensePage, async () => {
				const licensePage: string = await PvsPackageManager.downloadPvsLicensePage();
				this.connection?.sendRequest(serverEvent.downloadLicensePageResponse, { response: licensePage });
			});
			this.connection?.onRequest(serverRequest.startEvaluator, async (args: PvsioEvaluatorCommand) => {
				await this.startEvaluatorRequest(args);
			});
			this.connection?.onRequest(serverRequest.quitEvaluator, async (args: PvsTheory) => {
				await this.quitEvaluatorRequest(args);
				this.connection.sendRequest(serverEvent.quitEvaluatorResponse);
			});
			this.connection?.onRequest(serverRequest.evaluatorCommand, async (args: PvsioEvaluatorCommand) => {
				await this.pvsioEvaluatorCommandRequest(args);
			});
			this.connection?.onRequest(serverRequest.evalExpression, async (args: EvalExpressionRequest) => {
				this.evalExpressionRequest(args);
			});

			// search request
			this.connection?.onRequest(serverRequest.search, async (req: SearchRequest) => {
				const ans: SearchResult[] = await this.pvsSearchEngine?.searchNasalib(req.searchString);
				const res: SearchResponse = { req, ans };
				this.connection?.sendNotification(serverEvent.searchResponse, res);
			});

			// find declaration request
			this.connection?.onRequest(serverRequest.findSymbolDeclaration, async (req: FindSymbolDeclarationRequest) => {
				const ans: PvsDefinition[] = await this.definitionProvider.findSymbolDefinitionInTheory(req?.theory, req?.symbolName);
				const res: FindSymbolDeclarationResponse = { req, ans };
				this.connection?.sendRequest(serverEvent.findSymbolDeclarationResponse, res);
			});

			// prover commands
			this.connection?.onRequest(serverRequest.proverCommand, async (desc: ProofExecCommand | ProofEditCommand) => {
				if (desc) {
					switch (desc.action) {
						case "forward": { this.proofExplorer?.forward(); break; }
						case "back": { this.proofExplorer?.back(); break; }
						case "fast-forward": { this.proofExplorer?.fastForwardToNodeX(desc); break; }
						case "rewind": { this.proofExplorer?.rewindToNodeX(desc); break; }
						case "run": { await this.proofExplorer?.run({ feedbackToTerminal: true }); break; }
						
						case "quit-proof": { await this.proofExplorer?.quitProof({ notifyClient: true }); break; }
						case "quit-proof-and-save": { await this.proofExplorer?.quitProofAndSave(); break; }
						
						case "append-node": { this.proofExplorer?.appendNodeX(desc); break; }
						case "copy-node": { this.proofExplorer?.copyNodeX(desc); break; }
						case "paste-node": { this.proofExplorer?.pasteNodeX(desc); break; }
						case "copy-tree": { this.proofExplorer?.copyTreeX(desc); break; }
						case "paste-tree": { this.proofExplorer?.pasteTreeX(desc); break; }
						case "delete-node": { this.proofExplorer?.deleteNodeX(desc); break; }
						case "append-branch": { this.proofExplorer?.appendBranchX(desc); break; }
						case "cut-node": { this.proofExplorer?.cutNodeX(desc); break; }
						case "cut-tree": { this.proofExplorer?.cutTreeX(desc); break; }
						case "slice-tree": { this.proofExplorer?.sliceTreeX(desc); break; }
						case "delete-tree": { this.proofExplorer?.deleteTreeX(desc); break; }
						case "trim-node": { this.proofExplorer?.trimNodeX(desc); break; }
						case "trim-unused": { this.proofExplorer?.removeNotVisitedX(desc); break; }
						case "rename-node": { this.proofExplorer?.renameNodeX(desc); break; }
						case "open-proof": {
							if (desc?.opt?.restartProof) {
								await this.proveFormulaRequest({
									contextFolder: desc.formula.contextFolder,
									fileName: desc.formula.fileName,
									fileExtension: desc.formula.fileExtension,
									theoryName: desc.formula.theoryName,
									formulaName: desc.formula.formulaName,
									proofFile: desc.proofFile
								}, {
									skipSave: true
								});
							} else {
								await this.proofExplorer?.openProofRequest(desc.proofFile, desc.formula);
							}
							break;
						}
						case "import-proof": { await this.proofExplorer?.importProofRequest(desc.proofFile, desc.formula); break; }
	
						case "export-proof": { await this.proofExplorer?.exportProof(desc.proofFile); break; }
						case "start-new-proof": { await this.proveFormulaRequest(desc.formula, { newProof: true }); break; }
						case "interrupt-prover": { await this.proofExplorer?.interruptProofCommand(); break; }
						case "interrupt-and-quit-prover": { await this.proofExplorer?.interruptAndQuitProof(); break; }
						//------
						default: {
							console.warn(`[pvs-server] Warning: unhandled prover command ${JSON.stringify(desc)}`);
							break;
						}
					}
				}
			});
		});


		//-------------------------------
		//    LSP event handlers
		//-------------------------------

		this.connection?.onCompletion(async (tpp: TextDocumentPositionParams): Promise<CompletionItem[]> => {
			const isEnabled: boolean = !!(await this.connection?.workspace.getConfiguration("pvs.serviceProvider.autocompletion"));
			if (this.completionProvider && isEnabled) {
				const uri: string = tpp.textDocument.uri;
				if (uri.endsWith(".pvs") && this.completionProvider) {
					const document: TextDocument = this.documents?.get(tpp?.textDocument.uri);
					const txt: string = document?.getText(); //await this.readFile(uri);
					const completionItems: CompletionItem[] = await this.completionProvider.provideCompletionItems({ txt, fname: uri }, tpp.position);
					return completionItems;
				}
			}
			return null;
		});
		this.connection?.onCompletionResolve(async (item: CompletionItem): Promise<CompletionItem> => {
			// example item:
			//{ label: 'COROLLARY',
			// 	insertTextFormat: 1,
			// 	insertText: 'COROLLARY',
			// 	kind: 14,
			// 	commitCharacters: [ '\n' ] }
			// console.log(item);
			return item;
		});
		this.connection?.onHover(async (args: TextDocumentPositionParams): Promise<Hover> => {
			const isEnabled: boolean = !!(await this.connection?.workspace.getConfiguration("pvs.serviceProvider.hover"));
			if (this.hoverProvider && isEnabled) {
				// const isEnabled = await this.connection?.workspace.getConfiguration("pvs").settings.hoverProvider;
				const uri: string = args.textDocument.uri;
				if (fsUtils.isPvsFile(uri) && this.hoverProvider) {
					const document: TextDocument = this.documents?.get(args?.textDocument.uri);
					const txt: string = document?.getText() || await this.readFile(uri);
					const hover: Hover = await this.hoverProvider.provideHover({ txt, uri, position: args.position });
					return hover;
				}
			}
			return null;
		});
		this.connection?.onCodeLens(async (args: CodeLensParams): Promise<CodeLens[]> => {
			const isEnabled: boolean = !!(await this.connection?.workspace.getConfiguration("pvs.serviceProvider.codelens"));
			if (this.codeLensProvider && isEnabled) {
				const uri: string = args.textDocument.uri;
				if (fsUtils.isPvsFile(uri) && this.codeLensProvider) {
					const document: TextDocument = this.documents?.get(args?.textDocument.uri);
					const txt: string = document?.getText() || await this.readFile(uri);
					const codelens: CodeLens[] = await this.codeLensProvider.provideCodeLens({ txt, uri });
					return codelens;
				}
			}
			return null;
		});
		this.connection?.onCodeLensResolve(async (args: CodeLens): Promise<CodeLens> => {
			const isEnabled: boolean = !!(await this.connection?.workspace.getConfiguration("pvs.serviceProvider.autocompletion"));
			if (this.codeLensProvider && isEnabled) {
				return this.codeLensProvider.resolveCodeLens(args);
			}
			return null;
		});
		// this provider enables peek definition in the editor
		this.connection?.onDefinition(async (args: TextDocumentPositionParams): Promise<Definition> => {
			const isEnabled: boolean = !!(await this.connection?.workspace.getConfiguration("pvs.serviceProvider.definitions"));
			if (this.definitionProvider && isEnabled) {
				const uri: string = args.textDocument.uri;
				if (fsUtils.isPvsFile(uri) && this.codeLensProvider) {
					const document: TextDocument = this.documents?.get(args?.textDocument.uri);
					const txt: string = document?.getText() || await this.readFile(uri);
					const def: Definition = await this.definitionProvider.provideDefinition({ uri, position: args.position, txt });
					return def;
				}
			}
			return null;
		});
		this.connection?.onRenameRequest(async (args: RenameParams): Promise<WorkspaceEdit> => {
			const isEnabled: boolean = !!(await this.connection?.workspace.getConfiguration("pvs.serviceProvider.rename"));
			if (this.renameProvider && isEnabled) {
				const uri: string = args.textDocument.uri;
				if (fsUtils.isPvsFile(uri) && this.codeLensProvider) {
					const document: TextDocument = this.documents?.get(args?.textDocument.uri);
					const txt: string = document?.getText() || await this.readFile(uri);
					const wsEdit: WorkspaceEdit = await this.renameProvider.provideRename({ txt, uri, position: args.position, newName: args.newName });
					if (wsEdit && wsEdit.changes && Object.keys(wsEdit.changes) && Object.keys(wsEdit.changes).length) {
						const cdesc: PvsContextDescriptor = await this.getContextDescriptor({ contextFolder: fsUtils.getContextFolder(uri) });
						this.connection?.sendRequest(serverEvent.contextUpdate, cdesc);	
					}
					return {};
				}
			}
			return null;
		});
	}
}

// instantiate the language server
new PvsLanguageServer();