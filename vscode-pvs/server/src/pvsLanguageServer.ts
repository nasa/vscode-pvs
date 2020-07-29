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
	PvsContextDescriptor,
	serverEvent,
	serverCommand,
	PvsDownloadDescriptor,
	PvsFileDescriptor,
	FormulaDescriptor,
	PvsVersionDescriptor,
	ProofDescriptor,
	ServerMode,
	PvsFormula,
	ProofEditCommand,
	ProofExecCommand,
	PvsFile,
	ContextFolder,
	PvsTheory,
	PvsProofCommand,
	ProofExecDidEndProof,
	ProofStatus
} from './common/serverInterface'
import { PvsCompletionProvider } from './providers/pvsCompletionProvider';
import { PvsDefinitionProvider } from './providers/pvsDefinitionProvider';
import { PvsHoverProvider } from './providers/pvsHoverProvider';
import { PvsCodeLensProvider } from './providers/pvsCodeLensProvider';
import { PvsLinter } from './providers/pvsLinter';
import { PvsCliGateway } from './pvsCliGateway';
import { getErrorRange, SequentDescriptor } from './common/languageUtils';
import * as utils from './common/languageUtils';
import * as fsUtils from './common/fsUtils';
import * as path from 'path';
import { PvsProxy } from './pvsProxy';
import { PvsResponse, PvsError, ImportingDecl, TypedDecl, FormulaDecl, ShowTCCsResult, DischargeTccsResult } from './common/pvs-gui';
import { PvsPackageManager } from './providers/pvsPackageManager';
import { PvsIoProxy } from './pvsioProxy';
import { PvsErrorManager } from './pvsErrorManager';
import { ProcessCode } from './pvsProcess';
import { PvsProofExplorer } from './providers/pvsProofExplorer';

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
	protected linter: PvsLinter;
	protected proofExplorer: PvsProofExplorer;

	cliGateway: PvsCliGateway;
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
			this.connection.listen();
			// Create error manager
			this.pvsErrorManager = new PvsErrorManager(this.connection);
		} catch (connectionError) {
			console.error(`[pvs-server] Error: unable to create LSP connection`);
		}
	}
	
	//-- utility functions
	protected notifyStartExecution (desc: { msg: string }): void {
		if (this.connection) {
			this.connection.sendNotification("server.status.progress", desc);
		}
	}
	protected notifyEndExecution (desc?: { msg?: string }): void {
		if (this.connection) {
			this.connection.sendNotification("server.important-notification", desc);
		}
	}
	protected notifyStartImportantTask (desc: { id: string, msg: string }): void {
		if (this.connection) {
			this.connection.sendNotification(`server.status.start-important-task`, desc);
		}
	}
	protected notifyProgressImportantTask (desc: { id: string, msg: string, increment: number }): void {
		if (this.connection) {
			this.connection.sendNotification(`server.status.progress-important-task-${desc.id}`, desc);
		}
	}
	protected notifyEndImportantTask (desc: { id: string, msg?: string }): void {
		if (this.connection) {
			this.connection.sendNotification(`server.status.end-important-task-${desc.id}`, desc);
		}
	}

	protected notifyMessage (desc: { msg: string }): void {
		// error shown in the status bar
		if (this.connection) {
			this.connection.sendNotification("server.important-notification", desc);
		}
	}
	//--
	protected isSameWorkspace (contextFolder: string): boolean {
		return contextFolder === this.lastParsedContext
			|| contextFolder === path.join(this.lastParsedContext, fsUtils.pvsbinFolder);
	}

	/**
	 * Internal function, checks that pvsProxy and desc are defined
	 * @param desc 
	 */
	// protected checkArgs (methodID: string, desc: { fileName?: string, fileExtension?: string, contextFolder: string, theoryName?: string, formulaName?: string, cmd?: string }): boolean {
	// 	if (desc && desc.contextFolder) {
	// 		if (this.pvsProxy) {
	// 			switch (methodID) {
	// 				case "proveFormula": {
	// 					return desc.formulaName !== null && desc.formulaName !== undefined && desc.formulaName !== "";
	// 				}
	// 			}
	// 			return true;
	// 		} else {
	// 			console.error(`[pvs-language-server.${methodID}] Error: pvs proxy is null`);
	// 		}
	// 	} else {
	// 		console.error(`[pvs-language-server.${methodID}] Error: descriptor is null or malformed`);
	// 	}
	// 	return false;
	// }


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
	async proveFormula (args: PvsFormula): Promise<PvsResponse | null> {
		if (args && args.fileName && args.formulaName && args.fileExtension && args.contextFolder && args.theoryName) {
			try {
				args = fsUtils.decodeURIComponents(args);
				const useLispInterface: boolean = true;//!!(this.connection && await this.connection.workspace.getConfiguration("pvs.xperimental.developer.lispInterface"));
				const response: PvsResponse = await this.pvsProxy.proveFormula(args, { useLispInterface });
				if (this.connection) {
					this.connection.sendNotification(serverEvent.profilerData, `(prove-formula "${path.join(args.contextFolder, args.fileName + ".pvs" + "#" + args.theoryName)}")`);
				}
				return response;
			} catch (ex) {
				console.error('[pvs-language-server.proveFormula] Error: pvsProxy has thrown an exception', ex);
				return null;
			}
		}
		return null;
	}
	async proofCommandRequest (request: PvsProofCommand): Promise<void> {
		this.proofExplorer.proofCommandRequest(request);
	};
	async getServerMode (): Promise<ServerMode> {
		const proverStatus: PvsResponse = await this.pvsProxy.proverStatus();
		const mode: ServerMode = (proverStatus && proverStatus.result !== "inactive") ?
			"in-checker" : "lisp";
		return mode;
	} 
	async proveFormulaRequest (request: PvsFormula, opt?: { autorun?: boolean }): Promise<void> {
		opt = opt || {};
		request = fsUtils.decodeURIComponents(request);
		
		if (await this.getServerMode() === "in-checker") {
			// save then quit current proof
			if (this.proofExplorer.proofIsDirty() && !opt.autorun) {
				// ask if the proof needs to be saved
				this.connection.sendRequest(serverEvent.querySaveThenProveFormula, { args: request }); // this will trigger the confirmation dialog
			}
			await this.quitProof();
		}

		// make sure file exists
		const fname: string = fsUtils.desc2fname(request);
		if (!fsUtils.fileExists(fname)) {
			this.notifyMessage({ msg: `Warning: file ${fname} does not exist.` });
			return;
		}
		
		// make sure the file typechecks correctly before starting a proof attempt
		// send feedback to the front-end
		const taskId: string = `typecheck-${request.formulaName}`;
		if (!opt.autorun) {
			this.notifyStartImportantTask({ id: taskId, msg: `Starting prover session for formula '${request.formulaName}'` });
		}

		// make sure pvs files are typechecked before starting a proof attempt
		if (request.fileExtension === ".pvs") {	
			const response: PvsResponse = await this.typecheckFile(request);
			if (response && response.error) {
				const fname: string = (response.error.data.file_name) ? response.error.data.file_name : fsUtils.desc2fname(request);
				this.diags[fname] = {
					pvsResponse: response,
					isTypecheckError: true
				};
				this.sendDiagnostics("Typecheck");
				this.pvsErrorManager.handleTypecheckError({ request, response: <PvsError> response, taskId });
				return;
			}
		}
		// load proof -- this needs to be done before starting the prover session
		await this.proofExplorer.loadProofRequest(request);
		const response: PvsResponse = await this.proveFormula(request);
		if (response) {
			const channelID: string = utils.desc2id(request);
			if (response.result) {
				// notify the client that the server is in prover mode
				this.notifyServerMode("in-checker");
				// the following commands are necessary to create the log file and start up the interactive cli session
				// const proofLogPath: string = path.join(request.contextFolder, fsUtils.pvsbinFolder);
				// const pvsLogFile: string = path.join(proofLogPath, `${channelID}${fsUtils.logFileExtension}`);
				// const pvsTmpLogFile: string = path.join(proofLogPath, `${channelID}-tmp${fsUtils.logFileExtension}`);
				// await fsUtils.createFolder(proofLogPath);
				// await fsUtils.writeFile(pvsTmpLogFile, "");

				// load proof state -- the initial response should include only one sequent
				const result: SequentDescriptor[] = response.result;
				// await fsUtils.writeFile(pvsLogFile, utils.formatSequent(result[result.length - 1]));
				this.cliGateway.publish({ type: "pvs.event.proof-state", channelID, data: result[result.length - 1] });
				this.cliGateway.publish({ type: "gateway.publish.math-objects", channelID, data: this.pvsProxy.listMathObjects() });

				// load sequent in proof explorer
				this.proofExplorer.loadSequent(result[result.length - 1]);
				// start proof in proof explorer
				this.proofExplorer.startProof({ autorun: !!opt.autorun, autorunCallback: (status: ProofStatus) => {
					this.connection.sendRequest(serverEvent.autorunFormulaResponse, status);
					this.notifyServerMode("lisp");
				}});

				if (!opt.autorun) {
					this.notifyEndImportantTask({ id: taskId });
				}
			}
		} else {
			// there was an error
			this.pvsErrorManager.handleProveFormulaError({ request, response: <PvsError> response, taskId });
		}
		// if (this.connection) {
		// 	this.connection.sendRequest(serverEvent.proveFormulaResponse, { response, args: request });
		// }
	}
	/**
	 * Discharge TCCs (prove-tccs)
	 * function disabled for now --- pvs-server tends to crash often with this command
	 * @param args Handler arguments: filename, file extension, context folder, theory name, formula name
	 */
	// async dischargeTccs (args: { fileName: string, fileExtension: string, contextFolder: string }): Promise<PvsResponse | null> {
	// 	if (this.checkArgs("prove-tccs", args)) {
	// 		try {
	// 			args = fsUtils.decodeURIComponents(args);
	// 			const tccs: FormulaDescriptor[] = await utils.listTheoremsInFile(fsUtils.desc2fname({
	// 				fileName: args.fileName,
	// 				fileExtension: ".tccs",
	// 				contextFolder: args.contextFolder
	// 			}));
	// 			if (tccs) {
	// 				for (let i = 0; i < tccs.length; i++) {
	// 					const response: PvsResponse = await this.pvsProxy.proveFormula({
	// 						contextFolder: tccs[i].contextFolder, 
	// 						fileName: tccs[i].fileName, 
	// 						fileExtension: ".pvs", 
	// 						theoryName: tccs[i].theoryName,
	// 						formulaName: tccs[i].formulaName
	// 					});
	// 				}
	// 			}
	// 			// update .tccs file
	// 			await this.showTccsRequest(args);
	// 			// return response;
	// 		} catch (ex) {
	// 			console.error('[pvs-language-server.dischargeTccs] Error: pvsProxy has thrown an exception', ex);
	// 			return null;
	// 		}
	// 	}
	// 	return null;
	// }
	// async dischargeTccsRequest (request: { fileName: string, fileExtension: string, contextFolder: string }): Promise<void> {
	// 	request = fsUtils.decodeURIComponents(request);

	// 	// typecheck first
	// 	const taskId: string = `discharge-tccs-${fsUtils.desc2fname(request)}`;
	// 	this.notifyStartImportantTask({ id: taskId, msg: `Loading theories necessary to discharge typecheck conditions in ${request.fileName}${request.fileExtension}` });
	// 	// parse workspace files while typechecking
	// 	await this.parseWorkspaceRequest(request);
	// 	const req = { fileName: request.fileName, fileExtension: ".pvs", contextFolder: request.contextFolder };
	// 	const typecheckResponse: PvsResponse = await this.typecheckFile(req);
	// 	if (typecheckResponse && !typecheckResponse.error) {
	// 		this.notifyEndImportantTask({ id: taskId });
	// 		// send feedback to the front-end
	// 		this.notifyStartImportantTask({ id: taskId, msg: `Discharging typecheck conditions in ${request.fileName}${request.fileExtension}` });
	// 		const response: PvsResponse = await this.dischargeTccs(req); // pvs-server wants the name of the .pvs file, not the .tccs file
	// 		if (response && response.result) {
	// 			const info: DischargeTccsResult = response.result;
	// 			const msg: string = info.unproved ? 
	// 				`${info.unproved} typecheck conditions could not be proved`
	// 				: `All tccs discharged successfully!`;

	// 			this.connection.sendRequest(serverEvent.dischargeTccsResponse, { response, args: request });
	// 			this.notifyEndImportantTask({ id: taskId, msg });
	// 		} else {
	// 			this.notifyEndImportantTaskWithErrors({ id: taskId, msg: `Something went wrong while discharging TCCs (pvs-server returned error or null response). Please check pvs-server output for details.` });
	// 		}
	// 	} else {
	// 		this.notifyEndImportantTaskWithErrors({ id: taskId, msg: `Some files contain typecheck errors. Please fix those errors before trying to discharge TCCs.` });
	// 	}
	// }
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
	async evaluateExpressionRequest (request: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, cmd: string }): Promise<void> {
		request = fsUtils.decodeURIComponents(request);
		const response: PvsResponse = await this.pvsioProxy.evaluateExpression(request);
		const channelID: string = utils.desc2id(request);
		this.cliGateway.publish({ type: "pvs.event.evaluator-state", channelID, data: response });
		if (response && response.error) {
			this.pvsErrorManager.handleEvaluationError({ request, response: <PvsError> response });
		}
	}
	async startEvaluatorRequest (request: { fileName: string, fileExtension: string, theoryName: string, contextFolder: string }): Promise<void> {
		request = fsUtils.decodeURIComponents(request);
		// send feedback to the front-end
		const taskId: string = `pvsio-${request.fileName}@${request.theoryName}`;
		this.notifyStartImportantTask({ id: taskId, msg: `Loading files necessary to evaluate theory ${request.theoryName}` });
		// make sure the theory typechecks before starting the evaluator
		const response: PvsResponse = await this.typecheckFile(request);
		if (response && response.result) {
			// start pvsio evaluator
			let pvsioResponse: PvsResponse = await this.pvsioProxy.startEvaluator(request);
			const channelID: string = utils.desc2id(request);
			// replace standard banner
			pvsioResponse.result = "";
			pvsioResponse.banner = utils.pvsioBanner;
			this.cliGateway.publish({ type: "pvs.event.evaluator-state", channelID, data: pvsioResponse });
			this.connection.sendRequest(serverEvent.startEvaluatorResponse, { response: pvsioResponse, args: request });
			this.notifyEndImportantTask({ id: taskId, msg: "PVSio evaluator session ready!" });
		} else {
			this.pvsErrorManager.handleEvaluationError({ request, response: <PvsError> response, taskId });
		}
	}

	// /**
	//  * Request proof script to pvs-server
	//  * @param args Handler arguments: filename, file extension, context folder, theory name, formula name
	//  */
	// async proofScript (request: { 
	// 	fileName: string, 
	// 	fileExtension: string, 
	// 	theoryName: string, 
	// 	formulaName: string, 
	// 	contextFolder: string
	// }): Promise<PvsResponse> {
	// 	console.log(`[pvs-server] Loading proof script`);
	// 	request = fsUtils.decodeURIComponents(request);
	// 	return await this.pvsProxy.proofScript(request);
	// }

	/**
	 * Sends to the client the prooflite script associated with the formula indicated in the request
	 * @param request 
	 */
	async showProofLiteRequest (request: { 
		fileName: string, 
		fileExtension: string, 
		theoryName: string, 
		formulaName: string, 
		contextFolder: string
	}): Promise<void> {
		request = fsUtils.decodeURIComponents(request);
		// first, convert proofs stored in .prf format to .jprf format
		await this.loadProof(request);
		// load prooflite script from the .jprf file
		const proofScript: string = await utils.getProofLiteScript(request);
		// send response to the client
		this.connection.sendRequest(serverEvent.showProofLiteResponse, { response: proofScript, args: request });
	}

	/**
	 * Internal function, loads the proof script for the formula indicated in the request
	 * @param request 
	 */
	async loadProof (request: { 
		fileName: string, 
		fileExtension: string, 
		theoryName: string, 
		formulaName: string, 
		contextFolder: string
	}): Promise<ProofDescriptor> {
		if (this.pvsProxy) {
			return this.pvsProxy.loadProof(request);
		}
		// else
		console.error(`[pvs-language-server] Error: Could not load proof script (pvs-proxy is null)`);
		return null;
	}

	/**
	 * Load jprf proof file
	 * @param args Handler arguments: filename, file extension, context folder, theory name, formula name
	 */
	// async loadProofRequest (request: { 
	// 	fileName: string, 
	// 	fileExtension: string, 
	// 	theoryName: string, 
	// 	formulaName: string, 
	// 	contextFolder: string
	// }): Promise<void> {
	// 	if (request) {
	// 		request = fsUtils.decodeURIComponents(request);
	// 		const pdesc: ProofDescriptor = await this.loadProof(request);
	// 		if (pdesc) {
	// 			this.connection.sendRequest(serverEvent.loadProofResponse, { response: { result: pdesc }, args: request });
	// 		} else {
	// 			// something went wrong
	// 			console.warn(`[pvs-language-server] Warning: load-proof-request was unable to receive proof`);
	// 		}
	// 	}
	// }

	/**
	 * Save jprf proof file
	 * @param args Handler arguments: filename, file extension, context folder, theory name, formula name, proof
	 */
	async saveProofRequest (request: { 
		fileName: string, 
		fileExtension: string, 
		theoryName: string, 
		formulaName: string, 
		contextFolder: string, 
		proofDescriptor: ProofDescriptor,
		quit?: boolean
	}): Promise<void> {
		if (request) {
			const success: boolean = await this.saveProof(request);
			if (request.quit) {
				await this.quitProof();
			}
			this.connection.sendRequest(serverEvent.saveProofResponse, { response: { success }, args: request });
			// trigger a context update, so proof status will be updated on the front-end
			setTimeout(() => {
				this.getContextDescriptor({ contextFolder: request.contextFolder }).then((cdesc: PvsContextDescriptor) => {
					this.connection.sendRequest(serverEvent.contextUpdate, cdesc);
				});	
			}, 200);
		} else {
			console.error("[pvs-language-server] Warning: save-proof invoked with null or incomplete descriptor", request);
		}
	}
	async saveProof (request: { 
		fileName: string, 
		fileExtension: string, 
		theoryName: string, 
		formulaName: string, 
		contextFolder: string, 
		proofDescriptor: ProofDescriptor
	}): Promise<boolean> {
		if (this.pvsProxy) {
			return this.pvsProxy.saveProof(request);
		}
		// else
		console.error(`[pvs-language-server] Error: Could not save proof script (pvs-proxy is null)`);
		return false;
	}
	/**
	 * Typecheck file
	 * @param args Handler arguments: filename, file extension, context folder
	 */
	async typecheckFile (args: PvsFile): Promise<PvsResponse | null> {
		if (args && args.fileName && args.fileExtension && args.contextFolder) {
			try {
				args = fsUtils.decodeURIComponents(args);
				const response: PvsResponse = await this.pvsProxy.typecheckFile(args);
				return response;
			} catch (ex) {
				console.error('[pvs-language-server.typecheckFile] Error: pvsProxy has thrown an exception', ex);
				return null;
			}
		}
		return null;
	}
	async typecheckFileRequest (request: PvsFile): Promise<void> {
		if (request) {
			request = fsUtils.decodeURIComponents(request);
			const desc: {
				fileName: string, 
				fileExtension: string, 
				contextFolder: string 
			} = (typeof request === "string") ? fsUtils.fname2desc(request) : request;
			if (desc) {
				const fname: string = `${desc.fileName}${desc.fileExtension}`;
				// make sure file exists
				if (!fsUtils.fileExists(fname)) {
					this.notifyMessage({ msg: `Warning: file ${fname} does not exist.` });
					return;
				}
				const taskId: string = `typecheck-${fname}`;
				// send feedback to the front-end
				this.notifyStartImportantTask({ id: taskId, msg: `Typechecking ${fname}` });
				// parse workspace first, so the front-end is updated with statistics
				await this.parseWorkspaceRequest(request); // this could be done in parallel with typechecking -- pvs-server is not able for now tho.
				// proceed with typechecking
				const response: PvsResponse = await this.typecheckFile(desc);
				this.connection.sendRequest(serverEvent.typecheckFileResponse, { response, args: request });
				// send diagnostics
				if (response) {
					if (response.result) {
						const fname: string = fsUtils.desc2fname(desc);
						this.diags[fname] = {
							pvsResponse: response,
							isTypecheckError: true
						};
						this.sendDiagnostics("Typecheck");
						this.notifyEndImportantTask({ id: taskId, msg: `${desc.fileName}${desc.fileExtension} typechecked successfully!` });
					} else {
						this.pvsErrorManager.handleTypecheckError({ response: <PvsError> response, taskId, request });
						// send diagnostics
						if (response.error.data) {
							const fname: string = (response.error.data.file_name) ? response.error.data.file_name : fsUtils.desc2fname(desc);
							const msg: string = response.error.data.error_string || "";
							this.diags[fname] = {
								pvsResponse: response,
								isTypecheckError: true
							};
							this.sendDiagnostics("Typecheck");
						}
					}
				}
			} else {
				console.error("[pvs-language-server] Warning: pvs.typecheck-file is unable to identify filename for ", request);
			}
		} else {
			console.error("[pvs-language-server] Warning: pvs.typecheck-file invoked with null request");
		}
	}
	/**
	 * Returns all tccs for a given file
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
						const response: PvsResponse = await this.pvsProxy.tccs({
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
	async generateSummaryRequest (request: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, content?: string }): Promise<void> {
		if (request) {
			const content: string = request.content || "";
			const contextFolder: string = request.contextFolder;
			const fileName: string = request.theoryName;
			const fileExtension: string = ".summary";
			await fsUtils.writeFile(fsUtils.desc2fname({
				contextFolder,
				fileName,
				fileExtension
			}), content);
			this.connection.sendRequest(serverEvent.generateSummaryResponse, {
				response: {
					contextFolder,
					fileName,
					fileExtension
				}, 
				args: request 
			});
		}
	}
	async generateTccsRequest (request: { fileName: string, fileExtension: string, contextFolder: string }, opt?: { quiet?: boolean, showTccsRequest?: boolean }): Promise<void> {
		request = fsUtils.decodeURIComponents(request);
		if (request) {
			opt = opt || {};
			const desc: {
				fileName: string, 
				fileExtension: string, 
				contextFolder: string 
			} = (typeof request === "string") ? fsUtils.fname2desc(request) : request;
			if (desc) {
				desc.fileExtension = ".pvs"; // tccs can be generated only for .pvs files
				const fname: string = fsUtils.desc2fname(desc);
				const shortName: string = `${desc.fileName}${desc.fileExtension}`;
				const taskId: string = `generate-tcc-for-${fname}`;
				if (!opt.quiet) {
					this.notifyStartImportantTask({ id: taskId, msg: `Generating typecheck conditions for ${shortName}`});
				}
				// parse files first, so front-end is updated with stats
				// await this.parseWorkspaceRequest(request); // this could be done in parallel with typechecking, pvs-server is not able to do this tho.
				// then generate tccs
				const response: PvsContextDescriptor = await this.generateTccs(desc);
				if (opt.showTccsRequest) {
					this.connection.sendRequest(serverEvent.showTccsResponse, { response, args: request });
				} else {
					this.connection.sendRequest(serverEvent.generateTccsResponse, { response, args: request });
				}

				let nTccs: number = 0;
				let nProved: number = 0;
				if (response && response.fileDescriptors && response.fileDescriptors[fname] && response.fileDescriptors[fname].theories) {
					const theories: TheoryDescriptor[] = response.fileDescriptors[fname].theories;
					for (let i = 0; i < theories.length; i++) {
						nTccs += theories[i].theorems ? theories[i].theorems.length : 0;
						for (let j = 0; j < nTccs; j++) {
							if (utils.isProved(theories[i].theorems[j].status)) {
								nProved++;
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
	 * Re-runs the proofs for all theorems and tccs in the given pvs file
	 * @param args Handler arguments: filename, file extension, context folder
	 */
	// async dischargeTheorems (args: { fileName: string, fileExtension: string, contextFolder: string }): Promise<PvsResponse> {
	// 	args = fsUtils.decodeURIComponents(args);
	// 	if (this.checkArgs("proveFile", args)) {
	// 		try {
	// 			return await this.pvsProxy.proveFile(args);
	// 		} catch (ex) {
	// 			console.error('[pvs-language-server.proveFile] Error: pvsProxy has thrown an exception', ex);
	// 			return null;
	// 		}
	// 	}
	// 	return null;
	// }
	// async dischargeTheoremsRequest (request: { fileName: string, fileExtension: string, contextFolder: string }): Promise<void> {
	// 	if (request) {
	// 		request = fsUtils.decodeURIComponents(request);
	// 		const desc: {
	// 			fileName: string, 
	// 			fileExtension: string, 
	// 			contextFolder: string 
	// 		} = (typeof request === "string") ? fsUtils.fname2desc(request) : request;
	// 		if (desc) {
	// 			const fname: string = `${desc.fileName}${desc.fileExtension}`;
	// 			const taskId: string = `prove-theorems-for-${fname}`;
	// 			this.notifyStartImportantTask({ id: taskId, msg: `Re-running all proofs in ${fname}`});
	// 			// parse files first, so front-end is updated with stats
	// 			await this.parseWorkspaceRequest(request); // this could be done in parallel with typechecking, pvs-server is not able to do this tho.
	// 			// then generate tccs
	// 			await this.dischargeTheorems(desc);
	// 			this.connection.sendRequest(serverEvent.dischargeTheoremsResponse, { response: null, args: request });
	// 			this.notifyEndImportantTask({ id: taskId });
	// 		} else {
	// 			console.error("[pvs-language-server] Warning: pvs.discharge-theorems is unable to identify filename for ", request);
	// 		}
	// 	} else {
	// 		console.error("[pvs-language-server] Warning: pvs.discharge-theorems invoked with null request");
	// 	}
	// }
	/**
	 * Parse file
	 * @param args Handler arguments: filename, file extension, context folder
	 */
	async parseFile (args: PvsFile): Promise<PvsResponse> {
		args = fsUtils.decodeURIComponents(args);
		if (args && args.fileName && args.fileExtension && args.contextFolder) {			
			const enableEParser: boolean = !!(this.connection && await this.connection.workspace.getConfiguration("pvs.settings.parser.errorTolerant"));
			try {
				return await this.pvsProxy.parseFile(args, { enableEParser });
			} catch (ex) {
				console.error('[pvs-language-server.parseFile] Error: pvsProxy has thrown an exception', ex);
				return null;
			}
		}
		return null;
	}
	async parseFileRequest (request: PvsFile, opt?: { withFeedback?: boolean }): Promise<void> {
		if (await this.getServerMode() === "in-checker") {
			return;
		}
		request = fsUtils.decodeURIComponents(request);
		if (request) {
			opt = opt || {};
			const desc: {
				fileName: string, 
				fileExtension: string, 
				contextFolder: string 
			} = (typeof request === "string") ? fsUtils.fname2desc(request) : request;
			if (desc) {
				if (fsUtils.isPvsFile(desc)) {
					if (desc.contextFolder === path.join(this.lastParsedContext, fsUtils.pvsbinFolder)) {
						// nothing to do
						return;
					}
					// send information to the client, to populate theory explorer on the front-end
					this.lastParsedContext = desc.contextFolder;
					const cdesc: PvsContextDescriptor = await this.getContextDescriptor({ contextFolder: desc.contextFolder });
					this.connection.sendRequest(serverEvent.contextUpdate, cdesc);
					
					// we should only parse .pvs files
					if (desc.fileExtension === ".pvs") {
						const fname: string = fsUtils.desc2fname(desc);
						// make sure file exists
						if (!fsUtils.fileExists(fname)) {
							this.notifyMessage({ msg: `Warning: file ${fname} does not exist.` });
							return;
						}
						const taskId: string = `parse-${fname}`;
						// send feedback to the front-end
						if (opt.withFeedback) {
							this.notifyStartImportantTask({ id: taskId, msg: `Parsing file ${fname}` });
						}

						// send workspace stats
						const contextFiles: FileList = await fsUtils.listPvsFiles(desc.contextFolder);
						if (contextFiles && contextFiles.fileNames) {
							const nfiles: number = contextFiles.fileNames.length;
							this.connection.sendRequest(serverEvent.workspaceStats, { contextFolder: desc.contextFolder, files: nfiles });
						}
						
						// parse file, as requested
						const response: PvsResponse = await this.parseFile(desc);
						let source: string = "Parse";
						if (response) {
							// send parser response
							this.connection.sendRequest(serverEvent.parseFileResponse, response);
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
									this.pvsErrorManager.handleParseFileError({ taskId, request, source });
								} else {
									this.notifyEndImportantTask({ id: taskId, msg: `${desc.fileName}${desc.fileExtension} parsed successfully!` });
								}
							}
						} 
						// send diagnostics
						this.sendDiagnostics(source);
					}
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
			actionId?: string }
	): Promise<void> {
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
					this.connection.sendRequest(serverEvent.contextUpdate, cdesc);

					// parse/typecheck files, as requested
					const contextFiles: FileList = await fsUtils.listPvsFiles(contextFolder);
					if (contextFiles && contextFiles.fileNames && contextFiles.fileNames.length) {
						const nfiles: number = contextFiles.fileNames.length;
						let completed: number = 0;

						const msg: string = opt.msg	|| `${actionFriendlyNameContinuous} workspace ${workspaceName} (0/${nfiles} files processed)`;
						this.notifyProgressImportantTask ({ id: taskId, msg, increment: -1 }); 
						console.log(msg);

						this.connection.sendRequest(serverEvent.workspaceStats, { contextFolder, files: nfiles });
						// run the promises & cap the concurrent function execution
						let next_file_index: number = this.MAX_PARALLEL_PROCESSES;
						let completed_tasks: number = 0;
						let errors: string[] = [];
						
						const actionFunction: "parseFile" | "typecheckFile" = (action === "parse-workspace") ? "parseFile" : "typecheckFile";
						// worker
						const workspaceActionAux = async (
							actionName: "parseFile" | "typecheckFile", 
							desc: { fileName: string, fileExtension: string, contextFolder: string }
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
								this.connection.sendRequest(serverEvent.workspaceStats, {
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
										let msg: string = (opt.suppressFinalMessage) ? ""
											: (opt.msg) ? opt.msg
												: (errors.length === 1) ? errors[0] 
													: `Workspace ${workspaceName} contains ${actionFriendlyName.toLocaleLowerCase()} errors`;
										// for (let i = 0; i < errors.length && errors.length > 1; i++) {
										// 	this.notifyError({ msg: errors[i] });
										// }
										this.pvsErrorManager.handleWorkspaceActionError({ taskId, msg, request });
									} else {
										if (!opt.keepDialogOpen) {
											const msg: string = (opt.suppressFinalMessage) ? ""
												: opt.msg || `Workspace ${workspaceName} ${actionFriendlyNamePast.toLocaleLowerCase()} successfully (${completed} files processed)`;
											this.notifyEndImportantTask({ id: taskId, msg });
										}
									}
								}
								resolve();
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
							resolve();
						}
					} else {
						console.log(`[pvs-language-server] Workspace ${workspaceName} does not contain pvs files`);
						if (!opt.keepDialogOpen) {
							const msg: string = (opt.suppressFinalMessage) ? ""
								: (opt.msg) ? opt.msg : `Workspace ${workspaceName} ${actionFriendlyNamePast.toLocaleLowerCase()} (0 files processed)`;
							this.notifyEndImportantTask({ id: taskId, msg });
						}
						resolve();
					}
				} else {
					console.warn(`[pvs-language-server] Warning: workspace name is null`);
				}
			} else {
				console.error(`[pvs-language-server] Warning: pvs.${action} invoked with null request`);
			}
		});
	}
	async parseWorkspaceRequest (request: { contextFolder: string }, opt?: {
		withFeedback?: boolean, 
		suppressFinalMessage?: boolean,
		keepDialogOpen?: boolean,
		actionId?: string,
		msg?: string 
	}): Promise<void> {
		request = fsUtils.decodeURIComponents(request);
		return await this.workspaceActionRequest("parse-workspace", request, opt);
	}
	async typecheckWorkspaceRequest (request: { contextFolder: string }, opt?: { 
		withFeedback?: boolean,
		suppressDialogCreation?: boolean
	}): Promise<void> {
		request = fsUtils.decodeURIComponents(request);
		return await this.workspaceActionRequest("typecheck-workspace", request, opt);
	}
	/**
	 * Returns the list of pvs files in a given context folder
	 */
	async listContextFiles (args: { contextFolder: string }): Promise<FileList> {
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
			this.connection.sendRequest(serverEvent.listContextResponse, response);
		}
	}
	/**
	 * Returns the proof status for all theorems in a given theory
	 */
	async statusProofTheory (args: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string }): Promise<PvsResponse> {
		args = fsUtils.decodeURIComponents(args);
		return await this.pvsProxy.statusProofTheory(args);
	}
	/**
	 * Returns the proof status for all theorems in a given file
	 */
	async statusProof (args: { fileName: string, fileExtension: string, contextFolder: string }): Promise<{ [ fname: string ]: PvsResponse }> {
		args = fsUtils.decodeURIComponents(args);
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
	async getPreludeDescriptor (): Promise<PvsContextDescriptor> {
		return await this.cachePreludeDescriptor();
	}
	/**
	 * Returns a descriptor with information on all theories in a given context folder
	 * @param contextFolder Context folder
	 */
	async getContextDescriptor (args: { contextFolder: string }): Promise<PvsContextDescriptor> {
		if (args) {
			args = fsUtils.decodeURIComponents(args);
			if (this.pvsProxy) {
				if (this.pvsProxy.isProtectedFolder(args.contextFolder)) {
					return await this.getPreludeDescriptor();
				} // else
				return await utils.getContextDescriptor(args.contextFolder, { listTheorems: true, includeTccs: true });
			} else {
				console.error('[pvs-language-server.listTheories] Error: pvs proxy is null');
			}
		} else {
			console.error('[pvs-language-server.listTheories] Error: getContextDescriptor invoked with null descriptor');
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
				if (this.pvsProxy.isProtectedFolder(args.contextFolder)) {
					const cdesc: PvsContextDescriptor = await this.getPreludeDescriptor();
					if (cdesc) {
						return cdesc[fsUtils.desc2fname(args)];
					} else {
						console.error('[pvs-language-server.listTheories] Error: could not read file descriptor for protected file ', args);
						return null;
					}
				} // else
				const fname: string = fsUtils.desc2fname(args);
				return await utils.getFileDescriptor(fname, opt);
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
				return await this.pvsProxy.hp2pvs(args);
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
			const res: string = await this.pvsProxy.prettyPrintDdl(args);
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
					this.connection.sendRequest(serverEvent.parseFileResponse, response);
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
					this.pvsErrorManager.notifyEndImportantTaskWithErrors({ id: taskId, msg: `Error: ${desc.fileName}.pvs could not be generated -- please check pvs-server output to view errors.` });
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
				// trigger parsing
				await this.parseFileRequest({ fileName, fileExtension, contextFolder }); // async call, will automatically send diags to the client
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
		this.hoverProvider = new PvsHoverProvider(this.definitionProvider);
		this.linter = new PvsLinter();
		this.cliGateway = new PvsCliGateway(this);
		this.proofExplorer = new PvsProofExplorer(this.connection, this.pvsProxy, this);
	}

	getProofExplorer (): PvsProofExplorer {
		return this.proofExplorer;
	}

	getGatewayPort(): number {
		if (this.cliGateway) {
			return this.cliGateway.getPort();
		}
		return 0;
	}
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
						} : null;
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
				if (this.connection) {
					this.connection.sendNotification("pvs-error", `Error while reading file ${fileName} (${readError.message})`);
				}
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
		// cache prelude libraries
		const preludeCache: string = path.join(this.pvsPath, "prelude.cache.json");
		const cache: string = await fsUtils.readFile(preludeCache);
		if (cache) {
			try {
				const res: PvsContextDescriptor = JSON.parse(cache);
				if (res && res.fileDescriptors) {
					return res;
				}
			} catch (jsonError) {
				console.error("[pvs-language-server] Error: unable to parse prelude cache");
				return null;
			}	
		}
		// else, cache file not present or in wrong format, create it again
		const libPath: string = path.join(this.pvsPath, "lib");
		const cdesc: PvsContextDescriptor = await utils.getContextDescriptor(libPath);
		await fsUtils.writeFile(preludeCache, JSON.stringify(cdesc, null, " "));
		return cdesc;
	}

	protected async sendWorkspaceInfo (): Promise<void> {
		const res: PvsResponse = await this.pvsProxy.currentContext();
		if (res && res.result) {
			const contextFolder: string = res.result;
			const contextFiles: FileList = await fsUtils.listPvsFiles(contextFolder);
			const nfiles: number = contextFiles.fileNames.length;
			this.connection.sendRequest(serverEvent.workspaceStats, { contextFolder, files: nfiles });
		}
	}

	protected async sendPvsVersionInfo (): Promise<boolean> {
		const desc: PvsVersionDescriptor = this.pvsProxy.getPvsVersionInfo();
		if (desc) {
			const pvsVersion: number = parseFloat(desc["pvs-version"]);
			if (pvsVersion >= this.MIN_PVS_VERSION) {
				await this.sendWorkspaceInfo();
				this.pvsVersionDescriptor = desc;
				this.connection.sendRequest(serverEvent.pvsServerReady, desc);
				this.connection.sendRequest(serverEvent.pvsVersionInfo, desc);
				return true;
			} else {
				console.error(`[pvs-language-server] Error: incompatible pvs version ${desc["pvs-version"]}`);
				this.connection.sendRequest(serverEvent.pvsIncorrectVersion, `Incorrect PVS version ${desc["pvs-version"]} (vscode-pvs requires pvs ver >= ${this.MIN_PVS_VERSION})`);
			}
		} else {
			const msg: string = `PVS executable not found at ${this.pvsPath}`;
			console.error(msg);
			this.connection.sendRequest(serverEvent.pvsNotPresent, msg);
		}
		return false;
	}

	async startPvsServer (desc: { pvsPath: string, contextFolder?: string, externalServer?: boolean }, opt?: { verbose?: boolean, debugMode?: boolean }): Promise<boolean> {
		if (desc) {
			opt = opt || {};
			desc = fsUtils.decodeURIComponents(desc);
			if (desc.pvsPath !== this.pvsPath && this.pvsProxy) {
				// the server was already running, the user must have selected a different pvs path. Kill the existing server.
				await this.pvsProxy.killPvsServer();
			}
			this.pvsPath = desc.pvsPath || this.pvsPath;
			const externalServer: boolean = !!desc.externalServer;
			if (this.pvsPath) {
				console.log(`[pvs-language-server] Rebooting PVS (installation folder is ${this.pvsPath})`);
				if (this.pvsProxy) {
					if (externalServer) {
						await this.pvsProxy.enableExternalServer();
					} else {
						await this.pvsProxy.disableExternalServer();
					}
					await this.pvsProxy.restartPvsServer({ pvsPath: this.pvsPath });
				} else {
					this.pvsProxy = new PvsProxy(this.pvsPath, { connection: this.connection, externalServer });
					this.pvsioProxy = new PvsIoProxy(this.pvsPath, { connection: this.connection })
					this.createServiceProviders();
					const success: boolean = await this.pvsProxy.activate({
						debugMode: opt.debugMode, 
						verbose: opt.debugMode !== false,
						pvsErrorManager: this.pvsErrorManager
					});
					if (!success) {
						console.error("[pvs-language-server] Error: failed to activate pvs-proxy");
						this.connection.sendRequest(serverEvent.pvsNotPresent);
						return false;
					}
				}
				// activate cli gateway
				await this.cliGateway.activate();
				this.notifyServerMode("lisp");
				return true;
			} else {
				console.error("[pvs-language-server] Error: failed to identify PVS path");
				if (this.connection) {
					this.connection.sendRequest(serverEvent.pvsNotPresent);
				}
			}
		}
		return false;
	}

	/**
	 * Internal function, restarts pvs-server
	 * FIXME: create separate functions for starting pvs-server and pvs-proxy
	 * @param desc 
	 */
	protected async startPvsServerRequest (desc: { pvsPath: string, contextFolder?: string, externalServer?: boolean }): Promise<boolean> {
		const success: boolean = await this.startPvsServer(desc);
		if (success) {
			// send version info to the front-end
			await this.sendPvsVersionInfo();
			return true;
		}
		return false;
	}

	/**
	 * quitProofEvent triggers a dialog on the front-end that asks whether the proof should be saved
	 */
	// quitProofEvent (): void {
	// 	this.connection.sendRequest(serverEvent.quitProofEvent);
	// }

	/**
	 * Quits the prover
	 * @param opt 
	 */
	async quitProof (): Promise<void> {
		if (await this.getServerMode() === "in-checker") {
			const useLispInterface: boolean = true;//!!(this.connection && await this.connection.workspace.getConfiguration("pvs.xperimental.developer.lispInterface"));
			const response: PvsResponse = await this.pvsProxy.proofCommand({ cmd: "(quit)" }, { useLispInterface });
			if (response && response.error) {
				this.pvsErrorManager.handleProofCommandError({ cmd: "(quit)", response: <PvsError> response });
			}
		}
	}

	notifyServerMode (mode: ServerMode): void {
		if (this.connection) {
			this.connection.sendRequest(serverEvent.serverModeUpdateEvent, { mode });
		}
	}

	async quitProofRequest (): Promise<void> {
		await this.quitProof();
		if (this.connection) {
			this.connection.sendRequest(serverEvent.quitProofResponse);
		}
	}

	async viewPreludeFileRequest (): Promise<void> {
		if (this.connection) {
			this.connection.sendRequest(serverEvent.viewPreludeFileResponse, {
				contextFolder: path.join(this.pvsPath, "lib"),
				fileName: "prelude",
				fileExtension: ".pvs"
			});
		}
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
					definitionProvider: true,
					workspaceSymbolProvider: true
					// documentSymbolProvider: true
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
			this.connection.onRequest(serverCommand.cancelOperation, () => {
				console.log(`[pvs-server] Operation cancelled by the user`);
				// which pvs-server command should I invoke to stop the operation??
			});
			this.connection.onRequest(serverCommand.getContextDescriptor, (request: { contextFolder: string }) => {
				if (request && request.contextFolder) {
					// send information to the client, to populate theory explorer on the front-end
					this.getContextDescriptor(request).then((cdesc: PvsContextDescriptor) => {
						if (this.connection) {
							this.connection.sendRequest(serverEvent.getContextDescriptorResponse, cdesc);
						}
					});
				}
			});
			this.connection.onRequest(serverCommand.getFileDescriptor, (request: { contextFolder: string, fileName: string, fileExtension: string }) => {
				if (request && request.contextFolder) {
					// send information to the client, to populate theory explorer on the front-end
					this.getFileDescriptor(request).then((fdesc: PvsFileDescriptor) => {
						if (this.connection) {
							this.connection.sendRequest(serverEvent.getFileDescriptorResponse, fdesc);
						}
					});
				}
			});
			this.connection.onRequest(serverCommand.stopPvsServer, async () => {
				if (this.pvsProxy) {
					await this.pvsProxy.killPvsServer();
				}
			});
			this.connection.onRequest(serverCommand.startPvsServer, async (request: { pvsPath: string, contextFolder?: string, externalServer?: boolean }) => {
				// this should be called just once at the beginning
				const success: boolean = await this.startPvsServerRequest(request);
				if (success) {
					// send information to the client, to populate theory explorer on the front-end
					const contextFolder: string = request.contextFolder || this.lastParsedContext || this.pvsPath;
					if (!this.isSameWorkspace(contextFolder)) {
						this.lastParsedContext = contextFolder;
					}
					const cdesc: PvsContextDescriptor = await this.getContextDescriptor({ contextFolder });
					if (this.connection) {
						this.connection.sendRequest(serverEvent.contextUpdate, cdesc);
					}
				} else {
					console.error(`[pvs-server] Error: failed to start pvs-server`);
					this.pvsErrorManager.handleStartPvsServerError(ProcessCode.PVSSTARTFAIL);
				}
			});
			this.connection.onRequest(serverCommand.rebootPvsServer, async (desc?: { pvsPath?: string }) => {
				await fsUtils.deletePvsCache(this.lastParsedContext, { keepTccs: true }); // this will remove .pvscontext and pvsbin
				await this.pvsProxy.rebootPvsServer(desc);
				this.notifyServerMode("lisp");
				// send version info				
				await this.sendPvsVersionInfo();
			});
			this.connection.onRequest(serverCommand.parseFile, async (request: { fileName: string, fileExtension: string, contextFolder: string }) => {
				this.parseFileRequest(request); // async call
			});
			this.connection.onRequest(serverCommand.parseFileWithFeedback, async (request: { fileName: string, fileExtension: string, contextFolder: string }) => {
				this.parseFileRequest(request, { withFeedback: true }); // async call
			});
			this.connection.onRequest(serverCommand.parseWorkspace, async (request:{ contextFolder: string }) => {
				this.parseWorkspaceRequest(request); // async call
			});
			this.connection.onRequest(serverCommand.parseWorkspaceWithFeedback, async (request: { contextFolder: string }) => {
				this.parseWorkspaceRequest(request, { withFeedback: true }); // async call
			});
			this.connection.onRequest(serverCommand.typecheckWorkspace, async (request: { contextFolder: string }) => {
				const workspaceName: string = request.contextFolder;
				const shortName: string = fsUtils.getContextFolderName(workspaceName);
				await this.parseWorkspaceRequest(request, {
					withFeedback: true, 
					suppressFinalMessage: true, 
					keepDialogOpen: true,
					actionId: "typecheck-workspace", // this is done to keep the same dialog on the front-end
					msg: `Preparing to typecheck workspace ${shortName}` 
				});
				await this.typecheckWorkspaceRequest(request, {
					withFeedback: true,
					suppressDialogCreation: true
				});
			});
			this.connection.onRequest(serverCommand.hp2pvs, async (request: PvsFile) => {
				this.hp2pvsRequest(request); // async call
			});
			this.connection.onRequest(serverCommand.typecheckFile, async (request: PvsFile) => {
				this.typecheckFileRequest(request); // async call
			});
			this.connection.onRequest(serverCommand.showTccs, async (request: { fileName: string, fileExtension: string, contextFolder: string, quiet?: boolean }) => {
				this.generateTccsRequest(request, { quiet: request && request.quiet, showTccsRequest: true }); // async call
			});
			this.connection.onRequest(serverCommand.generateTccs, async (request: { fileName: string, fileExtension: string, contextFolder: string, quiet?: boolean }) => {
				this.generateTccsRequest(request, { quiet: request && request.quiet }); // async call
			});
			this.connection.onRequest(serverCommand.generateSummary, async (request: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, content?: string }) => {
				this.generateSummaryRequest(request); // async call
			});
			this.connection.onRequest(serverCommand.listContext, async (request: ContextFolder) => {
				this.listContextFilesRequest(request); // async call
			});
			this.connection.onRequest(serverCommand.proveFormula, async (args: PvsFormula) => {
				await this.proveFormulaRequest(args);
			});
			this.connection.onRequest(serverCommand.autorunFormula, async (args: PvsFormula) => {
				await this.proveFormulaRequest(args, { autorun: true });
			});
			this.connection.onRequest(serverCommand.showProofLite, async (args: PvsFormula) => {
				this.showProofLiteRequest(args); // async call
			});
			this.connection.onRequest(serverCommand.proofCommand, async (args: PvsProofCommand) => {
				this.proofExplorer.proofCommandRequest(args); // async call
			});
			this.connection.onRequest(serverCommand.getGatewayConfig, async () => {
				const port: number = this.getGatewayPort();
				if (this.connection) {
					this.connection.sendRequest(serverEvent.getGatewayConfigResponse, { port });
				}
			});
			this.connection.onRequest(serverCommand.viewPreludeFile, async () => {
				this.viewPreludeFileRequest();
			});
			this.connection.onRequest(serverCommand.quitProof, async () => {
				this.quitProofRequest(); // this method will send a quitProofResponse to the client
			});

			this.connection.onRequest(serverCommand.listDownloadableVersions, async () => {
				const versions: PvsDownloadDescriptor[] = await PvsPackageManager.listDownloadableVersions();
				if (this.connection) {
					this.connection.sendRequest(serverEvent.listDownloadableVersionsResponse, { response: versions });
				}
			});
			this.connection.onRequest(serverCommand.downloadPvs, async (desc: PvsDownloadDescriptor) => {
				const fname: string = await PvsPackageManager.downloadPvsExecutable(desc);
				if (this.connection) {
					this.connection.sendRequest(serverEvent.downloadPvsResponse, { response: fname });
				}
			});
			this.connection.onRequest(serverCommand.downloadLicensePage, async () => {
				const licensePage: string = await PvsPackageManager.downloadPvsLicensePage();
				if (this.connection) {
					this.connection.sendRequest(serverEvent.downloadLicensePageResponse, { response: licensePage });
				}
			});

			this.connection.onRequest(serverCommand.startEvaluator, async (args: PvsTheory) => {
				this.startEvaluatorRequest(args);
			});
			this.connection.onRequest(serverCommand.evaluateExpression, async (args: PvsProofCommand) => {
				this.evaluateExpressionRequest(args);
			});


			// prover commands
			this.connection.onRequest(serverCommand.proofExecCommand, async (desc: ProofExecCommand) => {
				if (desc) {
					switch (desc.action) {
						case "forward": { this.proofExplorer.forward(); break; }
						case "back": { this.proofExplorer.back(); break; }
						case "fast-forward": { this.proofExplorer.fastForwardToNodeX(desc); break; }
						case "run": { await this.proofExplorer.run({ feedbackToTerminal: true }); break; }
						case "quit": { await this.proofExplorer.quitProof(); break; }
						default: {
							console.warn(`[pvs-server] Warning: unhandled proof exec request ${JSON.stringify(desc)}`);
						}
					}
				}
			});
			this.connection.onRequest(serverCommand.proofEditCommand, async (desc: ProofEditCommand) => {
				if (desc) {
					switch (desc.action) {
						case "append-node": { this.proofExplorer.appendNodeX(desc); break; }
						case "copy-node": { this.proofExplorer.copyNodeX(desc); break; }
						case "paste-node": { this.proofExplorer.pasteNodeX(desc); break; }
						case "copy-tree": { this.proofExplorer.copyTreeX(desc); break; }
						case "paste-tree": { this.proofExplorer.pasteTreeX(desc); break; }
						case "delete-node": { this.proofExplorer.deleteNodeX(desc); break; }
						case "append-branch": { this.proofExplorer.appendBranchX(desc); break; }
						case "cut-node": { this.proofExplorer.cutNodeX(desc); break; }
						case "cut-tree": { this.proofExplorer.cutTreeX(desc); break; }
						case "delete-tree": { this.proofExplorer.deleteTreeX(desc); break; }
						case "trim-node": { this.proofExplorer.trimNodeX(desc); break; }
						case "rename-node": { this.proofExplorer.renameNodeX(desc); break; }
						case "save": { await this.proofExplorer.saveProof(); break; }
						default: {
							console.warn(`[pvs-server] Warning: unhandled proof edit request ${JSON.stringify(desc)}`);
						}
					}
				}
			});
		});


		//-------------------------------
		//    LSP event handlers
		//-------------------------------

		this.connection.onCompletion(async (tpp: TextDocumentPositionParams): Promise<CompletionItem[]> => {
			const isEnabled: boolean = !!(await this.connection.workspace.getConfiguration("pvs.settings.completionProvider"));
			if (this.completionProvider && isEnabled) {
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
			const isEnabled: boolean = !!(await this.connection.workspace.getConfiguration("pvs.settings.hoverProvider"));
			if (this.hoverProvider && isEnabled) {
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
			const isEnabled: boolean = !!(await this.connection.workspace.getConfiguration("pvs.settings.codelensProvider"));
			if (this.codeLensProvider && isEnabled) {
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
			const isEnabled: boolean = !!(await this.connection.workspace.getConfiguration("pvs.settings.completionProvider"));
			if (this.codeLensProvider && isEnabled) {
				return this.codeLensProvider.resolveCodeLens(codeLens);
			}
			return null;
		});
		// this provider enables peek definition in the editor
		this.connection.onDefinition(async (tpp: TextDocumentPositionParams): Promise<Definition> => {
			const isEnabled: boolean = !!(await this.connection.workspace.getConfiguration("pvs.settings.definitionProvider"));
			if (this.definitionProvider && isEnabled) {
				const uri: string = tpp.textDocument.uri;
				if (fsUtils.isPvsFile(uri)) {
					const txt: string = await this.readFile(uri);
					if (txt) {
						const position: Position = tpp.position;
						const info: { symbolName: string, definitions: PvsDefinition[] } = await this.definitionProvider.provideDefinition({ txt, uri, position });
						if (info) {
							const pvsDefinitions: PvsDefinition[] = info.definitions;
							if (pvsDefinitions) {
								const ans: Location[] = [];
								for (let i: number = 0; i < pvsDefinitions.length; i++) {
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