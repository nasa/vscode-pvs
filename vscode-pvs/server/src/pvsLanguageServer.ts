/**
 * @module PvsLanguageServer
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

import { 
	Connection, TextDocuments, TextDocument, CompletionItem, createConnection, ProposedFeatures, InitializeParams, 
	DidChangeConfigurationNotification, TextDocumentPositionParams, Hover, CodeLens, CodeLensParams,
	Diagnostic, Position, Range, DiagnosticSeverity, Definition, DocumentSymbolParams, SymbolInformation, 
	ResponseError, Location, combineConsoleFeatures
} from 'vscode-languageserver';
import { 
	PvsParserResponse, PvsSymbolKind, PvsVersionDescriptor, PvsResponseType,
	PvsFindDeclarationRequest, PvsDefinition, PRELUDE_FILE, PvsDeclarationDescriptor, PvsDeclarationType,
	PvsListDeclarationsRequest, ExpressionDescriptor, EvaluationResult, ProofResult, FormulaDescriptor,
	PvsTypecheckerResponse, FileList, TheoryMap, TheoryList, TheoriesMap,
	TheoriesStatusMap
} from './common/serverInterface'
import { PvsProcess } from './pvsProcess';
import { PvsCompletionProvider } from './providers/pvsCompletionProvider';
import { PvsDefinitionProvider } from './providers/pvsDefinitionProvider';
import { PvsHoverProvider } from './providers/pvsHoverProvider';
import { PvsCodeLensProvider } from './providers/pvsCodeLensProvider';
import { PvsLinter } from './providers/pvsLinter';
import { getErrorRange, findTheories,  listTheoryNames, listTheoremsInFile } from './common/languageUtils';
import * as utils from './common/languageUtils';
import * as fsUtils from './common/fsUtils';
import * as path from 'path';
import { ContextDiagnostics } from './pvsProcess';
import { ProofDescriptor } from './common/serverInterface';
import { PvsStatusUpdater } from './providers/pvsStatusUpdater';


const SERVER_COMMANDS = [
	"pvs.restart", // start / restart pvs
	"pvs.language.regexp", // request regexp for syntax highlighting
	"pvs.version", // show pvs version
	// "pvs.find-definition", // obsolete find definition
	"pvs.list-declarations", // list declarations
	"pvs.parse-file", // parse file
	"pvs.change-context-and-parse-files", // change context to the folder passed as parameter and parse all files in the context
	"pvs.exec-prover-command", // command for the interactive theorem prover
	"pvs.list-theories", // list theories in the current context
	"pvs.list-files", // list of the pvs files in the current context
	"pvs.typecheck-all-and-show-tccs", // typecheck all files in the current context and generate tccs for all files
	"pvs.typecheck-theory-and-show-tccs", // typecheck theory and generate tccs for the theory
	"pvs.typecheck-file-and-show-tccs", // typecheck file and generate tccs for the file
	"pvs.typecheck-file", // typecheck file
	"pvs.typecheck-prove", // typecheck file and discharge tccs for that file
	// "pvs.show-tccs", // show tccs for the current theory -- replaced by pvs.typecheck-file-and-show-tccs
	"pvs.runit", // evaluate an expression in pvsio (literate programming)
	"pvs.step-proof" // step proof
];

// Example server settings, this is not used at the moment
interface Settings {
	maxNumberOfProblems: number;
}

const DEBUG_MODE: boolean = true;

class PvsLanguageServer {
	// pvs path, context folder, server path
	pvsPath: string;
	pvsContextFolder: string = __dirname;
	pvsVersionInfo: PvsVersionDescriptor;

	// array of pvs processes
	private pvsParser: PvsProcess; 		 // pvs process dedicated to parsing
	private pvsTypeChecker: PvsProcess;  // pvs process dedicated to typechecking 
	private dynamicPool: PvsProcess[] = []; // pvs processes dynamically allocated

	private connection: Connection; // connection to the client
	private documents: TextDocuments; // list of documents opened in the editor
	private serverCapabilities: {
		hasConfigurationCapability: boolean,
		hasWorkspaceFolderCapability: boolean,
		hasDiagnosticRelatedInformationCapability: boolean
	}
	private settings: {
		global: Settings,
		documents: Map<string, Thenable<Settings>>
	}
	private readyString: string;
	/**
	 * Service providers necessary to support the language server APIs
	 */
	private completionProvider: PvsCompletionProvider;
	private definitionProvider: PvsDefinitionProvider;
	private hoverProvider: PvsHoverProvider;
	private codeLensProvider: PvsCodeLensProvider;
	private linter: PvsLinter;
	private statusUpdater: PvsStatusUpdater;
	/**
	 * @constructor
	 */
	constructor () {
		// configure settings and capabilities
		this.settings = {
			global: { maxNumberOfProblems: 1000 },
			documents: new Map()
		};
		this.serverCapabilities = {
			hasConfigurationCapability: true,
			hasWorkspaceFolderCapability: true,
			hasDiagnosticRelatedInformationCapability: true
		};

		// Create a connection channel to allow clients to connect.
		// The connection uses Node's IPC as a transport. Includes all proposed LSP features.
		this.connection = createConnection(ProposedFeatures.all);
		this.setupConnectionManager();

		// Create a simple text document manager. The text document manager supports full document sync only
		this.documents = new TextDocuments();
		this.setupDocumentsManager(this.connection);

		// Listen to the connection with the client
		this.connection.listen();
	}
	/**
	 * Internal function, install document managers
	 * @param connection 
	 */
	private setupDocumentsManager (connection: Connection) {
		// -- Close the file only if it has been deleted from the context?
		// this.documents.onDidClose(e => { this.settings.documents.delete(e.document.uri); });
		// -- This event fires when the text document is first opened in the editor or when then content of the opened document has changed.
		// this.documents.onDidChangeContent(change => { ... });

		// onDidOpen fires when a document is opened on the editor
		this.documents.onDidOpen(async open => {
			if (this.pvsParser) {
				const fileName: string = fsUtils.getFilename(open.document.uri);
				const pvsContextFolder: string = fsUtils.getContextFolder(open.document.uri);
				if (this.pvsContextFolder !== pvsContextFolder) {
					this.pvsParser.changeContext(pvsContextFolder);
					fsUtils.listPvsFiles(pvsContextFolder).then((pvsFiles: FileList) => {
						this.listTheories().then((theories: TheoryList) => {
							this.connection.sendRequest("server.response.list-theories", theories);
						});	
						// this.connection.sendRequest("server.response.list-files", pvsFiles);
					});
				}
				// parse all files in the context -- this is necessary in pvs to create the data structures for resolving names
				this.changeContextAndParseFiles(pvsContextFolder);
				// this.parseAll(context).then((res: { [fileName: string]: PvsParserResponse }) => {
				// this.pvsParser.parseFile(fileName).then((diag: PvsParserResponse) => {
				// 	// Send diagnostics to VS Code. This will trigger the visualization of red wavy lines if errors are reported by the parser
				// 	const fileName: string = getFilename(open.document.uri);
				// 	// let diagnostics: Diagnostic[] = this.compileDiagnostics(open.document, res[fileName]);
				// 	const diagnostics: Diagnostic[] = this.compileDiagnostics(open.document, diag);
				// 	connection.sendDiagnostics({ uri: open.document.uri, diagnostics });
				// });
			}
		});

		// onDidSave fires when a document is saved on the editor
		this.documents.onDidSave(save => {
			if (this.pvsParser) {
				// Send diagnostics to VS Code. This will trigger the visualization of red wavy lines under the error.
				this.provideDiagnostics(save.document).then((diagnostics: Diagnostic[]) => {
					// diagnostics = diagnostics.concat(this.linter.provideDiagnostics(save.document));
					connection.sendDiagnostics({ uri: save.document.uri, diagnostics });
				});
			}
			// send updated version of theories to the client
			this.listTheories().then((theories: TheoryList) => {
				this.connection.sendRequest("server.response.list-theories", theories);
			});
		});

		// Listen to document events triggered by the editor
		this.documents.listen(connection);
	}
	/**
	 * Initialises pvs parser and service providers
	 */
	private async initServiceProviders(): Promise<void> {
		// Create service providers
		this.definitionProvider = new PvsDefinitionProvider(this.pvsParser, this.documents);
		this.completionProvider = new PvsCompletionProvider(this.definitionProvider);
		this.codeLensProvider = new PvsCodeLensProvider(this.definitionProvider);
		this.hoverProvider = new PvsHoverProvider(this.definitionProvider);
		this.linter = new PvsLinter();
		this.statusUpdater = new PvsStatusUpdater(this.connection);
		// parse all files in the current context. This operation will also send diagnostics to the front-end
		this.changeContextAndParseFiles(this.pvsContextFolder);
	}
	/**
	 * Utility function, creates a new pvs process
	 */
	private async createPvsProcess(opt?: { emacsInterface?: boolean, enableNotifications?: boolean, processType?: string }): Promise<PvsProcess> {
		opt = opt || {};
		opt.processType = opt.processType || "typechecker";
		const connection: Connection = (opt.enableNotifications) ? this.connection : null;
		const proc: PvsProcess = new PvsProcess({ pvsPath: this.pvsPath, pvsContextFolder: this.pvsContextFolder, processType: opt.processType }, connection);
		const success: boolean = await proc.pvs({
			enableNotifications: opt.enableNotifications
		});
		if (success) {
			await proc.disableGcPrintout();
			if (opt.emacsInterface) { await proc.emacsInterface(); }
			await proc.changeContext(this.pvsContextFolder);
			if (this.connection && !connection) {
				this.connection.sendNotification(`server.new-pvs-${opt.processType}`);
			}
			return proc;
		}
		return null;
	}
	private async killAllPvsProcesses() {
		if (this.pvsParser) {
			this.pvsParser.kill();
			// pvs parser does not come with a connection, we need to notify its deletion to correctly update the status bar
			this.connection.sendNotification(`server.delete-pvs-parser`);
		}
		if (this.pvsTypeChecker) {
			this.pvsTypeChecker.kill();
		}
		if (this.dynamicPool) {
			const dynamicPool: string[] = Object.keys(this.dynamicPool);
			dynamicPool.forEach(procID => {
				this.dynamicPool[procID].kill;
			});
		}
	}
	/**
	 * TODO: move diagnostics functions to a new service provider
	 */
	async provideDiagnostics(document: TextDocument): Promise<Diagnostic[]> {
		let desc: PvsParserResponse = await this.pvsParser.parseFile(document.uri);
		return this.compileDiagnostics(document, desc);
	}
	private compileDiagnostics(document: TextDocument, desc: PvsParserResponse): Diagnostic[] {
		if (document && desc && desc.error) {
			const errorPosition: Position = { line: desc.error.line - 1, character: desc.error.character };
			const errorRange: Range = getErrorRange(document.getText(), errorPosition);
			// const symbolName: string = vscode.window.activeTextEditor.document.getText(errorRange);
			// const msg: string = 'Syntax error at symbol **' + symbolName + '**';
			const diag: Diagnostic = {
				severity: DiagnosticSeverity.Error,
				range: errorRange,
				message: desc.error.msg,
				source: "Syntax error"
			};
			return [ diag ];
		}
		return [];
	}

	/**
	 * Utility function, normalises the structure of a file name in the form /path/to/the/context/folder/filename.pvs
	 * @param fileName Filename to be normalized
	 */
	private normaliseFileName (fileName: string) {
		if (!fileName.endsWith(".pvs")) {
			fileName += ".pvs";
		}
		if (!fileName.startsWith("/")) {
			fileName = path.join(this.pvsContextFolder, fileName);
		}
		return fileName;
	}
	/**
	 * Utility function, reads the content of a file
	 * @param fileName Path to the filename
	 * @returns Promise<string> Promise that resolves to the content of the file
	 */
	private async readFile (fileName: string): Promise<string> {
		fileName = this.normaliseFileName(fileName);
		let doc: TextDocument = this.documents.get("file://" + fileName);
		if (doc) {
			return doc.getText();
		}
		try {
			return fsUtils.readFile(fileName);
		} catch (readError) {
			this.info(`Error while reading file ${fileName} (${readError.message})`);
		}
		return null;
	}

	/**
	 * Interface function for typechecking a given file
	 * @param fileName Path to the file to be typechecked
	 * @returns Promise<TccList> Promise that resolves to a list of type check conditions
	 */
	private async typecheckFileAndShowTccs (fileName: string, typechecker: PvsProcess): Promise<TheoriesMap> {
		const txt: string = await this.readFile(fileName); // it is necessary to use the readFile function because some pvs files may not be loaded yet in the context 
		if (txt) {
			const context: string = fsUtils.getContextFolder(fileName);
			typechecker.changeContext(context);
			
			let typecheckerResponse: PvsTypecheckerResponse = null;
			try {
				typecheckerResponse = await typechecker.typecheckFile(fileName);
			} catch (typecheckerError) {
				console.error(typecheckerError);
			}
			if (typecheckerResponse) {
				const response: TheoriesMap = {
					pvsContextFolder: context,
					theoriesStatusMap: null
				};
				if (typecheckerResponse.error) {
					const txt: string = await fsUtils.readFile(fileName);
					if (txt) {
						const errorPosition: Position = { line: typecheckerResponse.error.line - 1, character: typecheckerResponse.error.character };
						const errorRange: Range = getErrorRange(txt, errorPosition);
						const diag: Diagnostic = {
							severity: DiagnosticSeverity.Error,
							range: errorRange,
							message: typecheckerResponse.error.msg,
							source: "Typecheck error"
						};
						this.connection.sendDiagnostics({ uri: "file://" + fileName, diagnostics: [ diag ] });
					}
				}
				if (typecheckerResponse.res) {
					response.theoriesStatusMap = {};
					const theoryNames: string[] = Object.keys(typecheckerResponse.res); //listTheoryNames(txt);
					for (const i in theoryNames) {
						const theoryName: string = theoryNames[i];
						// note: showTccs automatically trigger typechecking. However, we are still calling typecheckFile because we want to save the binary files
						// const pvsResponse: PvsResponseType = await typechecker.showTccs(fileName, theoryName);
						// const tccArray: TccDescriptor[] = (pvsResponse && pvsResponse.res) ? pvsResponse.res : [];
						response.theoriesStatusMap[theoryName] = {
							theoryName: theoryName,
							fileName: fileName,
							theorems: typecheckerResponse.res[theoryName].theorems // this includes the status of theorems and tccs
						};
					}
					// report updated list/status of theorems
					this.connection.sendNotification('pvs.context.theories-status.update', response.theoriesStatusMap);
				}
				return response;
			}
		}
		return null;
	}
	private async parallelTypeCheckAllAndShowTccs(): Promise<void> {
		const pvsContextFolder: string = this.pvsContextFolder;
		const pvsFiles: FileList = await fsUtils.listPvsFiles(pvsContextFolder);
		const promises = [];
		this.connection.sendNotification('server.status.info', "Allocating pool of typecheckers...");
		for (const i in pvsFiles.fileNames) {
			promises.push(new Promise (async (resolve, reject) => {
				let fileName: string = pvsFiles.fileNames[i];
				fileName = this.normaliseFileName(fileName);
				this.connection.sendNotification('server.status.info', "Typechecking " + fileName);
				const proc: PvsProcess = await this.createPvsProcess({
					enableNotifications: true
				});
				if (proc) {
					this.dynamicPool[proc.getProcessId()] = proc;
					const response: TheoriesMap = await this.typecheckFileAndShowTccs(fileName, proc);
					if (this.dynamicPool[proc.getProcessId()]) {
						this.dynamicPool[proc.getProcessId()].kill();
					}
					// the list of proof obligations is provided incrementally to the client so feedback can be shown as soon as available
					this.connection.sendRequest("server.response.typecheck-file-and-show-tccs", response);
				}
				resolve();	
			}));
		}
		// parallel typechecking
		await Promise.all(promises);
		this.connection.sendNotification('server.status.info', "Typechecking complete!");
		this.connection.sendNotification('server.status.update', this.readyString);
	}
	private async serialTypeCheckAllAndShowTccs(): Promise<void> {
		const pvsContextFolder: string = this.pvsContextFolder;
		const pvsFiles: FileList = await fsUtils.listPvsFiles(pvsContextFolder);
		for (const i in pvsFiles.fileNames) {
			let fileName: string = pvsFiles.fileNames[i];
			this.connection.sendNotification('server.status.info', "Typechecking " + fileName);
			fileName = this.normaliseFileName(fileName);
			this.connection.sendNotification('server.status.update', "Typechecking " + fileName);

			const proc: PvsProcess = this.pvsTypeChecker;
			if (proc) {
				const response: TheoriesMap = await this.typecheckFileAndShowTccs(fileName, proc);
				// the list of proof obligations is provided incrementally to the client so feedback can be shown as soon as available
				this.connection.sendRequest("server.response.typecheck-file-and-show-tccs", response);
				// feed symbols to the parser
				this.pvsParser.typecheckFile(fileName, false);
			}
		}
		this.connection.sendNotification('server.status.info', "Typechecking complete!");
		this.connection.sendNotification('server.status.update', this.readyString);
	}

	private async serialTCP(fileName: string, theoryNames: string[]): Promise<TheoriesMap> {
		const proc: PvsProcess = this.pvsTypeChecker;
		if (proc) {
			const typecheckerResponse: PvsTypecheckerResponse = await proc.typecheckProve(fileName);
			const tccs: TheoriesStatusMap = {};
			for (const i in theoryNames) {
				const theoryName: string = theoryNames[i];
				// const pvsResponse: PvsResponseType = await proc.showTccs(fileName, theoryNames[i]);
				// const tccArray: TccDescriptor[] = (pvsResponse && pvsResponse.res) ? pvsResponse.res : [];
				tccs[theoryNames[i]] = {
					theoryName: theoryName,
					fileName: fileName,
					theorems: typecheckerResponse.res[theoryName].theorems
				};
			}
			const pvsContextFolder: string = this.pvsContextFolder;
			const response: TheoriesMap = {
				pvsContextFolder: pvsContextFolder,
				theoriesStatusMap: tccs
			};
			// feed symbols to the parser
			this.pvsParser.typecheckFile(fileName, false);
			// return response to the caller
			return response;
		}
		return null;
	}

	// utility functions for showing notifications on the status bar
	private info(msg: string) {
        this.connection.sendNotification('server.status.update', msg);
    }
    private pvsReady() {
        this.connection.sendNotification('pvs-ready', this.readyString);
    }
    private pvsError(msg: string) {
        this.connection.sendNotification('pvs-error', msg);
    }

	/**
	 * Lists all theories in the current context folder
	 */
	private async listTheories (): Promise<TheoryList> {
		// this.connection.console.info("list-all-theories, file " + uri);
		const pvsContextFolder: string = this.pvsContextFolder;
		let response: TheoryList = await utils.listTheories(pvsContextFolder);
		return response;
	}

	/**
	 * Lists all theorems in the current context folder
	 */
	private async listTheorems (): Promise<TheoriesMap> {
		// this.connection.console.info("list-all-theories, file " + uri);
		const pvsContextFolder: string = this.pvsContextFolder;
		let response: TheoriesMap = await utils.listTheorems(pvsContextFolder);
		return response;
	}

	async changeContextAndParseFiles(context: string): Promise<ContextDiagnostics> {
		await this.pvsParser.changeContext(context);
		const res: ContextDiagnostics = await this.pvsParser.parseCurrentContext();
		const pvsFiles: FileList = await fsUtils.listPvsFiles(context);
		this.connection.sendRequest("server.response.change-context-and-parse-files", pvsFiles);
		const theories: TheoryList = await this.listTheories();
		this.connection.sendRequest("server.response.list-theories", theories);
		const theorems: TheoriesMap = await this.listTheorems();
		this.connection.sendRequest("server.response.list-theorems", theorems);
		return res;
	}


	private async getPvsPath (): Promise<string> {
		if (this.connection) {
			return (await this.connection.workspace.getConfiguration()).get("pvs.path");
		}
		return null;
	}

	private async startPvs (): Promise<void> {
		this.info("Initialising PVS...");
		try {
			// create pvs process allocated for parsing
			this.pvsParser = await this.createPvsProcess({ emacsInterface: true, processType: "parser" });
			if (this.pvsParser) {
				this.pvsTypeChecker = await this.createPvsProcess({ enableNotifications: true });
				// fetch pvs version information
				const ans: PvsResponseType = await this.pvsParser.pvsVersionInformation();
				const versionInfo: PvsVersionDescriptor = {
					pvsVersion: ans.res.pvsVersion,
					lispVersion: ans.res.lispVersion
				};
				this.pvsVersionInfo = versionInfo;
			} else {
				this.pvsVersionInfo = {
					pvsVersion: "Error: could not start PVS, please check PVS path in the Settings menu (File -> Preferences -> Settings -> Extension -> PVS)",
					lispVersion: ""
				};
			}
			// start service providers
			await this.initServiceProviders();
			// TODO: get rid of readystring
			this.readyString = `${this.pvsVersionInfo.pvsVersion} ${this.pvsVersionInfo.lispVersion}`;
			this.pvsReady();
		} catch (err) {
			this.connection.sendNotification('server.status.update', err);
			this.connection.sendNotification('server.status.error', err);
			this.pvsError(`Error while initialising PVS ${JSON.stringify(err)}`);
		}
	}


	/**
	 * Internal function, used to setup LSP event listeners
	 */
	private setupConnectionManager () {
		this.connection.onInitialize((params: InitializeParams) => {
			this.serverCapabilities.hasConfigurationCapability = true;
			this.serverCapabilities.hasWorkspaceFolderCapability = true;
			this.serverCapabilities.hasDiagnosticRelatedInformationCapability = true;
			return {
				capabilities: {
					textDocumentSync: this.documents.syncKind,
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
					executeCommandProvider: {
						commands: SERVER_COMMANDS
					},
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
			// start pvs process
			// this.pvsPath = await this.getPvsPath();
			// this.startPvs();

			// register handlers
			if (this.serverCapabilities.hasConfigurationCapability) {
				this.connection.client.register(DidChangeConfigurationNotification.type, undefined);
			}
			if (this.serverCapabilities.hasWorkspaceFolderCapability) {
				this.connection.workspace.onDidChangeWorkspaceFolders(evt => {
					this.connection.console.info('Workspace folder change event received.');
				});
			}
			//
			this.connection.onRequest('pvs.restart', async (desc: { pvsPath: string, pvsContextFolder: string }) => {
				if (desc) {
					this.pvsPath = desc.pvsPath || this.pvsPath;
					this.pvsContextFolder = desc.pvsContextFolder || this.pvsContextFolder;
					await this.killAllPvsProcesses();
				}
				this.startPvs();
			});

			this.connection.onRequest('pvs.change-context', async (contextPath) => {
				this.info(`Context changed to ${contextPath}`);
				const msg = await this.pvsParser.changeContext(contextPath);
				this.connection.sendRequest("server.response.change-context", msg);
				setTimeout(() => {
					this.pvsReady();
				}, 500);
			});
			this.connection.onRequest('pvs.version', async () => {
				const version = await this.pvsParser.pvsVersionInformation();
				this.connection.sendRequest("server.response.pvs.version", version);
			});

			this.connection.onRequest('pvs.parse-file', async (fileName: string) => {
				// this.info(`Parsing ${fsUtils.getFilename(fileName, { removeFileExtension: true })}`);
				fileName = this.normaliseFileName(fileName);
				const response: PvsParserResponse = await this.pvsParser.parseFile(fileName);
				this.connection.sendRequest("server.response.parse-file", response);
				// this.pvsReady();
			});
			this.connection.onRequest('pvs.typecheck-file', async (fileName: string) => {
				this.info(`Typechecking ${fsUtils.getFilename(fileName, { removeFileExtension: true })}`);
				fileName = this.normaliseFileName(fileName);
				const proc: PvsProcess = this.pvsTypeChecker;
				if (proc) {
					const response: PvsTypecheckerResponse = await proc.typecheckFile(fileName, false);
					this.connection.sendRequest("server.response.typecheck-file", response);
					// feed symbols to the parser
					this.pvsParser.typecheckFile(fileName, false);
				}
				this.pvsReady();
			});
			this.connection.onRequest('pvs.typecheck-prove', async (fileName: string) => {
				this.info(`Typechecking ${fsUtils.getFilename(fileName, { removeFileExtension: true })}`);
				fileName = this.normaliseFileName(fileName);
				this.connection.sendNotification('server.status.update', "Typechecking " + fileName);
				const proc: PvsProcess = this.pvsTypeChecker;
				if (proc) {
					const response: PvsTypecheckerResponse = await proc.typecheckFile(fileName, true);
					this.connection.sendRequest("server.response.typecheck-file", response);
					// feed symbols to the parser
					this.pvsParser.typecheckFile(fileName, false);
				}
				this.pvsReady();
			});


			this.connection.onRequest('pvs.change-context-and-parse-files', async (context: string) => {
				this.connection.sendNotification('server.status.update', `Parsing files in context ${context}`);
				const response: ContextDiagnostics = await this.changeContextAndParseFiles(context);
				// this.connection.sendRequest("server.response.change-context-and-parse-files", response);
				// this.connection.sendNotification('server.status.info', "Parsing complete!");
				this.connection.sendNotification('server.status.update', this.readyString);
			});
			this.connection.onRequest("pvs.list-declarations", async (desc: PvsListDeclarationsRequest) => {
				const response: PvsDeclarationDescriptor[] = await this.pvsParser.listDeclarations(desc);
				this.connection.sendRequest("server.response.list-declarations", response);
			});
			// TODO: add context folder as function argument?
			this.connection.onRequest("pvs.list-files", async () => {
				const pvsContextFolder: string = this.pvsContextFolder;
				const response: FileList = await fsUtils.listPvsFiles(pvsContextFolder);
				this.connection.sendRequest("server.response.list-files", response);
			});
			this.connection.onRequest("pvs.typecheck-all-and-show-tccs", () => {
				// this.parallelTypeCheckAllAndShowTccs();
				this.serialTypeCheckAllAndShowTccs();
			});
			this.connection.onRequest("pvs.list-theories", async (uri: string) => {
				// this.connection.console.info("list-all-theories, file " + uri);
				// const pvsContextFolder: string = this.pvsParser.getContextFolder();
				this.info(`Loading list of theories for context ${this.pvsContextFolder}`);
				let response: TheoryList = await this.listTheories();
				this.connection.sendRequest("server.response.list-theories", response);
				this.pvsReady();
			});
			// this.connection.onRequest("pvs.show-tccs", async (fileName: string, theoryName: string) => {
			// 	if (theoryName) {
			// 		this.info(`Generating proof obligations for ${theoryName}`);
			// 		const typecheckerResponse: PvsTypecheckerResponse = await this.pvsTypeChecker.typecheckFile(fileName);
			// 		// we need to typecheck twice because pvsParser and pvsTypechecker are two distinct processes
			// 		// we are saving the context after the first typecheck, so the second is faster
			// 		// await this.pvsParser.typecheckFile(fileName);
			// 		const pvsResponse: PvsResponseType = await this.pvsParser.showTccs(fileName, theoryName);
			// 		const tccArray: TccDescriptor[] = (pvsResponse && pvsResponse.res) ? pvsResponse.res : [];
			// 		const pvsContextFolder: string = this.pvsParser.getContextFolder();
			// 		const tccs: TheoriesStatusMap = {};
			// 		tccs[theoryName] = {
			// 			theoryName: theoryName,
			// 			fileName: fileName,
			// 			tccs: tccArray,
			// 			theorems: typecheckerResponse.res[theoryName].theorems
			// 		};
			// 		const response: TheoriesMap = {
			// 			pvsContextFolder: pvsContextFolder,
			// 			theoriesStatusMap: tccs
			// 		};
			// 		this.connection.sendRequest("server.response.show-tccs", response);
			// 		this.ready();
			// 	} else {
			// 		this.connection.sendNotification('server.status.error', "Malformed pvs.typecheck-theory-and-show-tccs request received by the server (fileName is null)");
			// 	}
			// });
			this.connection.onRequest("pvs.typecheck-file-and-show-tccs", async (fileName: string) => {
				if (fileName) {
					this.info(`Typechecking ${fsUtils.getFilename(fileName, { removeFileExtension: true })}`);
					fileName = this.normaliseFileName(fileName);
					const proc: PvsProcess = this.pvsTypeChecker;
					if (proc) {
						const response: TheoriesMap = await this.typecheckFileAndShowTccs(fileName, proc);
						this.connection.sendRequest("server.response.typecheck-file-and-show-tccs", response);
						// feed symbols to the parser
						this.pvsParser.typecheckFile(fileName, false);
					}
					this.pvsReady();
				} else {
					this.connection.sendNotification('server.status.error', "Malformed pvs.typecheck-file-and-show-tccs request received by the server (fileName is null)");
				}
			});
			this.connection.onRequest('pvs.typecheck-prove-and-show-tccs', async (fileName: string) => {
				if (fileName) {
					this.info(`Discharging proof obligations for ${fsUtils.getFilename(fileName, { removeFileExtension: true })}`);
					fileName = this.normaliseFileName(fileName);
					// execute tcp
					// TODO: run tcp in parallel -- to do this, create n processes, each proving one tcc (tcp can only perform sequential execution)
					// await this.pvsTypeChecker.typecheckProve(fileName);
					// gather updated information about tccs
					const txt: string = await this.readFile(fileName);
					if (txt) {
						const theoryNames: string[] = listTheoryNames(txt);
						const response: TheoriesMap = await this.serialTCP(fileName, theoryNames);
						// const response: TccList = await this.parallelTCP(fileName, theoryNames);
						this.connection.sendRequest("server.response.typecheck-prove-and-show-tccs", response);
					}
					this.pvsReady();
				} else {
					this.connection.sendNotification('server.status.error', "Malformed pvs.typecheck-prove-and-show-tccs request received by the server (fileName is null)");
				}
			});
			this.connection.onRequest('pvs.step-proof', async (data: ProofDescriptor) => {
				if (data) {
					const proc: PvsProcess = this.pvsTypeChecker;
					if (proc) {
						this.info("Initialising step-proof...");
						await proc.typecheckFile(`${data.fileName}${data.fileExtension}`);
						const response: PvsResponseType = await proc.stepProof(data);
						if (response && response.res) {
							this.connection.sendRequest("server.response.step-proof", response.res);
						} else {
							this.connection.sendNotification('server.status.error', `Error while executing step-proof: ${JSON.stringify(response)}`);
						}
						this.pvsReady();
						// feed symbols to the parser
						this.pvsParser.typecheckFile(data.fileName, false);
					}
				} else {
					this.connection.sendNotification('server.status.error', `Malformed pvs.step-proof request received by the server: ${JSON.stringify(data)}`);
				}
			});
			this.connection.onRequest('pvs.step-tcc', async (data: ProofDescriptor) => {
				if (data) {
					if (DEBUG_MODE) {
						console.log(`[DEBUG] pvs.step-tcc ${data}`)
					}
					const proc: PvsProcess = this.pvsTypeChecker;
					if (proc) {
						this.info("Initialising step-proof for tcc...");
						await proc.typecheckFile(data.fileName);
						await proc.showTccs(data.fileName, data.theoryName);
						const response: PvsResponseType = await proc.stepTcc(data);
						if (response && response.res) {
							this.connection.sendRequest("server.response.step-tcc", response.res);
						} else {
							this.connection.sendNotification('server.status.error', `Error while executing step-tcc: ${JSON.stringify(response)}`);
						}
						this.pvsReady();
						// feed symbols to the parser
						this.pvsParser.typecheckFile(data.fileName, false);
					}
				} else {
					this.connection.sendNotification('server.status.error', `Malformed pvs.step-proof request received by the server: ${JSON.stringify(data)}`);
				}
			});
			/**
			 *  literate programming
			 */
			this.connection.onRequest("pvs.runit", async (desc: ExpressionDescriptor) => {
				this.connection.console.log("received command runit");
				const response: EvaluationResult = await this.pvsParser.runit(desc);
				this.connection.sendRequest("server.response.runit", response);
			});
			// this.connection.onRequest("pvs.proveit", async (desc: FormulaDescriptor) => {
			// 	this.connection.console.log("received command proveit");
			// 	const response: EvaluationResult = await this.pvsParser.proveit(desc);
			// 	this.connection.sendRequest("server.response.proveit", response);
			// });
			this.connection.onRequest("kill-pvs", () => {
				this.killAllPvsProcesses();
			});
		});

		// register LSP event handlers
		this.connection.onCompletion(async (tpp: TextDocumentPositionParams): Promise<CompletionItem[]> => {
			// const isEnabled = await this.connection.workspace.getConfiguration("pvs").settings.completionProvider;
			if (tpp.textDocument.uri.endsWith(".pvs") && this.completionProvider) {
				const document: TextDocument = this.documents.get(tpp.textDocument.uri);
				let completionItems: CompletionItem[] = await this.completionProvider.provideCompletionItems(document, tpp.position);
				return completionItems;
			}
			return null;
		});
		this.connection.onHover(async (tpp: TextDocumentPositionParams): Promise<Hover> => {
			// const isEnabled = await this.connection.workspace.getConfiguration("pvs").settings.hoverProvider;
			if (fsUtils.isPvsFile(tpp.textDocument.uri) && this.hoverProvider) {
				const document: TextDocument = this.documents.get(tpp.textDocument.uri);
				let hover: Hover = await this.hoverProvider.provideHover(document, tpp.position);
				return hover;
			}
			return null;
		});
		this.connection.onCodeLens(async (tpp: CodeLensParams): Promise<CodeLens[]> => {
			// const isEnabled = await this.connection.workspace.getConfiguration("pvs").settings.codelensProvider;
			if ((tpp.textDocument.uri.endsWith(".pvs") || tpp.textDocument.uri.endsWith(".tccs")) && this.codeLensProvider) {
				const document: TextDocument = this.documents.get(tpp.textDocument.uri);
				let codelens: CodeLens[] = await this.codeLensProvider.provideCodeLens(document);
				return codelens;
			}
			return null;
		});
		// this provider enables peek definition in the editor
		this.connection.onDefinition(async (tpp: TextDocumentPositionParams): Promise<Definition> => {
			if (fsUtils.isPvsFile(tpp.textDocument.uri) && this.definitionProvider) {
				// const pvsFolder = (await this.connection.workspace.getConfiguration("pvs")).path;
				// const filePath = tpp.textDocument.uri.trim().split("/");
				// const fileName = filePath[filePath.length - 1].split(".pvs")[0];
				const document: TextDocument = this.documents.get(tpp.textDocument.uri);
				if (document) { 
					const pvsDefinitions: PvsDefinition[] = await this.definitionProvider.provideDefinition(document, tpp.position);
					if (pvsDefinitions) {
						const ans: Location[] = [];
						for (const i in pvsDefinitions) {
							const def: PvsDefinition = pvsDefinitions[i];
							const uri: string = (def.symbolDeclarationFile === PRELUDE_FILE) ?
													path.join(this.pvsParser.getLibrariesPath(), "prelude.pvs")
													: path.join(this.pvsContextFolder, def.symbolDeclarationFile + ".pvs");
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
			return null;
		});
	}
}

// instantiate the language server
new PvsLanguageServer();
