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
	Diagnostic, Position, Range, DiagnosticSeverity, Definition, DocumentSymbolParams, SymbolInformation, ResponseError
} from 'vscode-languageserver';
import { 
	PvsParserResponse, PvsSymbolKind, PvsVersionDescriptor, PvsResponseType, getFilename, getPathname,
	PvsFindDeclarationRequest, PvsFindDeclarationResponse, PRELUDE_FILE, PvsDeclarationDescriptor, PvsDeclarationType,
	PvsListDeclarationsRequest, ExpressionDescriptor, EvaluationResult, ProofResult, FormulaDescriptor,
	PvsTheoryListDescriptor, TccDescriptorArray
} from './common/serverInterface'
import * as language from "./common/languageKeywords";
import { PvsExecutionContext } from './common/pvsExecutionContextInterface';
import { PvsProcess } from './pvsProcess';
import { PvsCompletionProvider } from './providers/pvsCompletionProvider';
import { PvsDefinitionProvider } from './providers/pvsDefinitionProvider';
import { PvsHoverProvider } from './providers/pvsHoverProvider';
import { PvsCodeLensProvider } from './providers/pvsCodeLensProvider';
import { PvsLinter } from './providers/pvsLinter';
import { getErrorRange, findTheories, FileList, TheoryMap, TheoryList, TccList } from './common/languageUtils';
import * as fs from './common/fsUtils';
import * as path from 'path';
import { SSL_OP_SSLEAY_080_CLIENT_DH_BUG } from 'constants';

const HOVER_PROVIDER_DISABLED: boolean = true;

const SERVER_COMMANDS = [
	"pvs.init", // start pvs
	"pvs.language.regexp", // request pvs language regexp
	"pvs.version", // request pvs version
	"pvs.find-definition", // find definition command
	"pvs.list-declarations", // list declarations command
	"pvs.parse-file", // parse file command
	"pvs.parse-importchain", // parses a file and the importchain of the theories included in the file
	"pvs.exec-prover-command", // command for the interactive theorem prover
	"pvs.list-theories", // list theories in the current context
	"pvs.list-files", // list of the pvs files in the current context
	"pvs.show-tccs", // list of tccs
	"pvs.typecheck-file", // typechecks a pvs file
	"pvs.runit" // pvsio command
];

// Example server settings, this is not used at the moment
interface Settings {
	maxNumberOfProblems: number;
}

class PvsLanguageServer {
	private pvsProcess: PvsProcess; // pvs Lisp process
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
	/**
	 * @constructor
	 */
	constructor () {
		this.settings = {
			global: { maxNumberOfProblems: 1000 },
			documents: new Map()
		}		
		this.serverCapabilities = {
			hasConfigurationCapability: false,
			hasWorkspaceFolderCapability: false,
			hasDiagnosticRelatedInformationCapability: false
		}
		// Create a connection for the server. The connection uses Node's IPC as a transport. Includes all proposed LSP features.
		this.connection = createConnection(ProposedFeatures.all);
		this.setupConnectionManager();
		
		// Create a simple text document manager. The text document manager supports full document sync only
		this.documents = new TextDocuments();
		this.setupDocumentsManager(this.connection);

		// Listen on the connection
		this.connection.listen();
	}
	/**
	 * Internal function, install document managers
	 * @param connection 
	 */
	private setupDocumentsManager (connection: Connection) {
		// close the file only if it has been deleted from the context?
		// this.documents.onDidClose(e => { this.settings.documents.delete(e.document.uri); });

		// This event fires when the text document is first opened in the edirot or when then content of the opened document has changed.
		this.documents.onDidChangeContent(change => {
			if (this.pvsProcess) {
				// parse importchain
				this.pvsProcess.parseAll();
				// TODO: send diagnostics
			}
		});

		// This event fires when a document is saved
		this.documents.onDidSave(save => {
			if (this.pvsProcess) {
				this.provideDiagnostics(save.document).then((diagnostics: Diagnostic[]) => {
					diagnostics = diagnostics.concat(this.linter.provideDiagnostics(save.document))
					// Send the computed diagnostics to VS Code. This will trigger the visualization of red wavy lines under the error.
					connection.sendDiagnostics({ uri: save.document.uri, diagnostics });
				});
			}
		})

		// Listen to 'open', 'change' and 'close' document events
		this.documents.listen(connection);
	}
	/**
	 * @description Provides the regular expressions necessary for syntax highlighing
	 * @returns LanguageDescriptor<string>
	 */
	private getLanguageRegExp (): PvsSymbolKind<string> {
		return {
			keywords: language.PVS_RESERVED_WORDS_REGEXP_SOURCE,
			numbers: language.PVS_NUMBER_REGEXP_SOURCE,
			strings: language.PVS_STRING_REGEXP_SOURCE,
			constants: language.PVS_TRUE_FALSE_REGEXP_SOURCE,
			builtinTypes: language.PVS_BUILTIN_TYPE_REGEXP_SOURCE,
			operators: language.PVS_LANGUAGE_OPERATORS_REGEXP_SOURCE,
			functions: language.PVS_LIBRARY_FUNCTIONS_REGEXP_SOURCE,
			comments: language.PVS_COMMENT_REGEXP_SOURCE
		};
	}
	/**
	 * @description Initialises PVS, i.e., creates a pvs process, disables GC printout, enables emacs interface, changes context, and returns pvs version info
	 * @param pvsExecutionContext PVS path and PVS context path
	 * @returns Promise<PvsVersionDescriptor> PVS version information
	 */
	private async pvsInit (pvsExecutionContext: PvsExecutionContext): Promise<PvsVersionDescriptor> {
		this.pvsProcess = new PvsProcess(pvsExecutionContext, this.connection);
		await this.pvsProcess.pvs();
		await this.pvsProcess.disableGcPrintout();
		await this.pvsProcess.emacsInterface();
		await this.pvsProcess.changeContext(pvsExecutionContext.pvsContextPath);
		const ans: PvsResponseType = await this.pvsProcess.pvsVersionInformation();
		const versionInfo: PvsVersionDescriptor = {
			pvsVersion: ans.res.pvsVersion,
			lispVersion: ans.res.lispVersion
		}

		// Create service providers
		this.definitionProvider = new PvsDefinitionProvider(this.pvsProcess);
		this.completionProvider = new PvsCompletionProvider(this.definitionProvider);
		this.codeLensProvider = new PvsCodeLensProvider(this.definitionProvider);
		if (!HOVER_PROVIDER_DISABLED) {
			this.hoverProvider = new PvsHoverProvider(this.definitionProvider);
		}
		this.linter = new PvsLinter();

		// parse all files in the current context
		this.pvsProcess.parseAll();

		// return version info to the caller
		return versionInfo;
	}

	private async provideDiagnostics(document: TextDocument): Promise<Diagnostic[]> {
		let ans: PvsParserResponse = await this.pvsProcess.parseFile(document.uri);
		if (ans && ans.error) {
			const errorPosition: Position = { line: ans.error.line - 1, character: ans.error.character };
			const errorRange: Range = getErrorRange(document.getText(), errorPosition);
			// const symbolName: string = vscode.window.activeTextEditor.document.getText(errorRange);
			// const msg: string = 'Syntax error at symbol **' + symbolName + '**';
			const diag: Diagnostic = {
				severity: DiagnosticSeverity.Error,
				range: errorRange,
				message: ans.error.msg,
				source: "Syntax error"
			};
			return [ diag ];
		}
		return [];
	}

	private async listPvsFiles (pvsContextPath: string): Promise<FileList> {
		const children: string[] = await fs.readDir(pvsContextPath);
		const fileList: FileList = {
			pvsContextPath: pvsContextPath,
			fileNames: children.filter(function (fileName) {
				return fileName.endsWith(".pvs");
			})
		};
		return fileList;
	}
	private async listTheories (uri: string): Promise<TheoryMap> {
		let response: TheoryMap = {};
		const doc: TextDocument = this.documents.get("file://" + uri);
		const fileName: string = getFilename(uri, { removeFileExtension: true });
		if (doc) {
			response = findTheories(fileName, doc.getText());
		}
		return response;
	}

	private setupConnectionManager () {
		this.connection.onInitialize((params: InitializeParams) => {
			let capabilities = params.capabilities;
			// Does the client support the `workspace/configuration` request?
			// If not, we will fall back using global settings
			this.serverCapabilities.hasConfigurationCapability = !!(capabilities.workspace && !!capabilities.workspace.configuration);
			this.serverCapabilities.hasWorkspaceFolderCapability = !!(capabilities.workspace && !!capabilities.workspace.workspaceFolders);
			this.serverCapabilities.hasDiagnosticRelatedInformationCapability =
				!!(capabilities.textDocument &&
				capabilities.textDocument.publishDiagnostics &&
				capabilities.textDocument.publishDiagnostics.relatedInformation);
		
			return {
				capabilities: {
					textDocumentSync: this.documents.syncKind,
					// The completion provider returns a list of completion items to the editor.
					completionProvider: {
						resolveProvider: true, // code completion
						triggerCharacters: [ '`', '#' ] //  ` and # are for records
					},
					// Hover provider is disabled in the server, so vscode can use the client-side implementation that includes a link in the hover
					hoverProvider: !HOVER_PROVIDER_DISABLED,
					// CodeLens provider returns commands that can be shown inline along with the specification, e.g,. to animate an expression or prove a theorem
					codeLensProvider: {
						resolveProvider: true
					},
					executeCommandProvider: {
						commands: SERVER_COMMANDS
					}
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
		this.connection.onDidChangeConfiguration(change => {
			if (this.serverCapabilities.hasConfigurationCapability) {
				this.settings.documents.clear();
			} else {
				this.settings.global = <Settings>((change.settings.languageServerExample || this.settings.global));
			}
		});				
		this.connection.onInitialized(() => {			
			if (this.serverCapabilities.hasConfigurationCapability) {
				this.connection.client.register(DidChangeConfigurationNotification.type, undefined);
			}
			if (this.serverCapabilities.hasWorkspaceFolderCapability) {
				this.connection.workspace.onDidChangeWorkspaceFolders(evt => {
					this.connection.console.info('Workspace folder change event received.');
				});
			}
			// TODO: perhaps PVS should be initialised automatically, when the client connects?
			this.connection.onRequest('pvs.init', async (pvsExecutionContext: PvsExecutionContext) => {
				this.connection.sendRequest('server.status.update', "Initialising PVS...");
				try {
					const versionInfo: PvsVersionDescriptor = await this.pvsInit(pvsExecutionContext);
					this.readyString = versionInfo.pvsVersion + " " + versionInfo.lispVersion ;
					this.connection.sendRequest('server.response.pvs.init', this.readyString);
				} catch (err) {
					this.connection.sendRequest('server.status.update', err);
				}
			});
			this.connection.onRequest('pvs.language.regexp', () => {
				const desc: PvsSymbolKind<string>  = this.getLanguageRegExp();
				this.connection.sendRequest("server.response.language.regexp", desc);
			});
			this.connection.onRequest('pvs.change-context', async (contextPath) => {
				const msg = await this.pvsProcess.changeContext(contextPath);
				this.connection.sendRequest("server.response.change-context", msg);
			});
			this.connection.onRequest('pvs.version', async () => {
				const version = await this.pvsProcess.pvsVersionInformation();
				this.connection.sendRequest("server.response.pvs.version", version);
			});
			this.connection.onRequest('pvs.parse-file', async (uri: string) => {
				this.connection.sendRequest('server.status.update', "Parsing " + uri);
				const response: PvsParserResponse = await this.pvsProcess.parseFile(uri);
				this.connection.sendRequest("server.response.parse-file", response);
				this.connection.sendRequest('server.status.update', this.readyString);
			});
			this.connection.onRequest('pvs.typecheck-file', async (cmd: { fileName: string, tcpFlag?: boolean }) => {
				this.connection.sendRequest('server.status.update', "Typechecking " + cmd.fileName);
				const response: PvsParserResponse = await this.pvsProcess.typecheckFile(cmd.fileName, cmd.tcpFlag);
				this.connection.sendRequest("server.response.typecheck-file", response);
				this.connection.sendRequest('server.status.update', this.readyString);
			});
			this.connection.onRequest('pvs.parse-importchain', async (uri: string) => {
				this.connection.sendRequest('server.status.update', "Parsing import-chain...");
				const response: { [fileName: string]: PvsParserResponse } = await this.pvsProcess.parseAll();
				this.connection.sendRequest("server.response.parse-importchain", response);
				this.connection.sendRequest('server.status.update', this.readyString);
			});
			this.connection.onRequest('pvs.continue-proof', async (cmd: string) => {
				this.pvsProcess.continueProof(cmd);
			});
			this.connection.onRequest("pvs.find-definition", async (desc: PvsFindDeclarationRequest) => {
				const document: TextDocument = this.documents.get("file://" + desc.file);
				const response: PvsFindDeclarationResponse = await this.definitionProvider.findSymbolDefinition(document, desc.symbolName, { line: desc.line || 0, character: desc.character || 0 });//await this.pvsProcess.findDefinition(desc);
				this.connection.sendRequest("server.response.find-definition", response);
			});
			this.connection.onRequest("pvs.list-declarations", async (desc: PvsListDeclarationsRequest) => {
				const response: PvsDeclarationDescriptor[] = await this.pvsProcess.listDeclarations(desc);
				this.connection.sendRequest("server.response.list-declarations", response);
			});
			// TODO: add context folder as parameter
			this.connection.onRequest("pvs.list-files", async () => {
				const pvsContextPath: string = this.pvsProcess.getContextPath();
				const response: FileList = await this.listPvsFiles(pvsContextPath);
				this.connection.sendRequest("server.response.list-files", response);
			});
			this.connection.onRequest("pvs.list-theories", async (uri: string) => {
				// this.connection.console.info("list-theories, file " + uri);
				let response: TheoryMap = await this.listTheories(uri);
				this.connection.sendRequest("server.response.list-theories", response);
			});
			// this.connection.onRequest("pvs.list-all-theories-and-declarations", async (uri: string) => {
			// 	const pvsContextPath: string = this.pvsProcess.getContextPath();
			// 	let response: TheoryList = {
			// 		pvsContextPath: pvsContextPath,
			// 		theories: {},
			// 		declarations: {}
			// 	};
			// 	const fileList: FileList = await this.listPvsFiles(pvsContextPath);
			// 	for (let i in fileList.fileNames) {
			// 		const fileName: string = fileList.fileNames[i];
			// 		const uri: string = path.join(pvsContextPath, fileName);
			// 		const theories: TheoryMap = await this.listTheories(uri);
			// 		let theoryNames: string[] = Object.keys(theories);
			// 		for (let i in theoryNames) {
			// 			let theoryName: string = theoryNames[i];
			// 			const declarations: PvsDeclarationDescriptor[] = await this.pvsProcess.listDeclarations({
			// 				file: fileName,
			// 				theoryName: theoryName
			// 			});
			// 			response.theories[theoryName] = theories[theoryName];
			// 			// Object.keys(declarations).forEach((key) => {
			// 			// // 	response.declarations[theoryName][key] = {
			// 			// // 		theoryName: theoryName,
			// 			// // 		symbolName: declarations[key].symbolName,
			// 			// // 		symbolDeclaration: declarations[key].symbolDeclaration,
			// 			// // 		symbolDeclarationRange: declarations[key].symbolDeclarationRange,
			// 			// // 		symbolDeclarationFile: declarations[key].symbolDeclarationFile,
			// 			// // 		symbolDoc: declarations[key].symbolDoc,
			// 			// // 		comment: declarations[key].comment
			// 			// // 	};
			// 			// });
			// 		}
			// 	}
			// 	this.connection.sendRequest("server.response.list-all-theories", response);
			// });
			this.connection.onRequest("pvs.list-all-theories", async (uri: string) => {
				// this.connection.console.info("list-all-theories, file " + uri);
				const pvsContextPath: string = this.pvsProcess.getContextPath();
				let response: TheoryList = {
					pvsContextPath: pvsContextPath,
					theories: {},
					declarations: {}
				};
				const fileList: FileList = await this.listPvsFiles(pvsContextPath);
				for (let i in fileList.fileNames) {
					let uri: string = path.join(pvsContextPath, fileList.fileNames[i]);
					let theories: TheoryMap = await this.listTheories(uri);
					let theoryNames: string[] = Object.keys(theories);
					for (let i in theoryNames) {
						// TODO: check if theory names in the same context are unique
						let theoryName: string = theoryNames[i];
						response.theories[theoryName] = theories[theoryName];
						// const declarations: PvsDeclarationDescriptor[] = await this.pvsProcess.listDeclarations({ theoryName: theoryName });
						// Object.keys(declarations).forEach((key) => {
						// // 	response.declarations[theoryName][key] = {
						// // 		theoryName: theoryName,
						// // 		symbolName: declarations[key].symbolName,
						// // 		symbolDeclaration: declarations[key].symbolDeclaration,
						// // 		symbolDeclarationRange: declarations[key].symbolDeclarationRange,
						// // 		symbolDeclarationFile: declarations[key].symbolDeclarationFile,
						// // 		symbolDoc: declarations[key].symbolDoc,
						// // 		comment: declarations[key].comment
						// // 	};
						// });
					}
				}
				this.connection.sendRequest("server.response.list-all-theories", response);
			});
			this.connection.onRequest("pvs.runit", async (desc: ExpressionDescriptor) => {
				this.connection.console.log("received command runit");
				const response: EvaluationResult = await this.pvsProcess.runit(desc);
				this.connection.sendRequest("server.response.runit", response);
			});
			this.connection.onRequest("pvs.proveit", async (desc: FormulaDescriptor) => {
				this.connection.console.log("received command proveit");
				const response: EvaluationResult = await this.pvsProcess.proveit(desc);
				this.connection.sendRequest("server.response.proveit", response);
			});
			this.connection.onRequest("pvs.show-tccs", async (cmd: { theoryName: string //, fileName: string 
			}) => {
				this.connection.sendRequest('server.status.update', "Typechecking " + cmd.theoryName);
				const ans: TccDescriptorArray = await this.pvsProcess.showTccs(cmd.theoryName);
				const pvsContextPath: string = this.pvsProcess.getContextPath();
				let tccs = {};
				tccs[cmd.theoryName] = {
					theoryName: ans.theoryName,
					fileName: ans.fileName,
					tccs: ans.tccs
				};
				const response: TccList = {
					pvsContextPath: pvsContextPath,
					tccs: tccs
				};
				this.connection.sendRequest("server.response.show-tccs", response);
				this.connection.sendRequest('server.status.update', this.readyString);
			})
		});

		// register service providers
		this.connection.onCompletion(async (tpp: TextDocumentPositionParams): Promise<CompletionItem[]> => {
			if (tpp.textDocument.uri.endsWith(".pvs") && this.completionProvider) {
				const document: TextDocument = this.documents.get(tpp.textDocument.uri);
				let completionItems: CompletionItem[] = await this.completionProvider.provideCompletionItems(document, tpp.position);
				return completionItems;
			}
			return null;
		});
		this.connection.onHover(async (tpp: TextDocumentPositionParams): Promise<Hover> => {
			if (tpp.textDocument.uri.endsWith(".pvs") && this.hoverProvider) {
				const document: TextDocument = this.documents.get(tpp.textDocument.uri);
				let hover: Hover = await this.hoverProvider.provideHover(document, tpp.position);
				return hover;
			}
			return null;
		});
		this.connection.onCodeLens(async (tpp: CodeLensParams): Promise<CodeLens[]> => {
			if (tpp.textDocument.uri.endsWith(".pvs") && this.codeLensProvider) {
				const document: TextDocument = this.documents.get(tpp.textDocument.uri);
				let codelens: CodeLens[] = await this.codeLensProvider.provideCodeLens(document);
				return codelens;
			}
			return null;
		});
		// this.connection.onDefinition(async function (tpp: TextDocumentPositionParams): Promise<Definition> {
		// 	if (tpp.textDocument.uri.endsWith(".pvs") && _this.definitionProvider) {
		// 		const document: TextDocument = _this.documents.get(tpp.textDocument.uri);
		// 		let ans: PvsFindDeclarationResponse = await _this.definitionProvider.provideDefinition(document, tpp.position);
		// 		let definition: Definition = {
		// 			uri: ans.symbolDeclarationFile,
		// 			range: ans.symbolDeclarationRange
		// 		}
		// 		return definition;
		// 	}
		// 	return null;
		// });
	}
}

// instantiate the language server
new PvsLanguageServer();
