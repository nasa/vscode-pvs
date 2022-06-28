/**
 * @module PvsLanguageClient
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
import * as path from 'path';
import { 
	TextDocument, window, workspace, ExtensionContext, TextEditor, TextDocumentChangeEvent, 
	commands, ConfigurationChangeEvent, Uri, FileRenameEvent, WindowState
} from 'vscode';
import { LanguageClient, LanguageClientOptions, TransportKind, ServerOptions, CancellationToken } from 'vscode-languageclient';
import { VSCodePvsDecorationProvider } from './providers/vscodePvsDecorationProvider';
import { VSCodePvsWorkspaceExplorer } from './views/vscodePvsWorkspaceExplorer';
import { VSCodePvsEmacsBindingsProvider } from './providers/vscodePvsEmacsBindingsProvider';
import { VSCodePvsProofExplorer } from './views/vscodePvsProofExplorer';
import * as fsUtils from './common/fsUtils';
import { VSCodePvsStatusBar } from './views/vscodePvsStatusBar';
import { EventsDispatcher } from './eventsDispatcher';
import { PvsFile, PvsVersionDescriptor, serverEvent, serverRequest } from "./common/serverInterface";
import * as vscodeUtils from './utils/vscode-utils';
import { VSCodePvsPackageManager } from './providers/vscodePvsPackageManager';
import { VSCodePvsProofMate } from './views/vscodePvsProofMate';
import { VSCodePvsFileOutlineProvider } from './providers/vscodsPvsOulineProvider';
import { VSCodePvsSnippetsProvider } from './providers/vscodePvsSnippetsProvider';
import { VSCodePvsLogger } from './views/vscodePvsLogger';
import { VSCodePvsPlotter } from './views/vscodePvsPlotter';
import { VSCodePvsSearch } from './views/vscodePvsSearch';
import { VSCodePvsioWeb } from './views/vscodePvsioWeb';
import { VSCodePvsXTerm } from './views/vscodePvsXTerm';
import { XTermColorTheme } from './common/colorUtils';
import { getActivePvsEditor } from './utils/vscode-utils';
import { VSCodePvsFileViewer } from './views/vscodePvsFileViewer';

const server_path: string = path.join('server', 'out', 'pvsLanguageServer.js');
const AUTOSAVE_INTERVAL: number = 10000; //ms Note: small autosave intervals (e.g., 1sec) create an unwanted scroll effect in the editor (the current line is scrolled to the top)

export class PvsLanguageClient { //implements vscode.Disposable {
	// language client
	protected client: LanguageClient;
	protected pvsPath: string;
	protected pvsLibraryPath: string; // comma-separated list of folders
	protected pvsServerReady: boolean = false;

	// context variables
	protected context: ExtensionContext;

	// timers, for periodic events such as autosave
	protected timers: {[key: string]: NodeJS.Timer } = {};

	// providers for text editor
	protected decorationProvider: VSCodePvsDecorationProvider;
	protected emacsBindingsProvider: VSCodePvsEmacsBindingsProvider;

	// providers for explorer
	protected workspaceExplorer: VSCodePvsWorkspaceExplorer;
	protected proofExplorer: VSCodePvsProofExplorer;

	// outline provider
	protected outlineProvider: VSCodePvsFileOutlineProvider;

	// snippets provider
	protected snippetsProvider: VSCodePvsSnippetsProvider;

	// integrated terminal panel and terminal link provider
	// protected vscodePvsTerminal: VSCodePvsTerminal;
	// protected terminalLinkProvider: VSCodePvsTerminalLinkProvider
	protected xterm: VSCodePvsXTerm;

	// status bar
	protected statusBar: VSCodePvsStatusBar;

	// proofmate
	protected proofMate: VSCodePvsProofMate;

	// events dispatcher
	protected eventsDispatcher: EventsDispatcher;

	// package manager
	protected packageManager: VSCodePvsPackageManager;

	// logger
	protected logger: VSCodePvsLogger;

	// plotter
	protected plotter: VSCodePvsPlotter;

	// search
	protected search: VSCodePvsSearch;

	// file viewer
	protected fileViewer: VSCodePvsFileViewer;

	// pvsioweb
	protected pvsioweb: VSCodePvsioWeb;

	/**
	 * Internal function, returns the current pvs path, as indicated in the configuration file
	 */
	protected getPvsPath (): string {
		return vscodeUtils.getConfiguration("pvs.path");
	}

	/**
	 * Internal function, autosaves pvs files with frequency AUTOSAVE_INTERVAL
	 * @param document 
	 */
	protected autosave (document: TextDocument) {
		// cancel any previously scheduled save 
		if (this.timers['autosave']) {
			clearTimeout(this.timers['autosave']);
			this.timers['autosave'] = null;
		}
		// save document after a delay
		this.timers['autosave'] = setTimeout(async () => {
			if (document.isDirty) {
				await document.save();
			}
		}, AUTOSAVE_INTERVAL);
	}

	/**
	 * Internal function, defines handlers for document events
	 */
	protected registerTextEditorHandlers () {
		
		// onDidOpenTextDocument is emitted when a text file is opened in the editor or when the language id of a text document has changed.
		workspace.onDidOpenTextDocument(async (event: TextDocument) => {
			const editor: TextEditor = getActivePvsEditor();
			if (event?.languageId === "pvs" || fsUtils.isPvsFile(editor?.document?.fileName)) {
				commands.executeCommand('setContext', 'pvs-server-active', true);
				// show status bar
				this.statusBar.show();
				// check if this is a session start and there's a file that needs to be opened
				const fname: string = event?.fileName;
				if (fsUtils.isPvsFile(fname) && fname === editor?.document?.fileName) {
					vscodeUtils.loadPvsFileIcons();
					const contextFolder: string = fsUtils.getContextFolder(fname);
					const explorerWorkspace: string = this.workspaceExplorer.getCurrentWorkspace();
					if (contextFolder !== explorerWorkspace) {
						this.client.sendRequest(serverRequest.getContextDescriptor, { contextFolder });
						// don't update file explorer, as any modification will create an Untitled workspace, which might be problematic for vscode-pvs users
						// because users will be asked to save the workspace on exit, and if they choose to save the workspace, 
						// they will also be asked whether they want to open the workspace configuration next time they will work on that folder
						// const contextFolderUri: Uri = Uri.file(contextFolder);
						// if (!workspace.getWorkspaceFolder(contextFolderUri)) {
						// 	// add the folder to file explorer
						// 	// commands.executeCommand('vscode.openFolder', Uri.file(contextFolder), { forceReuseWindow: true });
						// 	// const nOpenFolders: number = workspace?.workspaceFolders?.length || 0;
						// 	await vscodeUtils.updateWorkspaceFolders(0, 0, { uri: Uri.file(contextFolder) });
						// }
					}
				}
				// don't highlight the active file in the explorer -- doing so will put the focus on the file explorer
				// and the user might be looking at the pvs workspace explorer instead, which would become hidden
				// commands.executeCommand("workbench.files.action.showActiveFileInExplorer");
			} 
			// the following is commented out because workspace.findFiles can take a lot of CPU
			// else {
			// 	const pvsFiles: Uri[] = await workspace.findFiles("**/*.pvs", null, 1);
			// 	if (!pvsFiles.length) {
			// 		vscodeUtils.unloadPvsFileIcons();
			// 	}
			// }
		});

		// onDidChangeActiveTextEditor is emitted when the active editor focuses on a new document
		window.onDidChangeActiveTextEditor(async (event: TextEditor) => {
			const activeEditor: TextEditor = getActivePvsEditor();//window.activeTextEditor; //event || window.activeTextEditor;
			const fname: string =  activeEditor?.document?.fileName;
			const state: WindowState = window.state;
			if (event?.document?.languageId === "pvs" || fsUtils.isPvsFile(activeEditor?.document?.fileName)) {
				commands.executeCommand('setContext', 'pvs-server-active', true);
				// show status bar
				this.statusBar.show();
				// update decorations
				this.decorationProvider.updateDecorations(activeEditor);
				// trigger file parsing to get syntax diagnostics
				const contextFolder: string = fsUtils.getContextFolder(fname);
				// update workspace-explorer if needed
				const explorerWorkspace: string = this.workspaceExplorer.getCurrentWorkspace();
				if (contextFolder !== explorerWorkspace) {
					this.client.sendRequest(serverRequest.getContextDescriptor, { contextFolder });
					// don't update file explorer, see comments in onDidOpenTextDocument
					// const contextFolderUri: Uri = Uri.file(contextFolder);
					// if (!workspace.getWorkspaceFolder(contextFolderUri)) {
					// 	// add the folder to file explorer
					// 	// commands.executeCommand('vscode.openFolder', Uri.file(contextFolder), { forceReuseWindow: true });
					// 	// const nOpenFolders: number = workspace?.workspaceFolders?.length || 0;
					// 	await vscodeUtils.updateWorkspaceFolders(0, 0, { uri: Uri.file(contextFolder) });
					// }
				}
			} else {
				// hide status bar
				this.statusBar.hide();
			}
		}, null, this.context.subscriptions);

		// onDidChangeTextDocument is emitted when the content of a document changes
		workspace.onDidChangeTextDocument((event: TextDocumentChangeEvent) => {
			if (fsUtils.isPvsFile(event?.document?.fileName)) {
				const activeEditor: TextEditor = getActivePvsEditor();
				if (activeEditor) {
					this.decorationProvider.updateDecorations(activeEditor);
				}
				// autosave will trigger parsing, which in turn triggers diagnostics
				this.autosave(event.document);
			}
		}, null, this.context.subscriptions);

		// onDidChangeConfiguration is emitted when the configuration file changes
		workspace.onDidChangeConfiguration((event: ConfigurationChangeEvent) => {
			// get color theme
			const colorTheme: XTermColorTheme = vscodeUtils.detectColorTheme();
			this.xterm.updateColorTheme(colorTheme);

			// get brackets colorization flag
			const flag: boolean = vscodeUtils.getConfigurationFlag("pvs.settings.proverConsole.colorizeBracketPairs");
			this.xterm.enableBracketColors(flag);

			// re-initialise pvs if the executable is different
			const pvsPath: string = vscodeUtils.getConfiguration("pvs.path").trim();
			const pvsLibraryPath: string = vscodeUtils.getPvsLibraryPath();
			if (pvsPath !== this.pvsPath || this.pvsLibraryPath !== pvsLibraryPath) {
				this.pvsPath = pvsPath || this.pvsPath;
				this.pvsLibraryPath = pvsLibraryPath;
				if (this.pvsPath) {
					const msg: string = `Restarting PVS from ${this.pvsPath}`;
					this.statusBar.showProgress(msg);
					// window.showInformationMessage(msg);
					this.client.sendRequest(serverRequest.startPvsServer, {
						pvsPath: this.pvsPath, 
						pvsLibraryPath: this.pvsLibraryPath
					}); // the server will use the last context folder it was using	
				}
			}

			// update xterm-pvs settings, if xterm is active
			if (this.xterm.isActive()) {
				const flag: boolean = vscodeUtils.getConfigurationFlag("pvs.settings.proverConsole.autocompleteWithEnter");
				this.xterm.autocompleteWithEnter(flag);
			}
		}, null, this.context.subscriptions);

		// onDidRenameFiles is emitted when a file is renamed
		workspace.onDidRenameFiles(async (event: FileRenameEvent) => {
			const pvsFiles: { oldUri: Uri, newUri: Uri }[] = event?.files?.filter((value: { oldUri: Uri, newUri: Uri }) => {
				return value?.oldUri?.path?.endsWith(".pvs");
			}) || [];
			const activeEditor: TextEditor = getActivePvsEditor();
			if (pvsFiles.length > 0
				|| (activeEditor && 
						(fsUtils.isPvsFile(activeEditor?.document?.fileName)
							|| activeEditor?.document?.languageId === "Log"))) {
				// send clearWorkspace command to the server, otherwise the server will erroneously report a typecheck error because it may have cached the theory name from the old file
				this.client.sendRequest(serverRequest.clearWorkspace);
				// remove tccs file for the renamed file, if the file exists
                if (workspace?.workspaceFolders?.length) {
					for (let i in pvsFiles) {
						const tccFile: Uri = Uri.file(pvsFiles[i].oldUri.path.replace(".pvs", ".tccs"));
						console.log(`[pvs-client] Removing file ${tccFile}`);
						workspace.fs.delete(tccFile);
					}
				}
			}
		});
	}

	/**
	 * Internal function, creates the language client necessary for communicating with the language server
	 */
	protected createLanguageClient (context: ExtensionContext): void {
		// save pointer to extension context
		this.context = context;

		// The server is implemented in NodeJS
		const serverModule = this.context.asAbsolutePath(server_path);
		// If the extension is launched in debug mode then the debug server options are used
		// Otherwise the run options are used
		const serverOptions: ServerOptions = {
			run:  { module: serverModule, transport: TransportKind.ipc },
			debug: {
				module: serverModule,
				transport: TransportKind.ipc,
				options: { execArgv: ['--nolazy', '--inspect=6009'] } // --inspect=6009: runs the server in Node's Inspector mode so VS Code can attach to the server for debugging
			}
		};
		// Options to control the language client
		const clientOptions: LanguageClientOptions = {
			// Register the server for pvs files
			documentSelector: [{ scheme: 'file', language: 'pvs' }],
			synchronize: {
				// Notify the server about file changes to '.clientrc files contained in the workspace
				fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
			}
		};
		// Create the language client and start the client.
		this.client = new LanguageClient(
			'PvsLanguageServer',
			'pvs-server',
			serverOptions,
			clientOptions
		);
	}
	/**
	 * Client activation function.
	 * @param context 
	 */
	activate (context: ExtensionContext): void {
		// create language client
		this.createLanguageClient(context);
		
		// create status bar
		this.statusBar = new VSCodePvsStatusBar(this.client);
		this.statusBar.activate(this.context);
		this.statusBar.showProgress("Starting vscode-pvs...");
		
		// start client, which in turn will also start the server
		this.client.start();
		this.client.onReady().then(() => {
			// select pvs-server output channel (there's no such API, a workaround is to show and then hide the channel)
			this.client.outputChannel.show(true);
			this.client.outputChannel.hide();

			// initialise service providers defined on the client-side
			this.statusBar.showProgress("Starting vscode-pvs...");

			// start vscode-pvs components
			this.proofExplorer = new VSCodePvsProofExplorer(this.client, 'proof-explorer-view');
			this.proofExplorer.activate(this.context);
			this.workspaceExplorer = new VSCodePvsWorkspaceExplorer(this.client, this.proofExplorer, 'workspace-explorer-view');
			this.workspaceExplorer.activate(this.context);
			this.emacsBindingsProvider = new VSCodePvsEmacsBindingsProvider(this.client, this.statusBar);
			this.emacsBindingsProvider.activate(this.context);
			this.outlineProvider = new VSCodePvsFileOutlineProvider(this.client);
			this.outlineProvider.activate(this.context);
			this.snippetsProvider = new VSCodePvsSnippetsProvider(this.client);
			this.snippetsProvider.activate(this.context);

			this.xterm = new VSCodePvsXTerm(this.client, this.proofExplorer);
			this.xterm.activate(this.context);

			this.proofMate = new VSCodePvsProofMate(this.client, this.xterm, 'proof-mate-view');
			this.proofMate.activate(this.context);
			this.packageManager = new VSCodePvsPackageManager(this.client, this.statusBar);
			this.packageManager.activate(this.context);
			this.logger = new VSCodePvsLogger();
			this.logger.activate(this.context);
			this.plotter = new VSCodePvsPlotter(this.client);
			this.plotter.activate(this.context);
			this.search = new VSCodePvsSearch(this.client);
			this.search.activate(this.context);
			this.fileViewer = new VSCodePvsFileViewer(this.client);
			this.fileViewer.activate(this.context);
			
			this.pvsioweb = new VSCodePvsioWeb(this.client);
			this.pvsioweb.activate(this.context);
	
			// enable decorations for pvs syntax
			this.decorationProvider = new VSCodePvsDecorationProvider();
			this.decorationProvider.updateDecorations(getActivePvsEditor());
	
			// register handlers for document events
			this.registerTextEditorHandlers();
			
			// create event dispatcher for handling events for views
			this.eventsDispatcher = new EventsDispatcher(this.client, {
				statusBar: this.statusBar,
				emacsBindings: this.emacsBindingsProvider,
				workspaceExplorer: this.workspaceExplorer,
				proofExplorer: this.proofExplorer,

				// vscodePvsTerminal: this.vscodePvsTerminal,
				xterm: this.xterm,

				proofMate: this.proofMate,
				logger: this.logger,
				packageManager: this.packageManager,
				plotter: this.plotter,
				search: this.search,
				pvsioweb: this.pvsioweb,
				fileViewer: this.fileViewer
			});
			this.eventsDispatcher.activate(context);
			
			// start PVS
			// the server will respond with one of the following events: pvsServerReady, pvsNotPresent, pvsIncorrectVersion
			const contextFolder = vscodeUtils.getEditorContextFolder();
			// console.log(`Context folder: ${contextFolder}`);
			this.pvsPath = vscodeUtils.getConfiguration("pvs.path");
			this.pvsLibraryPath = vscodeUtils.getPvsLibraryPath();
			// setTimeout(() => {
			this.client.sendRequest(serverRequest.startPvsServer, {
				pvsPath: this.pvsPath,
				pvsLibraryPath: this.pvsLibraryPath,
				contextFolder,
				externalServer: false
			});
			// }, 1000);
			// set vscode context variable pvs-server-active to true -- this will create the PVS icon on the activity bar
			commands.executeCommand('setContext', 'pvs-server-active', true);
			// create handler for pvsServerReady event
			this.client.onRequest(serverEvent.pvsServerReady, (info: PvsVersionDescriptor) => {
				// reset other globals
				vscodeUtils.resetGlobals();

				// make toolbars (aka header actions) always visible
				vscodeUtils.setToolbarVisibility(true);

				// declutter vscode
				vscodeUtils.declutterVscode();

				// update status bar
				this.statusBar.ready();

				const activeEditor: TextEditor = getActivePvsEditor();
				if (activeEditor?.document) {
					// parse file opened in the editor
					const desc: PvsFile = fsUtils.fname2desc(activeEditor.document?.fileName);
					if (desc.contextFolder) {
						this.client.sendRequest(serverRequest.parseFile, desc);
						this.client.sendRequest(serverRequest.getContextDescriptor, { contextFolder: desc.contextFolder });
					}
				} else {
					// or get the descriptor of the current folder
					const workspaceFolder: Uri = (workspace.workspaceFolders && workspace.workspaceFolders.length) ? workspace.workspaceFolders[0].uri : null;
					const folder: string = (workspaceFolder) ? workspaceFolder.path : 
						contextFolder ? contextFolder : vscodeUtils.getDefaultContextFolder();
					if (folder) {
						this.client.sendRequest(serverRequest.getContextDescriptor, { contextFolder: folder });
					}
				}
			});
		});
	}
	/**
	 * Client stop function
	 */
	async stop (): Promise<void> {
		if (this.client) {
			this.client.sendRequest(serverRequest.stopPvsServer);
			await this.client.stop();
			// set vscode context variable pvs-server-active to true
			commands.executeCommand('setContext', 'pvs-server-active', false);
			vscodeUtils.resetGlobals();
		}
	}
}

// client instance
const pvsLanguageClient = new PvsLanguageClient();

/**
 * Function activate is invoked every time a new IDE session is started. This includes:
 *  - Opening a new folder in the IDE browser
 *  - ...
 * @param context 
 */
export function activate(context: ExtensionContext) {
	// Activate the client.
	pvsLanguageClient.activate(context); // async call
}

export function deactivate(): Thenable<void> {
	return new Promise((resolve, reject) => {
		pvsLanguageClient.stop().then(() => {
			resolve();
		});
	});
}
