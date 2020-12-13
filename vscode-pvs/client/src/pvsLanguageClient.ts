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
import * as comm from './common/serverInterface';
import { TextDocument, window, workspace, ExtensionContext, TextEditor, TextDocumentChangeEvent, commands, ConfigurationChangeEvent, ProgressLocation, Uri, WorkspaceConfiguration, Progress, FileRenameEvent, WorkspaceFolder } from 'vscode';
import { LanguageClient, LanguageClientOptions, TransportKind, ServerOptions, CancellationToken } from 'vscode-languageclient';
import { VSCodePvsDecorationProvider } from './providers/vscodePvsDecorationProvider';
import { VSCodePvsWorkspaceExplorer } from './views/vscodePvsWorkspaceExplorer';
import { VSCodePvsEmacsBindingsProvider } from './providers/vscodePvsEmacsBindingsProvider';
import { VSCodePvsTerminal } from './views/vscodePvsTerminal';
import { VSCodePvsProofExplorer } from './views/vscodePvsProofExplorer';
import * as fsUtils from './common/fsUtils';
import { VSCodePvsStatusBar } from './views/vscodePvsStatusBar';
import { EventsDispatcher } from './eventsDispatcher';
import { serverEvent } from "./common/serverInterface";
import * as vscodeUtils from './utils/vscode-utils';
import { VSCodePvsPackageManager } from './providers/vscodePvsPackageManager';
import { VSCodePvsProofMate } from './views/vscodePvsProofMate';
import { VSCodePvsFileOutlineProvider } from './providers/vscodsPvsOulineProvider';
import { VSCodePvsSnippetsProvider } from './providers/vscodePvsSnippetsProvider';
import { VSCodePvsLogger } from './views/vscodePvsLogger';
import { VSCodePvsPlotter } from './views/vscodePvsPlotter';

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

	// integrated command line interfaces
	protected vscodePvsTerminal: VSCodePvsTerminal;

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
		/**
		 * onDidOpenTextDocument is emitted when a [text document](#TextDocument) is opened or when the language id
		 * of a text document [has been changed](#languages.setTextDocumentLanguage).
		*/
		workspace.onDidOpenTextDocument((event: TextDocument) => {
			if ((event && event.languageId === "pvs") 
				|| (window.activeTextEditor && 
						(fsUtils.isPvsFile(window.activeTextEditor.document?.fileName)
							|| window.activeTextEditor.document?.languageId === "Log"))) {
				commands.executeCommand('setContext', 'pvs-server-active', true);
				// show status bar
				this.statusBar.show();
			}
		});

		workspace.onDidRenameFiles(async (event: FileRenameEvent) => {
			const pvsFiles: { oldUri: Uri, newUri: Uri }[] = event?.files?.filter((value: { oldUri: Uri, newUri: Uri }) => {
				return value?.oldUri?.path?.endsWith(".pvs");
			}) || [];
			if (pvsFiles.length > 0
				|| (window.activeTextEditor && 
						(fsUtils.isPvsFile(window.activeTextEditor.document?.fileName)
							|| window.activeTextEditor.document?.languageId === "Log"))) {
				// send clear theory command to the server, otherwise the server will erroneously report a typecheck error because it may have cached the theory name from the old file
				this.client.sendRequest(comm.serverRequest.clearTheories);
				// remove tccs file for the renamed file, if the file exists
				const folders: WorkspaceFolder[] = workspace?.workspaceFolders;
                if (folders && folders.length) {
					for (let i in pvsFiles) {
						const tccFile: Uri = Uri.file(pvsFiles[i].oldUri.path.replace(".pvs", ".tccs"));
						console.log(`[pvs-client] Removing file ${tccFile}`);
						workspace.fs.delete(tccFile);
					}
				}
			}
		});

		// onDidChangeActiveTextEditor is emitted when the active editor focuses on a new document
		window.onDidChangeActiveTextEditor((event: TextEditor) => {
			const editor: TextEditor = window.activeTextEditor; //event || window.activeTextEditor;
			if (editor && editor.document && (fsUtils.isPvsFile(editor.document.fileName) || editor.document.languageId === "Log")) {
				commands.executeCommand('setContext', 'pvs-server-active', true);
				// show status bar
				this.statusBar.show();
				// update decorations
				this.decorationProvider.updateDecorations(editor);
				// trigger file parsing to get syntax diagnostics
				const desc: comm.PvsFile = fsUtils.fname2desc(editor.document.fileName);
				this.client.sendRequest(comm.serverRequest.parseFile, desc);
			} else {
				// hide status bar
				this.statusBar.hide();
			}
		}, null, this.context.subscriptions);

		// onDidChangeTextDocument is emitted when the content of a document changes
		workspace.onDidChangeTextDocument((event: TextDocumentChangeEvent) => {
			if (fsUtils.isPvsFile(event?.document?.fileName)) {
				this.decorationProvider.updateDecorations(window.activeTextEditor);
				// autosave will trigger parsing, which in turn triggers diagnostics
				this.autosave(event.document);
			}
		}, null, this.context.subscriptions);

		// onDidChangeConfiguration is emitted when the configuration file changes
		workspace.onDidChangeConfiguration((event: ConfigurationChangeEvent) => {
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
					this.client.sendRequest(comm.serverRequest.startPvsServer, {
						pvsPath: this.pvsPath, 
						pvsLibraryPath: this.pvsLibraryPath
					}); // the server will use the last context folder it was using	
				}
			}
		}, null, this.context.subscriptions);
	}

	// event (eventName: string): void {
	// 	switch (eventName) {
	// 		case serverEvent.pvsServerReady: {
	// 			this.pvsServerReady = true;
	// 			break;
	// 		}
	// 		default: {
	// 			console.log(`[pvsLanguageClient] Warning: unhandled event ${eventName}`);
	// 		}
	// 	}
	// }

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
			// initialise service providers defined on the client-side
			this.statusBar.showProgress("Starting vscode-pvs...");

			// start vscode-pvs components
			this.emacsBindingsProvider = new VSCodePvsEmacsBindingsProvider(this.client, this.statusBar);
			this.emacsBindingsProvider.activate(this.context);
			this.proofExplorer = new VSCodePvsProofExplorer(this.client, 'proof-explorer-view');
			this.proofExplorer.activate(this.context);
			this.workspaceExplorer = new VSCodePvsWorkspaceExplorer(this.client, this.proofExplorer, 'workspace-explorer-view');
			this.workspaceExplorer.activate(this.context);
			this.outlineProvider = new VSCodePvsFileOutlineProvider(this.client);
			this.outlineProvider.activate(this.context);
			this.snippetsProvider = new VSCodePvsSnippetsProvider(this.client);
			this.snippetsProvider.activate(this.context);
			this.vscodePvsTerminal = new VSCodePvsTerminal(this.client);
			this.vscodePvsTerminal.activate(this.context);
			this.proofMate = new VSCodePvsProofMate(this.client, 'proof-mate-view');
			this.proofMate.activate(this.context);
			this.packageManager = new VSCodePvsPackageManager(this.client, this.statusBar);
			this.packageManager.activate(this.context);
			this.logger = new VSCodePvsLogger();
			this.logger.activate(this.context);
			this.plotter = new VSCodePvsPlotter(this.client);
			this.plotter.activate(this.context);
	
			// enable decorations for pvs syntax
			this.decorationProvider = new VSCodePvsDecorationProvider();
			this.decorationProvider.updateDecorations(window.activeTextEditor);
	
			// register handlers for document events
			this.registerTextEditorHandlers();
			
			// create event dispatcher for handling events for views
			this.eventsDispatcher = new EventsDispatcher(this.client, {
				statusBar: this.statusBar,
				emacsBindings: this.emacsBindingsProvider,
				workspaceExplorer: this.workspaceExplorer,
				proofExplorer: this.proofExplorer,
				vscodePvsTerminal: this.vscodePvsTerminal,
				proofMate: this.proofMate,
				logger: this.logger,
				packageManager: this.packageManager,
				plotter: this.plotter
			});
			this.eventsDispatcher.activate(context);
			
			// start PVS
			// the server will respond with one of the following events: pvsServerReady, pvsNotPresent, pvsIncorrectVersion
			const contextFolder = vscodeUtils.getEditorContextFolder();
			// console.log(`Context folder: ${contextFolder}`);
			this.pvsPath = vscodeUtils.getConfiguration("pvs.path");
			this.pvsLibraryPath = vscodeUtils.getPvsLibraryPath();
			// setTimeout(() => {
			this.client.sendRequest(comm.serverRequest.startPvsServer, {
				pvsPath: this.pvsPath,
				pvsLibraryPath: this.pvsLibraryPath,
				contextFolder
			});
			// }, 1000);
			// create handler for pvsServerReady event
			this.client.onRequest(serverEvent.pvsServerReady, (info: comm.PvsVersionDescriptor) => {
				// set vscode context variable pvs-server-active to true
				commands.executeCommand('setContext', 'pvs-server-active', true);

				// reset other globals
				commands.executeCommand('setContext', 'in-checker', false);
				commands.executeCommand('setContext', 'proof-explorer.running', false);
				commands.executeCommand('setContext', 'proof-mate.visible', false);
				commands.executeCommand('setContext', 'proof-explorer.visible', false);
				commands.executeCommand('setContext', 'autorun', false);

				// update status bar
				this.statusBar.ready();

				if (window.activeTextEditor && window.activeTextEditor.document) {
					// parse file opened in the editor
					const desc: comm.PvsFile = fsUtils.fname2desc(window.activeTextEditor?.document?.fileName);
					if (desc.contextFolder) {
						this.client.sendRequest(comm.serverRequest.parseFile, desc);
						this.client.sendRequest(comm.serverRequest.getContextDescriptor, { contextFolder: desc.contextFolder });
					}
				} else {
					// or get the descriptor of the current folder
					const workspaceFolder: Uri = (workspace.workspaceFolders && workspace.workspaceFolders.length) ? workspace.workspaceFolders[0].uri : null;
					const folder: string = (workspaceFolder) ? workspaceFolder.path : 
							contextFolder ? contextFolder : vscodeUtils.getDefaultContextFolder();
					if (folder) {
						this.client.sendRequest(comm.serverRequest.getContextDescriptor, { contextFolder: folder });
					}
				}
			});
		});
	}


	async stop (): Promise<void> {
		if (this.client) {
			this.client.sendRequest(comm.serverRequest.stopPvsServer);
			await this.client.stop();
			// set vscode context variable pvs-server-active to true
			commands.executeCommand('setContext', 'pvs-server-active', false);
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
