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
import { TextDocument, window, workspace, ExtensionContext, TextEditor, TextDocumentChangeEvent, commands, WorkspaceConfiguration, TextDocumentWillSaveEvent, TextEditorVisibleRangesChangeEvent, languages } from 'vscode';
import { LanguageClient, LanguageClientOptions, TransportKind, ServerOptions } from 'vscode-languageclient';
import { VSCodePvsDecorationProvider } from './providers/vscodePvsDecorationProvider';
import { VSCodePvsWorkspaceExplorer } from './views/vscodePvsWorkspaceExplorer';
import { VSCodePvsEmacsBindingsProvider } from './providers/vscodePvsEmacsBindingsProvider';
import { VSCodePVSioTerminal } from './views/vscodePVSioTerminal'; 
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

const server_path: string = path.join('server', 'out', 'pvsLanguageServer.js');
const AUTOSAVE_INTERVAL: number = 10000; //ms Note: small autosave intervals (e.g., 1sec) create an unwanted scroll effect in the editor (the current line is scrolled to the top)

export class PvsLanguageClient { //implements vscode.Disposable {
	// language client
	protected client: LanguageClient;
	protected pvsPath: string;
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

	// integrated command line interfaces
	protected pvsioTerminal: VSCodePVSioTerminal;
	protected vscodePvsTerminal: VSCodePvsTerminal;

	// status bar
	protected statusBar: VSCodePvsStatusBar;

	// proofmate
	protected proofMate: VSCodePvsProofMate;

	// events dispatcher
	protected eventsDispatcher: EventsDispatcher;

	// package manager
	protected packageManager: VSCodePvsPackageManager;

	/**
	 * Internal function, returns the pvs path indicated in the configuration file
	 */
	// protected getPvsPath (): string {
	// 	return workspace.getConfiguration().get("pvs.path");
	// }


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
		// onDidChangeActiveTextEditor is emitted when the active editor focuses on a new document
		window.onDidChangeActiveTextEditor((event: TextEditor) => {
			const editor: TextEditor = window.activeTextEditor; //event || window.activeTextEditor;
			if (editor && editor.document && fsUtils.isPvsFile(editor.document.fileName)) {
				// update decorations
				this.decorationProvider.updateDecorations(editor);
				// trigger file parsing to get syntax diagnostics
				this.client.sendRequest(comm.serverCommand.parseFile, editor.document.fileName);
				// const context: string = fsUtils.getContextFolder(editor.document.fileName);
				// this.client.sendRequest(comm.serverCommand.parseWorkspace, context);
			}
		}, null, this.context.subscriptions);

		// onDidChangeTextDocument is emitted when the content of a document changes
		workspace.onDidChangeTextDocument((event: TextDocumentChangeEvent) => {
			if (fsUtils.isPvsFile(event.document.fileName)) {
				this.decorationProvider.updateDecorations(window.activeTextEditor);
				// autosave will trigger parsing, which in turn triggers diagnostics
				this.autosave(event.document);
			}
		}, null, this.context.subscriptions);

		// onDidChangeConfiguration is emitted when the configuration file changes
		workspace.onDidChangeConfiguration(() => {
			// re-initialise pvs if the executable is different
			const pvsPath: string = workspace.getConfiguration().get("pvs.path");
			if (pvsPath !== this.pvsPath) {
				this.pvsPath = pvsPath;
				const msg: string = `Restarting PVS from ${pvsPath}`;
				this.statusBar.progress(msg);
				window.showInformationMessage(msg);
				this.client.sendRequest(comm.serverCommand.startPvsLanguageServer, { pvsPath: this.pvsPath }); // the server will use the last context folder it was using	
			}
		}, null, this.context.subscriptions);
	}

	event (eventName: string): void {
		switch (eventName) {
			case serverEvent.pvsServerReady: {
				this.pvsServerReady = true;
				break;
			}
			default: {
				console.log(`[pvsLanguageClient] Warning: unhandled event ${eventName}`);
			}
		}
	}

	/**
	 * Client activation function.
	 * @param context 
	 */
	activate (context: ExtensionContext): void {
		// set vscode context variable pvs-language-active to true
		commands.executeCommand('setContext', 'pvs-language-active', true);
		
		// save pointer to extension context
		this.context = context;
		
		// The server is implemented in NodeJS
		const serverModule = context.asAbsolutePath(server_path);
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
		
		// create status bar
		this.statusBar = new VSCodePvsStatusBar(this.client);
		this.statusBar.activate(this.context);
		this.statusBar.progress("Starting vscode-pvs...");
		
		// start client, which in turn will also start the server
		this.client.start();
		this.client.onReady().then(() => {		
			// initialise service providers defined on the client-side
			this.statusBar.progress("Activating vscode-pvs components...");	
			this.emacsBindingsProvider = new VSCodePvsEmacsBindingsProvider(this.client, this.statusBar);
			this.emacsBindingsProvider.activate(this.context);
			this.workspaceExplorer = new VSCodePvsWorkspaceExplorer(this.client, 'workspace-explorer-view');
			this.workspaceExplorer.activate(this.context);
			this.proofExplorer = new VSCodePvsProofExplorer(this.client, 'proof-explorer-view');
			this.proofExplorer.activate(this.context);
			this.outlineProvider = new VSCodePvsFileOutlineProvider(this.client);
			this.outlineProvider.activate(this.context);
			this.vscodePvsTerminal = new VSCodePvsTerminal(this.client);
			this.vscodePvsTerminal.activate(this.context);
			this.proofMate = new VSCodePvsProofMate(this.client, 'proof-mate-view');
			this.proofMate.activate(this.context);
			this.pvsioTerminal = new VSCodePVSioTerminal();
			this.pvsioTerminal.activate(this.context);
			this.packageManager = new VSCodePvsPackageManager(this.client, this.statusBar);
	
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
				proofMate: this.proofMate
			});
			this.eventsDispatcher.activate(context);
		
			// define error handlers
			this.client.onRequest(serverEvent.pvsNotPresent, () => {
				this.packageManager.installationWizard("Could not find PVS executable");
			});
			this.client.onRequest(serverEvent.pvsIncorrectVersion, (msg: string) => {
				this.packageManager.installationWizard(msg);
			});

			// start PVS
			const contextFolder = vscodeUtils.getEditorContextFolder();
			this.pvsPath = workspace.getConfiguration().get("pvs.path");
			this.client.sendRequest(comm.serverCommand.startPvsLanguageServer, {
				pvsPath: this.pvsPath, 
				contextFolder
			});
		});
	}


	async stop (): Promise<void> {
		if (this.client) {
			this.client.sendRequest("kill-pvs");
			await this.client.stop();
			// set vscode context variable pvs-language-active to true
			commands.executeCommand('setContext', 'pvs-language-active', false);
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
