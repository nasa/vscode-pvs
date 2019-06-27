/**
 * @module pvsLanguageServer
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
import { TextDocument, window, workspace, ExtensionContext, Position, Disposable, commands, TextEditor, TextDocumentChangeEvent } from 'vscode';
import { LanguageClient, LanguageClientOptions, TransportKind, ServerOptions } from 'vscode-languageclient';
import { log } from './utils/vscode-utils';
import { VSCodePvsDecorationProvider } from './providers/vscodePvsDecorationProvider';
import { VSCodePvsTheoryExplorer } from './views/vscodePvsTheoryExplorer';
import { VSCodePvsEmacsBindingsProvider } from './providers/vscodePvsEmacsBindingsProvider';
import { VSCodePVSioTerminal } from './terminals/vscodePVSioTerminal'; 
import { VSCodePvsTerminal } from './terminals/vscodePvsTerminalCli';
import { VSCodePvsProofExplorer } from './views/vscodePvsProofExplorer';
import * as fsUtils from './common/fsUtils';
import { VSCodePvsStatusBar } from './views/vscodePvsStatusBar';

//--------- experimental features -----------------
const EXPERIMENTAL: boolean = true;
import { VSCodePvsSequentExplorer } from './views/vscodePvsSequentExplorer';
//-------------------------------------------------

const server_path: string = path.join('server', 'out', 'pvsLanguageServer.js');
const AUTOSAVE_INTERVAL: number = 1000; //ms

export class PvsLanguageClient { //implements vscode.Disposable {
	// language client
	private client: LanguageClient;

	// input manager
	// private inputManager: MultiStepInput;
	private pvsPath: string;

	// context variables
	private context: ExtensionContext;

	private timers: {[key: string]: NodeJS.Timer } = {};

	// data providers for the text editor
	// private hoverProvider: VSCodePvsHoverProvider;
	private decorationProvider: VSCodePvsDecorationProvider;
	private emacsBindingsProvider: VSCodePvsEmacsBindingsProvider;

	// data provider for the tree views
	private theoryExplorer: VSCodePvsTheoryExplorer;
	private proofExplorer: VSCodePvsProofExplorer;

	// integrated terminals for PVSio
	private pvsioTerminal: VSCodePVSioTerminal;
	private pvsTerminal: VSCodePvsTerminal;

	// sequent viewer
	private sequentExplorer: VSCodePvsSequentExplorer;

	// status bar
	private pvsStatusBar: VSCodePvsStatusBar;

	private getPvsPath (): string {
		return workspace.getConfiguration().get("pvs.path");
	}

	// autosave pvs files with frequency AUTOSAVE_INTERVAL
	private autosave (document: TextDocument) {
		// cancel any previously scheduled save 
		if (this.timers['autosave']) {
			clearTimeout(this.timers['autosave']);
		}
		// save document after a delay
		// FIXME: check this function, it seems that save is triggered twice
		this.timers['autosave'] = setTimeout(() => {
			document.save();
			this.timers['autosave'] = null;
		}, AUTOSAVE_INTERVAL);
	}
	// utility function for registering handlers
	private _registerHandlers() {
		const _this = this;
		// register handlers for server responses
		this.client.onRequest("pvs.lisp", function (ans: string) {
			log("received answer for pvs.lisp");
			log(ans);
		});
		this.client.onRequest("server.response.runit", function (ans: comm.EvaluationResult) {
			// vscode.workspace.openTextDocument(vscode.Uri.parse("untitled:animation.result")).then(function (document) {
			// 	vscode.window.showTextDocument(document, vscode.window.activeTextEditor.viewColumn + 1, true);
			// });
			let content: string = ans.msg + "\n" + ans.result;
			workspace.openTextDocument({ language: 'pvs', content: content }).then((document: TextDocument) => {
				window.showTextDocument(document, window.activeTextEditor.viewColumn + 1, true);
			});
		});
		this.client.onRequest("server.response.proveit", async (ans: comm.ProofResult) => {
			// vscode.workspace.openTextDocument(vscode.Uri.parse("untitled:animation.result")).then(function (document) {
			// 	vscode.window.showTextDocument(document, vscode.window.activeTextEditor.viewColumn + 1, true);
			// });
			let content: string = ans.result;
			workspace.openTextDocument({ language: 'sequent', content: content }).then((document: TextDocument) => {
				window.showTextDocument(document, window.activeTextEditor.viewColumn + 1, true);
			});
			// // Create and show panel
			// const panel = new SequentView();
			// panel.showSequents(content);

			// let res = await _this.inputManager.createInputBox();

			// const terminal = vscode.window.createTerminal("Ext Terminal #1");
			// terminal.sendText(content);

		});
		this.client.onRequest("server.response.parse-file", (ans: comm.PvsParserResponse) => {
			// do nothing for now.
		});
		this.client.onRequest("server.response.change-context-and-parse-files", (ans: comm.PvsParserResponse) => {
			// do nothing for now.
		});
		window.onDidChangeActiveTextEditor((event: TextEditor) => {
			// event emitted when the active editor focuses on a new document
			const editor: TextEditor = event || window.activeTextEditor;
			if (editor.document && fsUtils.isPvsFile(editor.document.fileName)) {
				// update decorations
				this.decorationProvider.updateDecorations(editor);
				// trigger file parsing to get syntax diagnostics
				const context: string = fsUtils.getContextFolder(editor.document.fileName);
				this.client.sendRequest('pvs.change-context-and-parse-files', context);
			}
		}, null, _this.context.subscriptions);
		workspace.onDidChangeTextDocument((event: TextDocumentChangeEvent) => {
			// event emitted when the document changes
			if (fsUtils.isPvsFile(event.document.fileName)) {
				this.decorationProvider.updateDecorations(window.activeTextEditor);
				this.autosave(event.document); // this will trigger diagnostics (parsefile updates diagnostics every time the file is saved on disk)
				// this.theoriesDataProvider.showTheories(window.activeTextEditor.document.fileName);
			}
		}, null, _this.context.subscriptions);

		workspace.onDidChangeConfiguration(async () => {
			// re-initialise pvs if the executable is different
			const pvsPath: string = this.getPvsPath();
			if (pvsPath !== this.pvsPath) {
				window.showInformationMessage(`Restarting PVS (pvs path changed to ${pvsPath})`);
				this.pvsPath = pvsPath;
				await this.client.sendRequest('pvs.restart', { pvsPath: this.pvsPath }); // the server will use the last context folder it was using	
			}	
		});
	}
	activate (context: ExtensionContext) {
		// save pointer to extension context
		this.context = context;
		
		// register handlers
		let cmd = commands.registerCommand('cmd.runit', (resource: comm.ExpressionDescriptor) => {
			this.client.sendRequest('pvs.runit', resource);
		});
		context.subscriptions.push(cmd);
		cmd = commands.registerCommand('cmd.proveit', (resource: comm.FormulaDescriptor) => {
			this.client.sendRequest('pvs.proveit', resource);
		});
		context.subscriptions.push(cmd);
		cmd = commands.registerCommand('editor.typecheck-file', () => {
			// typechecking: shortcut C-t
			// The file name is given by the file opened in the active editor
			this.client.sendRequest('pvs.typecheck-file-and-show-tccs', window.activeTextEditor.document.fileName);
		});
		context.subscriptions.push(cmd);

		// The server is implemented in NodeJS
		const serverModule = context.asAbsolutePath(server_path);

		// If the extension is launched in debug mode then the debug server options are used
		// Otherwise the run options are used
		const serverOptions: ServerOptions = {
			run: { module: serverModule, transport: TransportKind.ipc },
			debug: {
				module: serverModule,
				transport: TransportKind.ipc,
				options: { execArgv: ['--nolazy', '--inspect=6009'] } // --inspect=6009: runs the server in Node's Inspector mode so VS Code can attach to the server for debugging
			}
		};

		// Options to control the language client
		let clientOptions: LanguageClientOptions = {
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
			'PVS Lisp',
			serverOptions,
			clientOptions
		);
		
		return this;
	}
	async start () {
		await this.client.start(); // this will start also the server
		await this.client.onReady();
		this._registerHandlers();

		// create status bar
		this.pvsStatusBar = new VSCodePvsStatusBar(this.client);
		this.pvsStatusBar.activate(this.context);
		
		// initialise service providers defined on the client-side
		this.decorationProvider = new VSCodePvsDecorationProvider();
		this.emacsBindingsProvider = new VSCodePvsEmacsBindingsProvider(this.client);
		this.emacsBindingsProvider.activate(this.context);

		this.pvsioTerminal = new VSCodePVSioTerminal(this.pvsStatusBar.getVersionInfo());
		this.pvsioTerminal.activate(this.context);

		this.theoryExplorer = new VSCodePvsTheoryExplorer(this.client, 'theory-explorer-view');
		this.theoryExplorer.activate(this.context);

		this.proofExplorer = new VSCodePvsProofExplorer(this.client, 'proof-explorer-view');
		this.proofExplorer.activate(this.context);

		if (EXPERIMENTAL) {
			this.sequentExplorer = new VSCodePvsSequentExplorer();
			this.sequentExplorer.activate(this.context);
		}

		this.pvsTerminal = new VSCodePvsTerminal(this.client, this.theoryExplorer);
		this.pvsTerminal.activate(this.context);

		if (window.activeTextEditor && fsUtils.isPvsFile(window.activeTextEditor.document.fileName)) {
			this.decorationProvider.updateDecorations(window.activeTextEditor);
		}

		// start PVS
		const contextFolder = (window.activeTextEditor) ? fsUtils.getContextFolder(window.activeTextEditor.document.fileName) : null;
		this.pvsPath = this.getPvsPath();
		this.client.sendRequest('pvs.restart', { pvsPath: this.pvsPath, contextFolder });			
	}
	stop () {
		if (this.client) {
			this.client.sendRequest("kill-pvs");
			return this.client.stop();
		}
		return Promise.resolve();
	}
}


const pvsLanguageClient = new PvsLanguageClient();

/**
 * Function activate is invoked every time a new IDE session is started. This includes:
 *  - Opening a new folder in the IDE browser
 *  - ...
 * @param context 
 */
export function activate(context: ExtensionContext) {
	// Activate the client.
	pvsLanguageClient.activate(context);
	// Start the client. This will also launch the server
	pvsLanguageClient.start();
}

export function deactivate(): Thenable<void> | undefined {
	return pvsLanguageClient.stop();
}
