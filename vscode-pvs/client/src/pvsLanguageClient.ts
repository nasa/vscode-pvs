import * as path from 'path';
import * as comm from './common/serverInterface';
import { TextDocument, window, workspace, ExtensionContext, Position, Disposable, commands } from 'vscode';
import { LanguageClient, LanguageClientOptions, TransportKind, ServerOptions } from 'vscode-languageclient';
import { log } from './utils/vscode-utils';
import { VSCodePvsDecorationProvider } from './providers/vscodePvsDecorationProvider';
// import { VSCodePvsHoverProvider } from './providers/vscodePvsHoverProvider-obsolete';
// import { MultiStepInput } from './theoryExplorer/multiStepInput';
import { VSCodePvsExplorer } from './views/vscodePvsTheoryExplorer';
import { VSCodePvsEmacsBindingsProvider } from './providers/vscodePvsEmacsBindingsProvider';
import { VSCodePVSioTerminal } from './terminals/vscodePVSioTerminal'; 
import { VSCodePvsTerminal } from './terminals/vscodePvsTerminalCli';
import { VSCodePvsProofExplorer } from './views/vscodePvsProofExplorer';
import * as fs from './common/fsUtils';
import { VSCodePvsStatusBar } from './views/vscodePvsStatusBar';

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
	private pvsContextFolder: string;

	private timers: {[key: string]: NodeJS.Timer } = {};

	// data providers for the text editor
	// private hoverProvider: VSCodePvsHoverProvider;
	private decorationProvider: VSCodePvsDecorationProvider;
	private emacsBindingsProvider: VSCodePvsEmacsBindingsProvider;

	// data provider for the tree views
	private theoriesDataProvider: VSCodePvsExplorer;
	private proofDataProvider: VSCodePvsProofExplorer;

	// integrated terminals for PVSio
	private pvsioTerminal: VSCodePVSioTerminal;
	private pvsTerminal: VSCodePvsTerminal;

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
		window.onDidChangeActiveTextEditor(editor => {
			// event emitted when the active editor focuses on a new document
			if (editor.document && fs.isPvsFile(editor.document.fileName)) {
				// update decorations
				this.decorationProvider.updateDecorations(editor);
				// trigger file parsing to get syntax diagnostics
				const context: string = fs.getPathname(window.activeTextEditor.document.fileName);
				this.client.sendRequest('pvs.change-context-and-parse-files', context);
				// this.theoriesDataProvider.showTheories(window.activeTextEditor.document.fileName);
			}
		}, null, _this.context.subscriptions);
		workspace.onDidChangeTextDocument(event => {
			// event emitted when the document changes
			if (fs.isPvsFile(event.document.fileName)) {
				this.decorationProvider.updateDecorations(window.activeTextEditor);
				this.autosave(event.document); // this will trigger diagnostics (parsefile updates diagnostics every time the file is saved on disk)
				// this.theoriesDataProvider.showTheories(window.activeTextEditor.document.fileName);
			}
		}, null, _this.context.subscriptions);

		workspace.onDidChangeConfiguration(async  () => {
			// re-initialise pvs if the executable is different
			const pvsPath: string = await this.getPvsPath();
			if (pvsPath !== this.pvsPath) {
				window.showInformationMessage(`Restarting PVS (pvs path changed to ${pvsPath})`);
				this.pvsPath = pvsPath;
				await this.client.sendRequest('pvs.restart', { pvsPath: this.pvsPath, pvsContextFolder: this.pvsContextFolder });	
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

		// register command handlers
		// const _this = this;
		// vscode.commands.registerTextEditorCommand("client.request.peek_definition", function (textEditor: vscode.TextEditor, edit: vscode.TextEditorEdit, ...args: any[]) {
		// 	const line = textEditor.selection.active.line;
		// 	const column = textEditor.selection.active.character;
		// 	const symbolRange = vscode.window.activeTextEditor.document.getWordRangeAtPosition(textEditor.selection.start); // vscode starts to count lines from 0, pvs starts from 1
		// 	const symbolName = textEditor.document.getText(symbolRange);
		// 	_this._commands["peek-definition"](textEditor.document, line, column, symbolName);
		// });
		// vscode.commands.registerTextEditorCommand("client.request.eval", function (textEditor: vscode.TextEditor, edit: vscode.TextEditorEdit, ...args: any[]) {
		// 	const text = textEditor.document.getText(textEditor.selection);
		// 	// let filename = textEditor.document.fileName;
		// 	// let line = textEditor.selection.active.line;
		// 	// let column = textEditor.selection.active.character;
		// 	const cmd = "pvsio.eval " + text;
		// 	// log("sending command " + cmd);
		// 	_this.client.sendRequest(ExecuteCommandRequest.type, {
		// 		command: cmd
		// 	});
		// });
		
		return this;
	}
	async start () {
		await this.client.start(); // this will start also the server
		await this.client.onReady();
		this._registerHandlers();

		// create status bar
		this.pvsStatusBar = new VSCodePvsStatusBar(this.client);
		this.pvsStatusBar.activate(this.context);

		// initialise pvs
		// await this.client.sendRequest('pvs.init', pvsExecutionContext);
		
		// initialise service providers defined on the client-side
		this.decorationProvider = new VSCodePvsDecorationProvider();
		// this.hoverProvider = new VSCodePvsHoverProvider(this.client);
		// this.hoverProvider.activate(this.context);
		this.emacsBindingsProvider = new VSCodePvsEmacsBindingsProvider(this.client);
		this.emacsBindingsProvider.activate(this.context);

		this.pvsioTerminal = new VSCodePVSioTerminal(this.pvsStatusBar.getVersionInfo());
		this.pvsioTerminal.activate(this.context);

		// this.inputManager = new MultiStepInput(this.client);
		this.theoriesDataProvider = new VSCodePvsExplorer(this.client, 'theory-explorer-view');
		this.theoriesDataProvider.activate(this.context);

		this.proofDataProvider = new VSCodePvsProofExplorer(this.client, 'proof-explorer-view');
		this.proofDataProvider.activate(this.context);

		this.pvsTerminal = new VSCodePvsTerminal(this.client, this.theoriesDataProvider);
		this.pvsTerminal.activate(this.context);

		if (window.activeTextEditor && fs.isPvsFile(window.activeTextEditor.document.fileName)) {
			this.decorationProvider.updateDecorations(window.activeTextEditor);
		}

		setTimeout(() => {
			// start PVS
			this.pvsContextFolder = fs.getPathname(window.activeTextEditor.document.fileName);
			this.pvsPath = this.getPvsPath();
			this.client.sendRequest('pvs.restart', { pvsPath: this.pvsPath, pvsContextFolder: this.pvsContextFolder });			
		}, 500);
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
