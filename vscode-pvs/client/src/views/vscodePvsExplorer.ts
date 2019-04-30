import { ExtensionContext, TreeItemCollapsibleState, commands, window, TextDocument, 
			Uri, Range, Position, TreeItem, Command, EventEmitter, Event,
			TreeDataProvider, workspace, MarkdownString, TreeView } from 'vscode';
import { LanguageClient } from 'vscode-languageclient';
import { TccDescriptorArray, TccDescriptor, PeekDefinitionCommand, PVS_LIBRARY_FILES } from '../common/serverInterface';
import { TheoryMap, FileList, TheoryList, TccList } from '../common/languageUtils';
import * as comm from '../common/serverInterface';
import * as path from 'path';
import { log } from '../utils/logger';

/**
 * Definition of tree items
 */
class TheoryItem extends TreeItem {
	contextValue: string = "theory";
	theoryName: string;
	command: Command;
	fileName: string;
	position: Position;
	pvsContextFolder: string;
	constructor (theoryName: string, fileName: string, position: Position, pvsContextFolder: string, collapsibleState: TreeItemCollapsibleState) {
		super(theoryName, collapsibleState);
		this.theoryName = theoryName;
		this.fileName = fileName;
		this.position = position;
		this.pvsContextFolder = pvsContextFolder;
		this.tooltip = "Click to open " + theoryName;
		this.command = {
			title: "Theory selected",
			command: "explorer.didSelectTheory",
			arguments: [ theoryName, fileName, position, pvsContextFolder ]
		};
	}
	setTccTotal (n: number) {
		// update label
		this.label = this.theoryName + " (tccs: " + n + ")";
		// update collapsible state
		this.collapsibleState = (n > 0) ? TreeItemCollapsibleState.Collapsed
									: TreeItemCollapsibleState.None;
	}
}
// class ShowTccsCommand extends TreeItem {
// 	contextValue: string = "cmd.show-tccs";
// 	command: Command;

// 	constructor (theoryName?: string) { 
// 		super("🔄 load tccs", TreeItemCollapsibleState.None);
// 		this.command = {
// 			title: "Show TCCs",
// 			command: "cmd.show-tccs",
// 			arguments: [ ]			
// 		};
// 	}
// }
class TccItem extends TreeItem {
	contextValue: string = "tcc";
	pvsContextFolder: string;
	fileName: string;
	theoryName: string;
	tcc: TccDescriptor;
	command: Command;
	constructor(tcc: TccDescriptor, theoryName: string, fileName: string, pvsContextFolder: string) {
		super(tcc.id + " ( " + tcc.status + " )", TreeItemCollapsibleState.Collapsed);
		this.tcc = tcc;
		this.theoryName = theoryName;
		this.fileName = fileName;
		this.pvsContextFolder = pvsContextFolder;
	}
}
class NoTccItem extends TreeItem {
	contextValue: string = "no-tcc";
	constructor() {
		super("No TCCs", TreeItemCollapsibleState.None);
	}
}
class SymbolItem extends TreeItem {
	contextValue: string = "tcc.symbol";
	symbol: string;
	range: Range;
	uri: Uri;
	constructor(symbol: string, uri: Uri, line: number, character: number) {
		super("obligation for " + symbol + " ( Ln " + line + ", Col " + character + " )", TreeItemCollapsibleState.None);
		this.symbol = symbol;
		this.range = new Range(
			new Position(line - 1, // in vscode line numbers start from 0, in pvs they start from 1
						 character), // in vscode column numbers start from 1, in pvs they start from 0
			new Position(line - 1, character + symbol.length)
		);
		this.uri = uri;
		this.command = {
			title: "Tcc symbol selected",
			command: "explorer.didSelectTccSymbol",
			arguments: [ this.uri, this.range ]
		};
	}
}
class TccsOverviewItem extends TreeItem {
	contextValue: string = "TCCs";
	theoryName: string; // theory that is associated to this tcc list
	fileName: string; // file that contains the theory
	constructor(theoryName: string, fileName: string) {
		super("tccs", TreeItemCollapsibleState.None);
		this.theoryName = theoryName;
		this.fileName = fileName;
	}
	setTccTotal (n: number, overview?: string) {
		// update label
		this.label = overview ? "tccs (" + overview + ")" : "tccs ( " + n + " )";
		// update collapsible state
		this.collapsibleState = (n > 0) ? TreeItemCollapsibleState.Collapsed
									: TreeItemCollapsibleState.None;
	}
}

export interface TheoryDescriptor {
	theoryName: string,
	fileName: string,
	position: Position
}

/**
 * Data provider for PVS Explorer view
 */
export class VSCodePvsExplorer implements TreeDataProvider<TreeItem> {
	private pvsLibrariesPath: string = null;

	/**
	 * Events for updating the tree structure
	 */
	private _onDidChangeTreeData: EventEmitter<TreeItem> = new EventEmitter<TreeItem>();
	readonly onDidChangeTreeData: Event<TreeItem> = this._onDidChangeTreeData.event;

	/**
	 * Language client for communicating with the server
	 */
	private client: LanguageClient;

	/**
	 * Name of the view associated with the data provider
	 */
	private providerView: string;

	private view: TreeView<TreeItem>

	private theories: TheoryItem[] = [];
	private tccsOverview: { [ theoryName: string ]: TccsOverviewItem } = {};
	private tccs: { [ theoryName: string ]: (TccItem | NoTccItem)[] } = {};

	/**
	 * @constructor
	 * @param client Language client 
	 * @param providerView VSCode view served by the data provider
	 */
	constructor(client: LanguageClient, providerView: string) {
		this.client = client;
		this.providerView = providerView;
		this.pvsLibrariesPath = path.join(workspace.getConfiguration().get("pvs.path"), "lib");
		// register tree view.
		// use window.createTreeView instead of window.registerDataProvider -- this allows to perform UI operations programatically. 
		// window.registerTreeDataProvider(this.providerView, this);
		this.view = window.createTreeView(this.providerView, { treeDataProvider: this });
	}

	/**
	 * Refresh tree view
	 */
	private refreshView(): void {
		this._onDidChangeTreeData.fire();
	}

	/**
	 * Resets the tree view
	 */
	private resetView () {
		this.theories = [];
		this.tccsOverview = {};
		this.tccs = {};
	}

	// FIXME: change the type of this.theories from array to Map so we have direct access?
	public getTheoryItem (theoryName: string): TheoryItem {
		if (this.theories) {
			let candidates: TheoryItem[] = this.theories.filter((item: TheoryItem) => {
				return item.theoryName === theoryName;
			});
			if (candidates && candidates.length === 1) {
				return candidates[0];
			} else {
				log("Warning, vscode client requested theory information but theory is not in the tree :/");
			}
		}
		return null;
	}

	// FIXME: change the type of this.theories from array to Map so we have direct access?
	public getTheoryDescriptor (theoryName: string): TheoryDescriptor {
		if (this.theories) {
			let candidates: TheoryItem[] = this.theories.filter((item: TheoryItem) => {
				return item.theoryName === theoryName;
			});
			if (candidates && candidates.length === 1) {
				return {
					theoryName: theoryName,
					fileName: candidates[0].fileName,
					position: candidates[0].position
				};
			} else {
				log("Warning, vscode client requested theory information but theory is not in the tree :/");
			}
		}
		return null;
	}

	/**
	 * Utility function, loads the content of all pvs files in the current context.
	 * The server automatically synchronizes with the client and loads the files.
	 */
	private async loadPvsFiles () {
		return new Promise((resolve, reject) => {
			this.client.onRequest("server.response.list-files", (ans: FileList) => {
				// load files in memory
				if (ans && ans.fileNames) {
					let n: number = 0;
					for (let i in ans.fileNames) {
						let uri: Uri = Uri.file(ans.pvsContextFolder + "/" + ans.fileNames[i]);
						workspace.openTextDocument(uri).then(() => {
							n++;
							if (n === ans.fileNames.length) {
								resolve(ans.fileNames); // resolve the promise when all files are loaded
							}
						});
					}
				}
			});
			this.client.sendRequest("pvs.list-files");
		});
	}

	private updateTccs(ans: TccList) {
		// the server will create a .tccs file containing the tccs
		if (ans && ans.tccs) {
			const theories: string[] = Object.keys(ans.tccs);
			let tccFileContent: string = "";
			for (let i in theories) {
				let theoryName: string = theories[i];
				let theoryItem: TheoryItem = this.getTheoryItem(theoryName);
				if (theoryItem !== null) {
					// populate the list of tccs
					let tccs: TccDescriptor[] = ans.tccs[theoryName].tccs;
					let overview = { };
					if (tccs && tccs.length > 0) {
						this.tccsOverview[theoryName] = new TccsOverviewItem(theoryItem.theoryName, theoryItem.fileName);
						this.tccs[theoryName] = tccs.map((tcc: TccDescriptor) => {
							// compile tccFileContent and TccItem
							tccFileContent += tcc.content;
							if (!overview[tcc.status]) {
								overview[tcc.status] = 0;
							}
							overview[tcc.status]++;
							return new TccItem(tcc, theoryName, theoryItem.fileName, theoryItem.pvsContextFolder);
						});
					} else {
						this.tccsOverview[theoryName] = null;
						this.tccs[theoryName] = null;
					}
					const ntccs: number = (tccs && tccs.length) ? tccs.length : 0;
					// update theory label to indicate the number of tccs generated for the theory
					theoryItem.setTccTotal(ntccs);
					// update tcc overview to indicate the number of tccs generated for the theory
					if (ntccs > 0 && this.tccsOverview[theoryName]) {
						let overviewArray: string[] = [ ];
						Object.keys(overview).forEach((key) => {
							overviewArray.push(overview[key] + " " + key);
						});
						this.tccsOverview[theoryName].setTccTotal(tccs.length, overviewArray.join(", "));
					}
				}
			}
		}
		this.refreshView();
	}

	/**
	 * Handlers for messages received from the server
	 */
	private installHandlers() {
		// server.response.list-all-theories events are automatically sent by the server when the context folder changes
		this.client.onRequest("server.response.list-all-theories", (ans: TheoryList) => {
			this.resetView();
			if (ans && ans.theories) {
				let theories: TheoryItem[] = Object.keys(ans.theories).map((key: string) => {
					let position: Position = new Position(ans.theories[key].position.line, ans.theories[key].position.character);
					let fileName: string = ans.theories[key].fileName;
					let theoryName: string = ans.theories[key].theoryName;
					let pvsContextFolder: string = ans.pvsContextFolder;
					return new TheoryItem(theoryName, fileName, position, pvsContextFolder, TreeItemCollapsibleState.None);
				});
				this.theories = this.theories.concat(theories);
			}
			this.refreshView();
		});
		this.client.onRequest("server.response.show-tccs", async (ans: TccList) => {
			this.updateTccs(ans);
			if (ans && ans.tccs) {
				const theoryNames: string[] = Object.keys(ans.tccs);
				// Open .tccs file when the command is show-tccs
				for (const i in theoryNames) {
					const fileName: string = path.join(ans.pvsContextFolder, theoryNames[i] + ".tccs");
					const document: TextDocument = await workspace.openTextDocument(fileName);
					window.showTextDocument(document, window.activeTextEditor.viewColumn + 1, true);
				}
			}
		});
		this.client.onRequest("server.response.typecheck-file-and-show-tccs", (ans: TccList) => {
			this.updateTccs(ans);
		});
		this.client.onRequest("server.response.typecheck-prove-and-show-tccs", (ans: TccList) => {
			this.updateTccs(ans);
		});
		this.client.onRequest("server.response.typecheck-all-and-show-tccs", (ans: TccList) => {
			this.updateTccs(ans);
		});
	}

	/**
	 * Shows the list of theories in the tree view
	 * @param uri PVS file
	 */
	private async showTheories (): Promise<void> {
		this.resetView();
		// this.client.sendRequest("pvs.list-theories", uri);
		await this.loadPvsFiles();
		this.client.sendRequest("pvs.list-all-theories");
		// setTimeout(() => {
		// 	this.client.sendRequest("pvs.list-all-theories-and-declarations");
		// }, 2000);
	}
	
	/**
	 * Handler activation function
	 * @param context Client context 
	 */
	activate(context: ExtensionContext) {
		// click on show-tccs (button shown inline with the tree element) to show tccs associated with the theory
		// let cmd = commands.registerCommand('cmd.show-tccs', (resource: TreeItem) => {
		// 	if (resource && resource.contextValue === "TCCs") {
		// 		const desc: TccsOverviewItem = <TccsOverviewItem> resource;
		// 		this.client.sendRequest('pvs.typecheck-theory-and-show-tccs', {
		// 			theoryName: desc.theoryName,
		// 			fileName: desc.fileName
		// 		});
		// 	}
		// });
		// context.subscriptions.push(cmd);
		let cmd = commands.registerCommand('explorer.typecheck-file-and-show-tccs', (resource: TreeItem) => {
			if (resource && resource.contextValue === "theory") {
				const desc: TheoryItem = <TheoryItem> resource;
				// TODO: find a way to select the tree item when clicking the button
				this.client.sendRequest('pvs.typecheck-file-and-show-tccs', desc.fileName);
			} else {
				window.showErrorMessage("Error while trying to execute explorer.typecheck-file-and-show-tccs: resource is null");
			}
		});
		context.subscriptions.push(cmd);

		// click on prove tccs to trigger tcp
		cmd = commands.registerCommand('explorer.typecheck-prove', (resource: TccsOverviewItem) => {
			if (resource && resource.contextValue === "TCCs") {
				this.client.sendRequest('pvs.typecheck-prove-and-show-tccs', resource.fileName);
			} else {
				window.showErrorMessage("Error while trying to execute explorer.typecheck-prove: resource is null");
			}
		});
		context.subscriptions.push(cmd);

		// typecheck-all command from explorer menu
		cmd = commands.registerCommand('explorer.typecheck-all', () => {
			this.client.sendRequest('pvs.typecheck-all-and-show-tccs');
		});
		context.subscriptions.push(cmd);
		
		// click on a tcc to open the file and highlight the tcc name in the theory
		cmd = commands.registerCommand('explorer.didSelectTccSymbol', async (uri: Uri, range: Range) => {
			// window.showInformationMessage("theory selected: " + theoryName + " " + fileName + "(" + position.line + ", " + position.character + ")");
			commands.executeCommand('vscode.open', uri, {
				viewColumn: window.activeTextEditor.viewColumn, // do not open new tabs
				selection: range
			});
		});
		context.subscriptions.push(cmd);
		
		// click on a theory name to open the file and highlight the theory name in the file
		cmd = commands.registerCommand('explorer.didSelectTheory', async (theoryName: string, fileName: string, position: Position, pvsContextFolder: string) => {
			// window.showInformationMessage("theory selected: " + theoryName + " " + fileName + "(" + position.line + ", " + position.character + ")");
			const uri: Uri = Uri.file(path.join(pvsContextFolder, fileName + ".pvs"));
			commands.executeCommand('vscode.open', uri, {
				viewColumn: window.activeTextEditor.viewColumn, // do not open new tabs
				selection: new Range(
					position,
					new Position(position.line, position.character + theoryName.length) // highlight the theory name
				)
			});
		});
		context.subscriptions.push(cmd);

		this.installHandlers();
		this.showTheories();
	}

	/**
	 * Returns the list of theories defined in the active pvs file
	 * @param element Element clicked by the user 
	 */
	getChildren(element: TreeItem): Thenable<TreeItem[]> {
		if (element) {
			let children: TreeItem[] = null;
			if (element.contextValue === "theory") {
				// pvs theory
				const desc: TheoryItem = <TheoryItem> element;
				children = [ this.tccsOverview[desc.theoryName] ];
			} else if (element.contextValue === "TCCs") {
				// tcc list
				const desc: TccsOverviewItem = <TccsOverviewItem> element;
				children = this.tccs[desc.theoryName];
			} else if (element.contextValue === "tcc") {
				// tcc
				const desc: TccItem = <TccItem> element;
				const tcc: TccDescriptor = desc.tcc;
				const fileName: string = desc.fileName;
				const pvsContextFolder: string = desc.pvsContextFolder;
				const uri: Uri = Uri.file(pvsContextFolder + "/" + fileName + ".pvs");
				children = [
					new SymbolItem(tcc.symbol, uri, tcc.line, tcc.character),
					new TreeItem(tcc.msg, TreeItemCollapsibleState.None)
				];
			}
			return Promise.resolve(children);
		}
		// root node: show the list of theories from the selected file
		return Promise.resolve(this.theories);
	}

	getTreeItem(element: TreeItem): TreeItem {
		return element;
	}

}
