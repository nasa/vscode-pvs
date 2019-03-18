import { ExtensionContext, TreeItemCollapsibleState, commands, window, TextDocument, 
			Uri, Range, Position, TreeItem, Command, EventEmitter, Event,
			TreeDataProvider, workspace, MarkdownString } from 'vscode';
import { LanguageClient } from 'vscode-languageclient';
import { TccDescriptorArray, TccDescriptor, PeekDefinitionCommand, PVS_LIBRARY_FILES, getPathname } from '../common/serverInterface';
import { TheoryMap, FileList, TheoryList, TccList } from '../common/languageUtils';
import * as comm from '../common/serverInterface';
import * as path from 'path';
import { log } from '../utils/logger';

// tree items
class TheoryItem extends TreeItem {
	contextValue: string = "theory";
	theoryName: string;
	command: Command;
	fileName: string;
	position: Position;
	pvsContextPath: string;
	constructor (theoryName: string, fileName: string, position: Position, pvsContextPath: string, collapsibleState: TreeItemCollapsibleState) {
		super(theoryName, collapsibleState);
		this.theoryName = theoryName;
		this.fileName = fileName;
		this.position = position;
		this.pvsContextPath = pvsContextPath;
		this.tooltip = "Click to open " + theoryName;
		this.command = {
			title: "Theory selected",
			command: "vscode.didSelectTheory",
			arguments: [ theoryName, fileName, position, pvsContextPath ]
		};
	}
	setTccTotal (n: number) {
		// update label
		this.label = (n === 1) ? this.theoryName + "  ( 1 TCC )"
						: this.theoryName + "  ( " + n + " TCCs )";
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
	pvsContextPath: string;
	fileName: string;
	theoryName: string;
	tcc: TccDescriptor;
	command: Command;
	constructor(tcc: TccDescriptor, theoryName: string, fileName: string, pvsContextPath: string) {
		super(tcc.id + " ( " + tcc.status + " )", TreeItemCollapsibleState.Collapsed);
		this.tcc = tcc;
		this.theoryName = theoryName;
		this.fileName = fileName;
		this.pvsContextPath = pvsContextPath;
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
			command: "vscode.didSelectTccSymbol",
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
	setTccTotal (n: number) {
		// update label
		this.label = (n > 0) ? "tccs ( " + n + " )" : "tccs";
		// update collapsible state
		this.collapsibleState = (n > 0) ? TreeItemCollapsibleState.Collapsed
									: TreeItemCollapsibleState.None;
	}
}

/**
 * Data provider for the PVS Explorer view in VSCode
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
		window.registerTreeDataProvider(this.providerView, this);
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

	// FIXME: change this.theories from array to Map
	private getTheoryItem (theoryName: string): TheoryItem {
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
						let uri: Uri = Uri.file(ans.pvsContextPath + "/" + ans.fileNames[i]);
						workspace.openTextDocument(uri).then(function () {
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

	/**
	 * Handlers for messages received from the server
	 */
	private installHandlers() {
		this.client.onRequest("server.response.list-all-theories", (ans: TheoryList) => {
			if (ans && ans.theories) {
				let theories: TheoryItem[] = Object.keys(ans.theories).map((key: string) => {
					let position: Position = new Position(ans.theories[key].position.line, ans.theories[key].position.character);
					let fileName: string = ans.theories[key].fileName;
					let theoryName: string = ans.theories[key].theoryName;
					let pvsContextPath: string = ans.pvsContextPath;
					return new TheoryItem(theoryName, fileName, position, pvsContextPath, TreeItemCollapsibleState.None);
				});
				this.theories = this.theories.concat(theories);
			}
			this.refreshView();
		});
		this.client.onRequest("server.response.show-tccs", (ans: TccList) => {
			if (ans && ans.tccs) {
				let theories: string[] = Object.keys(ans.tccs);
				for (let i in theories) {
					let theoryName: string = theories[i];
					let theoryItem: TheoryItem = this.getTheoryItem(theoryName);
					if (theoryItem !== null) {
						// populate the list of tccs
						let tccs: TccDescriptor[] = ans.tccs[theoryName].tccs;
						if (tccs && tccs.length > 0) {
							this.tccsOverview[theoryName] = new TccsOverviewItem(theoryItem.theoryName, theoryItem.fileName);
							this.tccs[theoryName] = tccs.map(function (tcc: TccDescriptor) {
								return new TccItem(tcc, theoryName, theoryItem.fileName, theoryItem.pvsContextPath);
							});
						} else {
							this.tccsOverview[theoryName] = null;
							this.tccs[theoryName] = null;
						}
						// update theory label to indicate the number of tccs generated for the theory
						// FIXME: indicate the number of unproved tccs
						theoryItem.setTccTotal(tccs.length);
						// update tcc overview to indicate the number of tccs generated for the theory
						if (this.tccsOverview[theoryName]) {
							this.tccsOverview[theoryName].setTccTotal(tccs.length);
						}
					}
				}
			}
			this.refreshView();
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
		// 		this.client.sendRequest('pvs.show-tccs', {
		// 			theoryName: desc.theoryName,
		// 			fileName: desc.fileName
		// 		});
		// 	}
		// });
		// context.subscriptions.push(cmd);
		let cmd = commands.registerCommand('cmd.typecheck-theory', (resource: TreeItem) => {
			if (resource && resource.contextValue === "theory") {
				const desc: TheoryItem = <TheoryItem> resource;
				// this.client.sendRequest('pvs.typecheck-file', desc.fileName);
				// show-tccs performs typechecking and reports tccs -- FIXME: this should be the standard action when typechecking, perhaps the show-tccs command on the server should be renamed to typecheck-file
				this.client.sendRequest('pvs.show-tccs', {
					theoryName: desc.theoryName,
					fileName: desc.fileName
				});
			}
		});
		context.subscriptions.push(cmd);
		
		// click on a tcc to open the file and highlight the tcc name in the theory
		cmd = commands.registerCommand('vscode.didSelectTccSymbol', async (uri: Uri, range: Range) => {
			// window.showInformationMessage("theory selected: " + theoryName + " " + fileName + "(" + position.line + ", " + position.character + ")");
			commands.executeCommand('vscode.open', uri, {
				viewColumn: window.activeTextEditor.viewColumn, // do not open new tabs
				selection: range
			});
		});
		context.subscriptions.push(cmd);
		
		// click on a theory name to open the file and highlight the theory name in the file
		cmd = commands.registerCommand('vscode.didSelectTheory', async (theoryName: string, fileName: string, position: Position, pvsContextPath: string) => {
			// window.showInformationMessage("theory selected: " + theoryName + " " + fileName + "(" + position.line + ", " + position.character + ")");
			const uri: Uri = Uri.file(path.join(pvsContextPath, fileName + ".pvs"));
			commands.executeCommand('vscode.open', uri, {
				viewColumn: window.activeTextEditor.viewColumn, // do not open new tabs
				selection: new Range(
					position,
					new Position(position.line, position.character + theoryName.length) // highlight the theory name
				)
			});
		});
		context.subscriptions.push(cmd);

		// click on prove tccs to trigger tcp
		cmd = commands.registerCommand('cmd.tcp', (resource: TccsOverviewItem) => {
			if (resource) {
				this.client.sendRequest('pvs.typecheck-file', {
					fileName: resource.fileName,
					tcpFlag: true
				});
			}
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
				const pvsContextPath: string = desc.pvsContextPath;
				const uri: Uri = Uri.file(pvsContextPath + "/" + fileName + ".pvs");
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
