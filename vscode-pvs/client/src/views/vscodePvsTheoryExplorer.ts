import { ExtensionContext, TreeItemCollapsibleState, commands, window, TextDocument, 
			Uri, Range, Position, TreeItem, Command, EventEmitter, Event,
			TreeDataProvider, workspace, TreeView } from 'vscode';
import { LanguageClient } from 'vscode-languageclient';
import { FileList, TheoryList, TheoriesMap, FormulaDescriptor,
			TheoryStatus, TheoryDescriptor } from '../common/serverInterface';
import * as path from 'path';
import * as fsUtils from '../common/fsUtils';

//-- theories
class TheoryItem extends TreeItem {
	contextValue: string = "theory";
	theoryName: string;
	command: Command;
	fileName: string;
	position: Position;
	tccsOverview: TccsOverviewItem;
	theoremsOverview: TheoremsOverviewItem;
	pvsContextFolder: string;
	constructor (theoryName: string, fileName: string, position: Position, pvsContextFolder: string, collapsibleState: TreeItemCollapsibleState) {
		super(theoryName, collapsibleState);
		this.theoryName = theoryName;
		this.fileName = fileName;
		this.position = position;
		this.pvsContextFolder = pvsContextFolder;
		this.tooltip = `Click to open ${theoryName}`;
		this.command = {
			title: "Theory selected",
			command: "explorer.didSelectTheory",
			arguments: [ theoryName, fileName, position, pvsContextFolder ]
		};
		this.theoremsOverview = new TheoremsOverviewItem({ fileName, pvsContextFolder });
		this.tccsOverview = new TccsOverviewItem({ fileName, pvsContextFolder });
		this.refreshLabel();
	}
	refreshLabel () {
		// update label
		const nTheorems: number = this.theoremsOverview.getTotal();
		const nTccs: number = this.tccsOverview.getTotal();
		const n: number = nTccs + nTheorems;
		this.label = `${this.theoryName}`;// (${this.tccsOverview.tccs.length} tccs, ${this.theoremsOverview.theorems.length} theorems)`;
		// update collapsible state
		this.collapsibleState = (n > 0) ? TreeItemCollapsibleState.Expanded : TreeItemCollapsibleState.None;
	}
	getFormula (formulaName: string): FormulaItem {
		return this.theoremsOverview.getFormula(formulaName);
	}
	updateFormula (desc: FormulaDescriptor): void {
		this.theoremsOverview.updateStatus(desc);
	}
}
class OverviewItem extends TreeItem {
	contextValue: string = "OverviewItem";
	fileName: string;
	pvsContextFolder: string;
	formulae: { [ formulaName: string ]: FormulaItem } = {};
	constructor(typeName: string, desc: { pvsContextFolder: string, fileName: string }) {
		super(typeName, TreeItemCollapsibleState.None);
		this.contextValue = typeName;
		this.label = typeName.toLowerCase();
		this.fileName = desc.fileName;
		this.pvsContextFolder = desc.pvsContextFolder;
	}
	getFormula (formulaName: string): FormulaItem {
		return this.formulae[formulaName];
	}
	listFormulae (): FormulaItem[] {
		const lst: FormulaItem[] = Object.keys(this.formulae).map(key => {
			return this.formulae[key];
		});
		return lst;
	}
	private refreshLabel () {
		const lst: string[] = Object.keys(this.formulae);
		this.label = `${this.contextValue.toLowerCase()} ( ${lst.length} )`;
		this.collapsibleState = (lst.length > 0) ? TreeItemCollapsibleState.Expanded
													: TreeItemCollapsibleState.None;
	}
	updateStatus (desc: FormulaDescriptor): void {
		if (desc) {
			if (this.formulae[desc.formulaName]) {
				if (desc.status) {
					this.formulae[desc.formulaName].setStatus(desc.status);
					this.refreshLabel();
				}
			} else {
				const formulaType: string = (desc.isTcc) ? "tcc" : "theorem"
				this.formulae[desc.formulaName] = new FormulaItem(formulaType, desc, this.pvsContextFolder);
				this.refreshLabel();
			} 
		}
	}
	getTotal (): number {
		return Object.keys(this.formulae).length;
	}
}
class FormulaItem extends TreeItem {
	contextValue: string = "formula";
	// desc: TheoremDescriptor;
	private fileName: string;
	private theoryName: string;
	private formulaName: string;
	private position: Position;
	private status: string;
	private pvsContextFolder: string;
	constructor(typeName: string, desc: FormulaDescriptor, pvsContextFolder: string) {
		super(typeName, TreeItemCollapsibleState.None);
		this.contextValue = typeName;
		this.fileName = desc.fileName;
		this.theoryName = desc.theoryName;
		this.formulaName = desc.formulaName;
		this.position = new Position (desc.position.line, desc.position.character);
		this.status = desc.status;
		this.pvsContextFolder = pvsContextFolder;
		const range: Range = new Range(
			new Position(desc.position.line - 1, 0),
			new Position(desc.position.line, 0)
		);
		this.refreshLabel();
		this.command = {
			title: "Formula selected",
			command: "explorer.didSelectTheorem",
			arguments: [
				Uri.file(path.join(pvsContextFolder, this.fileName)),
				range
			]
		};
	}
	getPosition(): Position { return this.position; }
	getFileName(): string { return this.fileName; }
	getTheoryName(): string { return this.theoryName; }
	getFormulaName(): string { return this.formulaName; }
	setStatus (status: string) {
		this.status = status;
		this.refreshLabel();
	}
	private refreshLabel() {
		this.status = this.status || "needs typechecking"
		switch (this.status) {
			case "proved": {
				this.label = `✅ ${this.formulaName} (${this.status})`;
				break;
			}
			case "unfinished": { // proof attempted but failed
				this.label = `❗ ${this.formulaName} (${this.status})`;
				break;
			}
			case "unchecked": // proof was successful, but needs to be checked again because of changes in the theories
			case "untried": {// proof has not been attempted yet
				this.label = `❄️ ${this.formulaName} (${this.status})`;
				break;
			}
			default: {
				this.label = `✨ ${this.formulaName} (${this.status})`;
			}
		}
	}
}
//-- overviews
class TheoremsOverviewItem extends OverviewItem {
	constructor(desc: { pvsContextFolder: string, fileName: string }) {
		super("THEOREMS", desc);
	}
}
class TccsOverviewItem extends OverviewItem {
	constructor(desc: { pvsContextFolder: string, fileName: string }) {
		super("TCCS", desc);
	}
}
//-- Items
class TheoremItem extends FormulaItem {
	constructor(desc: FormulaDescriptor, pvsContextFolder: string) {
		super("theorem", desc, pvsContextFolder);
	}
}
class TccItem extends FormulaItem {
	constructor(desc: FormulaDescriptor, pvsContextFolder: string) {
		super("tcc", desc, pvsContextFolder);
	}
}


/**
 * Data provider for PVS Explorer view
 */
export class VSCodePvsExplorer implements TreeDataProvider<TreeItem> {
	private pvsLibrariesPath: string = null;
	private pvsContextFolder: string = null;

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

	// TODO: remove theories, theoryMap, tccs and tccsOverview
	private theories: TheoryItem[] = [];	
	// private theoryMap: { [ theoryName: string ]: {
	// 	theorems: string[],
	// 	axioms: string[]
	//  } } = {};
	// private tccsOverview: { [ theoryName: string ]: TccsOverviewItem } = {};
	// private tccs: { [ theoryName: string ]: (TccItem | NoTccItem)[] } = {};

	private getPvsPath (): string {
		return workspace.getConfiguration().get("pvs.path");
	}

	/**
	 * @constructor
	 * @param client Language client 
	 * @param providerView VSCode view served by the data provider
	 */
	constructor(client: LanguageClient, providerView: string) {
		this.client = client;
		this.providerView = providerView;
		const pvsPath: string = this.getPvsPath();
		this.pvsLibrariesPath = path.join(pvsPath, "lib");
		// register tree view.
		// use window.createTreeView instead of window.registerDataProvider -- this allows to perform UI operations programatically. 
		// window.registerTreeDataProvider(this.providerView, this);
		this.view = window.createTreeView(this.providerView, { treeDataProvider: this });

		this.client.onNotification("pvs.context.theories-status.update", (theoriesMap: TheoriesMap) => {
			this.updateFormulae(theoriesMap);
        });
	}

	updateFormulae (theoriesMap: TheoriesMap): void {
		if (theoriesMap && theoriesMap.theoriesStatusMap) {
			const theoryNames: string[] = Object.keys(theoriesMap.theoriesStatusMap);
			theoryNames.forEach((theoryName: string) => {
				const theoryItem: TheoryItem = this.getTheoryItem(theoryName);
				const theoryStatus: TheoryStatus = theoriesMap.theoriesStatusMap[theoryName];
				// update status of tccs and theorems
				const formulaNames: string[] = Object.keys(theoryStatus.theorems);
				formulaNames.forEach((formulaName: string) => {
					const formulaDescriptor: FormulaDescriptor = theoriesMap.theoriesStatusMap[theoryName].theorems[formulaName];
					if (formulaDescriptor.isTcc) {
						theoryItem.tccsOverview.updateStatus(formulaDescriptor);
					} else {
						theoryItem.theoremsOverview.updateStatus(formulaDescriptor);
					}
					// if (theoremItem) {
					// 	theoremItem.setStatus(theoryStatus.theorems[formulaName].status);
					// } else {
					// 	// check if tcc

					// 	const theorem: TheoremItem = new TheoremItem(desc, theoriesMap.pvsContextFolder);
					// 	theoryItem.theoremsOverview.
					// 	const tccItem: TccItem = theoryItem.tccsOverview.getTccItem(formulaName);
					// 	if (tccItem) {
					// 		tccItem.setStatus(theoryStatus.theorems[formulaName].status);
					// 	} else {
					// 		console.error(`Error: trying to update theorem ${formulaName} not listed in Theory Explorer :/`);
					// 	}
					// }
				});
				theoryItem.refreshLabel();
			});
			this.refreshView();
		}
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
	}
	public getTheoryItem (theoryName: string): TheoryItem {
		if (this.theories) {
			let candidates: TheoryItem[] = this.theories.filter((item: TheoryItem) => {
				return item.theoryName === theoryName;
			});
			if (candidates && candidates.length === 1) {
				return candidates[0];
			}
		}
		return null;
	}
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
						let uri: Uri = Uri.file(path.join(ans.pvsContextFolder, ans.fileNames[i]));
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
	/**
	 * Handlers for messages received from the server
	 */
	private installHandlers() {
		// server.response.list-theories events are automatically sent by the server when the context folder changes
		this.client.onRequest("server.response.list-theories", (ans: TheoryList) => {
			// update the list of theories
			if (ans && ans.pvsContextFolder !== this.pvsContextFolder) {
				this.pvsContextFolder = ans.pvsContextFolder;
				this.resetView();
			}
			if (ans && ans.theories) {
				let theoryNames: string[] = Object.keys(ans.theories);
				let theoryItems: TheoryItem[] = [];
				for (const i in theoryNames) {
					const theoryName: string = theoryNames[i];
					// check if the item already exists
					const item: TheoryItem = this.getTheoryItem(theoryName);
					if (item) {
						theoryItems.push(item);
					} else {
						let position: Position = new Position(ans.theories[theoryName].position.line, ans.theories[theoryName].position.character);
						const fileName: string = ans.theories[theoryName].fileName;
						const theoryItem: TheoryItem = new TheoryItem(theoryName, fileName, position, ans.pvsContextFolder, TreeItemCollapsibleState.Collapsed);
						theoryItems.push(theoryItem);
					}
				}
				this.theories = theoryItems;
			}
			this.refreshView();
		});
		this.client.onRequest("server.response.list-theorems", (theoriesMap: TheoriesMap) => {
			// add theorems
			if (theoriesMap && theoriesMap.pvsContextFolder !== this.pvsContextFolder) {
				this.pvsContextFolder = theoriesMap.pvsContextFolder;
				this.resetView();
			}
			this.updateFormulae(theoriesMap);
		});
		// this.client.onRequest("server.response.show-tccs", async (ans: TheoriesMap) => {
		// 	// this.updateTccs(ans);
		// 	if (ans && ans.theoriesStatusMap) {
		// 		this.updateFormulae(ans.theoriesStatusMap);
		// 		const theoryNames: string[] = Object.keys(ans.theoriesStatusMap);
		// 		// Open .tccs file when the command is show-tccs
		// 		for (const i in theoryNames) {
		// 			const fileName: string = path.join(ans.pvsContextFolder, `${theoryNames[i]}.tccs`);
		// 			const document: TextDocument = await workspace.openTextDocument(fileName);
		// 			window.showTextDocument(document, window.activeTextEditor.viewColumn + 1, true);
		// 		}
		// 	}
		// });
		this.client.onRequest("server.response.typecheck-file-and-show-tccs", (theoriesMap: TheoriesMap) => {
			this.updateFormulae(theoriesMap);
		});
		this.client.onRequest("server.response.typecheck-prove-and-show-tccs", (theoriesMap: TheoriesMap) => {
			this.updateFormulae(theoriesMap);
		});
		this.client.onRequest("server.response.typecheck-all-and-show-tccs", (theoriesMap: TheoriesMap) => {
			this.updateFormulae(theoriesMap);
		});
	}

	/**
	 * Shows the list of theories in the tree view
	 * @param uri PVS file
	 */
	private async showTheories (): Promise<void> {
		this.resetView();
		await this.loadPvsFiles();
		this.client.sendRequest("pvs.list-theories");
	}
	
	/**
	 * Handler activation function
	 * @param context Client context 
	 */
	activate(context: ExtensionContext) {
		// click on show-tccs (button shown inline with the tree element) to show tccs associated with the theory
		// let cmd = commands.registerCommand('cmd.show-tccs', (resource: TreeItem) => {
		// 	if (resource && resource.contextValue === "TCCS") {
		// 		const desc: TccsOverviewItem = <TccsOverviewItem> resource;
		// 		this.client.sendRequest('pvs.typecheck-theory-and-show-tccs', {
		// 			theoryName: desc.theoryName,
		// 			fileName: desc.fileName
		// 		});
		// 	}
		// });
		// context.subscriptions.push(cmd);
		let cmd = commands.registerCommand('explorer.typecheck-file-and-show-tccs', (resource) => {
			if (resource) {
				if (resource.fileName) {
					this.client.sendRequest('pvs.typecheck-file-and-show-tccs', resource.fileName);
				} else if (resource.path) {
					this.client.sendRequest('pvs.typecheck-file-and-show-tccs', resource.path);
				} else {
					window.showErrorMessage("Error while trying to execute explorer.typecheck-file-and-show-tccs: resource does not provide filename information");
				}
			} else {
				window.showErrorMessage("Error while trying to execute explorer.typecheck-file-and-show-tccs: resource is null");
			}
		});
		context.subscriptions.push(cmd);

		// click on prove tccs to trigger tcp
		cmd = commands.registerCommand('explorer.typecheck-prove', (resource: TccsOverviewItem) => {
			if (resource) {
				if (resource.fileName) {
					this.client.sendRequest('pvs.typecheck-prove-and-show-tccs', resource.fileName);
				} else {
					window.showErrorMessage("Error while trying to execute explorer.typecheck-prove: fileName is null");
				}
			} else {
				window.showErrorMessage("Error while trying to execute explorer.typecheck-prove: resource is null");
			}
		});
		context.subscriptions.push(cmd);

		// click on prove tccs to trigger tcp
		cmd = commands.registerCommand('explorer.prove-formula', (resource: FormulaItem) => {
			if (resource) {
				// TODO: modify server APIs so that extension is included in filename
				const data = {
					fileName: fsUtils.getFilename(resource.getFileName(), { removeFileExtension: true }),
					formulaName: resource.getFormulaName(),
					theoryName: resource.getTheoryName(),
					line: resource.getPosition().line,
					fileExtension: ".pvs"
				};
				commands.executeCommand("terminal.pvs.prove", data);
			} else {
				window.showErrorMessage("Error while trying to execute explorer.prove-formula: resource is null");
			}
		});
		context.subscriptions.push(cmd);
		
		// typecheck-all command from explorer menu
		cmd = commands.registerCommand('explorer.typecheck-all', () => {
			this.client.sendRequest('pvs.typecheck-all-and-show-tccs');
		});
		context.subscriptions.push(cmd);
		
		// // click on a tcc symbol to open the file and highlight the tcc name in the theory
		// cmd = commands.registerCommand('explorer.didSelectTccSymbol', async (uri: Uri, range: Range) => {
		// 	// window.showInformationMessage("theory selected: " + theoryName + " " + fileName + "(" + position.line + ", " + position.character + ")");
		// 	commands.executeCommand('vscode.open', uri, {
		// 		viewColumn: window.activeTextEditor.viewColumn, // do not open new tabs
		// 		selection: range
		// 	});
		// });
		// context.subscriptions.push(cmd);

		// click on a tcc formula to open the file and highlight the tcc name in the theory
		cmd = commands.registerCommand('explorer.didSelectTcc', async (uri: Uri, range: Range) => {
			// window.showInformationMessage("theory selected: " + theoryName + " " + fileName + "(" + position.line + ", " + position.character + ")");
			commands.executeCommand('vscode.open', uri, {
				viewColumn: window.activeTextEditor.viewColumn, // do not open new tabs
				selection: range
			});
		});
		context.subscriptions.push(cmd);

		// click on a theorem to open the file and highlight the theorem in the theory
		cmd = commands.registerCommand('explorer.didSelectTheorem', async (uri: Uri, range: Range) => {
			// window.showInformationMessage("theory selected: " + theoryName + " " + fileName + "(" + position.line + ", " + position.character + ")");
			commands.executeCommand('vscode.open', uri, {
				viewColumn: window.activeTextEditor.viewColumn, // do not open new tabs
				selection: range
			});
		});
		context.subscriptions.push(cmd)

		// click on a tcc formula to open the .tccs file
		cmd = commands.registerCommand('explorer.didSelectTccFormula', async (uri: Uri, range: Range) => {
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
				children = [
					desc.tccsOverview,//this.tccsOverview[desc.theoryName], // todo: tccs attribute within theoryitem
					desc.theoremsOverview
				];
			} else if (element.contextValue === "TCCS") {
				// tcc list
				const desc: TccsOverviewItem = <TccsOverviewItem> element;
				children = desc.listFormulae();
			} else if (element.contextValue === "tcc") {
				// console.log('tcc');
			} else if (element.contextValue === "theorem") {
				// console.log('theorem');
			} else if (element.contextValue === "THEOREMS") {
				const desc: TheoremsOverviewItem = <TheoremsOverviewItem> element;
				children = desc.listFormulae();
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
