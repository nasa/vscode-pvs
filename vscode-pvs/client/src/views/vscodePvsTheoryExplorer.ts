import { ExtensionContext, TreeItemCollapsibleState, commands, window, TextDocument, 
			Uri, Range, Position, TreeItem, Command, EventEmitter, Event,
			TreeDataProvider, workspace, MarkdownString, TreeView } from 'vscode';
import { LanguageClient } from 'vscode-languageclient';
import { TccDescriptorArray, TccDescriptor, PeekDefinitionCommand, PVS_LIBRARY_FILES,
			TheoryMap, FileList, TheoryList, TheoriesMap, TheoremList, TheoremDescriptor,
			PvsTypecheckerResponse, TheoremsStatus, TheoriesStatusMap,
			TheoryStatus} from '../common/serverInterface';
import * as path from 'path';
import { log } from '../utils/vscode-utils';
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
		this.tooltip = "Click to open " + theoryName;
		this.command = {
			title: "Theory selected",
			command: "explorer.didSelectTheory",
			arguments: [ theoryName, fileName, position, pvsContextFolder ]
		};
		this.theoremsOverview = new TheoremsOverviewItem();
		this.tccsOverview = new TccsOverviewItem({ fileName });
	}
	setTccTotal (n: number) {
		// update label
		this.label = this.theoryName + " (tccs: " + n + ")";
		// update collapsible state
		this.collapsibleState = (n > 0) ? TreeItemCollapsibleState.Collapsed
									: TreeItemCollapsibleState.None;
	}
	getTheoremItem (fname: string): TheoremItem {
		return this.theoremsOverview.getTheoremItem(fname);
	}
}

//-- tccs
class TccItem extends TreeItem {
	contextValue: string = "tcc";
	pvsContextFolder: string;
	fileName: string;
	theoryName: string;
	uri: Uri;
	range: Range;
	tcc: TccDescriptor;
	command: Command;
	status: string;
	constructor(tcc: TccDescriptor, theoryName: string, fileName: string, pvsContextFolder: string) {
		super(tcc.formulaName, TreeItemCollapsibleState.Collapsed);
		// this.label = + " ( " + tcc.status + " )"
		this.tcc = tcc;
		this.theoryName = theoryName;
		this.fileName = fileName;
		this.pvsContextFolder = pvsContextFolder;
		this.uri = Uri.file(path.join(pvsContextFolder, `${theoryName}.tccs`));
		this.range = new Range (
			new Position(tcc.line - 1, 0),
			new Position(tcc.line - 1, tcc.formulaName.length)
		);
		this.command = {
			title: "Tcc selected",
			command: "explorer.didSelectTcc",
			arguments: [ this.uri, this.range ]
		};
	}
	setStatus (status: string) {
		this.status = status;
		this.updateLabel();
	}
	private updateLabel() {
		this.label = (this.status) ? `${this.tcc.formulaName} (${this.status})` : this.tcc.formulaName;
	}
}
class NoTccItem extends TreeItem {
	contextValue: string = "no-tcc";
	constructor() {
		super("No TCCs", TreeItemCollapsibleState.None);
	}
}
// class SymbolItem extends TreeItem {
// 	contextValue: string = "tcc.symbol";
// 	symbol: string;
// 	range: Range;
// 	uri: Uri;
// 	constructor(symbol: string, uri: Uri, line: number, character: number) {
// 		super("obligation for " + symbol + " ( Ln " + line + ", Col " + character + " )", TreeItemCollapsibleState.None);
// 		this.symbol = symbol;
// 		this.range = new Range(
// 			new Position(line - 1, // in vscode line numbers start from 0, in pvs they start from 1
// 						 character), // in vscode column numbers start from 1, in pvs they start from 0
// 			new Position(line - 1, character + symbol.length)
// 		);
// 		this.uri = uri;
// 		this.command = {
// 			title: "Tcc symbol selected",
// 			command: "explorer.didSelectTccSymbol",
// 			arguments: [ this.uri, this.range ]
// 		};
// 	}
// }
// class TccFormulaItem extends TreeItem {
// 	contextValue: string = "tcc.formula";
// 	private msg: string;
// 	private formulaName: string;
// 	private uri: Uri;
// 	private line: number;
// 	constructor (msg: string, formulaName: string, uri: Uri, line: number) {
// 		super(msg, TreeItemCollapsibleState.None);
// 		this.msg = msg;
// 		this.formulaName = formulaName;
// 		this.uri = uri;
// 		this.line = line;
// 		const range: Range = new Range (
// 			new Position(line - 1, 0),
// 			new Position(line - 1, formulaName.length)
// 		);
// 		this.command = {
// 			title: "Tcc formula selected",
// 			command: "explorer.didSelectTccFormula",
// 			arguments: [ this.uri, range ]
// 		};
// 	}
// }
class TccsOverviewItem extends TreeItem {
	contextValue: string = "TCCs";
	tccs: TccItem[] = [];
	fileName: string;
	constructor(desc: { fileName: string }) {
		super("TCCs", TreeItemCollapsibleState.None);
		this.label = "tccs";
		this.fileName = desc.fileName;
	}
	setTccs (tccs: TccItem[]) {
		this.tccs = tccs;
		if (tccs && tccs.length > 0) {
			this.setTotal(tccs.length);
			this.collapsibleState = TreeItemCollapsibleState.Expanded;
		} else {
			this.setTotal(0);
			this.collapsibleState = TreeItemCollapsibleState.None;
		}
	}
	getTccItem (formulaName: string): TccItem {
		const candidates: TccItem[] = this.tccs.filter((elem: TccItem) => {
			return elem.tcc.formulaName === formulaName;
		});
		if (candidates && candidates.length === 1) {
			return candidates[0];
		}
		return null;
	}
	listTccs (): TheoremItem[] {
		const lst: TheoremItem[] = Object.keys(this.tccs).map(key => {
			return this.tccs[key];
		});
		return lst;
	}
	setTotal (n: number, overview?: string) {
		// update label
		this.label = overview ? `tccs ( ${overview} )` : `tccs ( ${n} )`;
		// update collapsible state
		this.collapsibleState = (n > 0) ? TreeItemCollapsibleState.Collapsed
									: TreeItemCollapsibleState.None;
	}
}

//-- theorems
class TheoremsOverviewItem extends TreeItem {
	contextValue: string = "THEOREMS";
	theorems: TheoremItem[] = [];
	constructor() {
		super("THEOREMS", TreeItemCollapsibleState.None);
		this.label = "theorems";
	}
	setTheorems (theorems: TheoremItem[]) {
		this.theorems = theorems;
		if (theorems && theorems.length > 0) {
			this.setTotal(theorems.length);
			this.collapsibleState = TreeItemCollapsibleState.Expanded;
		} else {
			this.setTotal(0);
			this.collapsibleState = TreeItemCollapsibleState.None;
		}
	}
	getTheoremItem (formulaName: string): TheoremItem {
		const candidates: TheoremItem[] = this.theorems.filter((elem: TheoremItem) => {
			return elem.desc.formulaName === formulaName;
		});
		if (candidates && candidates.length === 1) {
			return candidates[0];
		}
		return null;
	}
	listTheorems (): TheoremItem[] {
		const lst: TheoremItem[] = Object.keys(this.theorems).map(key => {
			return this.theorems[key];
		});
		return lst;
	}
	setTotal (n: number, overview?: string) {
		// update label
		this.label = overview ? `theorems ( ${overview} )` : `theorems ( ${n} )`;
		// update collapsible state
		this.collapsibleState = (n > 0) ? TreeItemCollapsibleState.Collapsed
									: TreeItemCollapsibleState.None;
	}
}
class TheoremItem extends TreeItem {
	contextValue: string = "theorem";
	desc: TheoremDescriptor;
	pvsContextFolder: string;
	constructor(desc: TheoremDescriptor, pvsContextFolder: string) {
		super("theorem", TreeItemCollapsibleState.None);
		this.desc = desc;
		this.pvsContextFolder = pvsContextFolder;
		const range: Range = new Range(
			new Position(desc.position.line - 1, 0),
			new Position(desc.position.line, 0)
		);
		this.updateLabel();
		this.command = {
			title: "Theorem selected",
			command: "explorer.didSelectTheorem",
			arguments: [
				Uri.file(path.join(pvsContextFolder, this.desc.fileName)),
				range
			]
		};
	}
	setStatus (status: string) {
		this.desc.status = status;
		this.updateLabel();
	}
	private updateLabel() {
		this.label = (this.desc.status) ? `${this.desc.formulaName} (${this.desc.status})` : this.desc.formulaName;
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
	private currentContext: string = null;

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
	private theoryMap: { [ theoryName: string ]: {
		theorems: string[],
		axioms: string[]
	 } } = {};
	private tccsOverview: { [ theoryName: string ]: TccsOverviewItem } = {};
	private tccs: { [ theoryName: string ]: (TccItem | NoTccItem)[] } = {};

	private getPvsPath (): string {
		const mode: string = workspace.getConfiguration().get("pvs.zen-mode");
		if (mode === "pvs-6" || mode === "pvs-7") {
			return workspace.getConfiguration().get(`pvs.zen-mode:${mode}-path`);
		}
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

		this.client.onNotification("pvs.context.theorems.update", (theoremList: TheoremList) => {
			this.setTheorems(theoremList);
			this.refreshView();
		});
		this.client.onNotification("pvs.context.theories-status.update", (theoriesStatusMap: TheoriesStatusMap) => {
			this.updateFormulae(theoriesStatusMap);
			this.refreshView();
        });
	}

	updateFormulae (theoriesStatusMap: TheoriesStatusMap): void {
		if (theoriesStatusMap) {
			const theoryNames: string[] = Object.keys(theoriesStatusMap);
			theoryNames.forEach((theoryName: string) => {
				const theoryItem: TheoryItem = this.getTheoryItem(theoryName);
				const theoryStatus: TheoryStatus = theoriesStatusMap[theoryName];
				// add tccs
				const tccs: TccItem[] = theoryStatus.tccs.map((tcc: TccDescriptor) => {
					return new TccItem(tcc, "theoryName", "fileName", "pvsContextFolder");
				})
				theoryItem.tccsOverview.setTccs(tccs);
				// update status of tccs and theorems
				const formulaNames: string[] = Object.keys(theoryStatus.theorems);
				formulaNames.forEach((formulaName: string) => {
					const theoremItem: TheoremItem = theoryItem.theoremsOverview.getTheoremItem(formulaName);
					if (theoremItem) {
						theoremItem.setStatus(theoryStatus.theorems[formulaName].status);
					} else {
						const tccItem: TccItem = theoryItem.tccsOverview.getTccItem(formulaName);
						if (tccItem) {
							tccItem.setStatus(theoryStatus.theorems[formulaName].status);
						} else {
							console.error(`Error: trying to update theorem ${formulaName} not listed in Theory Explorer :/`);
						}
					}
				});
			});
			this.refreshView();
		}
	}
	setTheorems (theoremList: TheoremList): void {
		if (theoremList) {
			const theoryMap: { [ theoryName: string ]: TheoremItem[] } = {};
			theoremList.theorems.forEach((desc: TheoremDescriptor) => {
				const theorem: TheoremItem = new TheoremItem(desc, theoremList.pvsContextFolder);
				theoryMap[desc.theoryName] = theoryMap[desc.theoryName] || [];
				theoryMap[desc.theoryName].push(theorem);
			});
			Object.keys(theoryMap).forEach(theoryName => {
				const theoryItem: TheoryItem = this.getTheoryItem(theoryName);
				theoryItem.theoremsOverview.setTheorems(theoryMap[theoryName]);
			});
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

	// private updateTccs(ans: TheoriesMap) {
	// 	// the server will create a .tccs file containing the tccs
	// 	if (ans && ans.theoriesStatusMap) {
	// 		const theories: string[] = Object.keys(ans.theoriesStatusMap);
	// 		let tccFileContent: string = "";
	// 		for (let i in theories) {
	// 			let theoryName: string = theories[i];
	// 			let theoryItem: TheoryItem = this.getTheoryItem(theoryName);
	// 			if (theoryItem !== null) {
	// 				// populate the list of tccs
	// 				let tccs: TccDescriptor[] = ans.theoriesStatusMap[theoryName].tccs;
	// 				let overview = { };
	// 				if (tccs && tccs.length > 0) {
	// 					this.tccsOverview[theoryName] = new TccsOverviewItem(theoryItem.theoryName, theoryItem.fileName);
	// 					this.tccs[theoryName] = tccs.map((tcc: TccDescriptor) => {
	// 						// compile tccFileContent and TccItem
	// 						// tccFileContent += tcc.content;
	// 						// if (!overview[tcc.status]) {
	// 						// 	overview[tcc.status] = 0;
	// 						// }
	// 						// overview[tcc.status]++;
	// 						return new TccItem(tcc, theoryName, theoryItem.fileName, theoryItem.pvsContextFolder);
	// 					});
	// 				} else {
	// 					this.tccsOverview[theoryName] = null;
	// 					this.tccs[theoryName] = null;
	// 				}
	// 				const ntccs: number = (tccs && tccs.length) ? tccs.length : 0;
	// 				// update theory label to indicate the number of tccs generated for the theory
	// 				theoryItem.setTccTotal(ntccs);
	// 				// update tcc overview to indicate the number of tccs generated for the theory
	// 				if (ntccs > 0 && this.tccsOverview[theoryName]) {
	// 					let overviewArray: string[] = [ ];
	// 					Object.keys(overview).forEach((key) => {
	// 						overviewArray.push(overview[key] + " " + key);
	// 					});
	// 					this.tccsOverview[theoryName].setTccTotal(tccs.length, overviewArray.join(", "));
	// 				}
	// 			}
	// 		}
	// 	}
	// 	this.refreshView();
	// }

	/**
	 * Handlers for messages received from the server
	 */
	private installHandlers() {
		// server.response.list-theories events are automatically sent by the server when the context folder changes
		this.client.onRequest("server.response.list-theories", (ans: TheoryList) => {
			// add theories
			if (ans && ans.pvsContextFolder !== this.currentContext) {
				this.currentContext = ans.pvsContextFolder;
				this.resetView();
			}
			if (ans && ans.theories) {
				let theoryNames: string[] = Object.keys(ans.theories);
				let theoryItems: TheoryItem[] = [];
				let theoryMap: { [ theoryName: string ]: {
					theorems: string[],
					axioms: string[]
				} } = {}
				for (const i in theoryNames) {
					const theoryName: string = theoryNames[i];
					// check if theoryName was added to the file
					if (this.theoryMap[theoryName]) {
						// keep current entry
						const item: TheoryItem = this.getTheoryItem(theoryName);
						theoryItems.push(item);
						theoryMap[theoryName] = this.theoryMap[theoryName];
					} else {
						// add new entry
						let position: Position = new Position(ans.theories[theoryName].position.line, ans.theories[theoryName].position.character);
						let fileName: string = ans.theories[theoryName].fileName;
						let pvsContextFolder: string = ans.pvsContextFolder;
						theoryMap[theoryName] = {
							theorems: [],
							axioms: []
						}; // TODO: list theorems & axioms
						const theoryItem: TheoryItem = new TheoryItem(theoryName, fileName, position, pvsContextFolder, TreeItemCollapsibleState.Collapsed);
						theoryItems.push(theoryItem);
					}
				}
				this.theories = theoryItems;
				this.theoryMap = theoryMap;
			}
			this.refreshView();
		});
		this.client.onRequest("server.response.list-theorems", (theoremList: TheoremList) => {
			// add theorems
			if (theoremList && theoremList.pvsContextFolder !== this.currentContext) {
				this.currentContext = theoremList.pvsContextFolder;
				this.resetView();
			}
			this.setTheorems(theoremList);
			this.refreshView();
		});
		this.client.onRequest("server.response.typecheck-file", (ans: PvsTypecheckerResponse) => {
			// update theorems status
			console.log(ans);
		});
		this.client.onRequest("server.response.show-tccs", async (ans: TheoriesMap) => {
			// this.updateTccs(ans);
			if (ans && ans.theoriesStatusMap) {
				this.updateFormulae(ans.theoriesStatusMap);
				const theoryNames: string[] = Object.keys(ans.theoriesStatusMap);
				// Open .tccs file when the command is show-tccs
				for (const i in theoryNames) {
					const fileName: string = path.join(ans.pvsContextFolder, `${theoryNames[i]}.tccs`);
					const document: TextDocument = await workspace.openTextDocument(fileName);
					window.showTextDocument(document, window.activeTextEditor.viewColumn + 1, true);
				}
			}
		});
		this.client.onRequest("server.response.typecheck-file-and-show-tccs", (ans: TheoriesMap) => {
			// this.updateTccs(ans);
			if (ans && ans.theoriesStatusMap) {
				this.updateFormulae(ans.theoriesStatusMap);
			}
		});
		this.client.onRequest("server.response.typecheck-prove-and-show-tccs", (ans: TheoriesMap) => {
			// this.updateTccs(ans);
			if (ans && ans.theoriesStatusMap) {
				this.updateFormulae(ans.theoriesStatusMap);
			}
		});
		this.client.onRequest("server.response.typecheck-all-and-show-tccs", (ans: TheoriesMap) => {
			// this.updateTccs(ans);
			if (ans && ans.theoriesStatusMap) {
				this.updateFormulae(ans.theoriesStatusMap);
			}
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
		// 	if (resource && resource.contextValue === "TCCs") {
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
			if (resource && resource.contextValue === "TCCs") {
				this.client.sendRequest('pvs.typecheck-prove-and-show-tccs', resource.fileName);
			} else {
				window.showErrorMessage("Error while trying to execute explorer.typecheck-prove: resource is null");
			}
		});
		context.subscriptions.push(cmd);

		// click on prove tccs to trigger tcp
		cmd = commands.registerCommand('explorer.prove-tcc', (resource: TccItem) => {
			if (resource && resource.contextValue === "tcc") {
				// TODO: modify server APIs so that extension is included in filename
				const data = {
					fileName: fsUtils.getFilename(resource.fileName, { removeFileExtension: true }),
					formulaName: resource.tcc.formulaName,
					theoryName: resource.theoryName,
					line: resource.tcc.line,
					fileExtension: ".pvs"
				};
				commands.executeCommand("terminal.pvs.prove", data);
			} else {
				window.showErrorMessage("Error while trying to execute explorer.prove-tcc: resource is null");
			}
		});
		context.subscriptions.push(cmd);

		// click on prove tccs to trigger tcp
		cmd = commands.registerCommand('explorer.prove-theorem', (resource: TreeItem) => {
			if (resource && resource.contextValue === "theorem") {
				const theorem: TheoremItem = <TheoremItem> resource;
				const data = {
					fileName: theorem.desc.fileName,
					formulaName: theorem.desc.formulaName,
					theoryName: theorem.desc.theoryName,
					line: theorem.desc.position.line,
					fileExtension: ".pvs"
				};
				commands.executeCommand("terminal.pvs.prove", data);
			} else {
				window.showErrorMessage("Error while trying to execute explorer.prove-tcc: resource is null");
			}
		});
		context.subscriptions.push(cmd);
		
		// typecheck-all command from explorer menu
		cmd = commands.registerCommand('explorer.typecheck-all', () => {
			this.client.sendRequest('pvs.typecheck-all-and-show-tccs');
		});
		context.subscriptions.push(cmd);
		
		// click on a tcc symbol to open the file and highlight the tcc name in the theory
		cmd = commands.registerCommand('explorer.didSelectTccSymbol', async (uri: Uri, range: Range) => {
			// window.showInformationMessage("theory selected: " + theoryName + " " + fileName + "(" + position.line + ", " + position.character + ")");
			commands.executeCommand('vscode.open', uri, {
				viewColumn: window.activeTextEditor.viewColumn, // do not open new tabs
				selection: range
			});
		});
		context.subscriptions.push(cmd);

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
			} else if (element.contextValue === "TCCs") {
				// tcc list
				const desc: TccsOverviewItem = <TccsOverviewItem> element;
				children = desc.listTccs();
			} else if (element.contextValue === "tcc") {
				// tcc
				const desc: TccItem = <TccItem> element;
				const tcc: TccDescriptor = desc.tcc;
				const fileName: string = desc.fileName;
				const pvsContextFolder: string = desc.pvsContextFolder;
				const uri: Uri = Uri.file(path.join(pvsContextFolder, `${fileName}.pvs`));
				const tccFile: Uri = Uri.file(path.join(pvsContextFolder, `${desc.theoryName}.tccs`));
				children = null;
				// [
				// 	new SymbolItem(tcc.symbolName, uri, tcc.line, tcc.symbolCharacter),
				// 	new TccFormulaItem(null, tcc.formulaName, tccFile, tcc.line)
				// ];
			} else if (element.contextValue === "theorem") {
				console.log('theorem');
			} else if (element.contextValue === "THEOREMS") {
				const desc: TheoremsOverviewItem = <TheoremsOverviewItem> element;
				children = desc.listTheorems();
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
