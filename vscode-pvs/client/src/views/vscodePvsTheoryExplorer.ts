/**
 * @module VSCodePvsTheoryExplorer
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
import { ExtensionContext, TreeItemCollapsibleState, commands, window, TextDocument, 
			Uri, Range, Position, TreeItem, Command, EventEmitter, Event,
			TreeDataProvider, workspace, TreeView, ViewColumn } from 'vscode';
import { LanguageClient } from 'vscode-languageclient';
import { FileList, FormulaDescriptor,
			TheoryDescriptor, ContextDescriptor } from '../common/serverInterface';
import * as path from 'path';
import { serverCommand, serverEvent } from '../common/serverInterface';

//-- theories
class Loading extends TreeItem {
	contextValue: string = "loading-animation";
	constructor () {
		super("Loading content...", TreeItemCollapsibleState.None);
	}
}
class TheoryItem extends TreeItem {
	contextValue: string = "theory";
	theoryName: string;
	command: Command;
	fileName: string;
	fileExtension: string;
	path: string;
	position: Position;
	protected tccsOverview: TccsOverviewItem;
	protected theoremsOverview: TheoremsOverviewItem;
	contextFolder: string;
	/**
	 * Constructor
	 */
	constructor (desc: { theoryName: string, fileName: string, fileExtension: string, contextFolder: string, position: Position, collapsibleState: TreeItemCollapsibleState }) {
		super(desc.theoryName, desc.collapsibleState);
		this.theoryName = desc.theoryName;
		this.fileName = desc.fileName;
		this.fileExtension = desc.fileExtension;
		this.position = desc.position;
		this.contextFolder = desc.contextFolder;
		this.path = path.join(desc.contextFolder, `${desc.fileName}${desc.fileExtension}`);
		this.tooltip = `Click to open ${desc.theoryName}`;
		this.command = {
			title: "Theory selected",
			command: "explorer.didSelectTheory",
			arguments: [ desc ]
		};
		this.theoremsOverview = new TheoremsOverviewItem(desc);
		this.tccsOverview = new TccsOverviewItem(desc);
		this.refreshLabel();
	}
	refreshLabel (opt?: { keepCollapsibleState?: boolean }) {
		opt = opt || {};
		// update label
		const nTheorems: number = this.theoremsOverview.getTotal();
		const nTccs: number = this.tccsOverview.getTotal();
		const n: number = nTccs + nTheorems;
		this.label = `${this.theoryName}`;// (${this.tccsOverview.tccs.length} tccs, ${this.theoremsOverview.theorems.length} theorems)`;
		// update collapsible state
		if (opt.keepCollapsibleState) {
			if (n > 0 && this.collapsibleState === TreeItemCollapsibleState.None) {
				this.collapsibleState = TreeItemCollapsibleState.Collapsed;
			}
		} else {
			this.collapsibleState = (n > 0) ? TreeItemCollapsibleState.Expanded : TreeItemCollapsibleState.None;
		}
	}
	getFormula (formulaName: string): FormulaItem {
		return this.theoremsOverview.getFormula(formulaName);
	}
	updateFormula (desc: FormulaDescriptor): void {
		this.theoremsOverview.updateStatus(desc);
	}
	getTcc (formulaName: string): FormulaItem {
		return this.tccsOverview.getFormula(formulaName);
	}
	updateTcc (desc: FormulaDescriptor): void {
		this.tccsOverview.updateStatus(desc);
	}
	getTheoremsOverview (): TheoremsOverviewItem {
		return this.theoremsOverview;
	}
	getTccsOverview (): TheoremsOverviewItem {
		return this.tccsOverview;
	}
}
class OverviewItem extends TreeItem {
	contextValue: string = "OverviewItem";
	fileName: string;
	contextFolder: string;
	theorems: FormulaItem[] = [];
	constructor(typeName: string, desc: { contextFolder: string, fileName: string }) {
		super(typeName, TreeItemCollapsibleState.None);
		this.contextValue = typeName;
		this.label = typeName.toLowerCase();
		this.fileName = desc.fileName;
		this.contextFolder = desc.contextFolder;
	}
	getFormula (formulaName: string): FormulaItem {
		const candidates: FormulaItem[] = this.theorems.filter((elem: FormulaItem) => {
			return elem.getFormulaName() === formulaName;
		});
		if (candidates && candidates.length === 1) {
			return candidates[0]
		}
		return null;
	}
	listFormulae (): FormulaItem[] {
		return this.theorems;
	}
	protected refreshLabel (opt?: { keepCollapsibleState?: boolean }) {
		this.label = `${this.contextValue.toLowerCase()} ( ${this.theorems.length} )`;
		opt = opt || {};
		if (opt.keepCollapsibleState) {
			if (this.theorems.length > 0 && this.collapsibleState === TreeItemCollapsibleState.None) {
				this.collapsibleState = TreeItemCollapsibleState.Collapsed;
			}
		} else {
			this.collapsibleState = (this.theorems.length > 0) ? 
				TreeItemCollapsibleState.Expanded
				: TreeItemCollapsibleState.None;
		}
	}
	updateStatus (desc: FormulaDescriptor, opt?: { keepCollapsibleState?: boolean }): void {
		if (desc) {
			const candidates: FormulaItem[] = this.theorems.filter((elem: FormulaItem) => {
				return elem.getFormulaName() === desc.formulaName;
			});
			if (candidates && candidates.length === 1) {
				if (desc.status) {
					candidates[0].setStatus(desc.status);
					this.refreshLabel(opt);
				}
			} else {
				const formulaType: string = (desc.isTcc) ? "tcc" : "theorem"
				this.theorems.push(new FormulaItem(formulaType, desc));
				this.refreshLabel(opt);
			} 
		}
	}
	getTotal (): number {
		return this.theorems.length;
	}
}
class FormulaItem extends TreeItem {
	contextValue: string = "formula";
	// desc: TheoremDescriptor;
	protected fileName: string;
	protected theoryName: string;
	protected formulaName: string;
	protected position: Position;
	protected status: string;
	protected contextFolder: string;
	constructor(typeName: string, desc: FormulaDescriptor) {
		super(typeName, TreeItemCollapsibleState.None);
		this.contextValue = typeName;
		this.fileName = desc.fileName;
		this.theoryName = desc.theoryName;
		this.formulaName = desc.formulaName;
		this.position = new Position (desc.position.line, desc.position.character);
		this.status = desc.status;
		this.contextFolder = desc.contextFolder;
		// const range: Range = new Range(
		// 	new Position(desc.position.line - 1, 0),
		// 	new Position(desc.position.line, 0)
		// );
		this.refreshLabel();
		this.command = {
			title: "Formula selected",
			command: "explorer.didSelectTheorem",
			arguments: [ desc ]
		};
	}
	getPosition(): Position { return this.position; }
	getFileName(): string { return this.fileName; }
	getTheoryName(): string { return this.theoryName; }
	getFormulaName(): string { return this.formulaName; }
	getContextFolder(): string { return this.contextFolder; }
	setStatus (status: string) {
		this.status = status;
		this.refreshLabel();
	}
	protected refreshLabel() {
		this.status = this.status || "unchecked"; //'\u{2705}'
		switch (this.status) {
			case "proved": 
			case "proved - incomplete":
			case "proved - by mapping":
			case "proved - complete": {
				this.label = `✅ ${this.formulaName} (${this.status})`;
				break;
			}
			case "unfinished": { // proof attempted but failed
				this.label = `❗ ${this.formulaName} (${this.status})`;
				break;
			}
			case null:
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
	constructor(desc: { contextFolder: string, fileName: string }) {
		super("THEOREMS", desc);
	}
}
class TccsOverviewItem extends OverviewItem {
	constructor(desc: { contextFolder: string, fileName: string }) {
		super("TCCS", desc);
	}
	// @overrides
	updateStatus (desc: FormulaDescriptor): void {
		super.updateStatus(desc, { keepCollapsibleState: true });
	}
}
//-- Items
class TheoremItem extends FormulaItem {
	constructor(desc: FormulaDescriptor) {
		super("theorem", desc);
	}
}
class TccItem extends FormulaItem {
	constructor(desc: FormulaDescriptor) {
		super("tcc", desc);
	}
}


/**
 * Data provider for PVS Explorer view
 */
export class VSCodePvsTheoryExplorer implements TreeDataProvider<TreeItem> {
	// protected pvsLibrariesPath: string = null;
	protected contextFolder: string = null;

	/**
	 * Events for updating the tree structure
	 */
	protected _onDidChangeTreeData: EventEmitter<TreeItem> = new EventEmitter<TreeItem>();
	readonly onDidChangeTreeData: Event<TreeItem> = this._onDidChangeTreeData.event;

	/**
	 * Language client for communicating with the server
	 */
	protected client: LanguageClient;

	/**
	 * Name of the view associated with the data provider
	 */
	protected providerView: string;
	
	protected view: TreeView<TreeItem>;
	protected theories: TheoryItem[] = [];	

	protected getPvsPath (): string {
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
		// const pvsPath: string = this.getPvsPath();
		// this.pvsLibrariesPath = path.join(pvsPath, "lib");
		// register tree view.
		// use window.createTreeView instead of window.registerDataProvider -- this allows to perform UI operations programatically. 
		// window.registerTreeDataProvider(this.providerView, this);
		this.view = window.createTreeView(this.providerView, { treeDataProvider: this, showCollapseAll: true });
	}

	getClient (): LanguageClient {
		return this.client;
	}

	setStatusProved(desc: { theoryName: string, formulaName: string }): void {
		if (desc) {
			const theoryItem: TheoryItem = this.getTheoryItem(desc.theoryName);
			if (theoryItem) {
				const formulaItem: FormulaItem = theoryItem.getFormula(desc.formulaName);
				if (formulaItem) {
					formulaItem.setStatus("proved");
					this.refreshView();
				}
			}
		}
	}

	// updateTccs (desc: { [ fname: string ]: })

	updateView (desc: ContextDescriptor): void {
		// if (desc && desc.contextFolder !== this.contextFolder) {
		// 	this.contextFolder = desc.contextFolder;
		// 	this.resetView();
		// }
		// if (desc && desc.theories) {
		// 	desc.theories.forEach((theory: TheoryDescriptor) => {
		// 		const theoryItem: TheoryItem = this.getTheoryItem(theory.theoryName);
		// 		if (theoryItem) {
		// 			// update status of tccs and theorems
		// 			theory.theorems.forEach((formula: FormulaDescriptor) => {
		// 				if (formula.isTcc) {
		// 					theoryItem.updateTcc(formula);
		// 				} else {
		// 					theoryItem.updateFormula(formula);
		// 				}
		// 			});
		// 			theoryItem.updateLabel();
		// 		}
		// 	});
		// 	this.refreshView();
		// }
		// update the list of theories
		if (desc && desc.contextFolder !== this.contextFolder) {
			this.contextFolder = desc.contextFolder;
			this.resetView();
		}
		if (desc && desc.theories) {
			let theoryItems: TheoryItem[] = [];
			for (let i = 0; i < desc.theories.length; i++) {
				const tdesc: TheoryDescriptor = desc.theories[i];
				// check if the item already exists
				let theoryItem: TheoryItem = this.getTheoryItem(tdesc.theoryName);
				if (!theoryItem) {
					theoryItem = new TheoryItem({
						theoryName: tdesc.theoryName,
						fileName: tdesc.fileName,
						fileExtension: tdesc.fileExtension,
						position: new Position(tdesc.position.line, tdesc.position.character),
						contextFolder: tdesc.contextFolder,
						collapsibleState: TreeItemCollapsibleState.Collapsed
					});
				}
				for (let i = 0; i < tdesc.theorems.length; i++) {
					const formula: FormulaDescriptor = tdesc.theorems[i];
					if (formula.isTcc) {
						theoryItem.updateTcc(formula);
					} else {
						theoryItem.updateFormula(formula);
					}
				}
				theoryItem.refreshLabel({ keepCollapsibleState: true });
				theoryItems.push(theoryItem);
			}
			const diff: TheoryItem[] = this.diff(this.theories, theoryItems);
			this.theories = theoryItems.concat(diff);
		}
		this.refreshView();
	}
	/**
	 * Refresh tree view
	 */
	protected refreshView(): void {
		this._onDidChangeTreeData.fire();
	}
	/**
	 * Resets the tree view
	 */
	protected resetView () {
		this.theories = [];
	}
	protected diff (v1: TheoryItem[], v2: TheoryItem[]): TheoryItem[] {
		if (!v1 || v1.length === 0) {
			return [];
		}
		if (!v2 || v2.length === 0) {
			return v1;
		}
		let diff: TheoryItem[] = v1.filter((item: TheoryItem) => {
			return !v2.some((elem) => {
				return elem.theoryName === item.theoryName;
			});
		});
		return diff;
	}
	getTheoryItem (theoryName: string): TheoryItem {
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
	getTheoryDescriptor (theoryName: string): TheoryDescriptor {
		if (this.theories) {
			let candidates: TheoryItem[] = this.theories.filter((item: TheoryItem) => {
				return item.theoryName === theoryName;
			});
			if (candidates && candidates.length === 1) {
				return {
					theoryName: theoryName,
					fileName: candidates[0].fileName,
					fileExtension: candidates[0].fileExtension,
					contextFolder: candidates[0].contextFolder,
					position: candidates[0].position
				};
			}
		}
		return null;
	}

	/**
	 * Handler activation function
	 * @param context Client context 
	 */
	activate(context: ExtensionContext) {
		// all commands in the form vscode-pvs.xxxx are handled by VSCodeEventsDispatcher

		// click on a tcc formula to open the file and highlight the tcc name in the theory
		context.subscriptions.push(commands.registerCommand('explorer.didSelectTcc', async (uri: Uri, range: Range) => {
			// window.showInformationMessage("theory selected: " + theoryName + " " + fileName + "(" + position.line + ", " + position.character + ")");
			window.showTextDocument(uri, {
				preserveFocus: true, 
				preview: true,
				viewColumn: ViewColumn.Beside,
				selection: range
			});
		}));

		// click on a theorem to open the file and highlight the theorem in the theory
		context.subscriptions.push(commands.registerCommand('explorer.didSelectTheorem', async (desc: FormulaDescriptor) => {
			// window.showInformationMessage("theory selected: " + theoryName + " " + fileName + "(" + position.line + ", " + position.character + ")");
			const uri: Uri = Uri.file(path.join(desc.contextFolder, `${desc.fileName}${desc.fileExtension}`));

			window.showTextDocument(uri, {
				preserveFocus: true, 
				viewColumn: window.activeTextEditor.viewColumn,
				selection: new Range(
					new Position(desc.position.line - 1, 0), // - 1 is because lines in vscode start from 0
					new Position(desc.position.line - 1, 1000) // highlight entire line
				)
			});
		}))

		// // click on a tcc formula to open the .tccs file
		// cmd = commands.registerCommand('explorer.didSelectTccFormula', async (desc: { theoryName: string, fileName: string, fileExtension: string, contextFolder: string, position: Position }) => {
		// 	const uri: Uri = Uri.file(path.join(desc.contextFolder, `${desc.fileName}${desc.fileExtension}`));
		// 	commands.executeCommand('vscode.open', uri, {
		// 		viewColumn: window.activeTextEditor.viewColumn, // do not open new tabs
		// 		selection: range
		// 	});
		// });
		// context.subscriptions.push(cmd);
		
		// click on a theory name to open the file and highlight the theory name in the file
		context.subscriptions.push(commands.registerCommand('explorer.didSelectTheory', async (desc: { theoryName: string, fileName: string, fileExtension: string, contextFolder: string, position: Position }) => {
			// window.showInformationMessage("theory selected: " + theoryName + " " + fileName + "(" + position.line + ", " + position.character + ")");
			const uri: Uri = Uri.file(path.join(desc.contextFolder, `${desc.fileName}${desc.fileExtension}`));
			window.showTextDocument(uri, {
				preserveFocus: true,
				preview: true,
				viewColumn: window.activeTextEditor.viewColumn,
				selection: new Range(
					new Position(desc.position.line - 1, 0), // - 1 is because lines in vscode start from 0
					new Position(desc.position.line - 1, 1000) // highlight entire line
				)
			});
		}));

		this.resetView();
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
					desc.getTccsOverview(),
					desc.getTheoremsOverview()
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
		} else {
			// root node: show the list of theories from the selected file
			return Promise.resolve(this.theories);
		}
	}

	getTreeItem(element: TreeItem): TreeItem {
		return element;
	}

}
