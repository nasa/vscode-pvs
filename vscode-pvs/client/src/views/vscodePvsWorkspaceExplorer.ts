/**
 * @module VSCodePvsWorkspaceExplorer
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
import { ExtensionContext, TreeItemCollapsibleState, commands, window,
			Uri, Range, Position, TreeItem, Command, EventEmitter, Event,
			TreeDataProvider, workspace, TreeView, ViewColumn } from 'vscode';
import { LanguageClient } from 'vscode-languageclient';
import { FormulaDescriptor, TheoryDescriptor, PvsContextDescriptor, ProofStatus, PvsFileDescriptor } from '../common/serverInterface';
import * as path from 'path';
import * as fsUtils from '../common/fsUtils';
import * as utils from '../common/languageUtils';

//-- files
class PvsFileItem extends TreeItem {
	contextValue: string = "pvs-file";
	theoryName: string;
	command: Command;
	fileName: string;
	fileExtension: string;
	path: string;
	contextFolder: string;
	theoriesOverview: TheoriesOverviewItem;
	/**
	 * Constructor
	 */
	constructor () {
		super("Loading file descriptor...");
		this.theoriesOverview = new TheoriesOverviewItem();
	}
	updateFileDescriptor (desc: PvsFileDescriptor, opt?: { tccDescriptor?: boolean }): void {
		this.label = this.fileName = desc.fileName;
		this.fileExtension = desc.fileExtension;
		this.contextFolder = desc.contextFolder;
		this.path = path.join(desc.contextFolder, `${desc.fileName}${desc.fileExtension}`);
		this.command = {
			title: "PVS file selected",
			command: "explorer.didSelectPvsFile",
			arguments: [ desc ]
		};
		this.theoriesOverview.updateTheories(desc.theories, opt);
		this.refreshLabel();		
	}
	refreshLabel (opt?: { keepCollapsibleState?: boolean }) {
		opt = opt || {};
		// update label
		this.label = `${this.fileName}${this.fileExtension}`;
		this.tooltip = this.path;
		// update collapsible state
		const n: number = this.theoriesOverview.theories ? this.theoriesOverview.theories.length : 0;
		if (opt.keepCollapsibleState) {
			if (n > 0 && this.collapsibleState === TreeItemCollapsibleState.None) {
				this.collapsibleState = TreeItemCollapsibleState.Collapsed;
			}
		} else {
			this.collapsibleState = (n > 0) ? TreeItemCollapsibleState.Expanded : TreeItemCollapsibleState.None;
		}
	}
	updateFormula (desc: FormulaDescriptor): void {
		this.theoriesOverview.updateFormula(desc);
	}
	sort (): void {
		this.theoriesOverview.sort();
	}
	getChildren (): TreeItem[] {
		return this.theoriesOverview.getChildren();
	}
}
//-- theories
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
	constructor (desc: TheoryDescriptor, collapsibleState?: TreeItemCollapsibleState) {
		super(desc.theoryName, collapsibleState);
		this.theoryName = desc.theoryName;
		this.fileName = desc.fileName;
		this.fileExtension = desc.fileExtension;
		this.contextFolder = desc.contextFolder;
		this.path = path.join(desc.contextFolder, `${desc.fileName}${desc.fileExtension}`);
		this.position = (desc.position) ? new Position(desc.position.line, desc.position.character) : new Position(0, 0);
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
		this.label = `${this.theoryName}  (${this.fileName}${this.fileExtension}, Ln ${this.position.line})`;
		this.tooltip = `theory ${this.theoryName}`;
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
	setTccs (desc: FormulaDescriptor[]): void {
		this.tccsOverview.setTccs(desc);
	}
	getTheoremsOverview (): TheoremsOverviewItem {
		return this.theoremsOverview;
	}
	getTccsOverview (): TheoremsOverviewItem {
		return this.tccsOverview;
	}
}
class LoadingItem extends TreeItem {
	contextValue: string = "loading-content";
	message: string = "Loading..."
	protected points: number = 3;
	protected MAX_POINTS: number = 3;
	protected timer: NodeJS.Timer;
	constructor () {
		super ("loading-content", TreeItemCollapsibleState.None);
		this.label = this.message + "...";
		this.start();
	}
	start (): void {
		this.loading();
		this.timer = setInterval(() => {
			this.loading();
		}, 200);
	}
	protected loading (): void {
		this.label = this.message + ".".repeat(this.points);
		this.points = this.points < this.MAX_POINTS ? this.points + 1 : 1;
	}
	stop (): void {
		this.points = 1;
		clearInterval(this.timer);
		this.timer = null;
	}
}
abstract class OverviewItem extends TreeItem {
	contextValue: string = "abstract-overview";
	protected contextFolder: string;
	name: string;
	constructor(type: string, desc: { contextFolder?: string }, collapsibleState?: TreeItemCollapsibleState) {
		super(type, collapsibleState | TreeItemCollapsibleState.None);
		this.contextValue = type;
		this.label = this.name = type.replace("-overview", "");
		this.contextFolder = (desc && desc.contextFolder) ? desc.contextFolder : "";
	}
	getContextFolder (): string {
		return this.contextFolder;
	}
}
export class FormulaOverviewItem extends OverviewItem {
	contextValue: string = "formula-overview";
	theorems: FormulaItem[] = [];
	fileName: string;
	fileExtension: string;
	constructor(type: string, desc: TheoryDescriptor, collapsibleState?: TreeItemCollapsibleState) {
		super(type, desc, collapsibleState);
		this.fileName = desc.fileName;
		this.fileExtension = desc.fileExtension;
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
	getChildren (): FormulaItem[] {
		return this.theorems;
	}
	protected refreshLabel (opt?: { keepCollapsibleState?: boolean }) {
		opt = opt || {};
		const nTheorems: number = (this.theorems) ? this.theorems.length : 0;
		const nProved: number = (this.theorems) ? this.theorems.filter((item: FormulaItem) => {
			return item.getStatus() === "proved";
		}).length : 0;
		if (nProved === nTheorems) {
			this.label = `${utils.icons.check} ${this.name}  (${nProved} proved)`
		} else {
			this.label = `${utils.icons.whitecircle} ${this.name}  (${nProved} proved, ${nTheorems-nProved} to be proved)`;
		}
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
export class FormulaItem extends TreeItem {
	contextValue: string = "formula";

	protected contextFolder: string;
	protected fileName: string;
	protected fileExtension: string;
	protected theoryName: string;
	protected formulaName: string;

	protected position: Position;
	protected status: ProofStatus;

	constructor(typeName: string, desc: FormulaDescriptor) {
		super(typeName, TreeItemCollapsibleState.None);
		this.contextValue = typeName;
		this.fileName = desc.fileName;
		this.fileExtension = desc.fileExtension;
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
			command: "explorer.theorem-selected-event",
			arguments: [ desc ]
		};
	}
	getPosition(): Position { return this.position; }
	getFileName(): string { return this.fileName; }
	getTheoryName(): string { return this.theoryName; }
	getFormulaName(): string { return this.formulaName; }
	getContextFolder(): string { return this.contextFolder; }
	setStatus (status: ProofStatus) {
		this.status = status;
		this.refreshLabel();
	}
	getStatus (): ProofStatus {
		return this.status;
	}
	protected refreshLabel() {
		this.status = this.status || "unchecked"; //'\u{2705}'
		this.status = this.status.startsWith("proved") ? "proved" : this.status;
		this.label = `${utils.getIcon(this.status)} ${this.formulaName}  (${this.status})`;
	}
}
//-- overviews
class TheoremsOverviewItem extends FormulaOverviewItem {
	contextValue: string = "theorems-overview";
	constructor(desc: TheoryDescriptor) {
		super("theorems-overview", desc);
	}
}
class TccsOverviewItem extends FormulaOverviewItem {
	contextValue: string = "tccs-overview";
	constructor(desc: TheoryDescriptor) {
		super("tccs-overview", desc);
		this.refreshLabel({ keepCollapsibleState: true });
	}
	// @overrides
	updateStatus (desc: FormulaDescriptor): void {
		super.updateStatus(desc, { keepCollapsibleState: true });
	}
	// @overrides
	protected refreshLabel (opt?: { keepCollapsibleState?: boolean }) {
		if (this.getTotal() > 0) {
			super.refreshLabel(opt);
		} else {
			this.label = "No TCCs";
		}
	}
	setTccs (desc: FormulaDescriptor[]): void {
		this.theorems = [];
		if (desc && desc.length) {
			for (let i = 0; i < desc.length; i++) {
				this.updateStatus(desc[i]);
			}
		}
	}
}
class TheoriesOverviewItem extends TreeItem {
	contextValue = "theories-overview";
	theories: TheoryItem[] = [];
	constructor (desc?: TheoryDescriptor[]) {
		super("theories-overview", TreeItemCollapsibleState.Expanded);
		this.theories = [];
		this.updateTheories(desc);
	}
	updateTheories (desc: TheoryDescriptor[], opt?: { tccDescriptor?: boolean }) {
		if (desc) {
			opt = opt || {};
			if (opt.tccDescriptor) {
				// the descriptor contains only tccs, don't touch the theorems
				for (let i = 0; i < desc.length; i++) {
					const candidates: TheoryItem[] = this.theories.filter((item: TheoryItem) => {
						return item.contextFolder === desc[i].contextFolder && item.fileName === desc[i].fileName
							&& item.fileExtension === desc[i].fileExtension && item.theoryName === desc[i].theoryName;
					});
					if (candidates && candidates.length === 1) {
						candidates[0].setTccs(desc[i].theorems);
					} else {
						console.warn(`[workspace-explorer] Warning: could not find theory item for `, desc);
					}
				}
			} else {
				const theories: TheoryItem[] = [];
				for (let i = 0; i < desc.length; i++) {
					const tdesc: TheoryDescriptor = desc[i];
					// create a new item only if the item does not already exist in the tree view
					// this is useful, e.g., to retain the list of tccs when updating theorems
					let theoryItem: TheoryItem = this.getTheoryItem(tdesc.theoryName);
					let keepCollapsibleState: boolean = true;
					if (!theoryItem) {
						keepCollapsibleState = false;
						theoryItem = new TheoryItem(tdesc, TreeItemCollapsibleState.Expanded);
					}
					for (let i = 0; i < tdesc.theorems.length; i++) {
						const formula: FormulaDescriptor = tdesc.theorems[i];
						if (formula.isTcc) {
							theoryItem.updateTcc(formula);
						} else {
							theoryItem.updateFormula(formula);
						}
					}
					theoryItem.refreshLabel({ keepCollapsibleState });
					theories.push(theoryItem);
				}
				this.theories = theories;
			}
		}
	}
	// /**
	//  * Internal function, computes the difference between two arrays of theory items.
	//  * This is used when updating theories, to keep the collapsiblestate.
	//  */
	// protected diff (v1: TheoryItem[], v2: TheoryItem[]): TheoryItem[] {
	// 	if (!v1 || v1.length === 0) { return []; }
	// 	if (!v2 || v2.length === 0) { return v1; }
	// 	const res: TheoryItem[] = v1.filter((item: TheoryItem) => {
	// 		return !v2.some((elem) => {
	// 			return elem.theoryName === item.theoryName;
	// 		});
	// 	});
	// 	return res;
	// }

	getChildren (): TreeItem[] {
		return this.theories;
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
	updateFormula (desc: FormulaDescriptor): void {
		const candidates: TheoryItem[] = this.theories.filter((item: TheoryItem) => {
			return item.contextFolder === desc.contextFolder && item.fileName === desc.fileName
				&& item.fileExtension === desc.fileExtension && item.theoryName === desc.theoryName;
		});
		if (candidates && candidates.length === 1) {
			candidates[0].updateFormula(desc);
		}
	}
	sort (): void {
		this.theories = this.theories.sort((a: TheoryItem, b: TheoryItem): number => {
			return (a.theoryName > b.theoryName) ? 1 : -1;
		});
	}
}
class PvsFilesOverviewItem extends OverviewItem {
	contextValue = "pvs-files-overview";
	files: PvsFileItem[] = [];
	constructor(desc: PvsContextDescriptor) {
		super("pvs-files-overview", desc, TreeItemCollapsibleState.Expanded);
		this.files = [];
		this.updateContextFolder(desc);
	}
	getChildren (): TreeItem[] {
		let children: TreeItem[] = [];
		for (let i = 0; i < this.files.length; i++) {
			children = children.concat(this.files[i].getChildren());
		}
		return children;
	}
	updateFormula (desc: FormulaDescriptor): void {
		const candidates: PvsFileItem[] = this.files.filter((item: PvsFileItem) => {
			return item.fileName === desc.fileName && item.fileExtension === desc.fileExtension
				&& item.contextFolder === desc.contextFolder;
		});
		if (candidates && candidates.length === 1) {
			candidates[0].updateFormula(desc);
		}
	}
	sort (): void {
		this.files = this.files.sort((a: PvsFileItem, b: PvsFileItem): number => {
			return (a.fileName > b.fileName) ? 1 : -1;
		});
		for (let i = 0; i < this.files.length; i++) {
			this.files[i].sort();
		}
	}
	updateContextFolder (desc: PvsContextDescriptor, opt?: { tccDescriptor?: boolean }): void {
		opt = opt || {};
		if (opt.tccDescriptor) {
			const fnames: string[] = Object.keys(desc.fileDescriptors);
			for (let i = 0; i < fnames.length; i++) {
				const candidates: PvsFileItem[] = this.files.filter((item: PvsFileItem) => {
					return fnames[i] === fsUtils.desc2fname(item);
				});
				if (candidates && candidates.length === 1) {
					candidates[0].updateFileDescriptor(desc.fileDescriptors[fnames[i]], opt);	
				} else {
					console.warn(`[workspace-explorer] Warning: could not find tree item for `, desc);
				}
			}
		} else {
			this.files = [];
			this.contextFolder = desc.contextFolder;
			if (desc.fileDescriptors) {
				const fnames: string[] = Object.keys(desc.fileDescriptors);
				for (let i = 0; i < fnames.length; i++) {
					const item: PvsFileItem = new PvsFileItem();
					item.updateFileDescriptor(desc.fileDescriptors[fnames[i]]);
					this.files.push(item);
				}
				// this.sort();
			}
		}
	}
}
export class WorkspaceOverviewItem extends OverviewItem {
	contextValue: string = "workspace-overview";

	protected path: string; // full path of the workspace
	protected pvsFilesOverview: PvsFilesOverviewItem;

	constructor(desc: PvsContextDescriptor) {
		super("workspace-overview", desc, TreeItemCollapsibleState.Expanded);
		this.pvsFilesOverview = new PvsFilesOverviewItem(desc);
		this.refreshLabel();
	}
	protected refreshLabel (): void {
		// const n: number = (this.pvsFilesOverview && this.pvsFilesOverview.files) ? this.pvsFilesOverview.files.length : 0;
		// this.label = fsUtils.getContextFolderName(this.contextFolder) + ` (${n} pvs files)`;
		this.label = fsUtils.getContextFolderName(this.contextFolder);
		this.path = this.contextFolder;
		this.tooltip = this.path;
	}
	// setContextFolder (contextFolder: string) {
	// 	if (this.contextFolder !== contextFolder) {
	// 		this.contextFolder = contextFolder;
	// 		this.updateLabel();
	// 	}
	// }
	getChildren (): TreeItem[] {
		return this.pvsFilesOverview.getChildren();
	}
	updateFormula (desc: FormulaDescriptor): void {
		this.pvsFilesOverview.updateFormula(desc);
	}
	sort (): void {
		this.pvsFilesOverview.sort();
	}
	updateContextFolder (desc: PvsContextDescriptor, opt?: { tccDescriptor?: boolean, force?: boolean }): void {
		opt = opt || {};
		// if (desc && (desc.contextFolder !== this.contextFolder || opt.force)) {
			this.contextFolder = desc.contextFolder;
			this.refreshLabel();
			this.pvsFilesOverview.updateContextFolder(desc, opt);
		// }
	}
}
//-- Items
// class TheoremItem extends FormulaItem {
// 	constructor(desc: FormulaDescriptor) {
// 		super("theorem", desc);
// 	}
// }
// class TccItem extends FormulaItem {
// 	constructor(desc: FormulaDescriptor) {
// 		super("tcc", desc);
// 	}
// }


/**
 * Data provider for PVS Explorer view
 */
export class VSCodePvsWorkspaceExplorer implements TreeDataProvider<TreeItem> {
	// protected pvsLibrariesPath: string = null;

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
	protected root: WorkspaceOverviewItem;
	protected loading: LoadingItem = new LoadingItem();

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
		// register tree view.
		// use window.createTreeView instead of window.registerDataProvider -- this allows to perform UI operations programatically. 
		// window.registerTreeDataProvider(this.providerView, this);
		this.view = window.createTreeView(this.providerView, { treeDataProvider: this, showCollapseAll: true });
	}

	getClient (): LanguageClient {
		return this.client;
	}

	// setContextFolder (context: string): void {
	// 	if (this.root) {
	// 		this.root.setContextFolder(context);
	// 	} else {
	// 		console.error(`[workspace-explorer] Error: root node is null`);
	// 	}
	// }

	updateFormula (desc: FormulaDescriptor): void {
		if (desc) {
			this.root.updateFormula(desc);
		}
	}

	updateContextFolder (desc: PvsContextDescriptor, opt?: { tccDescriptor?: boolean }): void {
		if (desc) {
			if (this.root) {
				this.root.updateContextFolder(desc, opt);
			} else {
				this.root = new WorkspaceOverviewItem(desc);
			}
		} else {
			this.root = null;
		}
		this.refreshView();
	}
	// protected loading (desc: PvsContextDescriptor): void {
	// 	this.root = new WorkspaceOverviewItem(desc);
	// }
	/**
	 * Internal function, used to refresh the tree view
	 */
	protected refreshView(): void {
		this._onDidChangeTreeData.fire();
	}

	// getTheoryDescriptor (theoryName: string): TheoryDescriptor {
	// 	if (this.root && this.root.theoriesOverview.theories) {
	// 		let candidates: TheoryItem[] = this.root.theoriesOverview.theories.filter((item: TheoryItem) => {
	// 			return item.theoryName === theoryName;
	// 		});
	// 		if (candidates && candidates.length === 1) {
	// 			return {
	// 				theoryName: theoryName,
	// 				fileName: candidates[0].fileName,
	// 				fileExtension: candidates[0].fileExtension,
	// 				contextFolder: candidates[0].contextFolder,
	// 				position: candidates[0].position
	// 			};
	// 		}
	// 	}
	// 	return null;
	// }

	/**
	 * Handler activation function
	 * @param context Client context 
	 */
	activate(context: ExtensionContext) {
		// all commands in the form vscode-pvs.xxxx are handled by VSCodeEventsDispatcher

		// click on a tcc formula to open the file and highlight the tcc name in the theory
		context.subscriptions.push(commands.registerCommand('explorer.tcc-selected-event', async (uri: Uri, range: Range) => {
			// window.showInformationMessage("theory selected: " + theoryName + " " + fileName + "(" + position.line + ", " + position.character + ")");
			window.showTextDocument(uri, {
				preserveFocus: true, 
				preview: true,
				viewColumn: ViewColumn.Beside,
				selection: range
			});
		}));

		// click on a theorem to open the file and highlight the theorem in the theory
		context.subscriptions.push(commands.registerCommand('explorer.theorem-selected-event', async (desc: FormulaDescriptor) => {
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
		// cmd = commands.registerCommand('explorer.tcc-selected-eventFormula', async (desc: { theoryName: string, fileName: string, fileExtension: string, contextFolder: string, position: Position }) => {
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
	}

	/**
	 * Returns the list of theories defined in the active pvs file
	 * @param element Element clicked by the user 
	 */
	getChildren(element: TreeItem): Thenable<TreeItem[]> {
		if (element) {
			let children: TreeItem[] = null;
			if (element.contextValue === "workspace-overview") {
				children = this.root.getChildren();
			} else if (element.contextValue === "pvs-files-overview") {
				children = (<PvsFilesOverviewItem> element).getChildren();
			} else if (element.contextValue === "pvs-file") {
				children = (<PvsFileItem> element).getChildren();
			} else if (element.contextValue === "theory") {
				// pvs theory
				const desc: TheoryItem = <TheoryItem> element;
				const nTccs: number = desc.getTccsOverview().getTotal();
				if (nTccs > 0) {
					children = [].concat(desc.getTheoremsOverview().getChildren()).concat([ <TreeItem> desc.getTccsOverview() ]);
				} else {
					children = desc.getTheoremsOverview().getChildren();
				}
			} else if (element.contextValue === "tccs-overview") {
				// tcc list
				const desc: TccsOverviewItem = <TccsOverviewItem> element;
				children = desc.getChildren();
			} else if (element.contextValue === "tcc") {
				// console.log('tcc');
			} else if (element.contextValue === "theorem") {
				// console.log('theorem');
			} else if (element.contextValue === "theorems-overview") {
				const desc: TheoremsOverviewItem = <TheoremsOverviewItem> element;
				children = desc.getChildren();
			} else if (element.contextValue === "theories-overview") {
				children = (<TheoriesOverviewItem> element).getChildren();
			}
			return Promise.resolve(children);
		}
		// root node: show context
		if (this.root) {
			this.loading.stop();
			return Promise.resolve([ this.root ]);
		} else {
			return Promise.resolve([ this.loading ])
		}
	}

	getTreeItem(element: TreeItem): TreeItem {
		return element;
	}

}
