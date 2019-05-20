import { ExtensionContext, TreeItemCollapsibleState, commands, window, TextDocument, 
			Uri, Range, Position, TreeItem, Command, EventEmitter, Event,
			TreeDataProvider, workspace, MarkdownString, TreeView, Disposable } from 'vscode';
import { LanguageClient } from 'vscode-languageclient';
import { stringify } from 'querystring';
import { timingSafeEqual } from 'crypto';

/**
 * Definition of tree items
 */
class ProofItem extends TreeItem {
	contextValue: string = "proofItem";
	status: string = "";
	command: Command;
	args: string;
	children: TreeItem[];
	constructor (contextValue: string, label: string, id: string, collapsibleState?: TreeItemCollapsibleState) {
		super(contextValue, (collapsibleState === undefined) ? 
								(contextValue === "proofCommand") ? TreeItemCollapsibleState.Collapsed
										: TreeItemCollapsibleState.None 
									: collapsibleState);
		this.id = id;
		this.contextValue = contextValue;
		this.label = label;
		// this.tooltip = "Click to run " + this.contextValue;
		this.command = {
			title: this.contextValue,
			command: "explorer.didClickOnStrategy",
			arguments: [ this.contextValue ]
		};
	}
	setChildren (children: TreeItem[]) {
		this.children = children;
	}
	appendChild (child: TreeItem) {
		this.children = this.children || [];
		this.children.push(child);
	}
	getChildren (): TreeItem[] {
		return this.children;
	}
	proved () {
		this.status = "proved";
		this.tooltip = "proved ✔";
		this.label += " ✔";
		this.collapsibleState = TreeItemCollapsibleState.None;
	}
	failed () {
		this.status = "failed";
		this.tooltip = "failed X";
		this.label += " ❌";
		this.collapsibleState = TreeItemCollapsibleState.None;
	}
}
class Grind extends ProofItem {
	constructor (id: string, collapsibleState?: TreeItemCollapsibleState) {
		super("proofCommand", "grind", id, collapsibleState);
	}
}
class Split extends ProofItem {
	constructor (id: string, collapsibleState?: TreeItemCollapsibleState) {
		super("proofCommand", "split", id, collapsibleState);
	}
}
class Expand extends ProofItem {
	constructor (id: string, args: string, collapsibleState?: TreeItemCollapsibleState) {
		super("proofCommand", 'expand "' + args + '"', id, collapsibleState);
		this.args = args;
	}
}
class SkosimpStar extends ProofItem {
	constructor (id: string, collapsibleState?: TreeItemCollapsibleState) {
		super("proofCommand", "skosimp*", id, collapsibleState);
	}
}
class VDashItem extends ProofItem {
	constructor (id: string, collapsibleState?: TreeItemCollapsibleState) {
		super("vdash", "⊢", id, collapsibleState);
		// this.tooltip = "Click to view sequents";
	}
	setChildren (children: TreeItem[]) {
		super.setChildren(children);
		if (this.children && this.children.length > 0) {
			this.contextValue = "vdash-with-children";
			this.collapsibleState = TreeItemCollapsibleState.Collapsed;
		} else {
			this.contextValue = "vdash";
			this.collapsibleState = TreeItemCollapsibleState.None;
		}
	}
}
class ProofCommand extends TreeItem {
	static uid: number = 0;
	contextValue: string = "proofCommand";
	cmd: string; // prover command
	command: Command; // vscode action
	children: TreeItem[];
	constructor (cmd: string, collapsibleState?: TreeItemCollapsibleState) {
		super("proofCommand", (collapsibleState === undefined) ? TreeItemCollapsibleState.Expanded : collapsibleState);
		this.id = (ProofCommand.uid++).toString();
		this.cmd = cmd;
		this.notVisited();
		// this.tooltip = "Click to run " + this.contextValue;
		this.command = {
			title: this.contextValue,
			command: "explorer.didClickOnStrategy",
			arguments: [ this.contextValue ]
		};
	}
	notVisited () {
		this.tooltip = this.cmd;
		this.label = this.cmd;
	}
	visited () {
		this.tooltip = "executed";
		this.label = `🔹${this.cmd}`;
	}
	active () {
		this.tooltip = "ready to execute";
		this.label = `💠${this.cmd}`;
	}
	setChildren (children: TreeItem[]) {
		this.children = children;
	}
	appendChild (child: TreeItem) {
		this.children = this.children || [];
		this.children.push(child);
	}
	getChildren (): TreeItem[] {
		return this.children;
	}
}

// class Proved extends ProofItem {
// 	constructor (id: string) {
// 		super("✔", id, TreeItemCollapsibleState.None)
// 	}
// }
// class Failed extends ProofItem {
// 	constructor (id: string) {
// 		super("❌", id, TreeItemCollapsibleState.None)
// 	}
// }

// https://emojipedia.org/symbols/
//  🔵 ⚫ ⚪ 🔴 🔽 🔼 ⏯ ⏩ ⏪ ⏫ ⏬ ⧐ ▶️ ◀️ ⭕ 🔹🔸💠🔷🔶

class ProverCommands {
	activate (context: ExtensionContext) {
		let cmd: Disposable = commands.registerCommand("proof-explorer.grind", () => {
			window.showInformationMessage("grind");
		});
		context.subscriptions.push(cmd);
	}
}

/**
 * Data provider for PVS Proof Explorer view
 */
export class VSCodePvsProofExplorer implements TreeDataProvider<TreeItem> {
	/**
	 * Events for updating the tree structure
	 */
	private _onDidChangeTreeData: EventEmitter<TreeItem> = new EventEmitter<TreeItem>();
	readonly onDidChangeTreeData: Event<TreeItem> = this._onDidChangeTreeData.event;

	private proverCommands: ProverCommands;

	/**
	 * Language client for communicating with the server
	 */
	private client: LanguageClient;

	/**
	 * Name of the view associated with the data provider
	 */
	private providerView: string;

	private view: TreeView<TreeItem>

	private root;

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
		this.view = window.createTreeView(this.providerView, { treeDataProvider: this });
		this.proverCommands = new ProverCommands();
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
	}

	private fromJSON (json: { [key: string]: { id: string, children: any[] } }) {
		function makeTree(elem: { id: string, children: any[] }, parent: ProofCommand) {
			const cmd: ProofCommand = new ProofCommand(elem.id);
			parent.appendChild(cmd);
			if (elem.children && elem.children.length) {
				elem.children.forEach(child => {
					makeTree(child, cmd);
				});
			} else {
				cmd.collapsibleState = TreeItemCollapsibleState.None;
			}
		}
		Object.keys(json).forEach(key => {
			const elem: { id: string, children: any[] } = json[key];
			const cmd: ProofCommand = new ProofCommand(elem.id);
			this.root = cmd;
			if (elem.children && elem.children.length) {
				elem.children.forEach(child => {
					makeTree(child, cmd);
				});
			} else {
				cmd.collapsibleState = TreeItemCollapsibleState.None;
			}
		});
		// update front-end
		this.refreshView();
	}


	/**
	 * Handlers for messages received from the server
	 */
	private installHandlers(context: ExtensionContext) {
		this.client.onRequest('server.response.step-proof', (ans: string) => {
			// let root: ProofItem = new VDashItem("root", TreeItemCollapsibleState.Expanded);
			// this.nodes.set("root", root);
			this.fromJSON(JSON.parse(ans));
		});

		this.client.onRequest("server.response.prover", (ans) => {
		});
	}

	
	/**
	 * Handler activation function
	 * @param context Client context 
	 */
	activate(context: ExtensionContext) {
		this.installHandlers(context);
		this.proverCommands.activate(context);



		// create sample proof tree
		// let root: ProofItem = new VDashItem("root");
		// let node1: ProofItem = new SkosimpStar("node1");
		// let node2: ProofItem = new VDashItem("node2");
		// let node3: ProofItem = new Expand("node3", "per_release_fup");
		// let node4: ProofItem = new VDashItem("node4");
		// let node5: ProofItem = new Split("node5");
		// let node5_1: ProofItem = new VDashItem("node5.1");
		// let node5_2: ProofItem = new VDashItem("node5.2");
		// let node5_3: ProofItem = new VDashItem("node5.3");
		// let node5_4: ProofItem = new VDashItem("node5.4");
		// let node5_1_1: ProofItem = new Grind("node5.1.1");
		// let node5_2_1: ProofItem = new Grind("node5.2.1");
		// let node5_3_1: ProofItem = new Grind("node5.3.1");
		// let node5_4_1: ProofItem = new Grind("node5.4.1");
		// root.setChildren([ node1 ]);
		// node1.setChildren([ node2 ]);
		// node2.setChildren([ node3 ]);
		// node3.setChildren([ node4 ]);
		// node4.setChildren([ node5 ]);
		// node5.setChildren([ node5_1, node5_2, node5_3, node5_4 ]);
		// node5_1.setChildren([ node5_1_1 ]);
		// node5_2.setChildren([ node5_2_1 ]);
		// node5_3.setChildren([ node5_3_1 ]);
		// // node5_4.setChildren([ node5_4_1 ]);
		// node5_1_1.proved();
		// node5_2_1.failed();
		// node5_3_1.proved();
		// // node5_4_1.proved();
		// this.nodes.set("root", root);
	}

	/**
	 * Returns the list of theories defined in the active pvs file
	 * @param element Element clicked by the user 
	 */
	getChildren(element: TreeItem): Thenable<TreeItem[]> {
		if (element) {
			let children: TreeItem[] = (<ProofItem> element).getChildren();
			return Promise.resolve(children);
		}
		// root node: show the list of theories from the selected file
		return Promise.resolve([ this.root ]);
	}

	getTreeItem(element: TreeItem): TreeItem {
		return element;
	}

}
