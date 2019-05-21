import { ExtensionContext, TreeItemCollapsibleState, commands, window, TextDocument, 
			Uri, Range, Position, TreeItem, Command, EventEmitter, Event,
			TreeDataProvider, workspace, MarkdownString, TreeView, Disposable } from 'vscode';
import { LanguageClient } from 'vscode-languageclient';
import { ProofDescriptor, ProofStructure } from '../common/serverInterface';

/**
 * Definition of tree items
 */
class ProofItem extends TreeItem {
	static uid: number = 0;
	contextValue: string = "proofItem";
	name: string; // prover command
	command: Command; // vscode action
	children: ProofItem[];
	constructor (type: string, name: string, collapsibleState?: TreeItemCollapsibleState) {
		super(type, (collapsibleState === undefined) ? TreeItemCollapsibleState.Expanded : collapsibleState);
		this.contextValue = type;
		this.id = (ProofCommand.uid++).toString();
		this.name = name;
		this.notVisited();
	}
	notVisited () {
		this.tooltip = `${this.contextValue} ${this.name}`;
		this.label = this.name;
	}
	visited () {
		this.tooltip = "executed";
		this.label = `${this.name}`;
	}
	active () {
		this.tooltip = "ready to execute";
		this.label = `🔹${this.name}`;
	}
	setChildren (children: ProofItem[]) {
		this.children = children;
	}
	appendChild (child: ProofItem) {
		this.children = this.children || [];
		this.children.push(child);
	}
	getChildren (): ProofItem[] {
		return this.children;
	}
}
class ProofCommand extends ProofItem {
	constructor (cmd: string, collapsibleState?: TreeItemCollapsibleState) {
		super("proof-command", cmd);
		this.id = (ProofCommand.uid++).toString();
		this.name = cmd;
		this.notVisited();
		// this.tooltip = "Click to run " + this.contextValue;
		this.command = {
			title: this.contextValue,
			command: "explorer.didClickOnStrategy",
			arguments: [ this.contextValue ]
		};
	}
}
class ProofBranch extends ProofItem {
	constructor (cmd: string, collapsibleState?: TreeItemCollapsibleState) {
		super("proof-branch", cmd);
		this.id = (ProofCommand.uid++).toString();
		this.name = cmd;
		this.notVisited();
		// this.tooltip = "Click to run " + this.contextValue;
		this.command = {
			title: this.contextValue,
			command: "explorer.didClickOnStrategy",
			arguments: [ this.contextValue ]
		};
	}
}
class RootNode extends ProofItem {
	constructor (cmd: string, collapsibleState?: TreeItemCollapsibleState) {
		super("root", cmd);
		this.id = (ProofCommand.uid++).toString();
		this.name = cmd;
		this.notVisited();
		// this.tooltip = "Click to run " + this.contextValue;
		this.command = {
			title: this.contextValue,
			command: "explorer.didClickOnStrategy",
			arguments: [ this.contextValue ]
		};
	}
}

// https://emojipedia.org/symbols/
//  ❌ 🔵 ⚫ ⚪ 🔴 🔽 🔼 ⏯ ⏩ ⏪ ⏫ ⏬ ▶️ ◀️ ⭕ 🔹🔸💠🔷🔶


/**
 * Data provider for PVS Proof Explorer view
 */
export class VSCodePvsProofExplorer implements TreeDataProvider<TreeItem> {
	/**
	 * Events for updating the tree structure
	 */
	private _onDidChangeTreeData: EventEmitter<TreeItem> = new EventEmitter<TreeItem>();
	readonly onDidChangeTreeData: Event<TreeItem> = this._onDidChangeTreeData.event;

	// private proverCommands: ProverCommands;

	/**
	 * Language client for communicating with the server
	 */
	private client: LanguageClient;

	/**
	 * Name of the view associated with the data provider
	 */
	private providerView: string;
	private view: TreeView<TreeItem>
	private root: ProofItem;
	private desc: ProofDescriptor;

	private nodes: { index: number, node: ProofItem }[] = [];
	private activeIndex: number = 0;

	startProof (): void {
		this.activeIndex = 0;
		this.nodes[this.activeIndex].node.active();
	}
	getActiveCommand (): ProofItem {
		return this.nodes[this.activeIndex].node;
	}
	nextCommand (): void {
		if (this.activeIndex < this.nodes.length) {
			this.nodes[this.activeIndex].node.visited();
			this.activeIndex++;
			this.nodes[this.activeIndex].node.active();
			if (this.nodes[this.activeIndex].node.contextValue === "proof-branch"
				&& this.activeIndex < this.nodes.length) {
					this.nodes[this.activeIndex].node.visited();
				this.activeIndex++;
				this.nodes[this.activeIndex].node.active();	
			}
		}
	}

	// private getNextCommand (): ProofCommand {
	// 	// TODO
	// }

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
		// this.proverCommands = new ProverCommands();
	}

	/**
	 * Refresh tree view
	 */
	private refreshView(): void {
		this._onDidChangeTreeData.fire();
	}

	/**
	 * Utility function, loads a proof tree into proof-explorer
	 * @param json The proof to be loaded, in JSON format
	 */
	private fromJSON (json: ProofStructure) {
		let index: number = 0;
		const makeTree = (elem: { id: string, children: any[], type: string }, parent: ProofCommand) => {
			const node: ProofItem = (elem.type === "proof-command") ? new ProofCommand(elem.id) : 
										(elem.type === "proof-branch") ? new ProofBranch(elem.id)
										: new RootNode(elem.id);
			parent.appendChild(node);
			this.nodes.push({ index, node });
			index++;
			if (elem.children && elem.children.length) {
				elem.children.forEach(child => {
					makeTree(child, node);
				});
			} else {
				node.collapsibleState = TreeItemCollapsibleState.None;
			}
		}
		this.nodes = [];
		if (json && json.proof && json.desc) {
			const cmd: ProofCommand = new ProofCommand(json.proof.id);
			this.root = cmd; // this is the proof name
			if (json.proof.children && json.proof.children.length) {
				json.proof.children.forEach(child => {
					makeTree(child, cmd);
				});
			} else {
				cmd.collapsibleState = TreeItemCollapsibleState.None;
			}
			this.desc = json.desc;
			this.activeIndex = 0;
		}
		// update front-end
		this.refreshView();
	}

	/**
	 * Handlers for messages received from the server
	 */
	private installHandlers(context: ExtensionContext) {
		this.client.onRequest('server.response.step-proof', (ans: string) => {
			this.fromJSON(JSON.parse(ans));
		});
		this.client.onRequest('server.response.step-tcc', (ans: string) => {
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
		let cmd: Disposable = commands.registerCommand("proof-explorer.step", () => {
			const activeCommand: ProofCommand = this.getActiveCommand();
			const cmd: string = activeCommand.name;
			commands.executeCommand("terminal.pvs.send-proof-command", {
				fileName: this.desc.fileName, theoryName: this.desc.theoryName, formulaName: this.desc.formulaName, line: this.desc.line, cmd
			});
			this.refreshView();
		});
		context.subscriptions.push(cmd);
		cmd = commands.registerCommand("terminal.pvs.response.step-executed", () => {
			this.nextCommand();
			this.refreshView();
		});
		context.subscriptions.push(cmd);
		cmd = commands.registerCommand("terminal.pvs.response.step-proof-ready", () => {
			this.startProof();
			this.refreshView();
		});
		context.subscriptions.push(cmd);
		cmd = commands.registerCommand("proof-explorer.goto", (resource: ProofItem) => {
			if (resource) {
				const targetNodes: { index: number, node: ProofItem }[] = this.nodes.filter((elem: { index: number, node: ProofItem }) => {
					return elem.node.id === resource.id;
				});
				if (targetNodes && targetNodes.length === 1) {
					this.nodes[this.activeIndex].node.visited()
					this.activeIndex = targetNodes[0].index;
					this.nodes[this.activeIndex].node.active()
				}
				this.refreshView();
			} else {
				window.showErrorMessage(`Error while trying to move to command ${resource.name}`);
			}
		});
		context.subscriptions.push(cmd);
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
		// root node
		return Promise.resolve([ this.root ]);
	}

	getTreeItem(element: TreeItem): TreeItem {
		return element;
	}

}
