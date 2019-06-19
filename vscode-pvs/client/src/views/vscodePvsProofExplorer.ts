/**
 * @module VSCodePvsProofExplorer
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
			TreeDataProvider, workspace, MarkdownString, TreeView, Disposable, Terminal } from 'vscode';
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
		this.label = ` ${this.name}`;
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
		if (this.nodes && this.activeIndex < this.nodes.length) {
			this.nodes[this.activeIndex].node.active();
		} else {
			console.error("Error: unable to render proof tree");
		}
		this.refreshView();
	}
	getActiveCommand (): ProofItem {
		if (this.nodes && this.activeIndex < this.nodes.length) {
			return this.nodes[this.activeIndex].node;
		}
		return null;
	}
	getNextCommand (): ProofItem {
		if (this.activeIndex + 1 < this.nodes.length) {
			if (this.nodes[this.activeIndex + 1].node.contextValue === "proof-branch"
				&& this.activeIndex + 2 < this.nodes.length) {
				return this.nodes[this.activeIndex + 2].node
			}
			return this.nodes[this.activeIndex + 1].node;
		}
		return null;
	}
	private advance (): ProofItem {
		let nextCommand: ProofItem = null;
		if (this.activeIndex < this.nodes.length) {
			this.nodes[this.activeIndex].node.visited();
			this.activeIndex++;
			if (this.activeIndex < this.nodes.length) {
				this.nodes[this.activeIndex].node.active();
				if (this.nodes[this.activeIndex].node.contextValue === "proof-branch"
					&& this.activeIndex < this.nodes.length) {
						this.nodes[this.activeIndex].node.visited();
					this.activeIndex++;
					this.nodes[this.activeIndex].node.active();	
				}
				nextCommand = this.nodes[this.activeIndex].node;
			}
		}
		return nextCommand;
	}
	execActiveCommand(cmd?: string): ProofCommand {
		const activeCommand: ProofCommand = this.getActiveCommand();
		if (activeCommand) {
			cmd = cmd || activeCommand.name;
			const data: {
				fileName: string, theoryName: string, formulaName: string, line: number, cmd: string
			} = { fileName: this.desc.fileName, theoryName: this.desc.theoryName, formulaName: this.desc.formulaName, line: this.desc.line, cmd };
			commands.executeCommand("terminal.pvs.send-proof-command", data);
		}
		return activeCommand;
	}
	step(): void {
		this.execActiveCommand();
		this.advance();
		this.refreshView();
	}
	private fastForward (id: number): void {
		const commands: string[] = [];
		for (let i = this.activeIndex; i < this.nodes.length && +this.nodes[i].node.id !== id; i++) {
			if (this.nodes[i].node.contextValue === "proof-command") {
				commands.push(this.nodes[i].node.name);
			}
		}
		this.execActiveCommand(commands.join("\n"));
		this.jumpTo(id);
	}
	jumpTo (id: number): void {
		const targetNodes: { index: number, node: ProofItem }[] = this.nodes.filter((elem: { index: number, node: ProofItem }) => {
			return +elem.node.id === id;
		});
		if (targetNodes && targetNodes.length === 1) {
			if (this.nodes && this.activeIndex < this.nodes.length) {
				this.nodes[this.activeIndex].node.visited();
			}
			this.activeIndex = targetNodes[0].index;
			this.nodes[this.activeIndex].node.active();
		}
		this.refreshView();
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
		this.view = window.createTreeView(this.providerView, { treeDataProvider: this });
		// this.proverCommands = new ProverCommands();
	}

	/**
	 * Refresh tree view
	 */
	private refreshView(): void {
		this._onDidChangeTreeData.fire();
	}

	private resetView(): void {
		this.nodes = [];
		this.root = null;
		this.refreshView();
	}

	/**
	 * Utility function, loads a proof tree into proof-explorer
	 * @param json The proof to be loaded, in JSON format
	 */
	private fromJSON (json: ProofStructure) {
		let index: number = 0;
		const makeTree = (elem: { id: string, children: any[], type: string }, parent: ProofCommand) => {
			const node: ProofItem = (elem.type === "proof-command") ? new ProofCommand(elem.id) : new ProofBranch(elem.id);
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
		this.resetView();
		this.nodes = [];
		if (json && json.proof && json.desc) {
			const cmd: ProofCommand = new RootNode(json.proof.id);
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
		// register handler for terminal closed events
		window.onDidCloseTerminal((e: Terminal) => {
			if (e && e.name === json.proof.id) {
				this.resetView();
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
			this.fromJSON(JSON.parse(ans));
			this.startProof();
		});
		this.client.onRequest('server.response.step-tcc', (ans: string) => {
			this.fromJSON(JSON.parse(ans));
			this.startProof();
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

		// -- commands sent by pvsTerminalCli
		let cmd: Disposable = commands.registerCommand("proof-explorer.step", () => {
			this.step();
		});
		// context.subscriptions.push(cmd);
		// cmd = commands.registerCommand("terminal.pvs.response.step-executed", () => {
		// 	// this.refreshView();
		// });
		// context.subscriptions.push(cmd);
		// cmd = commands.registerCommand("terminal.pvs.response.step-proof-ready", () => {
		// 	this.startProof();
		// });

		// -- proof explorer commands
		context.subscriptions.push(cmd);
		cmd = commands.registerCommand("proof-explorer.jump-to", (resource: ProofItem) => {
			if (resource && this.nodes) {
				this.jumpTo(+resource.id);
			} else {
				window.showErrorMessage(`Error while trying to move to command ${resource.name}`);
			}
		});
		context.subscriptions.push(cmd);
		cmd = commands.registerCommand("proof-explorer.fast-forward", (resource: ProofItem) => {
			if (resource && this.nodes) {
				this.fastForward(+resource.id);
				this.refreshView();
			} else {
				window.showErrorMessage(`Error while trying to fast-forward to ${resource.name}`);
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
