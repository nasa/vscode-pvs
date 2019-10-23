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
import { ProofTree, serverCommand } from '../common/serverInterface';
import * as utils from '../common/languageUtils';
import * as fsUtils from '../common/fsUtils';
import { PvsResponse } from '../common/pvs-gui';
import { ProofStateNode } from '../common/languageUtils';

// https://emojipedia.org/symbols/
//  ❌ 🔵 ⚫ ⚪ 🔴 🔽 🔼 ⏯ ⏩ ⏪ ⏫ ⏬ ▶️ ◀️ ⭕ 🔹🔸💠🔷🔶

/**
 * Definition of tree items
 */
class ProofItem extends TreeItem {
	contextValue: string = "proofItem";
	name: string; // prover command
	command: Command; // vscode action
	protected lastState: {
		label?: string;
		tooltip?: string;
	} = {};
	children: ProofItem[];
	parent: ProofItem;
	activeFlag: boolean = false;
	visitedFlag: boolean = false;
	proofState: ProofStateNode = null;
	constructor (type: string, name: string, parent: ProofItem, collapsibleState?: TreeItemCollapsibleState) {
		super(type, (collapsibleState === undefined) ? TreeItemCollapsibleState.Expanded : collapsibleState);
		this.contextValue = type;
		this.id = fsUtils.get_fresh_id();
		this.name = name;
		this.parent = parent;
		this.tooltip = " ";
		this.notVisited();
	}
	pending (): void {
		this.label = `🔸${this.name}`;
		this.activeFlag = false;
		this.visitedFlag = true;
	}
	visited (): void {
		this.lastState.tooltip = this.tooltip;
		this.lastState.label = this.label = ` ★ ${this.name}`;
		this.activeFlag = false;
		this.visitedFlag = true;
	}
	notVisited (): void {
		this.lastState.tooltip = this.tooltip = " ";
		this.lastState.label = this.label = ` ∘ ${this.name}`;
		this.activeFlag = false;
		this.visitedFlag = false;
	}
	active (): void {
		this.label = `🔹${this.name}`;
		this.activeFlag = true;
	}
	isActive (): boolean {
		return this.activeFlag;
	}
	isVisitedOrPending (): boolean {
		return this.visitedFlag;
	}
	restore (): void {
		this.label = this.lastState.label;
		this.tooltip = this.lastState.tooltip;
	}
	setChildren (children: ProofItem[]): void {
		this.children = children;
	}
	insertChild (child: ProofItem, position: number): void {
		if (this.children && position < this.children.length) {
			this.children = this.children.slice(0, position).concat([ child ]).concat(this.children.slice(position));
		}
	}
	deleteChild (child: ProofItem): void {
		this.children = this.children.filter((ch: ProofItem) => {
			return ch.id !== child.id;
		});
	}
	getProofCommands (): ProofItem[] {
		let ans: ProofItem[] = [ this ];
		if (this.children) {
			for (let i = 0; i < this.children.length; i++) {
				ans = ans.concat(this.children[i].getProofCommands());
			}
		}
		return ans;
		// .filter(item => {
		// 	return item.contextValue !== "root";
		// });
	}
	getNext (): ProofItem {
		// check if this node has children, if so, return the first non-visited child
		if (this.children && this.children.length) {
			const children: ProofItem[] = this.children.filter((item: ProofItem) => {
				return !item.isVisitedOrPending();
			});
			if (children && children.length) {
				return children[0];
			}
		}
		// else, branch completed, go to next sibling
		this.visited();
		if (this.parent) {
			return this.parent.getNext();
		}
		return null;
	}
	selectLastVisitedChild (): ProofItem {
		if (this.children) {
			const children: ProofItem[] = this.children.filter(item => {
				return item.isVisitedOrPending();
			});
			if (children && children.length) {
				this.pending();
				return children[children.length - 1].selectLastVisitedChild();
			}
		}
		return this;
	}
	getPrevious (): ProofItem {
		this.notVisited();
		if (this.parent) {
			if (this.parent.children && this.parent.children.length) {
				const children: ProofItem[] = this.parent.children.filter((item: ProofItem) => {
					return item.isVisitedOrPending();
				});
				// return previous child, if any
				if (children && children.length) {
					const candidate: ProofItem = children[children.length - 1];
					if (candidate.children && candidate.children.length) {
						return candidate.selectLastVisitedChild();
					}
					return candidate;
				}
				// return first proof command, if parent is root
				if (this.parent.contextValue === "root") {
					return this.children[0];
				}
				// else return parent
				return this.parent;
			}
		}
		return null;
	}
	getSibling (): ProofItem {
		if (this.parent) {
			const children: ProofItem[] = this.parent.children;
			if (children && children.length > 1) {
				const index: number = children.indexOf(this);
				const next: number = index + 1;
				const prev: number = index - 1;
				return (next < children.length) ? children[next] : children[prev];	
			}
			// else, this is the only child, return the parent
			return this.parent;
		}
		return null;
	}
	appendChild (child: ProofItem): void {
		this.children = this.children || [];
		this.children.push(child);
	}
	getChildren (): ProofItem[] {
		return this.children;
	}
	concat (cmd: string): void {
		const parent: ProofItem = this.parent;
		if (parent) {
			// TODO: handle situation where command generates proof branches
			const proofCommand: ProofCommand = new ProofCommand(cmd, parent, TreeItemCollapsibleState.None);
			proofCommand.visited();
			const nodeIndex: number = parent.children.indexOf(this);
			parent.insertChild(proofCommand, nodeIndex);
		} else {
			console.error("[vscode-pvs-proof-explorer] Warning: could not find parent for new command ", cmd);
		}
	}
}
class ProofCommand extends ProofItem {
	constructor (cmd: string, parent: ProofItem, collapsibleState?: TreeItemCollapsibleState) {
		super("proof-command", cmd, parent, collapsibleState);
		this.name = (cmd && cmd.startsWith("(") && cmd.endsWith(")")) ? cmd.substr(1, cmd.length - 2) : cmd; // remove adorned () to make the visualisation slimmer
		this.notVisited();
		this.command = {
			title: this.contextValue,
			command: "explorer.didClickOnStrategy",
			arguments: [ this.contextValue ]
		};
	}
}
class ProofBranch extends ProofItem {
	constructor (cmd: string, parent: ProofItem, collapsibleState?: TreeItemCollapsibleState) {
		super("proof-branch", cmd, parent, collapsibleState);
		this.name = cmd;
		this.notVisited();
		this.command = {
			title: this.contextValue,
			command: "explorer.didClickOnStrategy",
			arguments: [ this.contextValue ]
		};
	}
}
class RootNode extends ProofItem {
	constructor (cmd: string, collapsibleState?: TreeItemCollapsibleState) {
		super("root", cmd, null, collapsibleState);
		this.name = cmd;
		this.notVisited();
		this.command = {
			title: this.contextValue,
			command: "explorer.didClickOnStrategy",
			arguments: [ this.contextValue ]
		};
	}
	// @overrides
	notVisited () {
		this.label = this.name;
	}
}

/**
 * Data provider for PVS Proof Explorer view
 */
export class VSCodePvsProofExplorer implements TreeDataProvider<TreeItem> {
	/**
	 * Events for updating the tree structure
	 */
	protected _onDidChangeTreeData: EventEmitter<TreeItem> = new EventEmitter<TreeItem>();
	readonly onDidChangeTreeData: Event<TreeItem> = this._onDidChangeTreeData.event;

	/**
	 * Language client for communicating with the server
	 */
	protected client: LanguageClient;

	protected desc: { fileName: string, fileExtension: string, theoryName: string, formulaName: string, contextFolder: string }; // proof descriptor

	/**
	 * Name of the view associated with the data provider
	 */
	protected providerView: string;
	protected view: TreeView<TreeItem>
	protected root: ProofItem;
	protected proofTree: ProofTree;

	protected nodes: ProofItem[] = [];
	protected activeNode: ProofItem = null;

	protected pendingExecution: boolean = false;

	protected initialProofState: ProofStateNode;
	
	// status flag, indicates whether we are running all proof commands, as opposed to stepping through the proof commands
	protected running: boolean = false;
	// indicates which node we are fast-forwarding to
	protected stopAt: ProofItem = null;

	// protected commands: ProofItem[] = [];

	protected getActiveCommand (): ProofCommand {
		// if (this.commands.length > 0) {
		// 	return this.commands[0];
		// }
		return this.activeNode;
	}
	protected moveIndicatorAtBeginning (): void {
		if (this.nodes && this.nodes.length) {
			this.activeNode = this.nodes[0];
			this.nodes[0].active();
		} else {
			console.error("Error: unable to render proof tree");
		}
		this.refreshView();
	}
	protected moveIndicatorBack (): void {
		if (this.activeNode) {
			this.activeNode.notVisited();
			this.activeNode = this.activeNode.getPrevious();
			if (this.activeNode) {
				this.activeNode.active();
			}
		} else {
			console.error("[vscode-pvs-proof-explorer] Warning: active node is null");
		}
	}
	protected moveIndicatorForward (): void {
		if (this.activeNode) {
			if (this.activeNode.contextValue === "proof-command") {
				this.activeNode.visited();
			} else {
				// proof branch or root
				this.activeNode.pending();
			}
			this.activeNode = this.activeNode.getNext();
			if (this.activeNode) {
				this.activeNode.active();
			}
			this.refreshView();
		} else {
			console.error("[vscode-pvs-proof-explorer] Warning: active node is null");
		}
	}
	run (): void {
		this.running = true;
		this.step();
	}
	step (cmd?: string): ProofCommand {
		const activeCommand: ProofCommand = this.getActiveCommand();
		if (activeCommand) {
			if (this.stopAt === activeCommand) {
				this.stopAt = null;
			} else {
				this.pendingExecution = true;
				switch (activeCommand.contextValue) {
					case "proof-command": {
						cmd = cmd || activeCommand.name;
						commands.executeCommand("vscode-pvs.send-proof-command", {
							fileName: this.desc.fileName,
							fileExtension: this.desc.fileExtension,
							theoryName: this.desc.theoryName,
							formulaName: this.desc.formulaName,
							contextFolder: this.desc.contextFolder,
							cmd: cmd.startsWith("(") ? cmd : `(${cmd})`
						});
						break;		
					}
					case "root":
					case "proof-branch": 
					default: {
						if (cmd && (cmd === "(undo)" || cmd === "undo" || cmd.startsWith("(undo ") || cmd.startsWith("undo "))) {
							this.moveIndicatorBack();
						} else {
							// propagate tooltip
							const tooltip: string = this.activeNode.tooltip;
							this.moveIndicatorForward();
							this.activeNode.tooltip = tooltip;
						}
						this.refreshView();
						if (this.running) {
							this.step();
						}
						break;	
					}
				}
			}
		} else {
			this.running = false;
		}
		return activeCommand;
	}
	fastForward (node: ProofItem): void {
		this.stopAt = node;
		this.running = true;
		this.step();
	}
	deleteNode (node: ProofItem): void {
		if (node && node.parent) {
			const parent: ProofItem = node.parent;
			const sibling: ProofItem = node.getSibling();
			const wasActive: boolean = node.isActive();
			parent.deleteChild(node);
			this.nodes = this.nodes.filter((nd: ProofItem) => {
				return nd.id !== node.id;
			});
			if (wasActive && sibling) {
				this.activeNode = sibling;
				sibling.active();
			}
			this.refreshView();
		}
	}
	renameNode (node: ProofItem): void {
		// TODO
		// if (node && node.parent) {
		// 	const parent: ProofItem = node.parent;
		// 	const sibling: ProofItem = node.getSibling();
		// 	const wasActive: boolean = node.isActive();
		// 	parent.deleteChild(node);
		// 	this.nodes = this.nodes.filter((nd: ProofItem) => {
		// 		return nd.id !== node.id;
		// 	});
		// 	if (wasActive && sibling) {
		// 		this.activeNode = sibling;
		// 		sibling.active();
		// 	}
		// 	this.refreshView();
		// }
	}
	jumpTo (node: ProofItem): void {
		if (node) {
			const targetNodes: ProofItem[] = this.nodes.filter((elem: ProofItem) => {
				return elem.id === node.id;
			});
			if (targetNodes && targetNodes.length === 1) {
				this.activeNode.restore();
				this.activeNode = targetNodes[0];
				this.activeNode.active();
			}
			this.refreshView();
		}
	}
	goto (node: ProofItem): void {
		this.jumpTo(node);
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
	protected refreshView(): void {
		this._onDidChangeTreeData.fire();
	}

	resetView(): void {
		this.nodes = [];
		this.root = null;
		this.refreshView();
	}

	onStepExecuted (desc: { response: PvsResponse, args: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string, cmd: string }}): void {
		if (desc && desc.response && desc.response.result && desc.args && desc.args.cmd) {
			const cmd: string = desc.args.cmd;
			const activeCommand: ProofCommand = this.getActiveCommand();
			if (activeCommand) {
				if (cmd === "(undo)" || cmd.startsWith("(undo ")) {
					this.pendingExecution = false;
					// const previous: ProofItem = activeCommand.getPrevious();
					this.moveIndicatorBack();
				} else {
					if (this.pendingExecution) {
						this.moveIndicatorForward();
						if (desc.response && desc.response.result) {
							this.activeNode.proofState = <ProofStateNode> desc.response.result;
							this.activeNode.tooltip = utils.formatProofState(this.activeNode.proofState);
						} else {
							console.error("[vscode-pvs-proof-explorer] Warning: could not save proof state information");
						}
						// the following block handles the execution of a series of command
						if (this.running) {
							this.step();
						} else {
							this.pendingExecution = false;
						}
					} else {
						// command entered manually -- append a new node after the active node
						activeCommand.concat(cmd);
					}
				}
				this.refreshView();
			}
		} else {
			console.error("[vscode-pvs-proof-explorer.onActiveCommandExecuted] Warning: null descriptor.");
		}
	}

	setProofDescriptor (desc: { fileName: string, fileExtension: string, theoryName: string, formulaName: string, contextFolder: string }): void {
		this.desc = desc;
	}

	showProofScript (script: string): void {
		const proofTree: ProofTree = utils.proofScriptToJson(script);
		this.loadProofTree(proofTree);
	}

	setInitialProofState (proofState: ProofStateNode): void {
		this.initialProofState = proofState;
	}

	activateSelectedProof (): void {
		this.root.pending();
	}



	/**
	 * Internal function, loads a proof tree into proof-explorer
	 * @param proofTree The proof to be loaded, in JSON format
	 */
	protected loadProofTree (proofTree: ProofTree): void {
		const makeTree = (elem: { id: string, children: any[], type: string }, parent: ProofItem): void => {
			const node: ProofItem = (elem.type === "proof-command") ? new ProofCommand(elem.id, parent) : new ProofBranch(elem.id, parent);
			parent.appendChild(node);
			this.nodes.push(node);
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
		if (proofTree && proofTree.proofStructure) {
			const item: RootNode = new RootNode(proofTree.proofStructure.id);
			this.root = item; // this is the proof name
			if (proofTree.proofStructure.children && proofTree.proofStructure.children.length) {
				proofTree.proofStructure.children.forEach(child => {
					makeTree(child, item);
				});
			} else {
				item.collapsibleState = TreeItemCollapsibleState.None;
			}
			this.proofTree = proofTree;
			this.activeNode = (this.root.children && this.root.children.length) ? this.root.children[0] : this.root;
			this.activeNode.proofState = this.initialProofState;
			this.activeNode.tooltip = utils.formatProofState(this.activeNode.proofState);
		}
		// register handler for terminal closed events
		window.onDidCloseTerminal((e: Terminal) => {
			if (e && e.name === proofTree.proofName) {
				this.resetView();
			}
		});
		// update placeholders
		this.moveIndicatorAtBeginning();
		// update front-end
		this.refreshView();
	}

	/**
	 * Handlers for messages received from the server
	 */
	protected installHandlers(context: ExtensionContext) {
		// this.client.onRequest('server.response.step-proof', (ans: string) => {
		// 	this.fromJSON(JSON.parse(ans));
		// 	this.startProof();
		// });
		// this.client.onRequest('server.response.step-tcc', (ans: string) => {
		// 	this.fromJSON(JSON.parse(ans));
		// 	this.startProof();
		// });
		// this.client.onRequest("server.response.prover", (ans) => {
		// });
	}
	
	/**
	 * Handler activation function
	 * @param context Client context 
	 */
	activate(context: ExtensionContext) {
		this.installHandlers(context);

		// -- user commands sent to pvsTerminalCli
		context.subscriptions.push(commands.registerCommand("proof-explorer.forward", () => {
			this.step();
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.back", () => {
			this.step("undo");
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.run", () => {
			this.run();
		}));
		// cmd = commands.registerCommand("terminal.pvs.response.step-executed", () => {
		// 	// this.refreshView();
		// });
		// context.subscriptions.push(cmd);
		// cmd = commands.registerCommand("terminal.pvs.response.step-proof-ready", () => {
		// 	this.startProof();
		// });
		// context.subscriptions.push(cmd);

		// -- proof explorer commands
		context.subscriptions.push(commands.registerCommand("proof-explorer.jump-to", (resource: ProofItem) => {
			if (resource && this.nodes) {
				this.jumpTo(resource);
			} else {
				window.showErrorMessage(`Error while trying to move to command ${resource.name}`);
			}
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.fast-forward", (resource: ProofItem) => {
			if (resource && this.nodes) {
				this.fastForward(resource);
				// this.refreshView();
			} else {
				window.showErrorMessage(`Error while trying to fast-forward to ${resource.name}`);
			}
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.delete-proof-command", (resource: ProofItem) => {
			if (resource && this.nodes) {
				this.deleteNode(resource);
				// this.refreshView();
			} else {
				window.showErrorMessage(`Error while trying to delete node ${resource.name}`);
			}
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.rename-proof-command", (resource: ProofItem) => {
			if (resource && this.nodes) {
				this.renameNode(resource);
				// this.refreshView();
			} else {
				window.showErrorMessage(`Error while trying to rename node ${resource.name}`);
			}
		}));
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
