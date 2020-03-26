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
import { ExtensionContext, TreeItemCollapsibleState, commands, window, 
			TreeItem, Command, EventEmitter, Event,
			TreeDataProvider, TreeView, Terminal } from 'vscode';
import { LanguageClient } from 'vscode-languageclient';
import { ProofTree, ProofNode, ProofNodeType } from '../common/serverInterface';
import * as utils from '../common/languageUtils';
import * as fsUtils from '../common/fsUtils';
import { PvsResponse } from '../common/pvs-gui';
import { ProofState } from '../common/languageUtils';
import * as vscode from 'vscode';


/**
 * TreeData provider for Proof Explorer
 */
export class VSCodePvsProofExplorer implements TreeDataProvider<TreeItem> {
	protected logFileName: string;
	protected tmpLogFileName: string;

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
	 * Clipboards for cut/paste operations
	 */
	protected clipboard: ProofItem = null;
	protected clipboardTree: ProofItem = null;

	/**
	 * Descriptor with information on the active proof
	 **/
	protected desc: { contextFolder: string, fileName: string, fileExtension: string, theoryName: string, formulaName: string };

	/**
	 * Name of the view associated with the data provider
	 */
	protected providerView: string;
	protected view: TreeView<TreeItem>;

	/**
	 * Attributes for run-time management of the proof tree rendered in the view
	 */
	protected root: RootNode; // the root of the tre
	protected ghostNode: GhostNode; // used when developing a proof, signposts where the next proof command will be appended in the proof tree
	protected activeNode: ProofCommand | ProofBranch | GhostNode;

	protected pendingExecution: boolean = false;
	protected running: boolean = false; // status flag, indicates whether we are running all proof commands, as opposed to stepping through the proof commands
	protected stopAt: ProofItem = null; // indicates which node we are fast-forwarding to

	/**
	 * JSON representation of the proof tree, updated at the beginning and end of the proof session
	 */
	protected proofTree: ProofTree = null;

	/**
	 * Initial proof state
	 */
	protected initialProofState: ProofState;
	

	/**
	 * @constructor
	 * @param client Language client 
	 * @param providerView Name of the VSCode view linked to proof explorer
	 */
	constructor(client: LanguageClient, providerView: string) {
		this.client = client;
		this.providerView = providerView;
		// Register tree view; use window.createTreeView instead of window.registerDataProvider -- this allows to perform UI operations programatically. 
		// window.registerTreeDataProvider(this.providerView, this);
		this.view = window.createTreeView(this.providerView, { treeDataProvider: this });
		this.root = new RootNode("Proof explorer is not active");
		this.ghostNode = new GhostNode({ parent: this.root, node: this.root });
	}

	/**
	 * Returns the active node in the proof tree
	 * @returns {ProofItem} The active node 
	 */
	getActiveNode (): ProofItem {
		return this.activeNode;
	}
	/**
	 * Internal function, marks a node as active, i.e., ready to be executed. Automatically updates all dependent data structures (e.g., ghostNode and tooltips)
	 * External classes should use setAsActive
	 * @param desc 
	 */
	protected setActiveNode (desc: { selected: ProofItem, tooltip?: string }): void {
		if (desc && desc.selected) {
			switch (desc.selected.contextValue) {
				// root should never be the active node
				case "root": {
					this.activeNode = this.ghostNode;
					this.ghostNode.parent = this.root;
					this.ghostNode.realNode = this.root;
					break;
				}
				default: {
					this.activeNode = desc.selected;
				}
			}
			this.activeNode.active();
			if (this.activeNode.contextValue !== "ghost") {
				this.ghostNode.parent = this.activeNode.parent;
				this.ghostNode.realNode = this.activeNode;
				this.ghostNode.notVisited();
			}
			if (desc.tooltip) {
				this.activeNode.tooltip = desc.tooltip;
			}
		} else {
			console.warn(`[proof-explorer] Warning: could not set active node (selected node is null)`);
		}
	}
	/**
	 * Internal function, marks the first node in the proof tree as active
	 */
	protected moveIndicatorAtBeginning (): void {
		if (this.root) {
			this.setActiveNode({ selected: this.root });
			this.refreshView();
		} else {
			console.error("Error: unable to render proof tree");
		}
	}
	/**
	 * Internal function, moves the active node one position back in the proof tree.
	 */
	protected moveIndicatorBack (): void {
		if (this.activeNode) {
			const prev: ProofItem = this.activeNode.moveIndicatorBack();
			this.setActiveNode({ selected: prev });
			this.refreshView();
		} else {
			console.warn("[vscode-pvs-proof-explorer] Warning: active node is null");
		}
	}
	/**
	 * Internal function, moves the active node one position forward in the proof tree.
	 */
	protected moveIndicatorForward (): void {
		if (this.activeNode) {
			if (this.activeNode.contextValue !== "ghost") {
				const next: ProofItem = this.activeNode.moveIndicatorForward() || this.ghostNode;
				this.setActiveNode({ selected: next });
				this.refreshView();
			}
		} else {
			console.warn("[vscode-pvs-proof-explorer] Warning: active node is null");
		}
	}
	/**
	 * Executes all proof commands in the proof tree, starting from the active node.
	 * The execution stops either at the end of the proof tree, or when an anomaly 
	 * is detected in the proof tree (e.g,. the prover generates more goals than those 
	 * indicated in the proof tree)
	 */
	run (): void {
		if (!this.root.isQED()) {
			this.running = true;
			this.step();
		}
	}
	/**
	 * Executes all proof commands in the proof tree, starting from the active node, 
	 * up to the selected node specified as parameter.
	 * The execution stops either at the end of the proof tree, or when an anomaly 
	 * is detected in the proof tree (e.g,. the prover generates more goals than those 
	 * indicated in the proof tree)
	 * @param desc Descriptor of the selected node where the execution should stop.
	 */
	fastForwardTo (desc: { selected: ProofItem }): void {
		if (desc && desc.selected) {
			if (!this.root.isQED()) {
				this.stopAt = desc.selected;
				this.running = true;
				this.step();
			} else {
				console.warn(`[proof-explorer] Warning: trying to fast forward to node ${desc.selected.name} when the proof is already completed`);
			}
		} else {
			console.warn(`[proof-explorer] Warning: failed to fast forward (selected node is null)`);
		}
	}
	/**
	 * Executes a proof command. 
	 * The command is taken from the proof tree.
	 * The command can also be specified as function argument -- this is useful for handling commands entered by the used at the prover prompt. 
	 * @param cmd Optional parameter, specifying the command to be executed.
	 */
	step (cmd?: string): void {
		if (this.root.isQED()) {
			this.running = false;
			this.pendingExecution = false;
		} else {
			if (this.stopAt === this.activeNode) {
				this.stopAt = null;
				this.running = false;
				this.pendingExecution = false;
			} else {
				this.pendingExecution = true;
				switch (this.activeNode.contextValue) {
					case "proof-command": {
						cmd = cmd || this.activeNode.name;
						const parent: ProofItem = this.activeNode.parent;
						if (parent) {
							if (utils.isUndoCommand(cmd) && parent.contextValue === "proof-branch" && this.activeNode.id === parent.children[0].id) {
								// the active node is the first child in a branch. 
								// The command has not been executed yet, just move the indicator back without sending any command to pvs-server
								this.moveIndicatorBack();
							} else {
								commands.executeCommand("vscode-pvs.send-proof-command", {
									fileName: this.desc.fileName,
									fileExtension: this.desc.fileExtension,
									theoryName: this.desc.theoryName,
									formulaName: this.desc.formulaName,
									contextFolder: this.desc.contextFolder,
									cmd: cmd.startsWith("(") ? cmd : `(${cmd})`
								});
							}
						} else {
							console.error(`[proof-explorer] Error: proof command ${this.activeNode.name} does not have a parent`);
						}
						break;		
					}
					case "ghost": {
						this.running = false;
						break;
					}
					case "root":
					case "proof-branch": 
					default: {
						if (utils.isUndoCommand(cmd)) {
							if (this.activeNode.contextValue === "proof-branch") {
								commands.executeCommand("vscode-pvs.send-proof-command", {
									fileName: this.desc.fileName,
									fileExtension: this.desc.fileExtension,
									theoryName: this.desc.theoryName,
									formulaName: this.desc.formulaName,
									contextFolder: this.desc.contextFolder,
									cmd: cmd.startsWith("(") ? cmd : `(${cmd})`
								});
								// this.moveIndicatorBack();
							}
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
		}
	}
	/**
	 * Call-back function invoked after step(), when the execution of a proof command is complete.
	 * @param desc Descriptor specifying the reponse of the prover, as well as the actual values of the arguments used to invoke the step function.
	 */
	onStepExecuted (desc: { response: PvsResponse, args: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string, cmd: string }}): void {
		if (desc && desc.response && desc.response.result && desc.args && desc.args.cmd) {
			const cmd: string = desc.args.cmd;
			if (utils.isQuitCommand(cmd)) {
				this.disposeView();
				return;	
			} else if (utils.isEmptyCommand(cmd) || utils.isInvalidCommand(desc.response.result)) {
				const msg: string = (desc.response.result.commentary && desc.response.result.commentary.length) ?
					desc.response.result.commentary[0]
					: `${desc.args.cmd} is not a valid command.`;
				window.showWarningMessage(msg);
				// @TODO: send warning message to the terminal
				// commands.executeCommand("vscode-pvs.print-warning-message-in-terminal", {
				// 	fileName: this.desc.fileName,
				// 	fileExtension: this.desc.fileExtension,
				// 	theoryName: this.desc.theoryName,
				// 	formulaName: this.desc.formulaName,
				// 	contextFolder: this.desc.contextFolder,
				// 	msg: "warning-msg"
				// });
				return;
			}
			if (utils.isQED(desc.response.result)) {
				this.QED();
				return;
			}
			if (desc.response && desc.response.result) {
				const proofState: ProofState = <ProofState> desc.response.result;
				const previousBranchId: string = this.activeNode.branchId;
				const newBranchId: string = utils.getBranchId(proofState.label);
				const branchHasChanged: boolean = previousBranchId !== newBranchId
													|| (previousBranchId !== "" && newBranchId.indexOf(previousBranchId) !== 0);
				if (cmd === "(undo)" || cmd.startsWith("(undo ")) {
					this.pendingExecution = false;
					if (branchHasChanged) {
						const targetBranch: ProofBranch = this.findProofBranch(newBranchId);
						if (targetBranch) {
							const msg: string = (newBranchId === "") ? `moving back to root branch` : `moving back to branch (${newBranchId})`;
							window.showInformationMessage(msg);
							this.activeNode.notVisited();
							// go to the last visited child command in this branch
							const visitedChildren: ProofCommand[] = targetBranch.children.filter((elem: ProofItem) => {
								return elem.contextValue === "proof-command" && elem.isVisited();
							});
							const targetNode: ProofItem = visitedChildren.length ? visitedChildren[visitedChildren.length - 1] : targetBranch;
							targetNode.treeNotVisited();
							this.setActiveNode({ selected: targetNode });
						} else {
							window.showWarningMessage(`Warning: could not find branch ${newBranchId} in the proof tree`);
						}
					} else {
						this.moveIndicatorBack();
					}
				} else {	
					// pendingExecution is false when the user manually enters a command at the prover terminal 
					if (!this.pendingExecution) {
						// command entered manually -- append a new node after the active node, only if the command entered at the prompt is different than the active command shown in the proof tree
						if (`(${this.activeNode.name})` !== cmd) {
							// concatenate new command
							const elem: ProofCommand = new ProofCommand(cmd, this.activeNode.branchId, this.activeNode.parent, TreeItemCollapsibleState.None)
							this.appendNode({ selected: this.activeNode, elem }, { beforeSelected: true });
							this.setActiveNode({ selected: elem }); // this is necessary to correctly update the data structures in endNode --- elem will become the parent of endNode
							window.showInformationMessage(`${elem.name} added to the proof tree.`);
						}
					}
					// check if the number of sub-goals is consistent with the current proof structure
					const n: number = (proofState["num-subgoals"] > 1) ? (proofState["num-subgoals"] - this.activeNode.children.length) : 0;
					// stop execution and notify user if n != 0
					if (n) {
						const msg: string = (n > 0) ? `${n} sub goals added to the proof tree.`
							: `Proof tree has more sub goals than expected (expected ${proofState["num-subgoals"]}, found ${this.activeNode.children.length}).`; 
						window.showInformationMessage(msg);
						if (this.running) {
							this.running = false;
							window.showWarningMessage(`Proof script might be broken (Mismatching number of sub goals). Stopping execution of proof script.`);
						}
					}
					if (!this.running && proofState["num-subgoals"] > 1) {
						window.showInformationMessage(`Proof command ${desc.args.cmd} generated ${proofState["num-subgoals"]} sub-goals`);
						// append branches if n > 0
						for (let i = 0; i < n; i++) {
							this.appendBranch({ selected: this.activeNode });
						}
					}
					if (branchHasChanged) {
						const elem: ProofItem = this.findProofBranch(newBranchId);
						if (elem) {
							window.showInformationMessage(`moving to branch (${newBranchId})`);
							this.activeNode.visited();
							this.setActiveNode({ selected: elem });
						} else {
							window.showWarningMessage(`Warning: could not find branch ${newBranchId} in the proof tree.`);
						}
					}
					// move indicator forward
					this.moveIndicatorForward();
					// propagate tooltip
					this.activeNode.proofState = proofState;
					this.activeNode.setTooltip(utils.formatProofState(this.activeNode.proofState));
				}
				this.refreshView();
				if (this.running && this.pendingExecution) {
					this.step();
				} else {
					this.pendingExecution = false;
				}
			} else {
				if (this.running) {
					window.showErrorMessage("Error: could not read proof state information.");
					this.pendingExecution = false;
					this.running = false;
				}
				// else, do nothing -- invalid command entered at the prover prompt, don't update the proof tree
			}
		} else {
			console.error("[proofExplorer] Error: onStepExecuted invoked with a null or incomplete descriptor.");
		}
	}
	/**
	 * Internal function, used for finding the node relative to a given proof branch
	 * @param id Name of the proof branch. Branch names are specified using a dot notation (e.g., 1.3.2)
	 */
	protected findProofBranch (id: string): ProofBranch {
		if (id === "") {
			return this.root;
		}
		// else
		const findNodeAux = (id: string, node: ProofItem): ProofBranch | null => {
			if (node && node.contextValue === "proof-branch" && node.branchId === id) {
				return <ProofBranch> node;
			}
			for (let i = 0; i < node.children.length; i++) {
				const res: ProofItem = findNodeAux(id, node.children[i]);
				if (res) {
					return res;
				}
			}
			return null;
		}
		return findNodeAux(id, this.root);
	}
	/**
	 * Utility function, marks the selected node as pending
	 * @param desc Descriptor of the selected node
	 */
	markAsPending (desc: { selected: ProofItem }): void {
		if (desc && desc.selected && desc.selected.id !== this.activeNode.id) {
			if (desc.selected.contextValue === "proof-branch") {
				desc.selected.treeNotVisited();
				desc.selected.pending();
			} else {
				desc.selected.pending();
			}
			this.refreshView();
		} else {
			console.warn(`[proof-explorer] Warning: could not mark node as visited`);
		}
	}
	/**
	 * Utility function, marks the selected node as visited
	 * @param desc Descriptor of the selected node
	 */
	markAsVisited (desc: { selected: ProofItem }): void {
		if (desc && desc.selected && desc.selected.id !== this.activeNode.id) {
			if (desc.selected.contextValue === "proof-branch") {
				desc.selected.treeVisited();
			} else {
				desc.selected.visited();
			}
			this.refreshView();
		} else {
			console.warn(`[proof-explorer] Warning: could not mark node as visited`);
		}
	}
	/**
	 * Utility function, marks the selected node as not-visited
	 * @param desc Descriptor of the selected node
	 */
	markAsNotVisited (desc: { selected: ProofItem }): void {
		if (desc && desc.selected && desc.selected.id !== this.activeNode.id) {
			if (desc.selected.contextValue === "proof-branch") {
				desc.selected.treeNotVisited();
			} else {
				desc.selected.notVisited();
			}
			this.refreshView();
		} else {
			console.warn(`[proof-explorer] Warning: could not to mark node as not visited`);
		}
	}
	/**
	 * Utility function, marks the selected node as active.
	 * @param desc Descriptor of the selected node
	 */
	setAsActive (desc: { selected: ProofItem }): void {
		if (desc && desc.selected) {
			this.running = false;
			if (desc.selected !== this.activeNode) {
				// const tooltip: string = this.activeNode.tooltip;
				this.activeNode.restore();
				if (desc.selected.isVisitedOrPending()) {
					// the node has already been executed, activate ghost node
					this.ghostNode.parent = desc.selected.parent;
					this.ghostNode.realNode = desc.selected;
					this.ghostNode.active();
				} else {
					this.setActiveNode({ selected: desc.selected });
				}
				this.refreshView();
			}
		} else {
			console.warn(`[proof-explorer] Warning: failed to set active node`);
		}
	}
	/**
	 * Appends a new node to the proof tree.
	 * If the new node is not provided as argument, then a dialogue is used to ask the user to enter the proof command to be appended.
	 * The behavior of appendNode is as follows:
	 * - selected node === proof command or endNode --> place the content of the clipboard as sibling after the selected node (or before, when beforeSelected is true)
	 * - selected node === branch or root --> place the content of the clipboard as first child 
	 * @param desc Descriptor indicating the selected node and the new element to be appended in the proof tree.
	 * 				If the new element is not specified, the function automatically queries the user to enter a proof command
	 * @param opt Options: beforeSelected (boolean) allows to append the new element before the selected node (rather than after)
	 */
	async appendNode (desc: { selected: ProofItem, elem?: ProofItem }, opt?: { beforeSelected?: boolean }): Promise<void> {
		if (desc && desc.selected) {
			opt = opt || {};
			const branchId: string = desc.selected.branchId;
			const parent: ProofItem = desc.selected.parent;
			let newNode: ProofItem = desc.elem;
			if (!newNode) {
				const cmd: string = await vscode.window.showInputBox({
					prompt: `Please enter proof command to be appended after ${desc.selected.name}`,
					placeHolder: ``,
					value: ``,
					ignoreFocusOut: true 
				});
				if (cmd) {
					newNode = new ProofCommand(cmd, branchId, parent, TreeItemCollapsibleState.None);
				}
			}
			if (newNode) {
				newNode.parent = parent;
				switch (desc.selected.contextValue) {
					case "root":
					case "proof-branch": {
						desc.selected.appendChildAtBeginning(newNode);
						break;
					}
					case "ghost":
					case "proof-command": {
						desc.selected.appendSibling(newNode, opt);
						break;
					}
					default: {
						// do nothing
					}
				}
				this.refreshView();
			} else {
				console.warn(`[proof-explorer] Warning: failed to create new node`)
			}
		} else {
			console.warn(`[proof-explorer] Warning: failed to insert after (node is null)`)
		}
	}
	/**
	 * Appends a new branch to the proof tree. The branch name is automatically computed based on the structure of the proof tree.
	 * @param desc Descriptor of the node where the branch will be appended.
	 * 			   If the selected node is a branch, then the branch will be appended to the parent of the selected branch.
	 * @param opt Optionals: beforeSelected (boolean) flag used when the selected node is a branch, indicates whether the branch should be appended before the selected branch. 
	 */
	appendBranch (desc: { selected: ProofItem, elem?: ProofItem }, opt?: { beforeSelected?: boolean }): void {
		if (desc && desc.selected) {
			opt = opt || {};
			let selectedNode: ProofItem = desc.selected;
			const branchId: string = selectedNode.branchId;
			let newBranch: ProofItem = desc.elem;
			switch (selectedNode.contextValue) {
				case "ghost": {
					selectedNode = (<GhostNode>selectedNode).realNode;
				}
				case "proof-command": {
					if (!newBranch) {
						let branchId: string = utils.makeBranchId({ branchId: selectedNode.branchId, goalId: 1 });
						// check which is the first available branch name -- there might be holes if branches have been deleted/renamed by the user
						let found: boolean = false;
						for (let i = 0; i < selectedNode.children.length && !found; i++) {
							branchId = utils.makeBranchId({ branchId: selectedNode.branchId, goalId: i + 1 });
							if (selectedNode.children[i].branchId !== branchId) {
								found = true;
							}
						}
						newBranch = new ProofBranch(branchId, branchId, desc.selected, TreeItemCollapsibleState.Expanded);
					}
					if (newBranch) {
						desc.selected.appendChild(newBranch);
						this.refreshView();
					}
					break;
				}
				case "root":
				case "proof-branch": {
					const parent: ProofItem = selectedNode.parent;
					if (parent && parent.children) {
						if (!newBranch) {
							const branchName: string = (opt.beforeSelected) ? `` : `${branchId}.${parent.children.length}`;
							newBranch = new ProofBranch(branchName, branchId, parent, TreeItemCollapsibleState.Expanded);
						}
						if (newBranch) {
							newBranch.parent = parent;
							const children: ProofItem[] = [];
							const n: number = parent.children.length;
							for (let i = 0; i < n; i++) {
								if (!opt.beforeSelected) {
									children.push(parent.children[i]);
								}
								if (parent.children[i].id === selectedNode.id) {
									children.push(newBranch);
								}
								if (opt.beforeSelected) {
									children.push(parent.children[i]);
								}
							}
							parent.children = children;
							this.refreshView();
						}
					}
				}
				default: {
					// do nothing
				}
			}
		} else {
			console.warn(`[proof-explorer] Warning: failed to insert after (node is null)`)
		}
	}
	/**
	 * Copies the selected node to the clipboard (i.e., the clipboard will store a copy of the selected node)
	 * @param desc Descriptor of the selected node.
	 */
	copyNode (desc: { selected: ProofItem }): void {
		if (desc && desc.selected) {
			this.clipboard = desc.selected.clone();
			this.clipboardTree = null;
			// set vscode context variable proof-explorer.clipboard-contains-node to true
			vscode.commands.executeCommand('setContext', 'proof-explorer.clipboard-contains-node', true);
		} else {
			console.warn(`[proof-explorer] Warning: unable to copy selected node`);
		}
	}
	/**
	 * Copies the tree rooted at the selected node to the clipboard 
	 * (i.e., the clipboard will store a copy of the tree rooted at the selected node)
	 * @param desc Descriptor of the selected node.
	 */
	copyTree (desc: { selected: ProofItem }): void {
		if (desc && desc.selected) {
			this.clipboardTree = desc.selected.cloneTree();
			this.clipboard = null;
			// set vscode context variable proof-explorer.clipboard-contains-node to true
			vscode.commands.executeCommand('setContext', 'proof-explorer.clipboard-contains-node', true);
		} else {
			console.warn(`[proof-explorer] Warning: unable to copy selected subtree`);
		}
	}
	/**
	 * Appends the node in the clipboard to the selected node. 
	 * @param desc Descriptor of the selected node where the content of the clipboard will be appended.
	 * @param opt Optionals parameters (see appendNode)
	 */
	pasteNode (desc: { selected: ProofItem }, opt?: { beforeSelected?: boolean }): void {
		if (desc && desc.selected) {
			opt = opt || {};
			if (this.clipboard) {
				this.appendNode({ selected: desc.selected, elem: this.clipboard.clone() }, opt);
			} else if (this.clipboardTree) {
				this.appendNode({ selected: desc.selected, elem: this.clipboardTree.cloneTree() });
			} else {
				console.warn(`[proof-explorer] Warning: unable to paste (clipboard is empty)`);
			}
		} else {
			console.warn(`[proof-explorer] Warning: unable to paste (selected node is null)`);
		}
	}
	/**
	 * Copies the selected node to the clipboard and then deletes the selected node. 
	 * Equivalent to copyNode + deleteNode. 
	 * @param desc Descriptor of the selected node.
	 */
	cutNode (desc: { selected: ProofItem }): void {
		if (desc && desc.selected) {
			this.copyNode(desc);
			this.deleteNode(desc, { confirm: false });
		} else {
			console.warn(`[proof-explorer] Warning: unable to cut node (selected node is null)`);
		}
	}
	/**
	 * Copies the tree rooted at the selected node to the clipboard and then deletes the selected node. 
	 * Equivalent to copyTree + deleteNode. 
	 * @param desc Descriptor of the selected node.
	 */
	cutTree (desc: { selected: ProofItem }): void {
		if (desc && desc.selected) {
			this.copyTree(desc);
			this.deleteNode(desc, { confirm: false });
		} else {
			console.warn(`[proof-explorer] Warning: unable to cut tree (selected node is null)`);
		}
	}
	/**
	 * Deletes the selected node.
	 * A confirmation dialog is automatically displayed (unless optional parameters indicate not to show it) to ask confirmation of the operation.
	 * @param desc Descriptor of the selected node.
	 * @param opt Optionals parameters: confirm (boolean) indicates whether a confirmation dialog should be displayed before deleting the node.
	 */
	async deleteNode (desc: { selected: ProofItem }, opt?: { confirm?: boolean }): Promise<void> {
		opt = opt || {};
		opt.confirm = (opt.confirm === undefined) ? true : opt.confirm; // choose to confirm if options are not specified
		if (desc && desc.selected && desc.selected.parent) {
			const selected: ProofItem = desc.selected;
			const yesno: string[] = [ "Yes", "Cancel" ];
			const msg: string = selected.contextValue === "root" ? `Delete entire proof?` : `Delete ${selected.name}?`;
			const ans: string = (opt.confirm) ?
				await vscode.window.showInformationMessage(msg, { modal: true }, yesno[0])
				: yesno[0];
			if (ans === yesno[0]) { 
				if (selected.contextValue === "root") {
					selected.children = [];
					this.setActiveNode({ selected: this.root });
				} else {
					const wasActive: boolean = selected.isActive();
					if (wasActive) {
						const sibling: ProofItem = selected.getNextSibling() || this.ghostNode;
						if (sibling === this.ghostNode) {
							this.ghostNode.parent = selected.parent;
							this.ghostNode.realNode = selected.getSiblingOrParent() || this.root;
						}
						this.setActiveNode({ selected: sibling });
					}
					selected.parent.deleteChild(selected);
				}
				this.refreshView();
			}
		} else {
			console.warn(`[proof-explorer] Warning: unable to delete selected node`);
		}
	}
	/**
	 * Deletes the proof commands after the selected node.
	 * - If the node has children, deletes all children; 
	 * - If the node has lower siblings, deletes all lower siblings
	 * @param desc Descriptor indicating which node is selected in the proof tree
	 * @param opt Optionals: whether user confirmation is required
	 */
	async trimNode (desc: { selected: ProofItem }, opt?: { confirm?: boolean }): Promise<void> {
		opt = opt || {};
		opt.confirm = (opt.confirm === undefined) ? true : opt.confirm; // choose to confirm if options are not specified
		if (desc && desc.selected && desc.selected.parent) {
			const node: ProofItem = desc.selected;
			const yesno: string[] = [ "Yes", "Cancel" ];
			const msg: string = node.contextValue === "root" ? `Delete entire proof?` 
									: node.contextValue === "proof-branch" ? `Delete proof commands in branch ${node.name}?`
									: `Delete proof commands after ${node.name}?`;
			const ans: string = (opt.confirm) ?
				await vscode.window.showInformationMessage(msg, { modal: true }, yesno[0])
				: yesno[0];
			if (ans === yesno[0]) {
				switch (node.contextValue) {
					case "root": {
						node.children = [];
						this.setActiveNode({ selected: this.root });
						break;
					}
					case "proof-branch": {
						// remove all children in this branch
						let childWasPendingOrActive: boolean = false;
						for (let i = 0; i < node.children.length && !childWasPendingOrActive; i++) {
							if (node.children[i].isPending() || node.children[i].isActive()
									|| (node.children[i] === this.ghostNode.realNode && this.ghostNode.isActive()) ) {
								childWasPendingOrActive = true;
							}
						}
						if (childWasPendingOrActive) {
							// mark the current branch as active
							this.setActiveNode({ selected: node });
						}
						node.children = [];
						break;
					}
					case "proof-command": {
						// remove lower siblings
						const parent: ProofItem = node.parent;
						const idx: number = parent.children.indexOf(node);
						parent.children = parent.children.slice(0, idx + 1);
						if (node.isVisited()) {
							this.setActiveNode({ selected: node });
						}
						break;
					}
					default: {
						console.warn(`[proof-explorer] Warning: unrecognized node type ${node.contextValue} detected while trimming ${node.name}`);
					}
				}
				this.refreshView();
			}
		} else {
			console.warn(`[proof-explorer] Warning: unable to delete selected node`);
		}
	}
	/**
	 * Renames the selected node. The new name is entered using a dialog window.
	 * @param desc Descriptor of the selected node.
	 */
	async renameNode (desc: { selected: ProofItem }): Promise<void> {
		if (desc && desc.selected) {
			const node: ProofItem = desc.selected;
			const newName: string = await vscode.window.showInputBox({ prompt: `Renaming proof command ${node.name}`, placeHolder: `${node.name}`, value: `${node.name}`, ignoreFocusOut: true });
			if (newName) {
				node.rename(newName);
			}
			this.refreshView();
		} else {
			console.warn(`[proof-explorer] Warning: unable to rename selected node`);
		}
	}
	/**
	 * Internal function, used to refresh the tree view
	 */
	protected refreshView(): void {
		this._onDidChangeTreeData.fire();
	}
	/**
	 * Internal function, used to delete the tree view
	 */
	protected disposeView(): void {
		this.deleteNode({ selected: this.root }, { confirm: false });
		vscode.commands.executeCommand('setContext', 'proof-explorer.clipboard-contains-node', false);
		this.refreshView();
	}

	/**
	 * Utility function, hides proof explorer view in the IDE
	 */
	hide(): void { commands.executeCommand('setContext', 'prover-session-active', false); }

	/**
	 * Utility function, reveals proof explorer view in the IDE
	 */
	reveal(): void { commands.executeCommand('setContext', 'prover-session-active', true); }

	/**
	 * Utility function, marks the current proof as complete
	 */
	QED (): void {
		// make sure execution stops
		this.pendingExecution = false;
		this.running = false;
		// move indicator forward so any proof branch that needs to be marked as visited will be marked
		this.activeNode.moveIndicatorForward();
		// set QED
		this.root.QED();
		// refresh tree view
		this.refreshView();

		// send QED to the terminal
		commands.executeCommand("vscode-pvs.send-proof-command", {
			fileName: this.desc.fileName,
			fileExtension: this.desc.fileExtension,
			theoryName: this.desc.theoryName,
			formulaName: this.desc.formulaName,
			contextFolder: this.desc.contextFolder,
			cmd: "Q.E.D."
		});

		// re-generate tccs
		commands.executeCommand("vscode-pvs.generate-tccs", {
			fileName: this.desc.fileName,
			fileExtension: this.desc.fileExtension,
			theoryName: this.desc.theoryName,
			formulaName: this.desc.formulaName,
			contextFolder: this.desc.contextFolder
		});
	}

	/**
	 * Utility function, used to identify which formula is being proved in the proof tree session
	 * @param desc 
	 */
	setProofDescriptor (desc: { fileName: string, fileExtension: string, theoryName: string, formulaName: string, contextFolder: string }): void {
		this.desc = desc;
	}

	/**
	 * Utility function, shows a pvs proof script in proof explorer.
	 * @param script 
	 */
	showProofScript (script: string): void {
		const proofTree: ProofTree = utils.proofScriptToJson(script);
		this.loadProofTree(proofTree);
	}

	/**
	 * Utility function, used to set the initial proof state.
	 * @param proofState 
	 */
	setInitialProofState (proofState: ProofState): void {
		this.initialProofState = proofState;
	}

	/**
	 * Utility function, activates the proof loaded in the view (i.e., sets the first node as active)
	 */
	activateSelectedProof (): void {
		if (this.root) {
			this.root.pending();
			if (this.root.children && this.root.children.length) {
				this.setActiveNode({ selected: this.root.children[0] });
				// propagate tooltip
				this.activeNode.proofState = this.root.proofState;
				this.activeNode.tooltip = this.root.tooltip;	
			}
		} else {
			console.warn(`[proof-explorer] Warning: unable to activate selected proof (root node is null)`);
		}
	}

	/**
	 * Internal function, loads a proof tree into proof-explorer
	 * @param proofTree The proof to be loaded, in JSON format
	 */
	protected loadProofTree (proofTree: ProofTree): void {
		// utility function for building the proof tree
		const createTree = (elem: ProofNode, parent: ProofItem): void => {
			const node: ProofItem = (elem.type === "proof-command") ? 
				new ProofCommand(elem.id, elem["branch-id"], parent) 
				: new ProofBranch(elem.id, elem["branch-id"], parent);
			parent.appendChild(node);
			if (elem.children && elem.children.length) {
				elem.children.forEach(child => {
					createTree(child, node);
				});
			} else {
				node.collapsibleState = TreeItemCollapsibleState.None;
			}
		}
		this.disposeView();
		if (proofTree && proofTree.proofStructure) {
			const proofStatus: string = "Unchecked";
			this.root = new RootNode(proofTree.proofStructure.id, proofStatus); // this is the proof name
			this.ghostNode = new GhostNode({ parent: this.root, node: this.root }); // this is the proof status
			if (proofTree.proofStructure.children && proofTree.proofStructure.children.length) {
				proofTree.proofStructure.children.forEach(child => {
					createTree(child, this.root);
				});
			} else {
				this.root.collapsibleState = TreeItemCollapsibleState.None;
			}
			this.proofTree = proofTree;
			this.root.proofState = this.initialProofState;
			this.root.tooltip = utils.formatProofState(this.initialProofState)
			this.setActiveNode({ selected: this.root });
			this.activateSelectedProof();
		}
		// update placeholders
		this.moveIndicatorAtBeginning();
		// register handler for terminal closed events
		window.onDidCloseTerminal((e: Terminal) => {
			if (e && e.name === proofTree.proofName) {
				this.disposeView();
			}
		});
	}

	/**
	 * Utility function for indicating the name of the log files provided by the server for storing the current proof state and temporary proof states. 
	 * The content of the file for the current proof state should be treated as read-only, as it is automatically updated by the server when a new proof state is generated.
	 * @param desc Name of the files for storing the current proof state and temporary proof states.
	 */	
	setLogFileName (desc: { pvsLogFile: string, pvsTmpLogFile: string }): void {
		if (desc) {
			this.logFileName = desc.pvsLogFile;
			this.tmpLogFileName = desc.pvsTmpLogFile;
		} else {
			console.warn("[proof-explorer] Warning: could not set log file name (descriptor is null)");
		}
    }

	/**
	 * Utility function, opens as text document in the editor the proof state associated to the selected node.
	 * This is useful for navigating definitions in the sequent (e.g., using hover, peek, goto functionalities of the editor)
	 * @param desc Selected node
	 */
	async showSequent (desc: { selected: ProofItem }): Promise<void> {
		if (desc && desc.selected) {
			if (desc.selected.id === this.activeNode.id) {
				this.showActiveSequent();
			} else {
				const content: string = desc.selected.tooltip;
				if (this.tmpLogFileName) {
					const uri: vscode.Uri = vscode.Uri.file(this.tmpLogFileName);
					const editors: vscode.TextEditor[] = vscode.window.visibleTextEditors.filter((editor: vscode.TextEditor) => {
						return editor.document.fileName === this.tmpLogFileName;
					});
					const viewColumn: number = (editors && editors.length > 0) ? editors[0].viewColumn : vscode.ViewColumn.Beside;
					await vscode.window.showTextDocument(uri, { preserveFocus: true, preview: true, viewColumn });
					const edit: vscode.WorkspaceEdit = new vscode.WorkspaceEdit();
					const range: vscode.Range = new vscode.Range(
						new vscode.Position(0, 0),
						new vscode.Position(1000, 0)
					);
					edit.replace(uri, range, content);
					const success: boolean = await vscode.workspace.applyEdit(edit);
				}		
				// const sequentFile: vscode.Uri = vscode.Uri.parse(`untitled:${path.join(vscode.workspace.rootPath, fsUtils.logFolder, "sequent.pr")}`);
				// const edit = new vscode.WorkspaceEdit();
				// edit.insert(sequentFile, new vscode.Position(0, 0), content);
				// const success: boolean = await vscode.workspace.applyEdit(edit);
				// if (success) {
				// 	const document: vscode.TextDocument = await vscode.workspace.openTextDocument(sequentFile);
				// 	await vscode.window.showTextDocument(document, vscode.ViewColumn.Beside, true);
				// } else {
				// 	vscode.window.showInformationMessage('[proof-explorer] Warning: unable to show sequent :/');
				// }
			}
		} else {
			console.warn(`[proof-explorer] Warning: failed to show sequent (selected node is null)`);
		}
	}
	/**
	 * Utility function, opens as text document in the editor the current proof state.
	 * This is useful for navigating definitions in the sequent (e.g., using hover, peek, goto functionalities of the editor)
	 */
	async showActiveSequent (): Promise<void> {
		// const content: string = formatProofState(desc.proofState);
		// const label: string = `${desc.proofState.label}.pr`;
		// vscode.workspace.openTextDocument({ language: 'pvs', content: content }).then((document: vscode.TextDocument) => {
		// 	// vscode.window.showTextDocument(document, vscode.ViewColumn.Beside, true);
		// 	vscode.window.showTextDocument(document.uri, { preserveFocus: true, preview: true, viewColumn: vscode.ViewColumn.Beside });
		// });
		// const sequentView: vscode.Uri = vscode.Uri.parse(`untitled:${path.join(vscode.workspace.rootPath, "prooflog", label)}`);
		// const edit = new vscode.WorkspaceEdit();
		// edit.insert(sequentView, new vscode.Position(0, 0), content);
		// const success: boolean = await vscode.workspace.applyEdit(edit);
		// if (success) {
		//     const document: vscode.TextDocument = await vscode.workspace.openTextDocument(sequentView);
		//     vscode.window.showTextDocument(document, vscode.ViewColumn.Beside, true);
		// } else {
		//     vscode.window.showInformationMessage('[vscode-pvs-terminal-cli] Warning: unable to create sequent view :/');
		// }
        if (this.logFileName) {
            const uri: vscode.Uri = vscode.Uri.file(this.logFileName);
            const editors: vscode.TextEditor[] = vscode.window.visibleTextEditors.filter((editor: vscode.TextEditor) => {
                return editor.document.fileName === this.logFileName;
            });
            const viewColumn: number = (editors && editors.length > 0) ? editors[0].viewColumn : vscode.ViewColumn.Beside;
            await vscode.window.showTextDocument(uri, { preserveFocus: true, preview: true, viewColumn });
        } else {
            console.error(`[proof-explorer] Error: log file storing active sequent information is null`);
        }
	}
	
	/**
	 * Activation function, installs all proof-explorer command handlers.
	 * @param context Client context 
	 */
	activate(context: ExtensionContext) {
		// -- handlers for proof explorer commands
		context.subscriptions.push(commands.registerCommand("proof-explorer.forward", () => {
			this.step();
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.back", () => {
			this.step("undo");
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.run-proof", () => {
			this.run();
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.fast-forward", (resource: ProofItem) => {
			this.fastForwardTo({ selected: resource });
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.copy-node", (resource: ProofItem) => {
			this.copyNode({ selected: resource });
			window.showInformationMessage(`Proof command ${resource.name} copied in clipboard`);
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.copy-subtree", (resource: ProofItem) => {
			this.copyTree({ selected: resource });
			window.showInformationMessage(`Proof subtree rooted in ${resource.name} copied in clipboard`);
		}));
		// context.subscriptions.push(commands.registerCommand("proof-explorer.paste-before-proof-command", (resource: ProofItem) => {
		// 	this.pasteBeforeNode({ selected: resource });
		// }));
		context.subscriptions.push(commands.registerCommand("proof-explorer.paste-node", (resource: ProofItem) => {
			this.pasteNode({ selected: resource });
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.delete-node", (resource: ProofItem) => {
			this.deleteNode({ selected: resource });
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.trim-subtree", (resource: ProofItem) => {
			this.trimNode({ selected: resource });
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.delete-proof-tree", (resource: ProofItem) => {
			this.deleteNode({ selected: resource });
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.cut-node", (resource: ProofItem) => {
			this.cutNode({ selected: resource });
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.cut-subtree", (resource: ProofItem) => {
			this.cutTree({ selected: resource });
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.prepend-node", (resource: ProofItem) => {
			this.appendNode({ selected: resource }, { beforeSelected: true });
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.append-node", (resource: ProofItem) => {
			this.appendNode({ selected: resource });
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.create-proof-branch", (resource: ProofItem) => {
			this.appendBranch({ selected: resource });
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.mark-as-pending", (resource: ProofItem) => {
			this.markAsPending({ selected: resource });
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.mark-as-visited", (resource: ProofItem) => {
			this.markAsVisited({ selected: resource });
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.mark-as-not-visited", (resource: ProofItem) => {
			this.markAsNotVisited({ selected: resource });
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.set-as-active", (resource: ProofItem) => {
			this.setAsActive({ selected: resource });
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.rename-node", (resource: ProofItem) => {
			this.renameNode({ selected: resource });
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.show-sequent", (resource: ProofItem) => {
			this.showSequent({ selected: resource }); // async call
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.show-active-sequent", () => {
			this.showActiveSequent(); // async call
		}));
	}

	/**
	 * Returns the children of a node in the proof tree
	 * @param item A node in the proof tree 
	 */
	getChildren(item: TreeItem): Thenable<TreeItem[]> {
		// node
		if (item) {
			let children: TreeItem[] = (<ProofItem> item).getChildren();
			if (this.ghostNode.isActive()) {
				for (let i = 0; i < children.length; i++) {
					if (children[i] === this.ghostNode.realNode) {
						const res: TreeItem[] = children.slice(0, i + 1).concat([this.ghostNode]).concat(children.slice(i + 2));
						return Promise.resolve(res);
					}
				}
			}
			return Promise.resolve(children);
		}
		// root
		if (this.ghostNode.isActive() && this.ghostNode.realNode === this.root) {
			return Promise.resolve([ this.root, this.ghostNode ]);
		}
		return Promise.resolve([ this.root ]);
	}

	/**
	 * Returns the requested node
	 * @param item Node
	 */
	getTreeItem(item: TreeItem): TreeItem {
		return item;
	}

}


//-------------------------------------------------------------
// Auxiliary constants and definitons
//-------------------------------------------------------------

// https://emojipedia.org/symbols/
//  ❌ 🔵 ⚫ ⚪ 🔴 🔽 🔼 ⏯ ⏩ ⏪ ⏫ ⏬ ▶️ ◀️ ⭕ 🔹🔸💠🔷🔶

export const QED: string = "Q.E.D.";

/**
 * Definition of tree items
 */
class ProofItem extends TreeItem {
	contextValue: string = "proofItem";
	name: string; // prover command or branch id
	branchId: string = ""; // branch in the proof tree where this command is located (branchId for root is "").
	command: Command; // vscode action associated to the node
	proofStatus: string; // this field is used only in the root node
	protected icon: string = ""; // icon indicating the state of the node
	protected previousState: {
		tooltip?: string,
		icon?: string
	} = {};
	children: ProofItem[] = [];
	parent: ProofItem;
	protected activeFlag: boolean = false;
	protected visitedFlag: boolean = false;
	protected pendingFlag: boolean = false;
	proofState: ProofState = null;
	constructor (type: ProofNodeType | "ghost", name: string, branchId: string, parent: ProofItem, collapsibleState?: TreeItemCollapsibleState) {
		super(type, (collapsibleState === undefined) ? TreeItemCollapsibleState.Expanded : collapsibleState);
		this.contextValue = type;
		this.id = fsUtils.get_fresh_id();
		this.name = name;
		this.branchId = branchId;
		this.parent = parent;
		this.tooltip = " "; // this is used to show the proof state before the execution of the proof command when hovering the mouse on the proof command
		this.notVisited();
	}
	clone (parent?: ProofItem): ProofItem {
		const c: ProofItem = 
			(this.contextValue === "proof-command") ? new ProofCommand(this.name, this.branchId, this.parent, this.collapsibleState)
			: (this.contextValue === "proof-branch") ? new ProofBranch(this.name, this.branchId, this.parent, this.collapsibleState)
			: new RootNode(this.name, this.proofStatus);
		c.parent = parent || null;
		if (this.contextValue === "root") {
			c.proofStatus = this.proofStatus;
			c.pending();
		}
		return c;
	}
	cloneTree (parent?: ProofItem): ProofItem {
		parent = parent || this.parent || null;
		const clonedRoot: ProofItem = this.clone(parent);
		if (this.children) {
			for (let i: number = 0; i < this.children.length; i++) {
				const child: ProofItem = this.children[i].cloneTree(clonedRoot);
				clonedRoot.insertChild(child, i);
			}
		}
		return clonedRoot;
	}
	setTooltip (tooltip: string): void {
		this.tooltip = tooltip;
	}
	rename (name: string): void {
		this.name = name;
		this.label = `${this.icon}${this.name}`;
	}
	pending (): void {
		this.icon = "🔶";
		this.label = `${this.icon}${this.name}`;
		this.activeFlag = false;
		this.visitedFlag = false;
		this.pendingFlag = true;
	}
	visited (): void {
		this.previousState.tooltip = this.tooltip;
		this.previousState.icon = this.icon = " ★ ";
		this.label = `${this.icon}${this.name}`;
		this.activeFlag = false;
		this.visitedFlag = true;
		this.pendingFlag = false;
	}
	notVisited (): void {
		this.previousState.tooltip = this.tooltip = " ";
		this.previousState.icon = this.icon = " ∘ ";
		this.label = `${this.icon}${this.name}`;
		this.activeFlag = false;
		this.visitedFlag = false;
		this.pendingFlag = false;
	}
	active (): void {
		this.icon = "🔷";
		this.label = `${this.icon}${this.name}`;
		this.activeFlag = true;
	}
	treeNotVisited (): void {
		this.notVisited();
		for (let i = 0; i < this.children.length; i++) {
			this.children[i].treeNotVisited();
		}
	}
	treeVisited (): void {
		this.visited();
		for (let i = 0; i < this.children.length; i++) {
			this.children[i].treeVisited();
		}
	}
	isActive (): boolean {
		return this.activeFlag;
	}
	isVisitedOrPending (): boolean {
		return this.visitedFlag || this.pendingFlag;
	}
	isPending (): boolean {
		return this.pendingFlag;
	}
	isVisited (): boolean {
		return this.visitedFlag;
	}
	restore (): void {
		this.label = `${this.previousState.icon}${this.name}`;
		this.tooltip = this.previousState.tooltip;
	}
	setChildren (children: ProofItem[]): void {
		this.children = children;
	}
	insertChild (child: ProofItem, position: number): void {
		if (this.children && position < this.children.length - 1) {
			this.children = this.children.slice(0, position).concat([ child ]).concat(this.children.slice(position));
		} else {
			// append at the end
			this.children = this.children.concat([ child ]);
		}
		this.collapsibleState = TreeItemCollapsibleState.Expanded;
	}
	deleteChild (child: ProofItem): void {
		this.children = this.children.filter((ch: ProofItem) => {
			return ch.id !== child.id;
		});
		if (this.contextValue !== "root" && this.children.length === 0) {
			this.collapsibleState = TreeItemCollapsibleState.None;
		}
	}
	deleteTree (child: ProofItem): void {
		const children: ProofItem[] = [];
		for (let i in this.children) {
			if (this.children[i].id !== child.id) {
				children.push(this.children[i]);
			} else {
				this.children = children;
				if (this.contextValue !== "root" && this.children.length === 0) {
					this.collapsibleState = TreeItemCollapsibleState.None;
				}
				return; // this stops the iteration and prunes the tree
			}
		}
	}
	getProofCommands (): ProofItem[] {
		let ans: ProofItem[] = [ this ];
		if (this.children) {
			for (let i = 0; i < this.children.length; i++) {
				ans = ans.concat(this.children[i].getProofCommands());
			}
		}
		return ans;
	}
	moveIndicatorForward (lastVisitedChild?: ProofItem): ProofItem | null {
		if (this.contextValue === "proof-command") {
			this.visited();
		} else {
			// proof branch or root
			this.pending();
		}
		// check if this node has children, if so, return the first non-visited child
		if (this.children && this.children.length) {
			if (lastVisitedChild) {
				// the next node to be visited is the one after the last visited
				const idx: number = this.children.indexOf(lastVisitedChild);
				const next: number = idx + 1;
				if (next < this.children.length) {
					return this.children[next];
				}
			} else {
				return this.children[0];
			}
		}
		// else, branch completed, go to next sibling
		this.visited();
		if (this.contextValue !== "root" && this.parent) {
			return this.parent.moveIndicatorForward(this);
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
	moveIndicatorBack (): ProofItem {
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
				if (this.parent.contextValue === "root" && this.parent.children && this.parent.children.length) {
					return this.parent.children[0];
				}
				// else return parent
				return this.parent;
			}
		} else {
			console.error(`Error: Could not find parent for node ${this.name}`);
		}
		return this;
	}
	getSiblingOrParent (): ProofItem {
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
		} else {
			console.error(`[proof-explorer] Error: could not find sibling or parent for node ${this.name}`);
		}
		return null;
	}
	getNextSibling (): ProofItem | null {
		if (this.parent) {
			const children: ProofItem[] = this.parent.children;
			if (children && children.length > 1) {
				const index: number = children.indexOf(this);
				const next: number = index + 1;
				if (next < children.length) {
					return children[next];
				}
			}
		}
		return null;
	}
	appendSibling (sib: ProofItem, opt?: { beforeSelected?: boolean }): void {
		let children: ProofItem[] = [];
		const n: number = this.parent.children.length;
		opt = opt || {};
		for (let i = 0; i < n; i++) {
			if (!opt.beforeSelected) {
				children.push(this.parent.children[i]);
			}
			if (this.parent.children[i].id === this.id) {
				if (sib.contextValue === "root") {
					children = children.concat(sib.children);
				} else {
					children.push(sib);
				}
			}
			if (opt.beforeSelected) {
				children.push(this.parent.children[i]);
			}
		}
		this.parent.children = children;
	}
	appendChildAtBeginning (child: ProofItem): void {
		this.children = this.children || [];
		child.parent = this;
		if (child.contextValue === "root") {
			this.children = child.children.concat(this.children);
		} else {
			this.children = [ child ].concat(this.children);
		}
		this.collapsibleState = TreeItemCollapsibleState.Expanded;
	}
	appendChild (child: ProofItem): void {
		this.children = this.children || [];
		child.parent = this;
		if (child.contextValue === "root") {
			this.children = this.children.concat(child.children);
		} else {
			this.children.push(child);
		}
		// keep children ordered by name
		this.children = this.children.sort((a: ProofItem, b: ProofItem): number => {
			// we need to compare number rather than strings, otherwise the order is incorrect (e.g., "10" is < "2")
			const aa: number = +(a.branchId.split(".").slice(-1));  
			const bb: number = +(b.branchId.split(".").slice(-1));  
			return (aa < bb) ? -1 : 1;
		});
		this.collapsibleState = TreeItemCollapsibleState.Expanded;
	}
	getChildren (): ProofItem[] {
		return this.children;
	}
}
class ProofCommand extends ProofItem {
	constructor (cmd: string, branchId: string, parent: ProofItem, collapsibleState?: TreeItemCollapsibleState) {
		super("proof-command", cmd, branchId, parent, collapsibleState);
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
	constructor (cmd: string, branchId: string, parent: ProofItem, collapsibleState?: TreeItemCollapsibleState) {
		super("proof-branch", cmd, branchId, parent, collapsibleState);
		this.name = `(${branchId})`;
		this.notVisited();
		this.command = {
			title: this.contextValue,
			command: "explorer.didClickOnStrategy",
			arguments: [ this.contextValue ]
		};
	}
}
class RootNode extends ProofItem {
	constructor (proofName: string, proofStatus?: string) {
		super("root", proofName, "", null, TreeItemCollapsibleState.Expanded);
		this.parent = this; // the parent of the root is the root itself
		this.name = proofName;
		this.setProofStatus(proofStatus);
		this.notVisited();
		this.command = {
			title: this.contextValue,
			command: "explorer.didClickOnStrategy",
			arguments: [ this.contextValue ]
		};
	}
	// @overrides
	notVisited (): void {
		if (this.proofStatus !== QED) {
			this.label = this.name;
		}
	}
	// @overrides
	visited (): void {
		if (this.proofStatus !== QED) {
			super.visited();
		}
	}
	QED (): void {
		super.visited();
		this.setProofStatus(QED);
		this.label = " ★ " + this.label;
	}
	isQED (): boolean {
		return this.proofStatus === QED;
	}
	setProofStatus (proofStatus: string): void {
		if (proofStatus) {
			this.proofStatus = proofStatus;
			this.label = `${this.name} (${this.proofStatus})`;
		}
	}
	resetProofStatus (): void {
		this.proofStatus = "";
		this.label = `${this.name}`;
	}
}
class GhostNode extends ProofItem {
	realNode: ProofItem;
	constructor (desc: { parent: ProofItem, node: ProofItem }) {
		super("ghost", "", "", desc.parent, TreeItemCollapsibleState.None);
		this.notVisited();
		this.realNode = desc.node;
	}
	qed (): void {
		this.label = QED;
	}
	// @overrides
	active (): void {
		this.activeFlag = true;
		this.label = " 🔷 ...";
	}
	// @overrides
	notVisited () {
		this.activeFlag = false;
		this.label = "";
	}
	// @overrides
	moveIndicatorBack (): ProofItem {
		this.notVisited();
		this.realNode.active();
		return this.realNode;
	}
	// @overrides
	moveIndicatorForward (): ProofItem {
		return null;
	}
	// @overrides
	appendSibling (sib: ProofItem, opt?: { beforeSelected?: boolean }): void {
		if (this.realNode.contextValue === "root") {
			this.realNode.appendChild(sib);
		} else {
			this.realNode.appendSibling(sib, opt);
		}
	}
	// @overrides
	appendChildAtBeginning (child: ProofItem): void {
		this.realNode.appendChildAtBeginning(child);
	}
	// @overrides
	appendChild (child: ProofItem): void {
		this.realNode.appendChild(child);
	}
	// @overrides
	setTooltip (tooltip: string): void {
		super.setTooltip(tooltip);
		this.realNode.tooltip = tooltip;
	}
}

