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
import { ProofNode, ProofNodeType, serverCommand, PvsVersionDescriptor, ProofDescriptor, ProofStatus, serverEvent, ProofTree } from '../common/serverInterface';
import * as utils from '../common/languageUtils';
import * as fsUtils from '../common/fsUtils';
import { PvsResponse } from '../common/pvs-gui';
import { ProofState } from '../common/languageUtils';
import * as vscode from 'vscode';


/**
 * TreeData provider for Proof Explorer
 */
export class VSCodePvsProofExplorer implements TreeDataProvider<TreeItem> {
	protected pvsVersionDescriptor: PvsVersionDescriptor;
	protected shasum: string;
	protected logFileName: string;
	protected tmpLogFileName: string;

	protected dirtyFlag: boolean = false; // indicates whether the proof has changed since the last time it was saved
	protected pendingExecution: boolean = false; // indicates whether step() has been triggered and we need to wait for onStepExecuted before doing anything else

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
	protected formulaDescriptor: { contextFolder: string, fileName: string, fileExtension: string, theoryName: string, formulaName: string, autorun?: boolean };
	protected autorunCallback: (status: ProofStatus) => void;

	/**
	 * Name of the view associated with the data provider
	 */
	protected providerView: string;
	protected view: TreeView<TreeItem>;

	/**
	 * Attributes for run-time management of the proof tree rendered in the view
	 */
	protected welcome: WelcomeScreen = new WelcomeScreen();
	protected root: RootNode = null // the root of the tree
	protected ghostNode: GhostNode = null; // this is a floating node that follows activeNode. It is used during proof development, to signpost where the next proof command will be appended in the proof tree
	protected activeNode: ProofCommand | ProofBranch | GhostNode = null;

	protected running: boolean = false; // status flag, indicates whether we are running all proof commands, as opposed to stepping through the proof commands
	protected stopAt: ProofItem = null; // indicates which node we are fast-forwarding to

	protected undoundo: string = null;

	/**
	 * JSON representation of the proof script for the current proof.
	 * The representation is updated at the beginning of the proof session.
	 */
	protected proofDescriptor: ProofDescriptor;

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
	}

	/**
	 * Automatic re-run of a proof
	 * @param desc Descriptor of the formula whose proof is to be re-run
	 */
	async autorun (desc: { 
		fileName: string, fileExtension: string, contextFolder: string, 
		theoryName: string, formulaName: string 
	}): Promise<ProofStatus> {
		return new Promise((resolve, reject) => {
			this.autorunCallback = (status: ProofStatus) => {
				resolve(status);
			};
			commands.executeCommand("vscode-pvs.autorun-formula", desc);
		});
	}

	async stopAutorun (): Promise<void> {
		this.running = false;
		this.formulaDescriptor.autorun = false;
		await this.quitProof({ confirm: false, save: false });
	}

	/**
	 * Returns the active node in the proof tree
	 * @returns {ProofItem} The active node 
	 */
	// getActiveNode (): ProofItem {
	// 	return this.activeNode;
	// }

	/**
	 * Internal function, moves the active node one position back in the proof tree.
	 */
	protected moveIndicatorBack (opt?: { keepSameBranch?: boolean }): void {
		if (this.activeNode) {
			opt = opt || {};
			const prev: ProofItem = this.activeNode.moveIndicatorBack(opt);
			if (prev.contextValue !== "root") {
				// this.revealNode({ selected: prev }); // commented for now, sometimes triggers exceptions from TreeView
				this.markAsActive({ selected: prev }, { force: true });
			}
		} else {
			console.warn("[vscode-pvs-proof-explorer] Warning: active node is null");
		}
	}
	/**
	 * Internal function, moves the active node one position forward in the proof tree.
	 */
	protected moveIndicatorForward (opt?: { keepSameBranch?: boolean, proofState?: ProofState }): void {
		if (this.activeNode) {
			opt = opt || {};
			if (this.activeNode.contextValue !== "ghost") {
				const next: ProofItem = this.activeNode.moveIndicatorForward(opt) || this.ghostNode;
				if (next === this.ghostNode) {
					this.ghostNode.parent = this.activeNode.parent;
					this.ghostNode.realNode = this.activeNode;
					this.ghostNode.proofState = opt.proofState;
					this.ghostNode.updateTooltip();
					// this.ghostNode.setTooltip(this.activeNode.tooltip);
					this.ghostNode.active();
					this.revealNode({ selected: this.ghostNode });
					if (this.ghostNode.parent.contextValue === "proof-command") {
						this.ghostNode.parent.visited();						
					} else {
						this.ghostNode.parent.pending();
					}
					this.root.pending();
				} else {
					this.markAsActive({ selected: next }, { restore: false });
					this.revealNode({ selected: next });
					if (opt.proofState) {
						this.activeNode.proofState = opt.proofState;
						this.activeNode.updateTooltip();
						// this.activeNode.setTooltip(utils.formatProofState(this.activeNode.proofState));		
					}
				}
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
		if (!this.running) {
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
			this.stopAt = desc.selected;
			this.running = true;
			this.step();
		} else {
			console.warn(`[proof-explorer] Warning: failed to fast forward (selected node is null)`);
		}
	}
	/**
	 * Utility function, checks if branch b1 is a sub-branch of b2
	 * @param b1 branch to be checked
	 * @param b2 reference branch
	 */
	protected branchHasChanged (b1: string, b2: string): boolean {
		return b1 !== b2 || (b2 !== "" && b1.indexOf(b2) !== 0);
	}
	
	/**
	 * repeats the last command
	 */
	redo (): void {
		this.forward();
	}

	/**
	 * executes the next step
	 */
	forward (): void {
		if (this.ghostNode && !this.ghostNode.isActive()) {
			this.root.pending();
			this.step();
		}
	}
	/**
	 *  goes one step back (undo)
	 */
	back (): void {
		this.undo();
	}
	/**
	 * goes one step back
	 */
	undo (): void {
		this.root.pending();
		this.step("undo");
	}
	/**
	 * Executes a proof command. 
	 * The command is taken from the proof tree.
	 * The command can also be specified as function argument -- this is useful for handling commands entered by the used at the prover prompt. 
	 * @param cmd Optional parameter, specifying the command to be executed.
	 */
	step (cmd?: string): void {
		if (this.stopAt === this.activeNode || this.pendingExecution) {
			this.stopAt = null;
			this.running = false;
			return;
		}
		// else
		switch (this.activeNode.contextValue) {
			case "proof-command": {
				cmd = cmd || this.activeNode.name;
				const parent: ProofItem = this.activeNode.parent;
				if (parent) {
					if (utils.isUndoCommand(cmd) && parent.contextValue === "proof-branch" && this.activeNode.id === parent.children[0].id && !this.ghostNode.isActive()) {
						// the active node is the first child in a branch. 
						// The command has not been executed yet, just move the indicator back without sending any command to pvs-server
						this.moveIndicatorBack();
						return;
					}
					// else
					this.pendingExecution = true;
					commands.executeCommand("vscode-pvs.send-proof-command", {
						fileName: this.formulaDescriptor.fileName,
						fileExtension: this.formulaDescriptor.fileExtension,
						theoryName: this.formulaDescriptor.theoryName,
						formulaName: this.formulaDescriptor.formulaName,
						contextFolder: this.formulaDescriptor.contextFolder,
						cmd: cmd.startsWith("(") ? cmd : `(${cmd})`
					});
				} else {
					console.error(`[proof-explorer] Error: proof command ${this.activeNode.name} does not have a parent`);
				}
				break;		
			}
			case "ghost": {
				this.running = false;
				if (this.formulaDescriptor.autorun) {
					// mark proof as unfinished
					if (this.root.proofStatus !== "untried") {
						this.root.setProofStatus("unfinished");
					}
					// automatically quit the proof attempt
					this.quitProof({ confirm: false, save: true }); // async call
				}
				break;
			}
			case "root":
			case "proof-branch": 
			default: {
				if (utils.isUndoCommand(cmd)) {
					if (this.activeNode.contextValue === "proof-branch") {
						this.pendingExecution = true;
						commands.executeCommand("vscode-pvs.send-proof-command", {
							fileName: this.formulaDescriptor.fileName,
							fileExtension: this.formulaDescriptor.fileExtension,
							theoryName: this.formulaDescriptor.theoryName,
							formulaName: this.formulaDescriptor.formulaName,
							contextFolder: this.formulaDescriptor.contextFolder,
							cmd: cmd.startsWith("(") ? cmd : `(${cmd})`
						});
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
	/**
	 * Call-back function invoked after step(), when the execution of a proof command is complete.
	 * @param desc Descriptor specifying the reponse of the prover, as well as the actual values of the arguments used to invoke the step function.
	 */
	onStepExecuted (desc: { response: PvsResponse, args: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string, cmd: string }}): void {
		this.pendingExecution = false;
		if (desc && desc.response && desc.response.result && desc.args) {
			// get command and proof state
			let cmd: string = desc.args.cmd; // command entered by the user
			const proofState: ProofState = <ProofState> desc.response.result;
			// identify active node in the proof tree
			let activeNode: ProofItem = this.ghostNode.isActive() ? this.ghostNode.realNode : this.activeNode;

			// if QED, update proof status and stop execution
			if (utils.isQED(desc.response.result)) {
				this.running = false;

				// if cmd !== activeNode.name then the user has entered a command manually: we need to append a new node to the proof tree
				if (utils.isSameCommand(activeNode.name, cmd) === false || this.ghostNode.isActive()) {
					// concatenate new command
					const elem: ProofCommand = new ProofCommand(cmd, activeNode.branchId, activeNode.parent, TreeItemCollapsibleState.None);
					// append before selected node (the active not has not been executed yet)
					if (activeNode.isActive()) {
						elem.proofState = activeNode.proofState;
						elem.updateTooltip();
						// elem.setTooltip(activeNode.tooltip);
						activeNode.notVisited(); // this resets the tooltip in activeNode
						this.appendNode({ selected: activeNode, elem }, { beforeSelected: true });
					} else {
						elem.proofState = (this.ghostNode.isActive()) ? this.ghostNode.proofState : activeNode.proofState;
						elem.updateTooltip();
						// elem.setTooltip(utils.formatProofState(proofState));
						this.appendNode({ selected: activeNode, elem });
					}
					this.markAsActive({ selected: elem }); // this is necessary to correctly update the data structures in endNode --- elem will become the parent of endNode
					activeNode = this.activeNode; // update local variable because the following instructions are using it
				}

				this.QED();
				return;
			}
			
			// if command is quit, do nothing
			if (utils.isQuitCommand(cmd)) {
				this.running = false;
				return;	
			}
			// if command is invalid command, stop execution and provide feedback to the user 
			if (utils.isEmptyCommand(cmd) || utils.isInvalidCommand(proofState)) {
				this.running = false;
				if (this.formulaDescriptor.autorun) {
					// mark proof as unfinished
					if (this.root.proofStatus !== "untried") {
						this.root.setProofStatus("unfinished");
					}
					// save and quit proof
					this.quitProof({ confirm: false, save: true });
				} else {
					if (utils.isInvalidCommand(proofState)) {
						window.showWarningMessage(proofState.commentary[0]);
					}
				}
				return;
			}
			// if command was show-hidden, then pvsCli will reveal hidden sequents when the proof state comes back; 
			// the proof script remains unchanged.
			if (utils.isShowHiddenCommand(cmd)) {
				this.running = false;
				if (this.formulaDescriptor.autorun) {
					// mark proof as unfinished
					if (this.root.proofStatus !== "untried") {
						this.root.setProofStatus("unfinished");
					}
					// save and quit proof					
					this.quitProof({ confirm: false, save: true });
				}
				return;
			}
			// if command produced no change, provide feedback to the user, stop any running proof
			// move the indicator forward (keep same proof branch) if the command was in the proof tree 
			if (utils.noChange(proofState) && !this.formulaDescriptor.autorun) {
				this.running = false;
				window.showWarningMessage(proofState.commentary.join("\n"));	
				if (utils.isSameCommand(activeNode.name, cmd) && !this.ghostNode.isActive()) {
					this.moveIndicatorForward({ keepSameBranch: true, proofState });
					// mark the tree rooted at the previous active node as not visited
					activeNode.treeNotVisited();
					// activeNode.noChange();
				}
				return;
			}

			const previousBranch: string = activeNode.branchId;
			const newBranch: string = utils.getBranchId(proofState.label);

			// if command is undo, go back to the last visited node
			if (utils.isUndoCommand(cmd)) {
				this.running = false;
				if (this.branchHasChanged(newBranch, previousBranch)) {
					const targetBranch: ProofBranch = this.findProofBranch(newBranch);
					if (targetBranch) {
						this.activeNode.notVisited();
						// find the last visited child in the new branch
						const visitedChildren: ProofCommand[] = targetBranch.children.filter((elem: ProofItem) => {
							return elem.contextValue === "proof-command" && elem.isVisited();
						});
						const targetNode: ProofItem = visitedChildren.length ? visitedChildren[visitedChildren.length - 1] : targetBranch;
						// mark the entire subtree as not visited
						targetNode.treeNotVisited();
						// mark the target node as active
						this.revealNode({ selected: targetNode });
						this.markAsActive({ selected: targetNode });
						// send feedback to the user
						const msg: string = (newBranch === "") ? `back to root branch` : `back to branch (${newBranch})`;
						window.showInformationMessage(msg);
					} else {
						window.showErrorMessage(`Error: could not find branch ${newBranch} in the proof tree`);
					}
				} else {
					this.moveIndicatorBack({ keepSameBranch: true });
				}
				// update undoundo buffer after moving the indicator back
				this.undoundo = this.activeNode.name;
				return;
			}

			// if command is postpone, move to the new branch
			if (utils.isPostponeCommand(cmd)) {
				this.running = false;
				if (this.formulaDescriptor.autorun) {
					// mark proof as unfinished
					if (this.root.proofStatus !== "untried") {
						this.root.setProofStatus("unfinished");
					}
					// save and quit proof					
					this.quitProof({ confirm: false, save: true });
					return;
				}
				if (this.branchHasChanged(newBranch, previousBranch)) {
					this.ghostNode.notActive();
					const targetBranch: ProofBranch = this.findProofBranch(newBranch);
					if (targetBranch) {
						this.activeNode.notVisited();
						// find the last visited child in the new branch
						const visitedChildren: ProofCommand[] = targetBranch.children.filter((elem: ProofItem) => {
							return elem.contextValue === "proof-command" && elem.isVisited();
						});
						const targetNode: ProofItem = visitedChildren.length ? visitedChildren[visitedChildren.length - 1] : targetBranch;
						targetNode.pending();
						// mark the target node as active
						this.markAsActive({ selected: targetNode });
						// send feedback to the user
						const msg: string = (newBranch === "") ? `moving to root branch` : `moving to branch (${newBranch})`;
						// window.showInformationMessage(msg);
						this.moveIndicatorForward({ keepSameBranch: true, proofState });
					} else {
						window.showErrorMessage(`Error: could not find branch ${newBranch} in the proof tree. Please report the bug to the developers. Do not continue the proof as Proof Explorer is out of sync with PVS.`);
					}
				} else {
					// do nothing, this is the only branch left to be proved
				}
				return;
			}

			let wasUndoUndo: boolean = false;
			// handle the special command (undo undo) before proceeding
			if (utils.isUndoUndoCommand(cmd)) {
				if (!this.undoundo) {
					window.showWarningMessage(`Warning: unable to execute ${cmd}`);
					return;
				}
				cmd = this.undoundo;
				wasUndoUndo = true;
			}
			if (utils.isUndoUndoPlusCommand(cmd)) {
				window.showWarningMessage(`Warning: ${cmd} is not a valid proof command`);
				return;
			}

			// else, the prover has made progress with the provided proof command
			// if cmd !== activeNode.name then the user has entered a command manually: we need to append a new node to the proof tree
			if (utils.isSameCommand(activeNode.name, cmd) === false || this.ghostNode.isActive()) {
				this.running = false;
				if (this.formulaDescriptor.autorun) {
					// mark proof as unfinished
					if (this.root.proofStatus !== "untried") {
						this.root.setProofStatus("unfinished");
					}
					// save and quit proof
					this.quitProof({ confirm: false, save: true });
					return;
				}
				// concatenate new command
				const elem: ProofCommand = new ProofCommand(cmd, activeNode.branchId, activeNode.parent, TreeItemCollapsibleState.None);
				// append before selected node (the active not has not been executed yet)
				if (activeNode.isActive()) {
					elem.proofState = activeNode.proofState;
					elem.updateTooltip();
					// elem.setTooltip(activeNode.tooltip);
					activeNode.notVisited(); // this resets the tooltip in activeNode
					this.appendNode({ selected: activeNode, elem }, { beforeSelected: true });
				} else {
					elem.proofState = (this.ghostNode.isActive()) ? this.ghostNode.proofState : activeNode.proofState;
					elem.updateTooltip();
					// elem.setTooltip(utils.formatProofState(proofState));
					this.appendNode({ selected: activeNode, elem });
				}
				this.markAsActive({ selected: elem }); // this is necessary to correctly update the data structures in endNode --- elem will become the parent of endNode
				activeNode = this.activeNode; // update local variable because the following instructions are using it
				// provide feedback to the user
				window.showInformationMessage(`${elem.name} added to the proof tree.`);
			}

			// check if the number of sub-goals generated by the proof command is consistent with the current proof structure
			let nSubGoals: number = (proofState["num-subgoals"] > 1) ? (proofState["num-subgoals"] - activeNode.children.length) : 0;
			let showMsg: boolean = true;
			if (proofState.commentary && proofState.commentary.length && proofState.commentary[0].startsWith("This completes the proof")) {
				// PVS has automatically discharged a proof branch -- e.g. check sorting.pvs, not_in_l_gives_lenght_l : LEMMA 
				nSubGoals += 2;
				if (!this.formulaDescriptor.autorun) {
					window.showInformationMessage(`Proof command ${cmd} generated ${nSubGoals} sub-goals. PVS automatically discharged the first subgoal.`);
				}
				showMsg = false;
			}
			if (nSubGoals) {
				const targetBranch: ProofItem = this.findProofBranch(newBranch);
				if (this.running && !this.formulaDescriptor.autorun) {
					// double check that the new branch is a sub-branch of the old one -- pvs may have proved all sub-branches silently
					if (this.branchHasChanged(newBranch, previousBranch) === false || !targetBranch) {
						// stop execution, there's a mismatch between the proof tree and the sub goals generated by the proof command
						this.running = false;
						// add branches if the new branch is missing.
						if (!targetBranch) {
							for (let i = 0; i < nSubGoals; i++) {
								this.appendBranch({ selected: activeNode }, { firstBranch: newBranch, proofState });
							}
						}
						if (utils.isProved(this.root.proofStatus) || utils.isUnchecked(this.root.proofStatus)) {
							window.showWarningMessage(`Warning: Proof script might be broken (PVS generated ${nSubGoals} sub goals, but proof script contains ${this.activeNode.children.length} sub goals). Stopping execution of proof script.`);
						}
					} else {
						if (showMsg) {
							window.showInformationMessage(`Proof command ${cmd} generated ${proofState["num-subgoals"]} sub-goals, but PVS has automatically discharged them`);
						}
					}
				} else {
					// append missing branches and provide feedback
					// double check that the new branch is a sub-branch of the old one -- pvs may have proved all sub-branches silently
					if (this.branchHasChanged(newBranch, previousBranch) === false || !targetBranch) {
						// add branches if any is missing. We also need to be careful with the branch names, pvs-server sometimes reports less branches because some were automatically discharged
						for (let i = 0; i < nSubGoals; i++) {
							this.appendBranch({ selected: activeNode }, { firstBranch: newBranch, proofState });
						}
						if (showMsg && !this.formulaDescriptor.autorun) {
							window.showInformationMessage(`Proof command ${cmd} on branch ${previousBranch} has generated ${proofState["num-subgoals"]} sub-goals`);
						}
					} else {
						if (!this.formulaDescriptor.autorun) {
							window.showInformationMessage(`Proof command ${cmd} on branch ${previousBranch} has generated ${proofState["num-subgoals"]} sub-goals, but PVS has automatically discharged them`);
						}
					}
				}
			}

			// if the branch has changed, move to the new branch
			// this case handles the completion of a proof branch (postpone is handled separately, see conditions above)
			if (this.branchHasChanged(newBranch, previousBranch)) {
				const targetBranch: ProofItem = this.findProofBranch(newBranch);
				if (targetBranch) {
					// go to the new branch
					activeNode.visited();
					this.activeNode.parent.visited();
					this.markAsActive({ selected: targetBranch }, { restore: false });
					targetBranch.proofState = proofState;
					targetBranch.updateTooltip();
					// targetBranch.setTooltip(utils.formatProofState(proofState))
					this.activeNode.pending();
					if (!this.running) {
						window.showInformationMessage(`moving to branch (${newBranch})`);
					}
				} else {
					window.showErrorMessage(`Error: could not find branch ${targetBranch} in the proof tree`);
				}
			}

			// update undoundo buffer
			if (!utils.isUndoUndoCommand(cmd)) {
				this.undoundo = (wasUndoUndo) ? null : this.activeNode.name; // undo undo can be performed only once
			}

			// finally, move indicator forward and propagate tooltip to the new active node
			this.moveIndicatorForward({ keepSameBranch: true, proofState });
			// this.refreshView();

			// if a proof is running, then iterate
			if (this.running) {
				// unless we have reached the end of the proof
				if (this.ghostNode.isActive()) {
					// and so we need to stop the execution
					this.running = false;
					if (this.formulaDescriptor.autorun) {
						// mark proof as unfinished
						if (this.root.proofStatus !== "untried") {
							this.root.setProofStatus("unfinished");
						}
						// save and quit proof
						this.quitProof({ confirm: false, save: true });
					}
				} else {
					this.step();
				}
			}
		} else {
			this.running = false;
			window.showErrorMessage("Error: could not read proof state information returned by pvs-server.");
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
	markAsActive (desc: { selected: ProofItem }, opt?: { force?: boolean, restore?: boolean }): void {
		if (desc && desc.selected) {
			opt = opt || {};
			const restore: boolean = (opt.restore === undefined || opt.restore === null) ? true : opt.restore;
			// this.running = false;
			if (opt.force || desc.selected !== this.activeNode) {
				// restore status of node currently active
				if (restore) {
					this.activeNode.restore();
				}
				// update activeNode
				this.activeNode = desc.selected;
				this.activeNode.active();
				if (this.activeNode.contextValue !== "ghost") {
					// update ghostNode
					this.ghostNode.parent = this.activeNode.parent;
					this.ghostNode.realNode = this.activeNode;
					this.ghostNode.notActive();
				}
				// Reveal the active node in the tree view
				// this.revealNode({ selected: this.activeNode }); // commented for now, sometimes triggers exceptions from TreeView
				this.refreshView();
			}
		} else {
			console.warn(`[proof-explorer] Warning: failed to set active node`);
		}
	}
	/**
	 * Internal function, reveals a node in the view.
	 */
	protected revealNode (desc: { selected: ProofItem }): void {
		if (desc && desc.selected) {
			// there is something I don't understand in the APIs of TreeItem 
			// because I'm getting exceptions (node not found / element already registered)
			// when option 'select' is set to true.
			// Sometimes the exception occurs also with option 'expand'
			// if (desc.selected.isActive() === false) {
				this.view.reveal(desc.selected, { expand: 2, select: false, focus: false }).then(() => {
				}, (error: any) => {
					// console.error(desc);
					// console.error(error);
				});
			// }
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
			this.dirtyFlag = true;
			opt = opt || {};
			const selectedNode: ProofItem = (desc.selected.contextValue === "ghost") ? (<GhostNode> desc.selected).realNode : desc.selected;
			const branchId: string = selectedNode.branchId;
			let newNode: ProofItem = desc.elem;
			if (!newNode) {
				const cmd: string = await vscode.window.showInputBox({
					prompt: `Please enter proof command to be appended after ${desc.selected.name}`,
					placeHolder: ``,
					value: ``,
					ignoreFocusOut: true 
				});
				if (cmd) {
					newNode = new ProofCommand(cmd, branchId, selectedNode.parent, TreeItemCollapsibleState.None);
				}
			}
			if (newNode) {
				switch (selectedNode.contextValue) {
					case "root":
					case "proof-branch": {
						newNode.parent = selectedNode;
						selectedNode.appendChildAtBeginning(newNode);
						break;
					}
					case "ghost":
					case "proof-command": {
						newNode.parent = selectedNode.parent;
						selectedNode.appendSibling(newNode, opt);
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
	appendBranch (desc: { selected: ProofItem, elem?: ProofItem }, opt?: { beforeSelected?: boolean, firstBranch?: string, proofState?: ProofState }): void {
		if (desc && desc.selected) {
			this.dirtyFlag = true;
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
						let firstGoal: number = 1;
						if (opt.firstBranch && opt.firstBranch.indexOf(".") > 0) {
							firstGoal = +opt.firstBranch.split(".").slice(-1);
						}
						let branchId: string = utils.makeBranchId({ branchId: selectedNode.branchId, goalId: firstGoal });
						// check which is the first available branch name -- there might be holes if branches have been deleted/renamed by the user
						let found: boolean = false;
						for (let i = 0; i < selectedNode.children.length && !found; i++) {
							if (selectedNode.children[i].branchId !== branchId) {
								found = true
							} else {
								branchId = utils.makeBranchId({ branchId: selectedNode.branchId, goalId: firstGoal + i + 1 });
							}
						}
						newBranch = new ProofBranch(branchId, branchId, selectedNode, TreeItemCollapsibleState.Expanded);
						if (opt.proofState) {
							newBranch.proofState = opt.proofState;
							newBranch.updateTooltip();
							// newBranch.setTooltip(utils.formatProofState(opt.proofState));
						}
					}
					if (newBranch) {
						selectedNode.appendChild(newBranch);
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
			// copy to the system clipboard as well
			vscode.env.clipboard.writeText(this.clipboard.name);
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
			// copy to the system clipboard as well
			vscode.env.clipboard.writeText(this.clipboardTree.name);
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
				this.dirtyFlag = true;
				if (selected.contextValue === "root") {
					selected.children = [];
					this.markAsActive({ selected: this.root });
				} else {
					const wasActive: boolean = selected.isActive();
					if (wasActive || this.ghostNode.realNode === selected) {
						const sibling: ProofItem = selected.getNextSibling() || this.ghostNode;
						if (sibling === this.ghostNode) {
							this.ghostNode.parent = selected.parent;
							this.ghostNode.realNode = selected.getSiblingOrParent() || this.root;
						}
						// this.setActiveNode({ selected: sibling });
						this.markAsActive({ selected: sibling });
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
				this.dirtyFlag = true;
				switch (node.contextValue) {
					case "root": {
						node.children = [];
						// this.setActiveNode({ selected: this.root });
						this.markAsActive({ selected: this.root });
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
							// this.setActiveNode({ selected: node });
							this.markAsActive({ selected: node });
						}
						node.children = [];
						break;
					}
					case "proof-command": {
						// remove children if any
						node.children = [];
						node.collapsibleState = TreeItemCollapsibleState.None;
						// remove also lower siblings
						const parent: ProofItem = node.parent;
						const idx: number = parent.children.indexOf(node);
						parent.children = parent.children.slice(0, idx + 1);
						if (node.isVisited()) {
							// this.setActiveNode({ selected: node });
							this.markAsActive({ selected: node });
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
			this.dirtyFlag = true;
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
	hide(): void { commands.executeCommand('setContext', 'in-checker', false); }

	/**
	 * Utility function, reveals proof explorer view in the IDE
	 */
	reveal(): void { commands.executeCommand('setContext', 'in-checker', true); }

	/**
	 * Utility function, marks the current proof as complete
	 */
	QED (): void {
		// stop execution
		this.running = false;
		// move indicator forward so any proof branch that needs to be marked as visited will be marked
		this.activeNode.moveIndicatorForward();
		// set QED
		this.root.QED();
		// save proof if status has changed
		if (this.root.proofStatusChanged() && !this.dirtyFlag) {
			this.saveProof({ force: true });
		}
		// update proof descriptor
		this.proofDescriptor = this.getProofDescriptor();
		// refresh tree view
		this.refreshView();

		// send QED to the terminal
		commands.executeCommand("vscode-pvs.send-proof-command", {
			fileName: this.formulaDescriptor.fileName,
			fileExtension: this.formulaDescriptor.fileExtension,
			theoryName: this.formulaDescriptor.theoryName,
			formulaName: this.formulaDescriptor.formulaName,
			contextFolder: this.formulaDescriptor.contextFolder,
			cmd: "Q.E.D."
		});

		// run the other proofs if desc.autorun === true
		if (this.formulaDescriptor.autorun) {
			// commands.executeCommand("vscode-pvs.autorun-formula-end", this.desc);
			// we need a timeout otherwise proof explorer is unable to refresh the view because update events get delayed in the event queue 
			setTimeout(() => {
				this.autorunCallback(this.root.proofStatus);
			}, 400);
		}
	}


	/**
	 * Utility function, used to identify which formula is being proved in the proof tree session
	 * @param desc 
	 */
	loadFormulaDescriptor (desc: { fileName: string, fileExtension: string, theoryName: string, formulaName: string, contextFolder: string, autorun?: boolean }): void {
		this.formulaDescriptor = desc;
	}

	/**
	 * Utility function, used to set the initial proof state.
	 * @param proofState 
	 */
	loadInitialProofState (proofState: ProofState): void {
		this.initialProofState = proofState;
	}

	/**
	 * Utility function, activates the proof loaded in the view (i.e., sets the first node as active)
	 */
	startProof (): void {
		if (this.root) {
			this.root.pending();
			this.root.proofState = this.initialProofState;
			this.root.tooltip = utils.formatProofState(this.initialProofState);

			this.root.proofState = this.initialProofState;
			this.root.tooltip = utils.formatProofState(this.initialProofState);
			this.root.pending();
			this.root.setProofStatus(this.proofDescriptor.info.status);
			// select either the first child or the root if children are not present
			const selected: ProofItem = (this.root.children && this.root.children.length) ? this.root.children[0] : this.root;
			// initialise this.activeNode
			this.initActiveNode({ selected }); 
			// update the user interface
			this.markAsActive({ selected: this.activeNode }, { force: true });
			this.activeNode.proofState = this.initialProofState;
			this.activeNode.updateTooltip();
			this.dirtyFlag = false;

			// start the proof
			if (this.root.children && this.root.children.length) {
				// this.setActiveNode({ selected: this.root.children[0] });
				this.markAsActive({ selected: this.root.children[0] });
				// propagate tooltip
				this.activeNode.proofState = this.root.proofState;
				this.activeNode.tooltip = this.root.tooltip;	
			}

			if (this.formulaDescriptor.autorun) {
				this.run();
			}
		} else {
			console.warn(`[proof-explorer] Warning: unable to activate selected proof (root node is null)`);
		}
	}

	/**
	 * Internal function, initialises data structures necessary for handling of activeNode, including ghostNode and tooltips.
	 * @param desc Descriptor used for the initialisation of activeNode
	 */
	protected initActiveNode (desc: { selected: ProofItem, tooltip?: string }): void {
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
					this.ghostNode.parent = this.activeNode.parent;
					this.ghostNode.realNode = this.activeNode;
				}
			}
			if (desc.tooltip) {
				this.activeNode.tooltip = desc.tooltip;
			}
		} else {
			console.warn(`[proof-explorer] Warning: could not initialize active node (selected node is null)`);
		}
	}

	/**
	 * Loads a proof descriptor in proof-explorer
	 * @param desc The proof descriptor to be loaded
	 */
	loadProofDescriptor (desc: ProofDescriptor): void {
		// utility function for building the proof tree
		const createTree = (elem: ProofNode, parent: ProofItem): void => {
			const node: ProofItem = (elem.type === "proof-command") ? 
				new ProofCommand(elem.name, elem.branch, parent) 
				: new ProofBranch(elem.name, elem.branch, parent);
			parent.appendChild(node);
			if (elem.rules && elem.rules.length) {
				elem.rules.forEach(child => {
					createTree(child, node);
				});
			} else {
				node.collapsibleState = TreeItemCollapsibleState.None;
			}
		}
		// initialise
		this.root = null;
		this.refreshView();
		if (desc && desc.info) {
			this.root = new RootNode({ 
				name: desc.info.formula, //(desc.proof) ? desc.proof.name : desc.info.formula, 
				proofStatus: desc.info.status
			});
			this.ghostNode = new GhostNode({ parent: this.root, node: this.root });
			if (desc.proofTree && desc.proofTree.rules && desc.proofTree.rules.length
					// when proof is simply (postpone), this is an empty proof, don't append postpone
					&& !(desc.proofTree.rules.length === 1 && desc.proofTree.rules[0].name === "(postpone)")) {
				desc.proofTree.rules.forEach((child: ProofNode) => {
					createTree(child, this.root);
				});
			} else {
				this.root.collapsibleState = TreeItemCollapsibleState.None;
			}
			this.proofDescriptor = desc;

			// this.root.proofState = this.initialProofState;
			// this.root.tooltip = utils.formatProofState(this.initialProofState);
			// this.root.pending();
			// this.root.setProofStatus(desc.info.status);
			// // select either the first child or the root if children are not present
			// const selected: ProofItem = (this.root.children && this.root.children.length) ? this.root.children[0] : this.root;
			// // initialise this.activeNode
			// this.initActiveNode({ selected }); 
			// // update the user interface
			// this.markAsActive({ selected: this.activeNode }, { force: true });
			// this.activeNode.proofState = this.initialProofState;
			// this.activeNode.updateTooltip();
			// // start the proof
			// // this.startProof();
			// this.dirtyFlag = false;
		}
		// refresh view
		this.refreshView();

		// register handler for terminal closed events
		const handler: vscode.Disposable = window.onDidCloseTerminal((e: Terminal) => {
			if (e && e.name === `${this.formulaDescriptor.theoryName}.${this.formulaDescriptor.formulaName}`) {
				// don't save the proof if the user types quit in the command prompt
				// @TODO: add a command to save the proof from the command prompt
				
				// unregister handler
				handler.dispose();
				// dispose view
				this.disposeView();
			}
		});
	}

	/**
	 * Utility function, returns the current proof tree in JSON format.
	 */
	getProofDescriptor (): ProofDescriptor {
		const makeProofStructure = (node: ProofItem): ProofNode => {
			const res: ProofNode = {
				branch: node.branchId,
				name: node.name,
				type: <ProofNodeType> node.contextValue,
				rules: []
			};
			for (let i = 0; i < node.children.length; i++) {
				const child: ProofNode = makeProofStructure(node.children[i]);
				res.rules.push(child);
			}
			return res;
		}
		const proofTree: ProofTree = makeProofStructure(this.root);
		const proofDescriptor: ProofDescriptor = {
			info: {
				theory: this.formulaDescriptor.theoryName,
				formula: this.formulaDescriptor.formulaName,
				status: this.root.proofStatus,
				prover: utils.pvsVersionToString(this.pvsVersionDescriptor) || "7.x",
				shasum: this.shasum
			},
			proofTree
		};
		return proofDescriptor;
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

	setShasum (shasum: string): void {
		this.shasum = shasum;
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
	 * Identifies the pvs version used for the proof
	 * @param desc PVS version descriptor
	 */
	pvsReady(desc: PvsVersionDescriptor): void {
		this.pvsVersionDescriptor = desc;
	}
	
	/**
	 * Save the current proof on file
	 * @param opt Optionals: whether confirmation is necessary before saving (default: confirmation is not needed)  
	 */
	async saveProof (opt?: { msg?: string, force?: boolean, quiet?: boolean }): Promise<boolean> {
		return new Promise(async (resolve, reject) => {
			opt = opt || {};
			// register handler that will resolve the promise when the proof needs to be saved
			this.client.onRequest(serverEvent.saveProofResponse, (desc: {
				response: { success: boolean, msg?: string }, 
				args: { 
					fileName: string, 
					fileExtension: string, 
					theoryName: string, 
					formulaName: string, 
					contextFolder: string, 
					proofDescriptor: ProofDescriptor
				}
			}) => {
				const fname: string = `${desc.args.fileName}.jprf`;
				if (desc.response.success) {
					if (!opt.quiet) {
						window.showInformationMessage(`Proof ${desc.args.proofDescriptor.proofTree.name} saved in file ${fname}`);
					}
				} else {
					window.showErrorMessage(`Unexpected error while saving file ${fname} (please check pvs-server output for details)`);
				}
				resolve(desc.response.success);
			});
			// ask the user if the proof is to be saved
			const yesno: string[] = [ "Yes", "No" ];
			const note: string = (opt.msg) ? `${opt.msg}\n` : "";
			const msg: string = note + `Save proof ${this.root.name}?`;
			const ans: string = (opt.force) ? yesno[0]
				: (this.dirtyFlag) ? await vscode.window.showInformationMessage(msg, { modal: true }, yesno[0])
				: yesno[1]; // dont' save if the proof hasn't changed
			if (ans === yesno[0]) {
				// update proof descriptor
				this.proofDescriptor = this.getProofDescriptor();
				// save proof descriptor to file
				this.client.sendRequest(serverCommand.saveProof, { 
					fileName: this.formulaDescriptor.fileName,
					fileExtension: this.formulaDescriptor.fileExtension,
					contextFolder: this.formulaDescriptor.contextFolder,
					theoryName: this.formulaDescriptor.theoryName,
					formulaName: this.formulaDescriptor.formulaName,
					proofDescriptor: this.proofDescriptor
				});
			} else {
				resolve();
			}
		});
	}
	/**
	 * Quit the current proof
	 * @param opt Optionals: whether confirmation is necessary before quitting (default: confirmation is needed)  
	 */
	async quitProof (opt?: { confirm?: boolean, save?: boolean }): Promise<void> {
		this.running = false;
		const autorun: boolean = this.formulaDescriptor.autorun;
		const status: ProofStatus = (this.root) ? this.root.proofStatus : "untried";
		opt = opt || {};
		opt.confirm = (opt.confirm === undefined || opt.confirm === null) ? true : opt.confirm;
		// ask the user if the proof is to be saved
		const yesno: string[] = [ "Yes", "No" ];
		const msg: string = `Quit proof session?`;
		const ans: string = (opt.confirm) ? await vscode.window.showInformationMessage(msg, { modal: true }, yesno[0])
								: yesno[0];
		if (ans === yesno[0]) {
			if (opt.save) {
				await this.saveProof({ force: true, quiet: true });
			}
			// send quit to the terminal
			commands.executeCommand("vscode-pvs.send-proof-command", {
				fileName: this.formulaDescriptor.fileName,
				fileExtension: this.formulaDescriptor.fileExtension,
				theoryName: this.formulaDescriptor.theoryName,
				formulaName: this.formulaDescriptor.formulaName,
				contextFolder: this.formulaDescriptor.contextFolder,
				cmd: "quit"
			});
			// delete data structures
			this.root = null;
			this.ghostNode = null;
			this.activeNode = null;
			this.formulaDescriptor = null;
		}

		// run the other proofs if desc.autorun === true
		if (autorun) {
			// we need to use a small delay to give pvs-server the time to quit
			setTimeout(() => {
				// commands.executeCommand("vscode-pvs.autorun-formula-end", this.desc);
				this.autorunCallback(status);
			}, 1000);
		}
	}
	/**
	 * Activation function, installs all proof-explorer command handlers.
	 * @param context Client context 
	 */
	activate(context: ExtensionContext): void {
		// -- handlers for proof explorer commands
		context.subscriptions.push(commands.registerCommand("proof-explorer.save-proof", () => {
			this.saveProof({ force: true });
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.quit-proof", () => {
			this.quitProof({ confirm: true });
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.forward", () => {
			this.forward();
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.back", () => {
			this.back();
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.run-proof", () => {
			this.root.pending();
			this.run();
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.fast-forward", (resource: ProofItem) => {
			this.root.pending();
			this.fastForwardTo({ selected: resource });
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.copy-node", (resource: ProofItem) => {
			this.copyNode({ selected: resource });
			window.showInformationMessage(`${resource.name} copied in clipboard`);
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.copy-subtree", (resource: ProofItem) => {
			this.copyTree({ selected: resource });
			window.showInformationMessage(`${resource.name} copied in clipboard`);
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
		context.subscriptions.push(commands.registerCommand("proof-explorer.mark-as-active", (resource: ProofItem) => {
			this.markAsActive({ selected: resource });
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
			if (this.ghostNode && this.ghostNode.isActive()) {
				for (let i = 0; i < children.length; i++) {
					if (children[i] === this.ghostNode.realNode) {
						const res: TreeItem[] = children.slice(0, i + 1).concat([this.ghostNode]).concat(children.slice(i + 1));
						return Promise.resolve(res);
					}
				}
			}
			return Promise.resolve(children);
		} else if (this.ghostNode && this.ghostNode.isActive() && this.ghostNode.realNode === this.root) {
			return Promise.resolve([ this.root, this.ghostNode ]);
		} else if (this.root) {
			return Promise.resolve([ this.root ]);
		} else {
			return Promise.resolve([ this.welcome ]);
		}
	}
	/**
	 * Returns the requested node
	 * @param item Node to be returned
	 */
	getTreeItem(item: TreeItem): TreeItem {
		return item;
	}
	/**
	 * Returns the parent of a node. This method is necessaty for the correct execution of view.reveal()
	 * @param item Node whose parent should be returned
	 */
	getParent(item: ProofItem): ProofItem {
		if (item.contextValue === "root") {
			return null;
		}
		// ghost node needs special treatment
		// we need to return the parent of the ghost rather than that of ghost.realNode otherwhise vscode won't be able to show up the ghost
		// ghost.realNode.parent should not be returned otherwise vscode will generate an exception because the same node (the parent) is counted twice
		// if (item === this.ghostNode && !this.ghostNode.isActive()) {
		// 	return null;
		// }
		return item.parent;
	}
}


//-------------------------------------------------------------
// Auxiliary constants and definitons
//-------------------------------------------------------------

// https://emojipedia.org/symbols/
//  ❌ 🔵 ⚫ ⚪ 🔴 🔽 🔼 ⏯ ⏩ ⏪ ⏫ ⏬ ▶️ ◀️ ⭕ 🔹🔸💠🔷🔶

export const QED: ProofStatus = "proved";

/**
 * Definition of tree items
 */
class ProofItem extends TreeItem {
	contextValue: string = "proofItem";
	name: string; // prover command or branch id
	branchId: string = ""; // branch in the proof tree where this command is located (branchId for root is "").
	command: Command; // vscode action associated to the node
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
	protected noChangeFlag: boolean = false;
	proofState: ProofState = null; // sequents *before* the execution of the node
	constructor (type: string, name: string, branchId: string, parent: ProofItem, collapsibleState?: TreeItemCollapsibleState) {
		super(type, (collapsibleState === undefined) ? TreeItemCollapsibleState.Expanded : collapsibleState);
		this.contextValue = type;
		this.id = fsUtils.get_fresh_id();
		this.name = name;
		this.branchId = branchId;
		this.parent = parent;
		this.tooltip = ""; // the tooltip shows the sequent before the execution of the proof command
		this.notVisited();
	}
	clone (parent?: ProofItem): ProofItem {
		switch (this.contextValue) {
			case "root": { return <RootNode> this.clone(); }
			case "proof-branch": { return <ProofBranch> this.clone(); }
			case "proof-command": { return <ProofCommand> this.clone(); }
			default: {
				console.warn(`[proof-explorer] Warning: trying to clone node type ${this.contextValue}`);
			}
		}
		return new ProofItem(this.contextValue, this.name, this.branchId, parent, this.collapsibleState);
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
	updateTooltip (): void {
		this.tooltip = (this.proofState) ? utils.formatProofState(this.proofState) : " ";
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
		this.noChangeFlag = false;
	}
	visited (): void {
		this.previousState.tooltip = this.tooltip;
		this.previousState.icon = this.icon = " ★ ";
		this.label = `${this.icon}${this.name}`;
		this.activeFlag = false;
		this.visitedFlag = true;
		this.pendingFlag = false;
		this.noChangeFlag = false;
	}
	notVisited (): void {
		this.previousState.tooltip = this.tooltip;
		this.previousState.icon = this.icon = " ∘  ";
		this.label = `${this.icon}${this.name}`;
		this.activeFlag = false;
		this.visitedFlag = false;
		this.pendingFlag = false;
		this.noChangeFlag = false;
	}
	noChange (): void {
		this.previousState.tooltip = this.tooltip;
		this.previousState.icon = this.icon;
		this.icon = '⭕';
		this.label = `${this.icon}${this.name}`;
		this.activeFlag = false;
		this.visitedFlag = false;
		this.pendingFlag = false;
		this.noChangeFlag = true;
	}
	active (): void {
		this.icon = "🔷";
		this.label = `${this.icon}${this.name}`;
		this.activeFlag = true;
		this.visitedFlag = false;
		this.pendingFlag = false;
		this.noChangeFlag = false;
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
		// this.tooltip = this.previousState.tooltip;
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
	moveIndicatorForward (opt?: { lastVisitedChild?: ProofItem, keepSameBranch?: boolean }): ProofItem | null {
		opt = opt || {};
		if (this.contextValue === "proof-command") {
			this.visited();
		} else {
			// proof branch or root
			this.pending();
		}
		// check if this node has children, if so, return the first non-visited child
		if (this.children && this.children.length) {
			if (opt.lastVisitedChild) {
				// the next node to be visited is the one after the last visited
				const idx: number = this.children.indexOf(opt.lastVisitedChild);
				const next: number = idx + 1;
				if (next < this.children.length) {
					return this.children[next];
				}
			} else {
				return this.children[0];
			}
		}
		// else, branch completed
		// go to next sibling, unless opt.keepSameBranch === true 
		if (opt.keepSameBranch 
				&& ((this.contextValue === "proof-branch" && this.children.length === 0) 
					|| (this.contextValue === "proof-command" && this.children.length > 0))) {
			return null;
		}
		this.visited();
		if (this.parent && this.contextValue === "proof-command") {
			return this.parent.moveIndicatorForward({ lastVisitedChild: this });
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
	moveIndicatorBack (opt?: { keepSameBranch?: boolean }): ProofItem {
		if (this.parent) {
			if (this.parent.children && this.parent.children.length) {
				const children: ProofItem[] = this.parent.children.filter((item: ProofItem) => {
					return item.isVisitedOrPending();
				});
				// return previous child, if any
				if (children && children.length) {
					this.notVisited();
					const candidate: ProofItem = children[children.length - 1];
					if (candidate.children && candidate.children.length) {
						return candidate.selectLastVisitedChild();
					}
					return candidate;
				}
				// return first proof command, if parent is root
				if (this.parent.contextValue === "root" && this.parent.children && this.parent.children.length) {
					// we were on the first proof command, cannot undo
					return this;
				}
				// else return parent
				// unless opt.keepSameBranch === true 
				if (opt.keepSameBranch && this.contextValue === "proof-command") {
					return this;
				}
				this.notVisited();
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
		this.name = (cmd && cmd.startsWith("(") && cmd.endsWith(")")) ? cmd : `(${cmd})`;
		this.notVisited();
		this.command = {
			title: this.contextValue,
			command: "explorer.didClickOnStrategy",
			arguments: [ this.contextValue ]
		};
	}
	// @override
	clone (parent?: ProofCommand): ProofCommand {
		const c: ProofCommand = new ProofCommand(this.name, this.branchId, parent, this.collapsibleState);
		return c;
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
	// @override
	clone (parent?: ProofBranch): ProofBranch {
		const c: ProofBranch = new ProofBranch(this.name, this.branchId, this.parent, this.collapsibleState);
		c.parent = parent || null;
		return c;
	}

}
class WelcomeScreen extends TreeItem {
	constructor () {
		super("welcome-screen", TreeItemCollapsibleState.None);
		this.label = "Proof Explorer will become active when starting a proof";
	}
}
class RootNode extends ProofItem {
	proofStatus: ProofStatus; // this is updated while running the proof
	initialProofStatus: ProofStatus; // this is set at the beginning (and at the end of the proof attempt if the proof succeeds)
	constructor (desc: { name: string, proofStatus?: ProofStatus }) {
		super("root", desc.name, "", null, TreeItemCollapsibleState.Expanded);
		this.parent = this; // the parent of the root is the root itself
		this.proofStatus = desc.proofStatus || "untried"
		this.initialProofStatus = this.proofStatus;
		this.notVisited();
		this.command = {
			title: this.contextValue,
			command: "explorer.didClickOnStrategy",
			arguments: [ this.contextValue ]
		};
	}
	// @overrides
	clone (parent?: RootNode): RootNode {
		const c: RootNode = new RootNode({ name: this.name, proofStatus: this.proofStatus });
		c.parent = parent || null;
		c.pending();
		return c;
	}

	// @overrides
	notVisited (): void {
		// this.proofStatus = "untried";
		super.notVisited();
		this.updateLabel();
	}
	// @overrides
	visited (): void {
		// do nothing
	}
	// @overrides
	pending (): void {
		if (this.proofStatus === "untried") {
			this.proofStatus = "unfinished";
		}
		super.pending();
		this.updateLabel();
	}
	QED (): void {
		super.visited();
		this.icon = utils.icons.checkmark;
		this.proofStatus = QED;
		this.setProofStatus(QED);
		this.updateLabel();
	}
	isQED (): boolean {
		return this.proofStatus === QED;
	}
	proofStatusChanged (): boolean {
		return this.initialProofStatus !== this.proofStatus;
	}
	setProofStatus (proofStatus: ProofStatus): void {
		if (proofStatus) {
			this.proofStatus = proofStatus;
			this.updateLabel();
		}
	}
	resetProofStatus (): void {
		this.proofStatus = this.initialProofStatus;
		this.updateLabel();
	}
	protected updateLabel (): void {
		const proofStatus: ProofStatus = this.proofStatus || "untried";
		this.label = `${this.icon}${this.name} (${proofStatus})`;
		// if (this.initialProofStatus === this.proofStatus) {
		// 	this.label = `${this.icon}${this.name} (${this.proofStatus})`;
		// } else {
		// 	this.label = `${this.icon}${this.name} (${this.initialProofStatus} - ${this.proofStatus})`;
		// }
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
	notActive (): void {
		this.activeFlag = false;
		this.label = "";
	}
	// @overrides
	notVisited () {
		super.notVisited();
		this.label = "";
	}
	// @overrides
	moveIndicatorBack (): ProofItem {
		this.notActive();
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
}

