/**
 * @module PvsProofExplorer
 * @author Paolo Masci
 * @date 2020.07.20
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
import { 
	ProofNode, ProofNodeType, ProofDescriptor, ProofStatus, serverEvent, 
	ProofTree, PvsFormula, ProofNodeX, PvsProofCommand, ProofEditCopyNode, ProofEditAppendNode, ProofEditDidCopyNode, 
	ProofEditPasteNode, ProofExecFastForward, ProofEditCopyTree, ProofEditDidCopyTree, 
	ProofEditPasteTree, ProofEditDeleteNode, ProofEditDidDeleteNode, ProofEditAppendBranch,
	ProofEditCutNode,
	ProofEditDidCutNode,
	ProofEditCutTree,
	ProofEditDidCutTree,
	ProofEditDeleteTree,
	ProofEditTrimNode,
	ProofEditDidTrimNode,
	ProofEditRenameNode,
	ProofEditDidRenameNode,
	ProofEditDidAppendNode,
	PvsContextDescriptor,
	ProofEditDidActivateCursor,
	ProofEditDidDeactivateCursor,
	CliGatewayPrintProofCommand,
	CliGatewayProofState,
	CliGatewayQED,
	ProofEditDidUpdateProofStatus,
	ProofExecDidLoadProof,
	ProofExecDidLoadSequent,
	ProofExecDidStartProof,
	ProofExecDidUpdateSequent,
	ProofEditTrimUnused,
	ProofExecQuit,
	ProofEditDidStartNewProof,
	CliGatewayQuitEvent,
	CliGatewayQuit,
	ProofFile,
	ProofExecDidOpenProof,
	PvsFile, ProofExecQuitAndSave, PvsVersionDescriptor, ProofExecDidImportProof, FileDescriptor
} from '../common/serverInterface';
import * as utils from '../common/languageUtils';
import * as fsUtils from '../common/fsUtils';
import { PvsResponse, PvsError } from '../common/pvs-gui';
import { SequentDescriptor } from '../common/languageUtils';
import { Connection } from 'vscode-languageserver';
import { PvsProxy } from '../pvsProxy';
import { PvsLanguageServer } from '../pvsLanguageServer';
import * as path from 'path';

abstract class TreeItem {
	id: string;
	tooltip: string;
	connection: Connection;
	constructor(label: string, connection: Connection) {
		this.id = fsUtils.get_fresh_id();
		this.connection = connection;
		this.tooltip = "";
	}
}

/**
 * Proof Explorer
 */
export class PvsProofExplorer {
	protected shasum: string;

	protected interruptFlag: boolean = false;

	protected logFileName: string;
	protected tmpLogFileName: string;

	protected dirtyFlag: boolean = false; // indicates whether the proof has changed since the last time it was saved

	protected connection: Connection; // connection to the client
	protected pvsProxy: PvsProxy;
	protected pvsLanguageServer: PvsLanguageServer;

	readonly DEBUG_LOG: boolean = false;
	protected log (...args): void {
		if (this.DEBUG_LOG) {
			console.log(args);
		}
	}

	/**
	 * Clipboards for cut/paste operations
	 */
	protected clipboard: ProofItem = null;
	protected clipboardTree: ProofItem[] = null;

	/**
	 * Information on the formula loaded in proof explorer
	 **/
	protected formula: PvsFormula;
	
	protected autorunFlag: boolean = false;
	protected autorunCallback: (status: ProofStatus) => void;

	protected filterOnTypeActive: boolean = false;

	/**
	 * Attributes for run-time management of the proof tree rendered in the view
	 */
	protected welcome: WelcomeScreen = null;
	protected root: RootNode = null // the root of the tree
	protected ghostNode: GhostNode = null; // this is a floating node that follows activeNode. It is used during proof development, to signpost where the next proof command will be appended in the proof tree
	protected activeNode: ProofCommand | ProofBranch | GhostNode = null;

	// protected expectProofRequest: boolean = false;

	protected running: boolean = false; // status flag, indicates whether we are running all proof commands, as opposed to stepping through the proof commands
	protected stopAt: ProofItem = null; // indicates which node we are fast-forwarding to

	protected undoundoTarget: ProofItem = null;
	// protected previousPath: string = "";

	/**
	 * JSON representation of the proof script for the current proof.
	 * The representation is updated at the beginning of the proof session.
	 */
	protected proofDescriptor: ProofDescriptor;

	/**
	 * Initial proof state
	 */
	protected initialProofState: SequentDescriptor;

	/**
	 * Current proof state
	 */
	protected proofState: SequentDescriptor;


	constructor (connection: Connection, pvsProxy: PvsProxy, pvsLanguageServer: PvsLanguageServer) {
		this.connection = connection;
		this.pvsProxy = pvsProxy;
		this.pvsLanguageServer = pvsLanguageServer;
		this.welcome = new WelcomeScreen(this.connection)
    }

	// utility accessor functions, useful for testing purposes
	getProofX (): ProofNodeX {
		if (this.root) {
			const structure: ProofNodeX = this.root.getNodeXStructure();
			return structure;
		}
		return null;
	}
	getActiveNode (): ProofNodeX {
		if (this.activeNode) {
			return this.activeNode.getNodeXStructure();
		}
		return null;
	}
	getTooltip (desc: { selected: { id: string, name: string }}): string | null {
		if (desc && desc.selected) {
			const item: ProofItem = this.findNode(desc.selected.id);
			if (item) {
				return item.tooltip;
			}
		}
		return null;
	}
	getGhostNode (): ProofNodeX {
		if (this.ghostNode) {
			return this.ghostNode.getNodeXStructure();
		}
		return null;
	}
	ghostNodeIsActive (): boolean {
		return this.ghostNode && this.ghostNode.isActive();
	}
	isVisited (desc: { id: string, name: string }): boolean {
		if (desc) {
			const item: ProofItem = this.findNode(desc.id);
			if (item) {
				return item.isVisited();
			}
		}
		console.warn(`[proof-explorer] Warning: failed to check visited flag`);
		return false;
	}
	isPending (desc: { id: string, name: string }): boolean {
		if (desc) {
			const item: ProofItem = this.findNode(desc.id);
			if (item) {
				return item.isPending();
			}
		}
		console.warn(`[proof-explorer] Warning: failed to check pending flag`);
		return false;
	}
	isActive (desc: { id: string, name: string }): boolean {
		if (desc) {
			const item: ProofItem = this.findNode(desc.id);
			if (item) {
				return item.isActive();
			}
		}
		console.warn(`[proof-explorer] Warning: failed to check active flag`);
		return false;
	}
	newProof (): void {
		const evt: ProofEditDidStartNewProof = {
			action: "did-start-new-proof" 
		};
		this.connection.sendNotification(serverEvent.proverEvent, evt);
	}


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
			console.warn("[proof-explorer] Warning: active node is null");
		}
	}
	/**
	 * Internal function, moves the active node one position forward in the proof tree.
	 */
	protected moveIndicatorForward (opt?: { keepSameBranch?: boolean, proofState?: SequentDescriptor, branchComplete?: boolean }): ProofItem {
		if (this.activeNode) {
			opt = opt || {};
			if (this.activeNode.contextValue !== "ghost") {
				const next: ProofItem = this.activeNode.moveIndicatorForward(opt) || this.ghostNode;
				if (next === this.ghostNode) {
					this.ghostNode.parent = this.activeNode.parent;
					this.ghostNode.realNode = this.activeNode;
					this.ghostNode.sequentDescriptor = opt.proofState;
					this.ghostNode.updateTooltip({ internalAction: this.autorunFlag });
					// this.ghostNode.setTooltip(this.activeNode.tooltip);
					this.ghostNode.active();
					this.revealNode({ selected: this.ghostNode });
					if (this.ghostNode.parent.contextValue === "proof-command" || opt.branchComplete) {
						this.ghostNode.parent.visited();						
					} else {
						this.ghostNode.parent.pending();
					}
					// this.root.pending();
				} else {
					this.markAsActive({ selected: next });
					this.revealNode({ selected: next });
					if (opt.proofState) {
						this.proofState = this.activeNode.sequentDescriptor = opt.proofState;
						this.activeNode.updateTooltip({ internalAction: this.autorunFlag });
						// this.activeNode.setTooltip(utils.formatProofState(this.activeNode.proofState));		
					}
				}
				return next;
			}
		} else {
			console.warn("[proof-explorer] Warning: active node is null");
		}
		return null;
	}
	/**
	 * Executes all proof commands in the proof tree, starting from the active node.
	 * The execution stops either at the end of the proof tree, or when an anomaly 
	 * is detected in the proof tree (e.g,. the prover generates more goals than those 
	 * indicated in the proof tree)
	 */
	async run (opt?: { feedbackToTerminal?: boolean }): Promise<void> {
		opt = opt || {};
		if (!this.running) {
			this.running = true;
			await this.step(opt);
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
	async fastForwardTo (desc: { selected: ProofItem }): Promise<void> {
		if (desc && desc.selected) {
			// adjust selected --- if it's a branch node, select the first children
			const target: ProofItem = (desc.selected && desc.selected.contextValue === "proof-branch" 
				&& this.activeNode.children && this.activeNode.children.length) ? desc.selected.children[0]
					: desc.selected;
			if (!target.isVisited() && this.activeNode && this.ghostNode && !this.ghostNode.isActive()) {
				this.stopAt = target;
				this.running = true;
				await this.step({ feedbackToTerminal: true });
			} else {
				// TODO: rewind
			}
		} else {
			console.warn(`[proof-explorer] Warning: failed to fast forward (selected node is null)`);
		}
	}
	async fastForwardToNodeX (desc: ProofExecFastForward): Promise<void> {
		if (desc && desc.selected) {
			const selected: ProofItem = this.findNode(desc.selected.id);
			if (selected) {
				await this.fastForwardTo({ selected });
			}
		} else {
			console.warn(`[proof-explorer] Warning: failed to perform proof exec/fast forward (selected node is null)`);
		}
	}
	
	/**
	 * repeats the last command
	 */
	async redo (): Promise<PvsResponse | null> {
		return await this.forward();
	}

	/**
	 * executes the next step
	 */
	async forward (): Promise<PvsResponse | null> {
		const response: PvsResponse = await this.step({ feedbackToTerminal: true });
		// if (response) {
		// 	if (response.result) {
		// 		const channelID: string = utils.desc2id(req);
		// 		const result: SequentDescriptor[] = response.result;
		// 		if (result.length) {
		// 			const sequent: SequentDescriptor = result[result.length - 1];
		// 			if (sequent["prover-session-status"]) {
		// 				// FIXME: this field is provided only by json-output patch, not by the xmlrpc server -- either use it or don't, adopt a standard solution!
		// 				// branch closed, or proof completed
		// 				console.dir(sequent);
		// 			} else {
		// 				// FIXME: pvs-server needs to provide a string representation of the command, not its structure!
		// 				const command: string = 
		// 					(sequent && sequent["last-cmd"] 
		// 						&& !utils.isUndoCommand(cmd)
		// 						&& !utils.isUndoUndoCommand(cmd)
		// 						&& !utils.isPostponeCommand(cmd)) ? sequent["last-cmd"] : cmd;
		// 				if (this.connection) {
		// 					this.connection.sendRequest(serverEvent.proofCommandResponse, { 
		// 						response: { result: sequent }, 
		// 						args: { 
		// 							fileName: req.fileName,
		// 							fileExtension: req.fileExtension,
		// 							contextFolder: req.contextFolder,
		// 							theoryName: req.theoryName,
		// 							formulaName: req.formulaName,
		// 							cmd: command
		// 						}
		// 					});
		// 				}
		// 				// check if the proof is complete
		// 				if (utils.QED(sequent)) {
		// 					if (this.connection) {
		// 						this.connection.sendRequest(serverEvent.QED, { response: { result: sequent }, args: req });
		// 						// trigger a context update, so proof status will be updated on the front-end
		// 						const cdesc: PvsContextDescriptor = await this.pvsLanguageServer.getContextDescriptor({ contextFolder: req.contextFolder });
		// 						this.connection.sendRequest(serverEvent.contextUpdate, cdesc);
		// 					}
		// 					// re-generate tccs
		// 					await this.pvsLanguageServer.generateTccsRequest(req, { quiet: true });
		// 					// stop the loop & don't send proof-state to cli gateway
		// 					return;
		// 				}
		// 				// show feedback in CLI only after executing the last command in the sequence
		// 				if (i === cmdArray.length - 1) {
		// 					this.pvsLanguageServer.cliGateway.publish({ type: "pvs.event.proof-state", channelID, data: sequent, cmd });
		// 				}
		// 			}
		// 		}	
		// 	} else {
		// 		this.pvsLanguageServer.pvsErrorManager.handleProofCommandError({ cmd, response: <PvsError> response });
		// 	}
		// }
		return response;
		// if (this.ghostNode && !this.ghostNode.isActive()) {
		// 	this.root.pending();
		// 	// if forward is called on a branch, move to the first child
		// 	if (this.activeNode && this.activeNode.contextValue === "proof-branch" 
		// 			&& this.activeNode.children && this.activeNode.children.length) {
		// 		this.stopAt = this.activeNode.children[0];
		// 		this.running = true;
		// 	}
		// 	this.step();
		// }
	}
	/**
	 *  goes one step back (undo)
	 */
	async back (): Promise<PvsResponse | null> {
		return await this.undo();
	}
	/**
	 * goes one step back
	 */
	async undo (): Promise<PvsResponse | null> {
		// this.root.pending();
		return await this.step({ cmd: "undo", feedbackToTerminal: true });
	}
	/**
	 * Executes a proof command. This handler is for proof commands from the proof tree stored in proof-explorer.
	 * The function is activated when the user clicks forward/back/play on the front-end.
	 * The command is taken from the proof tree.
	 * The command can also be specified as function argument -- this is useful for handling commands entered by the used at the prover prompt. 
	 * @param cmd Optional parameter, specifying the command to be executed.
	 */
	async step (opt?: { cmd?: string, feedbackToTerminal?: boolean }): Promise<PvsResponse | null> {
		opt = opt || {};

		if (this.stopAt && this.activeNode && this.stopAt.id === this.activeNode.id) {
			this.stopAt = null;
			this.running = false;
			return null;
		}

		const cmd: string = (opt.cmd) ? opt.cmd : 
				(this.activeNode?.contextValue === "proof-command" && (this.ghostNode && !this.ghostNode.isActive())) ?
					this.activeNode?.name 
						: null;
		if (cmd) {
			if (this.running && !this.autorunFlag && utils.isPostponeCommand(cmd)) {
				// stop the proof at the first postpone when running the proof in proof explorer, except if this is a re-run triggered by M-x prt or M-x pri (autorunFlag)
				this.running = false;
				return null;
			}
			if (opt.feedbackToTerminal && !this.autorunFlag) {
				const channelID: string = utils.desc2id(this.formula);
				const evt: CliGatewayPrintProofCommand = { type: "pvs.event.print-proof-command", channelID, data: { cmd } };
				this.pvsLanguageServer.cliGateway.publish(evt);
			}
			const command: PvsProofCommand = {
				fileName: this.formula.fileName,
				fileExtension: this.formula.fileExtension,
				theoryName: this.formula.theoryName,
				formulaName: this.formula.formulaName,
				contextFolder: this.formula.contextFolder,
				cmd: cmd.startsWith("(") ? cmd : `(${cmd})`
			};
			// console.dir(command, { depth: null });
			const response: PvsResponse = await this.proofCommand(command);
			if (this.interruptFlag) {
				this.interruptFlag = false;
				return response;
			}
			// console.dir(response, { depth: null });
			if (response && response.result && response.result.length) {
				// response.result = response.result.reverse(); // having the most recent sequent in position 0 is only bringing havoc!
				for (let i = 0; i < response.result.length; i++) {
					const proofState: SequentDescriptor = response.result[i]; // process proof commands
					// if (proofState && proofState.path) {
						await this.onStepExecutedNew({ proofState, args: command, lastSequent: i === response.result.length - 1 }, opt);
					// } else {
					// 	await this.onStepExecuted({ proofState, args: command}, opt);
					// }
				}
				// if a proof is running, then iterate
				if (this.running && !this.ghostNode.isActive()) {
					this.step(opt);
				}
			}
			return response;
		}
		if (this.autorunFlag) {
			this.running = false;
			this.stopAt = null;	
			// mark proof as unfinished
			if (this.root.getProofStatus() !== "untried") {
				this.root.setProofStatus("unfinished");
			}
			// quit proof and update proof status in the jprf file
			await this.quitProofAndSave({ jprfOnly: true });
			// await this.quitProof();
			return null;
		}
		return null;
	}
	protected restoreTreeAttributes (): void {
		if (this.root) {
			this.root.restoreTreeAttributes();
		}
	}
	protected saveTreeAttributes (): void {
		if (this.root) {
			this.root.saveTreeAttributes();
		}
	}
	/**
	 * Utility function, checks if the proof structure has changed
	 * This check is done when the active node (command) produces new branches and 
	 * the prover advances to a new branch that is not one of the children of the active node
	 * @param desc 
	 */
	protected proofStructureHasChanged (desc: { activeNode: ProofItem, targetBranch: ProofBranch }): boolean {
		if (desc) {
			return !desc.targetBranch || !desc.activeNode.children || desc.activeNode.children.filter((item) => {
				return item.name === desc.targetBranch.name;
			}).length === 0;
		}
		return false;
	}
	/**
	 * Call-back function invoked after step(), when the execution of a proof command is complete.
	 * @param desc Descriptor specifying the reponse of the prover, as well as the actual values of the arguments used to invoke the step function.
	 */
	// async onStepExecuted (desc: { proofState: SequentDescriptor, args: PvsProofCommand }, opt?: { feedbackToTerminal?: boolean }): Promise<void> {
	// 	if (desc && desc.proofState && desc.args) {
	// 		// get command and proof state
	// 		let cmd: string = desc.args.cmd;//desc.proofState["last-cmd"] || desc.args.cmd; // command entered by the user
	// 		this.proofState = desc.proofState;
			
	// 		// identify active node in the proof tree
	// 		let activeNode: ProofItem = this.ghostNode.isActive() ? this.ghostNode.realNode : this.activeNode;

	// 		//--- check meta-commands that will terminate the proof session: (QED), (quit)
	// 		// if QED, update proof status and stop execution
	// 		if (utils.QED(this.proofState)) {
	// 			this.running = false;
	// 			this.stopAt = null;

	// 			// if cmd !== activeNode.name then the user has entered a command manually: we need to append a new node to the proof tree
	// 			if (this.proofState.sequent && (utils.isSameCommand(activeNode.name, cmd) === false || this.ghostNode.isActive())) {
	// 				// concatenate new command
	// 				const elem: ProofCommand = new ProofCommand(cmd, activeNode.branchId, activeNode.parent, this.connection);
	// 				// append before selected node (the active not has not been executed yet)
	// 				if (activeNode.isActive()) {
	// 					elem.sequentDescriptor = activeNode.sequentDescriptor;
	// 					elem.updateTooltip({ internalAction: this.autorunFlag });
	// 					// elem.setTooltip(activeNode.tooltip);
	// 					activeNode.notVisited(); // this resets the tooltip in activeNode
	// 					this.appendNode({ selected: activeNode, elem }, { beforeSelected: true });
	// 				} else {
	// 					elem.sequentDescriptor = (this.ghostNode.isActive()) ? this.ghostNode.sequentDescriptor : activeNode.sequentDescriptor;
	// 					elem.updateTooltip({ internalAction: this.autorunFlag });
	// 					// elem.setTooltip(utils.formatProofState(proofState));
	// 					this.appendNode({ selected: activeNode, elem });
	// 				}
	// 				this.markAsActive({ selected: elem }); // this is necessary to correctly update the data structures in endNode --- elem will become the parent of endNode
	// 				activeNode = this.activeNode; // update local variable because the following instructions are using it
	// 			}

	// 			// trim the node if necessary
	// 			if (activeNode) {
	// 				this.trimNode({ selected: activeNode });
	// 			}

	// 			// disable ghost node if necessary
	// 			this.ghostNode.notActive();

	// 			// mark proof as proved
	// 			await this.proved();
	// 			return;
	// 		}
	// 		// if command is quit, stop execution
	// 		if (utils.isQuitCommand(cmd)) {
	// 			this.running = false;
	// 			this.stopAt = null;
	// 			return;	
	// 		}

	// 		// identify previous and current (new) branch
	// 		const previousBranch: string = activeNode.branchId;
	// 		const newBranch: string = utils.getBranchId(this.proofState.label);

	// 		// show sequent in the terminal, if feedback was requested
	// 		if (opt.feedbackToTerminal && !this.autorunFlag) {
	// 			const channelID: string = utils.desc2id(this.formula);
	// 			const evt: CliGatewayProofState = { type: "pvs.event.proof-state", channelID, data: this.proofState };
	// 			this.pvsLanguageServer.cliGateway.publish(evt);
	// 		}

	// 		//--- check other meta-commands: (undo), (undo undo), (postpone), (show-hidden), (comment "..."), (help xxx)
	// 		// if command is undo, go back to the last visited node
	// 		if (utils.isUndoCommand(cmd)) {
	// 			this.running = false;
	// 			this.stopAt = null;
	// 			this.undoundoTarget = this.activeNode;
	// 			this.saveTreeAttributes();
	// 			if (utils.branchHasChanged({ newBranch, previousBranch })) {
	// 				const targetBranch: ProofBranch = this.findProofBranch(newBranch);
	// 				if (targetBranch) {
	// 					this.activeNode.notVisited();
	// 					// find the last visited child in the new branch
	// 					const visitedChildren: ProofCommand[] = targetBranch.children.filter((elem: ProofItem) => {
	// 						return elem.contextValue === "proof-command" && elem.isVisited();
	// 					});
	// 					const targetNode: ProofItem = visitedChildren.length ? visitedChildren[visitedChildren.length - 1] : targetBranch;
	// 					// mark the entire subtree as not visited
	// 					targetNode.treeNotVisited();
	// 					// mark the target node as active
	// 					this.revealNode({ selected: targetNode });
	// 					this.markAsActive({ selected: targetNode });
	// 				} else {
	// 					console.error(`[proof-explorer] Error: could not find branch ${newBranch} in the proof tree`);
	// 				}
	// 			} else {
	// 				this.moveIndicatorBack({ keepSameBranch: true });
	// 			}
	// 			return;
	// 		}

	// 		// if command is postpone, move to the new branch
	// 		if (utils.isPostponeCommand(cmd)) {
	// 			if (this.autorunFlag) {
	// 				this.running = false;
	// 				this.stopAt = null;	
	// 				// mark proof as unfinished
	// 				if (this.root.getProofStatus() !== "untried") {
	// 					this.root.setProofStatus("unfinished");
	// 				}
	// 				// save and quit proof
	// 				await this.saveProof();
	// 				await this.quitProof();
	// 				return;
	// 			}
	// 			if (utils.branchHasChanged({ newBranch, previousBranch })) {
	// 				if (this.ghostNode.isActive()) {
	// 					this.ghostNode.notActive();
	// 				} else {
	// 					this.activeNode.notVisited();
	// 				}
	// 				const targetBranch: ProofBranch = this.findProofBranch(newBranch) || this.createBranchRecursive({ id: newBranch });
	// 				if (targetBranch) {
	// 					// before moving to the target branch, mark current branch as open (i.e., not visited)
	// 					if (this.activeNode.contextValue === "proof-command") {
	// 						this.activeNode.parent.notVisited();
	// 					} else {
	// 						this.activeNode.notVisited();
	// 					}
	// 					// find the last visited child in the new branch
	// 					const visitedChildren: ProofCommand[] = targetBranch.children.filter((elem: ProofItem) => {
	// 						return elem.contextValue === "proof-command" && elem.isVisited();
	// 					});
	// 					const targetNode: ProofItem = (visitedChildren.length) ? visitedChildren[visitedChildren.length - 1] : targetBranch;
	// 					targetNode.pending();
	// 					// mark the target node as active
	// 					this.markAsActive({ selected: targetNode });
	// 					// window.showInformationMessage(msg);
	// 					this.moveIndicatorForward({ keepSameBranch: true, proofState: this.proofState });
	// 				} else {
	// 					console.error(`[proof-explorer] Error: could not find branch ${newBranch} in the proof tree. Proof Explorer is out of sync with PVS. Please restart the proof.`);
	// 				}
	// 			} else {
	// 				// same branch: we need to stop the execution, because this is the only branch left to be proved
	// 				this.running = false;
	// 				this.stopAt = null;
	// 			}
	// 			return;
	// 		}

	// 		// handle the special command (undo undo)
	// 		if (utils.isUndoUndoCommand(cmd)) {
	// 			if (this.undoundoTarget) {
	// 				let target: ProofItem = this.undoundoTarget;
	// 				// the (undo undo) target is typically a visited node
	// 				// it the target was active, then (undo) was performed when the ghost node was active and (undo undo) should make the ghost node visible
	// 				if (this.undoundoTarget.isActive() || this.undoundoTarget.contextValue !== "proof-command") {
	// 					this.ghostNode.parent = this.undoundoTarget.parent;
	// 					this.ghostNode.realNode = this.undoundoTarget;
	// 					target = this.ghostNode;
	// 				}
	// 				this.restoreTreeAttributes();
	// 				this.markAsActive({ selected: target});
	// 				this.undoundoTarget = null;
	// 			} else {
	// 				console.error(`[proof-explorer] Warning: unable to execute ${cmd}`);
	// 			}
	// 			return;
	// 		}

	// 		// handle (show-hidden) and (comment "xxx")
	// 		if (utils.isShowHiddenCommand(cmd) || utils.isCommentCommand(cmd)) {
	// 			// nothing to do, the prover will simply show the hidden formulas
	// 			return;
	// 		}

	// 		if (utils.isHelpCommand(cmd)) {
	// 			// do nothing, CLI will show the help message
	// 			return;
	// 		}

	// 		//--- check special conditions: empty/null command, invalid command, no change before proceeding
	// 		// if command is invalid command, stop execution and provide feedback to the user 
	// 		if (utils.isUndoUndoPlusCommand(cmd)) {
	// 			// this.running = false;
	// 			// vscode.commands.executeCommand('setContext', 'proof-explorer.running', false);
	// 			if (this.autorunFlag) {
	// 				// mark proof as unfinished
	// 				if (this.root.getProofStatus() !== "untried") {
	// 					this.root.setProofStatus("unfinished");
	// 				}
	// 				// save and quit proof
	// 				await this.saveProof();
	// 				await this.quitProof();
	// 			}
	// 			// return;
	// 		} else if (utils.isInvalidCommand(this.proofState) || utils.isEmptyCommand(cmd)) {
	// 			if (utils.isSameCommand(activeNode.name, cmd) && !this.ghostNode.isActive()) {
	// 				this.moveIndicatorForward({ keepSameBranch: true, proofState: this.proofState });
	// 				// mark the tree rooted at the previous active node as not visited
	// 				activeNode.treeNotVisited();
	// 			}
	// 		} else if (utils.noChange(this.proofState)) {
	// 			// if command produced no change, provide feedback to the user, stop any running proof
	// 			// move the indicator forward (keep same proof branch) if the command was in the proof tree 				
	// 			// window.showWarningMessage(this.proofState.commentary.join("\n"));
	// 			if (utils.isSameCommand(activeNode.name, cmd) && !this.ghostNode.isActive()) {
	// 				this.moveIndicatorForward({ keepSameBranch: true, proofState: this.proofState });
	// 				// mark the tree rooted at the previous active node as not visited
	// 				activeNode.treeNotVisited();
	// 			}
	// 		} else {
	// 			// regular prover command
	// 			// else, the prover has made progress with the provided proof command
	// 			// if cmd !== activeNode.name then the user has entered a command manually: we need to append a new node to the proof tree
	// 			if (!utils.isSameCommand(activeNode.name, cmd) || this.ghostNode.isActive()) {
	// 				this.running = false;
	// 				this.stopAt = null;
	// 				if (this.autorunFlag && this.ghostNode.isActive()) {
	// 					// mark proof as unfinished
	// 					if (this.root.getProofStatus() !== "untried") {
	// 						this.root.setProofStatus("unfinished");
	// 					}
	// 					// save and quit proof
	// 					await this.saveProof();
	// 					await this.quitProof();
	// 					return;
	// 				}
	// 				// concatenate new command
	// 				const elem: ProofCommand = new ProofCommand(cmd, activeNode.branchId, activeNode.parent, this.connection);
	// 				// append before selected node (the active not has not been executed yet)
	// 				if (activeNode.isActive()) {
	// 					elem.sequentDescriptor = activeNode.sequentDescriptor;
	// 					elem.updateTooltip({ internalAction: this.autorunFlag });
	// 					activeNode.notVisited(); // this resets the tooltip in activeNode
	// 					this.appendNode({ selected: activeNode, elem }, { beforeSelected: true });
	// 				} else {
	// 					elem.sequentDescriptor = (this.ghostNode.isActive()) ? this.ghostNode.sequentDescriptor : activeNode.sequentDescriptor;
	// 					elem.updateTooltip({ internalAction: this.autorunFlag });
	// 					this.appendNode({ selected: activeNode, elem });
	// 				}
	// 				this.markAsActive({ selected: elem }); // this is necessary to correctly update the data structures in endNode --- elem will become the parent of endNode
	// 				activeNode = this.activeNode; // update local variable because the following instructions are using it
	// 				// if the branch has changed, then we will be moving to a sub-goal --- we need to trim the node
	// 				if (utils.branchHasChanged({ newBranch, previousBranch })) {
	// 					this.trimNode({ selected: activeNode });
	// 				}
	// 			}

	// 			// check if previous branch has been completed
	// 			if (utils.branchComplete(this.proofState, this.formula.formulaName, previousBranch)) {
	// 				// PVS has automatically discharged the previous proof branch
	// 				// trim the rest of the tree if necessary
	// 				this.trimNode({ selected: activeNode });
	// 				this.removeNotVisited({ selected: activeNode.parent });
	// 			}

	// 			// if the branch has changed, move to the new branch
	// 			if (utils.branchHasChanged({ newBranch, previousBranch })) {
	// 				// find target branch
	// 				const targetBranch: ProofItem = this.findProofBranch(newBranch) || this.createBranchRecursive({ id: newBranch });
	// 				if (targetBranch) {
	// 					// update tooltip in target branch
	// 					targetBranch.sequentDescriptor = this.proofState;
	// 					targetBranch.updateTooltip({ internalAction: this.autorunFlag });					
	// 					// go to the new branch
	// 					activeNode.visited();
	// 					activeNode.parent.visited();
	// 					// find the last visited child in the new branch
	// 					const visitedChildren: ProofCommand[] = targetBranch.children.filter((elem: ProofItem) => {
	// 						return elem.contextValue === "proof-command" && elem.isVisited();
	// 					});
	// 					const targetNode: ProofItem = visitedChildren.length ? visitedChildren[visitedChildren.length - 1] : targetBranch;
	// 					targetNode.pending();
	// 					// mark target node as active
	// 					this.markAsActive({ selected: targetNode });
	// 				} else {
	// 					console.error(`[proof-explorer] Error: could not find branch ${targetBranch} in the proof tree`); // this should never happen, because targetBranch is created if it doesn't exist already
	// 				}
	// 			} else {
	// 				// if branch has not changed and the active node has subgoals, then the structure of the proof tree has changed -- we need to trim
	// 				// NOTE: we don't want to prune in the case of autorun, otherwise part of the proof will be discarded permanently
	// 				if (!this.autorunFlag && activeNode.children && activeNode.children.length) {
	// 					this.trimNode({ selected: activeNode });
	// 				}
	// 			}

	// 			// // append subgoals if needed
	// 			// let nSubGoals: number = (this.proofState["num-subgoals"] && this.proofState["num-subgoals"] > 1) ? (this.proofState["num-subgoals"] - activeNode.children.length) : 0;
	// 			// if (nSubGoals) { //} || utils.siblingBranchComplete(this.proofState, newBranch)) {
	// 			// 	const targetBranch: ProofItem = this.findProofBranch(newBranch);
	// 			// 	// check if the tree has changed structure, if so trim the proof tree and create new branches
	// 			// 	if (this.proofStructureHasChanged({ activeNode: this.activeNode, targetBranch })) {
	// 			// 		this.trimNode({ selected: this.activeNode });
	// 			// 		for (let i = 0; i < nSubGoals; i++) {
	// 			// 			this.appendBranch({ selected: activeNode }, { firstBranch: newBranch, proofState: this.proofState });
	// 			// 		}
	// 			// 	}
	// 			// }

	// 			// finally, move indicator forward and propagate tooltip to the new active node
	// 			this.moveIndicatorForward({ keepSameBranch: true, proofState: this.proofState });
	// 		}

	// 		// check if we have reached a dead end where the proof has stopped
	// 		if (this.ghostNode.isActive()) {//} && !utils.branchComplete(this.proofState, previousBranch)) {
	// 			// and so we need to stop the execution
	// 			this.running = false;
	// 			if (this.autorunFlag) {
	// 				// mark proof as unfinished
	// 				if (this.root.getProofStatus() !== "untried") {
	// 					this.root.setProofStatus("unfinished");
	// 				}
	// 				// save and quit proof
	// 				await this.saveProof();
	// 				await this.quitProof();
	// 			}
	// 		}

	// 	} else {
	// 		this.running = false;
	// 		console.error("[proof-explorer] Error: could not read proof state information returned by pvs-server.");
	// 	}
	// }
	async onStepExecutedNew (desc: { proofState: SequentDescriptor, args?: PvsProofCommand, lastSequent: boolean }, opt?: { feedbackToTerminal?: boolean }): Promise<void> {
		if (desc && desc.proofState) {
			// get command and proof state
			let userCmd: string = desc.args ? desc.args.cmd : null; // command entered by the user
			const cmd: string = (typeof desc.proofState["prev-cmd"] === "string") ? desc.proofState["prev-cmd"] : userCmd;
			this.proofState = desc.proofState;
			
			// identify active node in the proof tree
			let activeNode: ProofItem = this.ghostNode.isActive() ? this.ghostNode.realNode : this.activeNode;

			//--- check meta-commands that will terminate the proof session: (QED), (quit)
			// if QED, update proof status and stop execution
			if (utils.QED(this.proofState)) {
				this.running = false;
				this.stopAt = null;

				// if cmd !== activeNode.name then the user has entered a command manually: we need to append a new node to the proof tree
				if (this.proofState.sequent && (utils.isSameCommand(activeNode.name, cmd) === false || this.ghostNode.isActive())) {
					// concatenate new command
					const elem: ProofCommand = new ProofCommand(cmd, activeNode.branchId, activeNode.parent, this.connection);
					// append before selected node (the active not has not been executed yet)
					if (activeNode.isActive()) {
						elem.sequentDescriptor = activeNode.sequentDescriptor;
						elem.updateTooltip({ internalAction: this.autorunFlag });
						// elem.setTooltip(activeNode.tooltip);
						activeNode.notVisited(); // this resets the tooltip in activeNode
						this.appendNode({ selected: activeNode, elem }, { beforeSelected: true, internalAction: this.autorunFlag });
					} else {
						elem.sequentDescriptor = (this.ghostNode.isActive()) ? this.ghostNode.sequentDescriptor : activeNode.sequentDescriptor;
						elem.updateTooltip({ internalAction: this.autorunFlag });
						// elem.setTooltip(utils.formatProofState(proofState));
						this.appendNode({ selected: activeNode, elem }, { internalAction: this.autorunFlag });
					}
					this.markAsActive({ selected: elem }); // this is necessary to correctly update the data structures in endNode --- elem will become the parent of endNode
					activeNode = this.activeNode; // update local variable because the following instructions are using it
				}

				// trim the node if necessary
				if (activeNode) {
					if (utils.isProofliteGlassbox(activeNode.name)) {
						this.deleteNode({ selected: activeNode });
					} else {
						this.trimNode({ selected: activeNode });
					}
				}

				// disable ghost node if necessary
				this.ghostNode.notActive();

				// mark proof as proved
				await this.proved();
				return;
			}
			// if command is quit, stop execution
			if (utils.isQuitCommand(cmd)) {
				this.running = false;
				this.stopAt = null;
				return;	
			}

			// identify previous and current (new) branch
			const previousBranchName: string = activeNode.branchId;
			const currentBranchName: string = utils.getBranchId(this.proofState.label);
			const previousBranch: ProofBranch = this.findProofBranch(previousBranchName);
			const currentBranch: ProofBranch = this.findProofBranch(currentBranchName);
			const branchCompleted: boolean = (previousBranch && !previousBranch.isComplete() && utils.branchComplete(this.proofState, this.formula.formulaName, previousBranchName))
					|| (currentBranch && !currentBranch.isComplete() && utils.branchComplete(this.proofState, this.formula.formulaName, currentBranchName));

			// const currentPath: string = this.proofState.path;
			// const pathHasChanged: boolean = utils.pathHasChanged({ newBranch: currentPath, previousBranch: this.previousPath });
			const pathHasChanged: boolean = utils.pathHasChanged({ newBranch: currentBranchName, previousBranch: previousBranchName }) && !branchCompleted;

			// show sequent in the terminal, if feedback was requested
			if (opt.feedbackToTerminal && !this.autorunFlag) {
				const channelID: string = utils.desc2id(this.formula);
				const evt: CliGatewayProofState = { type: "pvs.event.proof-state", channelID, data: this.proofState };
				this.pvsLanguageServer.cliGateway.publish(evt);
			}

			//--- check other meta-commands: (undo), (undo undo), (postpone), (show-hidden), (comment "..."), (help xxx)
			// if command is undo, go back to the last visited node
			if (utils.isUndoCommand(userCmd)) {
				this.running = false;
				this.stopAt = null;
				this.undoundoTarget = this.activeNode;
				this.saveTreeAttributes();
				if (utils.branchHasChanged({ newBranch: currentBranchName, previousBranch: previousBranchName })) {
					const targetBranch: ProofBranch = this.findProofBranch(currentBranchName);
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
					} else {
						console.error(`[proof-explorer] Error: could not find branch ${currentBranchName} in the proof tree`);
					}
				} else {
					this.moveIndicatorBack({ keepSameBranch: true });
				}
				return;
			}

			// if command is postpone, move to the new branch
			if (utils.isPostponeCommand(userCmd) || pathHasChanged) {
				// this.previousPath = currentPath;
				if (this.running && utils.isPostponeCommand(userCmd)) {
					this.running = false;
					this.stopAt = null;	
					if (this.autorunFlag) {
						// mark proof as unfinished
						if (this.root.getProofStatus() !== "untried") {
							this.root.setProofStatus("unfinished");
						}
						// save and quit proof
						await this.quitProofAndSave();
						// await this.quitProof();
						return;
					}
				}
				if (utils.branchHasChanged({ newBranch: currentBranchName, previousBranch: previousBranchName })) {
					if (this.ghostNode.isActive()) {
						this.ghostNode.notActive();
					} else {
						this.activeNode.notVisited();
					}
					const targetBranch: ProofBranch = this.findProofBranch(currentBranchName) || this.createBranchRecursive({ id: currentBranchName }, { internalAction: this.autorunFlag });
					if (targetBranch) {
						// before moving to the target branch, mark current branch as open (i.e., not visited)
						if (this.activeNode.contextValue !== "proof-command") {
						// 	this.activeNode.parent.notVisited();
						// } else {
							this.activeNode.notVisited();
						}
						// find the last visited child in the new branch
						const visitedChildren: ProofCommand[] = targetBranch.children.filter((elem: ProofItem) => {
							return elem.contextValue === "proof-command" && elem.isVisited();
						});
						const targetNode: ProofItem = (visitedChildren.length) ? visitedChildren[visitedChildren.length - 1] : targetBranch;
						// targetNode.pending();
						// mark the target node as active
						this.markAsActive({ selected: targetNode });
						// window.showInformationMessage(msg);
						this.moveIndicatorForward({ keepSameBranch: true, proofState: this.proofState });
					} else {
						console.error(`[proof-explorer] Error: could not find branch ${currentBranchName} in the proof tree. Proof Explorer is out of sync with PVS. Please restart the proof.`);
						this.running = false;
						this.stopAt = null;
						return;
					}
				} else {
					// same branch: we need to stop the execution, because this is the only branch left to be proved
					this.running = false;
					this.stopAt = null;
					return;
				}
			} else {
				// handle the special command (undo undo)
				if (utils.isUndoUndoCommand(userCmd)) {
					if (this.undoundoTarget) {
						let target: ProofItem = this.undoundoTarget;
						// the (undo undo) target is typically a visited node
						// it the target was active, then (undo) was performed when the ghost node was active and (undo undo) should make the ghost node visible
						if (this.undoundoTarget.isActive() || this.undoundoTarget.contextValue !== "proof-command") {
							this.ghostNode.parent = this.undoundoTarget.parent;
							this.ghostNode.realNode = this.undoundoTarget;
							target = this.ghostNode;
						}
						this.restoreTreeAttributes();
						this.markAsActive({ selected: target});
						this.undoundoTarget = null;
					} else {
						console.error(`[proof-explorer] Warning: unable to execute ${userCmd}`);
					}
					return;
				}

				// handle (show-hidden) and (comment "xxx")
				if (utils.isShowHiddenCommand(userCmd) || utils.isCommentCommand(userCmd)) {
					// nothing to do, the prover will simply show the hidden formulas
					return;
				}

				if (utils.isHelpCommand(userCmd)) {
					// do nothing, CLI will show the help message
					return;
				}

				//--- check special conditions: empty/null command, invalid command, no change before proceeding
				// if command is invalid command, stop execution and provide feedback to the user 
				if (utils.isUndoUndoPlusCommand(userCmd)) {
					// this.running = false;
					// vscode.commands.executeCommand('setContext', 'proof-explorer.running', false);
					if (this.autorunFlag) {
						// mark proof as unfinished
						if (this.root.getProofStatus() !== "untried") {
							this.root.setProofStatus("unfinished");
						}
						// save and quit proof
						await this.quitProofAndSave();
						// await this.quitProof();
					}
					// return;
				} else if (utils.isInvalidCommand(this.proofState)) {
					this.moveIndicatorForward({ keepSameBranch: true, proofState: this.proofState });
					// mark the sub tree of the invalid node as not visited
					activeNode.treeNotVisited();
				} else if (utils.noChange(this.proofState) || utils.isEmptyCommand(cmd)) {
					const command: string = utils.getNoChangeCommand(this.proofState);
					// check if the command that produced no change comes from the proof tree -- if so advance indicator
					if ((utils.isSameCommand(activeNode.name, command) || utils.isSameCommand(activeNode.name, cmd))
							&& !this.ghostNode.isActive()) {
						this.moveIndicatorForward({ keepSameBranch: true, proofState: this.proofState });
						// mark the sub tree of the invalid node as not visited
						activeNode.treeNotVisited();
					}
				} else {
					// regular prover command
					// else, the prover has made progress with the provided proof command
					// if cmd !== activeNode.name then the user has entered a command manually: we need to append a new node to the proof tree
					if (!utils.isSameCommand(activeNode.name, cmd) || this.ghostNode.isActive()) {
						if (!utils.isPropax(cmd)) {
							this.running = false;
							this.stopAt = null;
							if (this.autorunFlag && this.ghostNode.isActive()) {
								// mark proof as unfinished
								if (this.root.getProofStatus() !== "untried") {
									this.root.setProofStatus("unfinished");
								}
								// save and quit proof
								await this.quitProofAndSave();
								// await this.quitProof();
								return;
							}
						}
						// concatenate new command
						const elem: ProofCommand = new ProofCommand(cmd, activeNode.branchId, activeNode.parent, this.connection);
						// append before selected node (the active not has not been executed yet)
						if (activeNode.isActive()) {
							elem.sequentDescriptor = activeNode.sequentDescriptor;
							elem.updateTooltip({ internalAction: this.autorunFlag });
							activeNode.notVisited(); // this resets the tooltip in activeNode
							this.appendNode({ selected: activeNode, elem }, { beforeSelected: true });
						} else {
							elem.sequentDescriptor = (this.ghostNode.isActive()) ? this.ghostNode.sequentDescriptor : activeNode.sequentDescriptor;
							elem.updateTooltip({ internalAction: this.autorunFlag });
							this.appendNode({ selected: activeNode, elem });
						}
						this.markAsActive({ selected: elem }); // this is necessary to correctly update the data structures in endNode --- elem will become the parent of endNode
						activeNode = this.activeNode; // update local variable because the following instructions are using it
						// if the branch has changed, then we will be moving to a sub-goal --- we need to trim the node
						if (utils.branchHasChanged({ newBranch: currentBranchName, previousBranch: previousBranchName })) {
							this.trimNode({ selected: activeNode });
						}
					}

					// check if current or previous branch have been completed
					if (utils.branchComplete(this.proofState, this.formula.formulaName, previousBranchName)) {
						// PVS has automatically discharged the previous proof branch
						// trim the rest of the tree if necessary
						const selected: ProofBranch = this.findProofBranch(previousBranchName);
						this.removeNotVisited({ selected });
						selected.treeVisited();
						selected.treeComplete();
					}
					if (utils.branchComplete(this.proofState, this.formula.formulaName, currentBranchName)) {
						// PVS has automatically discharged the previous proof branch
						// trim the rest of the tree if necessary
						const selected: ProofBranch = this.findProofBranch(currentBranchName);
						this.removeNotVisited({ selected });
						selected.treeVisited();
						selected.treeComplete();
						selected.bubbleVisitedAndComplete();
					}

					// if the branch has changed, move to the new branch
					if (utils.branchHasChanged({ newBranch: currentBranchName, previousBranch: previousBranchName })) {
						// trim node if the number of subgoals has changed
						if (activeNode.children?.length !== this.proofState["num-subgoals"] && !this.autorunFlag) {
							this.trimNode({ selected: activeNode });
						}
						// find target branch
						const targetBranch: ProofItem = this.findProofBranch(currentBranchName) || this.createBranchRecursive({ id: currentBranchName }, { internalAction: this.autorunFlag });
						if (targetBranch) {
							// update tooltip in target branch
							targetBranch.sequentDescriptor = this.proofState;
							targetBranch.updateTooltip({ internalAction: this.autorunFlag });					
							// go to the new branch
							activeNode.visited();
							// activeNode.parent.visited();
							// find the last visited child in the new branch
							const visitedChildren: ProofCommand[] = targetBranch.children.filter((elem: ProofItem) => {
								return elem.contextValue === "proof-command" && elem.isVisited();
							});
							const targetNode: ProofItem = visitedChildren.length ? visitedChildren[visitedChildren.length - 1] : targetBranch;
							targetNode.pending();
							// mark target node as active
							this.markAsActive({ selected: targetNode });
						} else {
							console.error(`[proof-explorer] Error: could not find branch ${targetBranch} in the proof tree`); // this should never happen, because targetBranch is created if it doesn't exist already
						}
					} else {
						// if branch has not changed and the active node has subgoals, then the structure of the proof tree has changed -- we need to trim
						// NOTE: we don't want to prune in the case of autorun, otherwise part of the proof will be discarded permanently
						if (!this.autorunFlag && activeNode.children && activeNode.children.length) {
							this.trimNode({ selected: activeNode });
						}
					}

					// finally, move indicator forward and propagate tooltip to the new active node
					this.moveIndicatorForward({ keepSameBranch: true, proofState: this.proofState, branchComplete: branchCompleted });
				}
			}

			// this.previousPath = this.proofState.path || "";

			// check if we have reached a dead end where the proof has stopped
			if (this.ghostNode.isActive() && !branchCompleted && desc.lastSequent) {
				// and so we need to stop the execution
				this.running = false;
				if (this.autorunFlag) {
					// mark proof as unfinished
					if (this.root.getProofStatus() !== "untried") {
						this.root.setProofStatus("unfinished");
					}
					// save and quit proof
					await this.quitProofAndSave();
					// await this.quitProof();
				}
			}
		} else {
			this.running = false;
			console.error("[proof-explorer] Error: could not read proof state information returned by pvs-server.");
		}
	}

	/**
	 * Internal function, finds a given proof branch in the tree
	 * @param id Name of the proof branch. Branch names are specified using a dot notation (e.g., 1.3.2)
	 */
	protected findProofBranch (id: string): ProofBranch {
		if (id === "") {
			return this.root;
		}
		// else
		const findNodeAux = (id: string, node: ProofItem): ProofBranch | null => {
			if (node) {
				if (node.contextValue === "proof-branch" && node.branchNameEquals(id)) {
					return <ProofBranch> node;
				}
				for (let i = 0; i < node.children.length; i++) {
					const res: ProofItem = findNodeAux(id, node.children[i]);
					if (res) {
						return res;
					}
				}
			}
			return null;
		}
		return findNodeAux(id, this.root);
	}
	protected findNode (id: string): ProofBranch {
		const findNodeAux = (id: string, node: ProofItem): ProofBranch | null => {
			if (node && node.id === id) {
				return node;
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
	 * Internal function, finds first node not visited, starting to check from the given node
	 * @param id Name of the proof branch. Branch names are specified using a dot notation (e.g., 1.3.2)
	 */
	protected findFirstNotVisited (node: ProofItem): ProofItem {
		if (node && !node.isVisited() && !node.isPending()) {
			return node;
		}
		for (let i = 0; i < node.children.length; i++) {
			const res: ProofItem = this.findFirstNotVisited(node.children[i]);
			if (res) {
				return res;
			}
		}
		return null;
	}
	protected findLastVisitedOrActive (branch: ProofBranch): ProofItem {
		if (branch) {
			if (branch.children && branch.children.length) {
				const candidates: ProofItem[] = branch.children.filter(elem => {
					return elem.isVisited() || elem.isActive();
				});
				if (candidates && candidates.length) {
					return candidates[candidates.length - 1];
				}
			}
		}
		return null;
	}
	/**
	 * Internal function, creates a proof branch
	 * @param id Name of the proof branch. Branch names are specified using a dot notation (e.g., 1.3.2)
	 */
	protected createBranchRecursive (desc: { id: string }, opt?: { internalAction?: boolean }): ProofBranch | null {
		opt = opt || {};
		let branch: ProofBranch = null;
		if (desc && desc.id) {
			const depth: number = desc.id.split(".").length;
			let lastValidParent: ProofItem = this.findLastVisitedOrActive(this.root) || this.root;
			// navigate the proof tree from the root, and create the structure necessary to reach the target branch id
			for (let i = 0; i < depth; i++) {
				const branchId: string = desc.id.split(".").slice(0, i + 1).join(".");
				branch = this.findProofBranch(branchId);
				if (branch) {
					lastValidParent = this.findLastVisitedOrActive(branch) || branch;
				} else {
					// create branch
					branch = new ProofBranch(`(${branchId})`, branchId, lastValidParent, this.connection);
					// append branch
					this.appendBranch({ selected: lastValidParent, elem: branch }, { proofState: this.proofState, internalAction: opt.internalAction });
					lastValidParent = branch;
				}
			}
		} else {
			console.warn(`[proof-explorer] Warning: trying to create a new branch but branch descriptor was not provided`);
		}
		return branch;
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
		} else {
			console.warn(`[proof-explorer] Warning: could not to mark node as not visited`);
		}
	}
	/**
	 * Utility function, jumps to the selected node without executing it.
	 * @param desc Descriptor of the selected node
	 */
	jumpTo (desc: { selected: ProofItem }, opt?: { force?: boolean, restore?: boolean }): ProofItem {
		// if this is a branch, try to jump to the first child
		if (desc.selected.contextValue === "proof-branch" && desc.selected.children && desc.selected.children.length) {
			desc.selected = desc.selected.children[0];
		}
		return this.markAsActive(desc, opt);
	}
	/**
	 * Utility function, marks the selected node as active.
	 * @param desc Descriptor of the selected node
	 */
	protected markAsActive (desc: { selected: ProofItem }, opt?: { force?: boolean }): ProofItem {
		if (desc && desc.selected) {
			opt = opt || {};
			if (opt.force || desc.selected !== this.activeNode) {
				// update activeNode
				this.activeNode = desc.selected;
				this.activeNode.active();
				if (this.activeNode.contextValue !== "ghost") {
					// update ghostNode
					this.ghostNode.parent = this.activeNode.parent;
					this.ghostNode.realNode = this.activeNode;
					if (this.activeNode.contextValue === "proof-command") {
						this.ghostNode.notActive();
					} else {
						this.activeNode.pending();
						this.ghostNode.active();
					}
				}
			}
		} else {
			console.warn(`[proof-explorer] Warning: failed to set active node`);
		}
		return this.activeNode;
	}
	/**
	 * Internal function, marks a proof as dirty, indicating that the proof structure has changed and the proof needs re-saving and re-checking
	 */
	protected dirtyProof (): void {
		this.dirtyFlag = true;
		if (this.root && this.root.getProofStatus() === "proved") {
			this.root.setProofStatus("unchecked");
		}
	}
	/**
	 * Reveals a node in the view.
	 */
	protected revealNode (desc: { selected: ProofItem }): void {
		// TODO: send request to the client
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
	appendNode (desc: { selected: ProofItem, elem?: ProofItem | string }, opt?: { beforeSelected?: boolean, internalAction?: boolean }): ProofItem {
		if (desc && desc.selected) {
			this.dirtyProof();
			opt = opt || {};
			const selectedNode: ProofItem = (desc.selected.contextValue === "ghost") ? (<GhostNode> desc.selected).realNode : desc.selected;
			const branchId: string = selectedNode.branchId;
			let newNode: ProofItem = (typeof desc.elem === "string") ? 
				new ProofCommand(desc.elem, branchId, selectedNode.parent, this.connection)
					: desc.elem;
			if (newNode) {
				newNode.parent = selectedNode.parent;
				switch (selectedNode.contextValue) {
					case "root":
					case "proof-branch": {
						selectedNode.appendChildAtBeginning(newNode, opt);
						break;
					}
					case "ghost":
					case "proof-command": {
						selectedNode.appendSibling(newNode, opt);
						break;
					}
					default: {
						// do nothing
					}
				}
				// this.refreshView();
				return newNode;
			} else {
				console.warn(`[proof-explorer] Warning: failed to create new node`)
			}
		} else {
			console.warn(`[proof-explorer] Warning: failed to insert after (node is null)`)
		}
		return null;
	}
	appendNodeX (desc: ProofEditAppendNode, opt?: { beforeSelected?: boolean }): ProofItem {
		if (desc) {
			const selected: ProofItem = this.findNode(desc.selected.id);
			if (selected) {
				const elem: ProofItem = this.appendNode({ selected, elem: desc.name }, opt);
				return elem;
			}
		} else {
			console.warn(`[proof-explorer] Warning: failed to complete proof edit/append (node is null)`);
		}
		return null;
	}
	/**
	 * Appends a new branch to the proof tree. The branch name is automatically computed based on the structure of the proof tree.
	 * @param desc Descriptor of the node where the branch will be appended.
	 * 			   If the selected node is a branch, then the branch will be appended to the parent of the selected branch.
	 * @param opt Optionals: beforeSelected (boolean) flag used when the selected node is a branch, indicates whether the branch should be appended before the selected branch. 
	 */
	appendBranch (desc: { selected: ProofItem, elem?: ProofItem }, opt?: { beforeSelected?: boolean, firstBranch?: string, proofState?: SequentDescriptor, internalAction?: boolean }): boolean {
		if (desc && desc.selected) {
			this.dirtyProof();
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
						newBranch = new ProofBranch(branchId, branchId, selectedNode, this.connection);
						if (opt.proofState) {
							newBranch.sequentDescriptor = opt.proofState;
							newBranch.updateTooltip({ internalAction: this.autorunFlag });
							// newBranch.setTooltip(utils.formatProofState(opt.proofState));
						}
					}
					if (newBranch) {
						selectedNode.appendChild(newBranch);
					}
					break;
				}
				case "root":
				case "proof-branch": {
					const parent: ProofItem = selectedNode;
					if (parent && parent.children) {
						if (!newBranch) {
							const branchName: string = (opt.beforeSelected) ? `` : `${branchId}.${parent.children.length}`;
							newBranch = new ProofBranch(branchName, branchId, parent, this.connection);
						}
						if (newBranch) {
							newBranch.parent = parent;
							const children: ProofItem[] = [];
							const n: number = parent.children.length;
							let position: number = 0;
							for (let i = 0; i < n; i++) {
								if (!opt.beforeSelected) {
									children.push(parent.children[i]);
								}
								if (parent.children[i].id === selectedNode.id) {
									children.push(newBranch);
									position = i;
								}
								if (opt.beforeSelected) {
									children.push(parent.children[i]);
								}
							}
							parent.children = children;
							if (!opt.internalAction && this.connection) {
								const elem: ProofNodeX = newBranch.getNodeXStructure();
								this.log(`[proof-explorer] Appending branch ${elem.name} (${elem.id})`);
								const evt: ProofEditDidAppendNode = {
									action: "did-append-node",
									elem,
									position
								};
								if (this.connection) {
									this.connection.sendNotification(serverEvent.proverEvent, evt);
								}
							}					
						}
					}
				}
				default: {
					return false;
				}
			}
			return true;
		} else {
			console.warn(`[proof-explorer] Warning: failed to insert after (node is null)`);
		}
		return false;
	}
	appendBranchX (desc: ProofEditAppendBranch, opt?: { beforeSelected?: boolean, firstBranch?: string, proofState?: SequentDescriptor }): void {
		if (desc) {
			const selected: ProofItem = this.findNode(desc.selected.id);
			if (selected) { // the branch name is created automatically
				this.appendBranch({ selected }, opt);
				return;
			}
		}
		console.warn(`[proof-explorer] Warning: failed to complete proof edit/append (node is null)`);
	}
	/**
	 * Copies the selected node to the clipboard (i.e., the clipboard will store a copy of the selected node)
	 * @param desc Descriptor of the selected node.
	 */
	copyNode (desc: { selected: ProofItem }): boolean {
		if (desc && desc.selected) {
			this.clipboard = desc.selected.clone();
			this.clipboardTree = null;
			return true;
		} else {
			console.warn(`[proof-explorer] Warning: unable to copy selected node`);
		}
		return false;
	}
	copyNodeX (desc: ProofEditCopyNode): void {
		if (desc && desc.selected) {
			const selected: ProofItem = this.findNode(desc.selected.id);
			if (selected && this.copyNode({ selected })) {
				const evt: ProofEditDidCopyNode = { action: "did-copy-node", selected: desc.selected };
				if (this.connection && !this.autorunFlag) {
					this.connection.sendNotification(serverEvent.proverEvent, evt);
				}
				return;
			}
		}
		console.warn(`[proof-explorer] Warning: unable to copy selected node`);
	}
	/**
	 * Copies the tree rooted at the selected node to the clipboard, and all the siblings below the selected node
	 * (i.e., the clipboard will store a copy of the tree rooted at the selected node)
	 * @param desc Descriptor of the selected node.
	 */
	copyTree (desc: { selected: ProofItem }): string {
		if (desc && desc.selected) {
			if (desc.selected.contextValue !== "proof-command") {
				if (desc.selected && desc.selected.children && desc.selected.children.length) {
					desc.selected = desc.selected.children[0];
				}
			}
			const parent: ProofItem = desc.selected.parent;
			this.clipboard = null;
			this.clipboardTree = [];
			let seq: string = "";
			for (let i = parent.children.indexOf(desc.selected); i < parent.children.length; i++) {
				const item: ProofItem = parent.children[i];
				if (item) {
					this.clipboardTree.push(item.cloneTree());
					seq += item.printProofCommands();
				}
			}
			return seq;
		} else {
			console.warn(`[proof-explorer] Warning: unable to copy selected subtree`);
		}
		return null;
	}
	copyTreeX (desc: ProofEditCopyTree): void {
		if (desc && desc.selected) {
			const selected: ProofItem = this.findNode(desc.selected.id);
			if (selected) {
				const seq: string = this.copyTree({ selected });
				if (this.connection && seq && !this.autorunFlag) {
					const elems: ProofNodeX[] = this.clipboardTree.map(item => {
						return item.getNodeXStructure();
					});
					const evt: ProofEditDidCopyTree = { action: "did-copy-tree", selected: desc.selected, elems, clipboard: seq };
					this.connection.sendNotification(serverEvent.proverEvent, evt);
				}
				return;
			}
		}
		console.warn(`[proof-explorer] Warning: unable to copy selected node`);
	}
	/**
	 * Appends the node in the clipboard to the selected node. 
	 * @param desc Descriptor of the selected node where the content of the clipboard will be appended.
	 * @param opt Optionals parameters (see appendNode)
	 */
	pasteNode (desc: { selected: ProofItem }, opt?: { beforeSelected?: boolean }): boolean {
		if (desc && desc.selected) {
			opt = opt || {};
			if (this.clipboard) {
				this.appendNode({ selected: desc.selected, elem: this.clipboard.clone() }, opt);
				return true;
			}
			if (this.clipboardTree && this.clipboardTree.length) {
				// append just the first node from clipboardtree
				this.appendNode({ selected: desc.selected, elem: this.clipboardTree[0].cloneTree() });
				return true;
			} 
			//else
			console.warn(`[proof-explorer] Warning: unable to paste (clipboard is empty)`);
		} else {
			console.warn(`[proof-explorer] Warning: unable to paste (selected node is null)`);
		}
		return false;
	}
	pasteNodeX (desc: ProofEditPasteNode, opt?: { beforeSelected?: boolean }): void {
		if (desc && desc.selected) {
			const selected: ProofItem = this.findNode(desc.selected.id);
			if (selected) {
				this.pasteNode({ selected }, opt);
				return;
			}
		}
		console.warn(`[proof-explorer] Warning: unable to complete proof edit/paste (selected node is null)`);
	}
	/**
	 * Appends the tree in the clipboard to the selected node. 
	 * @param desc Descriptor of the selected node where the content of the clipboard will be appended.
	 * @param opt Optionals parameters (see appendNode)
	 */
	pasteTree (desc: { selected: ProofItem }, opt?: { beforeSelected?: boolean }): boolean {
		if (desc && desc.selected) {
			opt = opt || {};
			if (this.clipboardTree) {
				for (let i = 0; i < this.clipboardTree.length; i++) {
					this.appendNode({ selected: desc.selected, elem: this.clipboardTree[this.clipboardTree.length - i - 1].cloneTree() });
				}
				return true;
			} else {
				console.warn(`[proof-explorer] Warning: unable to paste (clipboard is empty)`);
			}
		} else {
			console.warn(`[proof-explorer] Warning: unable to paste (selected node is null)`);
		}
		return false;
	}
	pasteTreeX (desc: ProofEditPasteTree, opt?: { beforeSelected?: boolean }): void {
		if (desc && desc.selected) {
			const selected: ProofItem = this.findNode(desc.selected.id);
			if (selected) {
				this.pasteTree({ selected }, opt);
				return;
			}
		}
		console.warn(`[proof-explorer] Warning: unable to complete proof edit/paste (selected node is null)`);
	}
	/**
	 * Copies the selected node to the clipboard and then deletes the selected node. 
	 * Equivalent to copyNode + deleteNode. 
	 * @param desc Descriptor of the selected node.
	 */
	cutNode (desc: { selected: ProofItem }): boolean {
		if (desc && desc.selected) {
			this.copyNode(desc);
			this.deleteNode(desc);
			return true;
		} else {
			console.warn(`[proof-explorer] Warning: unable to cut node (selected node is null)`);
		}
		return false;
	}
	cutNodeX (desc: ProofEditCutNode): void {
		if (desc && desc.selected) {
			const selected: ProofItem = this.findNode(desc.selected.id);
			if (selected && this.cutNode({ selected })) {
				const evt: ProofEditDidCutNode = { action: "did-cut-node", selected: desc.selected };
				if (this.connection && !this.autorunFlag) {
					this.connection.sendNotification(serverEvent.proverEvent, evt);
				}
				return;
			}
		}
		console.warn(`[proof-explorer] Warning: unable to complete proof edit/paste (selected node is null)`);
	}
	/**
	 * Copies the tree rooted at the selected node to the clipboard and then deletes the selected node. 
	 * Equivalent to copyTree + deleteNode. 
	 * @param desc Descriptor of the selected node.
	 */
	cutTree (desc: { selected: ProofItem }): string {
		if (desc && desc.selected) {
			const seq: string = this.copyTree(desc);
			this.deleteTree(desc);
			return seq;
		} else {
			console.warn(`[proof-explorer] Warning: unable to cut selected subtree`);
		}
		return null;
	}
	cutTreeX (desc: ProofEditCutTree): void {
		if (desc && desc.selected) {
			const selected: ProofItem = this.findNode(desc.selected.id);
			if (selected) {
				const clipboard: string = this.cutTree({ selected });
				if (clipboard && this.clipboardTree && this.clipboardTree.length) {
					const elems: ProofNodeX[] = [];
					for (let i = 0; i < this.clipboardTree.length; i++) {
						elems.push(this.clipboardTree[i].getNodeXStructure());
					}
					const evt: ProofEditDidCutTree = { action: "did-cut-tree", selected: desc.selected, clipboard, elems };
					if (this.connection && !this.autorunFlag) {
						this.connection.sendNotification(serverEvent.proverEvent, evt);
					}
				}
				return;
			}
		}
		console.warn(`[proof-explorer] Warning: unable to complete proof edit/paste (selected node is null)`);
	}
	/**
	 * Deletes the subtree rooted at the selected node.
	 * A confirmation dialog is automatically displayed (unless optional parameters indicate not to show it) to ask confirmation of the operation.
	 * @param desc Descriptor of the selected node.
	 * @param opt Optionals parameters: confirm (boolean) indicates whether a confirmation dialog should be displayed before deleting the node.
	 */
	deleteTree (desc: { selected: ProofItem }): void {
		if (desc && desc.selected && desc.selected.parent) {
			const parent: ProofItem = desc.selected.parent;
			const len: number = parent.children.length - parent.children.indexOf(desc.selected);
			for (let i = 0; i < len; i++) {
				this.deleteNode({ selected: parent.children[parent.children.length - 1] });
			}
		} else {
			console.warn(`[proof-explorer] Warning: unable to delete selected node`);
		}
	}
	deleteTreeX (desc: ProofEditDeleteTree): void {
		if (desc && desc.selected) {
			const selected: ProofItem = this.findNode(desc.selected.id);
			if (selected) {
				return this.deleteTree({ selected });
			}
		}
		console.warn(`[proof-explorer] Warning: unable to complete proof edit/paste (selected node is null)`);
	}
	/**
	 * Deletes the selected node.
	 * A confirmation dialog is automatically displayed (unless optional parameters indicate not to show it) to ask confirmation of the operation.
	 * @param desc Descriptor of the selected node.
	 * @param opt Optionals parameters: confirm (boolean) indicates whether a confirmation dialog should be displayed before deleting the node.
	 */
	deleteNode (desc: { selected: ProofItem }): void {
		if (desc && desc.selected && desc.selected.parent) {
			const selected: ProofItem = desc.selected;
			this.dirtyProof();
			if (selected.contextValue === "root") {
				// delete children, don't deleted the root
				if (this.connection && !this.autorunFlag) {
					for (let i = 0; i < selected.children.length; i++) {
						this.log(`[proof-explorer] Deleting node ${selected.children[i].name} (${selected.children[i].id})`);
						const evt: ProofEditDidDeleteNode = {
							action: "did-delete-node", 
							selected: { id: selected.children[i].id, name: selected.children[i].name }
						};
						this.connection.sendNotification(serverEvent.proverEvent, evt);
					}
				}
				selected.children = [];
				this.markAsActive({ selected: this.root });
			} else {
				const activeGhost: ProofItem = (this.ghostNode && this.ghostNode.isActive()) ?
					this.ghostNode.realNode : null;
				const wasActive: boolean = selected.isActiveTree({ activeNode: activeGhost });
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
		} else {
			console.warn(`[proof-explorer] Warning: unable to delete selected node`);
		}
	}
	deleteNodeX (desc: ProofEditDeleteNode): void {
		if (desc && desc.selected) {
			const selected: ProofItem = this.findNode(desc.selected.id);
			if (selected) {
				return this.deleteNode({ selected });
			} 
		}
		console.warn(`[proof-explorer] Warning: unable to complete proof edit/paste (selected node is null)`);
	}
	/**
	 * Deletes the proof commands after the selected node.
	 * - If the node has children, deletes all children; 
	 * - If the node has lower siblings, deletes all lower siblings
	 * @param desc Descriptor indicating which node is selected in the proof tree
	 * @param opt Optionals: whether user confirmation is required
	 */
	trimNode (desc: { selected: ProofItem }): ProofItem[] | null {
		if (desc && desc.selected && desc.selected.parent) {
			const selected: ProofItem = desc.selected;
			let items: ProofItem[] = null;
			switch (selected.contextValue) {
				case "root": {
					items = selected.children;
					selected.children = [];
					// this.setActiveNode({ selected: this.root });
					this.markAsActive({ selected: this.root });
					break;
				}
				case "proof-branch": {
					// remove all children in this branch
					let childWasPendingOrActive: boolean = false;
					for (let i = 0; i < selected.children.length && !childWasPendingOrActive; i++) {
						if (selected.children[i].isPending() || selected.children[i].isActive()
								|| (selected.children[i] === this.ghostNode.realNode && this.ghostNode.isActive()) ) {
							childWasPendingOrActive = true;
						}
					}
					if (childWasPendingOrActive) {
						// mark the current branch as active
						// this.setActiveNode({ selected: node });
						this.markAsActive({ selected: selected });
					}
					items = selected.children;
					selected.children = [];
					break;
				}
				case "proof-command": {	
					// remove children if any
					items = selected.children;
					selected.children = [];
					// remove also lower siblings
					const parent: ProofItem = selected.parent;
					const idx: number = parent.children.indexOf(selected);
					items = items.concat(parent.children.slice(idx + 1, parent.children.length + 1));
					parent.children = parent.children.slice(0, idx + 1);
					if (selected.isVisited()) {
						this.activeNode = this.ghostNode;
						this.ghostNode.parent = selected.parent;
						this.ghostNode.realNode = selected;
						this.activeNode.active();
					}
					break;
				}
				default: {
					console.warn(`[proof-explorer] Warning: unrecognized node type ${selected.contextValue} detected while trimming ${selected.name}`);
				}
			}
			if (items && items.length) {
				this.dirtyProof();
				if (this.connection && !this.autorunFlag) {
					this.log(`[proof-explorer] Trimming node ${desc.selected.name} (${desc.selected.id})`);
					for (let i = 0; i < items.length; i++) {
						const evt: ProofEditDidDeleteNode = {
							action: "did-delete-node", 
							selected: { id: items[i].id, name: items[i].name }
						};
						this.connection.sendNotification(serverEvent.proverEvent, evt);
					}
					const evt: ProofEditDidTrimNode = { action: "did-trim-node", elems: items.map(item => {
						return item.getNodeXStructure();
					})};
					this.connection.sendNotification(serverEvent.proverEvent, evt);
				}
			}
			return items;
		} else {
			console.warn(`[proof-explorer] Warning: unable to delete selected node`);
		}
		return null;
	}
	trimNodeX (desc: ProofEditTrimNode): void {
		if (desc && desc.selected) {
			const selected: ProofItem = this.findNode(desc.selected.id);
			if (selected) {
				this.trimNode({ selected });
				return;
			}
		}
		console.warn(`[proof-explorer] Warning: unable to complete proof edit/paste (selected node is null)`);
	}

	/**
	 * Deletes proof commands that were not visited. This function is useful to clean up a proof script when a branch is closed or when the proof is QED.
	 * @param desc Descriptor indicating which node is the root of the subtree that needs to be cleaned up.
	 */
	removeNotVisited (desc: { selected: ProofItem }): void {
		if (desc && desc.selected && desc.selected.parent) {
			const node: ProofItem = desc.selected;
			if (node.children) {
				const n: number = node.children.length;
				for (let i = 0; i < n; i++) {
					this.removeNotVisited({ selected: node.children[n - 1 - i]});
				}
			}
			if (!node.isVisited() && !node.isActive() && (!node.children || node.children.length === 0)) {
				// remove node if not visited and not active and without children
				this.deleteNode({ selected: node });
			}
		} else {
			console.warn(`[proof-explorer] Warning: unable to delete selected node`);
		}
		return null;
	}
	removeNotVisitedX (desc: ProofEditTrimUnused): void {
		if (desc && desc.selected) {
			const selected: ProofItem = this.findNode(desc.selected.id);
			if (selected) {
				this.removeNotVisited({ selected });
				return;
			}
		}
		console.warn(`[proof-explorer] Warning: unable to complete proof edit/paste (selected node is null)`);
	}


	/**
	 * Renames the selected node. The new name is entered using a dialog window.
	 * @param desc Descriptor of the selected node.
	 */
	renameNode (desc: { selected: ProofItem, newName: string }): boolean {
		if (desc && desc.selected) {
			this.dirtyProof();
			const node: ProofItem = desc.selected;
			let newName: string = desc.newName;
			if (newName) {
				newName = newName.trim();
				if (!(newName.startsWith("(") && newName.endsWith(")"))) {
					newName = `(${newName})`;
				}
				node.rename(newName);
			}
			return true;
		} else {
			console.warn(`[proof-explorer] Warning: unable to edit selected node`);
		}
		return false;
	}
	renameNodeX (desc: ProofEditRenameNode): void {
		if (desc && desc.selected) {
			const selected: ProofItem = this.findNode(desc.selected.id);
			if (selected) {
				this.renameNode({ selected, newName: desc.newName });
				return;
			}
		}
		console.warn(`[proof-explorer] Warning: unable to complete proof edit/paste (selected node is null)`);
	}

	resetFlags (): void {
		this.autorunFlag = false;
	}

	// async autorunStop (): Promise<void> {
	// 	this.running = false;
	// 	// this.expectProofRequest = false;
	// 	this.pendingExecution = true;
	// 	if (this.autorunFlag) {
	// 		this.autorunFlag = false;
	// 		await this.quitProof();
	// 		this.autorunCallback((this.root) ? this.root.getProofStatus() : "untried");
	// 	}
	// }

	proofIsDirty (): boolean {
		return this.dirtyFlag;
	}

	/**
	 * Utility function, marks the current proof as proved
	 */
	async proved (): Promise<void> {
		// stop execution
		this.running = false;
		// move indicator forward so any proof branch that needs to be marked as visited will be marked
		this.activeNode.moveIndicatorForward();
		// remove unused commands
		this.removeNotVisited({ selected: this.root });
		// set QED
		this.root.QED();
		// save proof if status has changed
		const fname: string = fsUtils.desc2fname({
			fileName: this.formula.theoryName,
			fileExtension: ".prl",
			contextFolder: this.formula.contextFolder
		});
		const proofliteExists: boolean = await utils.containsProoflite(fname, this.formula.formulaName);
		if (this.root.proofStatusChanged() || this.dirtyFlag || !proofliteExists) {
			await this.quitProofAndSave(); // proof descriptor is automatically updated by saveproof
		} else {
			// update proof descriptor
			this.proofDescriptor = this.makeProofDescriptor();
		}

		// notify server mode change to the client
		this.pvsLanguageServer.notifyServerMode("lisp");

		// send QED to the terminal -- this will end the terminal session
		await this.proofCommandRequest({
			fileName: this.formula.fileName,
			fileExtension: this.formula.fileExtension,
			theoryName: this.formula.theoryName,
			formulaName: this.formula.formulaName,
			contextFolder: this.formula.contextFolder,
			cmd: "Q.E.D."
		});

		// run the other proofs if desc.autorun === true
		if (this.autorunFlag) {
			this.autorunFlag = false;
			this.autorunCallback(this.root.getProofStatus());
			this.root = null;
		}
	}

	/**
	 * Utility function, used to set the initial proof state.
	 * @param proofState 
	 */
	loadInitialSequent (proofState: SequentDescriptor): void {
		this.initialProofState = proofState;
		const evt: ProofExecDidLoadSequent = { action: "did-load-sequent", sequent: proofState };
		if (this.connection && !this.autorunFlag) {
			this.connection.sendNotification(serverEvent.proverEvent, evt);
		}
	}

	/**
	 * Utility function, activates the proof loaded in the view (i.e., sets the first node as active)
	 */
	startProof (opt?: { autorun?: boolean }): void {
		opt = opt || {};
		if (this.root) {
			this.autorunFlag = !!opt.autorun;
			this.autorunCallback = (status: ProofStatus) => {
				// the autorun call back is executed at the end of a proof re-run
				if (this.connection) {
					this.connection.sendRequest(serverEvent.autorunFormulaResponse, status);
					this.connection.sendRequest(serverEvent.serverModeUpdateEvent, { mode: "lisp" });
				}
			};

			this.root.sequentDescriptor = this.initialProofState;
			this.root.updateTooltip({ internalAction: this.autorunFlag });
			this.root.pending();
			this.root.setProofStatus(this.proofDescriptor.info.status);
			// select either the first child or the root if children are not present
			const selected: ProofItem = (this.root.children && this.root.children.length) ? this.root.children[0] : this.root;
			// initialise this.activeNode
			this.initActiveNode({ selected }); 
			// update the user interface
			this.markAsActive({ selected: this.activeNode }, { force: true });
			this.activeNode.sequentDescriptor = this.initialProofState;
			this.activeNode.updateTooltip({ internalAction: this.autorunFlag });
			this.dirtyFlag = false;
			this.running = false;

			// start the proof
			if (this.root.children && this.root.children.length) {
				// this.setActiveNode({ selected: this.root.children[0] });
				this.markAsActive({ selected: this.root.children[0] });
				// propagate tooltip
				this.activeNode.sequentDescriptor = this.root.sequentDescriptor;
				this.activeNode.tooltip = this.root.tooltip;	
			}
			
			if (this.autorunFlag) {
				this.run();
			} else {
				const evt: ProofExecDidStartProof = { action: "did-start-proof" };
				if (this.connection) {
					this.connection.sendNotification(serverEvent.proverEvent, evt);
				}
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
	 * Internal function, used by loadProofRequest and openProofRequest
	 * @param formula 
	 */
	protected async openProofFile (desc: PvsFile, formula: PvsFormula, opt?: { newProof?: boolean }): Promise<ProofDescriptor> {
		opt = opt || {};
		this.formula = formula;
		if (this.pvsProxy) {
			const pdesc: ProofDescriptor = (opt.newProof) ? await this.pvsProxy.newProof(formula) : await this.pvsProxy.openProofFile(desc, formula);
			if (pdesc) {
				// re-compute the shasum for the pvs file --- the shasum in the proof descriptor is from the last proof attempt, and it might be different if the file has been modified
				this.shasum = await fsUtils.shasumFile(formula);
				// load proof descriptor
				this.loadProofDescriptor(pdesc);
				// return the descriptor to the caller
				return pdesc;
			}
		}
		console.error(`[proof-explorer] Error: Could not load proof script (pvs-proxy is null)`);
		return null;
	}

	/**
	 * Loads the proof for a given formula
	 * @param desc 
	 */
	async loadProofRequest (desc: {
		fileName: string, 
		fileExtension: string, 
		contextFolder: string, 
		theoryName: string, 
		formulaName: string,
		proofFile?: FileDescriptor 
	}, opt?: { newProof?: boolean }): Promise<ProofDescriptor> {
		opt = opt || {};
		this.formula = desc;
		if (this.pvsProxy) {
			const fdesc: PvsFile = desc.proofFile || {
				fileName: desc.fileName,
				fileExtension: ".prf",
				contextFolder: desc.contextFolder
			};
			const pdesc: ProofDescriptor = (opt.newProof) ? await this.pvsProxy.newProof(desc) : await this.openProofFile(fdesc, desc, opt);
			// send feedback to the client
			const structure: ProofNodeX = this.root.getNodeXStructure();
			const evt: ProofExecDidLoadProof = { 
				action: "did-load-proof", 
				formula: this.formula, 
				desc: this.proofDescriptor,
				proof: structure
			};
			if (this.connection && !this.autorunFlag) {
				this.connection.sendNotification(serverEvent.proverEvent, evt);
			}
			return pdesc;
		}
		console.error(`[proof-explorer] Error: Could not load proof script (pvs-proxy is null)`);
		return null;
	}

	getFormula (): PvsFormula {
		return this.formula;
	}

	/**
	 * Internal function, used by loadProof to load the proof descriptor of a given formula in proof-explorer
	 * @param desc The proof descriptor to be loaded
	 */
	protected loadProofDescriptor (desc: ProofDescriptor): void {
		// utility function for building the proof tree
		const createTree = (elem: ProofNode, parent: ProofItem): void => {
			const node: ProofItem = (elem.type === "proof-command") ? 
				new ProofCommand(elem.name, elem.branch, parent, this.connection) 
				: new ProofBranch(elem.name, elem.branch, parent, this.connection);
			parent.appendChild(node, { internalAction: true });
			if (elem.rules && elem.rules.length) {
				elem.rules.forEach(child => {
					createTree(child, node);
				});
			}
		}
		// initialise
		this.root = null;
		if (desc && desc.info) {
			this.root = new RootNode({ 
				name: desc.info.formula, //(desc.proof) ? desc.proof.name : desc.info.formula, 
				proofStatus: desc.info.status,
				connection: this.connection
			});
			this.ghostNode = new GhostNode({ parent: this.root, node: this.root, connection: this.connection });
			if (desc.proofTree && desc.proofTree.rules && desc.proofTree.rules.length
					// when proof is simply (postpone), this is an empty proof, don't append postpone
					&& !(desc.proofTree.rules.length === 1 && utils.isPostponeCommand(desc.proofTree.rules[0].name))) {
				desc.proofTree.rules.forEach((child: ProofNode) => {
					createTree(child, this.root);
				});
			}
			this.proofDescriptor = desc;
		} else {
			console.warn(`[proof-explorer] Warning: null descriptor`);
		}
	}

	/**
	 * Internal function, builds a proof descriptor based on the structure of the current proof tree known to proof-explorer (i.e, this.root)
	 */
	protected makeProofDescriptor (): ProofDescriptor {
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
		const proofDescriptor: ProofDescriptor = new ProofDescriptor ({
			theory: this.formula.theoryName,
			formula: this.formula.formulaName,
			status: this.root.getProofStatus(),
			prover: (this.pvsProxy) ? utils.pvsVersionToString(this.pvsProxy.getPvsVersionInfo()) : "PVS 7.x",
			shasum: this.shasum
		}, proofTree);
		return proofDescriptor;
	}
	/**
	 * Loads a proof file in proof explorer
	 */
	async openProofRequest (desc: PvsFile, formula: PvsFormula): Promise<boolean> {
		if (desc && desc.fileName && desc.fileExtension && desc.contextFolder 
				&& formula && formula.fileName && formula.fileExtension 
				&& formula.theoryName && formula.formulaName) {
			// open proof descriptor
			const pdesc: ProofDescriptor = await this.pvsProxy.openProofFile(desc, formula);
			if (pdesc) {
				// save proof descriptor
				await utils.saveProofDescriptor(this.formula, pdesc, { saveProofTree: false });
				// load proof descriptor
				this.loadProofDescriptor(pdesc);
				// send feedback to the client
				const structure: ProofNodeX = this.root.getNodeXStructure();
				const evt: ProofExecDidOpenProof = { 
					action: "did-open-proof",
					proofFile: desc,
					formula: this.formula,
					desc: this.proofDescriptor,
					proof: structure
				};
				if (this.connection) {
					this.connection.sendNotification(serverEvent.proverEvent, evt);
				}
				// re-start proof in proof explorer
				this.startProof();
				return true;
			} else {
				const evt: ProofExecDidOpenProof = { 
					action: "did-open-proof",
					proofFile: desc,
					formula: this.formula,
					desc: this.proofDescriptor
				};
				this.connection?.sendNotification(serverEvent.proverEvent, evt);
			}
		}
		return false;
	}
	/**
	 * Imports the proof of a given formula in proof explorer
	 */
	async importProofRequest (desc: PvsFile, formula: PvsFormula): Promise<boolean> {
		if (desc && desc.fileName && desc.fileExtension && desc.contextFolder 
				&& formula && formula.fileName && formula.fileExtension 
				&& formula.theoryName && formula.formulaName) {
			// open proof descriptor
			const pdesc: ProofDescriptor = await this.pvsProxy.openProofFile(desc, formula);
			if (pdesc) {
				// update proof descriptor
				pdesc.info = this.proofDescriptor.info;
				pdesc.info.status = "untried";
				if (pdesc.proofTree) {
					pdesc.proofTree.name = pdesc.info.formula;
				}
				// save proof descriptor
				await utils.saveProofDescriptor(this.formula, pdesc, { saveProofTree: false });
				// load proof descriptor
				this.loadProofDescriptor(pdesc);
				// mark as dirty
				this.dirtyFlag = true;
				// send feedback to the client
				const structure: ProofNodeX = this.root.getNodeXStructure();
				const evt: ProofExecDidImportProof = { 
					action: "did-import-proof",
					proofFile: desc,
					formula: this.formula,
					importedFormula: formula,
					desc: this.proofDescriptor,
					proof: structure
				};
				this.connection?.sendNotification(serverEvent.proverEvent, evt);
				// re-start proof in proof explorer
				this.startProof();
				return true;
			} else {
				const evt: ProofExecDidImportProof = { 
					action: "did-import-proof",
					proofFile: desc,
					formula: this.formula,
					importedFormula: formula,
					desc: this.proofDescriptor
				};
				this.connection?.sendNotification(serverEvent.proverEvent, evt);
			}
		}
		return false;
	}
	/**
	 * Export the current proof to a given file format
	 * Only one supported format at the moment: .prl (prooflite)
	 */
	async exportProof (desc: { fileExtension: string }): Promise<void> {
		if (desc) {
			// update proof descriptor
			this.proofDescriptor = this.makeProofDescriptor();
			const proofFile: PvsFile = {
				fileName: this.formula.fileName,
				fileExtension: desc.fileExtension,
				contextFolder: this.formula.contextFolder
			}
			let success: boolean = false;
			let msg: string = null;
			switch (desc.fileExtension) {
				// case ".prf": {
				// 	const pvsResponse: PvsResponse = await this.pvsProxy.quitProofAndSave({ 
				// 		fileName: proofFile.fileName,
				// 		fileExtension: ".prf",
				// 		contextFolder: proofFile.contextFolder,
				// 		theoryName: this.formula.theoryName,
				// 		formulaName: this.formula.formulaName
				// 	});
				// 	if (pvsResponse) {
				// 		success = !(pvsResponse.error || (pvsResponse.result && typeof pvsResponse.result === "string" && pvsResponse.result.startsWith("Error:")));
				// 		msg = (typeof pvsResponse.result === "string") ? pvsResponse.result
				// 			: pvsResponse.error ? pvsResponse.error.error_string
				// 			: null;
				// 		if (success && formula && formula.formulaName && formula.theoryName
				// 				&& formula.fileExtension && formula.contextFolder && formula.fileName) {
				// 			const pvsVersionDescriptor: PvsVersionDescriptor = this.pvsProxy.getPvsVersionInfo();
				// 			const shasum: string = await fsUtils.shasumFile(formula);
				// 			const newDesc: ProofDescriptor = new ProofDescriptor({
				// 				theory: formula.theoryName,
				// 				formula: formula.formulaName,
				// 				shasum,
				// 				status: null,
				// 				prover: (pvsVersionDescriptor) ? pvsVersionDescriptor["pvs-version"] : null,
				// 				date: new Date().toISOString()
				// 			});
				// 			await utils.saveProofDescriptor(formula, newDesc);
				// 		}
				// 	}
				// 	break;
				// }
				case ".prl": {
					success = await this.pvsProxy.saveProoflite({ 
						fileName: proofFile.fileName,
						fileExtension: ".prl",
						contextFolder: proofFile.contextFolder,
						theoryName: this.formula.theoryName,
						formulaName: this.formula.formulaName,
						proofDescriptor: this.proofDescriptor
					});
					break;
				}
				default: {
					const msg: string = `Warning: unsupported export format ${desc.fileExtension}`;
					console.warn(`[proof-explorer] ${msg}`);
					this.connection.sendNotification("server.status.error", msg);
					return;
				}
			}
			const script: string = this.copyTree({ selected: this.root });
			this.connection.sendRequest(serverEvent.saveProofResponse, { 
				response: { 
					success,
					proofFile,
					msg,
					formula: this.formula,
					script
				}, 
				args: this.formula 
			});
		} else {
			this.connection.sendNotification("server.status.error", `Error: could not save proof (null descriptor)`);
		}
	}
	/**
	 * Quit proof and save the current proof in a .prf format
	 */
	async quitProofAndSave (opt?: { jprfOnly?: boolean }): Promise<{ success: boolean, msg?: string }> {
		opt = opt || {};
		const proofFile: PvsFile = {
			fileName: this.formula.fileName,
			fileExtension: ".prf",
			contextFolder: this.formula.contextFolder,
		}
		// update proof descriptor so it reflects the current proof structure
		this.proofDescriptor = this.makeProofDescriptor();
		await utils.saveProofDescriptor(this.formula, this.proofDescriptor, { saveProofTree: true });
		// save proof backup file -- just to be save in the case pvs hungs up and is unable to save
		const script: string = this.copyTree({ selected: this.root });
		let success: boolean = true;
		let msg: string = null;
		// quit proof
		await this.quitProof();
		if (!opt.jprfOnly) {
			// save proof descriptor to file
			const response: PvsResponse = await this.pvsProxy?.storeLastAttemptedProof(this.formula);
			success = !!(response?.result);
			if (success) {
				// clear dirty flag if proof saved successfully
				this.dirtyFlag = false;
			} else {
				msg = response?.error?.data?.error_string;
			}
		}
		// send feedback to the client
		this.connection?.sendRequest(serverEvent.saveProofResponse, {
			response: { 
				success,
				msg,
				proofFile,
				formula: this.formula,
				script
			}, 
			args: this.formula 
		});
		return { success, msg };
	}
	async inChecker(): Promise<boolean> {
		if (this.pvsProxy) {
			const response: PvsResponse = await this.pvsProxy.getProverStatus();
			return response && response.result && response.result === "active";
		}
		return false;
	}

	/**
	 * Quit the current proof
	 * @param opt Optionals: whether confirmation is necessary before quitting (default: confirmation is needed)  
	 */
	async quitProof (opt?: { notifyCliGateway?: boolean }): Promise<void> {
		opt = opt || {};
		this.running = false;
		const inchecker: boolean = await this.inChecker();
		if (this.formula && inchecker) {
			await this.proofCommand({
				fileName: this.formula.fileName,
				fileExtension: this.formula.fileExtension,
				theoryName: this.formula.theoryName,
				formulaName: this.formula.formulaName,
				contextFolder: this.formula.contextFolder,
				cmd: "quit"
			});
		}
		if (this.autorunFlag) {
			const status: ProofStatus = (this.root) ? this.root.getProofStatus() : "untried";
			this.autorunFlag = false;
			this.autorunCallback(status);
		}
		if (opt.notifyCliGateway) {
			const channelID: string = utils.desc2id(this.formula);
			const evt: CliGatewayQuit = { type: "pvs.event.quit", channelID };
			this.pvsLanguageServer.cliGateway.publish(evt);
		}
		this.connection?.sendRequest(serverEvent.serverModeUpdateEvent, { mode: "lisp" });
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
		return item.parent;
	}

	async interruptProofCommand (): Promise<PvsResponse> {
		if (this.pvsProxy && !this.interruptFlag) {
			this.interruptFlag = !this.autorunFlag;
			return await this.pvsProxy.interrupt();
		}
		return null;
	}
	
	async interruptAndQuitProver (): Promise<void> {
		await this.interruptProofCommand();
		await this.quitProof();
	}

	/**
	 * Send proof command
	 * @param args Handler arguments: filename, file extension, context folder, theory name, formula name, prover command
	 */
	async proofCommand (args: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string, cmd: string }): Promise<PvsResponse | null> {
		if (args) {
			args = fsUtils.decodeURIComponents(args);
			// const timeout: number = (this.connection) ? await this.connection.workspace.getConfiguration("pvs.pvsProver.watchdog") : 0;
			const useLispInterface: boolean = true;//!!(this.connection && await this.connection.workspace.getConfiguration("pvs.xperimental.developer.lispInterface"));

			const start: number = new Date().getTime();

			const response: PvsResponse = await this.pvsProxy.proofCommand({ cmd: args.cmd }, { useLispInterface });

			const ms: number = new Date().getTime() - start;
			if (this.connection) {
				this.connection.sendNotification(serverEvent.profilerData, `${ms}ms ${args.cmd}`);
			}

			return response;
		} else {
			console.error('[pvs-language-server] Error: proofCommand invoked with null descriptor');
		}
		return null;
	}
	async querySaveProof (formula: PvsFormula): Promise<void> {
		// ask if the proof needs to be saved
		if (this.connection) {
			this.connection.sendRequest(serverEvent.querySaveProof, { args: formula }); // this will trigger the confirmation dialog
			await Promise.resolve(new Promise((resolve, reject) => {
				this.connection.onRequest(serverEvent.querySaveProofResponse, async (response: ProofExecQuitAndSave | ProofExecQuit) => {
					if (response && response.action && response.action === "quit-proof-and-save") {
						await this.quitProofAndSave();
					}
					resolve();
				});
			}));
		}
	}
	// this handler is for commands entered by the user at the prover terminal
	async proofCommandRequest (request: PvsProofCommand): Promise<void> {
		request = fsUtils.decodeURIComponents(request);
		
		// handle meta-commands for saving and quitting
		// if (utils.isSaveCommand(request.cmd)) {
		// 	await this.quitProofAndSave();
		// 	return;
		// }
		if (utils.isSaveThenQuitCommand(request.cmd)) {
			await this.quitProofAndSave();
			// await this.quitProof();
			return;
		}
		if (utils.isQuitCommand(request.cmd)) {
			if (this.dirtyFlag) {
				// ask if the proof needs to be saved
				await this.querySaveProof(request)
			}
			await this.quitProof();
			return;
		}
		if (utils.isQuitDontSaveCommand(request.cmd)) {
			await this.quitProof();
			return;
		}
		if (utils.isFailCommand(request.cmd)) {
			if (this.activeNode.branchId === "") {
				// fail at the root sequent is equivalent to quit
				if (this.dirtyFlag) {
					// ask if the proof needs to be saved
					await this.querySaveProof(request)
				}
				await this.quitProof({ notifyCliGateway: true });
				return;	
			} else {
				// fail in a branch is equivalent to postpone
				request.cmd = "(postpone)";
			}
		}
		if (utils.isQEDCommand(request.cmd)) {
			// print QED in the terminal and close the terminal session
			const channelID: string = utils.desc2id(this.formula);
			const evt: CliGatewayQED = { type: "pvs.event.QED", channelID };
			this.pvsLanguageServer.cliGateway.publish(evt);
			
			if (this.connection) {
				this.connection.sendRequest(serverEvent.QED, { response: { result: request.cmd }, args: request });
				this.connection.sendRequest(serverEvent.serverModeUpdateEvent, { mode: "lisp" });
			}
			// trigger a context update, so proof status will be updated on the front-end
			const cdesc: PvsContextDescriptor = await this.pvsLanguageServer.getContextDescriptor({ contextFolder: request.contextFolder });
			if (this.connection) {
				this.connection.sendRequest(serverEvent.contextUpdate, cdesc);
			}
			// re-generate tccs
			await this.pvsLanguageServer.generateTccsRequest(request, { quiet: true });
			// stop the loop & don't send proof-state to cli gateway
			return;
		}

		// else, relay command to pvs-server
		const cmdArray: string[] = utils.splitCommands(request.cmd);
		if (cmdArray) {
			for (let i = 0; i < cmdArray.length; i++) {
				const cmd: string = cmdArray[i];
				// console.log(cmd);
				const req: PvsProofCommand = {
					fileName: request.fileName,
					fileExtension: request.fileExtension,
					contextFolder: request.contextFolder,
					theoryName: request.theoryName,
					formulaName: request.formulaName,
					cmd
				};
				const response: PvsResponse = await this.step({ cmd }); // await this.proofCommand(req); //
				// console.dir(response, { depth: null });
				if (response) {
					if (response.result) {
						const channelID: string = utils.desc2id(req);
						const result: SequentDescriptor[] = response.result;
						if (result.length) {
							const sequent: SequentDescriptor = result[result.length - 1];
							if (sequent["prover-session-status"]) {
								// FIXME: this field is provided only by json-output patch, not by the xmlrpc server -- either use it or don't, adopt a standard solution!
								// branch closed, or proof completed
								console.dir(sequent);
							} else {
								// FIXME: pvs-server needs to provide a string representation of the command, not its structure!
								const command: string = 
									(sequent && sequent["last-cmd"] 
										&& !utils.isUndoCommand(cmd)
										&& !utils.isUndoUndoCommand(cmd)
										&& !utils.isPostponeCommand(cmd)) ? sequent["last-cmd"] : cmd;
								if (this.connection) {
									this.connection.sendRequest(serverEvent.proofCommandResponse, { 
										response: { result: sequent }, 
										args: { 
											fileName: req.fileName,
											fileExtension: req.fileExtension,
											contextFolder: req.contextFolder,
											theoryName: req.theoryName,
											formulaName: req.formulaName,
											cmd: command
										}
									});
								}
								// check if the proof is complete
								if (utils.QED(sequent)) {
									if (this.connection) {
										this.connection.sendRequest(serverEvent.QED, { response: { result: sequent }, args: req });
										// trigger a context update, so proof status will be updated on the front-end
										const cdesc: PvsContextDescriptor = await this.pvsLanguageServer.getContextDescriptor({ contextFolder: req.contextFolder });
										this.connection.sendRequest(serverEvent.contextUpdate, cdesc);
									}
									// re-generate tccs
									await this.pvsLanguageServer.generateTccsRequest(req, { quiet: true });
									// stop the loop & don't send proof-state to cli gateway
									return;
								}
								// show feedback in CLI only after executing the last command in the sequence
								if (i === cmdArray.length - 1) {
									this.pvsLanguageServer.cliGateway.publish({ type: "pvs.event.proof-state", channelID, data: sequent, cmd });
								}
							}
						}	
					} else {
						this.pvsLanguageServer.pvsErrorManager.handleProofCommandError({ cmd, response: <PvsError> response });
					}
				}
			}
		}
	}	
}

export const QED: ProofStatus = "proved";

/**
 * Definition of tree items
 */
export class ProofItem extends TreeItem {
	contextValue: string = "proofItem";
	name: string; // prover command or branch id
	branchId: string = ""; // branch in the proof tree where this command is located (branchId for root is "").
	protected previousState: {
		tooltip?: string
	} = {};
	children: ProofItem[] = [];
	parent: ProofItem;
	protected activeFlag: boolean = false;
	protected completeFlag: boolean = false;

	protected visitedFlag: boolean = false;
	protected pendingFlag: boolean = false;
	protected noChangeFlag: boolean = false;
	protected prevFlags: {
		activeFlag: boolean,
		visitedFlag: boolean,
		pendingFlag: boolean
	} = {
		activeFlag: false,
		visitedFlag: false,
		pendingFlag: false
	};
	sequentDescriptor: SequentDescriptor = null; // sequent *before* the execution of the node

	isComplete(): boolean { return this.completeFlag; }

	constructor (type: string, name: string, branchId: string, parent: ProofItem, connection: Connection) {
		super(type, connection);
		this.contextValue = type;
		this.id = fsUtils.get_fresh_id();
		this.name = name;
		this.branchId = branchId;
		this.parent = parent;
		this.tooltip = "Double click sends command to terminal"; // the tooltip will shows the sequent before the execution of the proof command, as soon as the node becomes active
		this.notVisited({ internalAction: true });
	}
	readonly DEBUG_LOG: boolean = false;
	protected log (...args): void {
		if (this.DEBUG_LOG) {
			console.log(args);
		}
	}
	getNodeXStructure (): ProofNodeX {
		const res: ProofNodeX = {
			id: this.id,
			branch: this.branchId,
			name: this.name,
			type: <ProofNodeType> this.contextValue,
			rules: [],
			parent: this.parent.id
		};
		if (this.children) {
			for (let i = 0; i < this.children.length; i++) {
				const child: ProofNodeX = this.children[i].getNodeXStructure();
				res.rules.push({
					id: child.id,
					branch: child.branch,
					name: child.name,
					type: child.type,
					rules: child.rules,
					parent: this.id
				});
			}
		}
		return res;
	}
	branchNameEquals (b: string): boolean {
		// the following branch names should be equivalent: 1.2T.1 = 1.2.1 = 1.2.1T = 1.2T.1T 
		const b1x = this.branchId.split(".").map((elem: string) => {
			return `${parseInt(elem)}`
		}).join(".");
		const b2x = b.split(".").map((elem: string) => {
			return `${parseInt(elem)}`
		}).join(".");
		return b1x === b2x;
	}
	clone (opt?: { parent?: ProofItem, internalAction?: boolean }): ProofItem {
		opt = opt || {};
		switch (this.contextValue) {
			case "root": { return <RootNode> this.clone(opt); }
			case "proof-branch": { return <ProofBranch> this.clone(opt); }
			case "proof-command": { return <ProofCommand> this.clone(opt); }
			default: {
				console.warn(`[proof-explorer] Warning: trying to clone node type ${this.contextValue}`);
			}
		}
		return new ProofItem(this.contextValue, this.name, this.branchId, opt.parent, this.connection);
	}
	cloneTree (opt?: { parent?: ProofItem }): ProofItem {
		opt = opt || {};
		opt.parent = opt.parent || this.parent || null;
		const clonedRoot: ProofItem = this.clone(opt);
		if (this.children) {
			for (let i: number = 0; i < this.children.length; i++) {
				const child: ProofItem = this.children[i].cloneTree({ parent: clonedRoot });
				clonedRoot.insertChild(child, i, { internalAction: true });
			}
		}
		return clonedRoot;
	}
	saveAttributes (): void {
		this.prevFlags = {
			activeFlag: this.activeFlag,
			visitedFlag: this.visitedFlag,
			pendingFlag: this.pendingFlag
		};
	}
	restoreAttributes (): void {
		if (this.prevFlags.activeFlag) { return this.active(); }
		if (this.prevFlags.visitedFlag) { return this.visited(); }
		if (this.prevFlags.pendingFlag) { return this.pending(); }
	}
	updateTooltip (opt?: { internalAction?: boolean }): void {
		opt = opt || {};
		this.tooltip = (this.sequentDescriptor) ? utils.formatSequent(this.sequentDescriptor) : " ";
		if (!opt.internalAction && this.connection) {
			const evt: ProofExecDidUpdateSequent = {
				action: "did-update-sequent", 
				sequent: this.sequentDescriptor, 
				selected: { id: this.id, name: this.name } 
			};
			this.connection.sendNotification(serverEvent.proverEvent, evt);
		}
	}
	rename (newName: string, opt?: { internalAction?: boolean }): void {
		opt = opt || {};
		const oldName: string = this.name;
		this.name = newName;
		if (!opt.internalAction && this.connection) {
			const evt: ProofEditDidRenameNode = {
				action: "did-rename-node", 
				selected: { id: this.id, name: oldName }, 
				newName: newName
			};
			this.connection.sendNotification(serverEvent.proverEvent, evt);
		}
	}
	pending (): void {
		this.activeFlag = false;
		this.visitedFlag = false;
		this.pendingFlag = true;
		this.noChangeFlag = false;
		if (this.connection) {
			this.connection.sendNotification(serverEvent.proofNodeUpdate, {
				id: this.id,
				name: this.name,
				status: "pending"
			});
		}
	}
	visited (): void {
		this.previousState.tooltip = this.tooltip;
		this.activeFlag = false;
		this.visitedFlag = true;
		this.pendingFlag = false;
		this.noChangeFlag = false;
		if (this.connection) {
			this.connection.sendNotification(serverEvent.proofNodeUpdate, {
				id: this.id,
				name: this.name,
				status: "visited"
			});
		}
	}
	notVisited (opt?: { internalAction?: boolean }): void {
		this.previousState.tooltip = this.tooltip;
		this.activeFlag = false;
		this.visitedFlag = false;
		this.pendingFlag = false;
		this.noChangeFlag = false;
		this.completeFlag = false;
		opt = opt || {};
		if (!opt.internalAction && this.connection) {
			this.connection.sendNotification(serverEvent.proofNodeUpdate, {
				id: this.id,
				name: this.name,
				status: "not-visited"
			});
		}
	}
	complete(): void {
		this.completeFlag = true;
		this.connection?.sendNotification(serverEvent.proofNodeUpdate, {
			id: this.id,
			name: this.name,
			status: "complete"
		});
	}
	notComplete(): void {
		this.completeFlag = false;
		this.connection?.sendNotification(serverEvent.proofNodeUpdate, {
			id: this.id,
			name: this.name,
			status: "not-complete"
		});
	}
	// noChange (): void {
	// 	this.previousState.tooltip = this.tooltip;
	// 	this.activeFlag = false;
	// 	this.visitedFlag = false;
	// 	this.pendingFlag = false;
	// 	this.noChangeFlag = true;
	// }
	active (): void {
		this.activeFlag = true;
		this.visitedFlag = false;
		this.pendingFlag = false;
		this.noChangeFlag = false;
		if (this.connection) {
			this.connection.sendNotification(serverEvent.proofNodeUpdate, {
				id: this.id,
				name: this.name,
				status: "active"
			});
		}
	}
	treeNotVisited (): void {
		this.notVisited();
		if (this.children) {
			for (let i = 0; i < this.children.length; i++) {
				this.children[i].treeNotVisited();
			}
		}
	}
	treeVisited (): void {
		this.visited();
		if (this.children) {
			for (let i = 0; i < this.children.length; i++) {
				this.children[i].treeVisited();
			}
		}
	}
	treeComplete (): void {
		this.complete();
		if (this.children) {
			for (let i = 0; i < this.children.length; i++) {
				this.children[i].treeComplete();
			}
		}
	}
	checkChildrenComplete (): boolean {
		if (this.children && this.children.length) {
			const open: ProofItem[] = this.children.filter(item => {
				return !item.isVisited();
			});
			return open.length === 0;
		}
		return true;
	}
	bubbleVisitedAndComplete (): void {
		if (this.contextValue !== "root" && this.parent && this.parent.checkChildrenComplete()) {
			// the parent branch is also complete
			this.parent.visited();
			this.parent.treeComplete();
			this.parent.bubbleVisitedAndComplete();
		}

	}
	isActive (): boolean {
		return this.activeFlag;
	}
	isActiveTree (opt?: { activeNode?: ProofItem }): boolean {
		opt = opt || {};
		let active: boolean = this.isActive();
		if (!active && opt && opt.activeNode) {
			active = this.id === opt.activeNode.id;
		}
		if (!active && this.children && this.children.length) {
			for (let i = 0; i < this.children.length && !active; i++) {
				active = this.children[i].isActiveTree(opt);
			}
		}
		return active;
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

		// this.label = `${this.previousState.icon}${this.name}`;
		// this.tooltip = this.previousState.tooltip;
	}
	restoreTreeAttributes (): void {
		this.restoreAttributes();
		if (this.children && this.children.length) {
			for (let i = 0; i < this.children.length; i++) {
				this.children[i].restoreTreeAttributes();
			}
		}
	}
	saveTreeAttributes (): void {
		this.saveAttributes();
		if (this.children && this.children.length) {
			for (let i = 0; i < this.children.length; i++) {
				this.children[i].saveTreeAttributes();
			}
		}
	}
	setChildren (children: ProofItem[]): void {
		this.children = children;
	}
	insertChild (child: ProofItem, position: number, opt?: { internalAction?: boolean }): void {
		if (this.children && position < this.children.length - 1) {
			const children1: ProofItem[] = this.children.slice(0, position);
			const children2: ProofItem[] = this.children.slice(position);
			this.children = children1.concat([ child ]).concat(children2);
		} else {
			// append at the end
			position = this.children.length;
			this.children = this.children.concat([ child ]);
		}
		if (!opt.internalAction && this.connection) {
			const elem: ProofNodeX = child.getNodeXStructure();
			this.log(`[proof-explorer] Appending node ${elem.name} (${elem.id})`);
			const evt: ProofEditDidAppendNode = {
				action: "did-append-node",
				elem,
				position
			};
			if (this.connection) {
				this.connection.sendNotification(serverEvent.proverEvent, evt);
			}
		}
	}
	deleteChild (child: ProofItem, opt?: { internalAction?: boolean }): void {
		opt = opt || {};
		this.children = this.children.filter((ch: ProofItem) => {
			return ch.id !== child.id;
		});
		if (!opt.internalAction && this.connection) {
			this.log(`[proof-explorer] Deleting node ${child.name} (${child.id})`);
			const evt: ProofEditDidDeleteNode = {
				action: "did-delete-node",
				selected: { id: child.id, name: child.name }
			};
			if (this.connection) {
				this.connection.sendNotification(serverEvent.proverEvent, evt);
			}
		}
	}
	deleteTree (child: ProofItem): void {
		const children: ProofItem[] = [];
		for (let i in this.children) {
			if (this.children[i].id !== child.id) {
				children.push(this.children[i]);
			} else {
				this.children = children;
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
	moveIndicatorForward (opt?: { lastVisitedChild?: ProofItem, keepSameBranch?: boolean, proofState?: SequentDescriptor }): ProofItem | null {
		opt = opt || {};
		if (this.contextValue === "proof-command") {
			this.visited();
		} else {
			// proof branch or root
			this.pending();
		}
		// check if this node has children, if so, return the first non-visited child
		const proofState: SequentDescriptor = opt.proofState || this.sequentDescriptor;
		if (this.children && this.children.length) {
			if (!proofState) {
				console.error(`[pvs-explorer] Error: proofstate is null. Please restart the proof and report this error to the vscode-pvs developers.`);
			}
			const activeBranchId: string = utils.getBranchId(proofState.label);
			if (opt.keepSameBranch && !this.children[0].branchNameEquals(activeBranchId)) {
				return null;
			}
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
			return this.parent.moveIndicatorForward({ lastVisitedChild: this, proofState: this.sequentDescriptor });
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
	getPreviousSibling (): ProofItem | null {
		if (this.parent) {
			const children: ProofItem[] = this.parent.children;
			if (children && children.length > 1) {
				const index: number = children.indexOf(this);
				const prev: number = index - 1;
				if (prev >= 0) {
					return children[prev];
				}
			}
		}
		return null;
	}
	appendSibling (sib: ProofItem, opt?: { beforeSelected?: boolean, internalAction?: boolean }): void {
		let children: ProofItem[] = [];
		const n: number = this.parent.children.length;

		opt = opt || {};
		for (let i = 0; i < n; i++) {
			if (!opt.beforeSelected) {
				children.push(this.parent.children[i]);
			}
			if (this.parent.children[i].id === this.id) {
				if (sib.contextValue === "root") { // if the node to be appended is a root node, we append its children
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

		if (!opt.internalAction && this.connection) {
			const elem: ProofNodeX = sib.getNodeXStructure();
			this.log(`[proof-explorer] Appending node ${elem.name} (${elem.id})`);
			const evt: ProofEditDidAppendNode = {
				action: "did-append-node",
				elem,
				position: this.parent.children.indexOf(sib)
			};
			if (this.connection) {
				this.connection.sendNotification(serverEvent.proverEvent, evt);
			}
		}
	}
	appendChildAtBeginning (child: ProofItem, opt?: { internalAction?: boolean }): void {
		opt = opt || {};
		this.children = this.children || [];
		child.parent = this;
		if (child.contextValue === "root") {
			this.children = child.children.concat(this.children);
		} else {
			this.children = [ child ].concat(this.children);
		}

		if (!opt.internalAction && this.connection) {
			const elem: ProofNodeX = child.getNodeXStructure();
			this.log(`[proof-explorer] Appending node ${elem.name} (${elem.id})`);
			const evt: ProofEditDidAppendNode = {
				action: "did-append-node",
				elem,
				position: this.children.indexOf(child) // this should be 0
			};
			if (this.connection) {
				this.connection.sendNotification(serverEvent.proverEvent, evt);
			}
		}
	}
	appendChild (child: ProofItem, opt?: { internalAction?: boolean }): void {
		opt = opt || {};
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

		if (!opt.internalAction && this.connection) {
			const elem: ProofNodeX = child.getNodeXStructure();
			this.log(`[proof-explorer] Appending node ${elem.name} (${elem.id})`);
			const evt: ProofEditDidAppendNode = {
				action: "did-append-node",
				elem,
				position: this.children.indexOf(child)
			};
			if (this.connection) {
				this.connection.sendNotification(serverEvent.proverEvent, evt);
			}
		}
	}
	getChildren (): ProofItem[] {
		return this.children;
	}
	printProofCommands (): string | null {
		let ans: string = (this.contextValue === "proof-command") ? this.name : "";
		if (this.children && this.children.length) {
			for (let i = 0; i < this.children.length; i++) {
				ans += this.children[i].printProofCommands();
			}
		}
		return ans;
	}
	
}

class ProofCommand extends ProofItem {
	constructor (cmd: string, branchId: string, parent: ProofItem, connection: Connection) {
		super("proof-command", cmd, branchId, parent, connection);
		cmd = cmd.trim();
		this.name = (cmd && cmd.startsWith("(") && cmd.endsWith(")")) || utils.isUndoCommand(cmd) ? cmd : `(${cmd})`;
		this.notVisited({ internalAction: true });
	}
	// @override
	clone (opt?: { parent?: ProofItem }): ProofCommand {
		opt = opt || {};
		const parent: ProofCommand =  opt.parent || this.parent;
		const c: ProofCommand = new ProofCommand(this.name, this.branchId, parent, this.connection);
		return c;
	}
}
class ProofBranch extends ProofItem {
	constructor (cmd: string, branchId: string, parent: ProofItem, connection: Connection) {
		super("proof-branch", cmd, branchId, parent, connection);
		this.name = `(${branchId})`;
		this.notVisited({ internalAction: true });
	}
	// @override
	clone (opt?: { parent?: ProofCommand }): ProofBranch {
		opt = opt || {};
		const parent: ProofCommand =  opt.parent || this.parent;
		const c: ProofBranch = new ProofBranch(this.name, this.branchId, parent, this.connection);
		return c;
	}
}
class WelcomeScreen extends TreeItem {
	constructor (connection: Connection) {
		super("welcome-screen", connection);
	}
}
class RootNode extends ProofItem {
	protected proofStatus: ProofStatus; // this is updated while running the proof
	initialProofStatus: ProofStatus; // this is set at the beginning (and at the end of the proof attempt if the proof succeeds)
	constructor (desc: { name: string, connection: Connection, proofStatus?: ProofStatus }) {
		super("root", desc.name, "", null, desc.connection);
		this.parent = this; // the parent of the root is the root itself
		this.proofStatus = desc.proofStatus || "untried"
		this.initialProofStatus = this.proofStatus;
		this.notVisited({ internalAction: true });
	}
	// @overrides
	clone (): RootNode {
		const c: RootNode = new RootNode({ name: this.name, proofStatus: this.proofStatus, connection: this.connection });
		c.pending();
		return c;
	}

	// @overrides
	visited (): void {
		// do nothing
	}
	// @overrides
	pending (): void {
		if (this.proofStatus === "untried") {
			this.proofStatus = "unfinished";
			const evt: ProofEditDidUpdateProofStatus = {
				action: "did-update-proof-status",
				proofStatus: this.proofStatus
			};
			if (this.connection) {
				this.connection.sendNotification(serverEvent.proverEvent, evt);
			}
		}
		if (this.proofStatus !== "proved") {
			super.pending();
		}
	}
	QED (): void {
		super.treeVisited();
		this.setProofStatus(QED);
	}
	proofStatusChanged (): boolean {
		return this.initialProofStatus !== this.proofStatus;
	}
	setProofStatus (proofStatus: ProofStatus): void {
		if (proofStatus) {
			this.proofStatus = proofStatus;
			const evt: ProofEditDidUpdateProofStatus = {
				action: "did-update-proof-status",
				proofStatus: this.proofStatus
			};
			if (this.connection) {
				this.connection.sendNotification(serverEvent.proverEvent, evt);
			}
		}
	}
	resetProofStatus (): void {
		this.setProofStatus(this.initialProofStatus);
	}
	getProofStatus (): ProofStatus {
		return this.proofStatus;
	}
}
class GhostNode extends ProofItem {
	realNode: ProofItem;
	constructor (desc: { parent: ProofItem, node: ProofItem, connection: Connection }) {
		super("ghost", "ghost", "", desc.parent, desc.connection);
		this.realNode = desc.node;
	}
	// @overrides
	active (): void {
		this.activeFlag = true;
		if (this.connection) {
			if (this.realNode) {
				this.log(`[proof-explorer] Activating cursor after node ${this.realNode.name} (${this.realNode.id})`);
				const cursor: ProofNodeX = this.getNodeXStructure();
				cursor.parent = this.realNode.id;
				const evt: ProofEditDidActivateCursor = {
					action: "did-activate-cursor",
					cursor
				};
				if (this.connection) {
					this.connection.sendNotification(serverEvent.proverEvent, evt);
				}
			} else {
				console.warn(`[proof-explorer] Warning: trying to activate cursor node when realNode is null`);
			}
		}
	}
	notActive (): void {
		this.activeFlag = false;
		if (this.connection) {
			if (this.realNode) {
				this.log(`[proof-explorer] Activating cursor after node ${this.realNode.name} (${this.realNode.id})`);
				const evt: ProofEditDidDeactivateCursor = {
					action: "did-deactivate-cursor"
				};
				if (this.connection) {
					this.connection.sendNotification(serverEvent.proverEvent, evt);
				}
			} else {
				console.warn(`[proof-explorer] Warning: trying to activate cursor node when realNode is null`);
			}
		}
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

