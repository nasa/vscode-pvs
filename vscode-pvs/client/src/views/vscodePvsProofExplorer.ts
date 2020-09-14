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
	TreeItem, Command, EventEmitter, Event, TreeDataProvider, TreeView
} from 'vscode';
import { LanguageClient } from 'vscode-languageclient';
import { 
	ProofNode, serverRequest, PvsVersionDescriptor, ProofDescriptor, ProofStatus, 
	serverEvent, PvsFormula, ProofNodeX, ProofNodeStatus, ProofEditCopyNode, 
	ProofEditDidCopyNode, ProofEditEvent, ProofEditDidAppendNode, ProofEditPasteNode, 
	ProofExecForward, ProofExecBack, ProofExecFastForward, ProofEditSave, ProofExecRun, 
	ProofExecQuit, ProofEditCopyTree, ProofEditDidCopyTree, ProofEditPasteTree, 
	ProofEditDeleteNode, ProofEditTrimNode, ProofEditDeleteTree, ProofEditCutTree, 
	ProofEditCutNode, ProofEditAppendNode, ProofEditAppendBranch, ProofEditRenameNode, ProofEditDidTrimNode, ProofEditDidDeleteNode, ProofEditDidCutNode, ProofEditDidCutTree, ProofEditDidPasteTree, PvsProofCommand, ProofEditDidRenameNode, ProofEditDidActivateCursor, ProofEditDidDeactivateCursor, ProofEditDidUpdateProofStatus, ProofExecDidUpdateSequent, ProofEditTrimUnused, ServerMode, ProofEditSaveAs, ProofExecOpenProof, PvsFile, ProofExecStartNewProof 
} from '../common/serverInterface';
import * as utils from '../common/languageUtils';
import * as fsUtils from '../common/fsUtils';
import { SequentDescriptor } from '../common/languageUtils';
import * as vscode from 'vscode';
import * as vscodeUtils from '../utils/vscode-utils';
import * as path from 'path';

/**
 * TreeData provider for Proof Explorer
 */
export class VSCodePvsProofExplorer implements TreeDataProvider<TreeItem> {
	protected pvsVersionDescriptor: PvsVersionDescriptor;

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
	protected serverMode: ServerMode = "lisp";

	/**
	 * Flag indicating whether the view is enabled
	 */
	protected enabled: boolean = false;

	/**
	 * Information on the formula loaded in proof explorer
	 **/
	protected formula: PvsFormula;
	
	protected autorunFlag: boolean = false;
	protected autorunCallback: (status: ProofStatus) => void;

	protected filterOnTypeActive: boolean = false;

	/**
	 * Name of the view associated with the data provider
	 */
	protected providerView: string;
	protected view: TreeView<TreeItem>;

	/**
	 * Attributes for run-time management of the proof tree rendered in the view
	 */
	protected welcome: WelcomeScreen = new WelcomeScreen();
	protected loading: LoadingItem = new LoadingItem();
	protected root: RootNode = null // the root of the tree
	protected ghostNode: GhostNode = null; // this is a floating node that follows activeNode. It is used during proof development, to signpost where the next proof command will be appended in the proof tree
	protected activeNode: ProofCommand | ProofBranch | GhostNode = null;

	protected running: boolean = false; // status flag, indicates whether we are running all proof commands, as opposed to stepping through the proof commands

	/**
	 * JSON representation of the proof script for the current proof.
	 * The representation is updated at the beginning of the proof session.
	 */
	protected proofDescriptor: ProofDescriptor;

	/**
	 * Current proof state
	 */
	protected proofState: SequentDescriptor;

	
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

	didUpdateServerMode (mode: ServerMode): void {
		this.serverMode = mode;
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
			vscode.commands.executeCommand('setContext', 'proof-explorer.running', true);
			// this.step();
		}
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
	 * Internal function, reveals a node in the view.
	 */
	protected revealNode (desc: { id: string, name: string }): void {
		if (desc && desc.id) {
			// there is something I don't understand in the APIs of TreeItem 
			// because I'm getting exceptions (node not found / element already registered)
			// when option 'select' is set to true.
			// Sometimes the exception occurs also with option 'expand'
			// if (desc.selected.isActive() === false) {
				let selected: ProofItem = this.findNode(desc.id);
				if (!selected && this.ghostNode.isActive()) {
					selected = this.ghostNode;
					this.ghostNode.parent = this.ghostNode.parent || this.ghostNode.realNode;
				}
				if (selected && selected.parent) {
					this.view.reveal(selected, { expand: 2, select: true, focus: false }).then(() => {
					}, (error: any) => {
						// console.error(selected);
						// console.error(error);
					});
				}
			// }
		}
	}
	protected focusNode (desc: { id: string, name: string }): void {
		if (desc && desc.id) {
			let selected: ProofItem = this.findNode(desc.id);
			if (!selected && this.ghostNode.isActive()) {
				selected = this.ghostNode;
				this.ghostNode.parent = this.ghostNode.parent || this.ghostNode.realNode;
			}
			if (selected && selected.parent) {
				this.view.reveal(selected, { expand: 2, select: true, focus: true }).then(() => {
				}, (error: any) => {
					// console.error(selected);
					// console.error(error);
				});
			}
		}
	}
	/**
	 * Copies the selected node to the clipboard (i.e., the clipboard will store a copy of the selected node)
	 * @param desc Descriptor of the selected node.
	 */
	didCopyNode (desc: ProofEditDidCopyNode): void {
		if (desc && desc.selected) {
			// copy node to system clipboard
			vscode.env.clipboard.writeText(desc.selected.name);
			// set vscode context variable proof-explorer.clipboard-contains-node to true
			vscode.commands.executeCommand('setContext', 'proof-explorer.clipboard-contains-node', true);
			// show feedback
			window.showInformationMessage(`${desc.selected.name} copied to clipboard`);
		}
	}
	/**
	 * Copies the tree rooted at the selected node to the clipboard, and all the siblings below the selected node
	 * (i.e., the clipboard will store a copy of the tree rooted at the selected node)
	 * @param desc Descriptor of the selected node.
	 */
	didCopyTree (desc: ProofEditDidCopyTree): void {
		if (desc && desc.selected) {
			// copy node to system clipboard
			vscode.env.clipboard.writeText(desc.clipboard);
			// set vscode context variable proof-explorer.clipboard-contains-tree and clipboard-contains-node to true
			commands.executeCommand('setContext', 'proof-explorer.clipboard-contains-tree', true);
			commands.executeCommand('setContext', 'proof-explorer.clipboard-contains-node', true);
			// show feedback
			window.showInformationMessage(`Subtree rooted in ${desc.selected.name} copied to clipboard`);
		}
	}

	didCutNode (desc: ProofEditDidCutNode): void {
		if (desc && desc.selected) {
			// copy node to system clipboard
			vscode.env.clipboard.writeText(desc.selected.name);
			// set vscode context variable proof-explorer.clipboard-contains-tree and clipboard-contains-node to true
			commands.executeCommand('setContext', 'proof-explorer.clipboard-contains-node', true);				
			this.refreshView();
		}
	}

	didCutTree (desc: ProofEditDidCutTree): void {
		if (desc && desc.selected) {
			// copy node to system clipboard
			vscode.env.clipboard.writeText(desc.clipboard);			
			// set vscode context variable proof-explorer.clipboard-contains-tree and clipboard-contains-node to true
			commands.executeCommand('setContext', 'proof-explorer.clipboard-contains-tree', true);
			commands.executeCommand('setContext', 'proof-explorer.clipboard-contains-node', true);
			// refresh view
			this.refreshView();
			// append elems to sketchpad
			let sketchpadItems: ProofItem[] = [];
			for (let i = 0; i < desc.elems.length; i++) {
				const items: ProofItem[] = this.convertNodeX2ProofItem(desc.elems[i]);
				sketchpadItems = sketchpadItems.concat(items);
			}
			commands.executeCommand("proof-explorer.trim", { items: sketchpadItems });
		}
	}

	didPasteTree (desc: ProofEditDidPasteTree): void {
		// the tree structure is automatically updated whenever a node is added to the proof tree (see did-append-node)
		if (desc) {
			this.revealNode(desc.selected)
		}
		this.refreshView();
	}

	didRenameNode (desc: ProofEditDidRenameNode): void {
		if (desc && desc.selected && desc.newName) {
			const item: ProofItem = this.findNode(desc.selected.id);
			if (item) {
				item.rename(desc.newName);
				this.refreshView();
			} else {
				console.warn(`[vscode-proof-explorer] Warning: could not find item ${desc.selected.name} necessary for proofEdit/renameNode (${desc.selected.id})`)
			}
		} else {
			console.warn(`[vscode-proof-explorer] Warning: unable to complete proofEdit/renameNode`);
		}
	}

	didActivateCursor (desc: ProofEditDidActivateCursor): void {
		if (desc && desc.cursor) {
			const realNode: ProofItem = this.findNode(desc.cursor.parent);
			if (realNode) {
				this.ghostNode.realNode = realNode;
				this.ghostNode.parent = realNode;
				this.ghostNode.active();
				this.refreshView();
			}
		} else {
			console.warn(`[vscode-proof-explorer] Warning: unable to complete proofEdit/activateCursor`)
		}
	}

	didDeactivateCursor (desc: ProofEditDidDeactivateCursor): void {
		this.ghostNode.parent = null;
		this.ghostNode.notActive();
		this.refreshView();
	}

	didUpdateProofStatus (desc: ProofEditDidUpdateProofStatus): void {
		if (this.root) {
			if (desc.proofStatus === "proved") {
				this.root.QED();
			} else {
				this.root.pending();
				this.root.setProofStatus(desc.proofStatus);
			}
			this.refreshView();
		} else {
			console.warn(`[vscode-proof-explorer] Warning: could not update proof status (root node is null)`);
		}
	}

	willStartNewProof (): void {
		this.root = null;
		this.refreshView();
	}

	didTrimNode (desc: ProofEditDidTrimNode): void {
		if (desc && desc.elems && desc.elems.length) {
			let sketchpadItems: ProofItem[] = [];
			for (let i = 0; i < desc.elems.length; i++) {
				const items: ProofItem[] = this.convertNodeX2ProofItem(desc.elems[i]);
				sketchpadItems = sketchpadItems.concat(items);
			}
			commands.executeCommand("proof-explorer.trim", { items: sketchpadItems });
		} else {
			console.warn(`[vscode-proof-explorer] Warning: unable to complete proofEdit/trimNode`);
		}
	}

	didDeleteNode (desc: ProofEditDidDeleteNode): void {
		if (desc && desc.selected) {
			const item: ProofItem = this.findNode(desc.selected.id);
			if (item && item.parent) {
				item.parent.deleteChild(item);
				this.refreshView();
				console.log(`[vscode-proof-explorer] Did delete ${desc.selected.name} (${desc.selected.id})`);
			} else {
				console.warn(`[vscode-proof-explorer] Warning: could not find item ${desc.selected.name} necessary for proofEdit/deleteNode (${desc.selected.id})`)
			}
		} else {
			console.warn(`[vscode-proof-explorer] Warning: unable to complete proofEdit/deleteNode`);
		}
	}

	didAppendNode (desc: ProofEditDidAppendNode): void {
		if (desc) {
			const parent: ProofItem = this.findNode(desc.elem.parent);
			if (parent) {
				const items: ProofItem[] = this.convertNodeX2ProofItem(desc.elem); // do not give the parent otherwise the node will be automatically appended by convertNodeX2ProofItem
				const children1: ProofItem[] = parent.children.slice(0, desc.position);
				const children2: ProofItem[] = parent.children.slice(desc.position);
				parent.children = children1.concat(items.map(item => {
					item.parent = parent;
					return item;
				})).concat(children2);
				parent.collapsibleState = TreeItemCollapsibleState.Expanded;
				this.refreshView();
				console.log(`[vscode-proof-explorer] Did append ${desc.elem.name} (${desc.elem.id})`);
			} else {
				console.warn(`[vscode-proof-explorer] Warning: could not find parent ${desc.elem.parent} necessary for proofEdit/appendNode (${desc.elem.name})`)
			}
		} else {
			console.warn(`[vscode-proof-explorer] Warning: unable to complete proofEdit/appendNode`);
		}
	}

	/**
	 * Force refresh of the tree view
	 */
	refreshView(): void {
		if (this.enabled) {
			this._onDidChangeTreeData.fire();
		}
	}
	disableView (): void {
		this.enabled = false;
		vscode.commands.executeCommand('setContext', 'proof-explorer.visible', false);
	}
	enableView (): void {
		this.enabled = true;
		vscode.commands.executeCommand('setContext', 'proof-explorer.visible', true);
	}

	/**
	 * Internal function, used to delete the tree view
	 */
	disposeView(): void {
		this.root = null;
		vscode.commands.executeCommand('setContext', 'proof-explorer.clipboard-contains-node', false);
		vscode.commands.executeCommand('setContext', 'proof-explorer.clipboard-contains-tree', false);
		this.refreshView();
		this.disableView();
	}


	/**
	 * Utility function, used to set the initial proof state.
	 * @param sequent 
	 */
	didLoadSequent (sequent: SequentDescriptor): void {
		this.proofState = sequent;
		if (this.activeNode) {
			this.activeNode.updateTooltip(sequent);
		} else {
			this.root.tooltip = utils.formatSequent(sequent);
		}
	}

	updateTooltip (desc: ProofExecDidUpdateSequent): void {
		if (desc && desc.selected) {
			const selected: ProofItem = this.findNode(desc.selected.id);
			if (selected) {
				selected.updateTooltip(desc.sequent);
				this.refreshView();
			}
		}
	}

	startProof (): void {
		this.refreshView();
		if (this.root && this.root.children && this.root.children.length) {
			if (utils.isGlassboxTactic(this.root.children[0].name)) {
				this.queryUnfoldGlassbox();
			}
		}
	}
	
	protected convertNodeX2ProofItem (elem: ProofNodeX, parent?: ProofItem): ProofItem[] {
		const fromNodeX2 = (elem: ProofNodeX, parent?: ProofItem): ProofItem => {
			const node: ProofItem = (elem.type === "proof-command") ? 
					new ProofCommand({ id: elem.id, cmd: elem.name, branchId: elem.branch, parent }) 
					: new ProofBranch({ id: elem.id, cmd: elem.name, branchId: elem.branch, parent });
			if (parent) {
				parent.appendChild(node);
			}
			if (elem.rules && elem.rules.length) {
				elem.rules.forEach(child => {
					fromNodeX2(child, node);
				});
			} else {
				node.collapsibleState = TreeItemCollapsibleState.None;
			}
			return node;
		}
		const items: ProofItem[] = []
		if (elem.type === "root") {
			// convert only its children
			for (let i = 0; i < elem.rules.length; i++) {
				const item: ProofItem = fromNodeX2(elem.rules[i], parent);
				items.push(item);
			}
		} else {
			// append elem
			const item: ProofItem = fromNodeX2(elem, parent);
			items.push(item);				
		}
		return items;
	}

	loadProofStructure (formula: PvsFormula, desc: ProofDescriptor, proof: ProofNodeX): void {
		this.formula = formula;
		this.root = new RootNode({
			id: proof.id,
			name: proof.name, //(desc.proof) ? desc.proof.name : desc.info.formula, 
			proofStatus: (desc && desc.info && desc.info.status) ? desc.info.status : "unfinished"
		});
		this.ghostNode = new GhostNode({ parent: this.root, node: this.root });

		if (proof.rules && proof.rules.length) {
			this.root.children = this.convertNodeX2ProofItem(proof, this.root);
		}
		this.refreshView();
	}
	/**
	 * Loads a proof descriptor in proof-explorer
	 * @param desc The proof descriptor to be loaded
	 */
	loadProofDescriptor (desc: ProofDescriptor): void {
		// utility function for building the proof tree
		const createTree = (elem: ProofNode, parent: ProofItem): void => {
			const node: ProofItem = (elem.type === "proof-command") ? 
				new ProofCommand({ cmd: elem.name, branchId: elem.branch, parent }) 
				: new ProofBranch({ cmd: elem.name, branchId: elem.branch, parent });
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
					&& !(desc.proofTree.rules.length === 1 && utils.isPostponeCommand(desc.proofTree.rules[0].name))) {
				desc.proofTree.rules.forEach((child: ProofNode) => {
					createTree(child, this.root);
				});
			} else {
				this.root.collapsibleState = TreeItemCollapsibleState.None;
			}
			this.proofDescriptor = desc;
		} else {
			console.warn(`[proof-explorer] Warning: null descriptor`);
		}
		// refresh view
		this.refreshView();
	}

	async queryRunProof (msg: string): Promise<boolean> {
		const yesno: string[] = [ "Run Proof", "No" ];
		const ans: string = await vscode.window.showInformationMessage(msg, { modal: true }, yesno[0]);
		return ans === yesno[0];
	}
	/**
	 * Query unfold glassbox
	 */
	async queryUnfoldGlassbox (): Promise<void> {
		const msg: string = `The proof script has been imported from ProofLite. To view the proof structure in proof-explorer, you need to run the proof.`;
		const actionConfirmed: boolean = await this.queryRunProof(msg);
		if (actionConfirmed) {
			commands.executeCommand("proof-explorer.run-proof");
		}
	}
	/**
	 * Save the current proof on file
	 * @param opt Optionals: whether confirmation is necessary before saving (default: confirmation is not needed)  
	 */
	async querySaveProof (opt?: { msg?: string }): Promise<void> {
		opt = opt || {};
		const note: string = (opt.msg) ? `${opt.msg}\n` : "";
		const msg: string = (this.root) ? note + `Save proof ${this.root.name}?` : note + "Save proof?";
		const actionConfirmed: boolean = await this.queryConfirmation(msg);
		if (actionConfirmed) {
			// send quit to the terminal
			const action: ProofEditSave = { action: "save-proof" };
			this.client.sendRequest(serverEvent.querySaveProofResponse, action);
		} else {
			// send quit to the terminal
			const action: ProofExecQuit = { action: "quit" };
			this.client.sendRequest(serverEvent.querySaveProofResponse, action);
		}
	}
	/**
	 * Quit the current proof
	 * @param opt Optionals: whether confirmation is necessary before quitting (default: confirmation is needed)  
	 */
	async quitProof (): Promise<void> {
		const actionConfirmed: boolean = await this.queryConfirmation("Quit Proof Session?");
		if (actionConfirmed) {
			// send quit to the terminal
			commands.executeCommand("vscode-pvs.send-proof-command", {
				fileName: this.formula.fileName,
				fileExtension: this.formula.fileExtension,
				theoryName: this.formula.theoryName,
				formulaName: this.formula.formulaName,
				contextFolder: this.formula.contextFolder,
				cmd: "save-then-quit"
			});
		}
	}
	async queryConfirmation (msg: string): Promise<boolean> {
		const yesno: string[] = [ "Yes", "No" ];
		const ans: string = await vscode.window.showInformationMessage(msg, { modal: true }, yesno[0]);
		return ans === yesno[0];
	}
	/**
	 * Activation function, installs all proof-explorer command handlers.
	 * @param context Client context 
	 */
	activate(context: ExtensionContext): void {
		// -- handler for node updates
		this.client.onNotification(serverEvent.proofNodeUpdate, (desc: { id: string, name: string, status: ProofNodeStatus }) => {
			if (this.root) {
				const node: ProofItem = this.findNode(desc.id);
				if (node) {
					if (desc.status === "active") {
						this.activeNode = node;
					}
					node.updateStatus(desc.status);
					this.refreshView();
				} else {
					console.warn(`[vscode-proof-explorer] Warning: could not update status of node ${desc.name} to ${desc.status}`);
				}
			}
		});
		context.subscriptions.push(commands.registerCommand("proof-explorer.reveal-node", (desc: { id: string, name: string }) => {
            this.revealNode(desc);
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.focus-node", (desc: { id: string, name: string }) => {
            this.revealNode(desc);
		}));
		// -- handlers for proof explorer commands
		context.subscriptions.push(commands.registerCommand("proof-explorer.trim-unused", (resource: ProofItem) => {
			// save proof without asking confirmation
			const action: ProofEditTrimUnused = { action: "trim-unused", selected: { id: resource.id, name: resource.name } };
			this.client.sendRequest(serverRequest.proverCommand, action);
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.save-proof", () => {
			// save proof without asking confirmation
			const action: ProofEditSave = { action: "save-proof" };
			this.client.sendRequest(serverRequest.proverCommand, action);
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.save-proof-as-prf", () => {
			// save proof without asking confirmation
			const action: ProofEditSaveAs = { action: "save-proof-as", fileExtension: ".prf" };
			this.client.sendRequest(serverRequest.proverCommand, action);
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.save-proof-as-prl", () => {
			// save proof without asking confirmation
			const action: ProofEditSaveAs = { action: "save-proof-as", fileExtension: ".prl" };
			this.client.sendRequest(serverRequest.proverCommand, action);
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.new-proof", async () => {
			 // ask confirmation before deleting a node
			 if (this.formula) {
				const msg: string = `Start a new proof for ${this.formula.formulaName}?`;
				const actionConfirmed: boolean = await this.queryConfirmation(msg);
				if (actionConfirmed) {
					const action: ProofExecStartNewProof = { action: "start-new-proof", formula: this.formula };
					console.log(`[vscode-proof-explorer] Starting new proof for ${this.formula.formulaName}`);
					this.client.sendRequest(serverRequest.proverCommand, action);
				}
			}
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.open-proof", async () => {
			if (this.formula && this.formula.theoryName && this.formula.formulaName) {
				const desc: PvsFile = await vscodeUtils.openProofFile();
				if (desc && desc.fileExtension) {
					const action: ProofExecOpenProof = {
						action: "open-proof",
						proofFile: desc,
						formula: this.formula
					};
					this.client.sendRequest(serverRequest.proverCommand, action);
				}
			}
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.quit-proof", async () => {
			// ask confirmation before quitting proof
			const actionConfirmed: boolean = await this.queryConfirmation("Quit Proof Session?");
			if (actionConfirmed) {
				const action: ProofExecQuit = { action: "quit" };
				this.client.sendRequest(serverRequest.proverCommand, action);
			}
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.forward", () => {
			// execute next proof command
			const action: ProofExecForward = { action: "forward" };
			this.client.sendRequest(serverRequest.proverCommand, action);
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.back", () => {
			// go back one proof command
			const action: ProofExecBack = { action: "back" };
			this.client.sendRequest(serverRequest.proverCommand, action);
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.run-proof", () => {
			if (this.serverMode === "in-checker") {
				// run entire proof
				const action: ProofExecRun = { action: "run" };
				this.client.sendRequest(serverRequest.proverCommand, action);
			} else {
				commands.executeCommand("vscode-pvs.prove-formula", this.formula);
			}
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.fast-forward", (resource: ProofItem) => {
			// fast forward proof to a given proof command
			const action: ProofExecFastForward = { action: "fast-forward", selected: { id: resource.id, name: resource.name } };
			console.log(`[vscode-proof-explorer] Fast forward to ${resource.name} (${resource.id})`);
			this.client.sendRequest(serverRequest.proverCommand, action);
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.copy-node", (resource: ProofItem) => {
			// copy selected node
			const action: ProofEditCopyNode = { action: "copy-node", selected: { id: resource.id, name: resource.name } };
			console.log(`[vscode-proof-explorer] Copy node ${resource.name} (${resource.id})`);
			this.client.sendRequest(serverRequest.proverCommand, action);
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.copy-subtree", (resource: ProofItem) => {
			// copy selected node
			const action: ProofEditCopyTree = { action: "copy-tree", selected: { id: resource.id, name: resource.name } };
			console.log(`[vscode-proof-explorer] Copy tree rooted at ${resource.name} (${resource.id})`);
			this.client.sendRequest(serverRequest.proverCommand, action);
		}));
		// context.subscriptions.push(commands.registerCommand("proof-explorer.paste-before-proof-command", (resource: ProofItem) => {
		// 	this.pasteBeforeNode({ selected: resource });
		// }));
		context.subscriptions.push(commands.registerCommand("proof-explorer.paste-node", (resource: ProofItem) => {
			const action: ProofEditPasteNode = { action: "paste-node", selected: { id: resource.id, name: resource.name } };
			console.log(`[vscode-proof-explorer] Pasting clipboard content at ${resource.name} (${resource.id})`);
			this.client.sendRequest(serverRequest.proverCommand, action);
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.paste-subtree", (resource: ProofItem) => {
			const action: ProofEditPasteTree = { action: "paste-tree", selected: { id: resource.id, name: resource.name } };
			console.log(`[vscode-proof-explorer] Pasting clipboard content at ${resource.name} (${resource.id})`);
			this.client.sendRequest(serverRequest.proverCommand, action);
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.delete-node", async (resource: ProofItem) => {
			// ask confirmation before deleting a node
			const msg: string = (resource.contextValue === "root") ? `Delete current proof?` : `Delete ${resource.name}?`;
			const actionConfirmed: boolean = await this.queryConfirmation(msg);
			if (actionConfirmed) {
				const action: ProofEditDeleteNode = { action: "delete-node", selected: { id: resource.id, name: resource.name } };
				console.log(`[vscode-proof-explorer] Deleting node ${resource.name} (${resource.id})`);
				this.client.sendRequest(serverRequest.proverCommand, action);
			}
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.trim-node", async (resource: ProofItem) => {
			const msg: string = resource.contextValue === "root" ? `Delete current proof?` 
				: resource.contextValue === "proof-branch" ? `Delete proof commands in branch ${resource.name}?`
					: `Delete proof commands after ${resource.name}?`;
			const actionConfirmed: boolean = await this.queryConfirmation(msg);
			if (actionConfirmed) {
				const action: ProofEditTrimNode = { action: "trim-node", selected: { id: resource.id, name: resource.name } };
				console.log(`[vscode-proof-explorer] Trimming node ${resource.name} (${resource.id})`);
				this.client.sendRequest(serverRequest.proverCommand, action);
			}
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.delete-tree", async (resource: ProofItem) => {
			const msg: string = (resource.contextValue === "root") ? `Delete current proof?` 
					: `Delete ${resource.name}?`;
			const actionConfirmed: boolean = await this.queryConfirmation(msg);
			if (actionConfirmed) {
				const action: ProofEditDeleteTree = { action: "delete-tree", selected: { id: resource.id, name: resource.name } };
				console.log(`[vscode-proof-explorer] Deleting tree rooted at ${resource.name} (${resource.id})`);
				this.client.sendRequest(serverRequest.proverCommand, action);
			}
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.cut-node", (resource: ProofItem) => {
			const action: ProofEditCutNode = { action: "cut-node", selected: { id: resource.id, name: resource.name } };
			console.log(`[vscode-proof-explorer] Cutting node ${resource.name} (${resource.id})`);
			this.client.sendRequest(serverRequest.proverCommand, action);
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.cut-subtree", (resource: ProofItem) => {
			const action: ProofEditCutTree = { action: "cut-tree", selected: { id: resource.id, name: resource.name } };
			console.log(`[vscode-proof-explorer] Cutting tree rooted at ${resource.name} (${resource.id})`);
			this.client.sendRequest(serverRequest.proverCommand, action);
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.new-proof-command", async (resource: ProofItem) => {
			const name: string = await vscode.window.showInputBox({
				prompt: `Please enter proof command to be appended after ${resource.name}`,
				placeHolder: ``,
				value: ``,
				ignoreFocusOut: true 
			});
			if (name) {
				const action: ProofEditAppendNode = { action: "append-node", selected: { id: resource.id, name: resource.name }, name };
				// console.log(`[vscode-proof-explorer] Appending ${name} at ${resource.name} (${resource.id})`);
				this.client.sendRequest(serverRequest.proverCommand, action);
			}
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.create-proof-branch", (resource: ProofItem) => {
			const action: ProofEditAppendBranch = { action: "append-branch", selected: { id: resource.id, name: resource.name } };
			// console.log(`[vscode-proof-explorer] Appending new branch at ${resource.name} (${resource.id})`);
			this.client.sendRequest(serverRequest.proverCommand, action);
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.edit-node", async (resource: ProofItem) => {
			let newName: string = await vscode.window.showInputBox({ prompt: `Editing proof command ${resource.name}`, placeHolder: `${resource.name}`, value: `${resource.name}`, ignoreFocusOut: true });
			if (newName) {
				const action: ProofEditRenameNode = { action: "rename-node", selected: { id: resource.id, name: resource.name }, newName };
				console.log(`[vscode-proof-explorer] Renaming node ${resource.name} (${resource.id})`);
				this.client.sendRequest(serverRequest.proverCommand, action);
			}
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.show-sequent", (resource: ProofItem) => {
			const name: string = `${this.formula.theoryName}${fsUtils.logFileExtension}`;
			vscodeUtils.previewTextDocument(name, resource.tooltip, { contextFolder: path.join(vscode.workspace.rootPath, "pvsbin")});
		}));
		context.subscriptions.push(commands.registerCommand("proof-explorer.show-active-sequent", (resource: ProofItem) => {
			const name: string = `${this.formula.theoryName}${fsUtils.logFileExtension}`;
			vscodeUtils.previewTextDocument(name, resource.tooltip, { contextFolder: path.join(vscode.workspace.rootPath, "pvsbin")});
		}));

		let cmd: string = null;
		// click on the any node (except ghost nodes) enables search by type in the tree view
		context.subscriptions.push(commands.registerCommand('proof-explorer.root-selected', async (resource: ProofItem) => {
			if (this.filterOnTypeActive) { // this will capture future attempt to toggle the filter -- there's no other way to keep this filter on
				this.filterOnTypeActive = false;
				commands.executeCommand('list.toggleFilterOnType', false);
			}
		}));
		context.subscriptions.push(commands.registerCommand('proof-explorer.did-select-proof-branch', async (resource: ProofItem) => {
			if (this.filterOnTypeActive) { // this will capture future attempt to toggle the filter -- there's no other way to keep this filter on
				this.filterOnTypeActive = false;
				commands.executeCommand('list.toggleFilterOnType', false);
			}
		}));
		context.subscriptions.push(commands.registerCommand('proof-explorer.did-select-proof-command', async (resource: ProofItem) => {
			if (this.filterOnTypeActive) { // this will capture future attempt to toggle the filter -- there's no other way to keep this filter on
				this.filterOnTypeActive = false;
				commands.executeCommand('list.toggleFilterOnType', false);
			}
			// register double click handler
			if (!cmd || cmd !== resource.name) {
				cmd = resource.name;
				setTimeout(() => {
					cmd = null
				}, 250);	
			} else {
				const dd: PvsProofCommand = { 
					fileName: this.formula.fileName,
					fileExtension: this.formula.fileExtension,
					contextFolder: this.formula.contextFolder,
					theoryName: this.formula.theoryName, 
					formulaName: this.formula.formulaName,
					cmd
				}
				commands.executeCommand("proof-explorer.proof-command-dblclicked", dd);
				cmd = null;
			}
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
			this.loading.stop();
			return Promise.resolve([ this.root ]);
		} else {
			this.loading.start().then(() => { this.refreshView(); });
			return Promise.resolve([ this.loading ]);
		}
	}
	/**
	 * Returns the requested node
	 * @param item Node to be returned
	 */
	getTreeItem(item: TreeItem): TreeItem {
		return (this.enabled) ? item : null;
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
// Auxiliary constants and definitions
//-------------------------------------------------------------

// https://emojipedia.org/symbols/
//  ❌ 🔵 ⚫ ⚪ 🔴 🔽 🔼 ⏯ ⏩ ⏪ ⏫ ⏬ ▶️ ◀️ ⭕ 🔹🔸💠🔷🔶
// use https://iconify.design/icon-sets/ for proof explorer, to have a consistent look&feel on all systems.

export const QED: ProofStatus = "proved";

/**
 * Definition of tree items
 */
export class ProofItem extends TreeItem {
	contextValue: string = "proofItem";
	name: string; // prover command or branch id
	branchId: string = ""; // branch in the proof tree where this command is located (branchId for root is "").
	command: Command; // vscode action associated to the node
	// icon: string = ""; // icon indicating the state of the node
	// protected previousState: {
	// 	tooltip?: string,
	// 	icon?: string
	// } = {};
	children: ProofItem[] = [];
	parent: ProofItem;
	protected activeFlag: boolean = false;
	protected visitedFlag: boolean = false;
	protected pendingFlag: boolean = false;
	// protected noChangeFlag: boolean = false;
	// protected prevFlags: {
	// 	activeFlag: boolean,
	// 	visitedFlag: boolean,
	// 	pendingFlag: boolean
	// } = {
	// 	activeFlag: false,
	// 	visitedFlag: false,
	// 	pendingFlag: false
	// };
	proofState: SequentDescriptor = null; // sequents *before* the execution of the node
	constructor (desc: { id?: string, type: string, name: string, branchId: string, parent: ProofItem, collapsibleState?: TreeItemCollapsibleState }) {
		super(desc.type, (desc.collapsibleState === undefined) ? TreeItemCollapsibleState.Expanded : desc.collapsibleState);
		this.contextValue = desc.type;
		this.id = (desc.id) ? desc.id : fsUtils.get_fresh_id();
		this.name = desc.name;
		this.branchId = desc.branchId;
		this.parent = desc.parent;
		this.tooltip = "Double click sends command to terminal"; // the tooltip will shows the sequent before the execution of the proof command, as soon as the node becomes active
		this.notVisited();
	}
	// clone (opt?: { parent?: ProofItem }): ProofItem {
	// 	opt = opt || {};
	// 	switch (this.contextValue) {
	// 		case "root": { return <RootNode> this.clone(); }
	// 		case "proof-branch": { return <ProofBranch> this.clone(); }
	// 		case "proof-command": { return <ProofCommand> this.clone(); }
	// 		default: {
	// 			console.warn(`[proof-explorer] Warning: trying to clone node type ${this.contextValue}`);
	// 		}
	// 	}
	// 	return new ProofItem({ type: this.contextValue, name: this.name, branchId: this.branchId, parent: opt.parent, collapsibleState: this.collapsibleState });
	// }
	// cloneTree (opt?: { parent?: ProofItem }): ProofItem {
	// 	opt = opt || {};
	// 	opt.parent = opt.parent || this.parent || null;
	// 	const clonedRoot: ProofItem = this.clone(opt);
	// 	if (this.children) {
	// 		for (let i: number = 0; i < this.children.length; i++) {
	// 			const child: ProofItem = this.children[i].cloneTree(clonedRoot);
	// 			clonedRoot.insertChild(child, i);
	// 		}
	// 	}
	// 	return clonedRoot;
	// }
	// saveAttributes (): void {
	// 	this.prevFlags = {
	// 		activeFlag: this.activeFlag,
	// 		visitedFlag: this.visitedFlag,
	// 		pendingFlag: this.pendingFlag
	// 	};
	// }
	// restoreAttributes (): void {
	// 	if (this.prevFlags.activeFlag) { return this.active(); }
	// 	if (this.prevFlags.visitedFlag) { return this.visited(); }
	// 	if (this.prevFlags.pendingFlag) { return this.pending(); }
	// }
	updateTooltip (sequent?: utils.SequentDescriptor): void {
		this.tooltip = (sequent) ? utils.formatSequent(sequent)
			: (this.proofState) ? utils.formatSequent(this.proofState) 
				: " ";
	}
	updateStatus (status: ProofNodeStatus): void {
		switch (status) {
			case "active": { this.active(); break; }
			case "visited": { this.visited(); break; }
			case "not-visited": { this.notVisited(); break; }
			case "pending": { this.pending(); break; }
			default: {
				console.warn(`[vscode-proof-explorer] Warning: unrecognized node status ${status}`);
			}
		}
	}
	rename (name: string): void {
		this.name = name;
		this.label = this.name;//`${this.icon}${this.name}`;
	}
	pending (): void {
		// this.icon = "🔶";
		this.label = this.name;//`${this.icon}${this.name}`;
		this.activeFlag = false;
		this.visitedFlag = false;
		this.pendingFlag = true;
		// this.noChangeFlag = false;
		this.iconPath = {
			light: path.join(__dirname, "..", "..", "..", "icons", "svg-star.svg"),
            dark: path.join(__dirname, "..", "..", "..", "icons", "svg-star.svg")
            // light: path.join(__dirname, "..", "..", "..", "icons", "svg-orange-diamond.svg"),
            // dark: path.join(__dirname, "..", "..", "..", "icons", "svg-orange-diamond.svg")
        };
	}
	visited (): void {
		// this.previousState.tooltip = this.tooltip;
		// this.previousState.icon = 
		// this.icon = " ★ ";
		this.label = this.name;//`${this.icon}${this.name}`;
		this.activeFlag = false;
		this.visitedFlag = true;
		this.pendingFlag = false;
		// this.noChangeFlag = false;
		this.iconPath = {
            // light: path.join(__dirname, "..", "..", "..", "icons", "svg-star.svg"),
            // dark: path.join(__dirname, "..", "..", "..", "icons", "svg-star.svg")
            light: path.join(__dirname, "..", "..", "..", "icons", "star.png"),
            dark: path.join(__dirname, "..", "..", "..", "icons", "star.png")
        };
	}
	notVisited (): void {
		// this.previousState.tooltip = this.tooltip;
		// this.previousState.icon = 
		// this.icon = " ∘  ";
		this.label = this.name;//`${this.icon}${this.name}`;
		this.activeFlag = false;
		this.visitedFlag = false;
		this.pendingFlag = false;
		// this.noChangeFlag = false;
		this.iconPath = {
            light: path.join(__dirname, "..", "..", "..", "icons", "svg-dot-gray.svg"),
            dark: path.join(__dirname, "..", "..", "..", "icons", "svg-dot-white.svg")
        };
	}
	// noChange (): void {
	// 	// this.previousState.tooltip = this.tooltip;
	// 	// this.previousState.icon = this.icon;
	// 	this.icon = '⭕';
	// 	this.label = `${this.icon}${this.name}`;
	// 	this.activeFlag = false;
	// 	this.visitedFlag = false;
	// 	this.pendingFlag = false;
	// 	// this.noChangeFlag = true;
	// }
	active (): void {
		// this.icon = "🔷";
		this.label = this.name;//`${this.icon}${this.name}`;
		this.activeFlag = true;
		this.visitedFlag = false;
		this.pendingFlag = false;
		// this.noChangeFlag = false;
		vscode.commands.executeCommand("proof-explorer.reveal-node", { id: this.id, name: this.name });
		this.iconPath = {
            light: path.join(__dirname, "..", "..", "..", "icons", "svg-blue-diamond.svg"),
            dark: path.join(__dirname, "..", "..", "..", "icons", "svg-blue-diamond.svg")
        };
	}
	// treeNotVisited (): void {
	// 	this.notVisited();
	// 	for (let i = 0; i < this.children.length; i++) {
	// 		this.children[i].treeNotVisited();
	// 	}
	// }
	// treeVisited (): void {
	// 	this.visited();
	// 	for (let i = 0; i < this.children.length; i++) {
	// 		this.children[i].treeVisited();
	// 	}
	// }
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
	// restore (): void {
	// 	// this.label = `${this.previousState.icon}${this.name}`;
	// 	// this.tooltip = this.previousState.tooltip;
	// }
	setChildren (children: ProofItem[]): void {
		this.children = children;
	}
	// insertChild (child: ProofItem, position: number): void {
	// 	if (this.children && position < this.children.length - 1) {
	// 		this.children = this.children.slice(0, position).concat([ child ]).concat(this.children.slice(position));
	// 	} else {
	// 		// append at the end
	// 		this.children = this.children.concat([ child ]);
	// 	}
	// 	this.collapsibleState = TreeItemCollapsibleState.Expanded;
	// }
	deleteChild (child: ProofItem): void {
		this.children = this.children.filter((ch: ProofItem) => {
			return ch.id !== child.id;
		});
		if (this.contextValue !== "root" && this.children.length === 0) {
			this.collapsibleState = TreeItemCollapsibleState.None;
		}
	}
	// deleteTree (child: ProofItem): void {
	// 	const children: ProofItem[] = [];
	// 	for (let i in this.children) {
	// 		if (this.children[i].id !== child.id) {
	// 			children.push(this.children[i]);
	// 		} else {
	// 			this.children = children;
	// 			if (this.contextValue !== "root" && this.children.length === 0) {
	// 				this.collapsibleState = TreeItemCollapsibleState.None;
	// 			}
	// 			return; // this stops the iteration and prunes the tree
	// 		}
	// 	}
	// }
	getProofCommands (): ProofItem[] {
		let ans: ProofItem[] = [ this ];
		if (this.children) {
			for (let i = 0; i < this.children.length; i++) {
				ans = ans.concat(this.children[i].getProofCommands());
			}
		}
		return ans;
	}
	// moveIndicatorForward (opt?: { lastVisitedChild?: ProofItem, keepSameBranch?: boolean, proofState?: SequentDescriptor }): ProofItem | null {
	// 	opt = opt || {};
	// 	if (this.contextValue === "proof-command") {
	// 		this.visited();
	// 	} else {
	// 		// proof branch or root
	// 		this.pending();
	// 	}
	// 	// check if this node has children, if so, return the first non-visited child
	// 	const proofState: SequentDescriptor = opt.proofState || this.proofState;
	// 	if (this.children && this.children.length) {
	// 		if (!proofState) {
	// 			window.showErrorMessage(`[pvs-explorer] Error: proofstate is null. Please restart the proof and report this error to the vscode-pvs developers.`);
	// 		}
	// 		const activeBranchId: string = utils.getBranchId(proofState.label);
	// 		if (opt.keepSameBranch && this.children[0].branchId !== activeBranchId) {
	// 			return null;
	// 		}
	// 		if (opt.lastVisitedChild) {
	// 			// the next node to be visited is the one after the last visited
	// 			const idx: number = this.children.indexOf(opt.lastVisitedChild);
	// 			const next: number = idx + 1;
	// 			if (next < this.children.length) {
	// 				return this.children[next];
	// 			}
	// 		} else {
	// 			return this.children[0];
	// 		}
	// 	}
	// 	// else, branch completed
	// 	// go to next sibling, unless opt.keepSameBranch === true 
	// 	if (opt.keepSameBranch 
	// 			&& ((this.contextValue === "proof-branch" && this.children.length === 0) 
	// 				|| (this.contextValue === "proof-command" && this.children.length > 0))) {
	// 		return null;
	// 	}
	// 	this.visited();
	// 	if (this.parent && this.contextValue === "proof-command") {
	// 		return this.parent.moveIndicatorForward({ lastVisitedChild: this, proofState: this.proofState });
	// 	}
	// 	return null;
	// }
	// selectLastVisitedChild (): ProofItem {
	// 	if (this.children) {
	// 		const children: ProofItem[] = this.children.filter(item => {
	// 			return item.isVisitedOrPending();
	// 		});
	// 		if (children && children.length) {
	// 			this.pending();
	// 			return children[children.length - 1].selectLastVisitedChild();
	// 		}
	// 	}
	// 	return this;
	// }
	// moveIndicatorBack (opt?: { keepSameBranch?: boolean }): ProofItem {
	// 	if (this.parent) {
	// 		if (this.parent.children && this.parent.children.length) {
	// 			const children: ProofItem[] = this.parent.children.filter((item: ProofItem) => {
	// 				return item.isVisitedOrPending();
	// 			});
	// 			// return previous child, if any
	// 			if (children && children.length) {
	// 				this.notVisited();
	// 				const candidate: ProofItem = children[children.length - 1];
	// 				if (candidate.children && candidate.children.length) {
	// 					return candidate.selectLastVisitedChild();
	// 				}
	// 				return candidate;
	// 			}
	// 			// return first proof command, if parent is root
	// 			if (this.parent.contextValue === "root" && this.parent.children && this.parent.children.length) {
	// 				// we were on the first proof command, cannot undo
	// 				return this;
	// 			}
	// 			// else return parent
	// 			// unless opt.keepSameBranch === true 
	// 			if (opt.keepSameBranch && this.contextValue === "proof-command") {
	// 				return this;
	// 			}
	// 			this.notVisited();
	// 			return this.parent;
	// 		}
	// 	} else {
	// 		console.error(`Error: Could not find parent for node ${this.name}`);
	// 	}
	// 	return this;
	// }
	// getSiblingOrParent (): ProofItem {
	// 	if (this.parent) {
	// 		const children: ProofItem[] = this.parent.children;
	// 		if (children && children.length > 1) {
	// 			const index: number = children.indexOf(this);
	// 			const next: number = index + 1;
	// 			const prev: number = index - 1;
	// 			return (next < children.length) ? children[next] : children[prev];	
	// 		}
	// 		// else, this is the only child, return the parent
	// 		return this.parent;
	// 	} else {
	// 		console.error(`[proof-explorer] Error: could not find sibling or parent for node ${this.name}`);
	// 	}
	// 	return null;
	// }
	// getNextSibling (): ProofItem | null {
	// 	if (this.parent) {
	// 		const children: ProofItem[] = this.parent.children;
	// 		if (children && children.length > 1) {
	// 			const index: number = children.indexOf(this);
	// 			const next: number = index + 1;
	// 			if (next < children.length) {
	// 				return children[next];
	// 			}
	// 		}
	// 	}
	// 	return null;
	// }
	// getPreviousSibling (): ProofItem | null {
	// 	if (this.parent) {
	// 		const children: ProofItem[] = this.parent.children;
	// 		if (children && children.length > 1) {
	// 			const index: number = children.indexOf(this);
	// 			const prev: number = index - 1;
	// 			if (prev >= 0) {
	// 				return children[prev];
	// 			}
	// 		}
	// 	}
	// 	return null;
	// }
	// appendSibling (sib: ProofItem, opt?: { beforeSelected?: boolean }): void {
	// 	let children: ProofItem[] = [];
	// 	const n: number = this.parent.children.length;
	// 	opt = opt || {};
	// 	for (let i = 0; i < n; i++) {
	// 		if (!opt.beforeSelected) {
	// 			children.push(this.parent.children[i]);
	// 		}
	// 		if (this.parent.children[i].id === this.id) {
	// 			if (sib.contextValue === "root") {
	// 				children = children.concat(sib.children);
	// 			} else {
	// 				children.push(sib);
	// 			}
	// 		}
	// 		if (opt.beforeSelected) {
	// 			children.push(this.parent.children[i]);
	// 		}
	// 	}
	// 	this.parent.children = children;
	// }
	// appendChildAtBeginning (child: ProofItem): void {
	// 	this.children = this.children || [];
	// 	child.parent = this;
	// 	if (child.contextValue === "root") {
	// 		this.children = child.children.concat(this.children);
	// 	} else {
	// 		this.children = [ child ].concat(this.children);
	// 	}
	// 	this.collapsibleState = TreeItemCollapsibleState.Expanded;
	// }
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
	constructor (desc: { id?: string, cmd: string, branchId: string, parent: ProofItem, collapsibleState?: TreeItemCollapsibleState }) {
		super({ id: desc.id, type: "proof-command", name: desc.cmd, branchId: desc.branchId, parent: desc.parent, collapsibleState: desc.collapsibleState });
		const cmd: string = desc.cmd.trim();
		this.name = (cmd && cmd.startsWith("(") && cmd.endsWith(")")) || utils.isUndoCommand(cmd) ? cmd : `(${cmd})`;
		this.notVisited();
		this.command = {
			title: this.contextValue,
			command: "proof-explorer.did-select-proof-command",
			arguments: [ this ]
		};
	}
	// @override
	// clone (parent?: ProofCommand): ProofCommand {
	// 	const c: ProofCommand = new ProofCommand({ cmd: this.name, branchId: this.branchId, parent, collapsibleState: this.collapsibleState });
	// 	return c;
	// }
}
class ProofBranch extends ProofItem {
	constructor (desc: { id?: string, cmd: string, branchId: string, parent: ProofItem, collapsibleState?: TreeItemCollapsibleState }) {
		super({ id: desc.id, type: "proof-branch", name: desc.cmd, branchId: desc.branchId, parent: desc.parent, collapsibleState: desc.collapsibleState });
		this.name = `(${desc.branchId})`;
		this.notVisited();
		this.command = {
			title: this.contextValue,
			command: "proof-explorer.did-select-proof-branch",
			arguments: [ this ]
		};
	}
	// @override
	// clone (parent?: ProofBranch): ProofBranch {
	// 	const c: ProofBranch = new ProofBranch({ cmd: this.name, branchId: this.branchId, parent: this.parent, collapsibleState: this.collapsibleState });
	// 	c.parent = parent || null;
	// 	return c;
	// }

}
class WelcomeScreen extends TreeItem {
	constructor () {
		super("welcome-screen", TreeItemCollapsibleState.None);
		this.label = "Proof Explorer will become active when starting a proof";
	}
}
class LoadingItem extends TreeItem {
	contextValue: string = "loading-content";
	message: string = "Loading proof"
	protected points: number = 0;
	protected MAX_POINTS: number = 3;
	protected timer: NodeJS.Timer;
	constructor () {
		super ("loading-content", TreeItemCollapsibleState.None);
		this.label = this.message;
	}
	start (): Promise<void> {
		return new Promise ((resolve, reject) => {
			const timeout: number = this.points < this.MAX_POINTS ? 400 : 1000;
			this.timer = setInterval(() => {
				this.loading();
				resolve();
			}, timeout);
		});
	}
	protected loading (): void {
		this.label = this.message + ".".repeat(this.points);
		this.points = this.points < this.MAX_POINTS ? this.points + 1 : 0;
	}
	stop (): void {
		this.points = 0;
		clearInterval(this.timer);
		this.timer = null;
	}
}
class RootNode extends ProofItem {
	proofStatus: ProofStatus; // this is updated while running the proof
	initialProofStatus: ProofStatus; // this is set at the beginning (and at the end of the proof attempt if the proof succeeds)
	constructor (desc: { id?: string, name: string, proofStatus?: ProofStatus }) {
		super({ id: desc.id, type: "root", name: desc.name, branchId: "", parent: null, collapsibleState: TreeItemCollapsibleState.Expanded });
		this.parent = this; // the parent of the root is the root itself
		this.proofStatus = desc.proofStatus || "untried"
		this.initialProofStatus = this.proofStatus;
		this.notVisited();
		this.command = {
			title: this.contextValue,
			command: "proof-explorer.root-selected",
			arguments: [ this ]
		};
	}
	// @overrides
	// clone (parent?: RootNode): RootNode {
	// 	const c: RootNode = new RootNode({ name: this.name, proofStatus: this.proofStatus });
	// 	c.parent = parent || null;
	// 	c.pending();
	// 	return c;
	// }

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
		this.iconPath = {
            light: path.join(__dirname, "..", "..", "..", "icons", "svg-orange-diamond.svg"),
            dark: path.join(__dirname, "..", "..", "..", "icons", "svg-orange-diamond.svg")
        };
	}
	QED (): void {
		super.visited();
		// this.icon = utils.icons.checkmark;
		this.proofStatus = QED;
		this.setProofStatus(QED);
		this.updateLabel();
		this.iconPath = {
            light: path.join(__dirname, "..", "..", "..", "icons", "svg-checkmark.svg"),
            dark: path.join(__dirname, "..", "..", "..", "icons", "svg-checkmark.svg")
		};
		setTimeout(() => {
			vscode.commands.executeCommand("proof-explorer.focus-node", { id: this.id, name: this.name });
		}, 400);
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
	getProofStatus (): ProofStatus {
		return this.proofStatus;
	}
	protected updateLabel (): void {
		const proofStatus: ProofStatus = this.proofStatus || "untried";
		this.label = `${this.name} (${proofStatus})`; //`${this.icon}${this.name} (${proofStatus})`;
		// if (this.initialProofStatus === this.proofStatus) {
		// 	this.label = `${this.icon}${this.name} (${this.proofStatus})`;
		// } else {
		// 	this.label = `${this.icon}${this.name} (${this.initialProofStatus} - ${this.proofStatus})`;
		// }
	}
}
class GhostNode extends ProofItem {
	realNode: ProofItem;
	constructor (desc: { id?: string, parent: ProofItem, node: ProofItem }) {
		super({ type: "ghost", name: "", branchId: "", parent: desc.parent, collapsibleState: TreeItemCollapsibleState.None });
		this.realNode = desc.node;
		this.tooltip = "Waiting for proof command...";
	}
	qed (): void {
		this.label = QED;
	}
	// @overrides
	active (): void {
		super.active();
		this.label = " ...";
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
	// // @overrides
	// moveIndicatorBack (): ProofItem {
	// 	this.notActive();
	// 	this.realNode.active();
	// 	return this.realNode;
	// }
	// // @overrides
	// moveIndicatorForward (): ProofItem {
	// 	return null;
	// }
	// @overrides
	// appendSibling (sib: ProofItem, opt?: { beforeSelected?: boolean }): void {
	// 	if (this.realNode.contextValue === "root") {
	// 		this.realNode.appendChild(sib);
	// 	} else {
	// 		this.realNode.appendSibling(sib, opt);
	// 	}
	// }
	// // @overrides
	// appendChildAtBeginning (child: ProofItem): void {
	// 	this.realNode.appendChildAtBeginning(child);
	// }
	// @overrides
	appendChild (child: ProofItem): void {
		this.realNode.appendChild(child);
	}
}

