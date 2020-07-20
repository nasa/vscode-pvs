/**
 * @module VSCodePvsProofMate
 * @author Paolo Masci
 * @date 2019.12.20
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

import { TreeItem, TreeItemCollapsibleState, TreeDataProvider, EventEmitter, Event, ExtensionContext, TreeView, window, commands } from "vscode";
import { ProofState, SFormula } from "../common/languageUtils";
import { LanguageClient } from "vscode-languageclient";
import { PROOF_COMMANDS, printHelp, PROOF_TACTICS, ProofMateProfile, getCommands } from '../common/commandUtils';
import * as vscode from 'vscode';
import { ProofCommandDescriptor } from "../common/serverInterface";
import { ProofItem } from "./vscodePvsProofExplorer";

declare type ProofMateItemDescriptor = { name: string, tooltip?: string };

 /**
 * Definition of tree items
 */
class ProofMateItem extends TreeItem {
	contextValue: string = "proofmate-item";
	name: string; // prover command
	icon: string = " -  ";
	command: vscode.Command; // vscode action

    constructor (desc: ProofMateItemDescriptor) {
		super(desc.name, TreeItemCollapsibleState.None);
		this.name = desc.name;
		this.tooltip = desc.tooltip || printHelp(desc.name);
		this.label = this.icon + this.name;
		this.command = {
			title: this.name,
			command: "proof-mate.hint-clicked",
			arguments: [ { cmd: this.name } ]
		};
	}

}
abstract class ProofMateGroup extends TreeItem {
	contextValue: string = "proofmate-group";
    constructor (label: string, contextValue: string, collapsibleState: TreeItemCollapsibleState, tooltip: string) {
		super(label, collapsibleState);
		this.contextValue = contextValue;
		this.tooltip = tooltip;
	}
	getChildren (): TreeItem[] {
		return [];
	}
}
export type RecommendationRule = {
	name: string, 
	description: string, 
	commands: string[],
	test: (sequent: { succedents?: SFormula[], antecedents?: SFormula[] }) => boolean
};

//--------------------------------------------------------
// Recommendation Rules
//--------------------------------------------------------
/**
 * R1: succedent starts with `FORALL` or antecedent starts with `EXISTS` --> [ skosimp*, skeep ]
 */
export type TestFunction = (sequent: { succedents?: SFormula[], antecedents?: SFormula[] }) => boolean;
const r1a: TestFunction = (sequent: { succedents?: SFormula[], antecedents?: SFormula[] }) => {
	if (sequent && sequent.succedents) {
		for (let i = 0; i < sequent.succedents.length; i++) {
			const match: RegExpMatchArray = /^FORALL\b/g.exec(sequent.succedents[i].formula);
			if (match) {
				return true;
			}
		}
	}
	return false;
}
const r1b: TestFunction = (sequent: { succedents?: SFormula[], antecedents?: SFormula[] }) => {
	if (sequent && sequent.antecedents) {
		for (let i = 0; i < sequent.antecedents.length; i++) {
			const match: RegExpMatchArray = /^EXISTS\b/g.exec(sequent.antecedents[i].formula);
			if (match) {
				return true;
			}
		}
	}
	return false;
}
/**
 * R2: succedent starts with `EXISTS` or antecedent starts with `FORALL` --> [ inst?, insteep ]
 */
const r2a: TestFunction = (sequent: { succedents?: SFormula[], antecedents?: SFormula[] }) => {
	if (sequent && sequent.antecedents) {
		for (let i = 0; i < sequent.antecedents.length; i++) {
			const match: RegExpMatchArray = /^FORALL\b/g.exec(sequent.antecedents[i].formula);
			if (match) {
				return true;
			}
		}
	}
	return false;
}
const r2b: TestFunction = (sequent: { succedents?: SFormula[], antecedents?: SFormula[] }) => {
	if (sequent && sequent.succedents) {
		for (let i = 0; i < sequent.succedents.length; i++) {
			const match: RegExpMatchArray = /^EXISTS\b/g.exec(sequent.succedents[i].formula);
			if (match) {
				return true;
			}
		}
	}
	return false;
}
/**
 * R3: succedent in the form `LET x: X = ... IN ...` --> [ beta, skoletin ]
 */
const r3: TestFunction = (sequent: { succedents?: SFormula[], antecedents?: SFormula[] }) => {
	if (r1a(sequent) || r1b(sequent) || r2a(sequent) || r2b(sequent)) { return false; }
	if (sequent && sequent.succedents) {
		for (let i = 0; i < sequent.succedents.length; i++) {
			const match: RegExpMatchArray = /\bLET\b/g.exec(sequent.succedents[i].formula);
			if (match) {
				return true;
			}
		}
	}
	return false;
}
/**
 * R4: succedent in the form `... = IF ... THEN ... ENDIF` --> [ lift-if ]
 */
const r4: TestFunction = (sequent: { succedents?: SFormula[], antecedents?: SFormula[] }) => {
	if (r1a(sequent) || r1b(sequent) || r2a(sequent) || r2b(sequent)) { return false; }
	if (sequent) {
		if (sequent.succedents) {
			for (let i = 0; i < sequent.succedents.length; i++) {
				const match: RegExpMatchArray = /=\s*IF\b/g.exec(sequent.succedents[i].formula);
				if (match) {
					return true;
				}
			}
		}
		if (sequent.antecedents) {
			for (let i = 0; i < sequent.antecedents.length; i++) {
				const match: RegExpMatchArray = /=\s*IF\b/g.exec(sequent.antecedents[i].formula);
				if (match) {
					return true;
				}
			}
		}
	}
	return false;
}
/**
 * R4: succedent starts with `IF ... THEN ... ENDIF` or in the form `... IFF ...` --> [ split, ground ]
 */
const r5: TestFunction = (sequent: { succedents?: SFormula[], antecedents?: SFormula[] }) => {
	if (r1a(sequent) || r1b(sequent) || r2a(sequent) || r2b(sequent)) { return false; }
	if (sequent) {
		if (sequent.succedents) {
			for (let i = 0; i < sequent.succedents.length; i++) {
				const match: RegExpMatchArray = /^IF\b|\bIFF\b/g.exec(sequent.succedents[i].formula);
				if (match) {
					return true;
				}
			}
		}
	}
	return false;
}


class ProofMateHints extends ProofMateGroup {
	hints: ProofMateItem[] = [];
	constructor () {
		super("Hints", "proofmate-hints", TreeItemCollapsibleState.Expanded, "Proof Hints");
	}
	getChildren (): TreeItem[] {
		return this.hints;
	}
	addRecommendation (rec: { cmd: string, tooltip?: string }): void {
        if (rec) {
			this.hints.push(new ProofMateItem({
				name: `(${rec.cmd})`, 
				tooltip: rec.tooltip
			}));
        }
	}
	clearRecommendations (): void {
		this.hints = [];
	}
	// selectProfile (profile: ProofMateProfile): void {

	// }
}

/**
 * Proof mate provides a sketchpad where proof explorer 
 * can save proof fragments produced during proof attempts,
 * that otherwise would be thrown away.
 * This tipically happens when the proof structure changes 
 * after a spec change or editing of proof commands
 */
class ProofMateSketchpad extends ProofMateGroup {
	clips: ProofItem[] = [];
	constructor () {
		super("Sketchpad", "proofmate-sketchpad", TreeItemCollapsibleState.Expanded, "Proof Sketches");
	}
	getChildren (): TreeItem[] {
		return this.clips;
	}
	push (items: ProofItem[]): void {
		const updateCommands = (items: ProofItem[]): boolean => {
			let hasContent: boolean = false;
			if (items) {
				for (let i = 0; i < items.length; i++) {
					items[i].icon = " -  ";
					items[i].command = {
						title: items[i].name,
						command: "proof-mate.hint-clicked",
						arguments: [ { cmd: items[i].name } ]
					};
					items[i].label = items[i].icon + items[i].name;
					if (!hasContent && items[i].contextValue === "proof-command") {
						hasContent = true;
					}
					updateCommands(items[i].children);
				}
			}
			return hasContent;
		}
		if (items && items.length) {
			if (updateCommands(items)) {
				this.clips = items.concat(this.clips);
			}
		}
	}
	clear (): void {
		this.clips = [];
	}

}

/**
 * Data provider for PVS Proof Mate view
 */
export class VSCodePvsProofMate implements TreeDataProvider<TreeItem> {
	/**
	 * Events for updating the tree structure
	 */
	protected _onDidChangeTreeData: EventEmitter<TreeItem> = new EventEmitter<TreeItem>();
    readonly onDidChangeTreeData: Event<TreeItem> = this._onDidChangeTreeData.event;

	protected formulaDescriptor: { contextFolder: string, fileName: string, fileExtension: string, theoryName: string, formulaName: string };

	protected profile: ProofMateProfile;

	protected context: ExtensionContext;	
	protected client: LanguageClient;
	protected providerView: string;
	protected view: TreeView<TreeItem>;

	protected visible: boolean = false;

	// proof descriptor
	protected desc: { fileName: string, fileExtension: string, theoryName: string, formulaName: string, contextFolder: string };

	// elements in the view
	protected hints: ProofMateHints;
	protected sketchpad: ProofMateSketchpad;

	// protected autorunFlag: boolean = false;

	// rules for computing hints
	protected recommendationRules: RecommendationRule[] = [
		{ name: "forall", description: "Remove universal quantifier in succedent formula", test: r1a, commands: [ "skosimp*", "skeep" ] },
		{ name: "forall", description: "Remove existential quantifier in antecedent formula", test: r1b, commands: [ "skosimp*", "skeep" ] },
		{ name: "exists", description: "Remove universal quantifier in antecedent formula", test: r2a, commands: [ "inst?", "insteep" ] },
		{ name: "exists", description: "Remove existential quantifier in succedent formula", test: r2b, commands: [ "inst?", "insteep" ] },
		{ name: "let-in", description: "Remove let-in", test: r3, commands: [ "beta", "skoletin" ] },
		{ name: "lift-if", description: "Brings if-then-else to the top level", test: r4, commands: [ "lift-if" ] },
		{ name: "split", description: "Split cases", test: r5, commands: [ "split", "ground" ] }
	]

	// autorunStop (): void {
	// 	this.autorunFlag = false;
	// }
	// autorunStart (): void {
	// 	this.autorunFlag = true;
	// }
	hideView (): void {
		this.visible = false;
		vscode.commands.executeCommand('setContext', 'proof-mate.visible', false);
	}
	revealView (): void {
		this.visible = true;
		vscode.commands.executeCommand('setContext', 'proof-mate.visible', true);
	}


	/**
	 * Constructor
	 * @param client Language client for exchanging data and command with the language server
	 * @param providerView Name of the VSCode view associated to this data provider
	 */
	constructor(client: LanguageClient, providerView: string) {
		this.client = client;
		this.providerView = providerView;

		this.hints = new ProofMateHints();
		this.sketchpad = new ProofMateSketchpad();

		// enable basic profile by default
		this.profile = "basic";

		// register data provider
		this.view = window.createTreeView(this.providerView, { treeDataProvider: this, showCollapseAll: false });
	}
	
	/**
	 * Refresh tree view
	 */
	protected refreshView(): void {
		if (this.visible) {
			this._onDidChangeTreeData.fire();
		}
	}
	/**
	 * Reset tree view
	 */
	resetView (): void {
		this.hints.clearRecommendations();
		this.refreshView();
	}
	/**
	 * Internal function, reveals a node in the view.
	 */
	protected revealNode (selected: TreeItem): void {
		if (selected) {
			// there is something I don't understand in the APIs of TreeItem 
			// because I'm getting exceptions (node not found / element already registered)
			// when option 'select' is set to true.
			// Sometimes the exception occurs also with option 'expand'
			this.view.reveal(selected, { expand: 2, select: true, focus: true }).then(() => {
			}, (error: any) => {
				// console.error(desc);
				// console.error(error);
			});
		}
	}
    /**
	 * Handler activation function
	 * @param context Client context 
	 */
	activate(context: ExtensionContext) {
		this.context = context;
		this.selectProfile("basic");
		context.subscriptions.push(commands.registerCommand("proof-mate.exec-proof-command", (resource: ProofMateItem | ProofItem) => {
			if (resource && resource.name) {
				this.sendProofCommand(resource.name);
			} else {
				console.warn(`[proof-mate] Warning: action exec-proof-command is trying to use a null resource`);
			}
		}));
		context.subscriptions.push(commands.registerCommand("proof-mate.send-to-terminal", (resource: ProofMateItem | ProofItem) => {
			if (resource && resource.name) {
				const dd = { 
					fileName: this.desc.fileName,
					fileExtension: this.desc.fileExtension,
					contextFolder: this.desc.contextFolder,
					theoryName: this.desc.theoryName, 
					formulaName: this.desc.formulaName,
					cmd: resource.name
				}
				commands.executeCommand("proof-mate.proof-command-dblclicked", dd);
		} else {
				console.warn(`[proof-mate] Warning: action exec-proof-command is trying to use a null resource`);
			}
		}));
		context.subscriptions.push(commands.registerCommand("proof-mate.show-sequent", () => {
			commands.executeCommand("proof-explorer.show-active-sequent");
        }));
		context.subscriptions.push(commands.registerCommand("proof-mate.activate-basic-profile", () => {
			this.selectProfile("basic");
        }));
		context.subscriptions.push(commands.registerCommand("proof-mate.activate-advanced-profile", () => {
			this.selectProfile("advanced");
		}));

		let cmd: string = null;
		context.subscriptions.push(commands.registerCommand("proof-mate.hint-clicked", (desc: { cmd: string }) => {
			// register double click handler
			if (desc) {
				if (!cmd || cmd !== desc.cmd) {
					cmd = desc.cmd;
					setTimeout(() => {
						cmd = null
					}, 250);	
				} else {
					const dd = { 
						fileName: this.desc.fileName,
						fileExtension: this.desc.fileExtension,
						contextFolder: this.desc.contextFolder,
						theoryName: this.desc.theoryName, 
						formulaName: this.desc.formulaName,
						cmd: desc.cmd
					}
					commands.executeCommand("proof-mate.proof-command-dblclicked", dd);
					cmd = null;
				}
			}
		}));
	
	}

	setProofDescriptor (desc: { fileName: string, fileExtension: string, theoryName: string, formulaName: string, contextFolder: string }): void {
		this.desc = desc;
	}

	sendProofCommand (cmd: string): void {
		if (this.desc) {
			commands.executeCommand("vscode-pvs.send-proof-command", {
				fileName: this.desc.fileName,
				fileExtension: this.desc.fileExtension,
				theoryName: this.desc.theoryName,
				formulaName: this.desc.formulaName,
				contextFolder: this.desc.contextFolder,
				cmd: cmd.startsWith("(") ? cmd : `(${cmd.trim()})`
			});
		} else {
			console.warn(`[proof-mate] Warning: could not send proof command (please set proof descriptor before trying to send any command)`)
		}
	}

	selectProfile (profile: ProofMateProfile): void {
		this.profile = profile;
		vscode.commands.executeCommand('setContext', 'basic-profile-active', profile === "basic");
		vscode.commands.executeCommand('setContext', 'advanced-profile-active', profile === "advanced");
		this.refreshView();
		// forward the selection to PvsCli via event-dispatcher and cli-gateway
		commands.executeCommand("vscode-pvs.select-profile", {
			profile
		});
	}

	updateRecommendations (proofState: ProofState): void {
		if (proofState) {
			if (this.visible) {
				this.resetView();
				const recs: { cmd: string, tooltip?: string }[] = this.getRecommendations(proofState);
				if (recs) {
					for (let i in recs) {
						this.hints.addRecommendation(recs[i]);
					}
				}
				this.refreshView();
				// this.addRecommendations([ { cmd: "skosimp*", tooltip: "Removes universal quantifier" } ]);
			}
		} else {
			console.warn(`[proof-mate] Warning: null sequent`);
		}
	}
	getRecommendations (proofState: ProofState): { cmd: string, tooltip?: string }[] {
		const ans: { cmd: string, tooltip?: string }[] = [];
		if (proofState && proofState.sequent) {
			for (let i in this.recommendationRules) {
				if (this.recommendationRules[i].test(proofState.sequent)) {
					for (let j in this.recommendationRules[i].commands) {
						ans.push({
							cmd: this.recommendationRules[i].commands[j]//, 
							// tooltip: this.recommendationRules[i].description
						});
					}
				}	
			}
		}
		if (ans.length === 0) {
			ans.push({ cmd: "assert", tooltip: "Simplify using a combination of decision procedures"})
			ans.push({ cmd: "grind", tooltip: "Install rewrites and repeatedly simplify"});
		}
		return ans;
	}
	/**
	 * Utility function, updates adds a new item to the sketchpad
	 * @param desc 
	 */
	updateSketchpad (desc: { items: ProofItem[] }): void {
		if (desc && desc.items && desc.items.length) {
			this.sketchpad.push(desc.items);
			this.revealNode(desc.items[0]);
			this.refreshView();
		}
	}
	/**
	 * Utility function, used to identify which formula is being proved in the proof tree session
	 * @param desc 
	 */
	loadFormulaDescriptor (desc: { fileName: string, fileExtension: string, theoryName: string, formulaName: string, contextFolder: string, autorun?: boolean }): void {
		this.formulaDescriptor = desc;
	}
	startProof (): void {
		this.sketchpad.clear();
	}

	
	/**
	 * Returns the list of theories defined in the active pvs file
	 * @param item Element clicked by the user 
	 */
	getChildren(item: TreeItem): Thenable<TreeItem[]> {
		if (item) {
			const group: ProofMateGroup = <ProofMateGroup> item;
			return Promise.resolve(group.getChildren());
		}
		// root node
		return Promise.resolve([ this.hints, this.sketchpad ]);
	}
	getTreeItem(item: TreeItem): TreeItem {
		return item;
    }

}
