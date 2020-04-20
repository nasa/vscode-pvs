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

declare type ProofMateItemDescriptor = { name: string, tooltip?: string };

 /**
 * Definition of tree items
 */
class ProofMateItem extends TreeItem {
	contextValue: string = "proofmate-item";
	name: string; // prover command
	// command: Command; // vscode action

    constructor (desc: ProofMateItemDescriptor) {
		super(desc.name, TreeItemCollapsibleState.None);
		this.name = desc.name;
		this.tooltip = desc.tooltip || printHelp(desc.name);
	}
}
abstract class ProofMateGroup extends TreeItem {
	recommendations: ProofMateItem[] = [];
	contextValue: string = "proofmate-group";

    constructor (label: string, contextValue: string, collapsibleState: TreeItemCollapsibleState, tooltip: string) {
		super(label, collapsibleState);
		this.contextValue = contextValue;
		this.tooltip = tooltip;
	}
	clear (): void {
		this.recommendations = [];
	}
	getChildren (): ProofMateItem[] {
		return this.recommendations;
	}
}
class ProofMateRecommendedCommands extends ProofMateGroup {
	constructor () {
		super("─ Recommended ────────", "proofmate-recommended-commands", TreeItemCollapsibleState.Expanded, "Recommended proof commands");
	}
}
class ProofMateCoreCommands extends ProofMateGroup {
	constructor () {
		super("─ Frequently used ────────", "proofmate-core-commands", TreeItemCollapsibleState.Expanded, "Frequently used proof commands");
	}
	setProfile (profile: ProofMateProfile): void {
		const commands: string[] = Object.keys(getCommands(profile));
		try {
			this.recommendations = [];
			for (let i = 0; i < commands.length; i++) {
				this.recommendations.push(new ProofMateItem({
					name: commands[i],
					tooltip: printHelp(commands[i]) //CORE_PROOF_COMMANDS[commands[i]].description
				}));
			}
		} catch (init_error) {
			console.error(`[proof-mate] Error: could not initialise frequent commands list`, init_error)
		}
	}
}
class ProofMateAllCommands extends ProofMateGroup {
	constructor () {
		super("─ More... ────────", "proofmate-all-commands", TreeItemCollapsibleState.Collapsed, "More proof commands...");
	}
	setProfile (profile: ProofMateProfile): void {
		const commands: string[] = Object.keys(PROOF_COMMANDS);
		const coreCommands: { [cmd:string]: ProofCommandDescriptor } = getCommands(profile);
		try {
			this.recommendations = [];
			for (let i = 0; i < commands.length; i++) {
				if (!coreCommands[commands[i]]) {
					this.recommendations.push(new ProofMateItem({
						name: commands[i], 
						tooltip: printHelp(commands[i]) // PROOF_COMMANDS[commands[i]].description
					}));
				}
			}
		} catch (init_error) {
			console.error(`[proof-mate] Error: could not initialise full command list`, init_error)
		}
	}
}
class ProofMateProofCommands extends TreeItem {
	protected recommended: ProofMateRecommendedCommands;
	protected core: ProofMateCoreCommands;
	protected all: ProofMateAllCommands;
	constructor () {
		super("Proof commands", TreeItemCollapsibleState.Expanded);
		this.contextValue = "proofmate-proof-commands";
		this.tooltip = "Proof commands";
		this.recommended = new ProofMateRecommendedCommands();
		this.core = new ProofMateCoreCommands();
		this.all = new ProofMateAllCommands();
	}
	getChildren (): TreeItem[] {
		return [ this.recommended, this.core, this.all ];
	}
	addRecommendation (rec: { cmd: string, tooltip?: string }): void {
        if (rec) {
			this.recommended.recommendations.push(new ProofMateItem({
				name: rec.cmd, 
				tooltip: rec.tooltip
			}));
        }
	}
	clearRecommendations (): void {
		this.recommended.clear();
	}
	setProfile (profile: ProofMateProfile): void {
		this.core.setProfile(profile);
		this.all.setProfile(profile);
	}
}
class ProofMateTactic extends ProofMateGroup {
	constructor () {
		super("Proof tactics", "proofmate-tactics", TreeItemCollapsibleState.Collapsed, "Proof tactics");
		const commands: string[] = Object.keys(PROOF_TACTICS);
		try {
			for (let i = 0; i < commands.length; i++) {
				this.recommendations.push(new ProofMateItem({
					name: commands[i],
					tooltip: PROOF_TACTICS[commands[i]].description
				}));
			}
		} catch (init_error) {
			console.error(`[proof-mate] Error: could not initialise proof tactics list`, init_error)
		}
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


/**
 * Data provider for PVS Proof Mate view
 */
export class VSCodePvsProofMate implements TreeDataProvider<TreeItem> {
	/**
	 * Events for updating the tree structure
	 */
	protected _onDidChangeTreeData: EventEmitter<TreeItem> = new EventEmitter<TreeItem>();
    readonly onDidChangeTreeData: Event<TreeItem> = this._onDidChangeTreeData.event;

	protected mode: ProofMateProfile;

	protected context: ExtensionContext;	
	protected client: LanguageClient;
	protected providerView: string;
	protected view: TreeView<TreeItem>;

	// proof descriptor
	protected desc: { fileName: string, fileExtension: string, theoryName: string, formulaName: string, contextFolder: string };

	// elements in the view
	// protected recommended: ProofMateRecommendedCommands;
	// protected core: ProofMateCoreCommands;
	// protected all: ProofMateAllCommands;
	protected proofCommands: ProofMateProofCommands;
	protected tactics: ProofMateTactic;

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

	/**
	 * Constructor
	 * @param client Language client for exchanging data and command with the language server
	 * @param providerView Name of the VSCode view associated to this data provider
	 */
	constructor(client: LanguageClient, providerView: string) {
		this.client = client;
		this.providerView = providerView;

		this.proofCommands = new ProofMateProofCommands();
		this.tactics = new ProofMateTactic();

		// enable basic profile by default
		this.mode = "basic";

		// register data provider
		this.view = window.createTreeView(this.providerView, { treeDataProvider: this, showCollapseAll: false });
	}

	enableProfile(profile: ProofMateProfile): void {
		switch (profile) {
			case "basic": {
				// enable basic profile
				this.mode = "basic";
				vscode.commands.executeCommand('setContext', 'basic-profile-active', true);
				vscode.commands.executeCommand('setContext', 'advanced-profile-active', false);
				this.proofCommands.setProfile(this.mode);
				this.refreshView();
				break;
			}
			case "advanced": {
				// enable advanced profile
				this.mode = "advanced";
				vscode.commands.executeCommand('setContext', 'basic-profile-active', false);
				vscode.commands.executeCommand('setContext', 'advanced-profile-active', true);
				this.proofCommands.setProfile(this.mode);
				this.refreshView();
				break;
			}
			default: {
				console.warn(`[proof-mate] Warning: trying to activate unknown profile ${profile}`);
			}
		}
	}
	
	/**
	 * Refresh tree view
	 */
	protected refreshView(): void {
		this._onDidChangeTreeData.fire();
	}
	/**
	 * Reset tree view
	 */
	resetView (): void {
		this.proofCommands.clearRecommendations();
		this.refreshView();
	}

    /**
	 * Handler activation function
	 * @param context Client context 
	 */
	activate(context: ExtensionContext) {
		this.context = context;
		this.enableProfile("basic");
		context.subscriptions.push(commands.registerCommand("proof-mate.exec-proof-command", (resource: ProofMateItem) => {
			if (resource && resource.name) {
				this.sendProofCommand(resource.name);
			} else {
				console.warn(`[proof-mate] Warning: action exec-proof-command is trying to use a null resource`);
			}
		}));
		context.subscriptions.push(commands.registerCommand("proof-mate.show-sequent", () => {
			commands.executeCommand("proof-explorer.show-active-sequent");
        }));
		context.subscriptions.push(commands.registerCommand("proof-mate.activate-basic-profile", () => {
			this.enableProfile("basic");
        }));
		context.subscriptions.push(commands.registerCommand("proof-mate.activate-advanced-profile", () => {
			this.enableProfile("advanced");
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
				cmd: cmd.startsWith("(") ? cmd : `(${cmd})`
			});
		} else {
			console.warn(`[proof-mate] Warning: could not send proof command (please set proof descriptor before trying to send any command)`)
		}
	}

	updateRecommendations (proofState: ProofState): void {
		if (proofState) {
			this.resetView();
			const recs: { cmd: string, tooltip?: string }[] = this.getRecommendations(proofState);
			if (recs) {
				for (let i in recs) {
					this.proofCommands.addRecommendation(recs[i]);
				}
			}
			this.refreshView();
			// this.addRecommendations([ { cmd: "skosimp*", tooltip: "Removes universal quantifier" } ]);
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
	 * Returns the list of theories defined in the active pvs file
	 * @param item Element clicked by the user 
	 */
	getChildren(item: TreeItem): Thenable<TreeItem[]> {
		if (item) {
			const group: ProofMateGroup = <ProofMateGroup> item;
			return Promise.resolve(group.getChildren());
		}
		// root node
		return Promise.resolve([ this.proofCommands, this.tactics ]);
	}
	getTreeItem(item: TreeItem): TreeItem {
		return item;
    }

}
