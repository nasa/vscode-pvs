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

import { LanguageClient } from "vscode-languageclient";
import * as vscode from 'vscode';
import * as vscodeUtils from "../utils/vscode-utils";
import * as path from 'path';
import { Explorer, ProofBranch, ProofCommand, ProofItem, RootNode } from "./vscodePvsProofExplorer";
import { ProofEditCopyNode, ProofEditCopyTree, ProofMateProfile, ProofNode, ProofNodeType, ProofNodeX, PvsFormula, PvsProofCommand, SequentDescriptor, serverEvent, serverRequest, SFormula } from "../common/serverInterface";
import { openSketchpad, saveSketchpad } from "../common/fsUtils";
import { proverCommands } from "../common/languageUtils";
import { TreeItem, TreeItemCollapsibleState } from "vscode";
import { VSCodePvsXTerm } from "./vscodePvsXTerm";
import * as fsUtils from "../common/fsUtils";

declare type ProofMateItemDescriptor = {
	name: string, 
	tooltip?: string, 
	branchId?: string, 
	parent?: ProofItem, 
	sketchpadLabel?: SketchpadLabel,
	collapsibleState?: TreeItemCollapsibleState,
	type?: ProofNodeType
};

// abstract types
const PROOFMATE_ITEM: string = "proofmate-item";
const PROOFMATE_GROUP: string = "proofmate-group";
// group types
const PROOFMATE_HINTS: string = "proofmate-hints";
const PROOFMATE_SKETCHPAD: string = "proofmate-sketchpad";
// concrete types
const PROOFMATE_HINT: string = "proofmate-hint";
const PROOFMATE_CLIP: string = "proofmate-clip";
const PROOFMATE_LABEL: string = "proofmate-label";

/**
 * Abstract proofmate item -- this is the base class for hints and sketchpad items
 */
abstract class ProofMateItem extends ProofItem {
	// @override
	contextValue: string = PROOFMATE_ITEM;
	// @override
	children: ProofMateItem[] = [];
	// @override
	parent: ProofMateItem;

	// additional fields added by ProofMateItem
	sketchpadLabel?: SketchpadLabel; // pointer to the sketchpad label
	type?: ProofNodeType; // stored information abour the original proof item contextValue, indicating whether this node is a proof-command or a proof-branch

	/**
	 * Constructor
	 */
    constructor (desc: ProofMateItemDescriptor) {
		super({
			type: PROOFMATE_ITEM,
			name: desc?.name,
			branchId: desc?.branchId,
			parent: desc?.parent,
			collapsibleState: desc?.collapsibleState !== undefined && desc?.collapsibleState !== null ?
				desc.collapsibleState 
					: TreeItemCollapsibleState.None
		});
		this.name = desc?.name;
		this.tooltip = desc?.tooltip || "";
		this.label = this.name;
		this.type = desc?.type;
		this.sketchpadLabel = desc?.sketchpadLabel;
		this.command = {
			title: this.name,
			command: "proof-mate.did-click-item",
			arguments: [ { cmd: this.name } ]
		};
		this.iconPath = new vscode.ThemeIcon("debug-stackframe-dot");
		// this.iconPath = {
		// 	light: path.join(__dirname, "..", "..", "..", "icons", "svg-dot-gray.svg"),
		// 	dark: path.join(__dirname, "..", "..", "..", "icons", "svg-dot-white.svg")			
		// };
	}

	/**
	 * @override
	 */
	getType(): ProofNodeType {
		return (this.contextValue === PROOFMATE_CLIP) ? this.type : "root";
	}
	/**
	 * Returns true if this is a proof command
	 */
	isProofCommand (): boolean {
		return this.type === "proof-command";
	}
	/**
	 * Returns true if this is a proof command
	 */
	isProofBranch (): boolean {
		return this.type === "proof-branch";
	}
	/**
	 * Returns true if this is a proof command
	 */
	isClip (): boolean {
		return this.isProofBranch() || this.isProofCommand();
	}
	/**
	 * Internal function, rebases tree branches
	 * TODO: this function is almost identical to pvsProofExplorer.rebaseTree -- create base class so code can be reused
	 */
	rebaseTree (targetId: string, opt?: { forceTargetId?: boolean }): void {
		opt = opt || {};
		switch (this.type) {
			case "proof-command": {
				this.branchId = targetId;
				break;
			}
			case "proof-branch": {
				if (opt.forceTargetId) {
					// targetId should be forced only once, at the beginning
					// this corner case is used to handle pasteing of a proof branch under a proof command
					opt.forceTargetId = false;
					this.branchId = targetId;
				} else {
					const branchName: string = this.branchId.split(".").slice(-1)[0];
					this.branchId = targetId ? `${targetId}.${branchName}` : branchName;
				}
				// const oldName: string = this.name;
				this.label = this.name = `(${this.branchId})`;
				break;
			}
			case "root": {
				this.branchId = targetId;
				break;
			}
			default: {
				break;
			}
		}
		// update targetId to propagate branch name to the children
		targetId = this.branchId;
		for (let i = 0; i < this.children?.length; i++) {
			this.children[i].rebaseTree(targetId);
		}
	}
	/**
	 * Returns the parent node -- the signature of this function is imposed by the APIs of vscode
	 */
	getParent (): ProofMateItem {
		return this.parent || this.sketchpadLabel;
	}
	/**
	 * Returns the first child
	 */
	getFirstChild (): ProofMateItem {
		return this.children?.length ? this.children[0] : null; 
	}
	/**
	 * Returns the next node in the sketchpad
	 */
	getNext (opt?: { siblingOnly?: boolean }): ProofMateItem {
		let candidate: ProofMateItem = opt?.siblingOnly ? this.getNextSibling()
			: this.getFirstChild() || this.getNextSibling();
		if (!candidate && this.parent && this.parent !== this) {
			return this.parent?.getNext({ siblingOnly: true })
		}
		return candidate;
	}
	/**
	 * Returns the next sibling
	 */
	getNextSibling (): ProofMateItem {
		const parent: ProofMateItem = this.getParent();
		if (parent) {
			const children: ProofMateItem[] = parent.children;
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
	// /**
	//  * Returns the previous node in the sketchpad
	//  */
	// getPrevious (): ProofMateItem {
	// 	return this.getPreviousSibling() || this.parent?.getLastSibling();
	// }
	// /**
	//  * Returns the previous sibling
	//  */
	// getPreviousSibling (): ProofMateItem {
	// 	if (this.parent) {
	// 		const children: ProofMateItem[] = this.parent.children;
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
	/**
	 * Returns the last proof command in the subtree rooted at the node
	 */
	getLastChildInSubtree (): ProofMateItem {
		if (this.children?.length) {
			const candidate: ProofMateItem = this.children[this.children.length - 1];
			return candidate.getLastChildInSubtree();
		}
		return this;
	}
	/**
	 * Appends children to the given node
	 */
	// appendChildren (children: ProofMateItem[], opt?: { prepend?: boolean }): void {
	// 	if (children) {
	// 		this.children = this.children ?
	// 			opt?.prepend ? children.concat(this.children) : this.children.concat(children)
	// 			 	: children;
	// 	}
	// }
	/**
	 * Appends child as first child
	 */
	appendChildAtBeginning (child: ProofMateItem, opt?: { rebase?: boolean }): void {
		if (child) {
			child.parent = this; // this makes sure the child is pointing to the right parent
			opt = opt || {};
			this.children = this.children || [];
			if (opt.rebase) {
				// count the number of proof branches under this node -- this will be used in the case we are pasteing a proof branch
				const n: number = this?.children?.filter(elem => {
					return elem.getType() === "proof-branch";
				})?.length || 0;
				// adjust branch id for the node being pasted
				let targetId: string = 
					// if the child is a proof branch
					// rename the child as the currentBranchID.x, where x is n + 1
					child.getType() === "proof-branch" ? `${this.branchId}.${n + 1}`
					// otherwise use currentProofBranchID
						: this.branchId;
				targetId = targetId.startsWith(".") ? targetId.substring(1) : targetId;
				child.rebaseTree(targetId, { forceTargetId: child.getType() === "proof-branch" });
			}
			if (child.getType() === "root") {
				this.children = child.children.concat(this.children);
			} else {
				this.children = [ child ].concat(this.children);
			}
		}
	}
	/**
	 * Appends the given node as a sibling of the current node
	 * TODO: this function is nearly identical to pvsProofExplorer.appendSibling, create a base class so we can reuse code
	 */
	appendSibling (sib: ProofMateItem, opt?: { beforeSelected?: boolean, rebase?: boolean }): void {
		if (sib) {
			sib.parent = this.parent; // make sure the sibling and the current node have the same parent
			opt = opt || {};
			let children: ProofMateItem[] = [];
			const parent: ProofMateItem = this.getParent();
			if (parent) {
				const n: number = parent.children?.length || 0;
				for (let i = 0; i < n; i++) {
					if (!opt.beforeSelected) {
						children.push(parent.children[i]);
					}
					if (parent.children[i].id === this.id) {
						if (opt.rebase) {
							// adjust branch id for the node being pasted so it's identical to the id of the other siblings
							// use the previous child as reference
							sib.rebaseTree(parent.children[i].branchId);
						}
						if (sib.contextValue === "root") { // if the node to be appended is a root node, we append its children
							children = children.concat(sib.children);
						} else {
							children.push(sib);
						}
					}
					if (opt.beforeSelected) {
						children.push(parent.children[i]);
					}
				}
				parent.children = children;
			} else {
				console.warn("[proof-mate] Warning: trying to append sibling to a node without parent", this, sib);
			}
		}
	}
	/**
	 * Appends siblings after the current node (this)
	 */
	// appendSiblings (sibs: ProofMateItem[], opt?: { rebase?: boolean }): boolean {
	// 	if (sibs?.length) {
	// 		// append after the selected node
	// 		const parent: ProofMateItem = this.getParent();
	// 		if (opt?.rebase) {
	// 			const targetId: string = this.branchId || "";
	// 			for (let i = 0; i < sibs.length; i++) {
	// 				sibs[i].rebaseTree(targetId);
	// 			}
	// 		}
	// 		if (parent?.children?.length) {
	// 			const idx: number = parent.children.indexOf(this);
	// 			if (idx >= 0) {
	// 				const pre: ProofMateItem[] = parent.children.slice(0, idx + 1);
	// 				const post: ProofMateItem[] = parent.children.slice(idx + 1);
	// 				const newChildren: ProofMateItem[] = pre.concat(sibs).concat(post);
	// 				parent.children = newChildren;
	// 				return true;
	// 			}
	// 		} else {
	// 			parent.children = sibs;
	// 		}
	// 	}
	// 	return false;
	// }
	/**
	 * Clones the current node
	 * TODO: this function is nearly identical to pvsProofExplorer.clone -- defined a based class so we can reuse code
	 */
	clone (opt?: { parent?: ProofMateItem }): ProofMateItem {
		opt = opt || {};
		if (this.getType()) {
			return new ProofMateClip({
				name: this.name,
				branchId: this.branchId,
				parent: opt?.parent || this.parent,
				sketchpadLabel: this.sketchpadLabel,
				type: this.getType(),
				collapsibleState: this.collapsibleState
			});
		}
		return null;
	}
	/**
	 * Clones the subtree rooted at the current node
	 * TODO: this function is nearly identical to pvsProofExplorer.cloneTree -- defined a based class so we can reuse code
	 */
	cloneTree (opt?: { parent?: ProofMateItem }): ProofMateItem {
		opt = opt || {};
		opt.parent = opt.parent || this.parent || null;
		const clonedRoot: ProofMateItem = this.clone(opt);
		if (this.children) {
			for (let i: number = 0; i < this.children.length; i++) {
				const child: ProofMateItem = this.children[i].cloneTree({ parent: clonedRoot });
				clonedRoot.insertChild(child, i);
			}
		}
		return clonedRoot;
	}
	/**
	 * Inserts a given child node at the given position in the list of children of the current node
	 * TODO: this function is nearly identical to pvsProofExplorer.insertChild -- defined a based class so we can reuse code
	 */
	insertChild (child: ProofMateItem, position: number): void {
		if (this.children && position < this.children.length - 1) {
			const children1: ProofMateItem[] = this.children.slice(0, position);
			const children2: ProofMateItem[] = this.children.slice(position);
			this.children = children1.concat([ child ]).concat(children2);
		} else {
			// append at the end
			// position = this.children.length;
			this.children = this.children || [];
			this.children = this.children.concat([ child ]);
		}
	}
	/**
	 * Deletes a given child node
	 * TODO: this function is nearly identical to pvsProofExplorer.deleteChild -- defined a based class so we can reuse code
	 */
	deleteChild (child: ProofMateItem, opt?: { internalAction?: boolean }): void {
		opt = opt || {};
		this.children = this.children.filter((ch: ProofItem) => {
			return ch.id !== child.id;
		});
	}
	/**
	 * Deletes the proof commands after the selected node.
	 * - If the node has children, deletes all children; 
	 * - If the node has lower siblings, deletes all lower siblings
	 * TODO: this function is nearly identical to pvsProofExplorer.trimNode -- defined a based class so we can reuse code
	 */
	 trimNode (desc: { selected: ProofMateItem }): ProofMateItem[] {
		if (desc?.selected?.getParent()) {
			const selected: ProofMateItem = desc.selected;
			let items: ProofMateItem[] = null;
			switch (selected.getType()) {
				case "root": {
					items = selected.children;
					selected.children = [];
					break;
				}
				case "proof-branch": {
					// remove all children in this branch
					items = selected.children;
					selected.children = [];
					break;
				}
				case "proof-command": {	
					// remove children if any
					items = selected.children;
					selected.children = [];
					// remove also lower siblings
					const parent: ProofMateItem = selected.getParent();
					if (parent) {
						const idx: number = parent.children.indexOf(selected);
						items = items.concat(parent.children.slice(idx + 1, parent.children.length + 1));
						parent.children = parent.children.slice(0, idx + 1);
					}
					break;
				}
				default: {
					console.warn(`[proof-mate] Warning: unrecognized node type ${selected.getType()} detected while trimming ${selected.name}`);
				}
			}
			return items;
		} else {
			console.warn(`[proof-mate] Warning: unable to trim selected node`);
		}
		return null;
	}
	/**
	 * @override
	 * Utility function, updates the icon of the tree item based on the value of the status flags of the item
	 */
	protected updateIcon (): void {
		super.updateIcon();
		if (this.visitedFlag) {
			this.iconPath = new vscode.ThemeIcon("circle-filled");// star-full, new vscode.ThemeColor("pvs.orange"));
		} else if (this.activeFlag) {
			this.iconPath = {
				light: path.join(__dirname, "..", "..", "..", "icons", "svg-orange-diamond.svg"),
				dark: path.join(__dirname, "..", "..", "..", "icons", "svg-orange-diamond.svg")
			};	
		}
	}
}

//--------------------------------------------------------
// Recommendation Rules
//--------------------------------------------------------
export type RecommendationRule = {
	name: string, 
	description: string, 
	commands: string[],
	test: (sequent: { succedents?: SFormula[], antecedents?: SFormula[] }) => boolean
};
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
 * Utility class for rendering proof mate hints
 */
export class ProofMateHint extends ProofMateItem {
	contextValue: string = PROOFMATE_HINT;
}
/**
 * Utility class for rendering proof mate clips
 */
export class ProofMateClip extends ProofMateItem {
	contextValue: string = PROOFMATE_CLIP;
}

/**
 * Abstract proof mate group -- this is the base class for hints and sketchpad groups
 */
abstract class ProofMateGroup extends ProofMateItem {
	contextValue: string = PROOFMATE_GROUP;
}

/**
 * Utility class for rendering proof mate hints
 */
class ProofMateHints extends ProofMateGroup {
	// @override
	contextValue = PROOFMATE_HINTS;
	/**
	 * Constructor
	 */
	constructor () {
		super({
			name: "Hints",
			collapsibleState: TreeItemCollapsibleState.Expanded,
			tooltip: "Proof Hints"
		});
		// the full list of codeicos is at https://microsoft.github.io/vscode-codicons/dist/codicon.html
		this.iconPath = new vscode.ThemeIcon("lightbulb", new vscode.ThemeColor("pvs.yellow"));
		this.command = {
			title: this.contextValue,
			command: "proof-mate.hints.root-selected",
			arguments: [ this ]
		};
		this.parent = this;
	}
	/**
	 * Adds a recommendation to the hints
	 */
	addRecommendation (rec: { cmd: string, tooltip?: string }): void {
        if (rec) {
			const item: ProofMateHint = new ProofMateHint({
				name: `(${rec.cmd})`, 
				tooltip: rec.tooltip
			});
			this.children.push(item);
        }
	}
	/**
	 * Clears the recommendations
	 */
	clearRecommendations (): void {
		this.children = [];
	}
}

/**
 * Utility class, creates a sketchpad label
 */
class SketchpadLabel extends ProofMateItem {
	contextValue: string = PROOFMATE_LABEL;
	// @override
	children: ProofMateClip[] = [];
	// sequence indicating the execution order of nodes in the sketchpad clip (first depth visit)
	// seq: ProofMateClip[] = [];
	/**
	 * Constructor
	 * The sketchpad name is typically given the form of a date/time string indicating when the sketchpad has been created
	 */
	constructor (desc: { name: string, group: ProofMateGroup }) {
		super({
			name: desc?.name || new Date().toLocaleString(), 
			branchId: '', 
			parent: null, 
			collapsibleState: TreeItemCollapsibleState.Expanded
		});
		this.iconPath = new vscode.ThemeIcon("pinned");
		this.tooltip = "";
		this.command = null; 
		// = {
		// 	title: this.name,
		// 	command: "proof-mate.did-click-sketchpad-label",
		// 	arguments: [ { cmd: this.name } ]
		// };
		this.parent = desc?.group;
		this.sketchpadLabel = this;
	}
	// @override
	updateIcon (): void {
		// do nothing
	}
}

/**
 * Proof mate provides a sketchpad where proof explorer 
 * can save proof fragments produced during proof attempts,
 * that otherwise would be thrown away.
 * This tipically happens when the proof structure changes 
 * after a spec change or editing of proof commands
 */
class Sketchpad extends ProofMateGroup {
	// @override
	contextValue: string = PROOFMATE_SKETCHPAD;
	// @override
	children: SketchpadLabel[] = [];
	/**
	 * Constructor
	 */
	constructor () {
		super({
			name: "Sketchpad",
			collapsibleState: TreeItemCollapsibleState.Expanded,
			tooltip: "Proof Sketches"
		});
		// the full list of codeicons is at https://microsoft.github.io/vscode-codicons/dist/codicon.html
		this.iconPath = new vscode.ThemeIcon("clippy");
		this.command = {
			title: this.contextValue,
			command: "proof-mate.sketchpad.root-selected",
			arguments: [ this ]
		};
		this.parent = this;
	}
	/**
	 * Adds a node to the sketchpad
	 */
	add (desc: {
		selected: ProofMateItem, 
		items: ProofItem[]
	}, opt?: {
		label?: string,
		prepend?: boolean, // false by default
		force?: boolean // false by default
	}): ProofMateItem[] {
		if (desc && desc.items?.length && desc.selected) {
			const label: SketchpadLabel = 
				// if the selected item is the sketchpad root, then create a new sketchpad label
				desc.selected?.contextValue === PROOFMATE_SKETCHPAD ? new SketchpadLabel({ name: opt?.label, group: this })
				// if the selected item is a sketchpad label, then use the item
				: desc.selected?.contextValue === PROOFMATE_LABEL ? <SketchpadLabel> desc.selected
				// otherwise it's a clip -- get the sketchpad label from the clip
				: desc.selected?.sketchpadLabel;
			if (desc.selected?.contextValue === PROOFMATE_SKETCHPAD) {
				// append new label to the sketchpad
				this.children = [ label ].concat(this.children);
			}
			const parent: ProofMateItem = 
				desc.selected?.contextValue === PROOFMATE_CLIP ?
					desc.selected.isProofCommand() && desc.items[0].contextValue === "proof-command" ?
						desc.selected.getParent() 
							: desc.selected
					: undefined;
			let newClips: ProofMateItem[] = this.createClips(desc.items, label, {
				parent, force: opt?.force
			});
			if (newClips?.length) {
				const parent: ProofMateItem = desc.selected.isClip() ? desc.selected : label;
				for (let i = 0; i < newClips.length; i++) {
					this.appendNode({ selected: parent, elem: newClips[newClips.length - 1 - i] });
				}
				vscode.commands.executeCommand('setContext', "proof-mate.sketchpad-empty", false);
			}
			return newClips;
		}
		return null;
	}
	/**
	 * Returns true if the sketchpad is empty
	 */
	isEmpty (): boolean {
		return this.getFirstChild() === null;
	}
	/**
	 * Appends a new node to the proof tree.
	 * - selected node === proof command --> place the new node as sibling after the selected node (or before, when beforeSelected is true)
	 * - selected node === branch or root --> place the new node as first child 
	 * TODO: this function is nearly identical to pvsProofExplorer.appendNode, create a base class so we can reuse code
	 */
	appendNode (desc: { selected: ProofMateItem, elem: ProofMateItem }): void {
		if (desc?.selected && desc.elem) {
			const selectedNode: ProofMateItem = desc.selected;
			const newNode: ProofMateItem = desc.elem;
			switch (selectedNode.getType()) {
				case "root":
				case "proof-branch": {
					// newNode.parent = selectedNode; -- done within appendChildAtBeginning
					selectedNode.appendChildAtBeginning(newNode, { rebase: true });
					// force expanded state
					selectedNode.collapsibleState = TreeItemCollapsibleState.Expanded;
					selectedNode.id = fsUtils.get_fresh_id();
					break;
				}
				case "proof-command": {
					if (newNode.getType() === "proof-branch") {
						// proof branches are pasted as children of proof-commands
						// newNode.parent = selectedNode; -- done within appendChildAtBeginning
						selectedNode.appendChildAtBeginning(newNode, { rebase: true });
						// force expanded state
						selectedNode.collapsibleState = TreeItemCollapsibleState.Expanded;
						selectedNode.id = fsUtils.get_fresh_id();
					} else {
						// newNode.parent = selectedNode.parent; -- done within appendSibling
						selectedNode.appendSibling(newNode, { rebase: true });
					}
					break;
				}
				default: {
					// do nothing
				}
			}
		}
	}
	/**
	 * Returns a pointer to the first (ie., most recent) clip in the sketchpad
	 */
	getRecentClip (): ProofMateItem {
		if (this.children?.length && this.children[0].children?.length) {
			return this.children[0].children[0];
		}
		return null;
	}
	/**
	 * Internal function, converts proof items into clips
	 */
	protected createClips (items: ProofItem[], sketchpadLabel: SketchpadLabel, opt?: {
		parent?: ProofMateItem, 
		rebase?: {
			targetId: string,
			forceName: boolean
		},
		force?: boolean
	}): ProofMateItem[] {
		opt = opt || {};
		opt.force = opt.force || false;
		// rec = rec || {};
		// rec.hasContent = opt?.force || rec.hasContent || false;
		const ans: ProofMateItem[] = [];
		if (items) {
			for (let i = 0; i < items.length; i++) {
				const type: ProofNodeType | "ghost" = items[i].getType();
				if (type !== "ghost") {
					const pitem: ProofMateItem = new ProofMateClip({
						name: items[i].name,
						parent: opt.parent,
						branchId: items[i].branchId,
						sketchpadLabel,
						type
					});
					ans.push(pitem);
					if (items[i].contextValue === "proof-command") {
						opt.force = true;
					}


					// //-- renaming logic
					// // the renaming logic is equivalent to that adopted in pvsProofExplorer.appendNode
					// // count the number of proof branches under this node -- this will be used in the case we are pasteing a proof branch
					// const n: number = pitem?.children?.filter(elem => {
					// 	return elem.getType() === "proof-branch";
					// })?.length || 0;
					// let targetId: string = 
					// 	// if the child is a proof branch
					// 	// rename the child as the currentBranchID.x, where x is n + 1
					// 	items[i].getType() === "proof-branch" ? `${pitem.branchId}.${n + 1}`
					// 	// otherwise use currentProofBranchID
					// 		: pitem.branchId || "";
					// targetId = targetId.startsWith(".") ? targetId.substring(1) : targetId;
					// //---

					// opt.rebase = { targetId, forceName: true };
					const children: ProofMateItem[] = items[i].children?.length ?
						this.createClips(items[i].children, sketchpadLabel, { ...opt, parent: pitem })
							: [];
					pitem.setChildren(children);
					// pitem.rebaseTree(opt.rebase.targetId, { forceName: opt.rebase.forceName });
				}
			}
		}
		return opt.force ? ans : null;
	}
	/**
	 * Retuns the list of clips in the sketchpad
	 */
	protected getClips (): ProofNode[] {
		const nodes: ProofNode[] = [];
		if (this.children && this.children.length) {
			for (let i = 0; i < this.children.length; i++) {
				const elem: ProofItem = this.children[i];
				if (elem.children) {
					for (let j = 0; j < elem.children.length; j++) {
						const node: ProofNode = elem.children[j].getNodeStructure();
						if (node) {
							nodes.push(node);
						}
					}
				}
			}
		}
		return nodes;
	}
	/**
	 * Utility function, saves the sketchpad to a file
	 */
	async saveSketchpad (formula: PvsFormula): Promise<boolean> {
		return await saveSketchpad(formula, this.getClips());
	}
	/**
	 * Utility function, loads the sketchpad from a file
	 */
	async loadSketchpad (formula: PvsFormula): Promise<boolean> {
		const clips: ProofNode[] = await openSketchpad(formula);
		if (clips?.length) {
			let items: ProofItem[] = [];
			for (let i = 0; i < clips.length; i++) {
				const item: ProofItem = this.proofNode2proofItem(clips[i]);
				if (item) {
					items.push(item);
				}
			}
			if (items.length) {
				this.add({ selected: this, items }, { label: "Clips from previous proof attempt", prepend: true });
			}
			return true;
		}
		return false;
	}
	/**
	 * Utility function, converts a proof node to a proof item that can be rendered in the view
	 */
	proofNode2proofItem (node: ProofNode): ProofItem {
		// utility function for building the proof tree -- see also pvsProofExplorer.loadProofDescriptor
		const createTree = (elem: ProofNode, parent: ProofItem): void => {
			const node: ProofItem = 
				(elem.type === "proof-command") ? new ProofCommand({ cmd: elem.name, branchId: elem.branch, parent }) 
				: new ProofBranch({ branchId: elem.branch, parent });
			parent.appendChild(node);
			if (elem.rules && elem.rules.length) {
				elem.rules.forEach(child => {
					createTree(child, node);
				});
			}
		}
		// initialise
		const item: ProofItem = 
			(node.type === "proof-branch") ? new ProofBranch({ branchId: node.branch, parent: null }) 
			: (node.type === "proof-command") ? new ProofCommand({ cmd: node.name, branchId: node.branch, parent: null })
			: (node.type === "root") ? new RootNode({ name: node.name }) : null;
		if (item && node.rules && node.rules.length) {
			node.rules.forEach((child: ProofNode) => {
				createTree(child, item);
			});
		}
		return item;
	}
	/**
	 * Deletes the sketchpad content
	 */
	async queryDeleteSketchpad (opt?: { queryConfirm?: boolean }): Promise<boolean> {
		opt = opt || {};
		const queryConfirm: boolean = opt?.queryConfirm === false ? false : true;
		if (queryConfirm) {
			const ans: vscodeUtils.YesCancel = await vscodeUtils.showYesCancelDialog("Delete sketchpad content?");
			if (ans !== "yes") {
				return false;
			}
		}
		this.children = [];
		vscode.commands.executeCommand('setContext', "proof-mate.sketchpad-empty", true);
		return true;
	}
	/**
	 * Clears the visited flags in the sketchpad
	 */
	async queryClearMarks (selected?: ProofMateItem, opt?: { queryConfirm?: boolean }): Promise<boolean> {
		opt = opt || {};
		const queryConfirm: boolean = opt?.queryConfirm === false ? false : true;
		if (queryConfirm && this.children?.length) {
			const msg: string = selected ? 
				selected.isClip() ? `Clear visited mark for ${selected.name}?`
					: `Clear visited marks in sketchpad ${selected.name}?`
				: "Clear all sketchpad marks?" 
			const ans: vscodeUtils.YesCancel = await vscodeUtils.showYesCancelDialog(msg);
			if (ans !== "yes") {
				return false;
			}
		}
		if (selected) {
			selected.isClip() ?
				selected.treeVisited(false) 
					: selected.treeVisited(false);
		} else {
			for (let i = 0; i < this.children?.length; i++) {
				this.children[i].treeVisited(false);
			}
		}
		return true;
	}
	/**
	 * Clears a given sketchpad label
	 */
	async queryDeleteLabel (selected: ProofMateItem, opt?: { queryConfirm?: boolean }): Promise<boolean> {
		if (selected) {
			if (opt?.queryConfirm) {
				const ans: vscodeUtils.YesCancel = await vscodeUtils.showYesCancelDialog(`Delete sketchpad ${selected.label}?`);
				if (ans !== "yes") {
					return false;
				}
			} 
			// all labels are stored under sketchpad.children
			this.children = this.children.filter((elem: TreeItem) => {
				return elem.id !== selected.id;
			});
			if (this.children.length === 0) {
				vscode.commands.executeCommand('setContext', "proof-mate.sketchpad-empty", true);
			}
			return true;
		}
		return false;
	}
	/**
	 * Deletes a given sketchpad node
	 */
	async queryDeleteNode (selected: ProofMateItem, opt?: { queryConfirm?: boolean }): Promise<boolean> {
		if (selected) {
			if (opt?.queryConfirm) {
				const ans: vscodeUtils.YesCancel = await vscodeUtils.showYesCancelDialog(`Delete ${selected.label}?`);
				if (ans !== "yes") {
					return false;
				}
			}
			const parent: ProofMateItem = selected.getParent();
			parent?.deleteChild(selected);
			return true;
		}
		return false;
	}
	/**
	 * Trims a given proof node
	 */
	async queryTrimNode (selected: ProofMateItem, opt?: { queryConfirm?: boolean }): Promise<boolean> {
		if (selected) {
			const msg: string = 
				selected.getType() === "proof-branch" ? `Delete proof commands in branch ${selected.name}?`
					: `Delete proof commands after ${selected.name}?`;
			if (opt?.queryConfirm) {
				const ans: vscodeUtils.YesCancel = await vscodeUtils.showYesCancelDialog(msg);
				if (ans !== "yes") {
					return false;
				}
			}
			selected.trimNode({ selected });
			return true;
		}
		return false;
	}

}

/**
 * Data provider for PVS Proof Mate view
 */
export class VSCodePvsProofMate extends Explorer {
	protected profile: ProofMateProfile;
	protected context: vscode.ExtensionContext;	
	protected client: LanguageClient;
	protected providerView: string;

	// indicates which node we are fast-forwarding to
	protected stopAt: TreeItem;
	// running flag, indicates whether we are running all proof commands, as opposed to stepping through the proof commands
	protected runningFlag: boolean = false;

	// xterm for sending proof commands to the server
	protected xterm: VSCodePvsXTerm;

	// lock flag, indicating whether the sketchpad will accept clips
	protected lock: boolean = false;

	// proof descriptor
	protected formula: PvsFormula;

	// elements in the view: hints and sketchpad
	protected hints: ProofMateHints;
	protected sketchpad: Sketchpad;

	// active node in the sketchpad
	protected activeNode: ProofMateItem;
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
	];

	/**
	 * Constructor
	 * @param client Language client for exchanging data and command with the language server
	 * @param providerView Name of the VSCode view associated to this data provider
	 */
	constructor(client: LanguageClient, xterm: VSCodePvsXTerm, providerView: string) {
		super();
		this.client = client;
		this.providerView = providerView;
		this.xterm = xterm;

		this.hints = new ProofMateHints();
		this.sketchpad = new Sketchpad();

		// enable basic profile by default
		this.profile = "basic";

		// register data provider
		this.view = vscode.window.createTreeView(this.providerView, { treeDataProvider: this, showCollapseAll: false });
	}
	
	/**
	 * Deletes the view
	 */
	disposeView (): void {
		this.disableView();
	}
	/**
	 * Hides the view
	 */
	disableView (): void {
		this.enabled = false;
		vscode.commands.executeCommand('setContext', 'proof-mate.visible', false);
	}
	/**
	 * Reveals the view
	 */
	enableView (): void {
		this.enabled = true;
		vscode.commands.executeCommand('setContext', 'proof-mate.visible', true);
	}
	/**
	 * Saves the sketchpad as a .jprf file
	 */
	async saveSketchpadClips (): Promise<boolean> {
		return await this.sketchpad?.saveSketchpad(this.formula);
	}
	/**
	 * Loads sketchpad clips from a .jprf file
	 */
	async loadSketchpadClips (): Promise<boolean> {
		return await this.sketchpad?.loadSketchpad(this.formula);
	}
	/**
	 * Utility function, marks the provided item as active.
	 */
	protected markAsActive (item: ProofMateItem, opt?: { markSelectedAsActive?: boolean }): boolean {
		// sanity check -- only proofmate clips can be marked as active
		if (item?.contextValue === PROOFMATE_CLIP) {
			this.activeNode?.updateStatus("not-visited");
			this.activeNode = item;
			this.activeNode?.updateStatus("active");
			if (opt?.markSelectedAsActive) {
				this.selectNode(this.activeNode);
			}
			this.refreshView();
			return true;
		}
		return false;
	}
	/**
	 * Reset tree view
	 */
	resetView (): void {
		this.hints.clearRecommendations();
		this.refreshView();
	}
	/**
	 * Utility function, places the focus on the most recent sketchpad label
	 */
	// focusSketchpad (opt?: { markSelectedAsActive?: boolean, focusActive?: boolean }): void {
	// 	const elem: TreeItem = this.sketchpad;
	// 	this.focusNode(elem);
	// 	this.refreshView();
	// 	const clip: ProofMateItem = this.sketchpad.getRecentClip();
	// 	const success: boolean = opt?.markSelectedAsActive ? this.markAsActive(clip) : false;
	// 	if (success && opt?.focusActive) {
	// 		setTimeout(() => {
	// 			this.focusNode(clip);
	// 		}, this.maxTimer * 1.2);
	// 	}
	// }
	/**
	 * Locks the sketchpad, i.e., clips will not be added
	 */
	lockSketchpad (): void {
		this.lock = true;
	}
	/**
	 * Unlocks the sketchpad, i.e., clips can be added
	 */
	unlockSketchpad (): void {
		this.lock = false;
	}
	/**
	 * Executes the active node and moves indicator forward
	 */
	async forward (opt?: { useFirstIfNull?: boolean }): Promise<boolean> {
		if (opt?.useFirstIfNull) {
			const alternate: ProofMateItem = this.sketchpad.getFirstChild()?.children?.length ?
				this.sketchpad.getFirstChild().children[0]
					: null;
			this.activeNode = this.activeNode || alternate;
		}
		if (this.activeNode?.name) {
			// sanity check
			if (this.activeNode.isProofCommand()) {
				this.lockSketchpad();
				// this.activeNode.executing();
				await this.sendProverCommand(this.activeNode?.name);
				this.unlockSketchpad();
			}
			this.moveIndicatorForward({ selectNode: true });
			return true;
		}
		return false;
	}
	/**
	 * Copies the tree rooted at the selected node to the clipboard, and all the siblings below the selected node
	 * (i.e., the clipboard will store a copy of the tree rooted at the selected node)
	 * @param desc Descriptor of the selected node.
	 * TODO: this function is nearly identical to pvsProofExplorer.copyTree -- defined a based class so we can reuse code
	 */
	copyTree (desc: { selected: ProofMateItem }): {
		clipboard: ProofMateItem[],
		seq: string
	 } {
		if (desc?.selected) {
			if (desc.selected.getType() === "root") {
				if (desc.selected && desc.selected.children && desc.selected.children.length) {
					desc.selected = desc.selected.children[0];
				}
			}
			const parent: ProofMateItem = desc.selected.getParent();
			if (parent) {
				const clipboard: ProofMateItem[] = [];
				let seq: string = "";
				const n: number = desc.selected.getType() === "proof-branch" ?
					parent.children.indexOf(desc.selected) + 1 // copy only the proof branch
						: parent.children.length; // copy everything below the proof command
				for (let i = parent.children.indexOf(desc.selected); i >= 0 && i < n; i++) {
					const item: ProofMateItem = parent.children[i];
					if (item) {
						clipboard.push(item.cloneTree());
						seq += item.printProofCommands();
					}
				}
				return { clipboard, seq };
			} else {
				console.warn(`[proof-mate] Warning: unable to identify parent of ${desc.selected.name}`);
			}
		} else {
			console.warn(`[proof-mate] Warning: unable to copy selected subtree`);
		}
		return null;
	}
    /**
	 * Activation function -- install relevant handlers
	 */
	activate(context: vscode.ExtensionContext) {
		this.context = context;
		this.selectProfile("basic");
		context.subscriptions.push(vscode.commands.registerCommand("proof-mate.run-proof", async () => {
			const success: boolean = await this.queryRunProof();
			if (success) {
				this.collapseHints();
			}
		}));
		context.subscriptions.push(vscode.commands.registerCommand("proof-mate.pause-proof", async (opt?: { source?: string, force?: boolean }) => {
			this.pauseProof(opt);
        }));
		context.subscriptions.push(vscode.commands.registerCommand("proof-mate.forward", async () => {
			const success: boolean = await this.forward({ useFirstIfNull: true });
			if (success) {
				this.collapseHints();
			}
		}));
		context.subscriptions.push(vscode.commands.registerCommand("proof-mate.fast-forward", async (resource: ProofMateItem) => {
			this.fastForwardTo(resource); // async
		}));
		context.subscriptions.push(vscode.commands.registerCommand("proof-mate.jump-here", async (resource: ProofMateItem) => {
			this.markAsActive(resource, { markSelectedAsActive: true });
		}));
		context.subscriptions.push(vscode.commands.registerCommand("proof-mate.exec-clip", async (resource: ProofMateItem) => {
			if (resource?.contextValue === PROOFMATE_CLIP) {
				const success: boolean = this.markAsActive(resource);
				if (success) {
					await this.step();
				}
			}
		}));
		context.subscriptions.push(vscode.commands.registerCommand("proof-mate.back", async () => {
			// not implemented
			// this.moveIndicatorBack({ selectNode: true });
		}));
		context.subscriptions.push(vscode.commands.registerCommand("proof-mate.run-subtree", async (resource: ProofMateItem) => {
			this.runSubtree(resource); // async
		}));
		context.subscriptions.push(vscode.commands.registerCommand("proof-mate.delete-sketchpad", async () => {
			this.queryDeleteSketchPad(); // async
		}));
		context.subscriptions.push(vscode.commands.registerCommand("proof-mate.clear-sketchpad", async (resource: ProofMateItem) => {
			await this.queryClearMarks(resource, { queryConfirm: !resource?.isClip() });
			this.activeNode.active();
		}));
		context.subscriptions.push(vscode.commands.registerCommand("proof-mate.delete-sketch", async (resource: ProofMateItem) => {
			this.queryDeleteSketchPadLabel(resource, { queryConfirm: true }); // async
		}));
		context.subscriptions.push(vscode.commands.registerCommand("proof-mate.delete-node", async (resource: ProofMateItem) => {
			this.queryDeleteNode(resource, { queryConfirm: true }); // async
		}));
		context.subscriptions.push(vscode.commands.registerCommand("proof-mate.new-proof-command", async (resource?: ProofMateItem) => {
			this.queryNewProofCommand(resource);
		}));
		context.subscriptions.push(vscode.commands.registerCommand("proof-mate.new-proof-branch", async (resource?: ProofMateItem) => {
			this.queryAddNewProofBranch(resource);
		}));
		context.subscriptions.push(vscode.commands.registerCommand("proof-mate.trim-node", async (resource?: ProofMateItem) => {
			this.queryTrimNode(resource, { queryConfirm: true });
		}));
		// context.subscriptions.push(vscode.commands.registerCommand("proof-mate.run-sketchpad", (resource: Sketchpad) => {
		// 	const clips: ProofItem[] = resource?.getChildren();
		// 	if (clips?.length) {
		// 		let seq: string = "";
		// 		for (let i = 0; i < clips.length; i++) {
		// 			seq += clips[i].printProofCommands();
		// 		}
		// 		if (seq) {
		// 			this.sendProverCommand(seq);
		// 		}
		// 	} else {
		// 		console.warn(`[proof-mate] Warning: action exec-proof-command is trying to use null resource`);
		// 	}
		// }));
		// context.subscriptions.push(vscode.commands.registerCommand("proof-mate.exec-subtree", (resource: ProofMateItem | ProofItem) => {
		// 	if (resource && resource.name) {
		// 		const seq: string = resource.printProofCommands({ markExecuted: true });
		// 		this.sendProverCommand(seq);
		// 		this.refreshView();
		// 	} else {
		// 		console.warn(`[proof-mate] Warning: action exec-proof-command is trying to use null resource`);
		// 	}
		// }));
		context.subscriptions.push(vscode.commands.registerCommand("proof-mate.edit-node", async (resource: ProofMateItem) => {
			if (resource?.name) {
				let newName: string = await vscode.window.showInputBox({ prompt: `Editing proof command ${resource.name}`, placeHolder: `${resource.name}`, value: `${resource.name}`, ignoreFocusOut: true });
				if (newName && newName !== resource.name) {
					resource.name = resource.label = newName;
					this.refreshView();
				}
			} else {
				console.warn(`[proof-mate] Warning: action edit-proof-node is trying to use null resource`);
			}
		}));
		context.subscriptions.push(vscode.commands.registerCommand("proof-mate.exec-hint", (resource: ProofMateHint) => {
			if (resource?.name) {
				resource.iconPath = new vscode.ThemeIcon("star-half");
				this.sendProverCommand(resource.name);
				this.refreshView();
			} else {
				console.warn(`[proof-mate] Warning: action exec-proof-command is trying to use null resource`);
			}
		}));
		context.subscriptions.push(vscode.commands.registerCommand("proof-mate.copy-subtree", (resource: ProofMateItem) => {
			if (resource?.name) {
				const res: { clipboard: ProofMateItem[], seq: string } = this.copyTree({ selected: resource });
				if (res?.seq && res.clipboard?.length) {
					const elems: ProofNodeX[] = [];
					for (let i = 0; i < res.clipboard.length; i++) {
						const elem: ProofNodeX = res.clipboard[i].getNodeXStructure();
						if (elem) {
							elems.push(elem);
						}
					}
					const action: ProofEditCopyTree = {
						action: "copy-tree", 
						selected: { id: resource.nodeId, name: resource.name },
						data: {
							elems, 
							seq: res.seq
						}
					};
					console.log(`[proof-mate] Copy tree rooted at ${resource.name} (${resource.nodeId})`);
					this.client.sendRequest(serverRequest.proverCommand, action);
				}
			} else {
				console.warn(`[proof-mate] Warning: action exec-proof-command is trying to use null resource`);
			}
		}));
		context.subscriptions.push(vscode.commands.registerCommand("proof-mate.copy-node", (resource: ProofMateItem) => {
			if (resource && resource.name) {
				const elem: ProofNodeX = resource.getNodeXStructure({ skipChildren: true });
				const seq: string = resource.printProofCommands({ skipChildren: true });
				const action: ProofEditCopyNode = {
					action: "copy-node", 
					selected: { id: resource.nodeId, name: resource.name },
					data: {
						elem,
						seq
					}
				};
				console.log(`[proof-mate] Copy node ${resource.name} (${resource.nodeId})`);
				this.client.sendRequest(serverRequest.proverCommand, action);
			} else {
				console.warn(`[proof-mate] Warning: action exec-proof-command is trying to use null resource`);
			}
		}));
		context.subscriptions.push(vscode.commands.registerCommand("proof-mate.paste-subtree", async (resource: ProofMateItem) => {
			const clips: ProofItem[] = this.xterm?.getProofExplorer()?.getClipboard();
			if (clips?.length) {
				this.add({ selected: resource, items: clips }, {
					prepend: true, 
					markSelectedAsActive: this.sketchpad?.isEmpty(),
					select: this.sketchpad?.isEmpty()
				});
			}
		}));
		context.subscriptions.push(vscode.commands.registerCommand("proof-mate.paste-node", async (resource: ProofMateItem) => {
			const clips: ProofItem[] = this.xterm?.getProofExplorer()?.getClipboard();
			if (clips?.length) {
				this.add({ selected: resource, items: clips }, {
					prepend: true, 
					markSelectedAsActive: this.sketchpad?.isEmpty(),
					select: this.sketchpad?.isEmpty()
				});
			}
		}));
		context.subscriptions.push(vscode.commands.registerCommand("proof-mate.activate-basic-profile", () => {
			this.selectProfile("basic");
        }));
		context.subscriptions.push(vscode.commands.registerCommand("proof-mate.activate-advanced-profile", () => {
			this.selectProfile("advanced");
		}));

		let cmd: string = null; // local variable for handling the detection of double-clicks, which are not directly supported by the APIs of vscode trees
		context.subscriptions.push(vscode.commands.registerCommand("proof-mate.did-click-item", (desc: { cmd: string }) => {
			// register double click handler
			if (desc) {
				if (!cmd || cmd !== desc.cmd) {
					cmd = desc.cmd;
					setTimeout(() => {
						cmd = null
					}, 250);	
				} else {
					const dd = { 
						fileName: this.formula.fileName,
						fileExtension: this.formula.fileExtension,
						contextFolder: this.formula.contextFolder,
						theoryName: this.formula.theoryName, 
						formulaName: this.formula.formulaName,
						cmd: desc.cmd
					}
					vscode.commands.executeCommand("proof-mate.proof-command-dblclicked", dd);
					cmd = null;
				}
			}
		}));
		context.subscriptions.push(vscode.commands.registerCommand("proof-mate.did-click-sketchpad-label", (desc: { cmd: string }) => {
			// register double click handler
		}));
		context.subscriptions.push(vscode.commands.registerCommand("proof-mate.sketchpad.root-selected", () => {
			// nothing to do
		}));
		context.subscriptions.push(vscode.commands.registerCommand("proof-mate.hints.root-selected", () => {
			// nothing to do
		}));
	}

	/**
	 * Utility function, used to identify which formula is being proved in the proof tree session
	 */
	loadFormula (formula: PvsFormula): void {
		this.formula = {
			contextFolder: formula?.contextFolder,
			fileName: formula?.fileName,
			fileExtension: formula?.fileExtension,
			theoryName: formula?.theoryName,
			formulaName: formula?.formulaName
		};
	}

	/**
	 * Sends a proof command to the terminal
	 */
	async sendProverCommand (cmd: string): Promise<boolean> {
		if (this.formula) {
			if (cmd) {
				const req: PvsProofCommand = {
					fileName: this.formula.fileName,
					fileExtension: this.formula.fileExtension,
					theoryName: this.formula.theoryName,
					formulaName: this.formula.formulaName,
					contextFolder: this.formula.contextFolder,
					cmd: cmd.startsWith("(") ? cmd : `(${cmd.trim()})`
				};
				vscode.commands.executeCommand("xterm.showFeedbackWhileExecuting", {
					...req,
					cmd: this.runningFlag ? "run-proof" : req.cmd
				});
				const success: boolean = await this.xterm?.sendProverCommand(req);
				return success;
			}
		} else {
			console.warn(`[proof-mate] Warning: could not send proof command (please set proof descriptor before trying to send any command)`)
		}
		return false;
	}

	/**
	 * Returns running flag
	 */
	isRunning (): boolean {
		return this.runningFlag;
	}

	/**
	 * Utility function, selects a proof-mate profile
	 */
	selectProfile (profile: ProofMateProfile): void {
		this.profile = profile;
		vscode.commands.executeCommand('setContext', 'basic-profile-active', profile === "basic");
		vscode.commands.executeCommand('setContext', 'advanced-profile-active', profile === "advanced");
		this.refreshView();
		// forward the selection to PvsCli via event-dispatcher and cli-gateway
		vscode.commands.executeCommand("vscode-pvs.select-profile", {
			profile
		});
	}

	/**
	 * Utility function, updates the recommendations shown in proof mate
	 */
	updateRecommendations (proofState: SequentDescriptor): void {
		if (proofState) {
			if (this.enabled) {
				this.resetView();
				const recs: { cmd: string, tooltip?: string }[] = this.getRecommendations(proofState);
				if (recs) {
					for (let i in recs) {
						this.hints.addRecommendation(recs[i]);
					}
				}
				// this.expandHints();
				this.refreshView();
				// this.addRecommendations([ { cmd: "skosimp*", tooltip: "Removes universal quantifier" } ]);
			}
		} else {
			console.warn(`[proof-mate] Warning: null sequent`);
		}
	}
	/**
	 * Utility function, returns the recommendated proof commands based on the given sequent and the set of rules of the recommendation
	 */
	getRecommendations (proofState: SequentDescriptor): { cmd: string, tooltip?: string }[] {
		const ans: { cmd: string, tooltip?: string }[] = [];
		if (proofState && proofState.sequent) {
			for (let i in this.recommendationRules) {
				if (this.recommendationRules[i].test(proofState.sequent)) {
					for (let j in this.recommendationRules[i].commands) {
						const cmd: string = this.recommendationRules[i].commands[j];
						ans.push({
							cmd, 
							tooltip: proverCommands[cmd]?.description//this.recommendationRules[i].description
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
	 * Utility function, collapses the hints
	 */
	collapseHints (): void {
		if (this.hints?.getChildren()?.length) {
			this.collapseNode(this.hints);
		}
	}
	/**
	 * Utility function, expands the hints
	 */
	expandHints (): void {
		if (this.hints?.getChildren()?.length) {
			this.expandNode(this.hints);
		}
	}
	/**
	 * Utility function, adds a new item to the sketchpad
	 */
	add (desc: { 
		selected?: ProofMateItem, 
		items: ProofItem[]
	}, opt?: { 
		label?: string,
		select?: boolean,
		markSelectedAsActive?: boolean,
		prepend?: boolean,
		focus?: boolean
	}): boolean {
		const selected: ProofMateItem = desc?.selected || this.sketchpad;
		if (selected && desc?.items?.length && !this.lock) {
			const newClips: ProofMateItem[] = this.sketchpad.add({ selected, items: desc.items }, { ...opt, force: true });
			if (newClips?.length) {
				this.selectNode(selected);
				// expand parent node so clips are visible
				if (newClips[0].parent) { this.expandNode(newClips[0].parent);}
				// apply view options
				if (opt?.markSelectedAsActive) { this.markAsActive(newClips[0]); }
				if (opt?.select) { this.selectNode(newClips[0]); }
				// disabling focus for now
				// there is something wrong with the APIs provided by vscode
				// because if the view has never been expanded, then focusNode hangs forever
				// if (opt?.focus) { this.focusNode(newClips[0]); }
				return true;
			}
		}
		return false;
	}
	/**
	 * Marks the first clips as active
	 */
	markFirstAsActive (): void {
		// mark first node as visited
		const first: ProofMateItem = this.sketchpad?.getFirstChild()?.getChildren().length ?
		<ProofMateItem> this.sketchpad?.getFirstChild()?.getChildren()[0] 
			: null;
		this.markAsActive(first);
	}
	/**
	 * Utility function, starts a new proof in proof mate
	 */
	startProof (): void {
		// keep sketchpad content
		// - this.clearSketchPad({ queryConfirm: false });
		// clear sketchpad flags
		this.sketchpad.queryClearMarks(null, { queryConfirm: false });
		// mark first node as visited
		this.markFirstAsActive();
		// reset globals
		if (this.sketchpad.isEmpty()) {
			vscode.commands.executeCommand('setContext', "proof-mate.sketchpad-empty", true);
		}
		// unlock sketchpad
		this.unlockSketchpad();
		// - this.expandHints();
	}

	/**
	 * Utility function, deletes the sketchpad content
	 */
	async queryDeleteSketchPad (opt?: { queryConfirm?: boolean }): Promise<boolean> {
		const success: boolean = await this.sketchpad.queryDeleteSketchpad(opt);
		if (success) { this.refreshView(); }
		return success;
	}
	
	/**
	 * Utility function, clears the visited flags in the sketchpad
	 */
	async queryClearMarks (selected?: ProofMateItem, opt?: { queryConfirm?: boolean }): Promise<boolean> {
		const success: boolean = await this.sketchpad.queryClearMarks(selected, opt);
		if (success) { this.refreshView(); }
		return success;
	}

	/**
	 * Utility function, clears a given sketchpad label
	 */
	async queryDeleteSketchPadLabel (item: ProofMateItem, opt?: { queryConfirm?: boolean }): Promise<boolean> {
		const success = await this.sketchpad.queryDeleteLabel(item, opt);
		if (success) { this.refreshView(); }
		return success;
	}

	/**
	 * Utility function, clears a given sketchpad label
	 */
	async queryDeleteNode (item: ProofMateItem, opt?: { queryConfirm?: boolean }): Promise<boolean> {
		const success = await this.sketchpad.queryDeleteNode(item, opt);
		if (success) { this.refreshView(); }
		return success;
	}

	/**
	 * Utility function, trims a given sketchpad label
	 */
	async queryTrimNode (item: ProofMateItem, opt?: { queryConfirm?: boolean }): Promise<boolean> {
		const success = await this.sketchpad.queryTrimNode(item, opt);
		if (success) { this.refreshView(); }
		return success;
	}

	/**
	 * Queries the user to enter a new proof command
	 */
	async queryNewProofCommand (selected: ProofMateItem, opt?: { label?: string }): Promise<boolean> {
		if (selected) {
			opt = opt || {};
			const name: string = await vscode.window.showInputBox({
				prompt: `Please enter proof command to be appended after ${selected.name}`,
				placeHolder: ``,
				value: ``,
				ignoreFocusOut: true 
			});
			if (name) {
				const elem: ProofCommand = new ProofCommand({
					cmd: name, 
					branchId: selected.branchId,
					parent: selected.isProofCommand() ? selected.getParent() : selected
				});
				const success: boolean = this.add({ selected, items: [ elem ] }, {
					select: true, 
					prepend: !selected.isClip(),
					label: opt?.label,
					focus: true
				});
				return success;
			}
		}
		return false;
	}
	/**
	 * Queries the user to enter a new proof command
	 */
	async queryAddNewProofBranch (selected: ProofMateItem): Promise<boolean> {
		if (selected) {
			const parent: ProofMateItem = (selected.getType() === "proof-branch") ? 
				selected.getParent() 
					: selected;
			const n: number = parent?.children?.filter(elem => {
				return elem.getType() === "proof-branch";
			})?.length || 0;
			// const name: string = await vscode.window.showInputBox({
			// 	prompt: `Please enter name of proof branch, e.g., (${n + 1})`,
			// 	placeHolder: `(${n + 1})`,
			// 	value: `(${n + 1})`,
			// 	ignoreFocusOut: true 
			// });
			const name: string = `(${n + 1})`;
			if (name) {
				const match: RegExpMatchArray = /[\d\.]+/g.exec(name);
				if (match?.length && match[0]) {
					const elem: ProofCommand = new ProofBranch({
						branchId: match[0],
						parent: selected
					});
					const success: boolean = this.add({ selected, items: [ elem ] }, {
						select: true, 
						prepend: !selected.isClip(),
						focus: true
					});
					return success;
				} else {
					vscodeUtils.showInformationMessage(`Warning: ${name} is an invalid branch name. Branch names are in the form (1.2.3)`);
				}
			}
		}
		return false;
	}

	/**
	 * Returns hints and sketchpad clips
	 * @param item Element clicked by the user 
	 */
	getChildren(item: TreeItem): TreeItem[] {
		if (item) {
			const pitem: ProofMateItem = <ProofMateItem> item;
			return pitem?.getChildren();
		}
		// root node
		return [ this.hints, this.sketchpad ];
	}
	/**
	 * Returns the parent of a node
	 * @param item Element clicked by the user 
	 */
	getParent (item: TreeItem): TreeItem {
		if (item) {
			const pitem: ProofMateItem = <ProofMateItem> item;
			return pitem?.getParent();
		}
		return null;
	}
	/**
	 * Pauses a proof
	 */
	async pauseProof (opt?: { force?: boolean, source?: string | "proof-explorer" }): Promise<void> {
		// ask the user confirmation before pausing
		opt = opt || {};
		const yesno: string[] = [ "Yes", "No" ];
		const msg: string = `Interrupt the execution of the current proof?`;
		const ans: string = opt?.force ? yesno[0] : await vscode.window.showInformationMessage(msg, { modal: true }, yesno[0]);
		if (ans === yesno[0]) {
			this.runningFlag = false;
			this.stopAt = null;
			if (opt?.source !== "proof-explorer") {
				vscode.commands.executeCommand("proof-explorer.pause-proof", { force: true });
			}
		}
	}
	/**
	 * Moves active node indicator forward
	 */
	async step (): Promise<boolean> {
		if (this.stopAt && this.runningFlag) {
			if (this.activeNode && this.stopAt.id === this.activeNode.id) {
				this.stopAt = null;
				this.runningFlag = false;
				return true;
			}
			let success: boolean = await this.forward();
			success = success && await this.step();
			return success;
		}
		// else
		return await this.forward();
	}
	/**
	 * Moves active node indicator forward
	 */
	moveIndicatorForward (opt?: { visited?: boolean, selectNode?: boolean }): boolean {
		this.activeNode?.visited(opt?.visited);
		this.activeNode = this.activeNode?.getNext();
		if (this.activeNode) {
			// skip anything that is not a proof command
			while (this.activeNode && !this.activeNode.isProofCommand()) {
				this.activeNode?.visited(opt?.visited);
				this.activeNode = this.activeNode?.getNext();
			}
			this.activeNode?.active();
			if (opt?.selectNode && this.activeNode) { this.selectNode(this.activeNode); }
			this.refreshView();
			return true;
		}
		this.activeNode = null;
		this.refreshView();
		return false;
	}
	/**
	 * Moves active node indicator back
	 */
	// moveIndicatorBack (opt?: { selectNode?: boolean }): boolean {
	// 	const activeLabel: SketchpadLabel = this.activeNode?.sketchpadLabel;
	// 	this.activeNode?.notVisited();
	// 	this.activeNode = this.activeNode?.getPreviousSibling();
	// 	if (this.activeNode) {
	// 		// skip anything that is not a proof command
	// 		while (!this.activeNode.isProofCommand()) {
	// 			this.activeNode?.notVisited();
	// 			this.activeNode = this.activeNode?.getPreviousSibling();
	// 		}
	// 		this.activeNode.active();
	// 		if (opt?.selectNode && this.activeNode) { this.selectNode(this.activeNode); }
	// 		this.refreshView();
	// 		return true;
	// 	}
	// 	this.activeNode = null;
	// 	this.refreshView();
	// 	return false;
	// }
	/**
	 * Runs all nodes in a subtree
	 */
	async runSubtree (item: ProofMateItem): Promise<boolean> {
		item = item?.contextValue === PROOFMATE_LABEL ?
			item.children?.length ? item.children[0] : null
				: item;
		// sanity check
		if (item?.contextValue === PROOFMATE_CLIP || item?.contextValue === PROOFMATE_LABEL) {
			// fast forward to the last node of the brannch and then execute the last node
			const lastNode: ProofMateItem = item.isProofBranch() ?
				item.getLastChildInSubtree()
					: (<ProofMateItem> item.getParent())?.getLastChildInSubtree();
			if (lastNode) {
				const success: boolean = await this.fastForwardTo(lastNode);
				// double check that fast forward ended up in the right place
				if (this.activeNode?.id === lastNode.id) {
					return await this.step();
				}
				return success;
			}
		}
		return false;
	}
	/**
	 * Fast-forwards to the selected node
	 */
	async fastForwardTo (item: ProofMateItem): Promise<boolean> {
		if (item) {
			// adjust selected target --- if it's a branch, stop at the first child
			const target: ProofMateItem = (item.isProofBranch() && item.children.length) ? 
				item.children[0]
					: item;
			this.stopAt = target;
			this.runningFlag = true;
			return await this.step();
		}
		return false;
	}
	/**
	 * Run the entire content of the active sketchpad
	 */
	async queryRunProof (opt?: { queryConfirm?: boolean }): Promise<boolean> {
		opt = opt || {};
		const root: ProofMateItem = this.sketchpad?.getFirstChild();
		if (root?.children?.length) {
			const queryConfirm: boolean = opt?.queryConfirm === false ? false : true;
			if (queryConfirm) {
				const ans: vscodeUtils.YesCancel = await vscodeUtils.showYesCancelDialog(`Run sketchpad ${root.name}?`);
				if (ans !== "yes") {
					return false;
				}
			}
			this.runSubtree(root.children[0]);
		}
		return false;
	}
	/**
	 * Returns a given tree item
	 */
	getTreeItem(item: TreeItem): TreeItem {
		return item;
    }
}
