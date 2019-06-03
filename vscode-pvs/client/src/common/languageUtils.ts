import * as lang from './languageKeywords';
import * as fsUtils from './fsUtils';

// records literals are in the form id: ID = (# ac1: Ac1, ac2: Ac2 #)
// record types are in the form Rec: TYPE = [# x: nat, y: real #]
export const RECORD: { [key: string]: RegExp } = {
	declaration: /(\w+)\s*:\s*(\w+)(\s*=\s*[\[\(]#.+[\]\)])?/g,
	isLiteral: /\(#.+#\)/g,
	isType: /\[#.+#\]/g,
	accessors: /[\(\[]#(.+)#[\]\)]/g, // comma-separated list of accessors
	typeName: /\w+\s*:\s*(\w+)/g
}

// (?:\%.*\s)* removes comments
export const theoryRegexp: RegExp = /(\w+)\s*(?:\%.*\s)*(?:\[.+\])?\s*:\s*(?:\%.*\s)*\s*THEORY\b/gi;

/**
 * @function findTheoryName
 * @description Utility function, finds the name of the theory that immediately preceeds a given line
 * @param txt The text where the theory should be searched 
 * @param line The line in the document where search should end
 * @returns { string | null } The theory name if any is found, null otherwise
 */
export function findTheoryName(txt: string, line: number): string | null {
	const text: string = txt.split("\n").slice(0, line + 1).join("\n");
	const regexp: RegExp = new RegExp(theoryRegexp);
	let candidates: string[] = [];
	let match: RegExpMatchArray = null;
	while(match = regexp.exec(text)) {
		// the last match will be the closest to the current line number
		candidates.push(match[1]);
	}
	if (candidates.length > 0) {
		return candidates[candidates.length - 1];
	}
	return null;
};
/**
 * Utility function, finds all theories in a given file
 * @param fileName 
 * @param fileContent 
 */
export function findTheories(fileName: string, fileContent: string): TheoryMap {
	// TODO: return an array rather than a map? open the file directly in the function?
	let ans: TheoryMap = {};
	const regexp: RegExp = new RegExp(theoryRegexp);
	let match: RegExpMatchArray = null;
	let lines: string[] = fileContent.split("\n");
	while(match = regexp.exec(fileContent)) {
		let line: number = 0;
		let character: number = 0;
		let theoryName: string = null;
		for (let i = 0; i < lines.length; i++) {
			if (lines[i].includes(` ${match[1]} `)
				|| lines[i].includes(` ${match[1]}:`)
				|| lines[i].includes(` ${match[1]}[`)
				|| lines[i].includes(` ${match[1]}%`)
				|| lines[i].startsWith(`${match[1]} `)
				|| lines[i].startsWith(`${match[1]}:`)
				|| lines[i].startsWith(`${match[1]}[`)
				|| lines[i].startsWith(`${match[1]}%`)
				) {
				// the first match is the theory declaration -- in pvs, theories cannot be imported if they have not been declared first
				line = i;
				character = lines[i].split(match[1])[0].length;
				theoryName = match[1];
				ans[theoryName] = {
					position: {
						line: line,
						character: character
					},
					theoryName: theoryName,
					fileName: fileName
				};
				break;
			}
		}
	}
	return ans;
}

/**
 * @function listTheoryNames
 * @description Utility function, returns a list of all theories in the given file
 * @param txt The text where the theory should be searched 
 * @returns string[]
 */
// TODO: check if we can use listTheoryNames in place of findTheories
export function listTheoryNames (txt: string): string[] {
	const ans: string[] = [];
	let match: RegExpMatchArray = null;
	const regexp: RegExp = new RegExp(theoryRegexp);
	while (match = regexp.exec(txt)) {
		if (match && match.length > 1 && match[1]) {
			ans.push(match[1]);
		}
	}
	return ans;
};

// match[1] indicates commented section; match[2] is the theorem name
export const theoremRegexp: RegExp = /(%.*)?(\b\w+)\s*(?:\%.*\s)*:\s*(?:(?:\%.*\s)*\s*)*(?:CHALLENGE|CLAIM|CONJECTURE|COROLLARY|FACT|FORMULA|LAW|LEMMA|PROPOSITION|SUBLEMMA|THEOREM|OBLIGATION)\b/gi;

export function listTheorems (txt: string): string[] {
	const ans: string[] = [];
	let match: RegExpMatchArray = null;
	const regexp: RegExp = new RegExp(theoremRegexp);
	while (match = regexp.exec(txt)) {
		if (match && match.length > 2 && match[2] && !match[1]) {
			ans.push(match[2]);
		}
	}
	return ans;
}

export async function listTheoremsInFile (uri: string): Promise<TheoremDescriptor[]> {
	if (uri) {
		const fileName: string = uri.replace("file://", "");
		const txt: string = await fsUtils.readFile(fileName);
		const slices: string[] = txt.split("\n");
		const theories: TheoryMap = findTheories(fileName, txt);
		const boundaries: { theoryName: string, from: number, to: number }[] = []; // slices txt to the boundaries of the theories
		const theoryNames: string[] = Object.keys(theories);
		for (let i = 0; i < theoryNames.length; i++) {
			boundaries.push({
				theoryName: theoryNames[i],
				from: theories[theoryNames[i]].position.line,
				to: (i + 1 < theoryNames.length) ? theories[theoryNames[i + 1]].position.line : slices.length
			});
		}
		const theoremsArray: TheoremDescriptor[] = [];
		for (let i = 0; i < boundaries.length; i++) {
			const content: string = slices.slice(boundaries[i].from, boundaries[i].to).join("\n");
			const regex: RegExp = new RegExp(theoremRegexp);
			let match: RegExpMatchArray = null;
			while (match = regex.exec(content)) {
				if (match.length > 2 && match[2] && !match[1]) {
					const formulaName: string = match[2];
					const offset: number = content.slice(0, match.index).split("\n").length;
					const line: number = boundaries[i].from + offset;
					theoremsArray.push({
						fileName: fsUtils.getFilename(uri),
						theoryName: boundaries[i].theoryName,
						formulaName,
						position: { line, character: 0 }
					});
				}
			}
		}
		return theoremsArray;
	}
	return null;
}


/**
 * @function findTheorem
 * @description Utility function, finds the name of a theorem that immediately preceeds a given line
 * @param txt The text where the theory should be searched 
 * @param line The line in the document where search should end
 * @returns { string | null } The theory name if any is found, null otherwise
 */
export function findTheorem(txt: string, line: number): string | null {
	let text: string = txt.split("\n").slice(0, line + 1).join("\n");
	let candidates: string[] = [];
	// (?:\%.*\s)* removes comments
	const regexp: RegExp = new RegExp(theoremRegexp);
	let match: RegExpMatchArray = null;
	while(match = regexp.exec(text)) {
		if (match.length > 2 && match[2] && !match[1]) {
			candidates.push(match[2]);
		}
	}
	if (candidates.length > 0) {
		return candidates[candidates.length - 1];
	}
	return null;
};



interface Position {
	line: number;
	character: number;
}
interface Range {
	start: Position,
	end: Position
};

export interface FileList {
	pvsContextFolder: string;
	fileNames: string[]; // TODO: FileDescriptor[]
}

export interface TheoryList {
	pvsContextFolder: string;
	theories: TheoryMap; //  TODO TheoryDescriptor[]
}

export declare interface TheoremList {
	pvsContextFolder: string;
	theorems: TheoremDescriptor[];
	fileName?: string; // when fileName is specified, the theorem list describes the content of a specific file. This is useful for status updates.
}

export interface DeclarationMap {
	[ theoryName: string ]: {
		[key: string]: {
			theoryName: string;
			symbolName: string;
			symbolDeclaration: string;
			symbolDeclarationRange: Range;
			symbolDeclarationFile: string;
			symbolDoc?: string;
			comment?: string;
		}
	}
}

export interface TccList {
	pvsContextFolder: string;
	tccs: TccMap;
}

import { TccDescriptor, StrategyDescriptor } from './serverInterface';
export interface TccMap {
	[ theoryName: string ]: {
		fileName: string,
		theoryName: string,
		tccs: TccDescriptor[]
	};
}

export declare interface TheoryMap {
	[ theoryName: string ]: {
		theoryName: string,
		fileName: string,
		position: Position
	}
}

export declare interface TheoremDescriptor {
	fileName: string,
	theoryName: string,
	formulaName: string,
	position: Position
}

export declare interface TheoremsMap {
	[ theorem: string ]: TheoremDescriptor
}



/**
 * @function getWordRange
 * @TODO improve this function, currently operators are not recognized/resolved
 * @description Utility function, identifies the range of the word at the cursor position.
 * 				The default search strategy builds on a regular expression designed to identify symbol names (\w+) and strings (\"([^\"]*)\")
 * @param txt The document that contains the word
 * @param position Position in the document
 */
export function getWordRange(txt: string, position: Position): Range {
	let character: number = position.character;
	let len: number = 0;
	let lines: string[] = txt.split("\n");
	if (lines.length > position.line) {
		let txt: string = lines[position.line];
		let needle: RegExp = /(\w+\??\!?\+?)|\"([^\"]*)\"/g; // symbol, which may terminate with ? ! + | string
		let match: RegExpMatchArray = [];
		while(match = needle.exec(txt)) {
			if (match.index <= position.character) {
				character = match.index;
				len = (match[1]) ? match[1].length 
						: match[2].length + 2; // match[2] is a string, we need to add +2 because of the ""
			} else {
				break;
			}
		}
	}
	return {
		start: { line: position.line, character: character },
		end: { line: position.line, character: character + len }
	};
}

export function getText(txt: string, range: Range): string {
	const lines: string[] = txt.split("\n");
	let ans: string[] = lines.slice(range.start.line, range.end.line);
	ans[0] = ans[0].substr(range.start.character);
	const len: number = ans.length - 1;
	ans[ans.length - 1] = ans[ans.length - 1].substr(0, len - range.end.character);
	return ans.join("\n");
}

/**
 * @function getErrorRange
 * @description Utility function, identifies the range of a syntax error at the cursor position.
 * @param txt The document that contains the word
 * @param position Position in the document
 */
export function getErrorRange(txt: string, position: Position): Range {
	let character: number = position.character;
	let len: number = 0;
	let lines: string[] = txt.split("\n");
	if (lines.length > position.line) {
		let txt: string = lines[position.line];
		len = txt.length - position.character;
	}
	return {
		start: { line: position.line, character: character },
		end: { line: position.line, character: character + len }
	};
}

export const textColor: { [ key: string ]: number } = {
    blue: 32
}

export function colorText(text: string, colorCode: number): string {
    // \x1b[0m resets all attributes
	return `\x1b[38;5;${colorCode}m${text}\x1b[0m`;
}

export function backspaceDelete(text: string): string {
	if (text) {
		return "\u0008".repeat(text.length);
	}
	return "";
}


// list obtained with collect-strategy-names
export const PROVER_STRATEGIES_FULL_SET: StrategyDescriptor[] = [
	{ name: "abs-simp", description:""},
	{ name: "abstract", description:""},
	{ name: "abstract-and-mc", description:""},
	{ name: "add-formulas", description:""},
	{ name: "all-implicit-typepreds", description:""},
	{ name: "all-typepreds", description:""},
	{ name: "apply", description:""},
	{ name: "apply-eta", description:""},
	{ name: "apply-ext", description:""},
	{ name: "apply-extensionality", description:""},
	{ name: "apply-lemma", description:""},
	{ name: "apply-rewrite", description:""},
	{ name: "assert", description:""},
	{ name: "auto-rewrite", description:""},
	{ name: "auto-rewrite!", description:""},
	{ name: "auto-rewrite!!", description:""},
	{ name: "auto-rewrite-defs", description:""},
	{ name: "auto-rewrite-explicit", description:""},
	{ name: "auto-rewrite-expr", description:""},
	{ name: "auto-rewrite-theories", description:""},
	{ name: "auto-rewrite-theory", description:""},
	{ name: "auto-rewrite-theory-with-importings", description:""},
	{ name: "bash", description:""},
	{ name: "bddsimp", description:""},
	{ name: "beta", description:""},
	{ name: "both-sides", description:""},
	{ name: "both-sides-f", description:""},
	{ name: "branch", description:""},
	{ name: "branch-back", description:""},
	{ name: "cancel", description:""},
	{ name: "cancel-add", description:""},
	{ name: "cancel-add!", description:""},
	{ name: "cancel-by", description:""},
	{ name: "cancel-formula", description:""},
	{ name: "cancel-terms", description:""},
	{ name: "canon-tms", description:""},
	{ name: "case", description:""},
	{ name: "case*", description:""},
	{ name: "case-if", description:""},
	{ name: "case-if*", description:""},
	{ name: "case-old-lift-if", description:""},
	{ name: "case-replace", description:""},
	{ name: "checkpoint", description:""},
	{ name: "claim", description:""},
	{ name: "comment", description:""},
	{ name: "commentf", description:""},
	{ name: "contra-eqs", description:""},
	{ name: "copy", description:""},
	{ name: "copy*", description:""},
	{ name: "cross-add", description:""},
	{ name: "cross-mult", description:""},
	{ name: "cut", description:""},
	{ name: "decide", description:""},
	{ name: "decompose-equality", description:""},
	{ name: "default-strategy", description:""},
	{ name: "deftactic", description:""},
	{ name: "delabel", description:""},
	{ name: "delete", description:""},
	{ name: "demod-lin", description:""},
	{ name: "demod-num", description:""},
	{ name: "detuple-boundvars", description:""},
	{ name: "discriminate", description:""},
	{ name: "distrib", description:""},
	{ name: "distrib!", description:""},
	{ name: "div-by", description:""},
	{ name: "do-rewrite", description:""},
	{ name: "elim-unary", description:""},
	{ name: "elim-unary!", description:""},
	{ name: "else", description:""},
	{ name: "else*", description:""},
	{ name: "equate", description:""},
	{ name: "eta", description:""},
	{ name: "eval", description:""},
	{ name: "eval-expr", description:""},
	{ name: "eval-formula", description:""},
	{ name: "expand", description:""},
	{ name: "expand*", description:""},
	{ name: "expand-names", description:""},
	{ name: "extensionality", description:""},
	{ name: "extra-tcc-step", description:""},
	{ name: "extrategies-about", description:""},
	{ name: "factor", description:""},
	{ name: "factor!", description:""},
	{ name: "fail", description:""},
	{ name: "fert-tsos", description:""},
	{ name: "field", description:""},
	{ name: "field-about", description:""},
	{ name: "finalize", description:""},
	{ name: "flatten", description:""},
	{ name: "flatten-disjunct", description:""},
	{ name: "flip-ineq", description:""},
	{ name: "for", description:""},
	{ name: "for-each", description:""},
	{ name: "for-each-rev", description:""},
	{ name: "for@", description:""},
	{ name: "forward-chain", description:""},
	{ name: "forward-chain*", description:""},
	{ name: "forward-chain-theory", description:""},
	{ name: "forward-chain@", description:""},
	{ name: "gen-ex-cad", description:""},
	{ name: "generalize", description:""},
	{ name: "generalize-skolem-constants", description:""},
	{ name: "grind", description:""},
	{ name: "grind-reals", description:""},
	{ name: "grind-with-ext", description:""},
	{ name: "grind-with-lemmas", description:""},
	{ name: "ground", description:""},
	{ name: "ground-eval", description:""},
	{ name: "group", description:""},
	{ name: "group!", description:""},
	{ name: "has-sign", description:""},
	{ name: "help", description:""},
	{ name: "hide", description:""},
	{ name: "hide-all-but", description:""},
	{ name: "if", description:""},
	{ name: "if-label", description:""},
	{ name: "iff", description:""},
	{ name: "induct", description:""},
	{ name: "induct-and-rewrite", description:""},
	{ name: "induct-and-rewrite!", description:""},
	{ name: "induct-and-simplify", description:""},
	{ name: "inst", description:""},
	{ name: "inst!", description:""},
	{ name: "inst*", description:""},
	{ name: "inst-cp", description:""},
	{ name: "inst?", description:""},
	{ name: "install-rewrites", description:""},
	{ name: "instantiate", description:""},
	{ name: "instantiate-one", description:""},
	{ name: "insteep", description:""},
	{ name: "insteep*", description:""},
	{ name: "int-dom-zpb", description:""},
	{ name: "invoke", description:""},
	{ name: "isolate", description:""},
	{ name: "isolate-mult", description:""},
	{ name: "isolate-replace", description:""},
	{ name: "just-install-proof", description:""},
	{ name: "label", description:""},
	{ name: "lazy-grind", description:""},
	{ name: "lemma", description:""},
	{ name: "let", description:""},
	{ name: "let-name-replace", description:""},
	{ name: "lift-if", description:""},
	{ name: "lisp", description:""},
	{ name: "mapstep", description:""},
	{ name: "mapstep@", description:""},
	{ name: "match", description:""},
	{ name: "measure-induct+", description:""},
	{ name: "measure-induct-and-simplify", description:""},
	{ name: "merge-fnums", description:""},
	{ name: "model-check", description:""},
	{ name: "move-terms", description:""},
	{ name: "move-to-front", description:""},
	{ name: "mult-by", description:""},
	{ name: "mult-cases", description:""},
	{ name: "mult-eq", description:""},
	{ name: "mult-extract", description:""},
	{ name: "mult-extract!", description:""},
	{ name: "mult-ineq", description:""},
	{ name: "musimp", description:""},
	{ name: "name", description:""},
	{ name: "name-case-replace", description:""},
	{ name: "name-distrib", description:""},
	{ name: "name-extract", description:""},
	{ name: "name-induct-and-rewrite", description:""},
	{ name: "name-label", description:""},
	{ name: "name-label*", description:""},
	{ name: "name-mult", description:""},
	{ name: "name-mult!", description:""},
	{ name: "name-replace", description:""},
	{ name: "name-replace*", description:""},
	{ name: "neg-formula", description:""},
	{ name: "op-ident", description:""},
	{ name: "op-ident!", description:""},
	{ name: "open-ex-inf-cad", description:""},
	{ name: "open-frag-ex-inf-cad", description:""},
	{ name: "permute-mult", description:""},
	{ name: "permute-mult!", description:""},
	{ name: "permute-terms", description:""},
	{ name: "permute-terms!", description:""},
	{ name: "postpone", description:""},
	{ name: "presburger", description:""},
	{ name: "presburger-to-ws1s", description:""},
	{ name: "printf", description:""},
	{ name: "prop", description:""},
	{ name: "propax", description:""},
	{ name: "protect", description:""},
	{ name: "pvsio-about", description:""},
	{ name: "query*", description:""},
	{ name: "quit", description:""},
	{ name: "quote", description:""},
	{ name: "rahd", description:""},
	{ name: "rahd-simp", description:""},
	{ name: "rahd-waterfall", description:""},
	{ name: "random-test", description:""},
	{ name: "rcr-ineqs", description:""},
	{ name: "rcr-svars", description:""},
	{ name: "real-props", description:""},
	{ name: "recip-mult", description:""},
	{ name: "recip-mult!", description:""},
	{ name: "record", description:""},
	{ name: "redlet", description:""},
	{ name: "redlet*", description:""},
	{ name: "reduce", description:""},
	{ name: "reduce-with-ext", description:""},
	{ name: "relabel", description:""},
	{ name: "repeat", description:""},
	{ name: "repeat*", description:""},
	{ name: "replace", description:""},
	{ name: "replace*", description:""},
	{ name: "replace-eta", description:""},
	{ name: "replace-ext", description:""},
	{ name: "replace-extensionality", description:""},
	{ name: "replaces", description:""},
	{ name: "rerun", description:""},
	{ name: "residue-class-ring-ineqs", description:""},
	{ name: "reveal", description:""},
	{ name: "rewrite", description:""},
	{ name: "rewrite*", description:""},
	{ name: "rewrite-expr", description:""},
	{ name: "rewrite-lemma", description:""},
	{ name: "rewrite-msg-off", description:""},
	{ name: "rewrite-msg-on", description:""},
	{ name: "rewrite-with-fnum", description:""},
	{ name: "rewrites", description:""},
	{ name: "rotate++", description:""},
	{ name: "rotate--", description:""},
	{ name: "rule-induct", description:""},
	{ name: "rule-induct-step", description:""},
	{ name: "same-name", description:""},
	{ name: "set-print-depth", description:""},
	{ name: "set-print-length", description:""},
	{ name: "set-print-lines", description:""},
	{ name: "set-right-margin", description:""},
	{ name: "show-parens", description:""},
	{ name: "show-subst", description:""},
	{ name: "simp-arith", description:""},
	{ name: "simp-gls", description:""},
	{ name: "simp-real-null", description:""},
	{ name: "simp-tvs", description:""},
	{ name: "simp-zrhs", description:""},
	{ name: "simple-induct", description:""},
	{ name: "simple-measure-induct", description:""},
	{ name: "simplify", description:""},
	{ name: "simplify-with-rewrites", description:""},
	{ name: "skeep", description:""},
	{ name: "skeep*", description:""},
	{ name: "skip", description:""},
	{ name: "skip-msg", description:""},
	{ name: "skip-steps", description:""},
	{ name: "sklisp", description:""},
	{ name: "skodef", description:""},
	{ name: "skodef*", description:""},
	{ name: "skolem", description:""},
	{ name: "skolem!", description:""},
	{ name: "skolem-typepred", description:""},
	{ name: "skoletin", description:""},
	{ name: "skoletin*", description:""},
	{ name: "skosimp", description:""},
	{ name: "skosimp*", description:""},
	{ name: "smash", description:""},
	{ name: "splash", description:""},
	{ name: "split", description:""},
	{ name: "split-ineq", description:""},
	{ name: "spread", description:""},
	{ name: "spread!", description:""},
	{ name: "spread@", description:""},
	{ name: "sq-simp", description:""},
	{ name: "stop-rewrite", description:""},
	{ name: "stop-rewrite-theory", description:""},
	{ name: "sub-formulas", description:""},
	{ name: "suffices", description:""},
	{ name: "swap", description:""},
	{ name: "swap!", description:""},
	{ name: "swap-group", description:""},
	{ name: "swap-group!", description:""},
	{ name: "swap-rel", description:""},
	{ name: "tccs-expression", description:""},
	{ name: "tccs-formula", description:""},
	{ name: "tccs-formula*", description:""},
	{ name: "tccs-step", description:""},
	{ name: "then", description:""},
	{ name: "then*", description:""},
	{ name: "then@", description:""},
	{ name: "time", description:""},
	{ name: "touch", description:""},
	{ name: "trace", description:""},
	{ name: "track-all-current-rewrites", description:""},
	{ name: "track-rewrite", description:""},
	{ name: "transform-both", description:""},
	{ name: "triv-ideals", description:""},
	{ name: "trust", description:""},
	{ name: "trust!", description:""},
	{ name: "try", description:""},
	{ name: "try-branch", description:""},
	{ name: "try-rewrites", description:""},
	{ name: "typepred", description:""},
	{ name: "typepred!", description:""},
	{ name: "undo", description:""},
	{ name: "univ-sturm-ineqs", description:""},
	{ name: "unlabel", description:""},
	{ name: "unlabel*", description:""},
	{ name: "unless", description:""},
	{ name: "unless-label", description:""},
	{ name: "unless-label@", description:""},
	{ name: "unless@", description:""},
	{ name: "untrace", description:""},
	{ name: "untrack-rewrite", description:""},
	{ name: "unwind-protect", description:""},
	{ name: "use", description:""},
	{ name: "use*", description:""},
	{ name: "use-with", description:""},
	{ name: "when", description:""},
	{ name: "when-label", description:""},
	{ name: "when-label@", description:""},
	{ name: "when@", description:""},
	{ name: "with-focus-on", description:""},
	{ name: "with-focus-on@", description:""},
	{ name: "with-fresh-labels", description:""},
	{ name: "with-fresh-labels@", description:""},
	{ name: "with-fresh-names", description:""},
	{ name: "with-fresh-names@", description:""},
	{ name: "with-labels", description:""},
	{ name: "with-tccs", description:""},
	{ name: "wrap-formula", description:""},
	{ name: "wrap-manip", description:""},
	{ name: "ws1s", description:""},
	{ name: "ws1s-simp", description:""},
	{ name: "y2grind", description:""},
	{ name: "y2simp", description:""},
	{ name: "ygrind", description:""},
	{ name: "yices", description:""},
	{ name: "yices-with-rewrites", description:""},
	{ name: "yices2", description:""},
	{ name: "yices2-with-rewrites", description:""}
];

export const PROVER_STRATEGIES_CORE: StrategyDescriptor[] = [
	{ name: "all-typepreds", description:"" },
	{ name: "apply-extensionality", description:"" },
	{ name: "apply-lemma", description:"" },
	{ name: "apply-rewrite", description:"" },
	{ name: "assert", description:"" },
	{ name: "auto-rewrite", description:"" },
	{ name: "bash", description:"" },
	{ name: "bddsimp", description:"" },
	{ name: "beta", description:"" },
	{ name: "both-sides", description:"" },
	{ name: "branch", description:"" },
	{ name: "cancel", description:"" },
	{ name: "case", description:"" },
	{ name: "comment", description:"" },
	{ name: "expand", description:"" },
	{ name: "flatten", description:"" },
	{ name: "grind", description:"" },
	{ name: "grind-reals", description:"" },
	{ name: "ground", description:"" },
	{ name: "hide", description:"" },
	{ name: "hide-all-but", description:"" },
	{ name: "iff", description:"" },
	{ name: "inst?", description:"" },
	{ name: "lemma", description:"" },
	{ name: "lift-if", description:"" },
	{ name: "postpone", description:"" },
	{ name: "prop", description:"" },
	{ name: "reveal", description:"" },
	{ name: "rewrite", description:"" },
	{ name: "skeep", description:"" },
	{ name: "skosimp*", description:"" },
	{ name: "split", description:"" },
	{ name: "typepred", description:"" },
	{ name: "undo", description:"" },
	{ name: "use", description:"" }
];