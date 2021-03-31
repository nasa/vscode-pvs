/**
 * @module languageUtils
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

import * as language from './languageKeywords';
import { ProofNode, PvsVersionDescriptor, ProofDescriptor, 
	ProofStatus, Position, Range, ProofTree, PvsFormula
} from '../common/serverInterface';
import { colorText, PvsColor } from "./colorUtils";
import { splitCommands, isPostponeCommand, isUndoStarCommand, isShowHiddenCommand } from './commandUtils';
import { SequentDescriptor, SFormula } from './fsUtils';


// records literals are in the form id: ID = (# ac1: Ac1, ac2: Ac2 #)
// record types are in the form Rec: TYPE = [# x: nat, y: real #]
export const RECORD: { [key: string]: RegExp } = {
	declaration: /(\w+)\s*:\s*(\w+)(\s*=\s*[\[\(]#.+[\]\)])?/g,
	isLiteral: /\(#.+#\)/g,
	isType: /\[#.+#\]/g,
	accessors: /[\(\[]#(.+)#[\]\)]/g, // comma-separated list of accessors
	typeName: /\w+\s*:\s*(\w+)/g
}

export const commentRegexp: RegExp = /%.*/g;
export const stringRegexp: RegExp = /\"[^\"]+\"/g;
// group 1 is theoryName, group 2 is comma-separated list of theory parameters -- NB: this regexp is fast but not accurate, because it does not check the end of the theory. See example use in codelense.
export const theoryRegexp: RegExp = /([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*(?:\[([\w\W\s]+)\])?\s*\:\s*THEORY\s+BEGIN\b/gi;
export function endTheoryOrDatatypeRegexp(theoryName: string): RegExp {
	return new RegExp(`(\\bEND\\s*)(${theoryName})(\\b)`, "gi");
}
export const datatypeRegexp: RegExp = /([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*(?:\[([\w\W\s]+)\])?\s*\:\s*DATATYPE\s+(?:BEGIN|WITH)\b/gi;
export const theoryOrDatatypeRegexp: RegExp = /([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*(?:\[([\w\W\s]+)\])?\s*\:\s*(?:THEORY|DATATYPE)\b/gi;
export const declarationRegexp: RegExp = /([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*(?:\[([\w\W\s]+)\])?\s*\:/gi;
// group 1 is the formula name
export const formulaRegexp: RegExp = /([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*(%.+)?\s*:\s*(%.+)?\s*(?:CHALLENGE|CLAIM|CONJECTURE|COROLLARY|FACT|FORMULA|LAW|LEMMA|PROPOSITION|SUBLEMMA|THEOREM|OBLIGATION|JUDGEMENT|AXIOM)\b/gim;
// same as formulaRegExp, but does not include JUDGEMENT and AXIOM
export const theoremRegexp: RegExp = /([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*(%.+)?\s*:\s*(%.+)?\s*(?:CHALLENGE|CLAIM|CONJECTURE|COROLLARY|FACT|FORMULA|LAW|LEMMA|PROPOSITION|SUBLEMMA|THEOREM|OBLIGATION|JUDGEMENT)\b/gim;
// /(\w+)\s*(?:\%.*\s)*(?:\[([^\]]+)\])?\s*:\s*(?:\%.*\s)*\s*THEORY\b/gi;
export const tccRegexp: RegExp = /([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*:\s*OBLIGATION\b/gi;
export const tccStatusRegExp: RegExp = /%\s(proved|subsumed|simplified|unproved|unfinished|unchecked|untried)\b/g;
export const tccFormulaRegexp: RegExp = /[A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉\.]*_TCC\d+/g;
export function isTccFormula (desc: PvsFormula): boolean {
	return desc && desc.formulaName && new RegExp(tccFormulaRegexp).test(desc.formulaName);
}
// group 1 is a list of comma-separated list of imported theories. This regexp works only for theory names without parameters
export const simpleImportingRegexp: RegExp = /\bIMPORTING\s+((?:(?:[A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)+)(?:\s*,\s*(?:[A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)+)*)/gi;

// group 1 is the list of comma-separated values contained in the expression 
export const listRegexp: RegExp = /^\s*\(:([\s\w\,\/]*):\)/g;

// generic regular expression for symbol names, group 1 is the symbol name
export const symbolRegexp: RegExp = /([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)/g;

export function isListExpr (expr: string): boolean {
	if (expr) {
		return new RegExp(listRegexp).test(expr);
	}
	return false;
}
export function listExpr2doubleArray (data: string): number[] {
	const res: number[] = []
	const match: RegExpMatchArray = new RegExp(listRegexp).exec(data);
	if (match && match.length > 1) {
		const values: string[] = match[1]?.split(",");
		for (let i = 0; i < values.length; i++) {
			const val: string = values[i]?.trim();
			if (val && val.includes("/")) {
				const elems: string[] = val.split("/");
				const d1: number = +elems[0];
				const d2: number = +elems[1];
				res.push(d1/d2);
			} else {
				res.push(+val);
			}
		}
	}
	return res;
}

// capture group 1 is proofName
// capture group 2 is formulaName,
// capture group 3 is proofTree
export const prfRegExp: RegExp = /;;; Proof\s+([\w\?₀₁₂₃₄₅₆₇₈₉\.\-]+)\s+for formula\s+([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉\.]*)\s+(\((?:\"\")?[\n\w\W]+)/g;

export declare interface IntellisenseTriggers {
	recordExpression: RegExp,
	recordAccessor: RegExp,
	theoryDeclaration: RegExp,
	datatypeDeclaration: RegExp,
	formulaDeclaration: RegExp,
	declaration: RegExp
};

export const isense: IntellisenseTriggers = {
	recordExpression: /(\w+)\s*=\s*\(\#(?:\s*(?:\w+)\s*:=(?:.*,)?)*/g, // /(\w+)\s*=\s*\(\#/g, // rc1: Rec1 = (#... 
	recordAccessor: /(\w+)`/g, // rc1`...
	theoryDeclaration: theoryRegexp,
	datatypeDeclaration: datatypeRegexp,
	formulaDeclaration: formulaRegexp,
	declaration: declarationRegexp
};

// export type ProverResult = ProofState | ProofState[]

function sequentToString(sequents: SFormula[], opt?: { useColors?: boolean, htmlEncoding?: boolean }): string {
	let res: string = "";
	opt = opt || {};
	for (let i = 0; i < sequents.length; i++) {
		const sequent: SFormula = sequents[i];
		let label: string = sequent.labels.join(" ");
		label = (sequent.changed === 'true') ? `{${label}}` : `[${label}]` ;
		label = (sequent.changed === 'true' && opt.useColors) ? `${colorText(label, PvsColor.green)}` : `${label}` ;
		const formula: string = (opt.useColors) ? `${pvsCliSyntaxHighlighting(sequent.formula, opt)}` : sequent.formula;
		res += `${label}   ${formula}`;
		res += opt.htmlEncoding ? "<br>" : "\n";
	}
	return res;
}

function labelToString (label: string, opt?: { useColors?: boolean, htmlEncoding?: boolean }): string {
	opt = opt || {};
	return (opt && opt.useColors)?
		`\n${colorText(`${label} :`, PvsColor.green)}\n`
			: `\n${label} :\n`;
}

function commentToString (txt: string, opt?: { useColors?: boolean, htmlEncoding?: boolean }): string {
	opt = opt || {};
	const content: string = (opt && opt.useColors)? 
		`${colorText(`${txt}`, PvsColor.yellow)}`
			: `${txt}`;
	return opt.htmlEncoding ? `<br>${content}<br>` : `\n${content}\n`;
}

function commentaryToString (txt: string | string[], opt?: { htmlEncoding?: boolean }): string {
	let res = "";
	if (typeof txt === "string") {
		txt = txt.trim().endsWith(",") ? txt.trim().slice(0, -1) : txt.trim();
		res += opt.htmlEncoding ? `<br>${txt}<br>` : `\n${colorText(`${txt}`, PvsColor.gray)}\n`;
	} else {
		res += opt.htmlEncoding ? "<br>" : "\n";
		for (let i = 0; i < txt.length; i++) {
			let line: string = txt[i];
			if (i === txt.length - 1) {
				line = line.trim().endsWith(",") ? line.trim().slice(0, -1) : line.trim();
			}
			res += opt.htmlEncoding ? `${line}<br>` : `${colorText(`${line}`, PvsColor.gray)}\n`;
		}
	}
	return res;
}

export function desc2id (desc: { fileName: string, formulaName?: string, theoryName?: string }): string {
	if (desc) {
		if (desc.formulaName) {
			return desc.formulaName;
		}
		if (desc.fileName) {
			return desc.fileName;
		}
	}
	console.error("[languageUtils.desc2id] Warning: trying to generate ID from null descriptor");
	return null;
}

export function pvsCliSyntaxHighlighting(text: string, opt?: { htmlEncoding?: boolean }): string {
	if (text) {
		opt = opt || {};
		// numbers and operators should be highlighted first, otherwise the regexp will change characters introduced to colorize the string
		const number_regexp: RegExp = new RegExp(language.PVS_NUMBER_REGEXP_SOURCE, "g");
		text = text.replace(number_regexp, (number: string) => {
			return colorText(number, PvsColor.yellow);
		});
		const operators_regexp: RegExp = new RegExp(language.PVS_LANGUAGE_OPERATORS_REGEXP_SOURCE, "g");
		text = text.replace(operators_regexp, (op: string) => {
			return colorText(op, PvsColor.blue);
		});
		const keywords_regexp: RegExp = new RegExp(language.PVS_RESERVED_WORDS_REGEXP_SOURCE, "gi");
		text = text.replace(keywords_regexp, (keyword: string) => {
			return colorText(keyword, PvsColor.blue);
		});
		const function_regexp: RegExp = new RegExp(language.PVS_LIBRARY_FUNCTIONS_REGEXP_SOURCE, "g");
		text = text.replace(function_regexp, (fname: string) => {
			return colorText(fname, PvsColor.green);
		});
		const builtin_types_regexp: RegExp = new RegExp(language.PVS_BUILTIN_TYPE_REGEXP_SOURCE, "g");
		text = text.replace(builtin_types_regexp, (tname: string) => {
			return colorText(tname, PvsColor.green);
		});
		const truefalse_regexp: RegExp = new RegExp(language.PVS_TRUE_FALSE_REGEXP_SOURCE, "gi");
		text = text.replace(truefalse_regexp, (tf: string) => {
			return colorText(tf, PvsColor.blue);
		});
	}
	return text;
}

export function formatPvsIoState (pvsioState: string, opt?: { useColors?: boolean, showAction?: boolean }): string {
	if (pvsioState) {
		opt = opt || {};
		return (opt.useColors) ? pvsCliSyntaxHighlighting(pvsioState) : pvsioState;
	}
	return pvsioState;
}

export function formatHiddenFormulas (proofState: SequentDescriptor, opt?: { useColors?: boolean, showAction?: boolean, showHidden?: boolean }) {
	if (proofState) {
		opt = opt || {};
		let res: string = "";
		if (proofState.sequent) {
			if (proofState.sequent["hidden-antecedents"] || proofState.sequent["hidden-succedents"]) {
				res += "\n%-- Hidden formulas --\n\n";
				if (proofState.sequent["hidden-antecedents"]) {
					res += sequentToString(proofState.sequent["hidden-antecedents"], opt);
				}
				// res += "  |-------\n";
				res += "  ├───────\n";
				if (proofState.sequent["hidden-succedents"]) {
					res += sequentToString(proofState.sequent["hidden-succedents"], opt);
				}
				res += "\n\n";
			} else {
				res += "The current sequent does not have any hidden formula.\n";
			}
		}
		return res;
	} else {
		console.error("[language-utils.show-hidden-formulas] Error: proof state is null :/");
	}
	return null;
}

/**
 * Utility function, converts sequent formulas into a string
 */
export function sformulas2string (desc: SequentDescriptor): string {
	let res: string = "";
	if (desc?.sequent) { // print label and comment only if the sequent is non-empty (sequent empty means proof completed)
		if (desc.sequent.antecedents) {
			res += sequentToString(desc.sequent.antecedents);
		}
		// res += "  |-------\n";
		res += "  ├─────── \n";
		if (desc.sequent.succedents) {
			res += sequentToString(desc.sequent.succedents);
		}
	}
	return res;
}

/**
 * Prettyprints sequents. Syntax highlighting is introduced as ansi codes when option useColors is true.
 */
export function formatSequent (desc: SequentDescriptor, opt?: {
	useColors?: boolean, 
	showAction?: boolean,
	htmlEncoding?: boolean,
	formulasOnly?: boolean
}): string {
	if (desc) {
		opt = opt || {};
		let res: string = "";
		if (!opt.formulasOnly) {
			if (desc.action && opt.showAction) {
				const action: string = desc.action.endsWith(",") ? desc.action.substr(0, desc.action.length - 1) : desc.action;
				res += opt.htmlEncoding ? `<br>${action}.<br>` : `\n${action}.\n`;
			}
			if (desc.commentary) {
				res += commentaryToString(desc.commentary, opt);
			}
		}
		if (desc.sequent) { // print label and comment only if the sequent is non-empty (sequent empty means proof completed)
			if (desc.label) {
				res += labelToString(desc.label, opt);
			}
			if (desc.comment) {
				res += commentToString(desc.comment, opt);
			}
			res += opt.htmlEncoding ? "<br>" : "\n";
			if (desc.sequent.antecedents) {
				res += sequentToString(desc.sequent.antecedents, opt);
			}
			// res += "  |-------\n";
			res += opt.htmlEncoding ? "  ├─────── <br>" : "  ├─────── \n";
			if (desc.sequent.succedents) {
				res += sequentToString(desc.sequent.succedents, opt);
			}
		}
		return (opt.htmlEncoding ? "<br>" : "\n") + res.trimRight();
	} else {
		console.error("[language-utils.format-proof-state] Error: proof state is null :/");
	}
	return null;
}


/**
 * Utility function, returns the header for a prooflite script
 * @param formulaName Formula proved by this prooflite script
 * @param theoryName Theory of the formula
 * @param status Proof status
 */
export function makeProofliteHeader (formulaName: string, theoryName: string, status: ProofStatus): string { 
	return `%-------------------------------------------
% @formula: ${formulaName} 
% @theory: ${theoryName}
% @status: ${getIcon(status)} ${status}
%-------------------------------------------\n`;
}
/**
 * Utility function, returns the prooflite script for the theorem indicated in the fuction arguments
 * @param desc 
 */
// export async function getProofLiteScript (desc: { 
// 	fileName: string, 
// 	fileExtension: string, 
// 	contextFolder: string, 
// 	theoryName: string, 
// 	formulaName: string
// }): Promise<string> {
// 	if (desc) {
// 		let proofScript: string = makeProofliteHeader(desc.formulaName, desc.theoryName, "untried");
// 		// check if the .jprf file contains the proof status
// 		const jprf_file: string = fsUtils.desc2fname({
// 			fileName: desc.fileName, 
// 			fileExtension: ".jprf", 
// 			contextFolder: desc.contextFolder
// 		});
// 		const proofFile: ProofFile = await readJprfProofFile(jprf_file);
// 		if (proofFile) {
// 			const proofDescriptors: ProofDescriptor[] = proofFile[`${desc.theoryName}.${desc.formulaName}`];
// 			if (proofDescriptors && proofDescriptors.length && proofDescriptors[0] && proofDescriptors[0].info) {
// 				proofScript = makeProofliteHeader(desc.formulaName, desc.theoryName, proofDescriptors[0].info.status);
// 				const proofLite: string[] = proofDescriptor2ProofLite(proofDescriptors[0]);
// 				if (proofLite && proofLite.length) {
// 					proofScript += proofLite.join("\n");
// 				}
// 			}
// 		}
// 		return proofScript;
// 	}
// 	return null;
// }

/**
 * Regex for parsing the content of prooflite files
 * The regex returns the following groups
 * group 0: the prooflite script for the formula indicated in desc
 * group 1: formulaName
 * group 2: theoryName
 * group 3: proof status
 * group 4: prooflite script
 * @param desc 
 */
export function proofliteRegexp(desc: { theoryName: string, formulaName: string }): RegExp {
	const formulaName: string = desc.formulaName.replace(/\?/g, "\\\\?");
	const theoryName: string = desc.theoryName.replace(/\?/g, "\\\\?");
	return new RegExp(`(?:(?:%--*.*)\\s*(?:%\\s*@formula\\s*:\\s*(${formulaName}))?\\s*(?:%\\s*@theory\\s*:\\s*(${theoryName}))?\\s*(?:%\\s*@status\\s*:\\s*(.*))?\\s*(?:%--*.*))?\\s*(${desc.formulaName}\\s*:\\s*PROOF[\\w\\W\\s]*QED\\s*${desc.formulaName})`, "g");
}
export function proofliteDeclRegexp(desc: { theoryName: string, formulaName: string }): RegExp {
	const formulaName: string = desc.formulaName.replace(/\?/g, "\\\\?");
	return new RegExp(`\\b${formulaName}\\s*:\\s*PROOF\\b`, "g");
}
// group 1 is formula name
export const proofRegexp: RegExp = /([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*(%.+)?\s*:\s*(%.+)?\s*(?:PROOF)\b/gim;



/**
 * @function getWordRange
 * @TODO improve this function, currently operators are not recognized/resolved
 * @description Utility function, identifies the range of the word at the cursor position.
 * 				Uses regular expressions designed to identify symbol names (\w+) and strings (\"([^\"]*)\")
 * @param txt The document that contains the word
 * @param position Position in the document
 */
export function getWordRange(txt: string, position: Position): Range {

	const testTokenizer = (regexp: RegExp, txt: string): { token: string, character: number } => {
		let match: RegExpMatchArray = regexp.exec(txt);
		let needle: number = -1;
		let token: string = null;
		while (match && match.index <= position.character) {
			needle = match.index;
			token = match[0];
			match = regexp.exec(txt);
		}
		if (needle >= 0 && needle + token.length >= position.character) {
			character = needle;
			len = token.length;
			return { token, character: needle };
		}
		return null;
	}

	let character: number = position.character;
	let len: number = 0;
	const lines: string[] = txt.split("\n");
	if (lines && lines.length > position.line) {
		const txt: string = lines[position.line];
		const strings: RegExp = /\"[\w\W]+?\"/g;
		const numbers: RegExp = /([+-]?\d+\.?\d*)\b/g;
		const keywords: RegExp = new RegExp(language.PVS_RESERVED_WORDS_REGEXP_SOURCE, "gi");
		const symbols: RegExp = /[A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*/g;///(\w+\??)/g;
		let ans: { token: string, character: number } = testTokenizer(new RegExp(strings), txt)
			|| testTokenizer(new RegExp(numbers), txt)
			|| testTokenizer(new RegExp(keywords), txt)
			|| testTokenizer(new RegExp(symbols), txt);
		if (ans) {
			character = ans.character;
			len = ans.token.length;
		}
	}
	return {
		start: { line: position.line, character: character },
		end: { line: position.line, character: character + len }
	};
}



/**
 * @function listSymbols
 * @TODO improve this function, currently operators are not recognized/resolved
 * @description Utility function, identifies symbols in the given text.
 * 				Uses regular expressions designed to identify symbol names (\w+) and strings (\"([^\"]*)\")
 * @param txt The document that contains the word
 * @param position Position in the document
 */
export function listSymbols(txt: string): string[] {
	if (txt) {
		const symbols: RegExp = /(\w+\??)/g;
		const keywords: RegExp = new RegExp(language.PVS_RESERVED_WORDS_REGEXP_SOURCE, "gi");

		let symbolsMap: { [ symbol: string ]: { line: number, character: number } } = {};
		let match: RegExpMatchArray = null;
		const lines: string[] = txt.split("\n");
		const maxIterations: number = 100;
		if (lines && lines.length) {
			for (let i = 0; i < lines.length; i++) {
				const txt: string = lines[i];
				if (txt) {
					for (let n = 0; n < maxIterations && (match = symbols.exec(txt)); n++) {
						if (match && !keywords.test(match[0]) && isNaN(+match[0])) { //isNaN(match[0]) is used to exclude numbers -- we don't want to include numbers in the list of symbols
							symbolsMap[match[0]] = { line: i, character: match.index }; // position is used for debugging purposes here
						}
					}
				}
			}
		}

		return Object.keys(symbolsMap);
	}
	return [];
}

export const pvsioBanner: string = `
╔════════════════════════════════════════════════════════════════════════════════════
║ PVSio Evaluator
║
║ Usage:
║ - Enter a PVS expression followed by ';'
║  or
║ - Enter a Lisp expresssion followed by '!'
║  or
║ - Exit the evaluator with 'quit;' 
║
║ TAB autocompletes commands.
║ UP/DOWN arrow keys recall previous/next command.
╚════════════════════════════════════════════════════════════════════════════════════
`
;

export const pvsioBannerAlt: string = `
╔════════════════════════════════════════════════════════════════════════════════════
║ PVSio Evaluator
║
║ Usage:
║ - Enter a PVS expression followed by ';'
║  or
║ - Enter a Lisp expresssion followed by '!'
╚════════════════════════════════════════════════════════════════════════════════════
`
;

export const pvsioPrompt: string = "<PVSio>";
export const proverPrompt: string = ">>";

// ║
// ║ Note: Evaluating PVS expressions which depend on unproven TCCs may be unsound
// ║       and result in the evaluator becoming unresponsive or breaking into Lisp. 
// ║       If that happens, please close the evaluator terminal and start a new session.

/**
 * @function getErrorRange
 * @description Utility function, identifies the range of a syntax error at the cursor position.
 * @param txt The document that contains the word
 * @param start Position in the document where the error starts
 * @param end Position in the document where the error ends
 */
export function getErrorRange(txt: string, start: Position, end: Position): Range {
	if (end) {
		return {
			start,
			end: {
				line: end.line,
				// indicate at least 2 characters otherwise the underlined error is hard to find in the text
				character: (start.line === end.line && start.character === end.character) ? end.character + 2 : end.character
			}
		};
	} else {
		let character: number = start.character;
		let len: number = 2; // indicate at least 2 characters otherwise the underlined error is hard to find in the text
		let lines: string[] = txt.split("\n");
		if (lines && (start.line - 1) < lines.length) {
			let txt: string = lines[start.line - 1];
			if (txt) {
				len = txt.length - start.character;
			}
		}
		return {
			start: { line: start.line, character: character },
			end: { line: start.line, character: character + len }
		};
	}
}


export function makeBranchId (desc: { branchId: string, goalId: number }): string {
	if (desc) {
		return (isNaN(desc.goalId)) ? desc.branchId 
				: (desc.branchId) ? `${desc.branchId}.${desc.goalId}` 
					: `${desc.goalId}`
	}
	return "";
}

export function getBranchId (label: string): string {
	if (label) {
		return label.split(".").slice(1).join(".");
	}
	return "";
}

export function pvsVersionToString (version: PvsVersionDescriptor): string {
	if (version) {
		return `PVS ${version["pvs-version"]} (${version["lisp-version"]})`
	}
	return null;
}


export function proofTree2Prl (proofDescriptor: ProofDescriptor): string | null {
	const content: string[] = proofDescriptor2ProofLite(proofDescriptor, { omitTags: true, escape: true });
	if (content && content.length) {
		return content.join("\n");
	}
	return null;
}
export function proofLite2proofTree (desc: { prf: string, parent?: ProofNode, proofName?: string }): ProofNode {
	if (desc) {
		if (desc.parent) {
			let currentNode: ProofNode = desc.parent;
			let plite: string = desc.prf.trim();
			if (plite.startsWith("(then ")) {
				// series of proof commands
				plite = plite.replace("(then ", "");
				const cmdArray: string[] = splitCommands(plite);
				for (let i = 0; i < cmdArray.length; i++) {
					if (cmdArray[i].startsWith("(spread ")) {
						// series of proof branches
						proofLite2proofTree({ prf: plite, parent: currentNode });
					} else {
						currentNode = {
							name: cmdArray[i],
							type: "proof-command",
							rules: [],
							branch: currentNode.branch
						};
						desc.parent.rules.push(currentNode);
					}
				}
			} else if (plite.startsWith("(spread ")) {
				// series of proof branches
				plite = plite.replace("(spread ", "");
				const cmdArray: string[] = splitCommands(plite);
				for (let i = 0; i < cmdArray.length; i++) {
					currentNode = {
						name: `(${i + 1})`,
						type: "proof-branch",
						rules: [],
						branch: desc.parent.branch
					};
					desc.parent.rules.push(currentNode);
					proofLite2proofTree({ prf: cmdArray[i], parent: currentNode });
				}
			} else if (plite.startsWith(`("" `)) {
				// proof start
				plite = plite.replace(`("" `, "");
				proofLite2proofTree({ prf: plite, parent: currentNode });
			}
		} else {
			desc.parent = {
				name: desc.proofName, // proof name
				rules: [], // sequence of proof rules
				type: "root", // node type
				branch: `""` // branch id			
			};
			proofLite2proofTree(desc);
		}
		return desc.parent;
	}
	return null;
}
export type ProofOrigin = ".prf" | ".prl" | ".jprf";
export function proofLite2ProofDescriptor (prf: string, info: {
	theory: string, formula: string, status: ProofStatus, prover: string, shasum: string, date?: string
}): ProofDescriptor {
	const pdesc: ProofDescriptor = new ProofDescriptor(info, ".prl");
	const script: string = prf.replace(/\s*\n+\s*/g, " "); // remove all \n introduced by pvs in the expression
	// capture group 1 is proofName
	// capture group 2 is formulaName,
	// capture group 3 is proofTree
	const data: RegExpMatchArray = new RegExp(prfRegExp).exec(script);
	if (data && data.length > 3) {
		// const proofName: string =  data[2];
		const prf: string = data[3];
		pdesc.proofTree = proofLite2proofTree({ prf, proofName: info.formula });
	}
	return pdesc;
}
/**
 * Utility function, converts a proof descriptor to a prooflite script
 * @param proofDescriptor 
 * @param opt 
 */
export function proofDescriptor2ProofLite (proofDescriptor: ProofDescriptor, opt?: { barDash?: boolean, omitTags?: boolean, escape?: boolean }): string[] | null {
	opt = opt || {};
	const proofTreeToProofLite = (nodes: ProofNode[], currentBranch?: string, indent?: number): string => {
		indent = indent || 0;
		let res: string = "";
		if (nodes && nodes.length) {
			let closeRight: boolean = false;
			for (let i = 0; i < nodes.length; i++) {
				const node: ProofNode = nodes[i];
				switch (node.type) {
					case "root": {
						res += `${" ".repeat(indent)}` + proofTreeToProofLite(node.rules, "", indent);
						break;
					}
					case "proof-command": {
						if (node.branch === currentBranch && i === 0 && (!node.rules || node.rules.length === 0)) {
							res += `(then `; // the parenthesis will be closed at the end of the loop
							closeRight = true;
						}
						const cmd: string = node.name.startsWith("(") ? node.name : `(${node.name})`;
						if (node.rules && node.rules.length) {
							// these rules are sub-goals (proof-branches)
							// make sure sub-goals are ordered in ascending order based on their name
							node.rules = node.rules.sort((a: ProofNode, b: ProofNode) => {
								const valA: number = +a.name.replace(/[\(\)]/g, "");
								const valB: number = +b.name.replace(/[\(\)]/g, "");
								return valA < valB ? -1 : 1;
							});
							indent++;
							if (i > 0) {
								indent++;
								res += `\n${" ".repeat(indent)}`;
							}
							res += `(spread `;
							indent++;
							res += cmd + `\n${" ".repeat(indent)}(` + proofTreeToProofLite(node.rules, node.branch, indent);
							res += `))`; // we need to close the extra parenthesis opened by spread
						} else {
							res += cmd + proofTreeToProofLite(node.rules, node.branch, indent);
						}
						break;
					}
					case "proof-branch":
						if (i > 0) {
							res += `\n${" ".repeat(indent + 1)}`;
						}
					default: {
						if (node.rules && node.rules.length) {
							res += proofTreeToProofLite(node.rules, node.branch, indent);
						} else {
							res += "(postpone)";
						}
						break;
					}
				}
			}
			if (closeRight) {
				res += ")";
			}
		}
		return res;
	}
	if (proofDescriptor) {
		const nodes: ProofNode[] = proofDescriptor.proofTree ? [ proofDescriptor.proofTree ] : null
		let script: string = proofTreeToProofLite(nodes);
		script = script || "(then (postpone))";
		if (opt.omitTags) {
			if (opt.escape) {
				script = script.replace(/[\n\r]/g, " ").replace(/\"/g, `\\"`)
			}
			return script.split("\n");
		} else {
			// the theory name will be given by the file name
			const res: string = `${proofDescriptor.info.formula} : PROOF\n`
				+ script
				+ `\nQED ${proofDescriptor.info.formula}`;
			return (opt.barDash) ? res.split("\n").map(line => { return "%|- " + line; })
				: res.split("\n");
		}
	}
	return null;
}

export function prf2ProofTree (desc: { prf: string, proofName: string }): ProofTree {
	const expandBranchNames = (node: ProofNode, branchId?: string): void => {
		if (node) {
			const goalId: number = +node.name;
			branchId = branchId || "";
			branchId = makeBranchId({ branchId, goalId });
			node.branch = branchId;
			for (let i = 0; i < node.rules.length; i++) {
				expandBranchNames(node.rules[i], branchId);
			}
		}
	}
	const getProofCommands = (prf: string): string => {
		let par: number = 0;
		let match: RegExpMatchArray = null;
		const regexp: RegExp = new RegExp(/([\(\)])/g);
		while(match = regexp.exec(prf)) {
			switch (match[1]) {
				case "(": { par++; break; }
				case ")": { par--; break; }
				default: {}
			}
			if (par === 0) {
				return prf.substr(0, match.index + 1);
			}
		}
		return "";
	}
	const buildProofTree_aux = (desc: { prf: string, proofName: string, parent: ProofNode }): void => {	
		if (desc && desc.parent) {
			while (desc.prf && desc.prf.length) {
				// series of proof branches or a proof command
				const expr: string = getProofCommands(desc.prf);
				if (expr && expr.length) {
					if (expr.startsWith("((")) {
						// series of proof branches
						// remove a matching pair of parentheses and iterate
						const match: RegExpMatchArray = /\(([\w\W\s]+)\s*\)/.exec(desc.prf);
						const subexpr: string = match[1];
						const currentParent: ProofNode = desc.parent.rules[desc.parent.rules.length - 1];
						buildProofTree_aux({ prf: subexpr, proofName: desc.proofName, parent: currentParent });
					} else if (expr.startsWith(`("`)) {
						// proof command from a labelled branch -- remove the label and iterate
						const match: RegExpMatchArray = /\(\"(\d+)\"\s*([\w\W\s]+)/.exec(expr);
						const subexpr: string = match[2].replace(/\s*\n\s*/g, " "); // remove all \n introduced by pvs in the expression
						const currentBranch: ProofNode = {
							name: match[1], 
							rules:[], 
							type: "proof-branch", 
							branch: match[1]
						};
						desc.parent.rules.push(currentBranch);
						buildProofTree_aux({ prf: subexpr, proofName: desc.proofName, parent: currentBranch });
					} else {
						// proof command
						// check if there's a comment at the beginning 
						const cmd: string = (expr.trim().startsWith(`";;; `)) ?
							expr.replace(/\";;;\s[^"]*\"\s*\(/g, "(") 
								: expr;
						desc.parent.rules.push({
							name: cmd,
							rules: [],
							type: "proof-command",
							branch: expr
						});
					}
					// update prf
					desc.prf = desc.prf.substr(expr.length).trim();
				} else {
					// ) parentheses comes before (, from parsing a series labelled branches, just ignore them and iterate
					const match: RegExpMatchArray = /\)+([\w\W\s]*)/.exec(desc.prf);
					// update prf
					desc.prf = match[1].trim(); // remove all \n introduced by pvs in the expression
					if (desc.prf && desc.prf.length) {
						buildProofTree_aux(desc);
					}
				}
			}
		} else {
			console.error("[pvs-proxy] Warning: unable to build proof tree (parent node is null)");
		}
	}

	if (desc && desc.prf) {
		// root node
		const rootNode: ProofNode = {
			name: desc.proofName,
			rules: [],
			type: "root",
			branch: "root"
		};
		let prf: string = desc.prf.trim();
		if (desc.prf.startsWith(`(""`)) {
			const match: RegExpMatchArray = /\(\"\"([\w\W\s]+)\s*\)/.exec(desc.prf);
			prf = match[1].trim();
		}
		if (prf) {
			// if (prf.startsWith("(then ")) {
			// 	// prf contains prooflite glassbox
			// 	proofLite2proofTree({ prf, proofName: desc.proofName, parent: rootNode });	
			// } else {
				// regular prf file
				buildProofTree_aux({ prf, proofName: desc.proofName, parent: rootNode });
				expandBranchNames(rootNode);
			// }
			return rootNode;
		} else {
			console.error("[pvs-proxy] Warning: unrecognised proof structure", desc.prf);
		}
	}
	return null;
}

/**
 * Utility function, checks if the provided proof script is null or empty
 * @param prf proof script
 */
export function isEmptyPrf(prf: string): boolean {
	return !prf || prf === `("" (postpone))`;
}
export function isEmptyProof (pdesc: ProofDescriptor): boolean {
	return !pdesc || !pdesc.proofTree || !pdesc.proofTree.rules || pdesc.proofTree.rules.length === 0
		|| (pdesc.proofTree.rules.length === 1 && pdesc.proofTree.rules[0].name === "(postpone)");
}

const grind: string = `("" (grind))`;
const postpone: string = `("" (postpone))`;

/**
 * Utility function, transforms a proof tree into a json object
 * @param desc Descriptor specifying proofTree, formulaName, proofName, and parent node (keeps track of the current parent in the proof tree, used in recursive calls)
 */
export function prf2jprf (desc: { 
	prf: string, 
	theoryName: string, 
	formulaName: string, 
	version: PvsVersionDescriptor, 
	shasum: string,
	status?: ProofStatus,
	autorun?: boolean
}): ProofDescriptor {
	if (desc) {
		const result: ProofDescriptor = new ProofDescriptor ({
			theory: desc.theoryName,
			formula: desc.formulaName,
			status: desc.status || "untried",
			prover: pvsVersionToString(desc.version) || "PVS 7.x",
			shasum: desc.shasum,
			date: new Date().toISOString()
		}, ".prf");
		if (desc.prf) {
			const script: string = desc.prf.replace(/\s*\n+\s*/g, " "); // remove all \n introduced by pvs in the expression
			// capture group 1 is proofName
			// capture group 2 is formulaName,
			// capture group 3 is proofTree
			const data: RegExpMatchArray = new RegExp(prfRegExp).exec(script);
			if (data && data.length > 3) {
				const proofName: string =  data[2]; //data[1];
				const formulaName: string = data[2];
				// consistency check
				if (formulaName !== `${desc.theoryName}.${desc.formulaName}`) {
					console.warn(`[language-utils] Warning: proof script for ${desc.theoryName}.${desc.formulaName} has unexpected signature ${formulaName}`);
				}
				const prf: string = isEmptyPrf(data[3])? postpone : data[3];
				const proof: ProofNode = prf2ProofTree({ prf, proofName });
				result.proofTree = proof;
				// console.dir(result, { depth: null });
			}
		} 
		return result;
	}
	return null;
}


// group 1 is the command argument
export const helpCommandRegexp: RegExp = /^\s*\(?\s*help\s*\"?([^\)]+)/g;
export function isHelpCommand (cmd: string): boolean {
	cmd = (cmd) ? cmd.trim() : cmd;
	return cmd && new RegExp(helpCommandRegexp).test(cmd);
}

// group 1 is the command argument
export const helpBangCommandRegexp: RegExp = /^\s*\(?\s*help!\s*\"?([^\)"]+)/g;
export function isHelpBangCommand (cmd: string): boolean {
	cmd = (cmd) ? cmd.trim() : cmd;
	return cmd && new RegExp(helpBangCommandRegexp).test(cmd);
}

// export function isSaveCommand (cmd: string): boolean {
// 	cmd = (cmd) ? cmd.trim() : cmd;
// 	return cmd && (cmd === "save" 
// 		|| cmd === "save;"
// 		|| cmd === "(save)"
// 		|| /\(\s*save\s*\)/g.test(cmd))
// 		;
// }

export function isMetaProofCommand (cmd: string): boolean {
	return isPostponeCommand(cmd) || isUndoStarCommand(cmd) || isShowHiddenCommand(cmd);
}


export function proofTree2commandSequence (node: ProofNode): string | null {
	if (node) {
		let ans: string = (node.type === "proof-command") ? node.name : "";
		if (node.rules) {
			for (let i = 0; i < node.rules.length; i++) {
				ans += proofTree2commandSequence(node.rules[i]);
			}
		}
		return ans;
	}
	return null;
}

export function applyTimeout (cmd: string, sec?: number): string {
	if (cmd && sec) {
		const c: string = cmd.startsWith('(') && cmd.endsWith(')') ? cmd : `(${cmd})`
		return `(apply ${c} :timeout ${sec})`;
	}
	return cmd;
}

export function noChange (result: { commentary: string | string[] }): boolean {
	if (result && result.commentary) {
		if (typeof result.commentary === "string") {
			return result.commentary.startsWith("No change on:")
		} else if (typeof result.commentary === "object") {
			return result.commentary.length
				&& result.commentary.filter((comment: string)=> {
					return comment.startsWith("No change on:");
				}).length > 0;
		}
	}
	return false;
}

export function getNoChangeCommand (result: { commentary: string | string[] }): string {
	if (result && result.commentary) {
		if (typeof result.commentary === "string") {
			return result.commentary.replace("No change on:", "").trim();
		} else if (typeof result.commentary === "object" && result.commentary.length) {
			const comments: string[] = result.commentary.filter((comment: string)=> {
				return comment.startsWith("No change on:");
			});
			if (comments && comments.length && typeof comments[0] === "string") {
				return comments[0].replace("No change on:", "").trim();
			}
		}
	}
	return null;
}

export function QED (result: { commentary: string | string[] }): boolean {
	if (result && result.commentary) {
		if (typeof result.commentary === "string") {
			return result.commentary.trim().startsWith("Q.E.D.");
		} else if (typeof result.commentary === "object") {
			return result.commentary.length
			&& result.commentary.filter((comment: string)=> {
				return comment.trim().startsWith("Q.E.D.");
			}).length > 0;
		}
	}
	return false;
}

export function isGlassboxTactic (cmd: string): boolean {
	return cmd && (cmd.startsWith("(then ") || cmd.startsWith("(spread "));
}

export function isInterruptCommand (cmd: string): boolean {
	cmd = (cmd) ? cmd.trim() : cmd;
	return cmd && (cmd === "interrupt" 
		|| cmd === "interrupt;"
		|| cmd === "(interrupt)"
		|| /^\(\s*interrupt\s*\);?/gi.test(cmd))
		;
}

export function interruptedByClient (result: { commentary: string | string[] }): boolean {
	if (result && result.commentary) {
		if (typeof result.commentary === "string") {
			return result.commentary.trim().includes("interrupted by client");
		} else if (typeof result.commentary === "object") {
			return result.commentary.length
			&& result.commentary.filter((comment: string)=> {
				return comment.trim().includes("interrupted by client");
			}).length > 0;
		}
	}
	return false;
}

export function branchComplete (result: { commentary: string | string[] }, formulaName: string, previousBranch: string): boolean {
	if (result && result.commentary) {
		if (typeof result.commentary === "string") {
			if (typeof previousBranch === "string") {
				return result.commentary.startsWith("This completes") 
					&& (result.commentary.endsWith(`${formulaName}.${previousBranch}.`)
						|| result.commentary.endsWith(`${formulaName}.${previousBranch}`.trim()));
			}
			return result.commentary.startsWith("This completes");

		} else if (typeof result.commentary === "object") {
			return result.commentary.length 
				&& result.commentary.filter((comment: string) => {
					if (typeof previousBranch === "string") {
						return comment.startsWith("This completes") 
							&& (comment.endsWith(`${formulaName}.${previousBranch}.`)
								|| comment.endsWith(`${formulaName}.${previousBranch}`.trim()));
					}
				return comment.startsWith("This completes");
			}).length > 0;
		}
	}
	return false;
}

export function siblingBranchComplete (result: { commentary: string[] }, newBranch: string): boolean {
	return result && result.commentary
		&& result.commentary.length 
		&& result.commentary.filter((comment: string) => {
			if (typeof newBranch === "string" && comment.startsWith("This completes")) {
				const parentOfNewBranch: string = newBranch.substring(0, newBranch.lastIndexOf("."));
				const closedBranch: string = comment.substring(comment.indexOf(".") + 1, comment.lastIndexOf("."));
				const parentOfClosedBranch: string = closedBranch.substring(0, closedBranch.lastIndexOf("."));
				return parentOfClosedBranch === parentOfNewBranch;
			}
			return comment.startsWith("This completes");
		}).length > 0;
}

export function branchHasChanged (desc: { newBranch: string, previousBranch: string }): boolean {
	if (desc) {
		const newBranch: string = desc.newBranch.replace(/T/g, "");
		const previousBranch: string = desc.previousBranch.replace(/T/g, "");
		return (newBranch !== previousBranch || previousBranch !== "" && !newBranch.startsWith(previousBranch));
	}
	return false;
}

export function pathHasChanged (desc: { newBranch: string, previousBranch: string }): boolean {
	if (desc) {
		const newBranch: string = desc.newBranch.replace(/T/g, "");
		const previousBranch: string = desc.previousBranch.replace(/T/g, "");
		return !newBranch.startsWith(previousBranch);
	}
	return false;
}

// these icons are shown correctly only on recent os distributions that include the proper font set.
// use https://iconify.design/icon-sets/ for proof explorer, to have a consistent look&feel on all systems.
export const icons: {
	checkmark: string,
	bang: string,
	snowflake: string,
	sparkles: string,
	whitecircle: string
} = {
	checkmark: "✅",
	bang : "❗",
	snowflake : "❄️",
	sparkles: "✨",
	whitecircle: "⚪"
};

export function getIcon (proofStatus: ProofStatus): string {
	switch (proofStatus) {
		case "subsumed":
		case "simplified":
		// case "proved - incomplete":
		// case "proved - by mapping":
		// case "proved - complete": 
		case "proved":
			return icons.checkmark;
		case "unfinished": // proof attempted but failed
			return icons.bang;
		case "unchecked":  // proof was successful, but needs to be checked again because of changes in the theories
			return icons.snowflake;
		case "unproved":
		case "untried": // proof has not been attempted yet
			return icons.sparkles;
	}
}

export function isProved (proofStatus: ProofStatus): boolean {
	switch (proofStatus) {
		case "subsumed":
		case "simplified":
		// case "proved - incomplete":
		// case "proved - by mapping":
		// case "proved - complete": 
		case "proved":
			return true;
		default:
			return false;
	}
}

export function isUnchecked (proofStatus: ProofStatus): boolean {
	return proofStatus === "unchecked";
}

export function makeEmptyTheory (theoryName: string): string {
	return `%%
% @theory: ${theoryName}
% @author: ${(process && process.env && process.env.USER) ? process.env.USER : "xxx"}
% @date: ${new Date().toUTCString()}
%%
${theoryName}: THEORY
  BEGIN 
    
  END ${theoryName}
`;
}


/**
 * Structure representing a proof tree
 */
export type NodeType = "root" | "proof-branch" | "proof-node" | "ghost";
export interface TreeStructure {
    id?: string,
	name?: string,
	type?: NodeType,
    status?: {
        visited: boolean,
        pending: boolean,
        complete: boolean,
        active: boolean
    },
    children?: TreeStructure[],
    parent?: TreeStructure,
    depth?: number, // distance from the root node. height = 0 for the root node
	height?: number, // greatest distance from any descendant. height = 0 for leaf nodes
	span?: number // total number of leaf nodes
};
