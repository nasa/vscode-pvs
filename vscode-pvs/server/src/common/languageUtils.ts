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

import * as fsUtils from './fsUtils';
import * as path from 'path';
import * as language from './languageKeywords';
import { FileList, FormulaDescriptor, PvsContextDescriptor, 
	TheoryDescriptor, ProofNode,  PvsFileDescriptor, PvsVersionDescriptor, ProofDescriptor, 
	ProofFile, ProofStatus, Position, Range, ProofTree, PvsFormula, PvsTheory, FileDescriptor 
} from '../common/serverInterface';
import * as utils from '../common/languageUtils';


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
// group 1 is theoryName, group 2 is comma-separated list of theory parameters -- NB: this regexp is fast but not accurate, because it does not check the end of the theory. See example use in codelense.
export const theoryRegexp: RegExp = /([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*(?:\[([\w+\W+\s+]+)\])?\s*\:\s*THEORY\s*BEGIN\b/gi;
export function endTheoryRegexp(theoryName: string): RegExp {
	return new RegExp(`(\\bEND\\s*)(${theoryName})(\\b)`, "gi");
}
export const datatypeRegexp: RegExp = /([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*(?:\[([\w\W\s]+)\])?\s*\:\s*DATATYPE\s*BEGIN\b/gi;
export const declarationRegexp: RegExp = /([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*(?:\[([\w\W\s]+)\])?\s*\:/gi;
// group 1 is the formula name
export const formulaRegexp: RegExp = /([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*(%.+)?\s*:\s*(%.+)?\s*(?:CHALLENGE|CLAIM|CONJECTURE|COROLLARY|FACT|FORMULA|LAW|LEMMA|PROPOSITION|SUBLEMMA|THEOREM|OBLIGATION|JUDGEMENT|AXIOM)\b/gim;
// same as formulaRegExp, but does not include JUDGEMENT and AXIOM
export const theoremRegexp: RegExp = /([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*(%.+)?\s*:\s*(%.+)?\s*(?:CHALLENGE|CLAIM|CONJECTURE|COROLLARY|FACT|FORMULA|LAW|LEMMA|PROPOSITION|SUBLEMMA|THEOREM|OBLIGATION)\b/gim;
// /(\w+)\s*(?:\%.*\s)*(?:\[([^\]]+)\])?\s*:\s*(?:\%.*\s)*\s*THEORY\b/gi;
export const tccRegexp: RegExp = /([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*:\s*OBLIGATION\b/gi;
export const tccStatusRegExp: RegExp = /%\s(proved|subsumed|simplified|unproved|unfinished|unchecked|untried)\b/g;
export const tccFormulaRegexp: RegExp = /[A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉\.]*_TCC\d+/g;
export function isTccFormula (desc: PvsFormula): boolean {
	return desc && desc.formulaName && new RegExp(tccFormulaRegexp).test(desc.formulaName);
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

/**
 * @function findTheoryName
 * @description Utility function, finds the name of the theory that immediately preceeds a given line
 * @param fileContent The text where the theory should be searched 
 * @param line The line in the document where search should end
 * @returns { string | null } The theory name if any is found, null otherwise
 */
export function findTheoryName(fileContent: string, line: number): string | null {
	if (fileContent) {
		const txt = fileContent.replace(commentRegexp, ""); // this removes all commented text
		const regexp: RegExp = theoryRegexp;
		let candidates: string[] = [];

		// check that line number is not before keyword begin -- if so adjust line number otherwise regexp won't find theory name
		const matchFirstTheory: RegExpMatchArray = regexp.exec(txt)
		if (matchFirstTheory && matchFirstTheory.length > 1) {
			const theoryName: string = matchFirstTheory[1];

			const matchEnd: RegExpMatchArray = endTheoryRegexp(theoryName).exec(txt);
			if (matchEnd && matchEnd.length) {
				regexp.lastIndex = matchEnd.index; // restart the search from here

				const min: number = matchFirstTheory[0].split("\n").length;
				line = (line < min) ? min : line;
				candidates.push(theoryName);
			}
		}

		// keep searching theory names -- the first element in candidates will be the closest to the current line number
		const text: string = txt.split("\n").slice(0, line + 1).join("\n");
		let match: RegExpMatchArray = null;
		while (match = regexp.exec(text)) {
			if (match.length > 1 && match[1]) {
				const theoryName: string = match[1];

				const matchEnd: RegExpMatchArray = endTheoryRegexp(theoryName).exec(txt);
				if (matchEnd && matchEnd.length) {
					regexp.lastIndex = matchEnd.index; // restart the search from here
					candidates = [ theoryName ].concat(candidates);
				}
			}
		}
		if (candidates.length > 0) {
			return candidates[0];
		}
	}
	return null;
};


/**
 * @function listTheoryNames
 * @description Utility function, returns a list of all theories in the given file
 * @param fileContent The text where the theory should be searched 
 * @returns string[] the list of theories in the given text
 */
export function listTheoryNames (fileContent: string): string[] {
	const ans: string[] = [];
	if (fileContent) {
		const txt = fileContent.replace(commentRegexp, "");
		let match: RegExpMatchArray = null;
		const regexp: RegExp = theoryRegexp;
		while (match = regexp.exec(txt)) {
			if (match.length > 1 && match[1]) {
				const theoryName: string = match[1];

				const matchEnd: RegExpMatchArray = endTheoryRegexp(theoryName).exec(txt);
				if (matchEnd && matchEnd.length) {
					regexp.lastIndex = matchEnd.index; // restart the search from here
					ans.push(theoryName);
				}
			}
		}
	}
	return ans;
};

/**
 * Utility function, returns the list of theories defined in a given pvs file
 * @param fname Path to a pvs file
 */
export async function listTheoriesInFile (fname: string, opt?: { content?: string }): Promise<TheoryDescriptor[]> {
	// console.log(`listing theories in file ${fname}`);
	opt = opt || {};
	if (fname) {
		const fileName: string = fsUtils.getFileName(fname);
		const fileExtension: string = fsUtils.getFileExtension(fname);
		const contextFolder: string = fsUtils.getContextFolder(fname);
		const fileContent: string = (opt.content) ? opt.content : await fsUtils.readFile(fname);
		if (fileContent) {
			const response: TheoryDescriptor[] = listTheories({ fileName, fileExtension, contextFolder, fileContent });
			// console.dir(response);
			return response;
		}
	}
	return null;
}


/**
 * Utility function, returns the list of theories defined in a given pvs file
 * @param fname Path to a pvs file
 */
export async function mapTheoriesInFile (fname: string): Promise<{ [ key: string ]: TheoryDescriptor }> {
	if (fname) {
		const fileName: string = fsUtils.getFileName(fname);
		const fileExtension: string = fsUtils.getFileExtension(fname);
		const contextFolder: string = fsUtils.getContextFolder(fname);
		const fileContent: string = await fsUtils.readFile(fname);
		if (fileContent) {
			const response: TheoryDescriptor[] = listTheories({ fileName, fileExtension, contextFolder, fileContent });
			const theoryMap: { [ key: string ]: TheoryDescriptor } = {};
			for (const i in response) {
				theoryMap[ response[i].theoryName ] = response[i];
			}
			return theoryMap;
		}
	}
	return null;
}


/**
 * Utility function, finds all theories in a given file
 * @param desc Descriptor indicating filename, file extension, context folder, and file content
 */
export function listTheories(desc: { fileName: string, fileExtension: string, contextFolder: string, fileContent: string, prelude?: boolean }): TheoryDescriptor[] {
	let ans: TheoryDescriptor[] = [];
	if (desc && desc.fileContent) {
		const txt: string = desc.fileContent.replace(commentRegexp, "");
		const start: number = Date.now();
		const regexp: RegExp = theoryRegexp;
		let match: RegExpMatchArray = null;
		while (match = regexp.exec(txt)) {
			if (match.length > 1 && match[1]) {
				const theoryName: string = match[1];

				const matchEnd: RegExpMatchArray = endTheoryRegexp(theoryName).exec(txt);
				if (matchEnd && matchEnd.length) {
					theoryRegexp.lastIndex = matchEnd.index; // restart the search from here

					const clip: string = txt.slice(0, match.index);
					const lines: string[] = clip.split("\n"); 
					const line: number = lines.length;
					const character: number = 0; //match.index - lines.slice(-1).join("\n").length;
					ans.push({
						theoryName: desc.fileExtension === ".tccs" && theoryName.endsWith("_TCCS") ? theoryName.substr(0, theoryName.length - 5) : theoryName,
						position: {
							line: line,
							character: character
						},
						fileName: desc.fileName,
						fileExtension: desc.fileExtension,
						contextFolder: desc.contextFolder
					});
				}
			}
		}
		const stats: number = Date.now() - start;
		// console.log(`[languageUtils] listTheories(${desc.fileName}) completed in ${stats}ms`);
	}
	return ans;
}

/**
 * Utility function, returns the list of theories defined in a given pvs file
 * @param fname Path to a pvs file
 */
export async function listTheoremsInFile (fname: string, opt?: { content?: string }): Promise<FormulaDescriptor[]> {
	opt = opt || {};
	if (fname) {
		const fileName: string = fsUtils.getFileName(fname);
		const fileExtension: string = fsUtils.getFileExtension(fname);
		const contextFolder: string = fsUtils.getContextFolder(fname);
		const fileContent: string = (opt.content) ? opt.content : await fsUtils.readFile(fname);
		if (fileContent) {
			const response: FormulaDescriptor[] = await listTheorems({ fileName, fileExtension, contextFolder, fileContent });
			return response;
		}
	}
	return null;
}


export interface SFormula {
	labels: string[];
	changed: boolean;
	formula: string;
	'names-info': any[];
}
export type SequentDescriptor = {
	path?: string,
	label: string,
	commentary: string[] | string,
	action?: string // this field is useless
	"num-subgoals"?: number,
	"prev-cmd"?: Object | string; // object representing the last command executed
	comment?: string,
	sequent?: {
		succedents?: SFormula[], 
		antecedents?: SFormula[],
		"hidden-succedents"?: SFormula[], 
		"hidden-antecedents"?: SFormula[]
	}
};

// export type ProverResult = ProofState | ProofState[]

function sequentToString(s: SFormula[], opt?: { useColors?: boolean }): string {
	let res: string = "";
	opt = opt || {};
	s.forEach((sequent: SFormula) => {
		let label: string = sequent.labels.join(" ");
		label = (sequent.changed) ? `{${label}}` : `[${label}]` ;
		label = (sequent.changed && opt.useColors) ? `${colorText(label, textColor.green)}` : `${label}` ;
		const formula: string = (opt.useColors) ? `${pvsCliSyntaxHighlighting(sequent.formula)}` : sequent.formula;
		res += `${label}   ${formula}\n`;
	});
	return res;
}

function labelToString (label: string, opt?: { useColors?: boolean }): string {
	return (opt && opt.useColors)?
		`\n${colorText(`${label} :`, textColor.blue)}\n`
			: `\n${label} :\n`;
}

function commentToString (label: string, opt?: { useColors?: boolean }): string {
	return (opt && opt.useColors)?
		`\n${colorText(`${label} :`, textColor.yellow)}\n`
			: `\n${label} :\n`;
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

export function pvsCliSyntaxHighlighting(text: string): string {
	if (text) {
		// numbers and operators should be highlighted first, otherwise the regexp will change characters introduced to colorize the string
		const number_regexp: RegExp = new RegExp(language.PVS_NUMBER_REGEXP_SOURCE, "g");
		text = text.replace(number_regexp, (number: string) => {
			return colorText(number, textColor.yellow);
		});
		const operators_regexp: RegExp = new RegExp(language.PVS_LANGUAGE_OPERATORS_REGEXP_SOURCE, "g");
		text = text.replace(operators_regexp, (op: string) => {
			return colorText(op, textColor.blue);
		});
		const keywords_regexp: RegExp = new RegExp(language.PVS_RESERVED_WORDS_REGEXP_SOURCE, "gi");
		text = text.replace(keywords_regexp, (keyword: string) => {
			return colorText(keyword, textColor.blue);
		});
		const function_regexp: RegExp = new RegExp(language.PVS_LIBRARY_FUNCTIONS_REGEXP_SOURCE, "g");
		text = text.replace(function_regexp, (fname: string) => {
			return colorText(fname, textColor.green);
		});
		const builtin_types_regexp: RegExp = new RegExp(language.PVS_BUILTIN_TYPE_REGEXP_SOURCE, "g");
		text = text.replace(builtin_types_regexp, (tname: string) => {
			return colorText(tname, textColor.green);
		});
		const truefalse_regexp: RegExp = new RegExp(language.PVS_TRUE_FALSE_REGEXP_SOURCE, "gi");
		text = text.replace(truefalse_regexp, (tf: string) => {
			return colorText(tf, textColor.blue);
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

export function formatSequent (proofState: SequentDescriptor, opt?: { useColors?: boolean, showAction?: boolean }): string {
	if (proofState) {
		opt = opt || {};
		let res: string = "";
		if (proofState.action && opt.showAction) {
			const action: string = proofState.action.endsWith(",") ? proofState.action.substr(0, proofState.action.length - 1) : proofState.action;
			res += `\n${action}.\n`;
		}
		if (proofState.label) {
			res += labelToString(proofState.label, opt);
		}
		if (proofState.comment) {
			res += commentToString(proofState.comment, opt);
		}
		if (proofState.sequent) {
			res += "\n";
			if (proofState.sequent.antecedents) {
				res += sequentToString(proofState.sequent.antecedents, opt);
			}
			// res += "  |-------\n";
			res += "  ├───────\n";
			if (proofState.sequent.succedents) {
				res += sequentToString(proofState.sequent.succedents, opt);
			}
		}
		return "\n" + res.trim();
	} else {
		console.error("[language-utils.format-proof-state] Error: proof state is null :/");
	}
	return null;
}

export function getActualProofStatus (desc: ProofDescriptor, shasum: string): ProofStatus {
	if (desc) {
		if (shasum === desc.info.shasum) {
			return desc.info.status;
		} else {
			return (desc.info.status === "proved") ? "unchecked"
				: (desc.proofTree && desc.proofTree.rules && desc.proofTree.rules.length) ?
					"unfinished" : "untried";
		}
	}
	return "untried";
}	

/**
 * Utility function, returns the status of the proof for the theorem indicated in the fuction arguments
 * @param desc 
 */
export async function getProofStatus (desc: { 
	fileName: string, 
	fileExtension: string, 
	contextFolder: string, 
	theoryName: string, 
	formulaName: string
}): Promise<ProofStatus> {
	if (desc) {
		let status: ProofStatus = "untried";
		// check if the .jprf file contains the proof status
		const jprf_file: string = fsUtils.desc2fname({
			fileName: desc.fileName, 
			fileExtension: ".jprf", 
			contextFolder: desc.contextFolder
		});
		const proofFile: ProofFile = await readJprfProofFile(jprf_file);
		if (proofFile) {
			const proofDescriptors: ProofDescriptor[] = proofFile[`${desc.theoryName}.${desc.formulaName}`];
			if (proofDescriptors && proofDescriptors.length && proofDescriptors[0] && proofDescriptors[0].info) {
				if (desc.fileExtension === ".tccs") {
					status = proofDescriptors[0].info.status; // for tccs we choose not to adjust the status because tccs are automatically generated by pvs and status of the formula is visible to the user as a comment -- we don't want to confuse them
				} else {
					// compute shasum for the file, and check it with the shasum saved in the proof descriptor. If the two differ, then the file has changed and the proof status is not valid anymore
					const shasum: string = await fsUtils.shasumFile(desc);
					status = getActualProofStatus(proofDescriptors[0], shasum);
				}
			}
		}
		return status;
	}
	return null;
}

/**
 * Utility function, returns the proof descriptor for a given formula
 * @param formula 
 */
export async function getProofDescriptor (formula: PvsFormula): Promise<ProofDescriptor> {
	if (formula) {
		// check if the .jprf file contains the proof status
		const jprf_file: string = fsUtils.desc2fname({
			fileName: formula.fileName, 
			fileExtension: ".jprf", 
			contextFolder: formula.contextFolder
		});
		const proofFile: ProofFile = await readJprfProofFile(jprf_file);
		if (proofFile) {
			const proofDescriptors: ProofDescriptor[] = proofFile[`${formula.theoryName}.${formula.formulaName}`];
			if (proofDescriptors && proofDescriptors.length) {
				return proofDescriptors[0];
			}
		}
	}
	return null;
}

/**
 * Utility function, updates the proof descriptor for a given formula
 * @param formula 
 */
export async function saveProofDescriptor (formula: PvsFormula, newDesc: ProofDescriptor, opt?: { saveProofTree?: boolean }): Promise<boolean> {
	if (formula && newDesc) {
		opt = opt || {};
		const fname: string = fsUtils.desc2fname({
			fileName: formula.fileName,
			fileExtension: ".jprf",
			contextFolder: formula.contextFolder
		});
		const key: string = `${formula.theoryName}.${formula.formulaName}`;
		let fdesc: ProofFile = await readJprfProofFile(fname);
		// check if file contains a proof for the given formula
		if (fdesc && fdesc[key] && fdesc[key].length) {
			// we are updating only the default proof, this might be changed in the future
			const pdesc: ProofDescriptor = fdesc[key][0];
			if (pdesc.info) {
				pdesc.info.shasum = newDesc.info.shasum ? newDesc.info.shasum : pdesc.info.shasum;
				pdesc.info.prover = newDesc.info.prover ? newDesc.info.prover : pdesc.info.prover;
				pdesc.info.status = newDesc.info.status ? newDesc.info.status : pdesc.info.status;
				pdesc.info.date = newDesc.info.date ? newDesc.info.date : pdesc.info.date;
			}
			if (opt.saveProofTree) {
				pdesc.proofTree = newDesc.proofTree ? newDesc.proofTree : pdesc.proofTree;
			}
		} else {
			fdesc[key] = fdesc[key] || [ newDesc ];
		}
		// write descriptor to file
		const newContent: string = JSON.stringify(fdesc, null, " ");
		return await fsUtils.writeFile(fname, newContent);		
	}
	return false;
}

/**
 * Utility function, saves the sketchpad with proof clips for a given formula
 * @param formula 
 */
export async function saveSketchpad (formula: PvsFormula, clips: ProofNode[]): Promise<boolean> {
	if (formula) {
		const fname: string = fsUtils.desc2fname({
			fileName: formula.fileName,
			fileExtension: ".jprf",
			contextFolder: formula.contextFolder
		});
		const key: string = `${formula.theoryName}.${formula.formulaName}`;
		let fdesc: ProofFile = await readJprfProofFile(fname);
		// check if file contains a proof for the given formula
		if (fdesc && fdesc[key] && fdesc[key].length) {
			// update clips for default proof
			const pdesc: ProofDescriptor = fdesc[key][0];
			pdesc.clips = clips || [];
			// write descriptor to file
			const newContent: string = JSON.stringify(fdesc, null, " ");
			return await fsUtils.writeFile(fname, newContent);		
		}
	}
	return false;
}

/**
 * Utility function, opens the sketchpad with proof clips for a given formula
 * @param formula 
 */
export async function openSketchpad (formula: PvsFormula): Promise<ProofNode[] | null> {
	if (formula) {
		const fname: string = fsUtils.desc2fname({
			fileName: formula.fileName,
			fileExtension: ".jprf",
			contextFolder: formula.contextFolder
		});
		const key: string = `${formula.theoryName}.${formula.formulaName}`;
		let fdesc: ProofFile = await readJprfProofFile(fname);
		// check if file contains a proof for the given formula
		if (fdesc && fdesc[key] && fdesc[key].length) {
			// update clips for default proof
			const pdesc: ProofDescriptor = fdesc[key][0];
			return pdesc.clips;
		}
	}
	return null;
}

/**
 * Utility function, returns the date (day and time) a given proof was saved
 * @param desc 
 */
export async function getProofDate (desc: { 
	fileName: string, 
	fileExtension: string, 
	contextFolder: string, 
	theoryName: string, 
	formulaName: string
}): Promise<string | null> {
	if (desc) {
		// check if the .jprf file contains the date
		const jprf_file: string = fsUtils.desc2fname({
			fileName: desc.fileName, 
			fileExtension: ".jprf", 
			contextFolder: desc.contextFolder
		});
		const proofFile: ProofFile = await readJprfProofFile(jprf_file);
		if (proofFile) {
			const proofDescriptors: ProofDescriptor[] = proofFile[`${desc.theoryName}.${desc.formulaName}`];
			if (proofDescriptors && proofDescriptors.length && proofDescriptors[0] && proofDescriptors[0].info) {
				return proofDescriptors[0].info.date;
			}
		}
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
export async function getProofliteScript (desc: { 
	fileName: string, 
	fileExtension: string, 
	contextFolder: string, 
	theoryName: string, 
	formulaName: string 
}): Promise<string> {
	const fname: string = path.join(desc.contextFolder, `${desc.fileName}${desc.fileExtension}`);
	const txt: string = await fsUtils.readFile(fname);
	const matchProoflite: RegExpMatchArray = proofliteRegexp(desc).exec(txt);
	return matchProoflite ? matchProoflite[0] : null;
}
export async function getProofLitePosition (desc: { formula: PvsFormula, proofFile: FileDescriptor }): Promise<number> {
	if (desc && desc.formula && desc.proofFile) {
		const fname: string = fsUtils.desc2fname(desc.proofFile);
		const txt: string = await fsUtils.readFile(fname);
		const matchProoflite: RegExpMatchArray = proofliteDeclRegexp(desc.formula).exec(txt);
		if (matchProoflite) {
			const slice: string = txt.slice(0, matchProoflite.index);
			const lines: string[] = slice.split("\n");
			return lines.length;
		}
	}
	return 0;
}

/**
 * Utility function, returns the list of theorems defined in a given pvs file
 * @param desc Descriptor indicating filename, file extension, context folder, file content, and whether the file in question is the prelude (flag prelude)
 */
export async function listTheorems (desc: { fileName: string, fileExtension: string, contextFolder: string, fileContent: string, prelude?: boolean, cache?: { theories?: TheoryDescriptor[] } }): Promise<FormulaDescriptor[]> {
	if (desc && desc.fileContent) {
		const theories: TheoryDescriptor[] = (desc.cache && desc.cache.theories) ? desc.cache.theories : listTheories(desc);
		const boundaries: { theoryName: string, from: number, to: number }[] = []; // slices txt to the boundaries of the theories
		if (theories) {
			const start: number = Date.now();
			const fileContent: string = desc.fileContent.replace(commentRegexp, ""); // first, remove all comments
			const slices: string[] = fileContent.split("\n");
			for (let i = 0; i < theories.length; i++) {
				boundaries.push({
					theoryName: theories[i].theoryName,
					from: theories[i].position.line,
					to: (i + 1 < theories.length) ? theories[i + 1].position.line : slices.length
				});
			}
			const formulaDescriptors: FormulaDescriptor[] = [];
			for (let i = 0; i < boundaries.length; i++) {
				const content: string = slices.slice(boundaries[i].from - 1, boundaries[i].to - 1).join("\n");
				if (content && content.trim()) {
					const regex: RegExp = theoremRegexp;
					let match: RegExpMatchArray = null;
					while (match = regex.exec(content)) {
						if (match.length > 1 && match[1]) {
							const formulaName: string = match[1];
							const slice: string = content.slice(0, match.index);
							const offset: number = (slice) ? slice.split("\n").length : 0;
							const line: number = boundaries[i].from + offset - 1;
							const isTcc: boolean = desc.fileExtension === ".tccs";
							let status: ProofStatus = "untried";
							// if (isTcc) {
							// 	const matchStatus: RegExpMatchArray = tccStatusRegExp.exec(slice);
							// 	if (matchStatus && matchStatus.length > 1 && matchStatus[1]) {
							// 		status = <ProofStatus> matchStatus[1];
							// 	}
							// } else {
							if (desc.prelude) {
								status = "proved";
							} else {
								// check if the .jprf file contains the proof status
								const theoryName: string = boundaries[i].theoryName;
								status = await getProofStatus({
									fileName: desc.fileName, 
									fileExtension: desc.fileExtension, 
									contextFolder: desc.contextFolder,
									formulaName,
									theoryName
								});
							}
							const fdesc: FormulaDescriptor = {
								fileName: desc.fileName,
								fileExtension: desc.fileExtension,
								contextFolder: desc.contextFolder,
								theoryName: boundaries[i].theoryName,
								formulaName,
								position: { line, character: 0 },
								status,
								isTcc
							}
							formulaDescriptors.push(fdesc);
						}
					}
				} else {
					console.error("Error while finding theory names :/");
				}
			}
			const stats: number = Date.now() - start;
			// console.log(`[languageUtils] listTheorems(${desc.fileName}) completed in ${stats}ms`);
			return formulaDescriptors;
		}
	}
	return [];
}

/**
 * @function findFormulaName
 * @description Utility function, finds the name of a theorem that immediately preceeds a given line
 * @param fileContent The text where the theory should be searched 
 * @param line The line in the document where search should end
 * @returns { string | null } The theory name if any is found, null otherwise
 */
export function findFormulaName(fileContent: string, line: number): string | null {
	if (fileContent) {
		const txt: string = fileContent.replace(commentRegexp, "");
		let text: string = txt.split("\n").slice(0, line + 1).join("\n");
		let candidates: string[] = [];
		// (?:\%.*\s)* removes comments
		const regexp: RegExp = formulaRegexp;
		let match: RegExpMatchArray = null;
		while(match = regexp.exec(text)) {
			if (match && match.length > 1 && match[1]) {
				candidates.push(match[1]);
			}
		}
		if (candidates.length > 0) {
			return candidates[candidates.length - 1];
		}
	}
	return null;
};

/**
 * @function findProofObligation
 * @description Utility function, finds the line of a proof obligation
 * @param txt The text where the proof obligation should be searched 
 * @returns { string | null } The theory name if any is found, null otherwise
 */
export function findProofObligation(formulaName: string, txt: string): number {
	const formula: string = formulaName.replace("?", "\\?");
	const regexp: RegExp = new RegExp(`\\b${formula}:\\s*OBLIGATION\\b`, "g");
	let match: RegExpMatchArray = regexp.exec(txt);
	if (match) {
		const trim: string = txt.substr(0, match.index);
		if (trim && trim.length > 0) {
			return trim.split("\n").length;
		}
	}
	return 0;
};


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
		const symbols: RegExp = /(\w+\??)/g; // TODO: negative lookahead to remove strings
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
║ PVSio
║
║ How to use the evaluator:
║ - Enter a PVS expression followed by ';'
║  or
║ - Enter a Lisp expresssion followed by '!'
║ 
║ To exit the evalutor, enter 'exit'.
║ You can use TAB to complete commands at the PVSio prompt.
╚════════════════════════════════════════════════════════════════════════════════════
`
;

export const pvsioPrompt: string = "<PVSio>";
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

// based on the 256 color scheme, see colors at https://misc.flogisoft.com/bash/tip_colors_and_formatting
export const textColor: { [ key: string ]: number } = {
	blue: 32,
	yellow: 3,
	green: 10,
	red: 90 // this is actually magenta
}

export function colorText(text: string, colorCode: number): string {
    // \x1b[0m resets all attributes
	return `\x1b[38;5;${colorCode}m${text.toLocaleString()}\x1b[0m`;
}


/**
 * Lists all theorems in a given context folder
 */
export async function getContextDescriptor (contextFolder: string, opt?: { listTheorems?: boolean, includeTccs?: boolean }): Promise<PvsContextDescriptor> {
	const response: PvsContextDescriptor = {
		fileDescriptors: {},
		contextFolder
	};
	const fileList: FileList = await fsUtils.listPvsFiles(contextFolder);
	if (fileList) {
		for (let i in fileList.fileNames) {
			const fname: string = path.join(contextFolder, fileList.fileNames[i]);
			const desc: PvsFileDescriptor = await getFileDescriptor(fname, opt);
			response.fileDescriptors[fname] = desc;
		}
	}
	return response;
}

export async function getFileDescriptor (fname: string, opt?: { listTheorems?: boolean, includeTccs?: boolean }): Promise<PvsFileDescriptor> {
	opt = opt || {};
	opt.listTheorems = (opt.listTheorems !== undefined) ? opt.listTheorems : true;
	const start: number = Date.now();
	const contextFolder: string = fsUtils.getContextFolder(fname);
	const fileName: string = fsUtils.getFileName(fname);
	const fileExtension: string = fsUtils.getFileExtension(fname);
	const response: PvsFileDescriptor = {
		theories: [],
		contextFolder,
		fileName,
		fileExtension
	};
	const pvsFileContent: string = await fsUtils.readFile(fname);
	const tccsFileContent: string = (opt.includeTccs) ? await fsUtils.readFile(path.join(contextFolder, `${fileName}.tccs`)) : null;
	// console.log(`[languageUtils.getFileDescriptor] listTheories(${fileName})`);
	const theories: TheoryDescriptor[] = listTheories({ fileName, fileExtension, contextFolder, fileContent: pvsFileContent });
	if (theories) {
		// if (opt.listTheorems) { console.log(`[languageUtils.getFileDescriptor] listTheorems(${fileName})`);	}
		const lemmas: FormulaDescriptor[] = 
			(opt.listTheorems) 
				? await listTheorems({ fileName, fileExtension, contextFolder, fileContent: pvsFileContent, prelude: false, cache: { theories } })
					: [];
		const tccs: FormulaDescriptor[] = 
			(opt.listTheorems && fileExtension !== ".tccs" && tccsFileContent) 
				? await listTheorems({ fileName, fileExtension: ".tccs", contextFolder, fileContent: tccsFileContent, prelude: false })
					: [];
		const descriptors: FormulaDescriptor[] = lemmas.concat(tccs);
		for (let i = 0; i < theories.length; i++) {
			const theoryName: string = theories[i].theoryName;
			const position: Position = theories[i].position;
			const theoryDescriptor: TheoryDescriptor = {
				fileName, fileExtension, contextFolder, theoryName, position, 
				theorems: (descriptors && descriptors.length) ? descriptors.filter((desc: FormulaDescriptor) => {
					return desc.theoryName === theoryName;
				}) : []
			}
			response.theories.push(theoryDescriptor);
		}
	}
	const stats: number = Date.now() - start;
	// console.log(`[languageUtils.getFileDescriptor] File descriptor for ${fname} created in ${stats}ms`);
	return response;
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

export async function renameFormulaInProofFile (formula: PvsFormula, newInfo: { newFormulaName: string, newShasum?: string }): Promise<boolean> {
	if (formula && newInfo && newInfo.newFormulaName) {
		const fname: string = fsUtils.desc2fname({
			fileName: formula.fileName,
			fileExtension: ".jprf",
			contextFolder: formula.contextFolder
		});
		const fdesc: ProofFile = await readJprfProofFile(fname);
		// check if file contains a proof for the given formula
		const key: string = `${formula.theoryName}.${formula.formulaName}`;
		if (fdesc && fdesc[key] && fdesc[key].length) {
			const newKey: string = `${formula.theoryName}.${newInfo.newFormulaName}`;
			// we are updating only the default proof, this might be changed in the future
			const pdesc: ProofDescriptor = fdesc[key][0];
			if (pdesc.info) {
				pdesc.info.formula = newInfo.newFormulaName;
				pdesc.info.shasum = newInfo.newShasum || pdesc.info.shasum;
			}
			if (pdesc.proofTree) {
				pdesc.proofTree.name = newKey;
			}
			// delete old fdesc entry
			delete fdesc[key];
			// change old shasum for all other proofs
			const keys: string[] = Object.keys(fdesc);
			for (let i = 0; i < keys.length; i++) {
				if (fdesc[keys[i]] && fdesc[keys[i]].length && fdesc[keys[i]][0].info) {
					fdesc[keys[i]][0].info.shasum = newInfo.newShasum;
				}
			}
			// add new key
			fdesc[newKey] = [ pdesc ];			
			// write to file
			const newContent: string = JSON.stringify(fdesc, null, " ");
			return await fsUtils.writeFile(fname, newContent);
		}
	}
	return false;
}
export async function renameTheoryInProofFile (theory: PvsTheory, newInfo: { newTheoryName: string, newShasum?: string }): Promise<boolean> {
	if (theory && newInfo && newInfo.newTheoryName) {
		const fname: string = fsUtils.desc2fname({
			fileName: theory.fileName,
			fileExtension: ".jprf",
			contextFolder: theory.contextFolder
		});
		const fdesc: ProofFile = await readJprfProofFile(fname);
		// update all proofs
		// check if file contains a proof for the given formula
		if (fdesc) {
			const keys: string[] = Object.keys(fdesc);
			if (keys && keys.length) {
				const newFdesc: ProofFile = {};
				for (let i = 0; i < keys.length; i++) {
					const key: string = keys[i];
					// we are updating only the default proof, this might be changed in the future
					const pdesc: ProofDescriptor = fdesc[key][0];
					if (pdesc.info) {
						pdesc.info.theory = newInfo.newTheoryName;
						pdesc.info.shasum = newInfo.newShasum || pdesc.info.shasum;
					}
					const newKey: string = `${newInfo.newTheoryName}.${pdesc.info.formula}`;
					if (pdesc.proofTree) {
						pdesc.proofTree.name = newKey;
					}
					newFdesc[newKey] = [ pdesc ];
				}	
				// write to file
				const newContent: string = JSON.stringify(newFdesc, null, " ");
				const newFname: string = fsUtils.desc2fname({
					fileName: (theory.fileName === theory.theoryName) ? newInfo.newTheoryName : theory.fileName,
					fileExtension: ".jprf",
					contextFolder: theory.contextFolder		
				});
				const fileAlreadyExists: boolean = await fsUtils.fileExists(newFname);
				const success: boolean = (fileAlreadyExists) ? await fsUtils.writeFile(fname, newContent)
					: await fsUtils.writeFile(newFname, newContent);
				if (success && !fileAlreadyExists && fname !== newFname) {
					fsUtils.deleteFile(fname);
				}
				return success;
			}
		}
	}
	return false;
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
export function proofLite2ProofDescriptor (prf: string, info: {
	theory: string, formula: string, status: ProofStatus, prover: string, shasum: string, date?: string
}): ProofDescriptor {
	const pdesc: ProofDescriptor = new ProofDescriptor(info);
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
						desc.parent.rules.push({
							name: expr,
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
 * Reads the content of a .jprf file
 * @param fname Name of the prooflite file
 * @param opt Optionals
 *               - quiet (boolean): if true, the function will not print any message to the console. 
 */
export async function readJprfProofFile (fname: string, opt?: { quiet?: boolean }): Promise<ProofFile> {
	opt = opt || {};
	let proofFile: ProofFile = {};
	fname = fname.replace("file://", "");
	fname = fsUtils.tildeExpansion(fname);
	const content: string = await fsUtils.readFile(fname);
	if (content) {
		try {
			proofFile = JSON.parse(content);
		} catch (jsonError) {
			if (!opt.quiet) {
				console.error(`[fs-utils] Error: Unable to parse proof file ${fname}`, jsonError.message);
				console.error(`[fs-utils] Storing corrupted file content to ${fname}.err`);
			}
			// create a backup copy of the corrupted jprf file, because it might get over-written
			await fsUtils.renameFile(fname, `${fname}.err`);
			await fsUtils.writeFile(`${fname}.err.msg`, jsonError.message);
		} finally {
			return proofFile;
		}
	}
	return proofFile;
}

// /**
//  * Utility function, appends a prooflite script at the end of a given file
//  * @param fname Name of the prooflite file
//  * @param script The prooflite script to be appended
//  */
// export async function appendProoflite (fname: string, script: string): Promise<boolean> {
// 	if (fname && script) {
// 		const content: string = await fsUtils.readFile(fname);
// 		const newContent: string = (content && content.trim()) ? content + `\n\n${script}` : script;
// 		return await fsUtils.writeFile(fname, newContent);
// 	}
// 	return false;
// }
/**
 * Utility function, removes a prooflite script from a given file
 * @param fname Name of the prooflite file
 * @param formulaName name of the prooflite script to be removed
 */
export async function updateProoflite (fname: string, formulaName: string, newProoflite: string): Promise<boolean> {
	if (fname && formulaName) {
		fname = fsUtils.decodeURIComponents(fname);
		const fileExists: boolean = await fsUtils.fileExists(fname);
		if (!fileExists) {
			fsUtils.writeFile(fname, "");
		}
		const content: string = await fsUtils.readFile(fname);

		// group 1 is the header (this group can be null)
		// group 2 is the prooflite script
		const formula: string = formulaName.replace(/\?/g, "\\?");
		const regex: RegExp = new RegExp(`(%-*\\s%\\s*@formula\\s*:\\s*${formula}\\s[\\w\\W\\s]+%-*)?\\s*\\b(${formula}\\s*:\\s*PROOF\\b[\\s\\w\\W]+\\bQED\\b\\s*${formula}\\b\\s*)`, "g");
		if (regex.test(content)) {
			let newContent: string =  newProoflite + "\n\n\n" + content.replace(regex, "").trim();
			// update content
			return await fsUtils.writeFile(fname, newContent.trim());
		} else {
			const newContent: string = newProoflite + "\n\n\n" + content.trim();
			return await fsUtils.writeFile(fname, newContent.trim());
		}
	}
	return false;
}
/**
 * Utility function, checks if a prooflite script is present in a given file
 * @param fname Name of the prooflite file
 * @param formulaName name of the prooflite script
 */
export async function containsProoflite (fname: string, formulaName: string): Promise<boolean> {
	if (fname && formulaName) {
		fname = fsUtils.decodeURIComponents(fname);
		const fileExists: boolean = await fsUtils.fileExists(fname);
		if (fileExists) {
			const content: string = await fsUtils.readFile(fname);
			if (content) {
				// group 1 is the header (this group can be null)
				// group 2 is the prooflite script
				const formula: string = formulaName.replace(/\?/g, "\\?");
				const regex: RegExp = new RegExp(`\\b(${formula}\\s*:\\s*PROOF\\b[\\s\\w\\W]+\\bQED\\b\\s*${formula}\\b\\s*)`, "g");
				return regex.test(content);
			}
		}
	}
	return false;
}
/**
 * Utility function, returns the prooflite script without tags
 * @param fname Name of the prooflite file
 * @param formulaName name of the prooflite script
 */
export async function readProoflite (fname: string, formulaName: string): Promise<string | null> {
	if (fname && formulaName) {
		fname = fsUtils.decodeURIComponents(fname);
		const fileExists: boolean = await fsUtils.fileExists(fname);
		if (fileExists) {
			const content: string = await fsUtils.readFile(fname);
			if (content) {
				// group 1 is the header (this group can be null)
				// group 2 is the prooflite script (with tags)
				// group 3 is the prooflite script (without tags)
				const formula: string = formulaName.replace(/\?/g, "\\?");
				const regex: RegExp = new RegExp(`\\s*\\b(${formula}\\s*:\\s*PROOF\\b([\\s\\w\\W]+)\\bQED\\b\\s*${formula}\\b\\s*)`, "g");
				const match: RegExpMatchArray = regex.exec(content);
				if (match && match.length > 2) {
					return match[2].trim();
				}
			}
		}
	}
	return null;
}
/**
 * Utility function, saves a prooflite script for a given formula in the given file
 * @param fname Name of the prooflite file
 * @param formulaName name of the prooflite
 * @param script The prooflite script to be saved in the file
 */
export async function saveProoflite (fname: string, formulaName: string, script: string): Promise<boolean> {
	if (fname && formulaName && script) {
		fname = fsUtils.decodeURIComponents(fname);
		return await updateProoflite(fname, formulaName, script);
	}
	return false;
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
		});
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

// quit
export function isQuitCommand (cmd: string): boolean {
	cmd = (cmd) ? cmd.trim() : cmd;
	return cmd && (cmd === "quit" 
		|| cmd === "quit;"
		|| cmd === "(quit)"
		|| cmd.toLocaleLowerCase() === "(quit)y"
		|| cmd === "exit"
		|| cmd === "exit;"
		|| cmd === "(exit)"
		|| cmd.toLocaleLowerCase() === "(exit)y")
		|| /\(?\s*quit\s*\)?\s*y?;?/gi.test(cmd)
		|| /\(?\s*exit\s*\)?\s*y?;?/gi.test(cmd)
		;
}

export function isSaveThenQuitCommand (cmd: string): boolean {
	cmd = (cmd) ? cmd.trim() : cmd;
	return cmd && (cmd === "save-then-quit" 
		|| cmd === "save-then-quit;"
		|| cmd === "(save-then-quit)"
		|| cmd === "save-force-exit"
		|| cmd === "save-force-exit;"
		|| cmd === "(save-force-exit)")
		;
}

export function isQuitDontSaveCommand (cmd: string): boolean {
	cmd = (cmd) ? cmd.trim() : cmd;
	return cmd && (cmd === "quit-dont-save" 
		|| cmd === "quit-dont-save;"
		|| cmd === "(quit-dont-save)"
		|| cmd === "exit-dont-save"
		|| cmd === "exit-dont-save;"
		|| cmd === "(exit-dont-save)")
		;
}

export function isEmptyCommand (cmd: string): boolean {
	return !cmd
		|| cmd.trim() === "" 
		|| cmd.trim() === "()"
		;
}

export function isUndoCommand (cmd: string): boolean {
	cmd = (cmd) ? cmd.trim() : cmd;
	return cmd && (cmd === "undo" 
		|| cmd === "undo;"
		|| cmd === "(undo)"
		|| cmd.toLocaleLowerCase() === "(undo)y"
		|| /\(\s*undo(\s*\d+)?\s*\)\s*y?;?/gi.test(cmd))
		;
}

export function unfoldUndoCommand (cmd: string): string[] {
	const match: RegExpMatchArray = /\(\s*undo(\s*\d+)?\s*\)\s*y?;?/gi.exec(cmd);
	let cmds: string[] = [];
	if (match) {
		if (match.length > 1 && match[1]) {
			const n: number = +match[1];
			for (let i = 0; i < n; i++) {
				cmds.push('(undo)');
			}
		} else {
			cmds.push('(undo)');
		}
	}
	return cmds;
}

export function isRedoCommand (cmd: string): boolean {
	return cmd && /\(?\s*\bredo\b/g.test(cmd);
}

export function isUndoUndoCommand (cmd: string): boolean {
	if (cmd) {
		const cm: string = (cmd.startsWith("(")) ? cmd : `(${cmd})`;
		return /\(\s*\bundo\s+undo\b\s*\)/g.test(cm);
	}
	return false;
}

export function isUndoUndoPlusCommand (cmd: string): boolean {
	return cmd && /\(?(\s*\bundo)+/g.test(cmd);
}

export function isUndoStarCommand (cmd: string): boolean {
	return isUndoCommand(cmd) || isUndoUndoCommand(cmd) || isUndoUndoPlusCommand(cmd);
}

export function isPostponeCommand (cmd: string): boolean {
	return cmd && /\(?\s*\bpostpone\b/g.test(cmd);
}

export function isSkipCommand (cmd: string): boolean {
	return cmd && /\(?\s*\bskip\b/g.test(cmd);
}

export function isFailCommand (cmd: string): boolean {
	return cmd && /\(?\s*\bfail\b/g.test(cmd);
}

export function isShowHiddenCommand (cmd: string): boolean {
	return cmd && /\(?\s*show-hidden\b/g.test(cmd);
}

export function isGrindCommand (cmd: string): boolean {
	return cmd && /\(?\s*grind\b/g.test(cmd);
}

export function isProofliteGlassbox (cmd: string): boolean {
	return cmd && cmd.trim().startsWith("(then ");
}

export function isHelpCommand (cmd: string): boolean {
	return cmd && /\(?\s*help\b/g.test(cmd);
}

// group 1 is the command argument
export const helpCommandRegexp: RegExp = /\(?\s*help\s*\"?([^\)]+)/g;

export function isCommentCommand (cmd: string): boolean {
	return cmd && /\(?\s*comment\b/g.test(cmd);
}

export function isQEDCommand (cmd: string): boolean {
	cmd = (cmd) ? cmd.trim() : cmd;
	return cmd && (cmd.trim() === "Q.E.D."
		|| /\(?\s*Q\.E\.D\./g.test(cmd))
		;
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

export function isSameCommand (cmd1: string, cmd2: string): boolean {
	if (cmd1 && cmd2) {
		const c1: string = cmd1.replace(/[\s+\"\(\)]/g, ""); // remove all spaces, round parens, and double quotes
		const c2: string = cmd2.replace(/[\s+\"\(\)]/g, "");
		return c1 === c2;
	}
	return false;
}

export function isPropax (cmd: string): boolean {
	return cmd && cmd === "(propax)";
}

export function splitCommands (cmd: string): string[] {
	let cmds: string[] = [];
	if (cmd && isUndoCommand(cmd)) {
		cmds = unfoldUndoCommand(cmd);
	} else if (cmd && cmd.trim().startsWith("(")) {
		let input: string = cmd.trim().replace(/\)y/gi, ")");
		let par: number = 0;
		let start: number = 0;
		let stop: number = 0;
		let validStart: boolean = false;
		for (let i = 0; i < input.length; i++) {
			if (input[i] === "(") {
				if (par === 0) {
					start = i;
					validStart = true;
				}
				par++;
			} else if (input[i] === ")") {
				par--;
				if (par === 0) {
					stop = i;
				}
			}
			if (par === 0) {
				if (stop > start && validStart) { // sanity check
					const cmd: string = input.substring(start, stop + 1);
					cmds = cmds.concat(cmd);
					validStart = false;
				}
			}
			if (par < 0) {
				// too many closed parentheses -- try to skip
				par = 0;
			}
		}
	}
	return cmds;
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

export function isInvalidCommand (result: { commentary: string | string[] }): boolean {
	const proverErrorMessages: string[] = [
		"Error:",
		"not a valid prover command",
		"Found 'eof' when expecting",
		"bad proof command",
		"Expecting an expression",
		"Not enough arguments for prover command",
		"Could not find formula number",
		"There is garbage at the end"
	];
	const isInvalid = (cmd: string): boolean => {
		if (cmd) {
			for (let i = 0; i < proverErrorMessages.length; i++) {
				if (cmd.includes(proverErrorMessages[i])) {
					return true;
				}
			}
		}
		return false;
	}
	if (result && result.commentary) {
		if (typeof result.commentary === "string") {
			return isInvalid(result.commentary);
		} else if (typeof result.commentary === "object") {
			return result.commentary.length
				&& typeof result.commentary[0] === "string"
				&& result.commentary.filter((comment: string)=> {
					return isInvalid(comment);
				}).length > 0;
		}
	}
	return false;
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
	return desc && (desc.newBranch !== desc.previousBranch || desc.previousBranch !== "" && !desc.newBranch.startsWith(desc.previousBranch));
}

export function pathHasChanged (desc: { newBranch: string, previousBranch: string }): boolean {
	return desc && !desc.newBranch.startsWith(desc.previousBranch);
}

// these icons are shown correctly only on recent os distributions that include the proper font set.
// use https://iconify.design/icon-sets/ for proof explorer, to have a consistent look&feel on all systems.
export const icons: { [name:string]: string } = {
	"checkmark": "✅",
	"bang" : "❗",
	"snowflake" : "❄️",
	"stars": "✨",
	"whitecircle": "⚪"
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
			return icons.stars;
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
	return `%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @theory: ${theoryName}
% @author: ${(process && process.env && process.env.USER) ? process.env.USER : "xxx"}
% @date: ${new Date().toUTCString()}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

${theoryName}: THEORY
  BEGIN 
    
  END ${theoryName}
`;
}

export function makeProofSummary (desc: { total: number, tccsOnly?: boolean, theoryName: string, theorems: { theoryName: string, formulaName: string, status: ProofStatus, ms: number }[]}): string {
	const header: string = desc.tccsOnly ? "TCCs summary" : "Proof summary";
	let ans: string = `${header} for theory ${desc.theoryName}\n`;
	let nProved: number = 0;
	let totTime: number = 0;
	let importChainFlag: boolean = false;
	for (let i = 0; i < desc.theorems.length; i++) {
		if (desc.theorems[i].theoryName !== desc.theoryName && !importChainFlag) {
			importChainFlag = true;
			ans += `\n\t%-- importchain`;
		}
		const formulaName: string = desc.theorems[i].formulaName;
		const status: ProofStatus = desc.theorems[i].status;
		const ms: number = desc.theorems[i].ms;

		const points: number = 64 - formulaName.length;
		const spaces: number = 20 - status.length;

		if (isProved(status)) { nProved++; }
		totTime += ms;

		ans += `\n\t${formulaName}` + ".".repeat(points) + getIcon(status) + " " + status + " ".repeat(spaces) + `(${ms / 1000} s)`;
	}
	ans += `\n\nTheory ${desc.theoryName} totals: ${desc.total} formulas, ${desc.theorems.length} attempted, ${nProved} succeeded (${totTime / 1000} s)`;
	return ans;
}

// utility function, checks if parentheses are balanced
export function balancedPar (cmd: string): boolean {
	const openRegex: RegExp = new RegExp(/\(/g);
	const closeRegex: RegExp = new RegExp(/\)/g);
	let par: number = 0;
	while (openRegex.exec(cmd)) {
		par++;
	}
	while (closeRegex.exec(cmd)) {
		par--;
	}
	return par <= 0;
}

// utility function, ensures open brackets match closed brackets for commands
export function parCheck (cmd: string, opt?: { useColors?: boolean }): { success: boolean, msg: string } {
	opt = opt || {};
	let par: number = 0;
	let quotes: number = 0;
	cmd = cmd.trim();
	for (let i = 0; i < cmd.length; i++) {
		switch (cmd[i]) {
			case `(`: {
				par++;
				break;
			}
			case `)`: {
				par--; 
				if (quotes && quotes % 2 === 0 && par % 2 !== 0) {
					// unbalanced double quotes
					let msg: string = `Error: Unbalanced double quotes at position ${i}.`;
					msg += "\n" + cmd.substring(0, i);
					// msg += (opt.useColors) ? colorText(cmd[i], textColor.red) : cmd[i];
					msg += "\n" + " ".repeat(i) + "^";
					return { success: false, msg };
				}
				break;
			}
		}
	}
	const success: boolean = par <= 0;
	return { success, msg: (success) ? "" : "Error: Unbalanced parentheses." };
}


// utility function, returns a command with balanced parentheses
export function parMatch (cmd: string): string {
	if (cmd && !cmd.trim().startsWith("(")) {
		const openRegex: RegExp = new RegExp(/\(/g);
		const closeRegex: RegExp = new RegExp(/\)/g);
		let par: number = 0;
		while (openRegex.exec(cmd)) {
			par++;
		}
		while (closeRegex.exec(cmd)) {
			par--;
		}
		if (par > 0) {
			// missing closed brackets
			cmd = cmd.trimRight() + ')'.repeat(par);
			// console.log(`Mismatching parentheses automatically fixed: ${par} open round brackets without corresponding closed bracket.`)
		} else if (par < 0) {
			cmd = '('.repeat(-par) + cmd;
			// console.log(`Mismatching parentheses automatically fixed: ${-par} closed brackets did not match any other open bracket.`)
		}
		return cmd.startsWith('(') ? cmd : `(${cmd})`; // add outer parentheses if they are missing
	}
	return cmd;
}

export function decodePvsLibraryPath (pvsLibraryPath: string): string[] {
	const libs: string[] = (pvsLibraryPath) ? pvsLibraryPath.split(":").map((elem: string) => {
		return elem.trim();
	}) : [];
	return libs.filter((elem: string) => {
		return elem !== "";
	}).map((elem: string) => {
		return elem.endsWith("/") ? elem : `${elem}/`
	});
}
export function createPvsLibraryPath (libs: string[]): string {
	if (libs && libs.length) {
		return libs.join(":");
	}
	return "";
}

/**
 * Utility function, appends a proof summary at the end of a given file
 * @param fname Name of the summary file
 * @param summary The summary to be appended
 */
export async function appendSummary (fname: string, summary: string): Promise<boolean> {
	if (fname && summary) {
		const content: string = await fsUtils.readFile(fname);
		const newContent: string = (content && content.trim()) ? content + `\n\n${summary}` : summary;
		return await fsUtils.writeFile(fname, newContent);
	}
	return false;
}
/**
 * Utility function, removes a proof summary from a given file
 * @param fname Name of the summary file
 * @param theoryName name of the theory whose summary should be removed
 */
export async function removeSummary (fname: string, theoryName: string): Promise<boolean> {
	if (fname && theoryName) {
		fname = fsUtils.decodeURIComponents(fname);
		const fileExists: boolean = await fsUtils.fileExists(fname);
		if (fileExists) {
			const content: string = await fsUtils.readFile(fname);
			if (content) {
				const regex: RegExp = new RegExp(`\\bProof summary for theory ${theoryName}\\s[\\s\\w\\W]+\\bTheory ${theoryName}\\s.*`, "g");
				const newContent: string = content.replace(regex, "");
				return await fsUtils.writeFile(fname, newContent);
			}
		}
	}
	return false;
}
/**
 * Utility function, checks if a summary is present in a given file
 * @param fname Name of the summary file
 * @param theoryName name of the theory whose summary should be removed
 */
export async function containsSummary (fname: string, theoryName: string): Promise<boolean> {
	if (fname && theoryName) {
		fname = fsUtils.decodeURIComponents(fname);
		const fileExists: boolean = await fsUtils.fileExists(fname);
		if (fileExists) {
			const content: string = await fsUtils.readFile(fname);
			if (content) {
				const regex: RegExp = new RegExp(`\\bProof summary for theory ${theoryName}\\s[\\s\\w\\W]+\\bTheory ${theoryName}\\s.*`, "g");
				return regex.test(content);
			}
		}
	}
	return false;
}
/**
 * Utility function, saves a summary for a given theory in the given file
 * @param fname Name of the summary file
 * @param theoryName name of the theory whose summary should be saved
 * @param summary The summary to be saved in the file
 */
export async function saveSummary (fname: string, theoryName: string, summary: string): Promise<boolean> {
	if (fname && theoryName && summary) {
		fname = fsUtils.decodeURIComponents(fname);
		const fileExists: boolean = await fsUtils.fileExists(fname);
		if (fileExists) {
			// deleted any previous version of the summary
			await removeSummary(fname, theoryName);
		}
		// append the new summary
		return await appendSummary(fname, summary);
	}
	return false;
}