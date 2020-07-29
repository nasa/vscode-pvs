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
			TheoryDescriptor, ProofNode,  PvsFileDescriptor, PvsVersionDescriptor, ProofDescriptor, ProofFile, ProofStatus, Position, Range, ProofTree } from '../common/serverInterface';

			
// records literals are in the form id: ID = (# ac1: Ac1, ac2: Ac2 #)
// record types are in the form Rec: TYPE = [# x: nat, y: real #]
export const RECORD: { [key: string]: RegExp } = {
	declaration: /(\w+)\s*:\s*(\w+)(\s*=\s*[\[\(]#.+[\]\)])?/g,
	isLiteral: /\(#.+#\)/g,
	isType: /\[#.+#\]/g,
	accessors: /[\(\[]#(.+)#[\]\)]/g, // comma-separated list of accessors
	typeName: /\w+\s*:\s*(\w+)/g
}

// export function printCommand (cmd: any[]): string {
// 	const printArgs = (cmd: any[] | string | number): string => {
// 		if (cmd) {
// 			if (typeof cmd === "string") {
// 				return `"${cmd}"`;
// 			}
// 			if (typeof cmd === "number") {
// 				return `${cmd}`;
// 			}
// 			if (cmd.length) {
// 				let ans: string = "";
// 				for (let i = 0; i < cmd["length"]; i++) {
// 					ans += printArgs(cmd[i]);
// 				}
// 				return `(${ans.trim()})`
// 			}
// 			return null;
// 		}
// 	}
// 	if (cmd && cmd.length) {
// 		const cmdName: string = cmd[0];
// 		const args = cmd.slice(1);
// 		const ans: string = `${cmdName} ${printArgs(args)}`;
// 		return `(${ans.trim()})`;
// 	}
// 	return null;
// }

export const commentRegexp: RegExp = /%.*/g;
// group 1 is theoryName, group 2 is comma-separated list of theory parameters
export const theoryRegexp: RegExp = /(\w+)\s*(?:\[([\w\W\s]+)\])?\s*\:\s*THEORY\s*BEGIN\b/gi;
export const datatypeRegexp: RegExp = /(\w+)\s*(?:\[([\w\W\s]+)\])?\s*\:\s*DATATYPE\s*BEGIN\b/gi;
export const declarationRegexp: RegExp = /(\w+)\s*(?:\[([\w\W\s]+)\])?\s*\:/gi;
// group 1 is the formula name
export const formulaRegexp: RegExp = /(\b[\w\?]+)\s*(%.+)?\s*:\s*(%.+)?\s*(?:CHALLENGE|CLAIM|CONJECTURE|COROLLARY|FACT|FORMULA|LAW|LEMMA|PROPOSITION|SUBLEMMA|THEOREM|OBLIGATION|JUDGEMENT|AXIOM)\b/gim;
// same as formulaRegExp, but does not include JUDGEMENT and AXIOM
export const theoremRegexp: RegExp = /(\b[\w\?]+)\s*(%.+)?\s*:\s*(%.+)?\s*(?:CHALLENGE|CLAIM|CONJECTURE|COROLLARY|FACT|FORMULA|LAW|LEMMA|PROPOSITION|SUBLEMMA|THEOREM|OBLIGATION)\b/gim;
// /(\w+)\s*(?:\%.*\s)*(?:\[([^\]]+)\])?\s*:\s*(?:\%.*\s)*\s*THEORY\b/gi;
export const tccRegexp: RegExp = /(\b[\w\?]+)\s*:\s*OBLIGATION\b/gi;
export const tccStatusRegExp: RegExp = /%\s(proved|subsumed|simplified|unproved|unfinished|unchecked|untried)\b/g;

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
			const min: number = matchFirstTheory[0].split("\n").length;
			line = (line < min) ? min : line;
			candidates.push(matchFirstTheory[1]);
		}

		const text: string = txt.split("\n").slice(0, line + 1).join("\n");
		let match: RegExpMatchArray = null;
		while(match = regexp.exec(text)) {
			if (match.length > 1 && match[1]) {
				// the first match will be the closest to the current line number
				candidates = [ match[1] ].concat(candidates);
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
				ans.push(match[1]);
			}
		}
	}
	return ans;
};

/**
 * Utility function, returns the list of theories defined in a given pvs file
 * @param fname Path to a pvs file
 */
export async function listTheoriesInFile (fname: string): Promise<TheoryDescriptor[]> {
	// console.log(`listing theories in file ${fname}`);
	if (fname) {
		const fileName: string = fsUtils.getFileName(fname);
		const fileExtension: string = fsUtils.getFileExtension(fname);
		const contextFolder: string = fsUtils.getContextFolder(fname);
		const fileContent: string = await fsUtils.readFile(fname);
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
		const fileContent: string = desc.fileContent.replace(commentRegexp, "");
		const start: number = Date.now();
		const regexp: RegExp = theoryRegexp;
		let match: RegExpMatchArray = null;
		while (match = regexp.exec(fileContent)) {
			if (match.length > 1 && match[1]) {
				const theoryName: string = match[1];
				const clip: string = fileContent.slice(0, match.index);
				const lines: string[] = clip.split("\n"); 
				const line: number = lines.length;
				const character: number = 0; //match.index - lines.slice(-1).join("\n").length;
				ans.push({
					theoryName,
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
		const stats: number = Date.now() - start;
		// console.log(`[languageUtils] listTheories(${desc.fileName}) completed in ${stats}ms`);
	}
	return ans;
}

/**
 * Utility function, returns the list of theories defined in a given pvs file
 * @param fname Path to a pvs file
 */
export async function listTheoremsInFile (fname: string): Promise<FormulaDescriptor[]> {
	if (fname) {
		const fileName: string = fsUtils.getFileName(fname);
		const fileExtension: string = fsUtils.getFileExtension(fname);
		const contextFolder: string = fsUtils.getContextFolder(fname);
		const fileContent: string = await fsUtils.readFile(fname);
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
	label: string,
	commentary: string[],
	action?: string // this field is useless
	"num-subgoals"?: number,
	"prev-cmd"?: Object; // object representing the last command executed
	"last-cmd"?: string,
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
		const proofFile: ProofFile = await fsUtils.readProofFile(jprf_file);
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
 * Utility function, returns the prooflite script for the theorem indicated in the fuction arguments
 * @param desc 
 */
export async function getProofLiteScript (desc: { 
	fileName: string, 
	fileExtension: string, 
	contextFolder: string, 
	theoryName: string, 
	formulaName: string
}): Promise<string> {
	if (desc) {
		const makeHeader = (status: ProofStatus): string => { 
			return `%-------------------------------------------
% @formula: ${desc.formulaName} 
% @theory: ${desc.theoryName}
% @status: ${getIcon(status)}${status}
%-------------------------------------------\n`; 
		}
		let proofScript: string = makeHeader("untried");
		// check if the .jprf file contains the proof status
		const jprf_file: string = fsUtils.desc2fname({
			fileName: desc.fileName, 
			fileExtension: ".jprf", 
			contextFolder: desc.contextFolder
		});
		const proofFile: ProofFile = await fsUtils.readProofFile(jprf_file);
		if (proofFile) {
			const proofDescriptors: ProofDescriptor[] = proofFile[`${desc.theoryName}.${desc.formulaName}`];
			if (proofDescriptors && proofDescriptors.length && proofDescriptors[0] && proofDescriptors[0].info) {
				proofScript = makeHeader(proofDescriptors[0].info.status);

				const proofLite: string[] = proofTree2ProofLite(proofDescriptors[0].proofTree);
				if (proofLite && proofLite.length) {
					proofScript += proofLite.join("\n");
				}
			}
		}
		return proofScript;
	}
	return null;
}

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
	return new RegExp(`(?:(?:%--*.*)\\s*(?:%\\s*@formula\s*:\s*(${desc.formulaName}))?\s*(?:%\s*@theory\s*:\s*(${desc.theoryName}))?\s*(?:%\s*@status\s*:\s*(.*))?\s*(?:%--*.*))?\s*(${desc.formulaName}\s*:\s*PROOF[\w\W\s]*QED\s*${desc.formulaName}`, "g");
}
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
		const symbols: RegExp = /(\w+\??)/g;
		let ans = testTokenizer(strings, txt)
					|| testTokenizer(numbers, txt)
					|| testTokenizer(keywords, txt)
					|| testTokenizer(symbols, txt);
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
+---- 
| PVSio Evaluator
|
| Enter a PVS expression at the prompt, or 'help' for help, or 'exit' to exit the evaluator.
| You can use TAB to complete commands at the PVSio prompt.
|
| Note: evaluation of expressions which depend on unproven TCCs may be unsound,
| and result in the evaluator becoming unresponsive or crashing into Lisp. 
| If that happens, please close the evaluator session and start a new one.
|
+----

`
;

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
	return `\x1b[38;5;${colorCode}m${text}\x1b[0m`;
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

export function proofTree2ProofLite (proofTree: ProofNode, opt?: { barPecent?: boolean }): string[] | null {
	opt = opt || {};
	const proofTreeToProofLite_aux = (nodes: ProofNode[], currentBranch?: string, indent?: number): string => {
		indent = indent || 0;
		let res: string = "";
		if (nodes && nodes.length) {
			let closeRight: boolean = false;
			for (let i = 0; i < nodes.length; i++) {
				const node: ProofNode = nodes[i];
				switch (node.type) {
					case "root": {
						res += `${" ".repeat(indent)}` + proofTreeToProofLite_aux(node.rules, "", indent);
						break;
					}
					case "proof-command": {
						if (node.branch === currentBranch && i === 0 && (!node.rules || node.rules.length === 0)) {
							res += `(then `; // the parenthesis will be closed at the end of the loop
							closeRight = true;
						}
						const cmd: string = node.name.startsWith("(") ? node.name : `(${node.name})`;
						if (node.rules && node.rules.length) {
							indent++;
							if (i > 0) {
								indent++;
								res += `\n${" ".repeat(indent)}`;
							}
							res += `(spread `;
							indent++;
							res += cmd + `\n${" ".repeat(indent)}(` + proofTreeToProofLite_aux(node.rules, node.branch, indent);
							res += `))`; // we need to close the extra parenthesis opened by spread
						} else {
							res += cmd + proofTreeToProofLite_aux(node.rules, node.branch, indent);
						}
						break;
					}
					case "proof-branch":
						if (i > 0) {
							res += `\n${" ".repeat(indent + 1)}`;
						}
					default: {
						if (node.rules && node.rules.length) {
							res += proofTreeToProofLite_aux(node.rules, node.branch, indent);
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
	if (proofTree) {
		let script: string = proofTreeToProofLite_aux([ proofTree ]);
		script = script || "(postpone)";
		const res = `${proofTree.name} : PROOF
${script}
QED ${proofTree.name}`;
		return (opt.barPecent) ? res.split("\n").map(line => { return "%|- " + line; })
			: res.split("\n");
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
		desc.prf = desc.prf.trim();
		if (desc.prf.startsWith(`(""`)) {
			// root node
			const rootNode: ProofNode = {
				name: desc.proofName,
				rules: [],
				type: "root",
				branch: "root"
			};
			const match: RegExpMatchArray = /\(\"\"([\w\W\s]+)\s*\)/.exec(desc.prf);
			desc.prf = match[1].trim();
			buildProofTree_aux({ prf: desc.prf, proofName: desc.proofName, parent: rootNode });
			expandBranchNames(rootNode)
			return rootNode;
		} else {
			console.error("[pvs-proxy] Warning: unrecognised proof structure", desc.prf);
		}
	}
	return null;
}


export function isEmptyPrf(prf: string): boolean {
	return !prf || prf === `("" (postpone))`;
}

const grind: string = `("" (grind))`;


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
	autorun?: boolean
}): ProofDescriptor {
	if (desc) {
		const result: ProofDescriptor = {
			info: {
				theory: desc.theoryName,
				formula: desc.formulaName,
				status: "untried", // the prf file does not include the proof status
				prover: pvsVersionToString(desc.version) || "PVS 7.x",
				shasum: desc.shasum
			}
		};
		if (desc.prf) {
			const script: string = desc.prf.replace(/\s*\n+\s*/g, " "); // remove all \n introduced by pvs in the expression
			// capture group 1 is proofName
			// capture group 2 is formulaName,
			// capture group 3 is proofTree
			const data: RegExpMatchArray = /;;; Proof\s+([\w\-\.\?]+)\s+for formula\s+([\w\-\.\?]+).*\s*(\(""[\n\w\W]+)/g.exec(script);
			if (data && data.length > 3) {
				const proofName: string =  data[2]; //data[1];
				const formulaName: string = data[2];
				// consistency check
				if (formulaName !== `${desc.theoryName}.${desc.formulaName}`) {
					console.warn(`[language-utils] Warning: proof script for ${desc.theoryName}.${desc.formulaName} has unexpected signature ${formulaName}`);
				}
				const prf: string = (desc.autorun && isEmptyPrf(data[3]))? grind : data[3];
				const proof: ProofNode = prf2ProofTree({ prf, proofName });
				result.proofTree = proof;
				// console.dir(result, { depth: null });
			}
		} 
		return result;
	}
	return null;
}

// quit = quit-and-save
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
		|| /\(\s*undo\s*\)\s*y?;?/gi.test(cmd))
		;
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

export function isShowHiddenCommand (cmd: string): boolean {
	return cmd && /\(?\s*show-hidden\b/g.test(cmd);
}

export function isGrindCommand (cmd: string): boolean {
	return cmd && /\(?\s*grind\b/g.test(cmd);
}

export function isQEDCommand (cmd: string): boolean {
	cmd = (cmd) ? cmd.trim() : cmd;
	return cmd && (cmd.trim() === "Q.E.D."
		|| /\(?\s*Q\.E\.D\./g.test(cmd))
		;
}

export function isSaveCommand (cmd: string): boolean {
	cmd = (cmd) ? cmd.trim() : cmd;
	return cmd && (cmd === "save" 
		|| cmd === "save;"
		|| cmd === "(save)"
		|| /\(\s*save\s*\)/g.test(cmd))
		;
}

export function isMetaProofCommand (cmd: string): boolean {
	return isPostponeCommand(cmd) || isUndoStarCommand(cmd) || isShowHiddenCommand(cmd);
}

export function isSameCommand (cmd1: string, cmd2: string): boolean {
	if (cmd1 && cmd2) {
		const c1: string = cmd1.replace(/"/g, "").trim();
		const c2: string = cmd2.replace(/"/g, "").trim();
		return c1 === c2 
			|| (!c2.startsWith("(") && c1 === `(${c2})`) 
			|| (!c1.startsWith("(") && `(${c1})` === c2);
	}
	return false;
}

export function splitCommands (cmd: string): string[] {
	let cmds: string[] = [];
	if (cmd && cmd.trim().startsWith("(")) {
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

export function isInvalidCommand (result: { commentary: string[] }): boolean {
	return result && result.commentary
		&& typeof result.commentary === "object"
		&& result.commentary.length
		&& typeof result.commentary[0] === "string"
		&& result.commentary[0].endsWith("not a valid prover command");
}

export function noChange (result: { commentary: string[] }): boolean {
	return result && result.commentary
		&& typeof result.commentary === "object"
		&& result.commentary.length
		&& result.commentary.filter((comment: string)=> {
			return comment.startsWith("No change on:");
		}).length > 0;
}

export function QED (result: { commentary: string[] }): boolean {
	return result && result.commentary
		&& typeof result.commentary === "object"
		&& result.commentary.length
		&& result.commentary.filter((comment: string)=> {
			return comment.trim() === "Q.E.D.";
		}).length > 0; 
}

export function branchComplete (result: { commentary: string[] }, previousBranch: string): boolean {
	return result && result.commentary
		&& result.commentary.length 
		&& result.commentary.filter((comment: string) => {
			if (typeof previousBranch === "string") {
				return comment.startsWith("This completes the proof") 
					&& (comment.endsWith(previousBranch) || comment.endsWith(previousBranch + "."));
			}
			return comment.startsWith("This completes the proof");
		}).length > 0;
}

export function siblingBranchComplete (result: { commentary: string[] }, newBranch: string): boolean {
	return result && result.commentary
		&& result.commentary.length 
		&& result.commentary.filter((comment: string) => {
			if (typeof newBranch === "string" && comment.startsWith("This completes the proof")) {
				const parentOfNewBranch: string = newBranch.substring(0, newBranch.lastIndexOf("."));
				const closedBranch: string = comment.substring(comment.indexOf(".") + 1, comment.lastIndexOf("."));
				const parentOfClosedBranch: string = closedBranch.substring(0, closedBranch.lastIndexOf("."));
				return parentOfClosedBranch === parentOfNewBranch;
			}
			return comment.startsWith("This completes the proof");
		}).length > 0;
}

export function branchHasChanged (desc: { newBranch: string, previousBranch: string }): boolean {
	return desc && (desc.newBranch !== desc.previousBranch || desc.previousBranch !== "" && !desc.newBranch.startsWith(desc.previousBranch));
}

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
% @author: xxx
% @date: ${new Date().toUTCString()}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

${theoryName}: THEORY
  BEGIN 
    
  END ${theoryName}
`;
}

export function makeProofSummary (desc: { total: number, tccsOnly?: boolean, theoryName: string, theorems: { formulaName: string, status: ProofStatus, ms: number }[]}): string {
	const header: string = desc.tccsOnly ? "TCCs summary" : "Proof summary";
	let ans: string = `${header} for theory ${desc.theoryName}\n`;
	let nProved: number = 0;
	let totTime: number = 0;
	for (let i = 0; i < desc.theorems.length; i++) {
		const formulaName: string = desc.theorems[i].formulaName;
		const status: ProofStatus = desc.theorems[i].status;
		const ms: number = desc.theorems[i].ms;

		const points: number = 64 - formulaName.length;
		const spaces: number = 20 - status.length;

		if (isProved(status)) { nProved++; }
		totTime += ms;

		ans += `\n\t${formulaName}` + ".".repeat(points) + getIcon(status) + status + " ".repeat(spaces) + `(${ms / 1000} s)`;
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
	const startsWithPar: boolean = cmd.startsWith("(");
	for (let i = 0; i < cmd.length; i++) {
		switch (cmd[i]) {
			case `(`: {
				par++;
				break;
			}
			case `)`: {
				par--; 
				if (quotes && quotes % 2 === 0 && par % 2 !== 0) {
					// unbalanced parentheses
					let msg: string = `Error: Unbalanced double quotes at position ${i}.`;
					msg += "\n" + cmd.substring(0, i);
					// msg += (opt.useColors) ? colorText(cmd[i], textColor.red) : cmd[i];
					msg += "\n" + " ".repeat(i) + "^";
					return { success: false, msg };
				}
				break;
			}
			case `"`: {
				if ((startsWithPar && par % 2 !== 0) || (!startsWithPar && par === 0)) {
					quotes++;
				} else {
					// unbalanced double quotes
					let msg: string = `Error: Unbalanced parentheses at position ${i}.`;
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


// // utility function, ensures open brackets match closed brackets for commands
// export function quotesMatch (cmd: string): boolean {
// 	const quotesRegex: RegExp = new RegExp(/\"/g);
// 	let nQuotes: number = 0;
// 	while (quotesRegex.exec(cmd)) {
// 		nQuotes++;
// 	}
// 	return nQuotes % 2 === 0;
// }

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
