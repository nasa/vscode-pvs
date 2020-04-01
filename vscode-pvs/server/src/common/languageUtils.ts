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
import * as language from '../common/languageKeywords';
import { StrategyDescriptor, FileList, FormulaDescriptor, SimpleConnection, ContextDescriptor, 
			TheoryDescriptor, ProofNode,  PvsFileDescriptor, PvsVersionDescriptor, ProofDescriptor, ProofFile } from '../common/serverInterface';

export interface Position {
	line: number;
	character: number;
}
export interface Range {
	start: Position,
	end: Position
};
			
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
// group 1 is theoryName, group 2 is comma-separated list of theory parameters
export const theoryRegexp: RegExp = /(\w+)\s*(?:\[([\w\W\s]+)\])?\s*\:\s*THEORY\s*BEGIN\b/gi;
// /(\w+)\s*(?:\%.*\s)*(?:\[([^\]]+)\])?\s*:\s*(?:\%.*\s)*\s*THEORY\b/gi;
export const tccRegexp: RegExp = /(\b[\w\?]+)\s*:\s*OBLIGATION\b/gi;
export const tccStatusRegExp: RegExp = /%\s(proved|subsumed|simplified|unproved|unfinished|unchecked|untried)\b/g;

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
		const text: string = txt.split("\n").slice(0, line + 1).join("\n");
		const regexp: RegExp = theoryRegexp;
		let candidates: string[] = [];
		let match: RegExpMatchArray = null;
		while(match = regexp.exec(text)) {
			if (match.length > 1 && match[1]) {
				// the last match will be the closest to the current line number
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

// match[1] is the formula name
export const theoremRegexp: RegExp = /(\b[\w\?]+)\s*(%.+)?\s*:\s*(%.+)?\s*(?:CHALLENGE|CLAIM|CONJECTURE|COROLLARY|FACT|FORMULA|LAW|LEMMA|PROPOSITION|SUBLEMMA|THEOREM|OBLIGATION|JUDGEMENT)\b/gim;


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
				let theoryName: string = match[1];
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

export interface ProofState {
	action?: string;
	commentary: string[];
	label: string;
	"num-subgoals": number;
	'prev-cmd'?: string[]; // this is actually the last command executed. Why is this an array btw? from the execution I can see just one command in it
	sequent: {
		succedents?: SFormula[], 
		antecedents?: SFormula[],
		"hidden-succedents"?: SFormula[], 
		"hidden-antecedents"?: SFormula[]
	};
}


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

export function formatProofState (proofState: ProofState, opt?: { useColors?: boolean, showAction?: boolean }): string {
	if (proofState) {
		let res: string = "";
		opt = opt || {};
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
		return res;
	} else {
		console.error("[language-utils.format-proof-state] Error: proof state is null :/");
	}
	return null;
}

/**
 * Utility function, returns the list of theorems defined in a given pvs file
 * @param desc Descriptor indicating filename, file extension, context folder, file content, and whether the file in question is the prelude (flag prelude)
 */
export async function listTheorems (desc: { fileName: string, fileExtension: string, contextFolder: string, fileContent: string, prelude?: boolean, cache?: { theories?: TheoryDescriptor[] } }): Promise<FormulaDescriptor[]> {
	if (desc) {
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
							let status: string = (desc.prelude) ? "proved" : "untried";
							if (isTcc) {
								const matchStatus: RegExpMatchArray = tccStatusRegExp.exec(slice);
								if (matchStatus && matchStatus.length > 1 && matchStatus[1]) {
									status = matchStatus[1];
								}
							} else {
								// check if the .jprf file contains the proof status
								const jprf: string = fsUtils.desc2fname({ fileName: desc.fileName, fileExtension: ".jprf", contextFolder: desc.contextFolder });
								const jprf_content: string = await fsUtils.readFile(jprf);
								if (jprf_content) {
									const proofFile: ProofFile = JSON.parse(jprf_content);
									const theoryName: string = boundaries[i].theoryName;
									const proofDescriptors: ProofDescriptor[] = proofFile[`${theoryName}.${formulaName}`];
									if (proofDescriptors && proofDescriptors.length) {
										status = proofDescriptors[0].info.status;
									}
								}
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
	return null;
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
		const regexp: RegExp = theoremRegexp;
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

	const symbols: RegExp = /(\w+\??)/g; // TODO: negative lookahead to remove strings
	const keywords: RegExp = new RegExp(language.PVS_RESERVED_WORDS_REGEXP_SOURCE, "gi");

	let symbolsMap: { [ symbol: string ]: { line: number, character: number } } = {};
	let match: RegExpMatchArray = null;
	const lines: string[] = txt.split("\n");
	const maxIterations: number = 100;
	if (lines && lines.length) {
		for (let i = 0; i < lines.length; i++) {
			const txt: string = lines[i];
			for(let n = 0; n < maxIterations && (match = symbols.exec(txt)); n++) {
				if (!keywords.test(match[0]) && isNaN(+match[0])) {
					symbolsMap[match[0]] = { line: i, character: match.index }; // position is used for debugging purposes here
				}
			}
		}
	}

	return Object.keys(symbolsMap);
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
	if (lines && lines.length > position.line) {
		let txt: string = lines[position.line];
		if (txt) {
			len = txt.length - position.character;
		}
	}
	return {
		start: { line: position.line, character: character },
		end: { line: position.line, character: character + len }
	};
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
export async function getContextDescriptor (contextFolder: string, opt?: { listTheorems?: boolean, includeTccs?: boolean }): Promise<ContextDescriptor> {
	const response: ContextDescriptor = {
		theories: [],
		contextFolder
	};
	const fileList: FileList = await fsUtils.listPvsFiles(contextFolder);
	if (fileList) {
		for (let i in fileList.fileNames) {
			const fname: string = path.join(contextFolder, fileList.fileNames[i]);
			const desc: PvsFileDescriptor = await getFileDescriptor(fname, opt);
			if (desc && desc.theories) {
				response.theories =  response.theories.concat(desc.theories);
			}
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

/**
 * Utility function, transforms a proof tree into a json object
 * @param desc Descriptor specifying proofTree, formulaName, proofName, and parent node (keeps track of the current parent in the proof tree, used in recursive calls)
 */
export function proofScriptToJson (desc: { prf: string, theoryName: string, formulaName: string, version: PvsVersionDescriptor }): ProofDescriptor {
	const buildProofTree = (prf: string, proofName: string): ProofNode => {
		if (prf) {
			prf = prf.trim();
			if (prf.startsWith(`(""`)) {
				const rootNode: ProofNode = {
					name: proofName,
					rules: [],
					type: "root",
					branch: "root"
				};
				// root node
				const match: RegExpMatchArray = /\(\"\"([\w\W\s]+)\s*\)/.exec(prf);
				prf = match[1].trim();
				buildProofTree_aux(prf, proofName, rootNode);
				expandBranchNames(rootNode)
				return rootNode;
			} else {
				console.error("[pvs-proxy] Warning: unrecognised proof structure", prf);
			}
		}
		return null;
	}
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
	const buildProofTree_aux = (prf: string, proofName: string, parent: ProofNode): void => {
		if (parent) {
			while (prf && prf.length) {
				// series of proof branches or a proof command
				const expr: string = getProofCommands(prf);
				if (expr && expr.length) {
					if (expr.startsWith("((")) {
						// series of proof branches
						// remove a matching pair of parentheses and iterate
						const match: RegExpMatchArray = /\(([\w\W\s]+)\s*\)/.exec(prf);
						const subexpr: string = match[1];
						const currentParent: ProofNode = parent.rules[parent.rules.length - 1];
						buildProofTree_aux(subexpr, proofName, currentParent);
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
						parent.rules.push(currentBranch);
						buildProofTree_aux(subexpr, proofName, currentBranch);
					} else {
						// proof command
						parent.rules.push({
							name: expr,
							rules: [],
							type: "proof-command",
							branch: expr
						});
					}
					// update prf
					prf = prf.substr(expr.length).trim();
				} else {
					// ) parentheses comes before (, from parsing a series labelled branches, just ignore them and iterate
					const match: RegExpMatchArray = /\)+([\w\W\s]*)/.exec(prf);
					// update prf
					prf = match[1].trim(); // remove all \n introduced by pvs in the expression
					if (prf && prf.length) {
						buildProofTree_aux(prf, proofName, parent);
					}
				}
			}
		} else {
			console.error("[pvs-proxy] Warning: unable to build proof tree (parent node is null)");
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

	if (desc) {
		const result: ProofDescriptor = {
			info: {
				theory: desc.theoryName,
				formula: desc.formulaName,
				status: "untried", // the prf file does not include the proof status
				prover: pvsVersionToString(desc.version) || "7.x"
			}
		};
		if (desc.prf) {
			const script: string = desc.prf.replace(/\s*\n+\s*/g, " "); // remove all \n introduced by pvs in the expression
			// capture group 1 is proofName
			// capture group 2 is formulaName,
			// capture group 3 is proofTree
			const data: RegExpMatchArray = /;;; Proof\s+([\w\-\.\?]+)\s+for formula\s+([\w\-\.\?]+).*\s*(\(""[\n\w\W]+)/g.exec(script);
			if (data && data.length > 3) {
				const proofName: string = data[1];
				const formulaName: string = data[2];
				// consistency check
				if (formulaName !== `${desc.theoryName}.${desc.formulaName}`) {
					console.warn(`[language-utils] Warning: proof script for ${desc.theoryName}.${desc.formulaName} has unexpected signature ${formulaName}`);
				}
				const prf: string = data[3];
				const proofTree: ProofNode = buildProofTree(prf, proofName);
				result.proof = proofTree;
				// console.dir(result, { depth: null });
			}
		}
		return result;
	}
	return null;
}

export function isQuitCommand (cmd: string): boolean {
	return cmd === "quit" 
		|| cmd === "quit;"
		|| cmd === "(quit)"
		|| cmd === "exit"
		|| cmd === "exit;";
}

export function isEmptyCommand (cmd: string): boolean {
	return cmd === "" 
		|| cmd === "()"
		|| cmd === null;
}

export function isUndoCommand (cmd: string): boolean {
	return cmd && (cmd === "(undo)" 
					|| cmd === "undo" 
					|| cmd.startsWith("(undo ") 
					|| cmd.startsWith("undo "));
}

export function isPostponeCommand (cmd: string): boolean {
	return cmd && (cmd === "(postpone)" 
					|| cmd === "postpone");
}

export function isSaveCommand (cmd: string): boolean {
	return cmd && (cmd === "(save)" 
					|| cmd === "save");
}

export function isSameCommand (cmd1: string, cmd2: string): boolean {
	return cmd1 === cmd2 || cmd1 === `(${cmd2})` || `(${cmd1})` === cmd2;
}

export function isInvalidCommand (result: { commentary: string[] }): boolean {
	return result
		&& typeof result.commentary === "object"
		&& result.commentary.length
		&& typeof result.commentary[0] === "string"
		&& result.commentary[0].endsWith("not a valid prover command");
}

export function noChange (result: { commentary: string[] }): boolean {
	return result
		&& typeof result.commentary === "object"
		&& result.commentary.length
		&& result.commentary.filter((comment: string)=> {
			return comment.startsWith("No change on:");
		}).length > 0;
}

export function isQED (result: { result: string }): boolean {
	return result && result.result === "Q.E.D."; 
}



/**
 * Lists all theories in a given context foder
 * @param contextFolder Current context folder
 * @param connection Connection to the client, useful for sending status updates about what the function is doing (the function may take some time to complete for large files)
 */
// export async function listTheories (contextFolder: string, connection?: SimpleConnection): Promise<ContextDescriptor> {
// 	let response: ContextDescriptor = {
// 		theories: [],
// 		contextFolder
// 	};
// 	// send the empty response to trigger a refresh of the view
// 	sendTheories(response, connection);
// 	const fileList: FileList = await fsUtils.listPvsFiles(contextFolder);
// 	for (let i in fileList.fileNames) {
// 		let uri: string = path.join(contextFolder, fileList.fileNames[i]);
// 		let theories: TheoryDescriptor[] = await listTheoriesInFile(uri);
// 		response.theories.concat(theories);
// 	}
// 	sendTheories(response, connection);
// 	return Promise.resolve(response);
// }

// export function sendTheories (desc: ContextDescriptor, connection?: SimpleConnection): void {
// 	if (connection && connection.sendRequest) {
// 		// send the response incrementally, as soon as another bit of information is available
// 		connection.sendRequest("server.response.get-context-descriptor", desc);
// 	}
// }



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
	{ name: "all-typepreds", description:"make type constraints of subexpressions explicit" },
	{ name: "apply-extensionality", description:"use extensionality to prove equality" },
	{ name: "apply-lemma", description:"automatically determines the required substitutions in a lemma" },
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
	{ name: "lemma", description:"introduces an instance of a lemma" },
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