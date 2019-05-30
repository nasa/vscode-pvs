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

import { TccDescriptor } from './serverInterface';
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
