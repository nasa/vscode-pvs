/**
 * @module PvsDefinitionProvider
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

import { PvsDefinition, ProofStatus, TheoryDescriptor, PvsTheory } from '../common/serverInterface';
import * as language from "../common/languageKeywords";
import { Connection, Position, Range, CancellationToken, TextDocuments, TextDocumentPositionParams, Definition, TextDocument, Location } from 'vscode-languageserver';
import { getWordRange } from '../common/languageUtils';
import * as fsUtils from '../common/fsUtils';
import { PvsProxy } from '../pvsProxy';
import { FindDeclarationResult, Place, PvsResponse } from '../common/pvs-gui';
import * as path from 'path';
import * as utils from '../common/languageUtils';

export class PvsDefinitionProvider {
	connection: Connection;
	protected pvsProxy: PvsProxy;
	protected documents: TextDocuments;
	// private isenseSymbols: { [key: string]: PvsDeclarationDescriptor } = {};

	constructor(pvsProxy: PvsProxy, documents: TextDocuments) {
		this.pvsProxy = pvsProxy;
		this.documents = documents;
	}

	/**
	 * Utility function, prints a warning message in the console
	 * @param msg
	 */
	protected printWarning (msg: string) {
		if (this.connection) {
			this.connection.console.warn(`[pvs-definition-provider] ${msg}`);
		}
	}
	/**
	 * Utility function, prints a timestamped message in the console
	 * @param msg
	 */
	protected printInfo (msg: string) {
		if (this.connection) {
			this.connection.console.info(`[pvs-definition-provider] ${msg}`);
		}
	}

	isProtectedFolder (contextFolder: string): boolean {
		if (this.pvsProxy) {
			return this.pvsProxy.isProtectedFolder(contextFolder);
		}
		return false;
	}

	// async getTermAt(document: TextDocument, position: Position): Promise<PvsDefinition[]> {
	// 	let ans: PvsFindDeclarationResponseType = await this.pvsProcess.getTermAt(document.uri.replace("file://", ""), { line: position.line, character: position.character });
	// 	return null;
	// } 

	/**
	 * Utility function, finds a symbol definition for a given position within a file
	 * @param fname The document that contains the symbol
	 * @param symbolName The symbol whose definition needs to be found
	 * @param position Position where the symbol is used, helps to narrow down the list of potential definitions in the case of symbol overloading
	 */
	async findSymbolDefinition (fname: string, symbolName: string, position: Position): Promise<PvsDefinition[]> {
		if (fname) {
			if (fname.endsWith(".tccs")) {
				const theoryName: string = fsUtils.getFileName(fname);
				const theory: PvsTheory = {
					...fsUtils.fname2desc(fname),
					theoryName
				};
				return await this.findSymbolDefinitionInTheory(theory, symbolName, position);
			}
			// .pvs file
			const content: string = await fsUtils.readFile(fname);
			const theoryName: string = fsUtils.findTheoryName(content, position?.line);
			if (theoryName) {
				const theory: PvsTheory = {
					...fsUtils.fname2desc(fname),
					theoryName
				};
				return await this.findSymbolDefinitionInTheory(theory, symbolName, position);
			}			
		}
		return null;
	}

	/**
	 * Utility function, finds a symbol definition in a given theory
	 * @param fname The document that contains the symbol
	 * @param symbolName The symbol whose definition needs to be found
	 * @param position Position where the symbol is used, helps to narrow down the list of potential definitions in the case of symbol overloading
	 */
	async findSymbolDefinitionInTheory (theory: PvsTheory, symbolName: string, position?: Position): Promise<PvsDefinition[]> {
		if (!theory || !symbolName) {
			return null;
		}
		let response: PvsDefinition = {
			file: fsUtils.desc2fname(theory),
			theory: theory.theoryName,
			line: position?.line || 0,
			character: position?.character || 0,
			symbolName: symbolName,
			symbolTheory: null,
			symbolDeclaration: null,
			symbolDeclarationRange: null,
			symbolDeclarationFile: null,
			symbolDoc: null,
			comment: null,
			error: null
		};
		symbolName = (symbolName.toUpperCase() === "TRUE" || symbolName.toUpperCase() === "FALSE") ? symbolName.toUpperCase() : symbolName;
		const isNumber: boolean = new RegExp(language.PVS_NUMBER_REGEXP_SOURCE).test(symbolName);
		if (isNumber) {
			this.printInfo(`Number ${symbolName}`);
			response.comment = `Number ${symbolName}`;
			return [ response ];
		}
		const isString: boolean = new RegExp(language.PVS_STRING_REGEXP_SOURCE).test(symbolName);
		if (isString) {
			this.printInfo(`String ${symbolName}`);
			response.comment = `String ${symbolName}`;
			return [ response ];
		}
		const isKeyword: boolean = new RegExp(language.PVS_RESERVED_WORDS_REGEXP_SOURCE, "gi").test(symbolName);
		if (isKeyword) {
			this.printInfo(`Keyword ${symbolName}`);
			response.comment = symbolName === 'o' ? `Keyword ${symbolName}` : `Keyword ${symbolName.toUpperCase()}`;
			return [ response ];
		}

		// else
		const ans: PvsResponse = await this.pvsProxy.findDeclaration(symbolName); //await this.pvsProcess.findDeclaration(symbolName);

		// find-declaration may return more than one result -- e.g., because the file is not typechecked
		// we can narrow down the results by traversing the importchain
		// part of this extra logic can be removed when Sam completes the implementation of find-object
		const candidates: PvsDefinition[] = [];
		if (ans && ans.result) {
			const declarations: FindDeclarationResult = ans.result;
			if (declarations && typeof declarations === "object" && declarations.length > 0) {
				for (let i = 0; i < declarations.length; i++) {
					const info: {
						declname?: string;
						type?: string;
						theoryid?: string;
						filename?: string;
						place?: Place;
						"decl-ppstring"?: string;
					} = declarations[i];
					if (info) {
						let fname: string = info.filename;
						// FIXME: pvs-server does not include path when the file is in the current context
						if (fname && fname.indexOf("/") < 0) {
							// add contextFolder to fname, check if this is the pvslog folder (if so, remove /pvsbin)
							let contextFolder: string = theory.contextFolder;
							if (contextFolder.endsWith("/pvsbin") || contextFolder.endsWith("pvsbin/")) {
								contextFolder = contextFolder.split("/").slice(0, -1).join("/");
							}
							fname = path.join(contextFolder, fname);
						}
						let comment: string = "";
						if (fname && info.type === "theorem" || info.type === "lemma") {
							// check if the theorem has been proved, and if so add the proof status to the tooltip
							const proofStatus: ProofStatus = await fsUtils.getProofStatus({
								contextFolder: fsUtils.getContextFolder(fname),
								fileName: fsUtils.getFileName(fname),
								fileExtension: fsUtils.getFileExtension(fname),
								theoryName: info.theoryid,
								formulaName: info.declname
							});
							comment += `Formula ${info.declname} (${utils.getIcon(proofStatus)}${proofStatus})`;
							// try to fetch proof date
							const date: string = await fsUtils.getProofDate({
								contextFolder: fsUtils.getContextFolder(fname),
								fileName: fsUtils.getFileName(fname),
								fileExtension: fsUtils.getFileExtension(fname),
								theoryName: info.theoryid,
								formulaName: info.declname
							});
							if (date) {
								comment += ` (${new Date(date).toLocaleString()})`;
							}
						}
						const def: PvsDefinition = {
							theory: theory.theoryName,
							line: (position) ? position.line : null,
							character: (position) ? position.character : null,
							file: fsUtils.desc2fname(theory),
							symbolName: symbolName,
							symbolTheory: (info) ? info.theoryid : null,
							symbolDeclaration: (info) ? info["decl-ppstring"] : null,
							symbolDeclarationRange: (info && info.place && info.place.length > 1) ? { 
								start: { line: info.place[0], character: info.place[1] }, 
								end: (info.place.length === 4) ? { line: info.place[2], character: info.place[3] } : undefined
							} : null,
							symbolDeclarationFile: fname,
							symbolDoc: null,
							comment,
							error: null
						};
						candidates.push(def);
					}
				}
			}
		} else {
			const importedTheory: PvsResponse = await this.pvsProxy.findTheory(symbolName);
			if (importedTheory && importedTheory.result) {
				const fname: string = importedTheory.result;
				let desc: TheoryDescriptor[] = await fsUtils.listTheoriesInFile(fname);
				desc = desc?.filter(tdesc => {
					return tdesc.theoryName = symbolName;
				});
				const start: Position = (desc && desc.length) ? desc[0].position : { line: 0, character: 0 };
				let decl: string = "";
				if (desc && desc.length) {
					decl = await fsUtils.readFile(fname);
					decl = decl.split("\n").slice(start.line - 1).join("\n");
				}
				const def: PvsDefinition = {
					theory: symbolName,
					line: null,
					character: null,
					file: fname,
					symbolName: symbolName,
					symbolTheory: symbolName,
					symbolDeclaration: decl,
					symbolDeclarationRange: { 
						start, 
						end: start
					},
					symbolDeclarationFile: fname,
					symbolDoc: null,
					comment: `Imported theory ${symbolName}`,
					error: null
				};
				candidates.push(def);
			}
		}

		if (candidates.length === 0) {
			this.printWarning("Warning: Could not find declaration :/");
			return null;
		}
		if (candidates.length === 1) {
			response = candidates[0];
			const isBuiltinType: boolean = new RegExp(language.PVS_BUILTIN_TYPE_REGEXP_SOURCE, "g").test(symbolName);
			const isTrueFalse: boolean = new RegExp(language.PVS_TRUE_FALSE_REGEXP_SOURCE, "gi").test(symbolName);
			if (isBuiltinType) {
				response.comment = `Builtin type ${symbolName}`;
			} else if (isTrueFalse) {
				response.comment = `Builtin constant ${symbolName}`;
			} else if (!response.comment) {
				response.comment = `User-defined symbol ${symbolName}`;
			}
			return [ response ];
		} else {
			// more than one candidate -- return the definition from the current file as best guess
			const currentFile: PvsDefinition[] = candidates.filter(elem => {
				return fsUtils.getContextFolder(elem.symbolDeclarationFile) === theory.contextFolder
							&& fsUtils.getFileName(elem.symbolDeclarationFile) === theory.fileName;
			}) || [];
			if (currentFile && currentFile.length) {
				return currentFile.concat(candidates.filter(elem => {
					return !(fsUtils.getContextFolder(elem.symbolDeclarationFile) === theory.contextFolder
								&& fsUtils.getFileName(elem.symbolDeclarationFile) === theory.fileName);
				}));
			}
		}
		return candidates;
	}

	/**
	 * Standard API of the language server for requesting a symbol definition
	 * @param document Current document, including document URI, text content, and cursor position
	 * @param token Cancellation token (optional)
	 */
	async getDefinition(document: { uri: string, txt: string, position: Position }, token?: CancellationToken): Promise<{ symbolName: string, definitions: PvsDefinition[] }> {
		if (document && document.uri && document.txt && document.position) {
			const symbolRange: Range = getWordRange(document.txt, document.position);
			// console.log(`Provide definition`, symbolRange);
			// sanity check
			if (symbolRange?.end && symbolRange?.start && symbolRange.end.character > document.position.character) {
				const symbolName: string = fsUtils.getText(document.txt, symbolRange);
				if (symbolName) {
					const line: number = symbolRange.start.line + 1; // vscode starts lines from 0, we want to start from 1 (as in pvs)
					const character: number = symbolRange.start.character;
					// const fileName: string = document.uri;
					// const theoryName: string = this.findTheory(document, line);
					// console.log("(line, character) ", line, character);
					// await this.getTermAt(document, { line, character });

					const definitions: PvsDefinition[] = await this.findSymbolDefinition(document.uri, symbolName, { line: line, character: character });
					return { symbolName, definitions };
				}
			}
		}
		return null;
	}

	async provideDefinition (desc: { uri: string, position: Position, txt: string }): Promise<Definition> {
		if (desc && desc.uri && fsUtils.isPvsFile(desc.uri)) {
			if (desc.txt) {
				const position: Position = desc.position;
				const info: { symbolName: string, definitions: PvsDefinition[] } = await this.getDefinition({ txt: desc.txt, uri: desc.uri, position });
				if (info) {
					const pvsDefinitions: PvsDefinition[] = info.definitions;
					if (pvsDefinitions) {
						const ans: Location[] = [];
						for (let i: number = 0; i < pvsDefinitions.length; i++) {
							const def: PvsDefinition = pvsDefinitions[i];
							if (def?.symbolDeclarationRange) {
								const uri: string = def.symbolDeclarationFile;
								const range: Range = {
									start: {
										line: def.symbolDeclarationRange.start.line - 1,
										character: def.symbolDeclarationRange.start.character
									},
									end: {
										line: def.symbolDeclarationRange.end.line - 1,
										character: def.symbolDeclarationRange.end.character
									}
								}
								const location: Location = {
									uri: "file://" + uri,
									range: range
								}
								ans.push(location);
							}
						}
						return ans.reverse();
					}
				}
			}
		}
		return null;
	}
}
