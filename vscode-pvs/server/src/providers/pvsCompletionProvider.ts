/**
 * @module PvsCompletionProvider
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

import { CompletionItem, CompletionItemKind, Position, TextEdit, Range }  from 'vscode-languageserver';
import { PVS_BUILTIN_TYPES, PVS_KEYWORDS, INNER_DECLARATION_KEYWORDS, PVS_TRUE_FALSE, PVS_LIBRARY_FUNCTIONS } from "../common/languageKeywords";
// import { PvsDeclarationDescriptor } from '../common/serverInterface';
import { PvsDefinitionProvider } from './pvsDefinitionProvider';
import * as utils from "../common/languageUtils";
import { PvsDefinition } from '../common/serverInterface';

import * as fsUtils from '../common/fsUtils';
import * as path from 'path';

export class PvsCompletionProvider {
	protected definitionProvider: PvsDefinitionProvider;

	protected coreCompletionItems: CompletionItem[] = [];
	protected declarationCompletionItems: CompletionItem[] = [];

	protected mathSymbols: {
		description: string,
		prefix: string,
		scope: string,
		body: string[]
	}[];

	protected lowerAndUpperCase (elems: string[], kind: CompletionItemKind): CompletionItem[] {
		if (elems) {
			return elems.map(keyword => {
				return {
					label: keyword !== "o" ? keyword.toLocaleUpperCase() : keyword,
					insertText: keyword.toLocaleUpperCase(),
					commitCharacters: ['\n'],
					kind
				};
			}).concat(elems.map(keyword => {
				return {
					label: keyword.toLocaleLowerCase(),
					insertText: keyword.toLocaleUpperCase(),
					commitCharacters: ['\n'],
					kind
				};
			}));
		}
		return [];
	}

	/**
	 * @constructor
	 * @param declarationProvider Service used by IntelliSense engine to retrieve type information
	 */
	constructor (declarationProvider: PvsDefinitionProvider) {
		this.definitionProvider = declarationProvider;
		const types: CompletionItem[] = PVS_BUILTIN_TYPES.map((tp) => {
			return {
				label: tp,
				insertText: tp,
				commitCharacters: ['\n'],
				kind: CompletionItemKind.Constant
			};
		});
		const keywords: CompletionItem[] = this.lowerAndUpperCase(PVS_KEYWORDS, CompletionItemKind.Keyword);
		const declarationKeywords: CompletionItem[] = this.lowerAndUpperCase(INNER_DECLARATION_KEYWORDS, CompletionItemKind.Keyword);
		const truefalse: CompletionItem[] = this.lowerAndUpperCase(PVS_TRUE_FALSE, CompletionItemKind.Constant);
		const functions: CompletionItem[] = PVS_LIBRARY_FUNCTIONS.map((x) => {
			return {
				label: x,
				insertText: x,
				commitCharacters: ['\n'],
				kind: CompletionItemKind.Function
			};
		});

		this.coreCompletionItems = keywords.concat(truefalse).concat(functions);
		this.declarationCompletionItems = types.concat(declarationKeywords);
		this.loadMathSymbols(); // async call, no need to wait the completion of the call
	}
	/**
	 * Loads mathematical symbols from symbols.json
	 */
	protected async loadMathSymbols () {
		const symbolsFileName: string = path.join(__dirname, "../../../symbols.json");
		const symbols: string = await fsUtils.readFile(symbolsFileName);
		try {
			this.mathSymbols = JSON.parse(symbols);
		} catch (jsonError) {
			console.error("[pvs-completion-provider] Warning: error while parsing symbols file ", symbolsFileName);
		}
	}
	/**
	 * Standard API of the language server, provides a completion list while typing a pvs expression
	 * @param document Text document requiring intellisense
	 * @param position Current position of the cursor
	 * @param token Cancellation token
	 */
	async provideCompletionItems(document: { fname: string, txt: string }, position: Position): Promise<CompletionItem[]> {
		if (document && document.txt) {
			const lastCharacter: string = fsUtils.getText(document.txt, {
				start: { line: position.line, character: (position.character > 0) ? position.character - 1 : 0 },
				end: position
			});
			if (lastCharacter === "\\" && this.mathSymbols) {
				// math mode, implemented using snippets (see definitions in symbols.json)
				const lines: string[] = document.txt.split("\n");
				if (lines && lines.length > position.line) {
					const lineText: string = lines[position.line];
					const currentInput: string = lineText.substr(0, position.character).trim();
					const match: RegExpMatchArray = new RegExp(/\\[^\s]*/g).exec(currentInput);
					const range: Range = {
						start: { line: position.line, character: position.character - match[0].length }, 
						end: { line: position.line, character: position.character } 
					};
					const items: CompletionItem[] = this.mathSymbols.map(elem => {
						return {
							label: elem.prefix,
							documentation: elem.description,
							insertText: elem.body[0],
							additionalTextEdits: [ TextEdit.replace(range, "") ] // this removes \\label
						};
					});
					return items;

					// return Promise.resolve([{
					// 	label: "iff",
					// 	"prefix": "\\iff",
					// 	textEdit: (match && match.length > 0) ? { // this is used to delete \\iff
					// 		range: {
					// 			start: { line: position.line, character: position.character - 2 - match[0].length }, 
					// 			end: { line: position.line, character: position.character - 2 } 
					// 		},
					// 		newText: ""
					// 	} : null,
					// 	insertText: "⇔"
					// }]);
				}
			} else if (this.definitionProvider) {// && lastCharacter.trim()) { // lastCharacter.trim() makes autocompletion more gentle, as it disables tooltips when the user presses the space bar
				const lines: string[] = document.txt.split("\n");
				if (lines && lines.length > position.line) {
					let ans: CompletionItem[] = [];
					const lineText: string = lines[position.line];
					const currentLine: string = lineText.substring(0, position.character).trim();
					let currentInput: string = currentLine.substring(currentLine.lastIndexOf(" ") + 1);
					currentInput = (currentInput.includes(":")) ? currentInput.substring(currentInput.lastIndexOf(":") + 1).trim() : currentInput;
					currentInput = (currentInput.includes("`")) ? currentInput.substring(currentInput.lastIndexOf("`") + 1).trim() : currentInput;
					currentInput = (currentInput.includes("(")) ? currentInput.substring(currentInput.lastIndexOf("(") + 1).trim() : currentInput;
					currentInput = (currentInput.includes(")")) ? currentInput.substring(currentInput.lastIndexOf(")") + 1).trim() : currentInput;
					currentInput = (currentInput.includes("[")) ? currentInput.substring(currentInput.lastIndexOf("[") + 1).trim() : currentInput;
					currentInput = (currentInput.includes("]")) ? currentInput.substring(currentInput.lastIndexOf("]") + 1).trim() : currentInput;

					if (currentInput) {
						// add relevant local symbols
						const start: number = (position.line - 20) > 0 ? position.line - 20 : 0;
						let chunk: string = lines.slice(start, position.line).join("\n").replace(utils.commentRegexp, ""); // this removes all commented text
						chunk += " " + currentLine.replace(currentInput, "");
						const localSymbols: string[] = utils.listSymbols(chunk);
						for (let i = 0; i < localSymbols.length; i++) {
							const term: string = localSymbols[i];
							if (term.startsWith(currentInput)) {
								ans.push({
									label: term,
									insertText: term,
									commitCharacters: ['\n'],
									kind: CompletionItemKind.Variable
								});
							}
						}

						// if theories or datatypes have not been declared, include the THEORY keyword
						const theoryDeclared: boolean = new RegExp(utils.isense.theoryDeclaration).test(document.txt);
						if (!theoryDeclared) {
							//--- the user is declaring a theory
							if("THEORY".startsWith(currentInput.toUpperCase())) {
								return Promise.resolve([{
									label: "THEORY",
									insertText: "THEORY",
									commitCharacters: ['\n'],
									kind: CompletionItemKind.Keyword
								}]);
							}
						}
						const datatypeDeclared: boolean = new RegExp(utils.isense.datatypeDeclaration).test(document.txt);
						if (!datatypeDeclared) {
							//--- the user is declaring a theory
							if("DATATYPE".startsWith(currentInput.toUpperCase())) {
								return Promise.resolve([{
									label: "DATATYPE",
									insertText: "DATATYPE",
									commitCharacters: ['\n'],
									kind: CompletionItemKind.Keyword
								}]);
							}
						}
					}

					if (currentLine) {
						//--- the user is entering theory declarations or theory parameters
						// check if we are declaring a formula
						const declarationLine: boolean = new RegExp(utils.isense.declaration).test(currentLine);
						const formulaDeclared: boolean = new RegExp(utils.isense.formulaDeclaration).test(currentLine);
						if (declarationLine && !formulaDeclared) {
							if (this.declarationCompletionItems) {
								ans = ans.concat(this.declarationCompletionItems.filter((item: CompletionItem) => {
									return item.label.toUpperCase().startsWith(currentInput.toUpperCase());
								}));
							}
							// if (ans && ans.length) {
							// 	return ans;
							// }
						}

						let match: RegExpMatchArray = null;
						// add records
						if (match = (new RegExp(utils.isense.recordExpression).exec(currentLine) || new RegExp(utils.isense.recordAccessor).exec(currentLine))) {
							// RegExp objects are stateful, we need to reset them every time
							// utils.isense.recordExpression.lastIndex = utils.isense.recordAccessor.lastIndex = 0;
							if (currentLine.trim().endsWith("(#") || currentLine.trim().endsWith(",")) {
								console.log("[pvs-completion-provider] provideCompletionItems, currentLine: ", currentLine);
								// resolve accessor
								const symbolName: string = match[1];
								let declarations: PvsDefinition[] = await this.definitionProvider.findSymbolDefinition(document.fname, symbolName, position);
								if (declarations && declarations.length === 1 && declarations[0].symbolDeclaration) {
									let decl: PvsDefinition = declarations[0];
									let tmp: RegExpExecArray = utils.RECORD.declaration.exec(decl.symbolDeclaration);
									utils.RECORD.declaration.lastIndex = 0;
									// const id: string = tmp[1];
									const isTypeDeclaration: boolean = tmp[2].toUpperCase() === "TYPE";
									// const isUninterpreted: boolean = !tmp[3];
									if (!isTypeDeclaration) {
										const typeName: string = tmp[2];
										let definitions: PvsDefinition[] = await this.definitionProvider.findSymbolDefinition(document.fname, typeName, position);
										decl = (definitions && definitions.length === 1) ? definitions[0] : null;
									}
									if (decl) {
										tmp = utils.RECORD.accessors.exec(decl.symbolDeclaration);
										utils.RECORD.accessors.lastIndex = 0;
										if (tmp && tmp.length > 1) {
											const recordFields: string = tmp[1];
											if (recordFields) {
												const fields: string[] = recordFields.split(",");
												if (fields) {
													const fieldNames: CompletionItem[] = fields.map((decl) => {
														const insertText: string = decl.split(":")[0].trim() + " := ";
														return {
															label: insertText, //decl.trim(),
															insertText: insertText,
															kind: CompletionItemKind.Field
														};
													});
													return Promise.resolve(fieldNames);
												}
											}
										}
									}
								}
								return Promise.resolve([]);
							}
						} else {
							// resolve accessor type
							// TODO
						}
					}
					
					// add core completion items
					if (this.coreCompletionItems) {
						ans = ans.concat(this.coreCompletionItems.filter((item: CompletionItem) => {
							return item.label.startsWith(currentInput);
						}));
					}
					return Promise.resolve(ans);
				}
			}
		}
		return null;
	}
}