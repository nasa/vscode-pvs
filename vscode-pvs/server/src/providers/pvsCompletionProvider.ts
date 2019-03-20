/**
 * @module PvsCompletionProvider
 * @version 2019.02.07
 * @description Completion engine for PVS, provides a list of completion items to the editor.
 * @author Paolo Masci
 * @date 2019.02.07
 * @copyright 
 * Copyright 2016 United States Government as represented by the
 * Administrator of the National Aeronautics and Space Administration. No
 * copyright is claimed in the United States under Title 17, 
 * U.S. Code. All Other Rights Reserved.
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

import { CompletionItem, CompletionItemKind, TextDocument, Position }  from 'vscode-languageserver';
import { PVS_BUILTIN_TYPES, PVS_KEYWORDS, PVS_TRUE_FALSE, PVS_LIBRARY_FUNCTIONS } from "../common/languageKeywords";
import { PvsDeclarationDescriptor } from '../common/serverInterface';
import { PvsDefinitionProvider } from './pvsDefinitionProvider';
import * as languageUtils from "../common/languageUtils";
import { PvsFindDeclarationResponse } from '../common/serverInterface';

interface IntellisenseTriggers {
	recordExpression: RegExp,
	recordAccessor: RegExp
};

const isense: IntellisenseTriggers = {
	recordExpression: /(\w+)\s*=\s*\(\#/g, // rc1: Rec1 = (#... 
	recordAccessor: /(\w+)`/g // rc1`...
};

export class PvsCompletionProvider {
	private definitionProvider: PvsDefinitionProvider;
	private languageCompletionItems: CompletionItem[] = [];
	/**
	 * @constructor
	 * @param declarationProvider Service used by IntelliSense engine to retrieve type information
	 */
	constructor (declarationProvider: PvsDefinitionProvider) {
		this.definitionProvider = declarationProvider;
		const types: CompletionItem[] = PVS_BUILTIN_TYPES.map(function (tp) {
			return {
				label: tp,
				insertText: tp,
				commitCharacters: ['\n'],
				kind: CompletionItemKind.Constant
			}
		});
		const keywords: CompletionItem[] = PVS_KEYWORDS.map(function (keyword) {
			return {
				label: (keyword !== 'o') ? keyword.toUpperCase() : keyword, // make all uppercase, except operator o
				insertText: (keyword !== 'o') ? keyword.toUpperCase() : keyword,
				commitCharacters: ['\n'],
				kind: CompletionItemKind.Keyword
			}
		});
		const truefalse: CompletionItem[] = PVS_TRUE_FALSE.map(function (x) {
			return {
				label: x.toUpperCase(),
				insertText: x.toUpperCase(),
				commitCharacters: ['\n'],
				kind: CompletionItemKind.Constant
			}
		});
		const functions: CompletionItem[] = PVS_LIBRARY_FUNCTIONS.map(function (x) {
			return {
				label: x,
				insertText: x,
				commitCharacters: ['\n'],
				kind: CompletionItemKind.Function
			}
		});
		this.languageCompletionItems = types.concat(keywords).concat(truefalse).concat(functions);
	}
	/**
	 * Standard API of the language server, provides a completion list while typing a pvs expression
	 * @param document Text document requiring intellisense
	 * @param position Current position of the cursor
	 * @param token Cancellation token
	 */
	async provideCompletionItems(document: TextDocument, position: Position): Promise<CompletionItem[]> {
		if (document && this.definitionProvider) {
			const lines: string[] = document.getText().split("\n");
			if (lines && lines.length > position.line) {
				const lineText: string = lines[position.line];
				const currentInput: string = lineText.substr(0, position.character).trim();
				let localSymbols: PvsDeclarationDescriptor[] = []; // TODO

				let match: RegExpExecArray = null;
				if (match = (isense.recordExpression.exec(currentInput) || isense.recordAccessor.exec(currentInput))) {
					// RegExp objects are stateful, we need to reset them every time
					isense.recordExpression.lastIndex = isense.recordAccessor.lastIndex = 0; 
					const symbolName: string = match[1];
					let decl: PvsFindDeclarationResponse = await this.definitionProvider.findSymbolDefinition(document, symbolName, position);
					if (decl.symbolDeclaration) {
						let tmp: RegExpExecArray = languageUtils.RECORD.declaration.exec(decl.symbolDeclaration);
						languageUtils.RECORD.declaration.lastIndex = 0;
						// const id: string = tmp[1];
						const isTypeDeclaration: boolean = tmp[2].toUpperCase() === "TYPE";
						// const isUninterpreted: boolean = !tmp[3];
						if (!isTypeDeclaration) {
							const typeName: string = tmp[2];
							decl = await this.definitionProvider.findSymbolDefinition(document, typeName);
						}
						tmp = languageUtils.RECORD.accessors.exec(decl.symbolDeclaration);
						languageUtils.RECORD.accessors.lastIndex = 0;
						if (tmp && tmp.length > 1) {
							const recordFields: string = tmp[1];
							const fieldNames: CompletionItem[] = recordFields.split(",").map(function (decl) {
								const insertText: string = decl.split(":")[0].trim();
								return {
									label: decl.trim(),
									insertText: insertText,
									kind: CompletionItemKind.Field
								};
							});
							return Promise.resolve(fieldNames);
						}
					}
					return Promise.resolve([]);
				}
			}
		}
		return Promise.resolve(this.languageCompletionItems);
	}
}