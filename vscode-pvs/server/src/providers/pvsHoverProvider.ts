/**
 * @module PvsHoverProvider
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

import { PvsDefinition } from '../common/serverInterface';
import { TextDocument, Position, CancellationToken, Hover, Range } from 'vscode-languageserver';
import { MarkedString } from 'vscode-languageserver-types';
import { PvsDefinitionProvider } from "./pvsDefinitionProvider";
import * as language from "../common/languageKeywords";
import * as fsUtils from '../common/fsUtils';
import * as utils from '../common/languageUtils';

export class PvsHoverProvider {
	/**`
	 * Pointer to the definition provider
	 */
	private definitionProvider: PvsDefinitionProvider;

	/**
	 * Creates a new hover provider
	 * @param definitionProvider Definition provider, necessary for resolving symbol definitions.
	 */
	constructor (definitionProvider: PvsDefinitionProvider) {
		this.definitionProvider = definitionProvider;
	}
	
	/**
	 * Standard API of the language server for requesting hover information, activated when the cursor is over a symbol
	 * @param desc Current document
	 * @param position Current cursor position
	 * @param token Cancellation token (optional).
	 */
	async provideHover (desc: { txt: string, uri: string, position: Position }, token?: CancellationToken): Promise<Hover> {
		if (desc && desc.txt && desc.uri && desc.position) {
			let contents: MarkedString[] = [];
			// load the text preceeding the current position and check if this is a comment
			const line: string = fsUtils.getText(desc.txt, {
				start: { line:desc.position.line },
				end: { line: desc.position.line }
			});
			// check if the cursor is over a comment -- if that's the case, do nothing
			const commentRegex: RegExp = new RegExp(language.PVS_COMMENT_REGEXP_SOURCE);
			const matchComment: RegExpMatchArray = commentRegex.exec(line);
			const isComment: boolean = matchComment && matchComment.index <= desc.position.character; //commentRegex.test(line);
			if (isComment) { return null; }
			// else, not a comment
			const ans: { symbolName: string, definitions: PvsDefinition[] } = await this.definitionProvider.getDefinition(desc);
			if (ans) {
				const definitions: PvsDefinition[] = ans.definitions;
				if (definitions && definitions.length > 0) {
					// give preference to definitions in the same file
					const sameTheoryDefs: PvsDefinition[] = definitions.filter((def: PvsDefinition) => {
						return def.symbolDeclarationFile === desc.uri;
					});
					const def: PvsDefinition = (sameTheoryDefs && sameTheoryDefs.length > 0) ? sameTheoryDefs[0] : definitions[0];
					if (def.error) {
						// errors are shown alone in the hover to avoid cluttering
						contents.push(def.error.message);
					} else {
						if (def.comment) {
							contents.push({
								value: def.comment,
								language: "pvs"
							});
						}
						if (definitions.length > 1) {
							// const uri: string = (document.uri.startsWith("file://")) ? document.uri : `file://${document.uri}`;
							// contents.push(`Symbol [${ans.symbolName}](${uri}#L${document.position.line + 1}) is overloaded: ${definitions.length} definitions found.`);
							contents.push(`Symbol ${ans.symbolName} is overloaded: ${definitions.length} definitions found.`);
							contents.push(`This tooltip shows one definition. Use peek-definition to view all definitions.`);
						}	
						if (def.symbolTheory && def.symbolDeclaration) {
							if (def.symbolDeclarationRange && def.symbolDeclarationFile) {
								const fileName: string = fsUtils.getFileName(def.symbolDeclarationFile);
								const link: MarkedString = // encoded as a markdown string
									`[${fileName} (Ln ${def.symbolDeclarationRange.start.line}, Col ${def.symbolDeclarationRange.start.character})]`
									+ `(file://${def.symbolDeclarationFile}`
									+ `#L${def.symbolDeclarationRange.start.line})`;
									// + ", Col " + desc.symbolDeclarationRange.start.character + ")";
								contents.push(link);
							}
							const content: MarkedString = {
								value: def.symbolDeclaration,
								language: "pvs"
							};
							contents.push(content);
						}
					}
				} else {
					const link: MarkedString = // encoded as a markdown string
							`[${ans.symbolName}](file://${desc.uri}#L${desc.position.line + 1})`;
					contents.push(`No definition found for ${link}`);
				}
			}
			return {
				contents: contents,
				range: { start: desc.position, end: desc.position } // the hover is located at the mouse position
			};
		}
		return null;
	}
}