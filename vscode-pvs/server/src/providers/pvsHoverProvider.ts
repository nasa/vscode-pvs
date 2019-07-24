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
import * as path from 'path';
import * as fsUtils from '../common/fsUtils';

export class PvsHoverProvider {
	/**`
	 * Pointer to the definition provider
	 */
	private definitionProvider: PvsDefinitionProvider;

	/**
	 * folder where PVS context path
	 */
	private pvsLibrariesPath: string;

	/**
	 * Creates a new hover provider
	 * @param definitionProvider Definition provider, necessary for resolving symbol definitions.
	 */
	constructor (definitionProvider: PvsDefinitionProvider) {
		this.definitionProvider = definitionProvider;
	}
	
	/**
	 * Standard API of the language server for requesting hover information, activated when the cursor is over a symbol
	 * @param document Current document
	 * @param position Current cursor position
	 * @param token Cancellation token (optional).
	 */
	async provideHover (document: TextDocument, position: Position, token?: CancellationToken): Promise<Hover> {
		if (fsUtils.isPvsFile(document.uri)) {
			// load the text preceeding the current position and check if this is a comment
			const line: string = document.getText({
				start: { line: position.line, character: 0 },
				end: { line: position.line, character: position.character }
			});
			// check if the cursor is over a comment -- if that's the case, do nothing
			const commentRegex: RegExp = new RegExp(language.PVS_COMMENT_REGEXP_SOURCE);
			const isComment: boolean = commentRegex.test(line);
			if (isComment) { return null; }
			// else, not a comment
			let definitions: PvsDefinition[] = await this.definitionProvider.provideDefinition(document, position, null);
			if (definitions) {
				const desc: PvsDefinition = definitions[0];
				let contents: MarkedString[] = [];
				if (desc.error) {
					// errors are shown alone in the hover to avoid cluttering
					contents.push(desc.error.msg);
				} else {
					if (desc.comment) {
						contents.push({
							value: desc.comment,
							language: "pvs"
						});
					}
					if (desc.symbolTheory && desc.symbolDeclaration) {
						if (desc.symbolDeclarationRange && desc.symbolDeclarationFile) {
							const fileName: string = fsUtils.getFilename(desc.symbolDeclarationFile);
							const link: MarkedString = // encoded as a markdown string
								`[${fileName} (Ln ${desc.symbolDeclarationRange.start.line}, Col ${desc.symbolDeclarationRange.start.character})]`
								+ `(file://${desc.symbolDeclarationFile}`
								+ `#L${desc.symbolDeclarationRange.start.line})`;
								// + ", Col " + desc.symbolDeclarationRange.start.character + ")";
							contents.push(link);
						}
						const content: MarkedString = {
							value: desc.symbolDeclaration,
							language: "pvs"
						};
						contents.push(content);
					}
				}
				if (definitions.length > 1) {
					contents = contents.concat([
						`${definitions.length - 1} additional definitions found`,
						"Please use peek-definition to view all definitions"
					]);
				}
				return {
					contents: contents,
					range: { start: position, end: position } // the hover is located at the mouse position
				};
			}
		}
		return null;
	}
}