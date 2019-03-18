/**
 * @module PvsHoverProvider
 * @version 2019.02.07
 * @description Hover provider for PVS, supports the hover functionality in the editor.
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

import { PvsFindDeclarationResponse } from '../common/serverInterface';
import { TextDocument, Position, CancellationToken, Hover } from 'vscode-languageserver';
import { MarkedString } from 'vscode-languageserver-types';
import { PvsDefinitionProvider } from "./pvsDefinitionProvider";
import { PVS_LIBRARY_FILES, getPathname } from '../common/serverInterface';
import * as path from 'path';

export class PvsHoverProvider {
	/**
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
		if (document.uri.endsWith(".pvs")) {
			let desc: PvsFindDeclarationResponse = await this.definitionProvider.provideDefinition(document, position, null);
			if (desc) {
				let contents: MarkedString[] = [];
				if (desc.symbolTheory && desc.symbolDeclaration) {
					if (desc.symbolDeclarationRange && desc.symbolDeclarationFile) {
						const folder = (PVS_LIBRARY_FILES[desc.symbolDeclarationFile]) ?
										this.definitionProvider.getLibrariesPath()
											: this.definitionProvider.getContextPath();
						const fileName = PVS_LIBRARY_FILES[desc.symbolDeclarationFile] || (desc.symbolDeclarationFile + ".pvs");
						const link: MarkedString = // encoded as a markdown string
							"[" + desc.symbolDeclarationFile + ".pvs "
								+ "(Ln " + desc.symbolDeclarationRange.start.line 
								+ ", Col " + desc.symbolDeclarationRange.start.character + ")]"
							+ "(file://" + path.join(folder, fileName)
							+ "#L" + desc.symbolDeclarationRange.start.line	+ ")";
							// + ", Col " + desc.symbolDeclarationRange.start.character + ")";
						contents.push(link);
					}
					const content: MarkedString = {
						value: desc.symbolDeclaration,
						language: "pvs"
					};
					contents.push(content);
				} else if (desc.comment) {
					contents.push(desc.comment);
				} else if (desc.error) {
					contents.push(desc.error.msg);
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