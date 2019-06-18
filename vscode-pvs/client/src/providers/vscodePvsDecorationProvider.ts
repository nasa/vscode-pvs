/**
 * @module VSCodePvsDecorationProvider
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

import { TextEditorDecorationType } from "vscode";
import * as language from "../common/languageKeywords";
import * as fs from '../common/fsUtils';
import { window, TextEditor, TextDocument, DecorationOptions, Position, Range } from 'vscode';

const regexp = {
	keywords: new RegExp(language.PVS_RESERVED_WORDS_REGEXP_SOURCE, "gi"),
	comments: new RegExp(language.PVS_COMMENT_REGEXP_SOURCE)
};
const keywordsDecorator: TextEditorDecorationType = window.createTextEditorDecorationType({
	cursor: 'text',
	color: { id: 'pvs.keywords.color' }, // See package.json for declaration and default values.
	fontWeight: "500" // 100 is thin, 400 is normal, 700 is bold
});

export class VSCodePvsDecorationProvider {
	updateDecorations(editor: TextEditor) {
		if (editor && fs.isPvsFile(editor.document.fileName)) {
			const document: TextDocument = editor.document;
			const text: string = document.getText();
			let keywordsDecorations: DecorationOptions[] = [];
			let commentedSections: Position[] = [];
			let match = null;
			while (match = regexp.comments.exec(text)) {
				const startPos: Position = document.positionAt(match.index);
				// Syntax highlighting for comments is performed using pvs-language.json
				// const endPos: Position = document.positionAt(match.index + match[0].length);
				// const decoration: DecorationOptions = { range: new Range(startPos, endPos), hoverMessage: null };
				// decorations.comments.push(decoration);
				commentedSections.push(startPos);
			}
			regexp.keywords.lastIndex = 0;
			// syntax highlighting for comments, strings, builtin types, operators is performed using pvs-language.json for performance reason	
			while (match = regexp.keywords.exec(text)) {
				const startPos: Position = document.positionAt(match.index);
				let isComment: boolean = commentedSections.some((pos: Position) => {
					return pos.line === startPos.line && pos.character <= startPos.character;
				});
				if (!isComment) {
					const endPos: Position = document.positionAt(match.index + match[0].length);
					const decoration: DecorationOptions = {
						range: new Range(startPos, endPos)
					};
					keywordsDecorations.push(decoration);
				}
			}
			regexp.comments.lastIndex = 0;
			editor.setDecorations(keywordsDecorator, keywordsDecorations);
		}
	}
}