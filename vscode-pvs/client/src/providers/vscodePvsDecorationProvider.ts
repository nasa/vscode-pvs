import { TextEditorDecorationType } from "vscode";
import * as language from "../common/languageKeywords";
import { PvsSymbolKind, isPvsFile } from '../common/serverInterface';
import { window, TextEditor, TextDocument, DecorationOptions, Position, Range } from 'vscode';

const regexp = {
	keywords: new RegExp(language.PVS_RESERVED_WORDS_REGEXP_SOURCE, "gi"),
	comments: new RegExp(language.PVS_COMMENT_REGEXP_SOURCE, "g")
};
const keywordsDecorator: TextEditorDecorationType = window.createTextEditorDecorationType({
	cursor: 'text',
	color: { id: 'pvs.keywords.color' }, // See package.json for declaration and default values.
	fontWeight: "500" // 100 is thin, 400 is normal, 700 is bold
});

export class VSCodePvsDecorationProvider {
	updateDecorations(editor: TextEditor) {
		if (editor && isPvsFile(editor.document.fileName)) {
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