import { TextEditorDecorationType } from "vscode";
import * as language from "../common/languageKeywords";
import { PvsSymbolKind } from '../common/serverInterface';
import { window, TextEditor, TextDocument, DecorationOptions, Position, Range } from 'vscode';


const regexp: PvsSymbolKind<RegExp> = {
	keywords: new RegExp(language.PVS_RESERVED_WORDS_REGEXP_SOURCE, "gi"),
	numbers: new RegExp(language.PVS_NUMBER_REGEXP_SOURCE, "gi"),
	strings: new RegExp(language.PVS_STRING_REGEXP_SOURCE, "gi"),
	constants: new RegExp(language.PVS_TRUE_FALSE_REGEXP_SOURCE, "gi"),
	builtinTypes: new RegExp(language.PVS_BUILTIN_TYPE_REGEXP_SOURCE, "gi"),
	operators: new RegExp(language.PVS_LANGUAGE_OPERATORS_REGEXP_SOURCE, "gi"),
	functions: new RegExp(language.PVS_LIBRARY_FUNCTIONS_REGEXP_SOURCE, "gi"),
	comments: new RegExp(language.PVS_COMMENT_REGEXP_SOURCE, "g")
};

const decorators: PvsSymbolKind<TextEditorDecorationType> = {
	keywords: window.createTextEditorDecorationType({
		cursor: 'text',
		color: { id: 'pvs.keywords.color' }, // See package.json for declaration and default values.
		fontWeight: "500" // 100 is thin, 400 is normal, 700 is bold
	}),
	numbers: window.createTextEditorDecorationType({
		cursor: 'text',
		color: { id: 'pvs.numbers.color' } // See package.json for declaration and default values.
	}),
	strings: window.createTextEditorDecorationType({
		cursor: 'text',
		color: { id: 'pvs.strings.color' } // See package.json for declaration and default values.
	}),
	constants: window.createTextEditorDecorationType({
		cursor: 'text',
		color: { id: 'pvs.constants.color' } // See package.json for declaration and default values.
	}),
	builtinTypes: window.createTextEditorDecorationType({
		cursor: 'text',
		color: { id: 'pvs.builtinTypes.color' }, // See package.json for declaration and default values.
		fontWeight: "500" // 100 is thin, 400 is normal, 700 is bold
	}),
	operators: window.createTextEditorDecorationType({
		cursor: 'text',
		color: { id: 'pvs.operators.color' }, // See package.json for declaration and default values.
		fontWeight: "500" // 100 is thin, 400 is normal, 700 is bold
	}),
	functions: window.createTextEditorDecorationType({
		cursor: 'text',
		color: { id: 'pvs.functions.color' }, // See package.json for declaration and default values.
		fontWeight: "500" // 100 is thin, 400 is normal, 700 is bold
	}),
	comments: window.createTextEditorDecorationType({
		cursor: 'text',
		color: { id: 'pvs.comments.color' }, // See package.json for declaration and default values.
		fontWeight: "100" // 100 is thin, 400 is normal, 700 is bold
	})
};

export class VSCodePvsDecorationProvider {
	private getHoverMessage(key: string, symbol: string) {
		switch (key) {
			case "reservedKeywords": { return 'Keyword **' + symbol.toUpperCase() + '**'; }
			case "strings": { return 'String ' + symbol; }
			case "constants": { return 'Constant ' + symbol; }
			case "builtinTypes": { return 'Builtin type ' + symbol; }	
		}
		return null;
	}
	updateDecorations(editor: TextEditor) {
		if (editor && editor.document.fileName.endsWith(".pvs")) {
			const document: TextDocument = editor.document;
			const text: string = document.getText();
			let decorations: PvsSymbolKind<DecorationOptions[]> = {
				keywords: [],
				numbers: [],
				strings: [],
				constants: [],
				builtinTypes: [],
				operators: [],
				functions: [],
				comments: []
			};
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
			Object.keys(decorations).forEach((key) => {
				// syntax highlighting for comments and strings is performed using pvs-language.json
				if (key !== "comments" && key !== "strings") {
					while (match = regexp[key].exec(text)) {
						const startPos: Position = document.positionAt(match.index);
						let isComment: boolean = commentedSections.some((pos: Position) => {
							return pos.line === startPos.line && pos.character <= startPos.character;
						});
						if (!isComment) {
							const endPos: Position = document.positionAt(match.index + match[0].length);
							const decoration: DecorationOptions = {
								range: new Range(startPos, endPos), 
								hoverMessage: this.getHoverMessage(key, match[0]) 
							};
							decorations[key].push(decoration);
						}
					}
				}
			});
			Object.keys(decorations).forEach((key) => {
				editor.setDecorations(decorators[key], decorations[key]);
			});
		}
	}
}