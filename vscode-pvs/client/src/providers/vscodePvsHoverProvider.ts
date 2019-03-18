"use strict";

import { Range, Position, HoverProvider, workspace, ExtensionContext, languages,
		  Disposable, commands, window, Uri, MarkdownString, TextDocument,
		  CancellationToken, Hover } from "vscode";
import { PvsDeclarationDescriptor, PeekDefinitionCommand } from '../common/serverInterface'
import { log } from '../utils/logger';
import { LanguageClient } from 'vscode-languageclient';
import * as comm from '../common/serverInterface';
import * as language from "../common/languageKeywords";
import * as path from 'path';
import { findTheoryName } from '../common/languageUtils';

export class VSCodePvsHoverProvider implements HoverProvider {
	private client: LanguageClient;
	private pvsLibrariesPath: string;
	constructor (client: LanguageClient) {
		this.client = client;
		this.pvsLibrariesPath = path.join(workspace.getConfiguration().get("pvs.path"), "lib");
	}
	activate (context: ExtensionContext) {
		languages.registerHoverProvider({
			scheme: 'file',
			language: 'pvs'
		}, this);
		let cmd: Disposable = commands.registerCommand('vscode-pvs.open', (resource: comm.PeekDefinitionCommand) => {
			const contextFolder = (comm.PVS_LIBRARY_FILES[resource.fileName]) ? this.pvsLibrariesPath
						: comm.getPathname(window.activeTextEditor.document.fileName);
			const fileName = comm.PVS_LIBRARY_FILES[resource.fileName] || (resource.fileName + ".pvs");
			const uri: Uri = Uri.file(path.join(contextFolder, fileName));
			commands.executeCommand('vscode.open', uri, {
				viewColumn: window.activeTextEditor.viewColumn + 1, // this allows to open a window on the right side of the current editor
				selection: new Range(
					new Position(resource.range.start.line - 1, resource.range.start.character), // vscode counts lines from 0, pvs starts from 1
					new Position(resource.range.end.line - 1, resource.range.end.character)
				)
			});
		});
		context.subscriptions.push(cmd);
	}
	private getWordRange(document: TextDocument, position: Position): Range {
		return document.getWordRangeAtPosition(position, new RegExp(language.PVS_STRING_REGEXP_SOURCE, "g"))
				|| document.getWordRangeAtPosition(position, new RegExp(language.PVS_NUMBER_REGEXP_SOURCE, "g"))
				|| document.getWordRangeAtPosition(position);
	}
	private provideDefinition(document: TextDocument, position: Position): Promise<comm.PvsFindDeclarationResponse> {
		const symbolRange: Range = this.getWordRange(document, position); 
		const symbolName: string = document.getText(symbolRange);
		const line: number = symbolRange.start.line + 1; // vscode starts lines from 0, we want to start from 1 (as in pvs)
		const character: number = symbolRange.start.character;
		const fileName: string = document.fileName;

		const theoryName: string = findTheoryName(document.getText(), line); //this.findTheory(document, line);

		return new Promise((resolve, reject) => {
			this.client.onRequest("server.response.find-definition", async (resp: comm.PvsFindDeclarationResponse) => {
				const desc: comm.PvsFindDeclarationResponse = {
					file: fileName,
					theory: theoryName,
					line: line,
					character: character,
					symbolDoc: "",
					symbolName: resp.symbolName,
					symbolTheory: resp.symbolTheory,
					symbolDeclaration: resp.symbolDeclaration,
					symbolDeclarationRange: resp.symbolDeclarationRange,
					symbolDeclarationFile: resp.symbolDeclarationFile,
					comment: resp.comment,
					error: resp.error
				};
				resolve(desc);
			});
			this.client.sendRequest("pvs.find-definition", {
				file: document.fileName,
				theory: theoryName,
				symbolName: symbolName,
				line: line,
				character: character
			});
		});
	}
	async provideHover (document: TextDocument, position: Position, token: CancellationToken) {
		if (document.fileName.endsWith(".pvs")) {
			// load the text preceeding the current position and check if this is a comment
			const line: string = document.getText(
				new Range(
					new Position(position.line, 0), 
					new Position(position.line, position.character)
				)
			);
			const commentRegex: RegExp = new RegExp(language.PVS_COMMENT_REGEXP_SOURCE, "gi");
			const isComment: boolean = commentRegex.test(line);
			if (!isComment) {
				let desc: comm.PvsFindDeclarationResponse = await this.provideDefinition(document, position);
				if (desc) {
					let contents: MarkdownString[] = [];
					if (desc.symbolTheory && desc.symbolDeclaration) {
						if (desc.symbolDeclarationRange && desc.symbolDeclarationFile) {
							const peekDefinitionCommand: PeekDefinitionCommand = {
								fileName: desc.symbolDeclarationFile,
								range: desc.symbolDeclarationRange
							};
							const link: MarkdownString = new MarkdownString(
								"[" + desc.symbolDeclarationFile + ".pvs "
								+ "(Ln " + desc.symbolDeclarationRange.start.line 
								+ ", Col " + desc.symbolDeclarationRange.start.character + ")]"
								+ "(command:vscode-pvs.open?"
								+ JSON.stringify(peekDefinitionCommand)
								+ ")"
							);
							link.isTrusted = true;
							contents.push(link);
						}
						contents.push(new MarkdownString().appendCodeblock(desc.symbolDeclaration, 'pvs')); // FIXME: syntax highlighting is not working in the hover?
						// contents.push(new vscode.MarkdownString("Theory " + desc.symbolTheory).appendCodeblock(desc.symbolDeclaration, 'pvs'));
						// contents.push(new vscode.MarkdownString(desc.symbolDeclaration));
					} else if (desc.comment) {
						contents.push(new MarkdownString(desc.comment));
					} else if (desc.error) {
						contents.push(new MarkdownString(desc.error.msg));
					}
					return new Hover(contents);
				}
			}
		}
		return null;
	}
	refresh (document: TextDocument, position: Position) {
		// need to understand how to force hover refresh in the editor, the following line does not seem to trigger rendering of the hover
		// vscode.commands.executeCommand('vscode.executeHoverProvider', document.uri, position); // this is not working
		this.provideHover(document, position, null); // this is not working either
		return this;
	}
}