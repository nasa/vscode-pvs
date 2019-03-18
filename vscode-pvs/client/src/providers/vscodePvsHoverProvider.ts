"use strict";

import { Range, Position, HoverProvider, workspace, ExtensionContext, languages,
		  Disposable, commands, window, Uri, MarkdownString, TextDocument,
		  CancellationToken, Hover } from "vscode";
import { PvsDeclarationDescriptor, PeekDefinitionCommand } from '../common/serverInterface'
import { log } from '../utils/logger';
import * as comm from '../common/serverInterface';
import * as language from "../common/languageKeywords";
import * as path from 'path';
import { VSCodePvsDefinitionProvider } from "./vscodePvsDefinitionProvider";


export class VSCodePvsHoverProvider implements HoverProvider {
	private definitionProvider: VSCodePvsDefinitionProvider;
	private pvsLibrariesPath: string;
	constructor (definitionProvider: VSCodePvsDefinitionProvider) {
		this.definitionProvider = definitionProvider;
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
				let desc: comm.PvsFindDeclarationResponse = await this.definitionProvider.provideDefinition(document, position, null);
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