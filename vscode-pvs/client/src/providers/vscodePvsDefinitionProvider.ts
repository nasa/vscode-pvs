import * as vscode from "vscode";
import { LanguageClient } from 'vscode-languageclient';
// import { PvsDeclarationDescriptor } from '../common/serverInterface'
import * as comm from '../common/serverInterface';
import * as language from "../common/languageKeywords";
import { findTheoryName } from '../common/languageUtils';


export class VSCodePvsDefinitionProvider {
	private client: LanguageClient;

	constructor (client: LanguageClient) {
		this.client = client;
	}
	private getWordRange(document: vscode.TextDocument, position: vscode.Position): vscode.Range {
		return document.getWordRangeAtPosition(position, new RegExp(language.PVS_STRING_REGEXP_SOURCE, "g"))
				|| document.getWordRangeAtPosition(position, new RegExp(language.PVS_NUMBER_REGEXP_SOURCE, "g"))
				|| document.getWordRangeAtPosition(position);
	}

	// /**
	//  * @description OBSOLETE, replaced by findTheoryName 
	//  * 				Utility function, finds the name of the theory that immediately preceeds a given line
	//  * @param document 
	//  * @param line 
	//  */
	// private findTheory(document: vscode.TextDocument, line?: number): string | null {
	// 	if (line !== null && line !== undefined) {
	// 		let text: string = document.getText(new vscode.Range(
	// 			new vscode.Position(0, 0),
	// 			new vscode.Position(line, 0)
	// 		));
	// 		let candidates: string[] = [];
	// 		let regexp: RegExp = new RegExp(/(\w+)\s*(?:\[\s*[^\]]+\]\s*)?:\s*theory\s*begin/gi);
	// 		let match: RegExpMatchArray = null;
	// 		while(match = regexp.exec(text)) {
	// 			// the last match will be the closest to the current line number
	// 			candidates.push(match[1]);
	// 		}
	// 		return candidates[candidates.length - 1];
	// 	}
	// 	return null;
	// }

	async provideDefinition(document: vscode.TextDocument, position: vscode.Position, token: vscode.CancellationToken): Promise<comm.PvsFindDeclarationResponse> {
		const symbolRange: vscode.Range = this.getWordRange(document, position); 
		const symbolName: string = document.getText(symbolRange);
		const line: number = symbolRange.start.line + 1; // vscode starts lines from 0, we want to start from 1 (as in pvs)
		const character: number = symbolRange.start.character;
		const fileName: string = document.fileName;

		const theoryName: string = findTheoryName(document.getText(), line); //this.findTheory(document, line);

		return new Promise((resolve, reject) => {
			this.client.onRequest("server.response.find-definition", async function (resp: comm.PvsFindDeclarationResponse) {
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
}
