/**
 * PVS emacs bindings:
 * - typecheck: M-x tc
 * - prove: M-x prove
 */
import { ExtensionContext, commands, window, Disposable, TextDocument } from 'vscode';
import { LanguageClient } from 'vscode-languageclient';

export class VSCodePvsEmacsBindingsProvider {
	private client: LanguageClient;

	constructor (client: LanguageClient) {
		this.client = client;
	}
	activate (context: ExtensionContext) {
		let cmd: Disposable = commands.registerCommand("pvsemacs.M-x", () => {
			window.setStatusBarMessage("M-x", 2000);
			window.showInputBox({
				prompt: "M-x "
			}).then((userInput: string) => {
				if (userInput) {
					userInput = userInput.toLowerCase();
					// const document: TextDocument = window.activeTextEditor.document;
					switch (userInput) {
						case "tc": 
						case "typecheck": {
							// typecheck current file
							commands.executeCommand("pvs.terminal.typecheck");
							// this.client.sendRequest('pvs.typecheck-file', {
							// 	fileName: document.fileName
							// });
							break;
						}
						case "pvsio": {
							// open pvsio terminal
							commands.executeCommand("terminal.PVSio");
							break;
						}
						case "pr":
						case "prove": {
							// open pvs terminal
							commands.executeCommand("pvs.terminal.prove");
							break;
						}
					}
				}
			});
		});
		context.subscriptions.push(cmd);
	}
}