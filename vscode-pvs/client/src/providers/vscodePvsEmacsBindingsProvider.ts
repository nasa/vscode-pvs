/**
 * PVS emacs bindings:
 * - typecheck: M-x tc
 * - typecheck-prove: M-x tcp
 * - prove: M-x prove
 * - show tccs: M-x tccs
 * - pvsio: M-x pvsio
 * - view prelude: M-x view-prelude-file
 */
import { ExtensionContext, commands, window, Disposable, TextDocument, InputBox, QuickInputButtons } from 'vscode';
import { LanguageClient } from 'vscode-languageclient';
import { findTheoryName } from '../common/languageUtils';

const cmds: string[] = [
	"tc", "typecheck",
	"tcp", "typecheck-prove",
	//"pr", 
	"prove",
	"show-tccs",
	"pvsio",
	"step-proof"
];

export class VSCodePvsEmacsBindingsProvider {
	private client: LanguageClient;
	private inputBox: InputBox;
	private metax: string = "M-x ";
	private userInput: string; // used by autocompletion

	constructor (client: LanguageClient) {
		this.client = client;
	}
	private autocompleteInput(input: string): string {
		if (input) {
			for (const i in cmds) {
				if (cmds[i].startsWith(input)) {
					return cmds[i];
				}
			}
		}
		return "";
	}
	private onDidAccept(userInput) {
		if (userInput) {
			userInput = userInput.toLowerCase();
			// const document: TextDocument = window.activeTextEditor.document;
			switch (userInput) {
				case "tc": 
				case "typecheck": {
					// typecheck current file
					this.client.sendRequest('pvs.typecheck-file-and-show-tccs', window.activeTextEditor.document.fileName);
					// commands.executeCommand("terminal.pvs.typecheck");
					// this.client.sendRequest('pvs.typecheck-file', {
					// 	fileName: document.fileName
					// });
					break;
				}
				case "tcp": 
				case "typecheck-prove": {
					this.client.sendRequest('pvs.typecheck-prove-and-show-tccs', window.activeTextEditor.document.fileName);
					// commands.executeCommand("terminal.pvs.typecheck-prove");
					break;
				}
				case "pr":
				case "prove": {
					// open pvs terminal
					commands.executeCommand("terminal.pvs.prove");
					break;
				}
				case "show-tccs": {
					const fileName: string = window.activeTextEditor.document.fileName;
					const line: number = window.activeTextEditor.selection.active.line;
					const text: string = window.activeTextEditor.document.getText();
					const theoryName: string = findTheoryName(text, line);
					if (theoryName) {
						this.client.sendRequest('pvs.show-tccs', [ fileName, theoryName ]);
					} else {
						window.showErrorMessage("Unable to identify theory at line " + line);
					}
					break;
				}
				case "pvsio": {
					// open pvsio terminal
					commands.executeCommand("terminal.pvsio");
					break;
				}
				case "step-proof": {
					// open pvs terminal
					commands.executeCommand("terminal.pvs.prove");
					break;
				}
				default: {}
			}
		}
	}
	activate (context: ExtensionContext) {
		let cmd: Disposable = commands.registerCommand("pvsemacs.M-x", () => {
			window.setStatusBarMessage("M-x", 2000);
			// window.showInputBox({
			// 	prompt: "M-x ",
			// }).then((userInput: string) => {
			this.inputBox = window.createInputBox();
			this.inputBox.prompt = this.metax;
			this.inputBox.onDidAccept(() => {
				this.onDidAccept(this.userInput);
				this.inputBox.dispose();
			});
			this.inputBox.onDidChangeValue((input: string) => {
				// FIXME: VSCode does not seem to capture tabs in the input box??
				this.userInput = this.autocompleteInput(input);
				this.inputBox.prompt = this.metax + this.userInput;
			});
			this.inputBox.show();
		});
		context.subscriptions.push(cmd);
	}
}