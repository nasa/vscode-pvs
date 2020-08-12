/**
 * @module VSCodePvsEmacsBindingsProvider
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

/**
 * PVS emacs bindings:
 * - typecheck: M-x tc
 * - typecheck-prove: M-x tcp
 * - prove: M-x prove
 * - show tccs: M-x tccs
 * - pvsio: M-x pvsio
 * - view prelude: M-x view-prelude-file
 */
import { ExtensionContext, commands, window, TextDocument, InputBox } from 'vscode';
import { LanguageClient } from 'vscode-languageclient';
import { workspace } from 'vscode';
import * as fsUtils from '../common/fsUtils';
import * as utils from '../common/languageUtils';
import { VSCodePvsStatusBar } from '../views/vscodePvsStatusBar';
import { serverCommand, PvsFormula } from '../common/serverInterface';

/**
 * cmds is the list of commands that are supported by the emacs binding defined in this module
 * NB: The order of the commands in the array affects the behavior of autocompletion
 *     (autocompletion returns the first match that starts with the user input)
 */
const cmds: string[] = [
	"tc", "typecheck",
	"tcp", "typecheck-prove",
	"pr", "prove",
	"prt",
	"pvsio",

	"add-pvs-library",
	"clear-pvs-library-path",
	"reboot-pvs",
	"restart-pvs",
	"reinstall-pvs",
	"reinstall-nasalib",
	"set-pvs-path",
	"settings",

	"step-proof",
	"show-tccs",
	"vpf", "view-prelude-file"
];

export class VSCodePvsEmacsBindingsProvider {
	protected client: LanguageClient;
	protected inputBox: InputBox;
	protected metax: string = "M-x ";
	protected userInput: string; // used by autocompletion
	protected statusBar: VSCodePvsStatusBar;

	constructor (client: LanguageClient, statusBar: VSCodePvsStatusBar) {
		this.client = client;
		this.statusBar = statusBar;
	}
	activate (context: ExtensionContext) {
		// do nothing for now
	}
	protected autocompleteInput(input: string): string {
		if (input) {
			for (let i = 0; i < cmds.length; i++) {
				if (cmds[i].startsWith(input)) {
					return cmds[i];
				}
			}
		}
		return input;
	}
	protected onDidAccept(userInput: string) {
		if (userInput) {
			userInput = userInput.toLowerCase();
			const document: TextDocument = window.activeTextEditor.document;
			const line: number = window.activeTextEditor.selection.active.line;
			const theoryName: string = utils.findTheoryName(document.getText(), line);
			const formulaName: string = utils.findFormulaName(document.getText(), line);
			const desc: PvsFormula = { 
				fileName: fsUtils.getFileName(document.fileName),
				fileExtension: fsUtils.getFileExtension(document.fileName),
				contextFolder: fsUtils.getContextFolder(document.fileName),
				theoryName,
				formulaName
			};
			switch (userInput) {
				case "add-pvs-library": {
					commands.executeCommand('vscode-pvs.add-pvs-library');
					break;
				}
				case "clear-pvs-library-path": {
					commands.executeCommand('vscode-pvs.clear-pvs-library-path');
					break;
				}
				case "show-tccs": {
					desc.fileExtension = ".pvs"; // force file extension, in the case the command is invoked from the .tccs file
					commands.executeCommand('vscode-pvs.show-tccs', desc);
					break;
				}
				case "tc": 
				case "typecheck": {
					desc.fileExtension = ".pvs"; // force file extension, in the case the command is invoked from the .tccs file
					commands.executeCommand('vscode-pvs.typecheck-file', desc);
					break;
				}
				case "tcp": 
				case "typecheck-prove": {
					desc.fileExtension = ".pvs"; // force file extension, in the case the command is invoked from the .tccs file
					commands.executeCommand('vscode-pvs.discharge-tccs', desc);
					break;
				}
				case "parse": {
					desc.fileExtension = ".pvs"; // force file extension, in the case the command is invoked from the .tccs file
					commands.executeCommand('vscode-pvs.parse-file', desc);
					break;
				}
				case "pr":
				case "prove": {
					commands.executeCommand('vscode-pvs.prove-formula', desc);
					break;
				}
				case "prt": {
					desc.fileExtension = ".pvs"; // force file extension, in the case the command is invoked from the .tccs file
					commands.executeCommand('vscode-pvs.autorun-theory', desc);
					break;
				}
				case "pvsio": {
					desc.fileExtension = ".pvs"; // force file extension, in the case the command is invoked from the .tccs file
					commands.executeCommand('vscode-pvs.pvsio-evaluator', desc);
					break;
				}
				case "step-proof": {
					commands.executeCommand('vscode-pvs.prove-formula', desc)
					break;
				}
				case "restart-pvs":
				case "reboot-pvs": {
					commands.executeCommand('vscode-pvs.reboot-pvs');
					// const pvsPath: string = workspace.getConfiguration().get(`pvs.path`);
					// this.client.sendRequest(serverCommand.rebootPvsServer, { pvsPath });
					break;
				}
				case "reinstall-pvs": {
					commands.executeCommand('vscode-pvs.reinstall-pvs');
					break;
				}
				case "reinstall-nasalib": {
					commands.executeCommand('vscode-pvs.reinstall-nasalib');
					break;
				}
				case "settings": {
					commands.executeCommand('workbench.action.openSettings', '@ext:paolomasci.vscode-pvs');
					break;
				}
				case "set-pvs-path": {
					commands.executeCommand('vscode-pvs.set-pvs-path');
					break;
				}
				case "vpf":
				case "view-prelude-file": {
					commands.executeCommand('vscode-pvs.view-prelude-file');
				}
				default: {
					window.showWarningMessage(`Command ${userInput} not supported`);
				}
			}
		}
	}
	metaxPrompt (): void {
		this.statusBar.showMsg(this.metax);
		// window.showInputBox({
		// 	prompt: "M-x ",
		// }).then((userInput: string) => {
		this.inputBox = window.createInputBox();
		this.inputBox.prompt = this.metax;
		this.inputBox.onDidAccept(() => {
			this.onDidAccept(this.userInput);
			this.inputBox.dispose();
			this.statusBar.ready();
		});
		this.inputBox.onDidChangeValue((input: string) => {
			// FIXME: VSCode does not seem to capture tabs in the input box??
			this.userInput = this.autocompleteInput(input);
			this.inputBox.prompt = this.metax + this.userInput;
			this.statusBar.showMsg(this.inputBox.prompt);
		});
		this.inputBox.show();
	}
}