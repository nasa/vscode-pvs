import * as vscode from 'vscode';
import * as path from 'path';
import { findTheoryName, findTheorem } from '../common/languageUtils';
import { getPathname, getFilename } from '../common/serverInterface';
import { timingSafeEqual } from 'crypto';

class PvsTerminal {
    pvsExecutable: string;
    pvsInterpreter: string;
    pvsContextPath: string;
    terminal: vscode.Terminal;
    constructor (pvsInterpreter: string) {
        this.pvsInterpreter = pvsInterpreter;
    }
    prove (fileName: string, theoryName: string, formula: string) {
        this.pvsExecutable = path.join(vscode.workspace.getConfiguration().get("pvs.path"), "pvs");
        this.pvsContextPath = getPathname(vscode.window.activeTextEditor.document.fileName);
        const args: string[] = [ this.pvsInterpreter, this.pvsExecutable];
        const terminalName: string = `prove ${formula}`;
        this.terminal = vscode.window.createTerminal(terminalName, 'node', args);
        // this.terminal = vscode.window.createTerminal(terminalName, this.pvsExecutable, [ "-raw" ]);

        // (<any>this.terminal).onDidWriteData((data: string) => {
        //     console.log(data);
        // });

        this.terminal.show();
        this.terminal.sendText('(typecheck-file "' + fileName + '" nil nil nil)');
        this.terminal.sendText('(prove-formula "' + theoryName + '" "' + formula +'" nil)');
        this.terminal.sendText('(rewrite-msg-off)');
        // (prove-formula "test_me" "thm" nil)
    }
    typecheck (fileName: string) {
        this.pvsExecutable = path.join(vscode.workspace.getConfiguration().get("pvs.path"), "pvs");
        this.pvsContextPath = getPathname(vscode.window.activeTextEditor.document.fileName);
        const args: string[] = [ this.pvsInterpreter, this.pvsExecutable ];
        const terminalName: string = `typecheck ${fileName}`;
        this.terminal = vscode.window.createTerminal(terminalName, 'node', args);
        this.terminal.show();
        this.terminal.sendText('(typecheck-file "' + fileName + '" nil nil nil)');
    }
    printMessage(msg: string) {
        this.terminal.sendText("echo '" + msg + "'");
    }
}

export class VSCodePvsTerminal {
    activate (context: vscode.ExtensionContext) {
        const pvsInterpreter: string = context.asAbsolutePath(path.join('server', 'out', 'pvsCmd'));
        context.subscriptions.push(vscode.commands.registerCommand('pvs.terminal.prove', () => {
            // the file extension needs to be removed from the filename
            let activeEditor: vscode.TextEditor = vscode.window.activeTextEditor;
            if (activeEditor) {
                let fileName: string = vscode.window.activeTextEditor.document.fileName;
                fileName = fileName.split(".").slice(0, -1).join("."); // remove file extension
                fileName = fileName.split("/").slice(-1)[0]; // remove path

                const text: string = vscode.window.activeTextEditor.document.getText();
                const line: number = vscode.window.activeTextEditor.selection.active.line;
                const theoryName: string = findTheoryName(text, line);
                const formula: string = findTheorem(text, line);

                const pvsTerminal: PvsTerminal = new PvsTerminal(pvsInterpreter);
                pvsTerminal.prove(fileName, theoryName, formula);
            }
        }));
        context.subscriptions.push(vscode.commands.registerCommand('pvs.terminal.typecheck', () => {
            // the file extension needs to be removed from the filename
            let activeEditor: vscode.TextEditor = vscode.window.activeTextEditor;
            if (activeEditor) {
                let fileName: string = vscode.window.activeTextEditor.document.fileName;
                fileName = fileName.split(".").slice(0, -1).join("."); // remove file extension
                fileName = fileName.split("/").slice(-1)[0]; // remove path

                const pvsTerminal: PvsTerminal = new PvsTerminal(pvsInterpreter);
                pvsTerminal.typecheck(fileName);
            }
        }));
    }
}