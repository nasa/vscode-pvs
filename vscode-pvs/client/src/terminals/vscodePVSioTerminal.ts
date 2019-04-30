import * as vscode from 'vscode';
import * as path from 'path';
import { findTheoryName } from '../common/languageUtils';

class PVSioTerminal {
    pvsioExecutable: string;
    terminal: vscode.Terminal;
    constructor (fileName: string, theoryName: string) {
        const tname: string = `PVSio ${theoryName}`;
        this.pvsioExecutable = path.join(vscode.workspace.getConfiguration().get("pvs.path"), "pvsio");
        const args: string[] = [ this.pvsioExecutable, fileName + "@" + theoryName ];
        this.terminal = vscode.window.createTerminal(tname, '/bin/bash', args);
        this.terminal.show();
    }
    printMessage(msg: string) {
        this.terminal.sendText("echo '" + msg + "'");
    }
}

export class VSCodePVSioTerminal {
    private pvsVersionInfo: string;
    constructor (pvsVersionInfo?: string) {
        this.pvsVersionInfo = pvsVersionInfo || "PVS";
    }
    activate (context: vscode.ExtensionContext) {
        context.subscriptions.push(vscode.commands.registerCommand('terminal.pvsio', () => {
            // the file extension needs to be removed from the filename
            let activeEditor: vscode.TextEditor = vscode.window.activeTextEditor;
            if (activeEditor) {
                let fileName: string = vscode.window.activeTextEditor.document.fileName;
                fileName = fileName.split(".").slice(0, -1).join(".");
                const text: string = vscode.window.activeTextEditor.document.getText();
                const line: number = vscode.window.activeTextEditor.selection.active.line;
                const theoryName: string = findTheoryName(text, line);
                const terminal: PVSioTerminal = new PVSioTerminal(fileName, theoryName);
            }
        }));
    }
}