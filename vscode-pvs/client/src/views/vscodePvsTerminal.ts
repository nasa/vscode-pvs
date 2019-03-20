import * as vscode from 'vscode';
import * as path from 'path';
import { findTheoryName, findTheorem } from '../common/languageUtils';
import { getPathname, getFilename } from '../common/serverInterface';

class ProverTerminal {
    pvsExecutable: string;
    pvsInterpreter: string;
    pvsContextPath: string;
    terminal: vscode.Terminal;
    constructor (pvsInterpreter: string) {
        this.pvsInterpreter = pvsInterpreter;
        this.pvsExecutable = path.join(vscode.workspace.getConfiguration().get("pvs.path"), "pvs");
        this.pvsContextPath = getPathname(vscode.window.activeTextEditor.document.fileName);
        // (<any>this.terminal).onDidWriteData((data: string) => {
        //     console.log(data);
        // });
    }
    private typecheckFile(fileName: string) {
        // vscode.window.setStatusBarMessage("Typechecking " + fileName);
        this.terminal.sendText('(typecheck-file "' + fileName + '" nil nil nil)');
    }
    private proveFormula(theoryName: string, formula: string) {
        // vscode.window.setStatusBarMessage("Proving formula " + formula);
        this.terminal.sendText('(prove-formula "' + theoryName + '" "' + formula +'" nil)');
        this.terminal.sendText('(rewrite-msg-off)');
    }
    prove (fileName: string, theoryName: string, formula: string) {
        const terminalName: string = `prove ${formula}`;
        const args: string[] = [ this.pvsInterpreter, this.pvsExecutable];
        this.terminal = vscode.window.createTerminal(terminalName, 'node', args);
        this.terminal.show();
        this.typecheckFile(fileName);
        this.proveFormula(theoryName, formula);
        return this;
    }
    typecheck (fileName: string) {
        const terminalName: string = `typecheck ${fileName}`;
        const args: string[] = [ this.pvsInterpreter, this.pvsExecutable ];
        this.terminal = vscode.window.createTerminal(terminalName, 'node', args);
        this.terminal.show();
        this.typecheckFile(fileName);
        return this;
    }
    printMessage(msg: string) {
        this.terminal.sendText("echo '" + msg + "'");
        return this;
    }
}

export class VSCodePvsTerminal {
    activate (context: vscode.ExtensionContext) {
        const pvsInterpreter: string = context.asAbsolutePath(path.join('server', 'out', 'pvsCmd'));
        context.subscriptions.push(vscode.commands.registerCommand('pvs.terminal.prove', () => {
            if (vscode.window.activeTextEditor) {
                let fileName: string = vscode.window.activeTextEditor.document.fileName;
                fileName = fileName.endsWith(".pvs") ? fileName.substr(0, fileName.length - 4) : fileName; // remove file extension
                fileName = fileName.split("/").slice(-1)[0]; // remove path

                const text: string = vscode.window.activeTextEditor.document.getText();
                const line: number = vscode.window.activeTextEditor.selection.active.line;
                const theoryName: string = findTheoryName(text, line);
                const formula: string = findTheorem(text, line);

                const pvsTerminal: ProverTerminal = new ProverTerminal(pvsInterpreter);
                pvsTerminal.prove(fileName, theoryName, formula);
            }
        }));
        context.subscriptions.push(vscode.commands.registerCommand('pvs.terminal.typecheck', () => {
            if (vscode.window.activeTextEditor) {
                let fileName: string = vscode.window.activeTextEditor.document.fileName;
                fileName = fileName.split(".").slice(0, -1).join("."); // remove file extension
                fileName = fileName.split("/").slice(-1)[0]; // remove path

                const pvsTerminal: ProverTerminal = new ProverTerminal(pvsInterpreter);
                pvsTerminal.typecheck(fileName);
            }
        }));
    }
}