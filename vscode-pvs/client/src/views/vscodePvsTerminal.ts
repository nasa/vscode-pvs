import * as vscode from 'vscode';
import * as path from 'path';
import { findTheoryName, findTheorem } from '../common/languageUtils';
import { getPathname, getFilename, isPvsFile } from '../common/serverInterface';
import { TextDocument } from 'vscode-languageclient';
import { VSCodePvsExplorer, TheoryDescriptor } from './vscodePvsExplorer';

class ProverTerminal {
    pvsExecutable: string;
    pvsInterpreter: string;
    pvsContextFolder: string;
    terminal: vscode.Terminal;
    constructor (pvsInterpreter: string) {
        this.pvsInterpreter = pvsInterpreter;
        this.pvsExecutable = path.join(vscode.workspace.getConfiguration().get("pvs.path"), "pvs");
        this.pvsContextFolder = getPathname(vscode.window.activeTextEditor.document.fileName);
        // (<any>this.terminal).onDidWriteData((data: string) => {
        //     console.log(data);
        // });
    }
    private pvsLispTypecheckFile(fileName: string, opt?: {
        proveTccs: boolean,
        proveImportChain: boolean
    }) {
        // vscode.window.setStatusBarMessage("Typechecking " + fileName);
        let args: string = opt ? opt.proveTccs ? "nil t 'typecheck-prove" : "t t 'typecheck-prove-importchain"
                            : "nil nil nil";
        this.terminal.sendText('(typecheck-file "' + fileName + '" ' + args + ')');
    }
    private pvsLispProveFormula(theoryName: string, formula: string) {
        // vscode.window.setStatusBarMessage("Proving formula " + formula);
        this.terminal.sendText('(prove-formula "' + theoryName + '" "' + formula +'" nil)');
        this.terminal.sendText('(rewrite-msg-off)');
    }
    private pvsLispProveTcc(theoryName: string, formula: string) {
        // (rerun-proof-at? "alaris_th" nil 12 "tccs" nil nil)
        this.terminal.sendText('(prove-formula "' + theoryName + '" "' + formula +'" t)');
    }
    private pvsLispShowTccs(theoryName: string) {
        this.terminal.sendText('(show-tccs "' + theoryName + '" nil)');
    }
    proveFormula (fileName: string, theoryName: string, formula: string, line: number) {
        const terminalName: string = `prove ${formula}`;
        const args: string[] = [ this.pvsInterpreter, this.pvsExecutable];
        this.terminal = vscode.window.createTerminal(terminalName, 'node', args);
        this.terminal.show();
        this.pvsLispTypecheckFile(fileName);
        this.pvsLispProveFormula(theoryName, formula);
        return this;
    }
    proveTcc (fileName: string, theoryName: string, formula: string, line: number) {
        const terminalName: string = `prove ${formula}`;
        const args: string[] = [ this.pvsInterpreter, this.pvsExecutable];
        this.terminal = vscode.window.createTerminal(terminalName, 'node', args);
        this.terminal.show();
        this.pvsLispTypecheckFile(fileName);
        this.pvsLispProveTcc(theoryName, formula);
        return this;
    }
    typecheck (fileName: string) {
        const terminalName: string = `typecheck ${fileName}`;
        const args: string[] = [ this.pvsInterpreter, this.pvsExecutable ];
        this.terminal = vscode.window.createTerminal(terminalName, 'node', args);
        this.terminal.show();
        this.pvsLispTypecheckFile(fileName);
        return this;
    }
    typecheckProve (fileName: string) {
        const terminalName: string = `typecheck-prove ${fileName}`;
        const args: string[] = [ this.pvsInterpreter, this.pvsExecutable ];
        this.terminal = vscode.window.createTerminal(terminalName, 'node', args);
        this.terminal.show();
        this.pvsLispTypecheckFile(fileName, {
            proveTccs: true,
            proveImportChain: false
        });
        return this;
    }
    showTccs (fileName: string, theoryName: string) {
        const terminalName: string = `show-tccs ${theoryName}`;
        const args: string[] = [ this.pvsInterpreter, this.pvsExecutable ];
        this.terminal = vscode.window.createTerminal(terminalName, 'node', args);
        this.terminal.show();
        this.pvsLispTypecheckFile(fileName);
        this.pvsLispShowTccs(theoryName);
        return this;
    }
    printMessage(msg: string) {
        this.terminal.sendText("echo '" + msg + "'");
        return this;
    }
}

export class VSCodePvsTerminal {
    private theoryExplorer;
    constructor (theoryExplorer: VSCodePvsExplorer) {
        this.theoryExplorer = theoryExplorer;
    }
    activate (context: vscode.ExtensionContext) {
        const pvsInterpreter: string = context.asAbsolutePath(path.join('server', 'out', 'pvsCmd'));
        // register handlers
        context.subscriptions.push(vscode.commands.registerCommand('terminal.pvs.prove', async () => {
            if (vscode.window.activeTextEditor && isPvsFile(vscode.window.activeTextEditor.document.fileName)) {
                const document = vscode.window.activeTextEditor.document;
                let fileName: string = document.fileName;
                const text: string = document.getText();
                const line: number = vscode.window.activeTextEditor.selection.active.line;
                let currentTheory: string = null;
                if (fileName.endsWith(".tccs")) {
                    // .tccs file
                    currentTheory = getFilename(document.fileName, { removeFileExtension: true });
                    const desc: TheoryDescriptor = this.theoryExplorer.getTheoryDescriptor(currentTheory);
                    if (desc) {
                        fileName = desc.fileName;
                    }
                } else {
                    // .pvs file
                    currentTheory = findTheoryName(text, line);
                    fileName = fileName.split(".").slice(0, -1).join("."); // remove file extension
                    fileName = (fileName.includes("/")) ? fileName.split("/").slice(-1)[0] : fileName; // remove path, if present
                }

                const formula: string = findTheorem(text, line);

                if (!currentTheory) {
                    vscode.window.showErrorMessage("Unable to find theory name :/");
                } else if (!formula) {
                    vscode.window.showErrorMessage("Unable to find formula name :/");
                } else {
                    vscode.window.showInformationMessage("Starting a new prover session for " + formula);
                    const pvsTerminal: ProverTerminal = new ProverTerminal(pvsInterpreter);
                    if (document.fileName.endsWith(".pvs")) {
                        pvsTerminal.proveFormula(fileName, currentTheory, formula, line);
                    } else if (document.fileName.endsWith(".tccs")) {
                        pvsTerminal.proveTcc(fileName, currentTheory, formula, line);
                    } else {
                        vscode.window.showErrorMessage(document.fileName + " is not a valid pvs file :/");
                    }
                }
            }
        }));
        context.subscriptions.push(vscode.commands.registerCommand('terminal.pvs.typecheck', () => {
            if (vscode.window.activeTextEditor) {
                let fileName: string = vscode.window.activeTextEditor.document.fileName;
                fileName = fileName.split(".").slice(0, -1).join("."); // remove file extension
                fileName = fileName.split("/").slice(-1)[0]; // remove path

                const pvsTerminal: ProverTerminal = new ProverTerminal(pvsInterpreter);
                pvsTerminal.typecheck(fileName);
            }
        }));
        context.subscriptions.push(vscode.commands.registerCommand('terminal.pvs.typecheck-prove', () => {
            if (vscode.window.activeTextEditor) {
                let fileName: string = vscode.window.activeTextEditor.document.fileName;
                fileName = fileName.split(".").slice(0, -1).join("."); // remove file extension
                fileName = fileName.split("/").slice(-1)[0]; // remove path

                const pvsTerminal: ProverTerminal = new ProverTerminal(pvsInterpreter);
                pvsTerminal.typecheckProve(fileName);
            }
        }));
        context.subscriptions.push(vscode.commands.registerCommand('terminal.pvs.typecheck-theory-and-show-tccs', () => {
            if (vscode.window.activeTextEditor) {
                let fileName: string = vscode.window.activeTextEditor.document.fileName;
                if (fileName.endsWith(".pvs")) {
                    fileName = fileName.endsWith(".pvs") ? fileName.substr(0, fileName.length - 4) : fileName; // remove file extension
                    fileName = fileName.includes("/") ? fileName.split("/").slice(-1)[0] : fileName; // remove path

                    const text: string = vscode.window.activeTextEditor.document.getText();
                    const line: number = vscode.window.activeTextEditor.selection.active.line;
                    const theoryName: string = findTheoryName(text, line);
                    const formula: string = findTheorem(text, line);

                    const pvsTerminal: ProverTerminal = new ProverTerminal(pvsInterpreter);
                    pvsTerminal.showTccs(fileName, theoryName);
                } else {
                    vscode.window.showErrorMessage(fileName + " is not a valid pvs file :/");
                }
            }
        }));

    }
}