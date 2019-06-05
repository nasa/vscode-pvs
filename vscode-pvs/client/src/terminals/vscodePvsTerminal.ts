import * as vscode from 'vscode';
import * as path from 'path';
import { findTheoryName, findTheorem } from '../common/languageUtils';
import * as fs from '../common/fsUtils';
import { TextDocument, LanguageClient } from 'vscode-languageclient';
import { VSCodePvsExplorer, TheoryDescriptor } from '../views/vscodePvsTheoryExplorer';
import * as fsUtils from '../common/fsUtils';
import * as utils from '../utils/vscode-utils';
import { ProofDescriptor } from '../common/serverInterface';

function getPvsPath (): string {
    const mode: string = vscode.workspace.getConfiguration().get("pvs.zen-mode");
    if (mode === "pvs-6" || mode === "pvs-7") {
        return vscode.workspace.getConfiguration().get(`pvs.zen-mode:${mode}-path`);
    }
    return vscode.workspace.getConfiguration().get("pvs.path");
}

class ProverTerminal {
    pvsExecutable: string;
    pvsPath: string;
    pvsInterpreter: string;
    pvsContextFolder: string;
    terminal: vscode.Terminal;
    client: LanguageClient;
    private terminalID: string;
    private interactiveMode: string = null;
    constructor (client: LanguageClient, pvsInterpreter: string, terminalID: string) {
        this.terminalID = terminalID;
        this.pvsInterpreter = pvsInterpreter;
        this.pvsPath = getPvsPath();
        this.pvsExecutable = path.join(this.pvsPath, "pvs");
        this.pvsContextFolder = fs.getPathname(vscode.window.activeTextEditor.document.fileName);
        this.client = client;
    }
    private sendCommand(cmd: string): void {
        this.terminal.sendText(cmd);
    }
    private pvsLispTypecheckFile(fileName: string, opt?: {
        proveTccs: boolean,
        proveImportChain: boolean
    }) {
        // vscode.window.setStatusBarMessage("Typechecking " + fileName);
        let args: string = opt ? opt.proveTccs ? "nil t 'typecheck-prove" : "t t 'typecheck-prove-importchain"
                            : "nil nil nil";
        this.sendCommand('(typecheck-file "' + fileName + '" ' + args + ')');
        this.sendCommand('(save-context)');
    }
    private pvsLispProveFormula(theoryName: string, formulaName: string, line: number) {
        // prove-formula (formula-decl &optional theory-name proof)
        //(rerun-proof-at? "toy" nil 15 "pvs" nil nil)
        //(prove-formula "toy" "polynomialDomMonomCoeffOne" nil)
        // this.sendCommand('(prove-formula "' + theoryName + '" "' + formula +'" nil)');
        this.sendCommand(`(prove-file-at "${theoryName}" ${line} nil nil)`);
        // this.terminal.sendText('(rewrite-msg-off)');
    }
    private pvsLispProveTcc(theoryName: string, formula: string) {
        this.sendCommand('(prove-formula "' + theoryName + '" "' + formula +'" t)');
    }
    private pvsLispShowTccs(theoryName: string) {
        this.sendCommand('(show-tccs "' + theoryName + '" nil)');
    }
    stepCommand (cmd: string) {
        // TODO: place a semaphore
        this.interactiveMode = "step-executed";
        this.sendCommand(cmd);
    }
    private createPvsTerminal(terminalName: string, args: string[]): vscode.Terminal {
        const terminal: vscode.Terminal = vscode.window.createTerminal(terminalName, 'node', args);
        (<any>terminal).onDidWriteData((data: string) => {
            if (this.interactiveMode) {
                const PVS_COMINT_PROMPT_REGEXP: RegExp = /(.*)\s*pvs\(\d+\):|(.*)\[.+\]\s*pvs\(\d+\):/g; // capture group 1 is the pvs lisp output
                const PROVER_PROMPT: RegExp = /\bRule\?/g;
                if (PROVER_PROMPT.test(data)) {
                    vscode.commands.executeCommand(`terminal.pvs.response.${this.interactiveMode}`);
                    this.interactiveMode = null;
                }
            }
        });
        terminal.show();
        return terminal;
    }
    proveFormula (fileName: string, theoryName: string, formula: string, line: number) {
        if (fileName.endsWith(".pvs") || fileName.endsWith(".tccs")) {
            fileName = fsUtils.removeFileExtension(fileName);
        }
        const terminalName: string = `prove ${formula}`;
        const args: string[] = [ this.pvsInterpreter, this.pvsExecutable];
        this.terminal = this.createPvsTerminal(terminalName, args);
        this.interactiveMode = "step-proof-ready";
        this.pvsLispTypecheckFile(fileName);

        this.pvsLispProveFormula(theoryName, formula, line);
        return this;
    }
    proveTcc (fileName: string, theoryName: string, formula: string, line: number) {
        if (fileName.endsWith(".pvs") || fileName.endsWith(".tccs")) {
            fileName = fsUtils.removeFileExtension(fileName);
        }
        const terminalName: string = `prove ${formula}`;
        const args: string[] = [ this.pvsInterpreter, this.pvsExecutable];
        this.terminal = this.createPvsTerminal(terminalName, args);
        this.interactiveMode = "step-proof-ready";
        this.pvsLispTypecheckFile(fileName);

        this.pvsLispProveTcc(theoryName, formula);
        return this;
    }
    typecheck (fileName: string) {
        if (fileName.endsWith(".pvs") || fileName.endsWith(".tccs")) {
            fileName = fsUtils.removeFileExtension(fileName);
        }
        const terminalName: string = `typecheck ${fileName}`;
        const args: string[] = [ this.pvsInterpreter, this.pvsExecutable ];
        this.terminal = this.createPvsTerminal(terminalName, args);
        this.pvsLispTypecheckFile(fileName);
        return this;
    }
    typecheckProve (fileName: string) {
        if (fileName.endsWith(".pvs") || fileName.endsWith(".tccs")) {
            fileName = fsUtils.removeFileExtension(fileName);
        }
        const terminalName: string = `typecheck-prove ${fileName}`;
        const args: string[] = [ this.pvsInterpreter, this.pvsExecutable ];
        this.terminal = this.createPvsTerminal(terminalName, args);
        this.pvsLispTypecheckFile(fileName, {
            proveTccs: true,
            proveImportChain: false
        });
        return this;
    }
    showTccs (fileName: string, theoryName: string) {
        if (fileName.endsWith(".pvs") || fileName.endsWith(".tccs")) {
            fileName = fsUtils.removeFileExtension(fileName);
        }
        const terminalName: string = `show-tccs ${theoryName}`;
        const args: string[] = [ this.pvsInterpreter, this.pvsExecutable ];
        this.terminal = this.createPvsTerminal(terminalName, args);
        this.pvsLispTypecheckFile(fileName);
        this.pvsLispShowTccs(theoryName);
        return this;
    }
    // printMessage(msg: string) {
    //     this.terminal.sendText("echo '" + msg + "'");
    //     return this;
    // }
    async showProof(fileName: string, theoryName: string, formulaName: string, line: number) {
        if (fileName.endsWith(".pvs")) {
            fileName = fsUtils.removeFileExtension(fileName);
        }
        const data: ProofDescriptor = { fileName, theoryName, formulaName, line, fileExtension: ".pvs" };
        this.client.sendRequest('pvs.step-proof', data);
        return this;
    }
    async stepTcc(fileName: string, theoryName: string, formulaName: string, line: number) {
        if (fileName.endsWith(".tccs")) {
            fileName = fsUtils.removeFileExtension(fileName);
        }
        const data: ProofDescriptor = { fileName, theoryName, formulaName, line, fileExtension: ".tccs" };
        this.client.sendRequest('pvs.step-tcc', data);
        return this;
    }
}

export class VSCodePvsTerminal {
    private theoryExplorer: VSCodePvsExplorer;
    private client: LanguageClient;
    private activeTerminals: { [key: string]: ProverTerminal } = {};
    private context: vscode.ExtensionContext;
    constructor (client: LanguageClient, theoryExplorer: VSCodePvsExplorer) {
        this.theoryExplorer = theoryExplorer;
        this.client = client;
        vscode.window.onDidCloseTerminal((terminal) => {
            const keys: string[] = Object.keys(this.activeTerminals);
            if (keys && keys.length > 0) {
                for (const i in keys) {
                    if (this.activeTerminals[keys[i]].terminal.processId === terminal.processId) {
                        delete this.activeTerminals[keys[i]];
                        break;
                    }
                }
            }
        });
    }
    private findSelectedFormula(codelensLine?: number): { fileName: string, theoryName: string, formulaName: string, line: number, fileExtension: string } {
        const document = vscode.window.activeTextEditor.document;
        let fileName: string = document.fileName;
        const fileExtension: string = (document.fileName.endsWith(".pvs"))? ".pvs"
                                    : (document.fileName.endsWith(".tccs"))? ".tccs"
                                    : null;
        if (!fileExtension) {
            vscode.window.showErrorMessage(`${document.fileName} is not a valid pvs file :/`);
            return null;
        }
        const text: string = document.getText();
        const line: number = (codelensLine !== undefined && codelensLine !== null) ? (codelensLine + 1) : vscode.window.activeTextEditor.selection.active.line;
        let theoryName: string = null;
        let formulaName: string = null;
        if (fileName.endsWith(".tccs")) {
            // .tccs file
            theoryName = fs.getFilename(document.fileName, { removeFileExtension: true });
            if (theoryName) {
                const desc: TheoryDescriptor = this.theoryExplorer.getTheoryDescriptor(theoryName);
                fileName = (desc) ? desc.fileName : null;
                formulaName = findTheorem(text, line);
            }
        } else {
            // .pvs file
            theoryName = findTheoryName(text, line);
            if (theoryName) {
                fileName = fileName.split(".").slice(0, -1).join("."); // remove file extension
                fileName = (fileName.includes("/")) ? fileName.split("/").slice(-1)[0] : fileName; // remove path, if present
                formulaName = findTheorem(text, line);
            }
        }

        if (!theoryName) {
            vscode.window.showErrorMessage("Unable to find theory name :/");
            return null;
        }
        if (!formulaName) {
            vscode.window.showErrorMessage("Unable to find formula name :/");
            return null;
        } 
        return {
            fileName: fileName,
            theoryName: theoryName,
            formulaName: formulaName,
            line: line,
            fileExtension: fileExtension
        };
    }
    private info(msg: string) {
        // this.statusBar.info(msg);
        vscode.window.showInformationMessage(msg);
    }
    private ready() {
        // vscode.window.showInformationMessage("ready");
    }
    private error(msg: string) {
        vscode.window.showErrorMessage(msg);
    }

    // async prove (data: { 
    //     fileName: string, theoryName: string, formulaName: string, line: number, fileExtension: string 
    // }) {
    //     data = data || this.findSelectedFormula(data.line);
    //     if (data) {
    //         const terminalID: string = VSCodePvsTerminal.getTerminalID(data);
    //         const pvsInterpreter: string = this.context.asAbsolutePath(path.join('server', 'out', 'pvsInterpreter'));
    //         if (this.activeTerminals[terminalID]) {
    //             this.activeTerminals[terminalID].terminal.show();
    //         } else {
    //             // create new terminal
    //             this.info(`Starting new prover session for ${data.formulaName}`);
    //             const pvsTerminal: ProverTerminal = new ProverTerminal(this.client, pvsInterpreter, terminalID);
    //             this.activeTerminals[terminalID] = pvsTerminal;
    //             if (data.fileExtension === ".pvs") {
    //                 pvsTerminal.proveFormula(data.fileName, data.theoryName, data.formulaName, data.line);
    //             } else if (data.fileExtension === ".tccs") {
    //                 pvsTerminal.proveTcc(data.fileName, data.theoryName, data.formulaName, data.line);
    //             }
    //         }
    //         this.ready();
    //     }
    // }
    async stepProof (data: { 
        fileName: string, theoryName: string, formulaName: string, line: number, fileExtension: string 
    }) {
        data = (data) ? 
                (data.fileExtension === ".tccs") ?
                    this.findSelectedFormula(data.line) : data
                : this.findSelectedFormula();
        if (data) {
            const terminalID: string = VSCodePvsTerminal.getTerminalID(data);
            const pvsInterpreter: string = this.context.asAbsolutePath(path.join('server', 'out', 'pvsInterpreter'));
            if (this.activeTerminals[terminalID]) {
                this.activeTerminals[terminalID].terminal.show();
            } else {
                this.info(`Starting new prover session for ${data.formulaName}`);
                const pvsTerminal: ProverTerminal = new ProverTerminal(this.client, pvsInterpreter, terminalID);
                this.activeTerminals[terminalID] = pvsTerminal;
                if (data.fileExtension === ".pvs") {
                    pvsTerminal.showProof(data.fileName, data.theoryName, data.formulaName, data.line);
                    pvsTerminal.proveFormula(data.fileName, data.theoryName, data.formulaName, data.line);
                } else if (data.fileExtension === ".tccs") {
                    pvsTerminal.stepTcc(data.fileName, data.theoryName, data.formulaName, data.line);
                    pvsTerminal.proveTcc(data.fileName, data.theoryName, data.formulaName, data.line);
                }
            }
            this.ready();
        }
    }
    static getTerminalID(data: { fileName: string, formulaName?: string, theoryName?: string }): string {
        return `${fsUtils.getFilename(data.fileName, { removeFileExtension: true })}.${data.theoryName}.${data.formulaName}`;
    }
    activate (context: vscode.ExtensionContext) {
        this.context = context;

        // register handlers
        context.subscriptions.push(vscode.commands.registerCommand('terminal.pvs.prove', async (data: {
            fileName: string, theoryName: string, formulaName: string, line: number, fileExtension: string
        }) => {
            this.stepProof(data);
        }));
        context.subscriptions.push(vscode.commands.registerCommand('codelense.pvs.step-proof', async (data: { 
            fileName: string, theoryName: string, formulaName: string, line: number, fileExtension: string 
        }) => {
            this.stepProof(data);
        }));
        context.subscriptions.push(vscode.commands.registerCommand('terminal.ctrl-c', async () => {
            // const term: ProverTerminal = this.activeTerminals[0];
            // term.terminal.sendText(String.fromCharCode('C'.charCodeAt(0) - 64), false)
        }));

        // context.subscriptions.push(vscode.commands.registerCommand('terminal.pvs.step-proof', async (data: { 
        //     fileName: string, theoryName: string, formulaName: string, line: number, fileExtension: string 
        // }) => {
        //     data = data || this.findSelectedFormula();
        //     this.stepProof(data);
        // }));
        // context.subscriptions.push(vscode.commands.registerCommand('codelense.pvs.step-proof', async (data: { 
        //     fileName: string, theoryName: string, formulaName: string, line: number, fileExtension: string 
        // }) => {
        //     this.stepProof(data);
        // }));

        // context.subscriptions.push(vscode.commands.registerCommand('terminal.pvs.show-proof', async () => {
        //     const data: { 
        //         fileName: string, theoryName: string, formulaName: string, line: number, fileExtension: string 
        //     } = this.findSelectedFormula();
        //     this.stepProof(data);
        // }));
        // context.subscriptions.push(vscode.commands.registerCommand('codelense.pvs.show-proof', async (data: { 
        //     fileName: string, theoryName: string, formulaName: string, line: number, fileExtension: string 
        // }) => {
        //     this.stepProof(data);
        // }));

        context.subscriptions.push(vscode.commands.registerCommand('terminal.pvs.send-proof-command', async (data: {
            fileName: string, theoryName: string, formulaName: string, line: number, fileExtension: string, cmd: string
        }) => {
            if (data && data.cmd && vscode.window.activeTextEditor && fs.isPvsFile(vscode.window.activeTextEditor.document.fileName)) {
                const terminalID: string = VSCodePvsTerminal.getTerminalID(data);
                if (!this.activeTerminals[terminalID]) {
                    // identify the pvs interpreter
                    const pvsInterpreter: string = this.context.asAbsolutePath(path.join('server', 'out', 'pvsInterpreter'));
                    this.activeTerminals[terminalID] = new ProverTerminal(this.client, pvsInterpreter, terminalID);
                    const pvsTerminal = this.activeTerminals[terminalID];
                    if (data.fileExtension === ".pvs") {
                        pvsTerminal.showProof(data.fileName, data.theoryName, data.formulaName, data.line);
                        pvsTerminal.proveFormula(data.fileName, data.theoryName, data.formulaName, data.line);
                    } else if (data.fileExtension === ".tccs") {
                        pvsTerminal.showProof(data.fileName, data.theoryName, data.formulaName, data.line);
                        pvsTerminal.proveTcc(data.fileName, data.theoryName, data.formulaName, data.line);
                    }
                }
                // this.info(`Executing step ${data.cmd}`);
                await this.activeTerminals[terminalID].stepCommand(data.cmd);
                this.ready();
            }
        }));

        context.subscriptions.push(vscode.commands.registerCommand('terminal.pvs.typecheck', () => {
            if (vscode.window.activeTextEditor) {
                let fileName: string = vscode.window.activeTextEditor.document.fileName;
                fileName = fileName.split(".").slice(0, -1).join("."); // remove file extension
                fileName = fileName.split("/").slice(-1)[0]; // remove path

                this.info(`Typechecking ${fileName}`);
                const terminalID: string = VSCodePvsTerminal.getTerminalID({ fileName });
                // identify the pvs interpreter
                const pvsInterpreter: string = this.context.asAbsolutePath(path.join('server', 'out', 'pvsInterpreter'));
                const pvsTerminal: ProverTerminal = new ProverTerminal(this.client, pvsInterpreter, terminalID);
                pvsTerminal.typecheck(fileName);
                this.ready();
            }
        }));
        context.subscriptions.push(vscode.commands.registerCommand('terminal.pvs.typecheck-prove', () => {
            if (vscode.window.activeTextEditor) {
                let fileName: string = vscode.window.activeTextEditor.document.fileName;
                fileName = fileName.split(".").slice(0, -1).join("."); // remove file extension
                fileName = fileName.split("/").slice(-1)[0]; // remove path

                this.info(`Running typecheck-prove for ${fileName}`);
                const terminalID: string = VSCodePvsTerminal.getTerminalID({ fileName });
                // identify the pvs interpreter
                const pvsInterpreter: string = this.context.asAbsolutePath(path.join('server', 'out', 'pvsInterpreter'));
                const pvsTerminal: ProverTerminal = new ProverTerminal(this.client, pvsInterpreter, terminalID);
                pvsTerminal.typecheckProve(fileName);
                this.ready();
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
                    // const formula: string = findTheorem(text, line);

                    this.info(`Typechecking ${fileName}`);
                    const terminalID: string = VSCodePvsTerminal.getTerminalID({ fileName });
                    // identify the pvs interpreter
                    const pvsInterpreter: string = this.context.asAbsolutePath(path.join('server', 'out', 'pvsInterpreter'));
                    const pvsTerminal: ProverTerminal = new ProverTerminal(this.client, pvsInterpreter, terminalID);
                    pvsTerminal.showTccs(fileName, theoryName);
                    this.ready();
                } else {
                    this.error(fileName + " is not a valid pvs file :/");
                }
            }
        }));
    }
}