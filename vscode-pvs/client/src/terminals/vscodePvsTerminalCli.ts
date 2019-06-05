import * as vscode from 'vscode';
import * as path from 'path';
import { findTheoryName, findTheorem } from '../common/languageUtils';
import * as fs from '../common/fsUtils';
import { TextDocument, LanguageClient } from 'vscode-languageclient';
import { VSCodePvsExplorer, TheoryDescriptor } from '../views/vscodePvsTheoryExplorer';
import * as fsUtils from '../common/fsUtils';
import * as utils from '../utils/vscode-utils';
import { PvsCliInterface, ProofDescriptor } from '../common/serverInterface';
import { PVS_CLI_FILE } from '../common/serverInterface';

// TODO: move this function to vscode-utils.ts
function getPvsPath (): string {
    const mode: string = vscode.workspace.getConfiguration().get("pvs.zen-mode");
    if (mode === "pvs-6" || mode === "pvs-7") {
        return vscode.workspace.getConfiguration().get(`pvs.zen-mode:${mode}-path`);
    }
    return vscode.workspace.getConfiguration().get("pvs.path");
}

class ProverTerminal {
    pvsPath: string;
    pvsCliFileName: string; // PvsCli file
    pvsContextFolder: string;
    terminal: vscode.Terminal;
    client: LanguageClient;
    terminalID: string;
    private interactiveCommand: string = null;
    constructor (client: LanguageClient, pvsCliFileName: string, context: vscode.ExtensionContext, terminalID: string) {
        this.pvsCliFileName = pvsCliFileName;
        this.pvsPath = getPvsPath();
        this.pvsContextFolder = fs.getPathname(vscode.window.activeTextEditor.document.fileName);
        this.client = client;
        this.terminalID = terminalID;
    }
    private createPvsTerminal(terminalName: string, args: PvsCliInterface): vscode.Terminal {
        const terminal: vscode.Terminal = vscode.window.createTerminal(terminalName, 'node', [ this.pvsCliFileName, JSON.stringify(args) ]);
        (<any>terminal).onDidWriteData((data: string) => {
            if (this.interactiveCommand) {
                const PVS_COMINT_PROMPT_REGEXP: RegExp = /(.*)\s*pvs\(\d+\):|(.*)\[.+\]\s*pvs\(\d+\):/g; // capture group 1 is the pvs lisp output
                const PROVER_PROMPT: RegExp = /\bRule\?/g;
                if (PROVER_PROMPT.test(data)) {
                    vscode.commands.executeCommand(`terminal.pvs.response.${this.interactiveCommand}`);
                    this.interactiveCommand = null;
                }
            }
        });
        terminal.show();
        return terminal;
    }
    private sendCommand(cmd: string): void {
        this.terminal.sendText(cmd);
    }
    stepCommand (cmd: string) {
        this.interactiveCommand = "step-executed";
        this.sendCommand(cmd);
    }
    proveFormula (fileName: string, theoryName: string, formulaName: string, line: number) {
        if (fileName.endsWith(".pvs")) {
            fileName = fsUtils.removeFileExtension(fileName);
        }
        const terminalName: string = `${formulaName}`;
        this.terminal = this.createPvsTerminal(terminalName, {
            pvsPath: this.pvsPath, pvsContextFolder: this.pvsContextFolder, cmd: 'prove-formula', 
            fileName, theoryName, formulaName, line, fileExtension: ".pvs" });
        this.interactiveCommand = "step-proof-ready";

        // this.sendCommand(`(typecheck-file "${fileName}" nil nil nil)`);
        // this.sendCommand('(prove-formula "' + theoryName + '" "' + formulaName +'" nil)');

        return this;
    }
    proveTcc (fileName: string, theoryName: string, formulaName: string, line: number) {
        if (fileName.endsWith(".tccs")) {
            fileName = fsUtils.removeFileExtension(fileName);
        }
        this.terminal = this.createPvsTerminal(formulaName, {
            pvsPath: this.pvsPath, pvsContextFolder: this.pvsContextFolder, cmd: 'prove-formula', 
            fileName, theoryName, formulaName, fileExtension: ".tccs", line });
        this.interactiveCommand = "step-proof-ready";

        // this.sendCommand('(typecheck-file "' + fileName + '" nil nil nil)');
        // this.sendCommand('(prove-formula "' + theoryName + '" "' + formulaName +'" t)');
        return this;
    }
    // typecheck (fileName: string) {
    //     const fileExtension: string = fileName.endsWith(".pvs") ? ".pvs" : 
    //                                     fileName.endsWith(".tccs") ? ".tccs"
    //                                     : null;
    //     if (fileName.endsWith(".pvs") || fileName.endsWith(".tccs")) {
    //         fileName = fsUtils.removeFileExtension(fileName);
    //     }
    //     const terminalName: string = `typecheck ${fileName}`;
    //     this.terminal = this.createPvsTerminal(terminalName, { 
    //         pvsPath: this.pvsPath, pvsContextFolder: this.pvsContextFolder, cmd: 'typecheck-file',
    //         fileName, fileExtension });

    //     // this.sendCommand('(typecheck-file "' + fileName + '" nil nil nil)');
    //     return this;
    // }
    // typecheckProve (fileName: string) {
    //     const fileExtension: string = fileName.endsWith(".pvs") ? ".pvs" : 
    //                                     fileName.endsWith(".tccs") ? ".tccs"
    //                                     : null;
    //     if (fileName.endsWith(".pvs") || fileName.endsWith(".tccs")) {
    //         fileName = fsUtils.removeFileExtension(fileName);
    //     }
    //     const terminalName: string = `typecheck-prove ${fileName}`;
    //     this.terminal = this.createPvsTerminal(terminalName, { 
    //         pvsPath: this.pvsPath, pvsContextFolder: this.pvsContextFolder, cmd: 'typecheck-file', 
    //         fileName, fileExtension });

    //     // this.sendCommand(`(typecheck-file "${fileName}" nil t 'typecheck-prove)`);
    //     return this;
    // }
    // showTccs (fileName: string, theoryName: string) {
    //     const fileExtension: string = fileName.endsWith(".pvs") ? ".pvs" : 
    //                                     fileName.endsWith(".tccs") ? ".tccs"
    //                                     : null;
    //     if (fileName.endsWith(".pvs") || fileName.endsWith(".tccs")) {
    //         fileName = fsUtils.removeFileExtension(fileName);
    //     }
    //     const terminalName: string = `show-tccs ${theoryName}`;
    //     this.terminal = this.createPvsTerminal(terminalName, { 
    //         pvsPath: this.pvsPath, pvsContextFolder: this.pvsContextFolder, cmd: 'show-tccs', 
    //         fileName, fileExtension, theoryName });

    //     // this.sendCommand('(typecheck-file "' + fileName + '" nil nil nil)');
    //     // this.sendCommand('(show-tccs "' + theoryName + '" nil)');
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
    // // printMessage(msg: string) {
    // //     this.terminal.sendText("echo '" + msg + "'");
    // //     return this;
    // // }
    // async stepProof(fileName: string, theoryName: string, formulaName: string, line: number) {
    //     if (fileName.endsWith(".pvs")) {
    //         fileName = fsUtils.removeFileExtension(fileName);
    //     }
    //     this.client.sendRequest('pvs.step-proof', { fileName, theoryName, formulaName, line });
    //     return this;
    // }
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
    private context: vscode.ExtensionContext;
    private activeTerminals: { [key: string]: ProverTerminal } = {};
    private pvsCli: string;
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
    // TODO: move some of these functionalities to PvsProcess / PvsServer
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
    static getTerminalID(data: { fileName: string, formulaName?: string, theoryName?: string }): string {
        return `${fsUtils.getFilename(data.fileName, { removeFileExtension: true })}.${data.theoryName}.${data.formulaName}`;
    }
    async stepProof (data: { 
        fileName: string, theoryName: string, formulaName: string, line: number, fileExtension: string 
    }) {
        data = (data) ? 
                (data.fileExtension === ".tccs") ?
                    this.findSelectedFormula(data.line) : data
                : this.findSelectedFormula();
        if (data) {
            const terminalID: string = VSCodePvsTerminal.getTerminalID(data);
            if (this.activeTerminals[terminalID]) {
                this.activeTerminals[terminalID].terminal.show();
            } else {
                this.info(`Starting new prover session for ${data.formulaName}`);
                const pvsTerminal: ProverTerminal = new ProverTerminal(this.client, this.pvsCli, this.context, terminalID);
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

    // install handlers
    activate (context: vscode.ExtensionContext) {
        this.context = context;

        // identify the pvs interpreter
        this.pvsCli = context.asAbsolutePath(path.join('server', 'out', PVS_CLI_FILE));

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

        context.subscriptions.push(vscode.commands.registerCommand('terminal.pvs.send-proof-command', async (data: {
            fileName: string, theoryName: string, formulaName: string, line: number, fileExtension: string, cmd: string
        }) => {
            if (data && data.cmd && vscode.window.activeTextEditor && fs.isPvsFile(vscode.window.activeTextEditor.document.fileName)) {
                const terminalID: string = VSCodePvsTerminal.getTerminalID(data);
                if (!this.activeTerminals[terminalID]) {
                    this.activeTerminals[terminalID] = new ProverTerminal(this.client, this.pvsCli, this.context, terminalID);
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

        // OBSOLETE?
        // context.subscriptions.push(vscode.commands.registerCommand('terminal.pvs.typecheck', () => {
        //     if (vscode.window.activeTextEditor) {
        //         let fileName: string = vscode.window.activeTextEditor.document.fileName;
        //         fileName = fileName.split(".").slice(0, -1).join("."); // remove file extension
        //         fileName = fileName.split("/").slice(-1)[0]; // remove path

        //         this.info(`Typechecking ${fileName}`);
        //         const terminalID: string = VSCodePvsTerminal.getTerminalID({ fileName });
        //         const pvsTerminal: ProverTerminal = new ProverTerminal(this.client, this.pvsCli, this.context, terminalID);
        //         pvsTerminal.typecheck(fileName);
        //         this.ready();
        //     }
        // }));
        // context.subscriptions.push(vscode.commands.registerCommand('terminal.pvs.typecheck-prove', () => {
        //     if (vscode.window.activeTextEditor) {
        //         let fileName: string = vscode.window.activeTextEditor.document.fileName;
        //         fileName = fileName.split(".").slice(0, -1).join("."); // remove file extension
        //         fileName = fileName.split("/").slice(-1)[0]; // remove path

        //         this.info(`Running typecheck-prove for ${fileName}`);
        //         const terminalID: string = VSCodePvsTerminal.getTerminalID({ fileName });
        //         const pvsTerminal: ProverTerminal = new ProverTerminal(this.client, this.pvsCli, this.context, terminalID);
        //         pvsTerminal.typecheckProve(fileName);
        //         this.ready();
        //     }
        // }));
        // context.subscriptions.push(vscode.commands.registerCommand('terminal.pvs.typecheck-theory-and-show-tccs', () => {
        //     if (vscode.window.activeTextEditor) {
        //         let fileName: string = vscode.window.activeTextEditor.document.fileName;
        //         if (fileName.endsWith(".pvs")) {
        //             fileName = fileName.endsWith(".pvs") ? fileName.substr(0, fileName.length - 4) : fileName; // remove file extension
        //             fileName = fileName.includes("/") ? fileName.split("/").slice(-1)[0] : fileName; // remove path

        //             const text: string = vscode.window.activeTextEditor.document.getText();
        //             const line: number = vscode.window.activeTextEditor.selection.active.line;
        //             const theoryName: string = findTheoryName(text, line);
        //             // const formula: string = findTheorem(text, line);

        //             this.info(`Typechecking ${fileName}`);
        //             const terminalID: string = VSCodePvsTerminal.getTerminalID({ fileName });
        //             const pvsTerminal: ProverTerminal = new ProverTerminal(this.client, this.pvsCli, this.context, terminalID);
        //             pvsTerminal.showTccs(fileName, theoryName);
        //             this.ready();
        //         } else {
        //             this.error(fileName + " is not a valid pvs file :/");
        //         }
        //     }
        // }));

        // older commands

        // context.subscriptions.push(vscode.commands.registerCommand('terminal.pvs.prove', async () => {
        //     if (vscode.window.activeTextEditor && fs.isPvsFile(vscode.window.activeTextEditor.document.fileName)) {
        //         const data: { 
        //             fileName: string, theoryName: string, formulaName: string, line: number, fileExtension: string 
        //         } = this.findSelectedFormula();
        //         if (data) {
        //             this.info(`Starting new prover session for ${data.formulaName}`);
        //             const pvsTerminal: ProverTerminal = new ProverTerminal(this.client, pvsCli, context);
        //             if (data.fileExtension === ".pvs") {
        //                 pvsTerminal.proveFormula(data.fileName, data.theoryName, data.formulaName, data.line);
        //             } else if (data.fileExtension === ".tccs") {
        //                 pvsTerminal.proveTcc(data.fileName, data.theoryName, data.formulaName, data.line);
        //             }
        //             this.ready();
        //         }
        //     }
        // }));
        // context.subscriptions.push(vscode.commands.registerCommand('codelense.pvs.prove', async (data: { 
        //     fileName: string, theoryName: string, formulaName: string, line: number, fileExtension: string 
        // }) => {
        //     if (data) {
        //         // adjust data for codelense, and find formula name in the case of .tccs files
        //         data = this.findSelectedFormula(data.line);
        //         this.info(`Starting new prover session for ${data.formulaName}`);
        //         const pvsTerminal: ProverTerminal = new ProverTerminal(this.client, pvsCli, context);
        //         if (data.fileExtension === ".pvs") {
        //             pvsTerminal.proveFormula(data.fileName, data.theoryName, data.formulaName, data.line);
        //         } else if (data.fileExtension === ".tccs") {
        //             pvsTerminal.proveTcc(data.fileName, data.theoryName, data.formulaName, data.line);
        //         }
        //         this.ready();
        //     }
        // }));

        // context.subscriptions.push(vscode.commands.registerCommand('terminal.pvs.step-proof', async () => {
        //     if (vscode.window.activeTextEditor && fs.isPvsFile(vscode.window.activeTextEditor.document.fileName)) {
        //         const data: { 
        //             fileName: string, theoryName: string, formulaName: string, line: number, fileExtension: string 
        //         } = this.findSelectedFormula();
        //         if (data) {
        //             this.info(`Starting new prover session for ${data.formulaName}`);
        //             const terminalID: string = VSCodePvsTerminal.getTerminalID("", data);
        //             this.activeTerminals[terminalID] = (this.activeTerminals[terminalID]) ? this.activeTerminals[terminalID] 
        //                                                     : new ProverTerminal(this.client, pvsCli, context);
        //             const pvsTerminal = this.activeTerminals[terminalID];
        //             if (data.fileExtension === ".pvs") {
        //                 pvsTerminal.proveFormula(data.fileName, data.theoryName, data.formulaName, data.line); // this is the interactive terminal
        //                 pvsTerminal.stepProof(data.fileName, data.theoryName, data.formulaName, data.line); // this is the internal process used for explorer view
        //             } else if (data.fileExtension === ".tccs") {
        //                 pvsTerminal.proveTcc(data.fileName, data.theoryName, data.formulaName, data.line);
        //                 pvsTerminal.stepTcc(data.fileName, data.theoryName, data.formulaName, data.line);
        //             }
        //             this.ready();
        //         }
        //     }
        // }));
        // context.subscriptions.push(vscode.commands.registerCommand('codelense.pvs.step-proof', async (data: { 
        //     fileName: string, theoryName: string, formulaName: string, line: number, fileExtension: string 
        // }) => {
        //     if (data) {
        //         // adjust data for codelense, and find formula name in the case of .tccs files
        //         data = this.findSelectedFormula(data.line);
        //         this.info(`Starting new prover session for ${data.formulaName}`);
        //         const terminalID: string = VSCodePvsTerminal.getTerminalID("", data);
        //         this.activeTerminals[terminalID] = (this.activeTerminals[terminalID]) ? this.activeTerminals[terminalID] 
        //                                                 : new ProverTerminal(this.client, pvsCli, context);
        //         const pvsTerminal = this.activeTerminals[terminalID];
        //         if (data.fileExtension === ".pvs") {
        //             pvsTerminal.proveFormula(data.fileName, data.theoryName, data.formulaName, data.line);
        //             pvsTerminal.stepProof(data.fileName, data.theoryName, data.formulaName, data.line);
        //         } else if (data.fileExtension === ".tccs") {
        //             pvsTerminal.proveTcc(data.fileName, data.theoryName, data.formulaName, data.line);
        //             pvsTerminal.stepTcc(data.fileName, data.theoryName, data.formulaName, data.line);
        //         }
        //         this.ready();
        //     }
        // }));

        // context.subscriptions.push(vscode.commands.registerCommand('terminal.pvs.show-proof', async () => {
        //     if (vscode.window.activeTextEditor && fs.isPvsFile(vscode.window.activeTextEditor.document.fileName)) {
        //         const data: { 
        //             fileName: string, theoryName: string, formulaName: string, line: number, fileExtension: string 
        //         } = this.findSelectedFormula();
        //         if (data) {
        //             this.info(`Starting new prover session for ${data.formulaName}`);
        //             const terminalID: string = VSCodePvsTerminal.getTerminalID("", data);
        //             this.activeTerminals[terminalID] = (this.activeTerminals[terminalID]) ? this.activeTerminals[terminalID] 
        //                                                     : new ProverTerminal(this.client, pvsCli, context);
        //             const pvsTerminal = this.activeTerminals[terminalID];
        //             if (data.fileExtension === ".pvs") {
        //                 pvsTerminal.proveFormula(data.fileName, data.theoryName, data.formulaName, data.line);
        //                 pvsTerminal.stepProof(data.fileName, data.theoryName, data.formulaName, data.line);
        //             } else if (data.fileExtension === ".tccs") {
        //                 pvsTerminal.proveTcc(data.fileName, data.theoryName, data.formulaName, data.line);
        //                 pvsTerminal.stepTcc(data.fileName, data.theoryName, data.formulaName, data.line);
        //             }
        //             this.ready();
        //         }
        //     }
        // }));
        // context.subscriptions.push(vscode.commands.registerCommand('codelense.pvs.show-proof', async (data: { 
        //     fileName: string, theoryName: string, formulaName: string, line: number, fileExtension: string 
        // }) => {
        //     if (data) {
        //         // adjust data for codelense, and find formula name in the case of .tccs files
        //         data = this.findSelectedFormula(data.line);
        //         this.info(`Starting new prover session for ${data.formulaName}`);
        //         const terminalID: string = VSCodePvsTerminal.getTerminalID("", data);
        //         this.activeTerminals[terminalID] = (this.activeTerminals[terminalID]) ? this.activeTerminals[terminalID] 
        //                                                 : new ProverTerminal(this.client, pvsCli, context);
        //         const pvsTerminal = this.activeTerminals[terminalID];
        //         if (data.fileExtension === ".pvs") {
        //             pvsTerminal.proveFormula(data.fileName, data.theoryName, data.formulaName, data.line);
        //             pvsTerminal.stepProof(data.fileName, data.theoryName, data.formulaName, data.line);
        //         } else if (data.fileExtension === ".tccs") {
        //             pvsTerminal.proveTcc(data.fileName, data.theoryName, data.formulaName, data.line);
        //             pvsTerminal.stepTcc(data.fileName, data.theoryName, data.formulaName, data.line);
        //         }
        //         this.ready();
        //     }
        // }));

        // context.subscriptions.push(vscode.commands.registerCommand('terminal.pvs.send-proof-command', async (data: {
        //     fileName: string, theoryName: string, formulaName: string, line: number, fileExtension: string, cmd: string
        // }) => {
        //     if (data && data.cmd && vscode.window.activeTextEditor && fs.isPvsFile(vscode.window.activeTextEditor.document.fileName)) {
        //         const terminalID: string = VSCodePvsTerminal.getTerminalID("", data);
        //         if (!this.activeTerminals[terminalID]) {
        //             this.activeTerminals[terminalID] = new ProverTerminal(this.client, pvsCli, context);
        //             const pvsTerminal = this.activeTerminals[terminalID];
        //             if (data.fileExtension === ".pvs") {
        //                 pvsTerminal.proveFormula(data.fileName, data.theoryName, data.formulaName, data.line);
        //                 pvsTerminal.stepProof(data.fileName, data.theoryName, data.formulaName, data.line);
        //             } else if (data.fileExtension === ".tccs") {
        //                 pvsTerminal.proveTcc(data.fileName, data.theoryName, data.formulaName, data.line);
        //                 pvsTerminal.stepProof(data.fileName, data.theoryName, data.formulaName, data.line);
        //             }
        //         }
        //         // this.info(`Executing step ${data.cmd}`);
        //         await this.activeTerminals[terminalID].stepCommand(data.cmd);
        //         this.ready();
        //     }
        // }));

        // context.subscriptions.push(vscode.commands.registerCommand('terminal.pvs.typecheck', () => {
        //     if (vscode.window.activeTextEditor) {
        //         let fileName: string = vscode.window.activeTextEditor.document.fileName;
        //         fileName = fileName.split(".").slice(0, -1).join("."); // remove file extension
        //         fileName = fileName.split("/").slice(-1)[0]; // remove path

        //         this.info(`Typechecking ${fileName}`);
        //         const pvsTerminal: ProverTerminal = new ProverTerminal(this.client, pvsCli, context);
        //         pvsTerminal.typecheck(fileName);
        //         this.ready();
        //     }
        // }));
        // context.subscriptions.push(vscode.commands.registerCommand('terminal.pvs.typecheck-prove', () => {
        //     if (vscode.window.activeTextEditor) {
        //         let fileName: string = vscode.window.activeTextEditor.document.fileName;
        //         fileName = fileName.split(".").slice(0, -1).join("."); // remove file extension
        //         fileName = fileName.split("/").slice(-1)[0]; // remove path

        //         this.info(`Running typecheck-prove for ${fileName}`);
        //         const pvsTerminal: ProverTerminal = new ProverTerminal(this.client, pvsCli, context);
        //         pvsTerminal.typecheckProve(fileName);
        //         this.ready();
        //     }
        // }));
        // context.subscriptions.push(vscode.commands.registerCommand('terminal.pvs.typecheck-theory-and-show-tccs', () => {
        //     if (vscode.window.activeTextEditor) {
        //         let fileName: string = vscode.window.activeTextEditor.document.fileName;
        //         if (fileName.endsWith(".pvs")) {
        //             fileName = fileName.endsWith(".pvs") ? fileName.substr(0, fileName.length - 4) : fileName; // remove file extension
        //             fileName = fileName.includes("/") ? fileName.split("/").slice(-1)[0] : fileName; // remove path

        //             const text: string = vscode.window.activeTextEditor.document.getText();
        //             const line: number = vscode.window.activeTextEditor.selection.active.line;
        //             const theoryName: string = findTheoryName(text, line);
        //             // const formula: string = findTheorem(text, line);

        //             this.info(`Typechecking ${fileName}`);
        //             const pvsTerminal: ProverTerminal = new ProverTerminal(this.client, pvsCli, context);
        //             pvsTerminal.showTccs(fileName, theoryName);
        //             this.ready();
        //         } else {
        //             this.error(fileName + " is not a valid pvs file :/");
        //         }
        //     }
        // }));
    }
}