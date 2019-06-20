/**
 * @module vscodePvsTerminalCLI
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

import * as vscode from 'vscode';
import * as path from 'path';
import { findTheoryName, findTheorem } from '../common/languageUtils';
import { TextDocument, LanguageClient } from 'vscode-languageclient';
import { VSCodePvsTheoryExplorer } from '../views/vscodePvsTheoryExplorer';
import * as fsUtils from '../common/fsUtils';
import * as utils from '../utils/vscode-utils';
import { PvsCliInterface, ProofDescriptor, TheoryDescriptor } from '../common/serverInterface';
import { PVS_CLI_FILE } from '../common/serverInterface';

// TODO: move this function to vscode-utils.ts
function getPvsPath (): string {
    return vscode.workspace.getConfiguration().get("pvs.path");
}

class ProverTerminalCLI {
    pvsPath: string;
    pvsCliFileName: string; // PvsCli file
    terminal: vscode.Terminal;
    client: LanguageClient;
    private active: boolean = true;
    terminalID: string;
    private interactiveCommand: string = null;
    constructor (client: LanguageClient, pvsCliFileName: string, context: vscode.ExtensionContext, terminalID: string) {
        this.pvsCliFileName = pvsCliFileName;
        this.pvsPath = getPvsPath();
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
                const regex: RegExp = /:end-pvs-loc\b/;
                if (regex.test(data)) {
                    this.active = false;
                }
            }
        });
        terminal.show();
        return terminal;
    }
    private sendCommand(cmd: string): void {
        this.terminal.sendText(cmd);
    }
    isActive() {
        return this.active;
    }
    stepCommand (cmd: string) {
        this.sendCommand(cmd);
    }
    proveFormula (desc: { fileName: string, theoryName: string, formulaName: string, line: number, pvsContextFolder: string }) {
        if (desc.fileName.endsWith(".pvs")) {
            desc.fileName = fsUtils.removeFileExtension(desc.fileName);
        }
        const terminalName: string = `${desc.formulaName}`;
        const args: PvsCliInterface = {
            pvsPath: this.pvsPath, 
            pvsContextFolder: desc.pvsContextFolder, 
            cmd: 'prove-formula', 
            fileName: desc.fileName, 
            theoryName: desc.theoryName, 
            formulaName: desc.formulaName, 
            line: desc.line, 
            fileExtension: ".pvs"
        };
        this.terminal = this.createPvsTerminal(terminalName, args);
        // this.interactiveCommand = "step-proof-ready";
        return this;
    }
    proveTcc (desc: { fileName: string, theoryName: string, formulaName: string, line: number, pvsContextFolder: string }) {
        if (desc.fileName.endsWith(".tccs")) {
            desc.fileName = fsUtils.removeFileExtension(desc.fileName);
        }
        this.terminal = this.createPvsTerminal(desc.formulaName, {
            pvsPath: this.pvsPath, pvsContextFolder: desc.pvsContextFolder, cmd: 'prove-formula', 
            fileName: desc.fileName, theoryName: desc.theoryName, formulaName: desc.formulaName, fileExtension: ".tccs", line: desc.line });
        // this.interactiveCommand = "step-proof-ready";
        return this;
    }
    async showProof(desc: { fileName: string, theoryName: string, formulaName: string, line: number, pvsContextFolder: string }) {
        if (desc.fileName.endsWith(".pvs")) {
            desc.fileName = fsUtils.removeFileExtension(desc.fileName);
        }
        const data: ProofDescriptor = { fileName: desc.fileName, theoryName: desc.theoryName, formulaName: desc.formulaName, line: desc.line, fileExtension: ".pvs", pvsContextFolder: desc.pvsContextFolder };
        this.client.sendRequest('pvs.step-proof', data);
        return this;
    }
    async stepTcc(desc: { fileName: string, theoryName: string, formulaName: string, line: number, pvsContextFolder: string }) {
        if (desc.fileName.endsWith(".tccs")) {
            desc.fileName = fsUtils.removeFileExtension(desc.fileName);
        }
        const data: ProofDescriptor = { fileName: desc.fileName, theoryName: desc.theoryName, formulaName: desc.formulaName, line: desc.line, fileExtension: ".tccs", pvsContextFolder: desc.pvsContextFolder };
        this.client.sendRequest('pvs.step-tcc', data);
        return this;
    }
}

export class VSCodePvsTerminal {
    private theoryExplorer: VSCodePvsTheoryExplorer;
    private client: LanguageClient;
    private context: vscode.ExtensionContext;
    private activeTerminals: { [key: string]: ProverTerminalCLI } = {};
    private pvsCli: string;
    constructor (client: LanguageClient, theoryExplorer: VSCodePvsTheoryExplorer) {
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
    private findSelectedFormula(codelensLine?: number): { fileName: string, theoryName: string, formulaName: string, line: number, fileExtension: string, pvsContextFolder: string } {
        const document = vscode.window.activeTextEditor.document;
        let fileName: string = document.fileName;
        const pvsContextFolder: string = fsUtils.getContextFolder(document.fileName);
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
            theoryName = fsUtils.getFilename(document.fileName, { removeFileExtension: true });
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
        return { fileName, theoryName, formulaName, line, fileExtension, pvsContextFolder };
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
        fileName: string, theoryName: string, formulaName: string, line: number, fileExtension: string, pvsContextFolder: string 
    }) {
        data = (data) ? 
                (data.fileExtension === ".tccs") ?
                    this.findSelectedFormula(data.line) : data
                : this.findSelectedFormula();
        if (data) {
            const terminalID: string = VSCodePvsTerminal.getTerminalID(data);
            if (this.activeTerminals[terminalID] && this.activeTerminals[terminalID].isActive()) {
                this.activeTerminals[terminalID].terminal.show();
            } else {
                this.info(`Starting new prover session for ${data.formulaName}`);
                const pvsTerminal: ProverTerminalCLI = new ProverTerminalCLI(this.client, this.pvsCli, this.context, terminalID);
                this.activeTerminals[terminalID] = pvsTerminal;
                if (data.fileExtension === ".pvs") {
                    pvsTerminal.showProof(data);
                    pvsTerminal.proveFormula(data);
                } else if (data.fileExtension === ".tccs") {
                    pvsTerminal.stepTcc(data);
                    pvsTerminal.proveTcc(data);
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
            fileName: string, theoryName: string, formulaName: string, line: number, fileExtension: string, pvsContextFolder: string
        }) => {
            this.stepProof(data);
        }));
        context.subscriptions.push(vscode.commands.registerCommand('codelense.pvs.step-proof', async (data: { 
            fileName: string, theoryName: string, formulaName: string, line: number, fileExtension: string, pvsContextFolder: string 
        }) => {
            this.stepProof(data);
        }));

        context.subscriptions.push(vscode.commands.registerCommand('terminal.pvs.send-proof-command', async (data: {
            fileName: string, theoryName: string, formulaName: string, line: number, fileExtension: string, cmd: string, pvsContextFolder: string
        }) => {
            if (data && data.cmd && vscode.window.activeTextEditor && fsUtils.isPvsFile(vscode.window.activeTextEditor.document.fileName)) {
                const terminalID: string = VSCodePvsTerminal.getTerminalID(data);
                if (!this.activeTerminals[terminalID]) {
                    this.activeTerminals[terminalID] = new ProverTerminalCLI(this.client, this.pvsCli, this.context, terminalID);
                    const pvsTerminal = this.activeTerminals[terminalID];
                    if (data.fileExtension === ".pvs") {
                        pvsTerminal.showProof(data);
                        pvsTerminal.proveFormula(data);
                    } else if (data.fileExtension === ".tccs") {
                        pvsTerminal.showProof(data);
                        pvsTerminal.proveTcc(data);
                    }
                }
                // this.info(`Executing step ${data.cmd}`);
                await this.activeTerminals[terminalID].stepCommand(data.cmd);
                this.ready();
            }
        }));
    }
}