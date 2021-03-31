
/**
 * @module vscodePvsXTerm
 * @author Paolo Masci
 * @date 2021.03.10
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

import {
    Uri, WebviewPanel, ExtensionContext, Terminal, TerminalOptions, 
    ExtensionTerminalOptions, TerminalExitStatus, window, ViewColumn, TextEditor, commands
} from 'vscode';
import * as path from 'path';
import * as Handlebars from "handlebars";
import Backbone = require('backbone');
import * as utils from '../common/languageUtils';
import { LanguageClient } from 'vscode-languageclient';
import {
    EvaluatorCommandResponse, FileDescriptor, ProofCommandResponse, ProveFormulaResponse, 
    PvsFormula, PvsioEvaluatorCommand, PvsProofCommand, PvsTheory, serverEvent, serverRequest 
} from '../common/serverInterface';
import { PvsResponse } from '../common/pvs-gui';
import * as vscodeUtils from '../utils/vscode-utils';
import * as fsUtils from '../common/fsUtils';
import * as colorUtils from '../common/colorUtils';
import * as commandUtils from '../common/commandUtils';
import { XTermEvent, SessionType, interruptCommand, XTermCommands, UpdateCommandHistoryData, XTermMessage } from '../common/xtermInterface';


export enum XTermPvsEvent {
    DidCloseTerminal = "DidCloseTerminal",
    DidReceiveEvaluatorResponse = "DidReceiveEvaluatorResponse"
}

const HELP_PANEL_HEIGHT: number = 52; //px

const htmlTemplate: string = `
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{{title}}</title>
    {{#if style}}
    <style type="text/css">{{style}}</style>
    {{/if}}
    {{#each css}}
    <link rel="stylesheet" href="{{this}}">
    {{/each}}
    {{#each js}}
    <script src="{{this}}"></script>
    {{/each}}

    <style>
    body {
        background: transparent;
    }
    .terminal-help {
        height:${HELP_PANEL_HEIGHT}px;
        width:100%;
        background:transparent;
        color:white;
        font-size:11px;
        font-family:monospace;
        text-align:left;
        margin-top:10px;
        padding-top:4px !important;
        border-top:1px solid gray;
        overflow:auto;
    }    
    </style>
</head>
<body>
    <div id="terminal" class="animate__animated animate__fadeIn"></div>
    <div class="terminal-help p-0"></div>
    <script>
    const vscode = acquireVsCodeApi();
    console.log("xtermpvs", xtermpvs);

    const xterm = new xtermpvs.XTermPvs({
        sessionType: "{{sessionType}}",
        paddingBottom: ${HELP_PANEL_HEIGHT}
    });

    // Handlers for events triggered by the terminal
    xterm.on("${XTermEvent.sendText}", (event) => {
        vscode.postMessage({
            command: "${XTermEvent.sendText}",
            data: event.data
        });
    });

    // Handlers for messages exchanged between vscode and pvsioweb
    window.addEventListener('message', async (event) => {
        const message = event.data; // JSON data sent by vscode-pvs
        switch (message.command) {
            {{#each xtermCommands}}
            case "{{this}}": {
                // console.log("[xterm-webview] {{this}} data", message);
                xterm["{{this}}"](message.data);
                break;
            }
            {{/each}}
            default: {
                break;
            }
        }
    })
    </script>
</body>
</html>`;

export interface StartXTermProverRequest extends PvsFormula {
    proofFile?: FileDescriptor
};
export interface StartXTermEvaluatorRequest extends PvsTheory { };

export class VSCodePvsXTerm extends Backbone.Model implements Terminal {
    protected client: LanguageClient;
    protected context: ExtensionContext;
    protected prompt: string = "";
    protected target: PvsTheory | PvsFormula;
    protected panel: WebviewPanel;

    // lemmas, types, and definitions provided by the typechecker, used by the autocompletion engine
    protected mathObjects: commandUtils.MathObjects = {};

    // session type
    protected sessionType: SessionType;

    name: string;
    processId: Thenable<number>;
    creationOptions: Readonly<TerminalOptions | ExtensionTerminalOptions>;
    exitStatus: TerminalExitStatus;

    /**
     * Constructor
     */
    constructor (client: LanguageClient) {
        super();
        this.client = client;
    }

    /**
     * Internal function, adjusts the theory name for tcc formulas
     * @param theory 
     * @returns 
     */
    protected matchTccTheory (theory: PvsTheory): void {
        if (theory.fileExtension === ".tccs") {//} && theory.theoryName.endsWith("_TCCS")) {
            theory.theoryName = theory.theoryName.substr(0, theory.theoryName.length - 5);
        }
    }

    /**
     * Handler for starting a new prover session.
     * If the formula is not provided, then the prover is started with the active pvs document in the editor.
     * The file displayed in the active pvs editor is automatically saved upon invocation of this handler.
     */
    async onStartProverRequest (req?: StartXTermProverRequest): Promise<boolean> {
        const activeEditor: TextEditor = vscodeUtils.getActivePvsEditor();
        // if the file is currently open in the editor, save file first
        await activeEditor?.document?.save();
        if (!req) {
            // this happens when using the toolbar to start the evaluator
            const fname: string = activeEditor?.document?.fileName;
            if (fname) {
                const contextFolder: string  = fsUtils.getContextFolder(fname);
                const fileName: string = fsUtils.getFileName(fname);
                const fileExtension: string = fsUtils.getFileExtension(fname);
                const line: number = window.activeTextEditor?.selection?.active?.line || 0;
                const content: string = await fsUtils.readFile(fname);
                const theoryName: string = fsUtils.findTheoryName(content, line);
                const formulaName: string = fsUtils.findFormulaName(content, line);
                req = {
                    contextFolder,
                    fileName,
                    fileExtension,
                    theoryName,
                    formulaName
                };
                this.matchTccTheory(req);
            }
        }
        if (req && req.theoryName && req.formulaName && req.fileName && req.fileExtension && req.contextFolder) {
            const success: boolean = await this.startProverSession(req);
            return success;
        }
        vscodeUtils.showErrorMessage(`Error: Could not start Prover session. Please check that the file typechecks correctly.`);
        return false;
    }

    /**
     * Disables terminal input
     */
    disableTerminalInput (): void {
        const message: XTermMessage = {
            command: XTermCommands.disableInput
        };
        this.panel?.webview?.postMessage(message);
    }

    /**
     * Enables terminal input
     */
    enableTerminalInput (): void {
        const message: XTermMessage = {
            command: XTermCommands.enableInput
        };
        this.panel?.webview?.postMessage(message);
    }

    /**
     * Update command history
     */
    updateCommandHistory (cmd: string): void {
        const data: UpdateCommandHistoryData = { cmd, successHistory: true };
        const message: XTermMessage = {
            command: XTermCommands.updateCommandHistory,
            data
        };
        this.panel?.webview?.postMessage(message);
    }

    /**
     * Update integrated help
     */
    updateHelp (): void {
        const message: XTermMessage = {
            command: XTermCommands.updateHelp
        };
        this.panel?.webview?.postMessage(message);
    }

    /**
     * Internal function, reveals the webview and sends proveFormula request to pvs-server.
     */
    protected async startProverSession (formula: PvsFormula): Promise<boolean> {
        this.target = formula;
        this.sessionType = "prover"
        this.clearScreen();
        this.setPrompt(utils.proverPrompt);
        this.reveal();
        const welcome: string = `\nStarting prover session for ${colorUtils.colorText(formula.formulaName, colorUtils.PvsColor.blue)}\n`;
        this.log(welcome);
        this.disableTerminalInput();
        // send proveFormula request to pvs-server
        this.client.sendRequest(serverRequest.proveFormula, {
            ...formula,
            origin: "xterm-pvs"
        });
        const success: boolean = await new Promise((resolve, reject) => {
            this.client.onRequest(serverEvent.proveFormulaResponse, (data: ProveFormulaResponse) => {
                this.mathObjects = data?.mathObjects || {};
                this.onProverResponse(data);
                this.enableTerminalInput();
                this.showWelcomeMessage();
                resolve(true);
            });
            // The following handler is registered here because proof commands may originate from proof-explorer.
            // This handler will be replaced by the one in sendText as soon as a sendText is performed.
            this.client.onRequest(serverEvent.proofCommandResponse, (data: ProofCommandResponse) => {
                this.onProverResponse(data);
            });
        });
        return success;
    }

    /**
     * Internal function, handles prover responses
     */
    protected onProverResponse (data: ProofCommandResponse): void {
        if (typeof data?.res === "string") {
            if (data.res === "Q.E.D." || data.res === "bye!") {
                // Q.E.D.
                this.log(colorUtils.colorText(data.res, colorUtils.PvsColor.green), {
                    sessionEnd: true
                });
            } else {
                console.log(data.res);
            }
        } else {
            // res is a SequentDescriptor
            // echo last command
            if (data?.req?.cmd && data?.req?.origin !== "xterm-pvs") {
                // this.log(`${utils.colorText(utils.proverPrompt, pvsColor.blue, true)} ${data.req.cmd}`,);
                this.clearCommandLine();
                this.log(data.req.cmd);
            }
            if (data?.req?.cmd && !commandUtils.isInvalidCommand(data.res)) {
                this.updateCommandHistory(data.req.cmd);
            }

            // const sequent: string = utils.formatSequent(data?.res, { useColors: true, htmlEncoding: true });
            const sequent: string = utils.formatSequent(data?.res, { useColors: true });
            const lastState: string = utils.sformulas2string(data.res);
            const theoryContent: string = this.target?.fileContent;
            const hints: commandUtils.HintsObject = commandUtils.getHints(this.sessionType, {
                lastState, 
                theoryContent
            });
            this.log(sequent, { hints, mathObjects: this.mathObjects });
            // show prompt only if the request was not originated by xterm or if there's no sequent (e.g., this is an error reported from pvs)
            // if (!data?.res?.sequent || data?.req?.origin !== "xterm-pvs") {
                this.showPrompt();
            // }
        }
    };

    /**
     * Reboot prover session programmatically
     */
    async rebootProverSession (formula: PvsFormula): Promise<boolean> {
        if (formula) {
            await this.closeSession(formula, "prover");
            const success: boolean = await this.startProverSession(formula);
            return success;
        }
        return false;
    }

    /**
     * Handler for starting a new evaluator session.
     * If the theory is not provided, then the evaluator is started with the active pvs document in the editor.
     * The file displayed in the active pvs editor is automatically saved upon invocation of this handler.
     */
    async onStartEvaluatorRequest (req?: StartXTermEvaluatorRequest): Promise<boolean> {
        const activeEditor: TextEditor = vscodeUtils.getActivePvsEditor();
        // if the file is currently open in the editor, save file first
        await activeEditor?.document?.save();
        if (!req) {
            // this happens when using the toolbar to start the evaluator
            const fname: string = activeEditor?.document?.fileName;
            if (fname) {
                const contextFolder: string  = fsUtils.getContextFolder(fname);
                const fileName: string = fsUtils.getFileName(fname);
                const fileExtension: string = fsUtils.getFileExtension(fname);
                const line: number = window.activeTextEditor?.selection?.active?.line || 0;
                const fileContent: string = await fsUtils.readFile(fname);
                const theoryName: string = fsUtils.findTheoryName(fileContent, line);
                req = {
                    contextFolder,
                    fileName,
                    fileExtension,
                    theoryName,
                    fileContent
                };
                this.matchTccTheory(req);
            }
        }
        if (req && req.theoryName) {
            const success: boolean = await this.startEvaluatorSession(req);
            return success;
        }
        vscodeUtils.showErrorMessage(`Error: could not start Evaluator session. Please check that the file typechecks correctly.`);
        return false;
    }

    /**
     * Internal function, reveals the webview and sends startEvaluator request to pvs-server.
     */
    protected async startEvaluatorSession (theory: PvsTheory): Promise<boolean> {
        this.target = theory;
        this.sessionType = "evaluator";
        this.clearScreen();
        this.setPrompt(utils.pvsioPrompt);
        const welcome: string = `\nStarting PVSio evaluator session for theory ${colorUtils.colorText(theory.theoryName, colorUtils.PvsColor.blue)}\n`;
        this.reveal();
        this.log(welcome);
        this.disableTerminalInput();
        // send start-pvsio request to pvs-server
        this.client.sendRequest(serverRequest.startEvaluator, theory);
        const success: boolean = await new Promise((resolve, reject) => {
            this.client.onRequest(serverEvent.startEvaluatorResponse, (data: { response: PvsResponse, args: PvsTheory }) => {
                const banner: string = colorUtils.colorText(utils.pvsioBannerAlt, colorUtils.PvsColor.green);
                const hints: commandUtils.HintsObject = commandUtils.getHints(this.sessionType, {
                    theoryContent: this.target?.fileContent
                });
                this.log(banner, { hints });
                this.showPrompt();
                this.enableTerminalInput();
                this.showWelcomeMessage();
                resolve(true);
            });
        });
        return success;
    }

    /**
     * Handler for evaluator responses
     */
    protected onEvaluatorResponse (data: EvaluatorCommandResponse): void {
        if (data?.res === "bye!") {
            this.setPrompt("");
            this.log(colorUtils.colorText(data.res, colorUtils.PvsColor.green), {
                sessionEnd: true
            });
        } else {
            if (data?.res) {
                const hints: commandUtils.HintsObject = commandUtils.getHints(this.sessionType, {
                    lastState: data.res, 
                    theoryContent: this.target?.fileContent
                });
                this.log(data.res, { hints });
                // trigger event for interested listeners, e.g., pvsioweb
                this.trigger(XTermPvsEvent.DidReceiveEvaluatorResponse, data);
            }
            this.showPrompt();
        }
    }
    
    /**
     * Reboot evaluator session programmatically
     */
    async rebootEvaluatorSession (theory: PvsTheory): Promise<boolean> {
        if (theory) {
            await this.closeSession(theory, "evaluator");
            const success: boolean = await this.startEvaluatorSession(theory);
            return success;
        }
        return false;
    }

    /**
     * Closes a terminal session
     */
    async closeSession (theory: PvsTheory, sessionType: SessionType): Promise<boolean> {
        // perform a sanity check, to avoid closing the wrong session
        if (theory && this.sessionType === sessionType) {
            await this.sendTextToServer("quit");
            return true;
        }
    }
    
    /**
     * Sends text to the terminal
     */
    sendText (text: string, addNewLine?: boolean): void {
        this.write(text);
    }
    
    /**
     * Sends a command to pvs-server
     */
    async sendTextToServer (text: string, addNewLine?: boolean): Promise<EvaluatorCommandResponse | ProofCommandResponse | null> {
        // return new Promise((resolve, reject) => {
            if (this.sessionType === "evaluator") {
                const command: PvsioEvaluatorCommand = {
                    cmd: text,
                    ...this.target
                };
                return await this.sendEvaluatorCommand(command);
                // this.client.sendRequest(serverRequest.evaluatorCommand, command);
                // this.client.onRequest(serverEvent.evaluatorCommandResponse, (data: EvaluatorCommandResponse) => {
                //     this.onEvaluatorResponse(data);
                //     resolve(data)
                // });
            } 
            if (this.sessionType === "prover") {
                const command: PvsProofCommand = {
                    cmd: commandUtils.balancePar(text.trim()),
                    ...<PvsFormula> this.target,
                    origin: "xterm-pvs"
                };
                return await this.sendProverCommand(command);
                // this.client.sendRequest(serverRequest.proofCommand, command);
                // this.client.onRequest(serverEvent.proofCommandResponse, (data: ProofCommandResponse) => {
                //     this.onProverResponse(data);
                //     resolve(data);
                // });
            } //else {
                return null;
            //}
        // });
    }
    /**
     * Sends an evaluator command to the server
     */
    async sendEvaluatorCommand (req: PvsioEvaluatorCommand): Promise<EvaluatorCommandResponse> {
        return new Promise((resolve, reject) => {
            if (this.sessionType === "evaluator") {
                this.client.sendRequest(serverRequest.evaluatorCommand, req);
                this.client.onRequest(serverEvent.evaluatorCommandResponse, (data: EvaluatorCommandResponse) => {
                    this.onEvaluatorResponse(data);
                    resolve(data)
                });
            } else {
                resolve(null);
            }
        });
    }
    /**
     * Sends a prover command to the server
     */
    async sendProverCommand (req: PvsProofCommand): Promise<ProofCommandResponse> {
        return new Promise((resolve, reject) => {
            if (this.sessionType === "prover") {
                this.client.sendRequest(serverRequest.proofCommand, req);
                this.client.onRequest(serverEvent.proofCommandResponse, (data: ProofCommandResponse) => {
                    this.onProverResponse(data);
                    resolve(data);
                });
            } else {
                resolve(null);
            }
        });
    }
    /**
     * Places focus on the terminal
     */
    focus (): void {
        this.reveal();
        // Use a timeout so that the webview has time to render its content
        setTimeout(() => {
            const message: XTermMessage = {
                command: XTermCommands.focus
            };
            this.panel?.webview?.postMessage(message);
        }, 250);
    }
    /**
     * Shows the terminal
     */
    show (preserveFocus?: boolean): void {
        this.reveal();
    }
    /**
     * Deletes the terminal
     */
    dispose (): void {
        this.panel.dispose();
        this.trigger(XTermPvsEvent.DidCloseTerminal);
    }
    /**
     * Reveals the terminal
     */
    reveal (): void {
        if (!this.panel) {
            this.renderView();
        }
        this.panel.reveal(ViewColumn.Active, false); // false allows the webview to get the focus
    }
    /**
     * Hides the terminal
     */
    hide (): void {
        this.dispose();
    }
    /**
     * Activates the terminal
     */
    activate (context: ExtensionContext) {
        this.context = context;
        // context.subscriptions.push(vscode.commands.registerCommand("vscode-pvs.x-term", () => {
        //     this.reveal();
        // }));
    }
    /**
     * Writes a command in the terminal
     */
    write (data: string): void {
        if (data) {
            const message: XTermMessage = {
                command: XTermCommands.write,
                data: data
            };
            this.panel?.webview?.postMessage(message);
        }
    }
    /**
     * Logs data in the terminal
     */
    log (data: string, opt?: { 
        sessionEnd?: boolean, 
        hints?: commandUtils.HintsObject, 
        mathObjects?: commandUtils.MathObjects
    }): void {
        const message: XTermMessage = {
            command: XTermCommands.log,
            data: data
        };
        this.panel?.webview?.postMessage(message);
        if (opt?.sessionEnd) {
            // write a closing messages.
            if (this.sessionType === "evaluator") {
                message.data = " Evaluator session terminated.";
                this.panel?.webview?.postMessage(message);
            } else if (this.sessionType === "prover" && data.includes("bye!")) {
                message.data = " Prover session terminated."
                this.panel?.webview?.postMessage(message);
            }
            this.disableTerminalInput();
            return;
        }
        if (opt?.hints) {
            const message: XTermMessage = {
                command: XTermCommands.updateHints,
                data: opt.hints
            };
            this.panel?.webview?.postMessage(message);
        }
        if (opt?.mathObjects) {
            const message: XTermMessage = {
                command: XTermCommands.updateMathObjects,
                data: opt.mathObjects
            };
            this.panel?.webview?.postMessage(message);
        }
    }
    /**
     * Renders the webview
     */
    renderView (): void {
        this.createWebView();
    }
    /**
     * Sets the prompt
     */
    setPrompt (prompt: string): void {
        this.prompt = prompt;
    }
    /**
     * Show the prompt
     */
    showPrompt (): void {
        const message: XTermMessage = {
            command: XTermCommands.showPrompt,
            data: this.prompt
        };
        this.panel?.webview?.postMessage(message);
    }
    /**
     * Clears the screen the moves the prompt to the initial position (top-left corner)
     */
    clearScreen (): void {
        const message: XTermMessage = {
            command: XTermCommands.clearScreen,
            data: this.prompt
        };
        this.panel?.webview?.postMessage(message);
    }
    /**
     * Shows welcome message in the prover terminal
     */
    showWelcomeMessage (): void {
        const message: XTermMessage = {
            command: XTermCommands.showWelcomeMessage,
            data: this.prompt
        };
        this.panel?.webview?.postMessage(message);    
    }
    /**
     * Clears the command line in the terminal
     */
    clearCommandLine (): void {
        const message: XTermMessage = {
            command: XTermCommands.clearCommandLine,
            data: this.prompt
        };
        this.panel?.webview?.postMessage(message);        
    }
    /**
     * Returns true if the current session is an evaluator session
     */
    evaluatorSession (): boolean {
        return this.sessionType === "evaluator";
    }
    /**
     * Returns true if the current session is a prover session
     */
    proverSession (): boolean {
        return this.sessionType === "prover";
    }
    /**
     * Returns the theory associated to the active session
     */
    getTheory (): PvsTheory {
        return this.target;
    }
    /**
     * Internal function, creates the webview
     */
    protected createWebView () {
        const title: string = this.sessionType === "prover" ? "X-Term PVS" : "X-Term PVSio";
        if (this.panel) {
            this.panel.title = title;
        } else {
            this.panel = this.panel || window.createWebviewPanel(
                'x-term-pvs', // Identifies the type of the webview. Used internally
                title, // Title of the panel displayed to the user
                ViewColumn.Active, // Editor column to show the new webview panel in.
                {
                    enableScripts: true,
                    retainContextWhenHidden: true,
                    enableFindWidget: true
                }
            );
            try {
                // clean up data structures when webview is disposed
                this.panel.onDidDispose(
                    async () => {
                        // delete panel
                        this.panel = null;
                        // reset session type
                        this.sessionType = null;
                        // send quit command to the server
                        this.sendTextToServer("quit");
                        // reset global vscode-pvs variables so other views can be updated properly
                        vscodeUtils.resetGlobals();
                    },
                    null,
                    this.context.subscriptions
                );
                // Handle messages from the webview
                this.panel.webview.onDidReceiveMessage(
                    (message: XTermMessage) => {
                        console.log("[vscode-xterm] Received message", message);
                        if (message) {
                            switch (message.command) {
                                case XTermEvent.sendText: {
                                    if (message?.data === interruptCommand) {
                                        commands.executeCommand("vscode-pvs.interrupt-prover");
                                    } else {
                                        this.sendTextToServer(message?.data);
                                    }
                                    break;
                                }
                                default: {
                                    break;
                                }
                            }
                        }
                    },
                    undefined,
                    this.context.subscriptions
                );
                // Create webview content
                this.createContent();
                // set language to pvs
                vscodeUtils.setEditorLanguage();
            } catch (err) {
                console.error(err);
            }
        }
    }
    /**
     * Internal function, creates the html content of the webview
     */
    protected createContent (): void {
        // set webview content
        const xtermPvsJsOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/out/extra/xterm-pvs.min.js'));
        const xtermCssOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/xterm/css/xterm.css'));

        const bootstrapJsOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/bootstrap/dist/js/bootstrap.bundle.min.js'));
        const bootstrapCssOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/bootstrap/dist/css/bootstrap.min.css'));
        
        const jqueryOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/jquery/dist/jquery.min.js'));
        const fontawesomeCssOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/font-awesome/css/font-awesome.min.css'));

        const animateCssOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/animate.css/animate.min.css'));

        const css: Uri[] = [
            this.panel.webview.asWebviewUri(xtermCssOnDisk),
            this.panel.webview.asWebviewUri(bootstrapCssOnDisk),
            this.panel.webview.asWebviewUri(fontawesomeCssOnDisk),
            this.panel.webview.asWebviewUri(animateCssOnDisk)
        ];
        const js: Uri[] = [
            this.panel.webview.asWebviewUri(jqueryOnDisk),
            this.panel.webview.asWebviewUri(bootstrapJsOnDisk),
            this.panel.webview.asWebviewUri(xtermPvsJsOnDisk)
        ];

        try {
            const content: string = Handlebars.compile(htmlTemplate, { noEscape: true })({
                css, js,
                xtermCommands: [
                    XTermCommands.write,
                    XTermCommands.log,
                    XTermCommands.showPrompt,
                    XTermCommands.focus,
                    XTermCommands.updateHints,
                    XTermCommands.updateMathObjects,
                    XTermCommands.clearScreen,
                    XTermCommands.disableInput,
                    XTermCommands.enableInput,
                    XTermCommands.updateCommandHistory,
                    XTermCommands.updateHelp,
                    XTermCommands.clearCommandLine,
                    XTermCommands.showWelcomeMessage
                ],
                sessionType: this.sessionType
            });
            this.panel.webview.html = content;
        } catch (err) {
            console.error(err);
        }
    }    
}