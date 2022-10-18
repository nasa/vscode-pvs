
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
    ExtensionTerminalOptions, TerminalExitStatus, window, ViewColumn, TextEditor, commands, WebviewPanelOnDidChangeViewStateEvent
} from 'vscode';
import * as path from 'path';
import * as Handlebars from "handlebars";
import Backbone = require('backbone');
import * as utils from '../common/languageUtils';
import { LanguageClient } from 'vscode-languageclient';
import {
    EvaluatorCommandResponse, HintsObject, MathObjects, ProofCommandResponse, ProveFormulaRequest, ProveFormulaResponse, 
    PvsFormula, PvsioEvaluatorCommand, PvsProofCommand, PvsTheory, SequentDescriptor, serverEvent, serverRequest 
} from '../common/serverInterface';
import { PvsResponse } from '../common/pvs-gui';
import * as vscodeUtils from '../utils/vscode-utils';
import * as fsUtils from '../common/fsUtils';
import * as colorUtils from '../common/colorUtils';
import {
    XTermEvent, SessionType, interruptCommand, XTermCommands, UpdateCommandHistoryData, XTermMessage, PrettyPrinter, PrettyPrinterInfo
} from '../common/xtermInterface';
import { balancePar, checkPar, CheckParResult, getHints, isInvalidCommand, isQEDCommand, noChange } from '../common/languageUtils';
import { VSCodePvsProofExplorer } from './vscodePvsProofExplorer';
import { YesNoCancel } from '../utils/vscode-utils';

export enum XTermPvsEvent {
    DidCloseTerminal = "DidCloseTerminal",
    DidReceiveEvaluatorResponse = "DidReceiveEvaluatorResponse"
}

const HELP_PANEL_LINE_HEIGHT: number = 20; //px
const DEFAULT_HELP_PANEL_LINES: number = 2;

function getHtmlTemplate (sessionType: SessionType, opt?: { integratedHelpSize?: number }): string {
    const lines: number = opt?.integratedHelpSize || vscodeUtils.getIntegratedHelpSetting() || DEFAULT_HELP_PANEL_LINES;
    const frequentCommands: string = sessionType === "prover" ? vscodeUtils.getFrequentProverCommandsSetting() : "";
    return `
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
    .terminal-help {
        height:${lines * HELP_PANEL_LINE_HEIGHT}px;
    }
    .spacer {
        height:${HELP_PANEL_LINE_HEIGHT / 2}px;
    }
    </style>
</head>
<body>
    <!--<div id="terminal" class="animate__animated animate__fadeIn"></div>-->
    <div id="terminal"></div>
    <div class="spacer"></div>
    <div class="terminal-help p-0"></div>

    <script>
    const vscode = acquireVsCodeApi();
    const xterm = new xtermpvs.XTermPvs({
        sessionType: "{{sessionType}}",
        paddingBottom: ${1.5 * lines * HELP_PANEL_LINE_HEIGHT},
        integratedHelpSize: ${lines},
        frequentCommands: "${frequentCommands}"
    });

    // Handlers for events triggered by xterm
    {{#each xtermEvents}}
    xterm.on("{{this}}", (event) => {
        vscode.postMessage({
            command: "{{this}}",
            data: event && event.data ? event.data : undefined
        });
    });
    {{/each}}

    // Handler for sending commands to xterm
    window.addEventListener('message', async (event) => {
        // console.log("[xterm-webview] message", { event });
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
    });

    // ready event
    window.addEventListener('load', async (event) => {
        console.log("xterm", xterm);
        xterm.focus();
        vscode.postMessage({
            command: "${XTermEvent.ready}",
            data: true
        });
    });
    </script>
</body>
</html>`;
}

const DOT_BLINK: string = `<span class="animate__animated animate__flash animate__slow animate__infinite">.</span>`;
const PLEASE_WAIT: string = `please wait..${DOT_BLINK}`;

export type StartXTermProverRequest = ProveFormulaRequest;
export interface StartXTermEvaluatorRequest extends PvsTheory { };

export class VSCodePvsXTerm extends Backbone.Model implements Terminal {
    protected client: LanguageClient;
    protected context: ExtensionContext;
    protected prompt: string = "";
    protected target: PvsTheory | PvsFormula;
    protected panel: WebviewPanel = null;

    // color theme
    protected colorTheme: "dark" | "light";

    // whether matching parens should be colorized
    protected colorizeParens: boolean = false;

    // lemmas, types, and definitions provided by the typechecker, used by the autocompletion engine
    protected mathObjects: MathObjects = {};

    // session type
    protected sessionType: SessionType;

    // timer for delayed focus
    protected tfocus: NodeJS.Timeout = null;

    // pointer to proof explorer
    protected proofExplorer: VSCodePvsProofExplorer;

    // running flag, prevents this.showWelcomeScreen
    protected runningFlag: boolean = false;

    // pretty printer for prover console, useful for domain-specific languages, e.g., DDL
    protected prettyPrinter: PrettyPrinterInfo;

    name: string;
    processId: Thenable<number>;
    creationOptions: Readonly<TerminalOptions | ExtensionTerminalOptions>;
    exitStatus: TerminalExitStatus;

    /**
     * Constructor
     */
    constructor (client: LanguageClient, proofExplorer: VSCodePvsProofExplorer) {
        super();
        this.client = client;
        this.colorTheme = vscodeUtils.detectColorTheme();
        this.proofExplorer = proofExplorer;
    }

    /**
     * Internal function, adjusts the theory name for tcc formulas
     * @param theory 
     * @returns 
     */
    protected matchTccTheory (theory: PvsTheory): void {
        if (theory.fileExtension === ".tccs") {//} && theory.theoryName.endsWith("_TCCS")) {
            theory.theoryName = theory.theoryName.substring(0, theory.theoryName.length - 5);
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
                const activeEditor: TextEditor = vscodeUtils.getActivePvsEditor();
                const line: number = activeEditor?.selection?.active?.line || 0;
                const content: string = await fsUtils.readFile(fname);
                const theoryName: string = fsUtils.findTheoryName(content, line);
                const formulaName: string = fsUtils.findFormulaName(content, line);
                req = {
                    contextFolder,
                    fileName,
                    fileExtension,
                    theoryName,
                    formulaName,
                    line
                };
                this.matchTccTheory(req);
            }
        }
        if (req && req.theoryName && req.formulaName && req.fileName && req.fileExtension && req.contextFolder) {
            vscodeUtils.minimizeIntegratedTerminal();
            const success: boolean = await this.startProverSession(req);
            return success;
        }
        vscodeUtils.showErrorMessage(`Error: Could not start Prover session. Please check that the file typechecks correctly.`);
        return false;
    }

    /**
     * Utility function, updates color theme
     */
    updateColorTheme (theme: colorUtils.XTermColorTheme): void {
        if (theme && this.colorTheme !== theme) {
            this.colorTheme = theme;
            const message: XTermMessage = {
                command: XTermCommands.updateColorTheme,
                data: this.colorTheme
            };
            this.panel?.webview?.postMessage(message);
        }
    }

    /**
     * Utility function, disables colorize matching parens option
     */
    disableBracketColors (): void {
        this.colorizeParens = false;
    }
    /**
     * Utility function, disables colorize matching parens option
     */
    enableBracketColors (flag?: boolean): void {
        this.colorizeParens = flag !== undefined ? !!flag : true;
        // console.log("[xterm-pvs] Bracket colors: ", this.colorizeParens);
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
     * Returns true if the terminal is active
     */
    isActive (): boolean {
        return this.panel !== null;
    }

    /**
     * Internal function, configures xterm-pvs autocompletion based on vscode settings
     */
    configureAutocomplete (): void {
        const flag: boolean = vscodeUtils.getConfigurationFlag("pvs.settings.proverConsole.autocompleteWithEnter");
        this.autocompleteWithEnter(flag);
    }

    /**
     * Internal function, reveals the webview and sends proveFormula request to pvs-server.
     */
    protected async startProverSession (formula: StartXTermProverRequest): Promise<boolean> {
        this.target = formula;
        this.sessionType = "prover";
        this.prettyPrinter = formula?.prettyPrinter;
        this.clearScreen();
        this.setPrompt(utils.proverPrompt);
        this.configureAutocomplete();
        await this.reveal();
        const color: colorUtils.PvsColor = colorUtils.getColor(colorUtils.PvsColor.blue, this.colorTheme);
        const welcome: string = `\nStarting prover session for ${colorUtils.colorText(formula.formulaName, color)}\n`;
        this.log(welcome);
        this.showHelpMessage(`Starting prover session, ${PLEASE_WAIT}`);
        this.disableTerminalInput();
        // send proveFormula request to pvs-server
        this.client.sendRequest(serverRequest.proveFormula, {
            origin: "xterm-pvs",
            ...formula
        });
        const success: boolean = await new Promise((resolve, reject) => {
            this.client.onRequest(serverEvent.proveFormulaResponse, (data: ProveFormulaResponse) => {
                if (this.sessionType) {
                    if (this.prettyPrinter) {
                        const color: colorUtils.PvsColor = colorUtils.getColor(colorUtils.PvsColor.green, this.colorTheme);
                        this.log(`${colorUtils.colorText(`Using ${this.prettyPrinter.language} syntax`, color)}`);
                    }
                    // if session type is not defined, then the proof has already ended -- this may happen for trivial proofs
                    this.mathObjects = data?.mathObjects || {};
                    this.onProverResponse(data, { ignoreCommentary: true });
                    this.enableTerminalInput();
                    this.showWelcomeMessage();
                }
                resolve(true);
            });
            // The following handler is registered here because proof commands may originate from proof-explorer.
            // This handler will be replaced by the one in sendText as soon as a sendText is performed.
            this.client.onRequest(serverEvent.proofCommandResponse, (data: ProofCommandResponse) => {
                // console.log("[vscode-pvs-xterm] proofCommandResponse", data);
                this.onProverResponse(data);
            });
        });
        return success;
    }

    /**
     * Internal function, handles prover responses
     */
    protected onProverResponse (data: ProofCommandResponse, opt?: { ignoreCommentary?: boolean }): void {
        opt = opt || {};
        if (data) {
            if (typeof data.res === "string") {
                if (data.res === "Q.E.D." || data.res === "bye!") {
                    const xtermMsg: string = colorUtils.colorText(data.res, colorUtils.getColor(colorUtils.PvsColor.green, this.colorTheme));
                    this.log("\n" + xtermMsg, {
                        sessionEnd: true
                    });
                    const msg: string = data.res === "Q.E.D." ? `Proof completed successfully!\nThe proof has been saved. You can now <span class="btn btn-sm btn-primary btn-help close-action m-0 p-0">close</span> the prover console.`
                        : `Prover session terminated.\nYou can now <span class="btn btn-sm btn-primary btn-help close-action m-0 p-0">close</span> the prover console.`;
                    // send the message after a timeout, to avoid overwrites due to automatic updates of the help panel
                    setTimeout(() => {
                        this.showHelpMessage(msg);
                    }, 500);
                    // disable response handlers
                    this.disableHandlers();
                    this.sessionType = null;
                } else {
                    console.log(data.res);
                }
            } else {
                // res is a SequentDescriptor
                // echo last command if the response was not originated by xterm
                if (data.req?.cmd && data.req?.origin !== "xterm-pvs") {
                    // this.log(`${utils.colorText(utils.proverPrompt, pvsColor.blue, true)} ${data.req.cmd}`,);
                    this.clearCommandLine();
                    this.log(data.req.cmd);
                }
                if (data.req?.cmd && !isInvalidCommand(data.res)) {
                    this.updateCommandHistory(data.req.cmd);
                }
                // load prettyprinter from vscode configuration
                const pp: string = this.prettyPrinter?.file; //vscodeUtils.getPrettyPrinter();
                console.log({ pp });
                // format sequent
                const sequent: string = fsUtils.formatSequent(data?.res, { 
                    colorTheme: this.colorTheme, 
                    colorizeParens: this.colorizeParens,
                    useColors: true,
                    prettyPrinter: pp?.endsWith(".jar") ? { cmd: "java", options: [ "-jar", pp ] } : null, // only prettyprinters written in java are supported for now
                    ...opt 
                });
                const lastState: string = fsUtils.sformulas2string(data.res);
                const theoryContent: string = this.target?.fileContent;
                const hints: HintsObject = getHints(this.sessionType, {
                    lastState, 
                    theoryContent
                });
                this.log(sequent, { hints, mathObjects: this.mathObjects });
                // show prompt
                this.showPrompt();
                commands.executeCommand("xterm.did-execute-command");
            }
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
                const activeEditor: TextEditor = vscodeUtils.getActivePvsEditor();
                const line: number = activeEditor?.selection?.active?.line || 0;
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
            if (this.target?.contextFolder === req.contextFolder && this.target?.fileName === req.fileName
                && this.target?.theoryName === req.theoryName && this.target?.fileExtension === req.fileExtension) {
                    // pvsio already started, focus on the evaluator window
                    this.focus();
                    return true;
            }
            vscodeUtils.minimizeIntegratedTerminal();
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
        this.configureAutocomplete();
        const color: colorUtils.PvsColor = colorUtils.getColor(colorUtils.PvsColor.blue, this.colorTheme);
        const welcome: string = `\nStarting PVSio evaluator session for theory ${colorUtils.colorText(theory.theoryName, color)}\n`;
        await this.reveal();
        this.log(welcome);
        this.showHelpMessage(`Starting evaluator session, ${PLEASE_WAIT}`);
        this.disableTerminalInput();
        // send start-pvsio request to pvs-server
        this.client.sendRequest(serverRequest.startEvaluator, theory);
        const success: boolean = await new Promise((resolve, reject) => {
            this.client.onRequest(serverEvent.startEvaluatorResponse, (data: { response: PvsResponse, args: PvsTheory, error?: string }) => {
                if (data?.response) {
                    const banner: string = colorUtils.colorText(utils.pvsioBannerAlt, colorUtils.getColor(colorUtils.PvsColor.green, this.colorTheme));
                    const hints: HintsObject = getHints(this.sessionType, {
                        theoryContent: this.target?.fileContent
                    });
                    this.log(banner, { hints });
                    this.showPrompt();
                    this.showWelcomeMessage();
                    this.enableTerminalInput();
                } else {
                    this.log(data?.error);
                    this.onEvaluatorResponse({ res: "bye!", req: null, state: null });
                }
                resolve(true);
            });
        });
        return success;
    }

    /**
     * Handler for evaluator responses
     */
    protected onEvaluatorResponse (data: EvaluatorCommandResponse): void {
        if (data) {
            if (data.res === "bye!") {
                this.setPrompt("");
                this.log("\n" + colorUtils.colorText(data.res, colorUtils.getColor(colorUtils.PvsColor.green, this.colorTheme)), {
                    sessionEnd: true
                });
                const msg: string = `Evaluator session terminated.\nYou can now <span class="btn btn-sm btn-primary btn-help close-action m-0 p-0">close</span> the evaluator console.`;
                this.showHelpMessage(msg);
                // disable response handlers
                this.disableHandlers();
                // clear session type
                this.sessionType = null;
                // clear target
                this.target = null;
                // reset global vscode-pvs variables so other views can be updated properly
                vscodeUtils.resetGlobals();                    
                
            } else {
                if (data.res) {
                    const hints: HintsObject = getHints(this.sessionType, {
                        lastState: data.res, 
                        theoryContent: this.target?.fileContent
                    });
                    this.log("\n" + data.res, { hints });
                    // trigger event for interested listeners, e.g., pvsioweb
                    this.trigger(XTermPvsEvent.DidReceiveEvaluatorResponse, data);
                }
                this.showPrompt();
                this.showWelcomeMessage();
            }
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
        return false;
    }
    
    /**
     * Disables a terminal session
     */
     async bye (): Promise<boolean> {
        const xtermMsg: string = colorUtils.colorText("bye!", colorUtils.getColor(colorUtils.PvsColor.green, this.colorTheme));
        this.log("\n" + xtermMsg, {
            sessionEnd: true
        });
        this.disableHandlers();
        return true;
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
    async sendTextToServer (text: string, addNewLine?: boolean): Promise<boolean> {
        // return new Promise((resolve, reject) => {
            if (this.sessionType === "evaluator") {
                const command: PvsioEvaluatorCommand = {
                    cmd: text,
                    ...this.target
                };
                await this.sendEvaluatorCommand(command);
                return true;
                // this.client.sendRequest(serverRequest.evaluatorCommand, command);
                // this.client.onRequest(serverEvent.evaluatorCommandResponse, (data: EvaluatorCommandResponse) => {
                //     this.onEvaluatorResponse(data);
                //     resolve(data)
                // });
            } 
            if (this.sessionType === "prover") {
                const command: PvsProofCommand = {
                    cmd: balancePar(text.trim()),
                    ...<PvsFormula> this.target,
                    origin: "xterm-pvs"
                };
                const success: boolean = await this.sendProverCommand(command);
                return success;
                // this.client.sendRequest(serverRequest.proofCommand, command);
                // this.client.onRequest(serverEvent.proofCommandResponse, (data: ProofCommandResponse) => {
                //     this.onProverResponse(data);
                //     resolve(data);
                // });
            } //else {
                return false;
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
    async sendProverCommand (req: PvsProofCommand): Promise<boolean> {
        return new Promise((resolve, reject) => {
            if (this.sessionType === "prover") {
                // sanity check for parens, pvs sometimes does not report problems and this confuses proof explorer
                const parens: CheckParResult = checkPar(req?.cmd, { includeStringContent: true });
				if (parens?.success) {
                    this.client.sendRequest(serverRequest.proofCommand, req);
                    this.client.onRequest(serverEvent.proofCommandResponse, (data: ProofCommandResponse) => {
                        this.onProverResponse(data);
                        const success: boolean = data ? 
                            typeof data.res === "string" ? isQEDCommand(data.res)
                                : !isInvalidCommand(data.res) && !noChange(data.res)
                                    : false;
                        resolve(success);
                        // resolve(data);
                    });
                } else {
                    // report unbalanced parens to the user
                    const err: SequentDescriptor = {
                        label: parens.msg,
                        commentary: parens.msg
                    };
                    const res: ProofCommandResponse = {
                        res: err,
                        req
                    };
					this.onProverResponse(res);
                    resolve(true);
                }
            } else {
                resolve(null);
            }
        });
    }
    /**
     * Returns the clipboard
     */
    getProofExplorer (): VSCodePvsProofExplorer {
        return this.proofExplorer;
    }
    /**
     * Places focus on the terminal
     */
    async focus (): Promise<boolean> {
        let success: boolean = await this.reveal();
        if (success) {
            // Use a timeout so that the webview has time to render its content
            // this is a workaround because xterm does not seem to be able to gain focus if the content is not fully rendered
            clearTimeout(this.tfocus);
            this.tfocus = setTimeout(() => {
                this.focusxterm();
            }, 250);
        }
        return success;
    }
    /**
     * Internal function, sends a focus request to xterm
     */
    protected focusxterm (): void {
        const message: XTermMessage = {
            command: XTermCommands.focus
        };
        this.panel?.webview?.postMessage(message);
    }
    /**
     * Shows the terminal
     */
    async show (preserveFocus?: boolean): Promise<boolean> {
        return await this.reveal();
    }
    /**
     * Deletes the terminal
     */
    dispose (): void {
        this.panel?.dispose();
        this.trigger(XTermPvsEvent.DidCloseTerminal);
    }
    /**
     * Reveals the terminal
     */
    async reveal (): Promise<boolean> {
        return await this.renderView();
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
        context.subscriptions.push(commands.registerCommand("vscode-pvs.focus-xterm", () => {
            this.focus();
        }));
        // set brackets colorization based on current settings
        const flag: boolean = vscodeUtils.getConfigurationFlag("pvs.settings.proverConsole.colorizeBracketPairs");
        this.enableBracketColors(flag);
    }
    /**
     * Writes a command in the terminal
     */
    write (data: string): void {
        if (data) {
            if (this.panel?.webview) {
                const message: XTermMessage = {
                    command: XTermCommands.write,
                    data: data
                };
                this.panel?.webview?.postMessage(message);
            } else {
                console.warn("[vscode-xterm] @write Warning: panel is null");
            }
        }
    }
    /**
     * Logs data in the terminal
     */
    log (data: string, opt?: { 
        sessionEnd?: boolean, 
        hints?: HintsObject, 
        mathObjects?: MathObjects
    }): void {
        // console.log("[vscode-pvs-xterm] log", data);
        const message: XTermMessage = {
            command: XTermCommands.log,
            data
        };
        this.panel?.webview?.postMessage(message);
        if (opt?.sessionEnd) {
            // write a closing messages.
            if (this.sessionType === "evaluator") {
                message.data = " Evaluator session terminated.";
                this.panel?.webview?.postMessage(message);
            } else if (this.sessionType === "prover" && data.includes("bye!")) {
                message.data = " Prover session terminated.";
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
    async renderView (): Promise<boolean> {
        const success: boolean = await this.createWebView();
        return success;
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
     * Sets the running flag in xterm, which indicates that the prover is running a command
     */
    running (flag: boolean): void {
        if (this.runningFlag !== !!flag) {
            this.runningFlag = !!flag
            const message: XTermMessage = {
                command: XTermCommands.running,
                data: this.runningFlag
            };
            this.panel?.webview?.postMessage(message);
        }
    }
    /**
     * Shows feedback while executing a proof command
     */
    showFeedbackWhileExecuting (cmd: "run-proof" | "fast-forward" | "rewind" | string, target?: string): void {
        if (cmd && !utils.isQuitCommand(cmd)) {
            if (cmd === "run-proof") {
                this.running(true);
                this.showHelpMessage(`Executing all commands in the proof tree..${DOT_BLINK}<br>To interrupt the execution, press Ctrl+C`);
                commands.executeCommand("vscode-pvs.progress-info", "Executing all commands in the proof tree.");
            } else if (cmd === "fast-forward") {
                this.running(true);
                this.showHelpMessage(`Fast-forward to ${target}..${DOT_BLINK}<br>To interrupt the execution, press Ctrl+C`);
                commands.executeCommand("vscode-pvs.progress-info", `Fast-forward to ${target}.`);
            } else if (cmd === "rewind") {
                this.running(true);
                this.showHelpMessage(`Rewinding to ${target}..${DOT_BLINK}<br>To interrupt the execution, press Ctrl+C`);
                commands.executeCommand("vscode-pvs.progress-info", `Rewinding to ${target}.`);
            } else {
                this.running(true);
                this.showHelpMessage(`Executing ${cmd}..${DOT_BLINK}<br>To interrupt the execution, press Ctrl+C`);
                commands.executeCommand("vscode-pvs.progress-info", `Executing ${cmd}.`);
            }
        }
    }
    /**
     * Shows welcome message in the prover terminal
     */
    showWelcomeMessage (): void {
        if (!this.runningFlag) {
            const message: XTermMessage = {
                command: XTermCommands.showWelcomeMessage,
                data: this.prompt
            };
            this.panel?.webview?.postMessage(message);
            commands.executeCommand("vscode-pvs.progress-info");
        }
    }
    /**
     * Shows a help message in the integrated help panel
     */
    showHelpMessage (msg: string): void {
        const message: XTermMessage = {
            command: XTermCommands.showHelpMessage,
            data: msg
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
     * Set/Reset autocomplete with Enter flag
     */
    autocompleteWithEnter (flag: boolean): void {
        const message: XTermMessage = {
            command: XTermCommands.autocompleteWithEnter,
            data: flag
        };
        this.panel?.webview?.postMessage(message);
    }
    /**
     * Utility function, shows all available commands and some brief info for each command
     */
    helpStar (): void {
        const message: XTermMessage = {
            command: XTermCommands.helpStar
        };
        this.panel?.webview?.postMessage(message);
    }
    /**
     * Utility function, shows all available commands and some brief info for each command
     */
    helpVSCodePlot (): void {
        const message: XTermMessage = {
            command: XTermCommands.helpVSCodePlot
        };
        this.panel?.webview?.postMessage(message);
    }
    /**
     * Internal function, disables prover and evaluator handlers
     */
    protected disableHandlers (): void {
        this.client.onRequest(serverEvent.proofCommandResponse,() => {
            
        });
        this.client.onRequest(serverEvent.evaluatorCommandResponse, () => {

        });
    }
    /**
     * Internal function, creates the webview
     */
    protected createWebView (): Promise<boolean> {
        return new Promise ((resolve, reject) => {
            if (this.sessionType) {
                const title: string = this.sessionType === "prover" ?
                    `Proving '${(<PvsFormula> this.target).formulaName}'`
                        : `Evaluating ${this.target.theoryName}`;
                if (this.panel) {
                    this.panel.title = title;
                    // reveal the panel
                    this.panel.reveal(ViewColumn.Active, false); // false allows the webview to steal the focus
                    resolve(true);
                } else {
                    this.panel = this.panel || window.createWebviewPanel(
                        'vscode-pvs.xterm-pvs', // Identifies the type of the webview. Used internally
                        title, // Title of the panel displayed to the user
                        ViewColumn.Active, // Editor column to show the new webview panel in.
                        {
                            enableScripts: true,
                            retainContextWhenHidden: true,
                            enableFindWidget: true
                        }
                    );
                    try {
                        this.panel.iconPath = {
                            light: Uri.file(path.join(__dirname, "..", "..", "..", "icons", "pvs-file-icon.png")),
                            dark: Uri.file(path.join(__dirname, "..", "..", "..", "icons", "pvs-file-icon.png"))
                        };
                        // clean up data structures when webview is disposed
                        this.panel.onDidDispose(
                            () => {
                                // delete panel
                                this.panel = null;
                                // send quit command to the server
                                this.sendTextToServer("quit");
                                // reset session type
                                this.sessionType = null;
                                // clear target
                                this.target = null;
                                // reset global vscode-pvs variables so other views can be updated properly
                                vscodeUtils.resetGlobals();
                            },
                            null,
                            this.context?.subscriptions
                        );
                        this.panel.onDidChangeViewState(
                            (evt: WebviewPanelOnDidChangeViewStateEvent) => {
                                if (evt?.webviewPanel?.active) {
                                    this.focusxterm();
                                }
                            }
                        );
                        // Handle messages from the webview
                        this.panel.webview.onDidReceiveMessage(
                            async (message: XTermMessage) => {
                                // console.log("[vscode-xterm] Received message", message);
                                if (message) {
                                    switch (message.command) {
                                        case XTermEvent.sendText: {
                                            if (message?.data) {
                                                // check if this is a meta command
                                                if (message.data === interruptCommand) {
                                                    commands.executeCommand("vscode-pvs.interrupt-prover");
                                                } else if (utils.isQuitCommand(message.data) && this.sessionType === "prover") {
                                                    if (this.proofExplorer?.proofIsDirty()) {
                                                        // ask if the proof needs to be saved
                                                        const yesNoCancel: YesNoCancel = await this.proofExplorer.queryQuitProofAndSave();
                                                        if (yesNoCancel === "cancel") {
                                                            this.showPrompt();
                                                        }
                                                    } else {
                                                        const actionConfirmed: boolean = await this.proofExplorer.queryQuitProof({ force: true });
                                                        if (!actionConfirmed) {
                                                            this.showPrompt();
                                                        }
                                                    }
                                                } else if (utils.isHelpStarCommand(message.data)) {
                                                    this.helpStar();
                                                    this.showPrompt();
                                                } else if (utils.isHelpVSCodePlot(message.data)) {
                                                    this.helpVSCodePlot();
                                                    this.showPrompt();
                                                } else if (utils.isVSCodePlotCommand(message.data)) {
                                                    const expr: string = utils.getVSCodePlotExpression(message.data);
                                                    commands.executeCommand("vscode-pvs.plot-expression", { ...this.target, expr });
                                                    this.showPrompt();
                                                } else if (utils.isPVSioweb(message.data)) {
                                                    commands.executeCommand("vscode-pvs.pvsio-web", this.target, { force: true });
                                                    this.showPrompt();
                                                } else {
                                                    // send command to the server
                                                    this.showFeedbackWhileExecuting(message.data);
                                                    await this.sendTextToServer(message.data);
                                                    if (!utils.isQuitCommand(message.data)) {
                                                        this.running(false);
                                                        this.showWelcomeMessage();
                                                    }    
                                                }
                                            }
                                            break;
                                        }
                                        case XTermEvent.didCopyText: {
                                            if (message?.data?.length) {
                                                vscodeUtils.showStatusBarMessage(`Selected text copied to clipboard.`);
                                                // vscodeUtils.showInformationMessage(`Selected text copied to clipboard.`);
                                            }
                                            break;
                                        }
                                        case XTermEvent.click: {
                                            this.focus();
                                            break;
                                        }
                                        case XTermEvent.closeConsole: {
                                            this.dispose();
                                            break;
                                        }
                                        case XTermEvent.proofExplorerBack:
                                        case XTermEvent.proofExplorerForward:
                                        case XTermEvent.proofExplorerRun: 
                                        case XTermEvent.proofExplorerEdit: {
                                            commands.executeCommand(message.command);
                                            break;
                                        }
                                        case XTermEvent.ready: {
                                            resolve(message?.data);
                                            break;
                                        }
                                        default: {
                                            break;
                                        }
                                    }
                                }
                            },
                            undefined,
                            this.context?.subscriptions
                        );
                        // Create webview content
                        this.createContent();
                        // reveal the panel 
                        this.panel.reveal(ViewColumn.Active, false); // false allows the webview to steal the focus
                        // set language to pvs
                        vscodeUtils.setEditorLanguagetoPVS();
                        // set terminal visible to true
                        commands.executeCommand('setContext', 'terminal.visible', true);
                    } catch (err) {
                        console.error(err);
                        resolve(false);
                    }
                }
            }
        });
    }
    /**
     * Internal function, creates the html content of the webview
     */
    protected createContent (): void {
        if (this.panel) {
            // set webview content
            const xtermPvsJsOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/out/extra/xterm-pvs.min.js'));
            const xtermCssOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/xterm/css/xterm.css'));

            const bootstrapJsOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/bootstrap/dist/js/bootstrap.bundle.min.js'));
            const bootstrapCssOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/bootstrap/dist/css/bootstrap.min.css'));
            
            const backboneOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/backbone/backbone-min.js'));
            const underscoreOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/underscore/underscore-min.js'));
            const handlebarsOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/handlebars/dist/handlebars.min.js'));
    
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
                this.panel.webview.asWebviewUri(handlebarsOnDisk),
                this.panel.webview.asWebviewUri(bootstrapJsOnDisk),
                this.panel.webview.asWebviewUri(underscoreOnDisk),
                this.panel.webview.asWebviewUri(backboneOnDisk),
                this.panel.webview.asWebviewUri(xtermPvsJsOnDisk)
            ];

            try {
                const content: string = Handlebars.compile(getHtmlTemplate(this.sessionType), { noEscape: true })({
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
                        XTermCommands.showWelcomeMessage,
                        XTermCommands.updateColorTheme,
                        XTermCommands.showHelpMessage,
                        XTermCommands.running,
                        XTermCommands.autocompleteWithEnter,
                        XTermCommands.helpStar,
                        XTermCommands.helpVSCodePlot
                    ],
                    xtermEvents: [
                        XTermEvent.sendText,
                        XTermEvent.closeConsole,
                        XTermEvent.proofExplorerBack,
                        XTermEvent.proofExplorerForward,
                        XTermEvent.proofExplorerRun,
                        XTermEvent.proofExplorerEdit,
                        XTermEvent.didCopyText,
                        XTermEvent.click
                    ],
                    sessionType: this.sessionType
                });
                this.panel.webview.html = content;
            } catch (err) {
                console.error(err);
            }
        }
    }
}