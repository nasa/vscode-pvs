/**
 * @module vscodePvsioWeb
 * @author Paolo Masci
 * @date 2021.02.08
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

import { ExtensionContext, Uri, ViewColumn, WebviewPanel, window, workspace } from "vscode";
import * as path from 'path';
import { LanguageClient } from "vscode-languageclient";
import * as Handlebars from "handlebars";
import { PvsioEvaluatorCommand, PvsIoMode, PvsTheory, serverEvent, EvaluatorCommandResponse } from "../common/serverInterface";
import * as fsUtils from '../common/fsUtils';
// import { TerminalEvents, VSCodePvsTerminal } from "./vscodePvsTerminal";
import * as vscodeUtils from '../utils/vscode-utils';

import * as builderUtils from '../utils/builderUtils';
import { VSCodePvsXTerm, XTermPvsEvent } from "./vscodePvsXTerm";

const ioFileExt: string = ".io";
const webFileExt: string = ".web";
const pvsiowebFolder: string = "pvsioweb";

const dbg: boolean = false;
function log(...args: any): void {
    if (dbg) {
        console.log(args);
    }
}

enum WidgetEvents {
    DidSendRequest = "DidSendRequest"
};
enum builderCommands {
    loadPrototypeData = "loadPrototypeData",
    renderState = "renderState"
};
interface Message {
    command: string,
    data?: any
};

const defaultLabels = {
    mainModule: "Theory Name",
    mainFile: "PVS File"
};

const htmlTemplate: string = `
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{{title}}</title>
    {{#if style}}
    <style type="text/css">
    {{style}}
    </style>
    {{/if}}
    {{#each css}}
    <link rel="stylesheet" href="{{this}}">
    {{/each}}
    <!-- 
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap@4.5.3/dist/css/bootstrap.min.css" 
        integrity="sha384-TX8t27EcRE3e/ihU7zmQxVncDAy5uIKz4rEkgIXeMed4M0jlfIDPvg6uqKI2xXr2" 
        crossorigin="anonymous">
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.2/css/all.min.css">
    <script src="https://cdnjs.cloudflare.com/ajax/libs/jquery/3.5.1/jquery.min.js"></script>
    -->
</head>
<body style="overflow:auto; background:whitesmoke; font-size:small; padding-top:1em;">
    <div id="pvsio-web" class="container-fluid p-0" style="position:absolute;left:0;top:0;min-width:400px;"></div>
    
    {{#each js}}
    <script src="{{this}}"></script>
    {{/each}}

    <!-- widget libraries -->
    {{#each libs}}
    <script src="{{uri}}"></script>
    {{/each}}
    <!---->

    <script>
    // Handlers for messages exchanged between vscode and pvsioweb
    window.addEventListener('message', async (event) => {
        const message = event.data; // JSON data sent by vscode-pvs
        // console.log("[webview] Received message ", message);
        if (builder && message && message.command) {
            switch (message.command) {
                case "activate": {
                    const params = { parent: "#pvsio-web", ...message.data };
                    await builder.activate(params);

                    // Install handlers necessary to forward to vscode all events triggered by pvsioweb
                    const vscode = acquireVsCodeApi();

                    // console.log("builder", builder);
                    // console.log("connection", builder.connection);

                    // handlers for events sent by prototype builder on the connection
                    {{#each builderEvents}}
                    builder.on("{{this}}", (data) => {
                        const msg = {
                            command: "{{this}}",
                            data
                        };
                        // console.log(msg);
                        vscode.postMessage(msg);
                    });
                    {{/each}}

                    // handler for sendMessage events generated by widget
                    builder.connection.on("${WidgetEvents.DidSendRequest}", (data) => {
                        const msg = {
                            command: "${WidgetEvents.DidSendRequest}",
                            data
                        };
                        // console.log(msg);
                        vscode.postMessage(msg);
                    });

                    // import widget libraries
                    try {
                        {{#each libs}}
                        builder.importWidgetLibrary({{name}});
                        {{/each}}
                    } catch (libError) {
                        console.warn("[webview] Warning: unable to import widget library", libs);
                    } finally {
                        // trigger DidActivatePlugin event
                        vscode.postMessage({
                            command: '${builderUtils.PrototypeBuilderEvents.DidActivatePlugin}'
                        });
                    }
                    break;
                }
                {{#each builderCommands}}
                case "{{this}}": {
                    // console.log("[webview] {{this}} data", message.data);
                    if (message.data) {
                        if (typeof builder["{{this}}"] === "function") {
                            await builder["{{this}}"](message.data);
                        }
                    }
                    break;
                }
                {{/each}}
                default: {
                    break;
                }
            }
        }
    });

    // ready event
    window.addEventListener('load', async (event) => {
        vscode.postMessage({
            command: "ready",
            data: true
        });
    });
    </script>
</body>
</html>`;

interface TickConfig { fun: string, freq: number };

export class VSCodePvsioWeb {
    // vscode panel
    protected panel: WebviewPanel;
    // vscode client
    protected client: LanguageClient;
    // vscode context
    protected context: ExtensionContext;

    // dispose view callback
    protected disposeCallback: () => void;

    // evaluator response callback
    protected evaluatorResponseCallback: (res: EvaluatorCommandResponse) => void;

    // theory associated with the pvsioweb session
    protected theory: PvsTheory;
    // terminal associated with the pvsioweb session
    protected xterm: VSCodePvsXTerm;

    // title of the webview
    readonly title: string = "PVSio-Web Prototyping Toolkit";

    // prototype data
    protected data: builderUtils.PrototypeData;

    // tick timer
    protected tickTimer: NodeJS.Timer = null;
    readonly MIN_TICK_FREQ: number = 250; //ms
    readonly DEFAULT_TICK_FREQ: number = 1000; //ms

    // keeps track of whether the init command has actually been sent to the server
    protected stateMachineInitialized: boolean = false; // whether the state machine is initialized

    /**
     * Constructor
     * @param client 
     */
    constructor (client: LanguageClient) {
		this.client = client;
        this.evaluatorResponseCallback = (res: EvaluatorCommandResponse) => {
            this.renderState(res?.state)
        }
    }
    /**
     * Activate the webview
     * @param context 
     */
    activate (context: ExtensionContext): void {
        this.context = context;
    }
    /**
     * Reveal the webview
     */
    async reveal (): Promise<void> {
        if (!this.panel) {
            await this.renderView();
        }
        this.panel.reveal()
    }
    /**
     * Hide the webview
     */
    async dispose (): Promise<void> {
        if (this.panel) {
            return new Promise ((resolve, reject) => {
                this.disposeCallback = () => {
                    this.disposeCallback = null;
                    resolve();
                };
                this.panel.dispose();
            });
        }
    }
    /**
     * Defines the theory associated with the pvsioweb session
     * @param theory 
     */
    setTheory (theory: PvsTheory): void {
        this.theory = theory;
    }
    /**
     * Connects pvsioweb to a terminal with pvsio
     * @param terminal 
     */
    setTerminal (terminal: VSCodePvsXTerm): void {
        this.xterm = terminal;
        this.xterm.on(XTermPvsEvent.DidCloseTerminal, async () => {
            await this.dispose();
        });
        this.xterm.on(XTermPvsEvent.DidReceiveEvaluatorResponse, (data: EvaluatorCommandResponse) => {
            if (this.evaluatorResponseCallback) {
                this.evaluatorResponseCallback(data);
            }
        });
    }
    /**
     * Internal function, determines the base64 image type based on the file extension
     */
    protected getImageType (fname: string): string {
        let extName: string = fsUtils.getFileExtension(fname);
        if (extName) {
            extName = extName.substr(1);
            return extName === "jpg" ? "jpeg" : extName; 
        }
        return null;
    }
    /**
     * Internal function, tries to load the content of the .io file in to data
     */
    protected async loadIoFile (): Promise<boolean> {
        if (this.theory?.theoryName && this.theory?.contextFolder) {
            const prototypeFolder: string = path.join(this.theory.contextFolder, pvsiowebFolder);
            const pvsFile: string = fsUtils.desc2fname(this.theory);
            // read content of .io file
            const ioFileName: string = `${this.theory.theoryName}${ioFileExt}`;
            const ioFilePath: string = path.join(prototypeFolder, ioFileName);
            const ioFileContent: string = await fsUtils.readFile(ioFilePath) || "{}";
            try {
                const rawData: { [key:string]: any } = JSON.parse(ioFileContent);
                const ioData: builderUtils.IoFile = {};
                for (let key in builderUtils.IoFileAttribute) {
                    // mainModule and mainFile should be consistent with this.theory
                    switch (key) {
                        case "mainModule": {
                            ioData[key] = {
                                value: this.theory.theoryName,
                                label: defaultLabels.mainModule
                            };
                            break;                 
                        }
                        case "mainFile": {
                            ioData[key] = {
                                value: fsUtils.getFileName(pvsFile, { keepExtension: true }),
                                label: defaultLabels.mainFile
                            };
                            break;
                        }
                        default: {
                            ioData[key] = rawData[key];
                            break;
                        }
                    }
                }
                const loadedData: builderUtils.PrototypeData = {
                    ...this.data,
                    ...ioData
                };
                this.data = loadedData;
                return true;
            } catch (loadError) {
                console.warn(`[vscode-pvsioweb] Warning: unable to load .io file content for theory ${this.theory?.theoryName}`);
                return false;
            }
        }
        return false;
    }
    /**
     * Internal function, tries to load the content of the .web file in to data
     */
    protected async loadWebFile (): Promise<boolean> {
        if (this.theory?.theoryName && this.theory?.contextFolder) {
            const prototypeFolder: string = path.join(this.theory.contextFolder, pvsiowebFolder);
            // read content of .web file
            const webFileName: string = `${this.theory.theoryName}${webFileExt}`;
            const webFilePath: string = path.join(prototypeFolder, webFileName);
            const webFileContent: string = await fsUtils.readFile(webFilePath) || "{}";
            try {
                const rawData: { [key:string]: any } = JSON.parse(webFileContent);
                const webData: builderUtils.WebFile = {};
                for (let key in builderUtils.WebFileAttribute) {
                    webData[key] = rawData[key];
                }
                const loadedData: builderUtils.PrototypeData = {
                    ...this.data,
                    ...webData
                };
                this.data = loadedData;
                // try to load the picture data, if any picture is specified in the file
                if (webData.pictureFile) {
                    const fname: string = webData.pictureFile;
                    const imageType: string = this.getImageType(fname);
                    const pictureFile: string = path.join(prototypeFolder, fname);
                    const imageData: string = await fsUtils.readFile(pictureFile, { encoding: "base64" });
                    if (imageData) {
                        // convert binary data to base64 encoded string
                        const pictureData: string = 'data:image/' + imageType + ';base64,' + imageData;
                        loadedData.pictureData = pictureData;
                    }
                }
                return true;
            } catch (loadError) {
                console.warn(`[vscode-pvsioweb] Warning: unable to load .io file content for prototype ${this.theory?.theoryName}`);
                return false;
            }
        }
        return false;
    }
    /**
     * Loads a prototype in the webview
     */
    async loadPrototype (): Promise<boolean> {
        if (this.theory) {
            this.data = {
                contextFolder: this.theory.contextFolder
            };
            await this.loadIoFile();
            await this.loadWebFile();
            // send a load prototype request to the webview
            this.panel?.webview?.postMessage({
                command: builderCommands.loadPrototypeData,
                data: this.data
            });
            return true;
        }
        return false;
    }
    /**
     * Send a command to the evaluator
     * @param command 
     */
    async sendCommand (command: string, opt?: { mode?: PvsIoMode }): Promise<string> {
        opt = opt || {};
        if (this.xterm) {
            // make sure the command ends with ';'
            let cmd: string = command.trim();
            if (!cmd.endsWith(";")) { cmd = cmd + ";"; }
            log(`[vscode-pvsioweb] Sending command`, cmd);

            // create request
            const req: PvsioEvaluatorCommand = {
                ...this.theory,
                cmd,
                mode: opt.mode,
                sendResponse: true, // this flag tells the server to send an evaluatorCommandResponse
                showCommandInTerminal: true // this flag tells the server to show both the command and the result in the pvsio terminal
            };

            // send request through xterm and wait for callback
            const oldHandler = this.evaluatorResponseCallback;
            this.xterm.sendEvaluatorCommand(req);
            const ans: string = await new Promise ((resolve, reject) => {
                this.evaluatorResponseCallback = (data: EvaluatorCommandResponse) => {
                    log(`[vscode-pvsioweb] Received data from pvsio`, data);
                    this.renderState(data?.state);
                    resolve(data?.state);
                }
            });
            this.evaluatorResponseCallback = oldHandler;
            return ans;
        }
        console.warn(`[vscode-pvsioweb] Warning: terminal session is closed -- cannot send command`, command);
        return null;
    }
    /**
     * Save pvsioweb files
     */
    async savePVSioWebFiles (): Promise<boolean> {
        let success: boolean = await this.saveIoFile();
        success = success && await this.saveWebFile();
        return success
    }
    /**
     * Save the current hotspots shown in the webview
     */
    async saveIoFile (): Promise<boolean> {
        if (this.data) {
            const folder: string = path.join(this.theory.contextFolder, pvsiowebFolder);

            // save descriptor <theory-name>.hot
            const fname: string = `${this.theory.theoryName}${ioFileExt}`;
            const content: builderUtils.IoFile = builderUtils.getIoFile(this.data);
            if (content) {
                const success: boolean = await fsUtils.writeFile(path.join(folder, fname), JSON.stringify(content, null, " "));
                if (success) {
                    // vscodeUtils.showInformationMessage(`Prototype widgets updated successfully!`);
                } else {
                    vscodeUtils.showWarningMessage(`Warning: Unable to save file ${fname}`);
                }
                return success;
            }
        }
        return false;
    }
    /**
     * Save the current prototype shown in the webview
     */
    async saveWebFile (): Promise<boolean> {
        if (this.data) {
            const folder: string = path.join(this.theory.contextFolder, pvsiowebFolder);

            // save descriptor <theory-name>.web
            const fname: string = `${this.theory.theoryName}${webFileExt}`;
            const content: builderUtils.WebFile = builderUtils.getWebFile(this.data);
            if (content) {
                let success: boolean = await fsUtils.writeFile(path.join(folder, fname), JSON.stringify(content, null, " "));
                if (success) {
                    // vscodeUtils.showInformationMessage(`Prototype settings updated successfully!`);
                } else {
                    vscodeUtils.showWarningMessage(`Warning: Unable to save file ${fname}`);
                }
                return success;
            }
        }
        return false;
    }
    /**
     * Save picture
     * @param picture 
     */
    async savePictureFile (): Promise<boolean> {
        if (this.data) {
            const folder: string = path.join(this.theory.contextFolder, pvsiowebFolder);
            // save picture
            const pictureFile: string = this.data.pictureFile;
            if (pictureFile) {
                const pictureData: string = this.data.pictureData?.split(';base64,').pop();
                const success: boolean = await fsUtils.writeFile(path.join(folder, pictureFile), pictureData, { encoding: "base64" });
                if (success) {
                    // vscodeUtils.showInformationMessage(`Prototype picture updated successfully!`);
                } else {
                    vscodeUtils.showWarningMessage(`Warning: Unable to save file ${pictureFile}`);
                }
                return success;    
            }
        }
        return false;
    }
    /**
     * send a request to render widgets to pvsioweb
     */
    renderState (state: string): void {
        if (state) {
            const message: Message = {
                command: builderCommands.renderState,
                data: state
            };
            this.panel?.webview?.postMessage(message);
        }
    }
    /**
     * Internal function, checks if the returned state is an evaluation error
     * @param state 
     */
    protected evalutionError (state: string): boolean {
        return state?.includes("Expecting an expression")
            || state?.includes("does not uniquely resolve")
            || state?.includes("Parser error");
    }
    /**
     * Pause simulation
     */
    pauseSimulation (): void {
        // stop timers
        this.stopTick();
    }
    /**
     * Stop periodic execution of the tick function
     */
    stopTick (): void {
        if (this.tickTimer) {
            clearInterval(this.tickTimer);
            this.tickTimer = null;
        }
    }
    /**
     * Internal function, returns the tick configuration (function name and frequency) if they are valid
     */
    protected getTickConfig (): TickConfig {
        const tickFun: builderUtils.IoFileAttribute = builderUtils.IoFileAttribute.tickFunction;
        const fun: string = this.data[tickFun].value;
        if (fun) {
            const tickInterval: builderUtils.IoFileAttribute = builderUtils.IoFileAttribute.tickInterval;
            const value: string = this.data[tickInterval]?.value;
            if (value) {
                const v: number = builderUtils.getFrequency(value);
                const freq: number = v < this.MIN_TICK_FREQ ? this.MIN_TICK_FREQ : v;
                if (!isNaN(freq)) {
                    return { fun, freq }
                }
            }
        }
        return null;
    }
    /**
     * Start periodic execution of the tick function
     */
    startTick (): void {
        const initialTickConfig: TickConfig = this.getTickConfig();
        if (initialTickConfig) {
            // make sure the timer is not active
            this.stopTick();
            // start timer
            if (initialTickConfig.freq) {
                this.tickTimer = setInterval(async () => {
                    // check again the name of the function, in the case the user has updated the name during the simulation run
                    const tickConfig: TickConfig = this.getTickConfig();
                    if (tickConfig) {
                        if (initialTickConfig?.freq !== tickConfig?.freq || initialTickConfig?.fun !== tickConfig?.fun) {
                            // config has changed, restart tick
                            this.stopTick();
                            this.startTick();
                        } else {
                            // config is the same, keep ticking
                            await this.sendCommand(tickConfig.fun, { mode: "state-machine" });
                        }
                    } else {
                        // invalid config, stop tick
                        this.stopTick();
                    }
                }, initialTickConfig.freq);
            }
        }
    }
    /**
     * Starts the execution of tick after a delay. The default delay is the tick frequency
     */
    delayedStartTick (delay?: number): void {
        const tickConfig: TickConfig = this.getTickConfig();
        delay = delay || tickConfig?.freq;
        if (tickConfig && delay) {
            setTimeout(() => {
                this.startTick();
            }, delay)
        } else {
            this.startTick();
        }
    }
    /**
     * Reboots the evaluator
     */
    async rebootEvaluator (): Promise<void> {
        log(`[vscode-pvsioweb] Rebooting evaluator...`);
        await this.xterm.rebootEvaluatorSession(this.theory);
        log(`[vscode-pvsioweb] Reboot complete!`);        
    }
    /**
     * Send init command for the simulation
     */
    async sendInit (): Promise<string> {
        const init: builderUtils.IoFileAttribute = builderUtils.IoFileAttribute.initialState;
        if (this.data[init]?.value) {
            // send init to the server if the initial state is defined. 
            const initState: string = this.data[init].value;
            const state: string = await this.sendCommand(initState); // sendCommand will register a listere for serverEvent.evaluatorCommandResponse
            this.stateMachineInitialized = true;
            return state;
        } 
        // else {
        //     // register serverEvent.evaluatorCommandResponse listener so evaluator responses 
        //     // can be rendered in the prototype

            
        //     this.client.onRequest(serverEvent.evaluatorCommandResponse, (data: {
        //         req: PvsioEvaluatorCommand,
        //         res: string,
        //         state: string
        //     }) => {
        //         log(`[vscode-pvsioweb] Received data from pvsio`, data);
        //         this.renderState(data?.state);
        //     });
            // this.stateMachineInitialized = false;
        // }
        return null;
    }
    /**
     * Internal function, creates the webview
     */
    protected createWebView (): Promise<boolean> {
        return new Promise ((resolve, reject) => {
            if (this.panel) {
                this.panel.title = this.title;
                // reveal the panel
                this.panel.reveal(ViewColumn.Active, false); // false allows the webview to steal the focus
                resolve(true);            
            } else {
                this.panel = this.panel || window.createWebviewPanel(
                    'vscode-pvs.pvsioweb', // Identifies the type of the webview. Used internally
                    this.title, // Title of the panel displayed to the user
                    ViewColumn.Active, // Editor column to show the new webview panel in.
                    {
                        enableScripts: true,
                        retainContextWhenHidden: true
                    }
                );
                // clean up data structures when webview is disposed
                this.panel.onDidDispose(
                    async () => {
                        // save all files
                        await this.savePVSioWebFiles();
                        // reset initialization flag
                        this.stateMachineInitialized = false;
                        // delete panel
                        this.panel = null;
                        // exec dispose callback
                        if (this.disposeCallback) {
                            this.disposeCallback();
                        }
                    },
                    null,
                    this.context.subscriptions
                );
                // Handle messages from the webview
                this.panel.webview.onDidReceiveMessage(
                    async (message: Message) => {
                        // all files are saved under <context-folder>/pvsioweb/
                        log(`[vscode-pvsio-web] Received new message`, message);
                        switch (message.command) {
                            // webview ready
                            case "ready": {
                                resolve (true);
                                break;
                            }
                            // builder activation event
                            case builderUtils.PrototypeBuilderEvents.DidActivatePlugin: {
                                await this.loadPrototype();
                                break;
                            }

                            // file menu
                            case builderUtils.PrototypeBuilderEvents.SavePrototype: {
                                if (message.data) {
                                    this.data = message.data;
                                    await this.saveIoFile();
                                    await this.saveWebFile();
                                    await this.savePictureFile();
                                }
                                break;
                            }

                            // run menu
                            case builderUtils.PrototypeBuilderEvents.PauseSimulation: {
                                this.pauseSimulation();
                                break;
                            }
                            case builderUtils.PrototypeBuilderEvents.RebootSimulation: {
                                await this.rebootEvaluator();
                                await this.sendInit();
                                break;
                            }

                            // change events
                            case builderUtils.PrototypeBuilderEvents.DidChangePicture: {
                                // remove old picture, if present and file path different from the new one
                                const data: builderUtils.DidChangePictureEventData = message.data;
                                if (data?.old?.fileName && data?.old?.fileExtension
                                        && data?.old?.fileName !== data?.new?.fileName
                                        && data?.old?.fileExtension !== data?.new?.fileExtension
                                        && this.theory?.contextFolder) {
                                    // delete old picture
                                    const fname: string = path.join(this.theory.contextFolder, pvsiowebFolder, `${data.old.fileName}${data.old.fileExtension}`);
                                    fsUtils.deleteFile(fname);
                                }
                                if (data) {
                                    // remove extra fields
                                    delete data.old;
                                    delete data.new;
                                    // save prototype data
                                    this.data = data;
                                    await this.savePictureFile();
                                    await this.saveWebFile();
                                }
                                break;
                            }
                            case builderUtils.PrototypeBuilderEvents.DidUpdateWidgets: {
                                if (message.data) {
                                    this.data = message.data;
                                    // autosave .web file
                                    await this.saveWebFile();
                                }
                                break;
                            }
                            case builderUtils.PrototypeBuilderEvents.DidUpdateSettings: {
                                if (message.data) {
                                    this.data = message.data;
                                    // autosave .io file
                                    await this.saveIoFile();
                                    await this.saveWebFile();
                                }
                                break;
                            }
                            case builderUtils.PrototypeBuilderEvents.DidRemovePicture: {
                                if (message.data) {
                                    const data: builderUtils.DidRemovePictureEventData = message.data;
                                    if (data.old && this.theory?.contextFolder) {
                                        // delete old picture
                                        const fname: string = path.join(this.theory.contextFolder, pvsiowebFolder, `${data.old.fileName}${data.old.fileExtension}`);
                                        fsUtils.deleteFile(fname);
                                        // remove extra fields
                                        delete data.old;
                                        // save prototype data
                                        this.data = data;
                                        await this.saveWebFile();
                                    }
                                }
                                break;
                            }
                            
                            // switch events
                            case builderUtils.PrototypeBuilderEvents.DidSwitchToBuilderView: {
                                // stop timers
                                this.pauseSimulation();
                                break;
                            }
                            case builderUtils.PrototypeBuilderEvents.DidSwitchToSimulatorView: {
                                if (message.data) {
                                    // send init to server
                                    this.data = message.data;
                                    if (!this.stateMachineInitialized) {
                                        await this.sendInit();
                                    }
                                    // start tick
                                    this.delayedStartTick();
                                }
                                break;
                            }
                            case builderUtils.PrototypeBuilderEvents.DidSwitchToSettingsView: {
                                // stop timers
                                this.pauseSimulation();
                                break;
                            }

                            // widget events
                            case WidgetEvents.DidSendRequest: {
                                // message.data is a widget action
                                const action: {
                                    type: "sendCommand" | string,
                                    req: any
                                } = message.data;
                                const init: builderUtils.IoFileAttribute = builderUtils.IoFileAttribute.initialState;
                                if (action?.type === "sendCommand" && action?.req?.command) {
                                    // state-machine mode is enabled only when initialState is defined
                                    const mode: PvsIoMode = (this.data && this.data[init]?.value) ? "state-machine" : "standard";
                                    // make sure the state machine is initialized
                                    if (mode === "state-machine" && !this.stateMachineInitialized) {
                                        await this.sendInit();
                                    }
                                    await this.sendCommand(action.req.command, { mode });
                                }
                                break;
                            }
                            default: {
                                break;
                            }
                        }
                    },
                    undefined,
                    this.context.subscriptions
                );
                // Create webview content
                this.createContent();
                // reveal the panel 
                this.panel.reveal(ViewColumn.Active, false); // false allows the webview to steal the focus
                // set language to pvs
                vscodeUtils.setEditorLanguagetoPVS();                
            }
        });
    }
    /**
     * Internal function, creates the html content of the webview
     * @param root 
     */
    protected createContent (): void {
        // set webview content
        const bootstrapJsOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/bootstrap/dist/js/bootstrap.bundle.min.js'));
        const bootstrapCssOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/bootstrap/dist/css/bootstrap.min.css'));
        const jqueryOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/jquery/dist/jquery.min.js'));
        const fontawesomeCssOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/font-awesome/css/font-awesome.min.css'));

        const backboneOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/backbone/backbone-min.js'));
        const underscoreOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/underscore/underscore-min.js'));
        const handlebarsOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/handlebars/dist/handlebars.min.js'));
        const pvsiowebOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/pvsioweb.prototype-builder.min/prototype-builder.min.js'));

        const widgetLibs: string = vscodeUtils.getConfiguration("pvs.xtras.pvsioweb.widgetLibs");
        const libFiles: string[] = widgetLibs ? widgetLibs.split(":").map(folder => {
            return fsUtils.tildeExpansion(folder);
        }) : [];
        const currentRootFolder: string = workspace.workspaceFolders && workspace.workspaceFolders.length ? fsUtils.tildeExpansion(workspace.workspaceFolders[0].uri?.fsPath) : null;
        const libs: { name: string, uri: Uri }[] = libFiles.filter(fname => {
            // accept only paths that are in the current workspace, otherwise vscode will trigger a security exception
            const contextFolder: string = fsUtils.getContextFolder(fname);
            return fsUtils.fileExists(fname) && contextFolder?.startsWith(currentRootFolder);
        }).map(fname => {
            let name: string = fsUtils.getFileName(fname);
            name = name.endsWith(".min") ? name.substr(0, name.length - 4) : name;
            return { name, uri: this.panel.webview.asWebviewUri(Uri.file(fname)) };
        });
        const css: Uri[] = [
            this.panel.webview.asWebviewUri(bootstrapCssOnDisk),
            this.panel.webview.asWebviewUri(fontawesomeCssOnDisk)
        ];
        const js: Uri[] = [
            this.panel.webview.asWebviewUri(jqueryOnDisk), // jquery needs to be loaded before bootstrap
            this.panel.webview.asWebviewUri(handlebarsOnDisk),
            this.panel.webview.asWebviewUri(bootstrapJsOnDisk),
            this.panel.webview.asWebviewUri(underscoreOnDisk),
            this.panel.webview.asWebviewUri(backboneOnDisk),
            this.panel.webview.asWebviewUri(pvsiowebOnDisk)
        ];
        this.panel.webview.html = this.createHtmlContent({ css, js, libs });

        const mainFile: string = this.theory?.fileName && this.theory?.fileExtension ? this.theory.fileName + this.theory.fileExtension : "";
        const contextFolder: string = this.theory?.contextFolder || "";
        const theoryName: string = this.theory?.theoryName || "";
        const ioSettings: builderUtils.IoFile = {
            ...builderUtils.defaultIoSettings,
            mainModule: { label: defaultLabels.mainModule, value: theoryName },
            mainFile: { label: defaultLabels.mainFile, value: mainFile }
        };
        const settings: builderUtils.SettingsOptions = {
            contextFolder,
            io: ioSettings
        };
        const message: Message = {
            command: "activate",
            data: { settings }
        };
        this.panel?.webview?.postMessage(message);
    }
    /**
     * Renders the content of the webview
     */
    async renderView (): Promise<boolean> {
        const success: boolean = await this.createWebView();
        return success;
    }
    /**
     * Refresh the content of the webview
     */
    // refreshView (): void {
    //     // create webview
    //     this.createWebView();
    //     // create webview content
    //     this.createContent();
    //     // set language to pvs
    //     vscodeUtils.setEditorLanguagetoPVS();
    // }
    /**
     * Creates the html rendered in the webview
     * @param root Proof tree
     * @param opt Options
     * <li>css: css style files;</li>
     * <li>js: js files;</li>
     * <li>style: inline css style</li>
     */
    protected createHtmlContent (opt?: { css?: Uri[], js?: Uri[], libs?: { name: string, uri: Uri }[], style?: string }): string {
        const html: string = Handlebars.compile(htmlTemplate, { noEscape: true })({
            title: this.title,
            builderEvents: [
                // these events require saving files
                builderUtils.PrototypeBuilderEvents.SavePrototype,
                builderUtils.PrototypeBuilderEvents.DidChangePicture,
                builderUtils.PrototypeBuilderEvents.DidUpdateWidgets,
                builderUtils.PrototypeBuilderEvents.DidUpdateSettings,
                builderUtils.PrototypeBuilderEvents.DidRemovePicture,

                // these events produce mode changes in the webview
                builderUtils.PrototypeBuilderEvents.DidSwitchToBuilderView,
                builderUtils.PrototypeBuilderEvents.DidSwitchToSimulatorView,
                builderUtils.PrototypeBuilderEvents.DidSwitchToSettingsView,

                // these events may require sending init / reboot commands to the server
                builderUtils.PrototypeBuilderEvents.PauseSimulation,
                builderUtils.PrototypeBuilderEvents.RebootSimulation
            ],
            builderCommands,
            ...opt
        });
        return html;
    }
}