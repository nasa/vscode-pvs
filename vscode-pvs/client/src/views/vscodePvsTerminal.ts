/**
 * @module vscodePvsTerminal
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
import { LanguageClient } from 'vscode-languageclient';
import { PvsCliInterface, serverCommand, serverEvent } from '../common/serverInterface';
import * as language from '../common/languageUtils';
import * as WebSocket from 'ws';
import * as fsUtils from '../common/fsUtils';

export const PVS_CLI_FILE: string = 'pvsCli'; // pvsCli file name


class TerminalSession {
    protected cliFileName: string; // full path to the PvsCli file
    terminal: vscode.Terminal;
    protected client: LanguageClient;
    protected channelID: string;
    protected wsClient: WebSocket;
    protected clientID: string;
    protected pvsPath: string;

    protected isActive: boolean = false;

    /**
     * Constructor
     * @param client VSCode Language Client, necessary for sending requests to pvs-server
     * @param channelID Name of the channel where the terminal is listening
     */
    constructor (desc: { client: LanguageClient, channelID: string, pvsPath: string }) {
        this.client = desc.client;
        this.channelID = desc.channelID;
        this.clientID = fsUtils.get_fresh_id();
        this.pvsPath = desc.pvsPath;
    }
    async activate (
        context: vscode.ExtensionContext, 
        typeID: string, 
        desc: { 
            fileName: string, 
            fileExtension: string, 
            contextFolder: string, 
            theoryName: string, 
            formulaName?: string 
        }
    ): Promise<void> {
        return new Promise(async (resolve, reject) => {
            await this.subscribe(() => {
                resolve();
            });
            const terminalName: string = (desc) ? 
                desc.formulaName ? `${desc.fileName}@${desc.theoryName}.${desc.formulaName}` : `${desc.fileName}@${desc.theoryName}`
                    : typeID;
            const cliFileName: string = context.asAbsolutePath(path.join('server', 'out', PVS_CLI_FILE));
            const cliArgs: PvsCliInterface = {
                type: typeID,
                fileName: desc.fileName,
                fileExtension: desc.fileExtension,
                contextFolder: desc.contextFolder,
                theoryName: desc.theoryName,
                formulaName: desc.formulaName,
                channelID: this.channelID,
                pvsPath: this.pvsPath
            };
            this.terminal = vscode.window.createTerminal(terminalName, 'node', [ cliFileName, JSON.stringify(cliArgs) ]);
            this.terminal.show();
            this.isActive = true;

            // server events
            this.client.onRequest(serverEvent.quitDontSaveProofEvent, (desc: {
                args: { 
                    fileName: string, 
                    fileExtension: string, 
                    contextFolder: string, 
                    theoryName: string, 
                    formulaName: string, 
                    cmd: string 
                }
            }) => {
                this.isActive = false;
                vscode.commands.executeCommand('setContext', 'prover-session-active', false);
            });
            this.client.onRequest(serverEvent.closeDontSaveEvent, (desc: {
                args: { 
                    fileName: string, 
                    fileExtension: string, 
                    contextFolder: string, 
                    theoryName: string, 
                    formulaName: string, 
                    cmd: string 
                },
                msg?: string
            }) => {
                this.isActive = false;
                if (desc && desc.msg) {
                    this.terminal.sendText(desc.msg);
                }
                this.terminal.sendText("quit");
                vscode.commands.executeCommand('setContext', 'prover-session-active', false);
            });
        });
    }
    async subscribe (readyCB: () => void): Promise<boolean> {
        return new Promise((resolve, reject) => {
			this.wsClient = new WebSocket("ws://0.0.0.0:33445");
			this.wsClient.on("open", () => {
                this.wsClient.send(JSON.stringify({ type: "subscribe-vscode", channelID: this.channelID, clientID: this.clientID }));
			});
			this.wsClient.on("message", (msg: string) => {
				// console.log(msg);
				try {
					const data = JSON.parse(msg);
					if (data) {
						switch (data.type) {
							case "subscribe-response": {
                                resolve(data.success);
								break;
                            }
                            case "cli-ready": {
                                if (data.channelID !== this.channelID) {
                                    console.error(`[vscode-pvs-terminal] Error: received message on channel ${data.channelID} (was expecting on channel ${this.channelID})`);
                                }
                                readyCB();
                                break;
                            }
                            case "cli-end": {
                                if (data.channelID !== this.channelID) {
                                    console.error(`[vscode-pvs-terminal] Error: received message on channel ${data.channelID} (was expecting on channel ${this.channelID})`);
                                }
                                this.disposeTerminal();
                                break;
                            }
							default: {
								console.error("[vscode-pvs-terminal] Error: received unknown message type", data);
							}
						}
					} else {
						console.error("[vscode-pvs-terminal] Error: received empty message");
					}
				} catch (jsonError) {
					console.error("[vscode-pvs-terminal] Error: ", jsonError);
				}
            });
            this.wsClient.on("error", (err: Error) => {
                console.error(err);
                resolve(false);
            });
		});
    }
    selectProfile (desc: { profile: ProofMateProfile }): void {
        this.wsClient.send(JSON.stringify({
            type: "pvs.select-profile",
            profile: desc.profile
        }));
    }
    protected sendText(cmd: string): void {
        this.terminal.sendText(cmd);
    }
    disposeTerminal (): void {
        // hide all other terminals?
        // if (vscode.window.terminals && vscode.window.terminals.length) {
        //     for (let i = 0; i < vscode.window.terminals.length; i++) {
        //         vscode.window.terminals[i].hide();
        //     }
        // }
        // hide terminal
        this.terminal.hide();
        // dispose this terminal
        this.terminal.dispose();
        // close proof explorer and proofmate
        vscode.commands.executeCommand('setContext', 'prover-session-active', false);        
    }
    sendCommand (cmd: string) {
        this.sendText(cmd);
    }
    deactivate(): void {
        this.isActive = false;
    }
    async quitCommand (): Promise<void> {
        if (this.isActive) {
            this.sendCommand("quit");
            return new Promise((resolve, reject) => {
                this.client.onRequest(serverEvent.quitDontSaveProofEvent, (request: { 
                    fileName: string, 
                    fileExtension: string, 
                    contextFolder: string, 
                    theoryName: string, 
                    formulaName: string, 
                    cmd: string 
                }) => {
                    resolve();
                });
            });
        }
    }
}

import { cliSessionType } from '../common/serverInterface';
import { ProofMateProfile } from '../common/commandUtils';

export class VSCodePvsTerminal {
    protected client: LanguageClient;
    protected context: vscode.ExtensionContext;
    protected openTerminals: { [key: string]: TerminalSession } = {};
    /**
     * Constructor
     * @param client Language client 
     */
    constructor (client: LanguageClient) {
        this.client = client;
        vscode.window.onDidCloseTerminal((terminal) => {
            const keys: string[] = Object.keys(this.openTerminals);
            if (keys && keys.length > 0) {
                for (const i in keys) {
                    if (this.openTerminals[keys[i]].terminal.processId === terminal.processId) {
                        this.client.sendRequest(serverCommand.quitProver);
                        delete this.openTerminals[keys[i]];
                        // the following will hide proof explorer and proofmate
                        vscode.commands.executeCommand('setContext', 'prover-session-active', false);
                        break;
                    }
                }
            }
        });
    }
    selectProfile(desc: { profile: ProofMateProfile }): void {
        if (desc) {
            // change profile on all open terminals
            const keys: string[] = Object.keys(this.openTerminals);
            if (keys && keys.length > 0) {
                for (const i in keys) {
                    this.openTerminals[keys[i]].selectProfile(desc);
                }
            }
        }
    }
    sendProofCommand (desc: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string, cmd: string }): void {
        if (desc) {
            const channelID: string = language.desc2id(desc);
            if (this.openTerminals[channelID]) {
                this.openTerminals[channelID].terminal.show();
                this.openTerminals[channelID].terminal.sendText(desc.cmd);
            }
        }
    }
    activate (context: vscode.ExtensionContext) {
        this.context = context;
    }
    deactivate () {
        const keys: string[] = Object.keys(this.openTerminals);
        if (keys.length > 0) {
            // close all prover sessions first -- the current version of pvs-server supports one prover session at a time
            for (let i = 0; i < keys.length; i++) {
                const openTerminal: TerminalSession = this.openTerminals[keys[i]];
                openTerminal.deactivate();
            }
        }
    }
    protected info(msg: string) {
        vscode.window.showInformationMessage(msg);
    }
    protected error(msg: string) {
        vscode.window.showErrorMessage(msg);
    }
    async startProverSession (desc: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string }) {
        if (desc) {
            // !important: create a new command line interface first, so it can subscribe to events published by the server
            const channelID: string = language.desc2id(desc);
            const pvsPath: string = vscode.workspace.getConfiguration().get("pvs.path");

            const keys: string[] = Object.keys(this.openTerminals);
            if (keys.length > 0) {
                // close all prover sessions first -- the current version of pvs-server supports one prover session at a time
                for (let i = 0; i < keys.length; i++) {
                    const openTerminal: TerminalSession = this.openTerminals[keys[i]];
                    await openTerminal.quitCommand();
                    openTerminal.disposeTerminal();
                }
            }
            const pvsTerminal: TerminalSession = new TerminalSession({ client: this.client, channelID, pvsPath });
            await pvsTerminal.activate(this.context, cliSessionType.proveFormula, desc);
            this.openTerminals[channelID] = pvsTerminal;
            // send prove-formula request to pvs-server
            this.client.sendRequest(serverCommand.proveFormula, desc);
            // the proof script will be automatically loaded on the front-end when event serverEvent.proveFormulaResponse will be fired by the server
        }
    }
    async startEvaluatorSession (desc: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string }) {
        if (desc) {
            // !important: create a new command line interface first, so it can subscribe to events published by the server
            const channelID: string = language.desc2id(desc);
            const pvsPath: string = vscode.workspace.getConfiguration().get("pvs.path");
            if (this.openTerminals[channelID]) {
                this.openTerminals[channelID].terminal.show();
            } else {
                const pvsioTerminal: TerminalSession = new TerminalSession({ client: this.client, channelID, pvsPath });
                await pvsioTerminal.activate(this.context, cliSessionType.pvsioEvaluator, desc);
                this.openTerminals[channelID] = pvsioTerminal;
            }

            // send start-pvsio request to pvs-server
            this.client.sendRequest(serverCommand.startEvaluator, desc);
        }
    }
}