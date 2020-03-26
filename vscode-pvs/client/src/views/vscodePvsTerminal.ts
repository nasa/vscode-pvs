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
import { VSCodePvsWorkspaceExplorer } from './vscodePvsWorkspaceExplorer';
import { PvsCliInterface } from '../common/serverInterface';
import * as language from '../common/languageUtils';
import { serverEvent } from '../common/serverInterface';
import * as WebSocket from 'ws';
import * as fsUtils from '../common/fsUtils';

export const PVS_CLI_FILE: string = 'pvsCli'; // pvsCli file name


class TerminalSession {
    protected cliFileName: string; // full path to the PvsCli file
    terminal: vscode.Terminal;
    protected client: LanguageClient;
    protected active: boolean = true;
    protected channelID: string;
    protected workspaceExplorer: VSCodePvsWorkspaceExplorer;
    protected wsClient: WebSocket;
    protected clientID: string;
    protected proofExplorer: VSCodePvsProofExplorer;

    /**
     * Constructor
     * @param client VSCode Language Client, necessary for sending requests to pvs-server
     * @param channelID Name of the channel where the terminal is listening
     */
    constructor (client: LanguageClient, channelID: string, proofExplorer: VSCodePvsProofExplorer) {
        this.client = client;
        this.proofExplorer = proofExplorer;
        this.channelID = channelID;
        this.clientID = fsUtils.get_fresh_id();
    }
    async activate (context: vscode.ExtensionContext, typeID: string, desc: { fileName: string, fileExtension: string, contextFolder: string, theoryName?: string, formulaName?: string }): Promise<void> {
        return new Promise(async (resolve, reject) => {
            await this.subscribe(() => {
                resolve();
            });
            const terminalName: string = desc.formulaName || desc.fileName || typeID;
            const cliFileName: string = context.asAbsolutePath(path.join('server', 'out', PVS_CLI_FILE));
            const cliArgs: PvsCliInterface = {
                type: typeID,
                fileName: desc.fileName,
                fileExtension: desc.fileExtension,
                contextFolder: desc.contextFolder,
                theoryName: desc.theoryName,
                formulaName: desc.formulaName,
                channelID: this.channelID
            };
            this.terminal = vscode.window.createTerminal(terminalName, 'node', [ cliFileName, JSON.stringify(cliArgs) ]);
            this.terminal.show();
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
                                this.quit();
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
					console.error("[vscode-pvs-terminal] Error: received malformed json string", msg);
				}
            });
            this.wsClient.on("error", (err: Error) => {
                console.error(err);
                resolve(false);
            });
		});
    }
    protected sendText(cmd: string): void {
        if (this.active) {
            this.terminal.sendText(cmd);
        }
    }
    quit (): void {
        if (this.active) {
            // close terminal
            this.terminal.dispose();
            // close proof explorer and proofmate
            vscode.commands.executeCommand('setContext', 'prover-session-active', false);
        }
    }
    isActive() {
        return this.active;
    }
    stepCommand (cmd: string) {
        this.sendText(cmd);
    }
}

import { cliSessionType } from '../common/serverInterface';
import { VSCodePvsProofExplorer } from './vscodePvsProofExplorer';

export class VSCodePvsTerminal {
    protected client: LanguageClient;
    protected proofExplorer: VSCodePvsProofExplorer;
    protected context: vscode.ExtensionContext;
    protected activeTerminals: { [key: string]: TerminalSession } = {};
    /**
     * Constructor
     * @param client Language client 
     */
    constructor (client: LanguageClient, proofExplorer: VSCodePvsProofExplorer) {
        this.client = client;
        this.proofExplorer = proofExplorer;

        vscode.window.onDidCloseTerminal((terminal) => {
            const keys: string[] = Object.keys(this.activeTerminals);
            if (keys && keys.length > 0) {
                for (const i in keys) {
                    if (this.activeTerminals[keys[i]].terminal.processId === terminal.processId) {
                        this.activeTerminals[keys[i]].quit();
                        delete this.activeTerminals[keys[i]];
                        break;
                    }
                }
            }
        });
    }
    sendProofCommand (desc: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string, cmd: string }): void {
        if (desc) {
            const channelID: string = language.desc2id(desc);
            if (this.activeTerminals[channelID] && this.activeTerminals[channelID].isActive()) {
                this.activeTerminals[channelID].terminal.show();
                this.activeTerminals[channelID].terminal.sendText(desc.cmd);
            }
        }
    }
    activate (context: vscode.ExtensionContext) {
        this.context = context;
    }
    protected info(msg: string) {
        vscode.window.showInformationMessage(msg);
    }
    protected error(msg: string) {
        vscode.window.showErrorMessage(msg);
    }
    async startProveFormulaSession (desc: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string }) {
        if (desc) {
            const channelID: string = language.desc2id(desc);
            if (this.activeTerminals[channelID] && this.activeTerminals[channelID].isActive()) {
                this.activeTerminals[channelID].terminal.show();
            } else {
                const pvsTerminal: TerminalSession = new TerminalSession(this.client, channelID, this.proofExplorer);
                await pvsTerminal.activate(this.context, cliSessionType.proveFormula, desc);
                this.activeTerminals[channelID] = pvsTerminal;
            }
        }
    }
    async startPvsIoEvaluatorSession (desc: { fileName: string, fileExtension: string, contextFolder: string }) {
        // if (desc) {
        //     const channelID: string = language.desc2id(desc);
        //     if (this.activeTerminals[channelID] && this.activeTerminals[channelID].isActive()) {
        //         this.activeTerminals[channelID].terminal.show();
        //     } else {
        //         const pvsTerminal: TerminalSession = new TerminalSession(this.client, channelID);
        //         await pvsTerminal.activate(this.context, cliSessionType.pvsioEvaluator, desc);
        //         this.activeTerminals[channelID] = pvsTerminal;
        //     }
        // }
    }
}