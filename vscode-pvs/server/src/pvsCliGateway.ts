/**
 * @module PvsCliProvider
 * @author Paolo Masci
 * @date 2019.09.06
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

import * as http from 'http';
import * as WebSocket from 'ws';
import { PvsResponse } from './common/pvs-gui';
import { PvsLanguageServer } from './pvsLanguageServer'
import { serverCommand } from './common/serverInterface';

/**
 * PvsCliGateway provides a websocket gateway to the language server.
 * The main purpose for now is to allow connection of (client) CLI interfaces.
 * Clients can use it to subscribe to server events (see serverInterface.serverEvent) and send proof commands
 */
export class PvsCliGateway {
    protected wsServer: WebSocket.Server;
    protected httpServer: http.Server;
	protected port: number = 33445;
	
	// websocket clients
    protected pvsCli: { [ key: string ]: { [ clientID: string ]: WebSocket } } = {};
    protected vscodeTerminal: { [ key: string ]: { [ clientID: string ]: WebSocket } } = {};
	
	protected pvsLanguageServer: PvsLanguageServer;

    constructor (pvsLanguageServer: PvsLanguageServer) {
		this.pvsLanguageServer = pvsLanguageServer;
	}

	publish (desc: { type: string, channelID: string, data: any }): void {
		if (desc && desc.type && desc.channelID && desc.data) {
			if (this.pvsCli[desc.channelID]) {
				const clientIDs: string[] = Object.keys(this.pvsCli[desc.channelID]);
				for (let i = 0; i < clientIDs.length; i++) {
					this.pvsCli[desc.channelID][clientIDs[i]].send(JSON.stringify({ type: desc.type, response: desc.data }));
				}
			} else {
				console.error(`[pvs-cli-gateway] Warning: message could not be forwarded on channel ${desc.channelID}`)
			}
		} else {
			console.error("[pvs-cli-gateway] Warning: received null descriptor");
		}
	}

	async activate (): Promise<boolean> {
		if (this.wsServer) {
			return Promise.resolve(true);
		}
		return new Promise((resolve, reject) => {
			this.httpServer = http.createServer();
			this.httpServer.listen(this.port, "0.0.0.0", () => {
				const url = `http://${this.httpServer.address().address}:${this.httpServer.address().port}`;
				console.dir(this.httpServer.address(), { depth: null });
				// console.info(`Server folder ${daaDisplaysRoot}`);
				console.info(`[pvs-cli-gateway] http server ready at ${url}`);
			});
			// add support for websocket connections
			this.wsServer = new WebSocket.Server({
				server: this.httpServer
			});
			this.wsServer.on('connection', (wsClient: WebSocket) => {
				console.info("[pvs-cli-gateway] New terminal session started");
				wsClient.on('message', (msg: string) => {
					// FIXME: declare these message types in serverInterface
					try {
						const data: { 
							type: "subscribe", clientID: string, channelID: string 
						} | { 
							type: "subscribe-vscode", clientID: string, channelID: string 
						} | {
							type: "unsubscribe", clientID: string, channelID: string
						} | { 
							type: "pvs.proof-command", fileName: string, fileExtension: string, contextFolder: string, 
							formulaName: string, theoryName: string, cmd: string 
						} | {
							type: "publish", channelID: string
						} = JSON.parse(msg);
						if (data) {
							switch (data.type) {
								case "subscribe-vscode": {
									console.info('[pvs-cli-gateway] received subscription request');
									console.info('[pvs-cli-gateway] channel ID = ', data.channelID);
									console.info('[pvs-cli-gateway] client ID = ', data.clientID);
									this.vscodeTerminal[data.channelID] = this.vscodeTerminal[data.channelID] || {};
									this.vscodeTerminal[data.channelID][data.clientID] = wsClient;
									wsClient.send(JSON.stringify({
										type: "subscribe-response", success: true, channelID: data.channelID
									}));
									break;
								}
								case "subscribe": {
									console.info('[pvs-cli-gateway] received subscription request');
									console.info('[pvs-cli-gateway] channel ID = ', data.channelID);
									console.info('[pvs-cli-gateway] client ID = ', data.clientID);
									this.pvsCli[data.channelID] = this.pvsCli[data.channelID] || {};
									this.pvsCli[data.channelID][data.clientID] = wsClient;
									wsClient.send(JSON.stringify({ type: "subscribe-response", success: true, channelID: data.channelID }));
									if (this.vscodeTerminal[data.channelID]) {
										// send cli-ready event to vscode subscribers, if any
										const keys: string[] = Object.keys(this.vscodeTerminal[data.channelID]);
										for (let i = 0; i < keys.length; i++) {
											this.vscodeTerminal[data.channelID][keys[i]].send(JSON.stringify({
												type: "cli-ready", channelID: data.channelID
											}));
										}
									}
									break;
								}
								case "unsubscribe": {
									console.info('[pvs-cli-gateway] received unsubscription request');
									console.info('[pvs-cli-gateway] channel ID = ', data.channelID);
									console.info('[pvs-cli-gateway] client ID = ', data.clientID);
									if (this.pvsCli[data.channelID] && this.pvsCli[data.channelID][data.clientID]) {
										this.pvsCli[data.channelID][data.clientID].close();
										delete this.pvsCli[data.channelID][data.clientID];
										if (this.vscodeTerminal[data.channelID]) {
											// send cli-end event to vscode subscribers, if any
											const keys: string[] = Object.keys(this.vscodeTerminal[data.channelID]);
											for (let i = 0; i < keys.length; i++) {
												this.vscodeTerminal[data.channelID][keys[i]].send(JSON.stringify({
													type: "cli-end", channelID: data.channelID
												}));
											}
										}	
									} else {
										console.error("[pvs-cli-gateway] Warning: could not find records of client that wants to unsubscribe");
									}
									break;
								}
								case "pvs.proof-command": {
									console.info('[pvs-cli-gateway] received new command from pvs-cli', data.cmd);
									this.pvsLanguageServer.proofCommandRequest(data);
									break;
								}
								case "publish": {
									console.info('[pvs-cli-gateway] received request to forward message on channel ', data.channelID);
									this.publish({ type: "publish", channelID: data.channelID, data: null });
								}
								default: {
									console.error("[pvs-cli-gateway] Warning: unknown message type", msg);
								}
							}
						} else {
							console.error("[pvs-cli-gateway] Warning: data is null");
						}
					} catch (jsonError) {
						// FIXME: investigate why this exception is being trigged from time to time
						// console.error("[pvs-cli-gateway] Warning: error while parsing json message", msg);
					}
				});
				wsClient.on('close', () => {
					console.info('[pvs-cli-gateway] Terminal session end');
				});
				wsClient.on('error', (err) => {
					console.error('[pvs-cli-gateway] Terminal session error', err);
				});
			});
			this.wsServer.on('listening', () => {
				console.log("[pvs-cli-gateway] WebSocket server ready!");
				resolve(true);
			});
			this.wsServer.on('error', (err) => {
				console.error('[pvs-cli-gateway] WebSocket server error ', err);
				resolve(false);
			});
		});
	}}