/**
 * @module PvsCliGateway
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

import * as WebSocket from 'ws';
import { PvsLanguageServer } from './pvsLanguageServer'
import { CliGatewayRequest, CliGatewayEvent, CliGatewaySubscriberEvent, serverEvent, ProofExecDidQuitProof } from './common/serverInterface';
import { Connection } from 'vscode-languageserver';

/**
 * PvsCliGateway provides a websocket gateway to the language server.
 * The main purpose for now is to allow connection of (client) CLI interfaces.
 * Clients can use it to subscribe to server events (see serverInterface.serverEvent) and send proof commands
 */
export class PvsCliGateway {
    protected wsServer: WebSocket.Server;
	protected port: number = 33445;
	protected MAX_PORT_ATTEMPTS: number = 200;
	
	// websocket clients
    protected pvsCli: { [ key: string ]: { [ clientID: string ]: WebSocket } } = {};
    protected vscodeTerminal: { [ key: string ]: { [ clientID: string ]: WebSocket } } = {};
	
	protected pvsLanguageServer: PvsLanguageServer;

	getPort (): number { return this.port; }

	/**
	* Checks availability of the port. Returns true if the port is available, false otherwise.
	* The check works as follows. A dummy server is created at port p; if the creation of the server succeeds, an event 'listening' is triggered, otherwise an event 'error' is triggered.
	* The server is turned off as soon as an answer is available.
	*/
    protected checkPort (port: number, retry: boolean): Promise<boolean> {
	   // console.info(`checking port ${p}`);
	   return new Promise((resolve, reject) => {
		   const server: WebSocket.Server = new WebSocket.Server({ port: this.port })
		   const timeout: number = 1000; // msec
		   server.once('error', (error: Error) => {
			//    console.error(error);
			   if (error["code"] === 'EADDRINUSE' && retry) {
				   console.log(`[pvs-cli-gateway] port ${port} busy, retrying after timeout of ${timeout} msec`);
				   retry = false; // retry just once on the same port
				   setTimeout(() => {
					   this.checkPort(port, false);
				   }, timeout);
			   } else {
				   console.log(`[pvs-proxy] port ${port} is not available :/`);
				   resolve(false);
			   }
		   });
		   server.once('listening', () => {
			   console.log(`[pvs-cli-gateway] port ${port} is available :)`);
			   server.once('close', () => {
				   resolve(true);
			   });
			   server.close();
		   });
	   });
	}


    constructor (pvsLanguageServer: PvsLanguageServer) {
		this.pvsLanguageServer = pvsLanguageServer;
	}

	publish (desc: CliGatewayEvent): void {
		if (desc && desc.channelID) {
			if (this.pvsCli[desc.channelID]) {
				const clientIDs: string[] = Object.keys(this.pvsCli[desc.channelID]);
				for (let i = 0; i < clientIDs.length; i++) {
					const subscriberEvent: CliGatewaySubscriberEvent = desc;
					this.pvsCli[desc.channelID][clientIDs[i]].send(JSON.stringify(subscriberEvent));
				}
			} 
		} else {
			console.error("[pvs-cli-gateway] Warning: received null or incomplete descriptor");
		}
	}

	async activate (): Promise<boolean> {
		if (this.wsServer) {
			return Promise.resolve(true);
		}
		return new Promise(async (resolve, reject) => {
			let portIsAvailable: boolean = false;
			for (let i = 0; !portIsAvailable && i < this.MAX_PORT_ATTEMPTS; i++) {
				portIsAvailable = await this.checkPort(this.port, false);
				if (portIsAvailable === false) {
					this.port++;
				}
			}
			if (portIsAvailable) {
				// create websocket server
				this.wsServer = new WebSocket.Server({ port: this.port });
				this.wsServer.on('connection', (wsClient: WebSocket) => {
					wsClient.on('message', (msg: string) => {
						// FIXME: declare these message types in serverInterface
						try {
							const data: CliGatewayRequest = JSON.parse(msg);
							if (data) {
								switch (data.type) {
									case "subscribe-vscode": {
										// console.info('[pvs-cli-gateway] received vscode-pvs-terminal subscription request');
										// console.info('[pvs-cli-gateway] channel ID = ', data.channelID);
										// console.info('[pvs-cli-gateway] client ID = ', data.clientID);
										this.vscodeTerminal[data.channelID] = this.vscodeTerminal[data.channelID] || {};
										this.vscodeTerminal[data.channelID][data.clientID] = wsClient;
										wsClient.send(JSON.stringify({
											type: "subscribe-response", success: true, channelID: data.channelID
										}));
										break;
									}
									case "subscribe": {
										// console.info('[pvs-cli-gateway] received pvs-CLI subscription request');
										// console.info('[pvs-cli-gateway] channel ID = ', data.channelID);
										// console.info('[pvs-cli-gateway] client ID = ', data.clientID);
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
										// console.info('[pvs-cli-gateway] received unsubscription request');
										// console.info('[pvs-cli-gateway] channel ID = ', data.channelID);
										// console.info('[pvs-cli-gateway] client ID = ', data.clientID);
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
													delete this.vscodeTerminal[data.channelID][keys[i]];
												}
											}
											delete this.vscodeTerminal[data.channelID];
											const connection: Connection = this.pvsLanguageServer.getConnection();
											if (connection) {
												const evt: ProofExecDidQuitProof = { action: "did-quit-proof" };
												connection.sendNotification(serverEvent.proverEvent, evt);
											}
										} else {
											console.error("[pvs-cli-gateway] Warning: could not find records of client that wants to unsubscribe");
										}
										break;
									}
									case "pvs.proof-command": {
										if (data && data.cmd) {
											// console.info('[pvs-cli-gateway] received new command from pvs-cli', data.cmd);
											data.cmd = (data.cmd.trim().startsWith("(")) ? data.cmd : `(${data.cmd.trim()})`;
											this.pvsLanguageServer.getProofExplorer().proofCommandRequest(data);
										}
										break;
									}
									case "pvs.evaluate-expression": {
										if (data && data.cmd) {
											// console.info('[pvs-cli-gateway] received new evaluation request from pvs-cli', data.cmd);
											this.pvsLanguageServer.evaluateExpressionRequest(data);
										}
										break;
									}
									case "pvs.select-profile": {
										if (data && data.profile) {
											// console.info('[pvs-cli-gateway] received profile change request', data.profile);
											const channels: string[] = Object.keys(this.pvsCli);
											for (let i = 0; i < channels.length; i++) {
												const clients: string[] = Object.keys(this.pvsCli[channels[i]]);
												for (let j = 0; j < clients.length; j++) {
													this.pvsCli[channels[i]][clients[j]].send(JSON.stringify({
														type: data.type,
														data: { profile: data.profile }
													}));
												}
											}
											// this.pvsLanguageServer.evaluationRequest(data);
										}
										break;
									}
									// case "publish": {
									// 	console.info('[pvs-cli-gateway] received request to forward message on channel ', data.channelID);
									// 	this.publish({ type: "publish", channelID: data.channelID, data: null });
									// }
									default: {
										console.error(`[pvs-cli-gateway] Warning: unknown message type ${data.type}`, msg);
									}
								}
							} else {
								console.error("[pvs-cli-gateway] Warning: data is null");
							}
						} catch (jsonError) {
							console.error("[pvs-cli-gateway] Warning: error while parsing json message", msg);
						}
					});
					wsClient.on('close', () => {
						// console.info('[pvs-cli-gateway] Terminal session end');
					});
					wsClient.on('error', (err) => {
						console.error('[pvs-cli-gateway] Terminal session error', err);
					});
				});
				this.wsServer.on('listening', () => {
					console.info(`[pvs-cli-gateway] Gateway awaiting terminal sessions on ws://0.0.0.0:${this.port}`);
					resolve(true);
				});
				this.wsServer.on('error', (err) => {
					console.error('[pvs-cli-gateway] Gateway error ', err);
					resolve(false);
				});
			} else {
				console.error(`[pvs-cli-gateway] Gateway could not find any available port after ${this.MAX_PORT_ATTEMPTS} attempts`);
				resolve(false);
			}
		});
	}}