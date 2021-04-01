/**
 * @module PvsCli
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

import * as utils from './common/languageUtils';
import * as readline from 'readline';
import { PvsCliInterface, SimpleConsole, SimpleConnection, serverRequest, CliGatewaySubscriberEvent, ProofMateProfile, SequentDescriptor } from './common/serverInterface';
import * as fsUtils from './common/fsUtils';
import * as languageUtils from './common/languageUtils';

import { cliSessionType } from './common/serverInterface';
import { PvsProxy } from './pvsProxy';
import { PvsResponse } from './common/pvs-gui';

import * as WebSocket from 'ws';
import { colorText, PvsColor } from './common/colorUtils';
import { checkPar, EVALUATOR_COMMANDS, isQEDCommand, isQuitCommand, isSaveThenQuitCommand, isShowHiddenCommand, PROOF_COMMANDS_ADVANCED_PROFILE, PROOF_COMMANDS_BASIC_PROFILE } from './common/languageUtils';

const usage: string = `
${colorText("PVS Prover Command Line Interface (PVS-CLI)", PvsColor.blue)}
Usage: node pvsCli '{ "pvsPath": "<path-to-pvs-installation>", "contextFolder": "<context-folder>" }'
`;

class CliConsole implements SimpleConsole {
	protected connection: CliConnection = null;

	constructor (connection?: CliConnection) {
		this.connection = connection;
	}
	log (elem: any) {
		if (this.connection) {
			this.connection.console.log(elem);
		} else {
			console.log(elem);
		}
	}
	error (elem: any) {
		if (this.connection) {
			this.connection.console.error(elem);
		} else {
			console.error(elem);
		}
	}
	info (str: string) {
		if (this.connection) {
			this.connection.console.info(str);
		} else {
			console.info(str);
		}
	}
	warn (str: string) {
		if (this.connection) {
			this.connection.console.warn(str);
		} else {
			console.warn(str);
		}
	}
}

class CliConnection implements SimpleConnection {
	public console: CliConsole;
	constructor () {
		this.console = new CliConsole();
	}
	sendNotification (type: string, msg?: string): void { };
	sendRequest (type: string, data: any): void { };
}

let progressLevel: number = 0;
let progressMsg: string = "";
function showProgress (data: string) {
	progressLevel++;
	if (progressLevel > 4) {
		progressLevel = 0;
		readline.clearLine(process.stdin, 0);
	}
	console.log("  " + progressMsg + ".".repeat(progressLevel));
	readline.moveCursor(process.stdin, 0, -1);
}


class PvsCli {
	protected rl: readline.ReadLine;
	// protected pvsProcess: PvsProcess;
	protected isActive: boolean = false;

	protected mathObjects: { lemmas: string[], types: string[], definitions: string[] } = {
		lemmas: [],
		types: [],
		definitions: []
	};

	protected proofCommands: string[] = ["undo", "quit", "postpone"].concat(Object.keys(PROOF_COMMANDS_BASIC_PROFILE));
	protected evaluatorFunctions: string[] = Object.keys(EVALUATOR_COMMANDS);

	selectProfile (desc: { profile: ProofMateProfile }): void {
		if (desc && desc.profile) {
			switch (desc.profile) {
				case "advanced": {
					this.proofCommands = ["undo", "quit", "postpone"].concat(Object.keys(PROOF_COMMANDS_ADVANCED_PROFILE));
					break;
				}
				case "basic":
				default:
					{
					this.proofCommands = ["undo", "quit", "postpone"].concat(Object.keys(PROOF_COMMANDS_BASIC_PROFILE));
					break;
				}
			}
		}
	}
	// protected pvsPath: string;
	protected contextFolder: string;
	protected pvsProxy: PvsProxy;
	protected fileName: string;
	protected fileExtension: string;
	protected theoryName: string;
	protected formulaName: string;
	protected line: number;

	protected clientID: string;

	protected connection: CliConnection;

	protected tabCompleteMode: boolean = false;

	protected args: PvsCliInterface;

	protected proofState: string;

	protected mainContent: string = "";

	protected wsClient: WebSocket;

	protected proverPrompt: string = languageUtils.proverPrompt + " ";
	protected evaluatorPrompt: string = languageUtils.pvsioPrompt + " ";

	protected lines: string = "";

	/**
	 * @constructor
	 * @param args information necessary to launch the theorem prover
	 */
	constructor (args?: PvsCliInterface) {
		this.args = args;
		this.clientID = fsUtils.get_fresh_id();
	}
	// FIXME: the evaluator breaks into lisp when trying to evaluate malformed expressions, e.g., LET x = 1;
	async startEvaluatorCli (): Promise<void> {
		// read input file so we can autocomplete symbol names
		this.mainContent = await fsUtils.readFile(fsUtils.desc2fname(this.args));
		if (process.stdin.isTTY) {
			// this is necessary for correct handling of navigation keys and tab-autocomplete in the prover prompt
			process.stdin.setRawMode(true);
		}
		readline.emitKeypressEvents(process.stdin);
		// activate readline
		this.rl = readline.createInterface({ 
			output: process.stdout, 
			input: process.stdin, 
			completer: (line: string) => { 
				return this.evaluatorCompleter(line); 
			},
			removeHistoryDuplicates: true
		});
		this.rl.on("line", async (ln: string) => {
			if (this.isActive) {
				this.lines += " " + ln;
			}
		});
		this.isActive = true;
		let semicolorEntered: boolean = false;
		process.stdin.on("keypress", (input: string, key: readline.Key) => {
			try {
				if (key?.meta && key?.name === "up") {
					// move cursor up one line
					readline.moveCursor(process.stdin, 0, -1);
				}
				if (key?.meta && key?.name === "down") {
					// move cursor down one line
					readline.moveCursor(process.stdin, 0, 1);
				}
				if (key?.sequence.trim().endsWith(";")) {
					semicolorEntered = true;
				}
				if (key?.sequence.includes("\r")) {
					this.rl.setPrompt("");
					if (checkPar(this.lines)?.success && semicolorEntered) {
						const cmd: string = this.lines;
						semicolorEntered = false;
						this.lines = "";

						if (isQuitCommand(cmd)) {
							this.isActive = false;
							this.wsClient.send(JSON.stringify({
								type: serverRequest.evaluatorCommand,
								cmd: "quit",
								fileName: this.args.fileName,
								fileExtension: this.args.fileExtension,
								contextFolder: this.args.contextFolder,
								theoryName: this.args.theoryName,
								formulaName: this.args.formulaName
							}));	
							console.log();
							console.log("PVSio evaluator session terminated.");
							console.log();
							this.rl.question("Press Enter to close the terminal. ", () => {
								this.wsClient.send(JSON.stringify({ type: "unsubscribe", channelID: this.args.channelID, clientID: this.clientID }));
								this.wsClient.close();
							});
							return;
						} else {
							if (this.isActive) {
								this.rl.setPrompt(colorText(this.evaluatorPrompt, PvsColor.blue));
								this.wsClient.send(JSON.stringify({
									type: serverRequest.evaluatorCommand,
									cmd,
									fileName: this.args.fileName,
									fileExtension: this.args.fileExtension,
									contextFolder: this.args.contextFolder,
									theoryName: this.args.theoryName,
									formulaName: this.args.formulaName
								}));
							}
						}
					}
				}
			} catch (err) {
				console.error(err);
			}
		});
		this.connection = new CliConnection();
	}
	async startProverCli (): Promise<void> {
		if (process.stdin.isTTY) {
			// this is necessary for correct handling of navigation keys and tab-autocomplete in the prover prompt
			process.stdin.setRawMode(true);
		}
		readline.emitKeypressEvents(process.stdin);
		// activate readline
		this.rl = readline.createInterface({ 
			output: process.stdout, 
			input: process.stdin, 
			completer: (line: string) => { 
				return this.proverCompleter(line); 
			},
			removeHistoryDuplicates: true
		});
		this.rl.on("line", async (ln: string) => {
			if (this.isActive) {
				this.lines += " " + ln;
			}
		});
		this.isActive = true;
		process.stdin.on("keypress", (input: string, key: readline.Key) => {
			try {
				if (key && key.sequence.includes("\r")) {
					this.rl.setPrompt("");
					if (checkPar(this.lines)?.success) {
						const cmd: string = this.lines;
						this.lines = "";

						if (isSaveThenQuitCommand(cmd)) {
							console.log();
							console.log("Proof saved successfully!");
							console.log();
							console.log("Prover session terminated.");
							this.wsClient.send(JSON.stringify({
								type: serverRequest.proofCommand,
								cmd: "save-then-quit",
								fileName: this.args.fileName,
								fileExtension: this.args.fileExtension,
								contextFolder: this.args.contextFolder,
								theoryName: this.args.theoryName,
								formulaName: this.args.formulaName
							}));
							this.isActive = false;
							this.wsClient.send(JSON.stringify({ type: "unsubscribe", channelID: this.args.channelID, clientID: this.clientID }));
							this.wsClient.close();
							return;
						}
						if (isQuitCommand(cmd)) {
							console.log();
							console.log("Prover session terminated.");
							console.log();
							this.wsClient.send(JSON.stringify({
								type: serverRequest.proofCommand,
								cmd: "quit",
								fileName: this.args.fileName,
								fileExtension: this.args.fileExtension,
								contextFolder: this.args.contextFolder,
								theoryName: this.args.theoryName,
								formulaName: this.args.formulaName
							}));
							this.isActive = false;
							this.rl.question("Press Enter to close the terminal.", (answer: string) => {
								this.wsClient.send(JSON.stringify({ type: "unsubscribe", channelID: this.args.channelID, clientID: this.clientID }));
								this.wsClient.close();
							});
							return;
						}
						if (isQEDCommand(cmd)) {
							readline.moveCursor(process.stdin, 0, -1);
							readline.clearScreenDown(process.stdin);
							console.log();
							console.log(colorText("Q.E.D.", PvsColor.green));
							console.log();
							this.isActive = false;
							this.rl.question("Press Enter to close the terminal.", () => {
								this.wsClient.send(JSON.stringify({ type: "unsubscribe", channelID: this.args.channelID, clientID: this.clientID }));
								this.wsClient.close();
							});
							return;
						}
						if (utils.isHelpBangCommand(cmd) || utils.isHelpCommand(cmd)) {
							const match: RegExpMatchArray = new RegExp(utils.helpBangCommandRegexp).exec(cmd);
							if (match && match.length > 1) {
								this.wsClient.send(JSON.stringify({
									type: serverRequest.proofCommand, 
									cmd,
									fileName: this.args.fileName,
									fileExtension: this.args.fileExtension,
									contextFolder: this.args.contextFolder,
									theoryName: this.args.theoryName,
									formulaName: this.args.formulaName
								}));
							}
							return;
						}
						// if (utils.isHelpCommand(cmd)) {
						// 	console.log();
						// 	console.log(printHelp(cmd, { useColors: true }));
						// 	console.log();
						// 	// show prompt
						// 	this.rl.setPrompt(colorText(this.proverPrompt, PvsColor.blue));
						// 	this.rl.prompt();						
						// 	return;
						// }
						if (this.isActive) {
							this.rl.setPrompt(colorText(this.proverPrompt, PvsColor.blue));
							this.wsClient.send(JSON.stringify({
								type: serverRequest.proofCommand, 
								cmd,
								fileName: this.args.fileName,
								fileExtension: this.args.fileExtension,
								contextFolder: this.args.contextFolder,
								theoryName: this.args.theoryName,
								formulaName: this.args.formulaName
							}));
						}
					}
				}
			} catch (err) {
				console.error(err);
			}
		});
		// this.rl.on("pause", async () => {
		// 	console.log("PAUSE");
		// });
		// this.rl.on("resume", async () => {
		// 	console.log("RESUME");
		// });
		this.connection = new CliConnection();
	}
	async subscribe (channelID: string): Promise<boolean> {
        return new Promise((resolve, reject) => {
			const gatewayAddress: string = `ws://0.0.0.0:${this.args.gateway.port}`;
			this.wsClient = new WebSocket(gatewayAddress);
			this.wsClient.on("open", () => {
				// subscribe to cli gateway
				// const channelID: string = utils.desc2id({ fileName: this.args.fileName, theoryName: this.args.theoryName, formulaName: this.args.formulaName });
				// console.log(`Subscribing to ${terminalID}`);
                this.wsClient.send(JSON.stringify({ type: "subscribe", channelID, clientID: this.clientID }));
			});
			this.wsClient.on("message", (msg: string) => {
				// console.log(msg);
				try {
					const evt: CliGatewaySubscriberEvent = JSON.parse(msg);
					if (evt) {
						switch (evt.type) {
							case "subscribe-response": {
								// this.wsClient.send(JSON.stringify({ type: "publish", channelID: "event.cli-ready", clientID: this.clientID }));
								// console.log(`Client ${this.clientID} ready to receive events ${channelID}`);
								resolve(evt.success);
								break;
							}
							case "pvs.event.evaluator-ready": {
								if (evt.banner) {
									console.log(evt.banner + "\n");
								}
								this.rl.setPrompt(colorText(this.evaluatorPrompt, PvsColor.blue));
								this.rl.prompt();
								break;
							}
							case "pvs.event.prover-ready": {
								this.rl.setPrompt(colorText(this.proverPrompt, PvsColor.blue));
								this.rl.prompt();
								break;
							}
							case "pvs.event.evaluator-state": {
								const pvsResponse: PvsResponse = evt.data;
								if (pvsResponse) {
									if (pvsResponse.result) {
										this.rl.setPrompt("");
										console.log(utils.formatPvsIoState(pvsResponse.result, { useColors: true }));
										console.log("\n");
									}
								}
								// readline.moveCursor(process.stdin, 8, -1);
								// remove the standard prompt created by PVSio
								// readline.moveCursor(process.stdin, 0, -1);
								// readline.clearScreenDown(process.stdin);
								// show prompt
								this.rl.setPrompt(colorText(this.evaluatorPrompt, PvsColor.blue));
								this.rl.prompt();
								// readline.clearLine(process.stdin, 1); // clear any previous input
								break;
							}
							case "pvs.event.proof-state": {
								if (this.isActive) {
									const result: SequentDescriptor = evt.data;
									if (result) {
										const showHidden: boolean = isShowHiddenCommand(evt.cmd);
										if (showHidden) {
											console.log(utils.formatHiddenFormulas(result, { useColors: true, showAction: true })); // show proof state
										} else {
											// print commentary in the CLI
											if (result.commentary && typeof result.commentary === "object" && result.commentary.length > 0) {
												// the last line of the commentary is the sequent, dont' print it!
												for (let i = 0; i < result.commentary.length; i++) {
													console.log(result.commentary[i]);
												}
											}
											// update proof state -- for the completer
											this.proofState = utils.formatSequent(result);
											// print proof state using syntax highlighting 
											console.log(utils.formatSequent(result, { useColors: true, showAction: true })); // show proof state										
										}
										console.log();
										this.rl.prompt(); // show prompt
										readline.clearLine(process.stdin, 1); // clear any previous input
									} else {
										console.warn(`[pvs-cli] Warning: received null proof state from pvs-server`);
									}
								}
								break;
							}
							case "pvs.event.QED": {
								readline.moveCursor(process.stdin, 0, -1);
								readline.clearScreenDown(process.stdin);
								console.log();
								console.log(colorText("Q.E.D.", PvsColor.green));
								console.log();
								this.isActive = false;
								this.rl.question("Press Enter to close the terminal.", () => {
									this.wsClient.send(JSON.stringify({ type: "unsubscribe", channelID: this.args.channelID, clientID: this.clientID }));
									this.wsClient.close();
								});
								return;
							}
							case "pvs.event.quit": {
								readline.clearScreenDown(process.stdin);
								console.log("(quit)");
								console.log();
								this.isActive = false;
								this.rl.question("Press Enter to close the terminal.", () => {
									this.wsClient.send(JSON.stringify({ type: "unsubscribe", channelID: this.args.channelID, clientID: this.clientID }));
									this.wsClient.close();
								});
								return;
							}
							case "pvs.event.print-proof-command": {
								const data: { cmd: string } = evt.data;
								if (data && data.cmd) {
									console.log(data.cmd);
									console.log();
								} else {
									console.warn(`[pvs-cli] Warning: proof command could not be printed in the terminal`);
								}
								break;
							}
							case "gateway.publish.math-objects": {
								const pvsResponse: { lemmas: string[], types: string[], definitions: string[] } = evt.data;
								if (pvsResponse) {
									this.mathObjects = pvsResponse;
								}
								break;
							}
							case "pvs.select-profile": {
								if (evt && evt.data && evt.data.profile) {
									const profile: ProofMateProfile = <ProofMateProfile> evt.data.profile;
									this.selectProfile({ profile });
								}
								break;
							}
							
							// case serverEvent.typecheckFileResponse: {
							// 	const pvsResponse: PvsResponse = data.response;
							// 	if (pvsResponse.error) {
							// 		console.log(`${colorText("Typecheck error", PvsColor.red)}`);
							// 		console.dir(data.response, { colors: true, depth: null });
							// 	} else {
							// 		console.log(`[pvs-cli] Typechecking completed successfully!`);
							// 	}
							// 	this.rl.question("Press Enter to close the terminal", () => {
							// 		this.wsClient.send(JSON.stringify({ type: "unsubscribe", channelID, clientID: this.clientID }));
							// 		this.wsClient.close();	
							// 	});
							// 	break;
							// }
							default: {
								console.error("[pvs-cli] Warning: received unknown message type", evt);
							}
						}
					} else {
						console.error("[pvs-cli] Warning: received empty message");
					}
				} catch (jsonError) {
					// FIXME: investigate why sometimes we get malformed json
					// console.error("[pvs-cli] Error: received malformed json string", msg);
				}
            });
            this.wsClient.on("error", (err: Error) => {
                console.error(err);
                resolve(false);
            });
		});
	}
	
	protected proverCompleter (line: string) {
		let hits: string[] = [];
		// console.log(`[pvs-cli] trying to auto-complete ${line}`);
		if (line.startsWith("(expand") || line.startsWith("expand")
			|| line.startsWith("(rewrite") || line.startsWith("rewrite")
			|| line.startsWith("(eval-expr") || line.startsWith("eval-expr")) {
			// autocomplete symbol names
			const symbols: string[] = utils.listSymbols(this.proofState);
			// console.dir(symbols, { depth: null });
			const expandCommands: string[] = [
				"expand", "rewrite", "eval-expr"
			];
			if (symbols && symbols.length) {
				for (let i = 0; i < symbols.length; i++) {
					for (let j = 0; j < expandCommands.length; j++) {
						if (`(${expandCommands[j]} "${symbols[i]}"`.startsWith(line)) {
							hits.push(`(${expandCommands[j]} "${symbols[i]}"`);
						} else if (`${expandCommands[j]} "${symbols[i]}"`.startsWith(line)) {
							hits.push(`${expandCommands[j]} "${symbols[i]}"`);
						} 
					}
				}
			}
		} else if (line.startsWith("(lemma") || line.startsWith("lemma")
					|| line.startsWith("(apply-lemma") || line.startsWith("apply-lemma")) {
			if (this.mathObjects) {
				const symbols: string[] = this.mathObjects.lemmas;
				const lemmaCommands: string[] = [
					"lemma", "apply-lemma"
				];	
				if (symbols && symbols.length) {
					for (let i = 0; i < symbols.length; i++) {
						for (let j = 0; j < lemmaCommands.length; j++) {
							if (`(${lemmaCommands[j]} "${symbols[i]}"`.startsWith(line)) {
								hits.push(`(${lemmaCommands[j]} "${symbols[i]}"`);
							} else if (`${lemmaCommands[j]} "${symbols[i]}"`.startsWith(line)) {
								hits.push(`${lemmaCommands[j]} "${symbols[i]}"`);
							}							
						}
					}
				}
			}
		} else if (line.trim().startsWith("(")) {
			hits = this.proofCommands.map((c: string) => {
				// console.log(`(${c}`);
				return `(${c}`;
			}).filter((c: string) => c.startsWith(line));
		} else {
			// Show all completions if none found
			// return [ hits.length ? hits : PvsCli.completions, line ];
			// show nothing if no completion is found
			hits = this.proofCommands.filter((c: string) => c.startsWith(line));
		}
		return [ hits, line ];
	}
	protected evaluatorCompleter (line: string) {
		let hits: string[] = [];
		// autocomplete symbol names
		const symbols: string[] = utils.listSymbols(this.mainContent);
		// console.dir(symbols, { depth: null });
		if (symbols && symbols.length) {
			hits = symbols.filter((c: string) => c.startsWith(line));
		}
		// Include also pvsio functions in the list
		hits = hits.concat(this.evaluatorFunctions.filter((c: string) => c.startsWith(line)));
		return [ hits, line ];
	}
	protected notifyStartExecution (msg: string): void {
		if (this.connection) {
			this.connection.console.info(msg);
		}
	}
	protected notifyEndExecution (): void {
		if (this.connection) {
			this.connection.console.info("Ready!");
		}
	}
	protected qed (ans: PvsResponse): boolean {
		return ans && ans.result && ans.result.result && ans.result.result === "Q.E.D.";
	}
}


if (process.argv.length > 2) {
	// ATTN: the client must not print anything on the console until it's subscribed --- vscodePvsPvsCli checks the output on the console to understand when the client is ready.
	const args: PvsCliInterface = JSON.parse(process.argv[2]);
	readline.cursorTo(process.stdout, 0, 0);
	readline.clearScreenDown(process.stdout);
	const pvsCli: PvsCli = new PvsCli(args);
	pvsCli.subscribe(args.channelID).then(async (success: boolean) => {
		// console.log(args);
		if (success) {
			switch (args.type) {
				case cliSessionType.proveFormula: {
					console.log(`\nStarting new prover session for ${colorText(args.theoryName + "@" + args.formulaName, PvsColor.blue)}\n`);
					await pvsCli.startProverCli();
					break;
				}
				case cliSessionType.pvsioEvaluator: {
					console.log(`\nStarting new PVSio evaluator session for theory ${colorText(args.theoryName, PvsColor.blue)}\n`);
					await pvsCli.startEvaluatorCli();
					break;
				}
				default: {
					console.error("[pvsCli] Warning: unknown cli session type", args);
				}
			}
		} else {
			console.error("[pvs-cli] Error: unable to register client", args);
		}
	});
} else {
	console.log(usage);
}

