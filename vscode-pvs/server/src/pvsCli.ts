/**
 * @module pvsCli
 * @author Paolo Masci
 * @date 2019.06.05
 * @copyright 
 * Copyright 2016 United States Government as represented by the
 * Administrator of the National Aeronautics and Space Administration. No
 * copyright is claimed in the United States under Title 17, 
 * U.S. Code. All Other Rights Reserved.
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
import { PvsCliInterface, PvsResponseType, PvsVersionDescriptor, SimpleConsole, StrategyDescriptor } from './common/serverInterface';

class CliConsole implements SimpleConsole {
	private connection: CliConnection = null;

	constructor (connection?: CliConnection) {
		this.connection = connection;
	}
	log (str: string,) {
		if (this.connection) {
			this.connection.console.log(str);
		} else {
			console.log(str);
		}
	}
	error (str: string) {
		if (this.connection) {
			this.connection.console.error(str);
		} else {
			console.error(str);
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

class CliConnection {
	public console: CliConsole;
	constructor () {
		this.console = new CliConsole();
	}
}

import { PvsProcess } from './pvsProcess';
import { PvsExecutionContext } from './common/serverInterface';
import { ConnectionError } from 'vscode-jsonrpc';

// utility function, ensures open brackets match closed brackets for commands
function parMatch(cmd: string): string {
	const openRegex: RegExp = new RegExp(/\(/g);
	const closeRegex: RegExp = new RegExp(/\)/g);
	let par: number = 0;
	while (openRegex.exec(cmd)) {
		par++;
	}
	while (closeRegex.exec(cmd)) {
		par--;
	}
	if (par > 0) {
		// missing closed brackets
		cmd = cmd.trimRight() + ')'.repeat(par);
		// console.log(`Mismatching parentheses automatically fixed: ${par} open round brackets without corresponding closed bracket.`)
	} else if (par < 0) {
		cmd = '('.repeat(-par) + cmd;
		// console.log(`Mismatching parentheses automatically fixed: ${-par} closed brackets did not match any other open bracket.`)
	}
	return cmd;
}

// utility function, ensures open brackets match closed brackets for commands
function quotesMatch(cmd: string): boolean {
	const quotesRegex: RegExp = new RegExp(/\"/g);
	let nQuotes: number = 0;
	while (quotesRegex.exec(cmd)) {
		nQuotes++;
	}
	return nQuotes % 2 === 0;
}


class PvsCli {
	private rl: readline.ReadLine;
	private pvsProcess: PvsProcess;

	private static completions: string[] = utils.PROVER_STRATEGIES_CORE.map((strat: StrategyDescriptor) => {
		return `(${strat.name}`;
	});

	private ctx: PvsExecutionContext;
	private fileName: string;
	private fileExtension: string;
	private theoryName: string;
	private formulaName: string;
	private line: number;

	private connection: CliConnection;

	private cmds: string[] = []; // queue of commands to be executed
	private tabCompleteMode: boolean = false;


	private outChannel (data: string) {
		console.log(data);
		const regex: RegExp = /:end-pvs-loc\b/;
		if (regex.test(data)) {
			// Status update for theory explorer
			this.pvsProcess.getTheoryStatus({ fileName: this.fileName, fileExtension: this.fileExtension, theoryName: this.theoryName });
		}
	}
	/**
	 * @constructor
	 * @param args information necessary to launch the theorem prover
	 */
	constructor (args: PvsCliInterface) {
		this.ctx = {
			pvsContextFolder: args.pvsContextFolder,
			pvsPath: args.pvsPath
		}
		this.fileName = args.fileName; // FIXME: fileName needs to include extension
		this.fileExtension = args.fileExtension;
		this.theoryName = args.theoryName;
		this.formulaName = args.formulaName;
		this.line = args.line;
		this.rl = readline.createInterface(process.stdout, process.stdin, this.completer);
		// this.rl.setPrompt(utils.colorText("Prover > ", utils.textColor.blue));
		this.rl.setPrompt(utils.colorText("", utils.textColor.blue));
		this.rl.on("line", (cmd: string) => {
			// console.log(`Received command ${cmd}`);
			try {
				// console.log(`received command from keyboard: ${cmd}`);
				if (quotesMatch(cmd)) {
					// clear current line and retype in blue
					// readline.moveCursor(process.stdin, 0, -1);
					// readline.clearScreenDown(process.stdin);
					cmd = parMatch(cmd);
					console.log(utils.colorText(cmd, utils.textColor.blue)); // re-introduce the command with colors and parentheses
					this.pvsProcess.execCmd(`${cmd}\n`);
				} else {
					console.log("Mismatching double quotes, please check your expression");
					this.pvsProcess.execCmd("()\n");
				}
			} catch (err) {
				console.error(err);
			}
		});
		this.connection = new CliConnection();
	}
	/**
	 * Utility function, creates a new pvs process
	 */
	async createPvsProcess(): Promise<PvsProcess> {
		const proc: PvsProcess = new PvsProcess(this.ctx);
		// proc.removeConnection();
		const success: boolean = await proc.pvs();
		if (success) {
			await proc.disableGcPrintout();
			await proc.changeContext(this.ctx.pvsContextFolder);
			// const ans: PvsResponseType = await proc.listProofStrategies();
			// if (ans && ans.res) {
			// 	const strategies: StrategyDescriptor[] = ans.res;
			// 	console.log(JSON.stringify(strategies));
			// 	PvsCli.completions = strategies.map((strat: StrategyDescriptor) => {
			// 		return `(${strat.name}`;
			// 	});
			// }
			return proc;
		}
		return null;
	}
	// async killPvsProcess(): Promise<boolean> {

	// }
	async startPvs (args: PvsCliInterface) {
		// console.log(utils.colorText(args.cmd, utils.textColor.blue));
		// TODO: check why we need to clear double the number of lines executed
		// readline.moveCursor(process.stdin, 0, -y);
		// readline.clearLine(process.stdin, 0);
		// readline.moveCursor(process.stdin, 0, -y);
		// readline.clearLine(process.stdin, 0);
		// setup stdin to emit 'keypress' events
		if (process.stdin.isTTY) {
			// this is necessary for correct identification of keypresses for navigation keys and tab
			process.stdin.setRawMode(true);
		}
		// readline.emitKeypressEvents(process.stdin);
		// process.stdin.on('keypress', (str, key) => {
		// 	if (key) {
		// 		switch (key.name) {
		// 			case "tab": {
		// 				this.tabCompleteMode = true;
		// 				break;
		// 			}
		// 			case "a": {
		// 				if (this.tabCompleteMode) {
		// 					this.tabCompleteMode = false;
		// 					this.pvsProcess.execCmd("(assert)");
		// 				}
		// 				break;
		// 			}
		// 		}
		// 	}
		// });
		this.cmds.push(args.cmd);
		// create pvs process
		this.pvsProcess = await this.createPvsProcess();
		if (this.pvsProcess) {
			// fetch pvs version information
			const ans: PvsResponseType = await this.pvsProcess.pvsVersionInformation();
			const versionInfo: PvsVersionDescriptor = {
				pvsVersion: ans.res.pvsVersion,
				lispVersion: ans.res.lispVersion
			};
			console.log(`${versionInfo.pvsVersion} ${versionInfo.lispVersion}`);
		} else {
			console.error(`could not start pvs :/`);
		}
	}
	async launchTheoremProver () {
		console.log(`Typechecking...`)
		// this.pvsProcess.setConnection(this.connection);
		const fileName: string = `${this.fileName}${this.fileExtension}`;
		if (this.fileExtension === ".pvs") {
			await this.pvsProcess.typecheckFile(fileName); // FIXME -- use object as argument instead of string
			this.pvsProcess.startCli((data: string) => {
				this.outChannel(data);
			});
			await this.pvsProcess.stepProof({ fileName: this.fileName, theoryName: this.theoryName, formulaName: this.formulaName, line: this.line });
			await this.pvsProcess.proveFormula({ fileName: this.fileName, fileExtension: this.fileExtension, theoryName: this.theoryName, formulaName: this.formulaName, line: this.line });
		} else {
			// .tccs file
			await this.pvsProcess.showTccs(fileName, this.theoryName);
			this.pvsProcess.startCli((data: string) => {
				this.outChannel(data);
			});
			await this.pvsProcess.stepTcc({ fileName: this.fileName, theoryName: this.theoryName, formulaName: this.formulaName, line: this.line });
			await this.pvsProcess.proveFormula({ fileName: this.fileName, fileExtension: this.fileExtension, theoryName: this.theoryName, formulaName: this.formulaName, line: this.line });
		}
		this.rl.prompt();
	}
	// on (event: "line", listener: (input: string) => void) {
	// 	this.rl.on("line", listener);
	// }
	// private loadingTheorem () {
	// 	// readline.cursorTo(process.stdin, 0, 0);
	// 	// readline.clearScreenDown(process.stdin);
	// 	console.log(`Loading theorem ${utils.colorText(this.formulaName, utils.textColor.blue)}`); // todo: display theory name
	// }
	ready () {
		this.rl.prompt();
	}
	prover (str: string) {
		this.rl.prompt();
		console.log(str);
	}
	private completer (line: string) {
		const hits = PvsCli.completions.filter((c) => c.startsWith(line));
		// Show all completions if none found
		// return [ hits.length ? hits : PvsCli.completions, line ];
		// show nothing if no completion is found
		return [ hits, line ];
	}
}

if (process.argv.length > 2) {
	const args: PvsCliInterface = JSON.parse(process.argv[2]);
	console.log(args);
	const pvsCli: PvsCli = new PvsCli(args);
	pvsCli.startPvs(args).then(async () => {
		await pvsCli.launchTheoremProver();
	});
}

