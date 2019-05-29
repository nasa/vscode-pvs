/**
 * @module pvsInterpreter
 * @version 2019.03.14
 * Command line version of the pvs theorem prover.
 * BUG: unable to start the theorem prover, need to understand why.
 * 
Setting tmp dir to value of environment variable TMPDIR:
  /var/folders/n4/q42bj8nd5dd80q05k8s1gv0w0000gq/T/

pvs(1): 

t

pvs(2): 

t
pvs(3): 
(prove-formula "test_me" "thm" nil)

Parsing test

test parsed in 0.01 seconds

Typechecking test

foo_th typechecked in 0.01s: No TCCs generated

string_th typechecked in 0.05s: No TCCs generated; 4 conversions; 1 msg

Parsing lib

lib parsed in 0.00 seconds

Typechecking lib

lib typechecked in 0.00s: No TCCs generated

records_th typechecked in 0.01s: No TCCs generated

bool_th typechecked in 0.06s: No TCCs generated

list_example typechecked in 0.03s: 1 TCC, 0 proved, 0 subsumed, 1 unproved

bindings_th typechecked in 0.00s: No TCCs generated

test_me typechecked in 0.00s: No TCCs generated

test_th typechecked in 0.03s: No TCCs generated; 1 msg

Installing rewrite rule sets.singleton_rew (all instances)
:pvs-eval (pvs-checker-busy) :end-pvs-eval


 * @author Paolo Masci
 * @date 2019.03.14
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
import { spawn, ChildProcess } from 'child_process';
import * as fsUtils from './common/fsUtils';
import { Connection, TextDocument } from 'vscode-languageserver';
import * as utils from './common/languageUtils';
import * as readline from 'readline';

class Console {
	connection: Connection;
	constructor (connection?: Connection) {
		this.connection = connection;
	}
	log (str: string) {
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

/**
 * Wrapper class for PVS: spawns a PVS process, and exposes the PVS Lisp interface as an asyncronous JSON/RPC server.
 */
class PvsInterpreter {
	private pvsProcess: ChildProcess;
	private pvsProcessBusy: boolean = false;
	private pvsCmdQueue: Promise<string> = Promise.resolve("");
	private pvsExecutable: string;
	private pvsPath: string;
	private pvsContextFolder: string;
	private console: Console;
	private pvsCommands: { [key: string]: RegExp } = {
		"change-context": /\(change-context (.*)\)/g,
		"typecheck-file": /\(typecheck-file (.*)\)/g
	};

	private rl: readline.ReadLine;

	private async clearContext (): Promise<void> {
		const currentContext: string = this.pvsContextFolder;
		if (currentContext) {
			await fsUtils.deletePvsCache(currentContext);
		}
	}

	/**
	 * @constructor
	 * @param pvsExecutionContext PVS context 
	 * @param connection Connection with the language client
	 */
	constructor (pvsExecutable: string, connection?: Connection) {
		this.pvsExecutable = pvsExecutable;
		this.pvsPath = pvsExecutable.split("/").slice(0, -1).join("/");
		this.pvsContextFolder = __dirname;
		this.console = new Console(connection);
	}
	/**
	 * Utility function for executing pvs lisp commands using the pvs process. Unbuffered version.
	 * @param commandId Command name see list of commands in PvsLisp
	 * @param cmd PVS lisp command
	 */
	execUnbuffered(cmd: string): Promise<string> {
		if (this.pvsProcessBusy) {
			const msg: string = "PVS busy, cannot execute " + cmd + " :/";
			return Promise.resolve(msg);
		}	
		this.pvsProcessBusy = true;
		// connection.console.info("Executing command " + cmd);
		// this.console.log(cmd);
		return new Promise((resolve, reject) => {
			let pvsout: string = "";
			const listener = (data: string) => {
				this.console.log(data); // this is the crude output of pvs lisp
				pvsout += data;
				const PVS_COMINT_PROMPT_REGEXP: RegExp = /\s*pvs\(\d+\):|([\w\W\s]*)\spvs\(\d+\):/g;
				const PROVER_PROMPT: RegExp = /\bRule\?/g;
				const QUERY_YES_NO: RegExp = /\?\s*\(Y or N\)|\?\s*\(Yes or No\)/gi;
				const ready: boolean = PVS_COMINT_PROMPT_REGEXP.test(data)
										|| PROVER_PROMPT.test(data)
										|| QUERY_YES_NO.test(data);
				if (ready) {
					this.pvsProcess.stdout.removeListener("data", listener); // remove listener otherwise this will capture the output of other commands
					this.pvsProcessBusy = false;
					const ans: string = pvsout;
					pvsout = "";
					resolve(ans);
				}
			};
			this.pvsProcess.stdout.on("data", listener);
			this.pvsProcess.stderr.on("data", (data: string) => {
				this.console.error(data);
			});
			this.pvsProcess.stdin.write(cmd);
		});
	}
	/**
	 * Executes pvs lisp commands using the pvs process. Buffered version.
	 * @param commandId Command name  (see list of commands in PvsLisp)
	 * @param cmd PVS lisp command
	 */
	exec(cmd: string): Promise<string> {
		this.pvsCmdQueue = new Promise((resolve, reject) => {
			this.pvsCmdQueue.then(() => {
				this.execUnbuffered(cmd).then((ans: string) => {
					resolve(ans);
				});
			});
		});
		return this.pvsCmdQueue;
	}

	/**
	 * Starts the pvs process
	 */
	async start (): Promise<{}> {
		if (!this.pvsProcessBusy) {
			await this.clearContext();
			this.pvsProcessBusy = true;
			// const pvslispParser = new PvsLisp("pvs-init", { console: this.console });	
			const cmd: string = this.pvsExecutable;
			this.console.info("Spawning pvs process " + cmd);
			return new Promise((resolve, reject) => {
				this.pvsProcess = spawn(cmd, ["-raw"]);
				this.pvsProcess.stdout.setEncoding("utf8");
				this.pvsProcess.stderr.setEncoding("utf8");
				const waitPvsReadyPrompt = (data: string) => {
					this.console.log(data); // this is the crude output of pvs lisp
					const PVS_COMINT_PROMPT_REGEXP: RegExp = /\bpvs\(\d+\):|([\w\W\s]*)\spvs\(\d+\):/g;
					const ready: boolean = PVS_COMINT_PROMPT_REGEXP.test(data);
					if (ready) {
						this.pvsProcess.stdout.removeListener("data", waitPvsReadyPrompt); // remove listener otherwise this will capture the output of other commands
						this.pvsProcessBusy = false;
						this.disableGcPrintout();
						resolve();
					}
				};
				this.pvsProcess.stdout.on("data", waitPvsReadyPrompt);
				this.pvsProcess.stderr.on("data", (data: string) => {
					this.console.error(data);
				});
				this.console.info("PVS process ready!");
				// this.rl = readline.createInterface({
				// 	input: this.pvsProcess.stdin,
				// 	output: this.pvsProcess.stdout
				// });
			});
		}
	}

	/**
	 * Changes the current context. When the context is changed, all symbol information are erased and the parser/typechecker needs to be re-run.
	 * @param contextFolder Path to the pvs working directory 
	 */
	async changeContext(contextFolder: string): Promise<string> {
		this.pvsContextFolder = contextFolder;
		const cmd: string = '(change-context "' + contextFolder + '" t)';
		return await this.exec(cmd);
	}
	/**
	 * Returns the current context
	 * @returns Path of the current context
	 */
	async currentContext(): Promise<string> {
		const cmd: string = '(pvs-current-directory)';
		return (await this.exec(cmd));
	}

	/**
	 * Disables garbage collector messages
	 */
	private async disableGcPrintout(): Promise<string> {
		const cmd: string = '(setq *disable-gc-printout* t)';
		return await this.exec(cmd);
	}

	/**
	 * Typecheck file
	 * @param uri The uri of the file to be typechecked
	 * @param tcpFlag Optional flag, triggers automatic proof of tccs
	 */
	async typecheckFile(uri: string, tcpFlag?: boolean): Promise<string> {
		let fileName: string = fsUtils.getFilename(uri, { removeFileExtension: true });
		const cmd: string = (tcpFlag) ? 
			'(typecheck-file "' + fileName + '" nil t nil)'
				: '(typecheck-file "' + fileName + '" nil nil nil)';
		return await this.exec(cmd);
	}

}

// console.log(process.argv[2]);
async function start(pvsExecutable: string): Promise<PvsInterpreter> {
	const pvsInterpreter: PvsInterpreter = new PvsInterpreter(pvsExecutable);
	await pvsInterpreter.start();
	// await pvsProcess.emacsInterface(); --- NB: do not enable emacs interface, it will lock up the theorem prover
	process.stdin.on("data", async (data: Buffer) => {
		try {
			let cmd: string = data.toLocaleString();
			// console.log(`received command from keyboard: ${cmd}`);
			// surrounding parentheses are automatically added, if they are not provided
			// NB: need to handle response to questions like "are you sure Y N". These responses also need a carriage return at the end.
			// same applies for quit
			// if (!cmd.startsWith("(")
			// 		&& cmd.trim().toLocaleLowerCase() !== "y" && cmd.trim().toLocaleLowerCase() !== "n"
			// 		&& cmd.trim().toLocaleLowerCase() !== "yes" && cmd.trim().toLocaleLowerCase() !== "no"
			// 		&& cmd.trim().toLocaleLowerCase() !== "quit" && cmd.trim().toLocaleLowerCase() !== "quit;") { cmd = `(${cmd.trim()})`; }
			// console.log allow to write text in the terminal, readline allow to change stdout
			// readline.clearLine(process.stdout, 0);
			console.log(utils.colorText(cmd, utils.textColor.blue)); // re-introduce the command with colors and parentheses
			await pvsInterpreter.exec(cmd);
		} catch (err) {
			console.error(err);
		}
	});
	return pvsInterpreter;
}

if (process.argv.length > 2) {
	console.log(`args: ${JSON.stringify(process.argv)}`)
	start(process.argv[2]);
}



