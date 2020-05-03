/**
 * @module PvsProcessLegacy
 * @author Paolo Masci
 * @date 2019.02.07
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

import { spawn, ChildProcess, execSync } from 'child_process';
import { PvsVersionDescriptor, SimpleConnection } from '../common/serverInterface'
import * as path from 'path';
import * as fsUtils from '../common/fsUtils';

/**
 * Wrapper class for PVS: spawns a PVS process, and exposes the PVS REPS interface as an asyncronous server.
 * This eay of interacting with PVS emulates what the Emacs interface does
 */
export class PvsProcessLegacy {
	protected pvsProcess: ChildProcess = null;
	protected pvsVersionInfo: PvsVersionDescriptor;

	protected pvsPath: string = null;
	protected pvsLibraryPath: string = null;
	protected ready: boolean = false;
	protected data: string = "";
	protected cb: (data: string) => void;

	protected connection: SimpleConnection;

	/**
	 * utility function for sending error messages over the connection (if any connection is available)
	 * @param msg message to be sent
	 */
	protected error(msg: string): void {
		if (msg) {
			if (this.connection) {
				this.connection.sendNotification('pvs-error', msg);
			}
			console.log('[pvs-process-legacy] pvs-error', msg);
		}
	}

	/**
	 * @constructor
	 * @param desc Information on the PVS execution environment.
	 * @param connection Connection with the language client
	 */
	constructor (desc: { pvsPath: string }, connection?: SimpleConnection) {
		this.pvsPath = (desc && desc.pvsPath) ? fsUtils.tildeExpansion(desc.pvsPath) : __dirname
		this.pvsLibraryPath = path.join(this.pvsPath, "lib");
		// this.contextFolder = (desc && desc.contextFolder) ? fsUtils.tildeExpansion(desc.contextFolder) : __dirname;

		// this.processType = (desc && desc.processType) ? desc.processType : "typechecker"; // this is used only for debugging
		this.connection = connection;
	}

	getData (): string { return this.data; }
	resetData (): void { this.data = ""; }


	//----------------------------------------------------------------------------------------------------
	//--------------------- The following functions are the main APIs provided by PvsIoProcess
	//----------------------------------------------------------------------------------------------------

	/**
	 * Sends an expression at the PVSio prompt
	 * @param data Expression to be evaluated at the PVSio prompt 
	 */
	async sendText (data: string): Promise<string> {
		return new Promise((resolve, reject) => {
			this.cb = (data: string) => {
				resolve(data);
			}
			this.resetData();
			this.pvsProcess.stdin.write(data);
		});
	}
	/**
	 * Sends the quit command (followed by a confirmation) to PVSio
	 */
	quit (): void {
		this.pvsProcess.stdin.write("exit; Y");
	}
	/**
	 * Creates a new pvsio process.
	 * @returns true if the process has been created; false if the process could not be created.
	 */
	async activate (): Promise<boolean> {
		return new Promise (async (resolve, reject) => {
			if (this.pvsProcess) {
				// process already running, nothing to do
				return Promise.resolve(true);
			}
			const pvsExecutable = path.join(this.pvsPath, "pvs");
			const args: string[] = [ `-raw` ];
			// pvsio args
			console.info(`${pvsExecutable} ${args.join(" ")}`);
			const fileExists: boolean = await fsUtils.fileExists(pvsExecutable);
			if (fileExists && !this.pvsProcess) {
				this.pvsProcess = spawn(pvsExecutable, args);
				// console.dir(this.pvsProcess, { depth: null });
				this.pvsProcess.stdout.setEncoding("utf8");
				this.pvsProcess.stderr.setEncoding("utf8");
				this.pvsProcess.stdout.on("data", (data: string) => {
					this.connection.console.log(data);
					this.data += data;
					// console.dir({ 
					// 	type: "memory usage",
					// 	data: process.memoryUsage()
					// }, { depth: null });
					// console.log(data);

					// wait for the pvs prompt, to make sure pvs-server is operational
					const match: RegExpMatchArray = /\bpvs\(\d+\)\s*:/g.exec(data);
					if (match && match[0]) {
						if (!this.ready) {
							this.ready = true;
							resolve(true);
						}
						if (this.cb && typeof this.cb === "function") {
							const res: string = this.data.replace(/\bpvs\(\d+\)\s*:/g, "");
							this.cb(res.trim());
						}
					}
				});
				this.pvsProcess.stdin.on("data", (data: string) => {
					console.log("stdin", data);
				});
				this.pvsProcess.stderr.on("data", (data: string) => {
					console.log("[pvs-process-legacy] Error: " + data);
					this.error(data);
					// resolve(false);
				});
				this.pvsProcess.on("error", (err: Error) => {
					console.log("[pvs-process-legacy] Process error");
					// console.dir(err, { depth: null });
				});
				this.pvsProcess.on("exit", (code: number, signal: string) => {
					console.log("[pvs-process-legacy] Process exited");
					// console.dir({ code, signal });
				});
				this.pvsProcess.on("message", (message: any) => {
					console.log("[pvs-process-legacy] Process message");
					// console.dir(message, { depth: null });
				});
			} else {
				console.log(`\n>>> PVS executable not found at ${pvsExecutable} <<<\n`);
				resolve(false);
			}
		});
	}
	/**
	 * Utility function. Returns a string representing the ID of the pvs process.
	 * @returns String representation of the pvs process ID.
	 */
	protected getProcessID (): string {
		if (this.pvsProcess && !isNaN(this.pvsProcess.pid)) {
			return this.pvsProcess.pid.toString();
		}
		return null;
	}
	/**
	 * Kills the pvsio process.
	 * @returns True if the process was killed, false otherwise.
	 */
	async kill (): Promise<boolean> {
		return new Promise((resolve, reject) => {
			if (this.pvsProcess) {
				const pvs_shell: string = this.getProcessID();
				// before killing the process, we need to close & drain the streams, otherwisae an ERR_STREAM_DESTROYED error will be triggered
				// because the destruction of the process is immediate but previous calls to write() may not have drained
				// see also nodejs doc for writable.destroy([error]) https://nodejs.org/api/stream.html
				this.pvsProcess.stdin.destroy();
				// try {
				// 	execSync(`kill -9 ${pid}`);
				// } finally {
				// 	setTimeout(() => {
				// 		resolve(true);
				// 	}, 1000);
				// }
				try {
					execSync(`kill -9 ${pvs_shell}`);
					this.pvsProcess.on("close", (code: number, signal: string) => {
						console.log("[pvs-process] Process terminated");
						resolve(true);
						// console.dir({ code, signal }, { depth: null });
					});
				} catch (kill_error) {
					console.log(`[pvsProcess] Warning: Could not kill process id ${pvs_shell}.`);
					resolve(false);
				} finally {
					this.pvsProcess = null;
				}
			} else {
				resolve(true);
			}
		});
	}
}
