/**
 * @module PvsProcess
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

import { spawn, ChildProcess, execSync } from 'child_process';
// note: ./common is a symbolic link. if vscode does not find it, try to restart TS server: CTRL + SHIFT + P to show command palette, and then search for Typescript: Restart TS Server
import { 
	PvsParserResponse, PvsVersionDescriptor,
	SimpleConnection
} from './common/serverInterface'
import * as path from 'path';
import * as fsUtils from './common/fsUtils';

/**
 * Wrapper class for PVS: spawns a PVS process, and exposes the PVS Lisp interface as an asyncronous JSON/RPC server.
 */
export class PvsProcess {
	protected pvsProcess: ChildProcess = null;
	protected pvsVersionInfo: PvsVersionDescriptor;

	protected pvsPath: string = null;
	protected pvsLibraryPath: string = null;

	protected connection: SimpleConnection;
	protected enableNotifications: boolean;

	protected xmlRpcServer: { port: number; };

	/**
	 * utility function for sending error messages over the connection (if any connection is available)
	 * @param msg message to be sent
	 */
	protected error(msg: string): void {
		if (msg) {
			if (this.connection && this.enableNotifications) {
				this.connection.sendNotification('pvs-error', msg);
			}
			console.log('[pvs-process] pvs-error', msg);
		}
	}

	/**
	 * @constructor
	 * @param desc Information on the PVS execution environment.
	 * @param connection Connection with the language client
	 */
	constructor (desc: { pvsPath: string, contextFolder?: string, processType?: string }, connection?: SimpleConnection) {
		this.pvsPath = (desc && desc.pvsPath) ? fsUtils.tildeExpansion(desc.pvsPath) : __dirname
		this.pvsLibraryPath = path.join(this.pvsPath, "lib");
		// this.contextFolder = (desc && desc.contextFolder) ? fsUtils.tildeExpansion(desc.contextFolder) : __dirname;

		// this.processType = (desc && desc.processType) ? desc.processType : "typechecker"; // this is used only for debugging
		this.connection = connection;
	}

	/**
	 * Internal function. Runs the relocate script necessary for starting pvs.
	 */
	protected async relocate(): Promise<boolean> {
		let relocate: string = null;
		if (await fsUtils.fileExists(path.join(`${this.pvsPath}`, "install-sh"))) {
			relocate = `cd ${this.pvsPath} && ./install-sh` // pvs 7 has this new script
		} else if (await fsUtils.fileExists(path.join(`${this.pvsPath}`, "bin/relocate"))) {
			relocate = `cd ${this.pvsPath} && bin/relocate`; // this is for backwards compatibility
		}
		if (relocate) {
			try {
				const output: Buffer = execSync(relocate);
				// console.log(output.toString());
			} catch (relocateError) {
				console.log(relocateError);
				return false;
			}
			return true;
		}
		return false;
	}
	/**
	 * Internal function. Creates a new pvs process.
	 * @param opt Options: enableNotifications, transmits the output of the pvs process over the client connection (if any is available)
	 * @returns true if the process has been created; false if the process could not be created.
	 */
	protected async _activate (opt?: { enableNotifications?: boolean, xmlRpcServer?: boolean | { port: number } }): Promise<boolean> {
		opt = opt || {};
		if (!await this.relocate()) {
			console.warn("[pvs-process] Warning: could not execute PVS relocation/install script");
		}
		this.enableNotifications = opt.enableNotifications || this.enableNotifications;
		const serverPort: number = (opt.xmlRpcServer && typeof opt.xmlRpcServer === "object") ? opt.xmlRpcServer.port : 22334;
		this.xmlRpcServer = (opt.xmlRpcServer) ? { port: serverPort } : this.xmlRpcServer;

		const pvs: string = path.join(this.pvsPath, "pvs");
		const port: string = this.xmlRpcServer.port.toString();
		const args: string[] = (this.xmlRpcServer) ? [ "-raw", "-port", port ] : [ "-raw"];//, "-port", "22334" ];
		// pvs args
		console.info(`${this.pvsPath}/pvs ${args.join(" ")}`);
		const fileExists: boolean = await fsUtils.fileExists(pvs);
		if (fileExists) {
			const readyPrompt: RegExp = /\s*pvs\(\d+\):|([\w\W\s]*)\spvs\(\d+\):/g;
			return await new Promise((resolve, reject) => {
				this.pvsProcess = spawn(pvs, args);
				this.pvsProcess.stdout.setEncoding("utf8");
				this.pvsProcess.stderr.setEncoding("utf8");
				this.pvsProcess.stdout.on("data", (data: string) => {
					if (this.connection && this.connection.console) {
						this.connection.console.log(data);
					}
					// wait for the pvs prompt, to make sure pvs-server is operational
					const match: RegExpMatchArray = readyPrompt.exec(data);
					if (match) {
						resolve(true);
					}
				});
				this.pvsProcess.stderr.on("data", (data: string) => {
					this.error(data);
					// resolve(false);
				});	
			});
		} else {
			console.log(`\n>>> PVS executable not found at ${pvs} <<<\n`);
			return false;
		}
	}

	//----------------------------------------------------------------------------------------------------
	//--------------------- The following functions are the main APIs provided by PvsProcess
	//----------------------------------------------------------------------------------------------------

	/**
	 * Creates a new pvs process.
	 * @param opt Options: enableNotifications, transmits the output of the pvs process over the client connection (if any is available)
	 * @returns true if the process has been created; false if the process could not be created.
	 */
	async activate (opt?: { enableNotifications?: boolean, xmlRpcServer?: boolean | { port: number }}): Promise<boolean> {
		const success: boolean = await this._activate(opt);
		return success;
	}
	/**
	 * Kills the pvs process.
	 * @returns The ID of the process that was killed. Null if no process was killed.
	 */
	async kill (): Promise<boolean> {
		return new Promise((resolve, reject) => {
			if (this.pvsProcess) {
				// const pid: string = this.getProcessID();
				// before killing the process, we need to close & drain the streams, otherwisae an ERR_STREAM_DESTROYED error will be triggered
				// because the destruction of the process is immediate but previous calls to write() may not have drained
				// see also nodejs doc for writable.destroy([error]) https://nodejs.org/api/stream.html
				if (this.pvsProcess) {
					this.pvsProcess.stdin.end(() => {});
				}
				// try {
				// 	execSync(`kill -9 ${pid}`);
				// } finally {
				// 	setTimeout(() => {
				// 		resolve(true);
				// 	}, 1000);
				// }
				try {
					const allegro: string = execSync("ps -A | grep pvs").toString();
					if (allegro) {
						const procs: string[] = allegro.trim().split("\n");
						try {
							for (let i = 0; i < procs.length; i++) {
								const info: string = procs[i];
								const match: RegExpMatchArray = /\w+\s+\w+\s+\w+\s+(\w+)/.exec(info);
								if (match && match.length > 1 && match[1]) {
									const allegro_pid: string = match[1];
									if (allegro_pid) {
										execSync(`kill -15 ${allegro_pid}`);
									}
								}
							}
							// execSync(`kill -15 ${pid}`);
						} finally {
							setTimeout(() => {
								resolve(true);
							}, 1000);
						}
					}
				} finally {
					resolve(true);
				}
			} else {
				resolve(true);
			}
		});
	}
	/**
	 * Utility function. Returns a string representing the ID of the pvs process.
	 * @returns String representation of the pvs process ID.
	 */
	protected getProcessID (): string {
		if (this.pvsProcess && this.pvsProcess.pid) {
			return this.pvsProcess.pid.toString();
		}
		return null;
	}

	public async clearContext (contextFolder?: string): Promise<void> {
		const currentContext: string = contextFolder;// || this.contextFolder;
		if (currentContext) {
			// console.info(`** clearing pvs cache for context ${currentContext} **`)
			await fsUtils.deletePvsCache(currentContext);
		}
	}
}