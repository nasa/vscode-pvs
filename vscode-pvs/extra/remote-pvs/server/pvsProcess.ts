import { spawn, ChildProcess } from 'child_process';
import WebSocket from 'ws';
import * as path from 'path';
import { fileExistsCheck, findAvailablePort, tildeExpansion, forceLocale, logger } from './utils';

export enum ProcessCode {
    SUCCESS = 0,
    PVS_NOT_FOUND = -1,
    ADDR_IN_USE = -2,
    COMM_FAILURE = -3,
    PVS_START_FAIL = -4,
    PVSERROR = -5,
    UNSUPPORTED_PLATFORM = -6,
    TERMINATED = -7
}

interface PvsProcessOptions {
    pvsLibraryPath?: string;
}

/**
 * Wrapper class for PVS: spawns a PVS process, and exposes the PVS Lisp interface as an asynchronous JSON/web-socket server.
 */
export class PvsProcess {
    protected pvsProcess: ChildProcess | null = null;
    protected pvsPath: string;

    /**
     * Current status of the PVS process; `undefined` indicates that the current status cannot be determined (for example, because the process is starting but not yet ready).
     */
    protected currentStatus: ProcessCode | undefined = undefined;
    protected data: string = "";
    protected serverPort: number = 23456;
    protected reportedServerPort: number | undefined;
    protected clientId: string;
    ws: WebSocket;

    public getReportedServerPort(): number | undefined {
        return this.reportedServerPort;
    }

    /**
     * @constructor
     * @param pvsPath Path to PVS executable
     * @param ws WebSocket instance
     * @param clientId Client identifier
     * @param opt Options for PVS process
     */
    constructor(pvsPath: string, clientId: string, ws: WebSocket,opt: PvsProcessOptions = {}) {
        this.pvsPath = pvsPath ? tildeExpansion(pvsPath) : __dirname;
        this.clientId = clientId || '';
        this.ws = ws;
    }

    sendToClient(args: object){
        this.ws.send(JSON.stringify(args));
    }

    /**
     * Creates a new pvs process.
     * @returns A promise that resolves to a ProcessCode indicating the result of the activation.
     */
    async activate(): Promise<ProcessCode> {
        console.info(`[pvsProcess.activate] start for client - ${this.clientId}... `);
        if (this.pvsProcess) {
            console.info(`[pvsProcess.activate] process already running for - ${this.clientId}, nothing to do `);
            return ProcessCode.SUCCESS;
        }
        if (!this.pvsPath) {
            return ProcessCode.PVS_NOT_FOUND;
        }
        console.info(`[pvsProcess.activate] pvs is not already running for ${this.clientId}, path seems right `);
        
        forceLocale();
        logger.info(`[PvsProcess.activate] ACL_LOCALE=${process.env["ACL_LOCALE"]}, LC_ALL=${process.env["LC_ALL"]}, LANG=${process.env["LANG"]}`);
        
        const pvs: string = path.join(this.pvsPath, "pvs");
        const avlPort = await findAvailablePort(this.serverPort);
        const args: string[] = ["-raw", "-port", `${avlPort}`];
        console.info(`[PvsProcess.activate] shell command ${this.pvsPath}/pvs ${args.join(" ")}`);
        // console.info(`[PvsProcess.activate] pvsLibraryPath ${this.pvsLibraryPath}`);
        
        const fileExists: boolean = fileExistsCheck(pvs);
        if (!fileExists) {
            logger.info(`\n>>> PVS executable not found at ${pvs} <<<\n`);
            return ProcessCode.PVS_NOT_FOUND;
        }

        return this.startPvsProcess(pvs, args);
    }

    private async startPvsProcess(pvs: string, args: string[]): Promise<ProcessCode> {
        this.currentStatus = undefined;
        console.info('[PvsProcess.activate] about to spawn PVS');
        this.pvsProcess = spawn(pvs, args, { detached: true, env: { ...process.env} });
        console.info(`[PvsProcess.activate] PVS spawned (PID: ${this.pvsProcess.pid})`);

        this.setupProcessListeners();

        return this.waitForPvsConfirmation();
    }

    private setupProcessListeners(): void {
        if (!this.pvsProcess) return;
        let logData: string = "";
        let maxLogLimitReached: boolean = false;
        const resetLocalLog = () => {
            logData = "";
            maxLogLimitReached = false;
        };
        if (this.pvsProcess.stdout){
            this.pvsProcess.stdout.setEncoding("utf8");
            this.pvsProcess.stdout.on("data", this.handleStdoutData.bind(this, resetLocalLog));
        }
        if (this.pvsProcess.stderr){
            this.pvsProcess.stderr.setEncoding("utf8");
            this.pvsProcess.stderr.on("data", this.handleStderrData.bind(this));
        }
        this.pvsProcess.on("error", this.handleProcessError.bind(this));
        this.pvsProcess.on("exit", this.handleProcessExit.bind(this, resetLocalLog));
        this.pvsProcess.on("message", () => resetLocalLog());
    }

    private handleStdoutData(resetLocalLog: () => void, data: string): void {
        const dataNoLineBreaks = data.replace(/\n/g, ' ');
        this.data += dataNoLineBreaks;

        if (this.checkForLispDebugger(dataNoLineBreaks)) {
            // Handle Lisp debugger entry
            this.sendToClient({msg: "PVS entered the debugger. Please use M-x reboot-pvs to restart the process.", src: "pvs",type:"server-call", method: "pvsErrorManager.notifyPvsFailure"});
        }

        if (this.currentStatus !== ProcessCode.SUCCESS) {
            this.checkForUnsupportedPlatform(dataNoLineBreaks, resetLocalLog);
            this.checkForUsedPort(this.data);
        }

        this.checkForPrompts(dataNoLineBreaks, resetLocalLog);
    }

    private checkForLispDebugger(data: string): boolean {
        return /Welcome to LDB, a low-level debugger for the Lisp runtime environment/gi.test(data) ||
               /debugger invoked on/gi.test(data);
    }

    private checkForUnsupportedPlatform(data: string, resetLocalLog: () => void): void {
        const matchNoExecutable = /No executable available in (.+)/gi.exec(data);
        if (matchNoExecutable) {
            this.pvsProcess = null;
            resetLocalLog();
            this.currentStatus = ProcessCode.UNSUPPORTED_PLATFORM;
        }
    }

    private checkForUsedPort(data: string): void {
        const matchUsedPort = /\bListening +on +127\.0\.0\.1:(\d+)/g.exec(data);
        if (matchUsedPort) {
            this.reportedServerPort = +matchUsedPort[1];
            this.currentStatus = ProcessCode.SUCCESS;
        }
    }

    private checkForPrompts(data: string, resetLocalLog: () => void): void {
        const matchPvsPrompt = /(?:\[\d+\w*\])?\s+pvs\(\d+\)\s*:/ig.test(data);
        const matchProverPrompt = /\bRule\?/g.test(data);
        if (matchPvsPrompt || matchProverPrompt) {
            this.currentStatus = ProcessCode.SUCCESS;
            if (!this.currentStatus) {
                resetLocalLog();
            }
        }
    }

    private handleStderrData(data: string): void {
        const dataNoLineBreaks = data.replace(/\n/g, ' ');
        if (/debugger invoked on/gi.test(dataNoLineBreaks)) {
            this.sendToClient({msg: "PVS entered the debugger. Please use M-x reboot-pvs to restart the process.", src: "pvs",type:"server-call", method: "pvsErrorManager.notifyPvsFailure"});
        }
    }

    private handleProcessError(err: Error): void {
        logger.info(`[pvsProcess - ${this.clientId}] Process error \n>>> ${err} <<< `);
    }

    private handleProcessExit(resetLocalLog: () => void, code: number | null, signal: string | null): void {
        resetLocalLog();
        logger.info(`[pvsProcess - ${this.clientId}] Process exited, code: ${code}, signal: ${signal}, this.ready: ${this.currentStatus}`);
        if (!this.currentStatus) {
            this.currentStatus = ProcessCode.PVS_START_FAIL;
        } else {
            this.currentStatus = undefined;
        }
    }

    private async waitForPvsConfirmation(): Promise<ProcessCode> {
        return new Promise<ProcessCode>((resolve) => {
            const maxNumberOfAttempts = 100;
            const intervalTime = 200; // ms

            let currentAttempt = 0;
            logger.info("[pvsProcess.activate] Waiting for PVS confirmation ");
            const interval = setInterval(() => {
                logger.info(`[pvsProcess.activate] -> polling on currentStatus (check ${currentAttempt + 1} of ${maxNumberOfAttempts})...`);
                if (this.currentStatus === ProcessCode.SUCCESS) {
                    clearInterval(interval);
                    logger.info("[pvsProcess.activate] DONE: PVS is active and waiting for requests");
                    resolve(ProcessCode.SUCCESS);
                } else if (currentAttempt > maxNumberOfAttempts - 1) {
                    clearInterval(interval);
                    logger.info("[pvsProcess.activate] FAIL: reached max number of attempts");
                    resolve(ProcessCode.PVS_START_FAIL);
                } else if (this.currentStatus) {
                    clearInterval(interval);
                    logger.info(`[pvsProcess.activate] FAIL: status code ${this.currentStatus}`);
                    resolve(this.currentStatus);
                }
                currentAttempt++;
            }, intervalTime);
        });
    }

    /**
     * Kills the pvs process.
     * @returns A promise that resolves to a boolean indicating whether the process was successfully killed.
     */
    async kill(): Promise<boolean> {
        this.currentStatus = undefined;
        return new Promise<boolean>(async (resolve) => {
            if (!this.pvsProcess) {
                resolve(true);
                return;
            }

            if (this.pvsProcess.exitCode !== null) {
                this.pvsProcess = null;
                resolve(true);
                return;
            }

            const pid = this.getProcessID();
            this.setupKillListeners(resolve);

            try {
                await this.attemptGracefulExit();
            } finally {
                this.pvsProcess = null;
                resolve(true);
            }
        });
    }

    private setupKillListeners(resolve: (value: boolean) => void): void {
        if (!this.pvsProcess) return;

        this.pvsProcess.on("close", () => {
            logger.info("[pvsProcess] Process terminated");
            this.currentStatus = ProcessCode.TERMINATED;
            this.pvsProcess = null;
            resolve(true);
        });

        this.pvsProcess.on("error", () => {
            logger.info("[pvsProcess] Process terminated with error");
            this.currentStatus = ProcessCode.TERMINATED;
            resolve(true);
        });
    }

    private async attemptGracefulExit(): Promise<void> {
        if (!this.pvsProcess) return;

        return new Promise<void>((resolveExit) => {
            if (this.pvsProcess?.stdin && !this.pvsProcess.stdin.destroyed) {
                this.pvsProcess.stdin.write("(lisp (bye))\n");
            }

            if (this.pvsProcess && !this.pvsProcess.killed) {
                this.pvsProcess.kill('SIGKILL');
            }

            setTimeout(() => {
                resolveExit();
            }, 400);
        });
    }

    /**
     * Utility function. Returns the ID of the pvs process.
     * @returns pvs process ID or null if not available.
     */
    protected getProcessID(): number | null {
        return this.pvsProcess?.pid ?? null;
    }

    /**
     * Returns the current output of the process
     */
    getLispInterfaceOutput(): string {
        return this.data || "";
    }

    /**
     * Clears the current lisp output
     */
    clearLispInterfaceOutput(): void {
        this.data = "";
    }
}