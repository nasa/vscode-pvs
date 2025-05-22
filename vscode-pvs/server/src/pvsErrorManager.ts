import { Connection } from "vscode-languageserver";
import { PvsError } from "./common/pvs-gui";
import { ProofExecDidFailToStartProof, serverEvent } from "./common/serverInterface";
import * as fsUtils from './common/fsUtils';
import { ProcessCode } from "./pvsProcess";

export class PvsErrorManager {
    // connection to the client
    protected connection: Connection;

    /**
     * Constructor
     */
    constructor (connection: Connection) {
        this.connection = connection;
    }

    /**
     * Handler for prove-formula errors
     */
    handleProveFormulaError (desc: {
        request: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string }, 
        response: PvsError, 
        taskId: string,
        autorun?: boolean
    }): void {
        // PVS is returning error messages with different structures. @M3 #TODO fix me
        const msg: string = desc.response.error.message 
        + (desc.response.error.data? 
            " - " + (typeof desc.response.error.data === "string"? desc.response.error.data : desc.response.error.data.error_string) : 
            "");
        console.error(`[pvsErrorManager.handleProveFormulaError] PVS reported an error: ${msg}`);
        const evt: ProofExecDidFailToStartProof = {
            action: "did-fail-to-start-proof",
            msg: msg
        };
        this.connection?.sendNotification(serverEvent.proverEvent, evt);
        if (desc?.autorun) {
            this.connection?.sendRequest(serverEvent.autorunFormulaResponse, { status: "untried", error: `Unable to run proof ${desc?.request?.formulaName}` });
        } else {
            this.connection?.sendNotification(`server.status.end-important-task-${desc.taskId}`, desc);
        }
    }
    /**
     * Handler for start-evaluator errors
     */
    handleEvaluationError (desc: {
        request: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, cmd?: string }, 
        response: PvsError,
        taskId?: string
    }): void {
        if (desc) {
            if (desc.taskId) {
                const msg: string = `Typecheck errors in ${desc.request.fileName}${desc.request.fileExtension}.\nPlease fix the typecheck errors before trying to start the evaluator on theory ${desc.request.theoryName}.`;
                // this.connection?.sendRequest(serverEvent.closeDontSaveEvent, { args: desc.request, msg });
                this.notifyEndImportantTaskWithErrors({ id: desc.taskId, msg, showProblemsPanel: true });
            } else {
                const msg: string = (desc.response.error && desc.response.error.message) ? desc.response.error.message 
                : `Error: PVSio crashed into Lisp. Please start a new evaluator session.`;
                this.notifyError({ msg });
                console.error(`[pvs-language-server] ${msg}`);
            }
        }
    }
    /**
     * Handler for latex generation errors
     */
    handleLatexFileError (desc: {
        request: { fileName: string, fileExtension: string, contextFolder: string },
        response: PvsError,
        taskId?: string
    }): void {
        if (desc && desc.response && desc.response.error) {
            if (desc.response.error.data) {
                const fname: string = (desc.response.error.data.file_name) ? desc.response.error.data.file_name : fsUtils.desc2fname(desc.request);
                const msg: string = desc.response.error.data.error_string || desc.response.error.data;
                if (desc.taskId) 
                    this.notifyEndImportantTaskWithErrors({ id: desc.taskId, msg: `${msg}`, showProblemsPanel: false });
            } else if (desc.response.error.message) {
                this.notifyEndImportantTaskWithErrors({ id: desc.taskId, msg: desc.response.error.message, showProblemsPanel: false });
            }
        }
    }
    /**
     * Handler for typecheck-file errors
     */
    handleTypecheckError (desc: {
        request: { fileName: string, fileExtension: string, contextFolder: string },
        response: PvsError,
        taskId?: string
    }): void {
        if (desc && desc.response && desc.response.error) {
            if (desc.response.error.data) {
                const fname: string = (desc.response.error.data.file_name) ? desc.response.error.data.file_name : fsUtils.desc2fname(desc.request);
                const msg: string = desc.response.error.data.error_string || "";
                if (desc.taskId) {
                    if (fname === fsUtils.desc2fname(desc.request)) {
                        this.notifyEndImportantTaskWithErrors({ id: desc.taskId, msg: `Typecheck errors in ${desc.request.fileName}${desc.request.fileExtension}: ${msg}`, showProblemsPanel: true });
                    } else {
                        this.notifyEndImportantTaskWithErrors({ id: desc.taskId, msg: `File ${fsUtils.getFileName(fname)}${fsUtils.getFileExtension(fname)} imported by ${desc.request.fileName}${desc.request.fileExtension} contains typecheck errors.`, showProblemsPanel: true });
                    }
                }
            } else if (desc.response.error.message) {
                // this is typically an error thrown by pvs-server, not an error in the PVS spec
                this.notifyEndImportantTaskWithErrors({ id: desc.taskId, msg: desc.response.error.message, showProblemsPanel: true });
            }
        }
    }
    /**
     * Handler for show-tccs errors
     */
    handleShowTccsError (desc: {
        response: PvsError
    }): void {
        if (desc) {
            const msg: string = (desc?.response?.error) ? JSON.stringify(desc.response.error)
                : `Error: tccs could not be generated (please check pvs-server output for details)`;
            this.notifyError({ msg });
            console.error(`[pvsErrorManager.handleShowTccsError] Error: tccs could not be generated`, desc.response);
        }
    }
    // handleParseFileError (desc: {
    //     taskId: string,
    //     request: { fileName: string, fileExtension: string, contextFolder: string },
    //     source: string
    // }): void {
    //     if (desc) {
    //         this.notifyEndImportantTaskWithErrors({ id: desc.taskId, msg: `${desc.source} errors in ${desc.request.fileName}${desc.request.fileExtension}` });
    //     }
    // }
    // handleWorkspaceActionError (desc: {
    //     taskId: string,
    //     request: { contextFolder: string },
    //     msg: string
    // }): void {
    //     if (desc) {
    //         this.notifyEndImportantTaskWithErrors({ id: desc.taskId, msg: desc.msg });
    //     }
    // }
    /**
     * Handler for serverRequest.startPvsServer errors
     * @param success 
     */
    handleStartPvsServerError (success: ProcessCode): void {
        switch (success) {
            case ProcessCode.SUCCESS: {
                // nothing to do
                break;
            }
            case ProcessCode.PVS_NOT_FOUND: {
                this.connection?.sendRequest(serverEvent.pvsNotFound);
                break;
            }
            case ProcessCode.UNSUPPORTED_PLATFORM: {
                const platform: string = process.platform;
                this.connection?.sendRequest(serverEvent.pvsServerFail, { msg: `Error: Unsupported platform '${platform}'.`});
                break;
            }
            case ProcessCode.PVS_START_FAIL: {
                this.connection?.sendRequest(serverEvent.pvsServerFail, { msg: `Error: Couldn't start PVS process.\nDouble check PVS installation and try again.`});
                break;
            }
            default: {
                this.connection?.sendRequest(serverEvent.pvsServerFail);
                break;
            }
        }
    }
    
    // additional utility functions for notifying errors to the client
    notifyEndImportantTaskWithErrors (desc: { id: string, msg: string, showProblemsPanel: boolean }) {
        this.connection?.sendNotification(`server.status.end-important-task-${desc.id}-with-errors`, desc);
	}
	notifyPvsFailure (opt?: { msg?: string, fname?: string, method?: string, error_type?: string, src: string, log?: string }): void {
        this.connection?.sendNotification("server.status.pvs-failure", opt);
	}
	notifyError (desc: { msg: string }): void {
		// error shown in the status bar
        this.connection?.sendNotification("server.status.error", desc);
	}
    notifyPvsNotFound (pvsPath: string): void {
        const msg: string = `PVS executable not found at ${pvsPath}`;
        console.error(msg);
        this.connection?.sendRequest(serverEvent.pvsNotFound, msg);
    }
}