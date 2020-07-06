import { Connection } from "vscode-languageserver";
import { PvsError } from "./common/pvs-gui";
import { serverEvent } from "./common/serverInterface";
import * as fsUtils from './common/fsUtils';
import { ProcessCode } from "./pvsProcess";

export class PvsErrorManager {
    // connection to the client
    protected connection: Connection;

    constructor (connection: Connection) {
        this.connection = connection;
    }

    handleProofCommandError (desc: { response: PvsError }): void {
        this.notifyError({ msg: "Error: proof-command returned error (please check pvs-server output for details)" });
        console.error("[pvs-language-server.proofCommandRequest] Error: proof-command returned error", desc.response);
    }
    handleProveFormulaError (desc: {
        request: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string }, 
        response: PvsError, 
        taskId: string 
    }): void {
    }
    handleEvaluationError (desc: {
        request: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, cmd?: string }, 
        response: PvsError,
        taskId?: string
    }): void {
        if (desc) {
            if (desc.taskId) {
                const msg: string = (desc.response.error && desc.response.error.message) ? desc.response.error.message 
                    : `Error: PVSio crashed into Lisp. Please start a new evaluator session.`;
                this.notifyError({ msg });
                console.error(`[pvs-language-server] ${msg}`);
            } else {
                // there was an error
                const msg: string = `Typecheck errors in ${desc.request.fileName}${desc.request.fileExtension}.\nPlease fix the typecheck errors before trying to prove the formula.`;
                this.connection.sendRequest(serverEvent.closeDontSaveEvent, { args: desc.request, msg });
                this.notifyEndImportantTaskWithErrors({ id: desc.taskId, msg });
            }
        }
    }
    handleTypecheckError (desc: {
        request: { fileName: string, fileExtension: string, contextFolder: string },
        response: PvsError,
        taskId: string
    }): void {
        if (desc && desc.response && desc.response.error) {
            if (desc.response.error.data) {
                const fname: string = (desc.response.error.data.file_name) ? desc.response.error.data.file_name : fsUtils.desc2fname(desc.request);
                const msg: string = desc.response.error.data.error_string || "";
                if (fname === fsUtils.desc2fname(desc.request)) {
                    this.notifyEndImportantTaskWithErrors({ id: desc.taskId, msg: `Typecheck errors in ${desc.request.fileName}${desc.request.fileExtension}: ${msg}` });
                } else {
                    this.notifyEndImportantTaskWithErrors({ id: desc.taskId, msg: `File ${fsUtils.getFileName(fname)}${fsUtils.getFileExtension(fname)} imported by ${desc.request.fileName}${desc.request.fileExtension} contains typecheck errors.` });
                }
            } else if (desc.response.error.message) {
                // this is typically an error thrown by pvs-server, not an error in the PVS spec
                this.notifyEndImportantTaskWithErrors({ id: desc.taskId, msg: desc.response.error.message });
            }
        }
    }
    handleShowTccsError (desc: {
        response: PvsError
    }): void {
        if (desc) {
            this.notifyError({ msg: `Error: tccs could not be generated (please check pvs-server output for details)` });
            console.error(`[pvs-language-server.showTccs] Error: tccs could not be generated`, desc.response);
        }
    }
    handleParseFileError (desc: {
        taskId: string,
        request: { fileName: string, fileExtension: string, contextFolder: string },
        source: string
    }): void {
        if (desc) {
            this.notifyEndImportantTaskWithErrors({ id: desc.taskId, msg: `${desc.source} errors in ${desc.request.fileName}${desc.request.fileExtension}` });
        }
    }
    handleWorkspaceActionError (desc: {
        taskId: string,
        request: { contextFolder: string },
        msg: string
    }): void {
        if (desc) {
            this.notifyEndImportantTaskWithErrors({ id: desc.taskId, msg: desc.msg });
        }
    }
    handleStartPvsServerError (success: ProcessCode): void {
        if (success === ProcessCode.PVSNOTFOUND) {
            this.connection.sendRequest(serverEvent.pvsNotPresent);
        } else if (success !== ProcessCode.SUCCESS) {
            this.connection.sendRequest(serverEvent.pvsServerCrash);
        }
    }
    
    // utility functions
    notifyEndImportantTaskWithErrors (desc: { id: string, msg: string }) {
		if (this.connection) {
			this.connection.sendNotification(`server.status.end-important-task-${desc.id}-with-errors`, desc);
		}
	}
	notifyPvsFailure (opt?: { msg?: string, fname?: string, method?: string }): void {
		// error will be shown in a dialogue box
		if (this.connection) {
			this.connection.sendNotification("server.status.pvs-failure", opt);
		}
	}
	protected notifyError (desc: { msg: string }): void {
		// error shown in the status bar
		if (this.connection) {
			this.connection.sendNotification("server.status.error", desc);
		}
	}
}