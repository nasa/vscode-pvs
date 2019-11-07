/**
 * @module VSCodeEventsDispatcher
 * @author Paolo Masci
 * @date 2019.10.12
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

import { LanguageClient } from "vscode-languageclient";
import { VSCodePvsStatusBar } from "./views/vscodePvsStatusBar";
import { VSCodePvsEmacsBindingsProvider } from "./providers/vscodePvsEmacsBindingsProvider";
import { VSCodePvsTheoryExplorer } from "./views/vscodePvsTheoryExplorer";
import { VSCodePvsProofExplorer } from "./views/vscodePvsProofExplorer";
import { VSCodePvsTerminal } from "./views/vscodePvsTerminal";
import { ProofStateNode } from './common/languageUtils';
import { ContextDescriptor, serverEvent, serverCommand } from "./common/serverInterface";
import { window, commands, ExtensionContext } from "vscode";
import * as vscode from 'vscode';
import { PvsResponse } from "./common/pvs-gui";
import * as fsUtils from './common/fsUtils';
import { VSCodePvsSequentViewer } from "./views/vscodePvsSequentViewer";
import { PVSioTerminal } from "./views/vscodePVSioTerminal";

// FIXME: use publish-subscribe to allow easier introduction of new components
export class EventsDispatcher {
    protected client: LanguageClient;
    protected statusBar: VSCodePvsStatusBar;
    protected emacsBindings: VSCodePvsEmacsBindingsProvider;
    protected theoryExplorer: VSCodePvsTheoryExplorer;
    protected proofExplorer: VSCodePvsProofExplorer;
    protected vscodePvsTerminal: VSCodePvsTerminal;
    protected sequentViewer: VSCodePvsSequentViewer;

    constructor (client: LanguageClient, handlers: {
        statusBar: VSCodePvsStatusBar,
        emacsBindings: VSCodePvsEmacsBindingsProvider,
        theoryExplorer: VSCodePvsTheoryExplorer,
        proofExplorer: VSCodePvsProofExplorer,
        vscodePvsTerminal: VSCodePvsTerminal,
        sequentViewer: VSCodePvsSequentViewer
    }) {
        this.client = client;
        this.statusBar = handlers.statusBar;
        this.emacsBindings = handlers.emacsBindings;
        this.theoryExplorer = handlers.theoryExplorer;
        this.proofExplorer = handlers.proofExplorer;
        this.vscodePvsTerminal = handlers.vscodePvsTerminal;
        this.sequentViewer = handlers.sequentViewer;
    }
    protected resource2desc (resource: any): { 
        fileName: string, fileExtension: string, contextFolder: string 
    } | { 
        fileName: string, fileExtension: string, contextFolder: string, 
        theoryName: string, formulaName: string, line: number 
    } {
        if (resource) {
            if (resource.fileName && resource.fileExtension && resource.contextFolder) {
                return resource;
            } else if (resource.contextValue) {
                // resource coming from explorer
                // resource is of type FormulaItem
                return {
                    fileName: fsUtils.getFileName(resource.getFileName()),
                    fileExtension: ".pvs",
                    contextFolder: resource.getContextFolder(),
                    formulaName: resource.getFormulaName(),
                    theoryName: resource.getTheoryName(),
                    line: resource.getPosition().line
                };
            } else if (resource.path) {
                // resource coming from the editor
                // resource is of type vscode.Uri
                return {
                    fileName: fsUtils.getFileName(resource.path),
                    fileExtension: fsUtils.getFileExtension(resource.path),
                    contextFolder: fsUtils.getContextFolder(resource.path)
                };
            }
        }
        console.error(`[event-dispatcher] Warning: could not describe resource `, resource);
        return null;
    }
	activate (context: ExtensionContext): void {
        // -- handlers for server responses
        this.client.onRequest(serverEvent.pvsServerReady, (version: { "pvs-version": string, "lisp-version": string }) => {
            // parse file opened in the editor
            if (vscode.window.activeTextEditor && vscode.window.activeTextEditor.document) {
                this.client.sendRequest(serverCommand.parseFile, vscode.window.activeTextEditor.document.fileName);
            }
		});
		this.client.onRequest(serverEvent.pvsVersionInfo, (version: { "pvs-version": string, "lisp-version": string }) => {
			if (version) {
				this.statusBar.pvsReady(version);
			}
		});
		this.client.onRequest(serverEvent.contextUpdate, (desc: ContextDescriptor) => {
			if (this.theoryExplorer && desc) {
				this.theoryExplorer.updateView(desc);
			}
		});
		this.client.onRequest(serverEvent.typecheckFileResponse, (desc: { response: PvsResponse, args: { fileName: string, fileExtension: string, contextFolder: string }}) => {
            // request tccs
            if (desc && desc.args) {
                this.client.sendRequest(serverCommand.showTccs, desc.args);
            }
        });
		this.client.onRequest(serverEvent.showTccsResponse, (desc: { response: { [ theoryName: string ]: PvsResponse }, args: { fileName: string, fileExtension: string, contextFolder: string }}) => {
            // do nothing for now
            console.log(desc);
        });
		this.client.onRequest(serverEvent.parseFileResponse, (res: PvsResponse) => {
			// do nothing for now
        });
        this.client.onRequest(serverEvent.proofCommandResponse, (desc: { response: PvsResponse, args: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string, cmd: string }}) => {
            // notify proofexplorer
            this.proofExplorer.onStepExecuted(desc);
        });
        this.client.onRequest(serverEvent.proveFormulaResponse, (desc: { response: PvsResponse, args: { fileName: string, fileExtension: string, theoryName: string, formulaName: string, contextFolder: string }, pvsLogFile: string }) => {
            // initialise sequent viewer
            this.sequentViewer.setLogFileName(desc.pvsLogFile);
            // show action bar items
            this.statusBar.showSequentViewerControls();
            // this.statusBar.showProofStepperControls();
            // save initial proof state in proof explorer
            this.proofExplorer.setInitialProofState(desc.response.result);
            // request proof script
            this.client.sendRequest(serverCommand.proofScript, desc.args);
        });
		this.client.onRequest(serverEvent.proofScriptResponse, (desc: { response: PvsResponse, args: { fileName: string, fileExtension: string, theoryName: string, formulaName: string, contextFolder: string }, proofFile: string }) => {
            if (desc) {
                console.log(desc);
                if (desc.response && desc.response.result) {
                    this.proofExplorer.setProofDescriptor(desc.args);
                    this.proofExplorer.showProofScript(desc.response.result);
                    this.proofExplorer.activateSelectedProof();
                } else {
                    console.error(`[event-dispatcher] Warning: ${serverEvent.proofScriptResponse} response indicates error`, desc);
                }
            } else {
                console.error(`[event-dispatcher] Warning: ${serverEvent.proofScriptResponse} received null response`);
            }
        });
        // proof-state handler
        this.client.onRequest(serverEvent.proofStateUpdate, async (desc: { response: PvsResponse, args: { fileName: string, fileExtension: string, theoryName: string, formulaName: string, contextFolder: string }, pvsLogFile: string }) => {
            // do nothing for now
        });


        //---------------------------------------------------------
        // commands invoked using code lens, emacs bindings, explorer, etc
        //---------------------------------------------------------

        // vscode-pvs.editor-show-sequent
        context.subscriptions.push(commands.registerCommand("vscode-pvs.editor-show-sequent", (desc: { proofState: ProofStateNode }) => {
            this.sequentViewer.showSequent(desc);
        }));

        // vscode-pvs.send-proof-command
        context.subscriptions.push(commands.registerCommand("vscode-pvs.send-proof-command", (desc: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string, cmd: string }) => {
            this.vscodePvsTerminal.sendProofCommand(desc);
        }));

        // vscode-pvs.close-sequent-viewer
        context.subscriptions.push(commands.registerCommand("vscode-pvs.close-sequent-viewer", (desc: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string, cmd: string }) => {
            this.statusBar.hideSequentViewerControls();
        }));

        // vscode-pvs.close-proof-stepper
        context.subscriptions.push(commands.registerCommand("vscode-pvs.close-proof-stepper", (desc: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string, cmd: string }) => {
            this.statusBar.hideProofStepperControls();
        }));
        
        // vscode-pvs-metax
        context.subscriptions.push(commands.registerCommand("vscode-pvs.metax", () => {
            this.emacsBindings.metaxPrompt();
        }));

        // pvsio-evaluator
        context.subscriptions.push(commands.registerCommand("vscode-pvs.pvsio-evaluator", async (resource) => {
            if (resource) {
                let desc = <{ 
                    fileName: string, fileExtension: string, contextFolder: string, 
                    theoryName: string, formulaName: string, line: number 
                }> this.resource2desc(resource);
                if (desc) {
                    // PVSio is a different beast, not supported by pvs-server at the moment
                    new PVSioTerminal(desc);
                    // // !important: create a new command line interface first, so it can subscribe to events published by the server
                    // await this.vscodePvsTerminal.startPvsIoEvaluatorSession(desc);
                    // // send prove-formula request to pvs-server
                    // this.client.sendRequest(serverCommand.pvsioEvaluator, desc);
                } else {
                    console.error("[vscode-events-dispatcher] Warning: unknown vscode-pvs.prove-formula resource", resource);
                }
            } else {
                console.error("[vscode-events-dispatcher] Warning: pvsio-evaluator invoked with null resource", resource);
            }
        }));

        // vscode-pvs.prove-formula
        context.subscriptions.push(commands.registerCommand("vscode-pvs.prove-formula", async (resource) => {
            if (resource) {
                let desc = <{ 
                    fileName: string, fileExtension: string, contextFolder: string, 
                    theoryName: string, formulaName: string, line: number 
                }> this.resource2desc(resource);
                if (desc) {
                    // !important: create a new command line interface first, so it can subscribe to events published by the server
                    await this.vscodePvsTerminal.startProveFormulaSession(desc);
                    // send prove-formula request to pvs-server
                    this.client.sendRequest(serverCommand.proveFormula, desc);
                    // proof script will be loaded upon receiving serverEvent.proveFormulaResponse
                    // here, we show loading animation in proof explorer
                    // TODO
                } else {
                    console.error("[vscode-events-dispatcher] Warning: unknown vscode-pvs.prove-formula resource", resource);
                }
            } else {
                console.error("[vscode-events-dispatcher] Warning: prove-formula invoked with null resource", resource);
            }
        }));

        // vscode-pvs.typecheck-file
		context.subscriptions.push(commands.registerCommand("vscode-pvs.typecheck-file", async (resource: string | { path: string } | { contextValue: string }) => {
            if (!resource && window.activeTextEditor && window.activeTextEditor.document) {
                resource = { path: window.activeTextEditor.document.fileName };
            }
			if (resource) {
                let desc = <{ 
                    fileName: string, fileExtension: string, contextFolder: string
                }> this.resource2desc(resource);
                if (desc) {
                    // send typecheck request to pvs-server
                    this.client.sendRequest(serverCommand.typecheckFile, desc);
                }
            } else {
                console.error("[vscode-events-dispatcher] Warning: resource is null", resource);
            }
		}));


        //----------------------------------
        // events triggered by pvs-language-server
        //----------------------------------
        this.client.onNotification("server.status.update", (msg: string) => {
            this.statusBar.info(msg);
        });
        this.client.onNotification("server.status.info", (msg: string) => {
            this.statusBar.info(msg);
        });
        this.client.onNotification("server.status.error", (msg: string) => {
            this.statusBar.error(msg);
        });
        this.client.onNotification("server.status.progress", (msg: string) => {
            this.statusBar.progress(msg);
        });
        this.client.onNotification("server.status.ready", () => {
            this.statusBar.ready();
        });
        this.client.onNotification("server.status.start-important-task", (msg: string) => {
            window.showInformationMessage(msg);
            this.statusBar.progress(msg);
        });
        this.client.onNotification("server.status.end-important-task", (msg: string) => {
            window.showInformationMessage(msg);
            this.statusBar.ready();
        });
        this.client.onNotification("server.status.report-error", (msg: string) => {
            window.showErrorMessage(msg);
            this.statusBar.ready();
        });
    }
}