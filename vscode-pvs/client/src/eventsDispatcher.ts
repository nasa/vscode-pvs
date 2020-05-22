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
import { VSCodePvsWorkspaceExplorer } from "./views/vscodePvsWorkspaceExplorer";
import { VSCodePvsProofExplorer } from "./views/vscodePvsProofExplorer";
import { VSCodePvsTerminal } from "./views/vscodePvsTerminal";
import { PvsContextDescriptor, serverEvent, serverCommand, PvsVersionDescriptor, ProofDescriptor } from "./common/serverInterface";
import { window, commands, ExtensionContext, ProgressLocation } from "vscode";
import * as vscode from 'vscode';
import { PvsResponse, DischargeTccsResult } from "./common/pvs-gui";
import * as fsUtils from './common/fsUtils';
import { VSCodePvsProofMate } from "./views/vscodePvsProofMate";
import * as utils from './common/languageUtils';
import * as commandUtils from './common/commandUtils';

// FIXME: use publish-subscribe to allow easier introduction of new components
export class EventsDispatcher {
    protected client: LanguageClient;
    protected statusBar: VSCodePvsStatusBar;
    protected emacsBindings: VSCodePvsEmacsBindingsProvider;
    protected workspaceExplorer: VSCodePvsWorkspaceExplorer;
    protected proofExplorer: VSCodePvsProofExplorer;
    protected vscodePvsTerminal: VSCodePvsTerminal;
    protected proofMate: VSCodePvsProofMate;

    constructor (client: LanguageClient, handlers: {
        statusBar: VSCodePvsStatusBar,
        emacsBindings: VSCodePvsEmacsBindingsProvider,
        workspaceExplorer: VSCodePvsWorkspaceExplorer,
        proofExplorer: VSCodePvsProofExplorer,
        vscodePvsTerminal: VSCodePvsTerminal,
        proofMate: VSCodePvsProofMate
    }) {
        this.client = client;
        this.statusBar = handlers.statusBar;
        this.emacsBindings = handlers.emacsBindings;
        this.workspaceExplorer = handlers.workspaceExplorer;
        this.proofExplorer = handlers.proofExplorer;
        this.vscodePvsTerminal = handlers.vscodePvsTerminal;
        this.proofMate = handlers.proofMate;
    }
    protected resource2desc (resource: string | { 
        fileName?: string, fileExtension?: string, contextFolder?: string, theoryName?: string, formulaName?: string,
        path?: string,
        contextValue?: string
    }): { 
        contextFolder: string,
        fileName?: string, fileExtension?: string,
        theoryName?: string, 
        formulaName?: string
    } {
        if (resource) {
            if (typeof resource === "string") {
                const isFolder: boolean = !fsUtils.isPvsFile(resource);
                if (isFolder) {
                    return { contextFolder: fsUtils.normalizeContextFolder(resource) };
                }
                return {
                    fileName: fsUtils.getFileName(resource),
                    fileExtension: fsUtils.getFileExtension(resource),
                    contextFolder: fsUtils.getContextFolder(resource)
                };
            } else if (resource.contextFolder) {
                //@ts-ignore
                return resource;
            } else if (resource.path) {
                // resource coming from the editor
                // resource is of type vscode.Uri
                const isFolder: boolean = !fsUtils.isPvsFile(resource.path);
                if (isFolder) {
                    return { contextFolder: fsUtils.normalizeContextFolder(resource.path) };
                }
                return {
                    fileName: fsUtils.getFileName(resource.path),
                    fileExtension: fsUtils.getFileExtension(resource.path),
                    contextFolder: fsUtils.getContextFolder(resource.path)
                };
            } else if (resource.contextValue) {
                // resource coming from explorer
                // resource is of type FormulaItem or OverviewItem
                if (resource.contextValue.endsWith("-overview")) {
                    return {
                        contextFolder: resource["getContextFolder"](),
                    };    
                }
                return {
                    fileName: fsUtils.getFileName(resource["getFileName"]()),
                    fileExtension: ".pvs",
                    contextFolder: resource["getContextFolder"](),
                    formulaName: resource["getFormulaName"](),
                    theoryName: resource["getTheoryName"]()
                    //, line: resource.getPosition().line
                };
            }
        }
        console.error(`[event-dispatcher] Warning: could not describe resource `, resource);
        return null;
    }
	activate (context: ExtensionContext): void {
        // -- handlers for server responses
		this.client.onRequest(serverEvent.pvsVersionInfo, (version: PvsVersionDescriptor) => {
			if (version) {
                this.statusBar.pvsReady(version);
                this.proofExplorer.pvsReady(version);
                commands.executeCommand("vscode-pvs.show-version-info", { trailingNote: " :: Ready! ::"});
			}
		});
		this.client.onRequest(serverEvent.contextUpdate, (desc: PvsContextDescriptor) => {
			if (this.workspaceExplorer) {
				this.workspaceExplorer.updateContextFolder(desc);
			}
		});
		this.client.onRequest(serverEvent.typecheckFileResponse, (desc: { 
            response: PvsResponse, 
            args: { 
                fileName: string, 
                fileExtension: string, 
                contextFolder: string 
            }
        }) => {
            // request tccs for the files that typecheck correctly
            if (desc && desc.response && !desc.response.error && desc.args) {
                desc.args["opt"] = { quiet: true };
                this.client.sendRequest(serverCommand.generateTccs, desc.args);
            }
        });
		this.client.onRequest(serverEvent.showTccsResponse, (desc: { 
            response: PvsContextDescriptor, 
            args: { 
                fileName: string, 
                fileExtension: string, 
                contextFolder: string 
            }
        }) => {
            // console.log(desc);
            if (this.workspaceExplorer && desc.response) {
                this.workspaceExplorer.updateContextFolder(desc.response, { tccDescriptor: true });
            }
            if (desc && desc.args) {
                const fname: string = fsUtils.desc2fname(desc.args);
                if (desc && desc.response 
                        &&  desc.response.fileDescriptors 
                        && desc.response.fileDescriptors[fname] 
                        && desc.response.fileDescriptors[fname].theories 
                        && desc.response.fileDescriptors[fname].theories.length) {
                    // open tcc file in the editor
                    const uri: vscode.Uri = vscode.Uri.file(fsUtils.desc2fname({ fileName: desc.args.fileName, contextFolder: desc.args.contextFolder, fileExtension: ".tccs"}));
                    const editors: vscode.TextEditor[] = vscode.window.visibleTextEditors;
                    const viewColumn: number = (editors && editors.length > 0) ? editors[0].viewColumn : vscode.ViewColumn.Beside;
                    vscode.window.showTextDocument(uri, { preserveFocus: true, preview: true, viewColumn });
                }
            }
        });
        this.client.onRequest(serverEvent.dischargeTccsResponse, (desc: {
            response: DischargeTccsResult, 
            args: { 
                fileName: string, 
                fileExtension: string, 
                contextFolder: string 
            }
        }) => {
            // generate tccs again
            desc.args["opt"] = { quiet: true };
            this.client.sendRequest(serverCommand.generateTccs, desc.args);
        });
		this.client.onRequest(serverEvent.generateTccsResponse, (desc: {
            response: PvsContextDescriptor, 
            args: { 
                fileName: string, 
                fileExtension: string, 
                contextFolder: string 
            }
        }) => {
            // console.log(desc);
            if (this.workspaceExplorer && desc.response) {
                this.workspaceExplorer.updateContextFolder(desc.response, { tccDescriptor: true });
            }
            // if (desc && desc.response && desc.response.theories && desc.response.theories.length) {
            //     // open tcc file in the editor
            //     const uri: vscode.Uri = vscode.Uri.file(fsUtils.desc2fname({ fileName: desc.args.fileName, contextFolder: desc.args.contextFolder, fileExtension: ".tccs"}));
            //     const editors: vscode.TextEditor[] = vscode.window.visibleTextEditors;
            //     const viewColumn: number = (editors && editors.length > 0) ? editors[0].viewColumn : vscode.ViewColumn.Beside;
            //     vscode.window.showTextDocument(uri, { preserveFocus: true, preview: true, viewColumn });
            // }
        });
		this.client.onRequest(serverEvent.parseFileResponse, (res: PvsResponse) => {
            // show stats
            if (res && res["math-objects"] && res.contextFolder && res.fileName) {
                // display info in the status bar
                const stats: { types: number, definitions: number, lemmas: number } = <{ types: number, definitions: number, lemmas: number }> res["math-objects"];
                this.statusBar.updateStats({ contextFolder: res["contextFolder"], fileName: res["fileName"], fileExtension: res["fileExtension"], stats });
                this.statusBar.showStats();
            }
        });
		this.client.onRequest(serverEvent.workspaceStats, (res: PvsResponse) => {
            // show stats
            if (res) {
                if (res["files"]) {
                    this.statusBar.setFiles(res["files"]);
                }
                if (res["contextFolder"]) {
                    this.statusBar.setContextFolder(res["contextFolder"]);
                    if (res["math-objects"] && res["contextFolder"]) {
                        // display info in the status bar
                        const stats: { types: number, definitions: number, lemmas: number } = <{ types: number, definitions: number, lemmas: number }> res["math-objects"];
                        this.statusBar.updateStats({ contextFolder: res["contextFolder"], fileName: res["fileName"], fileExtension: res["fileExtension"], stats });
                        this.statusBar.showStats();
                    }
                }
            }
        });
        this.client.onRequest(serverEvent.proofCommandResponse, (desc: {
            response: PvsResponse, 
            args: { 
                fileName: string, 
                fileExtension: string, 
                contextFolder: string, 
                theoryName: string, 
                formulaName: string, 
                cmd: string 
            }
        }) => {
            if (desc) {
                // notify proofexplorer
                this.proofExplorer.onStepExecuted(desc);
                if (desc.response && desc.response.result) {
                    // update proof mate
                    this.proofMate.updateRecommendations(desc.response.result);
                }
            }
        });
        this.client.onRequest(serverEvent.proveFormulaResponse, (desc: {
            response: PvsResponse, 
            args: { 
                fileName: string, 
                fileExtension: string, 
                theoryName: string, 
                formulaName: string, 
                contextFolder: string 
            }, 
            pvsLogFile: string,
            pvsTmpLogFile: string
        }) => {
            if (desc) {
                // initialise sequent viewer
                this.proofExplorer.setLogFileName(desc);
                if (desc.response && desc.response.result) {
                    // update proof mate
                    this.proofMate.setProofDescriptor(desc.args);
                    this.proofMate.updateRecommendations(desc.response.result);
                    // save initial proof state in proof explorer
                    this.proofExplorer.setInitialProofState(desc.response.result);
                }
                // request proof script
                // this.client.sendRequest(serverCommand.loadProof, desc.args); // this operation is now performed on the server
                this.proofExplorer.startProof();
                
                // set vscode context variable prover-session-active to true
                vscode.commands.executeCommand('setContext', 'prover-session-active', true);
            }
        });
		this.client.onRequest(serverEvent.dischargeTheoremsResponse, (desc: { response: PvsResponse, args: { fileName: string, fileExtension: string, theoryName: string, formulaName: string, contextFolder: string }, proofFile: string }) => {
            // do nothing for now
        });
        this.client.onRequest(serverEvent.loadProofResponse, (desc: { response: { result: ProofDescriptor } | null, args: { fileName: string, fileExtension: string, theoryName: string, formulaName: string, contextFolder: string }, proofFile: string }) => {
            if (desc) {
                console.log(desc);
                if (desc.response && desc.response.result) {
                    console.dir(desc.response.result);
                    this.proofExplorer.setProofDescriptor(desc.args);
                    this.proofExplorer.loadProofDescriptor(desc.response.result);
                } else {
                    console.error(`[event-dispatcher] Error: ${serverEvent.loadProofResponse} response indicates error`, desc);
                    window.showErrorMessage(`[event-dispatcher] Error: ${serverEvent.loadProofResponse} response indicates error (please check pvs-server output for details)`);
                }
            } else {
                console.error(`[event-dispatcher] Error: ${serverEvent.loadProofResponse} received null response`);
                window.showErrorMessage(`[event-dispatcher] Error: ${serverEvent.loadProofResponse} received null response`);
            }
        });
        // proof-state handler
        this.client.onRequest(serverEvent.proofStateUpdate, async (desc: { response: PvsResponse, args: { fileName: string, fileExtension: string, theoryName: string, formulaName: string, contextFolder: string }, pvsLogFile: string, pvsTmpLogFile: string }) => {
            // do nothing for now
        });

        this.client.onRequest(serverEvent.startEvaluatorResponse, (desc: { response: PvsResponse, args: { fileName: string, fileExtension: string, theoryName: string, contextFolder: string }}) => {
            console.log(desc);
        });

        this.client.onRequest(serverEvent.pvsServerCrash, (desc: { msg?: string }) => {
            desc = desc || {};
            const msg: string = desc.msg || "Ups, pvs-server just crashed :/";
            this.statusBar.failure(msg);
        });

		this.client.onRequest(serverEvent.saveProofEvent, (request: { 
            args: {
                fileName: string, 
                fileExtension: string, 
                contextFolder: string, 
                theoryName: string, 
                formulaName: string, 
                cmd: string
            } 
		}) => {
			this.proofExplorer.saveProof();		
		});
		this.client.onRequest(serverEvent.quitProofEvent, (request: {
            args: { 
                fileName: string, 
                fileExtension: string, 
                contextFolder: string, 
                theoryName: string, 
                formulaName: string, 
                cmd: string
            } 
		}) => {
            this.proofExplorer.saveProof();
            this.vscodePvsTerminal.deactivate();
		});
		this.client.onRequest(serverEvent.QED, (request: {
            args: { 
                fileName: string, 
                fileExtension: string, 
                contextFolder: string, 
                theoryName: string, 
                formulaName: string, 
                cmd: string
            } 
		}) => {
            const msg: string = (request && request.args && request.args.formulaName) ? `Proof completed successfully!` : null;
            this.proofExplorer.saveProof({ msg });
            this.vscodePvsTerminal.deactivate();
		});




        //---------------------------------------------------------
        // commands invoked using code lens, emacs bindings, explorer, etc
        //---------------------------------------------------------

        // vscode-pvs.send-proof-command
        context.subscriptions.push(commands.registerCommand("vscode-pvs.send-proof-command", (desc: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string, cmd: string }) => {
            this.vscodePvsTerminal.sendProofCommand(desc);
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.select-profile", (desc: { profile: commandUtils.ProofMateProfile }) => {
            this.vscodePvsTerminal.selectProfile(desc);
        }));
        
        context.subscriptions.push(commands.registerCommand("vscode-pvs.show-version-info", async (opt?: { trailingNote?: string }) => {
            opt = opt || {};
            opt.trailingNote = opt.trailingNote || "";
            let info: PvsVersionDescriptor = {
                "pvs-version": "PVS version not available :/",
                "lisp-version": ""
            };
            const desc: PvsVersionDescriptor = this.statusBar.getVersionInfo();
            if (desc) {
                if (desc["pvs-version"]) { info["pvs-version"] = desc["pvs-version"]; }
                if (desc["lisp-version"]) { info["lisp-version"] = desc["lisp-version"]; }
            }
            const msg: string = `PVS ${info["pvs-version"]} (${info["lisp-version"]}) ${opt.trailingNote}`;
            window.showInformationMessage(msg);
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.reboot-pvs", async () => {
            // ask the user confirmation before restarting pvs
			const yesno: string[] = [ "Yes", "No" ];
			const msg: string = `Reboot pvs-server?\n\nThis action can resolve situations where the server crashed or is not responding.`;
			const ans: string = await vscode.window.showInformationMessage(msg, { modal: true }, yesno[0])
			if (ans === yesno[0]) {
                this.statusBar.showProgress("Rebooting pvs-server...");	
                this.client.sendRequest(serverCommand.rebootPvsServer);
            }
        }));

        context.subscriptions.push(commands.registerCommand("vscode-pvs.print-warning-message-in-terminal", (desc: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string, cmd: string }) => {
            // TODO
            // this.vscodePvsTerminal.printWarningMessage(desc);
        }));

        // vscode-pvs-metax
        context.subscriptions.push(commands.registerCommand("vscode-pvs.metax", () => {
            this.emacsBindings.metaxPrompt();
        }));

        // vscode-display-prooflite-script
        context.subscriptions.push(commands.registerCommand("vscode-pvs.display-prooflite-script", async (resource: string | { path: string } | { contextValue: string }) => {
            if (window.activeTextEditor && window.activeTextEditor.document) {
                // if the file is currently open in the editor, save file first
                await window.activeTextEditor.document.save();
                if (!resource) {
                    resource = { path: window.activeTextEditor.document.fileName };
                }
            }
            if (resource) {
                let desc = <{ 
                    fileName: string, fileExtension: string, contextFolder: string, 
                    theoryName: string, formulaName: string 
                }> this.resource2desc(resource);
                if (desc) {
                    if (!desc.theoryName) {
                        const document: vscode.TextDocument = window.activeTextEditor.document;
                        const line: number = window.activeTextEditor.selection.active.line;
                        const theoryName: string = utils.findTheoryName(document.getText(), line);
                        desc.theoryName = theoryName;
                    }
                    if (desc.theoryName) {
                        this.client.sendRequest(serverCommand.displayProofLiteScript, desc);
                    } else {
                        window.showErrorMessage(`Error while trying to display prooflite script (could not identify theory name, please check that the file typechecks correctly)`);
                    }
                } else {
                    console.error("[vscode-events-dispatcher] Error: prooflite script requested for unknown resource", resource);
                }
            } else {
                console.error("[vscode-events-dispatcher] Error: prooflite script request is null", resource);
            }
        }));

        // pvsio-evaluator
        context.subscriptions.push(commands.registerCommand("vscode-pvs.pvsio-evaluator", async (resource: string | { path: string } | { contextValue: string }) => {
            if (window.activeTextEditor && window.activeTextEditor.document) {
                // if the file is currently open in the editor, save file first
                await window.activeTextEditor.document.save();
                if (!resource) {
                    resource = { path: window.activeTextEditor.document.fileName };
                }
            }
            if (resource) {
                let desc = <{ 
                    fileName: string, fileExtension: string, contextFolder: string, 
                    theoryName: string, formulaName: string 
                }> this.resource2desc(resource);
                if (desc) {
                    if (!desc.theoryName) {
                        const document: vscode.TextDocument = window.activeTextEditor.document;
                        const line: number = window.activeTextEditor.selection.active.line;
                        const theoryName: string = utils.findTheoryName(document.getText(), line);
                        desc.theoryName = theoryName;
                    }
                    if (desc.theoryName) {
                        await this.vscodePvsTerminal.startEvaluatorSession(desc);
                    } else {
                        window.showErrorMessage(`Error while trying to invoke PVSio (could not identify theory name, please check that the file typechecks correctly)`);
                    }
                } else {
                    console.error("[vscode-events-dispatcher] Error: pvsio-evaluator invoked over an unknown resource", resource);
                }
            } else {
                console.error("[vscode-events-dispatcher] Error: pvsio-evaluator invoked with null resource", resource);
            }
        }));

        // vscode-pvs.prove-formula
        context.subscriptions.push(commands.registerCommand("vscode-pvs.prove-formula", async (resource) => {
            if (resource) {
                let desc = <{ 
                    fileName: string, fileExtension: string, contextFolder: string, 
                    theoryName: string, formulaName: string 
                }> this.resource2desc(resource);
                if (desc) {
                    if (desc.theoryName) {
                        await this.vscodePvsTerminal.startProverSession(desc);
                    } else {
                        console.error("[vscode-events-dispatcher] Error: theory name is null", desc);
                    }
                } else {
                    console.error("[vscode-events-dispatcher] Error: unknown vscode-pvs.prove-formula resource", resource);
                }
            } else {
                console.error("[vscode-events-dispatcher] Error: prove-formula invoked with null resource", resource);
            }
        }));

        // vscode-pvs.discharge-tccs
        context.subscriptions.push(commands.registerCommand("vscode-pvs.discharge-tccs", async (resource) => {
            if (resource) {
                let desc = <{ 
                    fileName: string, fileExtension: string, contextFolder: string, 
                    theoryName: string, formulaName: string 
                }> this.resource2desc(resource);
                if (desc) {
                    // send discharge-tccs request to pvs-server
                    this.client.sendRequest(serverCommand.dischargeTccs, desc);
                } else {
                    console.error("[vscode-events-dispatcher] Error: unknown vscode-pvs.prove-formula resource", resource);
                }
            } else {
                console.error("[vscode-events-dispatcher] Error: prove-formula invoked with null resource", resource);
            }
        }));
        // alias for vscode-pvs.discharge-tccs
		context.subscriptions.push(commands.registerCommand("vscode-pvs.discharge-tccs-alt", async (resource: string | { path: string } | { contextValue: string }) => {
            commands.executeCommand("vscode-pvs.discharge-tccs", resource);
        }));


        // vscode-pvs.discharge-theorems
        context.subscriptions.push(commands.registerCommand("vscode-pvs.discharge-theorems", async (resource) => {
            if (resource) {
                let desc = <{ 
                    fileName: string, fileExtension: string, contextFolder: string, 
                    theoryName: string, formulaName: string 
                }> this.resource2desc(resource);
                if (desc) {
                    // send discharge-theorems request to pvs-server
                    this.client.sendRequest(serverCommand.dischargeTheorems, desc);
                } else {
                    console.error("[vscode-events-dispatcher] Error: unknown vscode-pvs.prove-formula resource", resource);
                }
            } else {
                console.error("[vscode-events-dispatcher] Error: prove-formula invoked with null resource", resource);
            }
        }));
        // alias for vscode-pvs.discharge-tccs
        context.subscriptions.push(commands.registerCommand("vscode-pvs.discharge-theorems-alt", async (resource: string | { path: string } | { contextValue: string }) => {
            commands.executeCommand("vscode-pvs.discharge-theorems", resource);
        }));
        
        // vscode-pvs.typecheck-file
		context.subscriptions.push(commands.registerCommand("vscode-pvs.typecheck-file", async (resource: string | { path: string } | { contextValue: string }) => {
            if (window.activeTextEditor && window.activeTextEditor.document) {
                // if the file is currently open in the editor, save file first
                await window.activeTextEditor.document.save();
                if (!resource) {
                    resource = { path: window.activeTextEditor.document.fileName };
                }
            }
			if (resource) {
                let desc = this.resource2desc(resource);
                if (desc) {
                    // send typecheck request to pvs-server
                    this.client.sendRequest(serverCommand.typecheckFile, desc);
                }
            } else {
                console.error("[vscode-events-dispatcher] Warning: resource is null", resource);
            }
        }));
        // alias for vscode-pvs.typecheck-file
        context.subscriptions.push(commands.registerCommand("vscode-pvs.typecheck", async (resource: string | { path: string } | { contextValue: string }) => {
            commands.executeCommand("vscode-pvs.typecheck-file", resource);
        }));
        
        
        // vscode-pvs.show-tccs
		context.subscriptions.push(commands.registerCommand("vscode-pvs.show-tccs", async (resource: string | { path: string } | { contextValue: string }) => {
            if (!resource && window.activeTextEditor && window.activeTextEditor.document) {
                resource = { path: window.activeTextEditor.document.fileName };
            }
			if (resource) {
                let desc = <{ 
                    fileName: string, fileExtension: string, contextFolder: string
                }> this.resource2desc(resource);
                if (desc) {
                    // send show-tccs request to pvs-server
                    this.client.sendRequest(serverCommand.showTccs, desc);
                }
            } else {
                console.error("[vscode-events-dispatcher] Warning: resource is null", resource);
            }
        }));
        // alias for vscode-pvs.show-tccs
		context.subscriptions.push(commands.registerCommand("vscode-pvs.show-tccs-alt", async (resource: string | { path: string } | { contextValue: string }) => {
            commands.executeCommand("vscode-pvs.show-tccs", resource);
        }));
        // vscode-pvs.generate-tccs
		context.subscriptions.push(commands.registerCommand("vscode-pvs.generate-tccs", async (resource: string | { path: string } | { contextValue: string }) => {
            if (!resource && window.activeTextEditor && window.activeTextEditor.document) {
                resource = { path: window.activeTextEditor.document.fileName };
            }
			if (resource) {
                let desc = <{ 
                    fileName: string, fileExtension: string, contextFolder: string
                }> this.resource2desc(resource);
                if (desc) {
                    // send generate-tccs request to pvs-server
                    this.client.sendRequest(serverCommand.generateTccs, desc);
                }
            } else {
                console.error("[vscode-events-dispatcher] Warning: resource is null", resource);
            }
        }));

        // vscode-pvs.parse-file
		context.subscriptions.push(commands.registerCommand("vscode-pvs.parse-file", async (resource: string | { path: string } | { contextValue: string }) => {
            if (!resource && window.activeTextEditor && window.activeTextEditor.document) {
                resource = { path: window.activeTextEditor.document.fileName };
            }
			if (resource) {
                let desc = <{ 
                    fileName: string, fileExtension: string, contextFolder: string
                }> this.resource2desc(resource);
                if (desc) {
                    // send parse request to pvs-server
                    this.client.sendRequest(serverCommand.parseFileWithFeedback, desc);
                }
            } else {
                console.error("[vscode-events-dispatcher] Warning: resource is null", resource);
            }
        }));
        
        // vscode-pvs.parse-workspace
		context.subscriptions.push(commands.registerCommand("vscode-pvs.parse-workspace", async (resource: string | { path: string } | { contextValue: string }) => {
            if (!resource && window.activeTextEditor && window.activeTextEditor.document) {
                resource = { path: window.activeTextEditor.document.fileName };
            }
			if (resource) {
                let desc = this.resource2desc(resource);
                if (desc) {
                    // send parse request to pvs-server
                    this.client.sendRequest(serverCommand.parseWorkspaceWithFeedback, desc);
                }
            } else {
                console.error("[vscode-events-dispatcher] Warning: resource is null", resource);
            }
		}));

        // vscode-pvs.typecheck-workspace
		context.subscriptions.push(commands.registerCommand("vscode-pvs.typecheck-workspace", async (resource: string | { path: string } | { contextValue: string }) => {
            if (!resource && window.activeTextEditor && window.activeTextEditor.document) {
                resource = { path: window.activeTextEditor.document.fileName };
            }
			if (resource) {
                let desc = this.resource2desc(resource);
                if (desc) {
                    // send parse request to pvs-server
                    this.client.sendRequest(serverCommand.typecheckWorkspace, desc);
                }
            } else {
                console.error("[vscode-events-dispatcher] Warning: resource is null", resource);
            }
        }));
        
        // vscode-pvs.hp2pvs
		context.subscriptions.push(commands.registerCommand("vscode-pvs.hp2pvs", async (resource: string | { path: string } | { contextValue: string }) => {
            if (!resource && window.activeTextEditor && window.activeTextEditor.document) {
                resource = { path: window.activeTextEditor.document.fileName };
            }
			if (resource) {
                let desc = this.resource2desc(resource);
                if (desc) {
                    // send parse request to pvs-server
                    this.client.sendRequest(serverCommand.hp2pvs, desc);
                }
            } else {
                console.error("[vscode-events-dispatcher] Warning: resource is null", resource);
            }
		}));

        //----------------------------------
        // events triggered by pvs-language-server
        //----------------------------------
        this.client.onNotification("server.status.error", (desc: { msg: string }) => {
            if (desc && desc.msg) {
                window.showErrorMessage(desc.msg);
            }
        });
        this.client.onNotification("server.status.progress", (desc: { msg: string }) => {
            if (desc && desc.msg) {
                this.statusBar.showProgress(desc.msg);
            }
        });
        this.client.onNotification("server.important-notification", (desc?: { msg?: string }) => {
            this.statusBar.ready();
            if (desc && desc.msg) {
                window.showInformationMessage(desc.msg);
            }
        });
        this.client.onNotification("server.status.start-important-task", (desc: { id: string, msg: string, increment?: number }) => {
            if (desc && desc.msg) {
                // show dialog with progress
                window.withProgress({
                    location: ProgressLocation.Notification,
                    cancellable: true
                }, (progress, token) => { 
                    // show initial dialog with spinning progress   
                    progress.report({ increment: -1, message: desc.msg });
                    // update the dialog
                    return new Promise((resolve, reject) => {
                        token.onCancellationRequested(() => {
                            // send cancellation request to the server
                            this.client.sendRequest(serverCommand.cancelOperation);
                            // dispose of the dialog
                            resolve(null);
                        });
                        this.client.onNotification(`server.status.progress-important-task-${desc.id}`, (desc: { msg: string, increment?: number }) => {
                            if (desc) {
                                progress.report({
                                    increment: isNaN(desc.increment) ? -1 : desc.increment,
                                    message: desc.msg
                                });
                            }
                        });
                        this.client.onNotification(`server.status.end-important-task-${desc.id}`, (desc: { msg?: string }) => {
                            this.statusBar.ready();
                            resolve(null);
                            if (desc && desc.msg) {
                                window.showInformationMessage(desc.msg);
                            }
                        });
                        this.client.onNotification(`server.status.end-important-task-${desc.id}-with-errors`, (desc: { msg: string }) => {
                            this.statusBar.ready();
                            resolve(null);
                            if (desc && desc.msg) {
                                this.statusBar.showError(desc.msg); // use the status bar rather than dialogs, because we don't have APIs to close old dialogs with potentially stale information
                                // window.showErrorMessage(desc.msg);
                            }
                        });
                    });
                });

                // show progress on the status bar
                this.statusBar.showProgress(desc.msg);
            }

        });

        this.client.onNotification("server.status.report-error", (desc: { msg: string }) => {
            if (desc && desc.msg) {
                window.showErrorMessage(desc.msg);
            }
            this.statusBar.ready();
        });
    }
}