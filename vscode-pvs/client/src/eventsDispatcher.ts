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
import { VSCodePvsWorkspaceExplorer, TheoryItem, TccsOverviewItem } from "./views/vscodePvsWorkspaceExplorer";
import { VSCodePvsProofExplorer, ProofItem } from "./views/vscodePvsProofExplorer";
import { VSCodePvsTerminal } from "./views/vscodePvsTerminal";
import { PvsContextDescriptor, serverEvent, serverRequest, PvsVersionDescriptor, ProofDescriptor, ServerMode, FormulaDescriptor, PvsFormula, ProofNodeX, ProofEditEvent, PvsProofCommand, PvsFile, ProofStatus, ProofExecEvent, PvsTheory, ProofExecInterruptProver, WorkspaceEvent, ProofExecInterruptAndQuitProver } from "./common/serverInterface";
import { window, commands, ExtensionContext, ProgressLocation } from "vscode";
import * as vscode from 'vscode';
import { PvsResponse } from "./common/pvs-gui";
import * as fsUtils from './common/fsUtils';
import { VSCodePvsProofMate } from "./views/vscodePvsProofMate";
import * as utils from './common/languageUtils';
import * as commandUtils from './common/commandUtils';
import * as vscodeUtils from './utils/vscode-utils';
import { VSCodePvsLogger } from "./views/vscodePvsLogger";
import { VSCodePvsPackageManager } from "./providers/vscodePvsPackageManager";
import * as path from 'path';

// FIXME: use publish-subscribe to allow easier introduction of new components
export class EventsDispatcher {
    protected client: LanguageClient;
    protected statusBar: VSCodePvsStatusBar;
    protected emacsBindings: VSCodePvsEmacsBindingsProvider;
    protected workspaceExplorer: VSCodePvsWorkspaceExplorer;
    protected proofExplorer: VSCodePvsProofExplorer;
    protected vscodePvsTerminal: VSCodePvsTerminal;
    protected proofMate: VSCodePvsProofMate;
    protected logger: VSCodePvsLogger;
    protected packageManager: VSCodePvsPackageManager;

    protected inChecker: boolean = false;
    protected quietMode: boolean = false;

    constructor (client: LanguageClient, handlers: {
        statusBar: VSCodePvsStatusBar,
        emacsBindings: VSCodePvsEmacsBindingsProvider,
        workspaceExplorer: VSCodePvsWorkspaceExplorer,
        proofExplorer: VSCodePvsProofExplorer,
        vscodePvsTerminal: VSCodePvsTerminal,
        proofMate: VSCodePvsProofMate,
        logger: VSCodePvsLogger,
        packageManager: VSCodePvsPackageManager
    }) {
        this.client = client;
        this.statusBar = handlers.statusBar;
        this.emacsBindings = handlers.emacsBindings;
        this.workspaceExplorer = handlers.workspaceExplorer;
        this.proofExplorer = handlers.proofExplorer;
        this.vscodePvsTerminal = handlers.vscodePvsTerminal;
        this.proofMate = handlers.proofMate;
        this.logger = handlers.logger;
        this.packageManager = handlers.packageManager;
    }
    protected resource2desc (resource: string | { 
        fileName?: string, fileExtension?: string, contextFolder?: string, theoryName?: string, formulaName?: string,
        path?: string,
        contextValue?: string
    }): { 
        contextFolder: string,
        fileName: string,
        fileExtension: string,
        theoryName: string, 
        formulaName: string
    } {
        if (resource) {
            if (typeof resource === "string") {
                const isFolder: boolean = !fsUtils.isPvsFile(resource);
                if (isFolder) {
                    return { 
                        contextFolder: fsUtils.normalizeContextFolder(resource),
                        fileName: null,
                        fileExtension: null,
                        theoryName: null,
                        formulaName: null
                    };
                }
                return {
                    fileName: fsUtils.getFileName(resource),
                    fileExtension: fsUtils.getFileExtension(resource),
                    contextFolder: fsUtils.getContextFolder(resource),
                    theoryName: null,
                    formulaName: null
                };
            } else if (resource.contextFolder) {
                //@ts-ignore
                return {
                    fileName: resource.fileName,
                    fileExtension: resource.fileExtension,
                    contextFolder: resource.contextFolder,
                    theoryName: resource.theoryName,
                    formulaName: resource.formulaName
                };
            } else if (resource.path) {
                // resource coming from the editor
                // resource is of type vscode.Uri
                const isFolder: boolean = !fsUtils.isPvsFile(resource.path);
                if (isFolder) {
                    return {
                        contextFolder: fsUtils.normalizeContextFolder(resource.path),
                        fileName: null,
                        fileExtension: null,
                        theoryName: null,
                        formulaName: null
                    };
                }
                return {
                    fileName: fsUtils.getFileName(resource.path),
                    fileExtension: fsUtils.getFileExtension(resource.path),
                    contextFolder: fsUtils.getContextFolder(resource.path),
                    theoryName: null,
                    formulaName: null
                };
            } else if (resource.contextValue) {
                // resource coming from explorer
                // resource is of type FormulaItem or OverviewItem
                if (resource.contextValue.endsWith("-overview")) {
                    return {
                        contextFolder: resource["getContextFolder"](),
                        fileName: null,
                        fileExtension: null,
                        theoryName: null,
                        formulaName: null
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
                this.statusBar.showDownloadNasalibButton(!version["nasalib-version"]);
                // this.proofExplorer.pvsReady(version);
                // make sure a valid workspace is open in vscode
                if (!vscode.workspace.name) {
                    const fname: string = (vscode.window && vscode.window.activeTextEditor 
                        && vscode.window.activeTextEditor.document
                        && vscode.window.activeTextEditor.document.fileName) ? 
                            vscode.window.activeTextEditor.document.fileName 
                            : null;
                    if (fname) {
                        const cc: string = fsUtils.getContextFolder(fname);
                        if (cc) {
                            const uri: vscode.Uri = vscode.Uri.file(cc);
                            commands.executeCommand('vscode.openFolder', uri).then(async () => {
                                await window.showTextDocument(uri, { preserveFocus: true, preview: true });
                                commands.executeCommand("vscode-pvs.show-version-info", { trailingNote: " :: Ready! ::"});
                            });
                        }
                    }
                } else {
                    commands.executeCommand("vscode-pvs.show-version-info", { trailingNote: " :: Ready! ::"});
                    // commands.executeCommand("vscode-pvs.show-version-info", { trailingNote: ` :: ${vscode.workspace.name} ::`});
                }
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
                this.client.sendRequest(serverRequest.generateTccs, {
                    fileName: desc.args.fileName,
                    fileExtension: desc.args.fileExtension,
                    contextFolder: desc.args.contextFolder,
                    quiet: true
                });
            }
        });
        this.client.onRequest(serverEvent.generateTccsResponse, (desc: {
            response: PvsContextDescriptor, 
            args: { 
                fileName: string, 
                fileExtension: string, 
                contextFolder: string 
            }
        }) => {
            if (this.workspaceExplorer && desc.response) {
                this.workspaceExplorer.updateContextFolder(desc.response, { tccDescriptor: true });
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
            if (this.workspaceExplorer && desc.response) {
                this.workspaceExplorer.updateContextFolder(desc.response, { tccDescriptor: true });
            }
            if (desc && desc.args) {
                if (desc && desc.response) {
                    // open tcc file in the editor
                    const tccsContextFolder: string = path.join(desc.args.contextFolder, "pvsbin");
                    const uri: vscode.Uri = vscode.Uri.file(fsUtils.desc2fname({ fileName: desc.args.fileName, contextFolder: tccsContextFolder, fileExtension: ".tccs"}));
                    const editors: vscode.TextEditor[] = vscode.window.visibleTextEditors;
                    const viewColumn: number = (editors && editors.length > 0) ? editors[0].viewColumn : vscode.ViewColumn.Beside;
                    vscode.window.showTextDocument(uri, { preserveFocus: true, preview: true, viewColumn });
                }
            }
        });
		this.client.onRequest(serverEvent.parseFileResponse, (res: PvsResponse) => {
            // show stats
            if (res && res["math-objects"] && res.contextFolder && res.fileName) {
                // display info in the status bar
                const stats: { types: number, definitions: number, lemmas: number } = <{ types: number, definitions: number, lemmas: number }> res["math-objects"];
                this.statusBar.updateStats({ contextFolder: res["contextFolder"], fileName: res["fileName"], fileExtension: res["fileExtension"], stats });
            }
        });
		this.client.onRequest(serverEvent.workspaceStats, (res: PvsResponse) => {
            // show stats
            if (res) {
                if (res["contextFolder"]) {
                    this.statusBar.setContextFolder(res["contextFolder"]);
                    if (res["math-objects"] && res["contextFolder"]) {
                        // display info in the status bar
                        const stats: { types: number, definitions: number, lemmas: number } = <{ types: number, definitions: number, lemmas: number }> res["math-objects"];
                        this.statusBar.updateStats({ contextFolder: res["contextFolder"], fileName: res["fileName"], fileExtension: res["fileExtension"], stats });
                    }
                }
            }
        });
        this.client.onRequest(serverEvent.proofCommandResponse, async (desc: {
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
                // await this.proofExplorer.onStepExecuted(desc);
                if (desc.response && desc.response.result) {
                    // update proof mate
                    this.proofMate.updateRecommendations(desc.response.result);
                }
            }
        });

        //----------------
        this.client.onNotification(serverEvent.workspaceEvent, (desc: WorkspaceEvent) => {
            switch (desc.action) {
                case "did-rename-file": {
                    if (vscode.window && vscode.window.activeTextEditor && vscode.window.activeTextEditor.document
                            && fsUtils.isSameFile(vscode.window.activeTextEditor.document.fileName, desc.old_fname)) {
                        setTimeout(() => {
                            vscode.window.showTextDocument(vscode.Uri.file(desc.new_fname), { preserveFocus: false });
                        }, 250);
                    }
                    break;
                }
                default: {
                    console.warn(`[event-dispatcher] Warning: unrecognized prover event`, desc);
                    break;
                }
            }
        });

        //----------------
        this.client.onNotification(serverEvent.proverEvent, async (desc: ProofEditEvent | ProofExecEvent) => {
			switch (desc.action) {
				case "did-append-node": {
                    this.proofExplorer.didAppendNode(desc);
                    break; 
                }
				case "did-copy-node": {
                    this.proofExplorer.didCopyNode(desc); 
                    break; 
                }
				case "did-copy-tree": {
                    this.proofExplorer.didCopyTree(desc); 
                    break; 
                }
                case "did-delete-node": {
                    this.proofExplorer.didDeleteNode(desc);
                    break;
                }
                case "did-cut-node": {
                    this.proofExplorer.didCutNode(desc);
                    break;
                }
                case "did-cut-tree": {
                    this.proofExplorer.didCutTree(desc);
                    break;
                }
                case "did-paste-tree": {
                    this.proofExplorer.didPasteTree(desc);
                    break;
                }
				case "did-trim-node": {
                    this.proofExplorer.didTrimNode(desc); 
                    break; 
                }
				case "did-rename-node": {
                    this.proofExplorer.didRenameNode(desc); 
                    break; 
				}
				case "did-activate-cursor": {
                    this.proofExplorer.didActivateCursor(desc); 
                    break; 
				}
				case "did-deactivate-cursor": {
                    this.proofExplorer.didDeactivateCursor(desc); 
                    break; 
				}
				case "did-update-proof-status": {
                    this.proofExplorer.didUpdateProofStatus(desc);
                    break;
                }
				case "did-start-new-proof": {
                    this.proofExplorer.willStartNewProof();
                    break;
                }
                //---------------
                case "did-start-proof": {
                    this.proofExplorer.startProof();
                    this.proofMate.startProof();
                    await this.proofMate.loadSketchpadClips(); // loads sketchpad clips from the .jprf file
                    this.statusBar.showInterruptButton();
                    break;
                }
                case "did-quit-proof": { // this is sent by CliGateway when the prover CLI is closed
                    this.proofExplorer.disposeView();
                    await this.proofMate.saveSketchpadClips();  // saves sketchpad clips to the .jprf file
                    this.proofMate.disposeView();
                    this.statusBar.hideInterruptButton();
                    break;
                }
                case "did-load-sequent": {
                    this.proofExplorer.didLoadSequent(desc.sequent);
                    this.proofMate.updateRecommendations(desc.sequent);
                    break;
                }
                case "did-update-sequent": {
                    this.proofExplorer.updateTooltip(desc);
                    this.proofMate.updateRecommendations(desc.sequent);
                    break;
                }
                case "did-load-proof": {
                    if (desc && desc.formula) {
                        this.proofExplorer.loadProofStructure(desc.formula, desc.desc, desc.proof);
                        this.proofMate.loadFormula(desc.formula);
                    } else {
                        window.showWarningMessage(`Failed to load proof (null descriptor)`);
                    }
                    break;    
                }
                case "did-open-proof": {
                    if (desc && desc.formula) {
                        this.proofExplorer.loadProofStructure(desc.formula, desc.desc, desc.proof);
                        this.proofMate.loadFormula(desc.formula);
                        const fname: string = fsUtils.desc2fname(desc.formula);
                        window.showInformationMessage(`Proof ${desc.formula.formulaName} successfully loaded from ${fname}`);
                    } else {
                        window.showWarningMessage(`Failed to open proof (null descriptor)`);
                    }
                    break;
                }
                default: {
                    console.warn(`[event-dispatcher] Warning: unrecognized prover event`, desc);
                    break;
                }
            }
        });

        // register handler that will resolve the promise when the proof needs to be saved
        this.client.onRequest(serverEvent.saveProofResponse, async (desc: {
            response: { 
                success: boolean,
                msg?: string,
                proofFile: PvsFile,
                formula: PvsFormula,
                script?: string
            }, 
            args: { 
                fileName: string, 
                fileExtension: string, 
                theoryName: string, 
                formulaName: string, 
                contextFolder: string, 
                proofDescriptor: ProofDescriptor
            }
        }) => {
            if (desc && desc.response && desc.response.success && desc.response.proofFile && desc.response.formula) {
                const fname: string = fsUtils.desc2fname(desc.response.proofFile);
                if (!this.quietMode) {
                    window.showInformationMessage(`Proof ${desc.response.formula.formulaName} saved in file ${fname}`);
                }
            } else {
                if (desc && desc.args) {
                    if (desc.response && desc.response.msg) {
                        window.showErrorMessage(`Error while saving file ${fsUtils.desc2fname(desc.args)} (${desc.response.msg})`);
                    } else {
                        window.showErrorMessage(`Unexpected error while saving file ${fsUtils.desc2fname(desc.args)} (please check pvs-server output for details)`);
                    }
                    if (desc.response.script) {
                        const content: string = `# Proof for ${desc.args.formulaName}\n`
                            + 'An error occurred while trying to save your proof in PVS.\n'
                            + 'You might be using an obsolete version of PVS. Please try to re-install PVS with the command `M-x reinstall-pvs`.\n'
                            + `If the problem persists, please report an issue on [github](https://github.com/nasa/vscode-pvs/issues).\n\n`
                            + `## Your proof attempt\n`
                            + `Don't panic, your proof attempt for ${desc.args.formulaName} is not lost.\n`
                            + 'To repeat your proof, you can copy and paste the following sequence of proof commands in the prover terminal:\n'
                            + '```lisp\n'
                            + desc.response.script
                            + '\n```';
                        const fileName: string = desc.args.formulaName + ".md";
                        const fileUri: vscode.Uri = await vscodeUtils.createTextDocument(fileName, content);
                        if (fileUri) {
                            commands.executeCommand('markdown.showPreview', fileUri);
                        }
                    }
                }
            }
        });


        this.client.onRequest(serverEvent.proveFormulaResponse, (desc: {
            response: PvsResponse, 
            args: PvsFormula
        }) => {
            // if (desc) {
            //     // initialise proof explorer
            //     // this.proofExplorer.setLogFileName(desc);
            //     // this.proofExplorer.setShasum(desc.shasum);

            //     if (desc.response && desc.response.result) {
            //         // update proof mate
            //         this.proofMate.setProofDescriptor(desc.args);
            //         this.proofMate.updateRecommendations(desc.response.result);
            //         // save initial proof state in proof explorer
            //         // this.proofExplorer.loadInitialProofState(desc.response.result);
            //     }
            //     // start proof
            //     // this.proofExplorer.startProof();
            //     this.proofMate.startProof();                
            // }
        });

        // this.client.onRequest(serverEvent.loadProofResponse, (desc: { response: { result: ProofDescriptor } | null, args: FormulaDescriptor, proofFile: string }) => {
        //     if (desc) {
        //         console.log(desc);
        //         if (desc.response && desc.response.result) {
        //             // console.dir(desc.response.result);
        //             this.proofExplorer.loadFormula(desc.args);
        //             this.proofExplorer.loadProofDescriptor(desc.response.result);
        //             this.proofMate.loadFormula(desc.args);
        //         } else {
        //             console.error(`[event-dispatcher] Error: ${serverEvent.loadProofResponse} response indicates error`, desc);
        //             window.showErrorMessage(`[event-dispatcher] Error: ${serverEvent.loadProofResponse} response indicates error (please check pvs-server output for details)`);
        //         }
        //     } else {
        //         console.error(`[event-dispatcher] Error: ${serverEvent.loadProofResponse} received null response`);
        //         window.showErrorMessage(`[event-dispatcher] Error: ${serverEvent.loadProofResponse} received null response`);
        //     }
        // });


        // // proof-state handler
        // this.client.onRequest(serverEvent.proofStateUpdate, async (desc: { response: PvsResponse, args: { fileName: string, fileExtension: string, theoryName: string, formulaName: string, contextFolder: string }, pvsLogFile: string, pvsTmpLogFile: string, shasum: string }) => {
        //     // do nothing for now
        // });

        this.client.onRequest(serverEvent.startEvaluatorResponse, (desc: { response: PvsResponse, args: { fileName: string, fileExtension: string, theoryName: string, contextFolder: string }}) => {
            console.log(desc);
        });

        this.client.onRequest(serverEvent.pvsServerCrash, (desc: { msg?: string }) => {
            desc = desc || {};
            const msg: string = desc.msg || "Ups, pvs-server just crashed :/";
            this.statusBar.failure(msg);
        });

        this.client.onRequest(serverEvent.serverModeUpdateEvent, (desc: { mode: ServerMode }) => {
            if (desc) {
                switch (desc.mode) {
                    case "in-checker": {
                        this.inChecker = true;
                        vscode.commands.executeCommand('setContext', 'in-checker', true);
                        break;
                    }
                    case "pvsio":
                    case "lisp":
                    default: {
                        this.inChecker = false;
                        vscode.commands.executeCommand('setContext', 'in-checker', false);
                        break;
                    }
                }
                this.proofExplorer.didUpdateServerMode(desc.mode);
                this.workspaceExplorer.refreshView();
            }
        });

		this.client.onRequest(serverEvent.getContextDescriptorResponse, (desc: PvsContextDescriptor) => {
            this.workspaceExplorer.updateContextFolder(desc);
        });

		this.client.onRequest(serverEvent.querySaveProof, async (request: {
            args: PvsProofCommand
		}) => {
            if (request) {
                await this.proofExplorer.queryQuitProofAndSave();
                this.vscodePvsTerminal.deactivate();
            } else {
                console.error(`[events-dispatcher] Error: null request in quitProofEvent`);
            }
        });

		this.client.onRequest(serverEvent.QED, (request: {
            args: PvsProofCommand
		}) => {
            this.vscodePvsTerminal.deactivate();
        });

        this.client.onRequest(serverEvent.showProofLiteResponse, (desc: { 
            response: string, 
            args: PvsFormula
        }) => {
            if (desc && desc.response) {
                vscodeUtils.previewTextDocument(`${desc.args.theoryName}.prlite`, desc.response, { contextFolder: desc.args.contextFolder, viewColumn: vscode.ViewColumn.Beside });
            }
        });

        this.client.onRequest(serverEvent.generateSummaryResponse, (desc: { 
            response: PvsFile,
            args: { 
                contextFolder: string,
                fileName: string, 
                fileExtension: string, 
                theoryName: string,
                content?: string
            }
        }) => {
            if (desc && desc.response) {
                vscodeUtils.showTextDocument(desc.response);
            }
        });
        
        this.client.onNotification(serverEvent.profilerData, (data: string) => {
            this.logger.profilerData(data);
        });

        this.client.onNotification(serverEvent.proverData, (data: string) => {
            this.logger.proverData(data);
        });
        
        this.client.onNotification("pvs.progress-info", (data: string) => {
            this.statusBar.showProgress(data);
        });


        //---------------------------------------------------------
        // commands invoked using code lens, emacs bindings, explorer, etc
        //---------------------------------------------------------

        context.subscriptions.push(commands.registerCommand("vscode-pvs.view-prelude-file", () => {
            this.client.onRequest(serverEvent.viewPreludeFileResponse, (desc: { contextFolder: string, fileName: string, fileExtension: string }) => {
                vscodeUtils.showTextDocument(desc);
            });
            this.client.sendRequest(serverRequest.viewPreludeFile);
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.install-pvs", () => {
            this.packageManager.pvsInstallationWizard();
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.set-pvs-path", () => {
            this.packageManager.pvsPathWizard();
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.install-nasalib", () => {
            this.packageManager.nasalibInstallationWizard();
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.update-nasalib", () => {
            this.packageManager.updateNasalibWithProgress();
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.add-pvs-library", async () => {
            await vscodeUtils.addPvsLibraryFolderWizard();
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.clear-pvs-library-path", async () => {
            await vscodeUtils.clearPvsLibraryPath();
            vscode.window.showInformationMessage(`PVS library path cleared!`);
        }));
        // vscode-pvs.send-proof-command
        context.subscriptions.push(commands.registerCommand("vscode-pvs.send-proof-command", (desc: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string, cmd: string }) => {
            if (!this.vscodePvsTerminal.sendProofCommand(desc)) {
                this.client.sendRequest(serverRequest.proofCommand, desc);
            }
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.select-profile", (desc: { profile: commandUtils.ProofMateProfile }) => {
            this.vscodePvsTerminal.selectProfile(desc);
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.new-pvs-file", async () => {
            this.workspaceExplorer.newPvsFile(); // async method
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.open-pvs-file", async () => {
            this.workspaceExplorer.openPvsFile(); // async method
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.open-pvs-file-or-folder", async () => {
            await this.workspaceExplorer.openPvsFileOrFolder();
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.open-pvs-workspace", async () => {
            this.workspaceExplorer.openWorkspace(); // async method
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.clean-pvs-workspace", async () => {
            this.workspaceExplorer.cleanPvsWorkspace(); // async method
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.show-version-info", async (opt?: { trailingNote?: string }) => {
            opt = opt || {};
            opt.trailingNote = opt.trailingNote || "";
            let info: PvsVersionDescriptor = {
                "pvs-version": "PVS version not available :/",
                "lisp-version": "",
                "nasalib-version": ""
            };
            const desc: PvsVersionDescriptor = this.statusBar.getVersionInfo();
            if (desc) {
                if (desc["pvs-version"]) { info["pvs-version"] = desc["pvs-version"]; }
                if (desc["lisp-version"]) { info["lisp-version"] = desc["lisp-version"]; }
                if (desc["nasalib-version"]) { info["nasalib-version"] = desc["nasalib-version"]; }
            }
            let msg: string = `PVS ${info["pvs-version"]}`;
            let extras: string[] = [];
            if (info["lisp-version"]) {
                extras.push(info["lisp-version"]);
            }
            if (info["nasalib-version"]) {
                extras.push(info["nasalib-version"]);
            }
            if (extras && extras.length) {
                msg += ` (${extras.join(" + ")})`;
            }
            if (opt.trailingNote) {
                msg += opt.trailingNote;
            }
            window.showInformationMessage(msg);
            this.statusBar.hideInterruptButton();
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.reboot-pvs", async () => {
            // ask the user confirmation before restarting pvs
			const yesno: string[] = [ "Yes", "No" ];
			const msg: string = `Reboot pvs-server?\n\nThis action can resolve situations where the server crashed or is not responding.`;
			const ans: string = await vscode.window.showInformationMessage(msg, { modal: true }, yesno[0])
			if (ans === yesno[0]) {
                this.statusBar.showProgress("Rebooting pvs-server...");
                const currentContext: string = vscode.workspace.rootPath;
                this.client.sendRequest(serverRequest.rebootPvsServer, { cleanFolder: currentContext });
                // terminate any prover session
                await this.vscodePvsTerminal.quitAll();
            }
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.interrupt-prover", async () => {
            // ask the user confirmation before restarting pvs
			const yesno: string[] = [ "Yes", "No" ];
			const msg: string = `Interrupt the execution of the current proof command?`;
			const ans: string = await vscode.window.showInformationMessage(msg, { modal: true }, yesno[0])
			if (ans === yesno[0]) {
                const action: ProofExecInterruptProver = { action: "interrupt-prover" };
                this.client.sendRequest(serverRequest.proverCommand, action);
            }
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.interrupt-and-quit-prover", async () => {
            // ask the user confirmation before restarting pvs
            const action: ProofExecInterruptAndQuitProver = { action: "interrupt-and-quit-prover" };
            this.client.sendRequest(serverRequest.proverCommand, action);
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.download-nasalib", async () => {
            const success: boolean = await this.packageManager.nasalibInstallationWizard();
            if (success) {
                this.statusBar.hideDownloadNasalibButton();
                this.statusBar.showProgress("Rebooting pvs-server...");
                this.client.sendRequest(serverRequest.rebootPvsServer);
            }
        }));

        // context.subscriptions.push(commands.registerCommand("proof-explorer.step-command", (desc: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string, cmd: string }) => {
        //     if (desc && desc.cmd) {
        //         this.vscodePvsTerminal.sendProofCommand(desc, { addNewLine: true });
        //         // window.showInformationMessage(`${desc.cmd} sent to terminal`)
        //     }
        // }));
        context.subscriptions.push(commands.registerCommand("proof-mate.proof-command-dblclicked", (desc: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string, cmd: string }) => {
            if (desc && desc.cmd) {
                this.vscodePvsTerminal.sendProofCommand(desc, { addNewLine: false });
                // window.showInformationMessage(`${desc.cmd} sent to terminal`)
            }
        }));
        context.subscriptions.push(commands.registerCommand("proof-explorer.proof-command-dblclicked", (desc: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string, cmd: string }) => {
            if (desc && desc.cmd) {
                this.vscodePvsTerminal.sendProofCommand(desc, { addNewLine: false });
                // window.showInformationMessage(`${desc.cmd} sent to terminal`)
            }
        }));

        context.subscriptions.push(commands.registerCommand("proof-explorer.trim", (desc: { items: ProofItem[] }) => {
            if (desc) {
                this.proofMate.clearSketchPath();
                this.proofMate.updateSketchpad(desc);
            }
        }));

        context.subscriptions.push(commands.registerCommand("vscode-pvs.print-warning-message-in-terminal", (desc: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string, cmd: string }) => {
            // TODO
            // this.vscodePvsTerminal.printWarningMessage(desc);
        }));

        // vscode-pvs.metax
        context.subscriptions.push(commands.registerCommand("vscode-pvs.metax", () => {
            this.emacsBindings.metaxPrompt();
        }));

        // vscode-pvs.show-proflite
        context.subscriptions.push(commands.registerCommand("vscode-pvs.show-prooflite", async (resource: string | { path: string } | { contextValue: string }) => {
            if (window.activeTextEditor && window.activeTextEditor.document) {
                // if the file is currently open in the editor, save file first
                await window.activeTextEditor.document.save();
                if (!resource) {
                    resource = { path: window.activeTextEditor.document.fileName };
                }
            }
            if (resource) {
                let desc: PvsFormula = this.resource2desc(resource);
                if (desc) {
                    if (!desc.theoryName) {
                        // const document: vscode.TextDocument = window.activeTextEditor.document;
                        const info: { content: string, line: number } = (resource && resource["path"]) ? { content: await fsUtils.readFile(resource["path"]), line: 0 }
                                : { content: window.activeTextEditor.document.getText(), line: window.activeTextEditor.selection.active.line };
                        const theoryName: string = utils.findTheoryName(info.content, info.line);
                        desc.theoryName = theoryName;
                    }
                    if (desc.theoryName) {
                        this.client.sendRequest(serverRequest.showProofLite, desc);
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
                let desc: PvsFormula = this.resource2desc(resource);
                if (desc) {
                    if (!desc.theoryName) {
                        // const document: vscode.TextDocument = window.activeTextEditor.document;
                        const info: { content: string, line: number } = (resource && resource["path"]) ? { content: await fsUtils.readFile(resource["path"]), line: 0 }
                            : { content: window.activeTextEditor.document.getText(), line: window.activeTextEditor.selection.active.line };
                        const theoryName: string = utils.findTheoryName(info.content, info.line);
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
            if (window.activeTextEditor && window.activeTextEditor.document) {
                // if the file is currently open in the editor, save file first
                await window.activeTextEditor.document.save();
                if (!resource) {
                    resource = { path: window.activeTextEditor.document.fileName };
                }
            }
            if (resource) {
                const desc: PvsFormula = this.resource2desc(resource);
                if (desc) {
                    desc.contextFolder = (desc.fileExtension === ".tccs") ? path.join(desc.contextFolder, "..") : desc.contextFolder;
                    if (desc.theoryName) {
                        this.proofExplorer.willStartNewProof();
                        this.proofExplorer.enableView();
                        this.proofMate.enableView();
                        // the sequence of events triggered by this command is:
                        // 1. vscodePvsTerminal.startProverSession(desc) 
                        // 2. vscodePvsTerminal.sendRequest(serverCommand.proveFormula, desc)
                        // 3. pvsLanguageServer.proveFormulaRequest(desc)
                        //      3.1 typecheck
                        //      3.2 loadProofDescriptor
                        //      3.3 proveFormula
                        await this.vscodePvsTerminal.startProverSession(desc);
                    } else {
                        console.error("[vscode-events-dispatcher] Error: theory name is null", desc);
                    }
                } else {
                    console.error("[vscode-events-dispatcher] Error: unknown vscode-pvs.prove-formula resource", resource);
                }
            } else {
                console.error("[vscode-events-dispatcher] Error: vscode-pvs.prove-formula invoked with null resource", resource);
            }
        }));

        context.subscriptions.push(commands.registerCommand("vscode-pvs.show-proof-summary", async (resource: TheoryItem | { path: string }) => {
            const desc: PvsTheory = await vscodeUtils.getPvsTheory(resource);
            if (desc && desc.theoryName) {
                await this.workspaceExplorer.showProofSummary(desc);
            }
        }));
        // vscode-pvs.prove-theory
        // the sequence of events triggered by this command is:
        // 1. vscodePvsTerminal.startProverSession(desc) 
        // 2. vscodePvsTerminal.sendRequest(serverCommand.proveFormula, desc)
        // 3. pvsLanguageServer.proveFormulaRequest(desc)
        //      3.1 typecheck
        //      3.2 loadProofDescriptor
        //      3.3 proveFormula
        // <loop over all theorems>
        context.subscriptions.push(commands.registerCommand("vscode-pvs.prove-theory-inline", async (resource: TheoryItem | { path: string }) => {
            commands.executeCommand("vscode-pvs.prove-theory", resource);
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.prove-theory", async (resource: TheoryItem | { path: string }) => {
            const desc: PvsTheory = await vscodeUtils.getPvsTheory(resource);
            if (desc && desc.theoryName) {
                this.quietMode = true;
                this.proofMate.disableView();
                this.proofExplorer.disableView();
                this.statusBar.showProgress(`Re-running proofs in theory ${desc.theoryName}`);

                await this.workspaceExplorer.proveTheoryWithProgress(desc);

                this.statusBar.ready();
                this.quietMode = false;
            } else {
                console.error("[vscode-events-dispatcher] Error: vscode-pvs.prove-theory invoked with null resource", resource);
            }
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.prove-importchain", async (resource: TheoryItem | { path: string }) => {
            const desc: PvsTheory = await vscodeUtils.getPvsTheory(resource);
            if (desc && desc.theoryName) {
                this.quietMode = true;
                this.proofMate.disableView();
                this.proofExplorer.disableView();

                this.statusBar.showProgress(`Re-running importchain for theory ${desc.theoryName}`);
                await this.workspaceExplorer.proveImportChainWithProgress(desc);

                this.statusBar.ready();
                this.quietMode = false;
            } else {
                console.error("[vscode-events-dispatcher] Error: vscode-pvs.prove-importchain invoked with null resource", resource);
            }
        }));
        // this request comes from the context menu displayed by the editor
        context.subscriptions.push(commands.registerCommand("vscode-pvs.discharge-tccs", async (resource: TheoryItem | { path: string }) => {
            if (window.activeTextEditor && window.activeTextEditor.document) {
                // if the file is currently open in the editor, save file first
                await window.activeTextEditor.document.save();
                if (!resource) {
                    resource = { path: window.activeTextEditor.document.fileName };
                }
            }

            const desc: PvsTheory = await vscodeUtils.getPvsTheory(resource);
            if (desc && desc.theoryName) {
                this.quietMode = true;
                this.proofMate.disableView();
                this.proofExplorer.disableView();

                await this.workspaceExplorer.proveTheoryWithProgress(desc, { tccsOnly: true });

                this.statusBar.ready();
                this.quietMode = false;
            } else {
                console.error("[vscode-events-dispatcher] Error: vscode-pvs.discharge-tccs invoked with null resource", resource);
            }
        }));
        // this request comes from workspace explorer
        context.subscriptions.push(commands.registerCommand("vscode-pvs.discharge-tccs-explorer", async (resource: TccsOverviewItem) => {
            commands.executeCommand("vscode-pvs.discharge-tccs", resource);
        }));

        // vscode-pvs.discharge-tccs
        // context.subscriptions.push(commands.registerCommand("vscode-pvs.discharge-tccs", async (resource) => {
        //     if (resource) {
        //         let desc = <{ 
        //             fileName: string, fileExtension: string, contextFolder: string, 
        //             theoryName: string, formulaName: string 
        //         }> this.resource2desc(resource);
        //         if (desc) {
        //             // send discharge-tccs request to pvs-server
        //             this.client.sendRequest(serverCommand.dischargeTccs, desc);
        //         } else {
        //             console.error("[vscode-events-dispatcher] Error: unknown vscode-pvs.prove-formula resource", resource);
        //         }
        //     } else {
        //         console.error("[vscode-events-dispatcher] Error: prove-formula invoked with null resource", resource);
        //     }
        // }));
        // // alias for vscode-pvs.discharge-tccs
		// context.subscriptions.push(commands.registerCommand("vscode-pvs.discharge-tccs-explorer", async (resource: string | { path: string } | { contextValue: string }) => {
        //     commands.executeCommand("vscode-pvs.discharge-tccs", resource);
        // }));


        // vscode-pvs.discharge-theorems
        // context.subscriptions.push(commands.registerCommand("vscode-pvs.discharge-theorems", async (resource) => {
        //     if (resource) {
        //         let desc = <{ 
        //             fileName: string, fileExtension: string, contextFolder: string, 
        //             theoryName: string, formulaName: string 
        //         }> this.resource2desc(resource);
        //         if (desc) {
        //             // send discharge-theorems request to pvs-server
        //             this.client.sendRequest(serverCommand.dischargeTheorems, desc);
        //         } else {
        //             console.error("[vscode-events-dispatcher] Error: unknown vscode-pvs.prove-formula resource", resource);
        //         }
        //     } else {
        //         console.error("[vscode-events-dispatcher] Error: prove-formula invoked with null resource", resource);
        //     }
        // }));
        // // alias for vscode-pvs.discharge-tccs
        // context.subscriptions.push(commands.registerCommand("vscode-pvs.discharge-theorems-alt", async (resource: string | { path: string } | { contextValue: string }) => {
        //     commands.executeCommand("vscode-pvs.discharge-theorems", resource);
        // }));
        
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
                const desc: PvsFile = this.resource2desc(resource);
                if (desc) {
                    // show output panel for feedback
                    // commands.executeCommand("workbench.action.output.toggleOutput", true);
                    // send typecheck request to pvs-server
                    this.client.sendRequest(serverRequest.typecheckFile, desc);
                }
            } else {
                console.error("[vscode-events-dispatcher] Warning: resource is null", resource);
            }
        }));
        // alias for vscode-pvs.typecheck-file
        context.subscriptions.push(commands.registerCommand("vscode-pvs.typecheck-file-inline", async (resource: string | { path: string } | { contextValue: string }) => {
            commands.executeCommand("vscode-pvs.typecheck-file", resource);
        }));
        
        
        // vscode-pvs.show-tccs
		context.subscriptions.push(commands.registerCommand("vscode-pvs.show-tccs", async (resource: string | { path: string } | { contextValue: string }) => {
            if (!resource && window.activeTextEditor && window.activeTextEditor.document) {
                resource = { path: window.activeTextEditor.document.fileName };
            }
			if (resource) {
                const desc: PvsFile = this.resource2desc(resource);
                if (desc) {
                    // send show-tccs request to pvs-server
                    this.client.sendRequest(serverRequest.showTccs, desc);
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
                const desc: PvsFile = this.resource2desc(resource);
                if (desc) {
                    // send generate-tccs request to pvs-server
                    this.client.sendRequest(serverRequest.generateTccs, desc);
                    // register handler for response
                    this.client.onRequest(serverEvent.generateTccsResponse, (desc: {
                        response: PvsContextDescriptor, 
                        args: { 
                            fileName: string, 
                            fileExtension: string, 
                            contextFolder: string 
                        }
                    }) => {
                        if (this.workspaceExplorer && desc.response) {
                            this.workspaceExplorer.updateContextFolder(desc.response, { tccDescriptor: true });
                        }
                    });            
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
                const desc: PvsFile = this.resource2desc(resource);
                if (desc) {
                    // send parse request to pvs-server
                    this.client.sendRequest(serverRequest.parseFileWithFeedback, desc);
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
                    this.client.sendRequest(serverRequest.parseWorkspaceWithFeedback, desc);
                }
            } else {
                console.error("[vscode-events-dispatcher] Warning: resource is null", resource);
            }
		}));

        // vscode-pvs.typecheck-workspace
        context.subscriptions.push(commands.registerCommand("vscode-pvs.typecheck-workspace-inline", async (resource: string | { path: string } | { contextValue: string }) => {
            commands.executeCommand("vscode-pvs.typecheck-workspace", resource);
        }));
		context.subscriptions.push(commands.registerCommand("vscode-pvs.typecheck-workspace", async (resource: string | { path: string } | { contextValue: string }) => {
            if (!resource && window.activeTextEditor && window.activeTextEditor.document) {
                resource = { path: window.activeTextEditor.document.fileName };
            }
			if (resource) {
                let desc = this.resource2desc(resource);
                if (desc) {
                    // send parse request to pvs-server
                    this.client.sendRequest(serverRequest.typecheckWorkspace, desc);
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
                    this.client.sendRequest(serverRequest.hp2pvs, desc);
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
                if (this.quietMode) {
                    this.client.onNotification(`server.status.end-important-task-${desc.id}-with-errors`, (desc: { msg: string }) => {
                        this.statusBar.ready();
                        if (desc && desc.msg) {
                            this.statusBar.showError(desc.msg); // use the status bar rather than dialogs, because we don't have APIs to close old dialogs with potentially stale information
                            window.showErrorMessage(desc.msg);
                            // show problems panel -- see also Code->Preferences->KeyboardShortcuts
                            commands.executeCommand("workbench.panel.markers.view.focus");
                        }
                    });
                    return;
                }
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
                            this.client.sendRequest(serverRequest.cancelOperation);
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
                                // show problems panel -- see also Code->Preferences->KeyboardShortcuts
                                commands.executeCommand("workbench.panel.markers.view.focus");
                            }
                        });
                    });
                });

                // show progress on the status bar
                this.statusBar.showProgress(desc.msg);
            }

        });

        this.client.onNotification("server.status.pvs-failure", (opt?: { msg?: string, fname?: string, method?: string }) => {
            opt = opt || {};
            let msg: vscode.MarkedString = opt.msg || `pvs-server crashed into Lisp.\nTo continue, you may need to reboot pvs-server.`;
            if (opt.fname) {
                // msg += `\nThe error occurred while processing file [${opt.fname}](file://${opt.fname})`; // vscode is unable to render marked strings in dialogs
                msg += `\nThe error occurred while processing file ${opt.fname}`;
            } else if (opt.method) {
                // msg += `\nThe error occurred while executing method [${opt.method}](${opt.method})`; // vscode is unable to render marked strings in dialogs
                msg += `\nThe error occurred while executing method ${opt.method}`;
            }
            window.showErrorMessage("Error: " + msg);
            this.statusBar.showError(msg);
        });
    }
}