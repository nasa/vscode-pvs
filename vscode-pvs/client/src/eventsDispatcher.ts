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
import { VSCodePvsWorkspaceExplorer, TheoryItem, TccsOverviewItem, WorkspaceItem } from "./views/vscodePvsWorkspaceExplorer";
import { VSCodePvsProofExplorer, ProofItem, ProofExplorerEvent } from "./views/vscodePvsProofExplorer";
// import { TerminalSession, VSCodePvsTerminal } from "./views/vscodePvsTerminal";
import { 
    PvsContextDescriptor, serverEvent, serverRequest, PvsVersionDescriptor, 
    ProofDescriptor, ServerMode, PvsFormula, ProofEditEvent, PvsProofCommand, 
    ProofExecEvent, PvsTheory, ProofExecInterruptProver, WorkspaceEvent, 
    ProofExecInterruptAndQuitProver, FileDescriptor, ContextFolder, 
    PvsioEvaluatorCommand, EvalExpressionRequest, ProveFormulaResponse, ProofCommandResponse, ProofMateProfile, ProveFormulaRequest, PvsFile
} from "./common/serverInterface";
import { window, commands, ExtensionContext, ProgressLocation, Selection } from "vscode";
import * as vscode from 'vscode';
import { PvsResponse } from "./common/pvs-gui";
import * as fsUtils from './common/fsUtils';
import { VSCodePvsProofMate } from "./views/vscodePvsProofMate";
import * as utils from './common/languageUtils';
import * as vscodeUtils from './utils/vscode-utils';
import { VSCodePvsLogger } from "./views/vscodePvsLogger";
import { VSCodePvsPackageManager } from "./providers/vscodePvsPackageManager";
import { VSCodePvsPlotter } from "./views/vscodePvsPlotter";
import { VSCodePvsSearch } from "./views/vscodePvsSearch";
import { VSCodePvsioWeb } from "./views/vscodePvsioWeb";
import { VSCodePvsXTerm } from "./views/vscodePvsXTerm";

// FIXME: use Backbone.Model
export class EventsDispatcher {
    protected client: LanguageClient;
    protected statusBar: VSCodePvsStatusBar;
    protected emacsBindings: VSCodePvsEmacsBindingsProvider;
    protected workspaceExplorer: VSCodePvsWorkspaceExplorer;
    protected proofExplorer: VSCodePvsProofExplorer;

    // protected vscodePvsTerminal: VSCodePvsTerminal;
    protected xterm: VSCodePvsXTerm;

    protected proofMate: VSCodePvsProofMate;
    protected logger: VSCodePvsLogger;
    protected packageManager: VSCodePvsPackageManager;
    protected plotter: VSCodePvsPlotter;
    protected search: VSCodePvsSearch;
    protected pvsioweb: VSCodePvsioWeb;

    protected inChecker: boolean = false;
    protected inEvaluator: boolean = false;
    protected quietMode: boolean = false;

    protected NOTIFICATION_TIMEOUT: number = 2000; // 2sec

    constructor (client: LanguageClient, handlers: {
        statusBar: VSCodePvsStatusBar,
        emacsBindings: VSCodePvsEmacsBindingsProvider,
        workspaceExplorer: VSCodePvsWorkspaceExplorer,
        proofExplorer: VSCodePvsProofExplorer,

        // vscodePvsTerminal: VSCodePvsTerminal,
        xterm: VSCodePvsXTerm,

        proofMate: VSCodePvsProofMate,
        logger: VSCodePvsLogger,
        packageManager: VSCodePvsPackageManager,
        plotter: VSCodePvsPlotter,
        search: VSCodePvsSearch,
        pvsioweb: VSCodePvsioWeb
    }) {
        this.client = client;
        this.statusBar = handlers.statusBar;
        this.emacsBindings = handlers.emacsBindings;
        this.workspaceExplorer = handlers.workspaceExplorer;
        this.proofExplorer = handlers.proofExplorer;

        this.proofExplorer.on(ProofExplorerEvent.didAcquireFocus, () => {
            setTimeout(() => {
                this.xterm.focus(); // use a timeout so that proof-explorer does not steal the focus
            }, 250);
        });

        // this.vscodePvsTerminal = handlers.vscodePvsTerminal;
        this.xterm = handlers.xterm;

        this.proofMate = handlers.proofMate;
        this.logger = handlers.logger;
        this.packageManager = handlers.packageManager;
        this.plotter = handlers.plotter;
        this.search = handlers.search;
        this.pvsioweb = handlers.pvsioweb;
    }
	activate (context: ExtensionContext): void {
        // -- handlers for server responses
        this.client.onRequest(serverEvent.evaluatorCommandResponse, (data: {
            req: PvsioEvaluatorCommand,
            res: string,
            state: string
        }) => {
            // do nothing -- this is just a placeholder, the actual handler is in vscoePvsioWeb.ts
        });
		this.client.onRequest(serverEvent.pvsVersionInfo, (version: PvsVersionDescriptor) => {
			if (version) {
                this.statusBar.pvsReady(version);
                vscode.commands.executeCommand('setContext', 'nasalib-present', !!version["nasalib-version"]);
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
                                this.statusBar.showVersionDialog({ trailingNote: " :: Ready! ::"});
                            });
                        }
                    }
                } else {
                    this.statusBar.showVersionDialog({ trailingNote: " :: Ready! ::"});
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
                    const uri: vscode.Uri = vscode.Uri.file(fsUtils.desc2fname({ fileName: desc.args.fileName, contextFolder: desc.args.contextFolder, fileExtension: ".tccs"}));
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
        this.client.onRequest(serverEvent.proofCommandResponse, async (desc: ProofCommandResponse) => {
            // notify proofexplorer
            // await this.proofExplorer.onStepExecuted(desc);
            if (desc?.res && typeof desc.res !== "string") {
                // update proof mate
                this.proofMate.updateRecommendations(desc.res);
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
                    this.proofExplorer.enableView();
                    this.proofMate.enableView();
                    this.proofExplorer.resetView();
                    break;
                }
                case "did-fail-to-start-proof": {
                    this.proofExplorer.disableView();
                    this.proofMate.disableView();
                    this.proofExplorer.resetView();
                    this.xterm.dispose();
                    break;
                }
                //---------------
                case "did-start-proof": { // this event is for interactive proof sessions
                    this.proofExplorer.startProof();
                    this.proofMate.startProof();
                    await this.proofMate.loadSketchpadClips(); // loads sketchpad clips from the .jprf file
                    this.proofExplorer.focusActiveNode({ force: true }); // this will automatically open the view, in the case the view was hidden
                    // setTimeout(() => {
                    //     this.xterm.focus(); // place focus on the terminal. this is done after a timeout to ensure proof-explorer does not steal the focus
                    // }, 500);
                    break;
                }
                case "did-quit-proof": { // this is sent by CliGateway when the prover CLI is closed
                    this.proofExplorer.disposeView();
                    await this.proofMate.saveSketchpadClips();  // saves sketchpad clips to the .jprf file
                    this.proofMate.disposeView();
                    // this.statusBar.hideInterruptButton();
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
                case "did-stop-running": {
                    this.proofExplorer?.didStopRunning();
                    break;
                }
                case "did-open-proof": {
                    if (desc && desc.formula) {
                        if (desc.proof) {
                            this.proofExplorer.loadProofStructure(desc.formula, desc.desc, desc.proof);
                            this.proofMate.loadFormula(desc.formula);
                            const fname: string = fsUtils.desc2fname(desc.formula);
                            // window.showInformationMessage(`Proof ${desc.formula.formulaName} successfully loaded from ${fname}`);
                        } else {
                            window.showWarningMessage(`Formula ${desc.formula.formulaName} does not have a proof.`);
                        }
                    } else {
                        window.showWarningMessage(`Failed to open proof (null descriptor)`);
                    }
                    break;
                }
                case "did-import-proof": {
                    if (desc && desc.formula) {
                        if (desc.proof) {
                            this.proofExplorer.loadProofStructure(desc.formula, desc.desc, desc.proof);
                            this.proofMate.loadFormula(desc.formula);
                            vscodeUtils.showInformationMessage(`Proof ${desc.importedFormula.formulaName} successfully imported!`);
                        } else {
                            vscodeUtils.showInformationMessage(`Formula ${desc.formula.formulaName} does not have a proof.`);
                        }
                    } else {
                        window.showWarningMessage(`Failed to import proof (null descriptor)`);
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
                proofFile: FileDescriptor,
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
                    const msg: string = `Proof ${desc.response.formula.formulaName} saved`;// in file ${fname}`;
                    this.statusBar.showMsg(msg);
                    setTimeout(() => {
                        this.statusBar.ready();
                    }, 1000);
                    // window.showInformationMessage(msg);
                }
            } else {
                if (desc && desc.args) {
                    if (desc.response && desc.response.msg) {
                        vscodeUtils.showErrorMessage(`Error while saving file ${fsUtils.desc2fname(desc.args)} (${desc.response.msg})`);
                    } else {
                        vscodeUtils.showErrorMessage(`Unexpected error while saving file ${fsUtils.desc2fname(desc.args)} (please check pvs-server output for details)`);
                    }
                    if (desc.response.script) {
                        const fileContent: string = `# Proof for ${desc.args.formulaName}\n`
                            + 'An error occurred while trying to save your proof in PVS.\n'
                            + 'You might be using an obsolete version of PVS. Please try to re-install PVS with the command `M-x reinstall-pvs`.\n'
                            + 'The command will open a dialog, select `Download PVS` to install the latest version of PVS.\n'
                            + `If the problem persists, please report an issue on [github](https://github.com/nasa/vscode-pvs/issues) or on the [PVS group](https://groups.google.com/g/pvs-group), we will look into it.\n\n`
                            + `## Your proof attempt\n`
                            + `Don't panic, your proof attempt for ${desc.args.formulaName} is not lost.<br>\n`
                            + `VSCode-PVS saved your proof attempt, and you can restore it in Proof Explorer (**...** -> **Restore Proof**).<br>\n\n`
                            + `If everything else fails, below you can find the complete proof script, which you can paste in the prover terminal to repeat the proof:\n`
                            + '```lisp\n'
                            + desc.response.script
                            + '\n```';
                        const fdesc: FileDescriptor = {
                            fileName: desc.args.formulaName,
                            fileExtension: ".md",
                            contextFolder: vscodeUtils.getPreviewFolder(),
                            fileContent
                        };
                        await vscodeUtils.showMarkdownPreview(fdesc);
                    }
                }
            }
        });


        this.client.onRequest(serverEvent.proveFormulaResponse, (desc: ProveFormulaResponse) => {
            console.log(desc);
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

        this.client.onRequest(serverEvent.pvsServerCrash, (desc: { msg?: string }) => {
            desc = desc || {};
            if (desc.msg) {
                this.statusBar.failure(desc.msg);
            }
        });

        this.client.onRequest(serverEvent.serverModeUpdateEvent, (desc: { mode: ServerMode }) => {
            if (desc) {
                switch (desc.mode) {
                    case "in-checker": {
                        this.inChecker = true;
                        this.inEvaluator = false;
                        vscode.commands.executeCommand('setContext', 'in-checker', true);
                        vscode.commands.executeCommand('setContext', 'in-evaluator', false);
                        break;
                    }
                    case "pvsio": {
                        this.inChecker = false;
                        this.inEvaluator = true;
                        vscode.commands.executeCommand('setContext', 'in-checker', false);
                        vscode.commands.executeCommand('setContext', 'in-evaluator', true);
                        break;
                    }
                    case "lisp":
                    default: {
                        this.inChecker = false;
                        this.inEvaluator = false;
                        vscode.commands.executeCommand('setContext', 'in-checker', false);
                        vscode.commands.executeCommand('setContext', 'in-evaluator', false);
                        break;
                    }
                }
                this.proofExplorer.didUpdateServerMode(desc.mode);
                this.workspaceExplorer.refreshView();
            }
        });

		this.client.onRequest(serverEvent.getContextDescriptorResponse, (desc: PvsContextDescriptor) => {
            this.workspaceExplorer.updateContextFolder(desc);
            if (desc?.fileDescriptors && Object.keys(desc.fileDescriptors).length) {
                // use pvs file icons
                vscodeUtils.loadPvsFileIcons();
            }
        });

		this.client.onRequest(serverEvent.querySaveProof, async (request: {
            args: PvsProofCommand
		}) => {
            if (request) {
                await this.proofExplorer.queryQuitProofAndSave();
            } else {
                console.error(`[events-dispatcher] Error: null request in quitProofEvent`);
            }
        });

		this.client.onRequest(serverEvent.QED, (request: {
            args: PvsProofCommand
		}) => {
        });

        this.client.onRequest(serverEvent.showTheorySummaryResponse, (desc: { 
            response: FileDescriptor,
            args: PvsTheory
        }) => {
            if (desc && desc.response) {
                vscodeUtils.showTextDocument(desc.response);
            }
        });
        
        this.client.onRequest(serverEvent.showWorkspaceSummaryResponse, (desc: { 
            response: FileDescriptor,
            args: PvsTheory
        }) => {
            if (desc && desc.response) {
                vscodeUtils.showTextDocument(desc.response);
            }
        });

        this.client.onNotification(serverEvent.profilerData, (data: string) => {
            this.logger.profilerData(data);
        });
        
        this.client.onNotification("pvs.progress-info", (data: string) => {
            this.statusBar.showProgress(data);
        });


        //---------------------------------------------------------
        // commands invoked using code lens, emacs bindings, explorer, etc
        //---------------------------------------------------------

        context.subscriptions.push(commands.registerCommand("vscode-pvs.comment-prooflite-line", () => {
            vscodeUtils.commentProofliteInActiveEditor();
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.search-nasalib", () => {
            this.search.reveal();
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.view-prelude-file", () => {
            this.client.sendRequest(serverRequest.viewPreludeFile);
            this.client.onRequest(serverEvent.viewPreludeFileResponse, (desc: PvsFile) => {
                vscodeUtils.showTextDocument(desc);
            });
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.install-pvs", async () => {
            await this.packageManager.pvsInstallationWizard();
            // create default workspaces folder if it doesn't exist
            await vscodeUtils.createDefaultPvsWorkspacesDirectory();
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
        context.subscriptions.push(commands.registerCommand("vscode-pvs.reset-pvs-library-path", async () => {
            await vscodeUtils.clearPvsLibraryPath();
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.view-pvs-library-path", async () => {
            commands.executeCommand('workbench.action.openSettings', '@ext:paolomasci.vscode-pvs pvs-library-path');
        }));
        // vscode-pvs.send-proof-command
        context.subscriptions.push(commands.registerCommand("vscode-pvs.send-proof-command", (desc: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string, cmd: string }) => {
            this.client.sendRequest(serverRequest.proofCommand, desc);
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.select-profile", (desc: { profile: ProofMateProfile }) => {
            // this.vscodePvsTerminal.selectProfile(desc);
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.new-pvs-file", async (resource: { path: string }) => {
            console.log(resource);
            const contextFolder: string = (resource && resource.path) ? resource.path : vscodeUtils.getRootPath();
            this.workspaceExplorer.newPvsFile(contextFolder); // async method
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
        context.subscriptions.push(commands.registerCommand("vscode-pvs.show-version-info", () => {
            this.statusBar.showVersionInfo();
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.show-version-dialog", () => {
            this.statusBar.showVersionDialog({ downloadButtons: true });
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.reboot-pvs", async () => {
            // ask the user confirmation before restarting pvs
			const yesno: string[] = [ "Yes", "No" ];
			const msg: string = `Reboot pvs?\n\nThis action can resolve situations where the server crashed or is not responding.`;
			const ans: string = await vscode.window.showInformationMessage(msg, { modal: true }, yesno[0])
			if (ans === yesno[0]) {
                const currentContext: string = vscodeUtils.getRootPath();
                this.client.sendRequest(serverRequest.rebootPvsServer, { cleanFolder: currentContext });
                // terminate any prover session
                this.xterm.dispose();
            }
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.interrupt-prover", async () => {
            // ask the user confirmation before restarting pvs
            if (this.proofExplorer) {
                this.proofExplorer.queryPauseProof();
            } else {
                const yesno: string[] = [ "Yes", "No" ];
                const msg: string = `Interrupt the execution of the current proof command?`;
                const ans: string = await vscode.window.showInformationMessage(msg, { modal: true }, yesno[0])
                if (ans === yesno[0]) {
                    const action: ProofExecInterruptProver = { action: "interrupt-prover" };
                    this.client.sendRequest(serverRequest.proverCommand, action);
                }
            }
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.interrupt-and-quit-prover", async () => {
            // ask the user confirmation before restarting pvs
            const action: ProofExecInterruptAndQuitProver = { action: "interrupt-and-quit-prover" };
            this.client.sendRequest(serverRequest.proverCommand, action);
            this.statusBar.ready();
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.download-nasalib", async () => {
            const success: boolean = await this.packageManager.nasalibInstallationWizard();
            if (success) {
                this.statusBar.hideDownloadNasalibButton();
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
            if (desc?.cmd) {
                this.xterm.sendText(desc.cmd);
                this.xterm.focus();
                this.xterm.updateHelp();
                // window.showInformationMessage(`${desc.cmd} sent to terminal`)
            }
        }));
        context.subscriptions.push(commands.registerCommand("proof-explorer.proof-command-dblclicked", (desc: { fileName: string, fileExtension: string, contextFolder: string, theoryName: string, formulaName: string, cmd: string }) => {
            if (desc && desc.cmd) {
                this.xterm.sendText(desc.cmd);
                this.xterm.focus();
                this.xterm.updateHelp();
                // window.showInformationMessage(`${desc.cmd} sent to terminal`)
            }
        }));

        context.subscriptions.push(commands.registerCommand("proof-explorer.trim", (desc: { items: ProofItem[] }) => {
            if (desc) {
                // this.proofMate.clearSketchPath();
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
                let desc: PvsFormula = vscodeUtils.resource2desc(resource);
                if (desc) {
                    if (!desc.theoryName) {
                        // const document: vscode.TextDocument = window.activeTextEditor.document;
                        const info: { content: string, line: number } = (resource && resource["path"]) ? { content: await fsUtils.readFile(resource["path"]), line: 0 }
                                : { content: window.activeTextEditor.document.getText(), line: window.activeTextEditor.selection.active.line };
                        const theoryName: string = fsUtils.findTheoryName(info.content, info.line);
                        desc.theoryName = theoryName;
                    }
                    if (desc.theoryName) {
                        // show dialog with progress
                        await window.withProgress({
                            location: ProgressLocation.Notification,
                            cancellable: true
                        }, async (progress, token) => {
                            // show initial dialog with spinning progress
                            const message: string = `Generating ProofLite script for formula ${desc.formulaName}`;
                            progress.report({ increment: -1, message });

                            return new Promise<void>((resolve, reject) => {
                                this.client.sendRequest(serverRequest.showProofLite, desc);
                                this.client.onRequest(serverEvent.showProofLiteResponse, async (desc: { 
                                    response: { proofFile: FileDescriptor }, 
                                    args: PvsFormula
                                }) => {
                                    if (desc && desc.args && desc.response && desc.response.proofFile) {
                                        const line: number = await fsUtils.getProofLitePosition({ formula: desc.args, proofFile: desc.response.proofFile });
                                        vscodeUtils.showTextDocument(desc.response.proofFile, {
                                            viewColumn: vscode.ViewColumn.Beside,
                                            selection: new vscode.Range(
                                                new vscode.Position(line - 1, 0),
                                                new vscode.Position(line, 0)
                                            )
                                        });
                                        this.statusBar.ready();
                                        progress.report({ message: `Done!`, increment: 100 });
                                        resolve();
                                        // vscodeUtils.previewTextDocument(`${desc.args.theoryName}.prlite`, desc.response, { 
                                        //     contextFolder: path.join(desc.args.contextFolder, "pvsbin"), 
                                        //     viewColumn: vscode.ViewColumn.Beside
                                        // });
                                    } else {
                                        progress.report({ message: `${utils.icons.bang} Unable to generate prooflite script for formula ${desc.args.formulaName}`, increment: 100 });
                                        setTimeout(() => {
                                            resolve();
                                        }, 4000)
                                    }
                                });
                            });
                        });
                    } else {
                        vscodeUtils.showErrorMessage(`Error while trying to display prooflite script (could not identify theory name, please check that the file typechecks correctly)`);
                    }
                } else {
                    console.error("[vscode-events-dispatcher] Error: prooflite script requested for unknown resource", resource);
                }
            } else {
                console.error("[vscode-events-dispatcher] Error: prooflite script request is null", resource);
            }
        }));

        context.subscriptions.push(commands.registerCommand("vscode-pvs.pvsio-web", async (resource: string | { path: string } | { contextValue: string }) => {
            // TODO
            const success: boolean = await this.xterm.onStartEvaluatorRequest();
            // let evalSession: boolean = this.xterm.evaluatorSession();
            // if (evalSession) {
            //     // check if the current session corresponds to that indicated in the request
            //     const theory: PvsTheory = this.xterm.getTheory();
            //     const fname: string = fsUtils.desc2fname(theory);
            //     if (fname && fname !== resource && fname !== resource["path"]) {
            //         // await this.pvsioweb.dispose();
            //         await this.xterm.closeSession(theory, "evaluator");
            //         await this.xterm.onStartEvaluatorRequest();
            //     }
            // } else {
            //     await this.xterm.onStartEvaluatorRequest();
            // }
            // evalSession = this.xterm.evaluatorSession();
            if (success) {
                const theory: PvsTheory = this.xterm.getTheory();
                if (theory) {
                    this.pvsioweb.setTheory(theory);
                    this.pvsioweb.setTerminal(this.xterm);
                    this.pvsioweb.reveal();
                }
            } else {
                vscodeUtils.showWarningMessage("Unable to create a new PVSio Evaluator session");
            }
        }));
        // pvsio-evaluator
        context.subscriptions.push(commands.registerCommand("vscode-pvs.pvsio-evaluator", async () => {
            // the toolbar generates invocations with { path }
            // await this.vscodePvsTerminal.startEvaluator(resource);
            await this.xterm.onStartEvaluatorRequest();
        }));

        // pvsio-plot
        context.subscriptions.push(commands.registerCommand("vscode-pvs.plot-expression", async (resource: string | { path: string } | { contextValue: string }) => {
            if (window.activeTextEditor && window.activeTextEditor.document) {
                // if the file is currently open in the editor, save file first
                await window.activeTextEditor.document.save();
                if (!resource) {
                    resource = { path: window.activeTextEditor.document.fileName };
                }
            }
            if (resource) {
                let desc: PvsTheory = vscodeUtils.resource2desc(resource);
                if (desc) {
                    if (!desc.theoryName) {
                        // const document: vscode.TextDocument = window.activeTextEditor.document;
                        const info: { content: string, line: number } = (resource && resource["path"]) ? { content: await fsUtils.readFile(resource["path"]), line: 0 }
                            : { content: window.activeTextEditor.document.getText(), line: window.activeTextEditor.selection.active.line };
                        const theoryName: string = fsUtils.findTheoryName(info.content, info.line);
                        desc.theoryName = theoryName;
                    }
                    if (desc.theoryName) {
                        const selection: Selection = window?.activeTextEditor?.selection;
                        const expr: string = window?.activeTextEditor.document.getText(selection);
                        if (expr) {
                            const request: EvalExpressionRequest = {
                                contextFolder: desc.contextFolder,
                                fileName: desc.fileName,
                                fileExtension: desc.fileExtension,
                                theoryName: desc.theoryName,
                                expr
                            };
                            this.plotter.plot(request);
                        }
                    } else {
                        vscodeUtils.showErrorMessage(`Error while trying to invoke PVSio (could not identify theory name, please check that the file typechecks correctly)`);
                    }
                } else {
                    console.error("[vscode-events-dispatcher] Error: pvsio-evaluator invoked over an unknown resource", resource);
                }
            } else {
                console.error("[vscode-events-dispatcher] Error: pvsio-evaluator invoked with null resource", resource);
            }
        }));

        // vscode-pvs.prove-formula
        context.subscriptions.push(commands.registerCommand("vscode-pvs.jprove-formula", async (desc: {
            contextFolder: string,
            fileName: string,
            fileExtension: string,
            theoryName: string,
            formulaName: string
        }) => {
            if (desc) {
                commands.executeCommand("vscode-pvs.prove-formula", {
                    contextFolder: desc.contextFolder,
                    fileName: desc.fileName,
                    fileExtension: desc.fileExtension,
                    theoryName: (desc.fileExtension === ".tccs" && desc.theoryName.endsWith("_TCCS")) ? desc.theoryName.substr(0, desc.theoryName.length - 5) : desc.theoryName,
                    formulaName: desc.formulaName,
                    proofFile: {
                        contextFolder: desc.contextFolder,
                        fileName: desc.fileName,
                        fileExtension: ".jprf"
                    }
                });
                this.statusBar.showProgress(`Starting J-PRF proof for ${desc.formulaName}`);
            }
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.prove-formula-at-cursor-position", async () => {
            this.proofExplorer.proveFormulaAtCursorPosition();
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.prove-formula", async (req: ProveFormulaRequest) => {
            this.proofExplorer.resetView();
            this.proofExplorer.enableView();
            this.proofMate.enableView();
            const success: boolean = await this.xterm.onStartProverRequest(req);
            if (!success) {
                this.proofExplorer.disposeView();
                this.proofMate.disposeView();
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
        context.subscriptions.push(commands.registerCommand("vscode-pvs.jprove-theory", async (resource: TheoryItem | { path: string }) => {
            commands.executeCommand("vscode-pvs.prove-theory", resource, { useJprf: true });
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.prove-theory", async (resource: TheoryItem | { path: string }, opt?: { useJprf?: boolean, unprovedOnly?: boolean }) => {
            const desc: PvsTheory = await vscodeUtils.getPvsTheory(resource);
            if (desc && desc.theoryName) {
                opt = opt || {};
                // ask the user confirmation before restarting pvs
                const yesno: string[] = [ "Yes", "Unproved Only", "No" ];
                const msg: string = opt.useJprf ? `Re-run J-PRF proofs in theory ${desc.theoryName}?` : `Re-run all proofs in theory ${desc.theoryName}?`;
                const ans: string = await vscode.window.showInformationMessage(msg, { modal: true }, yesno[0], yesno[1]);
                if (ans === yesno[0] || ans === yesno[1]) {
                    opt.unprovedOnly = ans === yesno[1];
                    this.quietMode = true;
                    this.proofMate.disableView();
                    this.proofExplorer.disableView();
                    const msg: string = (opt.unprovedOnly) ? 
                        opt.useJprf ? `Running unproved J-PRF proofs (theory ${desc.theoryName})` : `Running unproved proofs in theory ${desc.theoryName}`
                            : opt.useJprf ? `Re-running J-PRF proofs (theory ${desc.theoryName})` : `Re-running proofs in theory ${desc.theoryName}`;
                    this.statusBar.showProgress(msg);

                    await this.workspaceExplorer.proveTheoryWithProgress(desc, opt);

                    this.statusBar.ready();
                    this.quietMode = false;
                }
            } else {
                console.error("[vscode-events-dispatcher] Error: vscode-pvs.prove-theory invoked with null resource", resource);
            }
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.jprove-importchain", async (resource: TheoryItem | { path: string }) => {
            commands.executeCommand("vscode-pvs.prove-importchain", resource, { useJprf: true });
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.prove-importchain", async (resource: TheoryItem | { path: string }, opt?: { useJprf?: boolean }) => {
            const desc: PvsTheory = await vscodeUtils.getPvsTheory(resource);
            if (desc && desc.theoryName) {
                opt = opt || {};
                // ask the user confirmation before restarting pvs
                const yesno: string[] = [ "Yes", "No" ];
                const msg: string = opt.useJprf ? `Re-run J-PRF importchain for theory ${desc.theoryName}?` : `Re-run importchain for theory ${desc.theoryName}?`;
                const ans: string = await vscode.window.showInformationMessage(msg, { modal: true }, yesno[0])
                if (ans === yesno[0]) {
                    this.quietMode = true;
                    this.proofMate.disableView();
                    this.proofExplorer.disableView();

                    const msg: string = opt.useJprf ? `Re-running J-PRF importchain for theory ${desc.theoryName}` : `Re-running importchain for theory ${desc.theoryName}`
                    this.statusBar.showProgress(msg);
                    await this.workspaceExplorer.proveImportChainWithProgress(desc, opt);

                    this.statusBar.ready();
                    this.quietMode = false;
                }
            } else {
                console.error("[vscode-events-dispatcher] Error: vscode-pvs.prove-importchain invoked with null resource", resource);
            }
        }));
        // this request comes from the context menu displayed by the editor
        context.subscriptions.push(commands.registerCommand("vscode-pvs.discharge-tccs", async (resource: TheoryItem | { path: string }, opt?: { useJprf?: boolean, unprovedOnly?: boolean }) => {
            if (window.activeTextEditor && window.activeTextEditor.document) {
                // if the file is currently open in the editor, save file first
                await window.activeTextEditor.document.save();
                if (!resource) {
                    resource = { path: window.activeTextEditor.document.fileName };
                }
            }

            const desc: PvsTheory = await vscodeUtils.getPvsTheory(resource);
            if (desc && desc.theoryName) {
                opt = opt || {};
                // ask the user confirmation before discharging
                const yesno: string[] = [ "Yes", "Unproved Only", "No" ];
                const msg: string = opt.useJprf ? `Discharge TCCs using J-PRF?` : `Discharge all TCCs?`;
                const ans: string = await vscode.window.showInformationMessage(msg, { modal: true }, yesno[0], yesno[1]);
                if (ans === yesno[0] || ans === yesno[1]) {
                    opt.unprovedOnly = ans === yesno[1];
                    this.quietMode = true;
                    this.proofMate.disableView();
                    this.proofExplorer.disableView();
                    const msg: string = (opt.unprovedOnly) ? 
                        opt.useJprf ? `Running unproved J-PRF TCCs (theory ${desc.theoryName})` : `Running unproved TCCs in theory ${desc.theoryName}`
                            : opt.useJprf ? `Re-running J-PRF TCCs (theory ${desc.theoryName})` : `Re-running TCCs in theory ${desc.theoryName}`;
                    this.statusBar.showProgress(msg);

                    await this.workspaceExplorer.proveTheoryWithProgress(desc, { tccsOnly: true, useJprf: opt.useJprf, unprovedOnly: opt.unprovedOnly });

                    this.statusBar.ready();
                    this.quietMode = false;
                }
            } else {
                console.error("[vscode-events-dispatcher] Error: vscode-pvs.discharge-tccs invoked with null resource", resource);
            }
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.jdischarge-tccs", async (resource: TheoryItem | { path: string }) => {
            commands.executeCommand("vscode-pvs.discharge-tccs", resource, { useJprf: true });
        }));
        // this request comes from workspace explorer
        context.subscriptions.push(commands.registerCommand("vscode-pvs.discharge-tccs-explorer", async (resource: TccsOverviewItem) => {
            commands.executeCommand("vscode-pvs.discharge-tccs", resource);
        }));

        context.subscriptions.push(commands.registerCommand("vscode-pvs.prove-workspace-inline", async (resource: WorkspaceItem | { path: string }) => {
            commands.executeCommand("vscode-pvs.prove-workspace", resource);
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.jprove-workspace", async (resource: WorkspaceItem | { path: string }) => {
            commands.executeCommand("vscode-pvs.prove-workspace", resource, { useJprf: true });
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.prove-workspace", async (resource: WorkspaceItem | { path: string }, opt?: { useJprf?: boolean, unprovedOnly?: boolean }) => {
            if (window.activeTextEditor && window.activeTextEditor.document) {
                // if the file is currently open in the editor, save file first
                await window.activeTextEditor.document.save();
                if (!resource) {
                    resource = { path: window.activeTextEditor.document.fileName };
                }
            }

            const desc: ContextFolder = await vscodeUtils.getPvsWorkspace(resource);
            if (desc && desc.contextFolder) {
                opt = opt || {};
                // ask the user confirmation before discharging
                const yesno: string[] = [ "Yes", "Unproved Only", "No" ];
                const contextFolderName: string = fsUtils.getContextFolderName(desc.contextFolder);
                const msg: string = opt.useJprf ? `Re-run J-PRF proofs in workspace ${contextFolderName}?` : `Re-run all proofs in workspace ${contextFolderName}?`;
                const ans: string = await vscode.window.showInformationMessage(msg, { modal: true }, yesno[0], yesno[1])
                if (ans === yesno[0] || ans === yesno[1]) {
                    opt.unprovedOnly = ans === yesno[1];
                    this.quietMode = true;
                    this.proofMate.disableView();
                    this.proofExplorer.disableView();

                    await this.workspaceExplorer.proveWorkspaceWithProgress(desc, opt);

                    this.statusBar.ready();
                    this.quietMode = false;
                }
            } else {
                console.error("[vscode-events-dispatcher] Error: vscode-pvs.discharge-tccs invoked with null resource", resource);
            }
        }));
        
        context.subscriptions.push(commands.registerCommand("vscode-pvs.clean-bin", async () => {
            // ask the user confirmation before deleting bin files
			const yesno: string[] = [ "Yes", "No" ];
			const msg: string = `Delete pvsbin folder?\n\nThis action can resolve situations where pvs fails to start or execute proof commands.`;
			const ans: string = await vscode.window.showInformationMessage(msg, { modal: true }, yesno[0])
			if (ans === yesno[0]) {
                if (vscode.workspace?.workspaceFolders?.length) {
                    const currentContext: string = vscode.workspace.workspaceFolders[0].uri.path;
                    if (currentContext) {
                        await fsUtils.cleanBin(currentContext, { removePvsbinFolder: true, keepTccs: true, recursive: fsUtils.MAX_RECURSION });
                    }
                }
            }
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.clean-tccs", async () => {
            // ask the user confirmation before deleting bin files
			const yesno: string[] = [ "Yes", "No" ];
			const msg: string = `Delete .tccs files?`;
			const ans: string = await vscode.window.showInformationMessage(msg, { modal: true }, yesno[0])
			if (ans === yesno[0]) {
                if (vscode.workspace?.workspaceFolders?.length) {
                    const currentContext: string = vscode.workspace.workspaceFolders[0].uri?.path;
                    if (currentContext) {
                        await fsUtils.cleanTccs(currentContext, { recursive: fsUtils.MAX_RECURSION });
                        // request context descriptor, so pvs explorer can refresh the view
                        const desc: FileDescriptor = fsUtils.fname2desc(window.activeTextEditor?.document?.fileName);
                        this.client.sendRequest(serverRequest.getContextDescriptor, { contextFolder: desc.contextFolder });
                    }
                }
            }
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.x-show-proof", async () => {
            this.proofExplorer?.showWebView({ recenter: true });
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.x-prove", async () => {
            commands.executeCommand("vscode-pvs.prove-formula-at-cursor-position");
            commands.executeCommand("vscode-pvs.x-show-proof");
        }));
        context.subscriptions.push(commands.registerCommand("vscode-pvs.clean-all", async () => {
            // ask the user confirmation before deleting bin files
			const yesno: string[] = [ "Yes", "No" ];
			const msg: string = `Delete temporary files (.tccs and pvsbin) created by PVS?`;
			const ans: string = await vscode.window.showInformationMessage(msg, { modal: true }, yesno[0])
			if (ans === yesno[0]) {
                if (vscode.workspace?.workspaceFolders?.length) {
                    const currentContext: string = vscode.workspace.workspaceFolders[0].uri.path;
                    if (currentContext) {
                        await fsUtils.cleanBin(currentContext, { removePvsbinFolder: true, keepTccs: false, recursive: fsUtils.MAX_RECURSION });
                    }
                }
            }
        }));
        // vscode-pvs.typecheck-file
		context.subscriptions.push(commands.registerCommand("vscode-pvs.typecheck-file", async (resource: string | { path: string } | { contextValue: string } | PvsFile) => {
            if (window.activeTextEditor && window.activeTextEditor.document) {
                // if the file is currently open in the editor, save file first
                await window.activeTextEditor.document.save();
                if (!resource) {
                    resource = { path: window.activeTextEditor.document.fileName };
                }
            }
			if (resource) {
                const desc: FileDescriptor = vscodeUtils.resource2desc(resource);
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
                const desc: FileDescriptor = vscodeUtils.resource2desc(resource);
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
                const desc: FileDescriptor = vscodeUtils.resource2desc(resource);
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
                const desc: FileDescriptor = vscodeUtils.resource2desc(resource);
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
                let desc = vscodeUtils.resource2desc(resource);
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
                let desc = vscodeUtils.resource2desc(resource);
                if (desc) {
                    // send parse request to pvs-server
                    this.client.sendRequest(serverRequest.typecheckWorkspaceWithFeedback, desc);
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
                let desc = vscodeUtils.resource2desc(resource);
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
                vscodeUtils.showErrorMessage(desc.msg);
            }
        });
        this.client.onNotification("server.status.progress", (desc: { msg: string }) => {
            if (desc && desc.msg) {
                this.statusBar.showProgress(desc.msg);
            }
        });
        this.client.onNotification("server.status.warning", (desc?: { msg?: string }) => {
            this.statusBar.ready();
            if (desc && desc.msg) {
                vscodeUtils.showWarningMessage(desc.msg);
            }
        });
        this.client.onNotification("server.status.info", (desc?: { msg?: string }) => {
            this.statusBar.ready();
            if (desc && desc.msg) {
                vscodeUtils.showInformationMessage(desc.msg);
            }
        });
        this.client.onNotification("server.status.start-important-task", (desc: { id: string, msg: string, increment?: number }) => {
            if (desc && desc.msg) {
                if (this.quietMode) {
                    this.client.onNotification(`server.status.end-important-task-${desc.id}-with-errors`, (desc: { msg: string }) => {
                        this.statusBar.ready();
                        if (desc && desc.msg) {
                            this.statusBar.showError(desc.msg); // use the status bar rather than dialogs, because we don't have APIs to close old dialogs with potentially stale information
                            vscodeUtils.showErrorMessage(desc.msg);
                            vscodeUtils.showProblemsPanel();
                        }
                    });
                    return;
                }
                // show dialog with progress
                window.withProgress({
                    location: ProgressLocation.Notification,
                    cancellable: true
                }, (progress, token) => { 
                    let complete: boolean = false;
                    // show initial dialog with spinning progress
                    progress.report({ increment: -1, message: desc.msg });
                    setTimeout(() => {
                        if (!complete) {
                            progress.report({
                                increment: isNaN(desc.increment) ? -1 : desc.increment,
                                message: "Almost ready, please wait... \n(" + desc.msg + ")"
                            });
                        }
                    }, 8000);
                    // update the dialog
                    return new Promise<void>((resolve, reject) => {
                        token.onCancellationRequested(() => {
                            if (!complete) {
                                complete = true;
                                // send cancellation request to the server
                                this.client.sendRequest(serverRequest.cancelOperation);
                                // clear status bar
                                this.statusBar.ready();
                                // dispose of the dialog
                                resolve(null);
                            }
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
                            complete = true;
                            if (desc && desc.msg) {
                                vscodeUtils.showInformationMessage(desc.msg);
                                resolve();
                                // progress.report({
                                //     increment: 100,
                                //     message: desc.msg
                                // });
                                // setTimeout(() => {
                                //     resolve(null);
                                // }, this.NOTIFICATION_TIMEOUT);
                                // window.showInformationMessage(desc.msg); // notification is given with the same dialog instead of creating a new one, this avoids cluttering vscode notification list
                            } else {
                                resolve();
                            }
                        });
                        this.client.onNotification(`server.status.end-important-task-${desc.id}-with-errors`, (desc: { msg: string }) => {
                            this.statusBar.ready();
                            complete = true;
                            if (desc && desc.msg) {
                                this.statusBar.showError(desc.msg);
                                // NOTE: we don't have APIs to close old error dialogs that contain stale information.
                                // The user will need to close the error dialog. 
                                // If the user doesn't close the dialog, the message may show up later, and bring confusion.
                                // vscodeUtils.showErrorMessage(desc.msg);
                                vscodeUtils.showErrorMessage("Error: " + desc.msg);
                                // progress.report({
                                //     increment: 100,
                                //     message: "Error: " + desc.msg
                                // });
                                // setTimeout(() => {
                                //     resolve();
                                // }, this.NOTIFICATION_TIMEOUT * 4);
                                vscodeUtils.showProblemsPanel();
                                resolve();
                            } else {
                                resolve();
                            }
                        });
                    });
                });

                // show progress on the status bar
                this.statusBar.showProgress(desc.msg);
            }

        });

        this.client.onNotification("server.status.pvs-failure", (opt?: { msg?: string, fname?: string, method?: string, error_type?: string, src?: string }) => {
            opt = opt || {};
            const src: string = opt.src || "pvs";
            let msg: string = opt.msg || "";
            msg = msg.replace("[pvs-server]", "").trim();
            if (opt.error_type === "dependency") {
                vscodeUtils.showDependencyError(msg);
            } else {
                msg = msg || `pvs-server crashed into Lisp.\nTo continue, you may need to Reboot pvs.`;
                if (opt.fname) {
                    // msg += `\nThe error occurred while processing file [${opt.fname}](file://${opt.fname})`; // vscode is unable to render marked strings in dialogs
                    msg += `\nThe error occurred while processing file ${opt.fname} ${opt?.method}`;
                } else if (opt.method) {
                    // msg += `\nThe error occurred while executing method [${opt.method}](${opt.method})`; // vscode is unable to render marked strings in dialogs
                    msg += `\nThe error occurred while executing the following method ${opt.method}`;
                }
                msg = (msg && msg.startsWith("Error:")) ? msg : `Error: ` + msg;
                // vscodeUtils.showErrorMessage(msg);
                vscodeUtils.showFailure(msg, src);
            }
        });
    }
}