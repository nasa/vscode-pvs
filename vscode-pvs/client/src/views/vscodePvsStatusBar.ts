/**
 * @module VSCodePvsStatusBar
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
import { StatusBarItem, ExtensionContext, StatusBarAlignment, window, commands, TextEditor } from "vscode";
import { LanguageClient } from "vscode-languageclient";
import { PvsVersionDescriptor } from "../common/serverInterface";
import * as vscodeUtils from '../utils/vscode-utils';

export class StatusBarPriority {
    public static Min: number = 1;
    public static Medium: number = 2;
    public static Max: number = 3;
}

export class VSCodePvsStatusBarItem {
    protected maxLen: number = 64;
    protected lab: StatusBarItem; // label
    protected ico: StatusBarItem; // animated icon
    constructor (alignment: StatusBarAlignment, priority: number) {
        this.ico = window.createStatusBarItem(alignment, priority);
        this.lab = window.createStatusBarItem(alignment, priority);
    }
    text (text: string): void {
        this.lab.text = text;
        if (text?.length > this.maxLen) {
            const start: string = text.substring(0, this.maxLen / 2);
            const end: string = text.substring(text.length - this.maxLen / 2);
            this.lab.text = start + " ... " + end;
        }
    }
    icon (text: string): void {
        if (text !== this.ico.text) {
            this.ico.text = text;
        }
    }
    command (cmd: string): void {
        this.lab.command = cmd;
        this.ico.command = cmd;
    }
    show (): void {
        this.ico.show();
        this.lab.show();
    }
    hide (): void {
        this.lab.hide();
        this.ico.hide();
    }
    clear (): void {
        this.lab.text = "";
        this.ico.text = "";
    }
}

export class VSCodePvsStatusBar {
    protected pvsVersionInfo: PvsVersionDescriptor;
    protected contextFolder: string = "";

    protected pvsStatus: VSCodePvsStatusBarItem;
    protected workspaceStatus: VSCodePvsStatusBarItem;
    protected versionInfo: VSCodePvsStatusBarItem;
    // protected crashReport: VSCodePvsStatusBarItem;
    protected restartPvs: VSCodePvsStatusBarItem;
    protected interruptProver: VSCodePvsStatusBarItem;
    protected downloadNasalib: VSCodePvsStatusBarItem;

    // running flag, disables this.ready()
    protected runningFlag: boolean = false;

    protected client: LanguageClient;

    /**
     * Constructor
     * @param client VSCode Language Client, necessary for registering event handlers 
     */
    constructor (client: LanguageClient) {
        this.client = client;
        // create status bar elements
        this.workspaceStatus = new VSCodePvsStatusBarItem(StatusBarAlignment.Left, StatusBarPriority.Max);
        this.pvsStatus = new VSCodePvsStatusBarItem(StatusBarAlignment.Left, StatusBarPriority.Medium);
        this.versionInfo = new VSCodePvsStatusBarItem(StatusBarAlignment.Right, StatusBarPriority.Medium);

        this.restartPvs = new VSCodePvsStatusBarItem(StatusBarAlignment.Left, StatusBarPriority.Max);
        // this.crashReport = new VSCodePvsStatusBarItem(StatusBarAlignment.Left, StatusBarPriority.Medium);

        this.interruptProver = new VSCodePvsStatusBarItem(StatusBarAlignment.Left, StatusBarPriority.Medium);
        this.downloadNasalib = new VSCodePvsStatusBarItem(StatusBarAlignment.Left, StatusBarPriority.Medium);
    }


    /**
     * Shows pvs version and lisp version in the status bar
     * @param desc pvs version and lisp version
     */
    pvsReady (desc: PvsVersionDescriptor): void {
        if (desc) {
            this.pvsVersionInfo = desc;
            const activeEditor: TextEditor = vscodeUtils.getActivePvsEditor();
            if (activeEditor?.document?.languageId === "pvs") {
                this.showVersionInfo();
            }
            this.ready();
        }
    }


    setContextFolder (contextFolder: string): void {
        if (this.contextFolder !== contextFolder) {
            this.contextFolder = contextFolder;
        }
    }
    updateStats (desc: { contextFolder: string, fileName: string, fileExtension: string, stats: { types: number, definitions: number, lemmas: number }}): void {
        // if (desc && desc.stats && desc.fileExtension !== ".tccs") {
        //     const fname: string = path.join(desc.contextFolder, `${desc.fileName}${desc.fileExtension}`);
        // }
    }

    /**
     * Clears the status bar and makes it visible
     */
    ready (): void {
        this.pvsStatus.clear();
        this.pvsStatus.show();
    };

    /**
     * Shows a spinning icon and a message in the status bar
     * @param msg message
     */
    showProgress (msg: string): void {
        if (!this.runningFlag) {
            if (msg) {
                this.pvsStatus.icon(`$(loading~spin)`);
                this.pvsStatus.text(msg);
                this.pvsStatus.show();
            } else {
                this.ready();
            }
        }
    }

    /**
     * Sets/resets running flag
     */
    running (flag: boolean): void {
        this.runningFlag = !!flag;
    }

    /**
     * Shows a megaphone icon and a message in the status bar
     * @param msg message
     */
    showInfo (msg: string): void {
        if (msg) {
            this.pvsStatus.icon("");
            this.pvsStatus.text(`$(info)  ${msg}`);
            this.pvsStatus.show();
        } else {
            this.ready();
        }
    }

    /**
     * Shows a message in the status bar
     * @param msg message
     */
    showMsg (msg: string): void {
        if (msg) {
            this.pvsStatus.icon("");
            this.pvsStatus.text(msg);
            this.pvsStatus.show();
        } else {
            this.ready();
        }
    }
    
    /**
     * Shows an error message in the status bar
     * @param msg message
     */
    showError (msg: string): void {
        if (msg) {
            const shortmsg: string = msg.split("\n")[0];
            this.pvsStatus.icon("");
            this.pvsStatus.text(`$(pinned-dirty)  ${shortmsg}`); // messages in the status bar should always be on one line
            this.pvsStatus.show();
            vscodeUtils.showProblemsPanel();
        } else {
            this.ready();
        }
    }

    getVersionInfo (): PvsVersionDescriptor {
        return this.pvsVersionInfo;
    }
    showVersionDialog (opt?: { trailingNote?: string, downloadButtons?: boolean }): void {
        opt = opt || {};
        opt.trailingNote = opt.trailingNote || "";
        if (this.pvsVersionInfo) {
            this.versionInfo.icon("");
            this.versionInfo.text(this.pvsVersionInfo["pvs-version"]);
            let msg: string = `PVS ${this.pvsVersionInfo["pvs-version"]}`;
            let extras: string[] = [];
            if (this.pvsVersionInfo["lisp-version"]) {
                extras.push(this.pvsVersionInfo["lisp-version"]);
            }
            if (this.pvsVersionInfo["nasalib-version"]) {
                extras.push(this.pvsVersionInfo["nasalib-version"]);
            }
            if (extras.length) {
                msg += ` (${extras.join(" + ")})`;
            }
            if (opt.trailingNote) {
                msg += opt.trailingNote;
            }
            if (opt.downloadButtons) {
                const options: string[] = [ "Reboot PVS", "Show Welcome Screen"];
                window.showInformationMessage(msg, options[0], options[1]).then((sel: string) => {
                    switch (sel) {
                        case options[0]: {
                            commands.executeCommand('vscode-pvs.reboot-pvs');
                            break;
                        }
                        case options[1]: {
                            vscodeUtils.showReleaseNotes();
                            break;
                        }
                        default: // do nothing
                    }    
                });
            } else {
                vscodeUtils.showInformationMessage(msg);
            }
        }
        this.versionInfo.show();
    }
    showVersionInfo (): void {
        if (this.pvsVersionInfo) {
            this.versionInfo.icon("");
            this.versionInfo.text(this.pvsVersionInfo["pvs-version"]);
            this.versionInfo.command("vscode-pvs.show-version-dialog");
        }
        this.versionInfo.show();
    }
    hideVersionInfo (): void {
        this.versionInfo.hide();
    }
    showRestartButton (): void {
        this.restartPvs.icon("");
        this.restartPvs.text(`$(debug-restart)  Reboot pvs`);
        this.restartPvs.command("vscode-pvs.reboot-pvs");
        this.restartPvs.show();
    }
    hideRestartButton (): void {
        this.restartPvs.hide();
    }
    showInterruptButton (): void {
        this.interruptProver.icon("");
        this.interruptProver.text(`$(stop-circle)  Interrupt Prover`);
        this.interruptProver.command("vscode-pvs.interrupt-prover");
        this.interruptProver.show();
    }
    hideInterruptButton (): void {
        this.interruptProver.hide();
    }
    showDownloadNasalibButton (showButton?: boolean): void {
        showButton = showButton === undefined ? true : !!showButton;
        if (showButton === true) {
            this.downloadNasalib.icon("");
            this.downloadNasalib.text(`$(symbol-function)  Download NASALib`);
            this.downloadNasalib.command("vscode-pvs.download-nasalib");
            this.downloadNasalib.show();
        } else {
            this.hideDownloadNasalibButton();
        }
    }
    hideDownloadNasalibButton (): void {
        this.downloadNasalib.hide();
    }

    /**
     * Shows a critical failures in the status bar -- these failures require restarting pvs
     * @param msg message
     */
    failure (msg: string): void {
        const shortmsg: string = (msg) ? msg.split("\n")[0] : msg;
        this.pvsStatus.icon("");
        this.pvsStatus.text(`$(exclude)  ${shortmsg}`); // messages in the status bar should be on one line
        this.pvsStatus.show();
    }

    /**
     * Activates the service provider
     */
    activate (context: ExtensionContext) {
        this.show();
        context.subscriptions.push(commands.registerCommand("vscode.show-statusbar-message", (msg: string) => {
            this.showInfo(msg);
        }));
    }

    show (): void {
        this.pvsStatus.show();
        this.showVersionInfo();
        // this.showRestartButton();
    }

    hide (): void {
        this.pvsStatus.hide();
        this.hideVersionInfo();
        this.hideInstallNasalib();
        // this.hideRestartButton();
    }

    showInstallNasalib (): void {
        this.showDownloadNasalibButton();
    }
    hideInstallNasalib (): void {
        this.hideDownloadNasalibButton();
    }

    /**
     * Internal function, returns the content of the tooltip for PVS
     */
    protected printfVersionInfo (): string {
        if (this.pvsVersionInfo) {
            return `PVS ${this.pvsVersionInfo["pvs-version"]} ${this.pvsVersionInfo["lisp-version"]}`;
        }
        return `PVS`;
    }
    
}