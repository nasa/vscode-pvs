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
import { StatusBarItem, ExtensionContext, StatusBarAlignment, window, commands } from "vscode";
import { LanguageClient } from "vscode-languageclient";
import * as path from 'path';
import { PvsVersionDescriptor } from "../common/serverInterface";

export class StatusBarPriority {
    public static Min: number = 1;
    public static Medium: number = 2;
    public static Max: number = 3;
}

export class VSCodePvsStatusBar {
    protected pvsVersionInfo: PvsVersionDescriptor;
    protected pvsStatus: StatusBarItem;
    protected workspaceStatus: StatusBarItem;
    protected versionInfo: StatusBarItem;
    protected crashReport: StatusBarItem;
    protected restartPvs: StatusBarItem;

    protected downloadNasalib: StatusBarItem;

    protected stats: {
        [filename: string]: { lemmas: number, definitions: number, types: number }
    } = { "!tot!": { lemmas: 0, definitions: 0, types: 0 } };
    protected nfiles: number = 0;
    protected contextFolder: string = "";

    protected client: LanguageClient;

    protected makeStats (): string {
        const nFiles: number = Object.keys(this.stats).length - 1; // -1 because stats includes key "!tot!" with summary info
        let wName: string = this.contextFolder.endsWith("/") ? this.contextFolder.substring(0, this.contextFolder.length - 1) : this.contextFolder;
        wName = wName.substring(wName.lastIndexOf("/") + 1, wName.length);
        let msg: string = `[ Active workspace: ${wName} ] `;
        // msg += (nFiles !== 1) ? `${nFiles} of ${this.nfiles} files parsed ` : `1 of ${this.nfiles} file parsed `;
        // if (os.platform() !== "darwin") {
        //     // stats are not yet supported in macOs
        //     msg += `(${this.stats["!tot!"].types} types, ${this.stats["!tot!"].lemmas} lemmas, ${this.stats["!tot!"].definitions} definitions)`;
        // }
        return msg;
    }
    protected resetStats (): void {
        this.stats = {};
        this.stats["!tot!"] = { lemmas: 0, definitions: 0, types: 0 };
    }

    /**
     * Constructor
     * @param client VSCode Language Client, necessary for registering event handlers 
     */
    constructor (client: LanguageClient) {
        this.client = client;
        // create status bar elements
        this.workspaceStatus = window.createStatusBarItem(StatusBarAlignment.Left, StatusBarPriority.Max);
        this.pvsStatus = window.createStatusBarItem(StatusBarAlignment.Left, StatusBarPriority.Medium);
        this.versionInfo = window.createStatusBarItem(StatusBarAlignment.Right, StatusBarPriority.Medium);

        this.restartPvs = window.createStatusBarItem(StatusBarAlignment.Left, StatusBarPriority.Max);
        this.crashReport = window.createStatusBarItem(StatusBarAlignment.Left, StatusBarPriority.Medium);

        this.downloadNasalib = window.createStatusBarItem(StatusBarAlignment.Left, StatusBarPriority.Medium);
    }


    /**
     * Shows pvs version and lisp version in the status bar
     * @param desc pvs version and lisp version
     */
    pvsReady (desc: PvsVersionDescriptor): void {
        if (desc) {
            this.pvsVersionInfo = desc;
            this.showVersionInfo();
            this.ready();
        }
    }


    setFiles (nfiles: number): void {
        this.nfiles = nfiles;
    }
    setContextFolder (contextFolder: string): void {
        if (this.contextFolder !== contextFolder) {
            this.contextFolder = contextFolder;
            this.resetStats();
            // this.showStats();
        }
    }
    updateStats (desc: { contextFolder: string, fileName: string, fileExtension: string, stats: { types: number, definitions: number, lemmas: number }}): void {
        if (desc && desc.stats && desc.fileExtension !== ".tccs") {
            const fname: string = path.join(desc.contextFolder, `${desc.fileName}${desc.fileExtension}`);
            if (!this.stats[fname] 
                || (this.stats[fname]
                    && (this.stats[fname].lemmas !== desc.stats.lemmas 
                        || this.stats[fname].definitions !== desc.stats.definitions
                        || this.stats[fname].types !== desc.stats.types))) {
                this.stats[fname] = desc.stats;
                const fnames: string[] = Object.keys(this.stats);
                this.stats["!tot!"] = { lemmas: 0, definitions: 0, types: 0 };
                for (const i in fnames) {
                    const fname: string = fnames[i];
                    if (fname !== "!tot!") {
                        this.stats["!tot!"].lemmas += this.stats[fname].lemmas;
                        this.stats["!tot!"].definitions += this.stats[fname].definitions;
                        this.stats["!tot!"].types += this.stats[fname].types;
                    }
                }
            } 
        }
    }
    showStats (): void {
        this.workspaceStatus.text = this.makeStats();
    }

    /**
     * Shows "ready" in the status bar for 2 secs
     */
    ready (): void {
        this.pvsStatus.text = "";//`$(check)  pvs-server ready!`;
        this.pvsStatus.show();
    };

    /**
     * Shows a spinning icon and a message in the status bar
     * @param msg message
     */
    showProgress (msg: string): void {
        this.pvsStatus.text = `$(loading~spin)  ${msg}`;
        this.pvsStatus.show();
    }

    /**
     * Shows a megaphone icon and a message in the status bar
     * @param msg message
     */
    showInfo (msg: string): void {
        this.pvsStatus.text = `$(megaphone)  ${msg}`;
        this.pvsStatus.show();
    }

    /**
     * Shows a message in the status bar
     * @param msg message
     */
    showMsg (msg: string): void {
        this.pvsStatus.text = msg;
        this.pvsStatus.show();
    }
    
    /**
     * Shows an error message in the status bar
     * @param msg message
     */
    showError (msg: string): void {
        const shortmsg: string = (msg) ? msg.split("\n")[0] : msg;
        this.pvsStatus.text = `$(warning~spin)  ${shortmsg}`; // messages in the status bar should always be on one line
        this.pvsStatus.show();
        // show problems panel -- see also Code->Preferences->KeyboardShortcuts
        commands.executeCommand("workbench.panel.markers.view.focus");
    }

    getVersionInfo (): PvsVersionDescriptor {
        return this.pvsVersionInfo;
    }

    showVersionInfo (): void {
        if (this.pvsVersionInfo) {
            this.versionInfo.text = this.pvsVersionInfo["pvs-version"];
            this.versionInfo.command = "vscode-pvs.show-version-info";
        }
        this.versionInfo.show();
    }
    hideVersionInfo (): void {
        this.versionInfo.hide();
    }
    showRestartButton (): void {
        this.restartPvs.text = `$(debug-restart) Reboot pvs-server`;
        this.restartPvs.command = "vscode-pvs.reboot-pvs";
        this.restartPvs.show();
    }
    hideRestartButton (): void {
        this.restartPvs.hide();
    }
    showDownloadNasalibButton (showButton?: boolean): void {
        if (showButton === true) {
            this.downloadNasalib.text = `$(symbol-function) Download NASALib`;
            this.downloadNasalib.command = "vscode-pvs.download-nasalib";
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
        this.crashReport.text = `$(debug~spin)  ${shortmsg}`; // messages in the status bar should be on one line
        this.crashReport.show();
        this.pvsStatus.hide();
    }

    /**
     * Activates the service provider
     */
    activate (context: ExtensionContext) {
        this.resetStats();
        this.show();
    }

    show (): void {
        // this.workspaceStatus.show();
        this.pvsStatus.show();
        this.showVersionInfo();
        this.showRestartButton();
    }

    hide (): void {
        // this.workspaceStatus.hide();
        this.pvsStatus.hide();
        this.hideVersionInfo();

        this.hideInstallNasalib();
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
        return `PVS ${this.pvsVersionInfo["pvs-version"]} ${this.pvsVersionInfo["lisp-version"]}`;
    }
    
}