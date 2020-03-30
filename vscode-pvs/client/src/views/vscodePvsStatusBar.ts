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
import { StatusBarItem, ExtensionContext, StatusBarAlignment, window } from "vscode";
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
    protected versionInfoBar: StatusBarItem;

    protected stats: {
        [filename: string]: { lemmas: number, definitions: number, types: number }
    } = { "!tot!": { lemmas: 0, definitions: 0, types: 0 } };
    protected nfiles: number = 0;
    protected contextFolder: string = "";

    protected client: LanguageClient;

    protected makeStats (): string {
        const nFiles: number = Object.keys(this.stats).length - 1; // -1 because stats includes key "!tot!" with summary info
        const wName: string = this.contextFolder.substring(this.contextFolder.lastIndexOf("/") + 1, this.contextFolder.length);
        let msg: string = `[ ${wName} ] `;
        msg += (nFiles !== 1) ? `${nFiles} of ${this.nfiles} files parsed ` : `1 of ${this.nfiles} file parsed `;
        msg += `(${this.stats["!tot!"].types} types, ${this.stats["!tot!"].lemmas} lemmas, ${this.stats["!tot!"].definitions} definitions)`;
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
        this.pvsStatus = window.createStatusBarItem(StatusBarAlignment.Left, StatusBarPriority.Max);
        this.workspaceStatus = window.createStatusBarItem(StatusBarAlignment.Left, StatusBarPriority.Max);
        this.versionInfoBar = window.createStatusBarItem(StatusBarAlignment.Right, StatusBarPriority.Medium);
    }


    /**
     * Shows pvs version and lisp version in the status bar
     * @param desc pvs version and lisp version
     */
    pvsReady (desc: PvsVersionDescriptor): void {
        if (desc) {
            this.pvsVersionInfo = desc;
            this.versionInfoBar.text = desc["pvs-version"];
            this.versionInfoBar.tooltip = this.printfVersionInfo();
        }
    }

    setFiles (nfiles: number): void {
        this.nfiles = nfiles;
    }
    setContextFolder (contextFolder: string): void {
        if (this.contextFolder !== contextFolder) {
            this.contextFolder = contextFolder;
            this.resetStats();
            this.showStats();
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
        this.pvsStatus.text = `$(check)  Ready!`;
    };

    /**
     * Shows a spinning icon and a message in the status bar
     * @param msg message
     */
    progress (msg: string): void {
        this.pvsStatus.text = `$(sync~spin)  ${msg}`;
    }

    /**
     * Shows a megaphone icon and a message in the status bar
     * @param msg message
     */
    info (msg: string): void {
        this.pvsStatus.text = `$(megaphone)  ${msg}`;
    }

    /**
     * Shows a message in the status bar
     * @param msg message
     */
    msg (msg: string): void {
        this.pvsStatus.text = msg;
    }
    
    /**
     * Shows an error message in the status bar
     * @param msg message
     */
    error (msg: string): void {
        this.pvsStatus.text = msg;
    }

    /**
     * Activates the service provider
     */
    activate (context: ExtensionContext) {
        this.pvsStatus.show();
        this.resetStats();
        this.workspaceStatus.show();
        this.versionInfoBar.show();
    }

    /**
     * Internal function, returns the content of the tooltip for PVS
     */
    protected printfVersionInfo (): string {
        return `PVS ${this.pvsVersionInfo["pvs-version"]} ${this.pvsVersionInfo["lisp-version"]}`;
    }
    
}