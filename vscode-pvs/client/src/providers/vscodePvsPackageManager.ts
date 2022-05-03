/**
 * @module VSCodePvsPackageManager
 * @author Paolo Masci
 * @date 2019.10.24
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
import {
    window, Uri, workspace, ConfigurationTarget, Progress, CancellationToken, 
    ProgressLocation, Terminal, ViewColumn, WebviewPanel, ExtensionContext
} from "vscode";
import {
    serverEvent, sriUrl, serverRequest, PvsDownloadDescriptor, NASALibUrl, 
    pvsUrl, DownloadWithProgressRequest, DownloadWithProgressResponse, 
    InstallWithProgressRequest, InstallWithProgressResponse, RebootPvsServerRequest, 
    NASALibDownloader, NASALibDownloaderRequest, NASALibDownloaderResponse,
    NASALibGithubFile, ShellCommand, NASALibGithubBranch, ListVersionsWithProgressResponse, ListVersionsWithProgressRequest, PvsVersionDescriptor
} from "../common/serverInterface";
import * as path from 'path';
import { VSCodePvsStatusBar } from "../views/vscodePvsStatusBar";
import * as fsUtils from '../common/fsUtils';
import * as vscodeUtils from '../utils/vscode-utils';
import { VSCodePvsEchoTerminal } from "./vscodePvsEchoTerminal";
import * as os from 'os';
import { colorText, PvsColor } from "../common/colorUtils";

export const errorMsgDownloaderNotPresent: string = `Error: Unable to download the file ('curl' or 'wget' not available). Please install 'curl' or 'wget', as vscode-pvs is unable to perform downloads without them.`; 
export const errorPvsVersionsNotAvailable: string = `Error: Unable to retrieve information on the PVS versions. Please try again later or proceed with manual installation of PVS.`; 

export class VSCodePvsPackageManager {
    // client for sending messages to the server
    protected client: LanguageClient;

    // status bar, used for brief feedback messages
    protected statusBar: VSCodePvsStatusBar;

    // terminal window, used to report feedback during the installation process
    protected terminalWindow: Terminal;
    protected terminal: VSCodePvsEchoTerminal;

    // flag indicating this is the first time the package manager is executed
    protected firstRun: boolean = true;

    /**
     * Common text messages displayed in different dialogs
     */
    readonly messages = {
        setPvsPath: "Select location of PVS executables",
        downloadPvs: "Download PVS",
        updatePvs: "Update PVS",
        chooseInstallationFolder: "Choose installation folder",
        selectInstallationFolder: "Use as installation folder",
        downloadNASALib: "Download NASALib",
        updateNASALib: "Update NASALib"
    };

    /**
     * Constructor
     */
	constructor (client: LanguageClient, statusBar: VSCodePvsStatusBar) {
        this.client = client;
        this.statusBar = statusBar;
    }
    
    /**
     * Activates the functionalities of the package manager
     * @param context 
     */
    activate (context: ExtensionContext): void {
        // pvs installation handlers
        this.client.onRequest(serverEvent.pvsNotFound, async () => {
            this.statusBar.ready();
            if (this.firstRun) {
                // show the wizard only the first time vscode is opened, to avoid annoying the user with messages if they don't want to install pvs right away during the current session
                this.firstRun = false;
                const success: boolean = await this.pvsInstallationWizard({
                    msg: `VSCode-PVS is almost ready!\n\nTo complete the installation, please choose one of the following actions.\n `
                });
                if (success) {
                    await this.nasalibInstallationWizard();
                }
            }
        });
        this.client.onRequest(serverEvent.pvsIncorrectVersion, async (msg: string) => {
            this.statusBar.ready();
            if (this.firstRun) {
                this.firstRun = false;
                // show the wizard only the first time vscode is opened, to avoid annoying the user with messages if they don't want to install pvs right away during the current session
                const success: boolean = await this.pvsInstallationWizard({ msg });
                if (success) {
                    await this.nasalibInstallationWizard();
                }
            }
        });
    }

    /**
     * Chooses PVS path with a dialog and then sets pvs.path in vscode settings
     */
    async choosePvsPath (): Promise <void> {
        // choose installation folder
        const pvsPath: string = await this.choosePvsInstallationFolder();
        if (pvsPath) {
            await workspace.getConfiguration().update("pvs.path", pvsPath, ConfigurationTarget.Global);
            const req: RebootPvsServerRequest = { pvsPath };
            this.client.sendRequest(serverRequest.rebootPvsServer, req);
            window.showInformationMessage(`PVS path is ${pvsPath}`);
        }
    }

    /**
     * Shows Allegro license agreement. Returns true if the user has accepted the agreement.
     */
    async showPvsLicense (): Promise<boolean> {
        // show license agreement
        const panel: WebviewPanel = window.createWebviewPanel(
            'vscode-pvs.pvs-license-agreement', // Identifies the type of the webview. Used internally
            'PVS Allegro License Agreement', // Title of the panel displayed to the user
            ViewColumn.One, // Editor column to show the new webview panel in.
            { enableFindWidget: true } // Webview options. More on these later.
        );
        panel.webview.html = await this.downloadPvsLicensePageWithProgress();

        const agreement: { [ btn: string ]: string } = {
            license: "View PVS License",
            cancel: "Cancel",
            accept: "I Accept",
            doNotAccept: "I DO NOT Accept"
        };
        const info: string = `The PVS version you are about to download from ${pvsUrl} is freely available, `
            + `but requires a license agreement. Please read carefully the terms of the PVS license and click "I Accept" `
            + `if you agree with its terms.`;
        const ans: string = await window.showInformationMessage(info, {
            modal: false
        }, agreement.accept, agreement.doNotAccept);

        // close license agreement
        panel.dispose();
        return ans === agreement.accept;
    }

    /**
     * Utility function, creates a terminal window for showing feedback to the user.
     * The function does nothing if the terminal window exists already
     */
    async createTerminal (name: string, opt?: { clearScreen?: boolean }): Promise<boolean> {
        this.terminal = this.terminal || new VSCodePvsEchoTerminal(this.client);
        if (opt?.clearScreen || !this.terminalWindow) {
            this.terminalWindow = window.createTerminal({ name, pty: this.terminal });
        }
        this.terminalWindow.show(true); // preserve focus    
        return await new Promise<boolean> ((resolve, reject) => {
            // a small timeout is needed to given the terminal the time to be rendered
            setTimeout(async () => {
                resolve(true);
            }, 250);
        });
    }

    /**
     * Installation wizard for PVS
     */
    async pvsInstallationWizard (opt?: { msg?: string, update?: boolean }): Promise<boolean> {
        opt = opt || {};
        const label: string = opt.msg || "PVS Installation Wizard";
		const item = await window.showInformationMessage(label, {
            modal: true
        }, opt.update ? this.messages.updatePvs : this.messages.downloadPvs, this.messages.setPvsPath);

        if (!item) { return false; }
        if (item === this.messages.setPvsPath) { return await this.selectPvsPath(); }
        if (item === this.messages.downloadPvs || item === this.messages.updatePvs) {
            // create terminal to show feedback during download/update operations
            await this.createTerminal(label, { clearScreen: true });
            let pvsPath: string = vscodeUtils.getConfiguration("pvs.path");
            let baseFolder: string = fsUtils.getContextFolder(pvsPath);

            if (item === this.messages.downloadPvs || !baseFolder || !fsUtils.folderExists(baseFolder)) {
                // choose installation folder
                baseFolder = await this.choosePvsInstallationFolder();
                if (baseFolder) {
                    // show license agreement
                    const licenseAccepted: boolean = await this.showPvsLicense();
                    // abort installation if target folder license not accepted
                    if (!licenseAccepted) {
                        this.statusBar.ready();
                        return false;
                    }
                }
            }

            if (baseFolder) {
                const info: { version: string, url: string } = await this.listPvsVersionsWithProgress();
                if (info?.version && info?.url) {
                    // download and install pvs
                    const desc: { fname: string } = await this.downloadWithProgress("PVS", {
                        url: info.url,
                        baseFolder
                    });
                    if (desc?.fname) {
                        // update pvsPath with the current version info
                        pvsPath = path.join(baseFolder, `pvs-${info.version}`);
                        this.terminal.log(colorText(`Installing PVS to ${pvsPath}`, PvsColor.blue), { addNewLine: true });
                        const shellCommand: ShellCommand = {
                            cmd: "tar",
                            args: [
                                `-C "${baseFolder}"`,
                                `-xvf "${desc.fname}"`
                            ]
                        };
                        const success: boolean = await this.installWithProgress("PVS", {
                            shellCommand,
                            targetFolder: pvsPath,
                            saveAndRestore: path.join(pvsPath, "nasalib"),
                            installScript: //{
                                // cwd: pvsPath,
                                `cd ${pvsPath} && ./install-sh`,
                                // quiet: true
                            // },
                            cleanTarget: true
                        });
                        if (success) {            
                            // reboot pvs-server
                            const req: RebootPvsServerRequest = { pvsPath };
                            this.client.sendRequest(serverRequest.rebootPvsServer, req);

                            // show release info
                            vscodeUtils.showReleaseNotes();
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }

    /**
     * Utility function, shows a dialog that allows the user to select the pvs installation folder in the file system
     */
    protected async selectPvsPath (): Promise<boolean> {
        const uris: Uri[] = await window.showOpenDialog({
            canSelectFiles: false,
            canSelectFolders: true,
            canSelectMany: false,
            openLabel: this.messages.setPvsPath
        });
        if (uris?.length === 1) {
            const pvsPath: string = uris[0].fsPath;
            await workspace.getConfiguration().update("pvs.path", pvsPath, ConfigurationTarget.Global);
            // pvs-server will be automatically rebooted when pvs.path is updated, see workspace.onDidChangeConfiguration in pvsLanguageClient.ts
            return pvsPath !== null && pvsPath !== undefined && pvsPath?.trim() !== "";
        }
        return false;
    }

    /**
     * Utility function used by pvsInstallationWizard to ask the user to choose the pvs installation folder.
     * The installation folder represents the basePath component of pvsPath -- pvsPath = path.join(<installation-folder>, <pvs-version>)
     */
     protected async choosePvsInstallationFolder (msg?: string): Promise<string> {
        const labels: { [ btn: string ]: string } = {
            browse: this.messages.chooseInstallationFolder,
            cancel: "Cancel"
        }
        msg =  msg || `Please choose PVS installation folder.\nA subfolder with the PVS executables will be automatically created under the selected folder.`;
        const item = await window.showInformationMessage(msg, { modal: true }, labels.browse);//, labels.cancel);
        if (item === labels.browse) {
            const pvsInstallationFolder: Uri[] = await window.showOpenDialog({
                canSelectFiles: false,
                canSelectFolders: true,
                canSelectMany: false,
                openLabel: this.messages.selectInstallationFolder
            });
            if (pvsInstallationFolder && pvsInstallationFolder.length === 1) {
                const baseFolder: string = pvsInstallationFolder[0].fsPath;
                return baseFolder;                
            }
        }
		return null;
	}

    /**
     * Utility function, returns the list of available pvs versions on the sri repository that are compatible with vscode-pvs
     */
    protected async listPvsVersionsWithProgress (): Promise<{ version: string, url: string }> {
        let message: string = `Checking PVS versions from ${pvsUrl}`;
        await this.createTerminal(message);
        this.terminal.log(colorText(message, PvsColor.blue), { addNewLine: true });
        
        // request list of versions -- this is useful in future implementation, e.g., to let the user choose a version when multiple versions are available
        this.client.sendRequest(serverRequest.listVersionsWithProgress);
        const lsResponse: string = await new Promise<string> ((resolve, reject) => {
            this.client.onNotification(serverRequest.listVersionsWithProgress, async (ans: {
                req: ListVersionsWithProgressRequest, 
                res: ListVersionsWithProgressResponse
            }) => {
                if (ans?.res?.progressInfo) {
                    if (ans?.res?.stdOut) { this.terminal.log(ans?.res?.stdOut); }
                    if (ans?.res?.stdErr) { this.terminal.log(ans?.res?.stdErr); }
                } else {
                    if (ans?.res?.versions?.length) {
                        resolve (ans?.res?.versions[0]);
                        return;
                    }
                    resolve (null);
                }
            });
        });
        const versions: PvsDownloadDescriptor[] = fsUtils.parseLsPvsVersions(lsResponse);
        if (versions?.length) {
            let message: string = versions.length === 1 ?
                `${versions.length} version available: ${versions[0].fileName}`
                    : `${versions.length} versions available: ${versions.map(info => { return info.fileName; }).join(", ")}`;
            this.terminal.log(message, { addNewLine: true });

            const version: string = versions[0].version;
            const url: string = versions[0].url;
            return { version, url };
        }
        return null;
    }

    /**
     * Utility function, shows the pvs license page in vscode
     */
     protected async downloadPvsLicensePageWithProgress (): Promise<string> {
        return window.withProgress({
            location: ProgressLocation.Notification,
            cancellable: true
        }, async (progress, token) => {
            progress.report({ increment: -1, message: `Loading PVS license page...` });
            return new Promise((resolve, reject) => {
                token.onCancellationRequested(() => {
                    window.showInformationMessage("Download cancelled");
                    resolve(null);
                });
                this.client.sendRequest(serverRequest.downloadLicensePage);
                this.client.onRequest(serverEvent.downloadLicensePageResponse, (desc: { response: string }) => {
                    if (desc && desc.response && desc.response) {
                        progress.report({ increment: -1, message: `Please read the PVS license agreement before proceeding` });
                        resolve(desc.response);
                    } else {
                        progress.report({ increment: 100, message: `Error: ${sriUrl} is not responding, please try again later.` });
                        resolve(null);
                    }
                });
            });
        });
    }

    /**
     * Utility function, downloads the pvs installation file at the given url.
     * The downloaded file will be saved in desc.baseFolder
     */
    protected async downloadWithProgress (name: "PVS" | "NASALib", req: DownloadWithProgressRequest): Promise<{ fname: string }> {
        if (!fsUtils.getDownloader()) {
            window.showErrorMessage(errorMsgDownloaderNotPresent);
            return null;
        }
        if (!(req?.url && req.baseFolder)) {
            window.showErrorMessage(errorPvsVersionsNotAvailable);
            return null;
        }

        // create terminal to show feedback
        const label: string = `Downloading ${name}`;
        await this.createTerminal(label);

        // download task
        const downloadTask = (
            progress: Progress<{ increment: number, message: string }>, 
            token: CancellationToken
        ): Promise<{ fname: string }> => {
            let taskCancelled: boolean = false;

            return new Promise(async (resolve, reject) => {
                token.onCancellationRequested(() => {
                    const msg: string = "Download cancelled";
                    window.showInformationMessage(msg);
                    this.terminal.log(colorText("\n" + msg, PvsColor.yellow), { addNewLine: true });
                    // send cancellation event to the server
                    taskCancelled = true;
                    req.cancellationToken = true;
                    this.client.sendRequest(serverRequest.downloadWithProgress, req);
                    resolve(null);
                });
                
                // show feedback before starting to download
                let message: string = `Downloading ${req.url}`;
                progress.report({ increment: -1, message });
                this.terminal.log(colorText(message, PvsColor.blue), { addNewLine: true });

                // send download request to the server
                this.client.sendRequest(serverRequest.downloadWithProgress, req);
                const fname: string = await new Promise<string> ((resolveDownload, rejectDownload) => {
                    this.client.onNotification(serverRequest.downloadWithProgress, (ans: {
                        req: DownloadWithProgressRequest, 
                        res: DownloadWithProgressResponse 
                    }) => {
                        if (ans?.res?.progressInfo) {
                            this.terminal.log(ans.res?.stdOut);
                            this.terminal.log(ans.res?.stdErr);
                        } else {
                            if (!taskCancelled) {
                                if (ans?.res?.success && ans?.res?.fname) {
                                    message = `Download completed successfully (${ans.res.fname})`;
                                    progress.report({ increment: 100, message });
                                    this.terminal.log(colorText(message, PvsColor.blue), { addNewLine: true });
                                } else {
                                    message = `Error: Unable to download ${name} at ${req.url}. Please try again later or proceed with the manual installation of ${name}.`;
                                    progress.report({ increment: 100, message });    
                                    window.showErrorMessage(message);
                                    this.terminal.log(colorText(message, PvsColor.red), { addNewLine: true });                    
                                }
                            }
                            resolveDownload(ans?.res?.fname);
                        }
                    });
                });
                resolve ({ fname });
            });
        };
        // create progress dialog
        return window.withProgress({
            location: ProgressLocation.Notification,
            cancellable: true
        }, downloadTask);
    }

    /**
     * Utility function, extracts the pvs executable and sets pvs.path in vscode.
     */
	protected async installWithProgress (name: "PVS" | "NASALib", req: InstallWithProgressRequest, opt?: { quiet?: boolean }): Promise<boolean> {
        opt = opt || {};
        if (req?.shellCommand && req?.targetFolder) {
            const label: string = `Installing ${name} to ${req.targetFolder}`;
            await this.createTerminal(label);

            // send request to the server
            this.client?.sendRequest(serverRequest.installWithProgress, req);
            const res: InstallWithProgressResponse = await new Promise ((resolve, reject) => {
                this.client?.onNotification(serverRequest.installWithProgress, (desc: { req: InstallWithProgressRequest, res: InstallWithProgressResponse }) => {
                    if (desc?.res?.progressInfo) {
                        if (!opt.quiet) {
                            if (desc.res.stdOut) { this.terminal.log(desc.res.stdOut); }
                            if (desc.res.stdErr) { this.terminal.log(desc.res.stdErr); }
                        }
                    } else {
                        resolve(desc?.res);
                    }
                });
            });
            if (res?.success && name === "PVS") {
                // update pvs.path
                await workspace.getConfiguration().update("pvs.path", req.targetFolder, ConfigurationTarget.Global);
            }
            return !!res?.success;
        }
        return false;
	}

    /**
     * Utility function, returns NASALib download command
     */
    getNASALibDownloadCommand (desc: {
        downloader: NASALibDownloader,
        basePath: string,
        targetFolder: string,
        update?: boolean
    }): ShellCommand {
        let shellCommand: ShellCommand = null;
        if (desc?.downloader && desc?.targetFolder && desc?.basePath) {
            const update: boolean = fsUtils.folderExists(desc.targetFolder) && desc?.update;
            switch (desc.downloader) {
                case "git": {
                    shellCommand = fsUtils.cloneNASALibCommand(NASALibUrl, {
                        basePath: desc.basePath,
                        branch: NASALibGithubBranch, 
                        update
                    });
                    break;
                }
                case "curl":
                case "wget":
                default: {
                    const tmpdir: string = os.tmpdir();
                    const fname: string = `${tmpdir}/nasalib7.zip`;
                    shellCommand = fsUtils.getDownloadCommand(NASALibGithubFile, { out: fname });
                    break;
                }
            }
        }
        return shellCommand;
    }

    /**
     * Installation wizard for NASALib
     */
    async nasalibInstallationWizard (opt?: { msg?: string, update?: boolean }): Promise<boolean> {
        opt = opt || {};
        const label: string = opt.msg || `NASALib is an extensive PVS library developed and maintained by the NASA Langley Formal Methods Team.\n\nWould you like to download NASALib?`;
		const item = await window.showInformationMessage(label, {
            modal: true
        }, opt.update ? this.messages.updateNASALib : this.messages.downloadNASALib);//, this.messages.setNasalibPath);

        if (!item) { return false; }
        // if (item === this.messages.setNasalibPath) { return await this.selectNasalibPath(); }
        if (item === this.messages.downloadNASALib || item === this.messages.updateNASALib) {
            const downloader: NASALibDownloader = await this.getNasalibDownloader({ preferred: "git" });
            let pvsPath: string = vscodeUtils.getConfiguration("pvs.path");
            const targetFolder: string = path.join(pvsPath, "nasalib");    
            // create terminal to show feedback during download/update operations
            await this.createTerminal("Installing NASALib...", { clearScreen: true });
            const message: string = downloader === "git" && item === this.messages.downloadNASALib ?
                `Cloning NASALib to ${targetFolder}` 
                    : "git" && item === this.messages.updateNASALib ? `Updating NASALib installation`
                        : `Downloading NASALib to ${targetFolder}`;
            const shellCommand: ShellCommand = this.getNASALibDownloadCommand({
                basePath: pvsPath,
                targetFolder,
                downloader,
                update: opt?.update
            });
            if (shellCommand && message) {
                this.terminal.log(colorText(message, PvsColor.blue), { addNewLine: true });
                const tmpdir: string = os.tmpdir();
                const fname: string = `${tmpdir}/nasalib7.zip`;
                const success: boolean = await this.installWithProgress("NASALib", {
                    shellCommand,
                    targetFolder: path.join(pvsPath, "nasalib"),
                    installScript: //{
                        // cwd: targetFolder,
                        shellCommand.cmd === "git" ? `cd ${targetFolder} && ./install-scripts --pvs-dir ${pvsPath}` 
                                : `cd ${tmpdir} && unzip -o -qq ${fname} -d ${tmpdir} && mv ${path.join(tmpdir, "pvslib-master")} ${targetFolder} && cd ${targetFolder} && ./install-scripts --pvs-dir ${pvsPath}`,
                        // quiet: true
                    // },
                    cleanTarget: !opt?.update
                });
                if (success) {
                    // reboot pvs-server
                    const req: RebootPvsServerRequest = { pvsPath };
                    this.client.sendRequest(serverRequest.rebootPvsServer, req);
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Get NASALib downloader --- git | curl | wget
     */
    async getNasalibDownloader (req?: NASALibDownloaderRequest): Promise<NASALibDownloader> {
        if (!fsUtils.getDownloader()) {
            window.showErrorMessage(errorMsgDownloaderNotPresent);
            return null;
        }
        this.client.sendRequest(serverRequest.getNasalibDownloader, req);
        return await new Promise ((resolve, reject) => {
            this.client.onNotification(serverRequest.getNasalibDownloader, (ans: {
                req: NASALibDownloaderRequest,
                res: NASALibDownloaderResponse
            }) => {
                resolve(ans?.res?.downloader);
            });
        })
    }
}