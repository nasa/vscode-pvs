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
    ProgressLocation, Terminal, ViewColumn, WebviewPanel, ExtensionContext, env,
    MessageItem
} from "vscode";
import {
    serverEvent, sriUrl, serverRequest, PvsDownloadDescriptor, NASALibUrl, 
    pvsUrl, DownloadWithProgressRequest, DownloadWithProgressResponse, 
    InstallWithProgressRequest, InstallWithProgressResponse, 
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
import { homedir } from "os";

export const errorMsgDownloaderNotPresent: string = `Error: Unable to download the file ('curl' or 'wget' not available). Please install 'curl' or 'wget', as vscode-pvs is unable to perform downloads without them.`; 
export const errorPvsVersionsNotAvailable: string = `Error: Unable to retrieve information on the PVS versions. Please try again later or proceed with manual installation of PVS.`; 

export class VSCodePvsPackageManager {
    // client for sending messages to the server
    protected client: LanguageClient;
	protected context: ExtensionContext;

    // status bar, used for brief feedback messages
    protected statusBar: VSCodePvsStatusBar;

    // terminal window, used to report feedback during the installation process
    protected terminalWindow: Terminal;
    protected terminal: VSCodePvsEchoTerminal;

    // flag indicating this is the first time the package manager is executed
    protected firstRun: boolean = true;

	// default pvs folder/version
	protected DEFAULT_PVS_VERSION: string = "8.0";
	protected DEFAULT_PVS_FOLDER: string = `pvs-${this.DEFAULT_PVS_VERSION}`;
	protected DEFAULT_INSTALLATION_FOLDER: string = `~/PVS`;
	protected DEFAULT_NASALIB_VERSION: string = "8.0";

    /**
     * Common text messages displayed in different dialogs
     */
    readonly messages = {
        setPvsPath: "Select location of PVS executables",
        downloadPvs: "Default Installation",
        updatePvs: "Update PVS",
		seeAdvancedOptions: "Advanced Options...",
        chooseInstallationFolder: "Choose PVS 8.0 installation folder",
        selectInstallationFolder: "Use as installation folder",
        downloadNASALib: "Download NASALib",
        setNASALibPath: "Select location of NASALib",
        updateNASALib: "Update NASALib",
        completeInstallationLater: "Complete installation later",
        useExternalServer: "Use external server"
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
		this.context = context;
        // pvs installation handlers
        this.client.onRequest(serverEvent.pvsNotFound, async () => {
            this.statusBar.clear();
            if (this.firstRun) {
                // show the wizard only the first time vscode is opened, to avoid annoying the user with messages if they don't want to install pvs right away during the current session
                this.firstRun = false;
                const pvsReady: boolean = await this.pvsInstallationWizard({
                    msg: `VSCode-PVS is almost ready!\n Select '${this.messages.downloadPvs}' to install PVS 8.0 to ${this.DEFAULT_INSTALLATION_FOLDER} or '${this.messages.seeAdvancedOptions}' to access advanced options.`,
					preferDefaultFolder: true,
					force: true
                });
                this.statusBar.clear();
                if (!pvsReady) this.statusBar.showInstallPvsButton();
            }
        });
        this.client.onRequest(serverEvent.pvsIncorrectVersion, async (msg: string) => {
            this.statusBar.clear();
            if (this.firstRun) {
                this.firstRun = false;
                // show the wizard only the first time vscode is opened, to avoid annoying the user with messages if they don't want to install pvs right away during the current session
                await this.pvsInstallationWizard({ msg });
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
            const req = { 
                pvsPath: pvsPath, 
                externalServer: vscodeUtils.getConfigurationFlag("pvs.externalServer"),
                webSocketPort: vscodeUtils.getConfigurationValue("pvs.initialPortNumber"),
                remote: vscodeUtils.getRemoteDetail(this.context)
             };
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
     * Installation wizard for PVS -- this is the main method for the installation of PVS
     */
    async pvsInstallationWizard (opt?: { msg?: string, update?: boolean, preferDefaultFolder?: boolean, force?: boolean }): Promise<boolean> {
        opt = opt || {};
        let label: string = opt.msg || "PVS Installation Wizard";
        let detail: string = undefined;

        if(label.search('\n') > -1){
            const splitteMsg = label.split('\n');
            label = splitteMsg[0];
            detail = splitteMsg.slice(1,splitteMsg.length).join('\n');
        }

        let askAgain: boolean = false;
        let item : MessageItem = undefined;
        let userDefinedInstallationFolder: string = undefined;

        do {
            askAgain = false;

            item = await window.showInformationMessage(label, { detail: detail, 
                modal: true }, 
                { isCloseAffordance: true, title: this.messages.completeInstallationLater },
                { isCloseAffordance: false, title: opt.update ? this.messages.updatePvs : this.messages.downloadPvs }, 
                { isCloseAffordance: false, title: this.messages.seeAdvancedOptions }
            );

            if (!item || item.title === this.messages.completeInstallationLater) { return false; }

            if (item.title === this.messages.seeAdvancedOptions) { 
                item = await window.showInformationMessage(label, { detail: `Available actions:\n\n ▻ '${this.messages.chooseInstallationFolder}' allows to select the folder where PVS 8.0 will be installed\n ▻ If PVS 8.0 is already installed in your system, use '${this.messages.setPvsPath}' to inform its folder\n ▻ You can also use '${this.messages.useExternalServer}' to prevent VSCode-PVS from starting the PVS backend.`, 
                    modal: true }, 
                    { isCloseAffordance: true, title: "Basic Options..." },
                    { isCloseAffordance: false, title: this.messages.chooseInstallationFolder },
                    { isCloseAffordance: false, title: this.messages.setPvsPath },
                    { isCloseAffordance: false, title: this.messages.useExternalServer }
                );
    
                if (item.title === this.messages.setPvsPath) { 
                    askAgain = ! await this.selectPvsPath(); // it restarts the server on success
                } else if (item.title === "Basic Options...") { 
                    askAgain = true; // it restarts the server on success
                } else 	if (item.title === this.messages.chooseInstallationFolder) { 
                    const pvsInstallationFolder: Uri[] = await window.showOpenDialog({
                        canSelectFiles: false,
                        canSelectFolders: true,
                        canSelectMany: false,
                        openLabel: this.messages.selectInstallationFolder
                    });
                    if (pvsInstallationFolder && pvsInstallationFolder.length === 1) {
                        userDefinedInstallationFolder = pvsInstallationFolder[0].fsPath;
                    } else
                        askAgain = true;
                }        
            }
    
        } while(askAgain);

        if (item.title === this.messages.useExternalServer) {
            const webSocketPort: number = vscodeUtils.getConfigurationValue("pvs.initialPortNumber");

            await window.showInformationMessage("External Server Mode Activated", { detail: `External Server mode was activated, VSCode-PVS will try to connect to the port ${webSocketPort}. You can choose a different port by editing the corresponding extention setting.`, 
                modal: true }, 
                { isCloseAffordance: true, title: "OK" });

            await workspace.getConfiguration().update("pvs.externalServer", true, ConfigurationTarget.Global);

            const req = { 
                externalServer: vscodeUtils.getConfigurationFlag("pvs.externalServer"),
                webSocketPort: webSocketPort,
                remote: vscodeUtils.getRemoteDetail(this.context)
             };
            this.client.sendRequest(serverRequest.rebootPvsServer, req);
        }

		if (item.title === this.messages.chooseInstallationFolder && userDefinedInstallationFolder) {
            item.title = this.messages.downloadPvs;
            opt.update = true;
		}
        
        if (item.title === this.messages.downloadPvs || item.title === this.messages.updatePvs) {
            // create terminal to show feedback during download/update operations
            await this.createTerminal(label, { clearScreen: true });
			const defaultInstallationFolder: string = path.join(fsUtils.tildeExpansion(this.DEFAULT_INSTALLATION_FOLDER), this.DEFAULT_PVS_FOLDER);
            let pvsPath: string = 
				(opt?.preferDefaultFolder || item.title === this.messages.downloadPvs) ? 
					defaultInstallationFolder 
						: vscodeUtils.getConfiguration("pvs.path");
            let baseFolder: string = fsUtils.tildeExpansion(userDefinedInstallationFolder) || fsUtils.getContextFolder(pvsPath) || defaultInstallationFolder;

			const info: { version: string, url: string } = { version: "8.0", url: "https://github.com/SRI-CSL/PVS" }; //await this.listPvsVersionsWithProgress();
			if (info?.version && info?.url) {
				// download and install pvs
				// const desc: { fname: string } = await this.downloadWithProgress("PVS", {
				//     url: info.url,
				//     baseFolder
				// });
				// if (desc?.fname) {
					// update pvsPath with the current version info
					pvsPath = path.join(baseFolder, `pvs-${info.version}`);
					let pvsExecutable: string = path.join(pvsPath, "pvs");
					if (!fsUtils.fileExists(pvsExecutable) || opt?.update || opt?.force) {
						this.terminal.log(colorText(`Installing PVS to ${pvsPath}`, PvsColor.blue), { addNewLine: true });
						const makefileOnDisk: string = path.join(this.context.extensionPath, 'extra/Makefile-PVS8');
						const shellCommand: ShellCommand = {
							cmd: "make",
							args: [
                                'install-pvs',
								`-f ${makefileOnDisk} INSTALLATION_PATH=${baseFolder}`
							]
						};
						const success: boolean = await this.installWithProgress("PVS", {
							shellCommand,
							targetFolder: pvsPath,
							saveAndRestore: path.join(pvsPath, "nasalib"),
							// installScript: //{
							//     // cwd: pvsPath,
							//     `cd ${pvsPath} && ./install-sh`,
							//     // quiet: true
							// // },
							cleanTarget: true
						});
						if (success) {            
							// show release info
							vscodeUtils.showReleaseNotes();
							return true;
						}
					}
				// }
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

            if (pvsPath !== null && pvsPath !== undefined && pvsPath?.trim() !== "" && fsUtils.fileExists(path.join(pvsPath, "pvs"))) {
                await workspace.getConfiguration().update("pvs.path", pvsPath, ConfigurationTarget.Global);
                // pvs-server will be automatically rebooted when pvs.path is updated, see workspace.onDidChangeConfiguration in pvsLanguageClient.ts
                return true;
            } else {
                window.showErrorMessage(`No executable found at ${pvsPath}. Please double check PVS is already installed in that folder.`, { modal: true });
            }
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
        if (env.remoteName==="dev-container"){
            msg = `Please choose PVS installation folder.\nA subfolder with the PVS executables will be automatically created under the selected folder. If you are installing the extension in a dev container , select a folder inside home directory of the container.`
        }
        const item = await window.showInformationMessage(msg, { modal: true }, labels.browse);//, labels.cancel);
        if (item === labels.browse) {
            const pvsInstallationFolder: Uri[] = await window.showOpenDialog({
                canSelectFiles: false,
                canSelectFolders: true,
                canSelectMany: false,
                openLabel: this.messages.selectInstallationFolder,
                defaultUri: env.remoteName==="dev-container" ? Uri.file(homedir()) : undefined
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

			// update status bar
			this.statusBar.showProgress(label, label);

            let errorMessages: string = `${name} installation failed: `;

            // send request to the server
            this.client?.sendRequest(serverRequest.installWithProgress, req);
            const res: InstallWithProgressResponse = await new Promise ((resolve, reject) => {
                this.client?.onNotification(serverRequest.installWithProgress, (desc: { req: InstallWithProgressRequest, res: InstallWithProgressResponse }) => {
                    if (desc?.res?.progressInfo) {
                        if (!opt.quiet) {
                            if (desc.res.stdOut) { 
                                this.terminal.log(desc.res.stdOut);
                                const errorMessage: RegExpMatchArray = /ERROR (.*)\*\*/gi.exec(desc.res.stdOut);
                                if (errorMessage && errorMessage.length > 1) {
                                    errorMessages += errorMessage[1];
                                }
                            }
                            if (desc.res.stdErr) { 
                                this.terminal.log(desc.res.stdErr); 
                                const errorMessage: RegExpMatchArray = /ERROR (.*)\*\*/gi.exec(desc.res.stdErr);
                                if (errorMessage && errorMessage.length > 1) {
                                    errorMessages += errorMessage[1];
                                }
                            }
                        }
                    } else {
                        resolve(desc?.res);
                    }
                });
            });
            if (!res?.success) {
                window.showErrorMessage(errorMessages);
            } else if (req?.targetFolder) {
				if (name === "PVS") {
					// update pvs.path
					await workspace.getConfiguration().update("pvs.path", req.targetFolder, ConfigurationTarget.Global);
				} else if (name === "NASALib") {
					// update pvs.pvsLibraryPath
					await vscodeUtils.addPvsLibraryFolder(req.targetFolder);
				}
            } 

			// update status bar
			this.statusBar.ready();

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
        let nasalibReady: boolean = false;
        if(!nasalibReady) {
            opt = opt || {};
            const label: string = opt.msg || `NASALib v8.0 could not be found.\n\nNASALib is an extensive PVS library developed and maintained by the NASA Langley Formal Methods Team.\n\nWould you like to download NASALib?`;
            const item = await window.showInformationMessage(label, {
                modal: true
            }, opt.update ? this.messages.updateNASALib : this.messages.downloadNASALib, this.messages.setNASALibPath);
    
            if (!item) { return false; }

            let pvsPath: string = vscodeUtils.getConfiguration("pvs.path");
            let nasalibPath: string = '';
            if (item === this.messages.downloadNASALib || item === this.messages.updateNASALib) {
				pvsPath = fsUtils.tildeExpansion(pvsPath);
                const downloader: NASALibDownloader = await this.getNasalibDownloader({ preferred: "git" });
                nasalibPath = path.join(pvsPath, "nasalib");    
                // create terminal to show feedback during download/update operations
                await this.createTerminal("Installing NASALib...", { clearScreen: true });
                const message: string = downloader === "git" && item === this.messages.downloadNASALib ?
                    `Cloning NASALib to ${nasalibPath}` 
                        : downloader === "git" && item === this.messages.updateNASALib ? `Updating NASALib installation`
                            : `Downloading NASALib to ${nasalibPath}`;
                const shellCommand: ShellCommand = this.getNASALibDownloadCommand({
                    basePath: pvsPath,
                    targetFolder: nasalibPath,
                    downloader,
                    update: opt?.update
                });
                if (shellCommand && message) {
                    this.terminal.log(colorText(message, PvsColor.blue), { addNewLine: true });
                    const tmpdir: string = os.tmpdir();
                    const fname: string = `${tmpdir}/nasalib${this.DEFAULT_NASALIB_VERSION}.zip`;
                    const nasalibFolderName: string = "nasalib";
					const targetFolder: string = path.join(pvsPath, nasalibFolderName);

					if (shellCommand.cmd === "git") {
						const makefileOnDisk: string = path.join(this.context.extensionPath, 'extra/Makefile-PVS8');
						const shellCommand: ShellCommand = {
							cmd: "make",
							args: [
								'install-nasalib',
                                `-f ${makefileOnDisk}`,
                                `PVS_PATH=${pvsPath}`,
                                `NASALIB_FOLDEr=${nasalibFolderName}`,
                                `NASALIB_PATH=${targetFolder}`
							]
						};
						nasalibReady = await this.installWithProgress("NASALib", {
							shellCommand,
							targetFolder,
							cleanTarget: true//,
							// env: { "PVS_LIBRARY_PATH": `${targetFolder}`}
						});
					} else {
						nasalibReady = await this.installWithProgress("NASALib", {
							shellCommand,
							targetFolder,
							installScript: 
								`cd ${tmpdir} && unzip -o -qq ${fname} -d ${tmpdir} && mv ${path.join(tmpdir, "pvslib-master")} ${nasalibPath} && cd ${nasalibPath} && ./install-scripts --pvs-dir ${pvsPath}`,
							cleanTarget: !opt?.update
						});
					}
					// add nasalib to pvsLibraryPath in vscode settings
                    if (nasalibReady)
					    nasalibReady = await vscodeUtils.addPvsLibraryFolder(fsUtils.tildeExpansion(nasalibPath));
                    // TODO Add error handling @M3 what happens if the library couldn't be added to vscode settings?
                }
            } else if (item === this.messages.setNASALibPath) { 
                const uris: Uri[] = await window.showOpenDialog({
                    canSelectFiles: false,
                    canSelectFolders: true,
                    canSelectMany: false,
                    openLabel: this.messages.setNASALibPath
                });
                if (uris?.length === 1) {
                    nasalibPath = uris[0].fsPath;
                    if (nasalibPath !== null && nasalibPath !== undefined && nasalibPath?.trim() !== ""){
                        nasalibReady = await vscodeUtils.addPvsLibraryFolder(nasalibPath);
                    }
                }
            }
            return nasalibReady;
        }
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