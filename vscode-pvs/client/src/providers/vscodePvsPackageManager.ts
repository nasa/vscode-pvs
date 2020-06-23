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
import { window, Uri, workspace, ConfigurationTarget, Progress, CancellationToken, ProgressLocation, Terminal, ViewColumn, WebviewPanel, ExtensionContext } from "vscode";
import { serverEvent, sriUrl, serverCommand, PvsDownloadDescriptor, nasalibUrl } from "../common/serverInterface";
import * as os from 'os';
import * as path from 'path';
import { VSCodePvsStatusBar } from "../views/vscodePvsStatusBar";
import * as fsUtils from '../common/fsUtils';

export class VSCodePvsPackageManager {
    protected client: LanguageClient;
    protected statusBar: VSCodePvsStatusBar;
    /**
     * Common text messages displayed in different dialogs
     */
    readonly messages: { [ btn: string ]: string } = {
        setPvsPath: "Select location of PVS executables",
        downloadPvs: "Download PVS",
        chooseInstallationFolder: "Choose PVS Installation Folder",
        installNasalib: "Install NASALib"
    };

    /**
     * Constructor
     * @param client language client for sending messages to the server 
     * @param statusBar pvs status bar, used for displaying progress information
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
        this.client.onRequest(serverEvent.pvsNotPresent, () => {
			const pvsPath: string = workspace.getConfiguration().get("pvs.path");
            this.pvsInstallationWizard(`Could not find PVS executable in folder '${pvsPath}'\nPlease choose the correct location of the PVS executables, or download PVS.`);
        });
        this.client.onRequest(serverEvent.pvsIncorrectVersion, (msg: string) => {
            this.pvsInstallationWizard(msg);
        });
        // nasalib installation handlers
        this.client.onRequest(serverEvent.nasalibNotPresent, () => {
            this.pvsInstallationWizard(`This wizard will automatically download and install the PVS NASA Library (NASALib), an extensive library that includes 10,000+ proved theorems on core mathematical concepts.`);
        });
    }


    /**
     * Installation wizard for NASALib
     * @param msg Opening message shown by the installation wizard
     */
    async nasalibInstallationWizard (msg: string): Promise<boolean> {
        const ans: string = await window.showInformationMessage(msg, { modal: true }, this.messages.installNasalib)
        if (ans === this.messages.installNasalib) {
            const desc: { fname: string, version: string } = await this.downloadNasalibWithProgress();
            if (desc) {
                const pvsPath: string = workspace.getConfiguration().get("pvs.path");
                if (pvsPath) {
                    const targetFolder: string = path.join(pvsPath, "nasalib");
                    return await this.installNasalib({ fname: desc.fname, targetFolder: targetFolder, version: desc.version });
                }
            }
        }
        return false;
    }

    /**
     * Utility function, used by pvsInstallationWizard to install pvs in vscode, i.e., extract the executable and set pvs.path in vscode
     * @param desc 
     */
	protected async installNasalib (desc: { fname: string, targetFolder: string, version: string }): Promise<boolean> {
        const label: string = `Installing NASALib in ${desc.targetFolder}`;
        window.showInformationMessage(label);
        // extract nasalib
        const extractNasalib = async (desc: { fname: string, targetFolder: string, version: string }): Promise<void> => {
            const terminal = window.createTerminal({ name: label });
    
            const extractCommand: string = `tar -C ${desc.targetFolder} -xvf ${desc.fname} && cd ${desc.targetFolder} && ./install && exit`;
            terminal.show();
            terminal.sendText(extractCommand);
    
            return new Promise ((resolve, reject) => {
                window.onDidCloseTerminal((t) => {
                    if (t.name === label) {
                        resolve();
                    }
                });	
            });
        }
        await extractNasalib(desc);
        // set nasalib in pvs
        return new Promise((resolve, reject) => {
            this.client.sendRequest(serverCommand.importNasalib, { path: `${desc.targetFolder}` });
            this.client.onRequest(serverEvent.importNasalibResponse, () => {
                resolve(true);
            });
        });
    }

    /**
     * Utility function, downloads the list of nasalib versions from github, and returns the descriptor of the most recent version
     */
    protected async downloadNasalibWithProgress (): Promise<{ fname: string, version: string }> {
        return window.withProgress({
            location: ProgressLocation.Notification,
            cancellable: true
        }, async (progress, token) => {
            let terminal: Terminal = null;

            progress.report({ increment: -1, message: `Checking NASALib versions from ${nasalibUrl}` });

            return new Promise((resolve, reject) => {
                token.onCancellationRequested(() => {
                    window.showInformationMessage("Download cancelled");
                    if (terminal) {
                        terminal.dispose();
                        resolve(null);
                    }
                });
                
                this.client.sendRequest(serverCommand.listDownloadableVersions);
                this.client.onRequest(serverEvent.listDownloadableVersionsResponse, (desc: { response: PvsDownloadDescriptor[] }) => {
                    if (desc && desc.response && desc.response && desc.response.length > 0) {
                        progress.report({ increment: -1, message: `Downloading NASALib ${desc.response[0].version} from ${nasalibUrl}` });

                        // download PVS with curl or wget in terminal
                        const label: string = `Downloading NASALib`;
                        const fname: string = `${os.tmpdir()}/${desc.response[0].fileName}`;
                        const version: string = desc.response[0].version;
                        const downloadCommand: string = fsUtils.downloadCommand(desc.response[0].url, { out: fname }) + ` && exit`;

                        terminal = window.createTerminal({ name: label });
                        terminal.show();
                        terminal.sendText(downloadCommand);

                        window.onDidCloseTerminal((t) => {
                            if (t.name === label) {
                                resolve({ fname, version });
                            }
                        });	
                    } else {
                        progress.report({ increment: 100, message: `Error: ${nasalibUrl} is not responding, please try again later.` });
                        resolve(null);
                    }
                });
            });
        });
    }
    

    /**
     * Installation wizard for PVS
     * @param msg Opening message shown by the installation wizard
     */
    async pvsInstallationWizard (msg: string): Promise<boolean> {
		const item = await window.showWarningMessage(msg, this.messages.downloadPvs, this.messages.setPvsPath);
		if (item === this.messages.setPvsPath) {
            return this.selectPvsPath();
		} else if (item === this.messages.downloadPvs) {

            // choose installation folder
            const targetFolder: string = await this.chooseInstallationFolder();
            if (!targetFolder) {
                this.statusBar.ready();
                // operation cancelled by the user
                return; 
            }

            // show license agreement
            const panel: WebviewPanel = window.createWebviewPanel(
                'pvsLicenseAgreement', // Identifies the type of the webview. Used internally
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
            const info: string = `The PVS version you are about to download from ${sriUrl} is freely available, 
                but requires a license agreement. Please read carefully the terms of the PVS license and click "I Accept" 
                if you agree with its terms.`;
            const item = await window.showWarningMessage(info, agreement.accept, agreement.doNotAccept);

            // install pvs if the user accepts the terms of the license
            if (item === agreement.accept) {
                const desc: { fname: string, version: string } = await this.downloadPvsExecutableWithProgress();
                if (desc) {
                    if (targetFolder) {
                        const pvsPath: string = await this.installPvs({ fname: desc.fname, targetFolder: targetFolder, version: desc.version });
                        if (pvsPath) {
                            await workspace.getConfiguration().update("pvs.path", pvsPath, ConfigurationTarget.Global);
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }
    

    /**
     * Utility function used by pvsInstallationWizard to ask the user to choose the pvs installation folder
     */
	protected async chooseInstallationFolder (): Promise<string> {
        const labels: { [ btn: string ]: string } = {
            browse: this.messages.chooseInstallationFolder,
            cancel: "Cancel"
        }
        const msg: string = `Please choose PVS installation folder. A sub-folder with the PVS release will be automatically created by the installation wizard.`;
        if (this.statusBar) { this.statusBar.showProgress(msg); }
        const item = await window.showInformationMessage(msg, labels.browse, labels.cancel);
        if (item === labels.browse) {
            const pvsInstallationFolder: Uri[] = await window.showOpenDialog({
                canSelectFiles: false,
                canSelectFolders: true,
                canSelectMany: false,
                openLabel: this.messages.chooseInstallationFolder
            });
            if (pvsInstallationFolder && pvsInstallationFolder.length === 1) {
                return pvsInstallationFolder[0].fsPath;
            }
        }
		return null;
	}

    /**
     * Utility function, used by pvsInstallationWizard to install pvs in vscode, i.e., extract the executable and set pvs.path in vscode
     * @param desc 
     */
	protected async installPvs (desc: { fname: string, targetFolder: string, version: string }): Promise<string> {
        const label: string = `Installing PVS in ${desc.targetFolder}`;
        window.showInformationMessage(label);
        // extract pvs
        const extractPvs = async (desc: { fname: string, targetFolder: string, version: string }): Promise<void> => {
            const terminal = window.createTerminal({ name: label });
    
            const extractCommand: string = `tar -C ${desc.targetFolder} -xvf ${desc.fname} && cd ${desc.targetFolder}/pvs-${desc.version} && ./install-sh && exit`;
            terminal.show();
            terminal.sendText(extractCommand);
    
            return new Promise ((resolve, reject) => {
                window.onDidCloseTerminal((t) => {
                    if (t.name === label) {
                        resolve();
                    }
                });	
            });
        }
        await extractPvs(desc);
        // set pvs.path configuration
        const pvsPath: string = path.join(desc.targetFolder, `pvs-${desc.version}`);
		await workspace.getConfiguration().update("pvs.path", pvsPath, ConfigurationTarget.Global); // the updated value is visible only at the next restart, that's why we are using pvsExecutable[0].fsPath in selectPvsPath
        return pvsPath;
	}

    /**
     * Utility function, used by pvsInstallationWizard to show a download dialog
     * @param progress 
     * @param token 
     */
	protected async downloadPvsDialog (progress: Progress<{ message?: string, increment?: number }>, token: CancellationToken): Promise<boolean> {
		progress.report({ increment: 0 });
		this.client.sendRequest(serverCommand.listDownloadableVersions);
		return new Promise((resolve, reject) => {
			this.client.onRequest(serverEvent.listDownloadableVersionsResponse, async (desc: { response: { versions: string[] }}) => {
				if (desc && desc.response && desc.response.versions && desc.response.versions.length > 0) {
					progress.report({ increment: 0 });
					setTimeout(() => {
						resolve(true);
					}, 1000)
				} else {
					progress.report({ increment: 100, message: `Error: ${sriUrl} is not responding, please try later` });
					resolve(false);
				}
			});
		});
    }

    /**
     * Utility function, shows a dialog that allows the user to select the pvs installation folder in the file system
     * @param pvsPath 
     */
    protected async selectPvsPath (): Promise<boolean> {
        const pvsExecutable: Uri[] = await window.showOpenDialog({
            canSelectFiles: false,
            canSelectFolders: true,
            canSelectMany: false,
            openLabel: this.messages.setPvsPath
        });
        if (pvsExecutable && pvsExecutable.length === 1) {
            const pvsPath: string = pvsExecutable[0].fsPath;
            await workspace.getConfiguration().update("pvs.path", pvsPath, ConfigurationTarget.Global);
            return true;
        }
        return false;
    }

    /**
     * Utility function, downloads the list of pvs versions from SRI's pvs-snapshots website, and returns the descriptor of the most recent version
     */
    protected async downloadPvsExecutableWithProgress (): Promise<{ fname: string, version: string }> {
        return window.withProgress({
            location: ProgressLocation.Notification,
            cancellable: true
        }, async (progress, token) => {
            let terminal: Terminal = null;

            progress.report({ increment: -1, message: `Checking PVS versions from ${sriUrl}` });

            return new Promise((resolve, reject) => {
                token.onCancellationRequested(() => {
                    window.showInformationMessage("Download cancelled");
                    if (terminal) {
                        terminal.dispose();
                        resolve(null);
                    }
                });
                
                this.client.sendRequest(serverCommand.listDownloadableVersions);
                this.client.onRequest(serverEvent.listDownloadableVersionsResponse, (desc: { response: PvsDownloadDescriptor[] }) => {
                    if (desc && desc.response && desc.response && desc.response.length > 0) {
                        progress.report({ increment: -1, message: `Downloading PVS ${desc.response[0].version} from ${sriUrl}` });

                        // download PVS with curl or wget in terminal
                        const label: string = `Downloading PVS`;
                        const fname: string = `${os.tmpdir()}/${desc.response[0].fileName}`;
                        const version: string = desc.response[0].version;
                        const downloadCommand: string = fsUtils.downloadCommand(desc.response[0].url, { out: fname }) + ` && exit`;

                        terminal = window.createTerminal({ name: label });
                        terminal.show();
                        terminal.sendText(downloadCommand);

                        window.onDidCloseTerminal((t) => {
                            if (t.name === label) {
                                resolve({ fname, version });
                            }
                        });	
                    } else {
                        progress.report({ increment: 100, message: `Error: ${sriUrl} is not responding, please try again later.` });
                        resolve(null);
                    }
                });
            });
        });
    }
    
    /**
     * Utility function, shows the pvs license page in vscode
     */
    protected async downloadPvsLicensePageWithProgress (): Promise<string> {
        return window.withProgress({
            location: ProgressLocation.Notification,
            cancellable: true
        }, async (progress, token) => {
            let terminal: Terminal = null;

            progress.report({ increment: -1, message: `Loading PVS license page from ${sriUrl}` });

            return new Promise((resolve, reject) => {
                token.onCancellationRequested(() => {
                    window.showInformationMessage("Download cancelled");
                    if (terminal) {
                        terminal.dispose();
                        resolve(null);
                    }
                });
                
                this.client.sendRequest(serverCommand.downloadLicensePage);
                this.client.onRequest(serverEvent.downloadLicensePageResponse, (desc: { response: string }) => {
                    if (desc && desc.response && desc.response) {
                        resolve(desc.response);
                    } else {
                        progress.report({ increment: 100, message: `Error: ${sriUrl} is not responding, please try again later.` });
                        resolve(null);
                    }
                });
            });
        });
    }

}