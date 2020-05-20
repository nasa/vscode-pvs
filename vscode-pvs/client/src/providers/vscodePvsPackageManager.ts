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
import { window, Uri, workspace, ConfigurationTarget, Progress, CancellationToken, WorkspaceConfiguration, ProgressLocation, Terminal, ViewColumn, WebviewPanel } from "vscode";
import { serverEvent, www_cls_sri_com, serverCommand, PvsDownloadDescriptor } from "../common/serverInterface";
import * as os from 'os';
import * as path from 'path';
import { VSCodePvsStatusBar } from "../views/vscodePvsStatusBar";

export class VSCodePvsPackageManager {
    protected client: LanguageClient;
    protected statusBar: VSCodePvsStatusBar;

	constructor (client: LanguageClient, statusBar: VSCodePvsStatusBar) {
        this.client = client;
        this.statusBar = statusBar;
	}

    async installationWizard (msg: string): Promise<boolean> {
		const labels: { [ btn: string ]: string } = {
			setPvsPath: "Select PVS installation folder",
			downloadPvs: "Download PVS"
        };
		const item = await window.showWarningMessage(msg, labels.downloadPvs, labels.setPvsPath);
		if (item === labels.setPvsPath) {
            return this.selectPvsPath();
		} else if (item === labels.downloadPvs) {

            // choose installation folder
            const targetFolder: string = await this.chooseInstallationFolder();
            if (!targetFolder) {
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
            const info: string = `The PVS version you are about to download from ${www_cls_sri_com} is freely available, 
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
                            this.updateVscodeConfiguration(pvsPath);
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }
    
	async extractPvs (desc: { fname: string, targetFolder: string }) {
		const label: string = `Installing PVS in ${desc.targetFolder}`;
		const terminal = window.createTerminal({ name: label });

		const extractCommand: string = `tar -C ${desc.targetFolder} -xvf ${desc.fname} && cd ${desc.targetFolder} && ./install-sh && exit`;
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

	async chooseInstallationFolder (): Promise<string> {
        const labels: { [ btn: string ]: string } = {
            browse: "Choose PVS Installation Folder",
            cancel: "Cancel"
        }
        const msg: string = `Please choose the installation folder`;
        if (this.statusBar) { this.statusBar.showProgress(msg); }
        const item = await window.showInformationMessage(msg, labels.browse, labels.cancel);
        if (item === labels.browse) {
            const pvsInstallationFolder: Uri[] = await window.showOpenDialog({
                canSelectFiles: false,
                canSelectFolders: true,
                canSelectMany: false,
                openLabel: "Select PVS installation folder"
            });
            if (pvsInstallationFolder && pvsInstallationFolder.length === 1) {
                return pvsInstallationFolder[0].fsPath;
            }
        }
		return null;
	}

	async installPvs (desc: { fname: string, targetFolder: string, version: string }): Promise<string> {
        window.showInformationMessage(`Installing PVS in ${desc.targetFolder}`);
        // extract pvs
        await this.extractPvs(desc);
        const pvsPath: string = path.join(desc.targetFolder, `pvs-${desc.version}`);
		await workspace.getConfiguration().update("pvs.path", pvsPath, ConfigurationTarget.Global); // the updated value is visible only at the next restart, that's why we are using pvsExecutable[0].fsPath in selectPvsPath
        return pvsPath;
	}

	async downloadPvsDialog (progress: Progress<{ message?: string, increment?: number }>, token: CancellationToken): Promise<boolean> {
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
					progress.report({ increment: 100, message: `Error: ${www_cls_sri_com} is not responding, please try later` });
					resolve(false);
				}
			});
		});
    }

    async updateVscodeConfiguration (pvsPath: string) {
        const config: WorkspaceConfiguration = workspace.getConfiguration();
        await config.update("pvs.path", pvsPath, ConfigurationTarget.Global);
        const updatedConfig: WorkspaceConfiguration = workspace.getConfiguration();
        // const msg: string = `PVS path is ${updatedConfig.pvs.path}`;
        // window.showInformationMessage(msg);
        // pvsLanguageClient will reboot the server, see handler workspace.onDidChangeConfiguration
    }

    async selectPvsPath (): Promise<boolean> {
        const pvsExecutable: Uri[] = await window.showOpenDialog({
            canSelectFiles: false,
            canSelectFolders: true,
            canSelectMany: false,
            openLabel: "Select as PVS installation folder"
        });
        if (pvsExecutable && pvsExecutable.length === 1) {
            this.updateVscodeConfiguration(pvsExecutable[0].fsPath);
            return true;
        }
        return false;
    }

    async downloadPvsExecutableWithProgress (): Promise<{ fname: string, version: string }> {
        return window.withProgress({
            location: ProgressLocation.Notification,
            cancellable: true
        }, async (progress, token) => {
            let terminal: Terminal = null;

            progress.report({ increment: -1, message: `Checking PVS versions from ${www_cls_sri_com}` });

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
                        progress.report({ increment: -1, message: `Downloading ${desc.response[0].version}` });

                        // download PVS with curl in terminal
                        const label: string = `Downloading PVS`;
                        const fname: string = `${os.tmpdir()}/${desc.response[0].fileName}`;
                        const version: string = desc.response[0].version;
                        const downloadCommand: string = `curl -o ${fname} ${desc.response[0].url} && exit`;

                        terminal = window.createTerminal({ name: label });
                        terminal.show();
                        terminal.sendText(downloadCommand);

                        window.onDidCloseTerminal((t) => {
                            if (t.name === label) {
                                resolve({ fname, version });
                            }
                        });	
                    } else {
                        progress.report({ increment: 100, message: `Error: ${www_cls_sri_com} is not responding, please try again later.` });
                        resolve(null);
                    }
                });
            });
        });
    }
    
    async downloadPvsLicensePageWithProgress (): Promise<string> {
        return window.withProgress({
            location: ProgressLocation.Notification,
            cancellable: true
        }, async (progress, token) => {
            let terminal: Terminal = null;

            progress.report({ increment: -1, message: `Loading PVS license page from ${www_cls_sri_com}` });

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
                        progress.report({ increment: 100, message: `Error: ${www_cls_sri_com} is not responding, please try again later.` });
                        resolve(null);
                    }
                });
            });
        });
    }

}