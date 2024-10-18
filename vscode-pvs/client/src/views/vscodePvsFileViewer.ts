/**
 * @module vscodeFileViewer
 * @author Paolo Masci
 * @date 2022.05.10
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

import { commands, ExtensionContext, Uri, ViewColumn, WebviewPanel, window } from "vscode";
import { LanguageClient } from "vscode-languageclient";
import { FileDescriptor, PvsDocKind, PvsDocRequest, PvsDocResponse, PvsTheory, serverRequest } from "../common/serverInterface";
import * as fsUtils from '../common/fsUtils';
import * as path from 'path';
import * as Handlebars from "handlebars";
import * as vscodeUtils from '../utils/vscode-utils';
import { WebViewStyles } from "./vscodePvsSearch";

const htmlTemplate: string = `
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{{title}}</title>
    {{#if style}}
    <style type="text/css">{{style}}</style>
    {{/if}}
    {{#each css}}
    <link rel="stylesheet" href="{{this}}">
    {{/each}}
    <style>
    .card-header {
        max-height: 52px !important;
        overflow: auto;
        font-size: small;
    }
    </style>
    {{#each js}}
    <script src="{{this}}"></script>
    {{/each}}
</head>
<body style="margin:0; padding:0;">
    <div class="card">
    <div class="card-header">
        {{title}}
    </div>
    <div class="card-body">
        {{#if pdfFile}}
        <a href="{{pdfFile}}">Open {{pdfFile}}</a>
        {{/if}}

        {{#if svgFile}}
        {{svgFile}}
        {{/if}}

        {{#if imageFile}}
        <img src="{{imageFile}}" alt="Alternative text">
        {{/if}}

        {{#if otherFile}}
        <code>
        {{otherFile}}
        </code>
        {{/if}}
    </div>
    {{#if footer}}
    <footer class="blockquote-footer">{{footer}}</footer>
    {{/if}}
</body>
</html>`;
 
// simple webview for viewing pdf and image files
export class VSCodePvsFileViewer {
    protected client: LanguageClient;
    protected context: ExtensionContext;
    protected panel: WebviewPanel;
    
    /**
     * Constructor
     */
    constructor (client: LanguageClient) {
		this.client = client;
    }
    /**
     * Activates the module
     */
    activate (context: ExtensionContext) {
        this.context = context;
    }
    /**
     * preview file
     */
    async open (desc: FileDescriptor): Promise<boolean> {
        return new Promise(async (resolve, reject) => {
            if (desc?.fileName && desc?.contextFolder) {
                if (fsUtils.isMarkdownFile(desc)) {
                    this.openAsMarkdownPreview(desc);
                } else if (fsUtils.isPvsFile(desc)) {
                    vscodeUtils.openPvsFile(desc);
                } else if (fsUtils.isAdobePdfFile(desc)) {
                    this.openWithExternalApp(desc); // async call
                } else {
                    // create webview
                    this.panel = window.createWebviewPanel(
                        'vscode-pvs.quick-open', // Identifies the type of the webview. Used internally
                        `${desc?.fileName}${desc?.fileExtension}`, // Title of the panel displayed to the user
                        ViewColumn.Beside, // Editor column to show the new webview panel in.
                        {
                            enableScripts: true,
                            retainContextWhenHidden: true
                        }
                    );
                    // set panel icon
                    this.panel.iconPath = {
                        light: Uri.file(path.join(__dirname, "..", "..", "..", "icons", "pvs-file-icon.png")),
                        dark: Uri.file(path.join(__dirname, "..", "..", "..", "icons", "pvs-file-icon.png"))
                    };
                    // set webview content
                    this.panel.webview.html = await this.createContent(desc);
                }
                resolve(true);
            } else {
                resolve(false);
            }
        });
    }
    /**
     * Open a file as markdown preview
     */
    async openAsMarkdownPreview (desc: FileDescriptor): Promise<boolean> {
        if (desc?.fileName) {
            const fname: string = fsUtils.desc2fname(desc);
            const fileOnDisk: Uri = Uri.file(fname);
            commands.executeCommand("markdown.showPreviewToSide", fileOnDisk);
            return true;
        }
        return false;
    }
    /**
     * Internal function, creates the html content of the webview
     */
    protected async createContent (desc: FileDescriptor): Promise<string> {
        const title: string = `Viewing <b>${desc?.fileName}${desc?.fileExtension}</b>`;
        // const bootstrapJsOnDisk: vscode.Uri = vscode.Uri.file(path.join(this.context.extensionPath, 'client/node_modules/bootstrap/dist/js/bootstrap.bundle.min.js'));
        const bootstrapCssOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/bootstrap/dist/css/bootstrap.min.css'));
        const fname: string = fsUtils.desc2fname(desc);
        const fileOnDisk: Uri = Uri.file(fname);
        const fileUri: Uri = this.panel.webview.asWebviewUri(fileOnDisk);

        const pdfFile: Uri = fsUtils.isAdobePdfFile(desc) ? fileUri : null;
        const svgFile: string = fsUtils.isSvgFile(desc) ? await fsUtils.readFile(fname) : null;
        const imageFile: Uri = fsUtils.isImageFile(desc) ? fileUri : null;
        const otherFile: string = !pdfFile && !svgFile && !imageFile ?
            await fsUtils.readFile(fname) 
                : null;
        const html: string = Handlebars.compile(htmlTemplate, { noEscape: true })({
            title,
            js: [
                // this.panel?.webview?.asWebviewUri(plotlyOnDisk)
            ],
            css: [
                this.panel?.webview?.asWebviewUri(bootstrapCssOnDisk)
            ],
            svgFile,
            pdfFile,
            imageFile,
            otherFile: otherFile?.replace(/\n/g, "<br>\n"),
            footer: otherFile ? `${otherFile.split("\n")?.length} lines` : undefined
        });
        return html;
    }
    /**
     * Utility function, documents a pvs theory (interactive version)
     */
    async pvsDocInteractive (theory: PvsTheory): Promise<boolean> {
        const kind: PvsDocKind = await vscodeUtils.queryDocumentTheory(theory);
        if (kind) {
            let success: boolean = true;
            switch (kind) {
                case PvsDocKind.embedded: {
                    success = await this.pvs2tags(theory);
                    break;
                }
                case PvsDocKind.html: {
                    success = await this.pvs2html(theory, { interactive: true });
                    break;
                }
                case PvsDocKind.latex: {
                    success = await this.pvs2latex(theory, { interactive: true });
                    break;
                }
                default: {
                    // do nothing
                }
            }
            return success;
        }
        return false;
    }
    /**
     * Utility function, opens a document with an external app
     */
    async openWithExternalApp (fdesc: FileDescriptor, opt?: { message?: string }): Promise<boolean> {
        const message: string = opt?.message || `Opening with default system app`;
        vscodeUtils.showInformationMessage(message);
        this.client?.sendRequest(serverRequest.openFileWithExternalApp, fdesc);
        const success: boolean = await new Promise((resolve, reject) => {
            this.client?.onNotification(serverRequest.openFileWithExternalApp, (ans: { res: string } ) => {
                console.log(`[${fsUtils.generateTimestamp()}] `+"[vscodePvsFileViewer] onNotification(serverRequest.openFileWithExternalApp)", ans);
                resolve(true);
            });
        });
        return success;
    }
    /**
     * Utility function, asks the user if they want to open a given file with an external app
     */
    async openWithExternalAppDialog (fdesc: FileDescriptor, opt?: { query?: string, opening?: string }): Promise<boolean> {
        const file: string = fsUtils.getFileName(fsUtils.desc2fname(fdesc), { keepExtension: true })
        const message: string = opt?.query || `Open ${file}?`;
        const ans: vscodeUtils.YesCancel = await vscodeUtils.showYesCancelDialog(message);
        if (ans === "yes") {
            return await this.openWithExternalApp(fdesc);
        }
        return false;
    }
    /**
     * Utility function, asks the user if they want to open a given file in vscode
     */
    async openWithVscode (fdesc: FileDescriptor, opt?: { message?: string }): Promise<boolean> {
        const file: string = fsUtils.getFileName(fsUtils.desc2fname(fdesc), { keepExtension: true })
        const message: string = opt?.message || `Open ${file}?`;
        const ans: vscodeUtils.YesCancel = await vscodeUtils.showYesCancelDialog(message);
        if (ans === "yes") {
            return await vscodeUtils.openFile(fsUtils.desc2fname(fdesc));
        }
        return false;
    }
    /**
     * Utility function, documents a pvs theory in html format
     */
    async pvs2html (theory: PvsTheory, opt?: { interactive?: boolean }): Promise<boolean> {
        if (this.client) {
            const docEngine: string = path.resolve(this.context.extensionPath, "server", "node_modules", "jsdoc", "jsdoc.js");
            const docEngineLibs: string = path.resolve(this.context.extensionPath, "extra", "pvs-doc", "pvs2html", "lib");
            const req: PvsDocRequest = { theory, docEngine, docEngineLibs, docKind: PvsDocKind.html };
            this.client.sendRequest(serverRequest.pvsDoc, req);
            const success: boolean = await new Promise ((resolve, reject) => {
                this.client.onNotification(serverRequest.pvsDoc, async (desc: PvsDocResponse) => {
                    if (desc?.res?.mainFile && desc?.res?.outputFolder) {
                        const fdesc: FileDescriptor = {
                            contextFolder: desc.res.outputFolder,
                            fileName: fsUtils.getFileName(desc.res.mainFile), 
                            fileExtension: fsUtils.getFileExtension(desc.res.mainFile)
                        };
                        if (opt?.interactive) {
                            // async call
                            this.openWithExternalAppDialog(fdesc, {
                                query: `HTML files generated successfully!\nWould you like to view the files?`,
                                opening: `Opening HTML files in the browser...`
                            });
                        }
                        return resolve(true);
                    }
                    return resolve(false);
                });
            });
            return success;
        }
        // else
        console.warn(`[vscode-pvs-file-viewer] Warning: unable to send request to the server (client is null)`);
        return false;
    }
    /**
     * Utility function, documents a pvs theory in latex format
     */
    async pvs2latex (theory: PvsTheory, opt?: { interactive?: boolean }): Promise<boolean> {
        if (this.client) {
            const req: PvsDocRequest = { theory, docKind: PvsDocKind.latex };
            this.client.sendRequest(serverRequest.pvsDoc, req);
            const success: boolean = await new Promise ((resolve, reject) => {
                this.client.onNotification(serverRequest.pvsDoc, async (desc: PvsDocResponse) => {
                    if (desc?.res?.mainFile && desc?.res?.outputFolder) {
                        const fdesc: FileDescriptor = {
                            contextFolder: desc.res.outputFolder,
                            fileName: fsUtils.getFileName(desc.res.mainFile), 
                            fileExtension: fsUtils.getFileExtension(desc.res.mainFile)
                        };
                        if (opt?.interactive) {
                            // async call
                            this.openWithVscode(fdesc, {
                                message: `LaTex files generated successfully!\nWould you like to view the files?`
                            });
                        }
                        return resolve(true);
                    }
                    return resolve(false);
                });
            });
            return success;
        }
        // else
        console.warn(`[vscode-pvs-file-viewer] Warning: unable to send request to the server (client is null)`);
        return false;
    }
    /**
     * Utility function, adds the theory/author tags to the pvs file
     */
    async pvs2tags (theory: PvsTheory, opt?: { interactive?: boolean }): Promise<boolean> {
        const success: boolean = await vscodeUtils.embedTheoryAuthorTags(theory);
        const msg: string = success ? `Theory tags added successfully!` : `Theory tags already present, nothing to do`;
        vscodeUtils.showInformationMessage(msg);
        return success;
    }

}