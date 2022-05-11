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
import { FileDescriptor, serverRequest } from "../common/serverInterface";
import * as fsUtils from '../common/fsUtils';
import * as path from 'path';
import * as Handlebars from "handlebars";
import * as vscodeUtils from '../utils/vscode-utils';
import { Terminal } from "xterm";

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
                    this.client?.sendRequest(serverRequest.openFileWithExternalApp, desc);
                    this.client?.onNotification(serverRequest.openFileWithExternalApp, (ans: { res: string } ) => {
                        console.log("[vscodePvsFileViewer] onNotification(serverRequest.openFileWithExternalApp)", ans);
                    });
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

}