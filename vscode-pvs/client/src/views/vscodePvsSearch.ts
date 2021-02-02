/**
 * @module vscodePvsSearch
 * @author Paolo Masci
 * @date 2021.02.01
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

import { ExtensionContext, Position, Range, Uri, ViewColumn, WebviewPanel, window } from "vscode";
import * as path from 'path';
import { LanguageClient } from "vscode-languageclient";
import * as Handlebars from "handlebars";
import { SearchRequest, SearchResponse, SearchResult, serverEvent, serverRequest } from "../common/serverInterface";

const htmlTemplate: string = `
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{{title}}</title>
    {{#if style}}
    <style type="text/css">
    {{style}}
    </style>
    {{/if}}
    {{#each css}}
    <link rel="stylesheet" href="{{this}}">
    {{/each}}
    {{#each js}}
    <script src="{{this}}"></script>
    {{/each}}
    <!-- 
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap@4.5.3/dist/css/bootstrap.min.css" 
        integrity="sha384-TX8t27EcRE3e/ihU7zmQxVncDAy5uIKz4rEkgIXeMed4M0jlfIDPvg6uqKI2xXr2" 
        crossorigin="anonymous">
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.2/css/all.min.css">
    <script src="https://cdnjs.cloudflare.com/ajax/libs/jquery/3.5.1/jquery.min.js"></script>
    -->
</head>
<script>
$(document).ready(function() {
    $(".filter-input").on("keyup", function() {
        const value = $(this).val().toLowerCase();
        $(".result-item").filter(function() {
            $(this).toggle($(this).text().toLowerCase().indexOf(value) > -1)
        });
    });
});
</script>
<body style="overflow:auto; background:whitesmoke; font-size:small; padding-top:1em;">
    <div class="container-fluid mb-1" style="position:sticky; top:0px; padding:4px; z-index:1;">
        <div class="input-group input-group-sm mb-1">
            <input type="text" class="form-control search-input" placeholder="{{#if searchString}}{{searchString}}{{else}}Search NASALib...{{/if}}" aria-label="Search NASALib" value="{{searchString}}">
            <div class="input-group-append">
                <button class="btn btn-primary search-btn" type="button"><i class="fa fa-search"></i></button>
            </div>
        </div>
        <div class="card filter" style="display:{{#if res}}block{{else}}none{{/if}};">
            <input class="form-control form-control-sm filter-input" type="text" placeholder="Filter results...">
        </div>
    </div>
    <div class="container-fluid" style="position:relative; z-index:0;">
        <div class="card">
            <div class="card-body p-0">
                <ul class="results list-group list-group-flush" style="overflow:auto;">
                    <a href="#" class="spinner list-group-item list-group-item-action" style="white-space:nowrap; display:{{#if showSpinner}}block{{else}}none{{/if}};"><span class='spinner-border spinner-border-sm' role='status' aria-hidden='true'></span> Loading...</a>
                    {{#each res}}
                    <a href="{{contextFolder}}/{{fileName}}{{fileExtension}}" {{#if line}}line="{{line}}"{{/if}} class="result-item list-group-item list-group-item-action" style="white-space:nowrap;"><b>{{fileName}}{{fileExtension}}{{#if line}} (Ln {{line}}){{/if}}</b>: {{fileContent}}</a>
                    {{/each}}
                </ul>
            </div>
        </div>
    </div>

    <script>
    // utility functions to send messages to vscode-pvs from the webview
    (function() {
        const vscode = acquireVsCodeApi();
        function search () {
            const searchString = $(".search-input")?.val()?.trim();
            if (searchString) {
                vscode.postMessage({ command: 'search', searchString });
            }
        } 
        $(".search-btn").on("click", (evt) => {
            search();
        });
        $(".search-input").on("keyup", (evt) => {
            if (evt.key === "Enter") {
                search();
            }
        });
        $(".result-item").on("click", (evt) => {
            const fname = $(evt?.currentTarget).attr("href");
            const line = $(evt?.currentTarget).attr("line");
            console.log(fname);
            vscode.postMessage({
                command: 'open-file',
                fname,
                line
            });
        });
    }());
    // Handle the message inside the webview
    window.addEventListener('message', event => {
        const message = event.data; // JSON data sent by vscode-pvs
        if (message) {
            const node = document.getElementById(message.id);
            switch (message.command) {
                case "show-spinner": {
                    $(".spinner").css("display", "block");
                    break;
                }
                default: {
                    break;
                }
            }
        }
    });
    </script>
</body>
</html>`;

import * as vscodeUtils from '../utils/vscode-utils';

export class VSCodePvsSearch {
    protected panel: WebviewPanel;

    protected client: LanguageClient;
    protected context: ExtensionContext;

    readonly title: string = "NASALib Search";
    
    constructor (client: LanguageClient) {
		this.client = client;
    }
    activate(context: ExtensionContext): void {
        this.context = context;
    }
    reveal (): void {
        if (!this.panel) {
            this.renderView();
        }
        this.panel.reveal()
    }
    hide (): void {
        this.panel.dispose();
    }

    protected createWebView () {
        if (this.panel) {
            this.panel.title = this.title;
        } else {
            this.panel = window.createWebviewPanel(
                'nasalib-search', // Identifies the type of the webview. Used internally
                this.title, // Title of the panel displayed to the user
                ViewColumn.Beside, // Editor column to show the new webview panel in.
                {
                    enableScripts: true
                }
            );
            // Clean up data structures when webview is disposed
            this.panel.onDidDispose(
                () => {
                    this.panel = null;
                },
                null,
                this.context.subscriptions
            );
            // Handle messages from the webview
            this.panel.webview.onDidReceiveMessage(
                async message => {
                    switch (message.command) {
                        case 'search': {
                            if (message.searchString) {
                                this.showSearching({ searchString: message.searchString });
                                const res: SearchResult[] = await this.search(message.searchString);
                                this.showResults({ res, searchString: message.searchString });
                            }
                            break;
                        }
                        case "open-file": {
                            if (message.fname) {
                                const selection: Range = new Range(
                                    new Position(+message.line - 1, 0), // line number and column number in vscode start from 0
                                    new Position(+message.line, 0)
                                );
                                vscodeUtils.previewPvsFile(message.fname, { selection });
                            }
                            break;
                        }
                        default: {
                            break;
                        }
                    }
                },
                undefined,
                this.context.subscriptions
            );
        }
    }
    showSearching (data: { searchString: string }): void {
        this.refreshView({ ...data, showSpinner: true });
    }
    showResults (data: { res: SearchResult[], searchString: string }): void {
        this.refreshView(data);
        // this.panel?.webview?.postMessage({
        //     command: "show-results",
        //     res
        // });
    }
    async search (searchString: string): Promise<SearchResult[]> {
        return new Promise ((resolve, reject) => {
            const req: SearchRequest = {
                searchString
            };
            this.client.sendRequest(serverRequest.search, req);
            this.client.onNotification(serverEvent.searchResponse, (res: SearchResponse) => {
                return resolve(res?.ans);
            });
        });
    }
    /**
     * creates the entire html content
     * @param root 
     */
    protected createContent (data?: { res?: SearchResult[], searchString?: string, showSpinner?: boolean }): void {
        // set webview content                
        const bootstrapJsOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/bootstrap/dist/js/bootstrap.bundle.min.js'));
        const bootstrapCssOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/bootstrap/dist/css/bootstrap.min.css'));
        const jqueryOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/jquery/dist/jquery.min.js'));
        const fontawesomeCssOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/font-awesome/css/font-awesome.min.css'));

        const css = [
            this.panel.webview.asWebviewUri(bootstrapCssOnDisk),
            this.panel.webview.asWebviewUri(fontawesomeCssOnDisk)
        ];
        const js = [
            this.panel.webview.asWebviewUri(jqueryOnDisk), // jquery needs to be loaded before bootstrap
            this.panel.webview.asWebviewUri(bootstrapJsOnDisk)
        ];
        this.panel.webview.html = this.createHtmlContent(data, { css, js });
    }
    /**
     * Renders the content of the webview
     * @param root 
     * @param opt
     */
    renderView (): void {
        this.refreshView();
    }
    /**
     * Refreshed the content of the webview
     * @param opt
     */
    refreshView (data?: { res?: SearchResult[], searchString?: string, showSpinner?: boolean }): void {
        // create webview
        this.createWebView();
        // create webview content
        this.createContent(data);
    }
    /**
     * Creates the html rendered in the webview
     * @param root Proof tree
     * @param opt Options
     * <li>css: css style files;</li>
     * <li>js: js files;</li>
     * <li>style: inline css style</li>
     */
    protected createHtmlContent (data: { res?: SearchResult[], searchString?: string, showSpinner?: boolean }, opt?: { css?: Uri[], js?: Uri[], style?: string }): string {
        const html: string = Handlebars.compile(htmlTemplate, { noEscape: true })({
            title: this.title,
            ...data,
            ...opt
        });
        return html;
    }
}