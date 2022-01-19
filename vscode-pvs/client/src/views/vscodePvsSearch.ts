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

import { 
    ExtensionContext, Position, Range, Uri, ViewColumn, WebviewPanel, window 
} from "vscode";
import { 
    LookUpTableStats,
    PvsTheory,
    SearchRequest, SearchResponse, SearchResult, serverEvent, serverRequest 
} from "../common/serverInterface";
import * as path from 'path';
import { LanguageClient } from "vscode-languageclient";
import * as Handlebars from "handlebars";

/**
 * Handlebars template for the html content of the view
 */
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
    $(".search-input").on("input", function() {
        const value = $(this).val();
        $(".clear-btn").css("display", value ? "block" : "none");
        if (value === "") {
            $("#welcome-screen").css("display", "block");
        }
    });
    $(".filter-results").on("keyup", function() {
        const value = $(this).val().toLowerCase();
        $(".result-item").filter(function() {
            $(this).toggle($(this).text().toLowerCase().indexOf(value) > -1)
        });
    });
    // init interactive tree
    const toggler = document.getElementsByClassName("caret");
    if (toggler && toggler.length) {
        for (let i = 0; i < toggler.length; i++) {
            toggler[i].addEventListener("click", function() {
                this.parentElement.querySelector(".nested").classList.toggle("active");
                this.classList.toggle("caret-down");
            });
        }
    }
});
</script>
<style>
.clear-btn {
    opacity: 0.6;
}
.bg-white {
    background: white;
}
</style>
<body style="overflow:auto; background:whitesmoke; font-size:small;" class="p-1">
    <div class="container-fluid mb-1" style="position:sticky; top:0px; padding:4px; z-index:1;">
        <div class="input-group input-group-sm mb-1 bg-white">
            <div class="input-group-prepend">
                <span class="input-group-text"><i class="fa fa-search" aria-hidden="true"></i></span>
            </div>
            <input type="text" class="form-control search-input" placeholder="Search NASALib..." aria-label="Search NASALib" value="{{searchString}}">
            <div class="input-group-append">
                <button class="btn btn-outline-secondary btn-sm clear-btn" type="button" style="display:{{#if res}}block{{else}}none{{/if}}"><i class="fa fa-times" aria-hidden="true"></i></button>
                <button class="btn btn-secondary btn-sm search-btn" type="button">Search</button>
            </div>
        </div>
        <div class="filter" style="display:{{#if res}}block{{else}}none{{/if}};">
            <div class="input-group input-group-sm mb-1">
                <div class="input-group-prepend">
                    <span class="input-group-text"><i class="fa fa-filter" aria-hidden="true"></i></span>
                </div>
                <input class="form-control form-control-sm filter-results" type="text" placeholder="Filter results...">
            </div>
        </div>
    </div>
    <div class="container-fluid px-1" style="position:relative; z-index:0;">
        <div class="card">
            <div class="card-body p-0">
                <ul class="results list-group list-group-flush" style="overflow:auto;">
                    <a href="#" class="spinner list-group-item list-group-item-action" style="white-space:nowrap; display:{{#if showSpinner}}block{{else}}none{{/if}};"><span class='spinner-border spinner-border-sm' role='status' aria-hidden='true'></span> Loading...</a>
                    <div id="welcome-screen" class="card" style="display:{{#if res}}none{{else}}block{{/if}};">
                        <div class="card-header p-1">
                            {{#if logo}}
                            <img src="{{logo}}" style="margin-top:-2px; padding:4px;">
                            {{/if}}
                            <span> NASALib Libraries </span>
                        </div>
                        <div class="card-body">
                        {{welcomeScreen}}
                        </div>
                        <div class="card-footer text-muted" style="white-space:nowrap; font-size:x-small;">
                            {{#each stats}}
                                {{#if @first}} {{this}} <br> ({{else}} {{@key}}: {{this}} {{/if}}
                                {{#if @first}} {{else}} {{#if @last}} {{else}} - {{/if}}{{/if}}
                            {{/each}})
                        </div>
                    </div>
                    <div id="search-results">
                    {{#each res}}
                    <a href="{{contextFolder}}/{{fileName}}{{fileExtension}}" {{#if line}}line="{{line}}"{{/if}} class="result-item list-group-item list-group-item-action py-2" style="white-space:nowrap;"><b>{{fileName}}{{fileExtension}}{{#if line}} (Ln {{line}}){{/if}}</b>: {{fileContent}}</a>
                    {{/each}}
                    </div>
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
        function clear () {
            $(".search-input").val("");
            $(".filter-results").val("");
            $(".filter").css("display", "none")
            $(".clear-btn").css("display", "none");
            $("#search-results").remove();
            $("#welcome-screen").css("display", "block");
            focus();
        }
        function focus () {
            $(".search-input").focus();
        }
        $(".search-btn").on("click", (evt) => {
            search();
        });
        $(".clear-btn").on("click", (evt) => {
            clear();
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
                case "focus": {
                    focus();
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

/**
 * Handlebars template for the welcome screen (shows folders and files in nasalib)
 * template based on https://www.w3schools.com/howto/howto_js_treeview.asp
 */
const welcomeScreenTemplate: string = `
<style>
/* Remove default bullets */
ul, #myUL {
  list-style-type: none;
}

/* Remove margins and padding from the parent ul */
#myUL {
  margin: 0;
  padding: 0;
}

/* Style the caret/arrow */
.caret {
  cursor: pointer;
  user-select: none; /* Prevent text selection */
}

/* Create the caret/arrow with a unicode, and style it */
.caret::before {
  content: "\\25B6";
  color: black;
  display: inline-block;
  margin-right: 6px;
}

/* Rotate the caret/arrow icon when clicked on (using JavaScript) */
.caret-down::before {
  transform: rotate(90deg);
}

/* Hide the nested list */
.nested {
  display: none;
}

/* Show the nested list when the user clicks on the caret/arrow (with JavaScript) */
.active {
  display: block;
}
</style>
<ul id="nasalib-tree">
    {{#each folders}}
    <li><span class="caret">{{@key}}</span>
        <ul class="nested">
        {{#each this}}
        <li>
            <a class="result-item" href="{{../../nasalibPath}}/{{contextFolder}}/{{fileName}}{{fileExtension}}" {{#if line}}line="{{line}}"{{/if}} style="white-space:nowrap;">{{theoryName}}</a>
        </li>
        {{/each}}
        </ul>
    </li>
    {{/each}}
</ul>`;

interface ViewData {
    res?: SearchResult[],
    searchString?: string, 
    showSpinner?: boolean
};
interface ViewOptions { 
    css?: Uri[], 
    js?: Uri[], 
    style?: string 
};
interface HtmlContent extends ViewData, ViewOptions {
    title: string,
    welcomeScreen: string,
    logo?: Uri,
    stats?: LookUpTableStats
};

import * as vscodeUtils from '../utils/vscode-utils';
import { nasalib_lookup_table } from '../common/nasalib-lookup-table';

/**
 * NASALib search class
 */
export class VSCodePvsSearch {
    protected panel: WebviewPanel;
    protected client: LanguageClient;
    protected context: ExtensionContext;

    // welcome screen
    protected welcomeScreen: string;

    // nasalib path
    protected nasalibPath: string;

    // nasalib data
    readonly nasalibLibraries: { [folderName: string]: PvsTheory[] } = nasalib_lookup_table.folders;

    // webview title
    readonly title: string = "NASALib Search";
    
    /**
     * Constructor
     */
    constructor (client: LanguageClient) {
		this.client = client;
        this.nasalibPath = this.getNasalibPath();
        this.welcomeScreen = "";
    }
    /**
     * Activates the view
     */
    activate(context: ExtensionContext): void {
        this.context = context;
    }
    /**
     * Reveals the view
     */
    reveal (): void {
        if (!this.panel) {
            this.renderView();
        }
        this.panel.reveal()
    }
    /**
     * Hides the view
     */
    hide (): void {
        this.panel.dispose();
    }
    /**
     * Internal function, creates the view
     */
    protected createWebView () {
        if (this.panel) {
            this.panel.title = this.title;
        } else {
            // create welcome screen
            this.welcomeScreen = Handlebars.compile(welcomeScreenTemplate, { noEscape: true })({
                folders: this.nasalibLibraries,
                nasalibPath: this.nasalibPath
            });            
            // create panel
            this.panel = window.createWebviewPanel(
                'vscode-pvs.nasalib-search', // Identifies the type of the webview. Used internally
                this.title, // Title of the panel displayed to the user
                ViewColumn.Beside, // Editor column to show the new webview panel in.
                {
                    enableScripts: true
                }
            );
            // set panel icon
            this.panel.iconPath = {
                light: Uri.file(path.join(__dirname, "..", "..", "..", "icons", "pvs-file-icon.png")),
                dark: Uri.file(path.join(__dirname, "..", "..", "..", "icons", "pvs-file-icon.png"))
            };
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
        // auto-focus search field
        this.focus();
    }
    /**
     * Utility function, show spinning wheel in the view to indicate search in progress
     */
    showSearching (data: { searchString: string }): void {
        this.refreshView({ ...data, showSpinner: true });
    }
    /**
     * Utility function, show search results in the view
     */
    showResults (data: { res: SearchResult[], searchString: string }): void {
        this.refreshView(data);
        // this.panel?.webview?.postMessage({
        //     command: "show-results",
        //     res
        // });
    }
    /**
     * Utility function, places the focus on the search input field of the view
     */
    focus (): void {
        this.panel?.webview?.postMessage({
            command: "focus"
        });
    }
    /**
     * Utility function, sends a search request to the server
     */
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
     * Internal function, creates the html content of the view
     */
    protected createContent (data?: ViewData): void {
        // set webview content
        const bootstrapJsOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/bootstrap/dist/js/bootstrap.bundle.min.js'));
        const bootstrapCssOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/bootstrap/dist/css/bootstrap.min.css'));
        const jqueryOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/jquery/dist/jquery.min.js'));
        const fontawesomeCssOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/font-awesome/css/font-awesome.min.css'));
        const css: Uri[] = [
            this.panel.webview.asWebviewUri(bootstrapCssOnDisk),
            this.panel.webview.asWebviewUri(fontawesomeCssOnDisk)
        ];
        const js: Uri[] = [
            this.panel.webview.asWebviewUri(jqueryOnDisk), // jquery needs to be loaded before bootstrap
            this.panel.webview.asWebviewUri(bootstrapJsOnDisk)
        ];
        this.panel.webview.html = this.createHtmlContent(data, { css, js });
    }
    /**
     * Renders the content of the webview
     */
    renderView (): void {
        this.refreshView();
    }
    /**
     * Refreshed the content of the webview
     */
    refreshView (data?: ViewData): void {
        // create webview
        this.createWebView();
        // create webview content
        this.createContent(data);
        // set language to pvs
        vscodeUtils.setEditorLanguagetoPVS();
    }
    /**
     * Creates the html rendered in the webview
     */
    protected createHtmlContent (data: ViewData, opt?: ViewOptions): string {
        const logoOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'icons/svg-nasa-meatball-icon.svg'));
        const logo: Uri = this.panel.webview.asWebviewUri(logoOnDisk);
        const htmlContent: HtmlContent = {
            title: this.title,
            welcomeScreen: this.welcomeScreen,
            logo,
            stats: nasalib_lookup_table.stats,
            ...data,
            ...opt
        };
        const html: string = Handlebars.compile(htmlTemplate, { noEscape: true })(htmlContent);
        return html;
    }
	/**
	 * Internal function, returns nasalib path
	 */
    protected getNasalibPath (): string {
		const pvsPath: string = vscodeUtils.getConfiguration("pvs.path");
		if (pvsPath) {
			return path.join(pvsPath, "nasalib");
		}
		return null;
	}
}