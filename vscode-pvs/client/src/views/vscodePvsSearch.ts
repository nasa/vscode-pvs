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
    LookUpTableStats, PvsContextDescriptor, PvsTheory, SearchLibrary,
    SearchRequest, SearchResponse, SearchResult, serverRequest 
} from "../common/serverInterface";
import * as path from 'path';
import { LanguageClient } from "vscode-languageclient";
import * as Handlebars from "handlebars";

// button labels
const SEARCH_PVSLIB: string = "Search User Libraries";
const SEARCH_NASALIB: string = "Search NASALib";

/**
 * Handlebars template for the html content of the view
 */
const htmlContent: string = `
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
            // remove results and show welcome screen
            $("#search-results").remove();
            $("#results-summary").css("display", "none");
            $(".filter").css("display", "none");
            $("#welcome-screen").css("display", "block");
        }
    });
    $(".filter-results").on("keyup", function() {
        $("#results-summary").css("display", "block");
        const value = $(this).val().toLowerCase();
        const visible = $("#search-results .result-item").filter(function() {
            $(this).toggle($(this).text().toLowerCase().indexOf(value) > -1);
        });
        $("#filter").text(value ? $("#search-results .result-item :visible").length + " of " : "");
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
.search-btn {
    cursor: pointer;
}
.bg-white {
    background: white;
}
.btn-outline-secondary:hover {
    background: white;
    color: black;
}
.toggle-btn {
    opacity:0.8;
    transform:scale(0.8);
    border-radius:8px !important;
}
</style>
<body style="overflow:auto; background:whitesmoke; font-size:small;" class="p-1">
    <div class="container-fluid mb-1" style="position:sticky; top:0px; padding:4px; z-index:1;">
        <div class="input-group input-group-sm mb-1 bg-white">
            <div class="input-group-prepend search-btn">
                <span class="input-group-text"><i class="fa fa-search" aria-hidden="true"></i></span>
            </div>
            <input type="text" class="form-control search-input" placeholder="Search..." aria-label="Search" value="{{searchString}}">
            <button class="btn btn-outline-secondary btn-sm clear-btn" type="button" style="display:{{#if res}}block{{else}}none{{/if}}"><i class="fa fa-times" aria-hidden="true"></i></button>
            <button class="btn btn-outline-secondary btn-sm case-sensitive-option-btn toggle-btn" type="button" data-toggle="tooltip" data-placement="bottom"  title="Toggle case sensitive search">Aa</i></button>
            <button class="btn btn-outline-primary btn-sm search-btn" id="search-btn" type="button">${SEARCH_NASALIB}</button>
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
                    <a href="#" id="results-summary" class="list-group-item list-group-item-action" style="white-space:nowrap; display:{{#if showResults}}block{{else}}none{{/if}};"><span id="filter"></span><span id="matches">{{res.length}}</span> matches</a>
                    <div id="welcome-screen" class="card" style="display:{{#if res}}none{{else}}block{{/if}};">                    
                        <div class="card-header p-1">
                            <div class="libraries btn-group btn-group-sm" role="group" aria-label="Basic example">
                                <button type="button" class="btn btn-outline-secondary active" id="nasalib-btn" name="nasalib">
                                    {{#if logo}}
                                    <img src="{{logo}}" style="margin-top:-2px; padding:4px;">
                                    {{/if}}
                                    <span> NASALib Libraries </span>
                                </button>
                                <button type="button" class="btn btn-outline-secondary" id="pvslib-btn" name="pvslib">
                                    <span> User-Defined Libraries </span>
                                </button>
                            </div>
                        </div>
                        <div class="card-body">
                        {{welcomeScreen}}
                        </div>
                        <div class="card-footer text-muted" style="white-space:nowrap; font-size:x-small;">
                            <div class="lib-footer" id="nasalib-footer">
                            {{#each nasalibStats}}
                                {{#if @first}} {{this}} <br> ({{else}} {{@key}}: {{this}} {{/if}}
                                {{#if @first}} {{else}} {{#if @last}} {{else}} - {{/if}}{{/if}}
                            {{/each}})
                            </div>
                            <div class="lib-footer" id="pvslib-footer" style="display:none;">
                            {{#each pvslibStats}}
                                {{#if @first}} {{this}} <br> ({{else}} {{@key}}: {{this}} {{/if}}
                                {{#if @first}} {{else}} {{#if @last}} {{else}} - {{/if}}{{/if}}
                            {{/each}})
                            </div>
                        </div>
                    </div>
                    <div id="search-results">
                    {{#each res}}
                    <a href="{{contextFolder}}/{{fileName}}{{fileExtension}}" {{#if line}}line="{{line}}"{{/if}} class="result-item list-group-item list-group-item-action py-2" style="white-space:nowrap;"><b>{{#if libName}}{{libName}}/{{/if}}{{fileName}}{{fileExtension}}{{#if line}} (Ln {{line}}){{/if}}</b>: {{fileContent}}</a>
                    {{/each}}
                    </div>
                </ul>
            </div>
        </div>
    </div>

    <script>
    // active view
    let activeView = "{{activeView}}";
    // case sensitive search flag
    let caseSensitive = {{caseSensitive}};
    // utility functions to send messages to vscode-pvs from the webview
    (function() {
        const vscode = acquireVsCodeApi();
        function search () {
            const searchString = $(".search-input")?.val()?.trim();
            caseSensitive = $(".case-sensitive-option-btn").hasClass("active");
            if (searchString) {
                vscode.postMessage({ command: 'search', searchString, activeView, caseSensitive });
            }
        }
        function clear () {
            $(".search-input").val("");
            $(".filter-results").val("");
            $(".filter").css("display", "none")
            $(".clear-btn").css("display", "none");
            $("#search-results").remove();
            $("#results-summary").css("display", "none");
            $("#welcome-screen").css("display", "block");
            refreshSearchOptions();
            selectActiveView();
            focus();
        }
        function focus () {
            $(".search-input").focus();
            // Move the cursor to the end of the input
            const len = $(".search-input").val().length;
            $(".search-input")[0].setSelectionRange(len, len);
        }
        function selectActiveView () {
            activeView === "nasalib" ? nasalibView() : pvslibView();
        }
        function refreshSearchOptions () {
            caseSensitive ? enableCaseSensitiveSearch() : disableCaseSensitiveSearch();
        }
        function nasalibView () {
            activeView = "nasalib";
            $(".lib-tree").css("display", "none");
            $(".lib-footer").css("display", "none");
            $("#nasalib-tree").css("display", "block");
            $("#nasalib-footer").css("display", "block");
            $("#pvslib-btn").removeClass("active");
            $("#nasalib-btn").addClass("active");
            $("#search-btn").text("${SEARCH_NASALIB}");
            focus();
        }
        function pvslibView () {
            activeView = "pvslib";
            $(".lib-tree").css("display", "none");
            $(".lib-footer").css("display", "none");
            $("#pvslib-tree").css("display", "block");
            $("#pvslib-footer").css("display", "block");
            $("#nasalib-btn").removeClass("active");
            $("#pvslib-btn").addClass("active");
            $("#search-btn").text("${SEARCH_PVSLIB}");
            focus();
        }
        function enableCaseSensitiveSearch () {
            $(".case-sensitive-option-btn").addClass("active").removeClass("btn-outline-secondary").addClass("btn-secondary");
        }
        function disableCaseSensitiveSearch () {
            $(".case-sensitive-option-btn").removeClass("active").addClass("btn-outline-secondary").removeClass("btn-secondary");
        }
        function toggleCaseSensitiveSearch () {
            const $btn = $(".case-sensitive-option-btn");
            $btn.hasClass("active") ? disableCaseSensitiveSearch() : enableCaseSensitiveSearch();
        }
        $(".toggle-btn").on("click", (evt) => {
            toggleCaseSensitiveSearch();
        });
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
        $(".libraries .btn").on("click", (evt) => {
            const isActive = $(evt?.currentTarget).hasClass("active");
            if (!isActive) {
                const btn_name = $(evt?.currentTarget).attr("name");
                btn_name === "nasalib" ? nasalibView() : pvslibView();
            }
        });
        // initialize all tooltips
        $('[data-toggle="tooltip"]').tooltip();
        // clear tooltips on mouse down and focus search input field
        $('[data-toggle="tooltip"]').on("mousedown", (evt) => {
            $('[data-toggle="tooltip"]').tooltip('hide');
            console.log(evt);
            evt.stopPropagation();
            focus();
        });
        // 'Enter' key on this button as a search request
        $('[data-toggle="tooltip"]').on("keypress", (evt) => {
            console.log(evt);
            evt.stopPropagation();
            refreshSearchOptions();
            switch (evt.key) {
                case "Enter": {
                    search();
                    break;
                }
                default: {
                    // do nothing
                }
            }
            focus();
        });
        // refresh search options
        refreshSearchOptions();
        // select the view
        selectActiveView ();
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
const libraryListTemplate: string = `
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
<ul id="nasalib-tree" class="lib-tree"">
    {{#each nasalibFolders}}
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
</ul>
<ul id="pvslib-tree" class="lib-tree">
    {{#each pvslibFolders}}
    <li><span class="caret">{{@key}}</span>
        <ul class="nested">
        {{#each this}}
        <li>
            <a class="result-item" href="{{contextFolder}}/{{fileName}}{{fileExtension}}" {{#if line}}line="{{line}}"{{/if}} style="white-space:nowrap;">{{theoryName}}</a>
        </li>
        {{/each}}
        </ul>
    </li>
    {{/each}}
</ul>`;
interface LibraryList {
    nasalibFolders: LibraryMap,
    nasalibPath: string,
    pvslibFolders: LibraryMap
}
interface SearchResultsOrSpinner {
    res?: SearchResult[],
    searchString?: string, // string to be searched
    caseSensitive?: boolean, // whether the search is case sensitive
    nasalib?: boolean,
    showSpinner?: boolean,
    showResults?: boolean
};
interface ViewOptions { 
    css?: Uri[], 
    js?: Uri[], 
    style?: string 
};
interface HtmlContent extends SearchResultsOrSpinner, ViewOptions {
    title: string,
    welcomeScreen: string,
    activeView: SearchLibrary,
    logo?: Uri,
    nasalibStats?: LookUpTableStats,
    pvslibStats?: LookUpTableStats
};
interface LibraryMap { [folderName: string]: PvsTheory[] };

import * as vscodeUtils from '../utils/vscode-utils';
import { nasalib_lookup_table } from '../common/nasalib-lookup-table';
import * as fsUtils from '../common/fsUtils';

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
    // nasalib data, cached in advance to speed up rendering
    readonly nasalibLibraries: LibraryMap = nasalib_lookup_table.folders;

    // pvs library data
    protected pvsLibraries: LibraryMap = {};
    protected pvslibStats: LookUpTableStats;

    // webview title
    readonly title: string = "Search NASALib";

    // active view, either nasalib or pvslib
    protected activeView: SearchLibrary = "nasalib";

    // whether the search is case sensitive
    protected caseSensitive: boolean = false;
    
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
    async reveal (): Promise<void> {
        if (!this.panel) {
            await this.loadPvsLibraryDescriptors();
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
     * Internal function, creates the view or simply updates the title if the view exists
     */
    protected createWebView () {
        // create welcome screen
        const data: LibraryList = {
            nasalibFolders: this.nasalibLibraries,
            nasalibPath: this.nasalibPath,
            pvslibFolders: this.pvsLibraries
        };
        this.welcomeScreen = Handlebars.compile(libraryListTemplate, { noEscape: true })(data);            
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
                            this.activeView = message.activeView === "nasalib" ? "nasalib" : "pvslib";
                            this.caseSensitive = message.caseSensitive === true;
                            this.showSearching({ searchString: message.searchString });
                            const res: SearchResult[] = await this.search(message.searchString, { caseSensitive: this.caseSensitive });
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
        // set language to pvs
        vscodeUtils.setEditorLanguagetoPVS();
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
        this.refreshView({ ...data, showResults: true });
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
    async search (searchString: string, opt?: { caseSensitive?: boolean }): Promise<SearchResult[]> {
        return new Promise ((resolve, reject) => {
            const req: SearchRequest = {
                searchString,
                caseSensitive: !!opt?.caseSensitive,
                library: this.activeView
            };
            this.client.sendRequest(serverRequest.search, req);
            this.client.onNotification(serverRequest.search, (res: SearchResponse) => {
                return resolve(res?.ans);
            });
        });
    }
    /**
     * Internal function, creates the html content of the view
     */
    protected renderSearchResultOrSpinner (data?: SearchResultsOrSpinner): void {
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
        const logoOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'icons/svg-nasa-meatball-icon.svg'));
        const logo: Uri = this.panel.webview.asWebviewUri(logoOnDisk);
        const htmlData: HtmlContent = {
            ...data,
            title: this.title,
            welcomeScreen: this.welcomeScreen,
            logo,
            nasalibStats: nasalib_lookup_table.stats,
            pvslibStats: this.pvslibStats,
            activeView: this.activeView,
            caseSensitive: this.caseSensitive,
            css,
            js
        };
        const html: string = Handlebars.compile(htmlContent, { noEscape: true })(htmlData);
        this.panel.webview.html = html;
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
    refreshView (data?: SearchResultsOrSpinner): void {
        // create webview
        if (!this.panel) { this.createWebView(); }
        // create webview content
        this.renderSearchResultOrSpinner(data);
        // auto-focus search field
        this.focus();
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
	/**
	 * Internal function, returns nasalib path
	 */
    protected async loadPvsLibraryDescriptors (): Promise<void> {
        this.pvsLibraries = {};
        const libraryFolders: string[] = vscodeUtils.getConfiguration("pvs.pvsLibraryPath")?.split(":").map(elem => {
            return elem.trim();
        }).filter(elem => {
            return elem && elem !== "";
        });
        let nTheories: number = 0;
		for (let i = 0; i < libraryFolders?.length; i++) {
			const contextFolders: string[] = fsUtils.listSubFolders(libraryFolders[i])?.map(elem => {
                return path.join(libraryFolders[i], elem);
            });
			for (let k = 0; k < contextFolders?.length; k++) {
				const desc: PvsContextDescriptor = await fsUtils.getContextDescriptor(contextFolders[k], {
					listTheorems: false, 
					includeTccs: false
				});
                const files: string[] = Object.keys(desc?.fileDescriptors);
                for (let f = 0; f < files?.length; f++) {
                    const contextFolder: string = desc.fileDescriptors[files[f]].contextFolder;
                    const contextFolderName: string = fsUtils.getContextFolderName(contextFolder);
                    if (desc.fileDescriptors[files[f]]?.theories?.length) {
        				this.pvsLibraries[contextFolderName] = this.pvsLibraries[contextFolderName] || [];
                        this.pvsLibraries[contextFolderName] = this.pvsLibraries[contextFolderName].concat(desc.fileDescriptors[files[f]]?.theories?.map(elem => {
                            return {
                                theoryName: elem.theoryName,
                                line: elem.position.line,
                                contextFolder: elem.contextFolder,
                                fileName: elem.fileName,
                                fileExtension: elem.fileExtension
                            };
                        }));
                        nTheories += desc.fileDescriptors[files[f]]?.theories?.length;
                    }
                }
                // TODO: sort theory names?
			}
		}
        this.pvslibStats = {
            version: "User-defined libraries",
            folders: this.pvsLibraries ? Object.keys(this.pvsLibraries).length : 0,
            theories: nTheories               
        };
    }
}