/**
 * @module vscodePvsProofTreeViz
 * @author Paolo Masci
 * @date 2021.01.13
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

import * as vscode from 'vscode';
import * as d3 from 'd3-hierarchy';
import * as Handlebars from "handlebars";
import { ExtensionContext, Uri, WebviewPanel } from 'vscode';
import { ProofNodeStatus, PvsFormula } from '../common/serverInterface';
import * as path from 'path';
import { TreeStructure } from '../common/languageUtils';
import * as fsUtils from '../common/fsUtils';

export type d3Iterable<T> = (node: T) => void;
export interface d3HierarchyNode<T>{
    x: number,
    y: number,
    parent?: d3HierarchyNode<T>,
    children?: d3HierarchyNode<T>[],
    data: T,
    each: (iterable: d3Iterable<d3HierarchyNode<T>>) => void,
    depth?: number, // distance from the root node. height = 0 for the root node
    height?: number // greatest distance from any descendant. height = 0 for leaf nodes
};

const exportTemplate: string = `
<!--
@formula: {{formula}}
@theory: {{theory}}
@pvsfile: {{pvsfile}}
@note: Use the Web Browser to view this file.
-->
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{{title}}</title>
    <style type="text/css">{{style}}</style>
</head>
<body style="margin-left:20px; margin-top:60px; padding:0; overflow:auto; background:whitesmoke;">
{{svg}}
</body>
`;

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
    {{#each js}}
    <script src="{{this}}"></script>
    {{/each}}
    <!--<link rel="stylesheet" 
        href="https://cdn.jsdelivr.net/npm/bootstrap@4.5.3/dist/css/bootstrap.min.css" 
        integrity="sha384-TX8t27EcRE3e/ihU7zmQxVncDAy5uIKz4rEkgIXeMed4M0jlfIDPvg6uqKI2xXr2" 
        crossorigin="anonymous">-->
</head>
<body style="margin-left:20px; margin-top:60px; padding:0; overflow:auto; background:whitesmoke;">
    <nav class="navbar navbar-light bg-dark fixed-top" style="width:100%; margin:0; padding:0;">
        <div class="container-fluid" style="margin:0; padding:2px;">
            <div class="btn-toolbar" role="toolbar" aria-label="Toolbar with button groups" style="transform:scale(0.8); transform-origin:left; min-width:120%;">
                <div class="dropdown" style="margin-left:20px;">
                    <button type="button" id="dropdown" data-toggle="dropdown" class="btn btn-sm btn-outline-light" aria-label="Settings"><i class="fa fa-bars"></i></button>
                    <div class="dropdown-menu dropdown-menu-left" style="padding:10px; width:250px;">
                        <b>Settings</b>
                        {{#each settings}}
                        <div class="input-group input-group-sm">
                            <div class="input-group-prepend"><span class="input-group-text" style="min-width:64px;">{{@key}}</span></div>
                            <input type="text" id="{{@key}}" class="settings form-control" placeholder="{{this}}" value="{{this}}">
                        </div>
                        {{/each}}
                    </div>
                </div>
                <div class="btn-group" role="group" style="margin-left:20px;">
                    <button type="button" id="zoom-minus" class="btn btn-sm btn-outline-warning" aria-label="Zoom minus"><i class="fa fa-minus"></i></button>
                    <button type="button" id="zoom-plus" class="btn btn-sm btn-outline-light" aria-label="Zoom plus"><i class="fa fa-plus"></i></button>
                    <button type="button" id="recenter" class="btn btn-sm btn-outline-light" aria-label="Recenter">Recenter</button>
                </div>
                <div class="btn-group" role="group" style="margin-left:20px;">
                    <button type="button" id="prev" class="btn btn-sm btn-outline-light control" alt="Back one step" aria-label="Back one step" style="width:61px;"><i class="fa fa-step-backward"></i></button>
                    <button type="button" id="pause" class="btn btn-sm btn-outline-light control" alt="Pause proof" aria-label="Pause proof" style="width:40px;"><i class="fa fa-pause"></i></button>
                    <button type="button" id="play" class="btn btn-sm btn-outline-light control" alt="Run proof" aria-label="Run proof" style="width:40px;padding:0px;"><i class="fa fa-play-circle fa-2x"></i></button>
                    <button type="button" id="next" class="btn btn-sm btn-outline-light control" alt="Step proof" aria-label="Step proof" style="width:80px;"><i class="fa fa-step-forward"></i></button>
                </div>
                <div class="btn-group" role="group" style="margin-left:20px;">
                    <button type="button" id="export-proof-tree" class="btn btn-sm btn-outline-light" aria-label="Export as HTML">Export as HTML</button>
                </div>
            </div>
        </div>
    </nav>
    <div id="content" style="transform: scale({{scale}}); transform-origin: top left";>
        <svg id="proof-tree" style="width:{{width}}px; height:{{height}}px;">
            <g class="links">
            {{#each links}}
                <g class="link {{source.data.id}} {{target.data.id}}">
                    <line x1="{{source.x}}" y1="{{source.y}}" x2="{{target.x}}" y2="{{target.y}}"></line>
                </g>
            {{/each}}
            </g>
            <g class="nodes">
            {{#each nodes}}
                {{#if @first}}
                <g id={{data.id}} class="node{{#if data.status.visited}} visited{{/if}}{{#if data.status.pending}} pending{{/if}}{{#if data.status.active}} active{{/if}}{{#if data.status.complete}} complete{{/if}}" transform="translate({{x}},{{y}})">
                    <circle class="root-node" r="2"></circle>
                    <text dy="-0.5em" text-anchor="middle" style="font:{{../settings.font}};">Proof: {{data.name}}</text>
                </g>
                {{else}}
                <g id={{data.id}} class="node{{#if data.status.visited}} visited{{/if}}{{#if data.status.pending}} pending{{/if}}{{#if data.status.active}} active{{/if}}{{#if data.status.complete}} complete{{/if}}" transform="translate({{x}},{{y}})">
                    <circle r="6"></circle>
                    <path class="checkmark" fill="none" d="M-3 0l2 3 6-6"></path>
                    <path class="star" d="M0 -6l1.379 4.246h4.465l-3.612 2.625 1.379 4.246-3.611-2.625-3.612 2.625 1.379-4.246-3.612-2.625h4.465l1.38-4.246"></path>
                    <!-- <rect y="-1.1em" x="-0.4em" width="0.8em" height="0.6em" style="fill:whitesmoke; fill-opacity:0.6; stroke:transparent;"></rect> -->
                    <text class="name" dy="0.25em" dx="1em" text-anchor="start" style="font:{{../settings.font}};">{{data.name}}</text>
                </g>
                {{/if}}
            {{/each}}
            </g>
        </svg>
    </div>
    <script>
    // utility functions to send messages to vscode-pvs from the webview
    (function() {
        const vscode = acquireVsCodeApi();
        {{#each controls}}
        $("#{{this}}").on("click", (evt) => {
            vscode.postMessage({ command: '{{this}}' });
        });
        {{/each}}
        $(".settings").on("input", (evt) => {
            const $elem = $(evt?.currentTarget);
            const id = $elem.attr("id")
            const val = $elem.val();
            vscode.postMessage({ command: 'settings', id, val });
        });
        $(".settings").on("keydown", (evt) => {
            const $elem = $(evt?.currentTarget);
            const id = $elem.attr("id")
            const val = $elem.val();
            const num = parseFloat(val);
            const valpp = evt.code === "ArrowUp" ? num + 1 
                : evt.code === "ArrowDown" ? num - 1
                    : num;
            if (valpp > 0) {
                const newval = val.replace(num, valpp);
                $elem.val(newval)
                vscode.postMessage({ command: 'settings', id, val: newval });    
            }
        });
        $("#export-proof-tree").on("click", () => {
            const innerHtml = $("#content").html();
            vscode.postMessage({ command: 'export-proof-tree', svg: innerHtml });

            // the following code does not work, need to investigate why
            // const xml = $("#content svg")[0];
            // const size = xml.getBBox();
            // const svg64 = btoa(xml);
            // const image64 = 'data:image/svg+xml;base64,' + svg64;
            // const image = new Image();
            // image.onload = (res) => {
            //     const canvas = document.createElement('canvas');
            //     canvas.width = size.width;
            //     canvas.height = size.height;
            //     const ctx = canvas.getContext('2d');
            //     ctx.drawImage(image, 0, 0, size.width, size.height);
            //     const png = canvas.toDataURL();
            //     vscode.postMessage({ command: 'export-proof-tree', svg: innerHtml, png });
            // }
            // image.onerror = (err) => {
            //     console.warn("Failed to create picture ", err);
            //     vscode.postMessage({ command: 'export-proof-tree', svg: innerHtml });
            // }
            // image.src = image64;
        });
    }());
    // Handle the message inside the webview
    window.addEventListener('message', event => {
        const message = event.data; // JSON data sent by vscode-pvs
        if (message) {
            const node = document.getElementById(message.id);
            switch (message.command) {
                case 'active': {
                    if (node) {
                        // only one node should be active
                        const all = document.getElementsByClassName("node");
                        for (let i = 0; i < all?.length; i++) {
                            all[i].classList?.remove("active");
                        }
                        node.classList?.add("active");
                        node.scrollIntoView({ behavior: "smooth", block: "center", inline: "center" });
                    }
                    break;
                }
                case 'pending': {
                    if (node) {
                        node.classList?.remove("visited");
                        node.classList?.add("pending");
                        node.classList?.remove("complete");
                    }
                    break;
                }
                case 'visited': {
                    if (node) {
                        node.classList?.add("visited");
                        node.classList?.remove("pending");
                        // node.classList?.remove("complete");
                    }
                    break;
                }
                case 'not-visited': {
                    if (node) {
                        node.classList?.remove("visited");
                        node.classList?.remove("pending");
                        node.classList?.remove("complete");
                    }
                    break;
                }
                case 'complete': {
                    if (node) {
                        node.classList?.remove("pending");
                        node.classList?.add("complete");
                    }
                    break;
                }
                case 'not-complete': {
                    if (node) {
                        node.classList?.remove("complete");
                    }
                    break;
                }
                case 'rename': {
                    if (node && node.name) {
                        $("#" + node + " .name").html(node.name);
                    }
                    break;
                }
                case 'deactivate-cursor': {
                    if (node) {
                        console.log("removing " + node.id);
                        // remove node
                        $("#" + node.id).remove();
                        // remove links connected to the node
                        $("." + node.id).remove();
                    }
                    break;
                }
                // other commands
                case 'zoom': {
                    if (message?.scale) {
                        $("#content").css({
                            transform: "scale(" + message.scale + ")"
                            //, "transform-origin": "top left"
                            //, transition: "200ms ease-out"
                        });
                    }
                    break;
                }
                case 'recenter': {
                    let $active = $(".active");
                    if ($active[0]) {
                        $active[0].scrollIntoView({ behavior: "smooth", block: "center", inline: "center" });
                    } else {
                        // center on root
                        $active = $(".root-node");
                        if (active[0]) {
                            $active[0].scrollIntoView({ behavior: "smooth", block: "center", inline: "center" });
                        }
                    }
                    break;
                }
                case 'recenter-fast': {
                    let $active = $(".active");
                    if ($active[0]) {
                        $active[0].scrollIntoView({ block: "center", inline: "center" });
                    } else {
                        // center on root
                        $active = $(".root-node");
                        if ($active[0]) {
                            $active[0].scrollIntoView({ block: "center", inline: "center" });
                        }
                    }
                    break;
                }
                case 'update-font': {
                    $(".node text").css("font", message.val);
                    break;
                }
                case 'disable-controls': {
                    $(".control").prop('disabled', true);
                    break;
                }
                case 'enable-controls': {
                    $(".control").prop('disabled', false);
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

const webviewStyle: string = `
.node {
    cursor: pointer;
    fill: white;
    stroke: darkgray;
}
.node .checkmark {
    stroke: transparent;
}
.node .star {
    stroke: transparent;
    fill: transparent;
}
.node:hover {
    cursor: pointer;
    stroke: steelblue;
}
.node.visited {
    fill: transparent;
    stroke: transparent;
}
.node.visited .star {
    fill: steelblue;
    stroke: steelblue;
}
.node.complete {
    fill: #7ac142;
    stroke: darkgray;
}
.node.complete .checkmark {
    stroke: white;
}
.node.complete .star {
    fill: transparent;
    stroke: transparent;
}
.node.pending {
    fill: transparent;
    stroke: transparent;
}
.node.pending .star {
    fill: white;
    stroke: steelblue;
}
.node text {
    font: 10px sans-serif;
    fill: darkslateblue;
    stroke: transparent;
}
.link {
    fill: none;
    stroke: #ccc;
    stroke-width: 1.5px;
}
`;
const webViewStyleAnim: string = `
.node.active {
    fill: steelblue !important;
    stroke: steelblue !important;
}
.node.active circle {
    fill: steelblue !important;
    stroke: steelblue !important;
    animation: pulser 2s linear infinite !important;
}
.node.active .star {
    fill: transparent !important;
    stroke: transparent !important;
}
.btn {
    min-width: 40px;
}
.spacer {
    display: none;
}
@keyframes pulser {
    0% { opacity: 1; }
    70% { opacity: 0.6; }
}`;

export type LayoutNode = d3HierarchyNode<TreeStructure>;
export type LayoutLink = { source: LayoutNode, target: LayoutNode };

const MAX_NAME_LEN: number = 64;

/**
 * Utility class, render the tree layout
 */
export class LayoutFactory {
    protected depth: number = 0;
    protected span: number = 0;
    protected width: number = 0;
    protected height: number = 0;
    protected layout: d3HierarchyNode<TreeStructure>;
    protected nodes: LayoutNode[] = [];
    protected links: LayoutLink[] = [];

    readonly marginTop: number = 20;

    clearLayout (): void {
        this.depth = 0;
        this.span = 0;
        this.width = 0;
        this.height = 0;
        this.nodes = [];
        this.links = [];
    }

	createLayout (root: TreeStructure, style: { hsep: number, vsep: number }): d3HierarchyNode<TreeStructure> {
        this.clearLayout();
        const structure: TreeStructure = { children: [ root ]}; // this is done to create some extra space at the top of the view, it's necessary to correctly render the proof name
        const layout: LayoutNode = d3.hierarchy(structure);
        if (layout) {
            this.depth = layout.height;

            layout?.each((node: LayoutNode) => {
                if (node?.depth && node?.data?.name) {
                    if (node.data.name?.length > MAX_NAME_LEN) {
                        node.data.name = node.data.name.substring(0, MAX_NAME_LEN) + "...";
                    }
                    this.nodes.push(node);
                    // the root node is special, the parent of the root is the root itself. Not sure this design is a good choice, the parent of the root should be null/undefined, maybe change this in the future.
                    if (node.parent?.data?.name && node.parent.data.id !== node.data.id) {
                        this.links.push({ source: node.parent, target: node });
                    }
                }
                if (node?.children?.length === 0 || !node.children) {
                    this.span++;
                }
            });

            this.width = this.span * style.hsep;
            this.height = this.depth * style.vsep;
            const d3Layout = d3.tree().size([ this.width, this.height]);
            this.layout = d3Layout(layout);
        }
        return this.layout;
    }

	getLayout (): d3HierarchyNode<TreeStructure> { return this.layout; }
	getDepth (): number { return this.depth; }
    getSpan (): number { return this.span; }
    getWidth (): number { return this.width; }
    getHeight (): number { return this.height; }
    getNodes (): LayoutNode[] { return this.nodes; }
    getLinks (): LayoutLink[] { return this.links; }
}

/**
 * Main class for rendering an interactive proof tree
 */
export class VSCodePvsVizTree {
    protected layout: LayoutFactory;
    protected panel: WebviewPanel;
    protected context: ExtensionContext;

    protected zoomLevel: number = 100;
    readonly minZoomLevel: number = 10;
    readonly maxZoomLevel: number = 1000;
    readonly zoomStep: number = 20;
    protected visible: boolean = false;

    protected root: TreeStructure;

    /**
	 * Settings
	 */
    protected font: string = "12px sans-serif";
    protected hsep: number = 4 * MAX_NAME_LEN; //px
    protected vsep: number = 32; //px

    /**
	 * Timer used to implement a delayed refresh of the view, useful to improve performance
	 */
	protected timer: NodeJS.Timer = null;
	protected tcounter: number = 0;
	readonly maxSkip: number = 10;
	readonly maxTimer: number = 500; //ms

    // stores information about which formula is being rendered
    protected formula: PvsFormula;

    /**
     * Constructor
     */
    constructor () {
        this.layout = new LayoutFactory();
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
        this.visible = true;
        this.panel.reveal()
    }
    /**
     * Hides the view
     */
    hide (): void {
        this.panel.dispose();
        this.visible = false;
    }
    /**
     * Utility function, returns true if the view is visible
     */
    isVisible (): boolean {
        return this.visible;
    }
    /**
     * Utility function, refreshes the font size in the view
     */
    refreshFont (): void {
        this.panel?.webview?.postMessage({
            command: "update-font",
            val: this.font
        });
    }
    /**
     * Utility function, re-centers the view on the active node
     * @param opt 
     */
    recenter (opt?: { fast?: boolean }): void {
        opt = opt || {};
        this.panel?.webview?.postMessage({
            command: opt.fast ? "recenter-fast" : "recenter"
        });
    }
    /**
     * Utility function, reduces the zoom level of the view
     */
    zoomMinus (): void {
        this.zoomLevel = this.zoomLevel - this.zoomStep < this.minZoomLevel ? this.minZoomLevel : this.zoomLevel - this.zoomStep;
        const scale = this.zoomLevel / 100;
        this.panel?.webview?.postMessage({
            command: "zoom",
            scale
        });
    }
    /**
     * Utility function, increases the zoom level of the view
     */
    zoomPlus(): void {
        this.zoomLevel = this.zoomLevel + this.zoomStep > this.maxZoomLevel ? this.maxZoomLevel : this.zoomLevel + this.zoomStep;
        const scale: number = this.zoomLevel / 100;
        this.panel?.webview?.postMessage({
            command: "zoom",
            scale
        });
    }
    /**
     * Internal function, creates the webview and installs relevant handlers
     */
    protected createWebView (title: string) {
        if (this.panel) {
            this.panel.title = `Proof: ${title}`;
        } else {
            this.panel = vscode.window.createWebviewPanel(
                'vscode-pvs.tree-viz', // Identifies the type of the webview. Used internally
                `Proof: ${title}`, // Title of the panel displayed to the user
                vscode.ViewColumn.Beside, // Editor column to show the new webview panel in.
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
            // Clean up data structures when webview is disposed
            this.panel.onDidDispose(
                () => {
                    this.panel = null;
                    this.visible = false;
                },
                null,
                this.context.subscriptions
            );
            // Handle messages from the webview
            this.panel.webview.onDidReceiveMessage(
                message => {
                    switch (message.command) {
                        case 'recenter': {
                            this.recenter();
                            break;
                        }
                        case 'zoom-minus': {
                            this.zoomMinus();
                            this.recenter({ fast: true });
                            break;
                        }
                        case 'zoom-plus': {
                            this.zoomPlus();
                            this.recenter({ fast: true });
                            break;
                        }
                        case 'next': {
                            vscode.commands.executeCommand("proof-explorer.forward");
                            break;
                        }
                        case 'prev': {
                            vscode.commands.executeCommand("proof-explorer.back");
                            break;
                        }
                        case 'play': {
                            vscode.commands.executeCommand("proof-explorer.run-proof");
                            break;
                        }
                        case 'pause': {
                            vscode.commands.executeCommand("proof-explorer.pause-proof");
                            break;
                        }
                        case 'settings': {
                            switch (message.id) {
                                case "font": {
                                    this.font = message.val;
                                    this.refreshFont();
                                    break;
                                }
                                case "vsep":
                                case "hsep": {
                                    if (parseFloat(message.val) > 0) {
                                        this[message.id] = parseFloat(message.val);
                                        this.refreshView();
                                    }
                                    break;
                                }
                            }
                            break;
                        }
                        case "export-proof-tree": {
                            if (this.formula) {
                                const fileName: string = `${this.formula.formulaName}`;
                                const exportFolder: string = "htmlExports";
                                const contextFolder: string = path.join(this.formula.contextFolder, exportFolder);
                                fsUtils.createFolder(path.join(this.formula.contextFolder, exportFolder));
                                if (message.png) {
                                    const fileExtension: string = ".png";
                                    const fname: string = fsUtils.desc2fname({
                                        contextFolder,
                                        fileName,
                                        fileExtension
                                    });
                                    fsUtils.writeFile(fname, message.png, { encoding: 'base64' });
                                    vscode.window.showInformationMessage(`Proof tree exported to ${exportFolder}/${fileName}${fileExtension}`);
                                } else {
                                    const content: string = Handlebars.compile(exportTemplate, { noEscape: true })({
                                        svg: message.svg,
                                        title: this.root?.name,
                                        width: this.layout.getWidth() * 2,
                                        height: this.layout.getHeight() * 1.1,
                                        style: webviewStyle,
                                        formula: this.formula.formulaName,
                                        theory: this.formula.theoryName,
                                        pvsfile: this.formula.fileName + this.formula.fileExtension
                                    });
                                    const fileExtension: string = ".html";
                                    const fname: string = fsUtils.desc2fname({
                                        contextFolder,
                                        fileName: fileName,
                                        fileExtension
                                    });
                                    fsUtils.writeFile(fname, content);
                                    vscode.window.showInformationMessage(`Proof tree exported to ${exportFolder}/${fileName}${fileExtension}`);
                                }
                            } else {
                                console.warn("[treeviz] Warning: could not export tree (formula is null)");
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
    /**
     * creates the entire html content
     * @param root 
     */
    protected createContent (root: TreeStructure): void {
        // set webview content
        const bootstrapJsOnDisk: vscode.Uri = vscode.Uri.file(path.join(this.context.extensionPath, 'client/node_modules/bootstrap/dist/js/bootstrap.bundle.min.js'));
        const bootstrapCssOnDisk: vscode.Uri = vscode.Uri.file(path.join(this.context.extensionPath, 'client/node_modules/bootstrap/dist/css/bootstrap.min.css'));
        const jqueryOnDisk: vscode.Uri = vscode.Uri.file(path.join(this.context.extensionPath, 'client/node_modules/jquery/dist/jquery.min.js'));
        const fontawesomeCssOnDisk: vscode.Uri = vscode.Uri.file(path.join(this.context.extensionPath, 'client/node_modules/font-awesome/css/font-awesome.min.css'));

        const css = [
            this.panel.webview.asWebviewUri(bootstrapCssOnDisk),
            this.panel.webview.asWebviewUri(fontawesomeCssOnDisk)
        ];
        const js = [
            this.panel.webview.asWebviewUri(jqueryOnDisk), // jquery needs to be loaded before bootstrap
            this.panel.webview.asWebviewUri(bootstrapJsOnDisk)
        ];
        this.panel.webview.html = this.createHtmlContent(root, { css, js, style: webviewStyle + webViewStyleAnim });
    }
    /**
     * Disables the back, forward, play controls
     */
    disableControls (): void {
        this.panel?.webview?.postMessage({ command: "disable-controls" });
    }
    /**
     * Enables the back, forward, play controls
     */
    enableControls (): void {
        this.panel?.webview?.postMessage({ command: "enable-controls" });
    }
    /**
     * Renders the content of the webview
     * @param root 
     * @param opt
     */
    renderView (root: TreeStructure, formula: PvsFormula, opt?: { reveal?: boolean, recenter?: boolean, source?: string, cursor?: string }): void {
        this.root = root;
        this.formula = formula;
        this.refreshView(opt);
    }
    /**
     * Refreshed the content of the webview
     * @param opt
     */
    refreshView (opt?: { reveal?: boolean, recenter?: boolean, source?: string, cursor?: string }): void {
        opt = opt || {};
        this.visible = opt.reveal !== undefined ? !!opt.reveal : this.visible;
        opt.recenter = opt.recenter === undefined ? true : opt.recenter;

        const ready: boolean = !!this.panel;
        if (this.root && this.visible) {
            const refresh = () => {
                // create webview
                this.createWebView(this.root.name);
                switch (opt.source) {
                    case "did-update-tooltip":
                    case "did-rename-node":
                    case "did-update-node-status": {
                        // todo: check if we can implement a lightweight refresh
                    }
                    default: {
                        // create webview content
                        this.createContent(this.root);
                        if (opt.recenter) {
                            this.recenter({ fast: true });
                        }
                        break;
                    }
                }
            }
            const delayedRefresh = () => {
                clearTimeout(this.timer);
                this.timer = setTimeout(() => {
                    refresh();
                }, this.maxTimer);
            }
            this.tcounter++;
            if (!ready || this.tcounter > this.maxSkip) {
                this.tcounter = 0;
                clearTimeout(this.timer);
                refresh();
            } else {
                delayedRefresh();
            }
        }
    }
    rename (id: string, name: string): void {
        if (id) {
            this.panel?.webview?.postMessage({ command: "rename", id, name });
        }
    }
    active (id: string): void {
        if (id) {
            this.panel?.webview?.postMessage({ command: "active", id });
        }
    }
    deactivateCursor (id: string): void {
        if (id) {
            this.panel?.webview?.postMessage({ command: "deactivate-cursor", id });
        }
    }
    visited (id: string): void {
        if (id) {
            this.panel?.webview?.postMessage({ command: "visited", id });
        }
    }
    notVisited (id: string): void {
        if (id) {
            this.panel?.webview?.postMessage({ command: "not-visited", id });
        }
    }
    pending (id: string): void {
        if (id) {
            this.panel?.webview?.postMessage({ command: "pending", id });
        }
    }
    complete (id: string): void {
        if (id) {
            this.panel?.webview?.postMessage({ command: "complete", id });
        }
    }
    notComplete (id: string): void {
        if (id) {
            this.panel?.webview?.postMessage({ command: "not-complete", id });
        }
    }
    protected updateStatus (desc: { id: string, status: ProofNodeStatus }): void {
        if (desc && this.visible) {
            switch (desc.status) {
                case "active": { this.active(desc.id); break; }
                case "visited": { this.visited(desc.id); break; }
                case "not-visited": { this.notVisited(desc.id); break; }
                case "pending": { this.pending(desc.id); break; }
                case "complete": { this.complete(desc.id); break; }
                case "not-complete": { this.notComplete(desc.id); break; }
                default: {
                    console.warn(`[vscode-viz-tree] Warning: unrecognized node status ${desc.status}`);
                }
            }
        }
    }
    /**
     * Creates the html rendered in the webview
     * @param root Proof tree
     * @param opt Options
     * <li>css: css style files;</li>
     * <li>js: js files;</li>
     * <li>style: inline css style</li>
     */
    protected createHtmlContent (root: TreeStructure, opt?: { css?: vscode.Uri[], js?: vscode.Uri[], style?: string }): string {
        this.layout.createLayout(root, { hsep: this.hsep, vsep: this.vsep });
        const nodes: LayoutNode[] = this.layout.getNodes();
        const links: LayoutLink[] = this.layout.getLinks();
        const height: number = this.layout.getHeight();
        const width: number = this.layout.getWidth();
        const html: string = Handlebars.compile(htmlTemplate, { noEscape: true })({
            title: root?.name,
            nodes,
            links,
            width: width * 2,
            height: height * 1.1,
            scale: this.zoomLevel / 100,
            settings: {
                font: this.font,
                // vsep: this.vsep + "px",
                // hsep: this.hsep + "px"
            },
            controls: [
                "recenter",
                "zoom-minus",
                "zoom-plus",
                "next",
                "prev",
                "play",
                "pause"
            ],
            ...opt
        });
        return html;
    }
}