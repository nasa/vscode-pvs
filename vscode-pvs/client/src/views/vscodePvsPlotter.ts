/**
 * @module vscodePvsPlotter
 * @author Paolo Masci
 * @date 2020.11.21
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

import { EvalExpressionRequest, PvsioEvaluatorCommand, serverRequest } from "../common/serverInterface";
import * as vscode from 'vscode';
import { PvsResponse } from "../common/pvs-gui";
import { LanguageClient } from "vscode-languageclient";
import * as utils from '../common/languageUtils';
import * as path from 'path';
import * as Handlebars from "handlebars";
import { Uri } from "vscode";

const MAX_INNER_LABEL_LEN: number = 128;
const MAX_TAB_LABEL_LEN: number = 24;

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

    <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
    
    <!--
    <link rel="stylesheet" 
            href="https://cdn.jsdelivr.net/npm/bootstrap@4.5.3/dist/css/bootstrap.min.css" 
            integrity="sha384-TX8t27EcRE3e/ihU7zmQxVncDAy5uIKz4rEkgIXeMed4M0jlfIDPvg6uqKI2xXr2" 
            crossorigin="anonymous">
    -->

</head>
<body style="margin:0; padding:0;">
    <div class="card">
    <div class="card-header">
        {{title}}
    </div>
    <div class="card-body">
        {{body}}
    </div>
</body>
</html>`;

export class VSCodePvsPlotter {
    protected client: LanguageClient;
    protected context: vscode.ExtensionContext;
    protected panel: vscode.WebviewPanel;
    
    /**
     * Constructor
     */
    constructor (client: LanguageClient) {
		this.client = client;
    }
    /**
     * Activates the module
     */
    activate (context: vscode.ExtensionContext) {
        this.context = context;
    }
    /**
     * Main API, plots a pvs expressions
     */
    async plot (desc: EvalExpressionRequest): Promise<boolean> {
        return new Promise((resolve, reject) => {
            if (desc && desc.expr) {
                const expr: string = desc.expr;
                const label: string = expr.length > MAX_INNER_LABEL_LEN ? expr.substring(0, MAX_TAB_LABEL_LEN) + "..." : expr;
                // create webview
                this.panel = vscode.window.createWebviewPanel(
                    'vscode-pvs.plot-expression', // Identifies the type of the webview. Used internally
                    `plot-expression ${label}`, // Title of the panel displayed to the user
                    vscode.ViewColumn.Beside, // Editor column to show the new webview panel in.
                    {
                        enableScripts: true,
                        retainContextWhenHidden: true
                    } // Webview options.
                );
                // set panel icon
                this.panel.iconPath = {
                    light: Uri.file(path.join(__dirname, "..", "..", "..", "icons", "pvs-file-icon.png")),
                    dark: Uri.file(path.join(__dirname, "..", "..", "..", "icons", "pvs-file-icon.png"))
                };
                // set webview content
                this.panel.webview.html = this.createContent({ expr });
                // send request to server
                this.client.sendRequest(serverRequest.evalExpression, desc);
                this.client.onNotification(serverRequest.evalExpression, async (desc: {
                    req: PvsioEvaluatorCommand,
                    res: PvsResponse
                }) => {
                    // update plot
                    this.panel.webview.html = this.createContent({
                        expr,
                        data: desc?.res?.result || desc?.res?.error
                    });
                    resolve(true);
                });
            } else {
                resolve(false);
            }
        });
    }
    /**
     * Internal function, parses a pvs expression and returns the plot data contained in the expression
     */
    protected getPlotData (data: string, opt?: { mode?: utils.PlotMode }): utils.PlotData[] {
        opt = opt || {};
        const plotData: utils.PlotData[] = 
            utils.isTupleTupleListExpr(data) ? utils.tupletuplelist2PlotData(data)
                : utils.isListExpr(data) ? [ utils.list2PlotData(data) ]
                    : utils.isTupleExpr(data) ? utils.tuple2PlotData(data)
                        : [];
        return plotData;
    }
    /**
     * Internal function, returns the html code for rendering the plot with plotly.js
     */
    protected getBody (desc: { expr: string, data?: string }, opt?: { title?: string, mode?: utils.PlotMode }): string {
        const plotData: utils.PlotData[] = this.getPlotData(desc?.data);
        const plotDataString: string = JSON.stringify(plotData);
        let title: string = opt?.title || "";
        title = title.replace(utils.commentRegexp, "").replace(/\s\s+/g, " ").replace(/\n/g, "");
        title = title.length > MAX_INNER_LABEL_LEN ? title.substring(0, MAX_INNER_LABEL_LEN) + "..." : title;
        const body: string = desc.data ?`
            <p class="card-text">
                <label class="btn btn-sm btn-secondary active">
                    <input type="radio" name="options" id="linear" checked> Linear
                </label>
                <label class="btn btn-sm btn-secondary">
                    <input type="radio" name="options" id="semilog"> SemiLog
                </label>
                <label class="btn btn-sm btn-secondary">
                    <input type="radio" name="options" id="loglog"> LogLog
                </label>
            </p>
            <div id="plot" style="width:100%;height:100%;"></div>
            <script>
                document.getElementById("linear").onclick = function() {
                    plot({ xScale: "linear", yScale: "linear" });
                };
                document.getElementById("semilog").onclick = function() {
                    plot({ xScale: "log", yScale: "linear" });
                };
                document.getElementById("loglog").onclick = function() {
                    plot({ xScale: "log", yScale: "log" });
                };
                function plot (opt) {
                    opt = opt || {};
                    const divElem = document.getElementById('plot');
                    Plotly.newPlot(divElem, ${plotDataString}, {
                        xaxis: {
                            type: opt.xScale || "linear",
                            autorange: true
                        },
                        yaxis: {
                            type: opt.yScale || "linear",
                            autorange: true
                        },
                        title: "${title?.length > 32 ? title?.substr(0,32) + "..." : title}"
                    }, {
                        editable: true,
                        scrollZoom: true,
                        responsive: true,
                        modeBarButtonsToRemove: [ 
                            'toImage', 'lasso2d', 'zoom2d', 'select2d', 
                            'hoverClosestCartesian', 'hoverCompareCartesian', 'resetScale2d'
                        ],
                    });
                }
                plot();
            </script> `
            :
            `<button class="btn btn-primary" type="button" disabled>
            <span class="spinner-border spinner-border-sm" role="status" aria-hidden="true"></span>
            Loading...
            </button>`
            ;
        return body;
    }

    /**
     * Internal function, creates the html content of the webview
     */
    protected createContent (series: { expr: string, data?: string }, opt?: { title?: string }): string {
        opt = opt || {};
        const title: string = opt.title || series.data || series.expr || "plot-expression";
        // const bootstrapJsOnDisk: vscode.Uri = vscode.Uri.file(path.join(this.context.extensionPath, 'client/node_modules/bootstrap/dist/js/bootstrap.bundle.min.js'));
        const bootstrapCssOnDisk: vscode.Uri = vscode.Uri.file(path.join(this.context.extensionPath, 'client/node_modules/bootstrap/dist/css/bootstrap.min.css'));
        // const plotlyOnDisk: vscode.Uri = vscode.Uri.file(path.join(this.context.extensionPath, 'client/node_modules/plotly.js/dist/plotly.min.js'));
        const body: string = this.getBody(series, { title: opt.title || series.expr });
        const html: string = Handlebars.compile(htmlTemplate, { noEscape: true })({
            title,
            js: [
                // this.panel?.webview?.asWebviewUri(plotlyOnDisk)
            ],
            css: [
                this.panel?.webview?.asWebviewUri(bootstrapCssOnDisk)
            ],
            body
        });
        return html;
    }
}