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

import { EvalExpressionRequest, PvsioEvaluatorCommand, serverEvent, serverRequest } from "../common/serverInterface";
import * as vscode from 'vscode';
import { PvsResponse } from "../common/pvs-gui";
import { LanguageClient } from "vscode-languageclient";
import * as utils from '../common/languageUtils';

export class VSCodePvsPlotter {
    protected client: LanguageClient;
    protected context: vscode.ExtensionContext;
    
    constructor (client: LanguageClient) {
		this.client = client;
    }
    activate (context: vscode.ExtensionContext) {
        this.context = context;
    }
    async plot (desc: EvalExpressionRequest): Promise<boolean> {
        return new Promise((resolve, reject) => {
            if (desc && desc.expr) {
                const expr: string = desc.expr;
                // create webview
                const panel = vscode.window.createWebviewPanel(
                    'plot-expression', // Identifies the type of the webview. Used internally
                    'Plot Expression', // Title of the panel displayed to the user
                    vscode.ViewColumn.Beside, // Editor column to show the new webview panel in.
                    {
                        enableScripts: true
                    } // Webview options.
                );
                // set webview content
                panel.webview.html = this.getHtmlPlot({ expr });

                this.client.sendRequest(serverRequest.evalExpression, desc);
                this.client.onRequest(serverEvent.evalExpressionResponse, async (desc: {
                    request: PvsioEvaluatorCommand,
                    response: PvsResponse
                }) => {
                    // update plot
                    panel.webview.html = this.getHtmlPlot({
                        expr,
                        data: desc?.response?.result || desc?.response?.error
                    });
                    resolve(true);
                });
            } else {
                resolve(false);
            }
        });
    }
    getHtmlPlot (desc: { expr: string, data?: string }, opt?: { title?: string }): string {
        opt = opt || {};
        const title: string = opt.title || desc.expr || "Plot Expression";
        const yValues: number[] = utils.isListExpr(desc.data) ?
            utils.listExpr2doubleArray(desc.data) : [];
        let y: string = "";
        let x: string = "";
        for (let i = 0; i < yValues.length; i++) {
            x += ` ${i}`;
            y += ` ${yValues[i]}`;
            if (i < yValues.length - 1) {
                x += ",";
                y += ",";
            }
        }
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
                    Plotly.newPlot(divElem, [{
                        x: [${x}],
                        y: [${y}]
                    }], {
                        xaxis: {
                            type: opt.xScale || "linear",
                            autorange: true
                        },
                        yaxis: {
                            type: opt.yScale || "linear",
                            autorange: true
                        }    
                    }, {
                        editable: true,
                        scrollZoom: true,
                        responsive: true,
                        modeBarButtonsToRemove: [ 'toImage', 'lasso2d', 'zoom2d', 'select2d', 'hoverClosestCartesian', 'hoverCompareCartesian', 'resetScale2d' ]
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
        const content: string = `
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>${title}</title>
        <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
        <link rel="stylesheet" 
            href="https://cdn.jsdelivr.net/npm/bootstrap@4.5.3/dist/css/bootstrap.min.css" 
            integrity="sha384-TX8t27EcRE3e/ihU7zmQxVncDAy5uIKz4rEkgIXeMed4M0jlfIDPvg6uqKI2xXr2" 
            crossorigin="anonymous">
    </head>
    <body style="margin:0; padding:0;">
        <div class="card">
        <div class="card-header">
            ${title}
        </div>
        <div class="card-body">
            ${body}
        </div>
    </body>
    </html>`;
        return content;
    }
    
}