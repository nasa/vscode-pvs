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
import * as vscodeUtils from '../utils/vscode-utils';
import { PvsResponse } from "../common/pvs-gui";
import { LanguageClient } from "vscode-languageclient";


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
                panel.webview.html = vscodeUtils.createPlot({ expr });

                this.client.sendRequest(serverRequest.evalExpression, desc);
                this.client.onRequest(serverEvent.evalExpressionResponse, async (desc: {
                    request: PvsioEvaluatorCommand,
                    response: PvsResponse
                }) => {
                    // update plot
                    panel.webview.html = vscodeUtils.createPlot({
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
}