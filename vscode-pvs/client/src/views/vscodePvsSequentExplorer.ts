/**
 * @module VSCodePvsSequentExplorer
 * @author Paolo Masci
 * @date 2019.06.18
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
import * as fsUtils from '../common/fsUtils';
import * as path from 'path';
import * as serverInterface from '../common/serverInterface';

export declare interface SequentDescriptor {
    contextFolder: string;
    fileName: string;
    fileExtension: string;
    theoryName: string;
    formulaName: string;
    line: number;
    sequent: string;
 }

 export class VSCodePvsSequentExplorer {
    constructor () {}
    async showSequent (desc: SequentDescriptor): Promise<void> {
        // const fname: string = path.join(desc.contextFolder, `${desc.fileName}.${desc.formulaName}.prs`);
        // await fsUtils.writeFile(fname, desc.sequent);
        // const doc: vscode.TextDocument = await vscode.workspace.openTextDocument(fname);
        // const doc: vscode.TextDocument = await vscode.workspace.openTextDocument(vscode.Uri.parse("untitled:current-sequent")); // is there a way to set the content?
        const doc: vscode.TextDocument = await vscode.workspace.openTextDocument({ language: 'pvs', content: desc.sequent });
        vscode.window.showTextDocument(doc, { viewColumn: vscode.ViewColumn.Beside, preview: true }); // open window on the side
    }
    /**
	 * Handler activation function
	 * @param context Client context 
	 */
	activate(context: vscode.ExtensionContext) {
        // -- commands sent by pvsTerminalCli
		let cmd: vscode.Disposable = vscode.commands.registerCommand("sequent-explorer.show-sequent", (desc: SequentDescriptor) => {
			this.showSequent(desc);
		});
		context.subscriptions.push(cmd);
    }
}