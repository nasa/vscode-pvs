/**
 * @module VSCodePvsSequentViewer
 * @author Paolo Masci
 * @date 2019.10.18
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
import { ProofStateNode, formatProofState } from '../common/languageUtils';
import * as path from 'path';

export class VSCodePvsSequentViewer {
    protected logFileName: string;

    constructor () {}

    activate (context: vscode.ExtensionContext) {
    }

    setLogFileName (sequentFileName: string): void {
        this.logFileName = sequentFileName;
    }
    showSequent (desc?: { proofState: ProofStateNode }) {
        // if (desc && desc.proofState) {
        //     const content: string = formatProofState(desc.proofState);
        //     // const label: string = `${desc.proofState.label}.pr`;

        //     vscode.workspace.openTextDocument({ language: 'pvs', content: content }).then((document: vscode.TextDocument) => {
        //         // vscode.window.showTextDocument(document, vscode.ViewColumn.Beside, true);
        //         vscode.window.showTextDocument(document.uri, { preserveFocus: true, preview: true, viewColumn: vscode.ViewColumn.Beside });
        //     });

        //     // const sequentView: vscode.Uri = vscode.Uri.parse(`untitled:${path.join(vscode.workspace.rootPath, "prooflog", label)}`);
        //     // const edit = new vscode.WorkspaceEdit();
        //     // edit.insert(sequentView, new vscode.Position(0, 0), content);
        //     // const success: boolean = await vscode.workspace.applyEdit(edit);
        //     // if (success) {
        //     //     const document: vscode.TextDocument = await vscode.workspace.openTextDocument(sequentView);
        //     //     vscode.window.showTextDocument(document, vscode.ViewColumn.Beside, true);
        //     // } else {
        //     //     vscode.window.showInformationMessage('[vscode-pvs-terminal-cli] Warning: unable to create sequent view :/');
        //     // }            
        // } else 
        if (this.logFileName) {
            const uri: vscode.Uri = vscode.Uri.file(this.logFileName);
            const editors: vscode.TextEditor[] = vscode.window.visibleTextEditors.filter((editor: vscode.TextEditor) => {
                return editor.document.fileName === this.logFileName;
            });
            const viewColumn: number = (editors && editors.length > 0) ? editors[0].viewColumn : vscode.ViewColumn.Beside;
            vscode.window.showTextDocument(uri, { preserveFocus: true, preview: true, viewColumn });
        } else {
            console.error(`[vscode-pvs-sequent-viewer] Warning: sequent file name is null`);
        }
    }
}