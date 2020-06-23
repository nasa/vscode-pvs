/**
 * @module vscode-utils
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

/**
 * Returns the context folder of the editor
 */
export function getEditorContextFolder () : string {
    return (vscode.window.activeTextEditor) ? fsUtils.getContextFolder(vscode.window.activeTextEditor.document.fileName) : null;
}

/**
 * Utility function, shows a text document in the editor
 * @param content 
 */
export function showTextDocument (desc: { contextFolder: string, fileName: string, fileExtension: string }, opt?: { viewColumn?: vscode.ViewColumn }): void {
    opt = opt || {};
    const viewColumn: vscode.ViewColumn = opt.viewColumn || vscode.ViewColumn.Active;
    if (desc) {
        const uri: vscode.Uri = vscode.Uri.file(path.join(desc.contextFolder, `${desc.fileName}${desc.fileExtension}`));
        vscode.window.showTextDocument(uri, { preserveFocus: true, preview: true, viewColumn });
    }
}

/**
 * Utility function, previews a text document in the editor
 * @param content 
 */
export async function previewTextDocument (name: string, content: string, opt?: { contextFolder?: string, viewColumn?: vscode.ViewColumn }): Promise<boolean> {
    opt = opt || {};
    let viewColumn: vscode.ViewColumn = opt.viewColumn || vscode.ViewColumn.Active;

    // vscode.workspace.openTextDocument({ language: 'pvs', content: content }).then((document: vscode.TextDocument) => {
    //     // vscode.window.showTextDocument(document, vscode.ViewColumn.Beside, true);
    //     vscode.window.showTextDocument(document.uri, { preserveFocus: true, preview: true, viewColumn });
    // });
    // const preview: vscode.Uri = vscode.Uri.parse(`untitled:${path.join(vscode.workspace.rootPath, "pvsbin", "preview")}`);
    
    const folder: string = opt.contextFolder || vscode.workspace.rootPath;
    const fname: string = path.join(folder, name);
    const preview: vscode.Uri = vscode.Uri.file(fname);

    const edit: vscode.WorkspaceEdit = new vscode.WorkspaceEdit();
    edit.deleteFile(preview, { ignoreIfNotExists: true });
    edit.createFile(preview, { overwrite: true });
    edit.insert(preview, new vscode.Position(0, 0), content);
    const success: boolean = await vscode.workspace.applyEdit(edit);
    // FIXME: applyEdit fails if the document is already open and active in the editor, understand why this is the case.
    if (success) {
        const document: vscode.TextDocument = await vscode.workspace.openTextDocument(preview);
        vscode.window.showTextDocument(document, { viewColumn, preserveFocus: true, preview: true });
        await document.save();
        return true;
    }
    // vscode.window.showInformationMessage(`[vscode-utils] Warning: unable to show ${name} in the editor`);
    return false;
}

