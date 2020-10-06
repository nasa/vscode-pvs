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
import * as utils from '../common/languageUtils';
import * as os from 'os';
import { TheoryItem } from "../views/vscodePvsWorkspaceExplorer";
import { PvsTheory, PvsFile } from '../common/serverInterface';


/**
 * Returns the context folder of the editor
 */
export function getEditorContextFolder () : string {
    return (vscode.window.activeTextEditor) ? fsUtils.getContextFolder(vscode.window.activeTextEditor.document.fileName) : null;
}

export function getDefaultContextFolder (): string {
    const workspaces: string = vscode.workspace.getConfiguration().get("pvs.pvsWorkspaces");
    if (workspaces) {
        const pvsWorkspaces: string = path.join(fsUtils.HOME_DIR, workspaces);
        if (fsUtils.folderExists(pvsWorkspaces)) {
            return pvsWorkspaces;
        }
    }
    return null;
}
export async function createDefaultPvsWorkspacesDirectory (): Promise<string> {
    const workspaces: string = vscode.workspace.getConfiguration().get("pvs.pvsWorkspaces");
    if (workspaces) {
        const pvsWorkspaces: string = path.join(fsUtils.HOME_DIR, workspaces);
        if (!fsUtils.folderExists(pvsWorkspaces)) {
            await fsUtils.createFolder(pvsWorkspaces);
            vscode.window.showInformationMessage(
`Welcome to VSCode-PVS!

VSCode-PVS has automatically created a folder '${workspaces}' under your home directory.

You can use this folder to develop your PVS theories.`,
                { modal: true }
            );
        }
        return pvsWorkspaces;
    }
    return null;
}

/**
 * Utility function, shows a text document in the editor
 * @param content 
 */
export function showTextDocument (desc: { 
    contextFolder: string, 
    fileName: string, 
    fileExtension: string 
}, opt?: { 
    viewColumn?: vscode.ViewColumn, 
    selection?: vscode.Range 
}): void {
    opt = opt || {};
    const viewColumn: vscode.ViewColumn = opt.viewColumn || ((vscode.window.activeTextEditor) ? vscode.window.activeTextEditor.viewColumn : vscode.ViewColumn.Active);
    if (desc) {
        const uri: vscode.Uri = vscode.Uri.file(path.join(desc.contextFolder, `${desc.fileName}${desc.fileExtension}`));
        vscode.window.showTextDocument(uri, { preserveFocus: true, preview: true, viewColumn, selection: opt.selection });
    }
}

/**
 * Utility function, previews a text document in the editor
 * @param content 
 */
export async function previewTextDocument (name: string, content: string, opt?: { contextFolder?: string, viewColumn?: vscode.ViewColumn }): Promise<void> {
    opt = opt || {};
    let viewColumn: vscode.ViewColumn = opt.viewColumn || ((vscode.window.activeTextEditor) ? vscode.window.activeTextEditor.viewColumn : vscode.ViewColumn.Active);

    // vscode.workspace.openTextDocument({ language: 'pvs', content: content }).then((document: vscode.TextDocument) => {
    //     // vscode.window.showTextDocument(document, vscode.ViewColumn.Beside, true);
    //     vscode.window.showTextDocument(document.uri, { preserveFocus: true, preview: true, viewColumn });
    // });
    // const preview: vscode.Uri = vscode.Uri.parse(`untitled:${path.join(vscode.workspace.rootPath, "pvsbin", "preview")}`);
    
    const folder: string = opt.contextFolder || vscode.workspace.rootPath || os.homedir();
    const fname: string = path.join(folder, name);
    const preview: vscode.Uri = vscode.Uri.file(fname);
    // const preview: vscode.Uri = vscode.Uri.parse(`untitled:${fname}`);

    const edit: vscode.WorkspaceEdit = new vscode.WorkspaceEdit();
    edit.createFile(preview, { overwrite: true });
    edit.insert(preview, new vscode.Position(0, 0), content);
    let success: boolean = await vscode.workspace.applyEdit(edit);
    // FIXME: applyEdit fails if the document is already open and active in the editor, understand why this is the case.
    if (!success) {
        success = await vscode.workspace.applyEdit(edit);
    }    
    if (success) {
        const document: vscode.TextDocument = await vscode.workspace.openTextDocument(preview);
        // const document: vscode.TextDocument = await vscode.workspace.openTextDocument(preview);
        vscode.window.showTextDocument(document, { viewColumn, preserveFocus: true, preview: true });
        await document.save();
    }
}


/**
 * Utility function, creates a text document in the editor with the given content
 * @param content 
 */
export async function createTextDocument (name: string, content: string, opt?: { contextFolder?: string, viewColumn?: vscode.ViewColumn }): Promise<vscode.Uri> {
    opt = opt || {};
    let viewColumn: vscode.ViewColumn = opt.viewColumn || ((vscode.window.activeTextEditor) ? vscode.window.activeTextEditor.viewColumn : vscode.ViewColumn.Active);
    
    const folder: string = opt.contextFolder || vscode.workspace.rootPath || os.homedir();
    const fname: string = path.join(folder, name);
    const preview: vscode.Uri = vscode.Uri.file(fname);
    // const preview: vscode.Uri = vscode.Uri.parse(`untitled:${fname}`);

    const edit: vscode.WorkspaceEdit = new vscode.WorkspaceEdit();
    edit.createFile(preview, { overwrite: true });
    edit.insert(preview, new vscode.Position(0, 0), content);
    let success: boolean = await vscode.workspace.applyEdit(edit);
    // FIXME: applyEdit fails if the document is already open and active in the editor, understand why this is the case.
    if (!success) {
        success = await vscode.workspace.applyEdit(edit);
    }
    return preview;
}

/**
 * Utility function, shows a dialog that allows the user to select the pvs installation folder in the file system
 * @param pvsPath 
 */
export async function addPvsLibraryFolderWizard (): Promise<boolean> {
    const selection: vscode.Uri[] = await vscode.window.showOpenDialog({
        canSelectFiles: false,
        canSelectFolders: true,
        canSelectMany: false,
        openLabel: "Add selected folder to PVS library path"
    });
    let success: boolean = false;
    if (selection && selection.length === 1) {
        const path: string = selection[0].fsPath;
        success = await addPvsLibraryFolder(path);
        if (success) {
            vscode.window.showInformationMessage(`Folder ${path} added to PVS library path`);
        } else {
            vscode.window.showInformationMessage(`Folder ${path} already in PVS library path`);
        }
    }
    return success;
}

export async function clearPvsLibraryPath (): Promise<void> {
    await vscode.workspace.getConfiguration().update("pvs.pvsLibraryPath", undefined, vscode.ConfigurationTarget.Global);
}
export async function getPvsLibraryPath (): Promise<string> {
    return await vscode.workspace.getConfiguration().get("pvs.pvsLibraryPath")
}
export async function addPvsLibraryFolder (path: string): Promise<boolean> {
    if (path) {
        path = (path.endsWith("/")) ? path : `${path}/`;
        const pvsLibraryPath: string = vscode.workspace.getConfiguration().get("pvs.pvsLibraryPath")
        const libs: string[] = utils.decodePvsLibraryPath(pvsLibraryPath);
        if (!libs.includes(path)) {
            const newPvsLibraryPath: string = utils.createPvsLibraryPath(libs.concat([ path ]));
            await vscode.workspace.getConfiguration().update("pvs.pvsLibraryPath", newPvsLibraryPath, vscode.ConfigurationTarget.Global);
            return true;
        }
    }
    return false;
}
/**
 * Opens a folder and adds the folder to file explorer
 */
export async function cleanPvsWorkspace (): Promise<void> {
    if (vscode.workspace.workspaceFolders) {
        let nCleaned: number = 0;
        for (let i = 0; i < vscode.workspace.workspaceFolders.length; i++) {
            const contextFolder: string = vscode.workspace.workspaceFolders[i].uri.path;
            if (contextFolder) {
                nCleaned += await fsUtils.cleanBin(contextFolder, { keepTccs: true, recursive: fsUtils.MAX_RECURSION });
            }
        }
        const name: string = (vscode.workspace.name && !vscode.workspace.name.startsWith("Untitled")) ? vscode.workspace.name 
            : vscode.workspace.workspaceFolders.length ? vscode.workspace.workspaceFolders[0].name
            : ""
        const msg: string = (name) ? `${nCleaned} folders cleaned in workspace ${name}` : `${nCleaned} folders cleaned.`;
        vscode.window.showInformationMessage(msg);
    } else {
        vscode.window.showInformationMessage(`Nothing to clean (no folder opened)`);
    }
}
// export async function createCodeWorkspace (contextFolder: string): Promise<boolean> {
//     const contextFolderName: string = fsUtils.getContextFolderName(contextFolder);
//     const codeWorkspaceFile: string = path.join(contextFolder, `${contextFolderName}.code-workspace`);
//     const uri: vscode.Uri = vscode.Uri.file(codeWorkspaceFile);
//     const edit: vscode.WorkspaceEdit = new vscode.WorkspaceEdit();
//     edit.createFile(uri, { overwrite: true });
//     edit.insert(uri, new vscode.Position(0, 0), JSON.stringify({ folders: [{
//         name: contextFolderName,
//         path: "."
//     }] }, null, " "));
//     let success: boolean = await vscode.workspace.applyEdit(edit);
//     // FIXME: applyEdit fails if the document is already open and active in the editor, understand why this is the case.
//     if (!success) {
//         success = await vscode.workspace.applyEdit(edit);
//     }
//     return success;
// }
/**
 * Opens a folder and adds the folder to file explorer
 */
export async function openWorkspace (): Promise<void> {
    const selection: vscode.Uri[] = await vscode.window.showOpenDialog({
        canSelectFiles: false,
        canSelectFolders: true,
        canSelectMany: false,
        openLabel: "Open"
    });
    if (selection && selection.length === 1) {
        const contextFolder: string = selection[0].path;
        const contextFolderUri: vscode.Uri = vscode.Uri.file(contextFolder);
        // add folder to workspace
        // if (!vscode.workspace.getWorkspaceFolder(contextFolderUri)) {
            // save and close all open files in the editor
            await vscode.workspace.saveAll();
            // vscode.commands.executeCommand("workbench.action.files.saveAll");
            vscode.commands.executeCommand("workbench.action.closeAllGroups");
            // open the new workspace
            vscode.commands.executeCommand('vscode.openFolder', contextFolderUri);
            // const deleteCount: number = vscode.workspace.workspaceFolders ? vscode.workspace.workspaceFolders.length : 0;
            // vscode.workspace.updateWorkspaceFolders(0, deleteCount, { uri: contextFolderUri });

            // await createCodeWorkspace(contextFolder);
        // }
    }
}
/**
 * Opens a pvs file in the editor and adds the containing folder in file explorer
 */
export async function openPvsFile (opt?: PvsFile): Promise<void> {
    const selection: vscode.Uri[] = (opt) ? [ vscode.Uri.file(fsUtils.desc2fname(opt)) ] : await vscode.window.showOpenDialog({
        canSelectFiles: true,
        canSelectFolders: false,
        canSelectMany: false,
        openLabel: "Open PVS File",
        filters: {
            "PVS": [ ".pvs" ]
        }
    });
    if (selection && selection.length === 1) {
        const fname: string = selection[0].path;
        const contextFolder: string = fsUtils.getContextFolder(fname);
        const fileUri: vscode.Uri = vscode.Uri.file(fname);
        const contextFolderUri: vscode.Uri = vscode.Uri.file(contextFolder);
        // add folder to workspace
        if (!vscode.workspace.getWorkspaceFolder(contextFolderUri)) {
            vscode.workspace.updateWorkspaceFolders(vscode.workspace.workspaceFolders ? vscode.workspace.workspaceFolders.length : 0, null, { uri: contextFolderUri });
        }
        vscode.window.showTextDocument(fileUri, { preserveFocus: true });
    }
}
/**
 * Opens a pvs file in the editor and adds the containing folder in file explorer
 */
export async function openPvsFileOrWorkspace (): Promise<string> {
    const selection: vscode.Uri[] = await vscode.window.showOpenDialog({
        canSelectFiles: true,
        canSelectFolders: true,
        canSelectMany: false,
        openLabel: "Open",
        filters: {
            "PVS": [ ".pvs" ]
        }
    });
    if (selection && selection.length === 1) {
        const fname: string = (fsUtils.isPvsFile(selection[0].path)) ? selection[0].path : null;
        const contextFolder: string = (fname) ? fsUtils.getContextFolder(fname) : selection[0].path;
        const contextFolderUri: vscode.Uri = vscode.Uri.file(contextFolder);
        // add folder to workspace
        if (!vscode.workspace.getWorkspaceFolder(contextFolderUri)) {
            const start: number = 0
            const end: number = (vscode.workspace.workspaceFolders) ? vscode.workspace.workspaceFolders.length - 1 : 0;
            // save and close all open files in the editor
            vscode.commands.executeCommand("workbench.action.files.saveAll");
            vscode.commands.executeCommand("workbench.action.closeAllGroups");
            vscode.workspace.updateWorkspaceFolders(start, end, { uri: contextFolderUri });
        }
        if (fname) {
            const fileUri: vscode.Uri = vscode.Uri.file(fname);
            vscode.window.showTextDocument(fileUri, { preserveFocus: true });
        }
        return contextFolder;
    }
    return null;
}
/**
 * Opens a proof file and returns the file content
 */
export async function openProofFile (opt?: { defaultFolder?: string, defaultExtension?: ".prf" | ".prl" | ".jprf" }): Promise<{
    fileName: string,
    fileExtension: string,
    contextFolder: string
} | null> {
    opt = opt || {};
    let filters: { [key: string]: string[] } = {
        ".prf": [ ".prf" ],
        ".prl": [ ".prl" ],
        ".jprf":[ ".jprf" ]
    };
    if (opt.defaultExtension) {
        const customFilter: { [key: string]: string[] } = {};
        customFilter[opt.defaultExtension] = filters[opt.defaultExtension];
        filters = customFilter;
    }
    const selection: vscode.Uri[] = await vscode.window.showOpenDialog({
        canSelectFiles: true,
        canSelectFolders: false,
        canSelectMany: false,
        openLabel: "Open Proof File",
        defaultUri: (opt.defaultFolder) ? vscode.Uri.parse(opt.defaultFolder) : null,
        filters
    });
    if (selection && selection.length === 1) {
        const fname: string = selection[0].path;
        const fileName: string = fsUtils.getFileName(fname);
        const fileExtension: string = fsUtils.getFileExtension(fname);
        const contextFolder: string = fsUtils.getContextFolder(fname);
        return {
            fileName,
            fileExtension,
            contextFolder
        };
    }
    return null;
}

export async function getPvsTheory (resource: PvsTheory | TheoryItem | { path: string }): Promise<PvsTheory | null> {
	if (resource) {
        if (resource["contextValue"]) {
            return {
                contextFolder: (<TheoryItem> resource).contextFolder,
                fileName: (<TheoryItem> resource).fileName,
                fileExtension: (<TheoryItem> resource).fileExtension,
                theoryName: (<TheoryItem> resource).theoryName
            };    
        } else if (resource["path"]) {
            const content: string = await fsUtils.readFile(resource["path"]);
            if (content) {
                // const document: vscode.TextDocument = window.activeTextEditor.document;
                const line: number = (vscode.window.activeTextEditor && vscode.window.activeTextEditor.selection && vscode.window.activeTextEditor.selection.active) ?
                    vscode.window.activeTextEditor.selection.active.line : 0;
                const theoryName: string = utils.findTheoryName(content, line);
                return {
                    contextFolder: fsUtils.getContextFolder(resource["path"]),
                    fileName: fsUtils.getFileName(resource["path"]),
                    fileExtension: fsUtils.getFileExtension(resource["path"]),
                    theoryName
                };
            }
		} else if (resource["contextFolder"]) {
            resource = <PvsTheory> resource;
            if (!resource["theoryName"]) {
                resource.fileExtension = (resource.fileExtension === ".summary") ? ".pvs" : resource.fileExtension;
                const content: string = await fsUtils.readFile(fsUtils.desc2fname(resource));
                if (content) {
                    const line: number = (vscode.window.activeTextEditor && vscode.window.activeTextEditor.selection && vscode.window.activeTextEditor.selection.active) ?
                        vscode.window.activeTextEditor.selection.active.line : 0;
                    const theoryName: string = utils.findTheoryName(content, line);
                    return {
                        contextFolder: resource.contextFolder,
                        fileName: resource.fileName,
                        fileExtension: resource.fileExtension,
                        theoryName
                    };
                }
            }
            return resource;
        }
    }
    return null;
}

export function showReleaseNotes (): void {
    const fileUri: vscode.Uri = vscode.Uri.file(path.join(__dirname, "..", "..", "..", "README.md"));
    vscode.commands.executeCommand('markdown.showPreview', fileUri);
}
