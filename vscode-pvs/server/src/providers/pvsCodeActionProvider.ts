/**
 * @module PvsCodeActionProvider
 * @author Paolo Masci
 * @date 2022.01.10
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

import { CodeAction, CodeActionContext, CodeActionKind, Command, Connection, Diagnostic, Position, Range } from "vscode-languageserver";
import { PvsTheory, quickFixReplaceCommand, QuickFixReplace, QuickFixAddImporting, quickFixAddImportingCommand, PvsContextDescriptor, LookUpTable, FileDescriptor, PvsFileDescriptor, FileList, PvsFile, PvsTypeDecl } from "../common/serverInterface";
import { nasalib_lookup_table } from "../core/nasalib-utils/nasalib-lookup-table";
import * as fsUtils from '../common/fsUtils';
import { errorCannotFindTheoryRegExp, expectingTypeRegExp, theoryRegexp } from "../common/languageUtils";
import { contextDescriptor2LookUpTable } from "../common/fsUtils";
import { PVS_BUILTIN_TYPES } from "../common/languageKeywords";

// list of all nasalib theories
const nasalib_theories: string[] = nasalib_lookup_table?.theories ? Object.keys(nasalib_lookup_table.theories) : [];
// list of all nasalib types
const nasalib_types: string[] = nasalib_lookup_table?.types ? Object.keys(nasalib_lookup_table.types) : [];
// list of the builtin types
const builtin_types: string[] = [...PVS_BUILTIN_TYPES];
/**
 * Utility function, checks if two strings are similar based on a simplified version of the N-gram algorithm (N=3)
 * The original N-gram algorithm is presented in Esko Ukkonen, "Approximate string-matching with q-grams and maximal matches", 
 * Theoretical Computer Science, Volume 92, Issue 1, 6 January 1992, Pages 191-211
 */
export function similar (str1: string, str2: string): boolean {
    const N: number = 3;
    // base cases
    if (str1 === str2) { return true; }
    if (str1.length < N || str2.length < N || Math.abs(str1.length - str2.length) > N) { return false; }

    const st1: string = str1.toLocaleLowerCase();
    let st2: string = str2.toLocaleLowerCase();

    // create ngrams for st1
    const ngrams: string[] = [];
    for (let i = 0; i < st1.length - 1; i++) {
        ngrams.push(st1.substring(i, i + N));
    }
    // remove the ngrams from a copy of st2 
    for (let i = 0; i < ngrams.length; i++) {
        st2 = st2.replace(ngrams[i], "");
    }
    // the strings are similar if the difference is less than N characters
    return st2.length < N;
}

/**
 * PvsCodeAction class, used for implementing quick-fix actions
 */
export class PvsCodeActionProvider extends fsUtils.PostTask {
    protected connection: Connection;

    /**
     * Lookup table for the current context
     */
    protected ctx: LookUpTable;

    /**
     * Constructor
     * @param pvsLanguageServer 
     */
    constructor (connection: Connection) {
        super();
		this.connection = connection;
    }

    /**
     * Returns a code action for adding a folder to the PVS library path
     */
    getCodeActionAddFolder (opt?: { msg?: string, declNotFound?: string }): CodeAction {
        opt = opt || {};
        const title: string = opt.msg ? opt.msg
            : opt.declNotFound ? `Add folder with the definition of "${opt.declNotFound}" to PVS library path`
            : `Add folder to PVS library path`;
        const command: Command = { title, command: "vscode-pvs.add-pvs-library" };
        const action: CodeAction = { title, kind: CodeActionKind.QuickFix, command };
        return action;        
    }
    /**
     * Returns a code action for viewing/editing pvs library path in vscode-pvs settings
     */
    getCodeActionEditLibraryPath (opt?: { msg?: string }): CodeAction {
        opt = opt || {};
        const title: string = `Open VSCode-PVS settings and edit the list of libraries in PVS library path`;
        const command: Command = { title, command: "vscode-pvs.view-pvs-library-path" };
        const action: CodeAction = { title, kind: CodeActionKind.QuickFix, command };
        return action;
    }
    /**
     * Returns a code action for replacing text
     */
    getCodeActionReplace (desc: { fdesc: FileDescriptor, title: string, range: Range, newText: string }): CodeAction {
        if (desc?.fdesc && desc?.range && desc.newText) {
            const params: QuickFixReplace = {
                ...desc.fdesc,
                range: desc.range,
                newText: desc.newText
            };
            const cmd: Command = {
                title: desc.title,
                command: quickFixReplaceCommand,
                arguments: [ params ]
            };
            const action: CodeAction = {
                title: desc.title,
                kind: CodeActionKind.QuickFix,
                command: cmd
            };
            return action;
        }
        return null;
    }
    /**
     * quickfix action for typecheck error "Cannot find theory"
     * - fix 1: add libName to pvs library path
     * - fix 2: view/edit pvs library path
     * - fix 3: check theories defined in the current context: if match is found suggest changing to IMPORTING libName 
     * - fix 4: check nasalib: if match is found suggest changing to IMPORTING folder@libName 
     */
    fixCannotFindTheory (file: FileDescriptor, diag: Diagnostic): CodeAction[] {
        let actions: CodeAction[] = [];
        if (diag?.message) {
            const message: string = diag.message;
            // const source: string = diag.source;
            const cannotFindTheory: RegExp = new RegExp(errorCannotFindTheoryRegExp);
            const match: RegExpMatchArray = cannotFindTheory.exec(message);
            if (match?.length > 1 && match[1]) { // && /Typecheck error/gi.test(source)) {
                // group 1 is the imported theory that cannot be found
                const libName: string = match[1];
                // fix 1: add libName to pvs library path
                const fix1: CodeAction = this.getCodeActionAddFolder({ declNotFound: libName });
                actions.push(fix1);
                // fix 2: view/edit pvs library path
                const fix2: CodeAction = this.getCodeActionEditLibraryPath();
                actions.push(fix2);
                // fix 4: check theories defined in the current context: if match is found suggest IMPORTING folder@libName                
                let candidates: string[] = nasalib_theories.filter(elem => {
                    return similar(elem, libName);
                });
                if (candidates?.length) {
                    for (let i = 0; i < candidates.length; i++) {
                        const theories: PvsTheory[] = nasalib_lookup_table?.theories[candidates[i]];
                        for (let t = 0; t < theories?.length; t++) {
                            const folder: string = theories[t].contextFolder;
                            const nasalibTheory: string = `${folder}@${candidates[i]}`;
                            const title: string = `Change "${libName}" to "${nasalibTheory}"`;
                            const fix: CodeAction = this.getCodeActionReplace({
                                fdesc:  {
                                    contextFolder: file.contextFolder,
                                    fileName: file.fileName,
                                    fileExtension: file.fileExtension
                                },
                                title, 
                                range: diag.range, 
                                newText: nasalibTheory
                            });
                            if (fix) {
                                // place this action at the front
                                actions = [ fix ].concat(actions);
                            }
                        }
                    }
                }
                // fix 3: check theories defined in the current context: if match is found suggest changing to IMPORTING libName 
                const contextTheories: string[] = this.ctx?.theories ? Object.keys(this.ctx.theories) : []
                candidates = contextTheories.filter(elem => {
                    return similar(elem, libName);
                });
                if (candidates?.length) {
                    for (let i = 0; i < candidates.length; i++) {
                        const theories: PvsTheory[] = this.ctx.theories[candidates[i]];
                        for (let t = 0; t < theories?.length; t++) {
                            const ctxTheory: string = candidates[i];
                            const title: string = `Change "${libName}" to "${ctxTheory}"`;
                            const fix: CodeAction = this.getCodeActionReplace({
                                fdesc:  {
                                    contextFolder: file.contextFolder,
                                    fileName: file.fileName,
                                    fileExtension: file.fileExtension
                                },
                                title, 
                                range: diag.range, 
                                newText: ctxTheory
                            });
                            if (fix) {
                                // place this action at the front
                                actions = [ fix ].concat(actions);
                            }
                        }
                    }
                }
            }
        }
        return actions;
    }

    /**
     * quickfix action for typecheck error "No resolution for type"
     * - fix 1: add folder containing typeName to pvs library path
     * - fix 2: Open VSCode-PVS settings and edit PVS library path
     * - fix 3: check if current context defines typeName in some theory: if match is found suggest changing to IMPORTING libName 
     * - fix 4: check if nasalib defines typeName in some theory: if a match is found suggest IMPORTING nasalibFolder@libName
     * - fix 5: check if the type is mispelled and suggest correct spelling
     */
    fixNoResolutionForType (file: FileDescriptor, diag: Diagnostic): CodeAction[] {
        let actions: CodeAction[] = [];
        if (diag?.message && file.fileContent) {
            const message: string = diag.message;
            // const source: string = diag.source;
            const expectingType: RegExp = new RegExp(expectingTypeRegExp);
            const match: RegExpMatchArray = expectingType.exec(message);
            if (diag && match?.length > 1 && match[1]) { // && /Typecheck error/gi.test(source)) {
                // group 1 is the type that cannot be found
                const typeName: string = match[1];
                // fix 1: add libName to pvs library path
                const fix1: CodeAction = this.getCodeActionAddFolder({ declNotFound: typeName });
                actions.push(fix1);
                // fix 2: view/edit pvs library path
                const fix2: CodeAction = this.getCodeActionEditLibraryPath();
                actions.push(fix2);
                // fix 3: check types defined in the current context: if match is found suggest IMPORTING libName
                let candidates: string[] = this.ctx?.types ? Object.keys(this.ctx?.types).filter(elem => {
                    return elem === typeName;
                }) : null;
                if (candidates?.length) {
                    for (let i = 0; i < candidates.length; i++) {
                        const types: PvsTheory[] = this.ctx?.types[candidates[i]];
                        for (let t = 0; t < types?.length; t++) {
                            const theory: string = types[t].theoryName;
                            const fix: string = `IMPORTING ${theory}`;
                            const currentTheory: string = fsUtils.findTheoryName(file.fileContent, diag.range.start.line);
                            if (currentTheory) {
                                // find location where to place the IMPORTING
                                // group 1 is the theory name
                                const regexp: RegExp = new RegExp(theoryRegexp);
                                let matchTheory: RegExpMatchArray = null;
                                let importingLine: number = -1;
                                let importingCharacter: number = -1;
                                while (matchTheory = regexp.exec(file.fileContent)) {
                                    if (matchTheory?.length > 1 && matchTheory[1] === currentTheory) {
                                        const frag: string = file.fileContent.substring(0, matchTheory.index + matchTheory[0].length);
                                        const lines: string[] = frag.split("\n");
                                        importingLine = lines.length - 1; // line is 0-based
                                        importingCharacter = lines[importingLine].indexOf("BEGIN") + 6;
                                        break;
                                    }
                                }
                                if (importingLine !== -1 && importingCharacter !== -1) {
                                    const msg: string = `Import "${candidates[i]}" from theory "${theory}"`;
                                    const params: QuickFixAddImporting = {
                                        contextFolder: file.contextFolder,
                                        fileName: file.fileName,
                                        fileExtension: file.fileExtension,
                                        position: { line: importingLine, character: importingCharacter },
                                        newImporting: fix
                                    };
                                    const cmd3: Command = {
                                        title: msg,
                                        command: quickFixAddImportingCommand,
                                        arguments: [ params ]
                                    };
                                    const action3: CodeAction = {
                                        title: msg,
                                        kind: CodeActionKind.QuickFix,
                                        command: cmd3
                                    };
                                    actions = [ action3 ].concat(actions);
                                } else {
                                    console.warn(`[pvs-code-action-provider] Warning: could not determine location of IMPORTING`);
                                }
                            } else {
                                console.warn(`[pvs-code-action-provider] Warning: could not determine current theory name`);
                            }
                        }
                    }
                }
                // fix 4: check if nasalib defines the type in some theory: if a match is found suggest IMPORTING nasalibFolder@libName 
                candidates = nasalib_types?.filter(elem => {
                    return elem.toLocaleLowerCase() === typeName.toLocaleLowerCase();
                });
                if (candidates?.length) {
                    for (let i = 0; i < candidates.length; i++) {
                        const types: PvsTheory[] = nasalib_lookup_table?.types[candidates[i]];
                        for (let t = 0; t < types?.length; t++) {
                            const folder: string = types[t].contextFolder;
                            const theory: string = types[t].theoryName;
                            const fix: string = `IMPORTING ${folder}@${theory}`;
                            const currentTheory: string = fsUtils.findTheoryName(file.fileContent, diag.range.start.line);
                            if (currentTheory) {
                                // find location where to place the IMPORTING
                                // group 1 is the theory name
                                const regexp: RegExp = new RegExp(theoryRegexp);
                                let matchTheory: RegExpMatchArray = null;
                                let importingLine: number = -1;
                                let importingCharacter: number = -1;
                                while (matchTheory = regexp.exec(file.fileContent)) {
                                    if (matchTheory?.length > 1 && matchTheory[1] === currentTheory) {
                                        const frag: string = file.fileContent.substring(0, matchTheory.index + matchTheory[0].length);
                                        const lines: string[] = frag.split("\n");
                                        importingLine = lines.length;
                                        importingCharacter = lines[importingLine - 1].indexOf("BEGIN") + 6;
                                        break;
                                    }
                                }
                                if (importingLine !== -1 && importingCharacter !== -1) {
                                    const msg: string = `Import "${candidates[i]}" from theory "${theory}" (NASALib)`;
                                    const params: QuickFixAddImporting = {
                                        contextFolder: file.contextFolder,
                                        fileName: file.fileName,
                                        fileExtension: file.fileExtension,
                                        position: { line: importingLine, character: importingCharacter },
                                        newImporting: fix
                                    };
                                    const cmd3: Command = {
                                        title: msg,
                                        command: quickFixAddImportingCommand,
                                        arguments: [ params ]
                                    };
                                    const action3: CodeAction = {
                                        title: msg,
                                        kind: CodeActionKind.QuickFix,
                                        command: cmd3
                                    };
                                    actions = [ action3 ].concat(actions);
                                } else {
                                    console.warn(`[pvs-code-action-provider] Warning: could not determine location of IMPORTING`);
                                }
                            } else {
                                console.warn(`[pvs-code-action-provider] Warning: could not determine current theory name`);
                            }
                        }
                    }
                }
                // fix 5: check if the type is mispelled and suggest correct spelling
                candidates = this.ctx?.types ? Object.keys(this.ctx?.types).filter(elem => {
                    return elem !== typeName // type mispelled
                            && similar(elem, typeName); // but similar
                }) : [];
                // check also builtin types
                candidates = candidates.concat(builtin_types.filter(elem => {
                    return elem.toLocaleLowerCase() === typeName.toLowerCase(); // wrong capitalization
                }));
                if (candidates?.length) {
                    for (let i = 0; i < candidates.length; i++) {
                        const type: string = candidates[i];
                        const title: string = `Change "${typeName}" to "${type}"`;
                        const fix: CodeAction = this.getCodeActionReplace({
                            fdesc:  {
                                contextFolder: file.contextFolder,
                                fileName: file.fileName,
                                fileExtension: file.fileExtension
                            },
                            title, 
                            range: diag.range, 
                            newText: type
                        });
                        if (fix) {
                            // place this action at the front
                            actions = [ fix ].concat(actions);
                        }
                    }
                }
            }
        }
        return actions;
    }

    /**
	 * Standard API of the language server, provides a code action
	 * @param document Text document requiring codeaction
	 * @param range Current range selected with the cursor, lines are 0-based, cols and 1-based
	 */
    provideCodeAction(document: { txt: string, uri: string }, range: Range, context: CodeActionContext): (Command | CodeAction)[] {
        if (document?.txt && document?.uri && range && context) {
            let actions: CodeAction[] = [];
            if (context.diagnostics?.length) {
                const contextFolder: string = fsUtils.getContextFolder(document.uri);
                const fileName: string = fsUtils.getFileName(document.uri);
                const fileExtension: string = fsUtils.getFileExtension(document.uri);
                const fdesc: FileDescriptor = {
                    contextFolder, 
                    fileName, 
                    fileExtension,
                    fileContent: document.txt
                };
                for (let i = 0; i < context.diagnostics.length; i++) {
                    const diag: Diagnostic = context.diagnostics[i];
                    // fixes for importing errors
                    if (diag.message) {
                        // fix for 'cannot find theory'
                        const cannotFindTheory: RegExp = new RegExp(errorCannotFindTheoryRegExp);
                        if (cannotFindTheory.test(diag.message)) {
                            const fixes: CodeAction[] = this.fixCannotFindTheory(fdesc, diag);
                            actions = actions.concat(fixes);
                        }
                        // fix for 'no resolution for type'
                        const expectingType: RegExp = new RegExp(expectingTypeRegExp);
                        if (expectingType.test(diag.message)) {
                            const fixes: CodeAction[] = this.fixNoResolutionForType(fdesc, diag);
                            actions = actions.concat(fixes);
                        }
                    }
                }
            }
            return actions;
        }
        return null;
    }

    /**
     * Internal task for updating the lookup table
     */
    protected async updateWorkspaceDescriptorTask (cdesc: PvsContextDescriptor): Promise<void> {
        // compile theories and formulas
        this.ctx = contextDescriptor2LookUpTable(cdesc);
        if (this.ctx) {
            this.ctx.types = await fsUtils.typesLookUpTable(cdesc);
        }
    }
    /**
     * Utility function, updates the lookup table for the current workspace 
     * based on the context descriptor passed as argument
     */
    updateWorkspaceDescriptor (cdesc: PvsContextDescriptor): void {
        this.postTask(() => {
            this.updateWorkspaceDescriptorTask(cdesc);
        });
    }

    /**
     * Utility function, clears the context descriptor
     */
    clearWorkspaceDescriptor (): void {
        this.ctx = null;
        clearTimeout(this.timer);
    }
}