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
import { nasalib_lookup_table } from "../common/nasalib-lookup-table";
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
 * Utility function, checks if two strings are similar. String similarity is defined based on a q-gram distance
 * An example algorithm based on q-gram distance is presented in Esko Ukkonen, "Approximate string-matching with q-grams and maximal matches", 
 * Theoretical Computer Science, Volume 92, Issue 1, 6 January 1992, Pages 191-211
 */
export function similar (str1: string, str2: string): boolean {
    const N: number = 3;

    // st1 is the shortest string
    const st1: string = str1.length < str2.length ? str1.toLocaleLowerCase() : str2.toLocaleLowerCase();
    const st2: string = str1.length < str2.length ? str2.toLocaleLowerCase() : str1.toLocaleLowerCase();

    // base case 1: strings are identical, case insensitive -> strings are similar
    if (st1 === st2) { return true; }
    // base case 2: strings are shorter than N, or their length is way too different -> strings are not similar
    if (st1.length < N || st2.length < N || st2.length - st1.length > 2 * N) { return false; }
    
    // create ngrams for st1 and st2
    const ngrams_st1: string[] = [];
    for (let i = 0; i <= st1.length - N; i++) {
        ngrams_st1.push(st1.substring(i, i + N));
    }
    const ngrams_st2: string[] = [];
    for (let i = 0; i <= st2.length - N; i++) {
        ngrams_st2.push(st2.substring(i, i + N));
    }
    // compute similarity level by checking the number of matches between ngrams_st1 and ngram_st2
    let sim_level: number = 0;
    for (let i = 0; i < ngrams_st1.length; i++) {
        if (ngrams_st2.includes(ngrams_st1[i])) {
            sim_level++;
        }
    }
    // the strings are similar if more than half of the ngrams match
    return sim_level >= (ngrams_st2.length / 2);
}

/**
 * PvsCodeAction class, used for implementing quick-fix actions
 */
export class PvsCodeActionProvider extends fsUtils.PostTask {
    /**
     * Connection with the client
     */
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
     * Internal function, creates a code action for adding a folder to the PVS library path
     */
    protected getCodeActionAddFolder (opt?: { msg?: string, declNotFound?: string }): CodeAction {
        opt = opt || {};
        const title: string = opt.msg ? opt.msg
            : opt.declNotFound ? `Add folder with the definition of "${opt.declNotFound}" to PVS library path`
            : `Add folder to PVS library path`;
        const command: Command = { title, command: "vscode-pvs.add-pvs-library" };
        const action: CodeAction = { title, kind: CodeActionKind.QuickFix, command };
        return action;        
    }
    /**
     * Internal function, creates a code action for viewing/editing pvs library path in vscode-pvs settings
     */
    protected getCodeActionEditLibraryPath (opt?: { msg?: string }): CodeAction {
        opt = opt || {};
        const title: string = `Open VSCode-PVS settings and edit the list of libraries in PVS library path`;
        const command: Command = { title, command: "vscode-pvs.view-pvs-library-path" };
        const action: CodeAction = { title, kind: CodeActionKind.QuickFix, command };
        return action;
    }
    /**
     * Internal function, creates a code action for replacing text
     */
    protected getCodeActionReplace (desc: { fdesc: FileDescriptor, title: string, range: Range, newText: string }): CodeAction {
        if (desc?.fdesc && desc?.range && desc.newText && desc?.title) {
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
     * Internal function, creates a code action for importing a theory
     */
    protected getCodeActionImportTheory (desc: { fdesc: FileDescriptor, title: string, range: Range, newText: string }): CodeAction {
        if (desc?.fdesc && desc?.range && desc.newText && desc?.title) {
            const currentTheory: string = fsUtils.findTheoryName(desc.fdesc.fileContent, desc.range.start.line);
            if (currentTheory) {
                // find location where to place the IMPORTING
                // group 1 is the theory name
                const regexp: RegExp = new RegExp(theoryRegexp);
                let matchTheory: RegExpMatchArray = null;
                let importingLine: number = -1;
                let importingCharacter: number = -1;
                while (matchTheory = regexp.exec(desc.fdesc.fileContent)) {
                    if (matchTheory?.length > 1 && matchTheory[1] === currentTheory) {
                        const frag: string = desc.fdesc.fileContent.substring(0, matchTheory.index + matchTheory[0].length);
                        const lines: string[] = frag.split("\n");
                        importingLine = lines.length - 1; // line is 0-based
                        importingCharacter = lines[importingLine].indexOf("BEGIN") + 6; // place the importing after BEGIN
                        break;
                    }
                }
                if (importingLine !== -1 && importingCharacter !== -1) {
                    const params: QuickFixAddImporting = {
                        contextFolder: desc.fdesc.contextFolder,
                        fileName: desc.fdesc.fileName,
                        fileExtension: desc.fdesc.fileExtension,
                        position: { line: importingLine, character: importingCharacter },
                        newImporting: desc.newText
                    };
                    const cmd: Command = {
                        title: desc.title,
                        command: quickFixAddImportingCommand,
                        arguments: [ params ]
                    };
                    const action: CodeAction = {
                        title: desc.title,
                        kind: CodeActionKind.QuickFix,
                        command: cmd
                    };
                    return action;
                } else {
                    console.warn(`[pvs-code-action-provider] Warning: could not determine location of IMPORTING`);
                }
            } else {
                console.warn(`[pvs-code-action-provider] Warning: could not determine current theory name`);
            }
        }
        return null;
    }

    /**
     * Quickfix action for typecheck error "Cannot find theory"
     * This typecheck error occurs when the typechecker cannot find the declaration 
     * of a theory theoryName used in the IMPORTING statement
     * - Case 1: Check if theoryName, or a name similar to theoryName, is defined in nasalib
     *           Name similarity is based on a q-gram distance measure (e.g., "Vect3" is similar to "Vect3D" and "vect")
     *           if a match is found, suggest changing theoryName to nasalibFolder@theoryName
     * - Case 2: Check if theoryName is mispelled
     *           if similar theory names are defined in the current context
     *           for each similar theory name thName, suggest changing theoryName to thName
     * - Case 3: theoryName is defined in an external library, but the library is not in the PVS_LIBRARY_PATH
     *           suggest adding a folder to the PVS_LIBRARY_PATH, or viewing/editing PVS_LIBRARY_PATH
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
                const theoryName: string = match[1];

                // Case 1: Check if theoryName, or a name similar to theoryName, is defined in nasalib
                //         if a match is found, suggest IMPORTING nasalibFolder@theoryName
                let candidates: string[] = nasalib_theories.filter(elem => {
                    // try to split theoryName in the two parts (libName@theoryName)
                    if (theoryName.includes("@")) {
                        const originals: string[] = theoryName.split("@");
                        const theories: PvsTheory[] = nasalib_lookup_table.theories[elem];
                        // nasalib theories have unique names (there are possibly only few isolated exceptions)
                        // it's sufficient to check the first entry 
                        const folder: string = theories?.length ? theories[0]?.contextFolder : null;
                        if (folder) {
                            return similar(originals[1], elem) && similar(originals[0], folder);
                        }
                        return similar(originals[1], elem);
                    }
                    return similar(elem, theoryName);
                });
                if (candidates?.length) {
                    for (let i = 0; i < candidates.length; i++) {
                        const theories: PvsTheory[] = nasalib_lookup_table?.theories[candidates[i]];
                        for (let t = 0; t < theories?.length; t++) {
                            const nasalibFolder: string = theories[t].contextFolder;
                            const nasalibTheoryName: string = candidates[i];
                            const fullTheoryName: string = `${nasalibFolder}@${nasalibTheoryName}`;
                            const title: string = `Change "${theoryName}" to "${fullTheoryName}"`;
                            const fix: CodeAction = this.getCodeActionReplace({
                                fdesc:  {
                                    contextFolder: file.contextFolder,
                                    fileName: file.fileName,
                                    fileExtension: file.fileExtension
                                },
                                title, 
                                range: diag.range, 
                                newText: fullTheoryName
                            });
                            if (fix) {
                                // place this action at the front
                                actions = [ fix ].concat(actions);
                            }
                        }
                    }
                }

                // Check if theoryName is mispelled
                // if similar theory names are defined in the current context
                // for each similar theory name thName, suggest changing theoryName to thName
                candidates = Object.keys(this.ctx?.theories || {}).concat(nasalib_theories).filter(elem => {
                    return similar(elem, theoryName);
                });
                if (candidates?.length) {
                    for (let i = 0; i < candidates.length; i++) {
                        const theories: PvsTheory[] = this.ctx.theories[candidates[i]];
                        for (let t = 0; t < theories?.length; t++) {
                            const thName: string = candidates[i];
                            const title: string = `Change "${theoryName}" to "${thName}"`;
                            const fix: CodeAction = this.getCodeActionReplace({
                                fdesc:  {
                                    contextFolder: file.contextFolder,
                                    fileName: file.fileName,
                                    fileExtension: file.fileExtension
                                },
                                title, 
                                range: diag.range, 
                                newText: thName
                            });
                            if (fix) { actions.push(fix); }
                        }
                    }
                }

                // Case 3: theoryName is defined in an external library, but the library is not in the PVS_LIBRARY_PATH
                //         suggest adding a folder to the PVS_LIBRARY_PATH, or viewing/editing PVS_LIBRARY_PATH
                actions = actions.concat([
                    this.getCodeActionAddFolder({ declNotFound: theoryName }),
                    this.getCodeActionEditLibraryPath()
                ]);                
            }
        }
        return actions;
    }

    /**
     * Quickfix action for typecheck error "No resolution for type"
     * This typecheck error occurs when the typechecker cannot find the declaration of a type typeName used in the theory
     * - Case 1: Check if typeName is defined in the current context
     *           if match is found, suggest IMPORTING theoryName
     * - Case 2: Check if typeName is defined in nasalib
     *           if match is found, suggest IMPORTING nasalibFolder@theoryName
     * - Case 3: Check if typeName is mispelled, when IMPORTING fixes cannot be found
     *           A similar typeName might be found in the current context, in nasalib, or among builtin types
     *           Name similarity is based on a q-gram distance measure (e.g., "Vect3" is similar to "Vect3D" and "vect")
     *           If matches are found, suggest replacing typeName with the identified matches
     *           For builtin types, the matched name must be of the same length of typeName (this is done to avoid too many spurious suggestions)
     *           An example algorithm based on q-gram distance is presented in Esko Ukkonen, 
     *           "Approximate string-matching with q-grams and maximal matches", 
     *           Theoretical Computer Science, Volume 92, Issue 1, 6 January 1992, Pages 191-211
     * - Case 4: typeName is defined in an external library, but the library is not in the PVS_LIBRARY_PATH
     *           suggest adding a folder to the PVS_LIBRARY_PATH, or viewing/editing PVS_LIBRARY_PATH
     */
    fixNoResolutionForType (file: FileDescriptor, diag: Diagnostic): CodeAction[] {
        let actions: CodeAction[] = [];
        if (diag?.message && file.fileContent) {
            const message: string = diag.message;
            let suggestedNames: { [name: string]: string } = {}; // this hashmap is used to avoid suggesting the same name multiple times
            // const source: string = diag.source;
            const expectingType: RegExp = new RegExp(expectingTypeRegExp);
            const match: RegExpMatchArray = expectingType.exec(message);
            if (diag && match?.length > 1 && match[1]) { // && /Typecheck error/gi.test(source)) {
                // group 1 is the type that cannot be found
                const typeName: string = match[1];

                // Case 1: check if typeName is defined in the current context
                //         if match is found, suggest IMPORTING theoryName
                let candidates: string[] = Object.keys(this.ctx?.types || {}).filter(elem => {
                    return elem === typeName;
                });
                if (candidates?.length) {
                    for (let i = 0; i < candidates.length; i++) {
                        const types: PvsTheory[] = this.ctx?.types[candidates[i]];
                        for (let t = 0; t < types?.length; t++) {
                            const theoryName: string = types[t].theoryName;
                            if (!suggestedNames[theoryName]) {
                                const fix: CodeAction = this.getCodeActionImportTheory({
                                    fdesc: file,
                                    title: `Import theory "${theoryName}"`,
                                    newText: `IMPORTING ${theoryName}`,
                                    range: diag.range
                                });
                                if (fix) {
                                    actions.push(fix);
                                    suggestedNames[theoryName] = theoryName;
                                }
                            }
                        }
                    }
                }
                // Case 2: check if typeName is defined in nasalib
                //         if match is found, suggest IMPORTING nasalibFolder@theoryName
                candidates = nasalib_types.filter(elem => {
                    return elem === typeName;
                });
                if (candidates?.length) {
                    for (let i = 0; i < candidates.length; i++) {
                        const types: PvsTheory[] = nasalib_lookup_table?.types[candidates[i]];
                        for (let t = 0; t < types?.length; t++) {
                            const nasalibFolder: string = types[t].contextFolder;
                            const theoryName: string = types[t].theoryName;
                            const fullTheoryName: string = `${nasalibFolder}@${theoryName}`;
                            if (!suggestedNames[theoryName]) {
                                const fix: CodeAction = this.getCodeActionImportTheory({
                                    fdesc: file,
                                    title: `Import theory "${fullTheoryName}"`,
                                    newText: `IMPORTING ${fullTheoryName}`,
                                    range: diag.range
                                });
                                if (fix) {
                                    actions.push(fix);
                                    suggestedNames[fullTheoryName] = fullTheoryName;
                                }
                            }
                        }
                    }
                }

                if (candidates.length === 0) {
                    // Case 3: Check if typeName is mispelled, when IMPORTING fixes cannot be found
                    //         A similar typeName might be found among builtin types, in the current context, or in nasalib
                    //         Name similarity is based on a q-gram distance measure (e.g., "Vect3" is similar to "Vect3D" and "vect")
                    //         If matches are found, suggest replacing typeName with the identified matches
                    //         For builtin types, the matched name must be of the same length of typeName (this is done to avoid too many spurious suggestions)
                    //         An example algorithm based on q-gram distance is presented in Esko Ukkonen, 
                    //         "Approximate string-matching with q-grams and maximal matches", 
                    //         Theoretical Computer Science, Volume 92, Issue 1, 6 January 1992, Pages 191-211
                    candidates = nasalib_types.concat(Object.keys(this.ctx?.types || {})).concat(nasalib_types).concat(builtin_types.filter(elem => {
                        return elem.length === typeName.length; // builtin type should be of the same length
                    })).filter(elem => {
                        return elem !== typeName // type mispelled
                                && similar(elem, typeName); // and similar
                    });
                    if (candidates?.length) {
                        for (let i = 0; i < candidates.length; i++) {
                            const newText: string = candidates[i];
                            const origin: string = nasalib_types[newText] ? "(NASALib)" : "";
                            const title: string = `Change "${typeName}" to "${newText}" ${origin}`;
                            if (!suggestedNames[newText]) {
                                const fix: CodeAction = this.getCodeActionReplace({
                                    fdesc:  {
                                        contextFolder: file.contextFolder,
                                        fileName: file.fileName,
                                        fileExtension: file.fileExtension
                                    },
                                    title, 
                                    range: diag.range, 
                                    newText
                                });
                                if (fix) {
                                    actions.push(fix);
                                    suggestedNames[newText] = newText;
                                }
                            }
                        }
                    }
                }

                // Case 4: typeName is defined in an external library, but the library is not in the PVS_LIBRARY_PATH
                //         suggest adding a folder to the PVS_LIBRARY_PATH, or viewing/editing PVS_LIBRARY_PATH
                actions = actions.concat([
                    this.getCodeActionAddFolder({ declNotFound: typeName }),
                    this.getCodeActionEditLibraryPath()
                ]);
            }
        }
        return actions;
    }

    /**
	 * Standard API of the language server, provides a code action
	 * @param document Text document requiring codeaction
	 * @param range Current range selected with the cursor, lines are 0-based, cols and 1-based
     * @param context Code action context, contains the diagnostics of the error to be fixed
	 */
    provideCodeAction (document: { txt: string, uri: string }, range: Range, context: CodeActionContext): (Command | CodeAction)[] {
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