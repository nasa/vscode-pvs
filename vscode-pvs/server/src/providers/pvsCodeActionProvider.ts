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
import { PvsTheory, quickFixReplaceCommand, QuickFixReplace, QuickFixAddImporting, quickFixAddImportingCommand } from "../common/serverInterface";
import { nasalib_lookup_table } from "../core/nasalib-utils/nasalib-lookup-table";
import * as fsUtils from '../common/fsUtils';
import { errorCannotFindTheoryRegExp, expectingTypeRegExp, theoryRegexp } from "../common/languageUtils";

// list of all nasalib theories
const nasalib_theories: string[] = nasalib_lookup_table?.theories ? Object.keys(nasalib_lookup_table.theories) : [];
// list of all nasalib types
const nasalib_types: string[] = nasalib_lookup_table?.types ? Object.keys(nasalib_lookup_table.types) : [];

/**
 * PvsCodeAction class, used for implementing quick-fix actions
 */
export class PvsCodeActionProvider {
    protected connection: Connection;

    /**
     * Constructor
     * @param pvsLanguageServer 
     */
    constructor (connection: Connection) {
		this.connection = connection;
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
                const lines: string[] = document.txt.split("\n");
                for (let i = 0; i < context.diagnostics.length; i++) {
                    const diag: Diagnostic = context.diagnostics[i];
                    const message: string = context.diagnostics[i]?.message;
                    const source: string = context.diagnostics[i]?.source;
                    // quickfix action for IMPORTING errors
                    // - fix 1: add libName to pvs library path
                    // - fix 2: view/edit pvs library path
                    // - fix 3: IMPORTING libName => check nasalib and if a match is found suggest changing to IMPORTING folder@libName 
                    let cannotFindTheory: RegExp = new RegExp(errorCannotFindTheoryRegExp);
                    let match: RegExpMatchArray = cannotFindTheory.exec(message);
                    if (diag && match?.length > 1 && match[1] && /Typecheck error/gi.test(source)) {
                        // group 1 is the imported theory that cannot be found
                        const libName: string = match[1];
                        // fix 1: add libName to pvs library path
                        let fix1: string = `Add folder containing theory "${libName}" to PVS library path`;
                        const cmd1: Command = {
                            title: fix1,
                            command: "vscode-pvs.add-pvs-library"
                        };
                        const action1: CodeAction = {
                            title: fix1,
                            kind: CodeActionKind.QuickFix,
                            command: cmd1
                        };
                        actions.push(action1);
                        // fix 2: view/edit pvs library path
                        const fix2: string = `Open VSCode-PVS settings and edit PVS library path`;
                        const cmd2: Command = {
                            title: fix2,
                            command: "vscode-pvs.view-pvs-library-path"
                        };
                        const action2: CodeAction = {
                            title: fix2,
                            kind: CodeActionKind.QuickFix,
                            command: cmd2
                        };
                        actions.push(action2);
                        // fix 3: IMPORTING libName => check nasalib and if a match is found suggest changing to IMPORTING nasalibFolder@libName 
                        const candidates: string[] = nasalib_theories.filter(elem => {
                            return elem.toLocaleLowerCase() === libName.toLocaleLowerCase();
                        });
                        if (candidates?.length) {
                            for (let i = 0; i < candidates.length; i++) {
                                const theories: PvsTheory[] = nasalib_lookup_table?.theories[candidates[i]];
                                for (let t = 0; t < theories?.length; t++) {
                                    const folder: string = theories[t].contextFolder;
                                    const fix: string = `${folder}@${candidates[i]}`;
                                    const msg: string = `Change "${libName}" to "${fix}"`;
                                    const params: QuickFixReplace = {
                                        contextFolder,
                                        fileName,
                                        fileExtension,
                                        range: diag.range,
                                        newText: fix
                                    };
                                    const cmd3: Command = {
                                        title: msg,
                                        command: quickFixReplaceCommand,
                                        arguments: [ params ]
                                    };
                                    const action3: CodeAction = {
                                        title: msg,
                                        kind: CodeActionKind.QuickFix,
                                        command: cmd3
                                    };
                                    actions = [ action3 ].concat(actions);
                                }
                            }
                        }
                    }
                    // quickfix action for type definition not found
                    let expectingType: RegExp = new RegExp(expectingTypeRegExp);
                    match = expectingType.exec(message);
                    if (diag && match?.length > 1 && match[1] && /Typecheck error/gi.test(source)) {
                        // group 1 is the type that cannot be found
                        const typeName: string = match[1];
                        // fix 1: add libName to pvs library path
                        let fix1: string = `Add folder containing type "${typeName}" to PVS library path`;
                        const cmd1: Command = {
                            title: fix1,
                            command: "vscode-pvs.add-pvs-library"
                        };
                        const action1: CodeAction = {
                            title: fix1,
                            kind: CodeActionKind.QuickFix,
                            command: cmd1
                        };
                        actions.push(action1);
                        // fix 2: view/edit pvs library path
                        const fix2: string = `Open VSCode-PVS settings and edit PVS library path`;
                        const cmd2: Command = {
                            title: fix2,
                            command: "vscode-pvs.view-pvs-library-path"
                        };
                        const action2: CodeAction = {
                            title: fix2,
                            kind: CodeActionKind.QuickFix,
                            command: cmd2
                        };
                        actions.push(action2);
                        // fix 3: check if nasalib defines the type in some theory, and if a match is found suggest IMPORTING nasalibFolder@libName 
                        const candidates: string[] = nasalib_types.filter(elem => {
                            return elem.toLocaleLowerCase() === typeName.toLocaleLowerCase();
                        });
                        if (candidates?.length) {
                            for (let i = 0; i < candidates.length; i++) {
                                const types: PvsTheory[] = nasalib_lookup_table?.types[candidates[i]];
                                for (let t = 0; t < types?.length; t++) {
                                    const folder: string = types[t].contextFolder;
                                    const theory: string = types[t].theoryName;
                                    const fix: string = `IMPORTING ${folder}@${theory}`;
                                    const currentTheory: string = fsUtils.findTheoryName(document.txt, diag.range.start.line);
                                    if (currentTheory) {
                                        // find location where to place the IMPORTING
                                        // group 1 is the theory name
                                        const regexp: RegExp = new RegExp(theoryRegexp);
                                        let matchTheory: RegExpMatchArray = null;
                                        let importingLine: number = -1;
                                        let importingCharacter: number = -1;
                                        while (matchTheory = regexp.exec(document.txt)) {
                                            if (matchTheory?.length > 1 && matchTheory[1] === currentTheory) {
                                                const frag: string = document.txt.substring(0, matchTheory.index + matchTheory[0].length);
                                                const lines: string[] = frag.split("\n");
                                                importingLine = lines.length;
                                                importingCharacter = lines[importingLine - 1].indexOf("BEGIN") + 6;
                                                break;
                                            }
                                        }
                                        if (importingLine !== -1 && importingCharacter !== -1) {
                                            const msg: string = `Import "${candidates[i]}" from theory "${theory}" (NASALib)`;
                                            const params: QuickFixAddImporting = {
                                                contextFolder,
                                                fileName,
                                                fileExtension,
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
                    }
                }
            }
            return actions;
        }
        return null;
    }
}