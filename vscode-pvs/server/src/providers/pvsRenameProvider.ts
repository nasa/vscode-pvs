/**
 * @module PvsRenameProvider
 * @author Paolo Masci
 * @date 2020.09.10
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

import { RenameParams, WorkspaceEdit, TextEdit, Range, Position, Connection } from "vscode-languageserver";
import { FormulaDescriptor, PvsFormula, TheoryDescriptor, PvsTheory, serverEvent, ServerDidRenameFile } from "../common/serverInterface";
import * as utils from '../common/languageUtils';
import * as fsUtils from '../common/fsUtils';

export class PvsRenameProvider {
    protected connection: Connection;
    /**
     * Constructor
     */
    constructor (connection: Connection) {
		this.connection = connection;
    }
    /**
     * Main API of the remane provider
     */
    async provideRename (desc: { txt: string, uri: string, position: Position, newName: string }): Promise<WorkspaceEdit> {
        // TODO: handle renaming of theories
        const changes: { [uri: string]: TextEdit[] } = {};
        if (desc && desc.txt && desc.uri && desc.position) {
            const fname: string = desc.uri;

            const lines: string[] = desc.txt.split("\n");
            if (lines && lines.length > desc.position.line) {
                const ln: string = lines[desc.position.line];
                let character: number = desc.position.character;
                const line: number = desc.position.line; 
                let range: Range = utils.getWordRange(desc.txt, desc.position);

                // check if the user is trying to rename a theory
                const theories: TheoryDescriptor[] = await fsUtils.listTheoriesInFile(fname, { content: desc.txt });
                if (theories && theories.length) {
                    const candidates: TheoryDescriptor[] = theories.filter((tdesc: TheoryDescriptor) => {
                        return tdesc.position.line === desc.position.line + 1; // line in rename parameters start from 0
                    });
                    if (candidates && candidates.length) {
                        const theoryName: string = candidates[0].theoryName;
                        const fileName: string = fsUtils.getFileName(fname);
                        const fileExtension: string = fsUtils.getFileExtension(fname);
                        const contextFolder: string = fsUtils.getContextFolder(fname);
                        if (theoryName !== desc.newName) {
                            // the character indicated in the request cannot always be trusted, e.g., when an entire line is selected
                            if (!range || range.start.character === range.end.character) {
                                character = ln.indexOf(theoryName);
                                range = utils.getWordRange(desc.txt, { line, character });
                            }
                            if (range) {
                                const newTheoryName: string = desc.newName;
                                console.log(`[rename-provider] Renaming theory ${theoryName} to ${newTheoryName}`);
                                // change text in the theory
                                const mod: string = ln.substring(0, range.start.character) + newTheoryName + ln.substring(range.end.character);
                                const newLines: string[] = lines.slice(0, desc.position.line).concat(mod).concat(lines.slice(desc.position.line + 1, lines.length + 1));
                                const theoryEndRegexp: RegExp = utils.endTheoryOrDatatypeRegexp(theoryName);
                                const newContent: string = newLines.join("\n").replace(theoryEndRegexp, `$1${newTheoryName}$3`);
                                await fsUtils.writeFile(fname, newContent);
                                const textEdit: TextEdit = {
                                    range,
                                    newText: desc.newName
                                };
                                changes[desc.uri] = [ textEdit ];

                                // change theory name in the jprf file, and all shasum for all proofs
                                const newShasum: string = fsUtils.shasum(newContent);
                                const theory: PvsTheory = { fileName, fileExtension, contextFolder, theoryName };
                                await fsUtils.renameTheoryInProofFile(theory, { newTheoryName, newShasum });

                                // if the theory name was identical to the file name, then change also the file name
                                if (theoryName === fileName && fileExtension === ".pvs") {
                                    const new_fname: string = fsUtils.desc2fname({
                                        fileName: newTheoryName,
                                        contextFolder,
                                        fileExtension: ".pvs"
                                    });
                                    const fileAlreadyExists: boolean = await fsUtils.fileExists(new_fname);
                                    if (!fileAlreadyExists) {
                                        const success: boolean = await fsUtils.renameFile(fname, new_fname);
                                        if (success && this.connection) {
                                            // notify the client that a file has been renamed
                                            const evt: ServerDidRenameFile = {
                                                action: "did-rename-file",
                                                old_fname: fname,
                                                new_fname
                                            };
                                            this.connection.sendNotification(serverEvent.workspaceEvent, evt);
                                    
                                        }
                                    }
                                }
                                return { changes };
                            }
                        }
                    }
                }

                // check if the user is trying to rename a formula
                // const oldShasum: string = fsUtils.shasum(desc.txt);
                const theorems: FormulaDescriptor[] = await fsUtils.listTheoremsInFile(fname, { content: desc.txt });
                if (theorems && theorems.length) {
                    const candidates: FormulaDescriptor[] = theorems.filter((fdesc: FormulaDescriptor) => {
                        return fdesc.position.line === desc.position.line + 1; // line in rename parameters start from 0
                    });
                    if (candidates && candidates.length) {
                        const formulaName: string = candidates[0].formulaName;
                        if (formulaName !== desc.newName) {
                            // the character indicated in the request cannot always be trusted, e.g., when an entire line is selected
                            if (!range || range.start.character === range.end.character) {
                                character = ln.indexOf(formulaName);
                                range = utils.getWordRange(desc.txt, { line, character });
                            }
                            if (range) {
                                const newFormulaName: string = desc.newName;
                                console.log(`[rename-provider] Renaming formula ${formulaName} to ${newFormulaName}`);
                                // change text in the theory
                                const mod: string = ln.substring(0, range.start.character) + newFormulaName + ln.substring(range.end.character);
                                const newLines: string[] = lines.slice(0, desc.position.line).concat(mod).concat(lines.slice(desc.position.line + 1, lines.length + 1));
                                const newContent: string = newLines.join("\n");
                                await fsUtils.writeFile(fname, newContent);
                                const textEdit: TextEdit = {
                                    range,
                                    newText: desc.newName
                                };
                                changes[desc.uri] = [ textEdit ];

                                // change formula name in the jprf file, and all shasum for all proofs
                                const newShasum: string = fsUtils.shasum(newContent);
                                const theoryName: string = fsUtils.findTheoryName(desc.txt, line);
                                const formula: PvsFormula = {
                                    fileName: fsUtils.getFileName(fname),
                                    fileExtension: fsUtils.getFileExtension(fname),
                                    contextFolder: fsUtils.getContextFolder(fname),
                                    theoryName,
                                    formulaName
                                };
                                await fsUtils.renameFormulaInProofFile(formula, { newFormulaName, newShasum });
                                return { changes };
                            }
                        }
                    }
                }
            }
        }
        return null;
    }
}