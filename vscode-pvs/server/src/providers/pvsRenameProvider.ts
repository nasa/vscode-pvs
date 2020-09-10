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

import { RenameParams, WorkspaceEdit, TextEdit, Range, Position } from "vscode-languageserver";
import { FormulaDescriptor, PvsFormula } from "../common/serverInterface";
import * as utils from '../common/languageUtils';
import * as fsUtils from '../common/fsUtils';

export class PvsRenameProvider {
    async provideRename (desc: { txt: string, uri: string, position: Position, newName: string }): Promise<WorkspaceEdit> {
        // TODO: handle renaming of theories
        const changes: { [uri: string]: TextEdit[] } = {};
        if (desc && desc.txt && desc.uri && desc.position) {
            const fname: string = desc.uri;
            // const shasum: string = fsUtils.shasum(content);
            const theorems: FormulaDescriptor[] = await utils.listTheoremsInFile(fname, { content: desc.txt });
            if (theorems && theorems.length) {
                const candidates: FormulaDescriptor[] = theorems.filter((fdesc: FormulaDescriptor) => {
                    return fdesc.position.line === desc.position.line + 1; // line in rename parameters start from 0
                });
                if (candidates && candidates.length) {
                    const formulaName: string = candidates[0].formulaName;
                    if (formulaName !== desc.newName) {
                        const lines: string[] = desc.txt.split("\n");
                        if (lines && lines.length > desc.position.line) {
                            const ln: string = lines[desc.position.line];
                            let character: number = desc.position.character;
                            const line: number = desc.position.line; 
                            let range: Range = utils.getWordRange(desc.txt, desc.position);
                            if (!range || range.start.character === range.end.character) {
                                character = ln.indexOf(formulaName); // the character indicated in the request cannot always be trusted, e.g., when an entire line is selected
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

                                // change shasum and formula name in the jprf file, if the file exists and a proof exists in the file
                                const newShasum: string = fsUtils.shasum(newContent);
                                const theoryName: string = utils.findTheoryName(desc.txt, line);
                                const formula: PvsFormula = {
                                    fileName: fsUtils.getFileName(fname),
                                    fileExtension: fsUtils.getFileExtension(fname),
                                    contextFolder: fsUtils.getContextFolder(fname),
                                    theoryName,
                                    formulaName
                                };
                                await utils.renameProof(formula, { newFormulaName, newShasum });
                            }
                        }
                    }
                }
            }
        }
        return { changes };
    }
}