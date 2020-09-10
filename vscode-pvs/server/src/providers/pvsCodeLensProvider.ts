
/**
 * @module PvsCodeLensProvider
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

import { CancellationToken, CodeLens, CodeLensRequest, Range, CodeLensParams } from 'vscode-languageserver';
import * as fsUtils from '../common/fsUtils';
import * as utils from '../common/languageUtils';
import { PvsFormula } from '../common/serverInterface';
import * as path from 'path';

export class PvsCodeLensProvider {    
    /**
	 * Standard API of the language server, provides a completion list while typing a pvs expression
     * TODO: improve performance of this function
	 * @param document Text document requiring codelens
	 * @param position Current position of the cursor
	 * @param token Cancellation token
	 */
	provideCodeLens(document: { txt: string, uri: string }, token?: CancellationToken): Thenable<CodeLens[]> {
        if (document) {
            const contextFolder: string = fsUtils.getContextFolder(document.uri);
            const fileName: string = fsUtils.getFileName(document.uri);
            const fileExtension: string = fsUtils.getFileExtension(document.uri);

            const codeLens: CodeLens[] = [];
            const content: string = document.txt.replace(utils.commentRegexp, "");
            let match: RegExpMatchArray = null;

            while (match = utils.theoremRegexp.exec(content)) {
                if (match.length > 1 && match[1]) {
                    const formulaName: string = match[1];

                    // the following can be done in the resolve if necessary for performance reasons
                    const docUp: string = content.slice(0, match.index + formulaName.length);
                    const lines: string[] = docUp.split("\n");
                    const line: number = lines.length - 1;
                    const character: number = lines[lines.length - 1].indexOf(match[1]);
                    
                    const theoryName: string = utils.findTheoryName(content, line);
                    if (theoryName) {
                        const args: PvsFormula = {
                            fileName,
                            fileExtension,
                            contextFolder,
                            theoryName, 
                            formulaName
                        };
                        const range: Range = {
                            start: { line, character }, 
                            end: { line, character: character + formulaName.length }
                        };
                        codeLens.push({
                            range,
                            command: {
                                title: "prove",
                                command: "vscode-pvs.prove-formula",
                                arguments: [ args ]
                            }
                            // ,
                            // data: {
                            //     line, character, doc: docUp, formulaName, fileName, fileExtension, contextFolder
                            // }
                        });
                        codeLens.push({
                            range,
                            command: {
                                title: "show-proof",
                                command: "vscode-pvs.show-prooflite",
                                arguments: [ args ]
                            }
                        });
                    }
                }
            }

            if (fileExtension === ".pvs") {
                while (match = utils.theoryRegexp.exec(content)) {
                    if (match.length > 1 && match[1]) {
                        const theoryName: string = match[1];

                        const matchEnd: RegExpMatchArray = utils.endTheoryRegexp(theoryName).exec(content);
                        if (matchEnd && matchEnd.length) {
                            utils.theoryRegexp.lastIndex = matchEnd.index; // restart the search from here
                            
                            const docUp: string = content.slice(0, match.index + theoryName.length);
                            const lines: string[] = docUp.split("\n");
                            const line: number = lines.length - 1;
                            const character: number = lines[lines.length - 1].indexOf(match[1]);
                            
                            const args = {
                                fileName,
                                fileExtension,
                                contextFolder,
                                theoryName, 
                                line
                            };

                            // pvsio codelens
                            const range: Range = {
                                start: { line, character },
                                end: { line, character: character + theoryName.length }
                            };
                            codeLens.push({
                                range,
                                command: {
                                    title: "evaluate-in-pvsio",
                                    command: "vscode-pvs.pvsio-evaluator",
                                    arguments: [ args ]
                                }
                            });
                        }
                    }
                }
            }

            if (fileExtension === ".prlite") {
                while (match = utils.proofRegexp.exec(content)) {
                    if (match.length > 1 && match[1]) {
                        const formulaName: string = match[1];
                        const theoryName: string = fileName; // by convention, .prlite filename is the theoryName
                            
                        const docUp: string = content.slice(0, match.index + theoryName.length);
                        const lines: string[] = docUp.split("\n");
                        const line: number = lines.length - 1;
                        const character: number = 0;
                        
                        const args: PvsFormula = {
                            fileName,
                            fileExtension: ".pvs",
                            contextFolder: path.join(contextFolder, ".."), // .prlite files are stored in the /pvsbin subfolder
                            theoryName, 
                            formulaName
                        };

                        // codelens in .prlite files
                        const range: Range = {
                            start: { line, character },
                            end: { line, character: character + theoryName.length }
                        };
                        codeLens.push({
                            range,
                            command: {
                                title: "prove",
                                command: "vscode-pvs.prove-formula",
                                arguments: [ args ]
                            }
                        });
                    }       
                }
            }
            return Promise.resolve(codeLens);
        }
        return Promise.resolve([]);
    }

    resolveCodeLens(codeLens: CodeLens, token?: CancellationToken): CodeLens {
        return codeLens;
        // if (codeLens && codeLens.data) {
        //     const doc: string = codeLens.data.doc;
        //     const line: number = codeLens.data.line;
        //     const character: number = codeLens.data.character;
        //     const formulaName: string = codeLens.data.formulaName;
        //     const fileName: string = codeLens.data.fileName;
        //     const fileExtension: string = codeLens.data.fileExtension;
        //     const contextFolder: string = codeLens.data.contextFolder;

        //     const theoryName: string = utils.findTheoryName(doc, line);
        //     console.log("codelense: theoryName = ", theoryName);
        //     console.log("codelense: formulaName = ", formulaName);
            
        //     const args = { fileName, theoryName, formulaName, line, fileExtension, contextFolder }
        //     return {
        //         range: {
        //             start: { line, character },
        //             end: { line, character: character + formulaName.length }
        //         },
        //         command: {
        //             title: `prove`,
        //             command: "codelense.pvs.prove-formula",
        //             arguments: [ args ]
        //         }
        //     };
        // }
        // return null;
    }

    async onCodeLens (args: CodeLensParams, txt: string): Promise<CodeLens[]> {
        const uri: string = args.textDocument.uri;
        if (fsUtils.isPvsFile(uri)) {
            let codelens: CodeLens[] = await this.provideCodeLens({ txt, uri });
            return codelens;
        }
        return null;
    }
 }