
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
import { ProveFormulaRequest, PvsFormula, PvsTheory } from '../common/serverInterface';

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
            let fileName: string = fsUtils.getFileName(document.uri);
            const fileExtension: string = fsUtils.getFileExtension(document.uri);
            const contextFolder: string = fsUtils.getContextFolder(document.uri);
            
            let match: RegExpMatchArray = null;
            const codeLens: CodeLens[] = [];
            const content: string = document.txt.replace(utils.commentRegexp, "");

            // typecheck-file | evaluate-in-pvsio
            // use a simple match on the theory declaration, so the commands show up even if the theory does not parse (e.g., because of missing END theoryname)
            if (fileExtension === ".pvs") {
                const regex: RegExp = new RegExp(utils.theoryOrDatatypeRegexp);
                while (match = regex.exec(content)) {
                    if (match.length > 1 && match[1]) {
                        const theoryName: string = match[1];
                            
                        const docUp: string = content.slice(0, match.index + theoryName.length);
                        const lines: string[] = docUp.split("\n");
                        const line: number = lines.length - 1;// + lineOffset;
                        const character: number = lines[lines.length - 1].indexOf(theoryName);
                        
                        const args: PvsTheory = {
                            fileName,
                            fileExtension,
                            contextFolder,
                            theoryName, 
                            // line
                        };

                        // codelens
                        const range: Range = {
                            start: { line, character },
                            end: { line, character: character + theoryName.length }
                        };
                        codeLens.push({
                            range,
                            command: {
                                title: "typecheck-file",
                                command: "vscode-pvs.typecheck-file",
                                arguments: [ args ]
                            }
                        });
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

            // show-prooflite | prove-formula
            if (fileExtension === ".pvs" || fileExtension === ".tccs") {
                const regex: RegExp = new RegExp(utils.theoremRegexp);
                while (match = regex.exec(content)) {
                    if (match.length > 1 && match[1]) {
                        const formulaName: string = match[1];

                        // the following can be done in the resolve if necessary for performance reasons
                        const docUp: string = content.slice(0, match.index + formulaName.length);
                        const lines: string[] = docUp.split("\n");
                        const line: number = lines.length - 1;
                        const character: number = lines[lines.length - 1].indexOf(match[1]);
                        
                        let theoryName: string = fsUtils.findTheoryName(content, line);
                        if (theoryName) {
                            const args: PvsFormula = {
                                fileName,
                                fileExtension,
                                contextFolder,
                                theoryName: (fileExtension === ".tccs") ? 
                                    theoryName.substr(0, theoryName.length - 5) // the theory name in the .tccs file ends with _TCCS
                                    : theoryName, 
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
                                    title: "show-prooflite",
                                    command: "vscode-pvs.show-prooflite",
                                    arguments: [ args ]
                                }
                            });
                        }
                    }
                }
            }

            // prove-formula
            if (fileExtension === ".prlite" || fileExtension === ".prl") {
                const regex: RegExp = new RegExp(utils.proofRegexp);
                while (match = regex.exec(content)) {
                    if (match.length > 1 && match[1]) {
                        const formulaName: string = match[1];
                        const theoryName: string = fileName; // by convention, .prlite filename is the theoryName

                        // check if a file name is specified in the tags
                        const matchFileName: RegExpMatchArray = utils.proofliteTagsRegexp({ theoryName, formulaName }).exec(document.txt);
                        if (matchFileName && matchFileName.length > 1) {
                            fileName = fsUtils.getFileName(matchFileName[1]);
                        }
                            
                        const docUp: string = content.slice(0, match.index + theoryName.length);
                        const lines: string[] = docUp.split("\n");
                        const line: number = lines.length - 1;
                        const character: number = 0;

                        const realContextFolder: string = contextFolder.endsWith("/pvsbin") || contextFolder.endsWith("/pvsbin/") ?
                            contextFolder.substring(0, contextFolder.lastIndexOf("/pvsbin")) : contextFolder;
                        
                        const args: ProveFormulaRequest = {
                            fileName,
                            fileExtension: (utils.tccFormulaRegexp.test(formulaName)) ? ".tccs" : ".pvs",
                            contextFolder: realContextFolder,
                            theoryName, 
                            formulaName,
                            origin: "proofilte",
                            proofFile: {
                                fileName,
                                fileExtension,
                                contextFolder
                            }
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

            // plot-expression
            if (fileExtension === ".pvs") {
                const regexs: RegExp[] = [
                    new RegExp(utils.linearPlotRegexp),
                    new RegExp(utils.linearPlotSimpleRegexp),
                    new RegExp(utils.scatterPlotRegexp)
                ];
                for (let i = 0; i < regexs.length; i++) {
                    while (match = regexs[i].exec(content)) {
                        if (match.length > 1 && match[1]) {
                            const formulaName: string = match[1];

                            // the following can be done in the resolve if necessary for performance reasons
                            const docUp: string = content.slice(0, match.index + formulaName.length);
                            const lines: string[] = docUp.split("\n");
                            const line: number = lines.length - 1;
                            const character: number = lines[lines.length - 1].indexOf(match[1]);
                            
                            if (fileName) {
                                const args: { path: string, expr: string } = {
                                    path: document.uri,
                                    expr: formulaName
                                };
                                const range: Range = {
                                    start: { line, character }, 
                                    end: { line, character: character + formulaName.length }
                                };
                                codeLens.push({
                                    range,
                                    command: {
                                        title: "plot-expression",
                                        command: "vscode-pvs.plot-expression",
                                        arguments: [ args ]
                                    }
                                });
                            }
                        }
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