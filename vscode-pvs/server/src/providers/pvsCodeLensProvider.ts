
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

import { CancellationToken, CodeLens, Range, CodeLensParams, Position } from 'vscode-languageserver';
import * as fsUtils from '../common/fsUtils';
import * as utils from '../common/languageUtils';
import { CopyProofliteRequest, FileDescriptor, FollowLink, GotoFileDescriptor, ProveFormulaRequest, PvsFormula, PvsTheory } from '../common/serverInterface';
import { PvsLanguageServer } from '../pvsLanguageServer';
import * as path from 'path';

/**
 * codelens provider, renders inline actions in the editor
 */
export class PvsCodeLensProvider {
    protected pvsLanguageServer: PvsLanguageServer;


    /**
     * Constructor
     * @param pvsLanguageServer 
     */
    constructor (pvsLanguageServer: PvsLanguageServer) {
        this.pvsLanguageServer = pvsLanguageServer;
    }

    /**
	 * Standard API of the language server, provides a completion list while typing a pvs expression
     * TODO: improve performance of this function
	 * @param document Text document requiring codelens
	 * @param token Cancellation token
	 */
	provideCodeLens(document: { txt: string, uri: string }, token?: CancellationToken): Thenable<CodeLens[]> {
        if (document && !this.pvsLanguageServer.isPreludeFile(document.uri)) {
            let fileName: string = fsUtils.getFileName(document.uri);
            const fileExtension: string = fsUtils.getFileExtension(document.uri);
            const contextFolder: string = fsUtils.getContextFolder(document.uri);
            
            let match: RegExpMatchArray = null;
            let codeLens: CodeLens[] = [];
            const content: string = document.txt.replace(utils.commentRegexp, "");

            // typecheck-file | evaluate-in-pvsio | view-as-markdown
            // use a simple match on the theory declaration, so the commands show up even if the theory does not parse (e.g., because of missing END theoryname)
            if (fileExtension === ".pvs") {
                const regex: RegExp = new RegExp(utils.theoryOrDatatypeRegexp);
                while (match = regex.exec(content)) {
                    if (match.length > 1 && match[1]) {
                        const theoryName: string = match[1];
                            
                        const docUp: string = content.slice(0, match.index + theoryName.length);
                        const lines: string[] = docUp.split("\n");
                        const line: number = lines.length - 1;
                        const character: number = lines[lines.length - 1].indexOf(theoryName);
                        
                        const pvsFile: FileDescriptor = {
                            fileName,
                            fileExtension,
                            contextFolder
                        };
                        const theory: PvsTheory = {
                            ...pvsFile,
                            theoryName, 
                            line
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
                                arguments: [ theory ]
                            }
                        });
                        codeLens.push({
                            range,
                            command: {
                                title: "evaluate-in-pvsio",
                                command: "vscode-pvs.pvsio-evaluator",
                                arguments: [ theory ]
                            }
                        });
                        codeLens.push({
                            range,
                            command: {
                                title: "view-as-markdown",
                                command: "vscode-pvs.view-as-markdown",
                                arguments: [ pvsFile ]
                            }
                        });
                        // add codelens for theory documentation if the theory does not include any doc header
                        const includesDoc: boolean = utils.includesTheoryHeader(theoryName, docUp);
                        if (!includesDoc) {
                            codeLens.push({
                                range,
                                command: {
                                    title: "document-theory",
                                    command: "vscode-pvs.document-theory",
                                    arguments: [ theory ]
                                }
                            });    
                        }
                    }
                }
            }

            // show-prooflite | prove-formula
            if (fileExtension === ".pvs" || fileExtension === ".tccs") {
                const regex: RegExp = new RegExp(utils.theoremRegexp);
                while (match = regex.exec(content)) {
                    if (match.length > 1 && match[1]) {
                        const formulaName: string = match[1];
                        const matchParams: RegExpMatchArray = new RegExp(utils.theoremParamsRegexp).exec(match[0].trim());
                        const blanks: number = match[0].replace(matchParams[0], "").replace(formulaName, "").length;
                        // the following can be done in the resolve if necessary for performance reasons
                        const docUp: string = content.slice(0, match.index + formulaName.length + blanks);
                        const lines: string[] = docUp.split("\n");
                        const line: number = lines.length - 1;
                        const character: number = lines[lines.length - 1].indexOf(match[1]);
                        
                        let theoryName: string = fsUtils.findTheoryName(content, line);
                        if (theoryName && character >= 0) {
                            const formula: PvsFormula = {
                                fileName,
                                fileExtension,
                                contextFolder,
                                theoryName: (fileExtension === ".tccs") ? 
                                    theoryName.substring(0, theoryName.length - 5) // the theory name in the .tccs file ends with _TCCS
                                    : theoryName, 
                                formulaName,
                                line: fileExtension === ".tccs" ? line - 3 : line // -3 is necessary to take into account the header of the tcc file created by vscode-pvs
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
                                    arguments: [ formula ]
                                }
                                // ,
                                // data: {
                                //     line, character, doc: docUp, formulaName, fileName, fileExtension, contextFolder
                                // }
                            });
                            codeLens.push({
                                range,
                                command: {
                                    title: (fileExtension === ".pvs") ? "discharge-tccs" : "discharge-matching-tccs",
                                    command: "vscode-pvs.discharge-matching-tccs",
                                    arguments: [ formula ]
                                }
                            });    
                            codeLens.push({
                                range,
                                command: {
                                    title: "status-proofchain",
                                    command: "vscode-pvs.status-proofchain",
                                    arguments: [ formula ]
                                }
                            });
                            codeLens.push({
                                range,
                                command: {
                                    title: "show-prooflite",
                                    command: "vscode-pvs.show-prooflite",
                                    arguments: [ formula ]
                                }
                            });
                        }
                    }
                }
            }

            // goto-pvs-file
            if (fileExtension === ".tccs") {
                let gotoCodeLensMap: { [key:string]: CodeLens } = {}; // using a map to avoid duplicate gotos
                // group 1 is the line-column information
                // group 2 is the line (1-based)
                // group 3 is the column (1-based)
                // const regex: RegExp = new RegExp(/\(at (line (\d+), column (\d+))\)/g);
                // group 4 is optional, and corresponds to the expression that generated the tcc -- this grop is captured only if the expression is on one line
                const regex: RegExp = new RegExp(/\(at (line (\d+), column (\d+))\)(?:[\s%]+for[\s%]+(.*)[\s%]+expected\b)?/g);
                while (match = regex.exec(document.txt)) {
                    // find position of the codelens
                    const lines: string[] = document.txt.slice(0, match.index).split("\n");
                    let line: number = lines.length - 1;
                    const character: number = lines[line].length;

                    // try to pull the codelens down to the level of the corresponding obligation
                    const docDown: string = document.txt.slice(match.index);
                    const regexObligation: RegExp = new RegExp(utils.tccRegexp);
                    const matchObligation: RegExpMatchArray = regexObligation.exec(docDown);
                    if (matchObligation?.length && matchObligation[1]) {
                        line += docDown.slice(0, matchObligation.index)?.split("\n")?.length - 1;
                    }

                    // set the codelens
                    const range: Range = {
                        start: { line, character }, 
                        end: { line, character: character + match[0].length }
                    };
                    const gotoPosition: Position = {
                        line: +match[2] - 1, // lines are 0-based
                        character: +match[3]
                    };
                    // check if the goto link has already been provided for the given line
                    const gotoRange: Range = {
                        start: gotoPosition,
                        end: {
                            line: gotoPosition.line,
                            character: gotoPosition.character // + match[4]?.length // highlighting the offending expression seems to be confusing in vscode, because all other similar expressions are also highlighted and the cursor ends up at the end of the expression not at the beginning
                        }
                    };
                    const gotoDesc: GotoFileDescriptor = {
                        contextFolder,
                        fileName,
                        fileExtension: ".pvs",
                        pos: gotoPosition,
                        range: gotoRange
                    };
                    gotoCodeLensMap[`${range.start.line}`] = {
                        range,
                        command: {
                            title: `(goto ${match[1]})`,
                            command: "vscode-pvs.goto-pvs-file",
                            arguments: [ gotoDesc ]
                        }
                    };
                    // codeLens.push({
                    //     range,
                    //     command: {
                    //         title: `(goto ${match[1]})`,
                    //         command: "vscode-pvs.goto-pvs-file",
                    //         arguments: [ gotoDesc ]
                    //     }
                    // });
                }
                codeLens = codeLens.concat(Object.values(gotoCodeLensMap));
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
                        
                        const proveFormulaArgs: ProveFormulaRequest = {
                            fileName,
                            fileExtension: (utils.tccFormulaRegexp.test(formulaName)) ? ".tccs" : ".pvs",
                            contextFolder: realContextFolder,
                            theoryName, 
                            formulaName,
                            origin: "prooflite",
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
                                arguments: [ proveFormulaArgs ]
                            }
                        });

                        const matchProoflite: RegExpMatchArray = utils.proofliteRegexp({
                            theoryName, formulaName
                        }).exec(content);
                        const prooflite: string = matchProoflite && matchProoflite.length ? matchProoflite[0].trim() : null;
                    
                        const insertProofliteArgs: CopyProofliteRequest = {
                            fileName,
                            fileExtension: (utils.tccFormulaRegexp.test(formulaName)) ? ".tccs" : ".pvs",
                            contextFolder: realContextFolder,
                            theoryName, 
                            formulaName,
                            prooflite
                        };

                        codeLens.push({
                            range,
                            command: {
                                title: "copy-prooflite",
                                command: "vscode-pvs.copy-prooflite",
                                arguments: [ insertProofliteArgs ]
                            }
                        });
                    }       
                }
            }

            // plot-expression
            if (fileExtension === ".pvs") {
                const regexs: RegExp[] = [
                    new RegExp(utils.linearPlotTupleTupleListRegexp),
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

            // quick-open | preview-theory-as-markdown
            if (fileExtension === ".pvs") {
                const regexp: RegExp = new RegExp(utils.markdownRegexp);
                while (match = regexp.exec(document.txt)) {
                    if (match.length > 2 && match[0].length && match[2].length) {
                        const docUp: string = document.txt.slice(0, match.index + match[0].length);
                        const lines: string[] = docUp.split("\n");
                        const line: number = lines.length - 1;
                        const character: number = 1;
                        const range: Range = {
                            start: { line, character }, 
                            end: { line, character }
                        };
                        const fname: string = fsUtils.normalizePath(path.join(contextFolder, match[2]));
                        const name: string = match[1] || fname;
                        const args: FollowLink = {
                            name,
                            fname
                        };
                        codeLens.push({
                            range,
                            command: {
                                title: `open(${name})`,
                                command: "vscode-pvs.quick-open",
                                arguments: [ args ]
                            }
                        });
                        const pvsFile: FileDescriptor = {
                            fileName,
                            fileExtension,
                            contextFolder
                        };
                        codeLens.push({
                            range,
                            command: {
                                title: "view-as-markdown",
                                command: "vscode-pvs.view-as-markdown",
                                arguments: [ pvsFile ]
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
        const uri: string = args?.textDocument?.uri;
        if (fsUtils.isPvsFile(uri)) {
            let codelens: CodeLens[] = await this.provideCodeLens({ txt, uri });
            return codelens;
        }
        return null;
    }
 }