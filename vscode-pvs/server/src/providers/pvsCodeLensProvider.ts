
/**
 * @module PvsCodeLensProvider
 * @version 2019.02.07
 * @description Code lens engine for PVS, provides commands that can be shown inline along with the specification, 
 *              e.g,. to animate an expression or prove a theorem.
 * @author Paolo Masci
 * @date 2019.02.07
 * @copyright 
 * Copyright 2016 United States Government as represented by the
 * Administrator of the National Aeronautics and Space Administration. No
 * copyright is claimed in the United States under Title 17, 
 * U.S. Code. All Other Rights Reserved.
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

import { TextDocument, CancellationToken, CodeLens, Range } from 'vscode-languageserver';
import { ExpressionDescriptor } from '../common/serverInterface';
import { PvsDefinitionProvider } from './pvsDefinitionProvider';
import { findTheoryName } from '../common/languageUtils';
import * as fs from '../common/fsUtils';
import * as utils from '../common/languageUtils';

export class PvsCodeLensProvider {
	private definitionProvider: PvsDefinitionProvider;
    /**
     * @constructor
     * @param definitionProvider Pointer to the definition provider 
     */
    constructor (definitionProvider: PvsDefinitionProvider) {
		this.definitionProvider = definitionProvider;
    }
    /**
	 * Standard API of the language server, provides a completion list while typing a pvs expression
	 * @param document Text document requiring intellisense
	 * @param position Current position of the cursor
	 * @param token Cancellation token
	 */
	provideCodeLens(document: TextDocument, token?: CancellationToken): CodeLens[] {
        const fileName: string = fs.getFilename(document.uri);
        const fileExtension: string = fs.getFileExtension(document.uri);
        const codeLens: CodeLens[] = [];
        const doc: string = document.getText();
        const lines: string[] = doc.split("\n");
        for (let i = 0; i < document.lineCount; i++) {
            // runit
            if (/\s*%\s*@\s*runit\b/gi.test(lines[i])) {
                const txt: string = lines.slice(i).join("\n");
                const match: RegExpMatchArray = /\s*%\s*@\s*runit\b\s*(?:(?:\s*%[^\n]*\s*)*)(\w+)/gi.exec(txt);
                if (match && match[1]) {
                    const theoryName: string = findTheoryName(doc, i);
                    const range: Range = {
						start: { line: i + 1, character: 0 },
						end: { line: i + 1, character: 10 }
					};
                    const req: ExpressionDescriptor = {
                        fileName: fileName,
                        theoryName: theoryName,
                        expression: match[1]
                    };
                    codeLens.push({
						range: range, 
						command: {
                        	title: "run " + match[1],
                        	command: "cmd.runit",
							arguments: [ req ]
						}
					});
                }
            }
            // step proof
            if (/(\w+)\s*:\s*(?:THEOREM)\b/gi.test(lines[i])) {
                const match: RegExpMatchArray = /(\w+)\s*:\s*(?:THEOREM)\b/gi.exec(lines[i]);
                if (match && match[1]) {
                    const formulaName: string = match[1];
                    const theoryName: string = utils.findTheoryName(doc,i);
                    codeLens.push({
                        range: {
                            start: { line: i, character: match.index },
                            end: { line: i, character: match.index + formulaName.length }
                        },
                        command: {
                            title: `prove`,
                            command: "codelense.pvs.prove",
                            arguments: [ { fileName, theoryName, formulaName, line: i, fileExtension } ]
                        }
                    });
                    codeLens.push({
                        range: {
                            start: { line: i, character: match.index },
                            end: { line: i, character: match.index + formulaName.length }
                        },
                        command: {
                            title: `show proof`,
                            command: "codelense.pvs.show-proof", // todo
                            arguments: [ { fileName, theoryName, formulaName, line: i, fileExtension } ]
                        }
                    });
                    codeLens.push({
                        range: {
                            start: { line: i, character: match.index },
                            end: { line: i, character: match.index + formulaName.length }
                        },
                        command: {
                            title: `step proof`,
                            command: "codelense.pvs.step-proof",
                            arguments: [ { fileName, theoryName, formulaName, line: i, fileExtension } ]
                        }
                    });
                }
            }
            // proveit
            // else if (/\s*%\s*@\s*proveit\b/gi.test(lines[i])) {
            //     let txt: string = lines.slice(i).join("\n");
            //     let match: RegExpMatchArray = /\s*%\s*@\s*proveit\b\s*(?:(?:\s*%[^\n]*\s*)*)(\w+)/gi.exec(txt);
            //     if (match[1]) {
			// 		let theoryName: string = findTheoryName(doc, i);
            //         let range: Range = {
			// 			start: { line: i + 1, character: 0 },
			// 			end: { line: i + 1, character: 10 }
			// 		};
            //         let req: FormulaDescriptor = {
            //             fileName: fileName,
            //             theoryName: theoryName,
            //             formulaName: match[1],
            //             line: range.start.line
            //         };
            //         codeLens.push({
			// 			range: range,
			// 			command: {
			// 				title: "prove " + match[1],
			// 				command: "cmd.proveit",
			// 				arguments: [ req ]
			// 			}
			// 		});
            //     }
            // }
        }
        return codeLens;
    }
}