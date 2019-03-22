/**
 * @module PvsLinter
 * @version 2019.03.22
 * PvsLinter aims to promote good engineering practices for the development of 
 * pvs specification. 
 * The current implementation of the linter checks that type definitions
 * have been properly documented.
 * @author Paolo Masci
 * @date 2019.03.22
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

import { TextDocument, Diagnostic, DiagnosticSeverity, Range, Position } from 'vscode-languageserver';
import * as language from "../common/languageKeywords";

const linter = [
    { regexp: /(\%+\s*.+\s+)?(\w+\s*:\s*(?:\bTYPE\b|\bDATATYPE\b))/gi, msg: "Type definitions should be preceeded by proper documentation" }
];

const comments: RegExp = new RegExp(language.PVS_COMMENT_REGEXP_SOURCE, "g")

export class PvsLinter {
    provideDiagnostics(document: TextDocument): Diagnostic[] {
        let diag: Diagnostic[] = [];
        let match: RegExpMatchArray = null;
        const text: string = document.getText();
        let commentedSections: Position[] = [];
        while (match = comments.exec(text)) {
            const startPos: Position = document.positionAt(match.index);
            commentedSections.push(startPos);
        }
        for (let i in linter) {
            while (match = linter[i].regexp.exec(text)) {
                if (!match[1] || match[1] === "") {
                    let position: Position = document.positionAt(match.index);
                    let isComment: boolean = commentedSections.some((pos: Position) => {
                        return pos.line === position.line && pos.character <= position.character;
                    });
                    if (!isComment) {
                        let range: Range = {
                            start: position,
                            end: { line: position.line, character: position.character + match[2].length }
                        };
                        if (range) {
                            diag.push({
                                severity: DiagnosticSeverity.Warning,
                                range: range,
                                message: linter[i].msg,
                                source: "PVS Linter"
                            });
                        }
                    }
                }
            }
        }
        return diag;
    }
}