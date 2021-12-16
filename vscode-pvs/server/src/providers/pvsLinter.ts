/**
 * @module PvsLinter
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

import { Diagnostic, DiagnosticSeverity, Range, Position } from 'vscode-languageserver';
import * as utils from '../common/languageUtils';

/**
 * Linter heuristics
 */
const linter = [
    {
        note: "group 1 is the annotation, group 2 is the type/datatype definition",
        regexp: /(\%+\s*.+\s+)?(\w+\s*:\s*(?:\bTYPE\b|\bDATATYPE\b))/gi, 
        msg: "Type definitions should be preceeded by proper documentation"
    }
];

/**
 * PVS linter class, identifies anti-patterns, i.e., pieces of code that should be amended
 */
export class PvsLinter {
    provideDiagnostics(document: { txt: string, uri: string }): Diagnostic[] {
        let diag: Diagnostic[] = [];
        let match: RegExpMatchArray = null;
        const content: string = document.txt.replace(utils.commentRegexp, "");
        for (let i = 0; i < linter.length; i++) {
            while (match = linter[i].regexp.exec(content)) {
                if (match.length > 2 && (!match[1] || match[1] === "")) {
                    const lines: string[] = content.substring(0,match.index).split("\n");
                    const line: number = lines.length - 1; // 0-based
                    const character: number = lines[line].length - 1; // 0-based
                    let position: Position = { line, character };
                    let range: Range = {
                        start: position,
                        end: { line: position.line, character: position.character + match[2].length }
                    };
                    diag.push({
                        severity: DiagnosticSeverity.Warning,
                        range: range,
                        message: linter[i].msg,
                        source: "PVS Linter"
                    });
                }
            }
        }
        return diag;
    }
}