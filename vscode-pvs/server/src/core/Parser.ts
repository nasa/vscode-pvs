/**
 * @module Parser
 * @author Paolo Masci
 * @date 2020.01.06
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

import { PvsParser, ParserDiagnostics } from './pvs-parser/javaTarget/pvsParser';
import { DdlParser } from './ddl-parser/javaTarget/ddlParser';
import * as path from 'path';
// import { PvsParserJS } from './pvs-parser/jsTarget/pvsParserJS';
import { FileDescriptor } from '../common/serverInterface';
import { Diagnostic } from 'vscode-languageserver';

export type AntlrTarget = "js" | "java";

export class Parser {
    protected pvsParser = new PvsParser();
    protected ddlParser = new DdlParser();
    // protected pvsParserJS = new PvsParserJS();

    /**
     * Parse a pvs file
     * @param desc File descriptor, includes file name, file extension, and context folder
     */
    async parseFile (desc: FileDescriptor, opt?: { useTarget?: AntlrTarget }): Promise<ParserDiagnostics> {
        if (desc) {
            opt = opt || {};
            switch (desc.fileExtension) {
                case ".pvs": {
                    // return opt.useTarget === "js" ? await this.pvsParserJS.parseFile(desc) : 
                    return await this.pvsParser.parseFile(desc); // requires Java
                }
                case ".hpvs": {
                    return await this.ddlParser.parseFile(desc); // requires Java
                }
                default: {
                    console.error(`[parser.parseFile] Error: unrecognized extension ${desc.fileExtension}`);
                }
            }
        }
        return null;
    }

    // parseExpression (expr: string): Diagnostic[] | true {
    //     if (expr) {
    //         return this.pvsParserJS.parseExpression(expr);
    //     }
    //     return null;
    // }
    
    async hp2pvs (desc: { fileName: string, fileExtension: string, contextFolder: string }): Promise<ParserDiagnostics> {
        if (desc) {
            if (desc.fileExtension === ".hpvs") {
                const ofname: string = path.join(desc.contextFolder, `${desc.fileName}.pvs`);
                return await this.ddlParser.parseFile(desc, { output: `${ofname}` });
            }
        }
        return null;
    }

    async prettyPrintDdl(desc: { fileName: string, fileExtension: string, contextFolder: string, expr: string }): Promise<string> {
        if (desc && desc.expr) {
            return await this.ddlParser.prettyPrint(desc);
        }
        return "";
    }

    killParser (): void {
        if (this.pvsParser) { this.pvsParser.stop(); }
        // if (this.ddlParser) { this.ddlParser.stop(); }
    }
}