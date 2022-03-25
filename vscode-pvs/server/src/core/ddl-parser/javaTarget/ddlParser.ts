/**
 * @module PvsParser (Java target)
 * @author Paolo Masci
 * @date 2019.12.27
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

import { execSync } from 'child_process';
// note: ./common is a symbolic link. if vscode does not find it, try to restart TS server: CTRL + SHIFT + P to show command palette, and then search for Typescript: Restart TS Server
import * as fsUtils from '../../../common/fsUtils';
import { Diagnostic } from 'vscode-languageserver';
import * as path from 'path';
import { ParserDiagnostics } from '../../pvs-parser/javaTarget/pvsParser';
import * as os from 'os';

export class DdlParser {

    /**
     * Parse a hybrid program file
     * @param desc File descriptor, includes file name, file extension, and context folder
     */
    async parseFile (desc: { fileName: string, fileExtension: string, contextFolder: string }, opt?: { output: string }): Promise<ParserDiagnostics> {
        const ofname: string = (opt && opt.output) ? opt.output : "";

        const ifname: string = fsUtils.desc2fname(desc);
        console.info(`[ddl-parser] Parsing ${ifname}`);

        let diags: ParserDiagnostics = null;
        const libFolder: string = path.join(__dirname, "../../../../out/core/lib");

        // const start: number = Date.now();
        let cmd: string = `cd ${libFolder} && java -jar DdlParser.jar ${ifname}`; // this command will produce a JSON object of type Diagnostic[] on stdout
        if (ofname) {
            console.log(`[ddl-parser] Writing file ${ofname}`);
            cmd += ` -out ${ofname};`;
        }
        try {
            const ans: Buffer = execSync(cmd);
            // const stats: number = Date.now() - start;
            if (ans) {
                const res: string = ans.toLocaleString();
                if (res) {
                    diags = JSON.parse(res);
                    if (diags && diags.errors && diags.errors.length > 0) {
                        let msg: string = `File ${desc.fileName}${desc.fileExtension} parsed with errors`;
                        if (diags["parse-time"]) { msg += ` in ${diags["parse-time"].ms}ms`; }
                        console.log(`[vscode-pvs-parser] ${msg}`);
                    } else {
                        let msg: string = `File ${desc.fileName}${desc.fileExtension} parsed successfully`;
                        if (diags && diags["parse-time"]) { msg += ` in ${diags["parse-time"].ms}ms`; }
                        console.log(`[vscode-pvs-parser] ${msg}`);
                    }
                }
            }
        } catch (parserError) {
            console.log(parserError);
        } finally {
            // console.log(`[vscode-pvs-parser] Sending diagnostics for ${desc.fileName}${desc.fileExtension}`);
            // if (diagnostics && diagnostics.length > 0) {
            //     console.dir(diagnostics, { depth: null });
            // }
            return diags;
        }
    }
    
    /**
     * Pretty prints a pvs expressions to ddl
     * @param desc File descriptor, includes file name, file extension, and context folder
     */
    async prettyPrint (desc: { fileName: string, fileExtension: string, contextFolder: string, expr: string }): Promise<string> {
        if (desc && desc.expr) {
            console.info(`[ddl-parser] Pretty printing pvs expression ${desc.expr}`);

            const ddlFile: string = path.join(os.tmpdir(), "ddlFile.tmp");
            fsUtils.writeFile(ddlFile, desc.expr);
            const libFolder: string = path.join(__dirname, "../../../../out/core/lib");
            // const start: number = Date.now();
            let cmd: string = `cd ${libFolder} && java -jar DdlPrettyPrinter.jar ${ddlFile}`; // this command will produce a JSON object of type Diagnostic[] on stdout
            let res: string = "";
            try {
                const ans: Buffer = execSync(cmd);
                // const stats: number = Date.now() - start;
                if (ans) {
                    res = ans.toLocaleString();
                }
            } finally {
                return res;
            }
        }
        return "";
	}
}