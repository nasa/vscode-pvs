/**
 * @module PvsTypechecker (Java target)
 * @author Paolo Masci
 * @date 2020.01.13
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

import { spawn, ChildProcess } from 'child_process';
// note: ./common is a symbolic link. if vscode does not find it, try to restart TS server: CTRL + SHIFT + P to show command palette, and then search for Typescript: Restart TS Server
import * as fsUtils from '../../../common/fsUtils';
import { Diagnostic } from 'vscode-languageserver';
import * as path from 'path';

export class PvsTypechecker {

    protected workers: { [ fname: string ]: ChildProcess } = {};
    protected processWorker (fname: string, args: string[]): Promise<string> {
        return new Promise((resolve, reject) => {
            const worker: ChildProcess = spawn("java", args.concat(fname));
            this.workers[fname] = worker;
            worker.stdout.setEncoding("utf8");
            worker.stderr.setEncoding("utf8");
            worker.stdout.on("data", (diags: string) => {
                resolve(diags);
            });
            worker.stderr.on("data", (data: string) => {
                console.log("[pvs-typechecker] Error: ", data);
                // resolve(false);
            });
            worker.on("error", (err: Error) => {
                console.log("[pvs-typechecker] Process error ", err);
                // console.dir(err, { depth: null });
            });
            worker.on("exit", (code: number, signal: string) => {
                // console.log("[pvs-typechecker] Process exited with code ", code);
                // file parsed successfully
                resolve(null);
                // console.dir({ code, signal });
            });
            worker.on("message", (message: any) => {
                console.log("[pvs-typechecker] Process message", message);
                // console.dir(message, { depth: null });
            });
        });
    }

    /**
     * Typecheck a pvs file
     * @param desc File descriptor, includes file name, file extension, and context folder
     */
    async typecheckFile (desc: { fileName: string, fileExtension: string, contextFolder: string }): Promise<Diagnostic[]> {
        const fname: string = fsUtils.desc2fname(desc);
        console.info(`[vscode-pvs-typechecker] Typechecking ${fname}`);

        let diagnostics: Diagnostic[] = [];
        const libFolder: string = path.join(__dirname, "../../../../out/core/lib");

        const start: number = Date.now();
        const args: string[] = [ "-jar", `${libFolder}/PvsTypechecker.jar` ]; // this will produce a JSON object of type Diagnostic[]
        try {
            const diags: string = await this.processWorker(fname, args);
            const stats: number = Date.now() - start;
            if (diags) {
                diagnostics = JSON.parse(diags);
                console.log(`[vscode-pvs-typechecker] File ${desc.fileName}${desc.fileExtension} typechecked with errors in ${stats}ms`);
            } else {
                console.log(`[vscode-pvs-typechecker] File ${desc.fileName}${desc.fileExtension} typechecked successfully in ${stats}ms`);
            }
        } catch (typecheckError) {
            console.log(typecheckError);
        } finally {
            // console.log(`[vscode-pvs-typechecker] Sending diagnostics for ${desc.fileName}${desc.fileExtension}`);
            // if (diagnostics && diagnostics.length > 0) {
            //     console.dir(diagnostics, { depth: null });
            // }
            return diagnostics;
        }
	}
}