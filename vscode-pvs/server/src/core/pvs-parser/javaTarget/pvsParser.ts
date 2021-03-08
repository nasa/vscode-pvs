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

import { spawn, ChildProcess } from 'child_process';
// note: ./common is a symbolic link. if vscode does not find it, try to restart TS server: CTRL + SHIFT + P to show command palette, and then search for Typescript: Restart TS Server
import * as fsUtils from '../../../common/fsUtils';
import { Diagnostic } from 'vscode-languageserver';
import * as path from 'path';

export declare interface ParserDiagnostics extends Diagnostic {
    fileName: string;
    fileExtension: string;
    contextFolder: string;
    "math-objects": {
        types: number,
        definitions: number,
        lemmas: number
    },
    "parse-time": { ms: number },
    "errors"?: Diagnostic[]
};

export declare interface Outline {
    contextFolder: string,
    fileName: string,
    fileExtension: string,
    outline: {
        types: [ { line: number, character: number, identifier: string } ],
        functions: [ { line: number, character: number, identifier: string } ],
        formulas: [ { line: number, character: number, identifier: string } ],
        locals: [ { line: number, character: number, identifier: string } ]
    },
    "parse-time": {
        ms: number
    }
}

export class PvsParser {

    protected workers: { [ fname: string ]: ChildProcess } = {};
    protected processWorker (fname: string, args: string[]): Promise<string> {
        return new Promise((resolve, reject) => {
            // const cmd: string = `java ${args.concat(fname).join(" ")}`;
            // console.log(cmd);
            // const res: Buffer = execSync(cmd);
            // console.log(res.toLocaleString());
            const worker: ChildProcess = spawn("java", args.concat(fname));
            this.workers[fname] = worker;
            worker.stdout.setEncoding("utf8");
            worker.stderr.setEncoding("utf8");
            worker.stdout.on("data", (diags: string) => {
                resolve(diags);
            });
            worker.stderr.on("data", (data: string) => {
                console.error(data);
                // resolve(false);
            });
            worker.on("error", (err: Error) => {
                console.log("[pvs-parser] Process error ", err);
                // console.dir(err, { depth: null });
            });
            worker.on("exit", (code: number, signal: string) => {
                // console.log("[pvs-parser] Process exited with code ", code);
                // file parsed successfully
                resolve(null);
                // console.dir({ code, signal });
            });
            worker.on("message", (message: any) => {
                console.log("[pvs-parser] Process message", message);
                // console.dir(message, { depth: null });
            });
        });
    }

    stop (): void {
        if (this.workers) {
            const keys: string[] = Object.keys(this.workers);
            for (let key in keys) {
                this.workers[key].kill();
            }
        }
    }

    async parseExpression (expr: string): Promise<ParserDiagnostics> {
        // TODO
        return null;
    }

    /**
     * Parse a pvs file
     * @param desc File descriptor, includes file name, file extension, and context folder
     */
    async parseFile (desc: { fileName: string, fileExtension: string, contextFolder: string }): Promise<ParserDiagnostics> {
        const fname: string = fsUtils.desc2fname(desc);
        console.info(`[vscode-pvs-parser] Parsing ${fname}`);

        let diags: ParserDiagnostics = null;
        const libFolder: string = path.join(__dirname, "../../../../out/core/lib");

        // const start: number = Date.now();
        const args: string[] = [ "-jar", `${libFolder}/PvsParser.jar` ]; // this command will produce a JSON object of type Diagnostic[]
        try {
            const ans: string = await this.processWorker(fname, args);
            // const stats: number = Date.now() - start;
            if (ans) {
                console.log(ans);
                diags = JSON.parse(ans);
                if (diags.errors) {
                    console.log(`[vscode-pvs-parser] File ${desc.fileName}${desc.fileExtension} parsed with errors in ${diags["parse-time"].ms}ms`);
                } else {
                    console.log(`[vscode-pvs-parser] File ${desc.fileName}${desc.fileExtension} parsed successfully in ${diags["parse-time"].ms}ms`);
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
     * Get statistics on the total number of definitions contained in the file
     * @param desc File descriptor, includes file name, file extension, and context folder
     */
    async getStats (desc: { fileName: string, fileExtension: string, contextFolder: string }): Promise<Diagnostic[]> {
        const fname: string = fsUtils.desc2fname(desc);
        console.info(`[vscode-pvs-parser] Parsing ${fname}`);

        let diagnostics: Diagnostic[] = [];
        const libFolder: string = path.join(__dirname, "../../../../out/core/lib");

        const start: number = Date.now();
        const options: string = "-stats"; // stats includes statistics about the number of declarations contained in the file
        const args: string[] = [ "-jar", `${libFolder}/PvsParser.jar`, options ]; // this command will produce a JSON object of type Diagnostic[]
        try {
            const diags: string = await this.processWorker(fname, args);
            const stats: number = Date.now() - start;
            if (diags) {
                console.log(diags);
                diagnostics = JSON.parse(diags);
                console.log(`[vscode-pvs-parser] File ${desc.fileName}${desc.fileExtension} parsed with errors in ${stats}ms`);
            } else {
                console.log(`[vscode-pvs-parser] File ${desc.fileName}${desc.fileExtension} parsed successfully in ${stats}ms`);
            }
        } catch (parserError) {
            console.log(parserError);
        } finally {
            // console.log(`[vscode-pvs-parser] Sending diagnostics for ${desc.fileName}${desc.fileExtension}`);
            // if (diagnostics && diagnostics.length > 0) {
            //     console.dir(diagnostics, { depth: null });
            // }
            return diagnostics;
        }
    }
    
    /**
     * Get an outline of the definitions contained in the file
     * @param desc File descriptor, includes file name, file extension, and context folder
     */
    async getOutline (desc: { fileName: string, fileExtension: string, contextFolder: string }): Promise<Outline> {
        const fname: string = fsUtils.desc2fname(desc);
        console.info(`[vscode-pvs-parser] Building outline for ${fname}`);

        let res: Outline = null;
        const libFolder: string = path.join(__dirname, "../../../../out/core/lib");

        const start: number = Date.now();
        const options: string = "-outline"; // stats includes statistics about the number of declarations contained in the file
        const args: string[] = [ "-jar", `${libFolder}/PvsParser.jar`, options ]; // this command will produce a JSON object of type Diagnostic[]
        try {
            const outline: string = await this.processWorker(fname, args);
            const stats: number = Date.now() - start;
            if (outline) {
                console.log(outline);
                res = JSON.parse(outline);
                console.log(`[vscode-pvs-parser] File ${desc.fileName}${desc.fileExtension} parsed with errors in ${stats}ms`);
            } else {
                console.log(`[vscode-pvs-parser] File ${desc.fileName}${desc.fileExtension} parsed successfully in ${stats}ms`);
            }
        } catch (parserError) {
            console.log(parserError);
        } finally {
            // console.log(`[vscode-pvs-parser] Sending diagnostics for ${desc.fileName}${desc.fileExtension}`);
            // if (diagnostics && diagnostics.length > 0) {
            //     console.dir(diagnostics, { depth: null });
            // }
            return res;
        }
	}
}