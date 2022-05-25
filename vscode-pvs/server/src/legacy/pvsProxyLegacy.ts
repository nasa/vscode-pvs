/**
 * @module PvsProcessLegacy
 * @author Paolo Masci
 * @date 2019.02.07
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
// import { PvsProcessLegacy } from './pvsProcessLegacy';
import { PvsResponse, ShowTCCsResult, PvsResult, FindDeclarationResult } from '../common/pvs-gui';
import { SimpleConnection, PvsFileDescriptor, TheoryDescriptor, FormulaDescriptor, PvsFormula, SequentDescriptor } from '../common/serverInterface'
import * as path from 'path';
import * as fsUtils from '../common/fsUtils';
import { PvsProcess } from '../pvsProcess';
import { PvsErrorManager } from '../pvsErrorManager';

interface ProofSessionStatus {
    label: string,
    "proof-session-status": string
};

export class PvsProxyLegacy {
	protected pvsPath: string;
	protected pvsLibraryPath: string;
    protected connection: SimpleConnection; // connection to the client
    protected pvsErrorManager: PvsErrorManager;
	protected pvsProcess: PvsProcess;
	constructor (pvsPath: string, opt?: { connection?: SimpleConnection }) {
		opt = opt || {};
		this.pvsPath = pvsPath;
		this.pvsLibraryPath = path.join(pvsPath, "lib");
		this.connection = opt.connection;
		// this.pvsProcess = new PvsProcessLegacy({ pvsPath: this.pvsPath }, this.connection);
	}
	// async startPvs (): Promise<boolean> {
    //     return await this.pvsProcess.activate();
    // }
    async activate (pvsProcess: PvsProcess, opt?: {
        pvsErrorManager?: PvsErrorManager
    }): Promise<void> {
        opt = opt || {};
        this.pvsProcess = pvsProcess;
        this.pvsErrorManager = opt.pvsErrorManager;
        if (!this.pvsProcess) {
            console.log(`[pvs-proxy-legacy] Activating stand-alone pvs process`);
            this.pvsProcess = new PvsProcess(this.pvsPath, opt);
            await this.pvsProcess.activate({ externalServer: true });
        }
    }
    async lisp (cmd: string, opt?: { quiet?: boolean }): Promise<PvsResponse> {
        opt = opt || {};
        // let data: string = await this.pvsProcess.sendText(`(lisp (let ((*in-checker* nil)) ${cmd}))`);
        let data: string = (this.pvsProcess) ? await this.pvsProcess.sendText(`(lisp ${cmd})`) : "";
        if (data) {
            data = data.split("\n").filter(line => { 
                return !line.startsWith("127.0.0")
                        && !(line.trim() === "nil")
                        && line.trim();
            }).join("\n");
            if (data && data.startsWith(`"`) && data.endsWith(`"`)) {
                data = data.substring(1, data.length - 1);
            }
        }
        const matchPvsError: RegExpMatchArray = /Error: (.+)/g.exec(data);
        const matchInterrupted: RegExpMatchArray = /Error: Interrupted by client/gm.exec(data);
        if (matchPvsError && !matchInterrupted && this.pvsErrorManager) {
            const error_string: string = data;
            const msg: string = (error_string.includes("not find lib-path")) ? 
                error_string + ". Please add external pvs libraries to vscode-pvs settings" 
                    : error_string;
            if (!opt.quiet) {
                // this.pvsErrorManager.notifyPvsFailure({ msg, src: "pvs-server (pvs-proxy-legacy)" });
            }
            return {
                jsonrpc: "2.0",
                id: "pvs-process-legacy",
                error: {
                    data: {
                        error_string
                    }
                }
            };    
        }
        return {
            jsonrpc: "2.0",
            id: "pvs-process-legacy",
            result: data
        };
    }
    async getProverStatus (): Promise<PvsResponse> {
        const inchecker: PvsResponse = await this.lisp("*in-checker*");
        if (inchecker && inchecker.result === "t") {
            return {
                jsonrpc: "2.0",
                id: "pvs-process-legacy",
                result: "active"
            };    
        }
        return {
            jsonrpc: "2.0",
            id: "pvs-process-legacy",
            result: "inactive"
        };
    }
    async proofCommand (cmd: string): Promise<PvsResponse> {
        const pvsResponse: PvsResponse = {
            jsonrpc: "2.0",
            id: "pvs-process-legacy"
        };
        let data: string = (this.pvsProcess) ? await this.pvsProcess.sendText(cmd) : "";
        if (data.includes("Error:")) {
            const error_string: string = data.substring(data.indexOf("Error:"), data.length + 1);
            pvsResponse.error = {
                message: error_string
            };
            return pvsResponse;
        }
        if (data.includes("</pvserror>")) {
            const error_string: string = data.substring(data.indexOf("</pvserror>") + 11, data.length + 1);
            pvsResponse.error = {
                message: error_string
            };
            return pvsResponse;
        }
        // else
        if (data.indexOf("[") !== 0) {
            console.log(cmd); // this is done only for debugging purposes, to check when vscode-output patch erroneously includes extraneous output
        }
        // let qed: boolean = data.endsWith("nil"); // this is a workaround while waiting vscode-output patch to be fixed 
        data = data.substring(data.indexOf("["), data.lastIndexOf("]") + 1);
        if (data) {
            try {
                const result: (SequentDescriptor | ProofSessionStatus)[] = JSON.parse(data);
                // convert result to ProofState[] (pvsResponse.result must be of type ProofState[])
                pvsResponse.result = [];
                for (let i = 0; i < result.length; i++) {
                    if (result[i]["prover-session-status"]) {
                        pvsResponse.result.push({
                            label: result[i].label,
                            commentary: (result[i].label.includes(".")) ? [ `This completes the proof in branch ${result[i].label}` ] : [ `Q.E.D.` ]
                        });
                    } else {
                        pvsResponse.result.push(result[i]);
                    }
                }
                // if (qed) {
                //     pvsResponse.result[pvsResponse.result.length - 1].commentary = pvsResponse.result[pvsResponse.result.length - 1].commentary || [];
                //     pvsResponse.result[pvsResponse.result.length - 1].commentary.push("Q.E.D.");
                //     pvsResponse.result[pvsResponse.result.length - 1].label = "Q.E.D."; // temporary fix while waiting vscode-output patch to be fixed so it always includes a label
                // }
            } catch (jsonerror) {
                console.error(jsonerror);
                console.error(data);
                pvsResponse.error = jsonerror;
                return pvsResponse;
            }
        }
        return pvsResponse;
    }
    async changeContext (ctx: string): Promise<PvsResponse> {
        const response: PvsResponse = await this.lisp(`(change-workspace "${ctx}" t)`);
        if (response.result) {
            const matchFolder: RegExpMatchArray = /\"(.+)\"/g.exec(response.result);
            if (matchFolder && matchFolder.length > 1) {
                return {
                    result: matchFolder[1],
                    jsonrpc: "2.0",
                    id: "pvs-process-legacy"
                };        
            }
        }
        return {
            jsonrpc: "2.0",
            id: "pvs-process-legacy"
        };
    }
    async proveFormula(desc: { contextFolder: string, fileName: string, fileExtension: string, theoryName: string, formulaName: string }): Promise<PvsResponse> {
        const pvsResponse: PvsResponse = {
            jsonrpc: "2.0",
            id: "pvs-process-legacy"
        };
        const response: PvsResponse = await this.lisp(`(prove-formula "${desc.theoryName}#${desc.formulaName}")`);
        // fix for commentary erroneously printed by pvs
        const data: string = response.result.substring((<string> response.result).indexOf("["), (<string> response.result).lastIndexOf("]") + 1);
        try {
            pvsResponse.result = <SequentDescriptor[]> JSON.parse(data);
        } catch (jsonerror) {
            console.error(data);
            console.error(jsonerror);
            pvsResponse.error = jsonerror;
            return pvsResponse;
        }
        return pvsResponse;
    }
    async showTccs (fname: string, theoryName: string): Promise<PvsResponse> {
        const pvsResponse: PvsResponse = {
            jsonrpc: "2.0",
            id: "pvs-process-legacy"
        };
        if (this.pvsProcess && fsUtils.getFileExtension(fname) === ".pvs") {
            const fullName: string = path.join(fname + "#" + theoryName);
            const response: PvsResponse = await this.lisp(`(show-tccs "${fullName}")`);
            const result: ShowTCCsResult = [];
            if (response && response.result) { 
                const ans: string = response.result;    
                const tccs: string[] = ans.split(";");
                for (let i = 0; i < tccs.length; i++) {
                    const info: string = tccs[i];
                    let comment: string = ""; 
                    let matchComment: RegExpMatchArray = null;
                    let proved: boolean = false;
                    const regexpComment: RegExp = /(%\s*.+)/g;
                    const regexpStatus: RegExp = /%\s*(?:is\s+)?(subsumed|simplified|proved|unproved|unfinished|unchecked|untried)/g;
                    while (matchComment = regexpComment.exec(info)) {
                        const matchStatus: RegExpMatchArray = regexpStatus.exec(matchComment[1]);
                        if (matchStatus) {
                            proved = matchStatus[1] === "subsumed" || matchStatus[1] === "simplified" || matchStatus[1] === "proved";
                        } else {
                            comment += "\n" + matchComment[1];
                        }
                    }

                    let id: string = "";
                    const matchId: RegExpMatchArray = /(.+): OBLIGATION/g.exec(info);
                    if (matchId?.length > 1 && /^[A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*/g.test(matchId[1]?.trim())) {
                        id = matchId[1];            
                    }

                    let definition: string = "";
                    let matchDefinition: RegExpMatchArray = null;
                    const regexpDefinition: RegExp = /: OBLIGATION\b([\w\W\s]+)/g;
                    while (matchDefinition = regexpDefinition.exec(info)) {
                        definition += matchDefinition[1] + "\n";
                    }

                    // add subsumed tccs
                    let subsumed: string = "";
                    let matchSubsumed: RegExpMatchArray = null;
                    const regexpSubsumed: RegExp = /%\s*The subtype TCC [\w\W\s]* is subsumed by .*\s/g;
                    while(matchSubsumed = regexpSubsumed.exec(tccs[i])) {
                        if (matchSubsumed && matchSubsumed.length > 0) {
                            subsumed += matchSubsumed[0].trim();
                        }
                    }

                    if (id && comment) {
                        result.push({
                            comment: [ comment.trim() ],
                            definition: definition.trim(),
                            id,
                            proved,
                            theory: theoryName,
                            "from-decl": null
                        });
                    }
                    
                    if (subsumed) {
                        result.push({
                            "subsumed-tccs": subsumed,
                            theory: theoryName
                        });
                    }

                }
            }
            pvsResponse.result = result;
        }
        return pvsResponse;
    }
    async parseFile (fname: string): Promise<PvsResponse> {
        const pvsResponse: PvsResponse = {
            jsonrpc: "2.0",
            id: "pvs-process-legacy"
        };
        if (this.pvsProcess && fsUtils.getFileExtension(fname) === ".pvs") {
            const response: PvsResponse = await this.lisp(`(parse-file "${fname}" nil)`, { quiet: true });
            const res: string = (response) ? response.result : "";
            const matchParseError: RegExpMatchArray = /\berror\"\>\s*\"([\w\W\s]+)\bIn file\s+([\w\W\s]+)\s+\(line\s+(\d+)\s*,\s*col\s+(\d+)/gm.exec(res);
            const matchAssertionError: RegExpMatchArray = /\bError:[\w\W\s]+/gm.exec(res);
            if (matchParseError || matchAssertionError) {
                const error_string: string = (matchParseError && matchParseError.length > 3) ? matchParseError[1].trim().replace(/\\n/g, "\n")
                    : matchAssertionError[0];
                const file_name: string = (matchParseError && matchParseError.length > 3) ? matchParseError[2].trim() : fname;
                const line: string = (matchParseError && matchParseError.length > 3) ? matchParseError[3] : "1";
                const character: string = (matchParseError && matchParseError.length > 3) ? matchParseError[4] : "0";
                pvsResponse.error = {
                    data: {
                        place: [ +line, +character ],
                        error_string,
                        file_name
                    }
                };
            } else {                
                pvsResponse.result = [];
            }
        }
        return pvsResponse;
    }
    /**
     * Typecheck file
     */
    async typecheckFile (fname: string): Promise<PvsResponse> {
        const pvsResponse: PvsResponse = {
            jsonrpc: "2.0",
            id: "pvs-process-legacy"
        };
        if (this.pvsProcess && fsUtils.getFileExtension(fname) === ".pvs") {
            const response: PvsResponse = await this.lisp(`(typecheck-file "${fname}" nil nil nil nil t)`, { quiet: true });
            if (response && response.error) {
                return response;
            }

            const res: string = (response) ? response.result : "";
            const matchTypecheckError: RegExpMatchArray = /\b(?:pvs)?error\"\>\s*\"([\w\W\s]+)\bIn file\s+([\w\W\s]+)\s+\(line\s+(\d+)\s*,\s*col\s+(\d+)\s*\)/gm.exec(res);
            const matchTypecheckError2: RegExpMatchArray = /<pvserror msg=\"(?:[\w\W\s]+)\"\>\s*\"([\w\W\s]+)\bIn file\s+([^\()]+)(?:\s+\(line\s+(\d+)\s*,\s*col\s+(\d+)\s*\))?\"/gm.exec(res);
            const matchLibraryError: RegExpMatchArray = /\bError: ([\w\W\s]+)\b/gm.exec(res);
            if (matchTypecheckError || matchTypecheckError2 || matchLibraryError) {
                let error_string: string = (matchTypecheckError && matchTypecheckError.length > 3) ? matchTypecheckError[1].trim().replace(/\\n/g, "\n")
                    : (matchTypecheckError2 && matchTypecheckError2.length > 3) ? matchTypecheckError2[1].trim().replace(/\\n/g, "\n")
                    : `Libraries imported by the theory cannot be found`;
                error_string = error_string.replace(/\n+/g, "\n");
                let line: string = (matchTypecheckError && matchTypecheckError.length > 3 && !isNaN(+matchTypecheckError[4])) ? matchTypecheckError[3]
                    : (matchTypecheckError2 && matchTypecheckError2.length > 3 && !isNaN(+matchTypecheckError2[3])) ? matchTypecheckError2[3]
                    : "1";
                let character: string = (matchTypecheckError && matchTypecheckError.length > 3 && !isNaN(+matchTypecheckError[4])) ? matchTypecheckError[4]
                    : (matchTypecheckError2 && matchTypecheckError2.length > 3 && !isNaN(+matchTypecheckError2[3])) ? matchTypecheckError2[4]
                    : "0" ;
                let file_name: string = (matchTypecheckError && matchTypecheckError.length > 3) ? matchTypecheckError[2].trim()
                    : (matchTypecheckError2 && matchTypecheckError2.length > 3) ? matchTypecheckError2[2].trim()
                    : fname;
                if (!file_name.endsWith('.pvs')) {
                    // pvs has not returned the true name, this happens when the file is in the current context
                    file_name = file_name + ".pvs";
                }
                if (!file_name.includes('/')) {
                    // pvs has not returned the true name, we include the name in the error message if the file is not in the current context
                    const candidate: string = path.join(fsUtils.getContextFolder(fname), file_name);
                    if (fsUtils.fileExists(candidate)) {
                        file_name = candidate;
                    } else {
                        error_string = `In imported file ${file_name} (line ${line}, col ${character}): ` + error_string;
                        line = "1";
                        character = "0";
                        file_name = fname; //path.join(fsUtils.getContextFolder(fname), file_name);
                    }
                }
                pvsResponse.error = {
                    data: {
                        place: [ +line, +character ],
                        error_string,
                        file_name
                    }
                };
                const fileName: string = fsUtils.getFileName(file_name);
                this.connection?.sendNotification("server.status.info", { msg: `Typecheck errors in file ${fileName}` });
                return pvsResponse;
            } 
            
            const matchSystemError: boolean = /\bRestart actions \(select using \:continue\)\:/g.test(res);
            if (matchSystemError) {
                const error_string: string = `pvs crashed into Lisp.\n To continue, you may need to reboot pvs.\nPlease report the following error log to vscode-pvs developers:\n(typecheck-file "${fname}" nil nil nil nil t)\n${res}`;
                pvsResponse.error = {
                    data: {
                        place: [ 1, 0 ],
                        error_string,
                        file_name: fname
                    }
                };
                if (this.pvsErrorManager) {
                    this.pvsErrorManager.notifyPvsFailure({ fname, msg: error_string, src: "pvs-proxy-legacy" });
                }
                const fileName: string = fsUtils.getFileName(fname);
                this.connection?.sendNotification("server.status.info", { msg: `Error while trying to typecheck ${fileName}` });
                return pvsResponse;
            }

            pvsResponse.result = res || `File ${fname} typechecks successfully`;
            // this.connection?.sendNotification("server.status.info", { msg: pvsResponse.result?.split("\n")[0] });
        }
        return pvsResponse;
    }
    /**
     * Find symbol declaration
     */
    async findDeclaration (symbolName: string): Promise<PvsResponse> {
        const data: PvsResponse = await this.lisp(`(find-declaration "${symbolName}")`);
        // example result: `((("declname" . "posnat") ("type" . "type") ("theoryid" . "integers")  ("filename"   . "/Users/pmasci/Work/pvs-snapshots/pvs-7.0.1212/lib/prelude.pvs")  ("place" 2194 2 2194 32)  ("decl-ppstring" . "posnat: TYPE+ = posint")))[Current process: Initial Lisp Listener][1]`
        let result: FindDeclarationResult = [];
        try {
            if (data && data.result) {
                data.result = data.result.replace(/\\"/g, '"').replace(/\\\\\//g, "/");
                // pvs 7.1 is now generating a json object
                result = JSON.parse(data.result);
                if (typeof result === "string") {
                    // pvs is erroneously returning a string encoding of a string encoding of a JSON object
                    result = JSON.parse(result);
                }
                if (result && result.length) {
                    for (let i = 0; i < result.length; i++) {
                        result[i]["decl-ppstring"] = result[i]["decl-ppstring"].replace(/\\n/g, "\n");
                    }
                }
                // let declInfo: string[] = data.result.split(`(("declname"`).filter((elem: string) => { return elem.includes(`"place"`)});
                // if (declInfo && declInfo.length) {
                //     for(let i = 0; i < declInfo.length; i++) {
                //         const info: string = `(("declname"` + declInfo[i];
                //         const matchDeclname: RegExpMatchArray = /\(\"declname\".*\.\s*\"([^\)]+)\"/g.exec(info);
                //         const matchDeclPPString: RegExpMatchArray = /\(\"decl-ppstring\"\s*\.\s*\"([\w\W\s]+)\"/g.exec(info);
                //         const matchFilename: RegExpMatchArray = /\(\"filename\"\s*\.\s*\"([^\)]+)\"/g.exec(info);
                //         const matchPlace: RegExpMatchArray = /\(\"place\"\s*([\d\s]+)/g.exec(info);
                //         const matchTheoryId: RegExpMatchArray = /\(\"theoryid\".*\.\s*\"([^\)]+)\"/g.exec(info);
                //         const matchType: RegExpMatchArray = /\(\"type\".*\.\s*\"([^\)]+)\"/g.exec(info);
                //         if (matchDeclname && matchDeclname.length > 1
                //                 && matchDeclPPString && matchDeclPPString.length > 1
                //                 && matchFilename && matchFilename.length > 1
                //                 && matchPlace && matchPlace.length > 1
                //                 && matchTheoryId && matchTheoryId.length > 1) {
                //             const place = matchPlace[1].split(" ");
                //             if (place.length > 1) {
                //                 result.push({
                //                     declname: matchDeclname[1],
                //                     filename: matchFilename[1],
                //                     "decl-ppstring": matchDeclPPString[1],
                //                     place: [
                //                         +place[0], 
                //                         +place[1], 
                //                         (place.length > 2) ? +place[2] : null, 
                //                         (place.length > 3) ? +place[3] : null 
                //                     ],
                //                     theoryid: matchTheoryId[1],
                //                     type: (matchType && matchType.length > 1) ? matchType[1] : null // what is this attribute for??
                //                 });
                //             }
                //         }
                //     }
                // }
            }
        } finally {
            return {
                jsonrpc: "2.0",
                id: "pvs-process-legacy",
                result
            }
        }
    }
    async getDefaultProofScript (formula: PvsFormula): Promise<PvsResponse> {
        let result: string = null; //`;;; Proof ${desc.formulaName}-1 for formula ${desc.theoryName}.${desc.formulaName}`; // this is an empty proof
        const error_string: string = `${formula.theoryName}.${formula.formulaName} does not have a proof`;
        await this.changeContext(formula.contextFolder);
        await this.typecheckFile(fsUtils.desc2fname(formula));
        if (formula && formula.contextFolder && formula.fileExtension && formula.fileName && formula.formulaName) {
            // const isTcc: boolean = utils.isTccFormula(desc);
            // if (isTcc) {
                const cmd: string = `(get-default-proof-script "${formula.theoryName}" "${formula.formulaName}")`;
                const data: PvsResponse = await this.lisp(cmd);
                if (data && data.result) {
                    const matchProof: RegExpMatchArray = /(;;; Proof\b[\w\W\s]+)/.exec(data.result);
                    if (matchProof && matchProof.length > 1) {
                        result = matchProof[1].replace(/\\"/g, `"`); // this is necessary because get-default-proof-script is erroneously escaping double quotes
                    }
                }
            // } else {
            //     // I'm keeping this for backwards compatibility, until the final version of pvs is released
            //     // extension is forced to .pvs, this is necessary as the request may come for a .tccs file
            //     const fname: string = fsUtils.desc2fname(desc);
            //     const fileDesc: PvsFileDescriptor = await utils.getFileDescriptor(fname, { listTheorems: true });
            //     if (fileDesc && fileDesc.theories && fileDesc.theories.length) {
            //         const theoryDesc: TheoryDescriptor[] = fileDesc.theories.filter(tdesc => { return tdesc.theoryName === desc.theoryName });
            //         if (theoryDesc && theoryDesc.length === 1 && theoryDesc[0].theorems && theoryDesc[0].theorems.length > 0) {
            //             const formulaDesc: FormulaDescriptor[] = theoryDesc[0].theorems.filter(formula => { return formula.formulaName === desc.formulaName; });
            //             if (formulaDesc && formulaDesc.length === 1 && formulaDesc[0].position) {
            //                 const line: number = formulaDesc[0].position.line;
            //                 const cmd: string = `(edit-proof-at "${fname}" nil ${line} "pvs" "${desc.fileName}${desc.fileExtension}" 0 nil)`;
            //                 const data: PvsResponse = await this.lisp(cmd);
            //                 if (data && data.result) {
            //                     const matchProof: RegExpMatchArray = /(;;; Proof\b[\w\W\s]+)/.exec(data.result);
            //                     if (matchProof && matchProof.length > 1) {
            //                         result = matchProof[1];
            //                     }
            //                 }
            //             }
            //         }
            //     }
            // }
        }
        // the APIs of PVS are really ugly here -- if the formula does not have a proof returns an error rather than just returning an empty proof
        return (result) ? {
            jsonrpc: "2.0",
            id: "pvs-process-legacy",
            result
        } : {
            jsonrpc: "2.0",
            id: "pvs-process-legacy",
            error: {
                data: {
                    error_string
                }
            }
        };
    }
    async installProofliteScript (desc: PvsFormula, prl: string): Promise<PvsResponse> {
        // To store a prooflite script into the prf file you can use the lisp function:
        // defun install-script (theory script formulas force &optional (overwrite-default-proof? t) (save-prf-file? t))
        // * script is a string containing the prooflite script
        // * theory-name is the name of the theory
        // * formula-name is the name of the formula
        // * If force is nil, the script only is installed if the formula has no proof.
        // * if overwrite-default-proof? is t, the script will replace the current default proof.
        // * The script only is stored to the prf file if save-prf-file? is t.
        // * Parameter overwrite-default-proof? is omitted when force is nil.
        const escaped: string = prl.replace(/\"/g, "\\\"");
        const cmd: string = `(install-script "${desc.theoryName}" "${escaped}" (list "${desc.formulaName}") t)`;
        const data: PvsResponse = await this.lisp(cmd);
        if (data && data.error) {
            console.error(data.error);
        } else if (data && data.result && typeof data.result === "string" && data.result.startsWith("Error:")) {
            console.error(data.result);
        }
        return data;
    }
    /**
     * The status-proofchain command provides a proof chain analysis of the
     * formula at the cursor and displays it in the PVS Status buffer.  The
     * proof chain analysis indicates whether the formula has been proved, and
     * analyses the formulas used in the proof to insure that the proof is
     * complete; lemmas used in the proof are proved, and sound, i.e. there are
     * no circularities.
     * Example invocation: (proofchain-status-at "/test/helloworld/helloworld.pvs" nil 12 "pvs")
     */
    async statusProofChain (desc: PvsFormula): Promise<PvsResponse> {
        // force .pvs extension
        const fname: string = fsUtils.desc2fname({ ...desc, fileExtension: ".pvs" });
        // typecheck the file first, this will prevent typecheck messages to appear in the result of status-proofchain
		let res: PvsResponse = await this.typecheckFile(fname);
        
        if (!res?.error || (typeof res?.result === "string" && !res?.result?.startsWith("Error:"))) {
            const ext: string = desc.fileExtension.replace(".", "");
            const formula: string = desc.fileExtension === ".pvs" ? `"${desc.formulaName}"` : "nil";
            const cmd: string = `(proofchain-status-at "${desc.fileName}" ${formula} ${desc.line} "${ext}")`;
            res = await this.lisp(cmd);
        }
        return res;
    }
}