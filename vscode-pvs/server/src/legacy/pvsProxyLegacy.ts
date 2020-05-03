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
import { SimpleConnection } from '../common/serverInterface'
import * as path from 'path';
import * as fsUtils from '../common/fsUtils';
import { PvsProcess } from '../pvsProcess';

export class PvsProxyLegacy {
	protected pvsPath: string;
	protected pvsLibraryPath: string;
	protected connection: SimpleConnection; // connection to the client
	pvsProcess: PvsProcess;
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
    async sendCommand (cmd: string): Promise<PvsResponse> {
        const data: string = await this.pvsProcess.sendText(cmd);
        return {
            jsonrpc: "2.0",
            id: "pvs-process-legacy",
            result: data
        };
    }
    async changeContext (ctx: string): Promise<PvsResponse> {
        const response: PvsResponse = await this.sendCommand(`(change-workspace "${ctx}")`);
        return {
            jsonrpc: "2.0",
            id: "pvs-process-legacy"
        };
    }
    async showTccs (fname: string, theoryName: string): Promise<PvsResponse> {
        const pvsResponse: PvsResponse = {
            jsonrpc: "2.0",
            id: "pvs-process-legacy"
        };
        if (this.pvsProcess && fsUtils.getFileExtension(fname) === ".pvs") {
            const fullName: string = path.join(fname + "#" + theoryName)
            const response: PvsResponse = await this.sendCommand(`(show-tccs "${fullName}")`);
            const result: ShowTCCsResult = [];
            if (response && response.result) { 
                const ans: string = response.result;    
                const tccs: string[] = ans.split(";");
                for (let i = 0; i < tccs.length; i++) {
                    let comment: string = ""; 
                    let matchComment: RegExpMatchArray = null;
                    let proved: boolean = false;
                    const regexpComment: RegExp = /(%\s*.+)/g;
                    const regexpStatus: RegExp = /%\s*(?:is\s+)?(subsumed|simplified|proved|unproved|unfinished|unchecked|untried)/g;
                    while (matchComment = regexpComment.exec(tccs[i])) {
                        const matchStatus: RegExpMatchArray = regexpStatus.exec(matchComment[1]);
                        if (matchStatus) {
                            proved = matchStatus[1] === "subsumed" || matchStatus[1] === "simplified" || matchStatus[1] === "proved";
                        } else {
                            comment += "\n" + matchComment[1];
                        }
                    }

                    let id: string = "";
                    const matchId: RegExpMatchArray = /(.+): OBLIGATION/g.exec(tccs[i]);
                    if (matchId) {
                        id = matchId[1];            
                    }

                    let definition: string = "";
                    let matchDefinition: RegExpMatchArray = null;
                    const regexpDefinition: RegExp = /: OBLIGATION\b([\w\W\s]+)/g;
                    while (matchDefinition = regexpDefinition.exec(tccs[i])) {
                        definition += matchDefinition[1] + "\n";
                    }

                    if (comment) {
                        result.push({
                            comment: [ comment.trim() ],
                            definition: definition.trim(),
                            id,
                            proved,
                            theory: theoryName,
                            "from-decl": null
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
            const res: string = await this.pvsProcess.sendText(`(parse-file "${fname}" nil)`);
            const match: RegExpMatchArray = /\berror\"\>\s*\"([\w\W\s]+)\bIn file\s+([\w\W\s]+)\s+\(line\s+(\d+)\s*,\s*col\s+(\d+)/gm.exec(res);
            if (match && match.length > 3) {
                const error_string: string = match[1].trim().replace("\\n", "\n");
                const file_name: string = match[2].trim();
                const line: string = match[3];
                const character: string = match[4];
                pvsResponse.error = {
                    data: {
                        place: [ +line, +character ],
                        error_string,
                        file_name
                    }
                }
            } else {                
                pvsResponse.result = [];
            }
        }
        return pvsResponse;
    }
    async typecheckFile (fname: string): Promise<PvsResponse> {
        const pvsResponse: PvsResponse = {
            jsonrpc: "2.0",
            id: "pvs-process-legacy"
        };
        if (this.pvsProcess && fsUtils.getFileExtension(fname) === ".pvs") {
            const res: string = await this.pvsProcess.sendText(`(typecheck-file "${fname}" nil nil nil nil t)`);
            const match: RegExpMatchArray = /\berror\"\>\s*\"([\w\W\s]+)\bIn file\s+([\w\W\s]+)\s+\(line\s+(\d+)\s*,\s*col\s+(\d+)/gm.exec(res);
            if (match && match.length > 3) {
                const error_string: string = match[1].trim().replace("\\n", "\n");
                let file_name: string = match[2].trim();
                if (!file_name.includes('/')) {
                    // pvs has not returned the true name, this happens when the file is in the current context
                    file_name = path.join(fsUtils.getContextFolder(fname), file_name);
                }
                if (!file_name.endsWith('.pvs')) {
                    // pvs has not returned the true name, this happens when the file is in the current context
                    file_name = file_name + ".pvs";
                }
                const line: string = match[3];
                const character: string = match[4];
                pvsResponse.error = {
                    data: {
                        place: [ +line, +character ],
                        error_string,
                        file_name
                    }
                }
            } else {
                pvsResponse.result = res;
            }
        }
        return pvsResponse;
    }
    async findDeclaration (symbolName: string): Promise<PvsResponse> {
        const data: PvsResponse = await this.sendCommand(`(find-declaration "${symbolName}")`);
        // example result: `((("declname" . "posnat") ("type" . "type") ("theoryid" . "integers")  ("filename"   . "/Users/pmasci/Work/pvs-snapshots/pvs-7.0.1212/lib/prelude.pvs")  ("place" 2194 2 2194 32)  ("decl-ppstring" . "posnat: TYPE+ = posint")))[Current process: Initial Lisp Listener][1]`
        let result: FindDeclarationResult = [];
        if (data && data.result) {
            let declInfo: string[] = data.result.split(`(("declname"`).filter((elem: string) => { return elem.includes(`"place"`)});
            if (declInfo && declInfo.length) {
                for(let i = 0; i < declInfo.length; i++) {
                    const info: string = `(("declname"` + declInfo[i];
                    const matchDeclname: RegExpMatchArray = /\(\"declname\".*\.\s*\"([^\)]+)\"/g.exec(info);
                    const matchDeclPPString: RegExpMatchArray = /\(\"decl-ppstring\"\s*\.\s*\"([\w\W\s]+)\"/g.exec(info);
                    const matchFilename: RegExpMatchArray = /\(\"filename\"\s*\.\s*\"([^\)]+)\"/g.exec(info);
                    const matchPlace: RegExpMatchArray = /\(\"place\"\s*([\d\s]+)/g.exec(info);
                    const matchTheoryId: RegExpMatchArray = /\(\"theoryid\".*\.\s*\"([^\)]+)\"/g.exec(info);
                    const matchType: RegExpMatchArray = /\(\"type\".*\.\s*\"([^\)]+)\"/g.exec(info);
                    if (matchDeclname && matchDeclname.length > 1
                            && matchDeclPPString && matchDeclPPString.length > 1
                            && matchFilename && matchFilename.length > 1
                            && matchPlace && matchPlace.length > 1
                            && matchTheoryId && matchTheoryId.length > 1) {
                        const place = matchPlace[1].split(" ");
                        if (place.length > 1) {
                            result.push({
                                declname: matchDeclname[1],
                                filename: matchFilename[1],
                                "decl-ppstring": matchDeclPPString[1],
                                place: [
                                    +place[0], 
                                    +place[1], 
                                    (place.length > 2) ? +place[2] : null, 
                                    (place.length > 3) ? +place[3] : null 
                                ],
                                theoryid: matchTheoryId[1],
                                type: (matchType && matchType.length > 1) ? matchType[1] : null // what is this attribute for??
                            });
                        }
                    }
                }
            }
        }
        return {
            jsonrpc: "2.0",
            id: "pvs-process-legacy",
            result
        }
    }
}