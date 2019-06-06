/**
 * @module PvsLisp
 * @version 2019.03.14
 * PvsLisp, transform pvs lisp output into JSON objects. 
 * @author Paolo Masci
 * @date 2019.03.14
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

import { spawn, ChildProcess } from 'child_process';
import * as language from "./common/languageKeywords";
import { 
	PvsResponseType, PRELUDE_FILE, PvsDeclarationType, TccDescriptor,
	StrategyDescriptor, TheoryStatus, TheoremsStatus
} from './common/serverInterface'
import * as utils from './common/languageUtils';

export interface PvsFindDeclarationInterface {
    [ key: string ] : PvsDeclarationType;
}
export interface PvsShowImportChain {
	theories: string[] // list of theories, ordered by import
}


// utility functions and constants
const PVS_LISP_ERROR_RETURN_TO_TOP_LEVEL: string = 'Return to Top Level (an "abort" restart)';
function getKey(theoryName: string, symbolName: string): string {
	return theoryName + "." + symbolName;
}

export interface Console {
    console: { 
        log: (str: string) => void,
        error: (str: string) => void,
        info: (str: string) => void,
        warn: (str: string) => void
    }
}

export class PvsLispReader {
	private pvsOut: string = "";
	private connection: Console = null;
	/**
	 * Constructor
	 */
	constructor (connection?: Console) {
		this.connection = connection;
	}
	/**
	 * Reads pvs lisp output.
	 * @param data A string representing the output of pvs lisp
	 * @param cb Callback function to be invoked when the output is ready (i.e., when the pvs ready prompt is detected)
	 */
	async read(data: string, cb: (out:string) => void) {
		this.pvsOut += data;
		// see also regexp from emacs-src/ilisp/ilisp-acl.el
		// const PVS_COMINT_PROMPT_REGEXP: RegExp = /\s*pvs\(\d+\):|([\w\W\s]*)\spvs\(\d+\):/g;
		// const PVSIO_PROMPT: RegExp = /<PVSio>/g;
		// const PROVER_PROMPT: RegExp = /\bRule\?/g;
		// const QUERY_YES_NO: RegExp = /\?\s*\(Y or N\)|\?\s*\(Yes or No\)/gi;
		// const ready: boolean = PVS_COMINT_PROMPT_REGEXP.test(data)
		// 						|| PVSIO_PROMPT.test(data)
		// 						|| PROVER_PROMPT.test(data)
		// 						|| QUERY_YES_NO.test(data);
		// if (ready) {
		// 	const ans: string = this.pvsOut;
		// 	this.pvsOut = ""
		// 	cb(ans);
		// }

		const PVS_COMINT_PROMPT_REGEXP: RegExp = /(.*)\s*pvs\(\d+\):|(.*)\[.+\]\s*pvs\(\d+\):/g; // capture group 1 is the pvs lisp output
		const PVSIO_PROMPT: RegExp = /<PVSio>/g;
		const PROVER_PROMPT: RegExp = /\bRule\?/g;
		const QUERY_YES_NO: RegExp = /\?\s*\(Y or N\)|\?\s*\(Yes or No\)/gi;
		if (PVS_COMINT_PROMPT_REGEXP.test(data) || PVSIO_PROMPT.test(data) 
				|| PROVER_PROMPT.test(data) || QUERY_YES_NO.test(data)) {
			const ans: string = (PVS_COMINT_PROMPT_REGEXP.test(data)) ?
								PVS_COMINT_PROMPT_REGEXP.exec(data)[1]
								: this.pvsOut;
			this.pvsOut = ""
			cb(ans);
		}

	}
	parse(commandId: string, data: string): PvsResponseType {
		const ans: PvsResponseType = {
			error: null,
			res: null,
			raw: data
		};
		switch (commandId) {
			case "get-pvs-version-information": {
				const PVS_VERSION_INFO_REGEXP: { [s:string]: RegExp } = {
					brief: /"(\d+.?\d*)"[\s|nil]*"([^"]*)"/g,
					full: /"(\d+.?\d*)"[\s|nil]*"([^"]*)"\s*"(\d+.?\d*)([^"]*)/g
				};
				let info = PVS_VERSION_INFO_REGEXP.brief.exec(data);
				if (info && info.length > 2) {
					const pvsVersion: string = info[1];
					const lispVersion: string = info.slice(2).join(" ");
					ans.res = {
						pvsVersion: "PVS " + pvsVersion,
						lispVersion: lispVersion
					};
				} else {
					if (this.connection) { this.connection.console.warn("Unexpected pvs version response\n" + data); }
					ans.res = {
						pvsVersion: data,
						lispVersion: ""
					};
				}
				break;
			}
			case "pvs-init": {
				ans.res = "pvs process ready!";
				break;
			}
			case "parser-init": {
				ans.res = "parser ready!";
				break;
			}
			case "parse-file":
			case "json-typecheck-file":
			case "typecheck-file": {
				if (data.includes(PVS_LISP_ERROR_RETURN_TO_TOP_LEVEL)) {
					const PVS_PARSE_FILE_ERROR_REGEXP = /Parsing (.*)([\w\W\n]+)In file (\w*)\s*\(line (\d+), col (\d+)\)\s+Error: parse error/gim;
					const matchParserError: RegExpMatchArray = PVS_PARSE_FILE_ERROR_REGEXP.exec(data);
					if (matchParserError && matchParserError.length === 6) {
						ans.error = {
							msg: `Error while parsing ${matchParserError[1]}`,
							parserError: {
								msg: matchParserError[2],
								fileName: matchParserError[3],
								line: +matchParserError[4],
								character: +matchParserError[5]
							}
						};
					} else {
						const PVS_TYPECHECK_ERROR_REGEXP = /(?:[\w\W\n]*)<pvserror .*>\s+"([\w\W\n]+)In file (\w*)\s*\(line (\d+), col (\d+)\)(?:[\w\W\n]+)<\/pvserror>/gim;
						const matchTypecheckError: RegExpMatchArray = PVS_TYPECHECK_ERROR_REGEXP.exec(data);
						if (matchTypecheckError && matchTypecheckError.length === 5) {
							ans.error = {
								msg: `Error while typechecking ${matchTypecheckError[1]}`,
								parserError: {
									msg: matchTypecheckError[1].split("\\n").join("\n"),
									fileName: matchTypecheckError[2],
									line: +matchTypecheckError[3],
									character: +matchTypecheckError[4]
								}
							};
						} else {
							if (this.connection) { this.connection.console.error("Unexpected parser error\n" + data); }
						}
					}
				} else if (/PVS file .+ is not in the current context/gim.test(data)) {
					const match: RegExpMatchArray = /(PVS file (.+) is not in the current context)/gim.exec(data);
					ans.error = {
						msg: match[0],
						parserError: {
							msg: `PVS file ${match[0]} is not in the current context`,
							fileName: (match.length > 2) ? match[2] : "",
							line: 0,
							character: 0
						}
					};
				} else if (data.includes("</pvserror>")) {
					// detailed information on the error are currently not provided by pvs
					ans.error = {
						msg: "Typecheck error",
						parserError: {
							msg: `PVS file does not typecheck correctly`,
							fileName: "",
							line: 0,
							character: 0
						}
					};
				} else {
					ans.res = data;
				}
				break;
			}
			case "list-declarations":
			case "find-declaration": {
				// regexp for parsing the pvs lisp response
				const PVS_FIND_DECLARATION_REGEXP = {
					prelude: /\("[^"]*\s*"\s*"([^\s"]*)"\s*"([^\s"]*)"\s*([nil]*)\s*\((\d+\s*\d+\s*\d+\s*\d+)\)\s*"([^"]*)"\)/g,
					other: /\("[^"]*\s*"\s*"([^\s"]*)"\s*"([^\s"]*)"\s*"([^"]*)"*\s*\((\d+\s*\d+\s*\d+\s*\d+)\)\s*"([^"]*)"\)/g
				}
				// key is symbolFile.symbolTheory.symbolName, value is { symbolKind: string, symbolDeclaration: string, symbolDeclarationRange: vscode.Range }
				let declarations: PvsFindDeclarationInterface = {};
				let info = null;
				while (info = PVS_FIND_DECLARATION_REGEXP.prelude.exec(data)) {
					if (info && info.length > 5) {
						const symbolName: string = info[1];
						const symbolTheory: string = info[2];
						const symbolDeclarationFile: string = PRELUDE_FILE;
						const symbolDeclarationPosition = info[4].split(" ");
						const symbolDeclarationRange = {
							start: {
								line: +symbolDeclarationPosition[0],
								character: +symbolDeclarationPosition[1]
							},
							end: {
								line: +symbolDeclarationPosition[2],
								character: +symbolDeclarationPosition[3]
							}
						};
						const symbolDeclaration: string = info[5];
						declarations[getKey(symbolTheory, symbolName)] = {
							symbolName: symbolName,
							symbolTheory: symbolTheory,
							symbolDeclaration: symbolDeclaration,
							symbolDeclarationRange: symbolDeclarationRange,
							symbolDeclarationFile: symbolDeclarationFile
						};
					}
				}
				while (info = PVS_FIND_DECLARATION_REGEXP.other.exec(data)) {
					if (info && info.length > 5) {
						const symbolName: string = info[1];
						const symbolTheory: string = info[2];
						const symbolDeclarationFile: string = info[3];
						const symbolDeclarationPosition = info[4].split(" ");
						const symbolDeclarationRange = {
							start: {
								line: +symbolDeclarationPosition[0],
								character: +symbolDeclarationPosition[1]
							},
							end: {
								line: +symbolDeclarationPosition[2],
								character: +symbolDeclarationPosition[3]
							}
						};
						const symbolDeclaration: string = info[5];
						declarations[getKey(symbolTheory, symbolName)] = {
							symbolName: symbolName,
							symbolTheory: symbolTheory,
							symbolDeclaration: symbolDeclaration,
							symbolDeclarationRange: symbolDeclarationRange,
							symbolDeclarationFile: symbolDeclarationFile
						};
					}
				}
				ans.res = declarations;
				break;
			}
			case "show-importchain": {
				const res: PvsShowImportChain = {
					theories: []
				};
				const regexp: RegExp = /Theory\s+([^\s]+) is\s*/gi;
				let match: RegExpMatchArray = null;
				while(match = regexp.exec(data)) {
					if (match && match.length > 1) {
						res.theories.push(match[1]);
					}
				}
				regexp.lastIndex = 0;
				ans.res = res;
				break;
			}
			case "show-tccs": {
				ans.raw = data;
				const matchTcc: RegExpMatchArray = /(%[\w\W\s]*)\bnil\b/.exec(data);
				if (matchTcc && matchTcc.length > 1) {
					data = matchTcc[1].trim(); // this removes two trailing lines included in the pvs response (nil + pvs prompt)
					ans.raw = data;
					const res: TccDescriptor[] = [];
					// capture group 1: tcc message (Subtype TCC generated at ...)
					// capture group 2: position (line) of the symbol that has triggered the tcc
					// capture group 3: position (column) of the symbol that has triggered the tcc
					// capture group 4: formulaName
					const regexp: RegExp = new RegExp(/(%\s*(?:.+)(?: generated)?\(at line (\d+), column (\d+)\)).*(?:\s*%.*\s)*([\w\?]+):\s*OBLIGATION/g);
					let match: RegExpMatchArray = null;
					while (match = regexp.exec(data)) {
						if (match.length > 4 && match[4]) {
							const formulaName: string = match[4];
							const line: number = utils.findProofObligation(formulaName, data);
							res.push({
								formulaName,
								line: (line > 0) ? line : 1, // position of the formula in the tccs file
								symbolLine: +match[2],
								symbolCharacter: +match[3]
							});
						}
					}
					ans.res = res;
				}
				break;
			}
			case "change-context": {
				const regexp: RegExp = /Context changed to (.*)\s*\".*\"/;
				const match: RegExpMatchArray = regexp.exec(data);
				if (match && match.length > 1 && match[1]) {
					ans.res = {
						context: match[1]
					}
				}
				break;
			}
			case "step-proof":
			case "edit-proof-at": {
				const regexp: RegExp = /[\s\w\W]*;;;\s*Proof .+ for formula .*\s;;;.*\s([\s\w\W]*)/gim;
				const match: RegExpMatchArray = regexp.exec(data);
				if (match && match.length > 1 && match[1]) {
					const innerMatch = /([\s\w\W]*)\snil/gim.exec(match[1]);
					if (innerMatch && innerMatch[1]) {
						ans.res = innerMatch[1].trim();
					} else {
						ans.error = {
							msg: "Error: Unable to parse output provided by step-proof"
						};
					}
				}
				break;
			}
			case "collect-strategy-names": {
				const regexp: RegExp = /(?:\s*\"([\w\-\+\?\!\*\@]+)\")/g;
				let match: RegExpMatchArray = null;
				const strategies: StrategyDescriptor[] = [];
				while (match = regexp.exec(data)) {
					if (match.length > 1) {
						const strat: StrategyDescriptor = {
							name: match[1],
							description: ""
						};
						strategies.push(strat);
					}
				}
				ans.res = strategies;
				break;
			}
			case "status-proof-theory":
			case "prove-tccs-theory": {
				// match[1] is theoryName, match[2] is the list of theorems, match[3] are time stats
				const regexp: RegExp = /\bProof summary for theory\s*([\w\d]+)\s*([\w\W\s]*)/g;
				let match: RegExpMatchArray = regexp.exec(data);
				if (match && match.length > 1 && match[1]) {
					const theoryStatus: TheoremsStatus = {
						theoryName: match[1],
						theorems: {}
					};
					const details: string = match[2];
					// match[1] is formulaName, match[2] is status, match[3] is time to prove
					// NOTE: formula names may include ?, therefore \w+ is not good, and regex needs to be [\w\?]+
					const regex_t: RegExp = /([\w\?]+)\.*(.*)\s*\[.*\]\((.*)\)/g;
					while (match = regex_t.exec(details)) {
						if (match.length > 3) {
							const formulaName: string = match[1];
							const status: string = match[2].trim();
							const time: string = match[3].trim();
							theoryStatus.theorems[formulaName] = {
								status,
								time 
							}
						}
					}
					ans.res = theoryStatus;
				}
				break;
			}
			case "current-context":
			case "disable-gc-printout":
			case "emacs-interface":
			case "pvs-continue":
			case "load-pvs-attachments":
			case "evaluation-mode-pvsio":
			case "install-prooflite-scripts":
			case "install-proof":
			case "prove-formula":
			default: {
				ans.res = data;
			}
		}
		return ans;
	}
}