/**
 * @module PvsDefinitionProvider
 * @version 2019.02.07
 * @description Definition provider for PVS, supports the "goto definition" functionality in the editor.
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

import { PvsDeclarationDescriptor, PvsFindDeclarationResponse, PvsDeclarationType, PRELUDE_FILE, PvsResponseType } from '../common/serverInterface';
import * as language from "../common/languageKeywords";
import { Connection, TextDocument, Position, Range, CancellationToken } from 'vscode-languageserver';
import { PvsProcess } from '../pvsProcess';
import { findTheoryName, getWordRange } from '../common/languageUtils';
import { PvsFindDeclarationInterface, PvsShowImportChain } from '../pvsLisp';

export class PvsDefinitionProvider {
	connection: Connection;
	private pvsProcess: PvsProcess;
	// private isenseSymbols: { [key: string]: PvsDeclarationDescriptor } = {};

	constructor(pvsProcess: PvsProcess) {
		this.pvsProcess = pvsProcess;
	}

	/**
	 * Utility function, prints a warning message in the console
	 * @param msg
	 */
	private printWarning (msg: string) {
		if (this.connection) {
			this.connection.console.warn(msg);
		}
	}
	/**
	 * Utility function, prints a timestamped message in the console
	 * @param msg
	 */
	private printInfo (msg: string) {
		if (this.connection) {
			this.connection.console.info(msg);
		}
	}

	/**
	 * Returns the current pvs context path
	 */
	public getContextPath () {
		return this.pvsProcess.getContextPath();
	}

	/**
	 * Returns the current pvs context path
	 */
	public getLibrariesPath () {
		return this.pvsProcess.getLibrariesPath();
	}

	/**
	 * Utility function, finds a symbol definition
	 * @param document The document that contains the symbol
	 * @param symbolName The symbol whose definition needs to be found
	 * @param line Optional argument, line where the symbol is used, helps to narrow down the list of potential definitions in the case of symbol overloading
	 * @param character Optional argument, character (i.e., column) where the symbol is used, helps to narrow down the list of potential definitions in the case of symbol overloading
	 */
	async findSymbolDefinition (document: TextDocument, symbolName: string, position?: Position): Promise<PvsFindDeclarationResponse> {
		const currentTheory: string = findTheoryName(document.getText(), position.line);
		let response: PvsFindDeclarationResponse = {
			file: document.uri,
			theory: currentTheory,
			line: position.line,
			character: position.character,
			symbolName: symbolName,
			symbolTheory: null,
			symbolDeclaration: null,
			symbolDeclarationRange: null,
			symbolDeclarationFile: null,
			symbolDoc: null,
			comment: null,
			error: null
		};
		let isNumber: boolean = new RegExp(language.PVS_NUMBER_REGEXP_SOURCE).test(symbolName);
		let isString: boolean = new RegExp(language.PVS_STRING_REGEXP_SOURCE).test(symbolName);
		let isKeyword: boolean = new RegExp(language.PVS_RESERVED_WORDS_REGEXP_SOURCE, "gi").test(symbolName);
		if (isNumber) {
			this.printInfo("Number " + symbolName);
			response.comment = "Number  " + symbolName;
		} else if (isString) {
			this.printInfo("String " + symbolName);
			response.comment = "String  " + symbolName;
		} else if (isKeyword) {
			this.printInfo("Keyword " + symbolName);
			response.comment = "Keyword  " + symbolName;
		} else {
			const path = document.uri.trim().split("/");
			const fileName = path[path.length - 1].split(".pvs")[0];
			if (fileName !== PRELUDE_FILE) {
				// find-declaration works even if a pvs file does not parse correctly 
				let ans: PvsResponseType = await this.pvsProcess.findDeclaration(symbolName);
				const allDeclarations: PvsFindDeclarationInterface = ans.res;
				// find-declaration may return more than one result -- the file is not typechecked
				// we can narrow down the results by traversing the importchain
				if (ans.res && ans.res !== {}) {
					// first, check if the symbol is defined in the current theory
					if (currentTheory && allDeclarations[currentTheory + "." + symbolName]) {
						response = allDeclarations[currentTheory + "." + symbolName];
					} else {
						// otherwise check the importchain
						const candidates: PvsDeclarationDescriptor[] = Object.keys(allDeclarations).map(function (key) {
							const info: PvsDeclarationType = allDeclarations[key];
							const ans: PvsDeclarationDescriptor = {
								theory: currentTheory,
								line: position.line,
								character: position.character,
								file: document.uri,
								symbolName: symbolName,
								symbolTheory: info.symbolTheory,
								symbolDeclaration: (info) ? info.symbolDeclaration : null,
								symbolDeclarationRange: (info) ? info.symbolDeclarationRange : null,
								symbolDeclarationFile: (info) ? info.symbolDeclarationFile : null,
								symbolDoc: null,
								comment: null,
								error: null
							}
							return ans;
						});
						if (candidates.length === 0) {
							this.printWarning("Could not find declaration :/");
						} else if (candidates.length === 1) {
							response = candidates[0];
						} else {
							const filteredResults: PvsDeclarationDescriptor[] = candidates.filter(function (desc) {
								return !(desc.symbolDeclarationFile == PRELUDE_FILE && 
											/^\w+\s*:\s*VAR\s+\w+/gi.test(desc.symbolDeclaration)); // VAR declarations from the prelude
							});
							const withoutObsoletePrelude: PvsDeclarationDescriptor[] = filteredResults.filter(function (desc) {
								return !(desc.symbolDeclarationFile == PRELUDE_FILE && 
											new RegExp(language.PVS_PRELUDE_OBSOLETE_THEORIES_REGEXP_SOURCE, "g").test(desc.symbolTheory));
							});
							let fromImportChain: PvsDeclarationDescriptor[] = [];
							const resp: PvsResponseType = await this.pvsProcess.showImportChain(currentTheory);
							const importChain: PvsShowImportChain = resp.res;
							if (importChain.theories.length > 0) {
								fromImportChain = filteredResults.filter(function (desc) {
									return importChain.theories.indexOf(desc.symbolTheory) >= 0;
								});
							}
							if (filteredResults.length === 1) {
								response = filteredResults[0];
							} else if (fromImportChain.length >= 1) {
								response = fromImportChain[0];
							} else if (withoutObsoletePrelude.length === 1) {
								response = withoutObsoletePrelude[0];
							} else {
								this.printWarning("Found several candidate declarations :/");
							}
						}
					}
				}
			}
		}
		return response;
	}

	/**
	 * Standard API of the language server for requesting a symbol definition
	 * @param document Current document
	 * @param position Current cursor position
	 * @param token Cancellation token (optional)
	 */
	async provideDefinition(document: TextDocument, position: Position, token?: CancellationToken): Promise<PvsFindDeclarationResponse> {
		const symbolRange: Range = getWordRange(document.getText(), position); 
		const symbolName: string = document.getText(symbolRange);
		const line: number = symbolRange.start.line + 1; // vscode starts lines from 0, we want to start from 1 (as in pvs)
		const character: number = symbolRange.start.character;
		// const fileName: string = document.uri;
		// const theoryName: string = this.findTheory(document, line);
		return this.findSymbolDefinition(document, symbolName, { line: line, character: character });
	}
}
