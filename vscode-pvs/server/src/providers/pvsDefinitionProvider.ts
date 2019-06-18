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

import { PvsDeclarationDescriptor, PvsDefinition, PvsDeclarationType, PRELUDE_FILE, PvsResponseType } from '../common/serverInterface';
import * as language from "../common/languageKeywords";
import { Connection, TextDocument, Position, Range, CancellationToken, TextDocuments } from 'vscode-languageserver';
import { PvsProcess } from '../pvsProcess';
import { findTheoryName, getWordRange, getText } from '../common/languageUtils';
import { PvsFindDeclarationInterface, PvsShowImportChain } from '../pvsLisp';
import * as fs from '../common/fsUtils';

export class PvsDefinitionProvider {
	connection: Connection;
	private pvsProcess: PvsProcess;
	private documents: TextDocuments;
	// private isenseSymbols: { [key: string]: PvsDeclarationDescriptor } = {};

	constructor(pvsProcess: PvsProcess, documents: TextDocuments) {
		this.pvsProcess = pvsProcess;
		this.documents = documents;
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
		return this.pvsProcess.getContextFolder();
	}

	/**
	 * Returns the current pvs context path
	 */
	public getLibrariesPath () {
		return this.pvsProcess.getPvsLibraryPath();
	}

	/**
	 * Utility function, finds a symbol definition
	 * @param document The document that contains the symbol
	 * @param symbolName The symbol whose definition needs to be found
	 * @param line Optional argument, line where the symbol is used, helps to narrow down the list of potential definitions in the case of symbol overloading
	 * @param character Optional argument, character (i.e., column) where the symbol is used, helps to narrow down the list of potential definitions in the case of symbol overloading
	 */
	async findSymbolDefinition (document: TextDocument, symbolName: string, position?: Position): Promise<PvsDefinition[]> {
		let currentTheory: string = null;
		if (document.uri.endsWith(".tccs") && position) {
			currentTheory = fs.getFilename(document.uri, { removeFileExtension: true });
		} else {
			// .pvs file
			currentTheory = (position) ? findTheoryName(document.getText(), position.line) : null;
		}
		let response: PvsDefinition = {
			file: document.uri,
			theory: currentTheory,
			line: (position) ? position.line : null,
			character: (position) ? position.character : null,
			symbolName: symbolName,
			symbolTheory: null,
			symbolDeclaration: null,
			symbolDeclarationRange: null,
			symbolDeclarationFile: null,
			symbolDoc: null,
			comment: null,
			error: null
		};
		const isNumber: boolean = new RegExp(language.PVS_NUMBER_REGEXP_SOURCE).test(symbolName);
		if (isNumber) {
			this.printInfo("Number " + symbolName);
			// response.comment = "Number  " + symbolName;
			return [ response ];
		}
		const isString: boolean = new RegExp(language.PVS_STRING_REGEXP_SOURCE).test(symbolName);
		if (isString) {
			this.printInfo("String " + symbolName);
			response.comment = "String " + symbolName;
			return [ response ];
		}
		const isKeyword: boolean = new RegExp(language.PVS_RESERVED_WORDS_REGEXP_SOURCE, "gi").test(symbolName);
		if (isKeyword) {
			this.printInfo("Keyword " + symbolName);
			response.comment = "Keyword " + symbolName.toUpperCase();
			return [ response ];
		} 
		// else
		const path = document.uri.trim().split("/");
		const fileName = path[path.length - 1].split(".pvs")[0];
		if (fileName !== PRELUDE_FILE) {
			// find-declaration works even if a pvs file does not parse correctly 
			let ans: PvsResponseType = await this.pvsProcess.findDeclaration(symbolName);
			// find-declaration may return more than one result -- the file is not typechecked
			// we can narrow down the results by traversing the importchain
			// part of this extra logic can be removed when Sam completes the implementation of find-object
			if (ans.res && ans.res !== {}) {
				const allDeclarations: PvsFindDeclarationInterface = ans.res;
				let candidates: PvsDefinition[] = Object.keys(allDeclarations).map(key => {
					const info: PvsDeclarationType = allDeclarations[key];
					const ans: PvsDefinition = {
						theory: currentTheory,
						line: (position) ? position.line : null,
						character: (position) ? position.character : null,
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
				// remove VAR from prelude
				candidates = candidates.filter(desc => {
					return !(desc.symbolDeclarationFile == PRELUDE_FILE && 
								/^\w+\s*:\s*VAR\s+\w+/gi.test(desc.symbolDeclaration)); // VAR declarations from the prelude
				});
				// remove obsolete prelude theories
				candidates = candidates.filter(desc => {
					return !(desc.symbolDeclarationFile == PRELUDE_FILE && 
								new RegExp(language.PVS_PRELUDE_OBSOLETE_THEORIES_REGEXP_SOURCE, "g").test(desc.symbolTheory));
				});
				if (candidates.length === 0) {
					this.printWarning("Could not find declaration :/");
					return null;
				}
				if (candidates.length === 1) {
					response = candidates[0];
					const isBuiltinType: boolean = new RegExp(language.PVS_BUILTIN_TYPE_REGEXP_SOURCE, "g").test(symbolName);
					if (isBuiltinType) {
						response.comment = 'Builtin type ' + symbolName;
					}
					return [ response ];
				} 
				// else
				return candidates;
			}
		}
		return null;
	}

	/**
	 * Standard API of the language server for requesting a symbol definition
	 * @param document Current document
	 * @param position Current cursor position
	 * @param token Cancellation token (optional)
	 */
	async provideDefinition(document: TextDocument, position: Position, token?: CancellationToken): Promise<PvsDefinition[]> {
		const txt: string = document.getText();
		const symbolRange: Range = getWordRange(txt, position);
		// console.log(`Provide definition`, symbolRange);
		// sanity check
		if (symbolRange.end.character > position.character) {
			const symbolName: string = document.getText(symbolRange);
			const line: number = symbolRange.start.line + 1; // vscode starts lines from 0, we want to start from 1 (as in pvs)
			const character: number = symbolRange.start.character;
			// const fileName: string = document.uri;
			// const theoryName: string = this.findTheory(document, line);
			const definitions: PvsDefinition[] = await this.findSymbolDefinition(document, symbolName, { line: line, character: character });
			return definitions;
		}
		return null;
	}
}
