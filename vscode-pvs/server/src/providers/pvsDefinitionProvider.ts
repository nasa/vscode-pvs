/**
 * @module PvsDefinitionProvider
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

import { PvsDeclarationDescriptor, PvsDefinition, PvsDeclarationType, PRELUDE_FILE, PvsResponseType, PvsFindDeclaration } from '../common/serverInterface';
import * as language from "../common/languageKeywords";
import { Connection, TextDocument, Position, Range, CancellationToken, TextDocuments } from 'vscode-languageserver';
import { findTheoryName, getWordRange } from '../common/languageUtils';
import * as fs from '../common/fsUtils';
import { PvsProxy } from '../pvsProxy';
import * as fsUtils from '../common/fsUtils';
import { FindDeclarationResult, Place, PvsResponse } from '../common/pvs-gui';
import * as path from 'path';

export class PvsDefinitionProvider {
	connection: Connection;
	protected pvsProxy: PvsProxy;
	protected documents: TextDocuments;
	// private isenseSymbols: { [key: string]: PvsDeclarationDescriptor } = {};

	constructor(pvsProxy: PvsProxy, documents: TextDocuments) {
		this.pvsProxy = pvsProxy;
		this.documents = documents;
	}

	/**
	 * Utility function, prints a warning message in the console
	 * @param msg
	 */
	private printWarning (msg: string) {
		if (this.connection) {
			this.connection.console.warn(`[pvs-definition-provider] ${msg}`);
		}
	}
	/**
	 * Utility function, prints a timestamped message in the console
	 * @param msg
	 */
	private printInfo (msg: string) {
		if (this.connection) {
			this.connection.console.info(`[pvs-definition-provider] ${msg}`);
		}
	}

	isProtectedFolder (contextFolder: string): boolean {
		if (this.pvsProxy) {
			return this.pvsProxy.isProtectedFolder(contextFolder);
		}
		return false;
	}

	// async getTermAt(document: TextDocument, position: Position): Promise<PvsDefinition[]> {
	// 	let ans: PvsFindDeclarationResponseType = await this.pvsProcess.getTermAt(document.uri.replace("file://", ""), { line: position.line, character: position.character });
	// 	return null;
	// } 

	/**
	 * Utility function, finds a symbol definition
	 * @param document The document that contains the symbol
	 * @param symbolName The symbol whose definition needs to be found
	 * @param position Position where the symbol is used, helps to narrow down the list of potential definitions in the case of symbol overloading
	 */
	async findSymbolDefinition (uri: string, symbolName: string, position?: Position): Promise<PvsDefinition[]> {
		symbolName = (symbolName.toUpperCase() === "TRUE" || symbolName.toUpperCase() === "FALSE") ? symbolName.toUpperCase() : symbolName;
		let currentTheory: string = null;
		// uri = uri.replace("file://", "");
		if (uri && uri.endsWith(".tccs") && position) {
			currentTheory = fs.getFileName(uri);
		} else {
			// .pvs file
			const txt: string = await fsUtils.readFile(uri);
			currentTheory = (position) ? findTheoryName(txt, position.line) : null;
		}
		let response: PvsDefinition = {
			file: uri,
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
			this.printInfo(`Number ${symbolName}`);
			response.comment = `Number ${symbolName}`;
			return [ response ];
		}
		const isString: boolean = new RegExp(language.PVS_STRING_REGEXP_SOURCE).test(symbolName);
		if (isString) {
			this.printInfo(`String ${symbolName}`);
			response.comment = `String ${symbolName}`;
			return [ response ];
		}
		const isKeyword: boolean = new RegExp(language.PVS_RESERVED_WORDS_REGEXP_SOURCE, "gi").test(symbolName);
		if (isKeyword) {
			symbolName = symbolName;
			this.printInfo(`Keyword ${symbolName}`);
			response.comment = symbolName === 'o' ? `Keyword ${symbolName}` : `Keyword ${symbolName.toUpperCase()}`;
			return [ response ];
		}
		// else
		// const fileName: string = fsUtils.getFilename(document.uri);
		// find-declaration works even if a pvs file does not parse correctly 
		let ans: PvsResponse = await this.pvsProxy.findDeclaration(symbolName); //await this.pvsProcess.findDeclaration(symbolName);

		// find-declaration may return more than one result -- the file is not typechecked
		// we can narrow down the results by traversing the importchain
		// part of this extra logic can be removed when Sam completes the implementation of find-object
		if (ans && ans.result) {
			const declarations: FindDeclarationResult = ans.result;
			if (declarations && typeof declarations === "object" && declarations.length > 0) {
				const candidates: PvsDefinition[] = declarations.map((info: {
					declname?: string;
					type?: string;
					theoryid?: string;
					filename?: string;
					place?: Place;
					"decl-ppstring"?: string;
				}) => {
					let fname: string = info ? info.filename : null;
					if (fname && fname.indexOf("/") < 0) {
						// FIXME: pvs-server does not include path when file is in the current context
						// add contextFolder to fname, check if this is the pvslog folder (if so, remove /pvslog)
						let contextFolder: string = fsUtils.getContextFolder(uri);
						if (contextFolder.endsWith("/pvslog")) {
							contextFolder = contextFolder.split("/").slice(0, -1).join("/");
						}
						fname = path.join(contextFolder, fname);
					}
					const ans: PvsDefinition = {
						theory: currentTheory,
						line: (position) ? position.line : null,
						character: (position) ? position.character : null,
						file: uri,
						symbolName: symbolName,
						symbolTheory: (info) ? info.theoryid : null,
						symbolDeclaration: (info) ? info["decl-ppstring"] : null,
						symbolDeclarationRange: (info && info.place && info.place.length > 1) ? { 
							start: { line: info.place[0], character: info.place[1] }, 
							end: (info.place.length === 4) ? { line: info.place[2], character: info.place[3] } : undefined
						} : null,
						symbolDeclarationFile: fname,
						symbolDoc: null,
						comment: null,
						error: null
					}
					return ans;
				});
			
				// // remove VAR from prelude
				// candidates = candidates.filter(desc => {
				// 	return !(desc.symbolDeclarationFile == PRELUDE_FILE && 
				// 				/^\w+\s*:\s*VAR\s+\w+/gi.test(desc.symbolDeclaration)); // VAR declarations from the prelude
				// });
				// // remove obsolete prelude theories
				// candidates = candidates.filter(desc => {
				// 	return !(desc.symbolDeclarationFile == PRELUDE_FILE && 
				// 				new RegExp(language.PVS_PRELUDE_OBSOLETE_THEORIES_REGEXP_SOURCE, "g").test(desc.symbolTheory));
				// });
				if (candidates.length === 0) {
					this.printWarning("Warning: Could not find declaration :/");
					return null;
				}
				if (candidates.length === 1) {
					response = candidates[0];
					const isBuiltinType: boolean = new RegExp(language.PVS_BUILTIN_TYPE_REGEXP_SOURCE, "g").test(symbolName);
					if (isBuiltinType) {
						response.comment = `Builtin type ${symbolName}`;
					} else {
						const isTrueFalse: boolean = new RegExp(language.PVS_TRUE_FALSE_REGEXP_SOURCE, "gi").test(symbolName);
						if (isTrueFalse) {
							response.comment = `Builtin constant ${symbolName}`;
						} else {
							response.comment = `User-defined symbol ${symbolName}`
						}
					}
					return [ response ];
				}
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
	async provideDefinition(document: { uri: string, txt: string, position: Position }, token?: CancellationToken): Promise<{ symbolName: string, definitions: PvsDefinition[] }> {
		if (document && document.uri && document.txt && document.position) {
			const symbolRange: Range = getWordRange(document.txt, document.position);
			// console.log(`Provide definition`, symbolRange);
			// sanity check
			if (symbolRange.end.character > document.position.character) {
				const symbolName: string = fsUtils.getText(document.txt, symbolRange);
				const line: number = symbolRange.start.line + 1; // vscode starts lines from 0, we want to start from 1 (as in pvs)
				const character: number = symbolRange.start.character;
				// const fileName: string = document.uri;
				// const theoryName: string = this.findTheory(document, line);
				// console.log("(line, character) ", line, character);
				// await this.getTermAt(document, { line, character });

				const definitions: PvsDefinition[] = await this.findSymbolDefinition(document.uri, symbolName, { line: line, character: character });
				return { symbolName, definitions };
			}
		}
		return null;
	}
}
