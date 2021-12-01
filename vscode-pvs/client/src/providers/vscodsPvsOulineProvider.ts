/**
 * @module VSCodePvsOutlineProvider
 * @author Paolo Masci
 * @date 2020.02.26
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

import * as vscode from 'vscode';
import { LanguageClient } from "vscode-languageclient";
import { serverRequest, serverEvent, TheoryDescriptor, PvsFileDescriptor, FormulaDescriptor } from '../common/serverInterface';
import * as fsUtils from '../common/fsUtils';

export class VSCodePvsFileOutlineProvider implements vscode.DocumentSymbolProvider {
    protected client: LanguageClient;
    protected context: vscode.ExtensionContext;

    /**
     * Common data structure, maintains information about the kind of objects recognized by the outline provider 
     */
    static readonly kind: { [key: string]: vscode.SymbolKind } = {
        theory: vscode.SymbolKind.Class,
        formula: vscode.SymbolKind.Method
    }

    /**
     * Constructor
     */
	constructor (client: LanguageClient) {
		this.client = client;
    }
    
    /**
     * Registers the outline provider
     * @param context Extension context
     */
    activate (context: vscode.ExtensionContext) {
        this.context = context;
        vscode.languages.registerDocumentSymbolProvider({ scheme: 'file', language: 'pvs' }, this);//, metaData?: DocumentSymbolProviderMetadata);
    }

    /**
     * main API of the outline provider
     */
    provideDocumentSymbols(document: vscode.TextDocument, token: vscode.CancellationToken): vscode.ProviderResult<vscode.SymbolInformation[] | vscode.DocumentSymbol[]> {
        return new Promise ((resolve, reject) => {
            const contextFolder: string = fsUtils.getContextFolder(document.fileName);
            const fileName: string = fsUtils.getFileName(document.fileName);
            const fileExtension: string = fsUtils.getFileExtension(document.fileName);
            this.client.sendRequest(serverRequest.getFileDescriptor, { contextFolder, fileName, fileExtension });
            this.client.onRequest(serverEvent.getFileDescriptorResponse, async (fdesc: PvsFileDescriptor) => {
                const dsym: vscode.DocumentSymbol[] = [];
                if (fdesc) {
                    if (fdesc.theories && fdesc.fileName === fileName && fdesc.contextFolder === contextFolder && fdesc.fileExtension === fileExtension) {
                        for (let i = 0; i < fdesc.theories.length; i++) {
                            const theory: TheoryDescriptor = fdesc.theories[i];
                            const startLine: number = theory.position.line - 1;
                            let endLine: number = startLine;
                            let endCol: number = 100;

                            // find theory end
                            if (theory?.theoryName && theory.fileName && theory.fileExtension === ".pvs") {
                                const fname: string = fsUtils.desc2fname(theory);
                                if (fname) {
                                    const content: string = await fsUtils.readFile(fname);
                                    if (content) {
                                        const match: RegExpMatchArray = new RegExp(`\\bEND\\s+${theory.theoryName}\\b`).exec(content);
                                        if (match && match[0]) {
                                            const frag: string = content.substring(0, match.index);
                                            endLine = frag.split("\n").length;
                                        }
                                    }
                                }
                            }

                            // assign range to symbol, this will feed outline view and trigger the creation of breadcrumbs in the editor, see https://code.visualstudio.com/Docs/editor/editingevolved#_breadcrumbs
                            const range: vscode.Range = new vscode.Range(
                                new vscode.Position(endLine, theory.position.character),
                                new vscode.Position(startLine, endCol)
                            );
                            const detail: string = (fileExtension === ".tccs") ? `(tccs)` : `(theory)`;
                            const symbol: vscode.DocumentSymbol = new vscode.DocumentSymbol(
                                theory.theoryName,
                                detail,
                                VSCodePvsFileOutlineProvider.kind.theory,
                                range,
                                range
                            );
                            symbol.children = [];
                            if (theory.theorems && theory.theorems.length) {
                                for (let j = 0; j < theory.theorems.length; j++) {
                                    const thm: FormulaDescriptor = theory.theorems[j];
                                    const thmRange: vscode.Range = new vscode.Range(
                                        new vscode.Position(thm.position.line - 1, thm.position.character),
                                        new vscode.Position(thm.position.line - 1, 100)
                                    );
                                    const thmDesc: vscode.DocumentSymbol = new vscode.DocumentSymbol(
                                        thm.formulaName,
                                        `(formula)`,
                                        VSCodePvsFileOutlineProvider.kind.formula,
                                        thmRange,
                                        thmRange
                                    );
                                    symbol.children = [ thmDesc ].concat(symbol.children);
                                }
                            }
                            dsym.push(symbol);
                        }
                    }
                }
                resolve(dsym);
            });
        });
    }
}