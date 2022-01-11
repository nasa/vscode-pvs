/**
 * @module PvsCodeActionProvider
 * @author Paolo Masci
 * @date 2022.01.10
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

import { CodeAction, CodeActionContext, CodeActionKind, Command, Connection, Position, Range } from "vscode-languageserver";

export class PvsCodeActionProvider {
    protected connection: Connection;

    /**
     * Constructor
     * @param pvsLanguageServer 
     */
    constructor (connection: Connection) {
		this.connection = connection;
    }

    /**
	 * Standard API of the language server, provides a code action
	 * @param document Text document requiring codeaction
	 * @param range Current range selected with the cursor
	 */
    provideCodeAction(document: { txt: string, uri: string }, range: Range, context: CodeActionContext): (Command | CodeAction)[] {
        if (document?.txt && document?.uri && range && context) {
            const actions: CodeAction[] = [];
            if (context.diagnostics?.length) {
                for (let i = 0; i < context.diagnostics.length; i++) {
                    const diag: string = context.diagnostics[i]?.message;
                    const source: string = context.diagnostics[i]?.source;
                    // quickfix action for IMPORTING errors tells the user to add a library path
                    if (diag && /Cannot find theory (.+)/gi.test(diag) && /Typecheck error/gi.test(source)) {
                        // group 1 is the imported theory that cannot be found
                        const match: RegExpMatchArray = /Cannot find theory (.+)/gi.exec(diag);
                        if (match?.length && match[1]) {
                            const fix: string = `Add  ${match[1]}  to PVS library path`
                            const cmd: Command = {
                                title: fix,
                                command: "vscode-pvs.add-pvs-library"
                            };
                            const action: CodeAction = {
                                title: fix,
                                kind: CodeActionKind.QuickFix,
                                command: cmd
                            }
                            actions.push(action);
                        }
                    }
                }
                console.log(range, context);
            }
            return actions;
        }
        return null;
    }
}