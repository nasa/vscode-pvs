/**
 * @module VSCodePvsTerminalLinkProvider
 * @author Paolo Masci
 * @date 2021.03.09
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

import { 
    window, CancellationToken, ProviderResult, TerminalLink, TerminalLinkContext, 
    TerminalLinkProvider, ExtensionContext, Position, Range
} from "vscode";
import { LanguageClient } from "vscode-languageclient";
import { symbolRegexp } from '../common/languageUtils';
import { FindSymbolDeclarationRequest, FindSymbolDeclarationResponse, PvsTheory, serverEvent, serverRequest } from "../common/serverInterface";
import * as vscodeUtils from '../utils/vscode-utils';
import * as serverInterface from '../common/serverInterface';

/**
 * Provides tooltip and go-to definition functionalities for the pseudo terminal 
 */
export class VSCodePvsTerminalLinkProvider implements TerminalLinkProvider {
    // language client for sending messages to the server
    protected client: LanguageClient;

    /**
     * Constructor
     */
    constructor (client: LanguageClient) {
		this.client = client;
    }
    /**
     * Activation function, registers the link provider
     */
    activate (context: ExtensionContext) {
        window.registerTerminalLinkProvider(this);
    }    

    /**
     * Internal function, creates the go-to definition links
     * @param txt 
     */
    protected createLinks (txt: string, theory: PvsTheory): TerminalLink[] {
        if (txt && theory) {
            const ans: TerminalLink[] = [];
            const regexp: RegExp = new RegExp(symbolRegexp);
            let match: RegExpMatchArray = regexp.exec(txt);
            const maxIterations: number = 100;
            for (let i = 0; i < maxIterations && match && match.length > 1 && match[1]; i++) {
                const link: TerminalLink = {
                    startIndex: match.index,
                    length: match[1].length,
                    tooltip: `Open declaration`
                    // You can return data in this object to access inside handleTerminalLink
                    // data: 'Example data'
                };
                link["theory"] = theory;
                link["symbolName"] = match[1];
                ans.push(link);
                match = regexp.exec(txt);
            }
            return ans;
        }
        return null;
    }

    /**
     * terminal link provider
     */
    provideTerminalLinks(context: TerminalLinkContext, token: CancellationToken): ProviderResult<TerminalLink[]> {
        // context.line is the text in the line where the mouse cursor is currently located
        const links: TerminalLink[] = this.createLinks(context.line, context?.terminal["theory"]);
        return links;
    }
    /**
     * Terminal link handler
     */
    handleTerminalLink(link: TerminalLink): ProviderResult<void> {
        if (link && link["theory"] && link["symbolName"]) {
            const req: FindSymbolDeclarationRequest = {
                symbolName: link["symbolName"],
                theory: link["theory"]
            };
            this.client.sendRequest(serverRequest.findSymbolDeclaration, req);
            this.client.onRequest(serverEvent.findSymbolDeclarationResponse, (res: FindSymbolDeclarationResponse) => {
                if (res.ans && res.ans.length === 1) {
                    const fname: string = res.ans[0].symbolDeclarationFile;
                    if (fname) {
                        const range: serverInterface.Range = res.ans[0].symbolDeclarationRange;
                        if (range?.start && range?.end) {
                            const selection: Range = new Range(
                                new Position(range.start.line - 1, range.start.character),
                                new Position(range.end.line - 1, range.end.character),
                            );
                            vscodeUtils.openFile(fname, { selection });
                        }
                    }
                }
            });
        }
        return null;
    }
    
}