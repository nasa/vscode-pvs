/**
 * @module VSCodePvsEchoTerminal
 * @author Paolo Masci
 * @date 2021.04.29
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
import { LanguageClient } from 'vscode-languageclient';
 
 /**
  * VSCodePvsEchoTerminal is used for showing feedback to the user about commands such as download pvs, update nasalib, etc.
  */
 export class VSCodePvsEchoTerminal implements vscode.Pseudoterminal {
    protected writeEmitter = new vscode.EventEmitter<string>();
    protected closeEmitter = new vscode.EventEmitter<void>();

    // event handlers inherited by pseudotermial
    onDidWrite: vscode.Event<string> = this.writeEmitter.event;
    onDidClose?: vscode.Event<void> = this.closeEmitter.event;
    onDidOverrideDimensions?: vscode.Event<vscode.TerminalDimensions>;
    handleInput?(data: string): void {};
    setDimensions?(dimensions: vscode.TerminalDimensions): void {};
    open(initialDimensions: vscode.TerminalDimensions): void {};
    close(): void {};

    // client for sending commands to be executed to the server/
    protected client: LanguageClient;

    /**
     * Constructor
     */
    constructor (client: LanguageClient) {
        this.client = client;
    }

    /**
     * Prints data in the pseudo terminal
     * @param data 
     */
    log (data: string, opt?: { addNewLine?: boolean }): void {
        if (data !== null && data !== undefined) {
            // the pseudo terminal needs \r\n, because \n moves to the next line but keeps the column
            let out: string = data.replace(/\n/g, "\r\n");
            if (opt?.addNewLine) {
                out += "\r\n";
            }
            // write data to the pseudo terminal
            this.writeEmitter?.fire(out);
        }
    }
 }