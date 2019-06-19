/**
 * @module VSCodePvsStatusBar
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
import { VSCodePvsProofExplorer } from "./vscodePvsProofExplorer";
import { StatusBarItem, ExtensionContext, StatusBarAlignment, window } from "vscode";
import { LanguageClient } from "vscode-languageclient";
import { PvsVersionDescriptor } from "../common/serverInterface";

export class StatusBarPriority {
    public static Low: number = 1;
    public static Medium: number = 2;
    public static High: number = 3;
    public static Max: number = 4;
}

export class VSCodePvsStatusBar {
    private pvsVersionInfo: string;
    private statusBar: StatusBarItem;
    private versionInfoBar: StatusBarItem;

    private client: LanguageClient;

    constructor (client: LanguageClient) {
        this.client = client;
        // register notification handlers
        this.client.onNotification("pvs-ready", (pvsVersion: string) => {
            if (pvsVersion) {
                this.versionInfoBar.text = pvsVersion;
            }
            this.statusBar.text = "";
        });
        this.client.onNotification("server.status.update", (msg: string) => {
			this.statusBar.text = msg;
		});
		this.client.onNotification("server.status.info", (msg: string) => {
			this.statusBar.text = msg;
		});
		this.client.onNotification("server.status.error", (msg: string) => {
			this.statusBar.text = msg;
        });

        this.statusBar = window.createStatusBarItem(StatusBarAlignment.Left, StatusBarPriority.High);
        this.versionInfoBar = window.createStatusBarItem(StatusBarAlignment.Left, StatusBarPriority.Max);
    }

    info (msg: string) {
        this.statusBar.text = msg;
    }

    ready () {
        this.statusBar.text = this.pvsVersionInfo;
    }

    activate (context: ExtensionContext) {
        this.statusBar.show();
        this.versionInfoBar.show();
    }

    getVersionInfo (): string {
        return this.pvsVersionInfo;
    }
}