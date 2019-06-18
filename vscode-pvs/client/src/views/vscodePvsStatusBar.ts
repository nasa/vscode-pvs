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
            this.statusBar.text = `Ready!`;
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