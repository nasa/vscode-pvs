import { VSCodePvsProofExplorer } from "./vscodePvsProofExplorer";
import { StatusBarItem, ExtensionContext, StatusBarAlignment, window } from "vscode";
import { LanguageClient } from "vscode-languageclient";

export class StatusBarPriority {
    public static Low: number = 1;
    public static Medium: number = 2;
    public static High: number = 3;
}

export class VSCodePvsStatusBar {
    private pvsVersionInfo: string;
    private statusBar: StatusBarItem;
    private progressBar: StatusBarItem;

    private client: LanguageClient;

    constructor (client: LanguageClient) {
        this.client = client;
        // register notification handlers
        this.client.onNotification("pvs-ready", (pvsVersionInfo: string) => {
            this.statusBar.text = this.pvsVersionInfo = pvsVersionInfo;
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
    }

    info (msg: string) {
        this.statusBar.text = msg;
    }

    ready () {
        this.statusBar.text = this.pvsVersionInfo;
    }

    activate (context: ExtensionContext) {
        this.statusBar.show();
    }

    getVersionInfo (): string {
        return this.pvsVersionInfo;
    }
}