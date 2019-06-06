import { VSCodePvsProofExplorer } from "./vscodePvsProofExplorer";
import { StatusBarItem, ExtensionContext, StatusBarAlignment, window } from "vscode";
import { LanguageClient } from "vscode-languageclient";

export class StatusBarPriority {
    public static Low: number = 1;
    public static Medium: number = 2;
    public static High: number = 3;
    public static Max: number = 4;
}

export class VSCodePvsStatusBar {
    private pvsVersionInfo: string;
    private statusBar: StatusBarItem;
    private workersBar: StatusBarItem;
    private parserCount: number = 0;
    private checkerCount: number = 0;

    private client: LanguageClient;

    private updateWorkersBar() {
        const parsers: string = (this.parserCount === 1) ? `${this.parserCount} Parser` : `${this.parserCount} Parsers`;
        const checkers: string = (this.checkerCount === 1) ? `${this.checkerCount} Typechecker` : `${this.checkerCount} Typecheckers`;
        this.workersBar.text = `Workers: [ ${parsers} | ${checkers} ]`;
    }

    constructor (client: LanguageClient) {
        this.client = client;
        // register notification handlers
        this.client.onNotification("pvs-ready", (pvsVersionInfo: string) => {
            this.statusBar.text = this.pvsVersionInfo = `${pvsVersionInfo} ready!`;
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

        this.client.onNotification("server.new-pvs-parser", () => {
            this.parserCount++;
            this.updateWorkersBar();
        });
        this.client.onNotification("server.new-pvs-typechecker", () => {
            this.checkerCount++;
            this.updateWorkersBar();
        });
        this.client.onNotification("server.delete-pvs-parser", () => {
            this.parserCount--;
            this.updateWorkersBar();
        });
        this.client.onNotification("server.delete-pvs-typechecker", () => {
            this.checkerCount--;
            this.updateWorkersBar();
        });
        this.statusBar = window.createStatusBarItem(StatusBarAlignment.Left, StatusBarPriority.High);
        this.workersBar = window.createStatusBarItem(StatusBarAlignment.Left, StatusBarPriority.Max);
        this.updateWorkersBar();
    }

    info (msg: string) {
        this.statusBar.text = msg;
    }

    ready () {
        this.statusBar.text = this.pvsVersionInfo;
    }

    activate (context: ExtensionContext) {
        this.statusBar.show();
        this.workersBar.show();
    }

    getVersionInfo (): string {
        return this.pvsVersionInfo;
    }
}