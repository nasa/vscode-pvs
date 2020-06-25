import { PvsLanguageClient } from "../pvsLanguageClient";
import { ExtensionContext } from "vscode";
import { TestVSCodePvsProofExplorer } from "./testPvsProofExplorer";

export class TestPvsLanguageClient extends PvsLanguageClient {
    activate (context: ExtensionContext): void {		
		// create language client
        this.createLanguageClient(context);
        // start client
        this.client.start();
		this.client.onReady().then(() => {
            const proofExplorer: TestVSCodePvsProofExplorer = new TestVSCodePvsProofExplorer(this.client, 'proof-explorer-view');
            proofExplorer.activate(this.context);

            proofExplorer.test();
        });
    }

}


// client instance
const pvsLanguageClient = new TestPvsLanguageClient();
export function activate(context: ExtensionContext) {
	// Activate the client.
	pvsLanguageClient.activate(context); // async call
}
export function deactivate(): Thenable<void> {
	return new Promise((resolve, reject) => {
		pvsLanguageClient.stop().then(() => {
			resolve();
		});
	});
}