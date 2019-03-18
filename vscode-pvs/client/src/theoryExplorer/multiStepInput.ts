import { LanguageClient } from 'vscode-languageclient';
import { QuickPickItem, window, Disposable, CancellationToken, QuickInputButton, QuickInput, ExtensionContext, QuickInputButtons, Uri } from 'vscode';

interface InputBoxParameters {
	title: string;
	step: number;
	totalSteps: number;
	value: string;
	prompt: string;
	validate: (value: string) => Promise<string | undefined>;
	buttons?: QuickInputButton[];
	shouldResume: () => Thenable<boolean>;
};

export class MultiStepInput {
	private current?: QuickInput;
	private client: LanguageClient;

	constructor (client: LanguageClient) {
		this.client = client;
	}

	async createInputBox<P extends InputBoxParameters>() {
		const disposables: Disposable[] = [];
		const _this = this;
		try {
			return await new Promise<string | (P extends { buttons: (infer I)[] } ? I : never)>((resolve, reject) => {
				const input = window.createInputBox();
				input.title = "Please enter prover command";
				input.step = 1;
				// input.totalSteps = totalSteps;
				input.value = "";
				input.prompt = ">> ";
				// input.buttons = [
				// 	...(this.steps.length > 1 ? [QuickInputButtons.Back] : []),
				// 	...(buttons || [])
				// ];
				disposables.push(
					input.onDidTriggerButton(item => {
						resolve(<any>item);
					}),
					input.onDidAccept(async () => {
						const cmd = input.value; // this is the prover command
						input.enabled = false;
						input.busy = true;
						input.enabled = true;
						input.busy = false;

						return new Promise(function (resolve, reject) {
							_this.client.onRequest("server.response.continue-proof", async function (resp) {
								resolve(resp);
							});
							_this.client.sendRequest("pvs.continue-proof", cmd);
						});
					}),
					input.onDidChangeValue(async text => {
					}),
					input.onDidHide(() => {
					})
				);
				if (this.current) {
					this.current.dispose();
				}
				this.current = input;
				this.current.show();
			});
		} finally {
			disposables.forEach(d => d.dispose());
		}
	}
}