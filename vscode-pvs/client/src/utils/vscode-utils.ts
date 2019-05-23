import * as vscode from "vscode";
export function log(str: string) { vscode.window.showInformationMessage(str); }

export const textColor: { [ key: string ]: number } = {
    "green": 92
};

export function colorText(text: string, colorCode: number): string {
    // \x1b[0m resets all attributes
	return `\x1b[38;5;${colorCode}m${text}\x1b[0m`;
}