
import { window, ViewColumn, WebviewPanel } from 'vscode';
import * as handlebars from 'handlebars';

import * as templates from './templates/sequents_template';

export class SequentView {
	private panel: WebviewPanel

	constructor() {
		// Create and show panel
		this.panel = window.createWebviewPanel(
			'sequents',
			'PVS Proof Status',
			window.activeTextEditor.viewColumn + 1,
			{}
		);
	}

	showSequents(content: string) {
		// Set its HTML content
		this.panel.webview.html = this.createWebviewContent(content);
		return this;
	}

	private createWebviewContent(content: string) {
		let regExp: RegExp = new RegExp(/([\[\{]-?\d+[\}\]]\s)?(.+)/g); // first capture group is the sequent number, second capture group is the sequent formula
		let match: RegExpMatchArray = null;
		let sequents: { id: string, content: string }[] = [];
		let lines: string[] = content.split("\n").filter(function (line) {
			return line.trim() !== ""
					&& line.trim() !== "|-------"
					&& line.trim() !== "Rule?";
		});
		let lastID: string = null;
		for (let i in lines) {
			match = regExp.exec(lines[i]);
			if (match[1]) {
				lastID = match[1];
			}
			if (match[2]) {
				if (lastID) {
					if (sequents[match[1]]) {
						sequents[match[1]].content += match[2];
					} else {
						sequents.push({
							id: match[1],
							content: match[2]
						});
					}
				}
			}
		}
		let html = handlebars.compile(templates.sequents)({
			sequents: sequents
		});

		return html;
	}
}