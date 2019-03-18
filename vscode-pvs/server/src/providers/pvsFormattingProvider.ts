import { Connection, TextDocument, Position, Range, CancellationToken } from 'vscode-languageserver';
import { PvsProcess } from '../pvsProcess';

export class PvsRangeFormattingProvider {
	connection: Connection;
	private pvsProcess: PvsProcess;

	/**
	 * @constructor
	 * @param pvsProcess PVS lisp process, necessary for executing prettyprint-region 
	 */
	constructor(pvsProcess: PvsProcess) {
		this.pvsProcess = pvsProcess;
	}
}