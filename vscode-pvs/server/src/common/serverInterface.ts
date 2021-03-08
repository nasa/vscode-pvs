/**
 * @module serverInterface
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

import { PvsResponse } from "./pvs-gui";
import { ProofMateProfile } from "./commandUtils";
import { ProofOrigin, SequentDescriptor } from "./languageUtils";

/**
 * PVSio modes
 * - standard: classic pvsio behavior
 * - state-machine: the pvs specification is considered as a state machine, with an initial state and transition functions
 */
export type PvsIoMode = "standard" | "state-machine";

export declare interface Position {
	line: number, // this attribute ranges from 1 to n, while vscode.line ranges from 0 to n-1 
	character: number
};

export declare interface Range {
	start: Position,
	end: Position
};

export declare interface PeekDefinitionCommand {
	fileName: string,
	range: Range
};

export declare interface PvsVersionDescriptor {
	"pvs-version": string,
	"lisp-version": string,
	"nasalib-version": string
};

export declare interface ErrorType {
	message: string,
	fileName?: string,
	line?: number,
	character?: number
};

export declare interface PvsDeclarationType {
	symbolName: string | null;
	theoryName: string | null;
	symbolDeclaration: string | null;
	symbolDeclarationRange: Range | null;
	symbolDeclarationFile: string | null;
};

export declare interface PvsDeclarationDescriptor {
	symbolName: string | null;
	theoryName: string | null;
	symbolDeclaration: string | null;
	symbolDeclarationRange: Range | null;
	symbolDeclarationFile: string | null;
	symbolDoc: string | null;
	comment?: string | null;
	error?: ErrorType | null;
};
export declare interface PvsListDeclarationsResponseType extends PvsResponseType {
	error: ErrorType,
	res: PvsDeclarationDescriptor[],
	raw: string
}

export declare interface PvsFindDeclarationRequest {
	symbolName: string;
	file?: string;
	theory?: string;
	line?: number;
	character?: number;
}
export declare interface PvsDefinition {
	file?: string;
	theory?: string;
	line?: number;
	character?: number;
	symbolName: string | null;
	symbolTheory: string | null;
	symbolDeclaration: string | null;
	symbolDeclarationRange: Range | null; // range
	symbolDeclarationFile: string | null; // uri
	symbolDoc?: string;
	comment?: string;
	error?: ErrorType;
};
export declare interface PvsListDeclarationsRequest {
	theoryName: string;
	file?: string;
	line?: number;
	character?: number;
}

// export declare interface PvsListDeclarationsResponse {
// 	file?: string;
// 	line?: number;
// 	character?: number;
// 	symbolName: string | null;
// 	symbolTheory: string | null;
// 	symbolDeclaration: string | null;
// 	symbolDeclarationRange: Range | null;
// 	symbolDeclarationFile: string | null;
// 	symbolDoc?: string;
// 	comment?: string;
// 	error?: ErrorType;
// }[];

export declare interface HelpDescriptor {
	// name: string,
	description?: string,
	syntax: string,
	note?: string,
	optionals?: { [key:string]: string }
};

// export declare interface PvsErrorType {
// 	msg: string,
// 	parserError?: ErrorType,
// 	restartOption?: number | string
// };
export declare interface PvsResponseType {
	error: ErrorType,
	res: any, // json object -- TODO: define the different types of objects, include a field "type" in each object
	raw: string // raw output of pvs-lisp
};
export declare interface ChangeContextResponseType {
	context: string
}
export declare interface PvsChangeContextResponseType extends PvsResponseType {
	error: ErrorType,
	res: ChangeContextResponseType,
	raw: string
}
export declare interface PvsCurrentContextResponseType extends PvsResponseType {
	error: ErrorType,
	res: string,
	raw: string
}
export declare interface VersionInfoResponseType {
	pvsVersion: string,
	lispVersion: string
}
export declare interface PvsVersionInfo extends PvsResponseType {
	error: ErrorType,
	res: VersionInfoResponseType,
	raw: string
}
export declare interface JsonType {
	[ key: string ]: JsonType | string; 
}
export declare type ProofNodeType = "root" | "proof-branch" | "proof-command";
export declare interface ProofNode { 
	name: string; // name of this node (proof name, branch name, or proof command)
	rules: ProofNode[]; // sequence of proof rules
	type: ProofNodeType; // node type
	branch: string; // branch id
}
export declare interface ProofNodeX extends ProofNode { 
	id: string; // unique node ID
	name: string; // name of this node (proof name, branch name, or proof command)
	rules: ProofNodeX[]; // sequence of proof rules
	type: ProofNodeType; // node type
	branch: string; // branch id
	parent: string;
}
export declare type ProofNodeStatus = "visited" | "not-visited" | "active" | "pending" | "complete" | "not-complete";
export declare type ProofTree = ProofNode;
export declare type ProofStatus = "subsumed" | "simplified" | "proved" | "unproved" | "unfinished" | "unchecked" | "untried";
export declare interface ProofFile {
	[key: string]: [ProofDescriptor] // key is theoryName.formulaName
}
export class ProofDescriptor {
	origin: ProofOrigin; // proof file that generated the proof (.jprf, .prf, .prl)
	info: {
		theory: string, // theory name
		formula: string, // formula name
		status: ProofStatus, // proof status (proved, untried, unfininshed,...)
		prover: string, // prover version
		shasum: string, // digest, obtained from the file content after removing all spaces
		date?: string // day and time the proof was saved, ISO format, e.g., 2011-10-10T14:48:00
	};
	proofTree?: ProofNode;
	clips?: ProofNode[];
	constructor (info: {
		theory: string, // theory name
		formula: string, // formula name
		status: ProofStatus, // proof status (proved, untried, unfininshed,...)
		prover: string, // prover version
		shasum: string, // digest, obtained from the file content after removing all spaces
		date?: string // day and time the proof was saved, ISO format, e.g., 2011-10-10T14:48:00
	}, origin: ProofOrigin, proofTree?: ProofNode) {
		this.info = info;
		this.origin = origin;
		this.proofTree = proofTree;
	}
	isEmpty (): boolean {
		return !this.proofTree || !this.proofTree.rules || this.proofTree.rules.length === 0;
	}
}
export declare interface PvsListProofStrategies extends PvsResponseType {
	error: ErrorType,
	res: HelpDescriptor[],
	raw: string
}

export declare interface PvsFindDeclaration extends PvsResponseType {
	error: ErrorType,
	res: PvsDeclarationType[],
	raw: string
}


export const PRELUDE_FILE: string = "*prelude*";
export const PVS_LIBRARY_FILES: { [ key: string ] : string } = {
	"*prelude*": "prelude.pvs",
	"character_adt": "character_adt.pvs",
	"lift_adt": "lift_adt.pvs",
	"list_adt": "list_adt.pvs",
	"ordstruct_adt": "ordstruct_adt.pvs",
	"union_adt": "union_adt.pvs"
};

export declare interface ParserResponse {
	msgs: string[];
}
export declare interface PvsParserResponse extends PvsResponseType {
	error: ErrorType,
	res: ParserResponse,
	raw: string
}

export declare interface PvsSymbolKind<type> {
	keywords: type,
	numbers: type,
	strings: type,
	constants: type,
	builtinTypes: type,
	operators: type, // language operators
	functions: type, // library functions
	comments: type
};

export declare interface ExpressionDescriptor {
	fileName: string,
	theoryName: string,
	expression: string
};

export declare interface EvaluationResult {
	theoryName: string,
	fileName: string,
	msg: string,
	result: string
};

export declare interface ProofResult {
	theoryName: string,
	fileName: string,
	msg: string,
	result: string
};

export declare interface PrettyPrintRegionRequest {
	theoryName: string;
	range: Range
}

export declare interface PrettyPrintRegionResult {
	theoryName: string;
	range: Range;
	result: string;
}

export declare interface PvsCliInterface {
	type: string;
	pvsPath: string;
	contextFolder: string;
	gateway: { port: number };
	// pvsProxy: PvsProxy,
	// cmd: string,
	fileName: string;
	fileExtension: string;
	channelID: string;
	theoryName?: string;
	formulaName?: string;
	line?: number;
	prompt?: string;
}

export declare interface SimpleConsole {
	log: (str: string) => void,
	error: (str: string) => void,
	info: (str: string) => void,
	warn: (str: string) => void
}

export declare interface SimpleConnection {
    console: SimpleConsole,
	sendNotification: (type: string, msg?: string) => void;
	sendRequest: (type: string, data: any) => void;
}


//TODO: move this declaration to fsUtils
export declare interface FileList {
	contextFolder: string;
	fileNames: string[]; // TODO: FileDescriptor[]
}

export declare interface DeclarationMap {
	[ theoryName: string ]: {
		[key: string]: {
			theoryName: string;
			symbolName: string;
			symbolDeclaration: string;
			symbolDeclarationRange: Range;
			symbolDeclarationFile: string;
			symbolDoc?: string;
			comment?: string;
		}
	}
}


export const cliSessionType = {
	pvsioEvaluator: "pvs.pvsio-evaluator",
	proveFormula: "pvs.prove-formula"
};

export declare interface ContextFolder {
	contextFolder: string;
}
export declare interface PvsContextDescriptor extends ContextFolder {
	contextFolder: string,
	fileDescriptors: { [fname: string]: PvsFileDescriptor }
}
export declare interface FileDescriptor extends ContextFolder {
	fileName: string;
	fileExtension: string;
	fileContent?: string;
}
export declare type PvsFile = FileDescriptor;
export declare interface PvsTheory extends PvsFile {
	theoryName: string;
}
export declare interface PvsFormula extends PvsTheory {
	formulaName: string;
}
export declare interface PvsProofCommand extends PvsFormula {
	cmd: string;
}
export declare interface PvsioEvaluatorCommand extends PvsTheory {
	cmd?: string,
	
	mode?: PvsIoMode,
	initialState?: string,
	sendResponse?: boolean,
	showCommandInTerminal?: boolean
}
export declare interface EvalExpressionRequest extends PvsTheory {
	expr: string;
}
export declare interface FormulaDescriptor extends PvsFormula {
	position: Position;
	status: ProofStatus; // proof status
	isTcc: boolean;
}
export declare interface TheoryDescriptor extends PvsTheory {
	position: Position; // position of the theory declaration
	theorems?: FormulaDescriptor[];
}
export declare interface PvsFileDescriptor extends PvsFile {
	theories: TheoryDescriptor[];
}
export declare interface SearchRequest {
	searchString: string
}
export interface SearchResult extends PvsFile {
	line: number
};
export declare interface SearchResponse {
	req: SearchRequest,
	ans: SearchResult[]
}

export const serverRequest = {
	typecheckFile: "pvs.typecheck-file",
	proveFormula: "pvs.prove-formula",
	autorunFormula: "pvs.autorun-formula",
	autorunFormulaFromJprf: "pvs.autorun-formula-from-jprf",
	showProofLite: "pvs.show-prooflite",
	proofCommand: "pvs.proof-command",
	evalExpression: "pvs.eval-expression",
	evaluatorCommand: "pvs.evaluator-command",
	parseFile: "pvs.parse-file",
	parseFileWithFeedback: "pvs.parse-file.with-feedback",
	parseWorkspace: "pvs.parse-workspace",
	parseWorkspaceWithFeedback: "pvs.parse-workspace.with-feedback",
	typecheckWorkspace: "pvs.typecheck-workspace",
	typecheckWorkspaceWithFeedback: "pvs.typecheck-workspace.with-feedback",
	listContext: "pvs.list-context",
	generateTccs: "pvs.generate-tccs",
	showTccs: "pvs.show-tccs",
	generateTheorySummary: "pvs.generate-theory-summary",
	showTheorySummary: "pvs.show-theory-summary",
	generateWorkspaceSummary: "pvs.generate-workspace-summary",
	showWorkspaceSummary: "pvs.show-workspace-summary",
	startPvsServer: "pvs.start-pvs-server",
	stopPvsServer: "pvs.stop-pvs-server",
	rebootPvsServer: "pvs.reboot-pvs-server",
	hp2pvs: "pvs.hp-to-pvs-file",
	startEvaluator: "pvs.start-evaluator",
	quitProof: "pvs.quit-proof",
	quitEvaluator: "pvs.quit-evaluator",
	clearTheories: "pvs.clear-theories",

	viewPreludeFile: "pvs.view-prelude-file",

	getContextDescriptor: "pvs.get-context-descriptor",
	getFileDescriptor: "pvs.get-file-descriptor",
	getGatewayConfig: "pvs.get-cli-descriptor",
	getImportChainTheorems: "pvs.get-importchain-theorems",
	getTheorems: "pvs.get-theorems",
	getTccs: "pvs.get-tccs",

	cancelOperation: "pvs.cancel-operation",

	proverCommand: "pvs.prover-command",

	search: "pvs.search",

	listDownloadableVersions: "pvs.list-downloadable-versions",
	downloadPvs: "pvs.download-pvs",
	downloadLicensePage: "pvs.download-license-page",

	getNasalibDownloader: "pvs.get-nasalib-downloader",
	downloadNasalib: "pvs.download-nasalib",
};

export const serverEvent = {
	typecheckFileResponse: "pvs.response.typecheck-file",
	typecheckWorkspaceResponse: "pvs.response.typecheck-workspace",
	proveFormulaResponse: "pvs.response.prove-formula",
	autorunFormulaResponse: "pvs.response.autorun-formula",
	loadProofResponse: "pvs.response.load-proof",
	saveProofResponse: "pvs.response.save-proof",
	showProofLiteResponse: "pvs.response.show-prooflite",
	proofCommandResponse: "pvs.response.proof-command",
	evalExpressionResponse: "pvs.response.eval-expression",
	evaluatorCommandResponse: "pvs.response.evaluator-command",
	parseFileResponse: "pvs.response.parse-file",
	listContextResponse: "pvs.response.list-context",
	generateTccsResponse: "pvs.response.generate-tccs",
	showTccsResponse: "pvs.response.show-tccs",
	showTheorySummaryResponse: "pvs.response.show-theory-summary",
	showWorkspaceSummaryResponse: "pvs.response.show-workspace-summary",
	startEvaluatorResponse: "pvs.response.start-evaluator",
	hp2pvsResponse: "pvs.response.hp-to-pvs-file",
	quitEvaluatorResponse: "pvs.response.quit-evaluator",
	quitProofResponse: "pvs.response.quit-proof",
	searchResponse: "pvs.response.search",

	viewPreludeFileResponse: "pvs.response.view-prelude-file",

	getContextDescriptorResponse: "pvs.response.get-context-descriptor",
	getFileDescriptorResponse: "pvs.response.get-file-descriptor",
	getGatewayConfigResponse: "pvs.response.get-cli-descriptor",
	getImportChainTheoremsResponse: "pvs.response.get-importchain-theorems",
	getTheoremsResponse: "pvs.response.get-theorems",
	getTccsResponse: "pvs.response.get-tccs",

	listDownloadableVersionsResponse: "pvs.response.list-downloadable-versions",
	downloadPvsResponse: "pvs.response.download-pvs",
	downloadLicensePageResponse: "pvs.response.download-license-page",

	getNasalibDownloaderResponse: "pvs.response.get-nasalib-downloader",
	setNasalibPathResponse: "pvs.response.set-nasalib-path",

	pvsServerReady: "pvs.response.pvs-server-ready",

	workspaceEvent: "pvs.workspace-event",
	contextUpdate: "pvs.event.context-update",
	// proofStateUpdate: "pvs.event.proof-state",
	QED: "pvs.event.qed",
	evaluatorStateUpdate: "pvs.event.evaluator-state",
	workspaceStats: "pvs.event.workspace-stats",
	// saveProofEvent: "pvs.event.save-proof",
	quitProofDontSaveEvent: "pvs.event.quit-dont-save-proof",
	saveProofForceQuitEvent: "pvs.event.save-then-quit",
	closeDontSaveEvent: "pvs.event.close-dont-save-proof",
	serverModeUpdateEvent: "pvs.event.server-mode-update",

	querySaveProof: "pvs.query.save-proof?",
	querySaveProofResponse: "pvs.query.response.save-proof",

	// proverForwardResponse: "pvs.response.prover-forward",
	proofNodeUpdate: "pvs.event.proof-node-update",
	// proofEditEvent: "pvs.event.proof-edit-event",
	// proofExecEvent: "pvs.event.proof-exec-event",
	proverEvent: "pvs.prover-event",
	clipboardEvent: "pvs.event.clipboard-event",

	// loadProofStructureEvent: "pvs.event.load-proof-structure",
	// startProofEvent: "pvs.event.start-proof",

	pvsServerCrash: "pvs.event.server-crash",

	pvsVersionInfo: "pvs.event.version-info",
	pvsNotPresent: "pvs.event.pvs-not-present",
	pvsIncorrectVersion: "pvs.event.pvs-incorrect-version",

	profilerData: "pvs.event.profiler-data"
};

export declare type ServerMode = "lisp" | "in-checker" | "pvsio";

export declare type WorkspaceEvent = ServerDidRenameFile;
export declare type ServerDidRenameFile = {
	action: "did-rename-file",
	old_fname: string,
	new_fname: string
};

// CliGateway
export declare type CliGatewayRequest = CliGatewaySubscribeServerRequest | CliGatewaySubscribeClientRequest
	| CliGatewayUnsubscribeRequest | CliGatewayExecProofCommandRequest | CliGatewayEvaluatorCommandRequest
	| CliGatewaySaveProofRequest | CliGatewayPublishRequest | CliGatewaySelectProfileRequest
export declare type CliGatewaySubscribeServerRequest = { 
	type: "subscribe", clientID: string, channelID: string 
};
export declare type CliGatewaySubscribeClientRequest = { 
	type: "subscribe-vscode", clientID: string, channelID: string 
};
export declare type CliGatewayUnsubscribeRequest = {
	type: "unsubscribe", clientID: string, channelID: string
};
export declare type CliGatewayExecProofCommandRequest = { 
	type: "pvs.proof-command", fileName: string, fileExtension: string, contextFolder: string, 
	theoryName: string, formulaName: string, cmd: string 
};
export declare type CliGatewayEvaluatorCommandRequest = { 
	type: "pvs.evaluator-command", fileName: string, fileExtension: string, contextFolder: string, 
	theoryName: string, cmd: string 
};
export declare type CliGatewaySaveProofRequest = {
	type: "pvs.save-proof", fileName: string, fileExtension: string, contextFolder: string, 
	theoryName: string, formulaName: string
};
export declare type CliGatewayPublishRequest = {
	type: "publish", channelID: string
};
export declare type CliGatewaySelectProfileRequest = {
	type: "pvs.select-profile", profile: ProofMateProfile
};


export declare type CliGatewaySubscriberEvent = CliGatewayProofStateInfo | CliGatewayEvaluatorStateInfo 
	| CliGatewayMathObjectsInfo | CliGatewayPrintProofCommandInfo | CliGatewaySubscribeEvent
	| CliGatewaySelectProfileEvent | CliGatewayQEDEvent | CliGatewayQuitEvent | CliGatewayEvaluatorReady
	| CliGatewayProverReady;
export declare type CliGatewayProofStateInfo = {  
	type: "pvs.event.proof-state",
	data: SequentDescriptor,
	cmd?: string // the command that produced this state
};
export declare type CliGatewayEvaluatorStateInfo = {  
	type: "pvs.event.evaluator-state",
	data: PvsResponse,
	cmd?: string // the command that produced this state
};
export declare type CliGatewayEvaluatorReady = {  
	type: "pvs.event.evaluator-ready",
	banner?: string
};
export declare type CliGatewayProverReady = {  
	type: "pvs.event.prover-ready"
};
export declare type CliGatewayMathObjectsInfo = {
	type: "gateway.publish.math-objects",
	data: { lemmas: string[], types: string[], definitions: string[] }
};
export declare type CliGatewayPrintProofCommandInfo = {  
	type: "pvs.event.print-proof-command",
	data: { cmd: string }
};
export declare type CliGatewayQEDEvent = {  
	type: "pvs.event.QED"
};
export declare type CliGatewayQuitEvent = {  
	type: "pvs.event.quit"
};
export declare type CliGatewaySubscribeEvent = {
	type: "subscribe-response",
	success: boolean
};
export declare type CliGatewaySelectProfileEvent = {
	type: "pvs.select-profile",
	data: { profile: string }
};


export declare type CliGatewayEvent = CliGatewayProofState | CliGatewayEvaluatorState 
	| CliGatewayMathObjects | CliGatewayPrintProofCommand | CliGatewayQED | CliGatewayQuit
	| CliGatewayEvaluatorReadyEvent | CliGatewayProverReadyEvent;
export declare interface CliGatewayEvaluatorReadyEvent extends CliGatewayEvaluatorReady {  
	channelID: string
};
export declare interface CliGatewayProverReadyEvent extends CliGatewayProverReady {  
	channelID: string
};
export declare interface CliGatewayProofState extends CliGatewayProofStateInfo {  
	channelID: string
};
export declare interface CliGatewayPrintProofCommand extends CliGatewayPrintProofCommandInfo {
	channelID: string
};
export declare interface CliGatewayEvaluatorState extends CliGatewayEvaluatorStateInfo {
	channelID: string
};
export declare interface CliGatewayMathObjects extends CliGatewayMathObjectsInfo {
	channelID: string
};
export declare interface CliGatewayQED extends CliGatewayQEDEvent {
	channelID: string
};
export declare interface CliGatewayQuit extends CliGatewayQuitEvent {
	channelID: string
};
//  | { 
// 	type: "publish",
// 	channelID: string
// };



export interface PvsDownloadDescriptor { url: string, fileName: string, version: string };
export interface NasalibDownloadDescriptor { pvsPath: string };

// useful constants
export const sriUrl: string = "www.csl.sri.com";
export const pvsUrl: string = "pvs.csl.sri.com";
// export const pvsSnapshotsUrl: string = `http://${sriUrl}/users/owre/drop/pvs-snapshots/`;
export const pvsDownloadUrl: string = `https://${pvsUrl}/downloads/`;
export const allegroLicenseUrl: string = `http://${pvsUrl}/cgi-bin/downloadlic.cgi?file=pvs-6.0-ix86_64-Linux-allegro.tgz`; //`https://pvs.csl.sri.com/download.shtml`;
export const nasalibUrl: string = "https://github.com/nasa/pvslib";
export const nasalibBranch: string = "master";
export const nasalibFile: string = `https://github.com/nasa/pvslib/archive/${nasalibBranch}.zip`;

// ProofEdit
export type ProofEditCommand = ProofEditAppendNode | ProofEditCopyNode | ProofEditPasteNode | ProofEditCopyTree
	| ProofEditPasteTree | ProofEditDeleteNode | ProofEditAppendBranch | ProofEditCutNode | ProofEditCutTree
	| ProofEditDeleteTree | ProofEditTrimNode | ProofEditRenameNode | ProofEditTrimUnused
	| ProofEditExportProof;
export type ProofEditAppendNode = {
	action: "append-node",
	name: string,
	selected: { id: string, name: string }
}
export type ProofEditCopyNode = {
	action: "copy-node",
	selected: { id: string, name: string }
};
export type ProofEditPasteNode = {
	action: "paste-node",
	selected: { id: string, name: string }
};
export type ProofEditCopyTree = {
	action: "copy-tree",
	selected: { id: string, name: string }
};
export type ProofEditPasteTree = {
	action: "paste-tree",
	selected: { id: string, name: string }
};
export type ProofEditDeleteNode = {
	action: "delete-node",
	selected: { id: string, name: string }
};
export type ProofEditAppendBranch = {
	action: "append-branch",
	selected: { id: string, name: string }
};
export type ProofEditCutNode = {
	action: "cut-node",
	selected: { id: string, name: string }
};
export type ProofEditCutTree = {
	action: "cut-tree",
	selected: { id: string, name: string }
};
export type ProofEditDeleteTree = {
	action: "delete-tree",
	selected: { id: string, name: string }
};
export type ProofEditTrimNode = {
	action: "trim-node",
	selected: { id: string, name: string }
};
export type ProofEditTrimUnused = {
	action: "trim-unused",
	selected: { id: string, name: string }
};
export type ProofEditRenameNode = {
	action: "rename-node",
	newName: string,
	selected: { id: string, name: string }
};
export type ProofEditExportProof = {
	action: "export-proof",
	proofFile: {
		// fileName?: string,
		fileExtension: string
		// contextFolder?: string
	}
};


export type ProofEditEvent = ProofEditDidAppendNode | ProofEditDidCopyNode | ProofEditDidPasteNode | ProofEditDidCopyTree
	| ProofEditDidPasteTree | ProofEditDidDeleteNode | ProofEditDidAppendBranch | ProofEditDidCutNode | ProofEditDidCutTree
	| ProofEditDidDeleteTree | ProofEditDidTrimNode | ProofEditDidRenameNode | ProofEditDidActivateCursor 
	| ProofEditDidDeactivateCursor | ProofEditDidUpdateProofStatus | ProofEditDidStartNewProof;
export type ProofEditDidAppendNode = {
	action: "did-append-node",
	elem: ProofNodeX,
	position: number
}
export type ProofEditDidCopyNode = {
	action: "did-copy-node",
	selected: { id: string, name: string }
};
export type ProofEditDidPasteNode = {
	action: "did-paste-node",
	elem: ProofNodeX
};
export type ProofEditDidCopyTree = {
	action: "did-copy-tree",
	selected: { id: string, name: string },
	elems: ProofNodeX[],
	clipboard: string
};
export type ProofEditDidPasteTree = {
	action: "did-paste-tree",
	selected: { id: string, name: string }
};
export type ProofEditDidDeleteNode = {
	action: "did-delete-node",
	selected: { id: string, name: string }
};
export type ProofEditDidAppendBranch = {
	action: "did-append-branch",
	elem: ProofNodeX
};
export type ProofEditDidCutNode = {
	action: "did-cut-node",
	selected: { id: string, name: string }
};
export type ProofEditDidCutTree = {
	action: "did-cut-tree",
	selected: { id: string, name: string },
	clipboard: string,
	elems: ProofNodeX[]
};
export type ProofEditDidDeleteTree = {
	action: "did-delete-tree",
	selected: { id: string, name: string }
};
export type ProofEditDidTrimNode = {
	action: "did-trim-node",
	elems: ProofNodeX[]
};
export type ProofEditDidRenameNode = {
	action: "did-rename-node",
	selected: { id: string, name: string },
	newName: string
};
export type ProofEditDidActivateCursor = {
	action: "did-activate-cursor",
	cursor: ProofNodeX
};
export type ProofEditDidDeactivateCursor = {
	action: "did-deactivate-cursor"
};
export type ProofEditDidUpdateProofStatus = {
	action: "did-update-proof-status",
	proofStatus: ProofStatus
};
export type ProofEditDidStartNewProof = {
	action: "did-start-new-proof"
};

// ProofExec
export type ProofExecCommand = ProofExecForward | ProofExecBack | ProofExecFastForward | ProofExecRun
	| ProofExecQuit | ProofExecOpenProof | ProofExecStartNewProof | ProofExecInterruptProver | ProofExecQuitAndSave
	| ProofExecInterruptAndQuitProver | ProofExecImportProof | ProofExecRewind;
export type ProofExecForward = {
	action: "forward"
};
export type ProofExecBack = {
	action: "back"
};
export type ProofExecFastForward = {
	action: "fast-forward",
	selected: { id: string, name: string }
};
export type ProofExecRewind = {
	action: "rewind",
	selected: { id: string, name: string }
};
export type ProofExecRun = {
	action: "run"
};
export type ProofExecQuit = {
	action: "quit-proof"
};
export type ProofExecQuitAndSave = {
	action: "quit-proof-and-save"
};
export type ProofExecOpenProof = {
	action: "open-proof",
	proofFile: {
		fileName: string,
		fileExtension: string,
		contextFolder: string
	},
	formula: PvsFormula,
	opt?: {
		restartProof?: boolean
	}
};
export type ProofExecImportProof = {
	action: "import-proof",
	proofFile: {
		fileName: string,
		fileExtension: string,
		contextFolder: string
	},
	formula: PvsFormula
};
export type ProofExecStartNewProof = {
	action: "start-new-proof",
	formula: PvsFormula
};
export type ProofExecInterruptProver = {
	action: "interrupt-prover"
};
export type ProofExecInterruptAndQuitProver = {
	action: "interrupt-and-quit-prover"
};

export type ProofExecEvent = ProofExecDidStartProof | ProofExecDidLoadProof | ProofExecDidLoadSequent
	| ProofExecDidQuitProof | ProofExecDidUpdateSequent | ProofExecDidOpenProof | ProofExecDidImportProof
	| ProofExecDidFailToStartProof | ProofExecDidStopRunning;
export type ProofExecDidStartProof = {
	action: "did-start-proof"
};
export type ProofExecDidFailToStartProof = {
	action: "did-fail-to-start-proof"
};
export type ProofExecDidLoadSequent = {
	action: "did-load-sequent",
	sequent: SequentDescriptor
};
export type ProofExecDidUpdateSequent = {
	action: "did-update-sequent",
	selected: { id: string, name: string },
	sequent: SequentDescriptor
};
export type ProofExecDidStopRunning = {
	action: "did-stop-running"
};
export type ProofExecDidLoadProof = {
	action: "did-load-proof",
	formula: PvsFormula,
	desc: ProofDescriptor,
	proof: ProofNodeX
};
export type ProofExecDidQuitProof = {
	action: "did-quit-proof"
};
export type ProofExecDidOpenProof = {
	action: "did-open-proof",
	proofFile: PvsFile,
	formula: PvsFormula,
	desc: ProofDescriptor,
	proof?: ProofNodeX
};
export type ProofExecDidImportProof = {
	action: "did-import-proof",
	proofFile: PvsFile,
	formula: PvsFormula,
	importedFormula: PvsFormula,
	desc: ProofDescriptor,
	proof?: ProofNodeX
};

// WorkspaceExec
// export type WorkspaceExecCommand = TypecheckFileCommand | ParseFileCommand;
