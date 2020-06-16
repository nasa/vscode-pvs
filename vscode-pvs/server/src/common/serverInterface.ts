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

 // import { PvsProxy } from "../pvsProxy";

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
	"lisp-version": string
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

export declare interface ProofCommandDescriptor {
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
	name: string, // name of this node (proof name, branch name, or proof command)
	rules: ProofNode[], // sequence of proof rules
	type: ProofNodeType, // node type
	branch: string // branch id
}
export declare type ProofTree = ProofNode;
export declare type ProofStatus = "subsumed" | "simplified" | "proved" | "unproved" | "unfinished" | "unchecked" | "untried";
export declare interface ProofFile {
	[key: string]: [ProofDescriptor] // key is theoryName.formulaName
}
export declare interface ProofDescriptor {
	info: {
		theory: string, // theory name
		formula: string, // formula name
		status: ProofStatus, // proof status (proved, untried, unfininshed,...)
		prover: string, // prover version
		shasum: string // digest, obtained from the file content after removing all spaces
	},
	proofTree?: ProofNode
}
export declare interface PvsListProofStrategies extends PvsResponseType {
	error: ErrorType,
	res: ProofCommandDescriptor[],
	raw: string
}
// export interface FindDeclarationResponseType {
//     [ key: string ] : PvsDeclarationType; // key is theoryName.symbolName
// }
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
// export declare interface TypecheckerResponse {
// 	[ theoryName: string ]: TheoryStatus
// }
// export declare interface PvsTypecheckerResponse extends PvsResponseType {
// 	error: ErrorType;
// 	res: TypecheckerResponse;
// 	raw: string;
// }

// export declare interface FormulaMap {
// 	[ formulaName: string ]: FormulaDescriptor;
// }

// export declare interface TheoremsStatus {
// 	theoryName: string;
// 	theorems: FormulaDescriptor[];
// }

// export declare interface TheoryStatusMap {
// 	[ theoryName: string ]: TheoremsStatus
// }





// export declare interface TccDescriptorArray {
// 	theoryName: string; // theory name
// 	fileName: string; // pvs file containing the theory
// 	tccs: TccDescriptor[]; // structured view of the list of tccs generated for the theory
// }

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

// export declare interface PvsTheoryListDescriptor {
// 	folder: string, // base path
// 	files: { [ fileName: string ] : string[] } // theories grouped by fileName
// 	theories: { [ theoryName: string ]: string[] } // files grouped by theoryName
// }




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

// export declare interface PvsExecutionContext {
// 	pvsPath: string,
// 	contextFolder: string
// }

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


// export declare interface TheoryMap {
// 	[ theoryName: string ]: {
// 		theoryName: string,
// 		fileName: string,
// 		fileExtension: string,
// 		contextFolder: string,
// 		position: Position
// 	}
// }

//TODO: move this declaration to fsUtils
export declare interface FileList {
	contextFolder: string;
	fileNames: string[]; // TODO: FileDescriptor[]
}

// export declare interface TheoryList {
// 	contextFolder: string;
// 	theories: TheoryDescriptor[]; //  TODO TheoryDescriptor[]
// }

// export declare interface TheoremList {
// 	contextFolder: string;
// 	theorems: TheoremDescriptor[];
// 	fileName?: string; // when fileName is specified, the theorem list describes the content of a specific file. This is useful for status updates.
// }

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

// export interface TheoryDescriptor {
// 	theoryName: string,
// 	fileName: string,
// 	position: Position
// }

export declare interface FormulaDescriptor {
	fileName: string;
	fileExtension: string;
	contextFolder: string;
	theoryName: string;
	formulaName: string;
	position: Position;
	status: ProofStatus; // proof status
	isTcc: boolean;
}

export declare interface TheoryDescriptor {
	fileName: string;
	fileExtension: string;
	contextFolder: string;
	theoryName: string;
	position: Position; // position of the theory declaration
	theorems?: FormulaDescriptor[];
}
// export declare interface TheoriesStatusMap {
// 	[ theoryName: string ]: TheoryStatus;
// }
// export declare interface TheoriesMap {
// 	contextFolder: string;
// 	theoriesStatusMap: TheoriesStatusMap;
// }

// export declare interface ContextDescriptor {
// 	contextFolder: string;
// 	theories: TheoryDescriptor[];
// };

export declare interface PvsContextDescriptor {
	contextFolder: string,
	fileDescriptors: { [fname: string]: PvsFileDescriptor }
}

// export declare interface PvsFileListDescriptor {
// 	folder: string, // base path
// 	fileNames: string[] // pvs files
// }


export declare interface PvsFileDescriptor {
	fileName: string;
	fileExtension: string;
	contextFolder: string;
	theories: TheoryDescriptor[];
}

export const cliSessionType = {
	pvsioEvaluator: "pvs.pvsio-evaluator",
	proveFormula: "pvs.prove-formula"
};

export const serverCommand = {
	typecheckFile: "pvs.typecheck-file",
	proveFormula: "pvs.prove-formula",
	dischargeTccs: "pvs.prove-tccs",
	dischargeTheorems: "pvs.prove-file",
	loadProof: "pvs.load-proof",
	saveProof: "pvs.save-proof",
	showProofLite: "pvs.show-prooflite",
	proofCommand: "pvs.proof-command",
	evaluateExpression: "pvs.evaluate-expression",
	parseFile: "pvs.parse-file",
	parseFileWithFeedback: "pvs.parse-file.with-feedback",
	parseWorkspace: "pvs.parse-workspace",
	parseWorkspaceWithFeedback: "pvs.parse-workspace.with-feedback",
	typecheckWorkspace: "pvs.typecheck-workspace",
	listContext: "pvs.list-context",
	generateTccs: "pvs.generate-tccs",
	showTccs: "pvs.show-tccs",
	startPvsServer: "pvs.start-pvs-server",
	rebootPvsServer: "pvs.reboot-pvs-server",
	hp2pvs: "pvs.hp-to-pvs-file",
	startEvaluator: "pvs.start-evaluator",
	quitProver: "pvs.quit-prover",

	viewPreludeFile: "pvs.view-prelude-file",

	getContextDescriptor: "pvs.get-context-descriptor",
	getFileDescriptor: "pvs.get-file-descriptor",
	getGatewayConfig: "pvs.get-cli-descriptor",

	cancelOperation: "pvs.cancel-operation",

	listDownloadableVersions: "pvs.list-downloadable-versions",
	downloadPvs: "pvs.download-pvs",
	downloadLicensePage: "pvs.download-license-page"
};
// TODO: add here type information for args
export const serverEvent = {
	typecheckFileResponse: "pvs.response.typecheck-file",
	proveFormulaResponse: "pvs.response.prove-formula",
	dischargeTccsResponse: "pvs.reponse.prove-tccs",
	dischargeTheoremsResponse: "pvs.reponse.prove-file",
	loadProofResponse: "pvs.response.load-proof",
	saveProofResponse: "pvs.response.save-proof",
	showProofLiteResponse: "pvs.response.show-prooflite",
	proofCommandResponse: "pvs.response.proof-command",
	parseFileResponse: "pvs.response.parse-file",
	listContextResponse: "pvs.response.list-context",
	generateTccsResponse: "pvs.response.generate-tccs",
	showTccsResponse: "pvs.response.show-tccs",
	startEvaluatorResponse: "pvs.response.start-evaluator",
	hp2pvsResponse: "pvs.response.hp-to-pvs-file",

	viewPreludeFileResponse: "pvs.response.view-prelude-file",

	getContextDescriptorResponse: "pvs.response.get-context-descriptor",
	getFileDescriptorResponse: "pvs.response.get-file-descriptor",
	getGatewayConfigResponse: "pvs.response.get-cli-descriptor",

	listDownloadableVersionsResponse: "pvs.response.list-downloadable-versions",
	downloadPvsResponse: "pvs.response.download-pvs",
	downloadLicensePageResponse: "pvs.response.download-license-page",

	pvsServerReady: "pvs.response.restart",

	contextUpdate: "pvs.event.context-update",
	proofStateUpdate: "pvs.event.proof-state",
	QED: "pvs.event.qed",
	evaluatorStateUpdate: "pvs.event.evaluator-state",
	workspaceStats: "pvs.event.workspace-stats",
	saveProofEvent: "pvs.event.save-proof",
	quitProofEvent: "pvs.event.quit-proof",
	quitDontSaveProofEvent: "pvs.event.quit-dont-save-proof",
	closeDontSaveEvent: "pvs.event.close-dont-save-proof",

	pvsServerCrash: "pvs.event.server-crash",

	pvsVersionInfo: "pvs.event.version-info",
	pvsNotPresent: "pvs.event.pvs-not-present",
	pvsIncorrectVersion: "pvs.event.pvs-incorrect-version"
};



export declare type CliGatewayRequest = { 
	type: "subscribe", clientID: string, channelID: string 
} | { 
	type: "subscribe-vscode", clientID: string, channelID: string 
} | {
	type: "unsubscribe", clientID: string, channelID: string
} | { 
	type: "pvs.proof-command", fileName: string, fileExtension: string, contextFolder: string, 
	theoryName: string, formulaName: string, cmd: string 
} | { 
	type: "pvs.evaluate-expression", fileName: string, fileExtension: string, contextFolder: string, 
	theoryName: string, cmd: string 
} | {
	type: "pvs.save-proof", fileName: string, fileExtension: string, contextFolder: string, 
	theoryName: string, formulaName: string
} | {
	type: "publish", channelID: string
} | {
	type: "pvs.select-profile", profile: ProofMateProfile
};

export declare type CliGatewayEvent = {  
	type: "pvs.event.proof-state" | "pvs.event.evaluator-state",
	channelID: string,
	data: PvsResponse,
	cmd?: string // the command that produced this state
} | {
	type: "gateway.publish.math-objects",
	channelID: string,
	data: { lemmas: string[], types: string[], definitions: string[] }
};
//  | { 
// 	type: "publish",
// 	channelID: string
// };

export declare type CliGatewaySubscriberEvent = {  
	type: "pvs.event.proof-state" | "pvs.event.evaluator-state",
	data: PvsResponse,
	cmd?: string // the command that produced this state
} | {
	type: "gateway.publish.math-objects",
	data: { lemmas: string[], types: string[], definitions: string[] }
} | {
	type: "subscribe-response",
	success: boolean
} | {
	type: "pvs.select-profile",
	data: { profile: string }
};

export interface PvsDownloadDescriptor { url: string, fileName: string, version: string };

export const sriUrl: string = "www.csl.sri.com";
export const pvsSnapshotsUrl: string = `http://${sriUrl}/users/owre/drop/pvs-snapshots/`;
export const allegroLicenseUrl: string = `http://pvs.csl.sri.com/cgi-bin/downloadlic.cgi?file=pvs-6.0-ix86_64-Linux-allegro.tgz`; //`https://pvs.csl.sri.com/download.shtml`;
