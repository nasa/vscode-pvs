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
	pvsVersion: string,
	lispVersion: string
};

export declare interface ErrorType {
	msg: string,
	fileName: string,
	line: number,
	character: number
};

export declare interface PvsDeclarationType {
	symbolName: string | null;
	symbolTheory: string | null;
	symbolDeclaration: string | null;
	symbolDeclarationRange: Range | null;
	symbolDeclarationFile: string | null;
};

export declare interface PvsDeclarationDescriptor {
	file?: string;
	theory?: string;
	line?: number;
	character?: number;
	symbolName: string | null;
	symbolTheory: string | null;
	symbolDeclaration: string | null;
	symbolDeclarationRange: Range | null;
	symbolDeclarationFile: string | null;
	symbolDoc: string | null;
	comment?: string | null;
	error?: ErrorType | null;
};
export declare interface PvsListDeclarationsResponseType extends PvsResponseType {
	error: PvsErrorType,
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

export declare interface StrategyDescriptor {
	name: string,
	description?: string
}


export declare interface PvsErrorType {
	msg: string,
	parserError?: ErrorType,
	restartOption?: number | string
};
export declare interface PvsResponseType {
	error: PvsErrorType,
	res: any, // json object -- TODO: define the different types of objects, include a field "type" in each object
	raw: string // raw output of pvs-lisp
};
export declare interface ChangeContextResponseType {
	context: string
}
export declare interface PvsChangeContextResponseType extends PvsResponseType {
	error: PvsErrorType,
	res: ChangeContextResponseType,
	raw: string
}
export declare interface PvsCurrentContextResponseType extends PvsResponseType {
	error: PvsErrorType,
	res: string,
	raw: string
}
export declare interface VersionInfoResponseType {
	pvsVersion: string,
	lispVersion: string
}
export declare interface PvsVersionInfoResponseType extends PvsResponseType {
	error: PvsErrorType,
	res: VersionInfoResponseType,
	raw: string
}
export declare interface JsonType {
	[ key: string ]: JsonType | string; 
}
export declare interface ProofNodeType { 
	id: string, 
	children: ProofNodeType[], 
	type: string 
}
export declare interface ProofObjectType {
	proof: ProofNodeType
}
export declare interface PvsListProofStrategiesResponseType extends PvsResponseType {
	error: PvsErrorType,
	res: StrategyDescriptor[],
	raw: string
}
export interface FindDeclarationResponseType {
    [ key: string ] : PvsDeclarationType;
}
export declare interface PvsFindDeclarationResponseType extends PvsResponseType {
	error: PvsErrorType,
	res: FindDeclarationResponseType,
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

export declare interface PvsParserResponse {
	fileName: string,
	res: string,
	error: ErrorType
}

export declare interface FormulaMap {
	[ formulaName: string ]: FormulaDescriptor
}

export declare interface TheoremsStatus {
	theoryName: string;
	theorems: FormulaMap;
}

export declare interface TheoryStatusMap {
	[ theoryName: string ]: TheoremsStatus
}

export declare interface PvsTypecheckerResponse {
	fileName: string,
	res: TheoryStatusMap,
	error: ErrorType
}



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

export declare interface PvsFileListDescriptor {
	folder: string, // base path
	fileNames: string[] // pvs files
}

export declare interface ProofDescriptor {
	fileName: string;
	fileExtension: string;
	formulaName: string;
	theoryName: string;
	line: number;
	pvsContextFolder: string;
}

export declare interface ProofStructure {
	desc: ProofDescriptor,
	proof: { id: string, children: any[] }
}

export declare interface PvsCliInterface {
	pvsPath: string,
	pvsContextFolder: string,
	cmd: string,
	fileName: string,
	fileExtension: string, // either .pvs or .tccs -- TODO: keep this info directly in fileName
	theoryName?: string,
	formulaName?: string,
	line?: number
}

// export declare interface PvsExecutionContext {
// 	pvsPath: string,
// 	pvsContextFolder: string
// }

export const PVS_CLI_FILE: string = 'PvsCli';

export declare interface SimpleConsole {
	log: (str: string) => void,
	error: (str: string) => void,
	info: (str: string) => void,
	warn: (str: string) => void
}

export declare interface SimpleConnection {
    console: SimpleConsole,
	sendNotification?: (type: string, msg?: string) => void;
	sendRequest?: (type: string, data: any) => void;
}


export declare interface TheoryMap {
	[ theoryName: string ]: {
		theoryName: string,
		fileName: string,
		position: Position
	}
}


export interface TheoryDescriptor {
	theoryName: string,
	fileName: string,
	position: Position
}

export declare interface FormulaDescriptor {
	fileName: string;
	theoryName: string;
	formulaName: string;
	position: Position;
	status: string; // proof status
	isTcc?: boolean;
}

export declare interface FileList {
	pvsContextFolder: string;
	fileNames: string[]; // TODO: FileDescriptor[]
}

export declare interface TheoryList {
	pvsContextFolder: string;
	theories: TheoryMap; //  TODO TheoryDescriptor[]
}

// export declare interface TheoremList {
// 	pvsContextFolder: string;
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
export declare interface TheoryStatus {
	fileName: string,
	theoryName: string,
	theorems: FormulaMap
}
export declare interface TheoriesStatusMap {
	[ theoryName: string ]: TheoryStatus
}
export declare interface TheoriesMap {
	pvsContextFolder: string;
	theoriesStatusMap: TheoriesStatusMap;
}