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
export declare interface PvsListDeclarationsResponse {
	file?: string;
	line?: number;
	character?: number;
	symbolName: string | null;
	symbolTheory: string | null;
	symbolDeclaration: string | null;
	symbolDeclarationRange: Range | null;
	symbolDeclarationFile: string | null;
	symbolDoc?: string;
	comment?: string;
	error?: ErrorType;
}[];

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