export interface Position {
	line: number, // this attribute ranges from 1 to n, while vscode.line ranges from 0 to n-1 
	character: number
};

export interface Range {
	start: Position,
	end: Position
};

export interface PeekDefinitionCommand {
	fileName: string,
	range: Range
};

export interface PvsVersionDescriptor {
	pvsVersion: string,
	lispVersion: string
};

export interface ErrorType {
	msg: string,
	fileName: string,
	line: number,
	character: number
};

export interface PvsDeclarationType {
	symbolName: string | null;
	symbolTheory: string | null;
	symbolDeclaration: string | null;
	symbolDeclarationRange: Range | null;
	symbolDeclarationFile: string | null;
};

export interface PvsDeclarationDescriptor {
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

export interface PvsFindDeclarationRequest {
	symbolName: string;
	file?: string;
	theory?: string;
	line?: number;
	character?: number;
}
export interface PvsFindDeclarationResponse {
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
export interface PvsListDeclarationsRequest {
	theoryName: string;
	file?: string;
	line?: number;
	character?: number;
}
export interface PvsListDeclarationsResponse {
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


export interface PvsErrorType {
	msg: string,
	parserError?: ErrorType,
	restartOption?: number | string
};
export interface PvsResponseType {
	error: PvsErrorType,
	res: any
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

export function getFilename(path: string, opt?: { removeFileExtension?: boolean }) {
	opt = opt || {};
	const pathlessFileName = path.split("/").slice(-1)[0];
	return (opt.removeFileExtension && pathlessFileName.endsWith(".pvs")) ?
				pathlessFileName.substr(0, pathlessFileName.length - 4)
					: pathlessFileName;
}
export function getPathname(path: string) {
	return path.split("/").slice(0, -1).join("/");
}

export interface PvsParserResponse {
	fileName: string,
	res: string,
	error: ErrorType
}

export interface PvsTypecheckerResponse {
	fileName: string,
	res: string,
	error: ErrorType
}

export interface TccDescriptor {
	id: string; // tcc ID
	formula: string; // Proof obligation
	symbol: string; // symbol that has triggered the tcc
	line: number; // position (line) of the symbol in the pvs file
	character: number; // position (character) of the symbol in the pvs file
	msg: string; // tcc message
	status: string; // tcc status
}

export interface TccDescriptorArray {
	theoryName: string; // theory name
	fileName: string; // file where the theory is defined
	tccs: TccDescriptor[]; // list of tccs
}

export function hasPvsExtension(fileName: string) {
	return fileName.endsWith('.pvs');
}

export interface PvsSymbolKind<type> {
	keywords: type,
	numbers: type,
	strings: type,
	constants: type,
	builtinTypes: type,
	operators: type, // language operators
	functions: type, // library functions
	comments: type
};

export interface ExpressionDescriptor {
	fileName: string,
	theoryName: string,
	expression: string
};

export interface EvaluationResult {
	theoryName: string,
	fileName: string,
	msg: string,
	result: string
};

export interface FormulaDescriptor {
	fileName: string,
	theoryName: string,
	formula: string,
	line: number
};

export interface ProofResult {
	theoryName: string,
	fileName: string,
	msg: string,
	result: string
};

export interface PrettyPrintRegionRequest {
	theoryName: string;
	range: Range
}

export interface PrettyPrintRegionResult {
	theoryName: string;
	range: Range;
	result: string;
}

export interface PvsTheoryListDescriptor {
	folder: string, // base path
	files: { [ fileName: string ] : string[] } // theories grouped by fileName
	theories: { [ theoryName: string ]: string[] } // files grouped by theoryName
}

export interface PvsFileListDescriptor {
	folder: string, // base path
	fileNames: string[] // pvs files
}

