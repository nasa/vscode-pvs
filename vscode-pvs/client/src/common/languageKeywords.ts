"use strict";

export function regExpSource(v: Array<string>) {
	v = v.filter((elem:string) => {
		return elem !== "auto_rewrite+" && elem !== "auto_rewrite-" && elem !== "type+";
	}).map((elem: string) => {
		// keywords ending with +/- need negative lookahead
		// regex /\btype\+?/ is not good enough because it does not check the keyword boundary (e.g., types would be captured)
		// regexp /\btype\+?\b/ does not work either, because \b is designed to work with words, not \+ symbols
		if (elem === "type") {
			return "\\b(?!" + elem + "\\+)"
					+ elem + "\\b|\\b" + elem + "\\+";
		} else if (elem === "auto_rewrite") {
			return "\\b(?!" + elem + "[\\+\\-])"
					+ elem + "\\b|\\b" + elem + "[\\+\\-]";
		}
		const op: string = elem.includes("?") ? "?"
							: elem.includes("+") ? "+" 
							: elem.includes("!") ? "!" : null;
		return (op) ? "\\b" + elem.replace(op, "\\" + op)
					: "\\b" + elem + "\\b";
	});
	let ans: string = v.join("|");
	return ans;
}

// from pvs-prover-helps.el
// export const PVS_RESERVED_WORDS = [ "assuming", "axiom", "accept", "changes", "all", "and", "array", "begin", "by", "case", "cond", "endcond", "declare", "definition", "else", "elsif", "endif", "endassuming", "endcase", "end", "exists", "exporting", "exit", "forall", "function", "formula", "from", "if", "iff", "implies", "importing", "in", "is", "lambda", "lemma", "loop", "mapping", "measure", "module", "not", "nothing", "of", "onto", "obligation", "opspec", "or", "proof", "prove", "recursive", "result", "theorem", "theory", "using", "var", "variable", "record", "verify", "where", "then", "type", "when", "while", "with", "let", "setvariable" ];

export const PVS_COMMENT_REGEXP_SOURCE: RegExp = /%[^\n\r]+/g; // line starting with %

// from pvs-mode.el
// const PVS_OPERATORS = [ "#", "##", "\\$", "\\$\\$", "&", "&&", "\\*", "\\*\\*", "\\+", "\\+\\+", "-", "/",
// 						"//", "/=", "/\\\\", "::", ":=", "<", "<-", "<<", "<<=", "<=", "<=>", "<>", "<|",
// 						"=", "==", "==>", "=>", ">", ">=", ">>", ">>=", "@", "@@", "\\[\\]", "\\\\/", "\\^",
// 						"\\^\\^", "|-", "|->", "|=", "|>", "~" ];

const BRACKETS_REGEXP = "[\\(\\[]#|#[\\)\\]]"; // (# #) [# #]
const PVS_OPERATORS_REGEXP_SOURCE = "\\(:|:\\)|->|:="; // (: :) -> :=

// from pvs-mode.el
// const PVS_OPERATORS = "\\(<|\\||-\\||->\\||=\\||>\\|\\[\\]\\|/\\\\\\)"
// 								+ "|" + "\\((#\\|#)\\|\\[#\\|#\\]\\)"
// 								+ "|" + "\\((:\\|:)\\|(|\\||)\\|(\\|)\\)"
// 								+ "|" + "\\(\\[|\\||\\]\\||\\[\\|\\]|\\|||\\)"
// 								+ "|" + "\\(\\[\\|->\\|\\]\\)";

// from pvs-mode.el
export const PVS_KEYWORDS = [ "and", "andthen", "array", "as", "assuming", "assumption", "auto_rewrite",
							"auto_rewrite+", "auto_rewrite-", "axiom", "begin", "but", "by", "cases",
							"challenge", "claim", "closure", "codatatype", "coinductive", "cond",
							"conjecture", "containing", "conversion", "conversion+", "conversion-",
							"corollary", "datatype", "else", "elsif", "end", "endassuming", "endcases",
							"endcond", "endif", "endtable", "exists", "exporting", "fact", // "false",
							"forall", "formula", "from", "function", "has_type", "if", "iff", "implies",
							"importing", "in", "inductive", "judgement", "lambda", "law", "lemma", "let",
							"library", "macro", "measure", "nonempty_type", "not", "o", "obligation", "of",
							"or", "orelse", "postulate", "proposition", "recursive", "sublemma", "subtypes",
							"subtype_of", "table", "then", "theorem", "theory", //"true", 
							"type", "type+", "var", "when", "where", "with", "xor" ]

export const PVS_TRUE_FALSE: string[] = [ "true", "false" ];
export const PVS_TRUE_FALSE_REGEXP_SOURCE: string = "true|false";

export const PVS_RESERVED_WORDS_REGEXP_SOURCE: string = regExpSource(PVS_KEYWORDS);
export const PVS_LANGUAGE_OPERATORS_REGEXP_SOURCE: string = BRACKETS_REGEXP + "|" + PVS_OPERATORS_REGEXP_SOURCE;// + "|" + regExp(PVS_OPERATORS);
// export const ARITHMETIC_OPERATORS_REGEXP_SOURCE = "[\\s\\d\\w]+(<|>|=|<=|>=)[\\s\\d\\w]+"; // < > = <= >= //FIXME!

// export const PVS_RESERVED_WORDS_REGEXP = new RegExp(PVS_RESERVED_WORDS_REGEXP_SOURCE, "gi");
// from pvs-utils.el
// "\\bassuming\\b|\\baxiom\\b|\\baccept\\b|\\bchanges\\b|\\ball\\b|\\band\\b|\\barray\\b"
// 										+ "|\\bbegin\\b|\\bby\\b|\\bcase\\b|\\bdeclare\\b|\\bdefinition\\b|\\belse\\b|\\belsif\\b"
// 										+ "|\\bendif\\b|\\bendassuming\\b|\\bendcase\\b|\\bend\\b|\\bexists\\b|\\bexporting\\b"
// 										+ "|\\bexit\\b|\\bforall\\b|\\bfunction\\b|\\bformula\\b|\\bfrom\\b|\\bif\\b|\\biff\\b"
// 										+ "|\\bimplies\\b|\\bimporting\\b|\\bin\\b|\\bis\\b|\\blambda\\b|\\blemma\\b|\\bloop\\b"
// 										+ "|\\bmapping\\b|\\bmeasure\\b|\\bmodule\\b|\\bnot\\b|\\bnothing\\b|\\bof\\b|\\bonto\\b"
// 										+ "|\\bobligation\\b|\\bopspec\\b|\\bor\\b|\\bproof\\b|\\bprove\\b|\\brecursive\\b|\\bresult\\b"
// 										+ "|\\btheorem\\b|\\btheory\\b|\\busing\\b|\\bvar\\b|\\bvariable\\b|\\brecord\\b|\\bverify\\b"
// 										+ "|\\bwhere\\b|\\bthen\\b|\\btype\\b|\\bwhen\\b|\\bwhile\\b|\\bwith\\b|\\blet\\b|\\bsetvariable\\b"
// 										+ "|\\[#|#\\]|[(]#|#[)]";

// todo: make the list of builtin types consistent with that in pvs-language.json 
export const PVS_BUILTIN_TYPES: string[] = [ "bool", "boolean", "nat", "natural", "int", "integer", "real", "string",
												"void", "posreal", "nonneg_real", "nnreal", "nzreal", "list", "set",
												"negreal", "nonzero_real", "nonpos_real", "nonzero_integer", "nonneg_rat",
												"posnat", "nzint", "rat", "npreal", "nonpos_rat", "posrat", "negrat", "nzrat",
											 	"nnrat", "nonneg_int", "nonpos_int", "posint", "negint" ];
export const PVS_BUILTIN_TYPE_REGEXP_SOURCE: string = regExpSource(PVS_BUILTIN_TYPES);
// export const PVS_BUILTIN_TYPES_REGEXP = new RegExp(PVS_BUILTIN_TYPES_REGEXP_SOURCE, "gi");

export const PVS_LIBRARY_FUNCTIONS: string[] = [ "cons", "car", "cdr", "str2pvs", "pvs2str", "abs", "nth", "length", "reverse",
												 "ceiling", "floor", "fractional", "expt", "pred", "PRED", "predicate", "PREDICATE", "setof", "SETOF",
												 "unique?", "injective?", "surjective?", "bijective?", "upto", "below", "upfrom", "above", "subrange" ];
export const PVS_LIBRARY_FUNCTIONS_REGEXP_SOURCE: string = regExpSource(PVS_LIBRARY_FUNCTIONS);

export const PVS_NUMBER_REGEXP_SOURCE: string = "([+-]?)\\b(\\d+\/\\d+)|([+-]?)\\b(\\d+(?:\\.\\d+)?)"; // the first two capturing groups are sign and value for rationals (e.g., -1/12); the second two groups are sign and value for integers (e.g., +1) and reals (e.g., +0.12)

const PVS_PRELUDE_OBSOLETE_THEORIES: string[] = [ "int_types", "nat_types" ];
export const PVS_PRELUDE_OBSOLETE_THEORIES_REGEXP_SOURCE: string = regExpSource(PVS_PRELUDE_OBSOLETE_THEORIES);

export const PVS_STRING_REGEXP_SOURCE: string = '\"[^\"]*\"';

export const PVS_IDENTIFIER_REGEXP_SOURCE: string = "\\w+|^|<|>|<=|=>"

export interface SnippetType {
	description: string,
	prefix: string,
	scope: string,
	body: string[]
};

export function regexpOr(regexpSourceV: string[], mod?: string): RegExp {
	mod = mod || "g";
	return new RegExp(regexpSourceV.join("|"), mod);
}