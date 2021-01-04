 /**
 * ANTLR4 lexer rules for the PVS language
 * @author Paolo Masci
 * @date 2019.12.22
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
 */
 
lexer grammar PvsLexer;

//------------------------
// Lexer Rules
// NOTES:
//  - in antlr, all lexer rules and constants start with a Capital letter
//  -  ANTLR resolves lexical ambiguities by matching the input string to the rule specified first in the grammar
//     An example common ambiguity in programming languages is that between keywords and identifier rules.
//     Keyword begin (followed by a nonletter) is also an identifier, at least lexically, so the lexer can match b-e-g-i-n to either rule.
//     Because of this, it is important to place all keywords before the identifiers
//------------------------
	
//------------------------
// Keywords, constants, and operators (case insensitive)
//------------------------
K_THEORY: T H E O R Y;
K_BEGIN: B E G I N;
K_END: E N D;
K_DATATYPE: D A T A T Y P E;
K_CODATATYPE: C O D A T A T Y P E;
K_IMPORTING: I M P O R T I N G;
K_TYPE: T Y P E;
K_NONEMPTY_TYPE: N O N E M P T Y '_' T Y P E;
K_TYPE_PLUS: T Y P E '+';
K_IF: I F;
K_THEN: T H E N;
K_ELSE: E L S E;
K_ELSIF: E L S I F;
K_ENDIF: E N D I F;
K_LAMBDA: L A M B D A;
K_EPSILON: E P S I L O N '!';
K_THE: T H E '!';
K_FORALL: F O R A L L;
K_EXISTS: E X I S T S;
K_FUNCTION: F U N C T I O N;
K_ARRAY: A R R A Y;
K_VAR: V A R;
K_RECURSIVE: R E C U R S I V E;
K_MEASURE: M E A S U R E;
K_BY: B Y;
K_FROM: F R O M;
K_CONTAINING: C O N T A I N I N G;
K_MACRO: M A C R O;
K_INDUCTIVE: I N D U C T I V E;
K_COINDUCTIVE: C O I N D U C T I V E;
K_LET: L E T;
K_IN: I N;
K_WHERE: W H E R E;
K_WITH: W I T H;
K_COND: C O N D;
K_ENDCOND: E N D C O N D;
K_CONVERSION: C O N V E R S I O N;
K_CONVERSION_PLUS: C O N V E R S I O N '+';
K_CONVERSION_MINUS: C O N V E R S I O N '-';
K_CASES: C A S E S;
K_ENDCASES: E N D C A S E S;
K_OF: O F;
K_TABLE: T A B L E;
K_ENDTABLE: E N D T A B L E;
K_AS: A S;

K_FORMULA
	: A X I O M 
	| C H A L L E N G E 
	| C L A I M 
	| C O N J E C T U R E 
	| C O R O L L A R Y 
	| F A C T 
	| F O R M U L A 
	| L A W 
	| L E M M A
	| O B L I G A T I O N 
	| P O S T U L A T E 
	| P R O P O S I T I O N
	| S U B L E M M A 
	| T H E O R E M
	;

K_ASSUMING: A S S U M I N G;
K_ENDASSUMING: E N D A S S U M I N G;
K_ASSUMPTION: A S S U M P T I O N;
K_JUDGEMENT: J U D G E M E N T;
K_HAS_TYPE: H A S '_' T Y P E;
K_SUBTYPE_OF: S U B T Y P E '_' O F;
K_AUTO_REWRITE: A U T O '_' R E W R I T E;
K_AUTO_REWRITE_PLUS: A U T O '_' R E W R I T E '+';
K_AUTO_REWRITE_MINUS: A U T O '_' R E W R I T E '-';
K_SUBTYPES: S U B T Y P E S;
K_EXPORTING: E X P O R T I N G;
K_ALL: A L L;
K_BUT: B U T;
K_CLOSURE: C L O S U R E;

O_IFF: (I F F) | '<=>' | '⇔';
O_IMPLIES: (I M P L I E S) | '=>' | '⇒';
O_NOT: (N O T) | '¬';
O_AND: (A N D) | '&' | '∧';
O_XOR: (X O R);
O_OR: (O R) | '∨';
O_NOT_EQUAL: '/=' | '≠';
O_LE: '<=' | '≤';
O_GE: '>=' | '≥';
O_EQUAL: '=';
O_EXP: '^';
O_COMMA: ',';
O_SUCH_THAT: '|';
COLON: ':';
TERMINATOR: ';';
O_DOUBLEHAT: '^^';

PAREN_L: '(';
PAREN_R: ')';
BRACKET_L: '[';
BRACKET_R: ']';
BRACE_L: '{';
BRACE_R: '}';
BRACE_BAR_L: '{|';
BRACE_BAR_R: '|}';

//------------------------
// Operators
//------------------------
O_DIV: '/';
// BINARYOP: 'o' | O_IFF | O_IMPLIES | O_AND | O_OR | '*' | '/' | '+' | '-' | '<=' | '<' | '>=' | '>' | '/=' | '='; //...
// UNARYOP: O_NOT | '~' | '[]' | '<>';

//------------------------
// Constants
//------------------------
TRUE_FALSE: C_TRUE | C_FALSE;

//------------------------
// Strings
//------------------------
STRING: '"' (ESC | .)*? '"';

//------------------------
// Numbers
//------------------------
NUMBER
	: RAT_NUMBER_FRACTIONAL_NOTATION
	| RAT_NUMBER_DOT_NOTATION
	| RAT_NUMBER_SCIENTIFIC_NOTATION
	| NAT_NUMBER
	| BUILTIN_CONSTANT
	;
RAT_NUMBER_FRACTIONAL_NOTATION: DIGIT+ '/' DIGIT+;
RAT_NUMBER_DOT_NOTATION: DIGIT+ ('.' DIGIT+)?;
RAT_NUMBER_SCIENTIFIC_NOTATION: DIGIT+ 'e' ('+'|'-')? DIGIT+;
NAT_NUMBER: '0' | (NZDIGIT DIGIT*);
BUILTIN_CONSTANT: PI;
PI: 'pi';

//------------------------
// Identifiers
//------------------------
ID: LETTER IDCHAR*;

//------------------------
// Fragments necessary for the lexer that we choose not to tokenize individually
//------------------------
fragment C_TRUE: T R U E;
fragment C_FALSE: F A L S E;
fragment IDCHAR
	: LETTER
	| DIGIT
	| '_'
	| '?'
	| SUBSCRIPT_NUMBER
	;
fragment SUBSCRIPT_NUMBER: '₀' | '₁' | '₂' | '₃' | '₄' | '₅' | '₆' | '₇' | '₈' | '₉';
fragment LETTER: [a-zA-Z];
fragment DIGIT: [0-9];
fragment NZDIGIT: [1-9];
fragment DIGITS: DIGIT+;
fragment ESC: '\\' [btnr"\\] ; // \b, \t, \n etc...

fragment A : [aA]; // match either an 'a' or 'A'
fragment B : [bB];
fragment C : [cC];
fragment D : [dD];
fragment E : [eE];
fragment F : [fF];
fragment G : [gG];
fragment H : [hH];
fragment I : [iI];
fragment J : [jJ];
fragment K : [kK];
fragment L : [lL];
fragment M : [mM];
fragment N : [nN];
fragment O : [oO];
fragment P : [pP];
fragment Q : [qQ];
fragment R : [rR];
fragment S : [sS];
fragment T : [tT];
fragment U : [uU];
fragment V : [vV];
fragment W : [wW];
fragment X : [xX];
fragment Y : [yY];
fragment Z : [zZ];

//------------------------
// Whitespace and comments
//------------------------
SPACE: [ ]+ -> channel(1);
TAB: [\t]+ -> channel(2);
CR: [\r\n\u000C]+ -> channel(3);
COMMENT: '%' ~[\r\n]* -> channel(4);
UnrecognizedChar: .;