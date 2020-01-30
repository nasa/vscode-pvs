 /**
 * ANTLR4 parser rules for the PVS language
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
 
grammar PvsLanguage;
import PvsLexer;

//------------------------
// Parser Rules (ATTN: in antlr, all parser rules start with a small letter)
//------------------------
parse
	: theory* EOF
	;

theory
 	: theoryBegin theoryBody theoryEnd
	;
theoryBegin
    : identifier theoryFormals? ':' K_THEORY K_BEGIN
    ;
theoryEnd
    : K_END identifier
	// error handling
	| { notifyErrorListeners("Theory name expected after keyword 'END'."); } K_END
    ;
theoryBody
	: assumingPart? (importing | declaration)*
	;
theoryFormals
    : ('[' (theoryFormalType | theoryFormalConstant) (',' (theoryFormalType | theoryFormalConstant))* ']')
    ;
theoryFormalType
	: ('(' importing ')')? typeDeclaration
	;
theoryFormalConstant
	: ('(' importing ')')? identifierOrOperators ':' typeExpression
	;

assumingPart
	: K_ASSUMING (assumingElement ';'?)+ K_ENDASSUMING
	;
assumingElement
    : importing
    | assumption
    | declaration
    ;
assumption
	: identifier ':' K_ASSUMPTION expr
	;

declaration
	: typeDeclaration
	| formulaDeclaration
	| varDeclaration
	| functionDeclaration
	| constantDeclaration
	| judgementDeclaration
	| conversionDeclaration
	| autorewriteDeclaration
	| declaration ';'
	;
autorewriteDeclaration
    : (K_AUTO_REWRITE | K_AUTO_REWRITE_PLUS | K_AUTO_REWRITE_MINUS) name (',' name)*
    ;
typeDeclaration
	: identifier arguments* (',' identifier arguments*)* ':' (K_TYPE | K_NONEMPTY_TYPE | K_TYPE_PLUS) (('=' | K_FROM) typeDefinition)?
	;
typeDefinition
	: typeExpression (K_CONTAINING expr)?
	// error handling
	| { notifyErrorListeners("'=' expected."); } typeExpression (K_CONTAINING expr)?
	;

formulaDeclaration
	: identifier ':' K_FORMULA formulaDefinition
	// error handling
	| identifier { notifyErrorListeners("':' expected."); } K_FORMULA formulaDefinition
	;
formulaDefinition
    : expr
    ;

constantDeclaration
	: constantName (',' constantName)* ':' K_MACRO? typeExpression ('=' constantDefinition)?
	;
constantName
	: (identifier | unaryOp | binaryOp | redefinableOperator)
	;
constantDefinition
    : expr
	;
functionDeclaration
	: functionName arguments+ 
		':' (K_MACRO | K_INDUCTIVE | K_RECURSIVE)? typeExpression 
		('=' functionDefinition measureExpression?)?
	;
functionName
	: (identifier | redefinableOperator)
	;
functionDefinition
    : expr
	// error handling
	// | { notifyErrorListeners("'=' expected."); } expr // this is erroneously catching also correct syntax (see sandbox/lib.pvs), need to refine it
    ;

judgementDeclaration
	: subtypeJudgement
	| constantJudgement
	;
subtypeJudgement
	: (judgementName ':')? K_JUDGEMENT typeExpression (',' typeExpression)* K_SUBTYPE_OF typeExpression
	;
constantJudgement
	: (judgementName ':')? K_RECURSIVE? K_JUDGEMENT (bindingExpression | ((name | redefinableOperator) arguments*))+ K_HAS_TYPE typeExpression
	;

judgementName
	: (identifier | unaryOp | binaryOp | redefinableOperator)
	;

conversionDeclaration
	: (K_CONVERSION | K_CONVERSION_PLUS | K_CONVERSION_MINUS) name (':' typeExpression)? (',' name (':' typeExpression))*
	;

varDeclaration
	: varIdentifiers':' K_VAR typeExpression
	;

varIdentifiers
	: (identifier | redefinableOperator) (',' (identifier | redefinableOperator))*
	;

arguments
	: '(' expr (':' typeExpression)? (',' expr (':' typeExpression)?)* ')'
	| '(' subtype (',' subtype)* ')'
	;
typeExpression
	: enumerationType
	| recordType
	| tupleType
	| functionType
	| bindingDeclaration
	| subtype
	| name
	| theoryName ('.' typeExpression)?
	;

theoryName: (identifier '@')? identifier actuals?;

expr:
	builtin                   #builtinExpr
	| groundExpression        #termExpr
	| expr (binaryOp (groundExpression | builtin | expr))   #binaryOpExpr // the use of term can reduce nesting for expressions in the parse tree
	| unaryOp+ (groundExpression | builtin | expr)          #unaryOpExpr
	| expr '`' expr           #exprAccessor
    | listExpression          #listExpr
    | recordExpression        #recordExpr
	| tableExpression         #tableExpr
	| ifExpression            #ifExpr
	| bindingExpression       #bindingExpr
    | letExpression           #letExpr
    | tupleExpression         #tupleExpr
	| expr '::' typeExpression #corcExpr
    | expr K_WITH  withAssignments #withExpr
    | expr K_WHERE letBindings #whereExpr
	| typeExpression          #typeExpr // NB: typeExpression needs to be after parenExpression, otherwise expression surrounded by parentheses will be mistakenly identified as subtypes
	| expr redefinableOperator expr  #binaryOpExpr
	| redefinableOperator     #operatorExpr
	| '(' expr ')'            #parenExpr
	// error handling
    | expr K_WITH '[' (assignmentExpression (',' assignmentExpression)*)? { notifyErrorListeners("',' expected."); } assignmentExpression (assignmentExpression+ (',' assignmentExpression)*)? ']' #exprError
	;

withAssignments
	: '[' assignmentExpression (',' assignmentExpression)* ']'
	;

// constantExpression
//     : 	open+='('* unaryOp? term closed+=')'* (binaryOp open+='('* unaryOp? term closed+=')'*)* // to maximize parsing speed, this rule does not check matching parentheses and does not enforce associativity of binary operators. A second parser, specialized for expression is in charge of those checks.
//     ;
groundExpression
    : term ('`' (number | term))*        #idAccessor
	| ('+' | '-' | O_NOT) groundExpression #unaryOpTerm
	| groundExpression (binaryOp groundExpression)+ #binaryOpTerm
	| groundExpression '::' typeExpression #coercTerm // coercion expression, i.e., expr is expected to be of type typeExpression
	| theoryName '.' groundExpression #nameDotTerm
	| '(' groundExpression ')' #parenTerm
    ;
builtin
	: number
    | true_false
	| string
	| builtin (comparisonBinaryOp builtin)
	| builtin (arithmeticBinaryOp builtin) 
	| '(' builtin ')'
	;
number
	: ('+' | '-')? NUMBER;
true_false: TRUE_FALSE;
string: STRING;
ifExpression
	: K_IF expr K_THEN expr (K_ELSIF expr K_THEN expr)* K_ELSE expr K_ENDIF
	| K_CASES expr K_OF expr ':' expr (',' expr ':' expr)* (K_ELSE expr)? K_ENDCASES
	| K_COND expr '->' expr (',' expr '->' expr)* (',' K_ELSE '->' expr)? K_ENDCOND
	| '(' ifExpression ')'
	;
letExpression
    : K_LET letBindings K_IN expr
    | '(' letExpression ')'
    ;
tupleExpression
    : '(' expr (',' expr)+ ')'
    | '(' tupleExpression ')'
    ;
listExpression
    : '(:' expr? (',' expr)* ':)'
    | 'cons' '(' expr (',' expr)* ')'
    // | listExpression ('o' '('* listExpression ')'*)+ // to maximize parsing speed, this rule does not check matching parentheses
    // | '(' listExpression ')'
    ;
recordExpression
    : '(#' assignmentExpression (',' assignmentExpression)* '#)'
	| '{|' expr (',' expr) '|}'
    | '(' recordExpression ')'
	// error handling
    | '(#' (assignmentExpression (',' assignmentExpression)*)? { notifyErrorListeners("','' expected."); } (assignmentExpression+ (',' assignmentExpression)*)* '#)' // error: omission of comma
    | '(#' (assignmentExpression (',' assignmentExpression)*)? (','+ { notifyErrorListeners("Assignment expression expected."); } assignmentExpression?)* '#)' // error: extra commas
    ;
tableExpression
	: K_TABLE colHeading? tableEntry+ K_ENDTABLE
	| '[|' expr (',' (expr | K_ELSE) )* '|]'
	;
colHeading
	: '|[' expr ('|' (expr | K_ELSE) )* ']''|'
	;
tableEntry
	: '|' expr ('|' (expr | K_ELSE) )* '|'
	;
measureExpression
	: K_MEASURE expr (K_BY (unaryOp | binaryOp | expr))?
	;

bindingExpression
	: (K_FORALL | K_EXISTS | K_LAMBDA | K_EPSILON) lambdaBindings ':' lambdaBody
	| '(' bindingExpression ')'
	;
lambdaBindings
	: bindingName (',' bindingName)* (':' typeExpression)? ('|' expr)?
	| lambdaBindings (',' lambdaBindings)+
	| '(' lambdaBindings ')' ('(' lambdaBindings ')')*
	;
bindingName: identifier | unaryOp | binaryOp | redefinableOperator;
lambdaBody: expr
	// : //lambdaBindings+ ':' expr
	// // | lambdaBody (',' lambdaBody)+
	// | '(' lambdaBody ')'
	;
// lambdaBindings
// 	: bindings (',' bindings)*
// 	| '(' lambdaBindings ')'
// 	;
// bindings
// 	: binding (',' binding)*
// 	;
// binding
// 	: typeId
// 	| '(' typeIds ')'
// 	;
typeId
	: localName (':' typeExpression)? ('|' expr)?
	;
localName
	: (identifier | unaryOp | binaryOp | redefinableOperator)
	;
typeIds
	: term (',' term)* (':' typeExpression)? ('|' expr)?
	// error handling
	| expr { notifyErrorListeners("':' expected."); } expr
	;
letBindings
	: letBinding (',' letBinding)*
	;
letBinding
	: letBind (',' letBind)* '=' expr
	;
letBind
	: (name | unaryOp | binaryOp) (':' typeExpression)?
	| '(' (name | unaryOp | binaryOp) (',' (name | unaryOp | binaryOp))* ')'  (':' typeExpression)?
	;
assignmentExpression
	: assignmentIdentifier (':=' | '|->') expr
	| '(' assignmentExpression ')'
	// error handling
	| assignmentIdentifier { notifyErrorListeners("':=' expected."); } '=' expr
	;
assignmentIdentifier
	: (name? '`')? name
	| '(' assignmentIdentifier ')'
	;
bindingDeclaration
	: identifier ':' expr
	;
recordType
	: '[#' bindingDeclaration (',' bindingDeclaration)* '#]'
	// error handling
	| '[#' (bindingDeclaration (',' bindingDeclaration)*)? { notifyErrorListeners("`,` expected."); } (bindingDeclaration+ (',' bindingDeclaration)*)* '#]' // error: missing commas
	| '[#' (bindingDeclaration (',' bindingDeclaration)*)? (','+ { notifyErrorListeners("Binding declaration expected."); } bindingDeclaration?)* '#]' // error: extra commas
	;

functionType
	: (K_FUNCTION | K_ARRAY)? 
		'[' (terms ':')? typeExpression (',' (terms ':')? typeExpression)* '->' typeExpression ']'
	;

tupleType
	: '[' (identifierOrOperators ':')? typeExpression (',' (identifierOrOperators ':')? typeExpression)* ']'
	;

subtype
	: '{' terms (':' typeExpression)? (',' terms (':' typeExpression)?)* ('|' expr)? '}'
	| '(' typeExpression (',' typeExpression)* ('|' expr)? ')' // shotcut for subtype
//	| '(' expr ')' // another shortcut for subtype
	// | name ('|' expr)? // another shortcut for subtype
	;

terms
	: term (',' term)*
	;

term
	: identifier actuals? arguments*
	| redefinableOperator actuals? arguments*
	| '(' term (',' term)* ')'
	;

name
	: //identifier actuals? arguments*
	// | (identifier '@')? identifierOrOperator actuals? arguments*//(arguments (arguments (arguments arguments?)?)?)? //(identifier '@')? (identifier | unaryOp | binaryOp) actuals? arguments*
	// | 
	(theoryName '.')? identifier actuals? arguments*//(arguments (arguments (arguments arguments?)?)?)? //(identifier '@')? (identifier | unaryOp | binaryOp) actuals? arguments*
	// | '(' name ')'
	;

actuals
	: '[' (expr | redefinableOperator) (',' (expr | redefinableOperator))* ']'
	;

enumerationType
	: '{' identifier (',' identifier) '}'
	;

importing
	: K_IMPORTING importTheoryName (',' importTheoryName)*
	;

importTheoryName
	: (identifier '@')? identifier actuals?
	;

datatype
	: identifier ':' K_DATATYPE K_BEGIN K_END identifier
	;

identifier
	: ID
	// | '(' identifier ')'
	;

identifierOrOperators
	: (identifier | redefinableOperator ) (',' (identifier | redefinableOperator ))*;

redefinableOperator: '~' | '[]' | '<>' | '##' | '<<' | '>>' | '<<=' | '>>=' | '{||}' | binaryOp | unaryOp | 'o' | '++';
unaryOp: '+' | '-' | O_NOT;
binaryOp: logicalBinaryOp | arithmeticBinaryOp | comparisonBinaryOp | O_EXP | O_SUCH_THAT;
logicalBinaryOp: O_IFF | O_IMPLIES | O_AND | O_OR;
arithmeticBinaryOp: '*' | operatorDiv | '+' | '-';
comparisonBinaryOp: O_LE | '<' | O_GE | '>' | O_NOT_EQUAL | O_EQUAL;
operatorDiv: O_DIV;
// operatorConcat: 'o';


