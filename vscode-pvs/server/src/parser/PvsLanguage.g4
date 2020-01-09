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
	| { notifyErrorListeners("Missing theory name after keyword 'END'"); } K_END
    ;
theoryBody
	: assumingPart? theoryPart?
	;

theoryFormals
    : ('[' (theoryFormalType | theoryFormalConstant) (',' (theoryFormalType | theoryFormalConstant))* ']')
    ;
theoryFormalType
	: identifierOrOperators ':' (K_TYPE | K_NONEMPTY_TYPE | K_TYPE_PLUS)
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
	: identifier (',' identifier)* ':' K_ASSUMPTION expr
	;

theoryPart
	: (importing | declaration)+
	;

declaration
	: typeDeclaration
	| formulaDeclaration
	| varDeclaration
	| functionDeclaration
	| judgementDeclaration
	| conversionDeclaration
	| autorewriteDeclaration
	| declaration ';'
	;
autorewriteDeclaration
    : (K_AUTO_REWRITE | K_AUTO_REWRITE_PLUS | K_AUTO_REWRITE_MINUS) name (',' name)*
    ;
typeDeclaration
	: identifier arguments* (identifier arguments*)* ':' (K_TYPE | K_NONEMPTY_TYPE | K_TYPE_PLUS) typeDefinition?
	;
typeDefinition
	: ('=' | K_FROM) typeExpression (K_CONTAINING expr)?
	// error handling
	| { notifyErrorListeners("Missing '=' before type definition"); } typeExpression (K_CONTAINING expr)?
	;

formulaDeclaration
	: identifier ':' K_FORMULA formulaDefinition
	// error handling
	| identifier { notifyErrorListeners("Missing ':' before keyword K_FORMULA"); } K_FORMULA formulaDefinition
	;
formulaDefinition
    : expr
    ;

functionDeclaration
	: identifierOrOperators ':' typeExpression functionDefinition?
	| (identifier | unaryOp | binaryOp) arguments* 
		':' (K_MACRO | K_INDUCTIVE | K_RECURSIVE)? typeExpression 
		(functionDefinition measureExpression?)?
	;

functionDefinition
    : '=' expr
	// error handling
	// | { notifyErrorListeners("Missing '=' before definition"); } expr // this is erroneously catching also correct syntax (see sandbox/lib.pvs), need to refine it
    ;

judgementDeclaration
	: subtypeJudgement
	| constantJudgement
	;
subtypeJudgement
	: ((identifier | unaryOp | binaryOp) ':')? K_JUDGEMENT typeExpression (',' typeExpression)* K_SUBTYPE_OF typeExpression
	;
constantJudgement
	: ((identifier | unaryOp | binaryOp) ':')? K_RECURSIVE? K_JUDGEMENT (NUMBER | (name bindings*)) (',' (NUMBER | '{' name bindings* '}'))* K_HAS_TYPE typeExpression
	;

conversionDeclaration
	: (K_CONVERSION | K_CONVERSION_PLUS | K_CONVERSION_MINUS) name (':' typeExpression)? (',' name (':' typeExpression))*
	;

varDeclaration
	: identifierOrOperators ':' K_VAR typeExpression
	;

arguments
	: '(' expr (':' expr)? (',' expr (':' expr)?)* ')'
	| '(' subtype (',' subtype)* ')'
	;
typeExpression
	: enumerationType
	| recordType
	| tupleType
	| functionType
	| bindingDeclaration
	| subtype
	| name ('.' typeExpression)?
	;
expr
//	: constantExpression
	: unaryOp+ expr
	| expr (binaryOp expr)+
    | listExpression
    | recordExpression
	| typeExpression
	| term
	| '(' expr ')'
	;

// constantExpression
//     : 	open+='('* unaryOp? term closed+=')'* (binaryOp open+='('* unaryOp? term closed+=')'*)* // to maximize parsing speed, this rule does not check matching parentheses and does not enforce associativity of binary operators. A second parser, specialized for expression is in charge of those checks.
//     ;
term
    : name ('`' term)*
	| ifExpression
	| bindingExpression
    | letExpression
    | tupleExpression
    | term K_WHERE letBindings
    | term K_WITH '[' assignmentExpression (',' assignmentExpression)* ']'
	| term '::' typeExpression // coercion expression, i.e., expr is expected to be of type typeExpression
    | NUMBER
    | TRUE_FALSE
	| STRING
    ;
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
    : '(:' expr (',' expr)* ':)'
    | 'cons' '(' expr (',' expr)* ')'
    | listExpression (O_CONCAT '('* listExpression ')'*)+ // to maximize parsing speed, this rule does not check matching parentheses
    | '(' listExpression ')'
    ;
recordExpression
    : '(#' assignmentExpression (',' assignmentExpression)* '#)'
    | '(' recordExpression ')'
	// error handling
    | '(#' (assignmentExpression (',' assignmentExpression)*)? { notifyErrorListeners("Missing `,` in record expression"); } (assignmentExpression+ (',' assignmentExpression)*)* '#)' // error: omission of comma
    | '(#' (assignmentExpression (',' assignmentExpression)*)? (','+ { notifyErrorListeners("Too many `,` in record expression"); } assignmentExpression?)* '#)' // error: extra commas
	| { notifyErrorListeners("Mismatching record parentheses: '(' should be '(#"); } '(' assignmentExpression (',' assignmentExpression)* '#)' // error: mismatching parentheses
	| '(#' assignmentExpression (',' assignmentExpression)* { notifyErrorListeners("Mismatching record parentheses: ')' should be '#)"); } ')' // error: mismatching parentheses
    ;
measureExpression
	: K_MEASURE expr (K_BY (unaryOp | binaryOp | expr))?
	;

bindingExpression
	: (K_FORALL | K_EXISTS | K_LAMBDA) lambdaBody
	| '(' bindingExpression ')'
	;
lambdaBody
	: lambdaBindings+ ':' expr
	| lambdaBody (',' lambdaBody)+
	| '(' lambdaBody ')'
	;
lambdaBindings
	: bindings (',' bindings)*
	| '(' lambdaBindings ')'
	;
bindings
	: binding (',' binding)*
	;
binding
	: typeId
	| '(' typeIds ')'
	;
typeId
	: (identifier | unaryOp | binaryOp) (':' expr)? ('|' expr)?
	;
typeIds
	: identifierOrOperators (':' expr)? ('|' expr)?
	// error handling
	| expr { notifyErrorListeners("Missing ':' in binding expression"); } expr
	;
letBindings
	: letBinding (',' letBinding)*
	;
letBinding
	: letBind (',' letBind)* '=' expr
	;
letBind
	: (identifier | unaryOp | binaryOp)? bindings* (':' expr)?
	;
assignmentExpression
	: assignmentIdentifier (':=' | '|->') expr
	| '(' assignmentExpression ')'
	// error handling
	| assignmentIdentifier { notifyErrorListeners("'=' should be ':="); } '=' expr
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
	| '[#' (bindingDeclaration (',' bindingDeclaration)*)? { notifyErrorListeners("Missing `,` in record type definition"); } (bindingDeclaration+ (',' bindingDeclaration)*)* '#]' // error: missing commas
	| '[#' (bindingDeclaration (',' bindingDeclaration)*)? (','+ { notifyErrorListeners("Too many `,` in record type definition"); } bindingDeclaration?)* '#)' // error: extra commas
	| { notifyErrorListeners("Mismatching record parentheses: '[' should be '[#"); } '[' bindingDeclaration (',' bindingDeclaration)* '#]' // error: mismatching parentheses
	| '[#' bindingDeclaration (',' bindingDeclaration)* { notifyErrorListeners("Mismatching record parentheses: ']' should be '#]'"); } ']' // error: mismatching parentheses
	| { notifyErrorListeners("Incorrect parentheses in record type definition: '[ .. ]' should be '[# .. #]'"); } '[' bindingDeclaration (',' bindingDeclaration)* ']'
	;

functionType
	: (K_FUNCTION | K_ARRAY)? 
		'[' (identifierOrOperators ':')? typeExpression (',' (identifierOrOperators ':')? typeExpression)* '->' typeExpression ']'
	;

tupleType
	: '[' (identifierOrOperators ':')? typeExpression (',' (identifierOrOperators ':')? typeExpression)* ']'
	;

subtype
	: '{' name (',' name)* (':' expr)? (',' name (',' name)* (':' expr)?)* ('|' expr)? '}'
	| '(' name (':' expr)? ('|' expr)? ')' // shotcut for subtype
//	| '(' expr ')' // another shortcut for subtype
	// | name ('|' expr)? // another shortcut for subtype
	;

name
	: (identifier '@')? (identifier | unaryOp | binaryOp) actuals? arguments*
	| '(' name ')'
	;

actuals
	: '[' expr (',' expr)* ']'
	;

enumerationType
	: '{' identifierOrOperators '}'
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
	: (ID '.')? ID
	| '(' identifier ')';

identifierOrOperators
	: (identifier | unaryOp | binaryOp) (',' (identifier | unaryOp | binaryOp))*;

unaryOp: '+' | '-' | O_NOT | '~' | '[]' | '<>';
binaryOp: O_IFF | O_IMPLIES | O_AND | O_OR | '*' | '/' | '+' | '-' | O_LE | '<' | O_GE | '>' | O_NOT_EQUAL | O_EQUAL | O_EXP | O_CONCAT | O_SUCH_THAT | '##' | '<<' | '>>' | '<<=' | '>>=';


