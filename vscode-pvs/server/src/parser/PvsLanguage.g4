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
 	: theoryBegin
			assumingPart?
			theoryPart?
	  theoryEnd
	;
theoryBegin
    : identifier theoryFormals? ':' K_THEORY K_BEGIN
    ;
theoryEnd
    : K_END identifier
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
	// judgement
	// conversion
declaration
	: typeDeclaration
	| formulaDeclaration
	| constantDeclaration
	| varDeclaration
	| functionDeclaration
	// | recursiveDeclaration
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
	;

formulaDeclaration
	: identifier ':' K_FORMULA formulaDefinition
	;
formulaDefinition
    : expr
    ;

constantDeclaration
 	: identifierOrOperators ':' typeExpression ('=' constantDefinition)?
 	;
constantDefinition
    : expr
    ;

functionDeclaration
	: (identifier | unaryOp | binaryOp) arguments* 
		':' (K_MACRO | K_INDUCTIVE | K_RECURSIVE)? typeExpression 
		('=' functionDefinition)?
		measureExpression?
	// | error_functionDeclaration_missed_equal
	;
functionDefinition
    : expr
    ;
// error_functionDeclaration_missed_equal
//    : (identifier | unaryOp | binaryOp) arguments* ':' (K_MACRO | K_INDUCTIVE)? typeExpression functionDefinition { notifyErrorListeners("Missing '=' before definition."); }
//    ;
// recursiveDeclaration
// 	: (identifier | unaryOp | binaryOp) arguments*
// 		':' K_RECURSIVE typeExpression
// 		'=' functionDefinition
// 		measureExpression
// 	;

judgementDeclaration
	: subtypeJudgement
	| constantJudgement
	;
subtypeJudgement
	: ((identifier | unaryOp | binaryOp) ':')? K_JUDGEMENT typeExpression (',' typeExpression)* K_SUBTYPE_OF typeExpression
	;
constantJudgement
	: ((identifier | unaryOp | binaryOp) ':')? K_JUDGEMENT (NUMBER | (name bindings*)) (',' (NUMBER | '{' name bindings* '}'))* K_HAS_TYPE typeExpression
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
	| name
	;
expr
	: constantExpression
//	unaryOp+ expr
//	| expr (binaryOp expr)+
    | listExpression
    | recordExpression
    | expr K_WHERE letBindings
    | expr K_WITH '[' assignmentExpression (',' assignmentExpression)* ']'
	| term
	| typeExpression
	;

constantExpression
    : 	'('* unaryOp? term ')'* (binaryOp '('* unaryOp? term ')'*)* // to maximize parsing speed, this rule does not check matching parentheses and does not enforce associativity of binary operators. A second parser, specialized for expression is in charge of those checks.
    ;
term
    : name ('`' term)*
	| ifExpression
	| bindingExpression
    | letExpression
    | tupleExpression
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
    | '(' listExpression ')'
    | listExpression (O_CONCAT '('* listExpression ')'*)+ // to maximize parsing speed, this rule does not check matching parentheses
    ;
recordExpression
    : '(#' assignmentExpression (',' assignmentExpression)* '#)'
    | '(' recordExpression ')'
    ;
measureExpression
	: K_MEASURE expr (K_BY (unaryOp | binaryOp | expr))?
	;

bindingExpression
	: (K_FORALL | K_EXISTS | K_LAMBDA) lambdaBindings+ ':' expr
	| '(' bindingExpression ')'
	;
lambdaBindings
	: lambdaBinding (',' lambdaBinding)*
	| '(' lambdaBinding (',' lambdaBinding)* ')'
	;
lambdaBinding
	: 
	//identifier | unaryOp | binaryOp
	//| 
	bindings
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
	: identifier (':=' | '|->') expr
	;
bindingDeclaration
	: identifier ':' expr
	;
recordType
	: '[#' bindingDeclaration (',' bindingDeclaration)* '#]'
	;

functionType
	: (K_FUNCTION | K_ARRAY)? 
		'[' (identifierOrOperators ':')? typeExpression (',' (identifierOrOperators ':')? typeExpression)* '->' typeExpression ']'
	;

tupleType
	: '[' (identifierOrOperators ':')? typeExpression (',' (identifierOrOperators ':')? typeExpression)* ']'
	;

subtype
	: '{' identifier (',' identifier)* (':' expr)? (',' identifier (',' identifier)* (':' expr)?)* ('|' expr)? '}'
	| '(' expr (':' expr)? ('|' expr)? ')' // shotcut for subtype
//	| '(' expr ')' // another shortcut for subtype
	// | name ('|' expr)? // another shortcut for subtype
	;

name
	: (identifier '@')? (identifier | unaryOp | binaryOp) actuals? arguments*
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

identifier: (ID '.')? ID;
// identifiers: identifier (',' identifier)*;
//identifierOrOperator: identifier | unaryOp | binaryOp;
identifierOrOperators: (identifier | unaryOp | binaryOp) (',' (identifier | unaryOp | binaryOp))*;

unaryOp: '+' | '-' | O_NOT | '~' | '[]' | '<>';
binaryOp: O_IFF | O_IMPLIES | O_AND | O_OR | '*' | '/' | '+' | '-' | O_LE | '<' | O_GE | '>' | O_NOT_EQUAL | O_EQUAL | O_EXP | O_CONCAT | O_SUCH_THAT | '##' | '<<' | '>>' | '<<=' | '>>=';


