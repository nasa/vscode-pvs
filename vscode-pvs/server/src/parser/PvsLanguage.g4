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
 	: identifier ('[' theoryFormal (',' theoryFormal)* ']')? ':' K_THEORY 
	 	K_BEGIN
			assumingPart?
			theoryPart?
		K_END identifier
	;

theoryFormal
	: ('(' importing ')')? theoryFormalDeclaration
	;
theoryFormalDeclaration
	: theoryFormalType
	| theoryFormalConst
	;
theoryFormalType
	: identifier ':' (K_TYPE | K_NONEMPTY_TYPE | K_TYPE_PLUS)
	;
theoryFormalConst
	: identifierOrOperators ':' typeExpression
	;

assumingPart
	: K_ASSUMING assumingElement (';' assumingElement)* K_ENDASSUMING
	;
assumingElement
	: importing
	| assumption
	| theoryDeclaration
	;
assumption
	: identifiers ':' K_ASSUMPTION expr
	;

theoryPart
	: theoryElement+
	;
theoryElement
	: importing
	// judgement
	// conversion
	// autorewrite
	| theoryDeclaration ';'?
	;
theoryDeclaration
	: typeDeclaration
	| formulaDeclaration
	| constantDeclaration
	| functionDeclaration
	| recursiveDeclaration
	| conversionDeclaration
	| varDeclaration
	;

typeDeclaration
	: identifiers arguments* ':' (K_TYPE | K_NONEMPTY_TYPE | K_TYPE_PLUS) typeDefinition?
	;
typeDefinition
	: ('=' | K_FROM) typeExpression (K_CONTAINING expr)?
	;

formulaDeclaration
	: identifiers ':' K_FORMULA expr
	;

constantDeclaration
	: identifierOrOperators ':' typeExpression ('=' expr)?
	;

functionDeclaration
	: identifierOrOperator arguments*
		':' (K_MACRO | K_INDUCTIVE)? typeExpression ('=' expr)?
	;

recursiveDeclaration
	: identifierOrOperator arguments*
		':' K_RECURSIVE typeExpression
		'=' expr
		measureExpression
	;

conversionDeclaration
	: (K_CONVERSION | K_CONVERSION_PLUS | K_CONVERSION_MINUS) name (':' typeExpression)? (',' name (':' typeExpression))*
	;

varDeclaration
	: identifierOrOperators ':' K_VAR typeExpression
	;

arguments
	: '(' identifiers (':' expr)? (',' identifiers (':' expr)?)* ')'
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
	: expr binaryOp expr+
	| expr arguments
	| functionExpression
	| typeExpression
	| unaryOp expr+
	| TRUE_FALSE
	| name
	| ifExpression
	| bindingExpression
	| '(:' expr (',' expr)* ':)' // list literal
	| '(#' assignmentExpression (',' assignmentExpression)* '#)' // record literal
	| '(' expr (',' expr)* ')'
	| K_LET letBindings K_IN expr+
	| expr K_WHERE letBindings
	| expr K_WITH '[' assignmentExpression (',' assignmentExpression)* ']'
	| NUMBER
	| STRING
	;
ifExpression
	: K_IF expr K_THEN expr (K_ELSIF expr K_THEN expr)* K_ELSE expr K_ENDIF
	| K_COND expr '->' expr (',' expr '->' expr)* (',' K_ELSE '->' expr)? K_ENDCOND
	;
measureExpression
	: K_MEASURE expr (K_BY expr)?
	;

bindingExpression
	: (K_FORALL | K_EXISTS | K_LAMBDA) lambdaBindings ':' expr
	;
lambdaBindings
	: lambdaBinding (',' lambdaBinding)* //identifierOrOperators ':' typeExpression (',' identifierOrOperators ':' typeExpression)*
	;
lambdaBinding
	: identifierOrOperator
	| bindings
	;
bindings
	: binding (',' binding)*
	;
binding
	: typeId
	| '(' typeIds ')'
	;
typeId
	: identifierOrOperator (':' expr+)? ('|' expr+)?
	;
typeIds
	: identifierOrOperators (':' expr+)? ('|' expr+)?
	;
letBindings
	: letBinding (',' letBinding)*
	;
letBinding
	: letBind (',' letBind)* '=' expr
	;
letBind
	: identifierOrOperator? bindings* (':' expr)?
	;
functionExpression
	: identifier actuals? '(' expr+ (',' expr+)* ')' ('(' expr+ (',' expr+)* ')')*
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
	: '{' identifiers ':' expr '|' expr '}'
	| '(' identifiers (':' expr ('|' expr)?)? ')' // shotcut for subtype
	| '(' expr ')' // another shortcut for subtype
	// | name ('|' expr)? // another shortcut for subtype
	;

name
	: (identifier '@')? identifierOrOperator actuals? arguments*
	;

actuals
	: '[' expr (',' expr)* ']'
	;

enumerationType
	: '{' identifierOrOperators '}'
	;

importing
	: K_IMPORTING theoryName (',' theoryName)*
	;

theoryName
	: (identifier '@')? identifier actuals?
	;

datatype
	: identifier ':' K_DATATYPE K_BEGIN K_END identifier
	;

identifier: ID;
identifiers: identifier (',' identifier)*;
identifierOrOperator: identifier | unaryOp | binaryOp;
identifierOrOperators: identifierOrOperator (',' identifierOrOperator)*;

unaryOp: '+' | '-' | O_NOT | '~' | '[]' | '<>';
binaryOp: '`' | O_CONCAT | O_IFF | O_IMPLIES | O_AND | O_OR | '*' | '/' | '+' | '-' | O_LE | '<' | O_GE | '>' | O_NOT_EQUAL | O_EQUAL | O_EXP;


