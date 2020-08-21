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
	: (theory | datatype)* EOF
	;

theory
 	: theoryBegin theoryBody theoryEnd
	;
theoryBegin
    : identifier theoryFormals? ':' K_THEORY exporting? K_BEGIN
    ;
theoryEnd
    : K_END identifier
	// error handling
	| { notifyErrorListeners("Theory name expected after keyword 'END'."); } K_END
    ;

datatype
 	: datatypeBegin datatypeBody datatypeEnd
	;    
datatypeBegin
    : identifier theoryFormals? ':' (K_DATATYPE | K_CODATATYPE) (K_WITH K_SUBTYPES name (',' name)*)? K_BEGIN
    ;
datatypeEnd
    : K_END identifier
	// error handling
	| { notifyErrorListeners("Datatype name expected after keyword 'END'."); } K_END
    ;
datatypeBody
	: (importing | datatypeConstructor)*
	;

datatypeConstructor
	: term ':' identifier (':' identifier)?
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
	| datatype
	| declaration ';'
	;
autorewriteDeclaration
    : (K_AUTO_REWRITE | K_AUTO_REWRITE_PLUS | K_AUTO_REWRITE_MINUS) name (',' name)*
    ;
typeDeclaration
	:  typeName (',' typeName)* ':' (K_TYPE | K_NONEMPTY_TYPE | K_TYPE_PLUS) (('=' | K_FROM) typeDefinition)?
	;
typeName
	: identifier actuals* arguments*
	;
typeDefinition
	: typeExpression (K_CONTAINING expr)?
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
	: constantName (',' constantName)* ':' typeExpression
	| constantName ':' K_MACRO? typeExpression '=' constantDefinition
	;
constantName
	: identifierOrOperator actuals* arguments*
	;
constantDefinition
    : expr
	;
functionDeclaration
	: functionName arguments+ 
		':' (((K_MACRO | K_INDUCTIVE | K_COINDUCTIVE)? typeExpression ('=' functionDefinition)?)
				| (K_RECURSIVE typeExpression '=' functionDefinition measureExpression))
	;
functionName
	: (identifier | redefinableOp)
	;
functionDefinition
    : expr
    ;

judgementDeclaration
	: (judgementName ':')? K_RECURSIVE? K_JUDGEMENT judgementExpression (K_HAS_TYPE | K_SUBTYPE_OF) typeExpression
	;

judgementName
	: identifierOrOperator
	;
judgementExpression
	: bindingExpression
	| identifierOrOperator actuals* arguments*
	| typeExpression
	| judgementExpression (',' judgementExpression)+
	;

conversionDeclaration
	: (K_CONVERSION | K_CONVERSION_PLUS | K_CONVERSION_MINUS) idType (',' idType)*
	;

varDeclaration
	: varIdentifiers':' K_VAR typeExpression
	;

varIdentifiers
	: identifierOrOperators
	;

arguments
	: '(' expr (':' typeExpression)? (',' expr (':' typeExpression)?)* ')'
	| '(' subtype (',' subtype)* ')'
	;
typeExpression
	: subtype // important: subtype needs to come before enumeration type
	| enumerationType
	| recordType
	| tupleType
	| functionType
	| bindingDeclaration
	| name
	| theoryName ('.' typeExpression)?
	| typeExpression actuals+
	;

theoryName: (identifier '@')? identifier actuals?;

expr:
	builtin                   #builtinExpr
	| expr logicalBinaryOp expr      #binaryOpExpr
	| expr comparisonBinaryOp expr      #binaryOpExpr
	| expr arithmeticBinaryOp expr      #binaryOpExpr
	| expr otherBuiltinBinaryOp expr      #binaryOpExpr
	| unaryOp expr            #unaryOpExpr
	| expr '`' expr           #accessorExpr
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
	| expr '(' expr ')'       #holFunctionExpr
	| '(' expr ')' expr       #hol2FunctionExpr
	| '(' expr ')'            #parenExpr
	| '{|' expr '|}'          #parenOperatorExpr
	| typeExpression          #typeExpr // NB: typeExpression needs to be after parenExpression, otherwise expression surrounded by parentheses will be mistakenly identified as subtypes
	| expr redefinableOp expr  #binaryOpExpr
	| redefinableOp     #operatorExpr
	// error handling
    | expr K_WITH '[' (assignmentExpression (',' assignmentExpression)*)? { notifyErrorListeners("',' expected."); } assignmentExpression (assignmentExpression+ (',' assignmentExpression)*)? ']' #exprError
	;

withAssignments
	: '[' assignmentExpression (',' assignmentExpression)* ']'
	;

builtin
	: number
    | true_false
	| string
	| builtin comparisonBinaryOp builtin
	| builtin arithmeticBinaryOp builtin 
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
    | 'cons' actuals? '(' expr (',' expr)* ')'
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
	: (K_FORALL | K_EXISTS | K_LAMBDA | K_EPSILON | K_THE) lambdaBindings ':' lambdaBody
	| '(' bindingExpression ')'
	;
lambdaBindings
	: bindingName (',' bindingName)* (':' typeExpression)? ('|' expr)?
	| lambdaBindings (',' lambdaBindings)+
	| '(' lambdaBindings ')' ('(' lambdaBindings ')')*
	;
bindingName: identifierOrOperator;
lambdaBody: expr
	;
typeId
	: localName (':' typeExpression)? ('|' expr)?
	;
localName
	: identifierOrOperator
	;
typeIds
	: terms (':' typeExpression)? ('|' expr)?
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
	| '(' letBind ')'
	;
assignmentExpression
	: (assignmentIdentifier | expr) (':=' | '|->') expr
	| '(' assignmentExpression ')'
	// error handling
	| assignmentIdentifier { notifyErrorListeners("':=' expected."); } '=' expr
	;
assignmentIdentifier
	: (name? '`')? name
	| '(' assignmentIdentifier ')'
	;
bindingDeclaration
	: K_LAMBDA? identifierOrOperators ':' expr
	;
recordType
	: (name K_WITH)? '[#' bindingDeclaration (',' bindingDeclaration)* '#]'
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
	: '{' idType ('|' expr)? '}'
	| '(' expr ((',' | '/' | '*')  expr)* ('|' expr)? ')' // shotcut for subtype
	;

idType
	: terms (':' typeExpression)?
	| idType (',' idType)+
	| '(' idType ')'
	;

terms
	: term (',' term)*
	;

term
	: identifierOrOperator actuals? arguments*
	| '(' term (',' term)* ')'
	;

name
	: (theoryName '.')? identifierOrOperator actuals? arguments*
	;

actuals
	: '[' (expr | typeDeclaration) (',' (expr | typeDeclaration))* ']'
	;

enumerationType
	: '{' identifier (',' identifier) '}'
	;

importing
	: K_IMPORTING importingElement (',' importingElement)* ';'?
	;

importingElement
	: importTheoryName (K_AS identifier)? ('{''{' identifierOrOperator ':=' identifierOrOperator (',' identifierOrOperator ':=' identifierOrOperator)* '}''}')? 
	;
importTheoryName
	: (identifier '@')? identifier actuals?
	;

exporting
	: K_EXPORTING exportingNames (K_WITH exportingTheories)?
	;

exportingNames
	: K_ALL ((K_BUT | K_WITH) exportingName (',' exportingName)*)?
	| exportingName (',' exportingName)*
	;

exportingName
	: identifierOrOperator actuals? (':' '{' (typeExpression | K_TYPE | K_FORMULA) '}')?
	;

exportingTheories
	: K_ALL | K_CLOSURE | (theoryName (',' theoryName)*)
	;

identifier
	: ID
	;

identifierOrOperators
	: identifierOrOperator (',' identifierOrOperator)*;

identifierOrOperator
	: identifier | redefinableConstant | redefinableOp
	;

binaryOp: redefinableOp;
unaryOp: builtinUnaryOp;
redefinableOp: '~' | '[]' | '<>' | '##' | '#' | '<<' | '>>' | '<<=' | '>>=' | '{||}' | '[||]' | builtinBinaryOp | unaryOp | 'o' | '++' | '^^' | '==' | '|=' | '|-';
redefinableConstant: NUMBER;
builtinUnaryOp: '+' | '-' | O_NOT;
builtinBinaryOp: logicalBinaryOp | arithmeticBinaryOp | comparisonBinaryOp | otherBuiltinBinaryOp;
otherBuiltinBinaryOp: O_EXP | O_SUCH_THAT;
logicalBinaryOp: O_IFF | O_IMPLIES | O_AND | O_OR | O_XOR;
arithmeticBinaryOp: '*' | operatorDiv | '+' | '-';
comparisonBinaryOp: O_LE | '<' | '>' | O_GE | O_NOT_EQUAL | O_EQUAL;
operatorDiv: O_DIV;


