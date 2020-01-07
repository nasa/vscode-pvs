/**
 * ANTLR4 parser rules for the PVS dynamic logic
 * @author Paolo Masci
 * @date 2019.12.31
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
 
grammar DdlLanguage;
import PvsLexer;

testDDL
    : dlProblem+ EOF
    ;

parseDDL
    : dlProblem EOF
    ;

dlProblem
    : initCondition operatorEntail dlProgram
    ;

initCondition
    : dlBooleanExpression
    ;

dlProgram: dlProgramIdentifier
    | dlAllRunsProgram
    | dlSomeRunsProgram
    ;

dlStatement
    : '(' dlStatement ')' '*' #dlStarStatement
    | dlStatement (operatorColon dlStatement)+ #dlSequentialStatement
    | dlStatement (',' dlStatement)+ #dlParallelStatement
    | dlStatement (operatorPlusPlus dlStatement)+ #dlUnionStatement
    | dlIdentifier operatorAssign '*' ('(' bindDeclaration (',' bindDeclaration)* ('|' dlBooleanExpression)? ')')? #dlAnyAssignmentStatement
    | dlDiffAssignmentElem (operatorColon dlDiffAssignmentElem)* (O_AND dlDiffInvariant)? #dlDiffStatement
    | dlSimpleAssignmentElem (operatorColon dlSimpleAssignmentElem)* #dlSimpleAssignmentStatement
    | '?' dlExpression #dlTestStatement
    | parenLeft dlStatement parenRight #dlParStatement
    ;


dlDiffAssignmentElem
    : dlDiffIdentifier operatorEqual dlExpression
    ;

dlDiffInvariant
    : dlBooleanExpression
    ;

dlSimpleAssignmentElem
    : dlIdentifier operatorAssign dlExpression
    ;

dlStarProgram
    : 
    ;

dlAllRunsProgram
    : '[' dlStatement ']' dlInvariant?
    ;

dlSomeRunsProgram
    : '<' dlStatement '>' dlInvariant?
    ;

dlInvariant
    : dlBooleanExpression
    ;

dlExpression
    : dlValue
    | dlBooleanExpression
    | dlArithmeticExpression
    | dlBindingExpression
    | operatorPlusMinus dlValue
    ;

dlArithmeticExpression
    :<assoc=right> dlArithmeticExpression operatorExp dlArithmeticExpression     #dlExpExpression
    |<assoc=left> dlArithmeticExpression operatorMulDiv dlArithmeticExpression   #dlMulDivExpression
    |<assoc=left> NUMBER dlValue                                                 #dlTimesExpression
    |<assoc=left> dlArithmeticExpression operatorPlusMinus dlArithmeticExpression   #dlPlusMinusExpression
    | parenLeft dlArithmeticExpression parenRight                                #dlParExpression
    | dlValue  #dlValueExpression
    | dlConst  #dlConstExpression
    | dlFunction #dlFunctionExpression
    ;

parenLeft: '(';
parenRight: ')';

dlBooleanExpression
    : dlBooleanExpression operatorAND dlBooleanExpression
    | dlBooleanExpression operatorOR dlBooleanExpression
    | operatorNOT dlBooleanExpression
    | dlBooleanExpression operatorCMP dlBooleanExpression
    | dlFunction
    | dlValue
    | dlConst
    | parenLeft dlBooleanExpression parenRight
    ;

operatorPlusPlus: '++';
operatorAND: O_AND;
operatorOR: O_OR;
operatorNOT: O_NOT;
operatorCMP: O_GE | O_LE | '>' | '<' | O_EQUAL;
operatorMulDiv: '*' | '/';
operatorPlusMinus: '+' | '-';
operatorComparison: O_LE | '<' | O_GE | '>' | O_NOT_EQUAL | O_EQUAL;
operatorEqual: O_EQUAL;

dlBindingExpression
    : K_FORALL '(' bindDeclaration (',' bindDeclaration)* ')' ':' dlExpression
    ;

bindDeclaration
    : dlIdentifier ':' typeName
    ;

dlFunction
    : dlFunctionName ('(' (dlArithmeticExpression | dlBooleanExpression) ')')+
    ;

dlFunctionName
    : ID
    ;

dlValue
    : ID
    ;

typeName
    : dlIdentifier
    ;

dlConst
    : operatorPlusMinus? NUMBER
    ;

dlDiffIdentifier
    : ID_PRIME
    ;

dlProgramIdentifier
    : ID
    ;

dlIdentifier
    : ID
    ;

operatorEntail: O_ENTAIL;
operatorAssign: O_ASSIGN;
operatorColon: ';';
operatorExp: '^';

// DIFF
ID_PRIME: ID'\'';

// keywords
K_PROBLEM: P R O B L E M;
O_ENTAIL: '|-';
O_ASSIGN: ':=';