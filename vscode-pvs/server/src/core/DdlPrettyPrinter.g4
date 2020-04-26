/**
 * ANTLR4 parser rules for pretty-printing PVS dynamic logic
 * @author Paolo Masci
 * @date 2020.04.22
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
 
grammar DdlPrettyPrinter;
import PvsLanguage, PvsLanguage;

prettyprintDdl
	: dlExpr*
	;

dlExpr
	: dlExpr logicalBinaryOp dlExpr
	| dlExpr comparisonBinaryOp dlExpr
	| dlExpr arithmeticBinaryOp dlExpr
	| dlExpr otherBuiltinBinaryOp dlExpr
	| unaryOp dlExpr
	| dlExpr '`' dlExpr

	| letExpression // overrides pvslanguage.letExpression
	| entailEmptyInit

	| quantifier
	| run
	| statement

	| dlExpr '(' dlExpr ')'   
	| '(' dlExpr ')' dlExpr   
	| '(' dlExpr ')'

	| expr	// regular pvs expression
	;

entailEmptyInit
	: emptyInit '|-' dlExpr
	;
emptyInit
	: '(:' ':)'
	;
quantifier
	: dlForallFunction
	| dlExistsFunction
	;
dlForallFunction
	: dlforall parL bindingPart dlExpr parR
    ;
dlExistsFunction
	: dlexists parL bindingPart dlExpr parR
    ;
bindingPart //-- most of this is syntactic sugar, we print only bindingName in ddl
	: K_LAMBDA lambdaBindings ':'
	;

// @override
letExpression
    : K_LET letBinding (',' letBinding)* K_IN dlExpr
    | '(' letExpression ')'
    ;

// @override
letBinding
	: letBind (',' letBind)* '=' dlExpr
	;


run
	: allRunsFunction
	| someRunsFunction
	;
statement
	: parallelDiffFunction
	| parallelAssignFunction
	| constFunction
	| valueFunction
	| statementsL1
	;
statementsL1
	: testFunction
	| unionFunction
	| seqFunction
	| anyFunction
	| diffFunction
	| assignFunction
	| identifier // this is used to handle local bindings
	;

allRunsFunction
	: allRunsFunctionName parL (statement | starFunction) comma invariant parR
	;
allRunsFunctionName
	: 'ALLRUNS'
	;
invariant
	: dlExpr
	;

someRunsFunction
	: someRunsFunctionName parL (statement | starFunction) comma invariant parR
	;
someRunsFunctionName
	: 'SOMERUNS'
	;

constFunction
	: constFunctionName parL number parR
	;
constFunctionName
	: 'cnst'
	;

anyFunction
	: anyFunctionName parL identifier comma dlrandom parR
	;
anyFunctionName
	: 'ANY'
	;

valueFunction
	: valueFunctionName parL identifier parR
	;
valueFunctionName
	: 'val'
	;

starFunction
	: starFunctionName parL statement parR
	;
starFunctionName
	: 'STAR'
	;


parallelDiffFunction
	: diffFunctionName parL parCL diffAssignment (comma diffAssignment )+ parCR (commaInv diffInvariant)? parR 
	;
diffFunction
	: diffFunctionName parL parCL diffAssignment parCR (commaInv diffInvariant)? parR 
	;
diffFunctionName
	: 'DIFF'
	;
diffAssignment
	: parL identifier comma dlExpr parR
	;
diffInvariant
	: dltrue
	| invariant
	;
commaInv
	: ','
	;

parallelAssignFunction
	: assignFunctionName parL parCL assign (',' assign)+ parCR parR
	;
assignFunction
	: assignFunctionName parL parCL assign parCR parR
	;
assign
	: parL identifier comma dlExpr parR
	;
assignFunctionName
	: 'ASSIGN'
	;

// dlStatement
// 	: dlAssignmentElem
// 	| seqFunction
// 	| dlExpression
// 	| testFunction
// 	;
	
testFunction
	: testFunctionName parL dlExpr parR
	;
testFunctionName
	: 'TEST'
	;
unionFunction
	: unionFunctionName parL dlExpr comma dlExpr parR
	;
unionFunctionName
	: 'UNION'
	;
	
seqFunction
	: seqFunctionName parL statement comma statement parR
	;
seqFunctionName
	: 'SEQ'
	;

parL: '(';
parR: ')';
parL1: '(';
parR1: ')';
parL2: '(';
parR2: ')';
parCL: '(:';
parCR: ':)';
comma: ',';

dlrandom: 'DLRANDOM';
dltrue: 'DLTRUE';
dlforall: 'DLFORALL';
dlexists: 'DLEXISTS';
