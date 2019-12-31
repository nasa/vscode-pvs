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
 
grammar PvsDDL;
import PvsLexer;

parseDDL
    : dlProgram* EOF
    ;

dlProgram
    : dlDiscreteProgramLoop
    | dlExpression
    | dlIdentifier
    ;

dlExpression
    : dlTerm
    | dlBooleanExpression
    | dlAssignmentExpression
    | dlArithmeticExpression
    | dlBindingExpression
    | dlStarExpression
    | dlAllRunsExpression
    | dlSomeRunsExpression
    ;

dlAssignmentExpression
    : dlSimpleAssignment
    | dlParallelAssignment
    | dlSequentialAssignment
    | dlAnyAssignment
    ;

dlParallelAssignment
    : dlSimpleAssignment (',' dlSimpleAssignment)+
    ;

dlSequentialAssignment
    : dlSimpleAssignment (';' dlSimpleAssignment)+
    ;

dlSimpleAssignment
    : dlIdentifier ':=' dlExpression
    ;

dlAnyAssignment
    : dlIdentifier ':=' 'ANY' '(' bindDeclaration (',' bindDeclaration)* ('|' dlBooleanExpression)? ')'
    ;

dlArithmeticExpression
    :<assoc=right> dlArithmeticExpression '^' dlArithmeticExpression
    |<assoc=left> dlArithmeticExpression ('*' | '/') dlArithmeticExpression
    | timesExpression
    |<assoc=left> dlArithmeticExpression ('+' | '-') dlArithmeticExpression
    | '(' dlArithmeticExpression ')'
    | dlTerm
    | NUMBER
    ;

timesExpression
    :<assoc=left> NUMBER dlTerm
    ;

dlBooleanExpression
    : dlBooleanExpression O_AND dlBooleanExpression
    | dlBooleanExpression O_OR dlBooleanExpression
    | O_NOT dlBooleanExpression
    | dlBooleanExpression (O_GE | O_LE | '>' | '<' | O_EQUAL) dlBooleanExpression
    | dlTerm
    | NUMBER
    | '(' dlBooleanExpression ')'
    ;

dlBindingExpression
    : 'FORALL' '(' bindDeclaration (',' bindDeclaration)* ')' ':' dlExpression
    ;

bindDeclaration
    : dlIdentifier ':' typeName
    ;

dlStarExpression
    : '(' dlExpression ')' '*'
    ;

dlAllRunsExpression
    : '[' dlProgram ']' dlBooleanExpression
    ;

dlSomeRunsExpression
    : '<' dlProgram '>' dlBooleanExpression
    ;

dlTerm
    : NUMBER
    | dlIdentifier ('(' dlTerm ')')*
    ;

typeName
    : dlIdentifier
    ;

dlDiscreteProgramLoop
    : dlBooleanExpression '|-' '[' dlExpression ']' '(' dlBooleanExpression ')'
    ;

dlIdentifier
    : ID
    ;

// keywords
K_PROGRAM: P R O G R A M;