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

parse
    : program* EOF
    ;

program
    : discreteProgramLoop
    | dlExpression
    | identifier
    ;

dlExpression
    : term
    | booleanExpression
    | assignmentExpression
    | arithmeticExpression
    | bindingExpression
    | starExpression
    | allRunsExpression
    | someRunsExpression
    ;

assignmentExpression
    : simpleAssignment
    | parallelAssignment
    | sequentialAssignment
    | anyAssignment
    ;

parallelAssignment
    : simpleAssignment (',' simpleAssignment)+
    ;

sequentialAssignment
    : simpleAssignment (';' simpleAssignment)+
    ;

simpleAssignment
    : identifier ':=' dlExpression
    ;

anyAssignment
    : identifier ':=' 'ANY' '(' bindDeclaration (',' bindDeclaration)* ('|' booleanExpression)? ')'
    ;

arithmeticExpression
    :<assoc=right> arithmeticExpression '^' arithmeticExpression
    |<assoc=left> arithmeticExpression ('*' | '/') arithmeticExpression
    | timesExpression
    |<assoc=left> arithmeticExpression ('+' | '-') arithmeticExpression
    | '(' arithmeticExpression ')'
    | term
    | NUMBER
    ;

timesExpression
    :<assoc=left> NUMBER term
    ;

booleanExpression
    : booleanExpression O_AND booleanExpression
    | booleanExpression O_OR booleanExpression
    | O_NOT booleanExpression
    | booleanExpression (O_GE | O_LE | '>' | '<' | O_EQUAL) booleanExpression
    | term
    | NUMBER
    | '(' booleanExpression ')'
    ;

bindingExpression
    : 'FORALL' '(' bindDeclaration (',' bindDeclaration)* ')' ':' dlExpression
    ;

bindDeclaration
    : identifier ':' typeName
    ;

starExpression
    : '(' dlExpression ')' '*'
    ;

allRunsExpression
    : '[' program ']' booleanExpression
    ;

someRunsExpression
    : '<' program '>' booleanExpression
    ;

term
    : NUMBER
    | identifier ('(' term ')')*
    ;

typeName
    : identifier
    ;

discreteProgramLoop
    : booleanExpression '-->' '[' dlExpression ']' '(' booleanExpression ')'
    ;

identifier
    : ID
    ;