

import { ProofCommandDescriptor } from "./serverInterface";


// the following list of commands obtained from pvs-server with collect-strategy-names
// the descriptions are based on those illustrated in the pvs prover guide
// some commands are commented out in this list are they were not deemed useful for the typical PVS user
export const PROOF_COMMANDS: { [key:string]: ProofCommandDescriptor } = {
    // 	"abs-simp": {
    // 		description: ``
    // },
        // "abstract": { description:""},
        // "abstract-and-mc": { description:""},
    // 	"add-formulas": { 
    // 		description: `
    // add-formulas: adds relational formulas.
    
    // The new formula is labeled as LABEL, if specified. 
    // If FNUM2 is nil, adds FNUM to itself.
    // If HIDE? is t, the original formulas are hidden.
    // TCCs generated during the execution of the command are discharged with the proof command tcc-step. 
    // At the end, the strategy tries to discharge the current branch using the proof command auto-step.
    
    // Syntax: add-formulas FNUM1 FNUM2?
    // `
    // },
        // "all-implicit-typepreds": { description:""},
    
    "all-typepreds": {
        description: `Make all type expressions explicit.`,
        syntax: `all-typepreds FNUMS*`,
        effect: `Provides type predicates information for all subexpressions FNUMS,
meaning those that are not already dealt with by the prover. A
typepred is deemed useful if it has an expandable definition or is
propositional, since these will have been treated as uninterpreted in the
prover until they are exposed.`,
        examples: {
            "all-typepreds": `Show type predicates for all sequent formulas in the current proof node`,
            "all-typepreds 1": `Show types predicates for sequent formula 1`,
            "all-typepreds -1 2": `Show type predicates for sequent formulas -1 and 2`,
            "all-typepreds +": `Show type predicates for all succedent formulas`
        }
    },
    "apply-eta": { 
        description: `Apply the eta form of extensionality.`,
        syntax: `apply-eta TERM TYPE?`,
        effect: `This rule is an extension of the extensionality rule. Given a succedent in
the form of an equation l = r, where the type of l and r has a corresponding
extensionality axiom scheme, apply-extensionality will generate a new
succedent that is the result of using replace-extensionality on l and r.`,
        examples: {}
    },
    // 	"apply-ext": { description:`
    // apply-ext: use extensionality to prove equality
    
    // This rule is an extension of the extensionality rule. Given a succedent in
    // the form of an equation l = r, where the type of l and r has a corresponding
    // extensionality axiom scheme, apply-extensionality will generate a new
    // succedent that is the result of using replace-extensionality on l and r.
    
    // If the keep? flag is set to T, the antecedent equality introduced by the
    // apply-extensionality command is retained in the resulting goal sequent.
    
    // Syntax: apply-ext FNUMS* keep?
    
    // Examples:
    // 	apply-ext
    // 	apply-ext 1 3
    // 	apply-ext keep
    
    // `},
    "apply-extensionality": {
        description: `Use extensionality to prove equality.`,
        syntax: `apply-extensionality FNUMS* keep?`,
        effect: `this rule is an extension of the extensionality rule. Given a succedent in
the form of an equation l = r, where the type of l and r has a corresponding
extensionality axiom scheme, apply-extensionality will generate a new
succedent that is the result of using replace-extensionality on l and r.\n
If the keep flag is specified, the antecedent equality introduced by the
apply-extensionality command is retained in the resulting goal sequent.`,
        examples: {
            "apply-extensionality": ``,
            "apply-extensionality 1 3": ``,
            "apply-extensionality keep:": ``
        }
    },
    // 	"apply-lemma": { description: `
    // apply-lemma: try applying a lemma with explicit instantiations using
    // an implicit variable list.  In PVS, lemma variables appear in alphabetical
    // order when introduced by the LEMMA rule.  That order needs to be observed
    // when entering EXPR-SPECS.
    // ` 
    // },
    // 	"apply-rewrite": {
    // 		description:`
    // apply-rewrite:  Try applying a (purely equational) rewrite rule with explicit
    // instantiations using an implicit variable list.  In PVS, lemma variables
    // appear in alphabetical order when introduced by the LEMMA rule.  That
    // order needs to be observed when entering EXPR-SPECS
    // `
    // },
    "assert": { 
        description: `Simplify expressions using decision procedures.`,
        syntax: `assert FNUMS* lr?`,
        effect: `This rule is a combination of four other rules (record, simplify, beta, 
do-rewrite). The use of decision procedures for equalities and linear inequalities
is perhaps the most signicant and pervasive part of PVS. These procedures
are invoked to prove trivial theorems, to simplify complex expressions (particularly
definitions), and even to perform matching. These decision procedures,
originally due to Shostak, employ congruence closure for equality reasoning,
and they also perform linear arithmetic reasoning over the natural numbers
and reals.\n
When the rewrite flag lr is specified, only the right-hand side of any equality formula
is simplied.\n`,
        examples: {
            "assert": ``,
            "assert -1 lr": ``
        }
    },
    // "auto-rewrite": { description:""},
    // "auto-rewrite!": { description:""},
    // "auto-rewrite!!": { description:""},
    // "auto-rewrite-defs": { description:""},
    // "auto-rewrite-explicit": { description:""},
    // "auto-rewrite-expr": { description:""},
    // "auto-rewrite-theories": { description:""},
    // "auto-rewrite-theory": { description:""},
    // "auto-rewrite-theory-with-importings": { description:""},
    //         "bash": { description: `
    // bash: executes assert, bddsimp, inst?, skolem-typepred, flatten, and lift-if.
    // `},
    "bddsimp": {
        description: `Propositional simplification.`,
        syntax: `bddsimp FNUMS*`,
        effect: `Repeatedly applies the propositional rules to all the formulas in the sequent to generate zero or more subgoals.`,
        examples: {
            "bddsimp": ``,
            "bddsimp +": ``
        }
    },
    "beta": { 
        description: `Applies beta reduction.`,
        syntax: `beta FNUMS*`,
        effect: `Rewrites redex expressions to their reduced form. 
Common redex expressions and their reduced forms are:
- LAMBDA (x1 .. xn: e)(t1 .. tn) reduces to e[t1=x1 .. tn=xn]. Note that LET and
WHERE expressions are syntactic sugar for redexes and will also be beta-reduced
by beta.
- ai((# a1:=t1 .. an:=tn#)) reduces to ai.
- f WITH [..., (i) := e, ...])(j) reduces to e if it can be shown that i = j, and the assignments following (i) := e do not affect f(j).
- CASES c(t1 .. tn) OF ..., c(x1 .. xn) : e, ... ENDCASES reduces to e[t1=x1 .. tn=xn], where c is a constructor for some datatype.`,
        examples: {
            "beta": ``,
            "beta +": ``
        }
    },
    "both-sides": { 
        description: `Applies an operand uniformly over a conjunction of inequalities`,
        syntax: `both-sides OP TERM FNUM?`,
        effect: `Given a sequent formula in the form 
e1 ≤ e2 AND e2 ≤ e3 AND e3 ≤ e4, both-sides replaces the chain with 
e1 OP TERM ≤ e2 OP TERM AND e2 OP TERM ≤ e3 OP TERM AND e3 OP TERM ≤ e4 OP TERM.
If the equivalence between this chain and the previous one doesn't simplify to 
TRUE using assert and do-rewrite with respect to the prelude theory real props, 
then a proof obligation is generated.\n
The optional argument FNUM can be used to specify the sequent formula.`,
        examples: {
            'both-sides "*" "2"': ``
        }
    },
    "both-sides-f": { 
        description: `Applies a function to both sides of the relational expression`,
        syntax: `both-sides FNUM F :postfix? t`,
        effect: `Given a sequent formula FNUM in the form e1 = e2,
both-sides-f replaces the formula with F(e1) = F(e2).\n
TCCs generated during the execution of the command are
discharged with the proof command tcc-step. At the end, the strategy
tries to discharge the current branch using the proof command
auto-step.\n
The optional setting :postfix? t can be used to add the function as a postfix string to the expressions.`,
        examples: {
            'both-sides-f 1 "abs"': ``, 
            'both-sides-f 1 "^2" :postfix? t': ``
        }
    },
//         "cancel": { description: `
// cancel: cancel terms from both sides of relational formulas involving arithmetic expressions.\n
// Syntax: cancel FNUMS*
// `},
//         "cancel-add": { description:""},
//         "cancel-add!": { description:""},
//         "cancel-by": { description:""},
//         "cancel-formula": { description:""},
//         "cancel-terms": { description:""},
//         "canon-tms": { description:""},
    "case": {
        description: `Case analysis on a series of formulas.`,
        syntax: `case FORMULAS+`,
        effect: `Splits according to the truth or falsity of FORMULAS.
Given a sequent A ⊢ B, CASE a b c generates four subgoals:\n
a, b, c, A ⊢ B\n
a, b, A ⊢ c, B\n
a, A ⊢ b, B\n
A ⊢ a, B.`,
        examples: {}
    },
    "case*": {
        description: `Full case analysis on formulas.`,
        syntax: `case FORMULAS+`,
        effect: `Splits along every branch, according to the truth or falsity of FORMULAS.`,
        examples: {}
    },
    // "case-if": { description:""},
    // "case-if*": { description:""},
    // "case-old-lift-if": { description:""},
    // "case-replace": { description:""},
    // "checkpoint": { description:""},
    // "claim": { description:""},
    "comment": { 
        description: `Attaches a comment to the sequent.`,
        syntax: `comment LABEL`,
        effect: `COMMENT is attached to the sequent. Formulas remain unchanged.`,
        examples: {}
    },
    // "commentf": { description:""},
//         "contra-eqs": { description: `
// contra-eqs: simple equality reasoning\n
// `},
    "copy": {
        description: `Copy a formula`,
        syntax: `copy FNUM`,
        effect: `Inserts a copy of sequent formula FNUM. If the formula is an antecedent, 
then the copy becomes the first antecedent. If the formula is a succedent, 
then the copy becomes the first succedent.`,
        examples: {}
    },
    "copy*": { 
        description: `Copy a series of formulas`,
        syntax: `copy* FNUMS+`,
        effect: `Iterative version of copy. Inserts a copy of sequent formulas FNUMS into the sequent.`,
        examples: {}
    },
    "cross-add": {
        description: `Apply cross addition to relational formulas.`,
        syntax: `cross-add FNUMS*`,
        effect: `Add to both sides of a formula the respective subtrahend of each side and then simplify.
Applies cross addition recursively until all outermost subtraction operators are gone.`,
        examples: {}
    },
    "cross-mult": {
        description: `Apply cross multiplication to relational expressions.`,
        syntax: `cross-mul FNUMS*`,
        effect: `Multiply both sides of a formula by the respective divisors of each side and
then simplify.  Checks for negative real divisors and invokes suitable
lemmas as needed.  Applies cross multiplication recursively until all
outermost division operators are gone.`,
        examples: {}
    },
    "cut": { 
        description: `Case analysis on a series of formulas. Equivalent to the 'case' command.`,
        syntax: `cut FORMULAS+`,
        effect: `Splits along every branch, according to the truth or falsity of FORMULAS.
TCCs generated during the execution of the command are discharged with the proof command TCC-STEP.`,
        examples: {}
    },
    "decide": {
        description: `Invokes the decision procedure, without simplification.`,
        syntax: `decide FNUMS*`,
        effect: ``,
        examples: {}
    },
    "decompose-equality": {
        description: `Decomposes an equality or disequality to the component equalities.`,
        syntax: ``,
        effect: `If it is an equality in the consequents or a disequality in the antecedents then this simply
invokes apply-extensionality. Otherwise it decomposes the (dis)equality into its component equalities.
This command only works for equalities between functions, records, or tuples.`,
        examples: {}
    },
    "delabel": {
        description: `Deletes a labelled formula from the current sequent.`,
        syntax: `delabel LABEL`,
        effect: `Deletes a labelled formula from the current sequent. 
If hide? is t, LABEL becomes a hidden sequent formula.
If hidden? is t, LABEL is removed from both visible and hidden sequent formulas.`,
        examples: {}
    },
    "delete": { 
        description: `Delete formulas from a given sequent`,
        syntax: `delete FNUMS*`,
        effect: `Returns the subgoal that is the result of deleting all of the sequent formulas
in the current goal that are indicated by FNUMS. If there are no formulas in the
sequent corresponding to those indicated in FNUMS, then the command has no effect.`,
        examples: {}
    },
    "demod-lin": {
        description: `Partial linear demodulator derivation and application.`,
        syntax: `demod-lin FNUMS*`,
        effect: ``,
        examples: {}
    },
    "demod-num": {
        description: `Numerical demodulation,`,
        syntax: `demod-num FNUMS*`,
        effect: ``,
        examples: {}
    },
    "detuple-boundvars": { 
        description: `Distributes tuple and record quantication.`,
        syntax: ``,
        effect: `A top-level formula of the form FORALL (x: [S1, S2, S3]): g(x) 
is replaced by FORALL (x1: S1), (x2: S2), (x3: S3): g(x1, x2, x3). 
Similarly, a top-level formula or FORALL (x: [# s : S, t : T #]) : g(x) 
is replaced by FORALL (x1: S), (x2: T)): g((# s := x1, t := x2 #)). This decomposition of
tuple and record quantication is needed, for example, to carry out an induction
over one of the components. Tuple quantication can be introduced when instantiating
parameterized theories such as the function theory in the prelude`,
        examples: {}
    },
    "discriminate": {
        description: `Labels formulas generated by a proof step.`,
        syntax: `discriminate STEP LABEL strict?`,
        effect: `Labels formulas generated by STEP as LABEL(s).
When strict? is set to t, all formulas that are considered new by PVS are also labeled.`,
        examples: {}
    },
    "distrib": {
        description: `Distribute multiplication operators`,
        syntax: `distrib fnums+ SIDE* TERM-NUMS*`,
        effect: `Distribute multiplication operators over factors having the form
of additive subexpressions.  Apply this action to the top-level additive
terms given by TERM-NUMS for the expression found on SIDE of each
relational formula in FNUMS.`,
        examples: {}
    },
    "distrib!": {
        description: `Distribute multiplication operands`,
        syntax: `distrib! EXPR-LOC`,
        effect: `Distribute multiplication operators over factors having the
form of additive subexpressions. Apply this action to the multiplicative
expression found at EXPR-LOC.`,
        examples: {}
    },
    "div-by": { 
        description: `Divide both sides of a relational formula`,
        syntax: `div-by FNUMS+ TERM (sign [+ | - | *])`,
        effect: `Divide both sides of a relational formula by the factor TERM.
        If TERM is known to be positive or negative, use + or - as the SIGN
        argument. Otherwise, use *, which introduces a conditional expression
        to handle the two cases.`,
        examples: {}
    },
    "do-rewrite": {
        description: `Uses decision procedures to rewrite sequent formulas.`,
        syntax: `de-rewrite FNUMS*`,
        optionals: [
            `rewrite-flag`,
            `flush?`,
            `linear?`,
            `cases-rewrite?`
        ],
        effect: `Uses decision procedures to rewrite the formulas in FNUMS.
If rewrite-flag is RL (LR) then only lhs (rhs) is simplified.
If flush? is T then the current asserted facts are deleted for efficiency.
If linear? is T, then multiplication and division are uninterpreted.
If cases-rewrite? is T, then the selections and else parts of a CASES expression are simplified.`,
        examples: {
            '(do-rewrite :rewrite-flag RL :linear? T)': ``
        }
    },
    "elim-unary": {
        description: `Eliminate unary minus functions in additive expressions.`,
        syntax: `elim-unary FNUM`,
        optionals: [
            "side"
        ],
        effect: `Convert expressions of the form x +/- -y to the form x -/+ y. Also convert -x + y to y - x.`,
        examples: {}
    },
    "elim-unary!": {
        description: `Eliminate unary minus functions in additive expressions.`,
        syntax: `elim-unary EXPR-LOC`,
        effect: `Convert expressions of the form x +/- -y to the form x -/+ y. Also convert -x + y to y - x.`,
        examples: {}
    },
    "eta": { 
        description: `Introduces Eta version of extensionality axiom for given TYPE`,
        syntax: `eta TYPE`,
        effect: ``,
        examples: {}
    },
    "eval": {
        description: `Evaluate an expression.`,
        syntax: `eval EXPR`,
        optionals: [
            "safe?",
            "quiet?",
            "timing?"
        ],
        effect: `Prints the evaluation of expression EXPR. If SAFE? is t and EXPR 
        generates TCCs, the expression is not evaluated. This strategy evaluates
        semantic attachments. Therefore, it may not terminate properly. When QUIET? 
        is t, the strategy fails silently.`,
        examples: {}
    },
    "eval-expr": {
        description: "Defines the ground value of an uninterpreted expression.",
        syntax: ``,
        optionals: [
            "safe?",
            "auto?",
            "quiet?",
            "timing?"
        ],
        effect: `Adds the hypothesis expr=eval(EXPR) to the current goal,
where eval(EXPR) is the ground evaluation of EXPR. If SAFE? is t and
EXPR generates TCCs, the expression is not evaluated. Otherwise, TCCs
are added as subgoals and the expression is evaluated. If AUTO? is t,
TCCs are ground evaluated. The strategy is sound in the sense that
user-defined semantic attachments are not evaluated. However, if SAFE?
is nil, the strategy may not terminate properly in the presence of
unproven TCCs. When QUIET? is t, the strategy fails silently. When
TIMING? is t, strategy prints timing information of the ground
evaluation.`,
        examples: {}
    },
    "eval-formula": {
        description: "",
        syntax: `eval-formula FNUM`,
        optionals: [
            "safe?",
            "quiet?",
            "timing?"
        ],
        effect: `Evaluates formula FNUM in Common Lisp and adds the
result to the antecedent of the current goal. If SAFE? is t and FNUM
generates TCCs, the expression is not evaluated. The strategy is safe
in the sense that user-defined semantic attachments are not
evaluated. However, if SAFE? is nil, the strategy may not terminate
properly in the presence of unproven TCCs.  When QUIET? is t, the
strategy fails silently. When TIMING? is t, strategy prints timing
information of the ground evaluation.`,
        examples: {}
    },
    "expand": {
        description: "Expand a name and simplify.",
        syntax: `expand NAME`,
        effect: ``,
        examples: {}
    },
    "expand*": {
        description: "Expand names and simplify.",
        syntax: `expand* NAMES+`,
        effect: ``,
        examples: {}
    },
    "expand-names": {
        description: "???",
        syntax: "???",
        effect: "???",
        examples: {}
    },
    "extensionality": {
        description: `Axiom scheme for functions, records, tuples and abstract datatypes.`,
        syntax: `extensionality TYPE`,
        effect: `The extensionality rule is similar to the lemma rule in that it introduces
an extensionality axiom for the given type as an antecedent formula. An extensionality
axiom can be generated corresponding to function, record, and tuple
types, and constructor subtypes of PVS abstract datatypes.`,
        examples: {
            'extensionality "[nat, nat -> nat]"': ``
        }
    },
    "factor": {
        description: `Extract common multiplicative factors from the additive terms.`,
        syntax: `factor FNUMS+`,
        optionals: [
            "side",
            "term-nums",
            "id?"
        ],
        effect: `Extract common multiplicative factors from the additive terms
given by TERM-NUMS for the expression found on SIDE (L or R) of each
relational formula in FNUMS, then rearrange.  ID? = T indicates the factor
made of summed terms should be embedded in a call to the identity function
to prevent later distribution.`,
        examples: {}
    },
    "factor!": {
        description: `Extract common multiplicative factors from the additive terms.`,
        syntax: `factor! EXPR-LOC`,
        optionals: [
            "term-nums",
            "id?"
        ],
        effect: ` Extract common multiplicative factors from the additive terms
given by TERM-NUMS for the expressions found at EXPR-LOC, then rearrange.
ID? = T indicates the factor made of summed terms should be embedded in
a call to the identity function to prevent later distribution`,
        examples: {}
    },
    "fert-tsos": {
        description: `???`,
        syntax: `???`,
        effect: `???`,
        examples: {}
    },
    "field": {
        description: `Removes divisions`,
        syntax: `field`,
        effect: `Removes divisions and apply simplification heuristics to the relational
formula on real numbers FNUM. It autorewrites with THEORIES when possible. If CANCEL?
is t, then it tries to cancel common terms once the expression is free of divisions.
TCCs generated during the execution of the command are discharged with the proof command TCC-STEP`,
        optionals: [
            "FNUM",
            "theories",
            "cancel?"
        ],
        examples: {}
    },
    // "field-about": {
    //     description: ` Prints Field's about information`,
    //     syntax: ``,
    //     effect: `???`,
    //     examples: {}
    // },
    "flatten": { 
        description: `Disjunctive simplification.`,
        syntax: `flatten FNUMS*`,
        effect: `The flatten rule yields a subgoal where the indicated formulas in the current
goal are disjunctively simplied.`,
        examples: {
            "flatten": ``,
            "flatten 1 2": ``
        }
    },
    "flatten-disjunct": {
        description: "Controlled disjunctive simplification.",
        syntax: ``,
        effect: ``,
        examples: {}
    },
    "flip-ineq": {
        description: `Move formula from antecedent to succedent and vice-versa.`,
        syntax: ``,
        effect: ` Negate the inequality formulas and move the resulting formulas
by exchanging between antecedents and consequents.  Conjunctions and
disjunctions of inequalities are also accepted, causing each conjunct
or disjunct in the form of an inequality to be negated and moved.
If HIDE? is set to NIL, the original formulas are left intact.`,
        examples: {}
    },
    "gen-ex-cad": {
        description: `Generic cylindrical algebraic decomposition via QEPCAD-B.`,
        syntax: `gen-ex-cad FNUMS*`,
        effect: ``,
        examples: {}
    },
    "generalize": {
        description: `Generalizes term by universal quantication.`,
        syntax: ``,
        effect: `If the sequent is of the form a1(t), a2(t) ├─ c1(t), c2(t), then applying
the generalize term t with variable x yields a sequent of the form FORALL x: (a1(x) AND a2(x)) IMPLIES (c1(x) OR c2(x)).`,
        examples: {}
    },
    "generalize-skolem-constants": {
        description: `Generalize skolem constants.`,
        syntax: ``,
        effect: `Applies universal generalization to the Skolem constants that occur in the
given FNUMS. Such a step is useful in rearranging quantiers by introducing
skolem constants and generalizing them over selected formulas.`,
        examples: {}
    },
    "grind": {
        description: `Install rewrites and repeatedly simplify.`,
        syntax: ``,
        effect: ``,
        examples: {}
    },
    "grind-reals": {
        description: `Apply grind with "real_props".`,
        syntax: ``,
        effect: `This strategy supports the same
options as grind. Additionally, grind-reals blocks distribution laws in main level
expressions in the list of formulas DONTDISTRIB and protects formulas in PROTECT.`,
        examples: {}
    },
    "grind-with-ext": {
        description: `Like GRIND, but calls REDUCE-EXT, which also uses APPLY-EXTENSIONALITY.  See GRIND for an explanation of the arguments.`,
        syntax: ``,
        effect: ``,
        examples: {}
    },
    "grind-with-lemmas": {
        description: `Does a combination of (lemma) and (grind); if lazy-match? is t, postpones instantiations to follow a first round of simplification.`,
        syntax: ``,
        effect: ``,
        examples: {}
    },
    "ground": {
        description: `Propositional simplification followed by the use of decision procedures.`,
        syntax: ``,
        effect: ``,
        examples: {}
    },
    "ground-eval": {
        description: `Ground evaluation of expression EXPR.`,
        syntax: ``,
        effect: ``,
        examples: {}
    },
    "group": { 
        description: `Try associatively regrouping three terms toward SIDE (L or R)
        and replacing.  Set INFIX? to nil for prefix applications.  Associativity
        proof for operator will be tried automatically.`,
        syntax: ``,
        effect: ``,
        examples: {}
    },
    "group!": {
        description: `Try associatively regrouping the three subexpressions of the
        function applications found at EXPR-LOC toward SIDE (L or R).  Associativity
        proof for operator will be tried automatically.`,
        syntax: ``,
        effect: ``,
        examples: {}
    },
    "has-sign": {
        description: `Try claiming that a TERM has the designated SIGN (relationship to 0).`,
        syntax: ``,
        effect: `Symbols for SIGN are (+ - 0 0+ 0- +-), which have meanings positive,
negative, zero, nonnegative, nonpositive, and nonzero.  Proof of the
justification step can be tried or deferred.  Use TRY-JUST to supply
a step for the justification proof or T for the default rule (GRIND).`,
        examples: {}
    },
    // "help": { description:""},
    "hide": { 
        description: `Hide formulas.`,
        syntax: ``,
        effect: `Hides sequent formulas that are indicated by FNUMS. Hidden formulas are saved,
so that they can be restored to a descendant of the current sequent by the reveal rule.`,
        examples: {
            '(hide -2)': ``
        }
    },
    // "hide-all-but": {
    // 	description: `Hide Selected Formulas: this is a variant of the hide rule that hides all the formulas indicated by
    // 	FNUMS except those indicated by keep-FNUMS. As with hide, hidden sequent
    // 	formulas are saved and can be restored to a descendant of the current sequent
    // 	by the reveal rule.`
    // },
    "iff": {
        description: `Convert boolean equality to equivalence.`,
        syntax: `iff FNUMS*`,
        effect: `Yields a subgoal where any boolean equalities of the form A = B, among the
formulas in the current sequent that are indicated by FNUMS are converted to A ⇔ B.`, 
        examples: {
            'iff (4  2  -1)': ``
        }
    },
    "induct": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "induct-and-rewrite": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "induct-and-rewrite!": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "induct-and-simplify": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "inst": {
        description: `Instante existential quantiers`,
        effect: `As the sequent calculus rules indicate, the universally quantied formulas in
        the antecedent and the existentially quantied formulas in the consequent are
        reduced by instantiating the quantied variables with the terms that are being
        existentially generalized in the proof. In an application of the instantiate
        rule, FNUM is used to select the suitable quantied formula that is either an
        antecedent formula of the form FORALL (x1 ... xn:A) or a succedent formula of the
        form EXISTS (x1 .. xn:A). The argument exprs provides the list of n terms t1..tn
        so that the chosen quantied formula is replaced by A[t1=x1 .. tn=xn] in the
        generated subgoal. Note that each term ti is typechecked to be of the type of xi, and
        this typechecking could generate additional goals corresponding to the type
        correctness conditions.`,
        syntax: `inst FNUM EXPR`,
        examples: {
            '(inst 1 ("x + 3" "y - z"))': ``
        }
    },
    "inst!": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "inst*": {
        description: ``,
        syntax: ``,
        effect: ``,
        examples: {}
    },
    "inst-cp": { 
        description: `Copy and instantiate existentially quantified formula`,
        syntax: ``,
        effect: ``,
        examples: {}
    },
    "inst?": {
        description: `Automatic instantiation of existentially quantified formula`,
        syntax: ``,
        effect: ``,
        examples: {}
    },
    "install-rewrites": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "instantiate": {
        description: `Instantiate existentially quantified formula`,
        syntax: ``,
        effect: ``,
        examples: {}
    },
    "instantiate-one": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "insteep": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "insteep*": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "int-dom-zpb": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "invoke": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "isolate": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "isolate-mult": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "isolate-replace": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "just-install-proof": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "label": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "lazy-grind": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "lemma": { 
        description: `Introduce a lemma and automatically instantiate the lemma`,
        syntax: ``,
        effect: ``,
        examples: {}
    },
    "let": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "let-name-replace": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "lift-if": {
        description: `Lift embedded IF connectives.`,
        effect: `This command lifts the leftmost-innermost contiguous IF or CASES branching structure out to the top level.`,
        syntax: `lift-if FNUMS*`,
        examples: {
            'lift-if': ``,
            'lift-if 1': ``,
            'lift-if (-1 2 3)': ``,
            'lift-if +': ``
        }
    },
    "lisp": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "mapstep": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "mapstep@": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "match": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "measure-induct+": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "measure-induct-and-simplify": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    // "merge-FNUMS": {
    // 	description: "Combine Sequent Formulas" 
    // },
    "model-check": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "move-terms": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "move-to-front": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "mult-by": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "mult-cases": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "mult-eq": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "mult-extract": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "mult-extract!": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "mult-ineq": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "musimp": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "name": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "name-case-replace": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "name-distrib": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "name-extract": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "name-induct-and-rewrite": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "name-label": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "name-label*": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "name-mult": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "name-mult!": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "name-replace": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "name-replace*": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "neg-formula": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "op-ident": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "op-ident!": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "open-ex-inf-cad": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "open-frag-ex-inf-cad": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "permute-mult": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "permute-mult!": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "permute-terms": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "permute-terms!": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "postpone": {
        description: `Postpone current goal`,
        effect: `Marks the current goal as pending to be proved and shifts the focus to the next remaining goal.`,
        syntax: `postpone`,
        examples: {
            'postpone': ``
        }
    },
    "presburger": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "presburger-to-ws1s": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "printf": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "prop": { 
        description: `Propositional simplification.`,
        effect: `Carries out propositional simplication. This command is effective on small formulas. For larger formulas, please use bddsimp.`,
        syntax: `prop`,
        examples: {}
    },
    // "propax": { description: `It is important to note that the propax step is automatically applied to every
    // sequent that is ever generated in a proof, so that there is never any need to
    // actively invoke it. It is simply included here for the sake of completeness.`},
    "protect": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "pvsio-about": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "query*": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "quit": {
        description: `Terminates the current proof session.`,
        effect: ``,
        syntax: ``,
        examples: {}
    },
    "quote": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "rahd": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "rahd-simp": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "rahd-waterfall": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "random-test": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "rcr-ineqs": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "rcr-svars": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "real-props": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "recip-mult": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "recip-mult!": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "record": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "redlet": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "redlet*": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "reduce": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "reduce-with-ext": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "relabel": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "repeat": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "repeat*": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "replace": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "replace*": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "replace-eta": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "replace-ext": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "replace-extensionality": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "replaces": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "rerun": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "residue-class-ring-ineqs": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "reveal": {
        description: `Reveal hidden formulas.`,
        effect: `Reintroduces the hidden formulas numbered FNUMS. The list of hidden formulas can be viewed with show-hidden.
        The formulas revealed are not removed from the list of hidden formulas.`,
        syntax: ``,
        examples: {
            '(reveal -2)': ``
        }
    },
    "rewrite": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "rewrite*": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "rewrite-expr": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "rewrite-lemma": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    // "rewrite-msg-off": {
    // 	description: `In the default mode, automatic rewriting by commands such as assert and
    // 	do-rewrite generate a fairly verbose commentary. This can be entirely shut off
    // 	by the rewrite-msg-off command. Behaves like a skip otherwise.`
    // },
    // "rewrite-msg-on": {
    // 	description: `The rewriting commentary turned off by rewrite-msg-off can be restored by
    // 	this command. Behaves like a skip otherwise.`
    // },
    "rewrite-with-FNUM": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "rewrites": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "rotate++": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "rotate--": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "rule-induct": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "rule-induct-step": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "same-name": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    // "set-print-depth": {
    // 	description: `Sets the print depth for displaying formulas. Num must be a number. 0
    // 	means print the entire formula, any other number causes terms below the given
    // 	depth to be elided. Behaves like a skip otherwise.`
    // },
    // "set-print-length": { 
    // 	description: `Sets the print length for displaying formulas. Num must be a number. 0
    // 	means print the entire formula, any other number causes terms longet than the
    // 	given number to be elided. Behaves like a skip otherwise.`
    // },
    // "set-print-lines": {
    // 	description: `Sets the number of print lines for displaying formulas. Num must be a number.
    // 	0 means print the entire formula, any other number causes only the rst
    // 	num lines of each formula of the sequent to be displayed. Behaves like a skip
    // 	otherwise.`
    // },
    "set-right-margin": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "show-hidden": {
        description: `Show List of hidden formulas.`,
        effect: `Shows formulas hidden in the current sequent.`,
        syntax: ``,
        examples: {}
    },
    "show-parens": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "show-subst": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "simp-arith": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "simp-gls": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "simp-real-null": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "simp-tvs": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "simp-zrhs": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "simple-induct": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "simple-measure-induct": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "simplify": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "simplify-with-rewrites": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "skeep": { 
        description: ``,
        syntax: ``,
        effect: ``,
        examples: {}
    },
    "skeep*": { 
        description: ``,
        syntax: ``,
        effect: ``,
        examples: {}
    },
    // "skip": { 
    // 	description: `Has no effect on the proof. The primary utility of skip is in writing strategies
    // 	where a step is required to have no effect unless some condition holds. Typing
    // 	(skip) in response to a goal sequent returns the same proof state with a "No
    // 	change." message.`
    // },
    // "skip-msg": { 
    // 	description: `Has no effect on the proof but prints the given string. The main use of
    // 	skip-msg is in generating error messages from within strategies, typically as:
    // 	(if good?(input) ...(skip-msg "Bad input.")).`
    // },
    "skip-steps": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "sklisp": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "skodef": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "skodef*": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "skolem": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "skolem!": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "skolem-typepred": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "skoletin": { 
        description: ``,
        syntax: ``,
        effect: ``,
        examples: {}
    },
    "skoletin*": { 
        description: ``,
        syntax: ``,
        effect: ``,
        examples: {}
    },
    "skosimp": { 
        description: `Generate Skolem names then flatten`,
        syntax: ``,
        effect: ``,
        examples: {}
    },
    "skosimp*": { 
        description: `Repeatedly generate Skolem names then flatten`,
        syntax: ``,
        effect: ``,
        examples: {}
    },
    "smash": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "splash": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "split": {
        description: `Conjunctive splitting.`,
        effect: `Selects and splits a conjunctive formula in the current goal sequent based 
on the information given in FNUM. When the depth argument is given, the top-level conjuncts are only split to that
given depth.`,
        syntax: `split FNUMS* depth?`,
        examples: {
            'split': ``,
            'split +': ``,
            'split + :depth 2': ``
        }
    },
    "split-ineq": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "sq-simp": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "stop-rewrite": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "stop-rewrite-theory": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "sub-formulas": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "suffices": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "swap": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "swap!": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "swap-group": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "swap-group!": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "swap-rel": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "tccs-expression": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "tccs-formula": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "tccs-formula*": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "tccs-step": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "then": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "then*": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "then@": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "time": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "touch": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    // "trace": {
    // 	description: `Turns on the tracing of the proof commands named in names so that any
    // 	time any one of the named rules or strategies is used in a proof, the entry into
    // 	and exit out of such commands is traced. This makes it possible to check if the
    // 	command is being properly invoked and has the desired effect. Behaves like a
    // 	skip otherwise.`
    // },
    // "track-all-current-rewrites": { description: "" },
    // "track-rewrite": {
    // 	description: `Explains why the attempt to apply a rewrite rule named in names was not
    // 	applied. Other than setting up the names of the rewrite rules to be tracked during
    // 	simplication, track-rewrite behaves like a skip. It has no effect on the current
    // 	proof sequent and is not saved as part of the partial or completed proof.`
    // },
    "transform-both": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "triv-ideals": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "trust": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "trust!": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "try": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "try-branch": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "try-rewrites": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "typepred": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "typepred!": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "undo": {
        description: `Undo the last proof command.`,
        effect: `The prover steps back to the ancestor node of the current proof node.`,
        syntax: ``,
        examples: {}
    },
    "univ-sturm-ineqs": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "unlabel": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "unlabel*": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "unless": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "unless-label": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "unless-label@": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "unless@": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    // "untrace": {
    // 	description: `Turns off the tracing of proof commands named in names, as initiated by
    // 	(trace). Behaves like a skip otherwise.`
    // },
    // "untrack-rewrite": { 
    // 	description: `Disables the tracking of rewrite rules invoked by track-rewrite. When
    // 	untrack-rewrite is invoked with no arguments, then tracking is discontinued
    // 	for all currently tracked rewrite rules. Other than removing the given names
    // 	from list of rewrite rules to be tracked during simplication, untrack-rewrite
    // 	behaves like a skip. It has no effect on the current proof sequent and is not
    // 	saved as part of the partial or completed proof.`
    // },
    "unwind-protect": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "use": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "use*": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "use-with": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "when": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "when-label": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "when-label@": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "when@": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "with-focus-on": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "with-focus-on@": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "with-fresh-labels": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "with-fresh-labels@": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "with-fresh-names": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "with-fresh-names@": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "with-labels": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "with-tccs": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "wrap-formula": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "wrap-manip": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "ws1s": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "ws1s-simp": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "y2grind": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "y2simp": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "ygrind": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "yices": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "yices-with-rewrites": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "yices2": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "yices2-with-rewrites": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
}
};

export const CORE_PROOF_COMMANDS: { [key: string]: ProofCommandDescriptor } = {
    "all-typepreds": PROOF_COMMANDS["all-typepreds"],
    "apply-extensionality": PROOF_COMMANDS["apply-extensionality"],
    "assert": PROOF_COMMANDS["assert"],
    "bddsimp": PROOF_COMMANDS["bddsimp"],
    "beta": PROOF_COMMANDS["beta"],
    "both-sides": PROOF_COMMANDS["both-sides"],
    "case": PROOF_COMMANDS["case"],
    "comment": PROOF_COMMANDS["comment"],
    "expand": PROOF_COMMANDS["expand"],
    "flatten": PROOF_COMMANDS["flatten"],
    "grind": PROOF_COMMANDS["grind"],
    "grind-reals": PROOF_COMMANDS["grind-reals"],
    "ground": PROOF_COMMANDS["ground"],
    "iff": PROOF_COMMANDS["iff"],
    "inst?": PROOF_COMMANDS["inst?"],
    "lemma": PROOF_COMMANDS["lemma"],
    "lift-if": PROOF_COMMANDS["lift-if"],
    "prop": PROOF_COMMANDS["prop"],
    "rewrite": PROOF_COMMANDS["rewrite"],
    "skeep": PROOF_COMMANDS["skeep"],
    "skosimp*": PROOF_COMMANDS["skosimp*"],
    "split": PROOF_COMMANDS["split"],
    "use": PROOF_COMMANDS["use"]
};

export const PROOF_STRATEGIES: { [key:string]: ProofCommandDescriptor } = {
    "apply": {
        description: `In-line definition of user-defined strategies.`,
        syntax: `apply strategy COMMENT`,
        effect: `Applies a user-defined strategy as if it were a primitive proof rule, and prints COMMENT.`,
        examples: {
            'apply (then (skosimp*)(flatten)(inst?)) "Skolemizing, flattening, and instantiating"': ``
        }
    },
    "branch": {
        description: `In-line definition of proof strategies for multiple branches.`,
        syntax: `branch STEP TACTIC+`,
        effect: `Performs a branching STEP and then applies the i'th tactic to the i'th subgoal.
If the number of subgoals is larger than the number of tactics, then the n'th tactic is applied to the remaining subgoals.\n  
This command is typically used when step splits the proof into multiple branches
where a different strategy is required for each of the branches.`,
        examples: {}
    },
    "branch-back": { 
        description: `In-line definition of proof multiple proof tactics.`,
        syntax: `branch-back STEP TACTIC+`,
        effect: `Perform a branching STEP and then applies the i'th tactic to the i'th subgoal.
Automatic backtracking is performed for branches that fail to prove their goals.
That is, if a tactic for a branch fails to prove its goal, the proof state for that branch 
is rolled back to the point before the step was invoked.`,
        examples: {}
    },
    // "default-strategy": { description:""},
    "deftactic": { 
        description: `Defines a labelled proof tactic.`,
        syntax: `deftactic TACTIC_NAME TACTIC`,
        effect: `Defines a labelled proof tactic. The tactic is local to the current branch of the proof. 
TACTIC_NAME needs to be a valid identifier in PVS.`,
        examples: {
            '(deftactic foo (then (flatten) (assert) (grind)))': ``
        }
    },
    "else": { 
        description: `Try STEPS in sequence until the first one succeeds.`,
        syntax: `else STEPS+`,
        effect: ``,
        examples: {}
    },
    "else*": { 
        description: `Try STEPS in sequence until the first one succeeds.`,
        syntax: `else* STEPS+`,
        effect: ``,
        examples: {}
    },
    "equate": {
        description: `Try equating two expressions and replacing the LHS by the
        RHS in FNUMS.  Proof of the justification step can be tried or deferred.
        Use TRY-JUST to supply the rule for the justification proof or T for
        the default rule (GRIND).`,
        syntax: `equate lhs rhs`,
        effect: ``,
        optionals: [ "try-just", "FNUMS*" ],
        examples: {}
    },
    "extra-tcc-step": {
        description: "???",
        syntax: `???`,
        effect: `???`,
        examples: {}
    },
    // "extrategies-about": { 
    //     description: ``,
    //     syntax: ``,
    //     effect: ``,
    //     examples: {}
    // },
    "fail": { 
        description: `Propagate failure to the parent`,
        syntax: `fail`,
        effect: `A failure signal is propagated to the parent proof goal. If the parent goal is
not able to act on this signal, it further propagates the failure to its parent.
This rule, like skip, is mainly employed in constructing strategies where it is
used to control backtracking. Applying fail to the root sequent causes the
proof to be unsuccessfully terminated.`,
        examples: {}
    },
    "finalize": {
        description: `Either finishes the current goal with STEP or does nothing.`,
        syntax: `field STEP`,
        effect: ``,
        examples: {}
    },
    "for": { 
        description: ``,
        syntax: ``,
        effect: ``,
        examples: {}
    },
    "for-each": { 
        description: ``,
        syntax: ``,
        effect: ``,
        examples: {}
    },
    "for-each-rev": { 
        description: ``,
        syntax: ``,
        effect: ``,
        examples: {}
    },
    "for@": { 
        description: ``,
        syntax: ``,
        effect: ``,
        examples: {}
    },
    "forward-chain": { 
        description: ``,
        syntax: ``,
        effect: ``,
        examples: {}
    },
    "forward-chain*": { 
        description: ``,
        syntax: ``,
        effect: ``,
        examples: {}
    },
    "forward-chain-theory": { 
        description: ``,
        syntax: ``,
        effect: ``,
        examples: {}
    },
    "forward-chain@": { 
        description: ``,
        syntax: ``,
        effect: ``,
        examples: {}
    },
    "if": {
        description: ` Evaluates the condition (in Lisp) and if it evaluates to nil, step2 is applied, otherwise step1 is applied.`,
        syntax: ``,
        effect: ``,
        examples: {}
    },
    "if-label": {
        description: `Applies THEN-STEP if at least one formula in the sequent is labeled LABEL. Otherwise, applies ELSE-STEP.`,
        syntax: ``,
        effect: ``,
        examples: {}
    },
    "spread": { 
        description: `Define proof strategies for branching steps.`, 
        syntax: `spread STEP STEPLIST`,
        effect: `Performs a branching STEP and then applies the i'th element of STEPLIST to the i'th subgoal. 
This command is typically used when step splits the proof into multiple branches
where a different strategy is required for each of the branches.`,
        examples: {}
    },
    "spread!": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},
    "spread@": { 
description: ``,
syntax: ``,
effect: ``,
examples: {}
},

};
    
export const EVALUATOR_COMMANDS: { [key:string]: ProofCommandDescriptor } = {
    "RANDOM": {
        description: `Generate a random number.`,
        syntax: `RANDOM`,
        effect: ``,
        examples: {}
    }
};

