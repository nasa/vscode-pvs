

import { ProofCommandDescriptor } from "./serverInterface";

export function printHelp (cmd: string, opt?: { optionals?: boolean, note?: boolean }): string {
    opt = opt || {};
    let help: string = cmd;
    const desc: ProofCommandDescriptor = PROOF_COMMANDS[cmd];
    if (desc) {
        if (desc.description) { help = desc.description; }
        if (desc.syntax) { help += `\n\nSyntax: (${desc.syntax})`; }
        if (desc.optionals && opt.optionals) {
            help += `\n\nOptionals:`;
            const keys: string[] = Object.keys(desc.optionals);
            for (let i = 0; i < keys.length; i++) {
                help += `\n\t${keys[i]}: ${desc.optionals[keys[i]]}`;
            } 
        }
        if (desc.note && opt.note) {
            help += `\n\nNote:`;
            help += `\n${desc.note}`; 
        };
    }
    return help;
}


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
        description: `Provide type predicate information that are not already dealt with by the prover`,
        syntax: `all-typepreds`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "apply-eta": { 
        description: `Apply the eta axiom scheme of extensionality`,
        syntax: `apply-eta TERM`,
    },
    "apply-ext": {
        description: `Try to prove equality via extensionality`,
        syntax: `apply-ext`,
        optionals: {
            "FNUMS": `Apply the command to the given sequent formula numbers. If FNUM is not given, then the first consequent that is an equation is used.`,
            "(keep? t)": `Keep the equality as an antecedent, rather than hiding it.`
        }
    },
    // "apply-extensionality": {
    //     description: `Try to prove equality via extensionality ** SUPERSEEDED BY apply-ext **`,
    //     syntax: `apply-extensionality`,
    //     optionals: {
    //         "FNUMS": `Apply the command to the given sequent formula numbers, e.g., (apply-extensionality "-1 2") applies the commmand to sequents -1 and 2. If FNUM is not given, then the first consequent that is an equation is used.`,
    //         "(keep? t)": `Keep the equality as an antecedent, rather than deleting it.`,
    //         "(hide t)": `Hide the equality formula to which extensionality is applied, rather than deleting the formula.` 
    //     }
    // },
    // "apply-lemma": { 
    //     description: `Apply a lemma`,
    //     syntax: ``,
    //     optionals: { 
    //         "EXPR-SPECS": ``
    //     }
    // },
    // "apply-rewrite": {
    //     description:`Apply a purely equational rewrite rule`,
    //     syntax: `apply-rewrite`,
    //     optionals: {
    //         "EXPR-SPECS": ``
    //     }
    // },
    "assert": { 
        description: `Simplify expressions using decision procedures`,
        syntax: `assert`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`,
            ":rewrite-flag rl": `Apply the command only to the right-hand side of the equality.`,
            ":rewrite-flag lr": `Apply the command only to the left-hand side of the equality.`
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
    // "bash": { description: `Executes assert, bddsimp, inst?, skolem-typepred, flatten, and lift-if.`},
    "bddsimp": {
        description: `Propositional simplification using binary decision diagrams`,
        syntax: `bddsimp FNUMS`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`,
        }
    },
    "beta": { 
        description: `Rewrites redex expressions to their reduced form`,
        note: `LET and WHERE expressions are syntactic sugar for redexes`,
        syntax: `beta`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "both-sides": { 
        description: `Apply an operand uniformly over a conjunction of inequalities.
            For example, given a sequent formula 'e1 ≤ e2 AND e2 ≤ e3 AND e3 ≤ e4', 
            both-sides replaces the chain with 'e1 OP TERM ≤ e2 OP TERM AND e2 OP TERM ≤ e3 OP TERM AND e3 OP TERM ≤ e4 OP TERM'.`,
        syntax: `both-sides OP TERM`
    },
    "both-sides-f": { 
        description: `Apply a function to both sides of a relational expression,
            For example, given a sequent formula FNUM in the form 'e1 = e2',
            both-sides-f replaces the formula with 'F(e1) = F(e2)'.`,
        syntax: `both-sides FNUM F`,
        optionals: {
            ':postfix? t': `Add the function as a postfix string.`
        }
    },
    // "cancel": { description: `Cancel terms from both sides of relational formulas involving arithmetic expressions.`},
    // "cancel-add": { description:""},
    // "cancel-add!": { description:""},
    // "cancel-by": { description:""},
    // "cancel-formula": { description:""},
    // "cancel-terms": { description:""},
    // "canon-tms": { description:""},
    "case": {
        description: `Case analysis based on given formulas`,
        syntax: `case FORMULAS`,
        note: `Sequents are split according to the truth or falsity of FORMULAS.
            For example, given a sequent 'A ⊢ B', the command 'case "a" "b" "c"' generates four subgoals:\n
            a, b, c, A ⊢ B\n
            a, b, A ⊢ c, B\n
            a, A ⊢ b, B\n
            A ⊢ a, B.`
    },
    "case*": {
        description: `Full case analysis based on given FORMULAS`,
        syntax: `case* FORMULAS`,
        note: `Splits along every branch, according to the truth or falsity of FORMULAS.`
    },
    // "case-if": { description:""},
    // "case-if*": { description:""},
    // "case-old-lift-if": { description:""},
    // "case-replace": { description:""},
    // "checkpoint": { description:""},
    // "claim": { description:""},
    // "commentf": { description:""},
    // "contra-eqs": { description: `simple equality reasoning`},
    "copy": {
        description: `Copy a sequent formula`,
        syntax: `copy FNUM`,
        note: `The command inserts a copy of sequent formula FNUM. 
            If the formula is an antecedent, then the copy becomes the first antecedent.
            If the formula is a succedent, then the copy becomes the first succedent.`
    },
    "copy*": { 
        description: `Copy a series of formulas`,
        syntax: `copy* FNUMS`,
        note: `This command is an iterative version of 'copy'.`,
    },
    "cross-add": {
        description: `Apply cross addition to relational sequent formulas`,
        syntax: `cross-add`,
        note: `Add to both sides of a formula the respective subtrahend of each side 
            and then simplify. The operation is applied recursively until all outermost 
            subtraction operators are gone.`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "cross-mult": {
        description: `Apply cross multiplication to relational expressions`,
        syntax: `cross-mul`,
        note: `Multiply both sides of a formula by the respective divisors of each side 
            and then simplify. Checks for negative real divisors and invokes suitable
            lemmas as needed.  Applies cross multiplication recursively until all
            outermost division operators are gone.`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }    
    },
    "cut": { 
        description: `Case analysis on a series of formulas. Equivalent to the 'case' command.`,
        syntax: `cut FORMULAS+`
    },
    "decide": {
        description: `Invoke the decision procedure, without simplification.`,
        syntax: `decide`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "decompose-equality": {
        description: `Decompose an expression into a series of equalities`,
        syntax: `decompose-equality`,
        note: `This command only works for equalities between functions, records, or tuples.`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "delabel": {
        description: `Delete a labelled sequent formula`,
        syntax: `delabel LABEL`,
        optionals: {
            "(hide? t)": `Sequent formula LABEL becomes a hidden sequent formula.`,
            "(hidden? t)": `Sequent formula LABEL is also removed from hidden formulas.`
        }
    },
    "delete": { 
        description: `Delete sequent formulas`,
        syntax: `delete FNUMS`,
        note: `This command is useful to remove sequent formulas that may have become irrelevant in the current goal.`
    },
    "demod-lin": {
        description: `Partial linear demodulator derivation and application`,
        syntax: `demod-lin`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "demod-num": {
        description: `Numerical demodulation`,
        syntax: `demod-num`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "detuple-boundvars": { 
        description: `Distribute tuple and record quantication`,
        syntax: `detuple-boundvars`,
        note: `A top-level formula of the form 'FORALL (x: [S1, S2, S3]): g(x)' 
            is replaced by 'FORALL (x1: S1), (x2: S2), (x3: S3): g(x1, x2, x3)'. 
            Similarly, a top-level formula 'FORALL (x: [# s : S, t : T #]) : g(x)' 
            is replaced by 'FORALL (x1: S), (x2: T)): g((# s := x1, t := x2 #))'.
            This decomposition of tuple and record quantication is usually needed
            to carry out an induction over one of the components. Tuple quantication 
            can be introduced when instantiating parameterized theories.`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }    
    },
    "distrib": {
        description: `Distribute multiplication over additive terms`,
        syntax: `distrib FNUMS`,
        optionals: {
            ":side l": `Apply the command to the left-hand side of the formula.`,
            ":side r": `Apply the command to the right-hand side of the formula.`,
            "TERM-NUMS": `Indexes specifying which terms the command should be applied to.` 
        }
    },
    "distrib!": {
        description: `Distribute multiplication over additive terms`,
        syntax: `distrib! EXPR-LOC`
    },
    "div-by": { 
        description: `Divide both sides of a sequent formula by a factor EXPR`,
        syntax: `div-by FNUMS EXPR`,
        optionals: {
            ":sign +": `EXPR is known to be positive.`,
            ":sign -": `EXPR is known to be negative.`,
            ":sign *": `Introduce conditional expressions to handle the cases EXPR > 0 and EXPR < 0.`
        }
    },
    "do-rewrite": {
        description: `Use decision procedures to rewrite sequent formulas`,
        syntax: `do-rewrite`,
        optionals: {
            "FNUMS": `Apply the command to the given sequent formula numbers, e.g., (do-rewrite "-1 2") applies the command to sequents -1 and 2.`,
            ":rewrite-flag rl": `Apply the command only to the right-hand side of the formula.`,
            ":rewrite-flag lr": `Apply the command only to the left-hand side of the formula.`,
            ":linear? t": `Multiplication and division are uninterpreted.`
        }
    },
    "elim-unary": {
        description: `Eliminate unary minus functions in additive expressions`,
        syntax: `elim-unary FNUM`,
        note: `This command converts sequent formulas of the form x +/- -y to the form x -/+ y. Also converts -x + y to y - x.`
    },
    "elim-unary!": {
        description: `Eliminate unary minus functions in additive expressions.`,
        syntax: `elim-unary EXPR-LOC`
    },
    "eta": { 
        description: `Introduces Eta version of extensionality axiom for a given TYPE`,
        syntax: `eta TYPE`
    },
    "eval": {
        description: `Print the evaluation of a given expression`,
        syntax: `eval EXPR`,
        note: `This command may use semantic attachments for the evaluation of uninterpreted terms.`,
    },
    "eval-expr": {
        description: "Adds a new antecedent formula given by the evaluation of the given expression",
        syntax: `eval-expr EXPR`,
        note: `This command may use semantic attachments for the evaluation of uninterpreted terms.`,
    },
    "eval-formula": {
        description: "",
        syntax: `eval-formula FNUM`,
        note: `This command may use semantic attachments for the evaluation of uninterpreted terms.`,
    },
    "expand": {
        description: "Expand a name and simplify",
        syntax: `expand NAME`
    },
    "expand*": {
        description: "Expand a series of names and simplify",
        syntax: `expand* NAMES`
    },
    "expand-names": {
        description: "Apply 'expand*' to sequents FNUMS",
        syntax: "expand-names FNUMS NAMES+"
    },
    "extensionality": {
        description: `Apply extensionality axiom scheme for functions types, records types, tuple types and abstract datatypes`,
        syntax: `extensionality TYPE`,
        note: `The extensionality rule is similar to the 'lemma' rule in that 
            it introduces an extensionality axiom for the given type as an 
            antecedent formula. An extensionality axiom can be generated 
            corresponding to function, record, and tuple types, and constructor 
            subtypes of abstract datatypes.`
    },
    "factor": {
        description: `Extract common multiplicative factors from additive terms, and then re-arrange the expression`,
        syntax: `factor FNUMS`,
        optionals: {
            ":side l": `Apply the command to the left-hand side of the formula.`,
            ":side r": `Apply the command to the right-hand side of the formula.`,
            "TERM-NUMS": `Indexes specifying which terms the command should be applied to.` 
        }
    },
    "factor!": {
        description: `Extract common multiplicative factors from the additive terms, and then re-arrange the expression`,
        syntax: `factor! EXPR-LOC`
    },
    "fert-tsos": {
        description: `Inequality fertilization for trivial sums of squares`,
        syntax: `fert-tsos`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "field": {
        description: `Remove divisions and apply simplification heuristics`,
        syntax: `field`,
        optionals: {
            "FNUM": `Apply the command to given sequent formula number.`,
            ":cancel? t": `Try to cancel common terms once the expression is free of divisions.`
        }
    },
    // "field-about": { description: `Prints Field's about information` },
    "flatten": { 
        description: `Apply disjunctive simplification`,
        syntax: `flatten`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "flatten-disjunct": {
        description: "Apply controlled disjunctive simplification",
        syntax: `flatten-disjunct`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "flip-ineq": {
        description: `Negate the inequality formulas and move the resulting formulas by exchanging between antecedents and succedents`,
        syntax: `flip-ineq FNUMS`
    },
    "gen-ex-cad": {
        description: `Generic cylindrical algebraic decomposition via QEPCAD-B.`,
        syntax: `gen-ex-cad`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "generalize": {
        description: `Generalize terms`,
        syntax: `generalize TERM VAR`,
        note: `If the sequent is of the form a1(t), a2(t) ├─ c1(t), c2(t), 
            then applying 'generalize "t" "x"' yields a sequent of the form 
            FORALL x: (a1(x) AND a2(x)) IMPLIES (c1(x) OR c2(x)).`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "generalize-skolem-constants": {
        description: `Generalize skolem constants`,
        syntax: `generalize-skolem-constants`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "grind": {
        description: `Install rewrites and repeatedly simplify`,
        syntax: `grind`,
        optionals: {
            ":if-match nil": "This option prevents the automatic instantiation of terms."
        }
    },
    "grind-reals": {
        description: `Apply grind with real_props`,
        syntax: `grind-reals`
    },
    // "grind-with-ext": {
    //     description: `Like 'grind', but calls 'reduce-ext', which also uses 'apply-extensionality'`
    // },
    // "grind-with-lemmas": {
    //     description: `Does a combination of (lemma) and (grind); if lazy-match? is t, postpones instantiations to follow a first round of simplification.`,
    // },
    "ground": {
        description: `Apply propositional simplification`,
        syntax: `ground`
    },
    "ground-eval": {
        description: `Prints the evaluation of a given ground expression`,
        syntax: `ground-eval EXPR`
    },
    "group": { 
        description: `Associatively grouping three terms`,
        syntax: `group TERM1 OPERATOR TERM2 TERM3`
    },
    "group!": {
        description: `Associatively grouping three terms of the function application found at EXPR-LOC`,
        syntax: `group! EXPR-LOC`
    },
    "has-sign": {
        description: `Try claiming that a given expression is either > 0 or < 0`,
        syntax: `has-sign EXPR`,
        optionals: {
            ":sign +": `Claim that a given expression is positive.`,
            ":sign -": `Claim that a given expression is negative.`
        }
    },
    // "help": { description:""},
    "hide": { 
        description: `Hide sequent formulas`,
        syntax: `hide FNUMS`,
        note: `Hidden sequents can be restored using the 'reveal' command. Use (show-hidden) to see the list of hidden sequents.`
    },
    // "hide-all-but": {
    // 	description: `Hide Selected Formulas: this is a variant of the hide rule that hides all the formulas indicated by
    // 	FNUMS except those indicated by keep-FNUMS. As with hide, hidden sequent
    // 	formulas are saved and can be restored to a descendant of the current sequent
    // 	by the reveal rule.`
    // },
    "iff": {
        description: `Convert boolean equality to equivalence`,
        syntax: `iff`,
        note: `Yields a subgoal where boolean equalities of the form A = B are converted to A ⇔ B.`, 
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "induct": { 
        description: `Perform an induction on VAR`,
        syntax: `induct VAR`
    },
    "induct-and-rewrite": { 
        description: `Perform an induction on VAR and then simplify using rewrites`,
        syntax: `induct-and-rewrite VAR`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`,
            "REWRITES": `Simplify using the given rewrite rules.`
        },
        note: `An example invocation is (induct-and-rewrite "x" 1 "append" "reverse"). This invocation 
            inducts on "x" in formula 1, then simplifies the base and induction using the definitions 
            "append" and "reverse".`
    },
    "induct-and-rewrite!": { 
        description: `Perform an induction on VAR and then simplify using rewrites while expanding all definitions`,
        syntax: `induct-and-rewrite! VAR`
    },
    "induct-and-simplify": { 
        description: `Perform an induction on VAR and then simplify using rewrites rules defined in the given theories`,
        syntax: `induct-and-simplify VAR :theories THEORIES`,
        optionals: {
            ":rewrites REWRITES": `Simplify using the given rewrite rules.`
        }
    },
    "inst": {
        description: `Instante existential quantifiers using the given terms`,
        syntax: `inst FNUM TERMS`
    },
    // "inst!": { 
    //     description: ``
    // },
    // "inst*": {
    //     description: ``
    // },
    // "inst-cp": { 
    //     description: `Copy and instantiate existentially quantified formula`
    // },
    "inst?": {
        description: `Use heuristic instantiation to remove existential quantifiers`,
        syntax: `inst?`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`,
            ":subst VAR EXPR": `List of substitutions for variable names.`
        }
    },
    // "install-rewrites": { 
    //     description: ``,
    //     syntax: ``,
    //     effect: ``,
    //     examples: {}
    // },
    // "instantiate": {
    //     description: `Same as 'inst`
    // },
    // "instantiate-one": { 
    //     description: `Same as 'inst', but has not effect if the instantiation would introduce a duplicate formula`
    // },
    "insteep": { 
        description: `Instantiate existentially quantified formula with constants that have the same name of the quantified variables`,
        syntax: `insteep FNUM`
    },
    "insteep*": { 
        description: `Iterates N times 'insteep'`,
        syntax: `insteep* FNUM`
    },
    "int-dom-zpb": { 
        description: `Integral domain zero product branching for explicit zero indeterminate products`,
        syntax: `int-dom-zpb`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`,
        }
    },
    "isolate": { 
        description: `Move all additive terms except that numbered TERM-NUM in relational formula FNUM from SIDE (l or r) to the other side`,
        syntax: `isolate FNUM SIDE TERM-NUM`
    },
    "isolate-mult": { 
        description: `Select the first factor from the left side of formula FNUM and divide both sides as needed to leave the selected term isolated.`,
        syntax: `isolate-mult FNUM`,
        note: "A case split to generate the appropriate condition on the divisor is automatically introduced.",
        optionals: {
            ":side r": `Apply the command to the right-hand side of the formula.`,
            ":sign +": `Claim that the product of the selected factors is positive.`,
            ":sign -": `Claim that the product of the selected factors is negative.`,
            ":term-num TERM-NUM": `Apply the formula to the term whose index is TERM-NUM.`
        }
    },
    "isolate-replace": { 
        description: `Isolate term TERM-NUM on SIDE (l or r) of relational formula FNUM, then replace and hide it.`,
        syntax: `isolate-replace FNUM SIDE TERM-NUM`
    },
    "label": { 
        description: `Define a label to a sequent formula`,
        syntax: `label LABEL FNUM`
    },
    "lazy-grind": { 
        description: `Install rewrites and repeatedly simplify`,
        syntax: `lazy-grind`,
        note: `Equivalent to (grind) with the instantiations postponed until after simplification.`
    },
    "lemma": { 
        description: `Import a lemma and add it as first antecedent`,
        syntax: `lemma LEMMA`
    },
    "lift-if": {
        description: `Lift embedded IF connectives to the top level`,
        note: `This command lifts the leftmost-innermost contiguous IF or CASES branching structure out to the top level.`,
        syntax: `lift-if`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`,
        }
    },
    "measure-induct+": { 
        description: `Suitably instantiates and uses the measure induction scheme`,
        syntax: `measure-induct+ MEASURE VARS`,
    },
    "measure-induct-and-simplify": { 
        description: `Invokes MEASURE-INDUCT+ then repeatedly expands definitions`,
        syntax: `measure-induct-and-simplify MEASURE VARS`
    },
    "merge-fnums": {
        description: "Merges indicated FNUMS into a single formula",
        syntax: "merge-fnums FNUMS" 
    },
    // "model-check": { 
    //     description: `THIS COMMAND IS BROKEN DON'T USE IT`,
    // },
    "move-terms": { 
        description: `Move additive terms from one side (l or r) of a relational formula to the other side, adding or substracting as needed.`,
        syntax: `move-term FNUM SIDE`,
        optionals: {
            "TERM-NUMS": `Indexes specifying which terms the command should be applied to.` 
        }
    },
    "move-to-front": { 
        description: `Moves sequent formulas FNUMS to the front of the antecedent or consequent lists as appropriate.`,
        syntax: `move-to-front FNUMS`,
    },
    "mult-by": { 
        description: `Multiply both sides of a sequent formula by a factor EXPR`,
        syntax: `mult-by FNUMS EXPR`,
        optionals: {
            ":sign +": `EXPR is known to be positive.`,
            ":sign -": `EXPR is known to be negative.`,
            ":sign *": `Introduce conditional expressions to handle the cases EXPR > 0 and EXPR < 0.`
        }
    },
    "mult-cases": { 
        description: `Generate case analyses for relational formulas containing products`,
        syntax: `mult-cases FNUM`
    },
    "mult-eq": { 
        description: `Given two formulas, one a relation 'a R b', and the other an antecedent equality 'x = y', introduce a new formula relating the products, 'a * x R b * y'`,
        syntax: `mult-eq REL-FNUM EQ-FNUM`
    },
    "mult-extract": { 
        description: `Extract additive terms from a relational formula, and treat the extracted terms as a product of factors`,
        syntax: `mult-extract NAME FNUM`
    },
    "mult-extract!": { 
        description: `Extract additive terms from a relational formula, and treat the extracted terms as a product of factors`,
        syntax: `mult-extract! NAME EXPR-LOC`
    },
    "mult-ineq": { 
        description: `Given two antecedent inequalities 'a R1 b' and 'x R2 y', form an inequality on their products 'a * x R3 b * y'`,
        syntax: `mult-ineq FNUM1 FNUM2`
    },
    "musimp": { 
        description: `MU calculus simplification`,
        syntax: `musimp`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`,
        }
    },
    "name": { 
        description: `Introduces an antecedent EXPR = NAME.`,
        note: `This command is useful for generalizing the goal, and 
            replacing EXPR with bound variables, so that the prover 
            can be applied to them.`,
        syntax: `name NAME EXPR`
    },
    "name-case-replace": { 
        description: `Replace A with B, then name B as X`,
        syntax: `name-case-replace A B X`
    },
    "name-distrib": { 
        description: `Introduces new names to block the automatic application of distributive laws in sequent formulas`,
        syntax: `name-distrib`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`,
        }
    },
    "name-extract": { 
        description: `Extract expressions`,
        syntax: `name-extract NAME`
    },
    "name-induct-and-rewrite": { 
        description: `Perform induction on VAR and then simplify using rewrites`,
        syntax: `name-induct-and-rewrite VAR`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`,
            "INDUCTION-NAME": `Name of the induction scheme to be used.`,
            "REWRITES": `Simplify using the given rewrite rules.`
        }
    },
    "name-label": { 
        description: `Adds formula EXPR = NAME, where NAME is a new name, as an antecedent and replaces EXPR by NAME in FNUMS`,
        syntax: `name-label NAME EXPR`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "name-label*": { 
        description: `Iterates 'name-label' over a list NAMES-AND-EXPRS, which is assumed to be a list of the form (NAME1 EXPR1 NAME2 EXPR2 ... NAMEn EXPRn).`,
        syntax: `name-label* NAMES-AND_EXPRS`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "name-mult": { 
        description: `Select a list of factors (indicated by TERM-NUMS) from the expression found on SIDE (l or r) of relational formula FNUM.
            Assign NAME to the product of the selected factors and replace the product by NAME`,
        syntax: `name-mult NAME FNUM SIDE`,
        optionals: {
            "TERM-NUMS": `Indexes specifying which terms the command should be applied to.` 
        }
    },
    "name-mult!": { 
        description: `Select a list of factors (indicated by TERM-NUMS) from the expression found at EXPR-LOC.
            Assign a NAME to the product of the selected factors and replace the product by NAME.
            Can only handle first expression that results from EXPR-LOC.`,
        syntax: `name-mult! NAME EXPR-LOC`,
        optionals: {
            "TERM-NUMS": `Indexes specifying which terms the command should be applied to.` 
        }
    },
    "name-replace": { 
        description: `Replace an expression with a given name`,
        syntax: `name-replace NAME EXPR`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "name-replace*": { 
        description: `Iterates 'name-replace' over a list NAMES-AND-EXPRS, which is assumed to be a list of the form (NAME1 EXPR1 NAME2 EXPR2 ... NAMEn EXPRn).`,
        syntax: `name-replace* NAMES-AND_EXPRS`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "neg-formula": { 
        description: `Negates both sides of a relational formula`,
        syntax: `neg-formula`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    "op-ident": { 
        description: `Apply the identity operator to rewrite the expressions in a relational formula`,
        syntax: `op-ident FNUM`,
        note: `This command is useful for removing unnecessary terms in arithmetic expressions, e.g., 'x + 0' and and 'x * 1' will become 'x'.`,
        optionals: {
            ":side l": `Apply the command to the left-hand side of the formula.`,
            ":side r": `Apply the command to the right-hand side of the formula.`
        }
    },
    "op-ident!": { 
        description: `Apply the identity operator to rewrite the expressions found at EXPR-LOC`,
        syntax: `op-ident! EXPR-LOC`
    },
    "open-ex-inf-cad": { 
        description: `Cylindrical algebraic decomposition with EX-INF-MANY relaxation for open predicates via QEPCAD-B`,
        syntax: `open-ex-inf-cad`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    "open-frag-ex-inf-cad": { 
        description: `Fragmented cylindrical algebraic decomposition with EX-INF-MANY relaxation for open predicates via QEPCAD-B`,
        syntax: `open-frag-ex-inf-cad`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    "permute-mult": { 
        description: `Reorder multiplicative terms`,
        syntax: `permute-mult FNUMS`,
        optionals: {
            ":side l": `Apply the command to the left-hand side of the formula.`,
            ":side r": `Apply the command to the right-hand side of the formula.`,
            "TERM-NUMS": `Indexes specifying which terms the command should be applied to.`     
        }
    },
    "permute-mult!": { 
        description: `Reorder multiplicative terms found at EXPR-LOC`,
        syntax: `permute-mult! EXPR-LOC`
    },
    "permute-terms": { 
        description: `Reorder additive terms on a side (l or r) of relational formula FNUM`,
        syntax: `permute-terms FNUM SIDE`
    },
    "permute-terms!": { 
        description: `Reorder additive terms found at EXPR-LOC`,
        syntax: `permute-terms! EXPR-LOC`
    },
    "postpone": {
        description: `Postpone current goal`,
        note: `Marks the current goal as pending to be proved and shifts the focus to the next remaining goal.`,
        syntax: `postpone`
    },
    "presburger": { 
        description: `Decision procedure for Presburger arithmetic by reduction to WS1S`,
        syntax: `presburger`,
        optionals: {
            "FNUMS": `Apply the command to sequent formula FNUMS.`
        }
    },
    "presburger-to-ws1s": { 
        description: `Translate Presburger formulas into WS1S`,
        syntax: `presburger-to-ws1s`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "prop": { 
        description: `Apply propositional simplification`,
        syntax: `prop`
    },
    // "propax": { description: `propax is automatically applied to every sequent that is ever generated in a proof, and there is never any need to actively invoke it.`},
    // "pvsio-about": { description: `` },
    // "quit": {
    //     description: `Terminates the current proof session without saving it`,
    //     syntax: `quit`
    // },
    "rahd": { 
        description: `Real algebra in high dimensions`,
        syntax: `rahd`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "rahd-simp": { 
        description: `Real algebra in high dimensions`,
        syntax: `rahd-simp`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "rahd-waterfall": { 
        description: `Emulates the RAHD waterfall`,
        syntax: `rahd-waterfall`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "random-test": { 
        description: `Runs a random test by creating random values for the skolem constants and running the ground evaluator on those values.`,
        note: `This command is useful for checking if the given sequent is worth proving.
            If it comes back with a counter example, then it may not be worth trying to prove.
            Of course, it may just be that a lemma is needed, or relevant formulas were
            hidden, and that it isn't really a counter example.`,
        syntax: `random-test`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`,
            ":count N": `Number of test runs to be executed.`,
            ":size MAX": `Generate random numbers between (-MAX ... +MAX).` 
        }
    },
    "rcr-ineqs": { 
        description: `Reduction of terms in inequalities to canonical rep's in residue class ring induced by equational constraints`,
        syntax: `rcr-ineqs`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "rcr-svars": { 
        description: `Scalar-valued indeterminate fertilization via bounded indeterminate power sequence reduction over residue class ring induced by equational constraints`,
        syntax: `rcr-svars`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "real-props": { 
        description: `Autorewrite with 'real-props'`,
        syntax: `real-props`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "recip-mult": { 
        description: `Convert the top-level division operation on a side (l or r) of a relational formula to a multiplication by the reciprocal of the divisor.`,
        syntax: `recip-mult FNUMS SIDE`
    },
    "recip-mult!": { 
        description: `Convert the top-level division operation found at EXPR-LOC to a multiplication by the reciprocal of the divisor.`,
        syntax: `recip-mult! EXPR-LOC`
    },
    "redlet": { 
        description: `Reduces a let-in expression`,
        syntax: `redlet`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    "redlet*": { 
        description: `Iteratively reduce let-in expressions`,
        syntax: `redlet*`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    // "reduce": { 
    //     description: `Core of GRIND (ASSERT, BDDSIMP, INST?, SKOLEM-TYPEPRED, FLATTEN, LIFT-IF, i.e., BASH then REPLACE*) without reestablishing all the rewrites`
    // },
    // "reduce-with-ext": { 
    //     description: `Core of GRIND-WITH-EXT (ASSERT, BDDSIMP, INST?, SKOLEM-TYPEPRED, FLATTEN, LIFT-IF, i.e., BASH then REPLACE*), like REDUCE, but includes APPLY-EXTENSIONALITY.`
    // },
    "replace": { 
        description: `Apply left-to-right rewrite using formula FNUM`,
        syntax: `replace FNUM`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "replace*": { 
        description: `Apply left-to-right rewrites`,
        syntax: `replace*`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "replace-eta": { 
        description: `Instantiates Eta axiom scheme with a given TERM`,
        syntax: `replace-eta TERM`
    },
    "replace-ext": { 
        description: `Use extensionality axiom to replace F by G`,
        syntax: `replace-ext F G`
    },
    // "replace-extensionality": { 
    //     description: `** Command superseeded by replace-ext`
    // },
    "replaces": { 
        description: `Iterates the proof command replace`,
        syntax: `replaces`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "residue-class-ring-ineqs": { 
        description: `Reduction of terms in inequalities to canonical rep's in residue class ring induced by equational constraints`,
        syntax: `residue-class-ring-ineqs`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "reveal": {
        description: `Reveal hidden formulas`,
        note: `The list of hidden formulas can be viewed with 'show-hidden'.`,
        syntax: `reveal FNUMS`
    },
    "rewrite": { 
        description: `Use lemmas or sequent formula FNUMS to rewrite expressions`,
        syntax: `rewrite LEMMA-OR-FNUM`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "rewrite*": { 
        description: `Recursively use LEMMAS-OR-FNUMS to rewrite expressions`,
        syntax: `rewrite* LEMMA-OR-FNUM`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "rewrite-expr": { 
        description: `Use lemmas to rewrite the expressions located at EXPR-LOC`,
        syntax: `rewrite-expre LEMMAS EXPR-LOC`
    },
    "rewrite-lemma": { 
        description: `Use lemma as a conditional rewrite rule relative to the given substitutions`,
        syntax: `rewrite-lemma LEMMA SUBST`
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
    "rewrite-with-fnum": { 
        description: `Use sequent formula FNUM to rewrite expressions`,
        syntax: `rewrite-with-fnum FNUM`
    },
    "rewrites": { 
        description: `Rewrites with a list of lemmas or fnums. LEMMAS-OR-FNUMS has the form (LEMMAS-OR-FNUMS1 ... LEMMAS-OR-FNUMS).`,
        syntax: `rewrites LEMMAS-OR-FNUMS`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "same-name": { 
        description: `Assume given constants are identical if their actuals are equal`,
        note: `This command is used to indicate that names are equal even if their actuals are not syntactically equal.`,
        syntax: `same-name NAME1 NAME2`
    },
    // "save": { 
    //     description: `Save current proof`,
    //     syntax: `save`
    // },
    "show-hidden": {
        description: `Show the list of hidden sequent formulas.`,
        syntax: `show-hidden`
    },
    // "show-parens": { 
    //     description: `Show how infix operators and operands are associated by displaying formulas with full parenthesization`
    // },
    // "show-subst": { 
    //     description: `Tests command formation with substitutions extracted from extended expression specifications.`
    // },
    "simp-arith": { 
        description: `Polynomial arithmetic simplification`,
        syntax: `simp-arith`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "simp-gls": { 
        description: `Ground literal simplification`,
        syntax: `simp-gls`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "simp-real-null": { 
        description: `Extraction of simple real nullstellensatz refutation certificates from equational constraints`,
        syntax: `simp-real-null`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "simp-tvs": { 
        description: `Truth value simplification`,
        syntax: `simp-tvs`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "simp-zrhs": { 
        description: `RHS zeroing with polynomial canonicalization`,
        syntax: `simp-zrhs`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "simple-induct": { 
        description: `Selects an induction scheme according to the type of VAR in FORMULA and uses FORMULA to formulate an induction predicate`,
        syntax: `simple-induct VAR FORMULA`
    },
    "simple-measure-induct": { 
        description: `Selects and insert an instance of measure induction as an antecedent formula`,
        syntax: `simple-measure-induct MEASURE VARS`
    },
    "simplify": { 
        description: `Simplify using decision procedures`,
        syntax: `simplify`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "simplify-with-rewrites": { 
        description: `Install rewrites and then simplify`,
        syntax: `simplify-and-rewrite`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "skeep": { 
        description: `Skolemize using the names of the bounded variables as the names of the skolem constants`,
        syntax: `skeep`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    "skeep*": { 
        description: `Iteratively skolemize a universally quantified formula using the names of the bounded variables as the names of the skolem constants`,
        syntax: `skeep*`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    // "skodef": { 
    //     description: ``
    // },
    // "skodef*": { 
    //     description: ``
    // },
    "skolem": { 
        description: `Replace universally quantified variables in sequent fomula FNUM with SKOLEM-CONSTANTS`,
        syntax: `skolem FNUM SKOLEM-CONSTANTS`
    },
    "skolem!": { 
        description: `Skolemize a universally quantified formula`,
        syntax: `skolem!`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    "skolem-typepred": { 
        description: `Skolemize and then introduces type-constraints of the Skolem constants`,
        syntax: `skolem-typepred`,
        optionals: {
            "FNUMS": `Apply the command to sequent formula FNUMS.`
        }
    },
    "skoletin": { 
        description: `Decompose a let-in expression`,
        syntax: `skoletin`
    },
    "skoletin*": { 
        description: `Iteratively decompose a let-in expression`,
        syntax: `skoletin*`,
        optionals: {
            "FNUMS": `Apply the command to sequent formula FNUMS.`
        }
    },
    "skosimp": { 
        description: `Skolemize and then simplify`,
        syntax: `skosimp`,
        optionals: {
            "FNUMS": `Apply the command to sequent formula FNUMS.`
        }
    },
    "skosimp*": { 
        description: `Iteratively skolemize and then simplify`,
        syntax: `skosimp*`
    },
    // "smash": { 
    //     description: `Repeatedly tries BDDSIMP, ASSERT, and LIFT-IF`
    // },
    "splash": { 
        description: `Asymmetrically split sequent fomulas`,
        syntax: `splash`,
        optionals: {
            "FNUMS": `Apply the command to sequent formula FNUMS.`
        }
    },
    "split": {
        description: `Split conjunctive sequent formulas`,
        syntax: `split`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "split-ineq": { 
        description: `Split non-strict antecedent inequality (<= or >=) into two cases`,
        syntax: `split-ineq FNUM`
    },
    "sq-simp": { 
        description: `Simplify using lemmas from theories 'sq' and 'sqrt'`,
        syntax: `sq-simp`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    "sub-formulas": { 
        description: `Subtract two sequent formulas`,
        syntax: `sub-formulas FNUM1 FNUM2`
    },
    "suffices": { 
        description: `Introduces a given expression in a universally quantified formula`,
        syntax: `suffices FNUM EXPR`
    },
    "swap": { 
        description: `Try commutatively swapping two terms and replacing`,
        syntax: `swap TERM1 OP TERM2`
    },
    "swap!": { 
        description: `Try commutatively swapping the two arguments of the function application found at EXPR-LOC`,
        syntax: `swap! EXPR-LOC`
    },
    "swap-group": { 
        description: `Try associatively regrouping and swapping three terms`,
        syntax: `swap-group TERM1 OP TERM2 TERM3`
    },
    "swap-group!": { 
        description: ` Try associatively regrouping the three subexpressions of the function applications found at EXPR-LOC`,
        syntax: `swap-group! EXPR-LOC`
    },
    "swap-rel": { 
        description: `Swap the two sides of relational formulas`,
        syntax: `swap-rel FNUMS`
    },
    "triv-ideals": { 
        description: `Ideal triviality checking via reduced Groebner bases`,
        syntax: `triv-ideals`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    "typepred": { 
        description: `Make subtype constraints explicit for given expressions`,
        syntax: `typepred EXPRS`
    },
    // "typepred!": {
    //     description: `Make subtype constraints explicit for given expressions`,
    //     syntax: `typepred! EXPRS`
    // },
    "univ-sturm-ineqs": { 
        description: `Sturm sequence sign-change analysis for univariate open-interval systems`,
        syntax: `univ-sturm-ineqs`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    "use": { 
        description: `Import a lemma and use heuristic instantiation and then beta-reduction`,
        syntax: `use LEMMA`,
        optionals: {
            ":subst VAR EXPR": `List of substitutions for variable names.`
        }
    },
    "use*": { 
        description: `Iternatively import a series of lemmas and use heuristic instantiation and then beta-reduction`,
        syntax: `use* LEMMAS`
    },
    "use-with": { 
        description: `Import a lemma and use given sequent formulas for heuristic instantiation`,
        syntax: `use-with LEMMA FNUMS`
    },
    "ws1s": { 
        description: `Decision procedure for Weak Second-order monadic logic of 1 Successor`,
        syntax: `ws1s`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    "ws1s-simp": { 
        description: `Decision procedure for Weak Second-order monadic logic of 1 Successor`,
        syntax: `ws1s-simp`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    // "y2grind": { 
    //     description: `Core of GRIND: Installs rewrites, repeatedly applies BASH, and then invokes YICES`
    // },
    "y2simp": { 
        description: `Repeatedly skolemizes and flattens, and then applies the Yices2 SMT solver`,
        syntax: `y2simp`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    // "ygrind": { 
    //     description: `Core of GRIND: Installs rewrites, repeatedly applies BASH, and then invokes YICES`
    // },
    // "yices": { 
    //     description: `Invokes Yices as an endgame SMT solver to prove that the conjunction of the negations of the selected formulas is unsatisfiable`,
    //     syntax: `yices`,
    //     optionals: {
    //         "FNUM": `Apply the command to sequent formula FNUM.`
    //     }
    // },
    // "yices-with-rewrites": { description: `` },
    "yices2": { 
        description: `Invokes Yices2 as an endgame SMT solver to prove that the conjunction of the negations of the selected formulas is unsatisfiable`,
        syntax: `yices2`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    // "yices2-with-rewrites": { description: `` }
};


export const PROOF_TACTICS: { [key:string]: ProofCommandDescriptor } = {
    "apply": {
        description: `In-line definition of user-defined strategies.`,
        syntax: `apply strategy COMMENT`,
        note: `Applies a user-defined strategy as if it were a primitive proof rule, and prints COMMENT.
            An example is as follows:
            'apply (then (skosimp*)(flatten)(inst?)) "Skolemizing, flattening, and instantiating"': 
        `
    },
    "branch": {
        description: `In-line definition of proof strategies for multiple branches.`,
        syntax: `branch STEP TACTIC+`,
        note: `Performs a branching STEP and then applies the i'th tactic to the i'th subgoal.
            If the number of subgoals is larger than the number of tactics, then the n'th tactic is applied to the remaining subgoals.\n  
            This command is typically used when step splits the proof into multiple branches
            where a different strategy is required for each of the branches.`
    },
    "branch-back": { 
        description: `In-line definition of proof multiple proof tactics.`,
        syntax: `branch-back STEP TACTIC+`,
        note: `Perform a branching STEP and then applies the i'th tactic to the i'th subgoal.
            Automatic backtracking is performed for branches that fail to prove their goals.
            That is, if a tactic for a branch fails to prove its goal, the proof state for that branch 
            is rolled back to the point before the step was invoked.`
    },
    "comment": { 
        description: `Attach a comment to a a proof node`,
        syntax: `comment LABEL`,
        note: `COMMENT is attached to the sequent. Formulas remain unchanged.`
    },
    // "default-strategy": { description:""},
    "deftactic": { 
        description: `Defines a labelled proof tactic.`,
        syntax: `deftactic TACTIC_NAME TACTIC`,
        note: `Defines a labelled proof tactic. 
            The tactic is local to the current branch of the proof. 
            TACTIC_NAME needs to be a valid identifier in PVS.
            An example is as follows:
                '(deftactic foo (then (flatten) (assert) (grind)))': 
            `
    },
    "discriminate": {
        description: `Label formulas generated by a proof step`,
        syntax: `discriminate STEP LABEL`,
        note: `Labels formulas generated by STEP as LABEL(s).`
    },
    "else": { 
        description: `Try STEPS in sequence until the first one succeeds.`,
        syntax: `else STEPS`
    },
    "else*": { 
        description: `Try STEPS in sequence until the first one succeeds.`,
        syntax: `else* STEPS`
    },
    "equate": {
        description: `Try equating two expressions and replacing the LHS by the
            RHS in FNUMS.  Proof of the justification step can be tried or deferred.
            Use TRY-JUST to supply the rule for the justification proof or T for
            the default rule (GRIND).`,
        syntax: `equate lhs rhs`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    "extra-tcc-step": {
        description: " Tries to prove TCCs by first using (assert) and then (subtype-tcc)",
        syntax: `extra-tcc-step`
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
        note: `A failure signal is propagated to the parent proof goal. If the parent goal is
            not able to act on this signal, it further propagates the failure to its parent.
            This rule, like skip, is mainly employed in constructing strategies where it is
            used to control backtracking. Applying fail to the root sequent causes the
            proof to be unsuccessfully terminated.`
    },
    "finalize": {
        description: `Either finishes the current goal with STEP or does nothing.`,
        syntax: `field STEP`
    },
    "for": { 
        description: ``,
        syntax: ``
    },
    "for-each": { 
        description: ``,
        syntax: ``
    },
    "for-each-rev": { 
        description: ``,
        syntax: ``
    },
    "for@": { 
        description: ``,
        syntax: ``
    },
    "forward-chain": { 
        description: ``,
        syntax: ``
    },
    "forward-chain*": { 
        description: ``,
        syntax: ``
    },
    "forward-chain-theory": { 
        description: ``,
        syntax: ``
    },
    "forward-chain@": { 
        description: ``,
        syntax: ``
    },
    "if": {
        description: `Evaluates the condition (in Lisp) and if it evaluates to nil, step2 is applied, otherwise step1 is applied.`,
        syntax: ``
    },
    "if-label": {
        description: `Applies THEN-STEP if at least one formula in the sequent is labeled LABEL. Otherwise, applies ELSE-STEP.`,
        syntax: ``
    },
    "invoke": { 
        description: `Invoke a rule or strategy by instantiating COMMAND with substitutions extracted from the extended expression specifications EXPR-SPECS`,
        syntax: `invoke COMMAND`,
        note: `Example: suppose formula 1 is f(x+y) = f(a*(z+1)).
            Then (invoke (case "%1 = %2") (? 1 "f(%1) = f(%2)"))
            would match and create the bindings %1='x+y' and %2='a*(z+1)', 
            which results in the prover command (case "x+y = a*(z+1)") being invoked.`
    },
    "just-install-proof": { 
        description: `Installs an edited PROOF without actually checking it, declares the subgoal as finished, but then marks the proof as unfinished`,
        syntax: "just-install-proof PROOF"
    },
    "let": { 
        description: `Allows variables in body to be bound to the results of Lisp computations.`,
        note: `Example: (let ((x (car *new-fmla-nums*))) (then (inst? x)(split x)))`,
        syntax: `let BINDING BODY`
    },
    "let-name-replace": { 
        description: `For each LET expressions of the current sequent, create local definitions corresponding to the LET bindings using the NAME strategy, substituting these names into the rest of the LET expr`,
        syntax: `let-name-replace`,
        optionals: {
            "FNUMS": `Apply the command to sequent FNUMS.`
        }
    },
    "lisp": { 
        description: `Evaluate a Lisp expression`,
        syntax: `lisp LISP-EXPR`
    },
    "mapstep": { 
        description: `Sequentially applies FUNSTEP to each element of LIST.`,
        syntax: `mapstep FUNSTEP LIST`
    },
    "mapstep@": { 
        description: `Sequentially applies FUNSTEP to each element of LIST.`,
        syntax: `mapstep@ FUNSTEP LIST`
    },
    "match": { 
        description: `Try matching syntax patterns against formulas in the sequent`,
        syntax: `match SPEC-ITEMS`
    },
    "printf": { 
        description: `Print a Lisp formatted string`,
        syntax: `printf MSG`
    },
    "protect": { 
        description: `Protects formulas FNUMS so that they are not affected by STEP`,
        syntax: `protect FNUMS STEP`
    },
    "query*": { 
        description: `Query the user for the next step.`,
        syntax: `query*`
    },
    // "quote": { 
    //     description: `This command is used by the let strategy to ensure that the values for variables are not evaluated again after substitution.`
    // },
    "record": { 
        description: `Uses decision procedures to simplify and record the formulas in FNUMS for further simplification`,
        syntax: `record`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "relabel": { 
        description: `Create a new label for sequent formulas and keep the old labels`,
        syntax: `relabel LABEL FNUMS`
    },
    "repeat": { 
        description: `Successively apply STEP along main branch until it does nothing`,
        syntax: `repeat STEP`
    },
    "repeat*": { 
        description: `Successively apply STEP along main branch until it does nothing`,
        syntax: `repeat* STEP`
    },
    "rerun": { 
        description: `Strategy to rerun existing or supplied proof`,
        syntax: `rerun`
    },
    "rotate++": {
        description: `Move the first succedent formula to the end of the succedents`,
        syntax: `rotate++`
    },
    "rotate--": { 
        description: `Moves the first antecedent formula to the end of the antecedents.`,
        syntax: `rotate--`
    },
    "rule-induct": { 
        description: `Applies co-induction over an inductive relation REL`,
        syntax: `rule-induct REL`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "rule-induct-step": { 
        description: `Applies co-induction over an inductive relation REL`,
        syntax: `rule-induct-step REL`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
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
    // "set-right-margin": { 
    //     description: `Sets the print right margin`
    // },
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
    // "skip-steps": { 
    //     description: `This strategy is used for debugging purposes`
    // },
    // "sklisp": { 
    //     description: `Evaluates lispexpr and skips`
    // },
    "spread": { 
        description: `Define proof strategies for branching steps.`, 
        syntax: `spread STEP STEPLIST`,
        note: `Performs a branching STEP and then applies the i'th element of STEPLIST to the i'th subgoal. 
            This command is typically used when step splits the proof into multiple branches
            where a different strategy is required for each of the branches.`
    },
    "spread!": { 
        description: ``,
        syntax: ``
    },
    "spread@": { 
        description: ``,
        syntax: ``
    },
    "stop-rewrite": { 
        description: `Turn off automatic rewrites`,
        syntax: `stop-rewrite NAMES`
    },
    "stop-rewrite-theory": { 
        description: `Turn off all automatic rewrites defined in given theories`,
        syntax: `stop-rewrite-theory THEORY-NAMES`
    },
    // "tccs-expression": { 
    //     description: ``
    // },
    // "tccs-formula": { 
    //     description: ``
    // },
    // "tccs-formula*": { 
    //     description: ``
    // },
    // "tccs-step": { 
    //     description: ``
    // },
    "then": { 
        description: ``,
        syntax: ``
    },
    "then*": { 
        description: ``,
        syntax: ``
    },
    "then@": { 
        description: ``,
        syntax: ``
    },
    "time": { 
        description: ``,
        syntax: ``
        },
    "touch": { 
        description: ``,
        syntax: ``
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
        description: `Apply TRANSFORM to both sides of relational formula FNUM`,
        syntax: `transform-both FNUM TRANSFORM`
    },
    // "trust": { 
    //     description: ``
    // },
    // "trust!": { 
    //     description: `This strategy performs a miracle on behalf of trusted orcale ORCL`
    // },
    "try": { 
        description: ``,
        syntax: ``
    },
    "try-branch": { 
        description: ``,
        syntax: ``
    },
    "try-rewrites": { 
        description: ``,
        syntax: ``
    },
    "undo": {
        description: `Undo the last proof command.`,
        note: `The prover steps back to the ancestor node of the current proof node.`,
        syntax: "undo"
    },
    "unlabel": { 
        description: `Remove labels attached to sequent formulas`,
        syntax: `unlabel`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    "unlabel*": { 
        description: `Remove labels attached to sequent formulas`,
        syntax: `unlabel*`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    "unless": { 
        description: `Behaves as (if (not FLAG) (then STEP1 ... STEPn) (skip))`,
        syntax: `unless FLAG STEPS`
    },
    "unless-label": { 
        description: `Sequentially applies STEPS to all branches as long as no formula in the sequent is labeled LABEL`,
        syntax: `unless-label LABEL STEPS`
    },
    "unless-label@": { 
        description: `Sequentially applies STEPS to the main branch as long as no formula in the sequent is labeled LABEL`,
        syntax: `unless-label@ LABEL STEPS`
    },
    "unless@": { 
        description: `Behaves as (if (not FLAG) (then@ STEP1 ... STEPn) (skip))`,
        syntax: `unless@ FLAG STEPS`
    },
    "untrace": {
        description: `Turns off the tracing of proof commands named in names, as initiated by (trace).`,
        syntax: "untrace"
    },
    "untrack-rewrite": { 
        description: `Disables the tracking of rewrite rules invoked by track-rewrite.`,
        syntax: "untrack-rewrite"
    },
    "unwind-protect": { 
        description: `Invoke MAIN-STEP followed by CLEANUP-STEP, which is performed even if MAIN-STEP leads to a proof of the current goal.`,
        syntax: `unwind-protect MAIN-STEP CLEANUP-STEP`
    },
    "when": { 
        description: ``,
        syntax: ``
    },
    "when-label": { 
        description: ``,
        syntax: ``
    },
    "when-label@": { 
        description: ``,
        syntax: ``
    },
    "when@": { 
        description: ``,
        syntax: ``
    },
    "with-focus-on": { 
        description: ``,
        syntax: ``
    },
    "with-focus-on@": { 
        description: ``,
        syntax: ``
    },
    "with-fresh-labels": { 
        description: ``,
        syntax: ``
    },
    "with-fresh-labels@": { 
        description: ``,
        syntax: ``
    },
    "with-fresh-names": { 
        description: ``,
        syntax: ``
    },
    "with-fresh-names@": { 
        description: ``,
        syntax: ``
    },
    "with-labels": { 
        description: ``,
        syntax: ``
    },
    "with-tccs": { 
        description: ``,
        syntax: ``
    },
    "wrap-formula": { 
        description: ``,
        syntax: ``
    },
    "wrap-manip": { 
        description: ``,
        syntax: ``
    },
};

// TODO: add more commands
export const EVALUATOR_COMMANDS: { [key:string]: ProofCommandDescriptor } = {
    "RANDOM": {
        description: `Generate a random number.`,
        syntax: `RANDOM`
    }
};

// a selection of 32 useful commands for advanced users. The selection has been based on statistics from nasalib and feedback from experienced pvs users
export const PROOF_COMMANDS_ADVANCED_PROFILE: { [key: string]: ProofCommandDescriptor } = {
    "all-typepreds": PROOF_COMMANDS["all-typepreds"],
    "apply-ext": PROOF_COMMANDS["apply-ext"],
    "assert": PROOF_COMMANDS["assert"],
    "beta": PROOF_COMMANDS["beta"],
    "bddsimp": PROOF_COMMANDS["bddsimp"],
    "case": PROOF_COMMANDS["case"],
    "decompose-equality": PROOF_COMMANDS["decompose-equality"],
    "expand": PROOF_COMMANDS["expand"],
    "eval-expr": PROOF_COMMANDS["eval-expr"],
    "flatten": PROOF_COMMANDS["flatten"],
    "grind": PROOF_COMMANDS["grind"],
    "grind-reals": PROOF_COMMANDS["grind-reals"],
    "ground": PROOF_COMMANDS["ground"],
    "hide": PROOF_COMMANDS["hide"],
    "iff": PROOF_COMMANDS["iff"],
    "induct": PROOF_COMMANDS["induct"],
    "inst?": PROOF_COMMANDS["inst?"],
    "insteep": PROOF_COMMANDS["insteep"],
    "label": PROOF_COMMANDS["label"],
    "lemma": PROOF_COMMANDS["lemma"],
    "lift-if": PROOF_COMMANDS["lift-if"],
    "name": PROOF_COMMANDS["name"],
    "prop": PROOF_COMMANDS["prop"],
    "replace": PROOF_COMMANDS["replace"],
    "rewrite": PROOF_COMMANDS["rewrite"],
    "random-test": PROOF_COMMANDS["random-test"],
    "skeep": PROOF_COMMANDS["skeep"],
    "skoletin": PROOF_COMMANDS["skoletin"],
    "skosimp*": PROOF_COMMANDS["skosimp*"],
    "split": PROOF_COMMANDS["split"],
    "typepred": PROOF_COMMANDS["typepred"],
    "use": PROOF_COMMANDS["use"]
};

export const PROOF_COMMANDS_BASIC_PROFILE: { [key: string]: ProofCommandDescriptor } = {
    "all-typepreds": PROOF_COMMANDS["all-typepreds"],
    "assert": PROOF_COMMANDS["assert"],
    "beta": PROOF_COMMANDS["beta"],
    "case": PROOF_COMMANDS["case"],
    "expand": PROOF_COMMANDS["expand"],
    "flatten": PROOF_COMMANDS["flatten"],
    "grind": PROOF_COMMANDS["grind"],
    "inst?": PROOF_COMMANDS["inst?"],
    "lemma": PROOF_COMMANDS["lemma"],
    "lift-if": PROOF_COMMANDS["lift-if"],
    "prop": PROOF_COMMANDS["prop"],
    "replace": PROOF_COMMANDS["replace"],
    "skosimp*": PROOF_COMMANDS["skosimp*"],
    "split": PROOF_COMMANDS["split"]
};

export function getCommands(profile: ProofMateProfile): { [key: string]: ProofCommandDescriptor } {
    switch (profile) {
        case "basic": { return PROOF_COMMANDS_BASIC_PROFILE; }
        case "advanced": { return PROOF_COMMANDS_ADVANCED_PROFILE; }
        default: return {};
    }
}

export declare type ProofMateProfile = "basic" | "advanced";