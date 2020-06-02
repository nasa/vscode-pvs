# VSCode-PVS Tutorial
This tutorial provides a series of simple examples suitable to demonstrate the functionalities of VSCode-PVS.
- Novice PVS users can use this tutorial as a gentle introduction to the PVS verification system.
- Expert PVS users can use this tutorial as a basis to discover the editing and proving capabilities provided by VSCode-PVS.

---

## Pre-requisites
- Familiarity with [Visual Studio Code](https://code.visualstudio.com)
- [VSCode-PVS](https://github.com/nasa/vscode-pvs) extension installed and enabled in Visual Studio Code (see installation instructions in [README.md](README.md))

---

## Troubleshooting
Please check our [FAQs](FAQ.md) document, where you can find answers to common questions on VSCode-PVS, including troubleshooting techniques for basic problems.  

---

## Example 1: Hello World!
This first example demonstrates the following functionalities of VSCode-PVS:
- PVS Syntax highlighting
- Navigation of PVS definitions

Try the following actions in VSCode-PVS:
1. Create a new file `helloworld.pvs` in the current workspace.
2. Enter the following specification:
```pvs
helloworld: THEORY
  BEGIN 
    abs (x: real): real = IF x > 0 THEN x ELSE -x ENDIF
  END helloworld
```
The specification defines a module (`theory` in the PVS language) called `helloworld`, and a function `abs` that computes the absolute value of a number. The function has one argument `x` of type `real`, and its return type is of type `real`.
3. Place the cursor over the return type `real`. A hover box will pop up and show the type definition.
> Hint 1: The box contains a clickable link to jump to the type definition.

> Hint 2: Try to type the specification rather than doing copy-paste: you will be able to see autocompletion in action.

> Hint 3: Introduce syntax errors in the specification: diagnostics will be provided every time you save the file.

---

## Example 2: Typechecking
The PVS language is strongly typed, and `typechecking` is the functionality for checking the correct use of types in a PVS specification. 

The following example demonstrates:
- How to typecheck a PVS specification
- How to inspect and resolve typecheck errors

Try the following actions in VSCode-PVS:
1. Open the pvs file `helloworld.pvs` created in the previous example.
2. To typecheck the file: right-click in the editor window. A contextual menu will be shown, Select `Typecheck` from the contextual menu.
3. VSCode-PVS will display a message box indicating whether the file typechecks correctly or contains errors.
> Hint 1: Try to introduce type errors in the specification, e.g., change the return type of the function from `real` to `bool`. Typecheck again the file, and see what errors are detected. 

> Hint 2: Are you an expert PVS user that likes the `M-x` shortcuts? Try them out, most of them will work in VSCode-PVS! For example, you can typecheck a pvs file with the shortcut `M-x tc`

---

## Example 3: Subtyping
A powerful feature of the PVS language is `sub-typing`, which allows developers to restrict the domain of an existing type. When using subtypes, PVS generates additional proof obligations, also known as typecheck conditions (`TCCs`) that can be conveniently used to improve the quality of a PVS specification.

The following example demonstrates:
- How to create a subtype
- How to inspect and discharge proof obligations (TCCs)

Try the following actions in VSCode-PVS:
1. Edit function `abs` in `helloworld.pvs` so as to change the return type to `posreal`, where posnat is defined as `posreal: TYPE = { r: real | r >=0 }`. The specification should look as follows:
```pvs
helloworld: THEORY
  BEGIN 
    posreal: TYPE = { r: real | r >=0 }
    abs (x: real): posreal = IF x > 0 THEN x ELSE -x ENDIF
  END helloworld
```
2. Typecheck the specification. The system will report that the specification typechecks correctly and two TCCs were generated
3. To inspect the TCCs: right-click in the editor window. A contextual menu will be shown, Select `Show TCCs` from the contextual menu. The editor will open a new file `helloworld.tccs` that allows you to check the TCCs.
4. To discharge all TCCS: right-click in the editor window showing the TCCs. A contextual menu will be displayed. Select `Discharge all TCCs` from the contextual menu.
> Hint: Check the functionalities of `PVS Workspace Explorer` located in the side panel, under file explorer. It shows all theories defined in the context and, for each theory, all formulas and TCCs.
---

## Example 4: Testing
Several PVS constructs are `executable`, that is, they can be evaluated using concrete inputs.
This is useful for validation purposes, e.g., to check that a given function produces expected outputs for given inputs.

The following example demonstrates:
- How to evaluate a PVS specification in the interactive `PVSio` (pronounced `PVS`-`io`) evaluator.

Try the following actions in VSCode-PVS:
1. Open a pvs file (e.g., `helloworld.pvs`). Make sure the file typechecks correctly.
2. To start the `PVSio` evaluator: right-click in the editor window. A contextual menu will be shown, Select `Evaluate in PVSio` from the contextual menu.
3. VSCode-PVS will open an integrated terminal window with the PVSio prompt. The content of the terminal window should look as follows:
```
+---- 
| PVSio Evaluator
|
| Enter a PVS expression at the prompt, or 'help' for help, or 'exit' to exit the evaluator.
| You can use TAB to complete commands at the PVSio prompt.
|
| Note: evaluation of expressions which depend on unproven TCCs may be unsound,
| and result in the evaluator becoming unresponsive or crashing into Lisp. 
| If that happens, please close the evaluator session and start a new one.
|
+----

<PVSio> 
```
4. To evaluate an executable PVS expression, enter the expression at the `<PVSio>` prompt, followed by `;` 
> Hint: If you are evaluating `helloworld.pvs`, you can enter the following expression: `abs(-7);`
5. To exit the PVSio evaluator, enter the following command at the `<PVSio>` prompt: `exit;` 

---

## Example 5: Proving
Properties of a PVS specifications are defined in expressions called `theorems`.
Theorems are proved interactively, using the PVS theorem prover.

The following example demonstrates:
- How to define a theorem
- How to prove the theorem in the interactive PVS theorem prover

Try the following actions in VSCode-PVS:
1. Edit theory `helloworld.pvs` so as to introduce a theorem `always_positive`. The specification should look as follows:
```pvs
helloworld: THEORY
  BEGIN 
    posreal: TYPE = { r: real | r >=0 }
    abs (x: real): posreal = IF x > 0 THEN x ELSE -x ENDIF

    always_positive: THEOREM
      FORALL (x: real): abs(x) >= 0
  END helloworld
```
2. VSCode-PVS will automatically create an in-line command `prove` above the theorem name. Click it to start an interactive prover session in the  integrated terminal. The content of the terminal window should look as follows:
```
always_positive :

  ├───────
{1}   FORALL (x: real): abs(x) >= 0

 >> 
```
4. To prove the theorem, enter the following proof command at the prover prompt: `grind`
> Hint 1: Formulate a theorem that is not true, and check how the theorem prover is able to provide useful diagnostics that can help fix the formulation. For example, try to prove the following theorem `always_positive_alt`, where the condition expressed in `always_positive` has been changed from `>=` to `>`:  
> ```pvs
> always_positive_alt: THEOREM
>   FORALL (x: real): abs(x) > 0
> ```
> The proof of this theorem cannot be completed with `grind`, and the theorem prover returns the following diagnostic information:
> ```
> {-1}   real_pred(x!1)
>   ├───────
> {1}   x!1 > 0
> {2}   -x!1 > 0
> ```
> Formula `{-1}` (called antecedent) indicates that `x` is a real number (`x!1` is a skolem constant representing a generic `x`).
> Formulas `{1}` and `{2}` (called succedents) indicate that the theorem is true when `x > 0` and `x < 0`, respectively.
> Based on these diagnostics, one can deduce that the theorem prover was unable to complete the proof when `x = 0`, as it is the only missing case.

> Hint 2: Check the functionalities of `PVS Proof Explorer` located in the side panel, under file explorer. It shows the proof steps and provides functions for proof playback and proof editing.

---

## Further reading
- For more information on the PVS language and theorem proving system, please refer to the [PVS documentation](http://pvs.cls.sri.com)
- Additional examples can be found in the [NASA PVS Class](https://shemesh.larc.nasa.gov/PVSClass2012/)
