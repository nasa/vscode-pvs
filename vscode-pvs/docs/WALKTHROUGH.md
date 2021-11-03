# Walkthrough on VSCode-PVS
This document includes two parts:
- A quick [primer on Visual Studio Code](#primer-on-visual-studio-code)
- Guidance on [how to use VSCode-PVS](#how-to-use-vscode-pvs)

<br>

# Primer on Visual Studio Code
`Visual Studio Code` has three main panels:
- `Editor panel`, located centrally in the user interface.
- `Explorer panel`, located on the side of the user interface.
- `Debug panel`, located at the bottom of the user interface.

The `Editor panel` shows code editors
- Each editor is associated with a tab. Dragging the tab allows you to create different layouts, e.g., split views
- A minimap on the side gives an overview of the entire text
- A toolbar located at the top of the editor provides convenient access to frequent commands
- Ctrl+F (Command+F on Mac) can be used to search strings in the active editor. The search can be case-sensitive, match entire word, regexp. Expand the search panel to perform replace operations.

The `Explorer panel` shows information in the form of a tree view, akin to file browsers.
- An `activity bar` is located on the side, and each icon in the bar activates a different view.
- The `file icon` activates File Explorer, which shows the list of files in the current workspace. Click on a file to open the file content in the editor. Right click on a file to perform rename/delete/copy/paste operations. 
- The `magnifying glass icon` activates a Search panel, that allows you to search text in all files in the current workspace.
- The `extensions icon` activates the extension manager, which shows the list of extensions currently installed in VSCode, and allows you to search and install new extensions from the VSCode marketplace.
- The `pvs icon` opens pvs-related views.
- The `settings icon` allows to customize VSCode, e.g., keyboard shortcuts, and color theme.

The `Debug panel` show information about warnings/errors.
- Click on errors/warnings to open the problematic file in the editor and jump to the location of error/warning.

More information on the VSCode interface can be found online at https://code.visualstudio.com/docs/getstarted/userinterface

<br>

# How to use VSCode-PVS
The typical workflow in VSCode-PVS is as follows:
1. Open a pvs workspace
2. Edit theories
3. Typecheck
4. Prove

## Opening a pvs workspace
A pvs workspace is a folder that contains (or will contain) pvs files
- To open a pvs workspace, click the `open folder` icon located in the editor toolbar. A window will pop up, that allows to browse the file system and select an existing folder or create a new folder that will be used as pvs workspace

Once a workspace is open, click on the `pvs icon` in the activity bar switch to `Workspace Explorer` and view the list of theories/theorems defined in the active workspace
- Click on a theory/theorem to open the corresponding pvs file in the editor and jump to the line where the theory/theorem is defined
- Right click on a theory/theorem to open a menu with actions on files (e.g., typecheck file) and theorems (e.g., prove formula)
- Click on the header menu `...` to access additional actions, e.g., create a new pvs file, view pvs settings, install pvs/nasalib, reboot pvs

## Defining/Editing theories and theorems
When a pvs file is open in the editor, the editor will automatically highlight keywords and library functions
- Hover the mouse on a term to view the definition
- Right click on a term to access more functions, e.g., "peek definition"
- Autocomplete is available for terms and keywords (use the TAB key to autocomplete)
- Snippets are available for common syntactic constructs, e.g., if-then-else blocks
- Math symbols are entered using latex shortcuts (e.g., \forall)
- Parsing errors are automatically detected and are underlined with a red squiggle

## Typechecking theories
To typecheck a theory, click the `build` icon in the editor toolbar
Alternatively, you can also use an inline actionable command 'typecheck' available next to the theory name.
- If typecheck errors are detected, the errors will be underlined with a red squiggle in the editor, and also shown in the Problems panel
- If the theory typechecks correctly, you can proceed to proving theorems

## Proving theorems
To prove a theorem, click the `prove` command displayed inline above the theorem name.
- When a proof is started, three components will be opened automatically: `Prover Console`, `Proof Explorer` and `Proof Mate`
- The `Prover Console` is the main way to interact with the theorem prover. Proof commands are entered at the prover prompt. A tooltip is displayed while typing commands. TAB autocompletes prover commands. Esc suppresses the tooltip. Math symbols can be entered using latex shortcuts. An integrated help shows useful information about the command being entered. Double click to expand definitions. UP/DOWN arrow keys allow you to navigate the command history.
- `Proof Explorer` shows the proof tree, and provides an integrated toolbar. The `play button` re-runs the proof, `forward button` steps the proof, and th `back button` rewinds the proof (i.e., undoes proof commands). Right click on proof commands to access additional functionalities, e.g., `edit` proof command, `cut/paste` proof commands, `fast forward` to a proof command, `show sequent` to open the sequent of executed proof commands in the editor. The `checkmark button` optimizes the tree view by folding all proved branches. Place the cursor over executed proof commands to view the sequent in a tooltip.
- `Proof Mate` provides `Hints` about proof commands that could be used to make progress with the proof (the heuristics implemented are really simple at the moment, don't expect too much for now!), and a `Sketchpad` that collects proof branches that become detached from the proof tree (this facilitates proof repair and proof re-use)

To `re-run all proofs` and generate a `summary file` that shows the proof status, click the `play button` located in Workspace Explorer, next to the theory name.

## Evaluating expressions
Several constructs of the pvs language are executable and can be evaluated to compute a result. This is useful, e.g., for validation purposes.
PVSio is the pvs component for evaluating expressions.
To evaluate expressions in PVSio:
- Click the PVSio evaluator icon in the editor toolbar
- An interactive prompt will be displayed, where you can enter the expression to be evaluated

<br>

# Additional functionalities of VSCode-PVS

## Plot Diagrams
Plot diagrams can be created for pvs expressions that return a list of numbers.
- To generate a plot diagram, select an expression in the pvs file open in the editor, right click on the selected expression, and choose `Plot Expression` from the menu.

## Rapid Prototyping
Interactive prototypes can be created based on executable pvs specifications.
- To create an interactive prototype, open an executable pvs specification, click on the `prototype icon` in the editor toolbar to launch the PVSio-web prototype builder and simulator. Examples of interactive prototypes can be downloaded at [https://github.com/pvsioweb/examples](https://github.com/pvsioweb/examples)

## Search NASALib
VSCode-PVS provides a simplified interface to search definitions and lemmas in NASALib, an extensive library created and maintained by the Formal Methods Team at NASA Langley.
- To search NASALib, click the `NASA meatball logo` in the editor toolbar. A side window will be opened with a search panel. Enter a search string in the search panel and press enter to search. Results will be displayed in the search panel.
- To filter NASALib search results, enter a string in the `filter` input.

<br>

# Emergency Exit
In the case VSCode-PVS becomes unresponsive, use the `Reboot PVS` command located in the `...` menu of Workspace Explorer. This will reboot the back-end and likely resolve the problem. 

Alternatively, close and re-open Visual Studio Code. 

If the problem persists, please report an issue on [github](https://github.com/nasa/vscode-pvs/issues) or leave us a message in the [PVS group on Google](https://groups.google.com/g/pvs-group), we will look into it.