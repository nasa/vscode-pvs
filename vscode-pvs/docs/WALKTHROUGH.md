# Primer on Visual Studio Code (VSCode)
`Visual Studio Code` has three main components:
- `Editor panel`, located centrally in the user interface.
- `Explorer panel`, located on the side of the user interface.
- `Output and Debug panels`, located at the bottom of the user interface.

The `Editor panel` shows text/code editors and integrated consoles.
- Editors and consoles can be dragged and organized in different layouts, e.g., to create split views.
- Editors have a minimap on the side, that gives an overview of the entire text/code.
- A toolbar located above the editor windows provides convenient access to frequent commands.
- Ctrl+F (Command+F on Mac) can be used to search strings in the active editor.
- Ctrl+/ (Command+/ on Mac) can be used to comment out a line in the active editor.

The `Explorer panel` shows information in the form of a tree view, akin to file browsers.
- An `activity bar` is located on the side, and contains toggle buttons for activating explorer functions.
- The `file button` activates File Explorer, which shows the list of files in the current workspace.
- The `search button` activates a Search panel, that allows to search text in all files in the current workspace.
- The `extensions button` activates the extension manager, which shows the list of extensions currently installed in VSCode, and allows to search and install new extensions from the marketplace.
- The `pvs button` opens pvs-related components. The default component shown in the view is Workspace Explorer, which presents the list of theories and theorems defined in the active workspace. Two additional components will become visible when proving a theorem: PVS Proof Explorer and PVS Proof Mate.
- The tree view supports the use of filter as a way to search elements in the tree view. To activate the filters, select the tree view and start typing the name that you want to search.

The `Output and Debug panels` show information about warnings/errors, and an integrated terminal for running shell commands.

<br>

# Workflow in VSCode-PVS
The typical workflow in VSCode-PVS is as follows:
1. Open a pvs workspace
2. Define/Edit theories and theorems
3. Typecheck theories
4. Prove theorems

## Opening a pvs workspace
A pvs workspace is a folder that contains (or will contain) pvs files.
- To open a pvs workspace, click the `open folder` icon located in the editor toolbar. A window will open that allows to select an existing folder or create a new folder that will be used as pvs workspace.
- File Explorer shows the list of files in the active pvs workspace. 
- Click on a file to open the file content in the editor.
- Right click on a file to perform rename/delete/copy/paste operations. 

Once a workspace is open, switch to `Workspace Explorer` to view the list of theories/theorems defined in the active workspace.
- Click on a theory/theorem to open the corresponding pvs file in the editor and jump to the line where the theory/theorem is defined.
- Right click on a theory/theorem to open a menu with actions on files (e.g., typecheck file) and theorems (e.g., prove formula).
- The `...` symbol at the top of PVS Workspace Explorer is a menu. You can use it to trigger actions on pvs files (e.g., create a new pvs file, view prelude file), and pvs installation and settings (e.g., update/install nasalib and pvs).

## Defining/Editing theories and theorems
When a pvs file is open in the editor, the editor will automatically enable pvs syntax highlighting.
- Hover the mouse on a term to show a tooltip with the definition.
- Right click on a term to access functions such as "go-to definition" and "peek definitionn"
- TAB autocompletes terms and keywords while editing
- Snippets are available for common constructs, e.g., if-then-else
- Math symbols are entered using latex-style shortcuts (e.g., \forall).
- Inline actionable commands are available, to typecheck theories and prove theorems
- Parsing is automatically performed while the user enters the specification.

## Typechecking theories
To typecheck a theory, click the `build` icon in the editor toolbar.
Alternatively, you can also use an inline actionable command 'typecheck' available next to the theory name.
- If the theory typechecks correctly, you can proceed to proving theorems
- If errors are detected, the errors will be underlined with a red squiggle in the editor, and also shown in the Problems panel.

## Proving theorems
To prove a theorem, click the actionable command `prove` displayed inline above the theorem name.
- When a proof is started, three components will be opened automatically: `Prover Console`, `Proof Explorer` and `Proof Mate`
- The `Prover Console` is the main way to interact with the theorem prover. Proof commands are entered in the prover console. A tooltip is displayed while typing commands. TAB autocompletes prover commands. Start a command with a space to suppress the tooltip (this is useful, e.g., if the tooltip covers a part of text that you need to see while typing). An integrated help shows useful information about the command being entered. Use (help cmd) to show a more comprehensive help for a given proof command, e.g., (help grind). UP/DOWN arrow keys provide access to the command history.
- `Proof Explorer` shows the proof tree, and provides an integrated toolbar with a `play button` for re-running the proof, `forward button` for stepping proof commands, and a `back button` for rewinding (i.e., undoing) proof commands. Right click on proof commands to access additional functionalities, e.g., `edit` proof command, `cut/paste` proof commands, `fast forward` to a proof command.
- `Proof Mate` is useful when repairing proofs. A `sketchpad` collects proof branches that become detached from the proof tree, to facilitate inspection of problems and re-use of proof commands. `Hints` about proof commands that could be used to make progress with the proof are also displayed (the heuristics implemented in the command suggester are really simple at the moment, don't expect too much for now!)

To `re-run all proofs` and generate a summary file that shows the proof status, click the `play button` located in Workspace Explorer, next to the theory name.

To evaluate a pvs specification in the PVSio evaluation environment, click on the play/debug icon in the editor toolbar.


<br>

# Extras

## Plotting functions
Plot diagrams can be created for pvs expressions that return a list of numbers.
- To generate a plot diagram, select an expression in the pvs file open in the editor, right click on the selected expression, and choose `Plot Expression` from the menu.

## Creating Interactive prototypes
Interactive prototypes can be created based on executable pvs specifications.
- To create an interactive prototype, open an executable pvs specification, click on the `prototype icon` in the editor toolbar to launch the PVSio-web prototype builder and simulator. Examples of interactive prototypes can be downloaded at [https://github.com/pvsioweb/examples](https://github.com/pvsioweb/examples)

## Searching NASALib
VSCode-PVS provides a simplified interface to search definitions and lemmas in NASALib, an extensive library created and maintained by the Formal Methods Team at NASA Langley.
- To search NASALib, use the `magnifying glass icon` in the editor toolbar.


