# VSCode-PVS User Interface
This document illustrates the layout and functionalities of VSCode-PVS.

<br>

## Layout

The layout of VSCode-PVS includes 8 main elements:
1. [PVS]() button. Located in the vscode activity bar, this button activates three custom views created for PVS: *Workspace Explorer*, *Proof Explorer*, and *Proof Mate*.
2. [Workspace Explorer]() view. Located in the vscode side panel, this view shows the name of the PVS theories defined in the current workspace, as well as the list of theorems defined in each theory. Contextual menus and in-line actions provide quick access to functionalities such as *typechecking* and *prove formula*.
3. [Proof Explorer]() view. Located in the vscode side panel, this view shows the proof tree for the current proof. Contextual menus and in-line actions provide quick access to functionalities such as *run proof* and *step proof*.
4. [Proof Mate]() view. Located in the vscode side panel, this view provides *hints* on proof commands that can be used to make progress with the current proof. It also contains a *sketchpad* that stores proof commands clipped from the current proof shown in Proof Explorer (e.g., when editing or repairing a proof).
5. [Editor]() view. Located in the vscode central panel, this view allows to edit PVS files.
6. [Prover Terminal]() view. Located in the vscode lower panel, this view provides an interactive prompt for sending proof commands to the PVS theorem prover.
7. [Reboot pvs]() and [Interrupt Prover]() buttons. Located in the vscode status bar, these buttons can be used to reset vscode-pvs in the case of unexpected errors or run-away situations.
8. PVS version information.

<img src="../screenshots/vscode-pvs-user-interface-annotated.png" width="600">

<br><br>

## Navigation

<br> <img src="../screenshots/vscode-pvs-theory-explorer.gif" width="600">

VSCode-PVS provides an interactive tree view, `Workspace Explorer`, to facilitate the navigation of folders containing PVS files (such folders are called *PVS workspaces*).

The interactive tree view includes two main types of nodes:
- `theory nodes`: top-level nodes, representing *theories* defined in the current PVS workspace.
- `formula nodes`: second-level nodes, representing *formulas* (theorems, lemmas, TCCs, etc.) defined within a theory.

Click on a node to select the node and open files in the Editor:
- Click on a theory node to open in the Editor the PVS file where the theory is defined.
- Click on a formula node to jump to the formula definition.

Right-click on a node to open contextual menus providing common actions on PVS files:
- `Typecheck file`: typechecks the selected file or theory
- `Typecheck workspace`: typechecks all files in the current workspace
- `Show Proof Summary`: shows a summary file indicating the status (proved, unfinished, untried, etc.) of each proof defined in the selected theory
- `Show Typecheck Conditions`: shows the proof obligations generated for the selected file or theory
- `Re-Run All Proofs`: re-runs all proofs defined in the selected theory
- `Re-Run ImportChain`: re-runs all proof defined in the selected theory and in the imported theories
- `Discharge TCCs`: tries to discharge all proof obligations for the selected theory
- `Evaluate in PVSio`: starts a PVSio evaluator session for the selected theory

In-line actionable commands are displayed next to each node and provide convenient access to frequent actions:
- `Typecheck`: typecheck the selected file or theory
- `Prove`: start a new prover session for the selected formula
- `Prove-All`: re-run all proofs defined in the selected theory


<br>

## Editing

VSCode-PVS includes the common editor features needed for creating and editing PVS theories.

- **Syntax highlighting**: PVS keywords and library functions are automatically highlighted.
- **Autocompletion and code snippets**: Tooltips suggesting function names and language keywords are automatically presented in the editor when placing the mouse over a symbol name. Code snippets are provided for frequent modeling blocks, e.g., if-then-else. 
<br><br> <img src="../screenshots/vscode-pvs-autocompletion.gif" width="600">

- **Hover information for symbol definitions**: Hover boxes providing information about identifiers are automatically displayed when the user places the cursor over an identifier.
<br><br> <img src="../screenshots/vscode-pvs-hover.gif" width="600">

- **Go-to definition**: Click on the name of the identifier while holding down the Ctrl key to jump to the location where the identifier is declared.
<br><br> <img src="../screenshots/vscode-pvs-goto-definition.gif" width="600">

- **Live diagnostics**: Parsing is automatically performed in the background, and errors are reported in-line in the editor. Problematic expressions are underlined with red wavy lines. Tooltips presenting the error details are shown when the user places the cursor over the wavy lines.
<br><br> <img src="../screenshots/vscode-pvs-live-diagnostics.gif" width="600">

<br><br>

## Proving

<br> <img src="../screenshots/vscode-pvs-proof-explorer.gif" width="600">

Interactive prover sessions can be started for each formula defined in the current PVS workspace.
To start a prover session, click the `Prove` button in `Workspace Explorer`.
This action will activate three components:
- `Proof Explorer`: interactive tree view for displaying, executing, and editing proof scripts.
- `Proof Mate`: interactive tree view displaying hints.
- `Integrated Prover Terminal`: interactive command line for sending proof commands to the PVS theorem prover.

<br>

**Proof Explorer**

The interactive tree view provided by Proof Explorer uses the following conventions to display a proof script:
- the root of the tree is the name of the formula currently being proved.
- a sequence of proof commands in the proof script is represented as a series of sibling nodes.
- sub-goals generated by a proof command are represented as children of the proof command.

Icons are displayed next to the nodes:
- blue diamond: indicates the *active command*, i.e., the proof command ready to be sent to the theorem prover.
- hollow star: indicates a *visited command*, i.e., a proof command that has already been sent to the theorem prover. 
- full star: indicates a *complete branch*, i.e., a branch that is proved and complete in the current proof. 
- bullet: indicates a proof node that has not yet been sent to the theorem prover (for proof nodes below the active node) or proof commands that were executed by the prover but did not produce any change in the sequent (proof nodes above the active node).

Action icons are provided in the title bar:
- *play*: re-runs the entire proof script.
- *forward*: sends the next command to the theorem prover.
- *back*: sends an *undo* command to the theorem prover.

Action icons are also provided next to each node:
- *fast-forward* re-runs the proof script up to the selected node (not included).

<br>

**Integrated Prover Terminal**

A terminal session is automatically started when starting a proof.
The user can enter proof commands in the terminal session.
Each command executed by the prover will be automatically appended to the proof tree displayed in Proof Explorer.
Auto-completion is provided (using the TAB key) for prover commands, as well as access to the commands history.

<br><br>

## Hot-Keys combinations
VSCode-PVS provides a series of hot-key combinations that facilitate access to frequent functions.
The hot-key combinations is activated by the `M-x` sequence, which is obtained by pressing the `META` key and the `x` letter simultaneously. The `META` key on Linux is the `Alt` key. On MacOS, it's usually the `option` key.

Hot-keys for frequent commands on pvs files:
- `M-x show-tccs` *(show proof obligations for the file open in the editor)*
- `M-x tc` *(typecheck the file open in the editor)*
- `M-x tcp` *(typecheck the file open in the editor and re-run all proofs in the file)*
- `M-x parse` *(parse the file open in the editor)*
- `M-x pr` *(prove formula, i.e., start an interactive prover session for the formula at the cursor location)*
- `M-x prt` *(prove theory, i.e., re-run all proofs in the current theory)*
- `M-x pri` *(prove importchain, i.e., re-run all proofs in the current theory and in the imported theories)*
- `M-x pvsio` *(start an interactive PVSio evaluator session for the theory opened in the editor)*
- `M-x show-proof-summary` *(show proof summary)*
- `M-x vpf` *(view prelude file)*

Additional hot-keys:
- `M-x add-pvs-library` *(adds a folder to the vscode-pvs library path)*
- `M-x pvs-library-path` *(shows the pvs library path specified in vscode-pvs settings)*
- `M-x reset-pvs-library-path` *(resets the vscode-pvs library path to empty)*
- `M-x reboot-pvs` *(reboots pvs-server)*
- `M-x clean-bin` *(removes pvsbin files created by pvs)*
- `M-x clean-tccs` *(removes .tccs files created by pvs)*
- `M-x clean-all` *(removes all temporary files (including .tccs and pvsbin) created by pvs)*
- `M-x install-pvs` *(starts as interactive wizard that allows to install or update PVS)*
- `M-x install-nasalib` *(starts an interactive wizard that allows to install NASALib)*
- `M-x update-nasalib` *(updates the installed version of NASALib)*
- `M-x set-pvs-path` *(sets the path to the PVS executables)*
- `M-x settings` *(shows vscode-pvs settings)*
- `M-x release-notes` *(shows vscode-pvs release notes)*

<br><br>

## Useful Visual Studio Code shortcuts

[Linux](https://code.visualstudio.com/shortcuts/keyboard-shortcuts-linux.pdf)
- `ctrl shift p ` *(command palette)*
- `ctrl shift l ` *(add more cursors at each occurrence of the current selected text)*
- `ctrl alt ↓ ` *(add a cursor at the line below)*
- `ctrl shift f ` *(find in workspace)*
- `ctrl f ` *(find in file)*

<br>

[MacOS](https://code.visualstudio.com/shortcuts/keyboard-shortcuts-macos.pdf)
- `⌘ ⇧ p ` *(command palette)*
- `⌘ ⇧ l ` *(add more cursors at each occurrence of the current selected text)*
- `⌘ ⌥ ↓ ` *(add a cursor at the line below)*
- `⌘ ⇧ f ` *(find in workspace)*
- `⌘ f ` *(find in file)*
