# VSCode-PVS: An Integrated Development Environment for the Prototype Verification System
VSCode-PVS is an integrated development environment for creating, evaluating, and verifying PVS specifications.
The environment redefines the way developers interact with PVS, and better aligns the PVS front-end to the features provided by development environments used by software developers.

## Features
The main features provided by the environment are as follows:
- **Syntax highlighting**: PVS keywords and library functions are automatically highlighted.
- **Autocompletion and code snippets**: Tooltips suggesting function names and language keywords are automatically presented to the user when the user is typing a symbol name in the editor. Code snippets are provided for frequent modeling blocks, e.g., if-then-else. 
- **Hover information for symbol definitions**: Hover boxes providing information about identifiers are automatically displayed when the developer places the cursor over an identifier.
- **Jump-to declaration**: Navigation of symbol declarations can be performed with simple point-and-click actions: the user places the cursor over the name of an identifier, and a click on the name of the identifier while holding the Ctrl key down allow to jump to the location where the identifier is declared.
- **Live diagnostics**: Parsing is automatically performed in the background, and errors are reported in-line in the editor. Problematic expressions are underlined with red wavy lines. Tooltips presenting the error details are shown when the user places the cursor over the wavy lines.
- **In-line actionable commands**: Actionable commands are available for PVS theorems. They are rendered in-line in the editor, above the name of the theorem, and can be used to start a new prover session for the theorem with a simple click action.
- **Overview of PVS theories**: The overall structure of a set of PVS theories is rendered using an interactive tree-based view. It shows the set of PVS theories in the active workspace, as well as the name and status (proved, unfinished, etc.) of the theorems defined in each theory. Point-and-click actions can be used to jump to theory definitions and type-check the theories.
- **Interactive proof tree visualizer and editor**: An interactive tree-based view shows the proof associated with a theorem. Point-and-click actions are provided for step-by-step execution of proof commands. Functionalites for editing the proof are currently under development.
- **Integrated PVS and PVSio Command Line Interfaces**: Integrated command line interfaces allow interaction with the theorem prover and the PVSio evaluator. Auto-completion is provided for prover commands, as well as access to the commands history.


## Installation instructions
- Download NodeJS from https://nodejs.org/en/download/
- Download PVS from http://pvs.csl.sri.com/
- Install vscode-pvs in VSCode: Extensions tab -> "..." menu -> "Install from VSIX”, and select the downloaded .vsix file
- Set the path to the PVS executable from the Settings menu: Files -> Preferences -> Settings -> Extensions -> PVS -> path
- Have fun using the extension!


## Structure
```
.
├── client                       // PVS Language Client (VSCode entry point)
│   └── src
│       ├── providers            // Client-side service providers (emacs binding, decorations)
│       ├── views                // PVS Proof Explorer, PVS Theory Explorer 
│       ├── terminals            // PVS Command Line Interface 
│       └── pvsLanguageClient.ts // PVS Language Client implementation
├── icons                        // PVS icons theme
├── package.json                 // The extension manifest
├── syntax                       // Syntax highlighting
├── LICENSES                     // NASA Open Source License Agreement
└── server                       // PVS Language Server
    └── src
        ├── providers                          // Server-side service providers
        │     ├── pvsCodeLensProvider.ts       // In-line actionable commands
        │     ├── pvsCompletionProvider.ts     // Auto-completion
        │     ├── pvsDefinitionProvider.ts     // Find definitions
        │     └── pvsHoverProvider.ts          // Hover information                
        ├── pvsCli.ts            // PVS Command Line Interface
        ├── pvsProcess.ts        // PVS process wrapper
        ├── pvsLisp.ts           // Lisp reader for parsing PVS responses
        └── pvsLanguageServer.ts // PVS Language Server implementation
```


## ChangeLog
### 1.1.2 (2019.06.07)
- PVS Command Line Interface (auto-completion and history navigation for proof commands)
- Improved Theory Explorer (proof status for theorems and tccs)
- Improved navigation of symbol definitions (typechecking information are now fed to the parser process)
- Improved status bar
- Cheat codes for developers: M-x pvs6 M-x pvs7 allow rapid switch of pvs version (requires setting of pvs-zen-path variables in vscode-pvs settings)

### 1.1.1 (2019.05.29)
- Improved Theory Explorer (navigation of theorems)

### 1.1.0 (2019.05.24)
- Added support for view-proof and step-proof commands
- Improved feedback on status bar

### 1.0.9 (2019.04.29)
- Performance improvement (parallel typechecking)
- Added support for automatic context change
- Code cleanup

### 1.0.8 (2019.04.16)
- Improved code navigation (peek definitions)
- Added xmlrpc infrastructure necessary to support pvs-server

### 1.0.7 (2019.04.09)
- Proof explorer front-end for pvs prover
- Improved syntax highlighting and autocompletion
- Autocompletion for M-x commands

### 1.0.6 (2019.04.01)
- Added support for math symbols
- Added handling of .tccs and .ppe files
- Shortcuts for M-x tcp, M-x show-tccs

### 1.0.5 (2019.03.19)
- Integrated terminals for PVSio and PVS Prover
- Shortcuts M-x tc, M-x pr, M-x pvsio
- Code folding for begin-end, if-endif, cond-endcond

### 1.0.4 (2019.03.08)
- Navigation and typechecking of pvs theories in explorer view

### 1.0.3 (2019.03.01)
- Overview of tccs in explorer view
- Shortcut C-t for typechecking files
- Bugfix (Language server not responsive when pvs file contains syntax errors)

### 1.0.2 (2019.02.24)
- Snippets for pvs language (theory declaration, if-then-else, cond-case)

### 1.0.1 (2019.02.22)
- Improved Language Server (buffered input)

### 1.0.0 (2019.02.21)
Initial distribution of vscode-pvs (PVS Language Server + VSCode Language Client).
The following functionalities are provided:
- Auto-Completion for language keywords and identifiers
- Hover information for language keywords and identifiers
- Go-to definition
- Diagnostics for syntax errors
- Literate programming for executable expressions

## Notices
### Copyright 
Copyright 2019 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.
 
### Disclaimers
**No Warranty**: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY
  WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY,
  INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE
  WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM
  INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR
  FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO
  THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER,
  CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT
  OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY
  OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.
  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES
  REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE,
  AND DISTRIBUTES IT "AS IS."
 
**Waiver and Indemnity**: RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS
  AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND
  SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF
  THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES,
  EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM
  PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT
  SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED
  STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY
  PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE
  REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL
  TERMINATION OF THIS AGREEMENT.

