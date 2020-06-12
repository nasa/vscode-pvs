# VSCode-PVS: An Integrated Development Environment for the Prototype Verification System
VSCode-PVS is a new integrated development environment for creating, evaluating and verifying PVS specifications.
The environment redefines the way developers interact with PVS, and better aligns the PVS front-end to the functionalities provided by development environments used by software developers.

## Getting started
New to VSCode-PVS and the PVS verification system? Try out our [tutorial](docs/TUTORIAL.md)!

## Functionalities
The main functionalities provided by the environment are as follows:
- **Syntax highlighting**: PVS keywords and library functions are automatically highlighted.
- **Autocompletion and code snippets**: Tooltips suggesting function names and language keywords are automatically presented in the editor when placing the mouse over a symbol name. Code snippets are provided for frequent modeling blocks, e.g., if-then-else.
- **Hover information for symbol definitions**: Hover boxes providing information about identifiers are automatically displayed when the user places the cursor over an identifier.
- **Go-to definition**: Click on the name of the identifier while holding down the Ctrl key to jump to the location where the identifier is declared.
- **Peek definitions**: Symbol definitions can be shown in mini editors embedded in the current view.
- **Live diagnostics**: Parsing is automatically performed in the background, and errors are reported in-line in the editor. Problematic expressions are underlined with red wavy lines. Tooltips presenting the error details are shown when the user places the cursor over the wavy lines.
- **Outline View**: Interactive tree view showing the outline of the pvs file open in the editor. Point-and-click actions can be used to jump to definitions.
- **Workspace Explorer**: Interactive tree view showing all theories in the current workspace, name and status of theorems and typecheck conditions.
- **Proof Explorer**: Interactive tree view for viewing and editing the current proof.
- **Prover Terminal**: An integrated terminal allows interaction with the theorem prover. Auto-completion is provided (using the TAB key) for prover commands, as well as access to the commands history.
- **Proof Mate**: Helper designed to suggest proof commands that can be used to make progress with the current proof.

## Requirements
- Visual Studio Code (v1.37.1 or greater) https://code.visualstudio.com
- NodeJS (v12.16.1 or greater) https://nodejs.org/en/download
- Java JDK (v1.8 or greater) https://openjdk.java.net
- PVS Allegro (v7.1 or greater) http://www.csl.sri.com/users/owre/drop/pvs-snapshots

## Installation instructions
1. Download the latest release of VSCode-PVS from the [github repository](../releases)
2. Install VSCode-PVS in Visual Studio Code: `View` -> `Extensions` -> `...` -> `Install from VSIX` -> Select the downloaded `.vsix` file
3. Have fun using the extension!

> Note: Having troubles with the extension? Check our [FAQs](docs/FAQ.md).

## Updating VSCode-PVS
When a new release of VSCode-PVS is available in the [github repository](../releases), download the new release and install it in Visual Studio Code:

- `View` -> `Extensions` -> `...` -> `Install from VSIX` -> Select the downloaded `.vsix` file

All settings and preferences from the previous version of VSCode-PVS will be maintained.

## Structure
```
.
├── client                       // PVS Language Client (VSCode entry point)
│   └── src
│       ├── providers            // Client-side service providers (emacs binding, decorations, package manager, outline provider)
│       ├── views                // Visual Components: Proof Explorer, Workspace Explorer, Terminals, Status Bar, Sequent Viewer, Proof Mate
│       ├── common               // Utility functions 
│       └── pvsLanguageClient.ts // PVS Language Client implementation
├── icons                        // PVS icons theme
├── package.json                 // The extension manifest
├── syntax                       // Syntax highlighting
├── LICENSES                     // NASA Open Source License Agreement
├── Makefile                     // Makefile for building a .vsix image from the source code
└── server                       // PVS Language Server
    └── src
        ├── providers                          // Server-side service providers
        │     ├── pvsCodeLensProvider.ts       // In-line actionable commands
        │     ├── pvsCompletionProvider.ts     // Auto-completion
        │     ├── pvsDefinitionProvider.ts     // Find definitions
        │     ├── pvsHoverProvider.ts          // Hover information 
        │     └── pvsPackageManager.ts         // Installation manager 
        ├── parser               // Parser grammar and scripts      
        ├── common               // Utility functions           
        ├── pvsCli.ts            // PVS Command Line Interface
        ├── pvsProcess.ts        // PVS process wrapper
        ├── pvsLisp.ts           // Lisp reader for parsing PVS responses
        └── pvsLanguageServer.ts // PVS Language Server implementation
```


## ChangeLog
### 1.0.22 (2020.06.xx)
- Improved usability

### 1.0.21 (2020.06.11)
- Added watchdog to prevent runaway situations with grind
- Improved hover tooltips
- Added FAQs
- Improved feedback & stability

### 1.0.20 (2020.05.26)
- Improved Workspace Explorer
- Improved feedback & stability

### 1.0.19 (2020.05.19)
- Introduced support for MacOS

### 1.0.18 (2020.05.03)
- Improved Workspace Explorer
- Improved editor feedback for parsing/typechecking
- Introduced APIs for MacOS

### 1.0.17 (2020.04.20)
- Improved Proof Mate functionalities (profiles, recommended commands, inline help)
- Improved Prover CLI (autocompletion for lemmas)

### 1.0.16 (2020.04.06)
- Improved Proof Explorer functionalities (tooltips, proof-stepper)
- Improved Editor functionalities (autocompletion, codelens)
- Improved PVSio evaluator terminal (pvs syntax highlighting)

### 1.0.15 (2020.03.12)
- New visual components: Proof Mate, Workspace Explorer (formerly, Theory Explorer)
- Added support for theory outline view (requires visual studio code >= 1.32.3)
- Introduced new pvs parser based on antlr4 (requires java development environment)
- Added support for hybrid programs (.hpvs files)

### 1.0.14 (2019.11.26)
- Improved error handling in pvs-proxy

### 1.0.13 (2019.10.25)
- New visual components: Proof Explorer, Sequent Viewer
- Revised architecture, to connect to the new XMLRPC server provided by PVS 7.0
- Improved PVS command line interface: autocompletion, syntax highlighting
- Improved status bar
- Installation wizard

### 1.0.12 (2019.06.07)
- PVS Command Line Interface (auto-completion and history navigation for proof commands, syntax highlighting for PVS language)
- Improved Theory Explorer (proof status for theorems and tccs)
- Improved navigation of symbol definitions (typechecking information are now fed to the parser process)
- Improved status bar

### 1.0.11 (2019.05.29)
- Improved Theory Explorer (navigation of theorems)

### 1.0.10 (2019.05.24)
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
- Improved PVS language server

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

