# vscode-pvs: PVS extension for Visual Studio Code
PVS language server & PVS language client for Visual Studio Code.


## Functionalities
The extension provides the following language features:
- PVS syntax highlighting
- Code folding
- Autocompletion
- Hover information
- Goto/Peek definitions
- Live diagnostics for syntax errors
- Literate programming for evaluating expressions
- Integrated terminals for PVSio and PVS-prover


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
│       ├── views                // Explorer views and integrated terminals 
│       └── pvsLanguageClient.ts // PVS Language Client implementation
├── icons                        // PVS icons theme
├── package.json                 // The extension manifest
├── syntax                       // Syntax highlighting
└── server                       // PVS Language Server
    └── src
        ├── providers                          // Server-side service providers
        │     ├── pvsCodeLensProvider.ts       // Literate programming
        │     ├── pvsCompletionProvider.ts     // Auto-completion
        │     ├── pvsDefinitionProvider.ts     // Goto/peek definition
        │     └── pvsHoverProvider.ts          // Hover information                
        │             
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

