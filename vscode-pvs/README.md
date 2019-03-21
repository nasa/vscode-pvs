# vscode-pvs: PVS extension for Visual Studio Code
PVS language server & PVS language client for Visual Studio Code.


## Functionalities
The plugin provides the following language features:
- PVS Syntax highlighting
- Code folding
- Auto-Completion for language keywords and identifiers
- Hover information for keywords and identifiers
- Goto/Peek definition for identifier
- Diagnostics for syntax errors
- Literate programming using code lenses
- Integrated terminals for PVSio and PVS-prover
- PVS Linter (TODO)
- Signature help (TODO)
- Refactoring (TODO)

## ChangeLog
### 1.0.5 (2019.03.19)
- Integrated terminals for PVSio and PVS Prover
- Shortcuts M-x tc, M-x pr
- Code folding for begin-end, if-endif, cond-endcond

### 1.0.4 (2019.03.08)
- Navigation and typechecking of pvs theories in explorer view

### 1.0.3 (2019.03.01)
- Shortcut C-t for typechecking files
- Overview of tccs shown in explorer view
- Bugfix (Language server not responsive when pvs file contains syntax errors)

### 1.0.2 (2019.02.24)
- Snippets for pvs language (theory declaration, if-then-else, cond-endcond)

### 1.0.1 (2019.02.22)
- Improved Language Server (buffered input)

### 1.0.0 (2019.02.21)
Initial distribution of vscode-pvs, providing a TypeScript implemention a PVS Language Server and a VSCode Language Client.
The following functionalities are supported:
- Auto-Completion for language keywords and identifiers
- Hover information for language keywords and identifiers
- Go-to definition
- Diagnostics for syntax errors
- Literate programming for executable expressions


## Structure
```
.
├── client                       // VSCode Language Client (VSCode entry point)
│   ├── src
│   │   ├── providers            // VSCode service providers (e.g., EmacsBindings) 
│   │   ├── views                // VSCode views (e.g., PVS Explorer) 
│   │   └── pvsLanguageClient.ts // PVS Language Client implementation 
├── package.json                 // The extension manifest.
└── server                       // Language Server
    └── src
        ├── providers            // folder with the service providers (auto-completion, hover, etc.)
        └── pvsLanguageServer.ts // PVS Language Server implementation
```


## Installation instructions
- Set the path to the PVS executable from the Settings Menu: Files -> Preferences -> Settings -> Extensions -> PVS
- Have fun using the extension!