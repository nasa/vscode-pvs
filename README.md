# VSCode-PVS: An Integrated Development Environment for the Prototype Verification System

[![version](https://vsmarketplacebadge.apphb.com/version/paolomasci.vscode-pvs.svg)](https://vsmarketplacebadge.apphb.com/version/paolomasci.vscode-pvs.svg)
[![downloads](https://vsmarketplacebadge.apphb.com/downloads-short/paolomasci.vscode-pvs.svg)](https://vsmarketplacebadge.apphb.com/downloads-short/paolomasci.vscode-pvs.svg)
[![installs](https://vsmarketplacebadge.apphb.com/installs-short/paolomasci.vscode-pvs.svg)](https://vsmarketplacebadge.apphb.com/installs-short/paolomasci.vscode-pvs.svg)
[![license](https://img.shields.io/badge/license-NASA-blue.svg)](https://opensource.org/licenses/NASA-1.3)

VSCode-PVS is a new integrated development environment for creating, evaluating and verifying PVS specifications.
The environment redefines the way developers interact with PVS, and better aligns the PVS front-end to the functionalities provided by development environments used by software developers.

<img src="https://github.com/nasa/vscode-pvs/raw/master/vscode-pvs/screenshots/vscode-pvs-screenshot.png" width="800">

## Latest version
[vscode-pvs-1.0.29-alpha](https://github.com/nasa/vscode-pvs/tree/master/releases/vscode-pvs-1.0.29-alpha.vsix)


## Documentation
- [Quick reference guide](https://github.com/nasa/vscode-pvs/blob/master/vscode-pvs/docs/USER-INTERFACE.md) for the VSCode-PVS User Interface
- [FAQs](https://github.com/nasa/vscode-pvs/blob/master/vscode-pvs/docs/FAQ.md) on VSCode-PVS, including troubleshooting techniques for basic problems.
- Are you new to PVS? Try out our [tutorial](https://github.com/nasa/vscode-pvs/blob/master/vscode-pvs/docs/TUTORIAL.md)!
- Join the new [PVS group on Google](https://groups.google.com/g/pvs-group)


## Requirements
- Linux or MacOS operating system
- NodeJS (v12.16.1 or greater) https://nodejs.org/en/download
- Visual Studio Code (v1.32.3 or greater) https://code.visualstudio.com

## Installation instructions
VSCode-PVS can be installed from the Visual Studio Code Marketplace or from GitHub.


**Automatic installation from Visual Studio Code Marketplace**
- Search `pvs` in https://marketplace.visualstudio.com and select `install`
<br><br><img src="https://github.com/nasa/vscode-pvs/raw/master/vscode-pvs/screenshots/how-to-install-vscode-pvs-from-marketplace.gif" width="600">

**Manual installation from GitHub**
1. Download the .vsix file of VSCode-PVS from [github](https://github.com/nasa/vscode-pvs/raw/master/releases).
2. Click on the Extensions icon in the Activity Bar 
3. Click on the `...` menu in the title bar, and use `Install from VSIX` to select the downloaded .vsix file
<br><br><img src="https://github.com/nasa/vscode-pvs/raw/master/vscode-pvs/screenshots/how-to-install-vscode-pvs.gif" width="600">

>Note: When installing VSCode-PVS for the first time, it will check if PVS Allegro v7.1.0 is present on your system. If not, VSCode-PVS will show a dialog that allows you to download PVS.

>Note: If you have used earlier releases of VSCode-PVS (`< 1.0.26`), you will need to download and install the latest version of PVS and NASALib. You can do this in VSCode-PVS, with the commands `M-x install-pvs` and `M-x install-nasalib`. 


## Updating VSCode-PVS
VSCode-PVS will be automatically updated every time we publish a new release in the marketplace.

All settings and preferences will be maintained when installing a new release.

If you would like to perform manual updates or try out nightly builds, you can download the .vsix file on [github](https://github.com/nasa/vscode-pvs/raw/master/releases).


## Publications
Paolo Masci and César Muñoz, [An Integrated Development Environment for the Prototype Verification System](https://dx.doi.org/10.4204/EPTCS.310.5), Electronic Proceedings in Theoretical Computer Science (EPTCS), Vol. 310, pp. 35-49, 2019 [[PDF](https://arxiv.org/pdf/1912.10632v1)]


## Functionalities
The main functionalities provided by the environment are as follows:
- **Syntax highlighting**: PVS keywords and library functions are automatically highlighted.
- **Autocompletion and code snippets**: Tooltips suggesting function names and language keywords are automatically presented in the editor when placing the mouse over a symbol name. Code snippets are provided for frequent modeling blocks, e.g., if-then-else. 
<br><br> <img src="https://github.com/nasa/vscode-pvs/raw/master/vscode-pvs/screenshots/vscode-pvs-autocompletion.gif" width="600">

- **Hover information for symbol definitions**: Hover boxes providing information about identifiers are automatically displayed when the user places the cursor over an identifier.
<br><br> <img src="https://github.com/nasa/vscode-pvs/raw/master/vscode-pvs/screenshots/vscode-pvs-hover.gif" width="600">

- **Go-to definition**: Click on the name of the identifier while holding down the Ctrl key to jump to the location where the identifier is declared.
<br><br> <img src="https://github.com/nasa/vscode-pvs/raw/master/vscode-pvs/screenshots/vscode-pvs-goto-definition.gif" width="600">

- **Live diagnostics**: Parsing is automatically performed in the background, and errors are reported in-line in the editor. Problematic expressions are underlined with red wavy lines. Tooltips presenting the error details are shown when the user places the cursor over the wavy lines.
<br><br> <img src="https://github.com/nasa/vscode-pvs/raw/master/vscode-pvs/screenshots/vscode-pvs-live-diagnostics.gif" width="600">

- **Workspace Explorer**: Interactive tree view showing all theories in the current workspace, name and status of theorems and typecheck conditions.
<br><br> <img src="https://github.com/nasa/vscode-pvs/raw/master/vscode-pvs/screenshots/vscode-pvs-theory-explorer.gif" width="600">

- **Proof Explorer + Integrated Terminal**: Interactive tree view for viewing and editing the current proof. An integrated terminal allows interaction with the theorem prover. Auto-completion is provided (using the TAB key) for prover commands, as well as access to the commands history.
<br><br> <img src="https://github.com/nasa/vscode-pvs/raw/master/vscode-pvs/screenshots/vscode-pvs-proof-explorer.gif" width="600">

<br>

## Structure
```
.
├── client                       // PVS Language Client (VSCode entry point)
│   └── src
│       ├── providers            // Client-side service providers
│       ├── views                // User interface components
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
        │     ├── pvsProofExplorer.ts          // Proof tree editor
        │     └── pvsPackageManager.ts         // Installation manager 
        ├── parser               // Parser grammar and scripts      
        ├── common               // Utility functions           
        ├── pvsCli.ts            // PVS Command Line Interface
        ├── pvsProcess.ts        // PVS process wrapper
        ├── pvsLisp.ts           // Lisp reader for parsing PVS responses
        └── pvsLanguageServer.ts // PVS Language Server implementation
```

<br>

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


## Contacts
* Paolo Masci (NIA) (paolo.masci@nianet.org)
* Cesar Munoz (NASA LaRC) (cesar.a.munoz@nasa.gov)

