# Welcome to VSCode-PVS!
VSCode-PVS is a new integrated development environment for creating, evaluating and verifying PVS specifications. The environment redefines the way developers interact with PVS, and better aligns the PVS front-end to the functionalities provided by development environments used by software developers.


## Documentation
- [Quick reference guide](https://github.com/nasa/vscode-pvs/blob/master/vscode-pvs/docs/USER-INTERFACE.md) for the VSCode-PVS User Interface
- [FAQs](https://github.com/nasa/vscode-pvs/blob/master/vscode-pvs/docs/FAQ.md) on VSCode-PVS, including troubleshooting techniques for basic problems.
- Are you new to PVS? Try out our [tutorial](https://github.com/nasa/vscode-pvs/blob/master/vscode-pvs/docs/TUTORIAL.md)!
- Join the new [PVS group on Google](https://groups.google.com/g/pvs-group)


## Functionalities
- [Syntax highlighting](): PVS keywords and library functions are automatically highlighted.
- [Autocompletion and code snippets](): Tooltips suggesting function names and language keywords are automatically presented in the editor when placing the mouse over a symbol name. Code snippets are provided for frequent modeling blocks, e.g., if-then-else.
- [Hover information for symbol definitions](): Hover boxes providing information about identifiers are automatically displayed when the user places the cursor over an identifier.
- [Go-to definition](): Click on the name of the identifier while holding down the Ctrl key to jump to the location where the identifier is declared.
- [Peek definitions](): Symbol definitions can be shown in mini editors embedded in the current view.
- [Live diagnostics](): Parsing is automatically performed in the background, and errors are reported in-line in the editor. Problematic expressions are underlined with red wavy lines. Tooltips presenting the error details are shown when the user places the cursor over the wavy lines.
- [Outline View](): Interactive tree view showing the outline of the pvs file open in the editor. Point-and-click actions can be used to jump to definitions.
- [Workspace Explorer](): Interactive tree view showing all theories in the current workspace, name and status of theorems and typecheck conditions.
- [Proof Explorer](): Interactive tree view for viewing and editing the current proof.
- [Prover Terminal](): An integrated terminal for interacting with the theorem prover. Auto-completion is provided (using the TAB key) for prover commands, as well as access to the commands history.
- [Proof Mate](): Helper tool, designed to suggest proof commands that can be used to make progress with the current proof.
- [Prototype Builder](): Rapid prototyping toolkit, for building interactive visual simulations based on PVS specifications.


## ChangeLog
- [1.0.45 (2021.05.05)]()      
  -Added 'jump-to' function in proof-explorer  
  -Fixed installation under Linux      

- [1.0.44 (2021.04.26)]()      
  -Updated user interface guide   
  -Fixed minor bugs (parsing of comments in proof files, proof-explorer focus)  

- [1.0.43 (2021.04.19)]()    
  -Improved loading/unloading of pvs icons in file-explorer  
  -Fixed minor bugs ('show sequent', cut/paste, focus-active-node, 'Open with VSCode')  

- [1.0.42 (2021.04.13)]()    
  -Added new function to export proof tree as HTML/SVG file  
  -Improved prover/evaluator console (light color theme, key bindings PageUp/PageDown to scroll content) 
  -Improved syntax highlighting and inline command 'typecheck-file'  

- [1.0.41 (2021.03.31)]()    
  -Added new prover/evaluator console (integrated help, tooltips, brackets matching, multi-line editing)  
  -Improved architecture and performance  

- [1.0.40 (2021.03.08)]()    
  -Added new visual component: PVSio-web Prototype Builder   
  -Improved proof explorer (header actions, dialogs, icons)   
  -Improved prover terminal (auto-focus, help, interrupt)  

- [1.0.39 (2021.02.05)]()    
  -Improved quick user guide       
  -Fixed minor bugs (icon theme, visibility of proof-explorer)   

- [1.0.38 (2021.02.03)]()    
  -Improved autocompletion       
  -Added inline prove command for judgements   

- [1.0.37 (2021.02.02)]()  
  -Added new front-end to search NASALib definitions      

- [1.0.36 (2020.01.27)]()  
  -Added support for prover command 'comment'  
  -Improved rendering in proof-explorer  
  -Fixed rendering of proof commands in proof-explorer   
  -Fixed command 'help' in prover CLI  
  
- [1.0.35 (2020.01.19)]()  
  -Added new proof tree visualization  
  -Added new command shortcuts (M-x x-show-proof, M-x x-prove, M-x x-pr)  
  -Added controls for pausing a proof in proof-explorer  
  -Added icons in editor toolbar for core pvs functions (open workspace, typecheck, prove, pvsio, x-show-proof)  

- [1.0.34 (2020.12.14)]()  
  -Fixed issues with PVSio terminal and file renaming       

- [1.0.33 (2020.11.23)]()  
  -Added functionality to plot ground PVS expressions       

- [1.0.32 (2020.11.20)]()  
  -Improved feedback and boot sequence    

- [1.0.31 (2020.11.16)]()  
  -Fixed bug with proof-explorer (missing tooltip / proof state update)    

- [1.0.30 (2020.11.16)]()  
  -Fixed bug with summary produced by `M-x pri`
  -Fixed issue with saving proof files from .jprf to .prf   

- [1.0.29 (2020.11.12)]()  
  -Improved codelens  
  -Added rename theory / formula  

- [1.0.28 (2020.11.09)]()  
  -Added option for re-running only unproved proofs  
  -Improved Proof Explorer (prove-workspace, rewind)   

- [1.0.27 (2020.10.29)]()  
  -Improved dialogs and feedback for errors  
  -Added interrupt command (ctrl+c) to stop the execution of a proof command in the theorem prover  

- [1.0.26 (2020.10.21)]()  
  -Improved PVSio terminal  
  -Improved dialogs and icons  
  -Improved installation wizard  
  -Improved Proof Explorer (import/export proof, automatic backup of proof script)

- [1.0.25 (2020.09.23)]()  
  -Proof files are now saved using legacy .prf format  
  -Added support for prove-importchain (M-x pri)  
  -Added support for glassbox tactics  
  -Added vscode-pvs shortcuts (reinstall-pvs, reinstall-nasalib, set-pvs-path, settings, add-library-path)  
  -Added vscode-pvs settings for pvs-library-path  
  -Improved menus and feedback  

- [1.0.24 (2020.08.11)]()  
  -Improved autocompletion  
  -Improved support for multiline input in prover terminal  
  -Improved Proof Explorer  
  -Added pvs-server profiler  
  -Added support for proof commands "comment" and "help"  
  -Added wizard for automatic download of NASALib  

- [1.0.23 (2020.07.01)]()  
  -Added support for multi-line commands in the prover prompt  
  -Added support for automatic proof of TCCs (M-x tcp)  
  -Improved handling of 'undo' proof commands  
  -Introduced interlocking mechanism for safer interaction with the prover  
  -Improved Proof Explorer  

- [1.0.22 (2020.06.18)]()  
  -Added support for rerunning all proofs (M-x prt)  
  -Added support for viewing the prelude file (M-x vpf)  
  -Added support for proofLite scripts  

- [1.0.21 (2020.06.11)]()  
  -Improved hover tooltips  
  -Improved feedback & stability  
  -Added watchdog to prevent runaway situations with grind  
  -Added FAQs  

- [1.0.20 (2020.05.26)]()  
  -Improved Workspace Explorer  
  -Improved feedback & stability  

- [1.0.19 (2020.05.19)]()  
  -Introduced support for MacOS  

- [1.0.18 (2020.05.03)]()  
  -Improved Workspace Explorer  
  -Improved editor feedback for parsing/typechecking  
  -Introduced APIs for MacOS  

- [1.0.17 (2020.04.20)]()  
  -Improved Proof Mate functionalities (profiles, recommended commands, inline help)  
  -Improved Prover CLI (autocompletion for lemmas)  

- [1.0.16 (2020.04.06)]()  
  -Improved Proof Explorer functionalities (tooltips, proof-stepper)  
  -Improved Editor functionalities (autocompletion, codelens)  
  -Improved PVSio evaluator terminal (pvs syntax highlighting)  

- [1.0.15 (2020.03.12)]()  
  -New visual components: Proof Mate, Workspace Explorer (formerly, Theory Explorer)  
  -Added support for theory outline view (requires visual studio code >= 1.32.3)  
  -Introduced new pvs parser based on antlr4 (requires java development environment)  
  -Added support for hybrid programs (.hpvs files)  

- [1.0.14 (2019.11.26)]()  
  -Improved error handling in pvs-proxy  

- [1.0.13 (2019.10.25)]()  
  -New visual components: Proof Explorer, Sequent Viewer  
  -Revised architecture, to connect to the new XMLRPC server provided by PVS 7.0  
  -Improved PVS command line interface: autocompletion, syntax highlighting  
  -Improved status bar  
  -Installation wizard  

- [1.0.12 (2019.06.07)]()  
  -PVS Command Line Interface (auto-completion and history navigation for proof commands, syntax highlighting for PVS language)  
  -Improved Theory Explorer (proof status for theorems and tccs)  
  -Improved navigation of symbol definitions (typechecking information are now fed to the parser process)  
  -Improved status bar  

- [1.0.11 (2019.05.29)]()  
  -Improved Theory Explorer (navigation of theorems)  

- [1.0.10 (2019.05.24)]()  
  -Added support for view-proof and step-proof commands  
  -Improved feedback on status bar  

- [1.0.9 (2019.04.29)]()  
  -Performance improvement (parallel typechecking)  
  -Added support for automatic context change  
  -Code cleanup  

- [1.0.8 (2019.04.16)]()  
  -Improved code navigation (peek definitions)  
  -Added xmlrpc infrastructure necessary to support pvs-server  

- [1.0.7 (2019.04.09)]()  
  -Proof explorer front-end for pvs prover  
  -Improved syntax highlighting and autocompletion  
  -Autocompletion for M-x commands  

- [1.0.6 (2019.04.01)]()  
  -Added support for math symbols  
  -Added handling of .tccs and .ppe files  
  -Shortcuts for M-x tcp, M-x show-tccs  

- [1.0.5 (2019.03.19)]()  
  -Integrated terminals for PVSio and PVS Prover  
  -Shortcuts M-x tc, M-x pr, M-x pvsio  
  -Code folding for begin-end, if-endif, cond-endcond  

- [1.0.4 (2019.03.08)]()  
  -Navigation and typechecking of pvs theories in explorer view  

- [1.0.3 (2019.03.01)]()  
  -Overview of tccs in explorer view  
  -Shortcut C-t for typechecking files  
  -Improved PVS language server  

- [1.0.2 (2019.02.24)]()  
  -Snippets for pvs language (theory declaration, if-then-else, cond-case)  

- [1.0.1 (2019.02.22)]()  
  -Improved Language Server (buffered input)  

- [1.0.0 (2019.02.21)]()  
  -Auto-Completion for language keywords and identifiers  
  -Hover information for language keywords and identifiers  
  -Go-to definition  
  -Diagnostics for syntax errors  
  -Literate programming for executable expressions  


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

