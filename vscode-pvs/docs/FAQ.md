# **VSCode-PVS FAQ**
This document contains answers to common questions on VSCode-PVS, including troubleshooting techniques for basic problems.

If you don't find an answer to your question here or on [github](https://github.com/nasa/vscode-pvs/issues), you can open an issue on [github](https://github.com/nasa/vscode-pvs/issues) or [get in touch with us](mailto:paolo.masci@nianet.org).

<br>

-------
## General questions
-------

### **Which OS is supported?**
VSCode-PVS currently runs on Linux and MacOS. For Windows systems, you will need to install the extension in a virtual machine with Linux.

### **How do I start VSCode-PVS?**
VSCode-PVS will automatically start with Visual Studio Code.
If this is not happening, please go through the following checklist, to make sure you have installed all dependencies necessary to run VSCode-PVS:
- [NodeJS](https://nodejs.org/en/download) (v12.16.1 or greater) is installed
- [Java JDK](https://openjdk.java.net) (v1.8 or greater) is installed
- [PVS Allegro](http://www.csl.sri.com/users/owre/drop/pvs-snapshots) (v7.1.0 or greater) is installed
- [VSCode-PVS](https://github.com/nasa/vscode-pvs) is installed and enabled in Visual Studio Code

### **I'm new to PVS, is there a tutorial I can use to get started?**
Yes, you we have a [tutorial](TUTORIAL.md) on VSCode-PVS, that introduces the basics functions of PVS, and a [NASA PVS Class](https://shemesh.larc.nasa.gov/PVSClass2012/) that covers a broader range of topics.

### **I'm not familiar with Visual Studio Code, is there a quick tutorial?**
Yes, you can find tutorials on the Visual Studio Code website, see https://code.visualstudio.com/docs/editor/codebasics

<br>

-------
## Troubleshooting
-------

### **VSCode-PVS reports an error "pvs-server crashed", what can I do?**
An unexpected error has occurred in the server. 
You can try a 'soft restart' by pressing the `Reboot pvs-server` button located in the status bar.
If the problem persists, please close and reopen Visual Studio Code.

### **VSCode-PVS became not responsive, what can I do?**
These situations may occur when the reasoning engine has crashed. You can try to restart the reasoning engine with the `Reboot pvs-server` button located in the status bar. If the problem persists, please try restarting Visual Studio Code.

>Note 1: If VSCode-PVS became not responsive during a prover session, you can still save the current proof using the `Save` command from the menu in Proof Explorer.

>Note 2: There is an output channel `pvs-server` in Visual Studio Code. You can use it to inspect the raw output of PVS, including errors thrown by the reasoning engine, when errors occur.

### **A proof because a pop-up message "Typechecking files necessary to prove formula" does not go away, what can I do?**
An unexpected error might have occurred while processing one of the files necessary for running the proof. Please restart the reasoning engine with the `Reboot pvs-server` button located in the status bar and try again. If the problem persists, please open an issue on [github](https://github.com/nasa/vscode-pvs/issues) or [get in touch with us](mailto:paolo.masci@nianet.org).

### **The terminal reports an error "node does not exist", what does it mean?**
The error indicates that one of the required software packages is not installed on your system --- you need to install [NodeJS](https://nodejs.org/en/download).

### **Can I use Emacs plugins for Visual Studio Code together with VSCode-PVS?**
No, we recommend not to use Emacs plugins, as they may interfere with the PVS hot-keys (M-x command).

<br>

-------
## Functionalities
-------

### **How do I open PVS files and workspaces that are not shown in Visual Studio Code?**
The easiest way to open PVS files and workspaces is through PVS Workspace Explorer.
In the title of PVS Workspace Explorer there's a menu `...` where you can find `Open PVS File...` and `Open PVS Workspace...`.

Alternatively, you can use the standard Visual Studio Code functions to open files and folders (File -> Open). 

### **Can I develop my pvs theories on my `Desktop`, or in my pvs installation folder?**
**No, that's a bad idea.** The recommended way to proceed is to create a folder `workspaces` in your home directory, and develop your pvs theories under such folder.

### **Can I use PVS hot-keys combinations?**
Yes, you can use the following set PVS hot-keys combinations in VSCode-PVS:
- `M-x show-tccs` *(show proof obligations for the file open in the editor)*
- `M-x tc` *(typecheck the file open in the editor)*
- `M-x tcp` *(typecheck the file open in the editor and re-run all proofs in the file)*
- `M-x parse` *(parse the file open in the editor)*
- `M-x pr` *(start an interactive Prover session for the formula at the cursor location)*
- `M-x prt` *(re-run all proofs in the current theory)*
- `M-x pvsio` *(start an interactive PVSio evaluator session for the theory at the cursor location)*
- `M-x show-proof-summary` *(show proof summary)*
- `M-x vpf` *(view prelude file)*

Additional hot-keys for quick access to configuration settings:
- `M-x add-pvs-library` *(adds a folder to the vscode-pvs library path)*
- `M-x clear-pvs-library-path` *(removes all entries in the vscode-pvs library path)*
- `M-x pvs-library-path` *(shows the pvs library path specified in vscode-pvs settings)*
- `M-x reboot-pvs` *(reboots pvs-server)*
- `M-x install-pvs` *(starts as interactive wizard that allows to install or update PVS)*
- `M-x install-nasalib` *(starts an interactive wizard that allows to install NASALib)*
- `M-x update-nasalib` *(updates the installed version of NASALib)*
- `M-x set-pvs-path` *(sets the path to the PVS executables)*
- `M-x settings` *(shows vscode-pvs settings)*
- `M-x release-notes` *(shows vscode-pvs release notes)*

### **What is `M-x`, and how do I use it?**
`M-x` is a key combination obtained by pressing the `META` key and the `x` letter simultaneously.
The `META` key on Linux is the `Alt` key. On MacOS, it's usually the `option` key.

### **In the prover session, can I move the cursor with the mouse using point-click actions?**
Yes, point-click actions are enabled in the prover session when pressing the `META` key.

### **In the prover session, can enter or copy-paste a multi-line proof commands?**
Yes, but you need enclose your multi-line proof command within round brackets.

### **Can I edit a proof tree, e.g., copy/paste proof branches?**
Yes, PVS Proof Explorer is also a proof editor. You can right click anywhere in the proof tree, and perform the following operations: rename, cut, copy, paste, trim, delete, cut branch, paste branch.

### **Is there a way to save a proof from the prover prompt?**
VSCode-PVS will automatically save the proof at the end of the prover session. If a proof is already present for the formula being proved, VSCode-PVS will ask confirmation before overwriting the old proof. 

### **Can I save multiple proofs for the same theorem?**
No, the current version of VSCode-PVS allows you to save only one proof for each theorem.

