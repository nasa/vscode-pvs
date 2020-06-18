# VSCode-PVS FAQ
This document contains answers to common questions on VSCode-PVS, including troubleshooting techniques for basic problems.

If you don't find an answer to your question here or on [github](https://github.com/nasa/vscode-pvs/issues), you can open an issue on [github](https://github.com/nasa/vscode-pvs/issues) or [get in touch with us](mailto:paolo.masci@nianet.org).

-------
## **General questions**
-------

### **Which OS is supported?**
VSCode-PVS currently runs on Linux and MacOS. For Windows systems, you will need to install the extension in a virtual machine with Linux.

### **How do I start VSCode-PVS?**
VSCode-PVS will automatically start when you open a `.pvs` file in Visual Studio Code.
If this is not happening, please go through the following checklist, to make sure you have installed all dependencies necessary to run VSCode-PVS:
- [NodeJS](https://nodejs.org/en/download) (v12.16.1 or greater) is installed
- [Java JDK](https://openjdk.java.net) (v1.8 or greater) is installed
- [PVS Allegro](http://www.csl.sri.com/users/owre/drop/pvs-snapshots) (v7.1 or greater) is installed
- [VSCode-PVS](https://github.com/nasa/vscode-pvs) is installed and enabled in Visual Studio Code

### **I'm new to PVS, is there a tutorial I can use to get started?**
Yes, you we have a [5-minutes crash course](TUTORIAL.md) on VSCode-PVS, that introduces the basics functions of PVS, and a [NASA PVS Class](https://shemesh.larc.nasa.gov/PVSClass2012/) that covers a broader range of topics.

### **I'm not familiar with Visual Studio Code, is there a quick tutorial?**
Yes, you can find tutorials on the Visual Studio Code website, see https://code.visualstudio.com/docs/editor/codebasics

-------
## **Troubleshooting**
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


-------
## **Functionalities**
-------

### **Can I save multiple proofs for the same theorem?**
No, the current version of VSCode-PVS allows you to save only one proof for each theorem.

### **Is there a way to delete a proof?**
Yes, using the functionalities provided by PVS Proof Explorer you can delete a proof. PVS Proof Explorer is located in the side panel, and shows the current proof tree. Place the mouse on the root node of the proof tree, right click, and select 'Delete'. Then, save the proof to make the change permanent.

### **Is there a way to save a proof from the prover prompt?**
Yes, enter the following command at the prover prompt: `save`

### **Can I edit a proof tree, e.g., copy/paste proof branches?**
Yes, PVS Proof Explorer is also a proof editor. You can right click anywhere in the proof tree, and perform the following operations: rename, cut, copy, paste, trim, delete, cut branch, paste branch.

### **Can I develop my pvs theories on my `Desktop`, or in my pvs installation folder?**
No, that's a bad idea. The recommended way to proceed is to create a subfolder in your home directory, and develop your pvs theories in such subfolder.

## **How do I open files and folders that are not shown in Visual Studio Code?**
You can add a folder to Explorer (File -> Add Folder to Workspace...).
Alternatively, you can open a new Visual Studio Code window (File -> New Window) and then use the `Open Folder` button to open the folder you need. 

### **Can I use the PVS hot-keys combinations?**
Yes, you can use the following basic set PVS hot-keys combinations in VSCode-PVS:
- M-x show-tccs *(show proof obligations for the file open in the editor)*
- M-x tc *(typecheck the file open in the editor)*
- M-x parse *(parse the file open in the editor)*
- M-x pr *(start an interactive Prover session for the formula at the cursor location)*
- M-x prt *(re-run all proofs in the current theory)*
- M-x pvsio *(start an interactive PVSio evaluator session for the theory at the cursor location)*

### **What is `M-x`, and how do I use it?**
`M-x` is a key combination obtained by pressing the `META` key and the `x` letter simultaneously.
The `META` key on Linux is the `Alt` key. On MacOS, it's usually the `option` key.

### **In the prover session, can I move the cursor with the mouse using point-click actions?**
Yes, point-click actions are enabled in the prover session when pressing the `META` key.

### **In the prover session, can copy-paste multi-line text?**
No, this is not supported at this time. We are working on this, please stay tuned for updates.
