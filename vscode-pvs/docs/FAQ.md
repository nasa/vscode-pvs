# VSCode-PVS FAQ
This document contains answers to common questions on VSCode-PVS, including troubleshooting techniques for basic problems.

If you don't find an answer to your question here or on [github](https://github.com/nasa/vscode-pvs/issues), you can open an issue on [github](https://github.com/nasa/vscode-pvs/issues) or [get in touch with us](mailto:paolo.masci@nianet.org).

## Which OS is supported?
VSCode-PVS currently runs on Linux and MacOS. For Windows systems, you will need to install the extension in a virtual machine with Linux.

## How do I start VSCode-PVS?
VSCode-PVS will automatically start when you open a `.pvs` file in Visual Studio Code.
If this is not happening, please go through the following checklist, to make sure you have installed all dependencies necessary to run VSCode-PVS:
- [NodeJS](https://nodejs.org/en/download) (v12.16.1 or greater) is installed
- [Java JDK](https://openjdk.java.net) (v1.8 or greater) is installed
- [PVS Allegro](http://www.csl.sri.com/users/owre/drop/pvs-snapshots) (v7.1 or greater) is installed
- [VSCode-PVS](https://github.com/nasa/vscode-pvs) is installed and enabled in Visual Studio Code

## I'm new to PVS, is there a tutorial I can use to get started?
Yes, you we have a [5-minutes crash course](TUTORIAL.md) on VSCode-PVS, that introduces the basics functions of PVS, and a [NASA PVS Class](https://shemesh.larc.nasa.gov/PVSClass2012/) that covers a broader range of topics.

## Can I use the PVS hot-keys combinations?
Yes, you can use the following basic set PVS hot-keys combinations in VSCode-PVS:
- M-x show-tccs *(show proof obligations for the file open in the editor)*
- M-x tc *(typecheck the file open in the editor)*
- M-x parse *(parse the file open in the editor)*
- M-x pr *(start an interactive Prover session for the formula at the cursor location)*
- M-x pvsio *(start an interactive PVSio evaluator session for the theory at the cursor location)*

## Is the Emacs plugin for Visual Studio Code compatible with VSCode-PVS?
The Emacs plugin is compatible with VSCode-PVS, **but** the PVS hot-keys won't work because the Emacs plugin captures the initial sequence M-x. We are working on a solution to this problem. Please stay tuned for updates.

## VSCode-PVS became not responsive, what can I do?
This situation tipically occurs when the reasoning engine crashes into Lisp. You can try to restart the reasoning engine with the **Reboot pvs-server** button located in the status bar. If the problem persists, please try restarting Visual Studio Code.

>Note 1: If VSCode-PVS became not responsive during a prover session, you can still save the current proof using the 'Save' command from the menu in Proof Explorer.

>Note 2: There is an output channel 'pvs-server' in Visual Studio Code. You can use it to inspect the raw output of PVS, including errors thrown by the reasoning engine.

## A pop-up message indicates "Typechecking files necessary to prove formula", and VSCode-PVS seems to be stuck on it?
An unexpected error might have occurred while processing one of the files. Please restart the reasoning engine with the **Reboot pvs-server** button located in the status bar and try again. If the problem persists, please open an issue on [github](https://github.com/nasa/vscode-pvs/issues) or [get in touch with us](mailto:paolo.masci@nianet.org).

## Can I save more than one proof for a theorem?
No, the current version of VSCode-PVS allows you to save only one proof for each theorem.

## Is there a way to delete a proof?
Yes, using the functionalities of PVS Proof Explorer you can delete a proof. PVS Proof Explorer is located in the side panel, and shows the current proof tree. Right click on the root node of the proof tree, and select 'Delete'. Then, save the proof to make the change permanent.

## Can I edit a proof tree, e.g., copy/paste proof branches?
Yes, PVS Proof Explorer is also a proof editor. You can right click anywhere in the proof tree, and perform the following operations: rename, cut, copy, paste, trim, delete, cut branch, paste branch.

## Can I develop my pvs theories on my Desktop?
No, that's a bad idea. Please create a subfolder in your home directory, and develop your pvs theories in it.