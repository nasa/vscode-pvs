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
Yes, you we have a [5-minutes crash course](TUTORIAL.md) on VSCode-PVS, that introduces the basics functions of PVS, and a [NASA PVS Class]() that covers a broader range of topics.

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