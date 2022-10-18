import { XTermColorTheme } from "./colorUtils";

export enum XTermEvent {
    ready = "ready",
    sendText = "sendText",
    closeConsole = "closeConsole",
    didCopyText = "didCopyText",
    proofExplorerBack = "proof-explorer.back", // F4
    proofExplorerForward = "proof-explorer.forward", //F6
    proofExplorerRun = "proof-explorer.run", // F5
    proofExplorerEdit = "proof-explorer.edit", // F2
    click = "click"
};
export enum XTermCommands {
    write = "write",
    log = "log",
    focus = "focus",
    showPrompt = "showPrompt",
    updateHints = "updateHints",
    updateMathObjects = "updateMathObjects",
    clearScreen = "clearScreen",
    disableInput = "disableInput",
    enableInput = "enableInput",
    updateCommandHistory = "updateCommandHistory",
    updateHelp = "updateHelp",
    clearCommandLine = "clearCommandLine",
    showWelcomeMessage = "showWelcomeMessage",
    updateColorTheme = "updateColorTheme",
    showHelpMessage = "showHelpMessage",
    running = "running",
    autocompleteWithEnter = "autocompleteWithEnter",
    helpStar = "helpStar",
    helpVSCodePlot = "helpVSCodePlot"
};

export interface XTermMessage {
    command: string,
    data?: any
};

export interface UpdateCommandHistoryData { cmd: string, successHistory?: boolean }

export const interruptCommand: string = "ctrl+c";

export declare type SessionType = "prover" | "evaluator";

/**
 * Utility function, detects the color theme used in the terminal
 */
export function xTermDetectColorTheme (themeClass: string): XTermColorTheme {
    const theme: XTermColorTheme = /light/gi.test(themeClass) ? "light" : "dark";
    return theme;
}

// domain-specific prettyprinter for the prover console
export type PrettyPrinter = { cmd: string, options: string[] };
export type PrettyPrinterInfo = { file: string, language: string };