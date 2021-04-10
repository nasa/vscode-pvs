import { XTermColorTheme } from "./colorUtils";

export enum XTermEvent {
    sendText = "sendText",
    proofExplorerBack = "proof-explorer.back", // F4
    proofExplorerForward = "proof-explorer.forward", //F6
    proofExplorerRun = "proof-explorer.run", // F5
    proofExplorerEdit = "proof-explorer.edit" // F2
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
    showHelpMessage = "showHelpMessage"
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
