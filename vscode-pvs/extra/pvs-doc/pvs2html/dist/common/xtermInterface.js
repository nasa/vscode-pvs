"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.xTermDetectColorTheme = exports.interruptCommand = exports.XTermCommands = exports.XTermEvent = void 0;
var XTermEvent;
(function (XTermEvent) {
    XTermEvent["ready"] = "ready";
    XTermEvent["sendText"] = "sendText";
    XTermEvent["closeConsole"] = "closeConsole";
    XTermEvent["escapeKeyPressed"] = "escapeKeyPressed";
    XTermEvent["didCopyText"] = "didCopyText";
    XTermEvent["proofExplorerBack"] = "proof-explorer.back";
    XTermEvent["proofExplorerForward"] = "proof-explorer.forward";
    XTermEvent["proofExplorerRun"] = "proof-explorer.run";
    XTermEvent["proofExplorerEdit"] = "proof-explorer.edit";
    XTermEvent["click"] = "click";
})(XTermEvent = exports.XTermEvent || (exports.XTermEvent = {}));
;
var XTermCommands;
(function (XTermCommands) {
    XTermCommands["write"] = "write";
    XTermCommands["log"] = "log";
    XTermCommands["focus"] = "focus";
    XTermCommands["showPrompt"] = "showPrompt";
    XTermCommands["updateHints"] = "updateHints";
    XTermCommands["updateMathObjects"] = "updateMathObjects";
    XTermCommands["clearScreen"] = "clearScreen";
    XTermCommands["disableInput"] = "disableInput";
    XTermCommands["enableInput"] = "enableInput";
    XTermCommands["updateCommandHistory"] = "updateCommandHistory";
    XTermCommands["updateHelp"] = "updateHelp";
    XTermCommands["clearCommandLine"] = "clearCommandLine";
    XTermCommands["showWelcomeMessage"] = "showWelcomeMessage";
    XTermCommands["updateColorTheme"] = "updateColorTheme";
    XTermCommands["showHelpMessage"] = "showHelpMessage";
    XTermCommands["running"] = "running";
    XTermCommands["autocompleteWithEnter"] = "autocompleteWithEnter";
    XTermCommands["helpStar"] = "helpStar";
    XTermCommands["helpVSCodePlot"] = "helpVSCodePlot";
})(XTermCommands = exports.XTermCommands || (exports.XTermCommands = {}));
;
;
exports.interruptCommand = "ctrl+c";
/**
 * Utility function, detects the color theme used in the terminal
 */
function xTermDetectColorTheme(themeClass) {
    const theme = /light/gi.test(themeClass) ? "light" : "dark";
    return theme;
}
exports.xTermDetectColorTheme = xTermDetectColorTheme;
//# sourceMappingURL=xtermInterface.js.map