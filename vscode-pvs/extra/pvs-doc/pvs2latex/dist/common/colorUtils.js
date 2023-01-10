"use strict";
/**
 * vscode-pvs colors.
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.colorText = exports.ansiColorText = exports.isPlainText = exports.getPlainText = exports.ansiColorRegex = exports.ansiColorCode = exports.htmlColorCode = exports.getColor = exports.ANSI_RESET = exports.ANSI_RESET_BACKGROUND = exports.ANSI_RESET_FOREGROUND = exports.ANSI_BACKGROUND = exports.ANSI_FOREGROUND = exports.ANSI_ESC = exports.getParenColor = exports.PvsColor = void 0;
// based on the 256 color scheme, see colors at https://misc.flogisoft.com/bash/tip_colors_and_formatting
var PvsColor;
(function (PvsColor) {
    PvsColor["blue"] = "blue";
    PvsColor["lightblue"] = "lightblue";
    PvsColor["darkblue"] = "darkblue";
    PvsColor["yellow"] = "yellow";
    PvsColor["darkyellow"] = "darkyellow";
    PvsColor["gold"] = "gold";
    PvsColor["green"] = "green";
    PvsColor["darkgreen"] = "darkgreen";
    PvsColor["red"] = "red";
    PvsColor["white"] = "white";
    PvsColor["black"] = "black";
    PvsColor["gray"] = "gray";
    PvsColor["darkgray"] = "darkgray";
    PvsColor["violet"] = "violet";
})(PvsColor = exports.PvsColor || (exports.PvsColor = {}));
;
// matching parens colors, based on style used in monaco-editor
// .monaco-editor .bracket-highlighting-0 { color: #ffd700; }
// .monaco-editor .bracket-highlighting-1 { color: #da70d6; }
// .monaco-editor .bracket-highlighting-2 { color: #179fff; }
function getParenColor(colorIndex) {
    switch (Math.abs(colorIndex) % 3) {
        case 0: {
            return PvsColor.gold;
        }
        case 1: {
            return PvsColor.violet;
        }
        case 2: {
            return PvsColor.lightblue;
        }
        default: {
            return PvsColor.gray;
        }
    }
}
exports.getParenColor = getParenColor;
exports.ANSI_ESC = "\x1b[";
exports.ANSI_FOREGROUND = "38;";
exports.ANSI_BACKGROUND = "48;";
exports.ANSI_RESET_FOREGROUND = "38;39;";
exports.ANSI_RESET_BACKGROUND = "48;49;";
exports.ANSI_RESET = `${exports.ANSI_ESC}0m`;
/**
 * Utility function, returns a color based on the theme
 */
function getColor(color, theme) {
    switch (color) {
        case PvsColor.yellow: {
            return theme === "dark" ? PvsColor.yellow : PvsColor.darkyellow;
        }
        case PvsColor.blue: {
            return theme === "dark" ? PvsColor.blue : PvsColor.darkblue;
        }
        case PvsColor.green: {
            return theme === "dark" ? PvsColor.green : PvsColor.darkgreen;
        }
        case PvsColor.gray: {
            return theme === "dark" ? PvsColor.gray : PvsColor.black;
        }
        default: {
            break;
        }
    }
    return color;
}
exports.getColor = getColor;
exports.htmlColorCode = {
    blue: "#00b6fc",
    lightblue: "lightblue",
    darkblue: "#117AD0",
    green: "mediumspringgreen",
    darkgreen: "darkgreen",
    yellow: "yellow",
    darkyellow: "#999900",
    gold: "gold",
    red: "#f26158",
    black: "#1e1e1e",
    white: "whitesmoke",
    gray: "gray",
    darkgray: "darkgray",
    violet: "violet"
};
// export const colorMap: { [ key in vscodeColor ]: string } = {
// 	blue: "#00adf4",
// 	yellow: "yellow",
// 	green: "#55b92d",
// 	red: "#f26158"
// }
exports.ansiColorCode = {
    blue: 39,
    lightblue: 51,
    darkblue: 26,
    yellow: 229,
    darkyellow: 202,
    gold: 220,
    green: 42,
    darkgreen: 22,
    red: 160,
    white: 254,
    black: 0,
    gray: 7,
    darkgray: 239,
    violet: 135 // violet
};
const ansiColor256 = {
    yellow: `5;${exports.ansiColorCode.yellow}`,
    darkyellow: `5;${exports.ansiColorCode.darkyellow}`,
    gold: `5;${exports.ansiColorCode.gold}`,
    blue: `5;${exports.ansiColorCode.blue}`,
    lightblue: `5;${exports.ansiColorCode.lightblue}`,
    darkblue: `5;${exports.ansiColorCode.darkblue}`,
    red: `5;${exports.ansiColorCode.red}`,
    white: `5;${exports.ansiColorCode.white}`,
    black: `5;${exports.ansiColorCode.black}`,
    green: `5;${exports.ansiColorCode.green}`,
    darkgreen: `5;${exports.ansiColorCode.darkgreen}`,
    gray: `5;${exports.ansiColorCode.gray}`,
    darkgray: `5;${exports.ansiColorCode.darkgray}`,
    violet: `5;${exports.ansiColorCode.violet}`,
};
exports.ansiColorRegex = /\x1b\[(\d;?)*m/g;
/**
 * Utility function, removes ansi sequences from the text
 */
function getPlainText(ctext) {
    if (ctext) {
        return ctext.replace(new RegExp(exports.ansiColorRegex), "");
    }
    return ctext;
}
exports.getPlainText = getPlainText;
/**
 * Utility function, checks if the provided text is plain text (i.e., without ansi escape sequences)
 */
function isPlainText(ctext) {
    if (ctext) {
        return !(new RegExp(exports.ansiColorRegex).test(ctext));
    }
    return true;
}
exports.isPlainText = isPlainText;
function ansiColorText(text, opt) {
    if (text) {
        opt = opt || {};
        let ansiCode = "";
        ansiCode += opt.bold ? `${exports.ANSI_ESC}1m` : "";
        ansiCode += opt.background && ansiColor256[opt.background] ?
            `${exports.ANSI_ESC}${exports.ANSI_BACKGROUND}${ansiColor256[opt.background]}m`
            : "";
        ansiCode += opt.foreground && ansiColor256[opt.foreground] ?
            `${exports.ANSI_ESC}${exports.ANSI_FOREGROUND}${ansiColor256[opt.foreground]}m`
            : "";
        ansiCode += text.toLocaleString();
        ansiCode += exports.ANSI_RESET; // reset attributes for the following text
        return ansiCode;
    }
    return text;
}
exports.ansiColorText = ansiColorText;
function colorText(text, opt) {
    if (text) {
        if (typeof opt === "object") {
            return ansiColorText(text, opt);
        }
        else {
            return ansiColorText(text, { foreground: opt });
        }
        //`\x1b[38;5;${ansiColorCode[color]}m${text.toLocaleString()}\x1b[0m`; // \x1b[0m resets all attributes -- ANSI encoding
    }
    return text;
}
exports.colorText = colorText;
//# sourceMappingURL=colorUtils.js.map