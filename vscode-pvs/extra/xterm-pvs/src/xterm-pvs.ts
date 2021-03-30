import { Terminal as XTerm } from 'xterm';
import { CommandDescriptor, CommandsMap, Position } from './common/serverInterface';
import * as colorUtils from './common/colorUtils';
import * as commandUtils from './common/commandUtils';
import { pvsColorTheme } from './common/languageKeywords';
import { interruptCommand, SessionType, UpdateCommandHistoryData, XTermEvent } from './common/xtermInterface';
import * as Backbone from 'backbone';
import * as Handlebars from 'handlebars';
import { HintsObject, MathObjects } from './common/commandUtils';

interface KeyEvent { key: string, domEvent: KeyboardEvent };
interface MatchBrackets { pos1?: Position, pos2: Position }; // if idx1 is not present, then bracket idx2 is mismatched

const MIN_POS: Position = { line: 1, character: 1 };

enum ContentEvent {
    rebase = "rebase",
    didAutocompleteContent = "didAutocompleteContent"
};
interface RebaseEvent {
    pos: Position
};

/**
 * Virtual content of the terminal, keeps track of the content of the cursor position
 */
export class Content extends Backbone.Model {
    // content of the terminal
    protected lines: string[] = [];

    // current cursor position
    protected pos: Position = { ...MIN_POS };

    // previous cursor position
    protected prevPos: Position = { ...MIN_POS };

    // minimum index: the position of the cursor should not go below this position (text before this position is considered read-only)
    protected base: Position = { ...MIN_POS };

    /**
     * Constructor
     */
    constructor () {
        super();
        console.log("[xterm-content] Init complete", { pos: this.pos, prevPos: this.prevPos, lines: this.lines });
    }

    /**
     * Internal function, updates previous cursor position
     */
    protected savePos (): void {
        // console.log("[xterm-content] savePos", { pos: this.pos, prevPos: this.prevPos, lines: this.lines });
        this.prevPos = {
            character: this.pos.character,
            line: this.pos.line
        };
    }

    /**
     * Clears the content and moves the cursor to its initial position (top-left corner)
     */
    deleteContent (): void {
        this.pos = { ...MIN_POS };
        this.prevPos = { ...MIN_POS };
        this.base = { ...MIN_POS };
        this.lines = [];
    }

    /**
     * get current cursor position, one-based cols and lines
     */
    cursorPosition (): Position {
        return {
            character: this.pos.character,
            line: this.pos.line
        };
    }

    /**
     * Utility function, returns true if the cursor is located at the home position, i.e., at the base position
     */
    cursorIsAtHomePosition (): boolean {
        return this.pos.line === this.base.line &&
            this.pos.character === this.base.character;
    }

    /**
     * get previous cursor position, one-based cols and lines
     */
    previousCursorPosition (): Position {
        return {
            character: this.prevPos.character,
            line: this.prevPos.line
        };
    }

    /**
     * Returns max line number
     */
    maxLineNumber (): number {
        return this.lines.length;
    }

    /**
     * Returns the entire content
     */
    text (): string {
        return this.lines.join("\n");
    }

    /**
     * Returns the command entered at the input line (i.e., everything after the base index)
     */
    command (opt?: { beforeCursor?: boolean }): string {
        opt = opt || {};
        return opt.beforeCursor ? this.textBetween(this.base, this.pos)
            : this.textAfter(this.base);
    }

    /**
     * Replaces the content of the command line with the provided command
     */
    setCommand (cmd: string, opt?: { cursorToHome?: boolean }): void {
        this.clearCommandLine();
        this.writeData(cmd);
        if (opt?.cursorToHome) {
            this.cursorTo(this.base);
        }
        this.trigger(ContentEvent.didAutocompleteContent);
    }

    /**
     * get line text at given position (position is 1-based)
     */
    textLineAt (pos: Position | number): string {
        const lineIndex: number = typeof pos === "number" ? (pos - 1) : (pos?.line - 1);
        if (lineIndex >= 0 && lineIndex < this.lines.length) {
            const textLine: string = this.lines[lineIndex];
            // console.log("[xterm-content] textLineAt", { pos, textLine, lines: this.lines });
            return textLine;
        }
        return "";
    }

    /**
     * Returns the line of text after the cursor, including the character at the cursor position
     */
    textLineAfter (pos: Position): string {
        const colIndex: number = pos?.character - 1;
        const textLine: string = this.textLineAt(pos);
        if (colIndex >= 0 && colIndex < textLine.length) {
            const textLineAfter: string = textLine.substr(colIndex);
            // console.log("[xterm-content] textLineAfter", { pos, textLine, textLineAfter, lines: this.lines });
            return textLineAfter;
        }
        return "";
    }

    /**
     * Returns the line of text before the cursor
     * from the start of the line (which could be behind base)
     * **excluding** the character at the cursor position
     */
    textLineBefore (pos: Position): string {
        const colIndex: number = pos?.character - 1;
        const textLine: string = this.textLineAt(pos);
        if (colIndex >= 0) {
            return textLine.substr(0, colIndex);
        }
        console.warn("[xterm-content] Warning, textLineBefore position column out of range", { pos, textLine, lines: this.lines });
        return "";
    }

    /**
     * Returns the lines below the cursor, **excluding** the character at the cursor position
     */
    linesBelow (pos: Position): string[] {
        const lineIndex: number = pos?.line - 1;
        if (lineIndex >= 0) {
            return this.lines.slice(lineIndex + 1);
        }
        return [];
    }

    /**
     * Returns the lines of text before the cursor, **excluding** the line of the cursor
     */
    linesAbove (pos: Position): string[] {
        const lineIndex: number = pos?.line - 1;
        if (lineIndex >= 0) {
            return this.lines.slice(0, lineIndex);
        }
        return [];
    }

    /**
     * Returns the lines below the cursor, **excluding** first and last lines
     */
    linesBetween (pos1: Position, pos2: Position): string[] {
        const lineIndex1: number = pos1?.line; // exclude first line
        const lineIndex2: number = pos2?.line - 1;
        if (lineIndex1 >= 0 && lineIndex2 >= 0 && lineIndex2 >= lineIndex1) {
            return this.lines.slice(lineIndex1, lineIndex2);
        }
        return [];
    }
    

    /**
     * Returns text between positions pos1 pos2. Character at pos1 is included. Character at pos2 is excluded
     */
    textBetween (pos1: Position, pos2: Position): string {
        if (pos1 && pos2) {
            const line1: string = this.textLineAfter(pos1);
            if (pos1.line === pos2.line && pos2.character >= pos1.character) {
                return line1.substring(0, pos2.character - pos1.character);
            }
            if (pos2.line > pos1.line) {
                const line2: string = this.textLineBefore(pos2);
                const linesBetween: string[] = this.linesBetween(pos1, pos2);
                return line1 + "\n" 
                    + (linesBetween.length ? linesBetween.join("\n") + "\n" : "") 
                    + line2;
            }
        }
        return "";
    }

    /**
     * Returns the entire text after the cursor
     * **including** the character at the cursor position
     */
    textAfter (pos: Position): string {
        const currLine: string = this.textLineAfter(pos);
        const linesAfter: string[] = [ currLine ].concat(this.linesBelow(pos));
        return linesAfter.join("\n");
    }
        
    /**
     * Returns the entire text before the cursor
     * from the start of the line (which could be behind base)
     * and **excluding** the character at the cursor position
     */
    textBefore (pos: Position): string {
        const currLine: string = this.textLineBefore(pos);
        const linesBefore: string[] = this.linesAbove(pos).concat([ currLine ]);
        return linesBefore.join("\n");
    }

    /**
     * Rebase the index, useful to make the text before the cursor read-only
     */
    rebase (opt?: { prompt?: string }): void {
        // keep only the last line, which contains the ready prompt
        if (opt?.prompt) {
            if (this.lines.length) {
                this.lines = [ opt.prompt ];
                this.pos = {
                    line: 1,
                    character: this.lines[0].length + 1
                };
                this.prevPos = {
                    line: 1,
                    character: this.lines[0].length + 1
                };
            }
        }
        this.base = {
            line: this.pos.line,
            character: this.pos.character
        };
        const evt: RebaseEvent = { pos: this.pos };
        this.trigger(ContentEvent.rebase, evt)
        // console.log("[xterm-content] rebase", { base: this.base, pos: this.pos, prevPos: this.prevPos, lines: this.lines, command: this.command() });
    }

    /**
     * Trim content, to discard old lines that are not needed anymore
     */
    trimLines (n: number): void {
        if (n > 0) {
            this.lines = this.lines.slice(n);
            this.base.line = this.base.line > n ? this.base.line - n : 0;
            this.prevPos.line = this.prevPos.line ? this.prevPos.line - n : 0;
            this.pos.line = this.pos.line > n ? this.pos.line - n : 0;
        }
    }

    /**
     * writes text at the cursor position
     */
    writeData (data: string): void {
        if (data) {
            // remove ansi codes before saving the content
            const plainText: string = colorUtils.getPlainText(data);
            const newText: string = plainText.replace(/\r?\n/g, "\n").replace(/\r\n?/g, "\n");
            const textBeforeCursor: string = this.textBefore(this.pos) + newText;
            const textAfterCursor: string = this.textAfter(this.pos);
            this.lines = (textBeforeCursor + textAfterCursor).split("\n");
            // move cursor at the end of the appended text
            this.savePos();
            this.pos.line = textBeforeCursor.split("\n").length || MIN_POS.line;
            this.pos.character = this.endCol(this.pos.line) + 1;
            console.log("[xterm-content] writeData", { textAfterCursor, textBeforeCursor, newText, pos: this.pos, prevPos: this.prevPos, lines: this.lines, command: this.command() });
        }
    }

    /**
     * autocompletes the current input with the provided substitution
     */
    autocomplete (currentInput: string, substitution: string): void {
        if (substitution) {
            const textBeforeCursor: string = this.textBefore(this.pos);
            const textAfterCursor: string = this.textAfter(this.pos);
            const completedText: string = textBeforeCursor.substring(0, textBeforeCursor.length - currentInput.length) + substitution;
            this.lines = (completedText + textAfterCursor).split("\n");
            // move cursor at the end of the appended text
            this.savePos();
            this.pos.line = completedText.split("\n").length || MIN_POS.line;
            this.pos.character = completedText.split("\n")[this.pos.line - 1].length + 1;
            console.log("[xterm-content] autocomplete", { textAfterCursor, textBeforeCursor, completedText, pos: this.pos, prevPos: this.prevPos, lines: this.lines, command: this.command() });
            this.trigger(ContentEvent.didAutocompleteContent);
        }
    }

    /**
     * Returns the text at the cursor position
     */
    charAtCursorPosition (): string {
        return this.charAt(this.pos);
    }

    /**
     * Returns the text at the given position
     */
    charAt (pos: Position): string {
        const lineIndex: number = pos?.line - 1;
        if (lineIndex >= 0 && lineIndex < this.lines.length) {
            const selectedLine: string = this.lines[lineIndex];
            const colIndex: number = pos?.character - 1;
            if (colIndex >= 0 && colIndex < selectedLine.length) {
                return selectedLine[colIndex];
            }
        }
        return "";
    }

    /**
     * Deletes the character at the given position
     */
    deleteCharAt (pos: Position): string {
        const lineIndex: number = pos?.line - 1;
        if (lineIndex >= 0 && lineIndex < this.lines.length) {
            const selectedLine: string = this.lines[lineIndex];
            const colIndex: number = pos?.character - 1;
            if (colIndex >= 0 && colIndex < selectedLine.length) {
                const deleted: string = selectedLine[colIndex];
                const textLine: string = selectedLine.substring(0, colIndex) + selectedLine.substr(colIndex + 1);
                this.lines[lineIndex] = textLine;
                return deleted;
            }
        }
        return "";
    }

    /**
     * Inserts the character at the given position and moves the cursor after the inserted character
     */
    insertCharAt (pos: Position, char: string): boolean {
        const lineIndex: number = pos?.line - 1;
        if (lineIndex >= 0 && lineIndex < this.lines.length) {
            const colIndex: number = pos?.character - 1;
            if (colIndex >= 0) {
                const textLineBefore: string = this.textLineBefore(pos);
                const textLineAfter: string = this.textLineAfter(pos);
                const textLine: string = textLineBefore + char + textLineAfter;
                this.lines[lineIndex] = textLine;
                this.savePos();
                this.pos.character++;
                return true;
            }
        }
        console.warn("[xterm-content] Warning: line out of range @insertCharAt", { pos, char, lines: this.lines });
        return false;
    }

    /**
     * Deletes the current content of the command line
     */
    clearCommandLine (): void {
        const prompt: string = this.textLineBefore(this.base); // this captures the prompt
        this.lines = [ prompt ];
        this.pos = {
            line: 1,
            character: this.lines[0].length + 1
        };
        this.prevPos = {
            line: 1,
            character: this.lines[0].length + 1
        };
    }

    /**
     * Cursor right. Returns true if the cursor could be moved, based on the current content and base marker.
     */
    onArrowRight (): boolean {
        const currentLine: string = this.textLineAt(this.pos);
        if (this.pos.character <= currentLine.length) { // the cursor can be moved beside the end of the line
            this.savePos();
            this.pos.character++;
            return true;
        }
        if (this.pos.line < this.maxLineNumber()) {
            // move one line below
            this.savePos();
            this.pos.line++;
            this.pos.character = this.startCol(this.pos.line);
            console.log("[xterm-content] moving to next line", {
                pos: this.pos, 
                lines: this.lines
            });
            return true;
        }
        console.log("[xterm-content] arrow right [reached text limit]", {
            pos: this.pos, 
            lines: this.lines
        });
        return false;
    }

    /**
     * Internal function, returns the start column number for a given line 
     */
    protected startCol(line: number): number {
        return (line === this.base.line) ?
            this.base.character
                : MIN_POS.character;
    }

    /**
     * Internal function, returns the end column number for a given line
     */
    protected endCol (line: number, opt?: { allowBesideEnd?: boolean }): number {
        opt = opt || {};
        const lineIndex: number = line - 1;
        if (lineIndex < this.lines.length) {
            let col: number = this.lines[lineIndex].length;
            if (col && opt.allowBesideEnd) {
                col++;
                // console.log("[xterm-content] endCol beside line end", { col });
            }
            // the line could be empty, and we need 1-based column
            if (col < MIN_POS.character) {
                col = MIN_POS.character;
                // console.log("[xterm-content] endCol takes MIN_POS", { col });
            }
            // this could be the base line
            if (line === this.base.line && col < this.base.character) {
                col = this.base.character;
                // console.log("[xterm-content] endCol takes base", { col });
            }
            return col;
        }
        console.warn("[xterm-content] Warning: line index out of bounds @endCol", { line });
        return opt.allowBesideEnd ? MIN_POS.character + 1 : MIN_POS.character;
    }

    /**
     * Cursor left. Returns true if the cursor could be moved, based on the current content.
     */
    onArrowLeft (): boolean {
        console.log("[xterm-content] arrow left");
        this.savePos();
        const minCol: number = this.startCol(this.pos.line);
        if (this.pos.character > minCol) {
            this.pos.character--;
            console.log("[xterm-content] arrow left moves one char back", { pos: this.pos, prev: this.prevPos, base: this.base, lines: this.lines });
            return true;
        }
        if (this.pos.line > this.base.line) {
            // move one line up
            this.pos.line--;
            this.pos.character = this.endCol(this.pos.line, { allowBesideEnd: true }); // move beside the end of the line
            console.log("[xterm-content] arrow left goes to prev line", { pos: this.pos, prev: this.prevPos, base: this.base, lines: this.lines });
            return true;
        }
        console.log("[xterm-content] arrow left [reached base marker or base line]", { pos: this.pos, prev: this.prevPos, base: this.base, lines: this.lines });
        return false;
    }

    /**
     * Cursor up. Returns true if the cursor could be moved, based on the current content.
     */
    onArrowUp (): boolean {
        if (this.pos.line > this.base.line) {
            this.savePos();
            // move one line up
            this.pos.line = this.pos.line - 1;
            // try to keep the same col
            const maxCol: number = this.endCol(this.pos.line, { allowBesideEnd: true }); // can move beside the end of the line
            const minCol: number = this.startCol(this.pos.line); // can move beside the end of the line
            this.pos.character = this.pos.character > maxCol ? maxCol
                : this.pos.character < minCol ? minCol
                    : this.pos.character;
            return true;
        }
        // console.log("[xterm-content] arrow up [reached base marker or base line]", { pos: this.pos, prev: this.prevPos, base: this.base, lines: this.lines });
        return false;
    }

    /**
     * Cursor down. Returns true if the cursor could be moved, based on the current content.
     */
    onArrowDown (): boolean {
        if (this.pos.line < this.lines.length) {
            this.savePos();
            // move one line down
            this.pos.line++;
            // try to keep the same col
            const maxCol: number = this.endCol(this.pos.line, { allowBesideEnd: true }); // can move beside the end of the line
            const minCol: number = this.startCol(this.pos.line); // can move beside the end of the line
            this.pos.character = this.pos.character > maxCol ? maxCol
                : this.pos.character < minCol ? minCol
                    : this.pos.character;
            return true;
        }
        // console.log("[xterm-content] arrow up [reached base marker or base line]", { pos: this.pos, prev: this.prevPos, base: this.base, lines: this.lines });
        return false;
    }
    /**
     * Backspace. Returns true if the cursor could be moved, based on the current content.
     */
    onBackspace (): boolean {
        // backspace is obtained with arrow left + delete
        const success: boolean = this.onArrowLeft();
        if (success) {
            this.onDelete();
            // console.log("[xterm-content] onBackspace", { base: this.base, pos: this.pos, lines: this.lines });
            return true;
        }
        // console.log("[xterm-content] onBackspace [reached base marker or base line]", { base: this.base, pos: this.pos, lines: this.lines });
        return false;
    }

    /**
     * Delete. Returns true if the cursor could be moved, based on the current content.
     */
    onDelete (): boolean {
        // remove character at cursor position: delete line
        const lineIndex: number = this.pos.line - 1;
        if (lineIndex >= 0 && lineIndex < this.lines.length) {
            // corner cases: line empty
            if (this.lines[lineIndex].length === 0) {
                // delete this line
                this.lines = this.lines.slice(0, lineIndex).concat(this.lines.slice(lineIndex + 1));
                // console.log("[xterm-content] onDelete has removed a text line", { base: this.base, pos: this.pos, lines: this.lines });
                return true;
            }
            // corner case: line not empty but cursor besides the end of the line: merge and pull up next line
            if (this.pos.character === this.lines[lineIndex].length + 1
                    && (lineIndex + 1) < this.lines.length) {
                // merge and pull up next line
                const merge: string = this.lines[lineIndex] + this.lines[lineIndex + 1];
                this.lines = this.lines.slice(0, lineIndex).concat(this.lines.slice(lineIndex + 1));
                this.lines[lineIndex] = merge;
                // console.log("[xterm-content] onDelete has merged and pulled up a text line", { base: this.base, pos: this.pos, lines: this.lines });
                return true;
            }
            // else, simple case, delete char at pos
            const deleted: string = this.deleteCharAt(this.pos);
            // console.log("[xterm-content] onDelete", { deleted, base: this.base, pos: this.pos, lines: this.lines });
            return true;
        }
        return false;
    }

    /**
     * Enter key.
     */
    onEnter (): boolean {
        const textBefore: string = this.textBefore(this.pos);
        const textAfter: string = this.textAfter(this.pos);
        const newLines: string = textBefore + "\n" + textAfter;
        this.lines = newLines.split("\n");
        this.savePos();
        // move cursor to beginning of next line
        this.pos.line = this.pos.line + 1;
        this.pos.character = this.startCol(this.pos.line);
        console.log("[xterm-content] onEnter", { textBefore: textBefore.split("\n"), textAfter, pos: this.pos, prevPos: this.prevPos, lines: this.lines });
        return true;
    }

    /**
     * Generic handler for printable characters: inserts character at position index
     */
    onChar (c: string): boolean {
        console.log("[onChar]", { c, pos: this.pos, prevPos: this.prevPos });
        return this.insertCharAt(this.pos, c);
    }

    /**
     * Moves the cursor to a given position
     */
    cursorTo (pos: Position): boolean {
        const line: number =
            pos?.line < this.base.line ? this.base.line
            : pos?.line > this.lines.length? this.lines.length
            : pos.line;
        const maxCol: number = this.endCol(line, { allowBesideEnd: true }); // can move beside the end of the line
        const minCol: number = this.startCol(line); // can move beside the end of the line
        const character: number =
            pos?.character < minCol ? minCol
            : pos?.character > maxCol ? maxCol
            : pos.character;
        if (line !== this.pos.line || character !== this.pos.character) {
            this.savePos();
            this.pos = {
                line,
                character
            };
            return true;
        }
        return false;
    }

    /**
     * Updates the content of the terminal based on key press events
     */
    updateContent (e: KeyEvent): boolean {
        // console.log("[xterm-content] updateContent", { e })
        const ev: KeyboardEvent = e.domEvent;
        const key: string = e.key;
        let keypressRegistered: boolean = false;

        switch (ev.key) {
            case "Enter": {
                keypressRegistered = this.onEnter();
                break;
            }
            case "Backspace": {
                keypressRegistered = this.onBackspace();
                break;
            }
            case "Delete": {
                keypressRegistered = this.onDelete();
                break;
            }
            case "Escape": {
                keypressRegistered = true;
                break;
            }
            case "Tab": {
                // tabs are used to trigger autocompletion
                break;
            }
            case "ArrowUp": {
                keypressRegistered = this.onArrowUp();
                break;
            }
            case "ArrowDown": {
                keypressRegistered = this.onArrowDown();
                break;
            }
            case "ArrowLeft": {
                keypressRegistered = this.onArrowLeft();
                break;
            }
            case "ArrowRight": {
                keypressRegistered = this.onArrowRight();
                break;
            }
            default: {
                keypressRegistered = this.onChar(key);
                break;
            }
        }
        return keypressRegistered;
    }

    /**
     * Match brackets
     */
    matchBrackets (bracket?: Bracket): MatchBrackets {
        const b1: Bracket = bracket || "(";
        const b2: Bracket = getB2(b1);
        const inputBeforeCursor: string = this.command({ beforeCursor: true });
        // console.log("[xterm-pvs] matchBrackets", { inputBeforeCursor });
        if (inputBeforeCursor?.endsWith(b2)) {
            const pos2: Position = {
                character: this.pos.character - 1,
                line: this.pos.line
            }
            const idx1: number = this.findMatchingOpenBracketIndex(inputBeforeCursor);
            if (idx1 >= 0) {
                const textBeforeIdx1: string = inputBeforeCursor.substring(0, idx1);
                const lineIndex: number = textBeforeIdx1.split("\n").length - 1;
                const padding: number = lineIndex > 0 ? textBeforeIdx1.lastIndexOf("\n") : 0;
                const character: number = lineIndex === 0 ? 
                    idx1 + this.base.character 
                        : idx1 - padding;
                const line: number = this.base.line + lineIndex;
                const ans: MatchBrackets = {
                    pos1: { character, line }, 
                    pos2
                };
                // console.log("[xterm-content] matchBrackets", { ans, lineIndex, base: this.base, idx1, padding });
                return ans;
            }
            return { pos2 };
        }
        return null;
    }
    
    /**
     * Internal function, finds matching bracket
     */
    protected findMatchingOpenBracketIndex (text: string): number {
        let par: number = 0;
        let quotes: number = 0;
        for (let i = 0; i < text.length; i++) {
            const pos: number = text.length - 1 - i;
            switch (text[pos]) {
                case "(": {
                    if (quotes % 2 === 0) { par++; }
                    if (par === 0) {
                        return pos;
                    }
                    break;
                }
                case ")": {
                    if (quotes % 2 === 0) { par--; }
                    break;
                }
                case '"': {
                    quotes++;
                    break;
                }
                default: {
                    break;
                }
            }
        }
        return -1;
    }

}

export type ModKeys = {
    alt: boolean,
    ctrl: boolean
    meta: boolean
};

const MIN_VIEWPORT_COLS: number = 80;
const MIN_VIEWPORT_ROWS: number = 8;

export type Bracket = "(" | ")" | "[" | "]" | "{" | "}";

/**
 * Utility function, returns the character for the second bracket, based on the first bracket
 */
function getB2 (b1: Bracket): Bracket {
    return b1 === "(" ? ")" 
        : b1 === "[" ? "]" 
        : "}";
};

const tooltipTemplate: string = `<div style="width:100%; cursor:pointer;">
{{#each hints}}
<div class="autocompletion-item p-0 m-0 {{#if @first}}selected highlighted{{/if}} index-{{@index}}">{{this}}</div>
{{/each}}
</div>`;
const tooltipStyle: string = `<style>
.tooltip.show {
    opacity:1 !important;
}
.tooltip-inner {
    max-height: 100px; 
    min-width: 300px;
    overflow:auto;
    font-size:12px;
    font-family:monospace;
    text-align:left;
    cursor:default;
    background:#1e1e1e;
    border: 1px solid;
    white-space: nowrap;
}
.arrow::before {
    border-bottom-color: white !important;
    border-top-color: white !important;
    display: none;
}
.selected {
    background: #004775 !important;
    color: white !important;
}
.highlighted {
    background: #292d2e;
    color: white;
}
</style>`;
const cursorStyle: string = `<style>
@keyframes pulser {
    0% { opacity: 1; }
    70% { opacity: 0.6; }
}
.xterm-cursor-layer {
    animation: pulser 2s linear infinite !important;
}
</style>`;

const terminalHelpTemplate: string = `
<style>
.terminal-help .cmd {
    color: #00b0ff;
}
.terminal-help .description {
    padding-left: 10px;
}
.terminal-help .key {
    color: gold;
}
.pl73 {
    padding-left:73px;
}
</style>
{{#if description}}
<span class="cmd">({{cmd}})</span><span class="description">{{description}}</span>
<div class="syntax">Syntax: ({{syntax}})</div>
{{#each optionals}}
{{#if @first}}<div class="optionals">Optionals: <span class="key">{{@key}}</span> {{this}}</div>
{{else}}      <div class="optionals">           <span class="key pl73">{{@key}}</span> {{this}}</div>{{/if}}
{{/each}}
{{#if note}}
<div class="optionals">Note: {{note}}</div>
{{/if}}
{{/if}}
`;

export enum AutocompleteEvent {
    didAutocomplete = "didAutocomplete"
};
export interface DidAutocompleteEvent {
    substitution: string,
    currentInput: string
};

/**
 * Utility class for command line history
 */
 export class History {
    // list of previous commands entered at the command line
    protected history: string[] = [];
    // current command being entered at the command line
    protected current: string = "";
    // index identifying current of previous command, -1 means current command, 0...history.length means previous command
    protected index: number = -1;

    // list of commands entered at the command line and accepted by the prover -- this is a subset of the history
    protected successHistory: string[] = [];

    /**
     * Utility function, pushes the given command to the history, if the command is not present alreayd.
     * Additionally, the function clears the current command and resets the index
     */
    push (cmd: string, opt?: { successHistory?: boolean }): void {
        if (cmd) {
            opt = opt || {};
            let elem: string = cmd.trim().replace(/\n/g, " ");
            if (elem.startsWith("(") && elem.endsWith(")")) {
                elem = elem.substring(1, elem.length - 1).trim();
            }
            if (opt.successHistory) {
                if (!this.successHistory.includes(elem)) {
                    this.successHistory = [ elem ].concat(this.successHistory);
                }
            } else {
                if (!this.history.includes(elem)) {
                    this.history = [ elem ].concat(this.history);
                }
            }
            this.current = "";
            this.index = -1;
        }
    }

    /**
     * Utility function, returns the size of the history
     */
    size (): number {
        return this.history.length;
    }

    /**
     * clear the entire history and the current command
     */
    clear (): void {
        this.history = [];
        this.current = "";
        this.index = -1;
    }

    /**
     * Updates the current command with the provided input
     */
    updateCurrentCommand (cmd: string): void {
        this.current = cmd;
    }

    /**
     * Returns the previous command in the history
     */
    prev (): string {
        if (this.history.length) {
            this.index = (this.index + 1 < this.history.length) ? this.index + 1 : this.index;
            return `(${this.history[this.index]})`;
        }
        return this.current;
    }

    /**
     * Returns the next command in the history
     */
    next (): string {
        if (this.index - 1 >= 0 && this.history.length) {
            this.index--;
            return `(${this.history[this.index]})`;
        }
        this.index = -1;
        return this.current;
    }

    /**
     * Returns the history of commands
     */
    getHistory (opt?: { match?: string }): string[] {
        opt = opt || {};
        if (opt.match !== undefined) {
            return opt.match ? this.history.filter((c: string) => c.toLocaleLowerCase().startsWith(opt.match))
                : [];
        }
        return this.history;
    }

    /**
     * Returns the history of commands successfully executed by the prover
     */
    getSuccessHistory (opt?: { match?: string }): string[] {
        opt = opt || {};
        if (opt.match !== undefined) {
            return opt.match ? this.successHistory.filter((c: string) => c.toLocaleLowerCase().startsWith(opt.match))
                : [];
        }
        return this.history;
    }

}

/**
 * Utility class for autocomplete
 */
export class Autocomplete extends Backbone.Model {
    // pointer to the current terminal content
    protected content: Content;

    // session type
    protected sessionType: SessionType;

    // hints data
    protected hintsObject: HintsObject = {};
    protected mathObjects: MathObjects = {};
    protected currentHints: string[] = [];
    protected currentInput: string = "";

    // command history
    history: History = new History();

    /**
     * Constructor
     */
    constructor (content: Content, sessionType: SessionType) {
        super();
        this.content = content;
        this.sessionType = sessionType;
        this.installHandlers();
    }
    /**
     * Internal function, install relevant handlers for key presses
     */
    protected installHandlers (): void {
        // the keydown handler on document is necessary to capture keypresses when the tooltip is selected
        $(document).on("keydown", (evt: JQuery.KeyDownEvent) => {
            // console.log("[xterm-autocomplete] keydown", { target: evt.target });
            switch (evt.key) {
                case "ArrowUp":
                case "ArrowDown":
                case "ArrowLeft":
                case "ArrowRight":
                case "Enter": 
                case "Escape": {
                    this.autocompleteOnKeyPress(evt);
                    break;
                }
                default: {
                    break;
                }
            }
        });
    }
    /**
     * Updates hints data for autocompletion
     */
    updateHints (hints: HintsObject): void {
        this.hintsObject = hints;
    }
    /**
     * Updates mathObjects for autocompletion
     */
    updateMathObjects (mathObjects: MathObjects): void {
        this.mathObjects = mathObjects;
    }
    /**
     * Shows a tooltip with the provided hints
     */
    showTooltip (hints: string[]): void {
        if (!this.sameHints(hints, this.currentHints)) {
            this.deleteTooltips();
            this.currentHints = hints || [];
        }
        this.renderTooltip();
    }
    /**
     * Updates the command history with the provided command, if the command is not one of the hints already known to autocomplete and history
     */
    updateCommandHistory (cmd: string, opt?: { successHistory?: boolean }): void {
        if (cmd) {
            // split commands in the case the user has entered multiple commands at the same time, e.g., (skosimp*)(grind)
            const cmdArray: string[] = commandUtils.splitCommands(cmd);
            if (cmdArray?.length) {
                const hints: string[] = this.getHints({ fullSet: true });
                for (let i = 0; i < cmdArray.length; i++) {
                    let elem: string = cmdArray[i];
                    if (elem.startsWith("(") && elem.endsWith(")")) {
                        elem = elem.substring(1, elem.length - 1).trim();
                    }
                    if (cmd && !hints?.includes(elem)) {
                        this.history.push(elem, opt);
                    }
                }
            }
        }
    }
    /**
     * Removes all tooltips
     */
    deleteTooltips (): void {
        //@ts-ignore
        $(".xterm-helper-textarea")?.tooltip('dispose');
    }
    /**
     * Returns the input string used to compute autocompletion
     */
    getCurrentInput (opt?: { regex?: RegExp }): string {
        opt = opt || {};
        this.currentInput = this.content?.command({ beforeCursor: true });
        console.log("[xterm-autocomplete] getCurrentInput", { currentInput: this.currentInput });
        if (opt.regex && this.currentInput) {
            const match: RegExpMatchArray = new RegExp(opt.regex).exec(this.currentInput);
            if (match && match.length && match[0]) {
                return match[0];
            }
            return "";
        }
        return this.currentInput;
    }
    /**
     * Internal function, checks if the provided hints are identical
     */
    protected sameHints (t1: string[], t2: string[]): boolean {
        if (t1?.length === t2?.length) {
            for (let i = 0; i < t1?.length; i++) {
                if (t1[i] !== t2[i]) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }
    /**
     * Internal function, shows tooltip at the cursor position
     */
    protected renderTooltip (): void {
        const hints: string[] = this.getHints();
        if (hints?.length) {
            const tooltip: string = Handlebars.compile(tooltipTemplate, { noEscape: true })({
                hints
            });
            const cursor: JQuery<HTMLElement> = $(".xterm-helper-textarea");
            // append tooltip to cursor
            cursor.attr("data-toggle", "tooltip");
            // show tooltip
            // @ts-ignore                
            cursor.tooltip({
                trigger: "manual hover click",
                title: tooltip,
                placement: "top",
                html: true,
                boundary: "viewport"
            }).tooltip('show');
            // install mouse and keypress handlers for the autocompletion items
            $(".autocompletion-item").on("mouseover", (evt: JQuery.MouseOverEvent) => {
                // console.log("[xterm-autocomplete] mouseover", { target: evt.target });
                const index: number = this.getIndex(evt.target);
                this.highlight(index);
            }).on("click", (evt: JQuery.ClickEvent) => {
                console.log("[xterm-autocomplete] click", { target: evt.target });
                const index: number = this.getIndex(evt.target);
                this.select(index);
            }).on("dblclick", (evt: JQuery.DoubleClickEvent) => {
                console.log("[xterm-autocomplete] click", { target: evt.target });
                const index: number = this.getIndex(evt.target);
                this.select(index);
                this.triggerAutocomplete();
            });
            // .on("keydown", (evt: JQuery.KeyDownEvent) => {
            //     // console.log("[.autocompletion-item.onkeypress]")
            //     this.autocompleteOnKeyPress(evt);
            // });
        } else {
            // remove tooltip
            this.deleteTooltips();
        }
    }
    /**
     * Autocompletion logic for the prover
     */
	autocompleteProverCommand (opt?: { fullSet?: boolean }): string[] {        
        // console.log("[xterm-autocomplete] autocompleteProverCommand", { currentInput: this.currentInput, hintsData: this.hintsObject });
        if (opt?.fullSet) {
            const symbols: string[] = this.hintsObject?.symbols; //utils.listSymbols(this.proofState);
            const mathObjects: string[] = this.mathObjects?.lemmas;
            const commands: string[] = Object.keys(this.hintsObject?.commands);
            return symbols.concat(mathObjects).concat(commands);
        }
        if (this.currentInput) {
            let hints: string[] = [];
            // console.log(`[xterm-autocomplete] trying to auto-complete ${currentInput}`);
            if (this.currentInput.startsWith("expand")
                || this.currentInput.startsWith("rewrite")
                || this.currentInput.startsWith("eval-expr")) {
                // autocomplete symbol names
                const symbols: string[] = this.hintsObject?.symbols; //utils.listSymbols(this.proofState);
                // console.dir(symbols, { depth: null });
                const expandCommands: string[] = [
                    "expand", "rewrite", "eval-expr"
                ];
                for (let i = 0; i < symbols?.length; i++) {
                    for (let j = 0; j < expandCommands.length; j++) {
                        if (`${expandCommands[j]} "${symbols[i]}"`.toLocaleLowerCase().startsWith(this.currentInput)) {
                            hints.push(`${expandCommands[j]} "${symbols[i]}"`);
                        } 
                    }
                }
            } else if (this.currentInput.startsWith("lemma")
                        || this.currentInput.startsWith("apply-lemma")) {
                if (this.mathObjects?.lemmas?.length) {
                    const symbols: string[] = this.mathObjects?.lemmas;
                    const lemmaCommands: string[] = [
                        "lemma", "apply-lemma"
                    ];	
                    for (let i = 0; i < symbols?.length; i++) {
                        for (let j = 0; j < lemmaCommands.length; j++) {
                            if (`${lemmaCommands[j]} "${symbols[i]}"`.toLocaleLowerCase().startsWith(this.currentInput)) {
                                hints.push(`${lemmaCommands[j]} "${symbols[i]}"`);
                            }							
                        }
                    }
                }
            } else {
                // other prover command
                hints = Object.keys(this.hintsObject?.commands)?.filter((c: string) => c.toLocaleLowerCase().startsWith(this.currentInput));
            }
            return hints;
        }
        return [];
	}
    /**
     * Autocompletion logic for the evaluator
     */
	autocompleteEvaluatorCommand (opt?: { fullSet?: boolean }): string[] {
        if (opt?.fullSet) {
            const symbols: string[] = this.hintsObject?.symbols;
            const commands: string[] = Object.keys(this.hintsObject?.commands);
            return symbols.concat(commands);
        }
        if (this.currentInput) {
            let hits: string[] = [];
            // autocomplete symbol names
            const symbols: string[] = this.hintsObject?.symbols;
            // console.dir(symbols, { depth: null });
            if (symbols?.length) {
                hits = symbols?.filter((c: string) => c.toLocaleLowerCase().startsWith(this.currentInput));
            }
            // Include also pvsio functions
            hits = hits.concat(Object.keys(this.hintsObject?.commands)?.filter((c: string) => c.toLocaleLowerCase().startsWith(this.currentInput)));
            return hits;
        }
        return [];
	}
    /**
     * Returns the hints, based on the current input entered by the user and the session type
     */
    getHints (opt?: { fullSet?: boolean }): string[] {
        // update current input
        const currentInput: string = this.getCurrentInput()?.toLocaleLowerCase();
        this.currentInput = currentInput?.startsWith("(") ? currentInput?.substring(1).trim() : currentInput;
        // get command hints
        let hints: string[] = this.sessionType === "evaluator" ?
            this.autocompleteEvaluatorCommand(opt)
                : this.autocompleteProverCommand(opt);
        // get list of commands previously accepted by the prover
        const successHistory: string[] = opt?.fullSet ? this.history.getSuccessHistory()
            : this.history.getSuccessHistory({ match: this.currentInput })
        hints = hints.concat(successHistory);
        // sort hints
        return hints?.sort((a: string, b: string) => {
            return a.toLocaleLowerCase() < b.toLocaleLowerCase() ? -1 : 1;
        }) || [];
    }
    /**
     * Updates autocomplete information and shows tooltip if the list of hints is non-empty
     */
    updateAutocomplete (): void {
        const hints: string[] = this.getHints();
        this.showTooltip(hints);
        // update integrated help
        this.updateHelp();
        console.log("[xterm-autocomplete] updateAutocomplete", { hints });
    }
    /**
     * Handler for arrow press events
     */
    onArrow (evt: KeyboardEvent | JQuery.KeyDownEvent): void {
        switch (evt.key) {
            case "ArrowDown": {
                this.selectNext();
                break;
            }
            case "ArrowUp": {
                this.selectPrev();
                break;
            }
            default: {
                break;
            }
        }
    }
    /**
     * Highlights the n-th hint shown in the tooltip
     */
    highlight (n: number): void {
        const $hints: JQuery<HTMLElement> = $(".autocompletion-item");
        this.clearHighlight();
        $($hints[n]).addClass("highlighted");
    }
    /**
     * Clears hints highlight
     */
    clearHighlight (): void {
        console.log("[xterm-autocomplete] clearHighlight");
        $(".autocompletion-item").removeClass("highlighted");
    }
    /**
     * Clears the integrated help
     */
    clearHelp (): void {
        $(".terminal-help").empty();
    }
    /**
     * Updates integrated help
     */
    updateHelp (): void {
        const cmd: string = this.getSelectedHint() || this.getCurrentInput({ regex: /[\w\+\@\-\*\?\!]+/ });
        console.log("[xterm-autocomplete] updateHelp", { cmd });
        const desc: CommandDescriptor = commandUtils.PROOF_COMMANDS[cmd] 
                                        || commandUtils.PROOF_TACTICS[cmd]
                                        || commandUtils.EVALUATOR_COMMANDS[cmd];
        const integratedHelp: string = Handlebars.compile(terminalHelpTemplate, { noEscape: true })({
            cmd,
            ...desc
        });
        $(".terminal-help").html(integratedHelp);
    }
    /**
     * Internal function, selects the n-th hint shown in the tooltip
     */
    select (index: number): void {
        if (index >= 0 && index < this.currentHints?.length) {
            this.highlight(index);
            this.clearSelection();
            this.highlight(index);
            $($(".autocompletion-item")[index]).addClass("selected");
            $(".autocompletion-item")[index].scrollIntoView({
                block: "nearest"
            });
            // update integrated help
            this.updateHelp();
        }
    }
    /**
     * Selects next tooltip
     */
    selectNext (): void {
        const selected: JQuery<HTMLElement> = $(".autocompletion-item.selected");
        const index: number = this.getIndex(selected[0]);
        console.log("[xterm-autocompletion] select next", { index, hints: this.currentHints });
        this.select(index + 1);
    }
    /**
     * Selects previous tooltip
     */
    selectPrev (): void {
        const selected: JQuery<HTMLElement> = $(".autocompletion-item.selected");
        const index: number = this.getIndex(selected[0]);
        console.log("[xterm-autocompletion] select prev", { index, hints: this.currentHints });
        this.select(index - 1);
    }
    /**
     * Clear selection
     */
    clearSelection (): void {
        $(".autocompletion-item").removeClass("selected");
    }
    /**
     * Internal function, returns the index of the selected item
     */
    getIndex (target: HTMLElement): number {
        if (target) {
            const matchIndex: RegExpMatchArray = /\bindex-(\d+)/g.exec(target?.className);
            if (matchIndex && matchIndex.length > 1) {
                const index: number = +matchIndex[1];
                return index;
            }
        }
        return NaN;
    }
    /**
     * Returns the selected hint
     */
    getSelectedHint (): string {
        const selected: JQuery<HTMLElement> = $(".autocompletion-item.selected");
        const matchIndex: RegExpMatchArray = /index-(\d+)/.exec(selected[0]?.className);
        if (matchIndex?.length && !isNaN(+matchIndex[1])) {
            const index: number = +matchIndex[1];
            if (index >= 0 && index < this.currentHints.length) {
                const val: string = this.currentHints[index];
                console.log("[xterm-autocomplete] Selected hint", { index, val });
                return val;
            }
        }
        // const tooltipContent: string = selected?.text()
        // console.warn("[xterm-pvs] Warning: incorrect index in tooltip, returning tooltip content as fallback", { matchIndex, tooltipContent, currentHints: this.currentHints });
        // return tooltipContent;
        return "";
    }
    /**
     * Internal function, triggers autocomplete
     */
    protected triggerAutocomplete (): void {
        const hints: string[] = this.getHints();
        if (hints?.length) {
            const evt: DidAutocompleteEvent = {
                substitution: this.getSelectedHint(),
                currentInput: this.currentInput
            };
            this.trigger(AutocompleteEvent.didAutocomplete, evt);
        }
        this.deleteTooltips();
    }
    /**
     * updates tooltips based on keypress events
     */
    autocompleteOnKeyPress (ev: KeyboardEvent | JQuery.KeyDownEvent): void {
        switch (ev.key) {
            case "Backspace": {
                this.updateAutocomplete();
                break;
            }
            case "Delete": {
                this.updateAutocomplete();
                break;
            }
            case "ArrowUp":
            case "ArrowDown":
            case "ArrowLeft":
            case "ArrowRight": {
                this.onArrow(ev);
                break;
            }
            case "Space": {
                this.deleteTooltips();
                break;
            }
            case "Escape": {
                this.deleteTooltips();
                break;
            }
            case "Enter":
            case "Tab": {
                this.triggerAutocomplete();
                break;
            }
            default: {
                this.updateAutocomplete();
                break;
            }
        }
    }
}



/**
 * XTermPvs extends the functionalities of xterm.js by introducing:
 * - syntax highlighting for pvs
 * - virtual document for storing terminal content and command line
 * - highligh for matching brackets
 * ANSI sequences supported by xterm.js are at https://xtermjs.org/docs/api/vtfeatures/
 */
export class XTermPvs extends Backbone.Model {
    // terminal content
    protected content: Content;

    // terminal renderer
    protected xterm: XTerm;

    // autocomplete engine and renderer
    protected autocomplete: Autocomplete;

    // session type (default: evaluator)
    protected sessionType: SessionType;

    // id of the DOM element where the terminal is attached
    protected parent: string;

    protected fontSize: number = 12; //px default font size used in the terminal
    protected lineHeight: number = 1.2; // normal line height is 20% larger than font size 

    protected paddingBottom: number = 0;

    // status of the mod keys
    protected modKeys: ModKeys = {
        alt: false,
        ctrl: false,
        meta: false    
    };

    // last matched bracket
    protected brackets: MatchBrackets;

    // scroll timer
    protected timer: NodeJS.Timer = null;
    readonly timerTimeout: number = 50; //ms

    // flag indicating whether input is enabled
    protected inputEnabled: boolean = true;

    // list of commands
    protected commands: CommandsMap = null;

    // tab size, used when writing in the terminal -- \t is replaced with TAB_SIZE spaces
    protected TAB_SIZE: number = 4;

    // terminal prompt
    protected prompt: string = "> ";
    protected ntrims: number = 0;

    // cursor position in the rendering buffer
    protected pos: Position = {
        line: MIN_POS.line,
        character: MIN_POS.character
    };

    /**
     * Constructor
     */
    constructor (opt?: {
        parent?: string, 
        cols?: number, 
        rows?: number, 
        sessionType?: SessionType,
        paddingBottom?: number
    }) {
        super();
        opt = opt || {};

        this.content = new Content();
        this.sessionType = opt?.sessionType || "evaluator";
        this.autocomplete = new Autocomplete(this.content, this.sessionType);

        this.paddingBottom = opt?.paddingBottom || 0;

        const cols: number = opt.cols || MIN_VIEWPORT_COLS;
        const rows: number = opt.rows || Math.floor((window.innerHeight - this.paddingBottom) / (this.fontSize * this.lineHeight)) || MIN_VIEWPORT_ROWS;
        this.xterm = new XTerm({
            rendererType: "canvas",
            cols,
            rows,
            fontSize: this.fontSize,
            fontFamily: "monospace",
            theme: {
                background: "#1e1e1e" // dark gray
            }
        });
    
        // create the terminal panel
        this.parent = opt?.parent || "terminal";
        this.xterm.open(document.getElementById(this.parent));
        $(".terminal").append(tooltipStyle);
        $(".terminal").append(cursorStyle);
    
        // install handlers
        this.installHandlers();

        // get the focus
        this.xterm.focus();

        // @ts-ignore
        // this.xterm.buffer.active._buffer.lines.onTrim((n: number) => {
        //     this.ntrims += n;
        //     this.content.trimLines(n);
        //     console.log("onTrimEmitter", {
        //         n,
        //         tot: this.ntrims,
        //         content: this.content,
        //         buffer: this.xterm.buffer.active,
        //         xterm: this.xterm
        //     });
        // });

        this.xterm.buffer.onBufferChange((evt) => {
            console.log("onBufferChange", evt);
        });

        console.log("[xterm-pvs] Init complete", { xterm: this.xterm, jquery: $(`#${this.parent}`) });
    }

    /**
     * Disables terminal input
     */
    disableInput (): void {
        this.inputEnabled = false;
    }

    /**
     * Enables terminal input
     */
    enableInput (): void {
        this.inputEnabled = true;
    }

    /**
     * Internal function, finds out which mod keys have been pressed
     */
    protected getActiveModKeys (): string {
        const active: string[] = [];
        for (let i in this.modKeys) {
            if (this.modKeys[i]) {
                active.push(i);
            }
        }
        return active.join(" ");
    }

    /**
     * Internal function, checks if a mod key is active
     */
    protected modKeyIsActive (): boolean {
        for (let i in this.modKeys) {
            if (this.modKeys[i]) {
                return true;
            }
        }
        return false;
    }

    /**
     * Updates hints data for autocompletion
     */
    updateHints (hints: HintsObject): void {
        this.autocomplete.updateHints(hints);
        if (hints.commands && !this.commands) {
            // the set of commands won't change -- save them the first time only
            this.commands = hints.commands;
        }
    }

    /**
     * Updates mathObjects for autocompletion
     */
    updateMathObjects (mathObjects: MathObjects): void {
        this.autocomplete.updateMathObjects(mathObjects);
    }

    /**
     * Clears the content of the screen and resets the cursor position to its initial position (top-left corner)
     * see also http://ascii-table.com/ansi-escape-sequences.php
     * and https://xtermjs.org/docs/api/vtfeatures/
     * and https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
     */
    clearScreen (): void {
        this.content.deleteContent();
        this.pos = {
            ...MIN_POS
        };
        this.xterm.write("\x1B[2J"); // erase entire screen
        this.xterm.write("\x1B[H"); // move cursor to home position
    }

    /**
     * Internal function, clears text after position pos
     */
    protected clearTextAfter (pos: Position): void {
        this.clearTextFrom({
            line: pos.line,
            character: pos.character + 1
        });
    }

    /**
     * Internal function, clears text from position pos
     */
    protected clearTextFrom (pos: Position): void {
        if (pos) {
            this.saveCursorPosition();
            this.moveCursorTo({
                line: pos.line,
                character: pos.character
            }, { src: "clearTextFrom", temp: true });
            this.xterm.write("\x1B[0J"); // erase from the cursor position to the end of the viewport
            this.restoreCursorPosition();
        }
    }

    /**
     * Internal function, clears text at line n
     */
    protected clearLine (n: number): void {
        if (n) {
            this.saveCursorPosition();
            this.moveCursorTo({
                line: n,
                character: MIN_POS.character
            }, { src: "clearLine", temp: true });
            this.xterm.write("\x1B[2K"); // erase complete line
            this.restoreCursorPosition();
        }
    }

    /**
     * Internal function, refreshes the terminal content at the active line
     */
    protected refreshCurrentLine (): void {
        const pos: Position = this.content.cursorPosition();
        this.moveCursorTo({
            line: pos.line,
            character: MIN_POS.character
        }, { src: "refreshCurrentLine" });

        // erase complete line
        this.saveCursorPosition();
        this.xterm.write("\x1B[2K");
        // replace text
        const text: string = this.content.textLineAt(pos.line);
        this.xterm.write(this.applySyntaxHighlighting(text));
        this.restoreCursorPosition();

        // move cursor to the current position
        this.moveCursorTo(pos,{ src: "refreshCurrentLine" });
    }

    /**
     * Scroll the terminal content to the given position
     * A timeout is used to give time to the buffer to render the content
     * (if scrolling is performed without the timeout, the buffer has still the 
     *  old content, and so it won't scroll to the intended final cursor position)
     */
    scrollCursorIntoView (): void {
        clearTimeout(this.timer);
        this.timer = setTimeout(() => {
            $(".xterm-helper-textarea")[0].scrollIntoView({
                // behavior: "smooth",
                block: "nearest"
            });
        }, this.timerTimeout);
    }

    /**
     * Highlights matching brackets
     */
    matchBrackets (bracket?: Bracket): void {
        const b1: Bracket = bracket || "(";
        const match: MatchBrackets = this.content.matchBrackets(b1);
        if (match?.pos1 !== this.brackets?.pos1 || match?.pos2 !== this.brackets?.pos2) {
            // console.log("[xterm-pvs] matchBrackets", { match, old: this.brackets });
            // reset colors for old match
            if (this.brackets) {
                if (this.brackets.pos1) {
                    this.moveCursorTo(this.brackets.pos1, { src: "matchBrackets" });
                    this.renderData(this.content.charAt(this.brackets.pos1));
                }
                if (this.brackets.pos2) {
                    this.moveCursorTo(this.brackets.pos2, { src: "matchBrackets" });
                    this.renderData(this.content.charAt(this.brackets.pos2));
                }
            }
            // console.log("[xterm-pvs] matchBrackets", match);
            if (match?.pos1) {
                this.moveCursorTo(match.pos1, { src: "matchBrackets" });
                const par: string = colorUtils.ansiColorText(this.content.charAt(match.pos1), {
                    background: colorUtils.PvsColor.gold,
                    foreground: colorUtils.PvsColor.black,
                    bold: true
                });
                this.renderData(par)
            }
            if (match?.pos2) {
                this.moveCursorTo(match.pos2, { src: "matchBrackets" });
                const opt: colorUtils.AnsiHighlightOptions = match.pos1 ?
                    { background: colorUtils.PvsColor.gold, foreground: colorUtils.PvsColor.black, bold: true }
                        : { background: colorUtils.PvsColor.red, foreground: colorUtils.PvsColor.white };
                const par: string = colorUtils.ansiColorText(this.content.charAt(match.pos2), opt);
                this.renderData(par)
            }
            this.brackets = match;
            this.moveCursorTo(this.content.cursorPosition(), { src: "matchBrackets" });
        }
    }

    /**
     * Internal function, triggers event "sendText" when input is ready
     */
    protected sendWhenReady (): void {
        const cmd: string = this.content.command().trim();
        if (cmd) {
            switch (this.sessionType) {
                case "evaluator": {
                    if (cmd.trim().endsWith(";") || cmd?.trim() === "quit") {
                        // send command
                        this.trigger(XTermEvent.sendText, { data: cmd });
                        // push command in the history
                        this.autocomplete.updateCommandHistory(cmd);
                    }
                    break;
                }
                case "prover": {
                    if (commandUtils.checkPar(cmd)?.success || cmd?.trim() === "quit") {
                        // send command
                        this.trigger(XTermEvent.sendText, { data: cmd });
                        // push command in the history
                        this.autocomplete.updateCommandHistory(cmd);
                    }
                    break;
                }
                default: {
                    break;
                }
            }
        }
    }
    /**
     * Internal function, updates the terminal view based on the information given by this.content
     */
    protected updateView (keyEvent: KeyEvent): void {
        // console.log("[xterm-pvs] onKeyPress", { evt });
        const domEvent: KeyboardEvent = keyEvent.domEvent;
        const key: string = keyEvent.key;        
        switch (domEvent.key) {
            case "Backspace": {
                this.onRenderBackspace();
                break;
            }
            case "Delete": {
                this.onRenderDelete();
                break;
            }
            case "Enter": {
                this.onRenderEnter();
                break;
            }
            case "ArrowUp":
            case "ArrowDown":
            case "ArrowLeft":
            case "ArrowRight": {
                this.onRenderArrow();
                break;
            }
            case "Space": {
                this.onRenderText(key);
                break;
            }
            case "Escape":
            case "Tab": {
                break;
            }
            default: {
                this.onRenderText(key);
                break;
            }
        }
        // apply syntax highlighting to the current line
        const pos: Position = this.cursorPosition();
        const textLine: string = this.content.textLineAt(pos);
        this.moveCursorTo({ line: pos.line, character: MIN_POS.character }, { src: "updateView" });

        this.saveCursorPosition();
        this.xterm.write(this.applySyntaxHighlighting(textLine));
        this.restoreCursorPosition();

        // match brackets
        this.matchBrackets();
        // move cursor to its current position
        this.moveCursorTo(pos, { src: "updateView" });
        // scroll cursor into view -- not sure why xterm.scrollToLine(...) is not working
        this.scrollCursorIntoView();
        // console.log("[xterm-pvs] onKeyPress", { textLine, pos });
    }

    /**
     * Updates success history
     */
    updateCommandHistory (data: UpdateCommandHistoryData): void {
        console.log("[xterm-autocomplete] updateCommandHistory", { data });
        if (data.cmd) {
            this.autocomplete.updateCommandHistory(data.cmd, { successHistory: true });
        }
    }

    /**
     * Handler for command history search
     */
    onHistorySearch (evt: KeyEvent): boolean {
        console.log("[xterm-pvs] onHistorySearch", { history: this.autocomplete.history, evt });
        if (this.autocomplete.history.size() && this.content.cursorIsAtHomePosition() 
                && (evt.domEvent.key === "ArrowUp" || evt.domEvent.key === "ArrowDown")) {
            // get command from the history
            const cmd: string = evt.domEvent.key === "ArrowUp" ? this.autocomplete.history.prev()
                    : this.autocomplete.history.next();
            // update command line
            this.content.setCommand(cmd, { cursorToHome: true });
            return true;
        }
        return false;
    }

    /**
     * Handler for key press events
     */
    onKeyPress (evt: KeyEvent): void {
        // check first if this is a history search
        const historySearch: boolean = this.onHistorySearch(evt);
        // process key press if this is not a history search and mod keys are not pressed
        if (!historySearch && !this.modKeyIsActive()) {
            // dispatch Enter events to autocomplete if there is a tooltip selected
            if (evt.domEvent.key === "Enter" && this.autocomplete.getSelectedHint()) {
                this.autocomplete.autocompleteOnKeyPress(evt?.domEvent);
                return;
            }
            // update content and show tooltips
            const contentHasChanged: boolean = this.content.updateContent(evt);
            this.autocomplete.autocompleteOnKeyPress(evt?.domEvent);
            if (contentHasChanged) {
                this.updateView(evt);
                if (evt.domEvent.key === "Enter") {
                    this.sendWhenReady();
                }
            }
        }
    }

    /**
     * Updates autocompletion tooltip
     */
    updateAutocomplete (): void {
        this.autocomplete.updateAutocomplete();
    }

    /**
     * Utility function, returns the current cursor position
     */
    cursorPosition (): Position {
        return this.content.cursorPosition();
    }

    /**
     * Handler for control sequences sent programmatically to the terminal, 
     * e.g., following a mouse click event that repositions the mouse
     */
    onControlSequence (seq: string[]): void {
        let delta: number = 0;
        for (let i = 0; i < seq.length; i++) {
            switch (seq[i]) {
                case "\x1B[C": { // right
                    delta++;
                    break;
                }
                case "\x1B[D": { // left
                    delta--;
                    break;
                }
                default: {
                    break;
                }
            }
        }
        const pos: Position = this.content.cursorPosition();        
        const relCol: number = this.xterm.buffer.active.cursorX + 1 + delta; // cursorX is 0-based
        const deltaLine: number = Math.floor(relCol / this.xterm.cols);
        const col: number = relCol % this.xterm.cols;
        const targetPos: Position = {
            character: col < 0 ? col + this.xterm.cols : col,
            line: pos.line + deltaLine
        };
        const success: boolean = this.content.cursorTo(targetPos);
        if (success) {
            this.moveCursorTo(this.content.cursorPosition(), { src: "onControlSequence" });
        }
    }

    /**
     * Handler for data events (e.g., cut/paste in the terminal)
     */
    onData (data: string): void {
        if (this.modKeyIsActive() && data) {
            if (data.startsWith("\x1B[")) {
                // this is a control sequence, e.g., a series of cursor right sent after a mouse click, to reposition the cursor
                const seq: string[] = data.split("\x1B[").map(elem => {
                    return "\x1B[" + elem;
                });
                // console.log("[xterm-pvs] onData received control sequence", { data, seq });
                this.onControlSequence(seq);
                return;
            }
            // write data at the cursor position
            this.content.writeData(data);
            // refresh the terminal with updated data -- the logic is a variation of onEnter
            const prevPos0: Position = {
                line: this.content.previousCursorPosition().line,
                character: 1
            };
            const textAfter: string = this.content.textAfter(prevPos0);
            console.log("[vscode-content] onData", { textAfter, prevPos0 });
            // clear old text
            this.clearTextAfter(prevPos0);
            // this.clearMultiLine({
            //     from: prevPos0.line, 
            //     to: prevPos0.line + 1 + textAfter.split("\n").length
            // });
            // move to the new cursor position and render text
            this.moveCursorTo(prevPos0, { src: "onData" });
            const colorText: string = this.applySyntaxHighlighting(textAfter.replace(/\n/g, "\r\n"));
            this.renderData(colorText);
            // make sure the cursor is rendered in the correct position
            const pos: Position = this.content.cursorPosition();
            this.moveCursorTo(pos, { src: "onData"});
            this.scrollCursorIntoView();
            // console.log("[vscode-content] onData", { textAfter, pos, prevPos0 });
        }
    }

    /**
     * Internal function, install relevant event handlers
     */
    protected installHandlers (): void {
        // a key is pressed
        this.xterm.onKey((evt: KeyEvent) => {
            this.onKeyPress(evt);
        });
        this.xterm.onData((data: string) => {
            this.onData(data);
        });
        this.xterm.attachCustomKeyEventHandler ((evt: KeyboardEvent): boolean => {
            this.modKeys = {
                alt: !!evt?.altKey,
                ctrl: !!evt?.ctrlKey,
                meta: !!evt?.metaKey
            };
            // check if this is ctrl+c
            if (evt.key === "c" && evt?.ctrlKey) {
                this.trigger(XTermEvent.sendText, { data: interruptCommand });
                return false;
            }
            return this.inputEnabled && 
                (
                    !this.modKeyIsActive()
                    || evt.key === "v"  // ctrl+v / command+v = paste
                    || evt.key === "Space" // ctrl+space = show autocomplete
                    || evt.key === "ArrowUp" || evt.key === "ArrowDown" // search history
                );
        });
        $(window).on("resize", (evt: JQuery.ResizeEvent) => {
            const rows: number = Math.floor((window.innerHeight - this.paddingBottom) / (this.fontSize * this.lineHeight)) || MIN_VIEWPORT_ROWS;
            this.resizeLines(rows);
        });
        $(document).on("click", (evt: JQuery.ClickEvent) => {
            // console.log("[xterm-pvs] click", { evt: evt });
            // this give the raw position of the cursor, in px, how do we convert this into lines/cols?
        });
        // content event handlers
        this.content.on(ContentEvent.rebase, (evt: RebaseEvent) => {
            this.onRebaseContent(evt);
            this.focus();
        });
        this.content.on(ContentEvent.didAutocompleteContent, () => {
            this.refreshCurrentLine();
            this.focus();
        });
        // autocomplete event handlers
        this.autocomplete.on(AutocompleteEvent.didAutocomplete, (evt: DidAutocompleteEvent) => {
            this.onResolveAutocomplete(evt);
            this.focus();
        });
    }

    /**
     * Internal function, handles rebase events triggered by content
     */
    protected onRebaseContent (evt: RebaseEvent): void {
        if (evt?.pos) {
            // rebase position
            this.pos = {
                line: evt.pos.line,
                character: evt.pos.character
            };
            // console.log("[xterm-pvs] onRebase", { pos: this.pos, content: this.content });
        }
    }

    /**
     * Internal function, handles resolve events triggered by autocomplete
     */
    protected onResolveAutocomplete (evt: DidAutocompleteEvent): void {
        this.content.autocomplete(evt.currentInput, evt.substitution);
    }

    /**
     * Handles delete key presses
     */
    protected onRenderDelete (): void {
        const pos: Position = this.content.cursorPosition();
        const textAfter: string = this.content.textAfter(pos);
        this.xterm.write(`\x1B[1P`); // the cursor does not move with this command
        // refresh text after cursor position, delete may have pulled up the content below
        if (textAfter) {
            this.clearTextFrom({
                line: pos.line + 1,
                character: MIN_POS.character
            });
            this.saveCursorPosition();
            this.xterm.write(textAfter.replace(/\n/g, "\r\n"));
            this.restoreCursorPosition();
        }
        console.log("[xterm-pvs] onRenderDelete", { textAfter, pos });
    }

    /**
     * Handles backspace key presses
     */
    protected onRenderBackspace (): void {
        const pos: Position = this.content.cursorPosition();
        this.moveCursorTo(pos, { src: "onRenderBackspace"});
        this.onRenderDelete();
    }

    /**
     * Handles renering for Enter key presses
     */
    protected onRenderEnter (): void {
        const pos: Position = this.content.cursorPosition();
        const prevPos: Position = this.content.previousCursorPosition();
        const textAfter: string = this.content.textAfter(pos);
        // clear old text
        if (textAfter) {
            this.clearTextFrom(prevPos);
        }

        this.saveCursorPosition();
        this.xterm.write("\r\n" + textAfter.replace(/\n/g, "\r\n"));
        this.restoreCursorPosition();
        
        this.moveCursorTo(pos, { src: "onRenderEnter"});
        console.log("[xterm-pvs] renderOnEnter", { content: this.content, textAfter, pos });
    }

    /**
     * Handles arrow key presses
     */
    protected onRenderArrow (): void {
        // get the new position of the cursor
        const pos: Position = this.content.cursorPosition();
        this.moveCursorTo(pos, { src: "onRenderArrow"});
        console.log("[xterm-pvs] onRenderArrow", {
            pos,
            prevPos: this.content.previousCursorPosition()
        });
    }

    /**
     * Generic handler for printable characters
     */
    protected onRenderText (txt: string): void {
        // get the new position of the cursor
        const pos: Position = this.content.cursorPosition();
        this.renderData(txt);
        // render text after cursor
        const textLineAfter: string = this.content.textLineAfter(pos);
        if (textLineAfter) {
            this.renderData(textLineAfter);
        }
        this.moveCursorTo(pos, { src: "onRenderChar" });
    }

    /**
     * Write text in the terminal. The received data will become read-only
     */
    write (data: string): void {
        if (data) {
            // console.log("[xterm-pvs] write", { data });
            this.saveCursorPosition();
            data = data.replace(/\t/g, " ".repeat(this.TAB_SIZE));
            this.content.writeData(data);
            this.renderData(data);
            this.restoreCursorPosition();

            this.moveCursorTo(this.content.cursorPosition(), { src: "write" });
            this.scrollCursorIntoView();
        }
    }

    /**
     * Write text in the terminal. The received data will become read-only
     */
    log (data: string): void {
        if (data) {
            // console.log("[xterm-pvs] log", { data });
            this.write(data);
            // rebase to make the received data read-only
            this.content.rebase();
        }
    }

    /**
     * Internal function, renders data in the terminal
     */
    protected renderData (data: string, opt?: {
        pos?: Position
    }): string {
        if (data) {
            opt = opt || {};
            let content: string = data.replace(/\n/g, "\r\n");
            if (content) {
                const renderLines: string[] = content.split("\n");

                // resize terminal before writing, 
                // to avoid line wrapping or rendering characters outside the field of view
                const pos: Position = opt.pos || this.content.cursorPosition();
                let maxCol: number = this.xterm.cols;
                for (let i = 0; i < renderLines.length; i++) {
                    const lineLen: number = pos.character + renderLines[i].length;
                    if (lineLen > maxCol) { maxCol = lineLen; }
                }
                this.resizeCol(maxCol);
                // const maxLine: number = Math.max(this.content.maxLineNumber(), this.xterm.rows);

                // apply syntax highlighting if the text does not already contain any syntax highlighting
                content = colorUtils.isPlainText(content) ? this.applySyntaxHighlighting(content) : content;
                this.saveCursorPosition();
                this.xterm.write(content);
                this.restoreCursorPosition();
                // console.log("[xterm-pvs] renderData", { pos, txt, renderLines, data });
            }
            return content;
        }
        return "";
    }

    /**
     * Resize the number of columns of the terminal. 
     * The number of columns should only grow -- otherwise some lines may become wrapped.
     */
    resizeCol (nCols: number): void {
        const maxCols: number = nCols > this.xterm.cols ? nCols : this.xterm.cols;
        if (maxCols > this.xterm.cols) {
            // console.log("[xterm-pvs] resize viewport col", { maxCols });
            this.xterm.resize(maxCols, this.xterm.rows);
        }
    }

    /**
     * Resize the number of lines of the terminal
     */
    resizeLines (nLines: number): void {
        // const maxLines: number = nLines > this.xterm.rows ? nLines : this.xterm.rows;
        // if (maxLines > this.xterm.rows) {
        //     // console.log("[xterm-pvs] resize viewport lines", { maxLines });
        //     this.xterm.resize(this.xterm.cols, maxLines);
        // }
        const baseY: number = this.xterm.buffer.active.baseY;
        const length: number = this.xterm.buffer.active.length;
        const cursorY: number = this.xterm.buffer.active.cursorY;
        // console.log("[xterm-pvs] resize viewport lines before", {
        //     xterm: this.xterm,
        //     baseY,
        //     length,
        //     cursorY
        // });
        this.xterm.resize(this.xterm.cols, nLines);
        // console.log("[xterm-pvs] resize viewport lines after", {
        //     xterm: this.xterm,
        //     baseY: this.xterm.buffer.active.baseY,
        //     length: this.xterm.buffer.active.length,
        //     cursorY: this.xterm.buffer.active.cursorY
        // });
    }

    /**
     * Resize the terminal
     */
    // resize (cols: number, lines: number): void {
    //     this.resizeCol(cols);
    //     this.resizeLines(lines);
    // }

    /**
     * Resize the terminal if needed, so that the given position is visible
     */
    // resizePos (pos: Position): void {
    //     const maxLine: number = pos?.line > this.xterm.rows ? pos.line : this.xterm.rows; 
    //     const maxCol: number = pos?.character > this.xterm.cols ? pos.character : this.xterm.cols;
    //     this.resize(maxCol, maxLine);
    // }
    
    /**
     * Select character at position pos
     */
    selectCharacter (pos: Position): void {
        if (pos) {
            const col: number = pos.character;
            const row: number = pos.line;
            const len: number = 1;
            this.xterm.select(col, row, len);
        }
        // console.log("[xterm-pvs] select position", pos);
    }

    /**
     * Place focus on the terminal
     */
    focus (): void {
        console.log("[xterm-pvs] focus");
        this.xterm.focus();
    }

    /**
     * Clears the command line
     */
    clearCommandLine (): void {
        console.log("[xterm-pvs] clearCommandLine");
        this.content.setCommand("");
    }

    /**
     * Updates integrated help
     */
    updateHelp (): void {
        console.log("[xterm-pvs] updateHelp");
        this.autocomplete.updateHelp();
    }

    /**
     * Move cursor up, see also http://ascii-table.com/ansi-escape-sequences.php
     * and https://xtermjs.org/docs/api/vtfeatures/
     * and https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
     */
    protected cursorUp (n: number): void {
        // for (let i = 0; i < n; i++) {
        //     this.content.onArrowUp();
        // }
        // this.moveCursorTo(this.content.cursorPosition(), { src: "cursorUp"});
        this.xterm.write(`\x1B[${n}A`);
        // console.log("[xterm-pvs] cursor up", n);
    }

    /**
     * Move cursor down, see also http://ascii-table.com/ansi-escape-sequences.php
     * and https://xtermjs.org/docs/api/vtfeatures/
     * and https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
     */
    protected cursorDown (n: number): void {
        // for (let i = 0; i < n; i++) {
        //     this.content.onArrowDown();
        // }
        // this.moveCursorTo(this.content.cursorPosition(), { src: "cursorDown"});
        this.xterm.write(`\x1B[${n}B`);
        // console.log("[xterm-pvs] cursor down", n);
    }

    /**
     * Move cursor right, see also http://ascii-table.com/ansi-escape-sequences.php
     * and https://xtermjs.org/docs/api/vtfeatures/
     * and https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
     */
    protected cursorRight (n: number): void {
        // for (let i = 0; i < n; i++) {
        //     this.content.onArrowRight();
        // }
        // this.moveCursorTo(this.content.cursorPosition(), { src: "cursorRight"});
        this.xterm.write(`\x1B[${n}C`); // don't write directly in the terminal, otherwise the cursor position known to content will diverge from what is being displayed in the terminal
        // console.log("[xterm-pvs] cursor right", n);
    }

    /**
     * Move cursor left, see also http://ascii-table.com/ansi-escape-sequences.php
     * and https://xtermjs.org/docs/api/vtfeatures/
     * and https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
     */
    protected cursorLeft (n: number): void {
        // for (let i = 0; i < n; i++) {
        //     this.content.onArrowLeft();
        // }
        // this.moveCursorTo(this.content.cursorPosition(), { src: "cursorLeft"});
        this.xterm.write(`\x1B[${n}D`);
        // console.log("[xterm-pvs] cursor left", n);
    }

    /**
     * Internal function, move cursor to given position, 
     * see also http://ascii-table.com/ansi-escape-sequences.php
     * and https://xtermjs.org/docs/api/vtfeatures/
     * and https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
     */
    protected moveCursorTo (pos: Position, opt: { src: string, temp?: boolean }): void {
        if (pos && pos.character && pos.line) {
            const deltaY: number = pos.line - this.pos.line;
            if (deltaY) {
                (deltaY > 0) ? this.cursorDown(deltaY) : this.cursorUp(-deltaY);
                if (!opt?.temp) { this.pos.line += deltaY; }
            }
            const deltaX: number = pos.character - this.pos.character;
            if (deltaX) {
                (deltaX > 0) ? this.cursorRight(deltaX) : this.cursorLeft(-deltaX);
                if (!opt?.temp) { this.pos.character += deltaX; }
            }
            // console.log("[xterm-pvs] moveCursorTo", {
            //     xterm: this.xterm,
            //     pos,
            //     buffer: this.xterm.buffer.active,
            //     src: opt?.src,
            //     deltaX,
            //     deltaY
            // });
        }
    }

    /**
     * Internal function, erases line from the cursor position (included) to the end of the line
     * @param n Repeat the operation n times
     */
    // protected clearTextLineAfter (pos: Position): void {
    //     this.saveCursorPosition();
    //     this.moveCursorTo(pos, { src: "clearTextLineAfter"});
    //     this.xterm.write(`\x1B[K`);
    //     this.restoreCursorPosition();
    //     // console.log("[xterm-pvs] erase line");
    // }

    /**
     * Internal function, saves cursor position
     */
    protected saveCursorPosition (): void {
        this.xterm.write(`\x1B[s`);
        // console.log("[xterm-pvs] save cursor position");
    }

    /**
     * Internal function, restores cursor position
     */
    protected restoreCursorPosition (): void {
        this.xterm.write(`\x1B[u`);
        // console.log("[xterm-pvs] save cursor position");
    }

    /**
     * Internal function, applies pvs syntax highlighting to the provided text
     */
    protected applySyntaxHighlighting (text: string): string {
        if (text) {
            let htext: string = text;
            // highlight pvs keywords
            for (let i in pvsColorTheme) {
                const color: colorUtils.PvsColor = pvsColorTheme[i].color;
                const regexp: RegExp = new RegExp(pvsColorTheme[i].regex, pvsColorTheme[i].flags);
                htext = htext.replace(regexp, (txt: string) => {
                    return colorUtils.colorText(txt, color );
                });
            }
            // highlight prompt
            const regexp: RegExp = new RegExp(`^${this.prompt.trimEnd()}\\s`);
            // console.log("[xterm-pvs] applySyntaxHighlighting", { test: regexp.test(htext), regexp, text });
            htext = htext.replace(regexp, (txt: string) => {
                return colorUtils.colorText(txt, colorUtils.PvsColor.blue);
            });
            return htext;
        }
        return text;    
    }

    /**
     * Shows the prompt.
     */
    showPrompt (prompt: string): void {
        // Make sure the provided prompt does not contain ansi codes and has a space after, otherwise the regexp will fail. 
        // Colors will be applied in applySyntaxHighlighting.
        this.prompt = colorUtils.getPlainText(prompt) + " ";
        const cprompt: string = "\n\n" + this.applySyntaxHighlighting(this.prompt);
        this.log(cprompt);
        this.content.rebase({ prompt: this.prompt });
        this.autocomplete.clearHelp();
        // console.log("[xterm-pvs] showPrompt", { prompt: this.prompt, cprompt, content: this.content });
    }
}