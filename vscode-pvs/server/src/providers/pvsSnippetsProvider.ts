// ## Snippet syntax (see https://code.visualstudio.com/docs/editor/userdefinedsnippets)
// The body of a snippet can use special constructs to control cursors and the text being inserted. The following are supported features and their syntaxes:
//
// # Tabstops
// With tabstops, you can make the editor cursor move inside a snippet. Use $1, $2 to specify cursor locations. The number is the order in which tabstops will be visited, whereas $0 denotes the final cursor position. Multiple occurrences of the same tabstop are linked and updated in sync.
//
// # Placeholders
// Placeholders are tabstops with values, like ${1:foo}. The placeholder text will be inserted and selected such that it can be easily changed. Placeholders can be nested, like ${1:another ${2:placeholder}}.
//
// # Choice
// Placeholders can have choices as values. The syntax is a comma separated enumeration of values, enclosed with the pipe-character, for example ${1|one,two,three|}. When the snippet is inserted and the placeholder selected, choices will prompt the user to pick one of the values.
//
// # Variables
// With $name or ${name:default} you can insert the value of a variable. When a variable isn’t set, its default or the empty string is inserted. When a variable is unknown (that is, its name isn’t defined) the name of the variable is inserted and it is transformed into a placeholder.
// The following variables can be used:
// TM_SELECTED_TEXT The currently selected text or the empty string
// TM_CURRENT_LINE The contents of the current line
// TM_CURRENT_WORD The contents of the word under cursor or the empty string
// TM_LINE_INDEX The zero-index based line number
// TM_LINE_NUMBER The one-index based line number
// TM_FILENAME The filename of the current document
// TM_FILENAME_BASE The filename of the current document without its extensions
// TM_DIRECTORY The directory of the current document
// TM_FILEPATH The full file path of the current document
// CLIPBOARD The contents of your clipboard
//
// For inserting the current date and time:
// CURRENT_YEAR The current year
// CURRENT_YEAR_SHORT The current year's last two digits
// CURRENT_MONTH The month as two digits (example '02')
// CURRENT_MONTH_NAME The full name of the month (example 'July')
// CURRENT_MONTH_NAME_SHORT The short name of the month (example 'Jul')
// CURRENT_DATE The day of the month
// CURRENT_DAY_NAME The name of day (example 'Monday')
// CURRENT_DAY_NAME_SHORT The short name of the day (example 'Mon')
// CURRENT_HOUR The current hour in 24-hour clock format
// CURRENT_MINUTE The current minute
// CURRENT_SECOND The current second
//
// For inserting line or block comments, honoring the current language:
// BLOCK_COMMENT_START Example output: in PHP /* or in HTML <!--
// BLOCK_COMMENT_END Example output: in PHP */ or in HTML -->
// LINE_COMMENT Example output: in PHP // or in HTML <!-- -->

// # Variable transforms
// Transformations allow you to modify the value of a variable before it is inserted. The definition of a transformation consists of three parts:
// A regular expression that is matched against the value of a variable, or the empty string when the variable cannot be resolved.
// A "format string" that allows to reference matching groups from the regular expression. The format string allows for conditional inserts and simple modifications.
// Options that are passed to the regular expression.
// regex: "${TM_FILENAME/[\\.]/_/}"
// - output: example-123_456-TEST.js
// - explanation: Replace the first . with _
// regex: "${TM_FILENAME/(.*)/${1:/upcase}/}"
// - output: EXAMPLE-123.456-TEST.JS
// - explanation: Change to all uppercase

export const SNIPPETS = [{
	documentation: "Inserts theory block",
	prefix: "theory",
	scope: "pvs",
	body: [
		// the regexp is in the form (\w+):(\s*\w) -- the first group captures the theory name, the second group captures the incomplete input ":theory"
		"${TM_CURRENT_LINE/\\w+\\:(\\s*\\w+)//}THEORY", // the second group is auto-completed to THEORY
		"  BEGIN $0",
		"  END ${TM_CURRENT_LINE/(\\w+)\\:(\\s*\\w+)/$1/}" // the theory name (first capture group) is inserted at the end of the theory declaration
	]
}, {
	documentation: "Inserts record type declaration [# ac1: type1, ac2: type2, ... #]",
	prefix: "type:record",
	scope: "pvs",
	body: [
		"TYPE = [#",
		"  ${1:accessor}: ${2:type}",
		"#]"
	]
}, {
	documentation: "Inserts enum type declaration { en1, en2, ... }",
	prefix: "type:enum",
	scope: "pvs",
	body: [
		"TYPE = { ${1:id1}, ${2:id2} }"
	]
}, {
	documentation: "Inserts record expression (# ac1: type1, ac2: type2, ... #)",
	prefix: "expr:record",
	scope: "pvs",
	body: [
		"(#",
		"  ${1:accessor}: ${2:type}",
		"#)"
	]
}, {
	documentation: "Inserts if-then-else block",
	prefix: "if-then-else",
	scope: "pvs",
	body: [
		"IF ${1:cond} THEN ${2:expr1} ELSE ${3:expr2} ENDIF",
	]
}, {
	documentation: "Inserts COND block",
	prefix: "cond",
	scope: "pvs",
	body: [
		"COND",
		"  ${1:be1} -> ${2:e1},",
		"  ${3:be2} -> ${4:e2}",
		"ENDCOND"
	]
}];