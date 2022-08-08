/**
 * These scripts are injected by vscode in the webview that renders markdown content
 */

// regex for pvs keywords (hljs-keyword)
const keywords = {
    patterns: [
    {
        "match": "\\b[Aa][Nn][Dd]\\b|\\b[Aa][Nn][Dd][Tt][Hh][Ee][Nn]\\b|\\b[Aa][Rr][Rr][Aa][Yy]\\b|\\b[Aa][Ss]\\b|\\b[Aa][Ss][Ss][Uu][Mm][Ii][Nn][Gg]\\b|\\b[Aa][Ss][Ss][Uu][Mm][Pp][Tt][Ii][Oo][Nn]\\b|\\b[Aa][Uu][Tt][Oo]_[Rr][Ee][Ww][Rr][Ii][Tt][Ee](?:\\+|\\-|\\s)|\\b[Aa][Xx][Ii][Oo][Mm]\\b",
        "name": "keyword"
    },
    {
        "match": "\\b[Bb][Ee][Gg][Ii][Nn]\\b|\\b[Bb][Uu][Tt]\\b|\\b[Bb][Yy]\\b|\\b[Cc][Aa][Ss][Ee][Ss]\\b|\\b[Cc][Hh][Aa][Ll][Ll][Ee][Nn][Gg][Ee]\\b|\\b[Cc][Ll][Aa][Ii][Mm]\\b|\\b[Cc][Ll][Oo][Ss][Uu][Rr][Ee]\\b|\\b[Cc][Oo][Dd][Aa][Tt][Aa][Tt][Yy][Pp][Ee]\\b|\\b[Cc][Oo][Ii][Nn][Dd][Uu][Cc][Tt][Ii][Vv][Ee]\\b|\\b[Cc][Oo][Nn][Dd]\\b|\\b[Cc][Oo][Nn][Jj][Ee][Cc][Tt][Uu][Rr][Ee]\\b|\\b[Cc][Oo][Nn][Tt][Aa][Ii][Nn][Ii][Nn][Gg]\\b|\\b[Cc][Oo][Nn][Vv][Ee][Rr][Ss][Ii][Oo][Nn][\\+\\-]?|\\b[Cc][Oo][Rr][Oo][Ll][Ll][Aa][Rr][Yy]\\b",
        "name": "keyword"
    },
    {
        "match": "\\b[Dd][Aa][Tt][Aa][Tt][Yy][Pp][Ee]\\b|\\b[Ee][Ll][Ss][Ee]\\b|\\b[Ee][Ll][Ss][Ii][Ff]\\b|\\b[Ee][Nn][Dd]\\b|\\b[Ee][Nn][Dd][Aa][Ss][Ss][Uu][Mm][Ii][Nn][Gg]\\b|\\b[Ee][Nn][Dd][Cc][Aa][Ss][Ee][Ss]\\b|\\b[Ee][Nn][Dd][Cc][Oo][Nn][Dd]\\b|\\b[Ee][Nn][Dd][Ii][Ff]\\b|\\b[Ee][Nn][Dd][Tt][Aa][Bb][Ll][Ee]\\b|\\b[Ee][Xx][Ii][Ss][Tt][Ss]\\b|\\b[Ee][Xx][Pp][Oo][Rr][Tt][Ii][Nn][Gg]\\b",
        "name": "keyword"
    },
    {
        "match": "\\b[Ff][Aa][Cc][Tt]\\b|\\b[Ff][Oo][Rr][Aa][Ll][Ll]\\b|\\b[Ff][Oo][Rr][Mm][Uu][Ll][Aa]\\b|\\b[Ff][Rr][Oo][Mm]\\b|\\b[Ff][Uu][Nn][Cc][Tt][Ii][Oo][Nn]\\b|\\b[Hh][Aa][Ss]_[Tt][Yy][Pp][Ee]\\b",
        "name": "keyword"
    },
    {
        "match": "\\b[Ii][Ff]\\b|\\b[Ii][Ff][Ff]\\b|\\b[Ii][Mm][Pp][Ll][Ii][Ee][Ss]\\b|\\b[Ii][Mm][Pp][Oo][Rr][Tt][Ii][Nn][Gg]\\b|\\b[Ii][Nn]\\b|\\b[Ii][Nn][Dd][Uu][Cc][Tt][Ii][Vv][Ee]\\b|\\b[Jj][Uu][Dd][Gg][Ee][Mm][Ee][Nn][Tt]\\b|\\b[Ll][Aa][Mm][Bb][Dd][Aa]\\b|\\b[Ll][Aa][Ww]\\b|\\b[Ll][Ee][Mm][Mm][Aa]\\b|\\b[Ll][Ee][Tt]\\b|\\b[Ll][Ii][Bb][Rr][Aa][Rr][Yy]\\b",
        "name": "keyword"
    },
    {
        "match": "\\b[Mm][Aa][Cc][Rr][Oo]\\b|\\b[Mm][Ee][Aa][Ss][Uu][Rr][Ee]\\b|\\b[Nn][Oo][Nn][Ee][Mm][Pp][Tt][Yy]_[Tt][Yy][Pp][Ee]\\b|\\b[Nn][Oo][Tt]\\b|\\b[Oo][Bb][Ll][Ii][Gg][Aa][Tt][Ii][Oo][Nn]\\b|\\b[Oo][Ff]\\b|\\b[Oo][Rr]\\b|\\b[Oo][Rr][Ee][Ll][Ss][Ee]\\b",
        "name": "keyword"
    },
    {
        "match": "\\b[Pp][Oo][Ss][Tt][Uu][Ll][Aa][Tt][Ee]\\b|\\b[Pp][Rr][Oo][Pp][Oo][Ss][Ii][Tt][Ii][Oo][Nn]\\b|\\b[Rr][Ee][Cc][Uu][Rr][Ss][Ii][Vv][Ee]\\b|\\b[Ss][Uu][Bb][Ll][Ee][Mm][Mm][Aa]\\b|\\b[Ss][Uu][Bb][Tt][Yy][Pp][Ee][Ss]\\b|\\b[Ss][Uu][Bb][Tt][Yy][Pp][Ee]_[Oo][Ff]\\b",
        "name": "keyword"
    },
    {
        "match": "\\b[Tt][Aa][Bb][Ll][Ee]\\b|\\b[Tt][Hh][Ee][Nn]\\b|\\b[Tt][Hh][Ee][Oo][Rr][Ee][Mm]\\b|\\b[Tt][Hh][Ee][Oo][Rr][Yy]\\b|\\b[Tt][Yy][Pp][Ee](?:\\+|\\b)|\\b[Vv][Aa][Rr]\\b|\\b[Ww][Hh][Ee][Nn]\\b|\\b[Ww][Hh][Ee][Rr][Ee]\\b|\\b[Ww][Ii][Tt][Hh]\\b|\\b[Xx][Oo][Rr]\\b",
        "name": "keyword"
    },
    {
        "match": "\\(\\#|\\#\\)|\\[\\#|\\#\\]|\\:\\=|\\<?\\=\\>|\\-\\>",
        "name": "keyword"
    },
    {
        "match": "∀|∃|⇒|⇔|¬",
        "name": "keyword"
    }
    ],
    class: "hljs-keyword" // HTML class
}

// regex for numeric constants (hljs-number)
const numbers = {
    pattern: {
            "match": "([+-]?)\\b(\\d+\/\\d+)|([+-]?)\\b(\\d+(?:\\.\\d+)?)",
            "name": "constant.numeric"
    },
    class: "hljs-number" // HTML class
}

// regex for comments (hljs-comment)
const comments = {
    pattern: {
        "match": "%.*",
        "name": "comment.line.percentage"
    },
    class: "hljs-comment" // HTML class
}

// regex for markdown headings
const markdownHeadingRegex = [
    /\<span class\=\"hljs\-comment\"\>\%\s*\#\s+(.+)\<\/span\>/g,
    /\<span class\=\"hljs\-comment\"\>\%\s*\#\#\s+(.+)\<\/span\>/g,
    /\<span class\=\"hljs\-comment\"\>\%\s*\#\#\#\s+(.+)\<\/span\>/g
];

// this regex is used for restoring pvs functions in the form f[a,b](id) which are incorrectly treated as links
// there's no easy way to distinguish pvs functions f[a,b](id) from hyperlinks, so hyperlinks need to be restored afterwards, by looking at the content of spans representing comments
// in the example f[a,b](id), group 1 is (id), group 2 is [a,b]
const hrefRegex = /\<a href\=(?:.+)data\-href=\"(.+)\"\s*title=\"(?:.+)\"(?:\>|&gt;)(.+)\<\/a\>/g;
// the following regex expressions are used to re-create real hyperlinks removed with hrefRegex
const spanRegex = /\<span class="hljs-comment">(.+)<\/span>/g;
const markdownLinkRegex = /\[(.+)\]\((.+)\)/g;

/**
 * Utility function, applies syntax highlighting by replacing pvs language keywords with <span class="hljs-keyword">keyword</span>
*/
function applySyntaxHighlighing (innerHTML) {
    if (innerHTML) {
        // remove any <em> block, these are leftovers from unintended markdown matches on pvs expressions form *text*
        innerHTML = innerHTML.replace(/\<\/?em\>/, "");
        let svgBlock = false;
        // process all lines to apply syntax highlighting
        const lines = innerHTML.split("\n");
        for (let i = 0; i < lines.length; i++) {
            // fix mis-recognized href, this happens for parametric functions, e.g., f[a,b](id) is transformed into a f<a href="id" data-href="id" title="id">a,b</a>
            let hrefs = new RegExp(hrefRegex, "g");
            if (hrefs.test(lines[i])) {
                // console.log(lines[i]);
                lines[i] = lines[i].replace(hrefRegex, `[$2]($1)`).replace(/\%5B/g,"[").replace(/\%5D/g,"]");
            }

            let commentRegex = new RegExp(comments.pattern.match, "g");
            // preserve images and svgs
            if (lines[i].includes("<img src=") || lines[i].includes("<svg ")) {
                // keep track of whether this is an svg block
                svgBlock = lines[i].includes("<svg ");
                // and skip syntax highlighting in both cases
            } else if (svgBlock) {
                // skip syntax highlighting and find end of svg block
                if (lines[i].includes("</svg>")) {
                    svgBlock = false;
                }
            } else if (commentRegex.test(lines[i])) {
                // syntax highlighting for comments
                lines[i] = lines[i].replace(commentRegex, (match) => { return `<span class="${comments.class}">${match}</span>`; });
            } else {
                // syntax highlighting for numbers
                let numbersRegex = new RegExp(numbers.pattern.match, "g");
                if (numbersRegex.test(lines[i])) {
                    lines[i] = lines[i].replace(numbersRegex, (match) => { return `<span class="${numbers.class}">${match}</span>`; });
                }
                // syntax highlighting for keywords
                for (let k = 0; k < keywords.patterns.length; k++) {
                    let regex = new RegExp(keywords.patterns[k].match, "g");
                    if (regex.test(lines[i])) {
                        lines[i] = lines[i].replace(regex, (match) => { return `<span class="${keywords.class}">${match}</span>`; });
                    }
                }
            }

            // append heading tags
            for (let h = 0; h < markdownHeadingRegex.length; h++) {
                let headingRegex = new RegExp(markdownHeadingRegex[h], "g");
                if (headingRegex.test(lines[i])) {
                    lines[i] = lines[i].replace(headingRegex, `<h${h+1} class="${comments.class}">$1</h${h+1}>`);
                }
            }

            // restore hyperlinks
            let span = new RegExp(spanRegex, "g");
            let match = span.exec(lines[i]);
            if (match) {
                let href = new RegExp(markdownLinkRegex, "g");
                if (href.test(match[0])) {
                    lines[i] = lines[i].replace(href, `<a href=\"$2\">$1</a>`);
                }
            }

        }
        innerHTML = lines.join("\n");
    }
    return innerHTML;
}
/**
 * Utility function, gets the current vscode theme kind, e.g., "vscode-dark", "vscode-light"
 */
function getThemeKind () {
    return document.body.getAttribute("data-vscode-theme-kind");
}
/**
 * Utility function, appends necessary styles to the page
 */
function appendStyles () {
    const css = window.document.styleSheets[0];
    css.insertRule(`.markdown-body { border:1px solid; border-radius:4px; }`);
    css.insertRule(`.katex { color:#57A64A; }`);
    css.insertRule(`h1 { padding-top:0.3em !important; margin-bottom:0.1em !important;}`);
    css.insertRule(`body { padding-left:0 !important; padding-right:0 !important; }`);
    css.insertRule(`pre { background-color:transparent !important; margin:0 !important; padding:0.2em !important; font-family: Menlo, Monaco, "Courier New", monospace; font-size: 12px; line-height: 18px; letter-spacing: 0px; }`);
}
/**
 * Main function, modifies the rendered html content of the markdown preview to match the pvs style
 */
function updateWebviewContent () {
    // run script only if this is a .pvs file
    const base = document.getElementsByTagName("base")[0].getAttribute("href");
    if (base?.endsWith(".pvs")) {
        // append styles
        this.appendStyles();
        // repair the content of <em> elements that were erroneously generated by the markdown engine because of unintended matches with the pvs syntax
        const em = document.getElementsByTagName("em");
        for (let i = 0; i < em?.length; i++) {
            em[i].innerHTML = `* ${em[i].innerHTML} *`; // lgtm [js/xss]
        }

        // get all code lines rendered in the preview
        const lines = document.getElementsByClassName("code-line");
        for (let i = 0; i < lines.length; i++) {
            // create a pre-formatted element that incorporates the html content
            const pre = document.createElement("pre");
            pre.innerHTML = applySyntaxHighlighing(lines[i].innerHTML); // lgtm [js/xss]
            // remove original content
            lines[i].innerHTML = ""; // lgtm [js/xss]
            // append new content
            lines[i].appendChild(pre);
        }
    }
}
// run the script when the content is updated
window.addEventListener('vscode.markdown.updateContent', updateWebviewContent);
// run the script now
updateWebviewContent();