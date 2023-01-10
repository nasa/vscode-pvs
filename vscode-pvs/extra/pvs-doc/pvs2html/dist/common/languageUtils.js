"use strict";
/**
 * @module languageUtils
 * @author Paolo Masci
 * @date 2019.06.18
 * @copyright
 * Copyright 2019 United States Government as represented by the Administrator
 * of the National Aeronautics and Space Administration. All Rights Reserved.
 *
 * Disclaimers
 *
 * No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY
 * WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY,
 * INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE
 * WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM
 * INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR
 * FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO
 * THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER,
 * CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT
 * OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY
 * OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.
 * FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES
 * REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE,
 * AND DISTRIBUTES IT "AS IS."
 *
 * Waiver and Indemnity: RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS
 * AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND
 * SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF
 * THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES,
 * EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM
 * PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT
 * SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED
 * STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY
 * PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE
 * REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL
 * TERMINATION OF THIS AGREEMENT.
 **/
Object.defineProperty(exports, "__esModule", { value: true });
exports.pvsioPrompt = exports.pvsioBannerAlt = exports.pvsioBanner = exports.listSymbols = exports.getWordRange = exports.proofliteTagsRegexp = exports.proofRegexp = exports.isValidProofliteScript = exports.commentProofliteScript = exports.proofliteDeclRegexp = exports.proofliteRegexp = exports.makeProofliteHeader = exports.desc2id = exports.pvsSyntaxHighlighting = exports.isense = exports.prfRegExp = exports.tupletuplelist2PlotData = exports.tuple2PlotData = exports.list2PlotData = exports.isTupleTupleListExpr = exports.isTupleExpr = exports.isListExpr = exports.symbolRegexp = exports.scatterPlotRegexp = exports.linearPlotTupleTupleListRegexp = exports.linearPlotSimpleRegexp = exports.linearPlotRegexp = exports.tupleRegexp = exports.listRegexp = exports.typesRegexp = exports.validTermRegExp = exports.simpleImportingRegexp = exports.isTccFormula = exports.tccFormulaRegexp = exports.tccStatusRegExp = exports.tccRegexp = exports.theoremKeywordRegexp = exports.theoremParamsRegexp = exports.theoremRegexp = exports.formulaRegexp = exports.declarationRegexp = exports.theoryOrDatatypeRegexp = exports.datatypeRegexp = exports.endTheoryOrDatatypeRegexp = exports.theoryRegexp = exports.stringRegexp = exports.commentRegexp = exports.markdownRegexp = exports.RECORD = exports.sanitizeForRegEx = void 0;
exports.PROOF_COMMANDS_BASIC_PROFILE = exports.PROOF_COMMANDS_ADVANCED_PROFILE = exports.EVALUATOR_COMMANDS = exports.PROOF_TACTICS = exports.PROOF_COMMANDS = exports.printHelp = exports.formatHelp = exports.getPvsTag = exports.PvsTag = exports.includesTheoryTag = exports.makeTheoryHeader = exports.makeEmptyTheory = exports.isUnchecked = exports.isProved = exports.getIcon = exports.icons = exports.pathHasChanged = exports.branchHasChanged = exports.siblingBranchComplete = exports.branchComplete = exports.interruptedByClient = exports.isInterruptCommand = exports.isGlassboxTactic = exports.QED = exports.getNoChangeCommand = exports.noChange = exports.applyTimeout = exports.proofTree2commandSequence = exports.isMetaProofCommand = exports.isHelpBangCommand = exports.helpBangCommandRegexp = exports.isHelpVSCodePlot = exports.isHelpStarCommand = exports.isHelpCommand = exports.helpCommandRegexp = exports.prf2jprf = exports.isEmptyProof = exports.isEmptyPrf = exports.prf2ProofTree = exports.proofDescriptor2ProofLite = exports.proofLite2ProofDescriptor = exports.proofLite2proofTree = exports.proofTree2Prl = exports.pvsVersionToString = exports.getBranchId = exports.makeBranchId = exports.getErrorRange = exports.expectingTypeRegExp = exports.errorCannotFindTheoryRegExp = exports.proverPrompt = void 0;
exports.forceLocale = exports.getInvalidCommandName = exports.isInvalidCommand = exports.isPropax = exports.isSameCommand = exports.isQEDCommand = exports.isCommentCommand = exports.splitCommands = exports.isShowFullyExpandedSequentCommand = exports.isShowExpandedSequentCommand = exports.isProofliteGlassbox = exports.isGrindCommand = exports.isShowHiddenFormulas = exports.isFailCommand = exports.isSkipCommand = exports.isPostponeCommand = exports.isUndoStarCommand = exports.isUndoUndoPlusCommand = exports.isUndoUndoCommand = exports.isRedoCommand = exports.unfoldUndoCommand = exports.isUndoCommand = exports.isEmptyCommand = exports.isLispCommand = exports.isQuitDontSaveCommand = exports.isSaveThenQuitCommand = exports.isQuitCommand = exports.getVSCodePlotExpression = exports.isPVSioweb = exports.isVSCodePlotCommand = exports.balancePar = exports.checkPar = exports.getHints = exports.proverCommands = exports.evaluatorCommands = exports.VSCODE_COMMANDS = exports.getCommands = void 0;
const language = require("./languageKeywords");
const serverInterface_1 = require("../common/serverInterface");
const colorUtils = require("./colorUtils");
const languageKeywords_1 = require("./languageKeywords");
/**
 * Utility function sanitizes formula name so it can be used in a regex
 */
function sanitizeForRegEx(str) {
    if (str) {
        return str.replace(/\?/g, "\\\?"); // lgtm [js/incomplete-sanitization]
    }
    return str;
}
exports.sanitizeForRegEx = sanitizeForRegEx;
// records literals are in the form id: ID = (# ac1: Ac1, ac2: Ac2 #)
// record types are in the form Rec: TYPE = [# x: nat, y: real #]
exports.RECORD = {
    declaration: /(\w+)\s*:\s*(\w+)(\s*=\s*[\[\(]#.+[\]\)])?/g,
    isLiteral: /\(#.+#\)/g,
    isType: /\[#.+#\]/g,
    accessors: /[\(\[]#(.+)#[\]\)]/g,
    typeName: /\w+\s*:\s*(\w+)/g
};
// markdown link regexp, group 1 is the human-readable name, group 2 is the url
exports.markdownRegexp = /\s*%\s*\!\[(.+)\]\((.+)\)/g;
exports.commentRegexp = /%.*/g; // pvs comment line
exports.stringRegexp = /\"[^\"]+\"/g;
// group 1 is theoryName, group 2 is comma-separated list of theory parameters -- NB: this regexp is fast but not accurate, because it does not check the end of the theory. See example use in codelense.
exports.theoryRegexp = /([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*(?:\[([\w\W\s]+)\])?\s*\:\s*THEORY\b/gi; //\s+BEGIN\b/gi;
function endTheoryOrDatatypeRegexp(theoryName) {
    return new RegExp(`(\\bEND\\s*)(${theoryName})(\\b)`, "gi");
}
exports.endTheoryOrDatatypeRegexp = endTheoryOrDatatypeRegexp;
exports.datatypeRegexp = /([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*(?:\[([\w\W\s]+)\])?\s*\:\s*DATATYPE\b/gi; //\s+(?:BEGIN|WITH)\b/gi;
exports.theoryOrDatatypeRegexp = /([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*(?:\[([\w\W\s]+)\])?\s*\:\s*(?:THEORY|DATATYPE)\b/gi;
exports.declarationRegexp = /([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*(?:\[([\w\W\s]+)\])?\s*\:/gi;
// group 1 is the formula name
exports.formulaRegexp = /^\s*\n\s*([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*(?:\[\s*\w+(?:\s*,\s*\w+)*\s*\:\s*(?:NONEMPTY_)?TYPE\+?\s*\])?\s*:\s*(?:CHALLENGE|CLAIM|CONJECTURE|COROLLARY|FACT|FORMULA|LAW|LEMMA|PROPOSITION|SUBLEMMA|THEOREM|OBLIGATION|JUDGEMENT|AXIOM)\b/gim;
// same as formulaRegExp, but does not include AXIOM
exports.theoremRegexp = /^\s*\n\s*([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*(?:\[\s*\w+(?:\s*,\s*\w+)*\s*\:\s*(?:NONEMPTY_)?TYPE\+?\s*\])?\s*:\s*(?:CHALLENGE|CLAIM|CONJECTURE|COROLLARY|FACT|FORMULA|LAW|LEMMA|PROPOSITION|SUBLEMMA|THEOREM|OBLIGATION|JUDGEMENT)\b/gim;
exports.theoremParamsRegexp = /\s*(?:\[\s*\w+(?:\s*,\s*\w+)*\s*\:\s*(?:NONEMPTY_)?TYPE\+?\s*\])?\s*:\s*(?:CHALLENGE|CLAIM|CONJECTURE|COROLLARY|FACT|FORMULA|LAW|LEMMA|PROPOSITION|SUBLEMMA|THEOREM|OBLIGATION|JUDGEMENT)\b/gim;
exports.theoremKeywordRegexp = /(?:CHALLENGE|CLAIM|CONJECTURE|COROLLARY|FACT|FORMULA|LAW|LEMMA|PROPOSITION|SUBLEMMA|THEOREM|OBLIGATION|JUDGEMENT)/gi;
// /(\w+)\s*(?:\%.*\s)*(?:\[([^\]]+)\])?\s*:\s*(?:\%.*\s)*\s*THEORY\b/gi;
exports.tccRegexp = /([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*:\s*OBLIGATION\b/gi;
exports.tccStatusRegExp = /%\s(proved|subsumed|simplified|unproved|unfinished|unchecked|untried)\b/g;
exports.tccFormulaRegexp = /[A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉\.]*_TCC\d+/g;
function isTccFormula(desc) {
    return desc && desc.formulaName && new RegExp(exports.tccFormulaRegexp).test(desc.formulaName);
}
exports.isTccFormula = isTccFormula;
// group 1 is a list of comma-separated list of imported theories. This regexp works only for theory names without parameters
exports.simpleImportingRegexp = /\bIMPORTING\s+((?:(?:[A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)+)(?:\s*,\s*(?:[A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)+)*)/gi;
// group 0 is the term name
exports.validTermRegExp = /[A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*/g;
/**
 *  group 1 is a list of type names
 *  this is a fast regex capturing types declared on a "clean line" in the following form
 * - t1: TYPE
 * - t1[params]: TYPE
 * - t1,t2,..., t3[params],..: TYPE
 */
exports.typesRegexp = /^\s*([A-Za-z].+):\s*(?:TYPE\+?)\b/gim;
// group 1 is the list of comma-separated values contained in the expression 
exports.listRegexp = /\(:([\(\)\s\+\-\w\/\,\.]*):\)/g;
// generic regular expression for recognizing tuples
exports.tupleRegexp = /\(([\(\)\s\+\-\w\/\,\:\#\.]*)\)/g;
// generic regular expressions for recognizing plottable expression series
exports.linearPlotRegexp = /([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*(?:\[([\w\W\s]+)\])?\s*\:\s*\[\s*list\s*\[\s*(?:real|int|rat|nat)\s*\](?:\,\s*list\s*\[\s*(?:real|int|rat|nat)\s*\])*\s*\]\s*=/g;
exports.linearPlotSimpleRegexp = /([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*(?:\[([\w\W\s]+)\])?\s*\:\s*list\s*\[\s*(?:real|int|rat|nat)\s*\]\s*=/g;
exports.linearPlotTupleTupleListRegexp = /([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*(?:\[([\w\W\s]+)\])?\s*\:\s*\[(?:\,?\s*(?:\[\s*list\s*\[\s*(?:real|int|rat|nat)\s*\](?:\,\s*list\s*\[\s*(?:real|int|rat|nat)\])*\s*\]))+\s*\]\s*=/g;
// generic regular expression for recognizing plottable expression series
exports.scatterPlotRegexp = /([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*(?:\[([\w\W\s]+)\])?\s*\:\s*list\s*\[\s*\[\s*(?:real|int|rat|nat)\s*,\s*(?:real|int|rat|nat)\s*\]\s*\]\s*=/g;
// generic regular expression for symbol names, group 1 is the symbol name
exports.symbolRegexp = /([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)/g;
/**
 * Utility function, checks if a given pvs expression is a list
 */
function isListExpr(expr) {
    if (expr === null || expr === void 0 ? void 0 : expr.trim().startsWith("(:")) {
        return new RegExp(exports.listRegexp).test(expr);
    }
    return false;
}
exports.isListExpr = isListExpr;
/**
 * Utility function, checks if a given pvs expression is a list
 */
function isTupleExpr(expr) {
    if (expr === null || expr === void 0 ? void 0 : expr.trim().startsWith("(")) {
        return new RegExp(exports.tupleRegexp).test(expr);
    }
    return false;
}
exports.isTupleExpr = isTupleExpr;
/**
 * Utility function, checks if a given pvs expression is a list
 */
function isTupleTupleListExpr(expr) {
    if (expr === null || expr === void 0 ? void 0 : expr.trim().startsWith("(((:")) {
        return new RegExp(exports.listRegexp).test(expr);
    }
    return false;
}
exports.isTupleTupleListExpr = isTupleTupleListExpr;
;
const bigIntTrigger = 128;
/**
 * Utility function, tries to convert a pvs list expression into plot data.
 */
function list2PlotData(data, opt) {
    const res = {
        x: [],
        y: [],
        mode: (opt === null || opt === void 0 ? void 0 : opt.mode) || "lines+markers"
    };
    function processSeries(desc) {
        var _a, _b;
        for (let i = 0; i < ((_a = desc === null || desc === void 0 ? void 0 : desc.ySeries) === null || _a === void 0 ? void 0 : _a.length); i++) {
            const xelem = i < ((_b = desc.xSeries) === null || _b === void 0 ? void 0 : _b.length) ? desc.xSeries[i] : `${i}`;
            const yelem = desc.ySeries[i];
            const xas = xelem.split("/")[0];
            const xbs = xelem.split("/")[1] || "1";
            let x = +xas / +xbs;
            if (xas.length > bigIntTrigger || xbs.length > bigIntTrigger) {
                // use bigint to correctly handle pvsio output
                let scale = Math.max(xas.length, xbs.length);
                scale *= scale;
                const xx = Number(BigInt(xas) * BigInt(scale) / BigInt(xbs));
                x = xx / scale;
            }
            const yas = yelem.split("/")[0];
            const ybs = yelem.split("/")[1] || "1";
            let y = +yas / +ybs;
            if (yas.length > bigIntTrigger || ybs.length > bigIntTrigger) {
                // use bigint to correctly handle pvsio output
                let scale = Math.max(yas.length, ybs.length);
                scale *= scale;
                const yy = Number(BigInt(yas) * BigInt(scale) / BigInt(ybs));
                y = yy / scale;
            }
            res.x.push(x);
            res.y.push(y);
            res.name = `series ${isNaN(+(opt === null || opt === void 0 ? void 0 : opt.seriesId)) ? 1 : +opt.seriesId}`;
        }
    }
    const match = new RegExp(exports.listRegexp).exec(data);
    if (match && match.length > 1) {
        const dataset = match[1];
        let xSeries = [];
        let ySeries = [];
        if (dataset.includes(")")) {
            // list of tuples, use scatter plot
            res.mode = "markers";
            const tuples = dataset.split(/\s*\),\s*\(/).map((elem) => {
                return elem === null || elem === void 0 ? void 0 : elem.replace(/[\(\)]/g, "");
            });
            for (let i = 0; i < tuples.length; i++) {
                const pair = tuples[i].split(",");
                if (pair.length > 1) {
                    xSeries.push(pair[0]);
                    ySeries.push(pair[1]);
                }
            }
        }
        else {
            // assume this is a list of numbers
            xSeries = opt === null || opt === void 0 ? void 0 : opt.x;
            ySeries = (dataset === null || dataset === void 0 ? void 0 : dataset.split(",")) || null;
        }
        processSeries({ xSeries, ySeries });
    }
    return res;
}
exports.list2PlotData = list2PlotData;
/**
 * Utility function, tries to convert a pvs tuple of lists of real numbers into plot data.
 * The first element of the tuple is treated as list of x coordinates.
 * The other elements are data points belonging to different series.
 */
function tuple2PlotData(data, opt) {
    opt = opt || {};
    const res = [];
    const regexp = new RegExp(exports.tupleRegexp);
    let match = regexp.exec(data);
    let id = 1;
    if ((match === null || match === void 0 ? void 0 : match.length) > 1 && match[1]) {
        const sep = match[1].indexOf(":)");
        // get labels
        const x = match[1].substring(0, sep).replace("(:", "").split(",");
        // get series
        const series = match[1].substring(sep + 1);
        const regexpSeries = new RegExp(exports.listRegexp);
        let matchSeries = null;
        while (matchSeries = regexpSeries.exec(series)) {
            if ((matchSeries === null || matchSeries === void 0 ? void 0 : matchSeries.length) > 1 && matchSeries[1]) {
                const datapoints = `(: ${matchSeries[1]} :)`;
                const plot_i = list2PlotData(datapoints, Object.assign(Object.assign({}, opt), { x, seriesId: id++ }));
                res.push(plot_i);
            }
        }
    }
    return res;
}
exports.tuple2PlotData = tuple2PlotData;
/**
 * Utility function, tries to convert a pvs tuple of tuple of list expression into plot data.
 * The odd element of the tuple is treated as list of x coordinates for the different data series.
 * The even elements are data points belonging to different series.
 */
function tupletuplelist2PlotData(data, opt) {
    opt = opt || {};
    const res = [];
    let id = 1;
    // get series
    // const series: string = match[0];
    const regexpSeries = new RegExp(exports.listRegexp);
    let matchSeriesX = null;
    let matchSeriesY = null;
    while (matchSeriesX = regexpSeries.exec(data)) {
        matchSeriesY = regexpSeries.exec(data);
        if ((matchSeriesX === null || matchSeriesX === void 0 ? void 0 : matchSeriesX.length) > 1 && matchSeriesX[1]
            && (matchSeriesY === null || matchSeriesY === void 0 ? void 0 : matchSeriesY.length) > 1 && matchSeriesY[1]) {
            // get labels
            const x = matchSeriesX[1].replace("(:", "").split(",");
            // get datapoints
            const datapoints = `(: ${matchSeriesY[1]} :)`;
            // get plot data
            const plot_i = list2PlotData(datapoints, Object.assign(Object.assign({}, opt), { x, seriesId: id++ }));
            res.push(plot_i);
        }
    }
    return res;
}
exports.tupletuplelist2PlotData = tupletuplelist2PlotData;
// capture group 1 is proofName
// capture group 2 is formulaName,
// capture group 3 is proofTree
exports.prfRegExp = /;;; Proof\s+([\w\?₀₁₂₃₄₅₆₇₈₉\.\-]+)\s+for formula\s+([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉\.]*)\s+(\((?:\"\")?[\n\w\W]+)/g;
;
exports.isense = {
    recordExpression: /(\w+)\s*=\s*\(\#(?:\s*(?:\w+)\s*:=(?:.*,)?)*/g,
    recordAccessor: /(\w+)`/g,
    theoryDeclaration: exports.theoryRegexp,
    datatypeDeclaration: exports.datatypeRegexp,
    formulaDeclaration: exports.formulaRegexp,
    declaration: exports.declarationRegexp
};
/**
 * Utility function, applies pvs syntax highlighting to the provided text
 */
function pvsSyntaxHighlighting(text, opt) {
    if (text) {
        opt = opt || {};
        const colorTheme = opt.colorTheme || "dark";
        // numbers and operators should be highlighted first, otherwise the regexp will change characters introduced to colorize the string
        const number_regexp = new RegExp(language.PVS_NUMBER_REGEXP_SOURCE, "g");
        text = text.replace(number_regexp, (number) => {
            return colorUtils.colorText(number, colorUtils.getColor(colorUtils.PvsColor.yellow, colorTheme));
        });
        if (opt.colorizeParens) {
            let colorIndex = -1;
            const paren_regexp = new RegExp(/[\(\)]/, "g");
            text = text.replace(paren_regexp, (paren) => {
                if (paren === "(") {
                    colorIndex++;
                }
                const res = colorUtils.colorText(paren, colorUtils.getParenColor(colorIndex));
                if (paren === ")") {
                    colorIndex--;
                }
                return res;
            });
        }
        const operators_regexp = new RegExp(language.PVS_LANGUAGE_OPERATORS_REGEXP_SOURCE, "g");
        text = text.replace(operators_regexp, (op) => {
            return colorUtils.colorText(op, colorUtils.getColor(colorUtils.PvsColor.blue, colorTheme));
        });
        const keywords_regexp = new RegExp(language.PVS_RESERVED_WORDS_REGEXP_SOURCE, "gi");
        text = text.replace(keywords_regexp, (keyword) => {
            return colorUtils.colorText(keyword, colorUtils.getColor(colorUtils.PvsColor.blue, colorTheme));
        });
        const function_regexp = new RegExp(language.PVS_LIBRARY_FUNCTIONS_REGEXP_SOURCE, "g");
        text = text.replace(function_regexp, (fname) => {
            return colorUtils.colorText(fname, colorUtils.getColor(colorUtils.PvsColor.green, colorTheme));
        });
        const builtin_types_regexp = new RegExp(language.PVS_BUILTIN_TYPE_REGEXP_SOURCE, "g");
        text = text.replace(builtin_types_regexp, (tname) => {
            return colorUtils.colorText(tname, colorUtils.getColor(colorUtils.PvsColor.green, colorTheme));
        });
        const truefalse_regexp = new RegExp(language.PVS_TRUE_FALSE_REGEXP_SOURCE, "gi");
        text = text.replace(truefalse_regexp, (tf) => {
            return colorUtils.colorText(tf, colorUtils.getColor(colorUtils.PvsColor.blue, colorTheme));
        });
    }
    return text;
}
exports.pvsSyntaxHighlighting = pvsSyntaxHighlighting;
function desc2id(desc) {
    if (desc) {
        if (desc.formulaName) {
            return desc.formulaName;
        }
        if (desc.fileName) {
            return desc.fileName;
        }
    }
    console.error("[languageUtils.desc2id] Warning: trying to generate ID from null descriptor");
    return null;
}
exports.desc2id = desc2id;
/**
 * Utility function, returns the header for a prooflite script
 * @param formulaName Formula proved by this prooflite script
 * @param theoryName Theory of the formula
 * @param status Proof status
 */
function makeProofliteHeader(pvsfile, formulaName, theoryName, status) {
    return `%-------------------------------------------
% @formula: ${formulaName} 
% @theory: ${theoryName}
% @pvsfile: ${pvsfile}
% @status: ${getIcon(status)} ${status}
%-------------------------------------------\n`;
}
exports.makeProofliteHeader = makeProofliteHeader;
/**
 * Utility function, returns the prooflite script for the theorem indicated in the fuction arguments
 * @param desc
 */
// export async function getProofLiteScript (desc: { 
// 	fileName: string, 
// 	fileExtension: string, 
// 	contextFolder: string, 
// 	theoryName: string, 
// 	formulaName: string
// }): Promise<string> {
// 	if (desc) {
// 		let proofScript: string = makeProofliteHeader(desc.formulaName, desc.theoryName, "untried");
// 		// check if the .jprf file contains the proof status
// 		const jprf_file: string = fsUtils.desc2fname({
// 			fileName: desc.fileName, 
// 			fileExtension: ".jprf", 
// 			contextFolder: desc.contextFolder
// 		});
// 		const proofFile: ProofFile = await readJprfProofFile(jprf_file);
// 		if (proofFile) {
// 			const proofDescriptors: ProofDescriptor[] = proofFile[`${desc.theoryName}.${desc.formulaName}`];
// 			if (proofDescriptors && proofDescriptors.length && proofDescriptors[0] && proofDescriptors[0].info) {
// 				proofScript = makeProofliteHeader(desc.formulaName, desc.theoryName, proofDescriptors[0].info.status);
// 				const proofLite: string[] = proofDescriptor2ProofLite(proofDescriptors[0]);
// 				if (proofLite && proofLite.length) {
// 					proofScript += proofLite.join("\n");
// 				}
// 			}
// 		}
// 		return proofScript;
// 	}
// 	return null;
// }
/**
 * Regex for parsing the content of prooflite files
 * The regex returns the following groups
 * group 0: the prooflite script for the formula indicated in desc
 * group 1: formulaName
 * group 2: theoryName
 * group 3: proof status
 * group 4: prooflite script
 * @param desc
 */
function proofliteRegexp(desc) {
    const formulaName = sanitizeForRegEx(desc.formulaName);
    const theoryName = sanitizeForRegEx(desc.theoryName);
    return new RegExp(`(?:(?:%--*.*)\\s*(?:%\\s*@formula\\s*:\\s*(${formulaName}))?\\s*(?:%\\s*@theory\\s*:\\s*(${theoryName}))?\\s*(?:%\\s*@status\\s*:\\s*(.*))?\\s*(?:%--*.*))?\\s*(${desc.formulaName}\\s*:\\s*PROOF[\\w\\W\\s]*QED\\s*${desc.formulaName})`, "g");
}
exports.proofliteRegexp = proofliteRegexp;
function proofliteDeclRegexp(desc) {
    const formulaName = desc.formulaName.replace(/\?/g, "\\\?"); // lgtm [js/incomplete-sanitization]
    return new RegExp(`\\b${formulaName}\\s*:\\s*PROOF\\b`, "g");
}
exports.proofliteDeclRegexp = proofliteDeclRegexp;
/**
 * Utility function, introduces '%|- ' before each line of a given proof script
 */
function commentProofliteScript(script) {
    if (script) {
        return script.split("\n").map(line => {
            return line.trim().startsWith("%|-") ? line : "%|- " + line;
        }).join("\n");
    }
    return script;
}
exports.commentProofliteScript = commentProofliteScript;
/**
 * Utility function, checks if the provided script is a valid prooflite script
 */
function isValidProofliteScript(script) {
    if (script) {
        const lines = script.split("\n");
        for (let i = 0; i < lines.length; i++) {
            const line = lines[i].trim();
            if (line && !line.startsWith("%|-")) {
                return false;
            }
        }
        return true;
    }
    return false;
}
exports.isValidProofliteScript = isValidProofliteScript;
// group 1 is formula name
exports.proofRegexp = /([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*(%.+)?\s*:\s*(%.+)?\s*(?:PROOF)\b/gim;
// group 1 is the file name where the theory/formula is defined
function proofliteTagsRegexp(desc) {
    if (desc && desc.theoryName && desc.formulaName) {
        return new RegExp(`%-+\\n%\\s*@formula:\\s*${desc.formulaName}\\s*\\n%\\s*@theory:\\s*${desc.theoryName}\\s*\\n%\\s*@pvsfile:\\s*(.+)\\n`);
    }
    return null;
}
exports.proofliteTagsRegexp = proofliteTagsRegexp;
/**
 * @function getWordRange
 * @TODO improve this function, currently operators are not recognized/resolved
 * @description Utility function, identifies the range of the word at the cursor position.
 * 				Uses regular expressions designed to identify symbol names (\w+) and strings (\"([^\"]*)\")
 * @param txt The document that contains the word
 * @param position Position in the document
 */
function getWordRange(txt, position) {
    const testTokenizer = (regexp, txt) => {
        let match = regexp.exec(txt);
        let needle = -1;
        let token = null;
        while (match && match.index <= position.character) {
            needle = match.index;
            token = match[0];
            match = regexp.exec(txt);
        }
        if (needle >= 0 && needle + token.length >= position.character) {
            character = needle;
            len = token.length;
            return { token, character: needle };
        }
        return null;
    };
    let character = position.character;
    let len = 0;
    const lines = txt.split("\n");
    if (lines && lines.length > position.line) {
        const txt = lines[position.line];
        const strings = /\"[\w\W]+?\"/g;
        const numbers = /([+-]?\d+\.?\d*)\b/g;
        const keywords = new RegExp(language.PVS_RESERVED_WORDS_REGEXP_SOURCE, "gi");
        const symbols = /[A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*/g; ///(\w+\??)/g;
        let ans = testTokenizer(new RegExp(strings), txt)
            || testTokenizer(new RegExp(numbers), txt)
            || testTokenizer(new RegExp(keywords), txt)
            || testTokenizer(new RegExp(symbols), txt);
        if (ans) {
            character = ans.character;
            len = ans.token.length;
        }
    }
    return {
        start: { line: position.line, character: character },
        end: { line: position.line, character: character + len }
    };
}
exports.getWordRange = getWordRange;
/**
 * @function listSymbols
 * @TODO improve this function, currently operators are not recognized/resolved
 * @description Utility function, identifies symbols in the given text.
 * 				Uses regular expressions designed to identify symbol names (\w+) and strings (\"([^\"]*)\")
 * @param txt The document that contains the word
 * @param position Position in the document
 */
function listSymbols(txt) {
    if (txt) {
        const symbols = /(\w+\??)/g;
        const keywords = new RegExp(language.PVS_RESERVED_WORDS_REGEXP_SOURCE, "gi");
        let symbolsMap = {};
        let match = null;
        const lines = txt.split("\n");
        const maxIterations = 100;
        if (lines && lines.length) {
            for (let i = 0; i < lines.length; i++) {
                const txt = lines[i];
                if (txt) {
                    for (let n = 0; n < maxIterations && (match = symbols.exec(txt)); n++) {
                        if (match && !keywords.test(match[0]) && isNaN(+match[0])) { //isNaN(match[0]) is used to exclude numbers -- we don't want to include numbers in the list of symbols
                            symbolsMap[match[0]] = { line: i, character: match.index }; // position is used for debugging purposes here
                        }
                    }
                }
            }
        }
        return Object.keys(symbolsMap);
    }
    return [];
}
exports.listSymbols = listSymbols;
exports.pvsioBanner = `
╔════════════════════════════════════════════════════════════════════════════════════
║ PVSio Evaluator
║
║ Usage:
║ - Enter a PVS expression followed by ';'
║  or
║ - Enter a Lisp expression followed by '!'
║  or
║ - Exit the evaluator with 'quit;' 
║
║ TAB autocompletes commands.
║ UP/DOWN arrow keys recall previous/next command.
╚════════════════════════════════════════════════════════════════════════════════════
`;
exports.pvsioBannerAlt = `
╔════════════════════════════════════════════════════════════════════════════════════
║ PVSio Evaluator
║
║ Usage:
║ - Enter a PVS expression followed by ';'
║  or
║ - Enter a Lisp expression followed by '!'
╚════════════════════════════════════════════════════════════════════════════════════
`;
exports.pvsioPrompt = "<PVSio>";
exports.proverPrompt = ">>";
// ║
// ║ Note: Evaluating PVS expressions which depend on unproven TCCs may be unsound
// ║       and result in the evaluator becoming unresponsive or breaking into Lisp. 
// ║       If that happens, please close the evaluator terminal and start a new session.
exports.errorCannotFindTheoryRegExp = /Cannot find theory\s+(.+)/gi;
exports.expectingTypeRegExp = /Expecting a type\s+No resolution for\s+(.+)/gi;
/**
 * @function getErrorRange
 * @description Utility function, identifies the range of a syntax error at the cursor position.
 * @param txt The document that contains the word
 * @param start Position in the document where the error starts
 * @param end Position in the document where the error ends
 */
function getErrorRange(txt, start, end, message) {
    if (start && end) {
        if (end.line === start.line && end.character === start.character && message) {
            // try to use the message to adjust the diagnostics
            const cannotFindTheory = new RegExp(exports.errorCannotFindTheoryRegExp);
            let match = cannotFindTheory.exec(message);
            if ((match === null || match === void 0 ? void 0 : match.length) > 1 && match[1]) {
                const theoryName = match[1];
                end.character += theoryName.length;
            }
            const expectingType = new RegExp(exports.expectingTypeRegExp);
            match = expectingType.exec(message);
            if ((match === null || match === void 0 ? void 0 : match.length) > 1 && match[1]) {
                const typeName = match[1];
                end.character += typeName.length;
            }
        }
        return {
            start,
            end: {
                line: end.line,
                // indicate at least 2 characters otherwise the underlined error is hard to find in the text
                character: (start.line === end.line && start.character === end.character) ? end.character + 2 : end.character
            }
        };
    }
    else {
        let character = start.character;
        let len = 2; // indicate at least 2 characters otherwise the underlined error is hard to find in the text
        let lines = txt.split("\n");
        if (lines && (start.line - 1) < lines.length) {
            let txt = lines[start.line - 1];
            if (txt) {
                len = txt.length - start.character;
            }
        }
        return {
            start: { line: start.line, character: character },
            end: { line: start.line, character: character + len }
        };
    }
}
exports.getErrorRange = getErrorRange;
function makeBranchId(desc) {
    if (desc) {
        return (isNaN(desc.goalId)) ? desc.branchId
            : (desc.branchId) ? `${desc.branchId}.${desc.goalId}`
                : `${desc.goalId}`;
    }
    return "";
}
exports.makeBranchId = makeBranchId;
function getBranchId(label) {
    if (label) {
        return label.split(".").slice(1).join(".");
    }
    return "";
}
exports.getBranchId = getBranchId;
function pvsVersionToString(version) {
    if (version) {
        return `PVS ${version["pvs-version"]} (${version["lisp-version"]})`;
    }
    return null;
}
exports.pvsVersionToString = pvsVersionToString;
function proofTree2Prl(proofDescriptor) {
    const content = proofDescriptor2ProofLite(proofDescriptor, { omitTags: true, escape: true });
    if (content && content.length) {
        return content.join("\n");
    }
    return null;
}
exports.proofTree2Prl = proofTree2Prl;
function proofLite2proofTree(desc) {
    if (desc) {
        if (desc.parent) {
            let currentNode = desc.parent;
            let plite = desc.prf.trim();
            if (plite.startsWith("(then ")) {
                // series of proof commands
                plite = plite.replace("(then ", "");
                const cmdArray = splitCommands(plite);
                for (let i = 0; i < cmdArray.length; i++) {
                    if (cmdArray[i].startsWith("(spread ")) {
                        // series of proof branches
                        proofLite2proofTree({ prf: plite, parent: currentNode });
                    }
                    else {
                        currentNode = {
                            name: cmdArray[i],
                            type: "proof-command",
                            rules: [],
                            branch: currentNode.branch
                        };
                        desc.parent.rules.push(currentNode);
                    }
                }
            }
            else if (plite.startsWith("(spread ")) {
                // series of proof branches
                plite = plite.replace("(spread ", "");
                const cmdArray = splitCommands(plite);
                for (let i = 0; i < cmdArray.length; i++) {
                    currentNode = {
                        name: `(${i + 1})`,
                        type: "proof-branch",
                        rules: [],
                        branch: desc.parent.branch
                    };
                    desc.parent.rules.push(currentNode);
                    proofLite2proofTree({ prf: cmdArray[i], parent: currentNode });
                }
            }
            else if (plite.startsWith(`("" `)) {
                // proof start
                plite = plite.replace(`("" `, "");
                proofLite2proofTree({ prf: plite, parent: currentNode });
            }
        }
        else {
            desc.parent = {
                name: desc.proofName,
                rules: [],
                type: "root",
                branch: `""` // branch id			
            };
            proofLite2proofTree(desc);
        }
        return desc.parent;
    }
    return null;
}
exports.proofLite2proofTree = proofLite2proofTree;
function proofLite2ProofDescriptor(prf, info) {
    const pdesc = new serverInterface_1.ProofDescriptor(info, ".prl");
    const script = prf.replace(/\s*\n+\s*/g, " "); // remove all \n introduced by pvs in the expression
    // capture group 1 is proofName
    // capture group 2 is formulaName,
    // capture group 3 is proofTree
    const data = new RegExp(exports.prfRegExp).exec(script);
    if (data && data.length > 3) {
        // const proofName: string =  data[2];
        const prf = data[3];
        pdesc.proofTree = proofLite2proofTree({ prf, proofName: info.formula });
    }
    return pdesc;
}
exports.proofLite2ProofDescriptor = proofLite2ProofDescriptor;
/**
 * Utility function, converts a proof descriptor to a prooflite script
 * @param proofDescriptor
 * @param opt
 */
function proofDescriptor2ProofLite(proofDescriptor, opt) {
    opt = opt || {};
    const proofTreeToProofLite = (nodes, currentBranch, indent) => {
        indent = indent || 0;
        let res = "";
        if (nodes && nodes.length) {
            let closeRight = false;
            for (let i = 0; i < nodes.length; i++) {
                const node = nodes[i];
                switch (node.type) {
                    case "root": {
                        res += `${" ".repeat(indent)}` + proofTreeToProofLite(node.rules, "", indent);
                        break;
                    }
                    case "proof-command": {
                        if (node.branch === currentBranch && i === 0 && (!node.rules || node.rules.length === 0)) {
                            res += `(then `; // the parenthesis will be closed at the end of the loop
                            closeRight = true;
                        }
                        const cmd = node.name.startsWith("(") ? node.name : `(${node.name})`;
                        if (node.rules && node.rules.length) {
                            // these rules are sub-goals (proof-branches)
                            // make sure sub-goals are ordered in ascending order based on their name
                            node.rules = node.rules.sort((a, b) => {
                                const valA = +a.name.replace(/[\(\)]/g, "");
                                const valB = +b.name.replace(/[\(\)]/g, "");
                                return valA < valB ? -1 : 1;
                            });
                            indent++;
                            if (i > 0) {
                                indent++;
                                res += `\n${" ".repeat(indent)}`;
                            }
                            res += `(spread `;
                            indent++;
                            res += cmd + `\n${" ".repeat(indent)}(` + proofTreeToProofLite(node.rules, node.branch, indent);
                            res += `))`; // we need to close the extra parenthesis opened by spread
                        }
                        else {
                            res += cmd + proofTreeToProofLite(node.rules, node.branch, indent);
                        }
                        break;
                    }
                    case "proof-branch":
                        if (i > 0) {
                            res += `\n${" ".repeat(indent + 1)}`;
                        }
                    default: {
                        if (node.rules && node.rules.length) {
                            res += proofTreeToProofLite(node.rules, node.branch, indent);
                        }
                        else {
                            res += "(postpone)";
                        }
                        break;
                    }
                }
            }
            if (closeRight) {
                res += ")";
            }
        }
        return res;
    };
    if (proofDescriptor) {
        const nodes = proofDescriptor.proofTree ? [proofDescriptor.proofTree] : null;
        let script = proofTreeToProofLite(nodes);
        script = script || "(then (postpone))";
        if (opt.omitTags) {
            if (opt.escape) {
                script = script.replace(/[\n\r]/g, " ").replace(/\"/g, `\\\"`);
            }
            return script.split("\n");
        }
        else {
            // the theory name will be given by the file name
            const res = `${proofDescriptor.info.formula} : PROOF\n`
                + script
                + `\nQED ${proofDescriptor.info.formula}`;
            return (opt.barDash) ? res.split("\n").map(line => { return "%|- " + line; })
                : res.split("\n");
        }
    }
    return null;
}
exports.proofDescriptor2ProofLite = proofDescriptor2ProofLite;
function prf2ProofTree(desc) {
    const expandBranchNames = (node, branchId) => {
        if (node) {
            const goalId = +node.name;
            branchId = branchId || "";
            branchId = makeBranchId({ branchId, goalId });
            node.branch = branchId;
            for (let i = 0; i < node.rules.length; i++) {
                expandBranchNames(node.rules[i], branchId);
            }
        }
    };
    const getProofCommands = (prf) => {
        let par = 0;
        let match = null;
        const regexp = new RegExp(/([\(\)])/g);
        while (match = regexp.exec(prf)) {
            switch (match[1]) {
                case "(": {
                    par++;
                    break;
                }
                case ")": {
                    par--;
                    break;
                }
                default: { }
            }
            if (par === 0) {
                return prf.substr(0, match.index + 1);
            }
        }
        return "";
    };
    const buildProofTree_aux = (desc) => {
        var _a;
        if (desc && desc.parent) {
            while (desc.prf && desc.prf.length) {
                // check if there's a comment in the prf file -- remove the comment before processing the command
                if ((_a = desc.prf) === null || _a === void 0 ? void 0 : _a.trim().startsWith(`";;; `)) {
                    desc.prf = desc.prf.replace(/\";;;\s[^"]*\"\s*\(/g, "(");
                }
                // series of proof branches or a proof command
                const expr = getProofCommands(desc.prf);
                if (expr && expr.length) {
                    if (expr.startsWith("((")) {
                        // series of proof branches
                        // remove a matching pair of parentheses and iterate
                        const match = /\(([\w\W\s]+)\s*\)/.exec(desc.prf);
                        const subexpr = match[1];
                        const currentParent = desc.parent.rules[desc.parent.rules.length - 1];
                        buildProofTree_aux({ prf: subexpr, proofName: desc.proofName, parent: currentParent });
                    }
                    else if (expr.startsWith(`("`)) {
                        // proof command from a labelled branch -- remove the label and iterate
                        const match = /\(\"(\d+)\"\s*([\w\W\s]+)/.exec(expr);
                        const subexpr = match[2].replace(/\s*\n\s*/g, " "); // remove all \n introduced by pvs in the expression
                        const currentBranch = {
                            name: match[1],
                            rules: [],
                            type: "proof-branch",
                            branch: match[1]
                        };
                        desc.parent.rules.push(currentBranch);
                        buildProofTree_aux({ prf: subexpr, proofName: desc.proofName, parent: currentBranch });
                    }
                    else {
                        // proof command
                        desc.parent.rules.push({
                            name: expr,
                            rules: [],
                            type: "proof-command",
                            branch: expr
                        });
                    }
                    // update prf
                    desc.prf = desc.prf.substr(expr.length).trim();
                }
                else {
                    // ) parentheses comes before (, from parsing a series labelled branches, just ignore them and iterate
                    const match = /\)+([\w\W\s]*)/.exec(desc.prf);
                    // update prf
                    desc.prf = match[1].trim(); // remove all \n introduced by pvs in the expression
                    if (desc.prf && desc.prf.length) {
                        buildProofTree_aux(desc);
                    }
                }
            }
        }
        else {
            console.error("[pvs-proxy] Warning: unable to build proof tree (parent node is null)");
        }
    };
    if (desc && desc.prf) {
        // root node
        const rootNode = {
            name: desc.proofName,
            rules: [],
            type: "root",
            branch: "root"
        };
        let prf = desc.prf.trim();
        if (desc.prf.startsWith(`(""`)) {
            const match = /\(\"\"([\w\W\s]+)\s*\)/.exec(desc.prf);
            prf = match[1].trim();
        }
        if (prf) {
            // if (prf.startsWith("(then ")) {
            // 	// prf contains prooflite glassbox
            // 	proofLite2proofTree({ prf, proofName: desc.proofName, parent: rootNode });	
            // } else {
            // regular prf file
            buildProofTree_aux({ prf, proofName: desc.proofName, parent: rootNode });
            expandBranchNames(rootNode);
            // }
            return rootNode;
        }
        else {
            console.error("[pvs-proxy] Warning: unrecognised proof structure", desc.prf);
        }
    }
    return null;
}
exports.prf2ProofTree = prf2ProofTree;
/**
 * Utility function, checks if the provided proof script is null or empty
 * @param prf proof script
 */
function isEmptyPrf(prf) {
    return !prf || prf === `("" (postpone))`;
}
exports.isEmptyPrf = isEmptyPrf;
function isEmptyProof(pdesc) {
    return !pdesc || !pdesc.proofTree || !pdesc.proofTree.rules || pdesc.proofTree.rules.length === 0
        || (pdesc.proofTree.rules.length === 1 && pdesc.proofTree.rules[0].name === "(postpone)");
}
exports.isEmptyProof = isEmptyProof;
const grind = `("" (grind))`;
const postpone = `("" (postpone))`;
/**
 * Utility function, transforms a proof tree into a json object
 * @param desc Descriptor specifying proofTree, formulaName, proofName, and parent node (keeps track of the current parent in the proof tree, used in recursive calls)
 */
function prf2jprf(desc) {
    if (desc) {
        const result = new serverInterface_1.ProofDescriptor({
            theory: desc.theoryName,
            formula: desc.formulaName,
            status: desc.status || "untried",
            prover: pvsVersionToString(desc.version) || "PVS 7.x",
            shasum: desc.shasum,
            date: new Date().toISOString()
        }, ".prf");
        if (desc.prf) {
            const script = desc.prf.replace(/\s*\n+\s*/g, " ").replace(/\s\s+/g, " "); // remove all \n and extra spaces introduced by pvs in the expression
            // capture group 1 is proofName
            // capture group 2 is formulaName,
            // capture group 3 is proofTree
            const data = new RegExp(exports.prfRegExp).exec(script);
            if (data && data.length > 3) {
                const proofName = data[2]; //data[1];
                const formulaName = data[2];
                // consistency check
                if (formulaName !== `${desc.theoryName}.${desc.formulaName}`) {
                    console.warn(`[language-utils] Warning: proof script for ${desc.theoryName}.${desc.formulaName} has unexpected signature ${formulaName}`);
                }
                const prf = isEmptyPrf(data[3]) ? postpone : data[3];
                const proof = prf2ProofTree({ prf, proofName });
                result.proofTree = proof;
                // console.dir(result, { depth: null });
            }
        }
        return result;
    }
    return null;
}
exports.prf2jprf = prf2jprf;
// group 1 is the command argument
exports.helpCommandRegexp = /^\s*\(?\s*help\s*\"?([^\)]+)/g;
function isHelpCommand(cmd) {
    cmd = (cmd) ? cmd.trim() : cmd;
    return cmd && new RegExp(exports.helpCommandRegexp).test(cmd);
}
exports.isHelpCommand = isHelpCommand;
function isHelpStarCommand(cmd) {
    cmd = (cmd) ? cmd.trim() : cmd;
    return cmd && (new RegExp(/\bhelp\s*\*/).test(cmd) || new RegExp(/\(\s*help\s*\*\s*\)/).test(cmd));
}
exports.isHelpStarCommand = isHelpStarCommand;
function isHelpVSCodePlot(cmd) {
    cmd = (cmd) ? cmd.trim() : cmd;
    return cmd && (new RegExp(/\bhelp\s*vscode\-plot\b/).test(cmd) || new RegExp(/\(\s*help\s*vscode\-plot\s*\)/).test(cmd));
}
exports.isHelpVSCodePlot = isHelpVSCodePlot;
// group 1 is the command argument
exports.helpBangCommandRegexp = /^\s*\(?\s*help!?\s*\"?([^\)"]+)/g;
function isHelpBangCommand(cmd) {
    cmd = (cmd) ? cmd.trim() : cmd;
    return cmd && new RegExp(exports.helpBangCommandRegexp).test(cmd);
}
exports.isHelpBangCommand = isHelpBangCommand;
// export function isSaveCommand (cmd: string): boolean {
// 	cmd = (cmd) ? cmd.trim() : cmd;
// 	return cmd && (cmd === "save" 
// 		|| cmd === "save;"
// 		|| cmd === "(save)"
// 		|| /\(\s*save\s*\)/g.test(cmd))
// 		;
// }
function isMetaProofCommand(cmd) {
    return isPostponeCommand(cmd) || isUndoStarCommand(cmd) || isShowHiddenFormulas(cmd);
}
exports.isMetaProofCommand = isMetaProofCommand;
function proofTree2commandSequence(node) {
    if (node) {
        let ans = (node.type === "proof-command") ? node.name : "";
        if (node.rules) {
            for (let i = 0; i < node.rules.length; i++) {
                ans += proofTree2commandSequence(node.rules[i]);
            }
        }
        return ans;
    }
    return null;
}
exports.proofTree2commandSequence = proofTree2commandSequence;
function applyTimeout(cmd, sec) {
    if (cmd && sec) {
        const c = cmd.startsWith('(') && cmd.endsWith(')') ? cmd : `(${cmd})`;
        return `(apply ${c} :timeout ${sec})`;
    }
    return cmd;
}
exports.applyTimeout = applyTimeout;
function noChange(result) {
    if (result && result.commentary) {
        if (typeof result.commentary === "string") {
            return result.commentary.startsWith("No change on:");
        }
        else if (typeof result.commentary === "object") {
            return result.commentary.length
                && result.commentary.filter((comment) => {
                    return comment.startsWith("No change on:");
                }).length > 0;
        }
    }
    return false;
}
exports.noChange = noChange;
function getNoChangeCommand(result) {
    if (result && result.commentary) {
        if (typeof result.commentary === "string") {
            return result.commentary.replace("No change on:", "").trim();
        }
        else if (typeof result.commentary === "object" && result.commentary.length) {
            const comments = result.commentary.filter((comment) => {
                return comment.startsWith("No change on:");
            });
            if (comments && comments.length && typeof comments[0] === "string") {
                return comments[0].replace("No change on:", "").trim();
            }
        }
    }
    return null;
}
exports.getNoChangeCommand = getNoChangeCommand;
function QED(result) {
    if (result && result.commentary) {
        if (typeof result.commentary === "string") {
            return result.commentary.trim().startsWith("Q.E.D.");
        }
        else if (typeof result.commentary === "object") {
            return result.commentary.length
                && result.commentary.filter((comment) => {
                    return comment.trim().startsWith("Q.E.D.");
                }).length > 0;
        }
    }
    return false;
}
exports.QED = QED;
function isGlassboxTactic(cmd) {
    return cmd && (cmd.startsWith("(then ") || cmd.startsWith("(spread "));
}
exports.isGlassboxTactic = isGlassboxTactic;
function isInterruptCommand(cmd) {
    cmd = (cmd) ? cmd.trim() : cmd;
    return cmd && (cmd === "interrupt"
        || cmd === "interrupt;"
        || cmd === "(interrupt)"
        || /^\(\s*interrupt\s*\);?/gi.test(cmd));
}
exports.isInterruptCommand = isInterruptCommand;
function interruptedByClient(result) {
    if (result && result.commentary) {
        if (typeof result.commentary === "string") {
            return result.commentary.trim().includes("interrupted by client");
        }
        else if (typeof result.commentary === "object") {
            return result.commentary.length
                && result.commentary.filter((comment) => {
                    return comment.trim().includes("interrupted by client");
                }).length > 0;
        }
    }
    return false;
}
exports.interruptedByClient = interruptedByClient;
function branchComplete(result, formulaName, previousBranch) {
    if (result && result.commentary) {
        if (typeof result.commentary === "string") {
            if (typeof previousBranch === "string") {
                return result.commentary.startsWith("This completes")
                    && (result.commentary.endsWith(`${formulaName}.${previousBranch}.`)
                        || result.commentary.endsWith(`${formulaName}.${previousBranch}`.trim()));
            }
            return result.commentary.startsWith("This completes");
        }
        else if (typeof result.commentary === "object") {
            return result.commentary.length
                && result.commentary.filter((comment) => {
                    if (typeof previousBranch === "string") {
                        return comment.startsWith("This completes")
                            && (comment.endsWith(`${formulaName}.${previousBranch}.`)
                                || comment.endsWith(`${formulaName}.${previousBranch}`.trim()));
                    }
                    return comment.startsWith("This completes");
                }).length > 0;
        }
    }
    return false;
}
exports.branchComplete = branchComplete;
function siblingBranchComplete(result, newBranch) {
    return result && result.commentary
        && result.commentary.length
        && result.commentary.filter((comment) => {
            if (typeof newBranch === "string" && comment.startsWith("This completes")) {
                const parentOfNewBranch = newBranch.substring(0, newBranch.lastIndexOf("."));
                const closedBranch = comment.substring(comment.indexOf(".") + 1, comment.lastIndexOf("."));
                const parentOfClosedBranch = closedBranch.substring(0, closedBranch.lastIndexOf("."));
                return parentOfClosedBranch === parentOfNewBranch;
            }
            return comment.startsWith("This completes");
        }).length > 0;
}
exports.siblingBranchComplete = siblingBranchComplete;
function branchHasChanged(desc) {
    if (desc) {
        const newBranch = desc.newBranch.replace(/T/g, "");
        const previousBranch = desc.previousBranch.replace(/T/g, "");
        return (newBranch !== previousBranch || previousBranch !== "" && !newBranch.startsWith(previousBranch));
    }
    return false;
}
exports.branchHasChanged = branchHasChanged;
function pathHasChanged(desc) {
    if (desc) {
        const newBranch = desc.newBranch.replace(/T/g, "");
        const previousBranch = desc.previousBranch.replace(/T/g, "");
        return !newBranch.startsWith(previousBranch);
    }
    return false;
}
exports.pathHasChanged = pathHasChanged;
// these icons are shown correctly only on recent os distributions that include the proper font set.
// use https://iconify.design/icon-sets/ for proof explorer, to have a consistent look&feel on all systems.
exports.icons = {
    checkmark: "✅",
    bang: "❗",
    snowflake: "❄️",
    sparkles: "✨",
    whitecircle: "⚪"
};
/**
 * Utility function, returns the icon associated to the given proof status
 */
function getIcon(proofStatus) {
    switch (proofStatus) {
        case "subsumed":
        case "simplified":
        // case "proved - incomplete":
        // case "proved - by mapping":
        // case "proved - complete": 
        case "proved":
            return exports.icons.checkmark;
        case "unfinished": // proof attempted but failed
            return exports.icons.bang;
        case "unchecked": // proof was successful, but needs to be checked again because of changes in the theories
            return exports.icons.snowflake;
        case "unproved":
        case "untried": // proof has not been attempted yet
            return exports.icons.sparkles;
    }
}
exports.getIcon = getIcon;
/**
 * Utility function, checkes if the proof status is 'proved'
 */
function isProved(proofStatus) {
    switch (proofStatus) {
        case "subsumed":
        case "simplified":
        // case "proved - incomplete":
        // case "proved - by mapping":
        // case "proved - complete": 
        case "proved":
            return true;
        default:
            return false;
    }
}
exports.isProved = isProved;
/**
 * Utility function, checkes if the proof status is 'unchecked'
 */
function isUnchecked(proofStatus) {
    return proofStatus === "unchecked";
}
exports.isUnchecked = isUnchecked;
/**
 * Utility function, creates an empty pvs theory
 */
function makeEmptyTheory(theoryName, opt) {
    const header = makeTheoryHeader(theoryName, opt);
    return `${header}${theoryName}: THEORY
  BEGIN 
    
  END ${theoryName}
`;
}
exports.makeEmptyTheory = makeEmptyTheory;
/**
 * Utility function, creates the documentation header for a theory
 */
function makeTheoryHeader(theoryName, opt) {
    var _a;
    opt = opt || {};
    const author = (opt === null || opt === void 0 ? void 0 : opt.authorKey) || ((_a = process === null || process === void 0 ? void 0 : process.env) === null || _a === void 0 ? void 0 : _a.USER) || "...";
    return `%%
% @theory: ${theoryName}
% @author: ${author}
% @date: ${new Date().toUTCString()}
%%
`;
}
exports.makeTheoryHeader = makeTheoryHeader;
/**
 * Utility function, checks if the theory includes the header documentation
 */
function includesTheoryTag(theoryName, fileContent) {
    if (theoryName && fileContent) {
        return fileContent.replace(/\s+/g, "").includes(`%@theory:${theoryName}`);
    }
    return false;
}
exports.includesTheoryTag = includesTheoryTag;
/**
 * tags that can be used in a pvs theory
 */
var PvsTag;
(function (PvsTag) {
    PvsTag["author"] = "@author";
    PvsTag["authors"] = "@authors";
    PvsTag["contributors"] = "@contributors";
    PvsTag["theory"] = "@theory";
    PvsTag["theorem"] = "@theorem";
    PvsTag["majortheorem"] = "@majortheorem";
    PvsTag["lemma"] = "@lemma";
    PvsTag["function"] = "@function";
    PvsTag["returns"] = "@returns";
    PvsTag["datatype"] = "@datatype";
    PvsTag["param"] = "@param";
    PvsTag["const"] = "@const";
    PvsTag["description"] = "@description";
    PvsTag["desc"] = "@desc";
    PvsTag["date"] = "@date";
    PvsTag["QED"] = "@QED";
    PvsTag["yields"] = "@yields";
    PvsTag["stats"] = "@stats"; // stats for a proof, e.g., run time
})(PvsTag = exports.PvsTag || (exports.PvsTag = {}));
;
/**
 * Utility function, returns the value of a given tag in the theory
 */
function getPvsTag(tag, fileContent) {
    if (tag && fileContent) {
        const regex = new RegExp(`%\\s*${tag}\[\\s+\|:\]\\s*(.*)`, "gi");
        const match = regex.exec(fileContent);
        const val = (match === null || match === void 0 ? void 0 : match.length) > 1 ? match[1].trim() : "";
        // console.log(`[language-utils] getPvsTag`, { tag, val, regex: regex.source });
        return val;
    }
    return null;
}
exports.getPvsTag = getPvsTag;
;
function formatHelp(desc, opt) {
    opt = opt || {};
    if (desc) {
        let msg = "  ";
        msg += (opt.useColors) ? `${colorUtils.colorText(desc.description, colorUtils.PvsColor.green)}` : desc.description;
        if (desc.syntax) {
            msg += "\n  Syntax: ";
            msg += (opt.useColors) ? `${colorUtils.colorText(desc.syntax, colorUtils.PvsColor.yellow)}` : desc.syntax;
        }
        if (desc.optionals) {
            const opts = Object.keys(desc.optionals);
            if (opts && opts.length) {
                msg += "\n  Optionals: ";
                for (let i = 0; i < opts.length; i++) {
                    msg += "\n    ";
                    msg += (opt.useColors) ? `${colorUtils.colorText(opts[i], colorUtils.PvsColor.yellow)}` : opts[i];
                    msg += " " + desc.optionals[opts[i]];
                }
            }
        }
        return msg;
    }
    return "";
}
exports.formatHelp = formatHelp;
/**
 * Prints a compact help for a given command
 */
function printHelp(helpCommand, opt) {
    const match = new RegExp(exports.helpCommandRegexp).exec(helpCommand);
    if (match && match.length > 1) {
        const availableHelp = exports.PROOF_COMMANDS[match[1]]
            || exports.PROOF_TACTICS[match[1]]
            || exports.EVALUATOR_COMMANDS[match[1]];
        return (availableHelp) ? formatHelp(availableHelp, opt)
            : `Help not available for ${match[1]}`;
    }
    return "";
}
exports.printHelp = printHelp;
// the following list of commands obtained from pvs-server with collect-strategy-names
// the descriptions are based on those illustrated in the pvs prover guide
// some commands are commented out in this list are they were not deemed useful for the typical PVS user
exports.PROOF_COMMANDS = {
    // 	"abs-simp": {
    // 		description: ``
    // },
    // "abstract": { description:""},
    // "abstract-and-mc": { description:""},
    // 	"add-formulas": { 
    // 		description: `
    // add-formulas: adds relational formulas.
    // The new formula is labeled as LABEL, if specified. 
    // If FNUM2 is nil, adds FNUM to itself.
    // If HIDE? is t, the original formulas are hidden.
    // TCCs generated during the execution of the command are discharged with the proof command tcc-step. 
    // At the end, the strategy tries to discharge the current branch using the proof command auto-step.
    // Syntax: add-formulas FNUM1 FNUM2?
    // `
    // },
    // "all-implicit-typepreds": { description:""},
    "all-typepreds": {
        description: `Provide type predicate information that are not already dealt with by the prover`,
        syntax: `all-typepreds`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "apply-eta": {
        description: `Apply the eta axiom scheme of extensionality`,
        syntax: `apply-eta TERM`,
    },
    "apply-ext": {
        description: `Try to prove equality via extensionality`,
        syntax: `apply-ext`,
        optionals: {
            "FNUMS": `Apply the command to the given sequent formula numbers. If FNUM is not given, then the first consequent that is an equation is used.`,
            "(keep? t)": `Keep the equality as an antecedent, rather than hiding it.`
        }
    },
    // "apply-extensionality": {
    //     description: `Try to prove equality via extensionality ** SUPERSEEDED BY apply-ext **`,
    //     syntax: `apply-extensionality`,
    //     optionals: {
    //         "FNUMS": `Apply the command to the given sequent formula numbers, e.g., (apply-extensionality "-1 2") applies the commmand to sequents -1 and 2. If FNUM is not given, then the first consequent that is an equation is used.`,
    //         "(keep? t)": `Keep the equality as an antecedent, rather than deleting it.`,
    //         "(hide t)": `Hide the equality formula to which extensionality is applied, rather than deleting the formula.` 
    //     }
    // },
    "apply-lemma": {
        description: `Apply lemma with expressions`,
        syntax: `apply-lemma LEMMA EXPR-SPECS`,
        optionals: {
            "EXPR-SPECS": ``
        }
    },
    // "apply-rewrite": {
    //     description:`Apply a purely equational rewrite rule`,
    //     syntax: `apply-rewrite`,
    //     optionals: {
    //         "EXPR-SPECS": ``
    //     }
    // },
    "assert": {
        description: `Simplify expressions using decision procedures`,
        syntax: `assert`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`,
            ":rewrite-flag rl": `Apply the command only to the right-hand side of the equality.`,
            ":rewrite-flag lr": `Apply the command only to the left-hand side of the equality.`
        }
    },
    // "auto-rewrite": { description:""},
    // "auto-rewrite!": { description:""},
    // "auto-rewrite!!": { description:""},
    // "auto-rewrite-defs": { description:""},
    // "auto-rewrite-explicit": { description:""},
    // "auto-rewrite-expr": { description:""},
    // "auto-rewrite-theories": { description:""},
    // "auto-rewrite-theory": { description:""},
    // "auto-rewrite-theory-with-importings": { description:""},
    // "bash": { description: `Executes assert, bddsimp, inst?, skolem-typepred, flatten, and lift-if.`},
    "bddsimp": {
        description: `Propositional simplification using binary decision diagrams`,
        syntax: `bddsimp FNUMS`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`,
        }
    },
    "beta": {
        description: `Rewrites redex expressions to their reduced form`,
        note: `LET and WHERE expressions are syntactic sugar for redexes`,
        syntax: `beta`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "both-sides": {
        description: `Apply an operand uniformly over a conjunction of inequalities.
    For example, given a sequent formula 'e1 ≤ e2 AND e2 ≤ e3 AND e3 ≤ e4', 
    both-sides replaces the chain with 'e1 OP TERM ≤ e2 OP TERM AND e2 OP TERM ≤ e3 OP TERM AND e3 OP TERM ≤ e4 OP TERM'.`,
        syntax: `both-sides OP TERM`
    },
    "both-sides-f": {
        description: `Apply a function to both sides of a relational expression,
    For example, given a sequent formula FNUM in the form 'e1 = e2',
    both-sides-f replaces the formula with 'F(e1) = F(e2)'.`,
        syntax: `both-sides FNUM F`,
        optionals: {
            ':postfix? t': `Add the function as a postfix string.`
        }
    },
    // "cancel": { description: `Cancel terms from both sides of relational formulas involving arithmetic expressions.`},
    // "cancel-add": { description:""},
    // "cancel-add!": { description:""},
    // "cancel-by": { description:""},
    // "cancel-formula": { description:""},
    // "cancel-terms": { description:""},
    // "canon-tms": { description:""},
    "case": {
        description: `Case analysis based on given formulas`,
        syntax: `case FORMULAS`,
        note: `Sequents are split according to the truth or falsity of FORMULAS.
        For example, given a sequent 'A ⊢ B', the command 'case "a" "b" "c"' generates four subgoals:\n
        a, b, c, A ⊢ B\n
        a, b, A ⊢ c, B\n
        a, A ⊢ b, B\n
        A ⊢ a, B.`
    },
    "case*": {
        description: `Full case analysis based on given FORMULAS`,
        syntax: `case* FORMULAS`,
        note: `Splits along every branch, according to the truth or falsity of FORMULAS.`
    },
    // "case-if": { description:""},
    // "case-if*": { description:""},
    // "case-old-lift-if": { description:""},
    // "case-replace": { description:""},
    // "checkpoint": { description:""},
    // "claim": { description:""},
    // "commentf": { description:""},
    // "contra-eqs": { description: `simple equality reasoning`},
    "copy": {
        description: `Copy a sequent formula`,
        syntax: `copy FNUM`,
        note: `The command inserts a copy of sequent formula FNUM. 
        If the formula is an antecedent, then the copy becomes the first antecedent.
        If the formula is a succedent, then the copy becomes the first succedent.`
    },
    "copy*": {
        description: `Copy a series of formulas`,
        syntax: `copy* FNUMS`,
        note: `This command is an iterative version of 'copy'.`,
    },
    "cross-add": {
        description: `Apply cross addition to relational sequent formulas`,
        syntax: `cross-add`,
        note: `Add to both sides of a formula the respective subtrahend of each side 
            and then simplify. The operation is applied recursively until all outermost 
            subtraction operators are gone.`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "cross-mult": {
        description: `Apply cross multiplication to relational expressions`,
        syntax: `cross-mul`,
        note: `Multiply both sides of a formula by the respective divisors of each side 
            and then simplify. Checks for negative real divisors and invokes suitable
            lemmas as needed.  Applies cross multiplication recursively until all
            outermost division operators are gone.`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "cut": {
        description: `Case analysis on a series of formulas. Equivalent to the 'case' command.`,
        syntax: `cut FORMULAS+`
    },
    "decide": {
        description: `Invoke the decision procedure, without simplification.`,
        syntax: `decide`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "decompose-equality": {
        description: `Decompose an expression into a series of equalities`,
        syntax: `decompose-equality`,
        note: `This command only works for equalities between functions, records, or tuples.`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "delabel": {
        description: `Delete a labelled sequent formula`,
        syntax: `delabel LABEL`,
        optionals: {
            "(hide? t)": `Sequent formula LABEL becomes a hidden sequent formula.`,
            "(hidden? t)": `Sequent formula LABEL is also removed from hidden formulas.`
        }
    },
    "delete": {
        description: `Delete sequent formulas`,
        syntax: `delete FNUMS`,
        note: `This command is useful to remove sequent formulas that may have become irrelevant in the current goal.`
    },
    "demod-lin": {
        description: `Partial linear demodulator derivation and application`,
        syntax: `demod-lin`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "demod-num": {
        description: `Numerical demodulation`,
        syntax: `demod-num`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "detuple-boundvars": {
        description: `Distribute tuple and record quantication`,
        syntax: `detuple-boundvars`,
        note: `A top-level formula of the form 'FORALL (x: [S1, S2, S3]): g(x)' 
            is replaced by 'FORALL (x1: S1), (x2: S2), (x3: S3): g(x1, x2, x3)'. 
            Similarly, a top-level formula 'FORALL (x: [# s : S, t : T #]) : g(x)' 
            is replaced by 'FORALL (x1: S), (x2: T)): g((# s := x1, t := x2 #))'.
            This decomposition of tuple and record quantication is usually needed
            to carry out an induction over one of the components. Tuple quantication 
            can be introduced when instantiating parameterized theories.`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "distrib": {
        description: `Distribute multiplication over additive terms`,
        syntax: `distrib FNUMS`,
        optionals: {
            ":side l": `Apply the command to the left-hand side of the formula.`,
            ":side r": `Apply the command to the right-hand side of the formula.`,
            "TERM-NUMS": `Indexes specifying which terms the command should be applied to.`
        }
    },
    "distrib!": {
        description: `Distribute multiplication over additive terms`,
        syntax: `distrib! EXPR-LOC`
    },
    "div-by": {
        description: `Divide both sides of a sequent formula by a factor EXPR`,
        syntax: `div-by FNUMS EXPR`,
        optionals: {
            ":sign +": `EXPR is known to be positive.`,
            ":sign -": `EXPR is known to be negative.`,
            ":sign *": `Introduce conditional expressions to handle the cases EXPR > 0 and EXPR < 0.`
        }
    },
    "do-rewrite": {
        description: `Use decision procedures to rewrite sequent formulas`,
        syntax: `do-rewrite`,
        optionals: {
            "FNUMS": `Apply the command to the given sequent formula numbers, e.g., (do-rewrite "-1 2") applies the command to sequents -1 and 2.`,
            ":rewrite-flag rl": `Apply the command only to the right-hand side of the formula.`,
            ":rewrite-flag lr": `Apply the command only to the left-hand side of the formula.`,
            ":linear? t": `Multiplication and division are uninterpreted.`
        }
    },
    "elim-unary": {
        description: `Eliminate unary minus functions in additive expressions`,
        syntax: `elim-unary FNUM`,
        note: `This command converts sequent formulas of the form x +/- -y to the form x -/+ y. Also converts -x + y to y - x.`
    },
    "elim-unary!": {
        description: `Eliminate unary minus functions in additive expressions.`,
        syntax: `elim-unary EXPR-LOC`
    },
    "eta": {
        description: `Introduces Eta version of extensionality axiom for a given TYPE`,
        syntax: `eta TYPE`
    },
    "eval": {
        description: `Print the evaluation of a given expression`,
        syntax: `eval EXPR`,
        note: `This command may use semantic attachments for the evaluation of uninterpreted terms.`,
    },
    "eval-expr": {
        description: "Adds a new antecedent formula given by the evaluation of the given expression",
        syntax: `eval-expr EXPR`,
        note: `This command may use semantic attachments for the evaluation of uninterpreted terms.`,
    },
    "eval-formula": {
        description: "",
        syntax: `eval-formula FNUM`,
        note: `This command may use semantic attachments for the evaluation of uninterpreted terms.`,
    },
    "expand": {
        description: "Expand a name and simplify",
        syntax: `expand NAME`
    },
    "expand*": {
        description: "Expand a series of names and simplify",
        syntax: `expand* NAMES`
    },
    "expand-names": {
        description: "Apply 'expand*' to sequents FNUMS",
        syntax: "expand-names FNUMS NAMES+"
    },
    "extensionality": {
        description: `Apply extensionality axiom scheme for functions types, records types, tuple types and abstract datatypes`,
        syntax: `extensionality TYPE`,
        note: `The extensionality rule is similar to the 'lemma' rule in that 
            it introduces an extensionality axiom for the given type as an 
            antecedent formula. An extensionality axiom can be generated 
            corresponding to function, record, and tuple types, and constructor 
            subtypes of abstract datatypes.`
    },
    "factor": {
        description: `Extract common multiplicative factors from additive terms, and then re-arrange the expression`,
        syntax: `factor FNUMS`,
        optionals: {
            ":side l": `Apply the command to the left-hand side of the formula.`,
            ":side r": `Apply the command to the right-hand side of the formula.`,
            "TERM-NUMS": `Indexes specifying which terms the command should be applied to.`
        }
    },
    "factor!": {
        description: `Extract common multiplicative factors from the additive terms, and then re-arrange the expression`,
        syntax: `factor! EXPR-LOC`
    },
    "fert-tsos": {
        description: `Inequality fertilization for trivial sums of squares`,
        syntax: `fert-tsos`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "field": {
        description: `Remove divisions and apply simplification heuristics`,
        syntax: `field`,
        optionals: {
            "FNUM": `Apply the command to given sequent formula number.`,
            ":cancel? t": `Try to cancel common terms once the expression is free of divisions.`
        }
    },
    // "field-about": { description: `Prints Field's about information` },
    "flatten": {
        description: `Apply disjunctive simplification`,
        syntax: `flatten`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "flatten-disjunct": {
        description: "Apply controlled disjunctive simplification",
        syntax: `flatten-disjunct`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "flip-ineq": {
        description: `Negate the inequality formulas and move the resulting formulas by exchanging between antecedents and succedents`,
        syntax: `flip-ineq FNUMS`
    },
    "gen-ex-cad": {
        description: `Generic cylindrical algebraic decomposition via QEPCAD-B.`,
        syntax: `gen-ex-cad`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "generalize": {
        description: `Generalize terms`,
        syntax: `generalize TERM VAR`,
        note: `If the sequent is of the form a1(t), a2(t) ├─ c1(t), c2(t), 
            then applying 'generalize "t" "x"' yields a sequent of the form 
            FORALL x: (a1(x) AND a2(x)) IMPLIES (c1(x) OR c2(x)).`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "generalize-skolem-constants": {
        description: `Generalize skolem constants`,
        syntax: `generalize-skolem-constants`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "grind": {
        description: `Install rewrites and repeatedly simplify`,
        syntax: `grind`,
        optionals: {
            ":if-match nil": "This option prevents the automatic instantiation of terms."
        }
    },
    "grind-reals": {
        description: `Apply grind with real_props`,
        syntax: `grind-reals`
    },
    // "grind-with-ext": {
    //     description: `Like 'grind', but calls 'reduce-ext', which also uses 'apply-extensionality'`
    // },
    // "grind-with-lemmas": {
    //     description: `Does a combination of (lemma) and (grind); if lazy-match? is t, postpones instantiations to follow a first round of simplification.`,
    // },
    "ground": {
        description: `Apply propositional simplification`,
        syntax: `ground`
    },
    "ground-eval": {
        description: `Prints the evaluation of a given ground expression`,
        syntax: `ground-eval EXPR`
    },
    "group": {
        description: `Associatively grouping three terms`,
        syntax: `group TERM1 OPERATOR TERM2 TERM3`
    },
    "group!": {
        description: `Associatively grouping three terms of the function application found at EXPR-LOC`,
        syntax: `group! EXPR-LOC`
    },
    "has-sign": {
        description: `Try claiming that a given expression is either > 0 or < 0`,
        syntax: `has-sign EXPR`,
        optionals: {
            ":sign +": `Claim that a given expression is positive.`,
            ":sign -": `Claim that a given expression is negative.`
        }
    },
    "help": {
        description: `Displays information about commands, including description and syntax`,
        syntax: `help CMD`
    },
    "hide": {
        description: `Hide sequent formulas`,
        syntax: `hide FNUMS`,
        note: `Hidden sequents can be restored using the 'reveal' command. Use (show-hidden-formulas) to see the list of hidden sequents.`
    },
    // "hide-all-but": {
    // 	description: `Hide Selected Formulas: this is a variant of the hide rule that hides all the formulas indicated by
    // 	FNUMS except those indicated by keep-FNUMS. As with hide, hidden sequent
    // 	formulas are saved and can be restored to a descendant of the current sequent
    // 	by the reveal rule.`
    // },
    "iff": {
        description: `Convert boolean equality to equivalence`,
        syntax: `iff`,
        note: `Yields a subgoal where boolean equalities of the form A = B are converted to A ⇔ B.`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "induct": {
        description: `Perform induction on VAR`,
        syntax: `induct VAR`
    },
    "induct-and-rewrite": {
        description: `Perform induction on VAR and then simplify using rewrites`,
        syntax: `induct-and-rewrite VAR`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`,
            "REWRITES": `Simplify using the given rewrite rules.`
        },
        note: `An example invocation is (induct-and-rewrite "x" 1 "append" "reverse"). This invocation 
            inducts on "x" in formula 1, then simplifies the base and the induction step using definitions 
            "append" and "reverse".`
    },
    "induct-and-rewrite!": {
        description: `Perform induction on VAR and then simplify using rewrites while expanding all definitions`,
        syntax: `induct-and-rewrite! VAR`
    },
    "induct-and-simplify": {
        description: `Perform induction on VAR and then simplify using rewrites rules defined in the given theories`,
        syntax: `induct-and-simplify VAR :theories THEORIES`,
        optionals: {
            ":rewrites REWRITES": `Simplify using the given rewrite rules.`
        }
    },
    "inst": {
        description: `Instante existential quantifiers using the given terms`,
        syntax: `inst FNUM TERMS`
    },
    // "inst!": { 
    //     description: ``
    // },
    // "inst*": {
    //     description: ``
    // },
    // "inst-cp": { 
    //     description: `Copy and instantiate existentially quantified formula`
    // },
    "inst?": {
        description: `Use heuristic instantiation to remove existential quantifiers`,
        syntax: `inst?`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`,
            ":subst VAR EXPR": `List of substitutions for variable names.`
        }
    },
    // "install-rewrites": { 
    //     description: ``,
    //     syntax: ``,
    //     effect: ``,
    //     examples: {}
    // },
    // "instantiate": {
    //     description: `Same as 'inst`
    // },
    // "instantiate-one": { 
    //     description: `Same as 'inst', but has not effect if the instantiation would introduce a duplicate formula`
    // },
    "insteep": {
        description: `Instantiate existentially quantified formula with constants that have the same name of the quantified variables`,
        syntax: `insteep FNUM`
    },
    "insteep*": {
        description: `Iterates N times 'insteep'`,
        syntax: `insteep* FNUM`
    },
    "int-dom-zpb": {
        description: `Integral domain zero product branching for explicit zero indeterminate products`,
        syntax: `int-dom-zpb`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`,
        }
    },
    "isolate": {
        description: `Move all additive terms except that numbered TERM-NUM in relational formula FNUM from SIDE (l or r) to the other side`,
        syntax: `isolate FNUM SIDE TERM-NUM`
    },
    "isolate-mult": {
        description: `Select the first factor from the left side of formula FNUM and divide both sides as needed to leave the selected term isolated.`,
        syntax: `isolate-mult FNUM`,
        note: "A case split to generate the appropriate condition on the divisor is automatically introduced.",
        optionals: {
            ":side r": `Apply the command to the right-hand side of the formula.`,
            ":sign +": `Claim that the product of the selected factors is positive.`,
            ":sign -": `Claim that the product of the selected factors is negative.`,
            ":term-num TERM-NUM": `Apply the formula to the term whose index is TERM-NUM.`
        }
    },
    "isolate-replace": {
        description: `Isolate term TERM-NUM on SIDE (l or r) of relational formula FNUM, then replace and hide it.`,
        syntax: `isolate-replace FNUM SIDE TERM-NUM`
    },
    "label": {
        description: `Define a label to a sequent formula`,
        syntax: `label LABEL FNUM`
    },
    "lazy-grind": {
        description: `Install rewrites and repeatedly simplify`,
        syntax: `lazy-grind`,
        note: `Equivalent to (grind) with the instantiations postponed until after simplification.`
    },
    "lemma": {
        description: `Import a lemma and add it as first antecedent`,
        syntax: `lemma LEMMA`
    },
    "lift-if": {
        description: `Lift embedded IF connectives to the top level`,
        note: `This command lifts the leftmost-innermost contiguous IF or CASES branching structure out to the top level.`,
        syntax: `lift-if`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`,
        }
    },
    "measure-induct+": {
        description: `Suitably instantiates and uses the measure induction scheme`,
        syntax: `measure-induct+ MEASURE VARS`,
    },
    "measure-induct-and-simplify": {
        description: `Invokes MEASURE-INDUCT+ then repeatedly expands definitions`,
        syntax: `measure-induct-and-simplify MEASURE VARS`
    },
    "merge-fnums": {
        description: "Merges indicated FNUMS into a single formula",
        syntax: "merge-fnums FNUMS"
    },
    // "model-check": { 
    //     description: `THIS COMMAND IS BROKEN DON'T USE IT`,
    // },
    "move-terms": {
        description: `Move additive terms from one side (l or r) of a relational formula to the other side, adding or substracting as needed.`,
        syntax: `move-term FNUM SIDE`,
        optionals: {
            "TERM-NUMS": `Indexes specifying which terms the command should be applied to.`
        }
    },
    "move-to-front": {
        description: `Moves sequent formulas FNUMS to the front of the antecedent or consequent lists as appropriate.`,
        syntax: `move-to-front FNUMS`,
    },
    "mult-by": {
        description: `Multiply both sides of a sequent formula by a factor EXPR`,
        syntax: `mult-by FNUMS EXPR`,
        optionals: {
            ":sign +": `EXPR is known to be positive.`,
            ":sign -": `EXPR is known to be negative.`,
            ":sign *": `Introduce conditional expressions to handle the cases EXPR > 0 and EXPR < 0.`
        }
    },
    "mult-cases": {
        description: `Generate case analyses for relational formulas containing products`,
        syntax: `mult-cases FNUM`
    },
    "mult-eq": {
        description: `Given two formulas, one a relation 'a R b', and the other an antecedent equality 'x = y', introduce a new formula relating the products, 'a * x R b * y'`,
        syntax: `mult-eq REL-FNUM EQ-FNUM`
    },
    "mult-extract": {
        description: `Extract additive terms from a relational formula, and treat the extracted terms as a product of factors`,
        syntax: `mult-extract NAME FNUM`
    },
    "mult-extract!": {
        description: `Extract additive terms from a relational formula, and treat the extracted terms as a product of factors`,
        syntax: `mult-extract! NAME EXPR-LOC`
    },
    "mult-ineq": {
        description: `Given two antecedent inequalities 'a R1 b' and 'x R2 y', form an inequality on their products 'a * x R3 b * y'`,
        syntax: `mult-ineq FNUM1 FNUM2`
    },
    "musimp": {
        description: `MU calculus simplification`,
        syntax: `musimp`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`,
        }
    },
    "name": {
        description: `Introduces an antecedent EXPR = NAME.`,
        note: `This command is useful for generalizing the goal, and 
            replacing EXPR with bound variables, so that the prover 
            can be applied to them.`,
        syntax: `name NAME EXPR`
    },
    "name-case-replace": {
        description: `Replace A with B, then name B as X`,
        syntax: `name-case-replace A B X`
    },
    "name-distrib": {
        description: `Introduces new names to block the automatic application of distributive laws in sequent formulas`,
        syntax: `name-distrib`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`,
        }
    },
    "name-extract": {
        description: `Extract expressions`,
        syntax: `name-extract NAME`
    },
    "name-induct-and-rewrite": {
        description: `Perform induction on VAR and then simplify using rewrites`,
        syntax: `name-induct-and-rewrite VAR`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`,
            "INDUCTION-NAME": `Name of the induction scheme to be used.`,
            "REWRITES": `Simplify using the given rewrite rules.`
        }
    },
    "name-label": {
        description: `Adds formula EXPR = NAME, where NAME is a new name, as an antecedent and replaces EXPR by NAME in FNUMS`,
        syntax: `name-label NAME EXPR`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "name-label*": {
        description: `Iterates 'name-label' over a list NAMES-AND-EXPRS, which is assumed to be a list of the form (NAME1 EXPR1 NAME2 EXPR2 ... NAMEn EXPRn).`,
        syntax: `name-label* NAMES-AND_EXPRS`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "name-mult": {
        description: `Select a list of factors (indicated by TERM-NUMS) from the expression found on SIDE (l or r) of relational formula FNUM.
    Assign NAME to the product of the selected factors and replace the product by NAME`,
        syntax: `name-mult NAME FNUM SIDE`,
        optionals: {
            "TERM-NUMS": `Indexes specifying which terms the command should be applied to.`
        }
    },
    "name-mult!": {
        description: `Select a list of factors (indicated by TERM-NUMS) from the expression found at EXPR-LOC.
    Assign a NAME to the product of the selected factors and replace the product by NAME.
    Can only handle first expression that results from EXPR-LOC.`,
        syntax: `name-mult! NAME EXPR-LOC`,
        optionals: {
            "TERM-NUMS": `Indexes specifying which terms the command should be applied to.`
        }
    },
    "name-replace": {
        description: `Replace an expression with a given name`,
        syntax: `name-replace NAME EXPR`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "name-replace*": {
        description: `Iterates 'name-replace' over a list NAMES-AND-EXPRS, which is assumed to be a list of the form (NAME1 EXPR1 NAME2 EXPR2 ... NAMEn EXPRn).`,
        syntax: `name-replace* NAMES-AND_EXPRS`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "neg-formula": {
        description: `Negates both sides of a relational formula`,
        syntax: `neg-formula`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    "op-ident": {
        description: `Apply the identity operator to rewrite the expressions in a relational formula`,
        syntax: `op-ident FNUM`,
        note: `This command is useful for removing unnecessary terms in arithmetic expressions, e.g., 'x + 0' and and 'x * 1' will become 'x'.`,
        optionals: {
            ":side l": `Apply the command to the left-hand side of the formula.`,
            ":side r": `Apply the command to the right-hand side of the formula.`
        }
    },
    "op-ident!": {
        description: `Apply the identity operator to rewrite the expressions found at EXPR-LOC`,
        syntax: `op-ident! EXPR-LOC`
    },
    "open-ex-inf-cad": {
        description: `Cylindrical algebraic decomposition with EX-INF-MANY relaxation for open predicates via QEPCAD-B`,
        syntax: `open-ex-inf-cad`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    "open-frag-ex-inf-cad": {
        description: `Fragmented cylindrical algebraic decomposition with EX-INF-MANY relaxation for open predicates via QEPCAD-B`,
        syntax: `open-frag-ex-inf-cad`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    "permute-mult": {
        description: `Reorder multiplicative terms`,
        syntax: `permute-mult FNUMS`,
        optionals: {
            ":side l": `Apply the command to the left-hand side of the formula.`,
            ":side r": `Apply the command to the right-hand side of the formula.`,
            "TERM-NUMS": `Indexes specifying which terms the command should be applied to.`
        }
    },
    "permute-mult!": {
        description: `Reorder multiplicative terms found at EXPR-LOC`,
        syntax: `permute-mult! EXPR-LOC`
    },
    "permute-terms": {
        description: `Reorder additive terms on a side (l or r) of relational formula FNUM`,
        syntax: `permute-terms FNUM SIDE`
    },
    "permute-terms!": {
        description: `Reorder additive terms found at EXPR-LOC`,
        syntax: `permute-terms! EXPR-LOC`
    },
    "postpone": {
        description: `Postpone current goal`,
        note: `Marks the current goal as pending to be proved and shifts the focus to the next remaining goal.`,
        syntax: `postpone`
    },
    "presburger": {
        description: `Decision procedure for Presburger arithmetic by reduction to WS1S`,
        syntax: `presburger`,
        optionals: {
            "FNUMS": `Apply the command to sequent formula FNUMS.`
        }
    },
    "presburger-to-ws1s": {
        description: `Translate Presburger formulas into WS1S`,
        syntax: `presburger-to-ws1s`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "prop": {
        description: `Apply propositional simplification`,
        syntax: `prop`
    },
    // "propax": { description: `propax is automatically applied to every sequent that is ever generated in a proof, and there is never any need to actively invoke it.`},
    // "pvsio-about": { description: `` },
    // "quit": {
    //     description: `Terminates the current proof session without saving it`,
    //     syntax: `quit`
    // },
    "rahd": {
        description: `Real algebra in high dimensions`,
        syntax: `rahd`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "rahd-simp": {
        description: `Real algebra in high dimensions`,
        syntax: `rahd-simp`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "rahd-waterfall": {
        description: `Emulates the RAHD waterfall`,
        syntax: `rahd-waterfall`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "random-test": {
        description: `Runs a random test by creating random values for the skolem constants and running the ground evaluator on those values.`,
        note: `This command is useful for checking if the given sequent is worth proving.
            If it comes back with a counter example, then it may not be worth trying to prove.
            Of course, it may just be that a lemma is needed, or relevant formulas were
            hidden, and that it isn't really a counter example.`,
        syntax: `random-test`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`,
            ":count N": `Number of test runs to be executed.`,
            ":size MAX": `Generate random numbers between (-MAX ... +MAX).`
        }
    },
    "rcr-ineqs": {
        description: `Reduction of terms in inequalities to canonical rep's in residue class ring induced by equational constraints`,
        syntax: `rcr-ineqs`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "rcr-svars": {
        description: `Scalar-valued indeterminate fertilization via bounded indeterminate power sequence reduction over residue class ring induced by equational constraints`,
        syntax: `rcr-svars`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "real-props": {
        description: `Autorewrite with 'real-props'`,
        syntax: `real-props`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "recip-mult": {
        description: `Convert the top-level division operation on a side (l or r) of a relational formula to a multiplication by the reciprocal of the divisor.`,
        syntax: `recip-mult FNUMS SIDE`
    },
    "recip-mult!": {
        description: `Convert the top-level division operation found at EXPR-LOC to a multiplication by the reciprocal of the divisor.`,
        syntax: `recip-mult! EXPR-LOC`
    },
    "redlet": {
        description: `Reduces a let-in expression`,
        syntax: `redlet`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    "redlet*": {
        description: `Iteratively reduce let-in expressions`,
        syntax: `redlet*`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    // "reduce": { 
    //     description: `Core of GRIND (ASSERT, BDDSIMP, INST?, SKOLEM-TYPEPRED, FLATTEN, LIFT-IF, i.e., BASH then REPLACE*) without reestablishing all the rewrites`
    // },
    // "reduce-with-ext": { 
    //     description: `Core of GRIND-WITH-EXT (ASSERT, BDDSIMP, INST?, SKOLEM-TYPEPRED, FLATTEN, LIFT-IF, i.e., BASH then REPLACE*), like REDUCE, but includes APPLY-EXTENSIONALITY.`
    // },
    "replace": {
        description: `Apply left-to-right rewrite using formula FNUM`,
        syntax: `replace FNUM`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "replace*": {
        description: `Apply left-to-right rewrites`,
        syntax: `replace*`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "replace-eta": {
        description: `Instantiates Eta axiom scheme with a given TERM`,
        syntax: `replace-eta TERM`
    },
    "replace-ext": {
        description: `Use extensionality axiom to replace F by G`,
        syntax: `replace-ext F G`
    },
    // "replace-extensionality": { 
    //     description: `** Command superseeded by replace-ext`
    // },
    "replaces": {
        description: `Iterates the proof command replace`,
        syntax: `replaces`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "residue-class-ring-ineqs": {
        description: `Reduction of terms in inequalities to canonical rep's in residue class ring induced by equational constraints`,
        syntax: `residue-class-ring-ineqs`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "reveal": {
        description: `Reveal hidden formulas`,
        note: `The list of hidden formulas can be viewed with 'show-hidden-formulas'.`,
        syntax: `reveal FNUMS`
    },
    "rewrite": {
        description: `Use lemmas or sequent formula FNUMS to rewrite expressions`,
        syntax: `rewrite LEMMA-OR-FNUM`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "rewrite*": {
        description: `Recursively use LEMMAS-OR-FNUMS to rewrite expressions`,
        syntax: `rewrite* LEMMA-OR-FNUM`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "rewrite-expr": {
        description: `Use lemmas to rewrite the expressions located at EXPR-LOC`,
        syntax: `rewrite-expre LEMMAS EXPR-LOC`
    },
    "rewrite-lemma": {
        description: `Use lemma as a conditional rewrite rule relative to the given substitutions`,
        syntax: `rewrite-lemma LEMMA SUBST`
    },
    // "rewrite-msg-off": {
    // 	description: `In the default mode, automatic rewriting by commands such as assert and
    // 	do-rewrite generate a fairly verbose commentary. This can be entirely shut off
    // 	by the rewrite-msg-off command. Behaves like a skip otherwise.`
    // },
    // "rewrite-msg-on": {
    // 	description: `The rewriting commentary turned off by rewrite-msg-off can be restored by
    // 	this command. Behaves like a skip otherwise.`
    // },
    "rewrite-with-fnum": {
        description: `Use sequent formula FNUM to rewrite expressions`,
        syntax: `rewrite-with-fnum FNUM`
    },
    "rewrites": {
        description: `Rewrites with a list of lemmas or fnums. LEMMAS-OR-FNUMS has the form (LEMMAS-OR-FNUMS1 ... LEMMAS-OR-FNUMS).`,
        syntax: `rewrites LEMMAS-OR-FNUMS`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "same-name": {
        description: `Assume given constants are identical if their actuals are equal`,
        note: `This command is used to indicate that names are equal even if their actuals are not syntactically equal.`,
        syntax: `same-name NAME1 NAME2`
    },
    // "save": { 
    //     description: `Save current proof`,
    //     syntax: `save`
    // },
    "show-expanded-sequent": {
        description: `Displays expanded form of the current sequent. Use (show-expanded-sequent t) to show fully expanded names.`,
        syntax: `show-expanded-sequent`,
        optionals: {
            "t": `Show fully expanded names.`
        }
    },
    // "show-hidden": {
    //     description: `Show the list of hidden sequent formulas.`,
    //     syntax: `show-hidden`
    // },
    "show-hidden-formulas": {
        description: `Show the list of hidden sequent formulas.`,
        syntax: `show-hidden-formulas`
    },
    // "show-parens": { 
    //     description: `Show how infix operators and operands are associated by displaying formulas with full parenthesization`
    // },
    // "show-subst": { 
    //     description: `Tests command formation with substitutions extracted from extended expression specifications.`
    // },
    "simp-arith": {
        description: `Polynomial arithmetic simplification`,
        syntax: `simp-arith`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "simp-gls": {
        description: `Ground literal simplification`,
        syntax: `simp-gls`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "simp-real-null": {
        description: `Extraction of simple real nullstellensatz refutation certificates from equational constraints`,
        syntax: `simp-real-null`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "simp-tvs": {
        description: `Truth value simplification`,
        syntax: `simp-tvs`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "simp-zrhs": {
        description: `RHS zeroing with polynomial canonicalization`,
        syntax: `simp-zrhs`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "simple-induct": {
        description: `Selects an induction scheme according to the type of VAR in FORMULA and uses FORMULA to formulate an induction predicate`,
        syntax: `simple-induct VAR FORMULA`
    },
    "simple-measure-induct": {
        description: `Selects and insert an instance of measure induction as an antecedent formula`,
        syntax: `simple-measure-induct MEASURE VARS`
    },
    "simplify": {
        description: `Simplify using decision procedures`,
        syntax: `simplify`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "simplify-with-rewrites": {
        description: `Install rewrites and then simplify`,
        syntax: `simplify-and-rewrite`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "skeep": {
        description: `Skolemize using the names of the bounded variables as the names of the skolem constants`,
        syntax: `skeep`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    "skeep*": {
        description: `Iteratively skolemize a universally quantified formula using the names of the bounded variables as the names of the skolem constants`,
        syntax: `skeep*`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    // "skodef": { 
    //     description: ``
    // },
    // "skodef*": { 
    //     description: ``
    // },
    "skolem": {
        description: `Replace universally quantified variables in sequent fomula FNUM with SKOLEM-CONSTANTS`,
        syntax: `skolem FNUM SKOLEM-CONSTANTS`
    },
    "skolem!": {
        description: `Skolemize a universally quantified formula`,
        syntax: `skolem!`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    "skolem-typepred": {
        description: `Skolemize and then introduces type-constraints of the Skolem constants`,
        syntax: `skolem-typepred`,
        optionals: {
            "FNUMS": `Apply the command to sequent formula FNUMS.`
        }
    },
    "skoletin": {
        description: `Decompose a let-in expression`,
        syntax: `skoletin`
    },
    "skoletin*": {
        description: `Iteratively decompose a let-in expression`,
        syntax: `skoletin*`,
        optionals: {
            "FNUMS": `Apply the command to sequent formula FNUMS.`
        }
    },
    "skosimp": {
        description: `Skolemize and then simplify`,
        syntax: `skosimp`,
        optionals: {
            "FNUMS": `Apply the command to sequent formula FNUMS.`
        }
    },
    "skosimp*": {
        description: `Iteratively skolemize and then simplify`,
        syntax: `skosimp*`
    },
    // "smash": { 
    //     description: `Repeatedly tries BDDSIMP, ASSERT, and LIFT-IF`
    // },
    "splash": {
        description: `Asymmetrically split sequent fomulas`,
        syntax: `splash`,
        optionals: {
            "FNUMS": `Apply the command to sequent formula FNUMS.`
        }
    },
    "split": {
        description: `Split conjunctive sequent formulas`,
        syntax: `split`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "split-ineq": {
        description: `Split non-strict antecedent inequality (<= or >=) into two cases`,
        syntax: `split-ineq FNUM`
    },
    "sq-simp": {
        description: `Simplify using lemmas from theories 'sq' and 'sqrt'`,
        syntax: `sq-simp`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    "sub-formulas": {
        description: `Subtract two sequent formulas`,
        syntax: `sub-formulas FNUM1 FNUM2`
    },
    "suffices": {
        description: `Introduces a given expression in a universally quantified formula`,
        syntax: `suffices FNUM EXPR`
    },
    "swap": {
        description: `Try commutatively swapping two terms and replacing`,
        syntax: `swap TERM1 OP TERM2`
    },
    "swap!": {
        description: `Try commutatively swapping the two arguments of the function application found at EXPR-LOC`,
        syntax: `swap! EXPR-LOC`
    },
    "swap-group": {
        description: `Try associatively regrouping and swapping three terms`,
        syntax: `swap-group TERM1 OP TERM2 TERM3`
    },
    "swap-group!": {
        description: ` Try associatively regrouping the three subexpressions of the function applications found at EXPR-LOC`,
        syntax: `swap-group! EXPR-LOC`
    },
    "swap-rel": {
        description: `Swap the two sides of relational formulas`,
        syntax: `swap-rel FNUMS`
    },
    "triv-ideals": {
        description: `Ideal triviality checking via reduced Groebner bases`,
        syntax: `triv-ideals`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    "typepred": {
        description: `Make subtype constraints explicit for given expressions`,
        syntax: `typepred EXPRS`
    },
    // "typepred!": {
    //     description: `Make subtype constraints explicit for given expressions`,
    //     syntax: `typepred! EXPRS`
    // },
    "univ-sturm-ineqs": {
        description: `Sturm sequence sign-change analysis for univariate open-interval systems`,
        syntax: `univ-sturm-ineqs`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    "use": {
        description: `Import a lemma and use heuristic instantiation and then beta-reduction`,
        syntax: `use LEMMA`,
        optionals: {
            ":subst VAR EXPR": `List of substitutions for variable names.`
        }
    },
    "use*": {
        description: `Iternatively import a series of lemmas and use heuristic instantiation and then beta-reduction`,
        syntax: `use* LEMMAS`
    },
    "use-with": {
        description: `Import a lemma and use given sequent formulas for heuristic instantiation`,
        syntax: `use-with LEMMA FNUMS`
    },
    "ws1s": {
        description: `Decision procedure for Weak Second-order monadic logic of 1 Successor`,
        syntax: `ws1s`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    "ws1s-simp": {
        description: `Decision procedure for Weak Second-order monadic logic of 1 Successor`,
        syntax: `ws1s-simp`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    // "y2grind": { 
    //     description: `Core of GRIND: Installs rewrites, repeatedly applies BASH, and then invokes YICES`
    // },
    "y2simp": {
        description: `Repeatedly skolemizes and flattens, and then applies the Yices2 SMT solver`,
        syntax: `y2simp`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    // "ygrind": { 
    //     description: `Core of GRIND: Installs rewrites, repeatedly applies BASH, and then invokes YICES`
    // },
    // "yices": { 
    //     description: `Invokes Yices as an endgame SMT solver to prove that the conjunction of the negations of the selected formulas is unsatisfiable`,
    //     syntax: `yices`,
    //     optionals: {
    //         "FNUM": `Apply the command to sequent formula FNUM.`
    //     }
    // },
    // "yices-with-rewrites": { description: `` },
    "yices2": {
        description: `Invokes Yices2 as an endgame SMT solver to prove that the conjunction of the negations of the selected formulas is unsatisfiable`,
        syntax: `yices2`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    // "yices2-with-rewrites": { description: `` }
};
exports.PROOF_TACTICS = {
    "apply": {
        description: `In-line definition of user-defined strategies.`,
        syntax: `apply strategy COMMENT`,
        note: `Applies a user-defined strategy as if it were a primitive proof rule, and prints COMMENT.
            An example is as follows:
            'apply (then (skosimp*)(flatten)(inst?)) "Skolemizing, flattening, and instantiating"': 
        `
    },
    "branch": {
        description: `In-line definition of proof strategies for multiple branches.`,
        syntax: `branch STEP TACTIC+`,
        note: `Performs a branching STEP and then applies the i'th tactic to the i'th subgoal.
            If the number of subgoals is larger than the number of tactics, then the n'th tactic is applied to the remaining subgoals.\n  
            This command is typically used when step splits the proof into multiple branches
            where a different strategy is required for each of the branches.`
    },
    "branch-back": {
        description: `In-line definition of proof multiple proof tactics.`,
        syntax: `branch-back STEP TACTIC+`,
        note: `Perform a branching STEP and then applies the i'th tactic to the i'th subgoal.
            Automatic backtracking is performed for branches that fail to prove their goals.
            That is, if a tactic for a branch fails to prove its goal, the proof state for that branch 
            is rolled back to the point before the step was invoked.`
    },
    "comment": {
        description: `Attach a comment to a a proof node`,
        syntax: `comment LABEL`,
        note: `COMMENT is attached to the sequent. Formulas remain unchanged.`
    },
    // "default-strategy": { description:""},
    "deftactic": {
        description: `Defines a labelled proof tactic.`,
        syntax: `deftactic TACTIC_NAME TACTIC`,
        note: `Defines a labelled proof tactic. 
            The tactic is local to the current branch of the proof. 
            TACTIC_NAME needs to be a valid identifier in PVS.
            An example is as follows:
                '(deftactic foo (then (flatten) (assert) (grind)))': 
            `
    },
    "discriminate": {
        description: `Label formulas generated by a proof step`,
        syntax: `discriminate STEP LABEL`,
        note: `Labels formulas generated by STEP as LABEL(s).`
    },
    "else": {
        description: `Try STEPS in sequence until the first one succeeds.`,
        syntax: `else STEPS`
    },
    "else*": {
        description: `Try STEPS in sequence until the first one succeeds.`,
        syntax: `else* STEPS`
    },
    "equate": {
        description: `Try equating two expressions and replacing the LHS by the
    RHS in FNUMS.  Proof of the justification step can be tried or deferred.
    Use TRY-JUST to supply the rule for the justification proof or T for
    the default rule (GRIND).`,
        syntax: `equate lhs rhs`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    "extra-tcc-step": {
        description: " Tries to prove TCCs by first using (assert) and then (subtype-tcc)",
        syntax: `extra-tcc-step`
    },
    // "extrategies-about": { 
    //     description: ``,
    //     syntax: ``,
    //     effect: ``,
    //     examples: {}
    // },
    "fail": {
        description: `Propagate failure to the parent`,
        syntax: `fail`,
        note: `A failure signal is propagated to the parent proof goal. If the parent goal is
            not able to act on this signal, it further propagates the failure to its parent.
            This rule, like skip, is mainly employed in constructing strategies where it is
            used to control backtracking. Applying fail to the root sequent causes the
            proof to be unsuccessfully terminated.`
    },
    "finalize": {
        description: `Either finishes the current goal with STEP or does nothing.`,
        syntax: `field STEP`
    },
    "for": {
        description: ``,
        syntax: ``
    },
    "for-each": {
        description: ``,
        syntax: ``
    },
    "for-each-rev": {
        description: ``,
        syntax: ``
    },
    "for@": {
        description: ``,
        syntax: ``
    },
    "forward-chain": {
        description: ``,
        syntax: ``
    },
    "forward-chain*": {
        description: ``,
        syntax: ``
    },
    "forward-chain-theory": {
        description: ``,
        syntax: ``
    },
    "forward-chain@": {
        description: ``,
        syntax: ``
    },
    "if": {
        description: `Evaluates the condition (in Lisp) and if it evaluates to nil, step2 is applied, otherwise step1 is applied.`,
        syntax: ``
    },
    "if-label": {
        description: `Applies THEN-STEP if at least one formula in the sequent is labeled LABEL. Otherwise, applies ELSE-STEP.`,
        syntax: ``
    },
    "invoke": {
        description: `Invoke a rule or strategy by instantiating CMD with substitutions extracted from the extended expression specifications EXPR-SPECS`,
        syntax: `invoke CMD`,
        note: `Example: suppose formula 1 is f(x+y) = f(a*(z+1)).
            Then (invoke (case "%1 = %2") (? 1 "f(%1) = f(%2)"))
            would match and create the bindings %1='x+y' and %2='a*(z+1)', 
            which results in the prover command (case "x+y = a*(z+1)") being invoked.`
    },
    // "just-install-proof": { 
    //     description: `Installs an edited PROOF without actually checking it, declares the subgoal as finished, and marks the proof as unfinished`,
    //     syntax: "just-install-proof PROOF"
    // },
    "let": {
        description: `Allows variables in body to be bound to the results of Lisp computations.`,
        note: `Example: (let ((x (car *new-fmla-nums*))) (then (inst? x)(split x)))`,
        syntax: `let BINDING BODY`
    },
    "let-name-replace": {
        description: `For each LET expressions of the current sequent, create local definitions corresponding to the LET bindings using the NAME strategy, substituting these names into the rest of the LET expr`,
        syntax: `let-name-replace`,
        optionals: {
            "FNUMS": `Apply the command to sequent FNUMS.`
        }
    },
    "lisp": {
        description: `Evaluate a Lisp expression`,
        syntax: `lisp LISP-EXPR`
    },
    "mapstep": {
        description: `Sequentially applies FUNSTEP to each element of LIST.`,
        syntax: `mapstep FUNSTEP LIST`
    },
    "mapstep@": {
        description: `Sequentially applies FUNSTEP to each element of LIST.`,
        syntax: `mapstep@ FUNSTEP LIST`
    },
    "match": {
        description: `Try matching syntax patterns against formulas in the sequent`,
        syntax: `match SPEC-ITEMS`
    },
    "printf": {
        description: `Print a Lisp formatted string`,
        syntax: `printf MSG`
    },
    "protect": {
        description: `Protects formulas FNUMS so that they are not affected by STEP`,
        syntax: `protect FNUMS STEP`
    },
    // "query*": { 
    //     description: `Query the user for the next step.`,
    //     syntax: `query*`
    // },
    // "quote": { 
    //     description: `This command is used by the let strategy to ensure that the values for variables are not evaluated again after substitution.`
    // },
    "record": {
        description: `Uses decision procedures to simplify and record the formulas in FNUMS for further simplification`,
        syntax: `record`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "relabel": {
        description: `Create a new label for sequent formulas and keep the old labels`,
        syntax: `relabel LABEL FNUMS`
    },
    "repeat": {
        description: `Successively apply STEP along main branch until it does nothing`,
        syntax: `repeat STEP`
    },
    "repeat*": {
        description: `Successively apply STEP along main branch until it does nothing`,
        syntax: `repeat* STEP`
    },
    "rerun": {
        description: `Strategy to rerun existing or supplied proof`,
        syntax: `rerun`
    },
    "rotate++": {
        description: `Move the first succedent formula to the end of the succedents`,
        syntax: `rotate++`
    },
    "rotate--": {
        description: `Moves the first antecedent formula to the end of the antecedents.`,
        syntax: `rotate--`
    },
    "rule-induct": {
        description: `Applies co-induction over an inductive relation REL`,
        syntax: `rule-induct REL`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    "rule-induct-step": {
        description: `Applies co-induction over an inductive relation REL`,
        syntax: `rule-induct-step REL`,
        optionals: {
            "FNUMS": `Apply the command to sequent formulas FNUMS.`
        }
    },
    // "set-print-depth": {
    // 	description: `Sets the print depth for displaying formulas. Num must be a number. 0
    // 	means print the entire formula, any other number causes terms below the given
    // 	depth to be elided. Behaves like a skip otherwise.`
    // },
    // "set-print-length": { 
    // 	description: `Sets the print length for displaying formulas. Num must be a number. 0
    // 	means print the entire formula, any other number causes terms longet than the
    // 	given number to be elided. Behaves like a skip otherwise.`
    // },
    // "set-print-lines": {
    // 	description: `Sets the number of print lines for displaying formulas. Num must be a number.
    // 	0 means print the entire formula, any other number causes only the rst
    // 	num lines of each formula of the sequent to be displayed. Behaves like a skip
    // 	otherwise.`
    // },
    // "set-right-margin": { 
    //     description: `Sets the print right margin`
    // },
    // "skip": { 
    // 	description: `Has no effect on the proof. The primary utility of skip is in writing strategies
    // 	where a step is required to have no effect unless some condition holds. Typing
    // 	(skip) in response to a goal sequent returns the same proof state with a "No
    // 	change." message.`
    // },
    // "skip-msg": { 
    // 	description: `Has no effect on the proof but prints the given string. The main use of
    // 	skip-msg is in generating error messages from within strategies, typically as:
    // 	(if good?(input) ...(skip-msg "Bad input.")).`
    // },
    // "skip-steps": { 
    //     description: `This strategy is used for debugging purposes`
    // },
    // "sklisp": { 
    //     description: `Evaluates lispexpr and skips`
    // },
    "spread": {
        description: `Define proof strategies for branching steps.`,
        syntax: `spread STEP STEPLIST`,
        note: `Performs a branching STEP and then applies the i'th element of STEPLIST to the i'th subgoal. 
            This command is typically used when step splits the proof into multiple branches
            where a different strategy is required for each of the branches.`
    },
    "spread!": {
        description: ``,
        syntax: ``
    },
    "spread@": {
        description: ``,
        syntax: ``
    },
    "stop-rewrite": {
        description: `Turn off automatic rewrites`,
        syntax: `stop-rewrite NAMES`
    },
    "stop-rewrite-theory": {
        description: `Turn off all automatic rewrites defined in given theories`,
        syntax: `stop-rewrite-theory THEORY-NAMES`
    },
    // "tccs-expression": { 
    //     description: ``
    // },
    // "tccs-formula": { 
    //     description: ``
    // },
    // "tccs-formula*": { 
    //     description: ``
    // },
    // "tccs-step": { 
    //     description: ``
    // },
    "then": {
        description: ``,
        syntax: ``
    },
    "then*": {
        description: ``,
        syntax: ``
    },
    "then@": {
        description: ``,
        syntax: ``
    },
    "time": {
        description: ``,
        syntax: ``
    },
    "touch": {
        description: ``,
        syntax: ``
    },
    // "trace": {
    // 	description: `Turns on the tracing of the proof commands named in names so that any
    // 	time any one of the named rules or strategies is used in a proof, the entry into
    // 	and exit out of such commands is traced. This makes it possible to check if the
    // 	command is being properly invoked and has the desired effect. Behaves like a
    // 	skip otherwise.`
    // },
    // "track-all-current-rewrites": { description: "" },
    // "track-rewrite": {
    // 	description: `Explains why the attempt to apply a rewrite rule named in names was not
    // 	applied. Other than setting up the names of the rewrite rules to be tracked during
    // 	simplication, track-rewrite behaves like a skip. It has no effect on the current
    // 	proof sequent and is not saved as part of the partial or completed proof.`
    // },    
    "transform-both": {
        description: `Apply TRANSFORM to both sides of relational formula FNUM`,
        syntax: `transform-both FNUM TRANSFORM`
    },
    // "trust": { 
    //     description: ``
    // },
    // "trust!": { 
    //     description: `This strategy performs a miracle on behalf of trusted orcale ORCL`
    // },
    "try": {
        description: ``,
        syntax: ``
    },
    "try-branch": {
        description: ``,
        syntax: ``
    },
    "try-rewrites": {
        description: ``,
        syntax: ``
    },
    "undo": {
        description: `Undo the last proof command.`,
        note: `The prover steps back to the ancestor node of the current proof node.`,
        syntax: "undo"
    },
    "unlabel": {
        description: `Remove labels attached to sequent formulas`,
        syntax: `unlabel`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    "unlabel*": {
        description: `Remove labels attached to sequent formulas`,
        syntax: `unlabel*`,
        optionals: {
            "FNUM": `Apply the command to sequent formula FNUM.`
        }
    },
    "unless": {
        description: `Behaves as (if (not FLAG) (then STEP1 ... STEPn) (skip))`,
        syntax: `unless FLAG STEPS`
    },
    "unless-label": {
        description: `Sequentially applies STEPS to all branches as long as no formula in the sequent is labeled LABEL`,
        syntax: `unless-label LABEL STEPS`
    },
    "unless-label@": {
        description: `Sequentially applies STEPS to the main branch as long as no formula in the sequent is labeled LABEL`,
        syntax: `unless-label@ LABEL STEPS`
    },
    "unless@": {
        description: `Behaves as (if (not FLAG) (then@ STEP1 ... STEPn) (skip))`,
        syntax: `unless@ FLAG STEPS`
    },
    "untrace": {
        description: `Turns off the tracing of proof commands named in names, as initiated by (trace).`,
        syntax: "untrace"
    },
    "untrack-rewrite": {
        description: `Disables the tracking of rewrite rules invoked by track-rewrite.`,
        syntax: "untrack-rewrite"
    },
    "unwind-protect": {
        description: `Invoke MAIN-STEP followed by CLEANUP-STEP, which is performed even if MAIN-STEP leads to a proof of the current goal.`,
        syntax: `unwind-protect MAIN-STEP CLEANUP-STEP`
    },
    "when": {
        description: ``,
        syntax: ``
    },
    "when-label": {
        description: ``,
        syntax: ``
    },
    "when-label@": {
        description: ``,
        syntax: ``
    },
    "when@": {
        description: ``,
        syntax: ``
    },
    "with-focus-on": {
        description: ``,
        syntax: ``
    },
    "with-focus-on@": {
        description: ``,
        syntax: ``
    },
    "with-fresh-labels": {
        description: ``,
        syntax: ``
    },
    "with-fresh-labels@": {
        description: ``,
        syntax: ``
    },
    "with-fresh-names": {
        description: ``,
        syntax: ``
    },
    "with-fresh-names@": {
        description: ``,
        syntax: ``
    },
    "with-labels": {
        description: ``,
        syntax: ``
    },
    "with-tccs": {
        description: ``,
        syntax: ``
    },
    "wrap-formula": {
        description: ``,
        syntax: ``
    },
    "wrap-manip": {
        description: ``,
        syntax: ``
    },
};
// TODO: add more commands
exports.EVALUATOR_COMMANDS = {
    "RANDOM": {
        description: `Generate a random number.`,
        syntax: `RANDOM`
    },
    "debug": {
        description: "Turn on printing of debugging information",
        syntax: "debug"
    },
    "nodebug": {
        description: "Turn off printing of debugging information",
        syntax: "nodebug"
    },
    "timing": {
        description: "Turn on timing information per evaluation",
        syntax: "timing"
    },
    "notiming": {
        description: "Turn off timing information per evaluation",
        syntax: "notiming"
    },
    "tccs": {
        description: "Turn on TCCs generation per evaluation",
        syntax: "tccs"
    },
    "notccs": {
        description: "Turn off TCCs generation per evaluation",
        syntax: "notccs"
    },
    "load_pvs_attachments": {
        description: "Force a reload .pvs-attachments and pvs-attachments",
        syntax: "load_pvs_attachments"
    },
    "list_pvs_attachments": {
        description: "List semantic attachments loaded in the current context",
        syntax: "list_pvs_attachments"
    },
    "pvsio_version": {
        description: "Show current version of PVSio",
        syntax: "pvsio_version"
    }
};
// a selection of 32 useful commands for advanced users. The selection has been based on statistics from nasalib and feedback from experienced pvs users
exports.PROOF_COMMANDS_ADVANCED_PROFILE = {
    "all-typepreds": exports.PROOF_COMMANDS["all-typepreds"],
    "apply-ext": exports.PROOF_COMMANDS["apply-ext"],
    "assert": exports.PROOF_COMMANDS["assert"],
    "beta": exports.PROOF_COMMANDS["beta"],
    "bddsimp": exports.PROOF_COMMANDS["bddsimp"],
    "case": exports.PROOF_COMMANDS["case"],
    "decompose-equality": exports.PROOF_COMMANDS["decompose-equality"],
    "expand": exports.PROOF_COMMANDS["expand"],
    "eval-expr": exports.PROOF_COMMANDS["eval-expr"],
    "flatten": exports.PROOF_COMMANDS["flatten"],
    "grind": exports.PROOF_COMMANDS["grind"],
    "grind-reals": exports.PROOF_COMMANDS["grind-reals"],
    "ground": exports.PROOF_COMMANDS["ground"],
    "hide": exports.PROOF_COMMANDS["hide"],
    "iff": exports.PROOF_COMMANDS["iff"],
    "induct": exports.PROOF_COMMANDS["induct"],
    "inst?": exports.PROOF_COMMANDS["inst?"],
    "insteep": exports.PROOF_COMMANDS["insteep"],
    "label": exports.PROOF_COMMANDS["label"],
    "lemma": exports.PROOF_COMMANDS["lemma"],
    "lift-if": exports.PROOF_COMMANDS["lift-if"],
    "name": exports.PROOF_COMMANDS["name"],
    "prop": exports.PROOF_COMMANDS["prop"],
    "replace": exports.PROOF_COMMANDS["replace"],
    "rewrite": exports.PROOF_COMMANDS["rewrite"],
    "random-test": exports.PROOF_COMMANDS["random-test"],
    "skeep": exports.PROOF_COMMANDS["skeep"],
    "skoletin": exports.PROOF_COMMANDS["skoletin"],
    "skosimp*": exports.PROOF_COMMANDS["skosimp*"],
    "split": exports.PROOF_COMMANDS["split"],
    "typepred": exports.PROOF_COMMANDS["typepred"],
    "use": exports.PROOF_COMMANDS["use"]
};
exports.PROOF_COMMANDS_BASIC_PROFILE = {
    "all-typepreds": exports.PROOF_COMMANDS["all-typepreds"],
    "assert": exports.PROOF_COMMANDS["assert"],
    "beta": exports.PROOF_COMMANDS["beta"],
    "case": exports.PROOF_COMMANDS["case"],
    "expand": exports.PROOF_COMMANDS["expand"],
    "flatten": exports.PROOF_COMMANDS["flatten"],
    "grind": exports.PROOF_COMMANDS["grind"],
    "inst?": exports.PROOF_COMMANDS["inst?"],
    "lemma": exports.PROOF_COMMANDS["lemma"],
    "lift-if": exports.PROOF_COMMANDS["lift-if"],
    "prop": exports.PROOF_COMMANDS["prop"],
    "replace": exports.PROOF_COMMANDS["replace"],
    "skosimp*": exports.PROOF_COMMANDS["skosimp*"],
    "split": exports.PROOF_COMMANDS["split"]
};
function getCommands(profile) {
    switch (profile) {
        case "basic": {
            return exports.PROOF_COMMANDS_BASIC_PROFILE;
        }
        case "advanced": {
            return exports.PROOF_COMMANDS_ADVANCED_PROFILE;
        }
        default: return {};
    }
}
exports.getCommands = getCommands;
;
exports.VSCODE_COMMANDS = {
    "vscode-plot": {
        description: `Plot a PVS expression in vscode, e.g., vscode-plot (: -1/2, 1, 2 :)`,
        syntax: `vscode-plot <expr>`
    }
};
// list of evaluator commands
exports.evaluatorCommands = Object.assign(Object.assign({ "quit": {
        description: `Quit the evaluator`,
        syntax: `quit`
    }, "exit": {
        description: `Quit the evaluator`,
        syntax: `exit`
    }, "help": {
        description: `Displays the list of available evaluator commands`,
        syntax: `help`
    } }, exports.VSCODE_COMMANDS), exports.EVALUATOR_COMMANDS);
// list of prover commands -- this is used to update the integrated help in the prover/evaluator console
exports.proverCommands = Object.assign(Object.assign(Object.assign({ "undo": {
        description: `Undo last prover command`,
        syntax: `undo`,
        optionals: {
            "N": `Repeat undo N times.`
        }
    }, "quit": {
        description: `Quit prover`,
        syntax: `quit`
    }, "postpone": {
        description: `Postpone current proof goal`,
        syntax: `postpone`
    }, "help": {
        description: `Displays information about commands, including description and syntax. Use (help *) to show the full list of commands.`,
        syntax: `help CMD`
    } }, exports.VSCODE_COMMANDS), exports.PROOF_COMMANDS), exports.PROOF_TACTICS);
/**
 * Returns a list of hints for the evaluator/prover prompt.
 * Hints include:
 * - words from the last evaluator/prover state
 * - words from the theory specification
 * - evaluator/prover commands
 * If active line is provided, the function tries to match the hints with the given line
 */
function getHints(type, req) {
    req = req || {};
    let hints = {
        symbols: []
    };
    const activeLine = req.activeLine || "";
    if (req.theoryContent) {
        let txt = colorUtils.getPlainText(req.theoryContent).replace(new RegExp(exports.commentRegexp), "") // this removes all commented text
            .replace(new RegExp(exports.stringRegexp), ""); // this removes all strings
        // autocomplete symbol names
        const symbols = listSymbols(txt);
        // console.dir(symbols, { depth: null });
        if (symbols === null || symbols === void 0 ? void 0 : symbols.length) {
            hints.symbols = symbols.filter((c) => c.startsWith(activeLine));
        }
    }
    if (req.lastState) {
        // autocomplete symbol names
        const symbols = listSymbols(colorUtils.getPlainText(req.lastState));
        // console.dir(symbols, { depth: null });
        if (symbols === null || symbols === void 0 ? void 0 : symbols.length) {
            hints.symbols = hints.symbols.concat(symbols.filter((c) => c.startsWith(activeLine)));
        }
    }
    // include pvs keywords and library functions
    hints.keywords = languageKeywords_1.PVS_KEYWORDS;
    hints.libs = languageKeywords_1.PVS_LIBRARY_FUNCTIONS;
    // Include evaluator/prover commands in the list
    hints.commands = type === "evaluator" ? exports.evaluatorCommands : exports.proverCommands;
    return hints;
}
exports.getHints = getHints;
function checkPar(content, opt) {
    let par = 0;
    let quotes = 0;
    let txt = (content === null || content === void 0 ? void 0 : content.trim()) || "";
    txt = (opt === null || opt === void 0 ? void 0 : opt.includeStringContent) ? txt : txt.replace(/"[^"]*"/g, ""); // remove strings, to avoid counting parentheses in the string
    for (let i = 0; i < txt.length; i++) {
        switch (txt[i]) {
            case `(`: {
                par++;
                break;
            }
            case `)`: {
                par--;
                if (quotes && quotes % 2 === 0 && par % 2 !== 0) {
                    // unbalanced double quotes
                    let msg = `Error: Unbalanced double quotes at position ${i}.`;
                    msg += "\n" + txt.substring(0, i);
                    msg += "\n" + " ".repeat(i) + "^";
                    return { success: false, msg };
                }
                break;
            }
        }
    }
    const success = par <= 0;
    return { success, msg: (success) ? "" : "Error: Unbalanced parentheses." };
}
exports.checkPar = checkPar;
// utility function, returns a command with balanced parentheses
function balancePar(cmd) {
    if (cmd && !cmd.trim().startsWith("(")) {
        const openRegex = new RegExp(/\(/g);
        const closeRegex = new RegExp(/\)/g);
        let par = 0;
        while (openRegex.exec(cmd)) {
            par++;
        }
        while (closeRegex.exec(cmd)) {
            par--;
        }
        if (par > 0) {
            // missing closed brackets
            cmd = cmd.trimRight() + ')'.repeat(par);
            // console.log(`Mismatching parentheses automatically fixed: ${par} open round brackets without corresponding closed bracket.`)
        }
        else if (par < 0) {
            cmd = '('.repeat(-par) + cmd;
            // console.log(`Mismatching parentheses automatically fixed: ${-par} closed brackets did not match any other open bracket.`)
        }
        return cmd.startsWith('(') ? cmd : `(${cmd})`; // add outer parentheses if they are missing
    }
    return cmd;
}
exports.balancePar = balancePar;
/**
 * Utility function, checks if the given command is vscode-plot
 */
function isVSCodePlotCommand(cmd) {
    cmd = (cmd) ? cmd.trim() : cmd;
    return cmd && /\bvscode-plot\b/g.test(cmd);
}
exports.isVSCodePlotCommand = isVSCodePlotCommand;
/**
 * Utility function, checks if the given command is pvsioweb
 */
function isPVSioweb(cmd) {
    cmd = (cmd) ? cmd.trim() : cmd;
    return cmd && /\bpvsioweb\b/g.test(cmd);
}
exports.isPVSioweb = isPVSioweb;
/**
 * Utility function, returns the expression specified as argument of vscode-plot
 */
function getVSCodePlotExpression(cmd) {
    return isVSCodePlotCommand(cmd) ? cmd === null || cmd === void 0 ? void 0 : cmd.replace("vscode-plot", "") : "";
}
exports.getVSCodePlotExpression = getVSCodePlotExpression;
/**
 * Utility function, checks if the given command is quit or exit
 */
function isQuitCommand(cmd) {
    cmd = (cmd) ? cmd.trim() : cmd;
    return cmd && (cmd === "quit"
        || cmd === "quit;"
        || cmd === "(quit)"
        || cmd.toLocaleLowerCase() === "(quit)y"
        || cmd === "exit"
        || cmd === "exit;"
        || cmd === "(exit)"
        || cmd.toLocaleLowerCase() === "(exit)y")
        || /^\(?\s*quit\s*\)?\s*y?;?/gi.test(cmd)
        || /^\(?\s*exit\s*\)?\s*y?;?/gi.test(cmd);
}
exports.isQuitCommand = isQuitCommand;
function isSaveThenQuitCommand(cmd) {
    cmd = (cmd) ? cmd.trim() : cmd;
    return cmd && (cmd === "save-then-quit"
        || cmd === "save-then-quit;"
        || cmd === "(save-then-quit)"
        || cmd === "save-force-exit"
        || cmd === "save-force-exit;"
        || cmd === "(save-force-exit)");
}
exports.isSaveThenQuitCommand = isSaveThenQuitCommand;
function isQuitDontSaveCommand(cmd) {
    cmd = (cmd) ? cmd.trim() : cmd;
    return cmd && (cmd === "quit-dont-save"
        || cmd === "quit-dont-save;"
        || cmd === "(quit-dont-save)"
        || cmd === "exit-dont-save"
        || cmd === "exit-dont-save;"
        || cmd === "(exit-dont-save)");
}
exports.isQuitDontSaveCommand = isQuitDontSaveCommand;
/**
 * Utility function, checks if the given command is a 'lisp' command given to the prover
 */
function isLispCommand(cmd) {
    cmd = (cmd) ? cmd.trim() : cmd;
    return cmd &&
        /^\(?\s*lisp\s/gi.test(cmd);
}
exports.isLispCommand = isLispCommand;
function isEmptyCommand(cmd) {
    return !cmd
        || cmd.trim() === ""
        || cmd.trim() === "()";
}
exports.isEmptyCommand = isEmptyCommand;
function isUndoCommand(cmd) {
    cmd = (cmd) ? cmd.trim() : cmd;
    return cmd && (cmd === "undo"
        || cmd === "undo;"
        || cmd === "(undo)"
        || cmd.toLocaleLowerCase() === "(undo)y"
        || /^\(\s*undo(\s*\d+)?\s*\)\s*y?;?/gi.test(cmd));
}
exports.isUndoCommand = isUndoCommand;
function unfoldUndoCommand(cmd) {
    cmd = (cmd) ? cmd.trim() : cmd;
    const match = /^\(\s*undo(\s*\d+)?\s*\)\s*y?;?/gi.exec(cmd);
    let cmds = [];
    if (match) {
        if (match.length > 1 && match[1]) {
            const n = +match[1];
            for (let i = 0; i < n; i++) {
                cmds.push('(undo)');
            }
        }
        else {
            cmds.push('(undo)');
        }
    }
    return cmds;
}
exports.unfoldUndoCommand = unfoldUndoCommand;
function isRedoCommand(cmd) {
    cmd = (cmd) ? cmd.trim() : cmd;
    return cmd && /^\(?\s*\bredo\b/g.test(cmd);
}
exports.isRedoCommand = isRedoCommand;
function isUndoUndoCommand(cmd) {
    cmd = (cmd) ? cmd.trim() : cmd;
    if (cmd) {
        const cm = (cmd.startsWith("(")) ? cmd : `(${cmd})`;
        return /^\(\s*\bundo\s+undo\b\s*\)/g.test(cm);
    }
    return false;
}
exports.isUndoUndoCommand = isUndoUndoCommand;
function isUndoUndoPlusCommand(cmd) {
    cmd = (cmd) ? cmd.trim() : cmd;
    return cmd && /^\(?(\s*\bundo)+/g.test(cmd);
}
exports.isUndoUndoPlusCommand = isUndoUndoPlusCommand;
function isUndoStarCommand(cmd) {
    return isUndoCommand(cmd) || isUndoUndoCommand(cmd) || isUndoUndoPlusCommand(cmd);
}
exports.isUndoStarCommand = isUndoStarCommand;
function isPostponeCommand(cmd, result) {
    cmd = (cmd) ? cmd.trim() : cmd;
    if (result && result.commentary) {
        if (typeof result.commentary === "string") {
            return result.commentary.toLocaleLowerCase().startsWith("postponing ");
        }
        else if (typeof result.commentary === "object") {
            return result.commentary.length
                && typeof result.commentary[0] === "string"
                && result.commentary.filter((comment) => {
                    return comment.toLocaleLowerCase().startsWith("postponing ");
                }).length > 0;
        }
    }
    return cmd && /^\(?\s*\bpostpone\b/g.test(cmd);
}
exports.isPostponeCommand = isPostponeCommand;
function isSkipCommand(cmd) {
    cmd = (cmd) ? cmd.trim() : cmd;
    return cmd && /^\(?\s*\bskip\b/g.test(cmd);
}
exports.isSkipCommand = isSkipCommand;
function isFailCommand(cmd) {
    cmd = (cmd) ? cmd.trim() : cmd;
    return cmd && /^\(?\s*\bfail\b/g.test(cmd);
}
exports.isFailCommand = isFailCommand;
function isShowHiddenFormulas(cmd) {
    cmd = (cmd) ? cmd.trim() : cmd;
    return cmd && /^\(?\s*show-hidden(?:-formulas)?\b/g.test(cmd);
}
exports.isShowHiddenFormulas = isShowHiddenFormulas;
function isGrindCommand(cmd) {
    cmd = (cmd) ? cmd.trim() : cmd;
    return cmd && /^\(?\s*grind\b/g.test(cmd);
}
exports.isGrindCommand = isGrindCommand;
function isProofliteGlassbox(cmd) {
    return cmd && cmd.trim().startsWith("(then ");
}
exports.isProofliteGlassbox = isProofliteGlassbox;
// recognizes (show-expanded-sequent) command
function isShowExpandedSequentCommand(cmd) {
    cmd = (cmd) ? cmd.trim() : cmd;
    return cmd && /^\(?\s*show\-expanded\-sequent\s*t?/g.test(cmd);
}
exports.isShowExpandedSequentCommand = isShowExpandedSequentCommand;
function isShowFullyExpandedSequentCommand(cmd) {
    cmd = (cmd) ? cmd.trim() : cmd;
    return cmd && /^\(?\s*show\-expanded\-sequent\s*t\b/g.test(cmd);
}
exports.isShowFullyExpandedSequentCommand = isShowFullyExpandedSequentCommand;
/**
 * Utility function, splits a prover command into multiple commads based on the round parentheses
 */
function splitCommands(cmd) {
    var _a;
    let cmds = [];
    if (cmd === null || cmd === void 0 ? void 0 : cmd.trim()) {
        // try to split the command only if the parentheses match
        if (cmd.trim().startsWith("(") && ((_a = checkPar(cmd, { includeStringContent: true })) === null || _a === void 0 ? void 0 : _a.success)) {
            let input = cmd.trim().replace(/\)y/gi, ")");
            let par = 0;
            let start = 0;
            let stop = 0;
            let validStart = false;
            for (let i = 0; i < input.length; i++) {
                if (input[i] === "(") {
                    if (par === 0) {
                        start = i;
                        validStart = true;
                    }
                    par++;
                }
                else if (input[i] === ")") {
                    par--;
                    if (par === 0) {
                        stop = i;
                    }
                }
                if (par === 0) {
                    if (stop > start && validStart) { // sanity check
                        let cmd = input.substring(start, stop + 1);
                        if (isUndoCommand(cmd)) {
                            cmds = cmds.concat(unfoldUndoCommand(cmd));
                        }
                        else {
                            cmds = cmds.concat(cmd);
                        }
                        validStart = false;
                    }
                }
                if (par < 0) {
                    // too many closed parentheses -- try to skip
                    par = 0;
                }
            }
        }
        else if (isUndoCommand(cmd)) {
            cmds = unfoldUndoCommand(cmd);
        }
        else {
            cmds = [cmd];
        }
    }
    return cmds;
}
exports.splitCommands = splitCommands;
function isCommentCommand(cmd) {
    cmd = (cmd) ? cmd.trim() : cmd;
    return cmd && /^\(?\s*comment\b/g.test(cmd);
}
exports.isCommentCommand = isCommentCommand;
function isQEDCommand(cmd) {
    cmd = (cmd) ? cmd.trim() : cmd;
    return cmd && (cmd.trim() === "Q.E.D."
        || /^\(?\s*Q\.E\.D\./g.test(cmd));
}
exports.isQEDCommand = isQEDCommand;
function isSameCommand(cmd1, cmd2) {
    if (cmd1 && cmd2) {
        const c1 = cmd1.replace(/\s+/g, "").replace(/[\s+\"\(\)]/g, ""); // remove all spaces, round parens, and double quotes
        const c2 = cmd2.replace(/\s+/g, "").replace(/[\s+\"\(\)]/g, "");
        return c1 === c2;
    }
    return false;
}
exports.isSameCommand = isSameCommand;
function isPropax(cmd) {
    cmd = (cmd) ? cmd.trim() : cmd;
    return cmd && cmd === "(propax)";
}
exports.isPropax = isPropax;
/**
 * Utility function, detects if pvs has returned a string indicating an invalid command
 */
function isInvalidCommand(result) {
    const proverErrorMessages = [
        "Error:",
        "not a valid prover command",
        "Found 'eof' when expecting",
        "bad proof command",
        "Expecting an expression",
        "Not enough arguments for prover command",
        "Could not find formula number",
        "There is garbage at the end",
        "No such rule, defined rule or strategy"
    ];
    const isInvalid = (cmd) => {
        if (cmd) {
            for (let i = 0; i < proverErrorMessages.length; i++) {
                if (cmd.includes(proverErrorMessages[i])) {
                    return true;
                }
            }
        }
        return false;
    };
    if (result && result.commentary) {
        if (typeof result.commentary === "string") {
            return isInvalid(result.commentary);
        }
        else if (typeof result.commentary === "object") {
            return result.commentary.length
                && typeof result.commentary[0] === "string"
                && result.commentary.filter((comment) => {
                    return isInvalid(comment);
                }).length > 0;
        }
    }
    return false;
}
exports.isInvalidCommand = isInvalidCommand;
/**
 * Utility function, returns the invalid command communicated by PVS through the commentary field
 */
function getInvalidCommandName(result) {
    if (result && result.commentary) {
        // group 1 is the invalid command name
        const invalidCommandRegex = /Error:\s+(.+)\s+is\s+not\s+a\s+valid\s+prover\s+command\b/g;
        if (typeof result.commentary === "string") {
            const match = invalidCommandRegex.exec(result.commentary);
            return (match && match.length > 1 && match[1]) ? match[1] : null;
        }
        else if (typeof result.commentary === "object" && result.commentary.length) {
            for (let i = 0; i < result.commentary.length; i++) {
                const match = invalidCommandRegex.exec(result.commentary[i]);
                if (match && match.length > 1 && match[1]) {
                    return match[1];
                }
            }
        }
    }
    return null;
}
exports.getInvalidCommandName = getInvalidCommandName;
/**
 * Internal function, forces locale settings needed by pvs to correctly handle utf-8 symbols
 */
function forceLocale() {
    process.env["ACL_LOCALE"] = "en_US.UTF-8";
    process.env["LC_ALL"] = "en_US.UTF-8";
    process.env["LANG"] = "en_US.UTF-8";
    console.log(`\nACL_LOCALE=${process.env["ACL_LOCALE"]}`);
    console.log(`LC_ALL=${process.env["LC_ALL"]}`);
    console.log(`LANG=${process.env["LANG"]}\n`);
}
exports.forceLocale = forceLocale;
//# sourceMappingURL=languageUtils.js.map