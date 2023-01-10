"use strict";
/**
 * @module Pvs2Latex
 * @author Paolo Masci
 * @date 2023.01.03
 * @desc PvsDoc module for generating latex documentation files for pvs theories.
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
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.Pvs2Latex = void 0;
const fsUtils = require("./common/fsUtils");
const path = require("path");
;
// useful constants
// const DEFAULT_BASE_PATH: string = __dirname;
const DEFAULT_OUTPUT_FOLDER_NAME = "pvsdoc/latex";
const DEFAULT_INPUT_FOLDER = ".";
const DEFAULT_INPUT_FILE = "";
const DEFAULT_MAIN_FILE = "main.tex";
const LATEX_TEMPLATE_PREAMBLE = `
\\documentclass[preprint,12pt,authoryear,a4paper]{article}
\\usepackage{lineno,hyperref}
\\modulolinenumbers[1]

\\usepackage{graphicx}
\\usepackage{pdflscape}
\\usepackage{caption}
\\usepackage{subcaption}
\\usepackage{upquote} % this is to display properly backticks in verbatim
\\usepackage{listings}
\\usepackage{color}
\\definecolor{lightgray}{rgb}{.98,.98,.98}
\\definecolor{darkgray}{rgb}{.4,.4,.4}
\\definecolor{purple}{rgb}{0.65, 0.12, 0.82}
\\definecolor{darkgreen}{rgb}{0.2, 0.4, 0.3}
\\definecolor{brown}{cmyk}{0,0.81,1,0.60}
\\definecolor{olivegreen}{cmyk}{0.64,0,0.95,0.40}
\\definecolor{cadetblue}{cmyk}{0.62,0.57,0.23,0}
\\definecolor{lightlightgray}{gray}{0.9}

\\lstdefinelanguage{PVS}{
  keywords={true, false, if, then, else endif, recursive, measure, let, bool, list, in, endif, else, begin, type, theory, cond, endcond, importing},
  keywordstyle=\\color{blue}\\bfseries,
  ndkeywords={},
  ndkeywordstyle=\\color{blue}\\bfseries,
  identifierstyle=\\color{black},
  sensitive=false,
  comment=[l]{\\%},
  morecomment=[s]{/*}{*/},
  commentstyle=\\color{darkgreen}\\ttfamily,
  stringstyle=\\color{red}\ttfamily,
  morestring=[b]',
  morestring=[b]"
}
\\lstset{
   backgroundcolor=\\color{lightgray},
   extendedchars=true,
   basicstyle=\\scriptsize\\ttfamily,
   showstringspaces=false,
   showspaces=false,
   numbers=left,
   numberstyle=\\tiny\\color{darkgray},
   numbersep=5pt,
   tabsize=2,
   breaklines=true,
   showtabs=false,
   captionpos=b,
   %xleftmargin=10pt
}

\\title{title}
\\author{Paolo Masci}
\\ead{paolo.masci@nianet.org}
\\address{National Institute of Aerospace\\100 Exploration Way, 23666, Hampton, VA, USA}
\\fntext[thanks]{Research carried out by the author is supported by the National Aeronautics and Space Administration under NASA/NIA Cooperative Agreement NNL09AA00A.}

\\date{\\today}

\\begin{document}
\\begin{lstlisting}
`;
const LATEX_TEMPLATE_TRAILER = `
\\end{lstlisting}
\\end{document}
`;
/**
 * Pvs2Latex generates latex documentation files for pvs.
 */
class Pvs2Latex {
    /**
     * Generates latex documentation using a custom latex template
     */
    run(settings) {
        return __awaiter(this, void 0, void 0, function* () {
            if ((settings === null || settings === void 0 ? void 0 : settings.inputFile) || (settings === null || settings === void 0 ? void 0 : settings.inputFolder)) {
                this.inputFile = settings.inputFile || DEFAULT_INPUT_FILE;
                this.inputFolder = this.inputFile ? fsUtils.getContextFolder(this.inputFile) : (settings.inputFolder || DEFAULT_INPUT_FOLDER);
                this.outputFolder = settings.outputFolder ? path.resolve(settings.outputFolder) : path.resolve(fsUtils.tildeExpansion(this.inputFolder), DEFAULT_OUTPUT_FOLDER_NAME);
                const outputFolder = path.resolve(this.outputFolder);
                const mainFile = path.resolve(outputFolder, DEFAULT_MAIN_FILE);
                let success = yield this.createLatexFile(mainFile);
                success = success && (yield this.createMakefile());
                if (success) {
                    return {
                        outputFolder,
                        mainFile
                    };
                }
            }
            return null;
        });
    }
    /**
     * Utility function, creates the latex file
     */
    createLatexFile(mainFile) {
        return __awaiter(this, void 0, void 0, function* () {
            const content = yield fsUtils.readFile(path.resolve(this.inputFile));
            const latex = LATEX_TEMPLATE_PREAMBLE + content + LATEX_TEMPLATE_TRAILER;
            const success = yield fsUtils.writeFile(mainFile, latex);
            return success;
        });
    }
    /**
     * Utility function, creates a makefile with convenient targets to compile the latex source into a pdf
     */
    createMakefile() {
        return __awaiter(this, void 0, void 0, function* () {
            const content = `
all:
    pdflatex main.tex

clean:
    -rm *.aux
        `;
            const fname = path.resolve(this.outputFolder, "Makefile");
            const success = yield fsUtils.writeFile(fname, content);
            return success;
        });
    }
    /**
     * Utility function, returns the current settings
     */
    getSettings() {
        return {
            inputFile: this.inputFile,
            inputFolder: this.inputFolder,
            outputFolder: this.outputFolder
        };
    }
}
exports.Pvs2Latex = Pvs2Latex;
//# sourceMappingURL=pvs2latex.js.map