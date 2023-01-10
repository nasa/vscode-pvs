"use strict";
/**
 * @module Pvs2Html
 * @author Paolo Masci
 * @date 2022.12.06
 * @desc PvsDoc module for generating html documentation files for pvs theories.
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
exports.Pvs2Html = void 0;
const child_process_1 = require("child_process");
const languageUtils_1 = require("./common/languageUtils");
const fsUtils = require("./common/fsUtils");
const path = require("path");
;
// useful constants
// const DEFAULT_BASE_PATH: string = __dirname;
const DEFAULT_OUTPUT_FOLDER_NAME = "pvsdoc/html";
const DEFAULT_README_FILE = "README.md";
const DEFAULT_INPUT_FOLDER = ".";
const DEFAULT_INPUT_FILE = "";
const DEFAULT_MAIN_FILE = "index.html";
// library files, to be copied to the output folder
const LIB_FILES = [
    { fileName: "prettify-pvs-code.css", targetFolder: "styles" },
    { fileName: "prettify-pvs-code.js", targetFolder: "scripts/prettify" }
    // { fileName: "jsdocconfig.json", targetFolder: "scripts/prettify" }
];
const DEFAULT_JSDOC_EXECUTABLE = path.resolve("../../../server/node_modules", "jsdoc", "jsdoc.js");
const DEFAULT_LIB_FOLDER = path.resolve("lib");
/**
 * Pvs2Html generates html documentation files for pvs.
 */
class Pvs2Html {
    constructor() {
        this.inputFile = DEFAULT_INPUT_FILE;
        this.inputFolder = DEFAULT_INPUT_FOLDER;
        this.outputFolder = path.resolve(DEFAULT_INPUT_FOLDER, DEFAULT_OUTPUT_FOLDER_NAME);
        this.readmeFile = DEFAULT_README_FILE;
        this.mainFile = DEFAULT_MAIN_FILE;
        this.libFolder = DEFAULT_LIB_FOLDER;
        this.jsdocExecutable = DEFAULT_JSDOC_EXECUTABLE;
        /**
         * Documentation tags that can be introduced in the pvs files
         * The engine builds on jsdoc, so we need to use tags that are understood by jsdoc
         * To do this, we create here a mapping between pvs tags and jsdoc tags
         */
        this.tags = [
            { pvs: languageUtils_1.PvsTag.author, jsdoc: "@author" },
            { pvs: languageUtils_1.PvsTag.authors, jsdoc: "@author" },
            { pvs: languageUtils_1.PvsTag.contributors, jsdoc: "@author" },
            { pvs: languageUtils_1.PvsTag.theory, jsdoc: "@module" },
            { pvs: languageUtils_1.PvsTag.theorem, jsdoc: "@event" },
            { pvs: languageUtils_1.PvsTag.majortheorem, jsdoc: "@class" },
            { pvs: languageUtils_1.PvsTag.lemma, jsdoc: "@event" },
            { pvs: languageUtils_1.PvsTag.function, jsdoc: "@member" },
            { pvs: languageUtils_1.PvsTag.returns, jsdoc: "@returns" },
            { pvs: languageUtils_1.PvsTag.datatype, jsdoc: "@typedef" },
            { pvs: languageUtils_1.PvsTag.param, jsdoc: "@param" },
            { pvs: languageUtils_1.PvsTag.const, jsdoc: "@const" },
            { pvs: languageUtils_1.PvsTag.description, jsdoc: "@description" },
            { pvs: languageUtils_1.PvsTag.desc, jsdoc: "@desc" },
            { pvs: languageUtils_1.PvsTag.date, jsdoc: "@date" },
            { pvs: languageUtils_1.PvsTag.QED, jsdoc: "@yields QED" },
            { pvs: languageUtils_1.PvsTag.yields, jsdoc: "@yields" },
            { pvs: languageUtils_1.PvsTag.stats, jsdoc: "@stats" },
        ];
    }
    /**
     * Generates html documentation using jsdoc as baseline engine
     */
    run(settings) {
        return __awaiter(this, void 0, void 0, function* () {
            if ((settings === null || settings === void 0 ? void 0 : settings.inputFile) || (settings === null || settings === void 0 ? void 0 : settings.inputFolder)) {
                this.inputFile = settings.inputFile || DEFAULT_INPUT_FILE;
                this.inputFolder = this.inputFile ? fsUtils.getContextFolder(this.inputFile) : (settings.inputFolder || DEFAULT_INPUT_FOLDER);
                this.outputFolder = settings.outputFolder ? path.resolve(settings.outputFolder) : path.resolve(fsUtils.tildeExpansion(this.inputFolder), DEFAULT_OUTPUT_FOLDER_NAME);
                this.jsdocExecutable = settings.docEngine ? path.resolve(settings.docEngine) : DEFAULT_JSDOC_EXECUTABLE;
                this.libFolder = settings.docEngineLibs ? path.resolve(settings.docEngineLibs) : DEFAULT_LIB_FOLDER;
                this.readmeFile = settings.readmeFile || DEFAULT_README_FILE;
                this.mainFile = path.resolve(this.outputFolder, DEFAULT_MAIN_FILE);
                let success = yield this.preProcess();
                success = success && (yield this.execJsDoc());
                success = success && (yield this.postProcess());
                success = success && (yield this.copyImages());
                success = success && (yield this.copyLibraries());
                if (success) {
                    return {
                        outputFolder: this.outputFolder,
                        mainFile: this.mainFile
                    };
                }
            }
            return null;
        });
    }
    /**
     * Utility function, returns the current settings
     */
    getSettings() {
        return {
            inputFile: this.inputFile,
            inputFolder: this.inputFolder,
            outputFolder: this.outputFolder,
            docEngine: this.jsdocExecutable,
            readmeFile: this.readmeFile
        };
    }
    /**
     * Copies images to the output folder
     */
    copyImages() {
        return __awaiter(this, void 0, void 0, function* () {
            const imgInputFolder = path.resolve(this.inputFolder, "img");
            const imgFolderExists = fsUtils.folderExists(imgInputFolder);
            if (imgFolderExists) {
                const imgOutputFolder = path.resolve(this.outputFolder, "img");
                console.log(`[pvs-doc] Copying images`, { imgInputFolder, imgOutputFolder });
                try {
                    const cmd = `cp -R ${imgInputFolder} ${imgOutputFolder}`;
                    // console.log(cmd);
                    (0, child_process_1.execSync)(cmd, { encoding: "utf8" });
                    console.log(`[pvs-doc] Done with copying images`, { imgInputFolder, imgOutputFolder });
                    return true;
                }
                catch (error) {
                    console.error(`[pvs-doc] Error while trying to copy images `, { imgInputFolder, imgOutputFolder });
                    return false;
                }
            }
            return true;
        });
    }
    /**
     * Copies .js libraries needed for syntax highlighting
     */
    copyLibraries() {
        return __awaiter(this, void 0, void 0, function* () {
            if (this.outputFolder) {
                const targetFolder = path.resolve(this.outputFolder);
                console.log(`[pvs-doc] Copying files to ${targetFolder}`);
                let success = true;
                for (let i = 0; i < LIB_FILES.length; i++) {
                    const libFile = path.resolve(this.libFolder, LIB_FILES[i].fileName);
                    const libTargetFile = path.resolve(targetFolder, LIB_FILES[i].targetFolder, LIB_FILES[i].fileName);
                    success = success && fsUtils.copyFile(libFile, libTargetFile);
                }
                console.log(`[pvs-doc] Done with copying files!`);
                return success;
            }
            return false;
        });
    }
    /**
     * Utility function, generates a default readme file content
     */
    getDefaultReadmeFileContent(desc) {
        return __awaiter(this, void 0, void 0, function* () {
            if (desc === null || desc === void 0 ? void 0 : desc.inputFile) {
                // it's a theory, try to pull information from @theory @author @date @desc
                console.log(`[pvs2html] Processing theory tags...`);
                const fileContent = yield fsUtils.readFile(path.resolve(desc.inputFile));
                const theoryName = (0, languageUtils_1.getPvsTag)(languageUtils_1.PvsTag.theory, fileContent) || fsUtils.getFileName(desc.inputFile) || "";
                console.log(`[pvs2html] theory name: ${theoryName}`);
                let ans = `# Theory ${theoryName}`;
                const author = (0, languageUtils_1.getPvsTag)(languageUtils_1.PvsTag.author, fileContent) || "";
                if (author) {
                    ans += `\n### Author\n- ${author}`;
                }
                const authors = (0, languageUtils_1.getPvsTag)(languageUtils_1.PvsTag.authors, fileContent) || "";
                if (authors) {
                    ans += `\n### Authors\n- ${authors}`;
                }
                const contributors = (0, languageUtils_1.getPvsTag)(languageUtils_1.PvsTag.contributors, fileContent) || "";
                if (contributors) {
                    ans += `\n### Contributors\n- ${contributors}`;
                }
                const date = (0, languageUtils_1.getPvsTag)(languageUtils_1.PvsTag.date, fileContent) || "";
                if (date) {
                    ans += `\n### Date\n- ${date}`;
                }
                const description = (0, languageUtils_1.getPvsTag)(languageUtils_1.PvsTag.desc, fileContent) || (0, languageUtils_1.getPvsTag)(languageUtils_1.PvsTag.description, fileContent) || "";
                if (description) {
                    ans += `\n### Description\n- ${description}`;
                }
                return ans;
            }
            else if (desc === null || desc === void 0 ? void 0 : desc.inputFolder) {
                // it's a library
                const libName = fsUtils.getContextFolderName(desc.inputFolder);
                return `# Library ${libName}`;
            }
            return null;
        });
    }
    /**
     * Executes the jsdoc script
     */
    execJsDoc() {
        return __awaiter(this, void 0, void 0, function* () {
            const inputFile = this.inputFile ? path.resolve(this.inputFile) : null;
            const inputFolder = path.resolve(this.inputFolder);
            const outputFolder = path.resolve(this.outputFolder);
            let readmeFile = path.resolve(inputFolder, this.readmeFile);
            const readmeFileExists = fsUtils.fileExists(readmeFile);
            if (!readmeFileExists) {
                readmeFile = path.resolve(this.getTempFolder(this.inputFile ? this.inputFile : this.inputFolder), "README.md");
                // create a predefined readme file containing basic information about the theory or library name
                console.log(`[pvs2html] Generating default README.md file`);
                const content = yield this.getDefaultReadmeFileContent({ inputFile, inputFolder });
                if (content) {
                    yield fsUtils.writeFile(readmeFile, content);
                }
            }
            const opt_readme = `--readme ${readmeFile}`; // uses the indicated readme file to create the content of the index page
            const opt_recurse = inputFile ? "" : "--recurse"; // look into all subfolders if a specific input file is not specified
            const jsdocInput = inputFile ?
                path.resolve(this.getTempFolder(inputFile), `${fsUtils.getFileName(inputFile, { keepExtension: false })}.js`)
                : path.resolve(this.getTempFolder(inputFolder), "*.js");
            const jsdoc = `node ${this.jsdocExecutable} -d ${outputFolder} ${opt_readme} ${opt_recurse} ${jsdocInput}`;
            console.log("------------------------");
            console.log(`[pvs-doc] Executing script: ${jsdoc}`);
            const success = yield new Promise((resolve, reject) => {
                (0, child_process_1.exec)(jsdoc, { cwd: inputFolder, encoding: "utf8" }, (error, stdout, stderr) => {
                    if (error) {
                        console.error(error);
                        return resolve(false);
                    }
                    resolve(true);
                });
            });
            console.log("------------------------");
            if (success) {
                // remove temporary files
                const tempFolder = path.resolve(this.inputFolder, "temp");
                console.log(`[pvs-doc] Temp folder: ${tempFolder}`);
                // if (tempFolder) {
                //     console.log(`[pvs-doc] Removing temporary files in ${tempFolder}`);
                //     execSync(`rm -r ${tempFolder}`, { encoding: "utf8" });
                // }
            }
            return success;
        });
    }
    /**
     * Performs pre-processing for all pvs files in the context folder
     */
    preProcess() {
        var _a;
        return __awaiter(this, void 0, void 0, function* () {
            let success = true;
            const inputFolder = path.resolve(this.inputFolder);
            console.log(`[pvs-doc] Preprocessing folder ${inputFolder}`);
            const lst = yield this.listFiles(inputFolder, ".pvs");
            if ((_a = lst === null || lst === void 0 ? void 0 : lst.fileNames) === null || _a === void 0 ? void 0 : _a.length) {
                for (let i = 0; i < lst.fileNames.length; i++) {
                    success = success && (yield this.preProcessFile(path.resolve(lst.contextFolder, lst.fileNames[i])));
                }
            }
            console.log(`[pvs-doc] Done with preprocessing!`);
            return success;
        });
    }
    /**
     * Utility function, returns the temporary folder where files to be processed by jsdoc are stored
     */
    getTempFolder(fname) {
        const contextFolder = fsUtils.getContextFolder(fname);
        return path.resolve(contextFolder, "temp");
    }
    /**
     * Performs pre-processing for a given pvs file
     */
    preProcessFile(ifname) {
        return __awaiter(this, void 0, void 0, function* () {
            console.log(`[pvs-doc] Preprocessing file ${ifname}`);
            const content = yield fsUtils.readFile(ifname);
            let dummyContent = "";
            let info = "";
            if (content) {
                // get comments in the header and use them to describe the file
                info = content.substring(0, content.indexOf("THEORY") || content.indexOf("theory"));
                info = info.substring(0, info.lastIndexOf("%"));
                info = info.substring(0, info.indexOf("@theory"));
                info = info.replace(/%/g, "");
                // comment everything
                dummyContent = content.split("\n").map(line => { return `//${line}`; }).join("\n");
                // map pvs tags to jsdoc tags
                for (let i = 0; i < this.tags.length; i++) {
                    const regexp = new RegExp(`\/\/\\s*%+\\s*${this.tags[i].pvs}\\b`, "g");
                    dummyContent = dummyContent.replace(regexp, `* ${this.tags[i].jsdoc}`);
                }
                // remove any line that is not a tag
                dummyContent = dummyContent.replace(/\/\/.*/g, "//");
                // check if @yields QED tags are associated with a @theorem tag, if not, add the @theorem tag
                // group 1 is the theorem name, group 2 are comments
                const regexQED = [
                    new RegExp(`${languageUtils_1.PvsTag.yields}\\s+QED\\s+([^\\s]+)(.*)`, "g"),
                    new RegExp(`${languageUtils_1.PvsTag.QED}\\s+([^\\s]+)(.*)`, "g"),
                ];
                let match = null;
                for (let i = 0; i < regexQED.length; i++) {
                    const regex = regexQED[i];
                    while (match = regex.exec(dummyContent)) {
                        const theoremName = match[1];
                        // check that the theorem name is valid, i.e., it starts with a letter
                        if (theoremName && /\w/.test(theoremName[0])) {
                            const regexTheorem = new RegExp(`${languageUtils_1.PvsTag.theorem}\\s+${theoremName}\\b`, "g");
                            const regexMajorTheorem = new RegExp(`${languageUtils_1.PvsTag.majortheorem}\\s+${theoremName}\\b`, "g");
                            // if @theorem tag is not present, add it before @yields QED
                            if (!regexTheorem.test(dummyContent) && !regexMajorTheorem.test(dummyContent)) {
                                dummyContent = dummyContent.replace(match[0], `@event ${theoremName}`);
                            }
                        }
                    }
                }
                // create comment marker start '/**'
                dummyContent = dummyContent.replace(/(\/\/\s*)(\*\s*\@)/g, "$1/*$2");
                if (dummyContent.trimStart().startsWith("*")) {
                    dummyContent = "/*" + dummyContent;
                }
                // create comment marker end '*/'
                dummyContent = dummyContent.replace(/(\*\s*\@.*)(\n\/\/)/g, "$1 */ $2");
                if (!dummyContent.trimEnd().endsWith("*/")) {
                    dummyContent += "*/";
                }
            }
            const fileName = fsUtils.getFileName(ifname);
            const tempFolder = this.getTempFolder(ifname);
            const ofname = path.resolve(tempFolder, `${fileName}.js`);
            const readmeFile = path.resolve(tempFolder, this.readmeFile);
            console.log(`[pvs-doc] Writing file ${ofname}`);
            yield fsUtils.createFolder(tempFolder);
            let success = yield fsUtils.writeFile(ofname, dummyContent);
            success = success && (yield fsUtils.writeFile(readmeFile, info));
            return success;
        });
    }
    /**
     * Performs post-processing for all files generated by jsdoc
     */
    postProcess() {
        return __awaiter(this, void 0, void 0, function* () {
            let success = true;
            const outputFolder = path.resolve(this.outputFolder);
            console.log(`[pvs-doc] Postprocessing folder ${outputFolder}`);
            const fileList = yield this.listFiles(outputFolder, ".html");
            if (fileList && fileList.fileNames && fileList.fileNames.length) {
                // first process index.html
                const index = path.resolve(outputFolder, "index.html");
                console.log(`[pvs-doc] Postprocessing file ${index}`);
                let content = yield fsUtils.readFile(index);
                content = content.replace(`<h1 class="page-title">Home</h1>`, "");
                content = content.replace(/<h3>\s*<\/h3>/, "");
                const matchLibName = /<article><h1>(.*)<\/h1>/g.exec(content);
                if (matchLibName && matchLibName.length > 1 && matchLibName[1]) {
                    content = content.replace(/Home<\//g, `${matchLibName[1]}</`);
                }
                success = success && (yield fsUtils.writeFile(index, content));
                // then, process the entire folder
                for (let i = 0; i < fileList.fileNames.length; i++) {
                    const ofname = path.resolve(outputFolder, fileList.fileNames[i]);
                    // console.log(`[pvs-doc] Postprocessing file ${ofname}`);
                    let content = yield fsUtils.readFile(ofname);
                    // content = content.replace(/\s\s+/g, " ");
                    content = content.replace(/<span class=\"type\-signature\">\(inner\) <\/span>/g, "");
                    content = content.replace(/>Modules<\/h3>/g, ">Theories</h3>");
                    content = content.replace(/>\s*\/?Classes<\/h3>/g, ">Major Theorems</h3>");
                    content = content.replace(/>\s*\/?Events<\/h3>/g, ">Theorems</h3>");
                    content = content.replace(/>\s*\/?Members<\/h3>/g, ">Definitions</h3>");
                    content = content.replace(/\bJSDoc:\s*/g, "");
                    content = content.replace(/\bClass:/g, "Major Theorem:");
                    content = content.replace(/<header>[\w\W\s]*<\/header>/g, "");
                    content = content.replace(/<span class="signature">\(\)<\/span>/g, "");
                    content = content.replace(/<span class="type-signature"><\/span>new/g, "");
                    content = content.replace(/\bModule:/g, "Theory:");
                    content = content.replace(/.js.html/g, ".pvs.html");
                    content = content.replace(/.js</g, ".pvs<");
                    content = content.replace(/\bprettify.js\b/g, "prettify-pvs-code.js");
                    content = content.replace(/\bjsdoc-default.css\b/g, "prettify-pvs-code.css");
                    content = content.replace(/\/\s*<\/div>/g, `<\/div>`);
                    if (matchLibName && matchLibName.length > 1 && matchLibName[1]) {
                        content = content.replace(/Home<\//g, `${matchLibName[1]}</`);
                    }
                    content = content.replace(`Documentation generated by <a href="https://github.com/jsdoc/jsdoc">JSDoc `, `Documentation generated by <a href="https://github.com/nasa/vscode-pvs">VSCode-PVS</a> through <a href="https://github.com/jsdoc/jsdoc">JSDoc`);
                    if (ofname.endsWith(".js.html")) {
                        // replace <code> ... </code> section with the actual pvs spec
                        const fileName = fsUtils.getFileName(ofname, { extension: ".js.html" });
                        const pvsFile = path.resolve(this.inputFolder, `${fileName}.pvs`);
                        // console.log("original pvs file:", pvsFile);
                        let theory = yield fsUtils.readFile(pvsFile);
                        // console.log(theory);
                        content = content.replace(/<code>[\w\W\s]+<\/code>/, "<code>" + theory + "<\/code>");
                        success = success && (yield fsUtils.writeFile(ofname.replace(".js.html", ".pvs.html"), content));
                        // await fsUtils.deleteFile(ofname);
                    }
                    else {
                        success = success && (yield fsUtils.writeFile(ofname, content));
                    }
                }
            }
            console.log(`[pvs-doc] Done with postprocessing folder ${outputFolder}`);
            return success;
        });
    }
    /**
     * Utility function, returns the list of files in a given folder
     */
    listFiles(folder, extension) {
        return __awaiter(this, void 0, void 0, function* () {
            if (folder) {
                const children = yield fsUtils.readDir(folder);
                const fileList = {
                    fileNames: children.filter((fileName) => {
                        return fileName.endsWith(extension)
                            && !fileName.startsWith("."); // this second part is necessary to filter out temporary files created by pvs
                    }),
                    contextFolder: folder
                };
                return fileList;
            }
            return null;
        });
    }
}
exports.Pvs2Html = Pvs2Html;
//# sourceMappingURL=pvs2html.js.map