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

import { exec, ExecException, execSync } from 'child_process';
import { FileList, PvsDocDescriptor } from './common/serverInterface'
import { getPvsTag, PvsTag } from './common/languageUtils';
import * as fsUtils from './common/fsUtils';
import * as path from 'path';

/**
 * Pvs2Html settings interface
 */
export interface Pvs2HtmlSettings {
    inputFile?: string,
    inputFolder?: string,
    outputFolder?: string,
    readmeFile?: string,
    docEngine?: string, // location of the jsdoc executable
    docEngineLibs?: string // location of the js libraries needed for syntax highlighting
};

// useful constants
// const DEFAULT_BASE_PATH: string = __dirname;
const DEFAULT_OUTPUT_FOLDER_NAME: string = "pvsdoc/html";
const DEFAULT_README_FILE: string = "README.md";
const DEFAULT_INPUT_FOLDER: string = ".";
const DEFAULT_INPUT_FILE: string = "";
const DEFAULT_MAIN_FILE: string = "index.html";

// library files, to be copied to the output folder
const LIB_FILES: { fileName: string, targetFolder: string }[] = [
    { fileName: "prettify-pvs-code.css", targetFolder: "styles" },
    { fileName: "prettify-pvs-code.js", targetFolder: "scripts/prettify" }
    // { fileName: "jsdocconfig.json", targetFolder: "scripts/prettify" }
];
const DEFAULT_JSDOC_EXECUTABLE: string = path.resolve("../../../server/node_modules", "jsdoc", "jsdoc.js");
const DEFAULT_LIB_FOLDER: string = path.resolve("lib");

/**
 * Pvs2Html generates html documentation files for pvs.
 */
export class Pvs2Html {
    protected inputFile: string = DEFAULT_INPUT_FILE;
    protected inputFolder: string = DEFAULT_INPUT_FOLDER;
    protected outputFolder: string = path.resolve(DEFAULT_INPUT_FOLDER, DEFAULT_OUTPUT_FOLDER_NAME);
    protected readmeFile: string = DEFAULT_README_FILE;
    protected mainFile: string = DEFAULT_MAIN_FILE;
    protected libFolder: string = DEFAULT_LIB_FOLDER;
    protected jsdocExecutable: string = DEFAULT_JSDOC_EXECUTABLE

    /**
     * Documentation tags that can be introduced in the pvs files
     * The engine builds on jsdoc, so we need to use tags that are understood by jsdoc
     * To do this, we create here a mapping between pvs tags and jsdoc tags
     */
    readonly tags: { pvs: PvsTag, jsdoc: string }[] = [
        { pvs: PvsTag.author, jsdoc: "@author" },
        { pvs: PvsTag.authors, jsdoc: "@author" },
        { pvs: PvsTag.contributors, jsdoc: "@author" },

        { pvs: PvsTag.theory, jsdoc: "@module" },
        { pvs: PvsTag.theorem, jsdoc: "@event" },
        { pvs: PvsTag.majortheorem, jsdoc: "@class" },
        { pvs: PvsTag.lemma, jsdoc: "@event" },

        { pvs: PvsTag.function, jsdoc: "@member" },
        { pvs: PvsTag.returns, jsdoc: "@returns" },
        { pvs: PvsTag.datatype, jsdoc: "@typedef" },
        { pvs: PvsTag.param, jsdoc: "@param" },
        { pvs: PvsTag.const, jsdoc: "@const" },

        { pvs: PvsTag.description, jsdoc: "@description" },
        { pvs: PvsTag.desc, jsdoc: "@desc" },
        { pvs: PvsTag.date, jsdoc: "@date" },
        { pvs: PvsTag.QED, jsdoc: "@yields QED" },
        { pvs: PvsTag.yields, jsdoc: "@yields" },
        { pvs: PvsTag.stats, jsdoc: "@stats" },
    ];

    /**
     * Generates html documentation using jsdoc as baseline engine
     */
    async run (settings: Pvs2HtmlSettings): Promise<PvsDocDescriptor | null> {
        if (settings?.inputFile || settings?.inputFolder) {
            this.inputFile = settings.inputFile || DEFAULT_INPUT_FILE;
            this.inputFolder = this.inputFile ? fsUtils.getContextFolder(this.inputFile) : (settings.inputFolder || DEFAULT_INPUT_FOLDER);
            this.outputFolder = settings.outputFolder ? path.resolve(settings.outputFolder) : path.resolve(fsUtils.tildeExpansion(this.inputFolder), DEFAULT_OUTPUT_FOLDER_NAME);
            this.jsdocExecutable = settings.docEngine ? path.resolve(settings.docEngine) : DEFAULT_JSDOC_EXECUTABLE;
            this.libFolder = settings.docEngineLibs ? path.resolve(settings.docEngineLibs) : DEFAULT_LIB_FOLDER;
            this.readmeFile = settings.readmeFile || DEFAULT_README_FILE;
            this.mainFile = path.resolve(this.outputFolder, DEFAULT_MAIN_FILE);
            let success: boolean = await this.preProcess();
            success = success && await this.execJsDoc();
            success = success && await this.postProcess();
            success = success && await this.copyImages();
            success = success && await this.copyLibraries();
            if (success) {
                return {
                    outputFolder: this.outputFolder,
                    mainFile: this.mainFile
                };
            }
        }
        return null;
    }

    /**
     * Utility function, returns the current settings
     */
    getSettings (): Pvs2HtmlSettings {
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
    async copyImages (): Promise<boolean> {
        const imgInputFolder: string = path.resolve(this.inputFolder, "img");
        const imgFolderExists: boolean = fsUtils.folderExists(imgInputFolder);
        if (imgFolderExists) {
            const imgOutputFolder: string = path.resolve(this.outputFolder, "img");
            console.log(`[pvs-doc] Copying images`, { imgInputFolder, imgOutputFolder });
            try {
                const cmd: string = `cp -R ${imgInputFolder} ${imgOutputFolder}`;
                // console.log(cmd);
                execSync(cmd, { encoding: "utf8" });
                console.log(`[pvs-doc] Done with copying images`, { imgInputFolder, imgOutputFolder });
                return true;
            } catch (error) {
                console.error(`[pvs-doc] Error while trying to copy images `, { imgInputFolder, imgOutputFolder });
                return false;
            }
        }
        return true;
    }

    /**
     * Copies .js libraries needed for syntax highlighting 
     */
    async copyLibraries (): Promise<boolean> {
        if (this.outputFolder) {
            const targetFolder: string = path.resolve(this.outputFolder);
            console.log(`[pvs-doc] Copying files to ${targetFolder}`);
            let success: boolean = true;
            for (let i = 0; i < LIB_FILES.length; i++) {
                const libFile: string = path.resolve(this.libFolder, LIB_FILES[i].fileName);
                const libTargetFile: string = path.resolve(targetFolder, LIB_FILES[i].targetFolder, LIB_FILES[i].fileName);
                success = success && fsUtils.copyFile(libFile, libTargetFile);
            }
            console.log(`[pvs-doc] Done with copying files!`);
            return success;
        }
        return false;
    }
    /**
     * Utility function, generates a default readme file content
     */
    async getDefaultReadmeFileContent (desc: { inputFile?: string, inputFolder?: string }): Promise<string> {
        if (desc?.inputFile) {
            // it's a theory, try to pull information from @theory @author @date @desc
            console.log(`[pvs2html] Processing theory tags...`);
            const fileContent: string = await fsUtils.readFile(path.resolve(desc.inputFile));
            const theoryName: string = getPvsTag(PvsTag.theory, fileContent) || fsUtils.getFileName(desc.inputFile) || "";
            console.log(`[pvs2html] theory name: ${theoryName}`);
            let ans: string = `# Theory ${theoryName}`;
            const author: string = getPvsTag(PvsTag.author, fileContent) || "";
            if (author) { ans += `\n### Author\n- ${author}`; }
            const authors: string = getPvsTag(PvsTag.authors, fileContent) || "";
            if (authors) { ans += `\n### Authors\n- ${authors}`; }
            const contributors: string = getPvsTag(PvsTag.contributors, fileContent) || "";
            if (contributors) { ans += `\n### Contributors\n- ${contributors}`; }
            const date: string = getPvsTag(PvsTag.date, fileContent) || "";
            if (date) { ans += `\n### Date\n- ${date}`; }
            const description: string = getPvsTag(PvsTag.desc, fileContent) || getPvsTag(PvsTag.description, fileContent) || "";
            if (description) { ans += `\n### Description\n- ${description}`; }
            return ans;
        } else if (desc?.inputFolder) {
            // it's a library
            const libName: string = fsUtils.getContextFolderName(desc.inputFolder);
            return `# Library ${libName}`;
        }
        return null;
    }
    /**
     * Executes the jsdoc script
     */
    protected async execJsDoc (): Promise<boolean> {
        const inputFile: string = this.inputFile ? path.resolve(this.inputFile) : null;
        const inputFolder: string = path.resolve(this.inputFolder);
        const outputFolder: string = path.resolve(this.outputFolder);
        let readmeFile: string = path.resolve(inputFolder, this.readmeFile);
        const readmeFileExists: boolean = fsUtils.fileExists(readmeFile);
        if (!readmeFileExists) {
            readmeFile = path.resolve(this.getTempFolder(this.inputFile ? this.inputFile : this.inputFolder), "README.md");
            // create a predefined readme file containing basic information about the theory or library name
            console.log(`[pvs2html] Generating default README.md file`);
            const content: string = await this.getDefaultReadmeFileContent ({ inputFile, inputFolder });
            if (content) {
                await fsUtils.writeFile(readmeFile, content);
            }
        }
        const opt_readme: string =`--readme ${readmeFile}`; // uses the indicated readme file to create the content of the index page
        const opt_recurse: string = inputFile ? "" : "--recurse"; // look into all subfolders if a specific input file is not specified
        const jsdocInput: string = inputFile ? 
            path.resolve(this.getTempFolder(inputFile), `${fsUtils.getFileName(inputFile, { keepExtension: false })}.js`)
                : path.resolve(this.getTempFolder(inputFolder), "*.js")
        const jsdoc: string = `node ${this.jsdocExecutable} -d ${outputFolder} ${opt_readme} ${opt_recurse} ${jsdocInput}`;
        console.log("------------------------");
        console.log(`[pvs-doc] Executing script: ${jsdoc}`);
        const success: boolean = await new Promise ((resolve, reject) => {
            exec(jsdoc, { cwd: inputFolder, encoding: "utf8" }, (error: ExecException | null, stdout: string, stderr: string) => {
                if (error) {
                    console.error(error);
                    return resolve(false);
                }
                resolve(true);
            });    
        })
        console.log("------------------------");

        if (success) {
            // remove temporary files
            const tempFolder: string = path.resolve(this.inputFolder, "temp");
            console.log(`[pvs-doc] Temp folder: ${tempFolder}`);
            // if (tempFolder) {
            //     console.log(`[pvs-doc] Removing temporary files in ${tempFolder}`);
            //     execSync(`rm -r ${tempFolder}`, { encoding: "utf8" });
            // }
        }
        return success;
    }
    /**
     * Performs pre-processing for all pvs files in the context folder
     */
    protected async preProcess (): Promise<boolean> {
        let success: boolean = true;
        const inputFolder: string = path.resolve(this.inputFolder);
        console.log(`[pvs-doc] Preprocessing folder ${inputFolder}`);
        const lst: FileList | null = await this.listFiles(inputFolder, ".pvs");
        if (lst?.fileNames?.length) {
            for (let i = 0; i < lst.fileNames.length; i++) {
                success = success && await this.preProcessFile(path.resolve(lst.contextFolder, lst.fileNames[i]));
            }
        }
        console.log(`[pvs-doc] Done with preprocessing!`);
        return success;
    }
    /**
     * Utility function, returns the temporary folder where files to be processed by jsdoc are stored
     */
    protected getTempFolder (fname: string): string {
        const contextFolder: string = fsUtils.getContextFolder(fname);
        return path.resolve(contextFolder, "temp");
    }
    /**
     * Performs pre-processing for a given pvs file
     */    
    protected async preProcessFile (ifname: string): Promise<boolean> {
        console.log(`[pvs-doc] Preprocessing file ${ifname}`);
        const content: string = await fsUtils.readFile(ifname);
        let dummyContent: string = "";
        let info: string = "";
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
                const regexp: RegExp = new RegExp(`\/\/\\s*%+\\s*${this.tags[i].pvs}\\b`, "g");
                dummyContent = dummyContent.replace(regexp, `* ${this.tags[i].jsdoc}`);
            }
            // remove any line that is not a tag
            dummyContent = dummyContent.replace(/\/\/.*/g, "//");
            // check if @yields QED tags are associated with a @theorem tag, if not, add the @theorem tag
            // group 1 is the theorem name, group 2 are comments
            const regexQED: RegExp[] = [
                new RegExp(`${PvsTag.yields}\\s+QED\\s+([^\\s]+)(.*)`, "g"),
                new RegExp(`${PvsTag.QED}\\s+([^\\s]+)(.*)`, "g"),
            ];
            let match: RegExpMatchArray = null;
            for (let i = 0; i < regexQED.length; i++) {
                const regex: RegExp = regexQED[i];
                while (match = regex.exec(dummyContent)) {
                    const theoremName: string = match[1];
                    // check that the theorem name is valid, i.e., it starts with a letter
                    if (theoremName && /\w/.test(theoremName[0])) {
                        const regexTheorem: RegExp = new RegExp(`${PvsTag.theorem}\\s+${theoremName}\\b`, "g");
                        const regexMajorTheorem: RegExp = new RegExp(`${PvsTag.majortheorem}\\s+${theoremName}\\b`, "g");
                        // if @theorem tag is not present, add it before @yields QED
                        if (!regexTheorem.test(dummyContent) && !regexMajorTheorem.test(dummyContent)) {
                            dummyContent = dummyContent.replace(match[0], `@event ${theoremName}`);
                        }
                    }
                }
            }
            // create comment marker start '/**'
            dummyContent = dummyContent.replace(/(\/\/\s*)(\*\s*\@)/g, "$1/*$2");
            if (dummyContent.trimStart().startsWith("*")) { dummyContent = "/*" + dummyContent; }
            // create comment marker end '*/'
            dummyContent = dummyContent.replace(/(\*\s*\@.*)(\n\/\/)/g, "$1 */ $2");
            if (!dummyContent.trimEnd().endsWith("*/")) { dummyContent += "*/"; }
        }
        const fileName: string = fsUtils.getFileName(ifname);
        const tempFolder: string = this.getTempFolder(ifname);
        const ofname: string = path.resolve(tempFolder, `${fileName}.js`);
        const readmeFile: string = path.resolve(tempFolder, this.readmeFile);
        console.log(`[pvs-doc] Writing file ${ofname}`);
        await fsUtils.createFolder(tempFolder);
        let success: boolean = await fsUtils.writeFile(ofname, dummyContent);
        success = success && await fsUtils.writeFile(readmeFile, info);
        return success;
    }
    /**
     * Performs post-processing for all files generated by jsdoc
     */
    protected async postProcess (): Promise<boolean> {
        let success: boolean = true;
        const outputFolder: string = path.resolve(this.outputFolder);
        console.log(`[pvs-doc] Postprocessing folder ${outputFolder}`);
        const fileList: FileList | null = await this.listFiles(outputFolder, ".html");
        if (fileList && fileList.fileNames && fileList.fileNames.length) {
            
            // first process index.html
            const index: string = path.resolve(outputFolder, "index.html");
            console.log(`[pvs-doc] Postprocessing file ${index}`);
            let content: string = await fsUtils.readFile(index);
            content = content.replace(`<h1 class="page-title">Home</h1>`, "");
            content = content.replace(/<h3>\s*<\/h3>/, "");
            const matchLibName: RegExpMatchArray | null = /<article><h1>(.*)<\/h1>/g.exec(content);
            if (matchLibName && matchLibName.length > 1 && matchLibName[1]) {
                content = content.replace(/Home<\//g, `${matchLibName[1]}</`);
            }
            success = success && await fsUtils.writeFile(index, content);

            // then, process the entire folder
            for (let i = 0; i < fileList.fileNames.length; i++) {
                const ofname: string = path.resolve(outputFolder, fileList.fileNames[i]);
                // console.log(`[pvs-doc] Postprocessing file ${ofname}`);
                let content: string = await fsUtils.readFile(ofname);
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
                    const fileName: string = fsUtils.getFileName(ofname, { extension: ".js.html" });
                    const pvsFile: string = path.resolve(this.inputFolder, `${fileName}.pvs`);
                    // console.log("original pvs file:", pvsFile);
                    let theory: string = await fsUtils.readFile(pvsFile);
                    // console.log(theory);
                    content = content.replace(/<code>[\w\W\s]+<\/code>/, "<code>" + theory + "<\/code>");
                    success = success && await fsUtils.writeFile(ofname.replace(".js.html", ".pvs.html"), content);
                    // await fsUtils.deleteFile(ofname);
                } else {
                    success = success && await fsUtils.writeFile(ofname, content);
                }
            }
        }
        console.log(`[pvs-doc] Done with postprocessing folder ${outputFolder}`);
        return success;
    }

    /**
     * Utility function, returns the list of files in a given folder
     */
    protected async listFiles (folder: string, extension: string): Promise<FileList | null> {
        if (folder) {
            const children: string[] = await fsUtils.readDir(folder);
            const fileList: FileList = {
                fileNames: children.filter((fileName) => {
                    return fileName.endsWith(extension) 
                        && !fileName.startsWith("."); // this second part is necessary to filter out temporary files created by pvs
                }),
                contextFolder: folder
            };
            return fileList;
        }
        return null;
    }

}
