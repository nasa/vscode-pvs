import { execSync } from 'child_process';
import * as path from 'path';
import * as fsUtils from './common/fsUtils';
import { FileList } from './common/serverInterface'

export class PrettifyPvs {
    // protected inputFile: string = "";
    protected contextFolder: string = __dirname;
    protected inputFolder: string = ""; // path relative to contextFolder
    protected outputFolder: string = "out"; // path relative to contextFolder
    protected readmeFile: string = "README.md"; // path relative to contextFolder+inputFolder
    readonly helpMessage: string = `prettify-pvs: documentation generator for PVS specification files
Usage:
    prettify-pvs <pvs-file>

Options:
    -out <folder> (folder where the documentation will be generated, default is <current folder>/out )
    -ctx <folder> (folder containing the input files, default is <current folder>)
    -help         (shows this help message)
`;

    readonly tags: { pvs: string, jsdoc: string }[] = [
        { pvs: "@theory", jsdoc: "@module" },
        { pvs: "@module", jsdoc: "@module" },
        { pvs: "@description", jsdoc: "@description" },
        { pvs: "@desc", jsdoc: "@desc" },
        { pvs: "@date", jsdoc: "@date" },
        { pvs: "@author", jsdoc: "@author" },
        { pvs: "@authors", jsdoc: "@author" },
        { pvs: "@contributors", jsdoc: "@author" },
        { pvs: "@stats", jsdoc: "@stats" },
        { pvs: "@QED", jsdoc: "@yields QED" },
        { pvs: "@qed", jsdoc: "@yields QED" },
        { pvs: "@yields", jsdoc: "@yields" },
        { pvs: "@const", jsdoc: "@const" },
        { pvs: "@datatype", jsdoc: "@typedef" },
        { pvs: "@majortheorem", jsdoc: "@class" },
        { pvs: "@theorem", jsdoc: "@event" },
        { pvs: "@lemma", jsdoc: "@event" },
        { pvs: "@function", jsdoc: "@member" },
        { pvs: "@param", jsdoc: "@param" },
        { pvs: "@returns", jsdoc: "@returns" }
    ];

    /**
     * main API
     */
    async run (args: string[]): Promise<void> {
        this.parseCliArgs(args);

        await this.preProcess();
        await this.execJsDoc();
        await this.postProcess();
        await this.copyImages();

        console.log("------------------------");
        console.log(`Documentation files ready at ${path.join(this.contextFolder, this.outputFolder, "index.html")}`);
        console.log("------------------------");
    }


    /**
     * Copies images to the output folder
     */
    protected async copyImages (): Promise<void> {
        const imgInputFolder: string = path.join(this.contextFolder, this.inputFolder, "img");
        const imgOutputFolder: string = path.join(this.contextFolder, this.outputFolder, "img");
        try {
            const cmd: string = `cp -R ${imgInputFolder} ${imgOutputFolder}`;
            // console.log(cmd);
            execSync(`${cmd}`);
        } catch (error) {
            // do nothing
        }
    }
    /**
     * Executes the jsdoc script
     */
    protected async execJsDoc (): Promise<void> {
        const inputFolder: string = path.join(this.contextFolder, this.inputFolder);
        const outputFolder: string = path.join(this.contextFolder, this.outputFolder);
        const jsdoc: string = 
            `node node_modules/jsdoc/jsdoc.js -d ${outputFolder} -R ${path.join(inputFolder, this.readmeFile)} ${path.join(inputFolder, "temp", "*.js")}`;
        console.log("------------------------");
        console.log(`[prettify-pvs] Executing script \n${jsdoc}`);
        execSync(jsdoc);
        console.log("------------------------");

        // removing temporary files
        const tempFolder: string = path.join(this.contextFolder, this.inputFolder, "temp");
        if (tempFolder) {
            console.log(`[prettyprint-pvs] Removing temporary files in ${tempFolder}`);
            execSync(`rm -r ${tempFolder}`);
        }
    }

    /**
     * Performs pre-processing for all pvs files in the context folder
     */
    protected async preProcess (): Promise<void> {
        const inputFolder: string = path.join(this.contextFolder, this.inputFolder);
        console.log(`[prettyfy-pvs] Preprocessing folder ${inputFolder}`);
        const lst: FileList = await this.listFiles(inputFolder, ".pvs");
        if (lst && lst.fileNames && lst.fileNames.length) {
            for (let i = 0; i < lst.fileNames.length; i++) {
                await this.preProcessFile(path.join(lst.contextFolder, lst.fileNames[i]));
            }
        }
    }
    /**
     * Performs pre-processing for a given pvs file
     */    
    protected async preProcessFile (ifname: string): Promise<void> {
        console.log(`[prettyfy-pvs] Preprocessing file ${ifname}`);
        const content: string = await fsUtils.readFile(ifname);
        let dummyContent: string = "";
        let info: string = "";
        if (content) {
            // get comments in the header and use them to describe the file
            info = content.substring(0, content.indexOf("THEORY") || content.indexOf("theory"));
            info = info.substring(0, info.lastIndexOf("%"));
            info = info.substring(0, info.indexOf("@theory"));
            info = info.replace(/%/g, "");
            // console.log(info);
            // comment everything
            dummyContent = content.split("\n").map(line => { return `//${line}`; }).join("\n");
            // pull out tags
            for (let i = 0; i < this.tags.length; i++) {
                const regexp: RegExp = new RegExp(`\/\/\\s*%+\\s*${this.tags[i].pvs}\\b`, "g");
                dummyContent = dummyContent.replace(regexp, `* ${this.tags[i].jsdoc}`);
            }
            // preserve multiline comments
            dummyContent = dummyContent.replace(/\/\/\s*%+/g, `* `);
            // comment out other lines
            dummyContent = dummyContent.replace(/\/\/.*/g, "//");
            // create comment marker start /**
            dummyContent = dummyContent.replace(/\/\/\s+\*/g, "/**\n*");
            // fix beginning of file
            if (dummyContent.startsWith("*")) {
                dummyContent = dummyContent.replace("*", "/**");
            }
            // create comment marker end */
            dummyContent = dummyContent.replace(/\*\s+\/\//g, "*\n\*\/\/\/");
            dummyContent = dummyContent.replace(/\/\//g, "");
            // fix tail-head connections between comments
            dummyContent = dummyContent.replace(/\n\n/g, "\n/**/\n");
            // fix end of file
            dummyContent = dummyContent + "\n/**/";
        }
        const fileName: string = fsUtils.getFileName(ifname);
        const contextFolder: string = fsUtils.getContextFolder(ifname);
        const tempFolder: string = path.join(contextFolder, "temp");
        const ofname: string = path.join(tempFolder, `${fileName}.js`);
        console.log(`[prettify-pvs] Writing file ${ofname}`);
        await fsUtils.createFolder(tempFolder);
        await fsUtils.writeFile(ofname, dummyContent);
    }
    /**
     * Performs post-processing for all files generated by jsdoc
     */
    protected async postProcess (): Promise<void> {
        const outputFolder: string = path.join(this.contextFolder, this.outputFolder);
        console.log(`[prettyfy-pvs] Postprocessing folder ${outputFolder}`);
        const fileList: FileList = await this.listFiles(outputFolder, ".html");
        if (fileList && fileList.fileNames && fileList.fileNames.length) {
            
            // first process index.html
            const index: string = path.join(outputFolder, "index.html");
            console.log(`[prettyfy-pvs] Postprocessing file ${index}`);
            let content: string = await fsUtils.readFile(index);
            content = content.replace(`<h1 class="page-title">Home</h1>`, "");
            content = content.replace(/<h3>\s*<\/h3>/, "");
            const matchLibName: RegExpMatchArray = /<article><h1>(.*)<\/h1>/g.exec(content);
            if (matchLibName && matchLibName.length > 1 && matchLibName[1]) {
                content = content.replace(/Home<\//g, `${matchLibName[1]}</`);
            }
            await fsUtils.writeFile(index, content);

            // then, process the entire folder
            for (let i = 0; i < fileList.fileNames.length; i++) {
                const ofname: string = path.join(outputFolder, fileList.fileNames[i]);
                console.log(`[prettyfy-pvs] Postprocessing file ${ofname}`);
                let content: string = await fsUtils.readFile(ofname);
                // content = content.replace(/\s\s+/g, " ");
                content = content.replace(/<span class=\"type\-signature\">\(inner\) <\/span>/g, "");
                content = content.replace(/<h3>Modules<\/h3>/g, "<h3>Theories</h3>");
                content = content.replace(/<h3>\s*\/?Classes<\/h3>/g, "<h3>Major Theorems</h3>");
                content = content.replace(/<h3>\s*\/?Events<\/h3>/g, "<h3>Theorems</h3>");
                content = content.replace(/<h3>\s*\/?Members<\/h3>/g, "<h3>Definitions</h3>");
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
                content = content.replace(/Documentation generated by /g, `Documentation generated by <a href="https://github.com/nasa/vscode-pvs">VSCode-PVS</a> through `);
                if (ofname.endsWith(".js.html")) {
                    // replace code section with actual pvs spec
                    const pvsFile: string = path.join(this.contextFolder, this.inputFolder, `${fsUtils.getFileName(ofname.split(".js")[0])}.pvs`);
                    let theory: string = await fsUtils.readFile(pvsFile);
                    content = content.replace(/<code>[\w\W\s]+<\/code>/, "<code>" + theory + "<\/code>");
                    await fsUtils.writeFile(ofname.replace(".js.html", ".pvs.html"), content);
                    await fsUtils.deleteFile(ofname);
                } else {
                    await fsUtils.writeFile(ofname, content);
                }
            }
        }
    }

    /**
     * Utility function used to process command line interface arguments
     * @param args 
     */
    protected parseCliArgs (args: string[]): void {
        console.log("------------------------");
        if (args && args.length) {
            for (let i = 0; i < args.length; i++) {
                if (args[i] === "-out" || args[i] === "--out" || args[i] === "-output" || args[i] === "--output") {
                    if (i + 1 < args.length) {
                        i++;
                        this.outputFolder = args[i];
                        console.log(`** Output folder: ${this.outputFolder}`);
                    } else {
                        console.warn(`[pvs-doc] Warning: ${args[i]} option provided without indicating output folder. Using default output folder ${path.join(__dirname, this.outputFolder)}`);
                    }
                } else if (args[i] === "-ctx" || args[i] === "--ctx" || args[i] === "-context" || args[i] === "--context") {
                    if (i + 1 < args.length) {
                        i++;
                        this.contextFolder = args[i];
                        console.log(`** Context folder: ${this.contextFolder}`);
                    } else {
                        console.warn(`[pvs-doc] Warning: ${args[i]} option provided without indicating context folder. Using default context folder ${path.join(__dirname, this.contextFolder)}`);
                    }
                } else if (args[i] === "-readme" || args[i] === "--readme") {
                    if (i + 1 < args.length) {
                        i++;
                        this.readmeFile = args[i];
                        console.log(`** Readme file: ${this.readmeFile}`);
                    } else {
                        console.warn(`[pvs-doc] Warning: ${args[i]} option provided without indicating the README.md file.`);
                    }
                } else if (args[i] === "-help" || args[i] === "--help") {
                    console.log(this.helpMessage);
                } else {
                    this.inputFolder = args[i];
                    console.log(`** Location of input files: ${this.inputFolder}`);
                }
            }
        } else {
            console.log(this.helpMessage);
        }
        console.log("------------------------");
    }

    protected async listFiles (folder: string, extension: string): Promise<FileList> {
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

const worker: PrettifyPvs = new PrettifyPvs();
console.log(process.argv)
worker.run(process.argv.slice(2)); // the first two args are 'node' and 'prettify-pvs.js'

