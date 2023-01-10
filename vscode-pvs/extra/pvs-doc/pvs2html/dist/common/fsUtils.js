"use strict";
/**
 * @module fsUtils
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
exports.getOs = exports.getSourceControl = exports.getDownloader = exports.logFileExtension = exports.decodeURIComponents = exports.listSubFolders = exports.getCommandOpenWithExternalApp = exports.desc2fname = exports.fname2desc = exports.shasumFile = exports.shasum = exports.get_fresh_id = exports.getText = exports.normalizePath = exports.pathExists = exports.folderExists = exports.dirExists = exports.fileExists = exports.isSvgFile = exports.isMarkdownFile = exports.isAdobePdfFile = exports.isImageFile = exports.isProofliteFile = exports.isSummaryFile = exports.isDumpFile = exports.isPvsFile = exports.getContextFolderName = exports.getContextFolder = exports.normalizeContextFolder = exports.getFileExtension = exports.moveFolder = exports.removeFileExtension = exports.getFileName = exports.renameFile = exports.writeFile = exports.createFolder = exports.cleanBin = exports.cleanTccs = exports.deleteBinFiles = exports.deleteFolder = exports.deleteFile = exports.copyFile = exports.isSameFile = exports.readFile = exports.readDir = exports.stat = exports.tildeExpansion = exports.DUMP_FILE_EXTENSION = exports.MAX_RECURSION = exports.HOME_DIR = void 0;
exports.commentaryToString = exports.PostTask = exports.getUndumpFolderName = exports.parseLsPvsVersions = exports.lsPvsVersions = exports.saveSummary = exports.containsSummary = exports.removeSummary = exports.appendSummary = exports.createPvsLibraryPath = exports.decodePvsLibraryPath = exports.makeTheorySummary = exports.makeWorkspaceSummary = exports.renameTheoryInProofFile = exports.renameFormulaInProofFile = exports.contextDescriptor2LookUpTable = exports.getFileDescriptor = exports.getContextDescriptor = exports.findProofObligation = exports.findFormulaName = exports.listTheorems = exports.getProofLitePosition = exports.getProofliteScript = exports.getProofliteFileName = exports.saveProoflite = exports.readProoflite = exports.containsProoflite = exports.updateProoflite = exports.readJprfProofFile = exports.getProofDate = exports.openSketchpad = exports.saveSketchpad = exports.saveProofDescriptor = exports.getProofDescriptor = exports.getProofStatus = exports.getActualProofStatus = exports.typesLookUpTable = exports.listTypesInFile = exports.listTheoremsInFile = exports.mapTheoriesInFile = exports.listTheoriesInFile = exports.listTheoryNames = exports.listTheories = exports.listPvsFiles = exports.findTheoryName = exports.execShellCommand = exports.pvsFolderName = exports.getDownloadCommand = exports.shellCommandToString = exports.cloneNASALibCommand = void 0;
exports.formatSequent = exports.sformulas2string = exports.formatHiddenFormulas = exports.formatPvsIoState = void 0;
const fs = require("fs");
const path = require("path");
const child_process_1 = require("child_process");
const crypto = require("crypto");
const serverInterface_1 = require("../common/serverInterface");
const languageUtils_1 = require("./languageUtils");
const child_process_2 = require("child_process");
const colorUtils_1 = require("./colorUtils");
// home dir of the current installation
exports.HOME_DIR = require('os').homedir();
exports.MAX_RECURSION = 4;
// dump file extension
exports.DUMP_FILE_EXTENSION = ".dmp";
/**
 * Utility function, expands the leading ~/ in fname with the home folder $HOME_DIR
 * and normalizes the path structure
 * This function should be used before invoking any nodeJS function from fs and path
 * because nodeJS does not understand ~/, it's a shell thing
 */
function tildeExpansion(pathName) {
    if (pathName) {
        if (pathName.startsWith("~/") || pathName === "~") {
            pathName = pathName.replace("~", exports.HOME_DIR);
        }
        return path.normalize(pathName);
    }
    return pathName;
}
exports.tildeExpansion = tildeExpansion;
/**
 * Utility function, returns stat info about a file or folder
 */
function stat(fname) {
    if (fname) {
        fname = tildeExpansion(fname);
        return new Promise((resolve, reject) => {
            fs.stat(fname, (error, stat) => {
                // ignore errors for now
                resolve(stat);
            });
        });
    }
    return null;
}
exports.stat = stat;
;
/**
 * Utility function, reads the content of a folder and returns the list of children contained in the folder
 */
function readDir(contextFolder) {
    if (contextFolder) {
        contextFolder = tildeExpansion(contextFolder);
        return new Promise((resolve, reject) => {
            fs.readdir(contextFolder, (error, children) => {
                // ignore errors for now
                resolve(children || []);
            });
        });
    }
    return null;
}
exports.readDir = readDir;
/**
 * Utility function, reads the content of a file
 * default encoding is utf8, a different encoding can be specified with the optional parameter
 */
function readFile(fname, opt) {
    return __awaiter(this, void 0, void 0, function* () {
        if (fname) {
            opt = opt || {};
            opt.encoding = opt.encoding || "utf8";
            fname = fname.replace("file://", "");
            fname = tildeExpansion(fname);
            try {
                const exists = yield fileExists(fname);
                if (exists) {
                    const data = fs.readFileSync(fname, { encoding: opt.encoding });
                    // if (data) {
                    // 	return data.toLocaleString();
                    // }
                    return data || "";
                }
            }
            catch (error) {
                console.error(`[fs-utils] Error while reading file ${fname}`, error);
                return "";
            }
        }
        return "";
    });
}
exports.readFile = readFile;
/**
 * Utility function, checks if two file names are identical
 * This function is designed to be protocol independent
 * e.g., file:///usr/myfile.pvs is the same of /usr/myfile.pvs
 * TODO: check if there is a better way to implement this function
 */
function isSameFile(fname1, fname2) {
    if (fname1 && fname2) {
        const f1 = fname1.replace("file://", "");
        const f2 = fname2.replace("file://", "");
        return f1 === f2;
    }
    return false;
}
exports.isSameFile = isSameFile;
/**
 * Copies a file to a target location
 */
function copyFile(fname, target_fname) {
    if (fname) {
        try {
            fname = fname.replace("file://", "");
            fname = tildeExpansion(fname);
            target_fname = tildeExpansion(target_fname);
            fs.copyFileSync(fname, target_fname);
            return true;
        }
        catch (error) {
            console.warn(`[fs-utils] Error: could not copy file`, { fname, target_fname, error });
            return false;
        }
    }
    return false;
}
exports.copyFile = copyFile;
/**
 * Utility function, deletes a given file
 */
function deleteFile(fname) {
    if (fname) {
        try {
            fname = fname.replace("file://", "");
            fname = tildeExpansion(fname);
            fs.unlinkSync(fname);
        }
        catch (deleteError) {
            return false;
        }
    }
    return true;
}
exports.deleteFile = deleteFile;
/**
 * Utility function, deletes a given folder and its content, including sub-folders
 */
function deleteFolder(contextFolder) {
    try {
        if (contextFolder) {
            contextFolder = tildeExpansion(contextFolder);
            if (fs.existsSync(contextFolder)) {
                (0, child_process_1.execSync)(`rm -r ${contextFolder}`);
            }
        }
    }
    catch (deleteError) {
        console.error(`[fs-utils] `, deleteError);
        return false;
    }
    return true;
}
exports.deleteFolder = deleteFolder;
/**
 * Utility function, deletes the .bin files generated by pvs in a given pvsbin folder
 */
function deleteBinFiles(binFolder, opt) {
    opt = opt || {};
    try {
        if (binFolder) {
            binFolder = tildeExpansion(binFolder);
            if (fs.existsSync(binFolder)) {
                if (opt.removePvsbinFolder) {
                    deleteFolder(binFolder);
                }
                else {
                    (0, child_process_1.execSync)(`rm ${binFolder}/*.bin 2> /dev/null`); // 2> /dev/null suppresses 'file not found' messages
                }
            }
        }
    }
    catch (deleteError) {
        return false;
    }
    return true;
}
exports.deleteBinFiles = deleteBinFiles;
/**
 * Utility function, removes all .tccs files in a given folder
 * Subfolders are also processed when the optional parameter recursive is true
 */
function cleanTccs(contextFolder, opt) {
    return __awaiter(this, void 0, void 0, function* () {
        opt = opt || {};
        let nCleaned = 0;
        try {
            if (contextFolder) {
                contextFolder = tildeExpansion(contextFolder);
                // console.log(`reading folder ${contextFolder}`);
                const files = fs.readdirSync(contextFolder);
                if (files) {
                    // console.log(files);
                    // remove .tccs files
                    files.filter(name => {
                        // console.log(name);
                        return name.endsWith(".tccs");
                    }).forEach(file => {
                        // console.log(`deleting ${file}`);
                        deleteFile(path.join(contextFolder, file));
                    });
                    nCleaned++;
                    // repeat recursively to subdirs -- do this only for one level
                    if (opt.recursive) {
                        opt.recursive--;
                        const dirs = fs.readdirSync(contextFolder);
                        for (let i = 0; i < dirs.length; i++) {
                            const dir = path.join(contextFolder, dirs[i]);
                            if (fs.lstatSync(dir).isDirectory()) {
                                nCleaned += yield cleanTccs(dir, opt);
                            }
                        }
                    }
                }
            }
        }
        catch (deleteError) {
            return Promise.resolve(nCleaned);
        }
        return Promise.resolve(nCleaned);
    });
}
exports.cleanTccs = cleanTccs;
/**
 * Utility function, deletes all cache files generated by pvs in a given context folder.
 * These include: pvsbin/*.bin, .pvscontext, .prlite, .log, temporary files with extension ending in ~ (e.g., test.prf~)
 * @param contextFolder
 * @param opt
 * @returns
 */
function cleanBin(contextFolder, opt) {
    return __awaiter(this, void 0, void 0, function* () {
        opt = opt || {};
        let nCleaned = 0;
        try {
            // console.log(`Deleting cache for context ${contextFolder}`);
            if (contextFolder) {
                contextFolder = tildeExpansion(contextFolder);
                // console.log(`Deleting cache for context ${contextFolder}`);
                const pvsbinFolder = path.join(contextFolder, "pvsbin");
                deleteBinFiles(pvsbinFolder, opt);
                // console.log(`removing ${path.join(contextFolder, ".pvscontext")}`);
                deleteFile(path.join(contextFolder, ".pvscontext"));
                // console.log(`reading folder ${contextFolder}`);
                const files = fs.readdirSync(contextFolder);
                if (files) {
                    // remove .prlite files
                    files.filter(name => {
                        // console.log(name);
                        return name.endsWith(".prlite") || name.endsWith(".log") || name.endsWith("~");
                    }).forEach(file => {
                        // console.log(`deleting ${file}`);
                        deleteFile(path.join(contextFolder, file));
                    });
                    // remove .tccs files
                    if (!opt.keepTccs) {
                        // console.log(files);
                        files.filter(name => {
                            // console.log(name);
                            return name.endsWith(".tccs");
                        }).forEach(file => {
                            // console.log(`deleting ${file}`);
                            deleteFile(path.join(contextFolder, file));
                        });
                    }
                    nCleaned++;
                    // repeat recursively to subdirs -- do this only for one level
                    if (opt.recursive) {
                        opt.recursive--;
                        const dirs = fs.readdirSync(contextFolder);
                        for (let i = 0; i < dirs.length; i++) {
                            const dir = path.join(contextFolder, dirs[i]);
                            if (fs.lstatSync(dir).isDirectory()) {
                                nCleaned += yield cleanBin(dir, opt);
                            }
                        }
                    }
                }
            }
        }
        catch (deleteError) {
            return Promise.resolve(nCleaned);
        }
        return Promise.resolve(nCleaned);
    });
}
exports.cleanBin = cleanBin;
/**
 * Creates a folder, if the folder does not exist already.
 */
function createFolder(path) {
    return __awaiter(this, void 0, void 0, function* () {
        if (!fs.existsSync(path)) {
            fs.mkdirSync(path, { recursive: true });
        }
    });
}
exports.createFolder = createFolder;
/**
 * Utility function, write a file to disk.
 * Returns true if the operation is successful, false otherwise.
 * Default encoding is utf8
 * A different encoding can be specified with the optional parameter encoding
 * If the file exists, the file is overwritten by default
 * The content can be appended to an existing file with the optional parameter append
 */
function writeFile(fname, content, opt) {
    return __awaiter(this, void 0, void 0, function* () {
        opt = opt || {};
        opt.encoding = opt.encoding || "utf8";
        if (fname) {
            try {
                fname = fname.replace("file://", "");
                fname = tildeExpansion(fname);
                const contextFolder = getContextFolder(fname);
                yield createFolder(contextFolder);
                if (opt.append) {
                    const previousContent = yield readFile(fname);
                    content = previousContent + "\n\n" + content;
                    content = content.trim();
                }
                // writeFileSync does not synchronously write to the file system -- use open/write/fdatasync instead?
                fs.writeFileSync(fname, content, { encoding: opt.encoding });
                // const fd: number = fs.openSync(fname, "w+");
                // fs.writeSync(fd, content, null, opt.encoding);
                // fs.fdatasyncSync(fd)
            }
            catch (error) {
                console.error(`[fs-utils] Error while writing file ${fname}`, error);
                return false;
            }
        }
        return true;
    });
}
exports.writeFile = writeFile;
/**
 * Utility function, renames a file
 * TODO: use fsPromises.rename(oldPath, newPath), see https://nodejs.org/api/fs.html#fspromisesrenameoldpath-newpath
 */
function renameFile(old_fname, new_fname) {
    return __awaiter(this, void 0, void 0, function* () {
        if (old_fname && new_fname) {
            const content = (yield readFile(old_fname)) || "";
            let success = yield writeFile(new_fname, content);
            if (success) {
                success = yield deleteFile(old_fname);
            }
            return success;
        }
        return false;
    });
}
exports.renameFile = renameFile;
/**
 * Utility function, returns the last portion of a path fname
 * The file extension is removed by default
 * TODO: use path.posix.basename(fname), see https://nodejs.org/api/path.html
 */
function getFileName(fname, opt) {
    if (fname) {
        fname = fname.replace("file://", "");
        fname = fname.includes("/") ? fname.split("/").slice(-1)[0] : fname;
        if (!(opt === null || opt === void 0 ? void 0 : opt.keepExtension)) {
            const index = (opt === null || opt === void 0 ? void 0 : opt.extension) ? fname.lastIndexOf(opt.extension) : fname.lastIndexOf(".");
            fname = index > 0 ? fname.substring(0, index) : fname;
        }
        return fname;
    }
    return null;
}
exports.getFileName = getFileName;
/**
 * Utility function, returns the last portion of a path fname and without file extension
 * TODO: this should be equivalent to getFileName(fname, { keepExtension: false })
 */
function removeFileExtension(fname) {
    if (fname && fname.indexOf(".") >= 0) {
        return fname.split(".").slice(0, -1).join(".");
    }
    return null;
}
exports.removeFileExtension = removeFileExtension;
/**
 * Utility function, moves a given context folder to a given target folder
 */
function moveFolder(contextFolder, toFolder) {
    try {
        if (contextFolder && toFolder && fs.existsSync(contextFolder) && !fs.existsSync(toFolder)) {
            (0, child_process_1.execSync)(`mv ${contextFolder} ${toFolder}`);
            return true;
        }
    }
    catch (moveError) {
        return false;
    }
    return false;
}
exports.moveFolder = moveFolder;
/**
 * Utility function, returns the file extension
 * TODO: use path.extname(path), see https://nodejs.org/api/path.html
 */
function getFileExtension(fname) {
    if (fname) {
        const parts = fname.split("/");
        return (parts === null || parts === void 0 ? void 0 : parts.length) && parts[parts.length - 1].includes(".") ?
            `.${parts[parts.length - 1].split(".").slice(-1).join(".")}`
            : "";
    }
    return null;
}
exports.getFileExtension = getFileExtension;
/**
 * Utility function, normalizes the context folder name
 * TODO: check in which situations is necessary to remove file protocol header (file://)
 * and prefer path.normalize(path) if possible.
 * See https://nodejs.org/api/fs.html#fspromisesreaddirpath-options for info about the
 * file protocol and which nodeJS functions can be used to handle it in a transparent way
 */
function normalizeContextFolder(contextFolder) {
    if (contextFolder) {
        return contextFolder.replace("file://", "");
    }
    return null;
}
exports.normalizeContextFolder = normalizeContextFolder;
/**
 * Utility function, returns the path to a file fname
 * TODO: use path.dirname(path), see https://nodejs.org/api/path.html
 */
function getContextFolder(fname) {
    if (fname) {
        const ctx = fname.replace("file://", "");
        return ctx.split("/").slice(0, -1).join("/").replace("//", "/");
    }
    return null;
}
exports.getContextFolder = getContextFolder;
/**
 * Utility function, returns the name of the folder where fname is stored,
 * i.e., the last portion of the path to a given fname
 */
function getContextFolderName(contextFolder) {
    if (contextFolder) {
        const ctx = normalizeContextFolder(contextFolder);
        return ctx.substring(ctx.lastIndexOf('/') + 1, ctx.length);
    }
    return null;
}
exports.getContextFolderName = getContextFolderName;
/**
 * Utility function, returns true if the file extension indicates this is a pvs file
 */
function isPvsFile(desc) {
    if (desc) {
        const ext = (typeof desc === "string") ? desc : desc === null || desc === void 0 ? void 0 : desc.fileExtension;
        if (ext) {
            return ext.endsWith('.pvs') || ext.endsWith('.tccs') || ext.endsWith('.ppe') || ext.endsWith('.pr')
                || ext.endsWith('.hpvs') || ext.endsWith(".summary") || ext.endsWith(".prl");
        }
    }
    return false;
}
exports.isPvsFile = isPvsFile;
/**
 * Utility function, returns true if the file extension indicates this is a dump file
 */
function isDumpFile(desc) {
    if (desc) {
        const ext = (typeof desc === "string") ? desc : desc === null || desc === void 0 ? void 0 : desc.fileExtension;
        if (ext) {
            return ext.endsWith(".dmp");
        }
    }
    return false;
}
exports.isDumpFile = isDumpFile;
/**
 * Utility function, returns true if the file extension indicates this is a summary file
 */
function isSummaryFile(desc) {
    if (desc) {
        const ext = (typeof desc === "string") ? desc : desc === null || desc === void 0 ? void 0 : desc.fileExtension;
        if (ext) {
            return ext.endsWith(".summary");
        }
    }
    return false;
}
exports.isSummaryFile = isSummaryFile;
/**
 * Utility function, returns true if the file extension indicates this is a prooflite file
 */
function isProofliteFile(desc) {
    if (desc) {
        const ext = (typeof desc === "string") ? desc : desc === null || desc === void 0 ? void 0 : desc.fileExtension;
        if (ext) {
            return ext.endsWith(".prl") || ext.endsWith(".prlite") || ext.endsWith('.pr');
        }
    }
    return false;
}
exports.isProofliteFile = isProofliteFile;
/**
 * Utility function, returns true if the file extension indicates this is an image file (.jpg, .)
 */
function isImageFile(desc) {
    if (desc) {
        const ext = (typeof desc === "string") ? desc : desc === null || desc === void 0 ? void 0 : desc.fileExtension;
        if (ext) {
            return ext.endsWith(".jpg") || ext.endsWith(".jpeg") || ext.endsWith(".png") || ext.endsWith(".bmp") || ext.endsWith(".gif");
        }
    }
    return false;
}
exports.isImageFile = isImageFile;
/**
 * Utility function, returns true if the file extension indicates this is a pdf file (.pdf)
 */
function isAdobePdfFile(desc) {
    if (desc) {
        const ext = (typeof desc === "string") ? desc : desc === null || desc === void 0 ? void 0 : desc.fileExtension;
        if (ext) {
            return ext.endsWith(".pdf");
        }
    }
    return false;
}
exports.isAdobePdfFile = isAdobePdfFile;
/**
 * Utility function, returns true if the file extension indicates this is a markdown file
 */
function isMarkdownFile(desc) {
    if (desc) {
        const ext = (typeof desc === "string") ? desc : desc === null || desc === void 0 ? void 0 : desc.fileExtension;
        if (ext) {
            return ext.endsWith(".md");
        }
    }
    return false;
}
exports.isMarkdownFile = isMarkdownFile;
/**
 * Utility function, returns true if the file extension indicates this is an svg file
 */
function isSvgFile(desc) {
    if (desc) {
        const ext = (typeof desc === "string") ? desc : desc === null || desc === void 0 ? void 0 : desc.fileExtension;
        if (ext) {
            return ext.endsWith(".svg");
        }
    }
    return false;
}
exports.isSvgFile = isSvgFile;
/**
 * Utility function, returns true if a file exists
 */
function fileExists(fname) {
    return pathExists(fname);
}
exports.fileExists = fileExists;
/**
 * Utility function, returns true if a folder exists
 */
function dirExists(contextFolder) {
    return pathExists(contextFolder);
}
exports.dirExists = dirExists;
/**
 * Utility function, returns true if a folder exists (alias of dirExists)
 */
function folderExists(contextFolder) {
    return dirExists(contextFolder);
}
exports.folderExists = folderExists;
/**
 * Utility function, returns true if a path exists
 */
function pathExists(path) {
    if (path) {
        let ans = false;
        path = tildeExpansion(path);
        try {
            ans = fs.existsSync(path);
        }
        catch (readError) {
            // console.error(readError);
        }
        finally {
            return ans;
        }
    }
    return false;
}
exports.pathExists = pathExists;
/**
 * Utility function, normalizes the structure of a path -- expands ~ and terminates the path with /
 */
function normalizePath(p) {
    if (p) {
        p = path.normalize(p);
        p = (p.endsWith("/")) ? p.substring(0, p.length - 1) : p;
        p = tildeExpansion(p);
    }
    return p;
}
exports.normalizePath = normalizePath;
/**
 * Utility function, returns the fragment between range.start (included) and range.end (excluded)
 */
function getText(txt, range) {
    if (txt && range) {
        const lines = txt.split("\n");
        let ans = lines.slice(range.start.line, range.end.line + 1);
        if (ans && ans.length > 0) {
            if (!isNaN(range.start.character)) {
                ans[0] = ans[0].substring(range.start.character);
            }
            if (!isNaN(range.end.character)) {
                let endCharacter = range.end.character;
                if (!isNaN(range.start.character) && range.start.line === range.end.line) {
                    endCharacter -= range.start.character;
                }
                ans[ans.length - 1] = ans[ans.length - 1].substring(0, endCharacter);
            }
            return ans.join("\n");
        }
    }
    return txt;
}
exports.getText = getText;
/**
 * Utility function, returns a fresh identifier
 */
function get_fresh_id() {
    return shasum(Math.random().toString(36));
}
exports.get_fresh_id = get_fresh_id;
/**
 * Utility function, computes the shasum of a given text
 */
function shasum(txt) {
    if (txt) {
        const spaceless = txt.replace(/%.*/g, "").replace(/\s+/g, ""); // removes comments and white spaces in the pvs file
        return crypto.createHash('sha256').update(spaceless).digest('hex');
    }
    return "";
}
exports.shasum = shasum;
/**
 * Utility function, computes the shasum of a given file
 */
function shasumFile(desc) {
    return __awaiter(this, void 0, void 0, function* () {
        const content = yield readFile(desc2fname(desc));
        if (content) {
            return shasum(content);
        }
        return "";
    });
}
exports.shasumFile = shasumFile;
/**
 * Utility function, converts a file path into a file descriptor
 */
function fname2desc(fname) {
    if (fname) {
        const fileName = getFileName(fname);
        const fileExtension = getFileExtension(fname);
        const contextFolder = getContextFolder(fname);
        return { fileName, fileExtension, contextFolder };
    }
    return null;
}
exports.fname2desc = fname2desc;
/**
 * Utility function, converts a file descriptor into a file path
 */
function desc2fname(desc) {
    return path.join(desc.contextFolder, `${desc.fileName}${desc.fileExtension}`);
}
exports.desc2fname = desc2fname;
/**
 * Utility function, returns the command for opening a file with an external application
 */
function getCommandOpenWithExternalApp(desc) {
    const os = getOs();
    const fname = desc2fname(desc);
    return (os === null || os === void 0 ? void 0 : os.version) === "Linux" ? `xdg-open ${fname}`
        : (os === null || os === void 0 ? void 0 : os.version) === "MacOSX" ? `open ${fname}`
            : "";
}
exports.getCommandOpenWithExternalApp = getCommandOpenWithExternalApp;
/**
 * Utility function, returns a list of all subfolders contained in the given folder
 */
function listSubFolders(contextFolder) {
    const folder = tildeExpansion(contextFolder);
    if (folder && folderExists(folder)) {
        return fs.readdirSync(folder, { withFileTypes: true })
            .filter((dir) => { return dir.isDirectory(); })
            .map((dir) => { return dir.name; });
    }
    return null;
}
exports.listSubFolders = listSubFolders;
/**
 * Gets the unencoded version of an encoded components of a descriptor.
 */
function decodeURIComponents(desc) {
    if (desc) {
        if (typeof desc === "string") {
            return decodeURIComponent(desc);
        }
        // else
        const keys = ["fileName", "fileExtension", "contextFolder", "origin"];
        for (let i = 0; i < keys.length; i++) {
            if (desc[keys[i]] && typeof desc[keys[i]] === "string") {
                desc[keys[i]] = decodeURIComponent(desc[keys[i]]);
            }
        }
    }
    return desc;
}
exports.decodeURIComponents = decodeURIComponents;
/**
 * file extension for sequent files created under pvsbin/, to show a sequent in the editor
 */
exports.logFileExtension = ".pr";
/**
 * Utility function, checks if wget or curl are available
 */
function getDownloader() {
    const candidates = ["wget", "curl"];
    for (let i = 0; i < candidates.length; i++) {
        try {
            if ((0, child_process_1.execSync)(`which ${candidates[i]}`)) {
                return candidates[i];
            }
        }
        catch (error) {
            // keep going, command not found
        }
    }
    return null;
}
exports.getDownloader = getDownloader;
/**
 * Utility function, checks if git is available
 */
function getSourceControl() {
    try {
        if ((0, child_process_1.execSync)(`which git`)) {
            return "git";
        }
    }
    catch (error) {
        // command not found
    }
    return null;
}
exports.getSourceControl = getSourceControl;
/**
 * Utility function, detects the OS platform
 */
function getOs() {
    try {
        if (process.platform === 'linux' || process.platform === 'freebsd' || process.platform === 'openbsd' || process.platform === 'sunos' || process.platform === 'aix') {
            return { version: 'Linux' };
        }
        else if (process.platform === 'darwin') {
            return { version: 'MacOSX' };
        }
        return { version: process.platform };
    }
    catch (err) {
        const error = err.message + "Unable to detect OS version. This problem is likey due to missing dependency 'node' (please download node from https://nodejs.org/)";
        console.log(`[pvs-server] ${error}`);
        return { error };
    }
}
exports.getOs = getOs;
/**
 * Utility function, creates the 'git clone' / 'git pull' for nasalib
 */
function cloneNASALibCommand(url, opt) {
    const shellCommand = {
        cmd: "git",
        args: (opt === null || opt === void 0 ? void 0 : opt.update) ? ["pull"] : [`clone "${url}" "nasalib"`],
        cwd: (opt === null || opt === void 0 ? void 0 : opt.update) ? path.join(opt.basePath, "nasalib") : opt.basePath
    };
    if (opt.branch && !opt.update) {
        shellCommand.args.push(`-b "${opt.branch}"`);
    }
    return shellCommand;
}
exports.cloneNASALibCommand = cloneNASALibCommand;
/**
 * Utility function, converts a shell command into a string
 */
function shellCommandToString(shellCommand) {
    var _a;
    if (shellCommand) {
        const cmd = shellCommand.args ? `${shellCommand.cmd} ${(_a = shellCommand.args) === null || _a === void 0 ? void 0 : _a.join(" ")}`
            : shellCommand.cmd;
        return shellCommand.cwd ? `cd ${shellCommand.cwd} && ${cmd}` : cmd;
    }
    return "";
}
exports.shellCommandToString = shellCommandToString;
/**
 * Utility function, creates the command for downloading a file from a give URL with curl or wget
 */
function getDownloadCommand(url, opt) {
    const cmd = getDownloader();
    if (cmd) {
        let args = (cmd === "curl") ? ["-L"] // -L allows curl to follow URL redirect.
            : ["--progress=bar:force", "--show-progress"]; // wget automatically follows up to 20 URL redirect.
        args.push(url);
        if (opt === null || opt === void 0 ? void 0 : opt.out) {
            if (cmd === "curl") {
                args.push(`-o ${opt.out}`);
            }
            else {
                args.push(`-O ${opt.out}`);
            }
        }
        return { cmd, args };
    }
    return null;
}
exports.getDownloadCommand = getDownloadCommand;
/**
 * Default folder name for the installationn of pvs
 */
exports.pvsFolderName = "pvs-7.1.0";
/**
 * Utility function for executing commands in the shell
 */
function execShellCommand(req, opt) {
    var _a;
    if (req === null || req === void 0 ? void 0 : req.cmd) {
        const cmd = req.args ? `${req.cmd} ${(_a = req.args) === null || _a === void 0 ? void 0 : _a.join(" ")}` : req.cmd;
        const shellProcess = (0, child_process_1.exec)(cmd, { cwd: req === null || req === void 0 ? void 0 : req.cwd });
        // spawn(req.cmd, req.args || [], { cwd: req?.cwd });
        shellProcess.stdout.setEncoding("utf8");
        shellProcess.stderr.setEncoding("utf8");
        shellProcess.stdout.on("data", (data) => __awaiter(this, void 0, void 0, function* () {
            opt === null || opt === void 0 ? void 0 : opt.stdOut(data);
        }));
        shellProcess.stderr.on("data", (data) => {
            opt === null || opt === void 0 ? void 0 : opt.stdErr(data);
        });
        shellProcess.once("error", (err) => {
            opt === null || opt === void 0 ? void 0 : opt.stdErr(`Error: ${JSON.stringify(err, null, " ")}`);
            opt === null || opt === void 0 ? void 0 : opt.callback(false);
        });
        shellProcess.once("exit", (code, signal) => {
            // code === 0 means success
            opt === null || opt === void 0 ? void 0 : opt.callback(code === 0);
        });
        shellProcess.on("message", (message) => {
            console.log(message);
        });
        return shellProcess;
    }
    return null;
}
exports.execShellCommand = execShellCommand;
// export async function getNodeJsVersion (): Promise<{ version?: string, error?: string }> {
// 	const cmd: string = "node --version";
// 	try {
// 		const buf: Buffer = execSync(cmd);
// 		if (buf) {
// 			const info: string = buf.toLocaleString();
// 			console.log(`[pvs-server] ${cmd}\n `, info);
// 			const match: RegExpMatchArray = /(v?[\d\.]+)/g.exec(info);
// 			if (match && match.length > 1) {
// 				return { version: match[1] };
// 			} else {
// 				return { error: info };
// 			}
// 		} else {
// 			console.log("[pvs-server] Missing dependency: node (please download node from https://nodejs.org/)");
// 		}
// 	} catch (error) {
// 		console.log("[pvs-server]", error);
// 		return { error };
// 	}
// 	return null;
// }
/**
 * @function findTheoryName
 * @description Utility function, finds the name of the theory that includes the expression at a given line
 * @param fileContent The text where the theory should be searched
 * @param line The line in the document where search should end
 * @returns { string | null } The theory name if any is found, null otherwise
 */
function findTheoryName(fileContent, line) {
    if (fileContent) {
        let txt = fileContent.replace(languageUtils_1.commentRegexp, ""); // this removes all commented text
        const regexp = new RegExp(languageUtils_1.theoryRegexp);
        let candidates = [];
        // check that line number is not before keyword begin -- if so adjust line number otherwise regexp won't find theory name
        const matchFirstTheory = regexp.exec(txt);
        if (matchFirstTheory && matchFirstTheory.length > 1) {
            const theoryName = matchFirstTheory[1];
            const matchEnd = (0, languageUtils_1.endTheoryOrDatatypeRegexp)(theoryName).exec(txt);
            if (matchEnd && matchEnd.length) {
                regexp.lastIndex = matchEnd.index; // restart the search from here
                // const min: number = matchFirstTheory[0].split("\n").length;
                // line = (line < min) ? min : line;
                candidates.push(theoryName);
            }
        }
        // keep searching theory names -- the first element in candidates will be the closest to the current line number
        txt = txt.split("\n").slice(0, line).join("\n");
        // console.log({ line, txt: txt.split("\n") });
        let match = regexp.exec(txt);
        const maxIterations = 64;
        // while (match) {
        for (let i = 0; i < maxIterations && match; i++) {
            if (match.length > 1 && match[1]) {
                const theoryName = match[1];
                const matchEnd = (0, languageUtils_1.endTheoryOrDatatypeRegexp)(theoryName).exec(txt);
                if (matchEnd && matchEnd.length) {
                    // found theory end, the definition is not in this theory
                    const endIndex = matchEnd.index + matchEnd[0].length;
                    txt = txt.slice(endIndex);
                    // need to create a new regexp when txt is updated
                    match = new RegExp(regexp).exec(txt);
                    // candidates = [ theoryName ].concat(candidates);
                }
                else {
                    // theory end not found, this is a good candidate
                    candidates = [theoryName].concat(candidates);
                    match = regexp.exec(txt);
                }
                // console.log("match", { theoryName, candidates });
            }
            else {
                match = regexp.exec(txt);
            }
        }
        if (candidates.length > 0) {
            return candidates[0];
        }
    }
    return null;
}
exports.findTheoryName = findTheoryName;
;
/**
 * Utility function, returns the list of pvs files contained in a given folder
 * @param contextFolder Path to a folder containing pvs files.
 * @returs List of pvs files, as a structure FileList. Null if the folder does not exist.
 */
function listPvsFiles(contextFolder) {
    return __awaiter(this, void 0, void 0, function* () {
        const folder = tildeExpansion(contextFolder);
        if (folder) {
            const children = yield readDir(folder);
            const fileList = {
                fileNames: children.filter((fileName) => {
                    return (fileName.endsWith(".pvs") || fileName.endsWith(".hpvs"))
                        && !fileName.startsWith("."); // this second part is necessary to filter out temporary files created by pvs
                }),
                contextFolder: folder
            };
            return fileList;
        }
        return null;
    });
}
exports.listPvsFiles = listPvsFiles;
/**
 * Utility function, finds all theories in a given file
 * @param desc Descriptor indicating filename, file extension, context folder, and file content
 */
function listTheories(desc) {
    // console.log(`[language-utils] Listing theorems in file ${desc.fileName}${desc.fileExtension}`);
    let ans = [];
    if (desc && desc.fileContent) {
        let txt = desc.fileContent.replace(languageUtils_1.commentRegexp, "");
        // console.log(txt);
        const start = Date.now();
        const regexp = new RegExp(languageUtils_1.theoryOrDatatypeRegexp);
        // let lastIndex: number = 0;
        let match = new RegExp(regexp).exec(txt);
        let lineOffset = 0;
        while (match) {
            // console.log(`[language-utils] Found ${match[0]}`);
            if (match.length > 1 && match[1]) {
                const theoryName = match[1];
                const matchEnd = (0, languageUtils_1.endTheoryOrDatatypeRegexp)(theoryName).exec(txt);
                if (matchEnd && matchEnd.length) {
                    const endIndex = matchEnd.index + matchEnd[0].length;
                    const fullClip = txt.slice(0, endIndex);
                    const clipStart = txt.slice(0, match.index);
                    const lines = clipStart.split("\n");
                    const line = lines.length + lineOffset;
                    const character = 0;
                    txt = txt.slice(endIndex);
                    lineOffset += fullClip.split("\n").length - 1;
                    ans.push({
                        theoryName: desc.fileExtension === ".tccs" && theoryName.endsWith("_TCCS") ? theoryName.substr(0, theoryName.length - 5) : theoryName,
                        position: {
                            line: line,
                            character: character
                        },
                        fileName: desc.fileName,
                        fileExtension: desc.fileExtension,
                        contextFolder: desc.contextFolder
                    });
                    // need to create a new regexp when txt is updated
                    match = new RegExp(regexp).exec(txt);
                }
                else {
                    match = regexp.exec(txt);
                }
            }
            else {
                match = regexp.exec(txt);
            }
            // console.log(match);
        }
        const stats = Date.now() - start;
        // console.log(`[languageUtils] listTheories(${desc.fileName}) completed in ${stats}ms`);
    }
    return ans;
}
exports.listTheories = listTheories;
/**
 * @function listTheoryNames
 * @description Utility function, returns a list of all theories in the given file
 * @param fileContent The text where the theory should be searched
 * @returns string[] the list of theories in the given text
 */
function listTheoryNames(fileContent) {
    const ans = [];
    if (fileContent) {
        let txt = fileContent.replace(languageUtils_1.commentRegexp, "");
        const regexp = languageUtils_1.theoryRegexp;
        let match = new RegExp(regexp).exec(txt);
        while (match) {
            if (match.length > 1 && match[1]) {
                const theoryName = match[1];
                const matchEnd = (0, languageUtils_1.endTheoryOrDatatypeRegexp)(theoryName).exec(txt);
                if (matchEnd && matchEnd.length) {
                    const endIndex = matchEnd.index + matchEnd[0].length;
                    txt = txt.slice(endIndex);
                    // need to create a new regexp when txt is updated
                    match = new RegExp(regexp).exec(txt);
                    ans.push(theoryName);
                }
                else {
                    match = regexp.exec(txt);
                }
            }
            else {
                match = regexp.exec(txt);
            }
        }
    }
    return ans;
}
exports.listTheoryNames = listTheoryNames;
;
/**
 * Utility function, returns the list of theories defined in a given pvs file
 * @param fname Path to a pvs file
 */
function listTheoriesInFile(fname, opt) {
    return __awaiter(this, void 0, void 0, function* () {
        // console.log(`listing theories in file ${fname}`);
        opt = opt || {};
        if (fname) {
            const fileName = getFileName(fname);
            const fileExtension = getFileExtension(fname);
            const contextFolder = getContextFolder(fname);
            const fileContent = (opt.content) ? opt.content : yield readFile(fname);
            if (fileContent) {
                const response = listTheories({ fileName, fileExtension, contextFolder, fileContent });
                // console.dir(response);
                return response;
            }
        }
        return null;
    });
}
exports.listTheoriesInFile = listTheoriesInFile;
/**
 * Utility function, returns the list of theories defined in a given pvs file
 * @param fname Path to a pvs file
 */
function mapTheoriesInFile(fname) {
    return __awaiter(this, void 0, void 0, function* () {
        if (fname) {
            const fileName = getFileName(fname);
            const fileExtension = getFileExtension(fname);
            const contextFolder = getContextFolder(fname);
            const fileContent = yield readFile(fname);
            if (fileContent) {
                const response = listTheories({ fileName, fileExtension, contextFolder, fileContent });
                const theoryMap = {};
                for (const i in response) {
                    theoryMap[response[i].theoryName] = response[i];
                }
                return theoryMap;
            }
        }
        return null;
    });
}
exports.mapTheoriesInFile = mapTheoriesInFile;
/**
 * Utility function, returns the list of theories defined in a given pvs file
 * @param fname Path to a pvs file
 */
function listTheoremsInFile(fname, opt) {
    return __awaiter(this, void 0, void 0, function* () {
        opt = opt || {};
        if (fname) {
            const fileName = getFileName(fname);
            const fileExtension = getFileExtension(fname);
            const contextFolder = getContextFolder(fname);
            const fileContent = (opt.content) ? opt.content : yield readFile(fname);
            if (fileContent) {
                const ans = yield listTheorems({ fileName, fileExtension, contextFolder, fileContent });
                return ans;
            }
        }
        return null;
    });
}
exports.listTheoremsInFile = listTheoremsInFile;
;
/**
 * Utility function, returns the list of all type declarations in a given pvs file
 * This function is optimized for performance, and finds type declarations only if they are in the following forms:
 * - t1: TYPE
 * - t1[params]: TYPE
 * - t1,t2,..., t3[params],..: TYPE
 */
function listTypesInFile(fdesc) {
    var _a, _b;
    return __awaiter(this, void 0, void 0, function* () {
        if (isPvsFile(fdesc)) {
            const fname = desc2fname(fdesc);
            const fileContent = fdesc.fileContent || (yield readFile(fname));
            const content = fileContent.replace(languageUtils_1.commentRegexp, ""); // remove all comments
            const types = [];
            if (content) {
                const regex = new RegExp(languageUtils_1.typesRegexp);
                let match = null;
                while (match = regex.exec(content)) {
                    // group 0 is the list of type names
                    if (match[0]) {
                        const line = (_b = (_a = content.slice(0, match.index)) === null || _a === void 0 ? void 0 : _a.split("\n")) === null || _b === void 0 ? void 0 : _b.length;
                        if (line > 0) {
                            // candidates is raw information, names are split brutally at the commas
                            const candidates = match[0].split(",");
                            // names is the list of valid names
                            const names = candidates.filter(elem => {
                                const vrex = new RegExp(languageUtils_1.validTermRegExp);
                                const matchName = vrex.exec(elem);
                                return matchName && matchName[0].length === elem.replace(/:\s*TYPE/gi, "").trim().length;
                            }).map(elem => {
                                const vrex = new RegExp(languageUtils_1.validTermRegExp);
                                const matchName = vrex.exec(elem);
                                return matchName[0];
                            });
                            for (let i = 0; i < names.length; i++) {
                                const typeName = names[i];
                                const theoryName = findTheoryName(content, line);
                                types.push({
                                    contextFolder: fdesc.contextFolder,
                                    fileName: fdesc.fileName,
                                    fileExtension: fdesc.fileExtension,
                                    theoryName,
                                    typeName,
                                    line,
                                    character: 0
                                });
                            }
                        }
                    }
                }
            }
            return types;
        }
        return null;
    });
}
exports.listTypesInFile = listTypesInFile;
/**
 * Utility function, returns the lookup table of types for a given folder
 */
function typesLookUpTable(cdesc) {
    var _a;
    return __awaiter(this, void 0, void 0, function* () {
        // compile types
        if (cdesc) {
            const lookupTable = {};
            const files = yield listPvsFiles(cdesc.contextFolder);
            if ((_a = files === null || files === void 0 ? void 0 : files.fileNames) === null || _a === void 0 ? void 0 : _a.length) {
                for (let i = 0; i < files.fileNames.length; i++) {
                    const fname = files.fileNames[i];
                    const fileName = getFileName(fname);
                    const fileExtension = getFileExtension(fname);
                    const cdesc = {
                        contextFolder: files.contextFolder,
                        fileName,
                        fileExtension
                    };
                    const types = yield listTypesInFile(cdesc);
                    for (let t = 0; t < (types === null || types === void 0 ? void 0 : types.length); t++) {
                        const tp = types[t];
                        lookupTable[tp.typeName] = lookupTable[tp.typeName] || [];
                        lookupTable[tp.typeName].push({
                            contextFolder: tp.contextFolder,
                            fileName: tp.fileName,
                            fileExtension: tp.fileExtension,
                            theoryName: tp.theoryName,
                            line: tp.line,
                            character: tp.character
                        });
                    }
                }
            }
            // console.log(lookupTable);
            return lookupTable;
        }
        return null;
    });
}
exports.typesLookUpTable = typesLookUpTable;
/**
 * Utility function, changes the proof status from 'proved' ro 'unchecked' if the file content has changed
 */
function getActualProofStatus(desc, shasum) {
    var _a, _b;
    if (desc === null || desc === void 0 ? void 0 : desc.info) {
        if (shasum === desc.info.shasum) {
            return desc.info.status;
        }
        else {
            return (desc.info.status === "proved") ? "unchecked"
                : ((_b = (_a = desc === null || desc === void 0 ? void 0 : desc.proofTree) === null || _a === void 0 ? void 0 : _a.rules) === null || _b === void 0 ? void 0 : _b.length) ? "unfinished"
                    : "untried";
        }
    }
    return "untried";
}
exports.getActualProofStatus = getActualProofStatus;
/**
 * Utility function, returns the status of the proof for the theorem indicated in the fuction arguments
 */
function getProofStatus(desc) {
    return __awaiter(this, void 0, void 0, function* () {
        if (desc) {
            let status = "untried";
            // check if the .jprf file contains the proof status
            const jprf_file = desc2fname({
                fileName: desc.fileName,
                fileExtension: ".jprf",
                contextFolder: desc.contextFolder
            });
            const proofFile = yield readJprfProofFile(jprf_file);
            if (proofFile) {
                const proofDescriptors = proofFile[`${desc.theoryName}.${desc.formulaName}`];
                if (proofDescriptors && proofDescriptors.length && proofDescriptors[0] && proofDescriptors[0].info) {
                    if (desc.fileExtension === ".tccs") {
                        status = proofDescriptors[0].info.status; // for tccs we choose not to adjust the status because tccs are automatically generated by pvs and status of the formula is visible to the user as a comment -- we don't want to confuse them
                    }
                    else {
                        // compute shasum for the file, and check it with the shasum saved in the proof descriptor. If the two differ, then the file has changed and the proof status is not valid anymore
                        const shasum = yield shasumFile(desc);
                        status = getActualProofStatus(proofDescriptors[0], shasum);
                    }
                }
            }
            return status;
        }
        return null;
    });
}
exports.getProofStatus = getProofStatus;
/**
 * Utility function, returns the proof descriptor for a given formula
 * @param formula
 */
function getProofDescriptor(formula) {
    return __awaiter(this, void 0, void 0, function* () {
        if (formula) {
            // check if the .jprf file contains the proof status
            const jprf_file = desc2fname({
                fileName: formula.fileName,
                fileExtension: ".jprf",
                contextFolder: formula.contextFolder
            });
            const proofFile = yield readJprfProofFile(jprf_file);
            if (proofFile) {
                const proofDescriptors = proofFile[`${formula.theoryName}.${formula.formulaName}`];
                if (proofDescriptors && proofDescriptors.length) {
                    return proofDescriptors[0];
                }
            }
        }
        return null;
    });
}
exports.getProofDescriptor = getProofDescriptor;
/**
 * Utility function, updates the proof descriptor for a given formula
 * @param formula
 */
function saveProofDescriptor(formula, newDesc, opt) {
    return __awaiter(this, void 0, void 0, function* () {
        if (formula && newDesc) {
            opt = opt || {};
            const fname = desc2fname({
                fileName: formula.fileName,
                fileExtension: ".jprf",
                contextFolder: formula.contextFolder
            });
            const key = `${formula.theoryName}.${formula.formulaName}`;
            let fdesc = yield readJprfProofFile(fname);
            // check if file contains a proof for the given formula
            if (fdesc && fdesc[key] && fdesc[key].length) {
                // we are updating only the default proof, this might be changed in the future
                const pdesc = fdesc[key][0];
                if (pdesc.info) {
                    pdesc.info.shasum = newDesc.info.shasum ? newDesc.info.shasum : pdesc.info.shasum;
                    pdesc.info.prover = newDesc.info.prover ? newDesc.info.prover : pdesc.info.prover;
                    pdesc.info.status = newDesc.info.status ? newDesc.info.status : pdesc.info.status;
                    pdesc.info.date = newDesc.info.date ? newDesc.info.date : pdesc.info.date;
                }
                if (opt.saveProofTree) {
                    pdesc.proofTree = newDesc.proofTree ? newDesc.proofTree : pdesc.proofTree;
                }
            }
            else {
                fdesc[key] = fdesc[key] || [newDesc];
            }
            // write descriptor to file
            const newContent = JSON.stringify(fdesc, null, " ");
            return yield writeFile(fname, newContent);
        }
        return false;
    });
}
exports.saveProofDescriptor = saveProofDescriptor;
/**
 * Utility function, saves the sketchpad with proof clips for a given formula
 * @param formula
 */
function saveSketchpad(formula, clips) {
    return __awaiter(this, void 0, void 0, function* () {
        if (formula) {
            const fname = desc2fname({
                fileName: formula.fileName,
                fileExtension: ".jprf",
                contextFolder: formula.contextFolder
            });
            const key = `${formula.theoryName}.${formula.formulaName}`;
            let fdesc = yield readJprfProofFile(fname);
            // check if file contains a proof for the given formula
            if (fdesc && fdesc[key] && fdesc[key].length) {
                // update clips for default proof
                const pdesc = fdesc[key][0];
                pdesc.clips = clips || [];
                // write descriptor to file
                const newContent = JSON.stringify(fdesc, null, " ");
                return yield writeFile(fname, newContent);
            }
        }
        return false;
    });
}
exports.saveSketchpad = saveSketchpad;
/**
 * Utility function, opens the sketchpad with proof clips for a given formula
 * @param formula
 */
function openSketchpad(formula) {
    return __awaiter(this, void 0, void 0, function* () {
        if (formula) {
            const fname = desc2fname({
                fileName: formula.fileName,
                fileExtension: ".jprf",
                contextFolder: formula.contextFolder
            });
            const key = `${formula.theoryName}.${formula.formulaName}`;
            let fdesc = yield readJprfProofFile(fname);
            // check if file contains a proof for the given formula
            if (fdesc && fdesc[key] && fdesc[key].length) {
                // update clips for default proof
                const pdesc = fdesc[key][0];
                return pdesc.clips;
            }
        }
        return null;
    });
}
exports.openSketchpad = openSketchpad;
/**
 * Utility function, returns the date (day and time) a given proof was saved
 * @param desc
 */
function getProofDate(desc) {
    return __awaiter(this, void 0, void 0, function* () {
        if (desc) {
            // check if the .jprf file contains the date
            const jprf_file = desc2fname({
                fileName: desc.fileName,
                fileExtension: ".jprf",
                contextFolder: desc.contextFolder
            });
            const proofFile = yield readJprfProofFile(jprf_file);
            if (proofFile) {
                const proofDescriptors = proofFile[`${desc.theoryName}.${desc.formulaName}`];
                if (proofDescriptors && proofDescriptors.length && proofDescriptors[0] && proofDescriptors[0].info) {
                    return proofDescriptors[0].info.date;
                }
            }
        }
        return null;
    });
}
exports.getProofDate = getProofDate;
/**
 * Reads the content of a .jprf file
 * @param fname Name of the prooflite file
 * @param opt Optionals
 *               - quiet (boolean): if true, the function will not print any message to the console.
 */
function readJprfProofFile(fname, opt) {
    return __awaiter(this, void 0, void 0, function* () {
        opt = opt || {};
        let proofFile = {};
        fname = fname.replace("file://", "");
        fname = tildeExpansion(fname);
        const content = yield readFile(fname);
        if (content) {
            try {
                proofFile = JSON.parse(content);
            }
            catch (jsonError) {
                if (!opt.quiet) {
                    console.error(`[fs-utils] Error: Unable to parse proof file ${fname}`, jsonError.message);
                    console.error(`[fs-utils] Storing corrupted file content to ${fname}.err`);
                }
                // create a backup copy of the corrupted jprf file, because it might get over-written
                yield renameFile(fname, `${fname}.err`);
                yield writeFile(`${fname}.err.msg`, jsonError.message);
            }
            finally {
                return proofFile;
            }
        }
        return proofFile;
    });
}
exports.readJprfProofFile = readJprfProofFile;
// /**
//  * Utility function, appends a prooflite script at the end of a given file
//  * @param fname Name of the prooflite file
//  * @param script The prooflite script to be appended
//  */
// export async function appendProoflite (fname: string, script: string): Promise<boolean> {
// 	if (fname && script) {
// 		const content: string = await readFile(fname);
// 		const newContent: string = (content && content.trim()) ? content + `\n\n${script}` : script;
// 		return await writeFile(fname, newContent);
// 	}
// 	return false;
// }
/**
 * Utility function, removes a prooflite script from a given file
 * @param fname Name of the prooflite file
 * @param formulaName name of the prooflite script to be removed
 */
function updateProoflite(fname, formulaName, newProoflite) {
    return __awaiter(this, void 0, void 0, function* () {
        if (fname && formulaName) {
            fname = decodeURIComponents(fname);
            const success = yield fileExists(fname);
            if (!success) {
                writeFile(fname, "");
            }
            const content = yield readFile(fname);
            // group 1 is the header (this group can be null)
            // group 2 is the prooflite script
            const formula = (0, languageUtils_1.sanitizeForRegEx)(formulaName);
            const regex = new RegExp(`(%-*\\s%\\s*@formula\\s*:\\s*${formula}\\s[\\w\\W\\s]+%-*)?\\s*\\b(${formula}\\s*:\\s*PROOF\\b[\\s\\w\\W]+\\bQED\\b\\s*${formula}\\b\\s*)`, "g");
            if (regex.test(content)) {
                let newContent = newProoflite + "\n\n\n" + content.replace(regex, "").trim();
                // update content
                return yield writeFile(fname, newContent.trim());
            }
            else {
                const newContent = newProoflite + "\n\n\n" + content.trim();
                return yield writeFile(fname, newContent.trim());
            }
        }
        return false;
    });
}
exports.updateProoflite = updateProoflite;
/**
 * Utility function, checks if a prooflite script is present in a given file
 * @param fname Name of the prooflite file
 * @param formulaName name of the prooflite script
 */
function containsProoflite(fname, formulaName) {
    return __awaiter(this, void 0, void 0, function* () {
        if (fname && formulaName) {
            fname = decodeURIComponents(fname);
            const success = yield fileExists(fname);
            if (success) {
                const content = yield readFile(fname);
                if (content) {
                    // group 1 is the header (this group can be null)
                    // group 2 is the prooflite script
                    const formula = (0, languageUtils_1.sanitizeForRegEx)(formulaName);
                    const regex = new RegExp(`\\b(${formula}\\s*:\\s*PROOF\\b[\\s\\w\\W]+\\bQED\\b\\s*${formula}\\b\\s*)`, "g");
                    return regex.test(content);
                }
            }
        }
        return false;
    });
}
exports.containsProoflite = containsProoflite;
/**
 * Utility function, returns the prooflite script without tags
 * @param fname Name of the prooflite file
 * @param formulaName name of the prooflite script
 */
function readProoflite(fname, formulaName) {
    return __awaiter(this, void 0, void 0, function* () {
        if (fname && formulaName) {
            fname = decodeURIComponents(fname);
            const success = yield fileExists(fname);
            if (success) {
                const content = yield readFile(fname);
                if (content) {
                    // group 1 is the header (this group can be null)
                    // group 2 is the prooflite script (with tags)
                    // group 3 is the prooflite script (without tags)
                    const formula = (0, languageUtils_1.sanitizeForRegEx)(formulaName);
                    const regex = new RegExp(`\\s*\\b(${formula}\\s*:\\s*PROOF\\b([\\s\\w\\W]+)\\bQED\\b\\s*${formula}\\b\\s*)`, "g");
                    const match = regex.exec(content);
                    if (match && match.length > 2) {
                        return match[2].trim();
                    }
                }
            }
        }
        return null;
    });
}
exports.readProoflite = readProoflite;
/**
 * Utility function, saves a prooflite script for a given formula in the given file
 * @param fname Name of the prooflite file
 * @param formulaName name of the prooflite
 * @param script The prooflite script to be saved in the file
 */
function saveProoflite(fname, formulaName, script) {
    return __awaiter(this, void 0, void 0, function* () {
        if (fname && formulaName && script) {
            fname = decodeURIComponents(fname);
            return yield updateProoflite(fname, formulaName, script);
        }
        return false;
    });
}
exports.saveProoflite = saveProoflite;
/**
 * Utility function, returns the default prooflite file name and path
 */
function getProofliteFileName(formula, opt) {
    if ((formula === null || formula === void 0 ? void 0 : formula.contextFolder) && (formula === null || formula === void 0 ? void 0 : formula.theoryName)) {
        // by default, save under pvsbin
        opt = opt || {};
        const usePvsBinFolder = opt.usePvsBinFolder === undefined ? true : !!opt.usePvsBinFolder;
        return {
            contextFolder: usePvsBinFolder ? path.join(formula.contextFolder, "pvsbin") : formula.contextFolder,
            fileName: formula.theoryName,
            fileExtension: ".prl"
        };
    }
    console.warn("[fsUtils] Warning: getProofliteFileName received a malformed request", formula);
    return null;
}
exports.getProofliteFileName = getProofliteFileName;
/**
 * Utility function, returns the prooflite script for a given formula
 */
function getProofliteScript(desc) {
    return __awaiter(this, void 0, void 0, function* () {
        const fname = path.join(desc.contextFolder, `${desc.fileName}${desc.fileExtension}`);
        const txt = yield readFile(fname);
        const matchProoflite = (0, languageUtils_1.proofliteRegexp)(desc).exec(txt);
        return matchProoflite ? matchProoflite[0] : null;
    });
}
exports.getProofliteScript = getProofliteScript;
/**
 * Utility function, returns the line number (1-based) of a given prooflite script in the file
 */
function getProofLitePosition(desc) {
    return __awaiter(this, void 0, void 0, function* () {
        if (desc && desc.formula && desc.proofFile) {
            const fname = desc2fname(desc.proofFile);
            const txt = yield readFile(fname);
            const matchProoflite = (0, languageUtils_1.proofliteDeclRegexp)(desc.formula).exec(txt);
            if (matchProoflite) {
                const slice = txt.slice(0, matchProoflite.index);
                const lines = slice.split("\n");
                return lines.length;
            }
        }
        return 1;
    });
}
exports.getProofLitePosition = getProofLitePosition;
/**
 * Utility function, returns the list of theorems defined in a given pvs file
 * @param desc Descriptor indicating filename, file extension, context folder, file content, and whether the file in question is the prelude (flag prelude)
 */
function listTheorems(desc) {
    return __awaiter(this, void 0, void 0, function* () {
        if (desc && desc.fileContent) {
            const theories = (desc.cache && desc.cache.theories) ? desc.cache.theories : listTheories(desc);
            const boundaries = []; // slices txt to the boundaries of the theories
            if (theories) {
                // const start: number = Date.now();
                const fileContent = desc.fileContent.replace(languageUtils_1.commentRegexp, ""); // first, remove all comments
                const slices = fileContent.split("\n");
                for (let i = 0; i < theories.length; i++) {
                    boundaries.push({
                        theoryName: theories[i].theoryName,
                        from: theories[i].position.line,
                        to: (i + 1 < theories.length) ? theories[i + 1].position.line : slices.length
                    });
                }
                const formulaDescriptors = [];
                for (let i = 0; i < boundaries.length; i++) {
                    const content = slices.slice(boundaries[i].from - 1, boundaries[i].to - 1).join("\n");
                    if (content && content.trim()) {
                        const regex = new RegExp(languageUtils_1.theoremRegexp);
                        let match = null;
                        while (match = regex.exec(content)) {
                            // const startMatch: number = Date.now();
                            if (match.length > 1 && match[1]) {
                                const formulaName = match[1];
                                const matchParams = new RegExp(languageUtils_1.theoremParamsRegexp).exec(match[0].trim());
                                const blanks = match[0].replace(matchParams[0], "").replace(formulaName, "").length;
                                const docUp = content.slice(0, match.index + formulaName.length + blanks);
                                const offset = (docUp) ? docUp.split("\n").length : 0;
                                const line = boundaries[i].from + offset - 1;
                                const isTcc = desc.fileExtension === ".tccs";
                                let status = "untried";
                                // if (isTcc) {
                                // 	const matchStatus: RegExpMatchArray = tccStatusRegExp.exec(slice);
                                // 	if (matchStatus && matchStatus.length > 1 && matchStatus[1]) {
                                // 		status = <ProofStatus> matchStatus[1];
                                // 	}
                                // } else {
                                if (desc.prelude) {
                                    status = "proved";
                                }
                                else {
                                    // check if the .jprf file contains the proof status
                                    const theoryName = boundaries[i].theoryName;
                                    status = yield getProofStatus({
                                        fileName: desc.fileName,
                                        fileExtension: desc.fileExtension,
                                        contextFolder: desc.contextFolder,
                                        formulaName,
                                        theoryName
                                    });
                                }
                                const fdesc = {
                                    fileName: desc.fileName,
                                    fileExtension: desc.fileExtension,
                                    contextFolder: desc.contextFolder,
                                    theoryName: boundaries[i].theoryName,
                                    formulaName,
                                    position: { line, character: 0 },
                                    status,
                                    isTcc
                                };
                                formulaDescriptors.push(fdesc);
                            }
                            // const matchTime: number = Date.now() - startMatch;
                            // console.log(`[languageUtils] listTheorems(${desc.fileName}${desc.fileExtension}.${match[0]?.trim()}) completed in ${matchTime}ms`);			
                        }
                    }
                    else {
                        console.error("Error while finding theory names :/");
                    }
                }
                // const time: number = Date.now() - start;
                // console.log(`[languageUtils] listTheorems(${desc.fileName}${desc.fileExtension}) completed in ${time}ms`);
                return formulaDescriptors;
            }
        }
        return [];
    });
}
exports.listTheorems = listTheorems;
/**
 * @function findFormulaName
 * @description Utility function, finds the name of a theorem that immediately preceeds a given line
 * @param fileContent The text where the theory should be searched
 * @param line The line in the document where search should end
 * @returns { string | null } The theory name if any is found, null otherwise
 */
function findFormulaName(fileContent, line) {
    if (fileContent) {
        const txt = fileContent.replace(languageUtils_1.commentRegexp, "");
        let text = txt.split("\n").slice(0, line + 1).join("\n");
        let candidates = [];
        // (?:\%.*\s)* removes comments
        const regexp = languageUtils_1.formulaRegexp;
        let match = null;
        while (match = regexp.exec(text)) {
            if (match && match.length > 1 && match[1]) {
                candidates.push(match[1]);
            }
        }
        if (candidates.length > 0) {
            return candidates[candidates.length - 1];
        }
    }
    return null;
}
exports.findFormulaName = findFormulaName;
;
/**
 * @function findProofObligation
 * @description Utility function, finds the line of a proof obligation
 * @param txt The text where the proof obligation should be searched
 * @returns { string | null } The theory name if any is found, null otherwise
 */
function findProofObligation(formulaName, txt) {
    const formula = formulaName.replace("?", "\\?");
    const regexp = new RegExp(`\\b${formula}:\\s*OBLIGATION\\b`, "g");
    let match = regexp.exec(txt);
    if (match) {
        const trim = txt.substr(0, match.index);
        if (trim && trim.length > 0) {
            return trim.split("\n").length;
        }
    }
    return 0;
}
exports.findProofObligation = findProofObligation;
;
/**
 * Lists all theorems in a given context folder
 */
function getContextDescriptor(contextFolder, opt) {
    var _a;
    return __awaiter(this, void 0, void 0, function* () {
        // console.log(`[language-utils] Generating context descriptor for ${contextFolder}...`);
        const response = {
            fileDescriptors: {},
            contextFolder
        };
        const fileList = yield listPvsFiles(contextFolder);
        for (let i = 0; i < ((_a = fileList === null || fileList === void 0 ? void 0 : fileList.fileNames) === null || _a === void 0 ? void 0 : _a.length); i++) {
            const fname = path.join(contextFolder, fileList.fileNames[i]);
            // console.log(`[language-utils] Processing file ${fname}`);
            const desc = yield getFileDescriptor(fname, opt);
            response.fileDescriptors[fname] = desc;
        }
        // console.log("[language-utils] Done");
        return response;
    });
}
exports.getContextDescriptor = getContextDescriptor;
/**
 * Lists all theorems in a given file
 */
function getFileDescriptor(fname, opt) {
    return __awaiter(this, void 0, void 0, function* () {
        opt = opt || {};
        opt.listTheorems = (opt.listTheorems !== undefined) ? opt.listTheorems : true;
        const start = Date.now();
        const contextFolder = getContextFolder(fname);
        const fileName = getFileName(fname);
        const fileExtension = getFileExtension(fname);
        const response = {
            theories: [],
            contextFolder,
            fileName,
            fileExtension
        };
        const pvsFileContent = yield readFile(fname);
        const tccsFileContent = (opt.includeTccs) ? yield readFile(path.join(contextFolder, `${fileName}.tccs`)) : null;
        // console.log(`[languageUtils.getFileDescriptor] listTheories(${fileName})`);
        const theories = listTheories({ fileName, fileExtension, contextFolder, fileContent: pvsFileContent });
        if (theories) {
            // if (opt.listTheorems) { console.log(`[languageUtils.getFileDescriptor] listTheorems(${fileName})`);	}
            const lemmas = (opt.listTheorems)
                ? yield listTheorems({ fileName, fileExtension, contextFolder, fileContent: pvsFileContent, prelude: opt === null || opt === void 0 ? void 0 : opt.prelude, cache: { theories } })
                : [];
            const tccs = (opt.listTheorems && fileExtension !== ".tccs" && tccsFileContent)
                ? yield listTheorems({ fileName, fileExtension: ".tccs", contextFolder, fileContent: tccsFileContent, prelude: opt === null || opt === void 0 ? void 0 : opt.prelude })
                : [];
            const descriptors = lemmas.concat(tccs);
            // console.log(`[language-utils] Processing ${theories.length} theories`);
            for (let i = 0; i < theories.length; i++) {
                const theoryName = theories[i].theoryName;
                // console.log(`[language-utils] Processing theory ${theoryName}`);
                const position = theories[i].position;
                const theoryDescriptor = {
                    fileName, fileExtension, contextFolder, theoryName, position,
                    theorems: (descriptors && descriptors.length) ? descriptors.filter((desc) => {
                        return desc.theoryName === theoryName;
                    }) : []
                };
                // console.log(`[language-utils] Done`);
                response.theories.push(theoryDescriptor);
            }
        }
        // timing stats, collected for debugging purposes
        const stats = Date.now() - start;
        // console.log(`[languageUtils.getFileDescriptor] File descriptor for ${fname} created in ${stats}ms`);
        return response;
    });
}
exports.getFileDescriptor = getFileDescriptor;
/**
 * Utility function, converts a PvsContextDescriptor into a LookUpTable
 */
function contextDescriptor2LookUpTable(ctx) {
    var _a;
    if (ctx === null || ctx === void 0 ? void 0 : ctx.contextFolder) {
        const table = {
            stats: {
                version: "",
                folders: 1,
                theories: 0,
                types: 0,
                functions: 0,
                formulas: 0
            },
            folders: {},
            theories: {},
            types: {},
            functions: {},
            formulas: {}
        };
        table.folders[ctx.contextFolder] = [];
        if (ctx.fileDescriptors) {
            const keys = Object.keys(ctx.fileDescriptors);
            for (let i = 0; i < keys.length; i++) {
                const key = keys[i];
                const tdescs = ((_a = ctx.fileDescriptors[key]) === null || _a === void 0 ? void 0 : _a.theories) || [];
                for (let t = 0; t < tdescs.length; t++) {
                    const tdesc = tdescs[t];
                    // compile folders and theories
                    const theory = {
                        contextFolder: tdesc.contextFolder,
                        fileName: tdesc.fileName,
                        fileExtension: tdesc.fileExtension,
                        theoryName: tdesc.theoryName,
                        line: tdesc.line,
                        character: tdesc.character
                    };
                    if (!table[ctx.contextFolder]) {
                        table[ctx.contextFolder] = [];
                    }
                    table[ctx.contextFolder].push(theory);
                    table.theories[theory.theoryName] = [theory];
                    // compile formulas
                    const fdescs = tdesc.theorems || [];
                    for (let f = 0; f < fdescs.length; f++) {
                        const fdesc = fdescs[f];
                        const formula = {
                            contextFolder: fdesc.contextFolder,
                            fileName: fdesc.fileName,
                            fileExtension: fdesc.fileExtension,
                            theoryName: fdesc.theoryName,
                            formulaName: fdesc.formulaName,
                            line: fdesc.line,
                            character: fdesc.character
                        };
                        if (!table.formulas[formula.formulaName]) {
                            table.formulas[formula.formulaName] = [];
                        }
                        table.formulas[formula.formulaName].push(formula);
                    }
                }
            }
        }
        return table;
    }
    return null;
}
exports.contextDescriptor2LookUpTable = contextDescriptor2LookUpTable;
/**
 * Utility function, renames a formula in a .jprf proof file
 */
function renameFormulaInProofFile(formula, newInfo) {
    return __awaiter(this, void 0, void 0, function* () {
        if (formula && newInfo && newInfo.newFormulaName) {
            const fname = desc2fname({
                fileName: formula.fileName,
                fileExtension: ".jprf",
                contextFolder: formula.contextFolder
            });
            const fdesc = yield readJprfProofFile(fname);
            // check if file contains a proof for the given formula
            const key = `${formula.theoryName}.${formula.formulaName}`;
            if (fdesc && fdesc[key] && fdesc[key].length) {
                const newKey = `${formula.theoryName}.${newInfo.newFormulaName}`;
                // we are updating only the default proof, this might be changed in the future
                const pdesc = fdesc[key][0];
                if (pdesc.info) {
                    pdesc.info.formula = newInfo.newFormulaName;
                    pdesc.info.shasum = newInfo.newShasum || pdesc.info.shasum;
                }
                if (pdesc.proofTree) {
                    pdesc.proofTree.name = newKey;
                }
                // delete old fdesc entry
                delete fdesc[key];
                // change old shasum for all other proofs
                const keys = Object.keys(fdesc);
                for (let i = 0; i < keys.length; i++) {
                    if (fdesc[keys[i]] && fdesc[keys[i]].length && fdesc[keys[i]][0].info) {
                        fdesc[keys[i]][0].info.shasum = newInfo.newShasum;
                    }
                }
                // add new key
                fdesc[newKey] = [pdesc];
                // write to file
                const newContent = JSON.stringify(fdesc, null, " ");
                return yield writeFile(fname, newContent);
            }
        }
        return false;
    });
}
exports.renameFormulaInProofFile = renameFormulaInProofFile;
/**
 * Utility function, renames a theory in a .jprf proof file
 */
function renameTheoryInProofFile(theory, newInfo) {
    return __awaiter(this, void 0, void 0, function* () {
        if (theory && newInfo && newInfo.newTheoryName) {
            const fname = desc2fname({
                fileName: theory.fileName,
                fileExtension: ".jprf",
                contextFolder: theory.contextFolder
            });
            const fdesc = yield readJprfProofFile(fname);
            // update all proofs
            // check if file contains a proof for the given formula
            if (fdesc) {
                const keys = Object.keys(fdesc);
                if (keys && keys.length) {
                    const newFdesc = {};
                    for (let i = 0; i < keys.length; i++) {
                        const key = keys[i];
                        // we are updating only the default proof, this might be changed in the future
                        const pdesc = fdesc[key][0];
                        if (pdesc.info) {
                            pdesc.info.theory = newInfo.newTheoryName;
                            pdesc.info.shasum = newInfo.newShasum || pdesc.info.shasum;
                        }
                        const newKey = `${newInfo.newTheoryName}.${pdesc.info.formula}`;
                        if (pdesc.proofTree) {
                            pdesc.proofTree.name = newKey;
                        }
                        newFdesc[newKey] = [pdesc];
                    }
                    // write to file
                    const newContent = JSON.stringify(newFdesc, null, " ");
                    const newFname = desc2fname({
                        fileName: (theory.fileName === theory.theoryName) ? newInfo.newTheoryName : theory.fileName,
                        fileExtension: ".jprf",
                        contextFolder: theory.contextFolder
                    });
                    const fileAlreadyExists = yield fileExists(newFname);
                    const success = (fileAlreadyExists) ? yield writeFile(fname, newContent)
                        : yield writeFile(newFname, newContent);
                    if (success && !fileAlreadyExists && fname !== newFname) {
                        deleteFile(fname);
                    }
                    return success;
                }
            }
        }
        return false;
    });
}
exports.renameTheoryInProofFile = renameTheoryInProofFile;
/**
 * Utility function, generates a human readable summary for a workspace
 * - the list of theorems in a given context folder
 * - the status of each theorem (proved/missed)
 * - the total number of proved/missed
 */
function makeWorkspaceSummary(desc) {
    if (desc === null || desc === void 0 ? void 0 : desc.contextFolder) {
        const header = "Proof summary";
        const workspaceName = getContextFolderName(desc.contextFolder);
        let ans = `${header} for workspace ${workspaceName}\n`;
        let nProved = 0;
        let nMissed = 0;
        let totTime = 0;
        let libraries = {};
        for (let i = 0; i < (desc === null || desc === void 0 ? void 0 : desc.theories.length); i++) {
            const theory = desc.theories[i];
            if ((theory === null || theory === void 0 ? void 0 : theory.theoryName) && theory.contextFolder) {
                libraries[theory === null || theory === void 0 ? void 0 : theory.contextFolder] = true;
                const points = (64 - theory.theoryName.length) > 0 ? 64 - theory.theoryName.length : 0;
                const overall = (theory.miss) ? `${languageUtils_1.icons.sparkles} partly proved [${theory.ok}/${theory.total}]`
                    : `${languageUtils_1.icons.checkmark}  fully proved [${theory.ok}/${theory.total}]`;
                const spaces = (20 - overall.length) > 0 ? 20 - overall.length : 0;
                nProved += theory.ok;
                nMissed += theory.miss;
                ans += `\n\t${theory.theoryName}` + ".".repeat(points) + " " + overall + " ".repeat(spaces) + `(${(+theory.ms / 1000).toFixed(3)} s)`;
                totTime += theory.ms;
            }
        }
        ans += `\n\nWorkspace ${workspaceName} totals: ${desc.total} formulas, ${nProved + nMissed} attempted, ${nProved} succeeded (${(+totTime / 1000).toFixed(3)} s)`;
        // 	ans += `\n
        // *** Grand Totals: ${nProved} proofs / ${desc.total} formulas. Missed: ${nMissed} formulas.
        // *** Number of libraries: ${Object.keys(libraries).length}`;	
        return ans;
    }
    return null;
}
exports.makeWorkspaceSummary = makeWorkspaceSummary;
/**
 * Utility function, generates a human readable summary for a theory
 * - the list of theorems in the theory
 * - the status of each theorem (proved/missed)
 * - the total number of proved/missed
 */
function makeTheorySummary(desc) {
    if (desc === null || desc === void 0 ? void 0 : desc.theoryName) {
        const header = desc.tccsOnly ? "TCCs summary" : "Proof summary";
        let ans = `${header} for theory ${desc.theoryName}\n`;
        let nProved = 0;
        let totTime = 0;
        let importChainFlag = false;
        for (let i = 0; i < desc.theorems.length; i++) {
            if (desc.theorems[i].theoryName !== desc.theoryName && !importChainFlag) {
                importChainFlag = true;
                ans += `\n\t%-- importchain`;
            }
            const formulaName = desc.theorems[i].formulaName;
            const status = desc.theorems[i].status;
            const ms = desc.theorems[i].ms;
            const points = (64 - formulaName.length) > 0 ? 64 - formulaName.length : 0;
            const spaces = (20 - status.length) > 0 ? 20 - status.length : 0;
            if ((0, languageUtils_1.isProved)(status)) {
                nProved++;
            }
            totTime += ms;
            ans += `\n\t${formulaName}` + ".".repeat(points) + (0, languageUtils_1.getIcon)(status) + " " + status + " ".repeat(spaces) + `(${(+ms / 1000)} s)`;
        }
        ans += `\n\nTheory ${desc.theoryName} totals: ${desc.total} formulas, ${desc.theorems.length} attempted, ${nProved} succeeded (${+(totTime / 1000).toFixed(3)} s)`;
        return ans;
    }
    return null;
}
exports.makeTheorySummary = makeTheorySummary;
/**
 * Utility function, breaks down a string representing a pvs library path into an array of folder names
 */
function decodePvsLibraryPath(pvsLibraryPath) {
    const libs = (pvsLibraryPath) ? pvsLibraryPath.split(":").map((elem) => {
        return elem.trim();
    }) : [];
    return libs.filter((elem) => {
        return elem !== "";
    }).map((elem) => {
        return elem.endsWith("/") ? elem : `${elem}/`;
    });
}
exports.decodePvsLibraryPath = decodePvsLibraryPath;
/**
 * Utility function, creates a string representing the pvsLibraryPath given by the concatenation of a list of folders
 */
function createPvsLibraryPath(libs) {
    if (libs && libs.length) {
        return libs.filter((elem) => {
            return elem.trim() !== "";
        }).map((elem) => {
            return elem.trim().endsWith("/") ? elem.trim() : `${elem.trim()}/`;
        }).join(":");
    }
    return "";
}
exports.createPvsLibraryPath = createPvsLibraryPath;
/**
 * Utility function, appends a proof summary at the end of a given file
 * @param fname Name of the summary file
 * @param summary The summary to be appended
 */
function appendSummary(fname, summary) {
    return __awaiter(this, void 0, void 0, function* () {
        if (fname && summary) {
            const content = yield readFile(fname);
            const newContent = (content && content.trim()) ? content + `\n\n${summary}` : summary;
            return yield writeFile(fname, newContent);
        }
        return false;
    });
}
exports.appendSummary = appendSummary;
/**
 * Utility function, removes a proof summary from a given file
 * @param fname Name of the summary file
 * @param theoryName name of the theory whose summary should be removed
 */
function removeSummary(fname, theoryName) {
    return __awaiter(this, void 0, void 0, function* () {
        if (fname && theoryName) {
            fname = decodeURIComponents(fname);
            const success = yield fileExists(fname);
            if (success) {
                const content = yield readFile(fname);
                if (content) {
                    const regex = new RegExp(`\\bProof summary for theory ${theoryName}\\s[\\s\\w\\W]+\\bTheory ${theoryName}\\s.*`, "g");
                    const newContent = content.replace(regex, "");
                    return yield writeFile(fname, newContent);
                }
            }
        }
        return false;
    });
}
exports.removeSummary = removeSummary;
/**
 * Utility function, checks if a summary is present in a given file
 * @param fname Name of the summary file
 * @param theoryName name of the theory whose summary should be removed
 */
function containsSummary(fname, theoryName) {
    return __awaiter(this, void 0, void 0, function* () {
        if (fname && theoryName) {
            fname = decodeURIComponents(fname);
            const success = yield fileExists(fname);
            if (success) {
                const content = yield readFile(fname);
                if (content) {
                    const regex = new RegExp(`\\bProof summary for theory ${theoryName}\\s[\\s\\w\\W]+\\bTheory ${theoryName}\\s.*`, "g");
                    return regex.test(content);
                }
            }
        }
        return false;
    });
}
exports.containsSummary = containsSummary;
/**
 * Utility function, saves a summary for a given theory in the given file
 * @param fname Name of the summary file
 * @param theoryName name of the theory whose summary should be saved
 * @param summary The summary to be saved in the file
 */
function saveSummary(fname, theoryName, summary) {
    return __awaiter(this, void 0, void 0, function* () {
        if (fname && theoryName && summary) {
            fname = decodeURIComponents(fname);
            const success = yield fileExists(fname);
            if (success) {
                // deleted any previous version of the summary
                yield removeSummary(fname, theoryName);
            }
            // append the new summary
            return yield appendSummary(fname, summary);
        }
        return false;
    });
}
exports.saveSummary = saveSummary;
/**
 * Utility function, creates the command for downloading the list of pvs versions
 */
function lsPvsVersions() {
    var _a;
    const osName = getOs();
    if (osName && osName.version) {
        const preferredVersion = "7.1.0"; //"ge7a69672"; //"g762f82aa"; //"ga3f9dbb7";//"g03fe2100";
        const shellCommand = getDownloadCommand(serverInterface_1.pvsDownloadUrl);
        if (shellCommand === null || shellCommand === void 0 ? void 0 : shellCommand.cmd) {
            let lsCommand = `${shellCommand.cmd} ${(_a = shellCommand.args) === null || _a === void 0 ? void 0 : _a.join(" ")}`;
            if (shellCommand.cmd === "wget") {
                lsCommand += " -O- "; // this is needed to redirect the output of wget to stdout, otherwise wget will write a file
            }
            lsCommand += `| grep -oE '(pvs.*\.tgz)\"' `
                + `| sed 's/"$//' `
                + `| grep ${preferredVersion} `
                + `| grep ${osName.version} `
                + `| grep allegro`;
            return lsCommand;
        }
    }
    return null;
}
exports.lsPvsVersions = lsPvsVersions;
/**
 * Utility function, parses the result of lsPvsVersions
 */
function parseLsPvsVersions(str) {
    if (str) {
        const res = str === null || str === void 0 ? void 0 : str.toLocaleString().trim();
        const elems = res === null || res === void 0 ? void 0 : res.split("\n");
        const versions = elems === null || elems === void 0 ? void 0 : elems.map((fileName) => {
            const match = /pvs-?([\d\.\-]+)\-\w+/.exec(fileName);
            const version = (match && match.length > 1) ? match[1].replace(/\-/g, ".") : null;
            const url = (serverInterface_1.pvsDownloadUrl.endsWith("/") ? serverInterface_1.pvsDownloadUrl : serverInterface_1.pvsDownloadUrl + "/") + fileName;
            return { url, fileName, version };
        });
        return versions;
    }
    return null;
}
exports.parseLsPvsVersions = parseLsPvsVersions;
/**
 * Utility function, returns the name of the default undump folder
 */
function getUndumpFolderName(dmpFile) {
    const folder = `undumped_${dmpFile.fileName}`;
    return folder;
}
exports.getUndumpFolderName = getUndumpFolderName;
/**
 * Base class capable of posting long-running tasks
 */
class PostTask {
    constructor() {
        // timer used for delayed execution of tasks
        this.timer = null;
        // task start delay
        this.TASK_START_DELAY = 250; //ms
    }
    /**
     * Utility function for posting the execution of a potentially long tasks
     * that do not require immediate completion
     */
    postTask(task) {
        clearTimeout(this.timer);
        this.timer = setTimeout(() => {
            task();
        }, this.TASK_START_DELAY);
    }
}
exports.PostTask = PostTask;
// TODO: move the following functions to a new file "prettyPrinterUtils.ts"
/**
 * Utility function, prettyprints the sequent label
 */
function sequentToString(sequents, opt) {
    let res = "";
    opt = opt || {};
    const colorTheme = opt.colorTheme || "dark";
    const color = (0, colorUtils_1.getColor)(colorUtils_1.PvsColor.green, colorTheme);
    for (let i = 0; i < sequents.length; i++) {
        const sequent = sequents[i];
        let label = sequent.labels.join(" ");
        label = (sequent.changed === 'true') ? `{${label}}` : `[${label}]`;
        label = (sequent.changed === 'true' && opt.useColors) ? `${(0, colorUtils_1.colorText)(label, color)}` : `${label}`;
        let fmla = tryPrettyPrintFormula(sequent.formula, opt === null || opt === void 0 ? void 0 : opt.prettyPrinter);
        const formula = (opt.useColors) ? `${(0, languageUtils_1.pvsSyntaxHighlighting)(fmla, opt)}` : fmla;
        res += `${label}   ${formula}`;
        res += opt.htmlEncoding ? "<br>" : "\n";
    }
    return res;
}
/**
 * Utility function, tries to pretty-print a sequent formula
 */
function tryPrettyPrintFormula(formula, pp) {
    var _a;
    if (pp && formula) {
        let fmla = formula;
        const opts = (pp === null || pp === void 0 ? void 0 : pp.options) || [];
        const args = [...opts, formula];
        try {
            fmla = (pp === null || pp === void 0 ? void 0 : pp.cmd) ?
                (0, child_process_2.execFileSync)(pp.cmd, args, { encoding: "utf-8" })
                : formula;
            if ((_a = fmla === null || fmla === void 0 ? void 0 : fmla.trim()) === null || _a === void 0 ? void 0 : _a.startsWith("[{")) {
                const diags = JSON.parse(fmla);
                console.dir("[language-utils] External prettyprinter returned an error");
                console.dir(diags);
                // restore original output
                fmla = formula;
            }
        }
        catch (err) {
            console.log(`[language-utils] Error while trying to use external prettyprinter (${pp.cmd} ${args.join(" ")})`, err);
            // restore original output
            fmla = formula;
        }
        return fmla;
    }
    return formula;
}
/**
 * Utility function, prettyprints the sequent label
 */
function labelToString(label, opt) {
    opt = opt || {};
    const colorTheme = opt.colorTheme || "dark";
    const color = (0, colorUtils_1.getColor)(colorUtils_1.PvsColor.green, colorTheme);
    return (opt && opt.useColors) ?
        `\n${(0, colorUtils_1.colorText)(`${label} :`, color)}\n`
        : `\n${label} :\n`;
}
/**
 * Utility function, prettyprints user comments included in the sequent returned by the prover
 */
function commentToString(txt, opt) {
    opt = opt || {};
    const colorTheme = opt.colorTheme || "dark";
    const color = (0, colorUtils_1.getColor)(colorUtils_1.PvsColor.yellow, colorTheme);
    const content = (opt && opt.useColors) ?
        `${(0, colorUtils_1.colorText)(`${txt}`, color)}`
        : `${txt}`;
    return opt.htmlEncoding ? `<br>${content}<br>` : `\n${content}\n`;
}
/**
 * Utility function, prettyprints the commentary string included in the sequent returned by the prover
 */
function commentaryToString(commentary, opt) {
    let res = "";
    if (commentary) {
        const colorTheme = opt.colorTheme || "dark";
        const gray = (0, colorUtils_1.getColor)(colorUtils_1.PvsColor.gray, colorTheme);
        const yellow = (0, colorUtils_1.getColor)(colorUtils_1.PvsColor.yellow, colorTheme);
        if (typeof commentary === "string") {
            commentary = commentary.trim().endsWith(",") ? commentary.trim().slice(0, -1) : commentary.trim();
            res += opt.htmlEncoding ? `<br>${commentary}<br>`
                : opt.useColors ? `\n${(0, colorUtils_1.colorText)(`${commentary}`, commentary.includes("This completes the proof") ? yellow : gray)}\n`
                    : `\n${commentary}\s`;
        }
        else {
            res += opt.htmlEncoding ? "<br>" : "\n";
            for (let i = 0; i < commentary.length; i++) {
                let line = commentary[i];
                if (i === commentary.length - 1) {
                    line = line.trim().endsWith(",") ? line.trim().slice(0, -1) : line.trim();
                }
                res += opt.htmlEncoding ? `${line}<br>`
                    : opt.useColors ? `${(0, colorUtils_1.colorText)(`${line}`, line.includes("This completes the proof") ? yellow : gray)}\n`
                        : `${line}\n`;
            }
        }
    }
    return res;
}
exports.commentaryToString = commentaryToString;
function formatPvsIoState(pvsioState, opt) {
    if (pvsioState) {
        opt = opt || {};
        return (opt.useColors) ? (0, languageUtils_1.pvsSyntaxHighlighting)(pvsioState) : pvsioState;
    }
    return pvsioState;
}
exports.formatPvsIoState = formatPvsIoState;
function formatHiddenFormulas(proofState, opt) {
    if (proofState) {
        opt = opt || {};
        let res = "";
        if (proofState.sequent) {
            if (proofState.sequent["hidden-antecedents"] || proofState.sequent["hidden-succedents"]) {
                res += "\n%-- Hidden formulas --\n\n";
                if (proofState.sequent["hidden-antecedents"]) {
                    res += sequentToString(proofState.sequent["hidden-antecedents"], opt);
                }
                // res += "  |-------\n";
                res += "  ├───────\n";
                if (proofState.sequent["hidden-succedents"]) {
                    res += sequentToString(proofState.sequent["hidden-succedents"], opt);
                }
                res += "\n\n";
            }
            else {
                res += "The current sequent does not have any hidden formula.\n";
            }
        }
        return res;
    }
    else {
        console.error("[language-utils.show-hidden-formulas] Error: proof state is null :/");
    }
    return null;
}
exports.formatHiddenFormulas = formatHiddenFormulas;
/**
 * Utility function, converts sequent formulas into a string
 */
function sformulas2string(desc) {
    let res = "";
    if (desc === null || desc === void 0 ? void 0 : desc.sequent) { // print label and comment only if the sequent is non-empty (sequent empty means proof completed)
        if (desc.sequent.antecedents) {
            res += sequentToString(desc.sequent.antecedents);
        }
        // res += "  |-------\n";
        res += "  ├─────── \n";
        if (desc.sequent.succedents) {
            res += sequentToString(desc.sequent.succedents);
        }
    }
    return res;
}
exports.sformulas2string = sformulas2string;
/**
 * Prettyprints sequents. Syntax highlighting is introduced as ansi codes when option useColors is true.
 */
function formatSequent(desc, opt) {
    if (desc) {
        opt = opt || {};
        let res = "";
        if (!opt.formulasOnly) {
            if (desc.action && opt.showAction) {
                const action = desc.action.endsWith(",") ? desc.action.substring(0, desc.action.length - 1) : desc.action;
                res += opt.htmlEncoding ? `<br>${action}.<br>` : `\n${action}.\n`;
            }
            if (desc.commentary && !opt.ignoreCommentary) {
                res += commentaryToString(desc.commentary, opt);
            }
        }
        // print label and comment only if 
        // - the sequent is non-empty (sequent empty means proof completed)
        // - the commentary string does not indicate error
        if (desc.sequent && (opt.ignoreCommentary || !(0, languageUtils_1.isInvalidCommand)(desc))) {
            if (desc.label) {
                res += labelToString(desc.label, opt);
            }
            if (desc.comment) {
                res += commentToString(desc.comment, opt);
            }
            res += opt.htmlEncoding ? "<br>" : "\n";
            if (desc.sequent.antecedents) {
                res += sequentToString(desc.sequent.antecedents, opt);
            }
            // res += "  |-------\n";
            res += opt.htmlEncoding ? "  ├─────── <br>" : "  ├─────── \n";
            if (desc.sequent.succedents) {
                res += sequentToString(desc.sequent.succedents, opt);
            }
        }
        return (opt.htmlEncoding ? "<br>" : "\n") + res.trimRight();
    }
    else {
        console.error("[language-utils.format-proof-state] Error: proof state is null :/");
    }
    return null;
}
exports.formatSequent = formatSequent;
//# sourceMappingURL=fsUtils.js.map