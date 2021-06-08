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

import * as fs from 'fs';
import * as path from 'path';
import { ChildProcess, exec, execSync } from 'child_process';
import * as crypto from 'crypto';
import { 
	FileDescriptor, FileList, FormulaDescriptor, Position, ProofDescriptor, 
	ProofFile, ProofNode, ProofStatus, PvsContextDescriptor, PvsDownloadDescriptor, 
	pvsDownloadUrl, PvsFileDescriptor, 
	PvsFormula, PvsTheory, TheoryDescriptor, Downloader, ShellCommand
} from '../common/serverInterface';
import { 
	commentRegexp, endTheoryOrDatatypeRegexp, formulaRegexp, getIcon, 
	icons, isProved, proofliteDeclRegexp, proofliteRegexp, theoremRegexp, theoryRegexp 
} from './languageUtils';


export const HOME_DIR: string = require('os').homedir();
export const MAX_RECURSION: number = 4;
// nodeJS does not support tilde expansion for the home folder
export function tildeExpansion(pathName: string): string {
	if (pathName) {
		if (pathName.startsWith("~/") || pathName === "~") {
			pathName = pathName.replace("~", HOME_DIR);
		}
		return path.normalize(pathName);
	}
	return pathName;
}
export function stat(fname: string): Promise<fs.Stats> {
	if (fname) {
		fname = tildeExpansion(fname);
		return new Promise<fs.Stats>((resolve, reject) => {
			fs.stat(fname, (error, stat) => {
				// ignore errors for now
				resolve(stat);
			});
		});
	}
	return null;
};
export function readDir(contextFolder: string): Promise<string[]> {
	if (contextFolder) {
		contextFolder = tildeExpansion(contextFolder);
		return new Promise<string[]>((resolve, reject) => {
			fs.readdir(contextFolder, (error: NodeJS.ErrnoException, children: string[]) => {
				// ignore errors for now
				resolve(children || []);
			});
		});
	}
	return null;
}
export async function readFile(fname: string, opt?: { encoding?: BufferEncoding }): Promise<string> {
	if (fname) {
		opt = opt || {};
		opt.encoding = opt.encoding || "utf8";
		fname = fname.replace("file://", "");
		fname = tildeExpansion(fname);
		try {
			const exists: boolean = await fileExists(fname);
			if (exists) {
				const data: string = fs.readFileSync(fname, { encoding: opt.encoding });
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
}
export function isSameFile (fname1: string, fname2: string): boolean {
	if (fname1 && fname2) {
		const f1: string = fname1.replace("file://", "");
		const f2: string = fname2.replace("file://", "");
		return f1 === f2;
	}
	return false;
}
export function deleteFile (fname: string): boolean {
	if (fname) {
		try {
			fname = fname.replace("file://", "");
			fs.unlinkSync(fname);
		} catch (deleteError) {
			return false;
		}
	}
	return true;
}
export function deleteFolder(contextFolder: string): boolean {
	try {
		if (contextFolder) {
			contextFolder = tildeExpansion(contextFolder);
			if (fs.existsSync(contextFolder)) {
				execSync(`rm -r ${contextFolder}`);
			}
		}
	} catch (deleteError) {
		console.error(`[fs-utils] `, deleteError);
		return false;
	}
	return true;
}
export function deleteBinFiles(binFolder: string, opt?: { removePvsbinFolder?: boolean }): boolean {
	opt = opt || {};
	try {
		if (binFolder) {
			binFolder = tildeExpansion(binFolder);
			if (fs.existsSync(binFolder)) {
				if (opt.removePvsbinFolder) {
					deleteFolder(binFolder);
				} else {
					execSync(`rm ${binFolder}/*.bin 2> /dev/null`); // 2> /dev/null suppresses 'file not found' messages
				}
			}
		}
	} catch (deleteError) {
		return false;
	}
	return true;
}
export async function cleanTccs(contextFolder: string, opt?: { 
	recursive?: number, 
}): Promise<number> {
	opt = opt || {};
	let nCleaned: number = 0;
	try {
		if (contextFolder) {
			contextFolder = tildeExpansion(contextFolder);
			// console.log(`reading folder ${contextFolder}`);
			const files: string[] = fs.readdirSync(contextFolder);
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
					const dirs: string[] = fs.readdirSync(contextFolder);
					for (let i = 0; i < dirs.length; i++) {
						const dir: string = path.join(contextFolder, dirs[i]);
						if (fs.lstatSync(dir).isDirectory()) {
							nCleaned += await cleanTccs(dir, opt);
						}
					}
				}
			}
		}
	} catch (deleteError) {
		return Promise.resolve(nCleaned);
	}
	return Promise.resolve(nCleaned);
}
export async function cleanBin(contextFolder: string, opt?: { 
	keepTccs?: boolean, 
	recursive?: number, 
	removePvsbinFolder?: boolean
}): Promise<number> {
	opt = opt || {};
	let nCleaned: number = 0;
	try {
		// console.log(`Deleting cache for context ${contextFolder}`);
		if (contextFolder) {
			contextFolder = tildeExpansion(contextFolder);
			// console.log(`Deleting cache for context ${contextFolder}`);
			const pvsbinFolder: string = path.join(contextFolder, "pvsbin");
			deleteBinFiles(pvsbinFolder, opt);
			// console.log(`removing ${path.join(contextFolder, ".pvscontext")}`);
			deleteFile(path.join(contextFolder, ".pvscontext"));
			// console.log(`reading folder ${contextFolder}`);
			const files: string[] = fs.readdirSync(contextFolder);
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
					const dirs: string[] = fs.readdirSync(contextFolder);
					for (let i = 0; i < dirs.length; i++) {
						const dir: string = path.join(contextFolder, dirs[i]);
						if (fs.lstatSync(dir).isDirectory()) {
							nCleaned += await cleanBin(dir, opt);
						}
					}
				}
			}
		}
	} catch (deleteError) {
		return Promise.resolve(nCleaned);
	}
	return Promise.resolve(nCleaned);
}
/**
 * Creates a folder, if the folder does not exist already.
 */
export async function createFolder(path: string): Promise<void> {
	if (!fs.existsSync(path)){
		fs.mkdirSync(path, { recursive: true });
	}
}
export async function writeFile(fname: string, content: string, opt?: { append?: boolean, encoding?: BufferEncoding }): Promise<boolean> {
	opt = opt || {};
	opt.encoding = opt.encoding || "utf8";
	if (fname) {
		try {
			fname = fname.replace("file://", "");
			fname = tildeExpansion(fname);
			const contextFolder: string = getContextFolder(fname);
			await createFolder(contextFolder);
			if (opt.append) {
				const previousContent: string = await readFile(fname);
				content = previousContent + "\n\n" + content;
				content = content.trim();
			}
			fs.writeFileSync(fname, content, { encoding: opt.encoding });
		} catch (error) {
			console.error(`[fs-utils] Error while writing file ${fname}`, error);
			return false;
		}
	}
	return true;
}
export async function renameFile(old_fname: string, new_fname: string): Promise<boolean> {
	if (old_fname && new_fname) {
		const content: string = await readFile(old_fname) || "";
		let success: boolean = await writeFile(new_fname, content);
		if (success) {
			success = await deleteFile(old_fname);
		}
		return success;
	}
	return false;
}
export function getFileName(fname: string, opt?: { keepExtension?: boolean }): string {
	if (fname) {
		fname = fname.replace("file://", "");
		fname = fname.includes("/") ? fname.split("/").slice(-1)[0] : fname;
		if (!opt?.keepExtension) {
			fname = fname.includes(".") ? fname.split(".").slice(0, -1).join(".") : fname;
		}
		return fname;
	}
	return null;
}
export function removeFileExtension(fname: string): string {
	if (fname && fname.indexOf(".") >= 0) {
		return fname.split(".").slice(0, -1).join(".");
	}
	return null;
}
export function moveFolder(contextFolder: string, toFolder: string): boolean {
	try {
		if (contextFolder && toFolder && fs.existsSync(contextFolder) && !fs.existsSync(toFolder)) {
			execSync(`mv ${contextFolder} ${toFolder}`);
			return true;
		}
	} catch (moveError) {
		return false;
	}
	return false;
}
export function getFileExtension(fname: string): string {
	if (fname) {
		return `.${fname.split(".").slice(-1).join(".")}`;
	}
	return null;
}
export function normalizeContextFolder(contextFolder: string): string {
	if (contextFolder) {
		return contextFolder.replace("file://", "");
	}
	return null;
}
export function getContextFolder(fname: string): string {
	if (fname) {
		const ctx: string = fname.replace("file://", "");
		return ctx.split("/").slice(0, -1).join("/").replace("//", "/");
	}
	return null;
}
export function getContextFolderName(contextFolder: string): string {
	if (contextFolder) {
		return contextFolder.substring(contextFolder.lastIndexOf('/') + 1, contextFolder.length);
	}
	return null;
}
export function isPvsFile(desc: string | { fileName: string, fileExtension: string, contextFolder: string }): boolean {
	if (desc) {
		const ext: string = (typeof desc === "string") ? desc : (desc) ? desc.fileExtension : null;
		if (ext) {
			return ext.endsWith('.pvs') || ext.endsWith('.tccs') || ext.endsWith('.ppe') || ext.endsWith('.pr')
					|| ext.endsWith('.hpvs') || ext.endsWith(".summary") || ext.endsWith(".prl");
		}
	}
	return false;
}
export function isSummaryFile(desc: string | { fileName: string, fileExtension: string, contextFolder: string }): boolean {
	if (desc) {
		const ext: string = (typeof desc === "string") ? desc : (desc) ? desc.fileExtension : null;
		if (ext) {
			return ext.endsWith(".summary");
		}
	}
	return false;
}
export function isProofliteFile(desc: string | { fileName: string, fileExtension: string, contextFolder: string }): boolean {
	if (desc) {
		const ext: string = (typeof desc === "string") ? desc : (desc) ? desc.fileExtension : null;
		if (ext) {
			return ext.endsWith(".prl") || ext.endsWith(".prlite") || ext.endsWith('.pr');
		}
	}
	return false;
}
export function fileExists(fname: string): boolean {
	return pathExists(fname);
}
export function dirExists(contextFolder: string): boolean {
	return pathExists(contextFolder);
}
export function folderExists(contextFolder: string): boolean {
	return dirExists(contextFolder);
}
export function pathExists(path: string): boolean {
	if (path) {
		let ans: boolean = false;
		path = tildeExpansion(path);
		try {
			ans = fs.existsSync(path);
		} catch (readError) {
			// console.error(readError);
		} finally {
			return ans;
		}
	}
	return false;
}
/**
 * Utility function, returns the list of pvs files contained in a given folder
 * @param contextFolder Path to a folder containing pvs files.
 * @returs List of pvs files, as a structure FileList. Null if the folder does not exist.
 */
export async function listPvsFiles (contextFolder: string): Promise<FileList> {
	if (contextFolder) {
		const children: string[] = await readDir(contextFolder);
		const fileList: FileList = {
			fileNames: children.filter((fileName) => {
				return (fileName.endsWith(".pvs") || fileName.endsWith(".hpvs"))
						&& !fileName.startsWith("."); // this second part is necessary to filter out temporary files created by pvs
			}),
			contextFolder: contextFolder
		};
		return fileList;
	}
	return null;
}


export function normalizePath(p: string) {
	if (p) {
		p = path.normalize(p);
		p = (p.endsWith("/")) ? p.substr(0, p.length - 1) : p;
		p = tildeExpansion(p);
	}
	return p;
}

export function getText(txt: string, range: { start: { line: number, character?: number }, end: { line: number, character?: number } }): string {
	if (txt && range) {
		const lines: string[] = txt.split("\n");
		let ans: string[] = lines.slice(range.start.line, range.end.line + 1);
		if (ans && ans.length > 0) {
			if (!isNaN(range.start.character)) {
				ans[0] = ans[0].substr(range.start.character);
			}
			if (!isNaN(range.end.character)) {
				let endCharacter: number = range.end.character;
				if (!isNaN(range.start.character) && range.start.line === range.end.line) {
					endCharacter -= range.start.character;
				}
				ans[ans.length - 1] = ans[ans.length - 1].substr(0, endCharacter);
			}
			return ans.join("\n");
		}
	}
	return txt;
}


export function get_fresh_id(): string {
	return shasum(Math.random().toString(36));
}

export function shasum (txt: string): string {
	if (txt) {
		const spaceless: string = txt.replace(/%.*/g, "").replace(/\s+/g, ""); // removes comments and white spaces in the pvs file
		return crypto.createHash('sha256').update(spaceless).digest('hex');
	}
	return "";
}

export async function shasumFile (desc: FileDescriptor): Promise<string> {
	const content: string = await readFile(desc2fname(desc));
	if (content) {
		return shasum(content);
	}
	return "";
}

export function fname2desc (fname: string): FileDescriptor | null {
	if (fname) {
		const fileName: string = getFileName(fname);
		const fileExtension: string = getFileExtension(fname);
		const contextFolder: string = getContextFolder(fname);
		return { fileName, fileExtension, contextFolder };
	}
	return null;
}

export function desc2fname (desc: FileDescriptor): string {
	return path.join(desc.contextFolder, `${desc.fileName}${desc.fileExtension}`);
}

export function listSubFolders (folder: string): string[] {
	if (folder) {
		return fs.readdirSync(folder, { withFileTypes: true })
			.filter((dir: fs.Dirent) => { return dir.isDirectory(); })
			.map((dir: fs.Dirent) => { return dir.name; });
	}
	return null;
}

/**
 * Gets the unencoded version of an encoded components of a descriptor.
 */
export function decodeURIComponents (desc) {
	if (desc) {
		if (typeof desc === "string") {
			return decodeURIComponent(desc);
		}
		// else
		const keys: string[] = [ "fileName", "fileExtension", "contextFolder", "origin"];
		for (let i = 0; i < keys.length; i++) {
			if (desc[keys[i]] && typeof desc[keys[i]] === "string") {
				desc[keys[i]] = decodeURIComponent(desc[keys[i]]);
			}
		}
	}
	return desc;
}

/** 
 * file extension for sequent files created under pvsbin/, to show a sequent in the editor
 */
export const logFileExtension: string = ".pr";

/**
 * Utility function, checks if wget or curl are available
 */
export function getDownloader (): Downloader {
	const candidates: Downloader[] = [ "wget", "curl" ];
	for (let i = 0; i < candidates.length; i++) {
		try {
			if (execSync(`which ${candidates[i]}`)) {
				return candidates[i];
			}
		} catch (error) {
			// keep going, command not found
		}
	}
	return null;
} 

/**
 * Utility function, checks if git is available
 */
export function getSourceControl (): "git" | null {
	try {
		if (execSync(`which git`)) {
			return "git";
		}
	} catch (error) {
		// command not found
	}
	return null;
}

/**
 * Utility function, detects the OS platform
 */
export function getOs (): { version?: "Linux" | "MacOSX" | string, error?: string } {
	try {
		if (process.platform === 'linux' || process.platform === 'freebsd' || process.platform === 'openbsd' || process.platform === 'sunos' || process.platform === 'aix') {
			return { version: 'Linux' };
		} else if (process.platform === 'darwin') {
			return { version: 'MacOSX' };
		}
		return { version: process.platform };
	} catch (err) {
		const error: string = err.message + "Unable to detect OS version. This problem is likey due to missing dependency 'node' (please download node from https://nodejs.org/)";
		console.log(`[pvs-server] ${error}`);
		return { error };
	}
}

/**
 * Utility function, creates the 'git clone' / 'git pull' for nasalib
 */
export function cloneNASALibCommand (url: string, opt?: { update?: boolean, branch?: string, basePath }): ShellCommand {
	const shellCommand: ShellCommand = {
		cmd: "git",
		args: (opt?.update) ? [ "pull" ] : [ `clone "${url}" "nasalib"` ],
		cwd: (opt?.update) ? path.join(opt.basePath, "nasalib") : opt.basePath
	};
	if (opt.branch && !opt.update) {
		shellCommand.args.push(`-b "${opt.branch}"`);
	}
	return shellCommand;
}

/**
 * Utility function, converts a shell command into a string
 */
export function shellCommandToString (shellCommand: ShellCommand): string {
	if (shellCommand) {
		const cmd: string = shellCommand.args ? `${shellCommand.cmd} ${shellCommand.args?.join(" ")}`
			: shellCommand.cmd;
		return shellCommand.cwd ? `cd ${shellCommand.cwd} && ${cmd}`
			: cmd;
	}
	"";
}

/**
 * Utility function, creates the command for downloading a file from a give URL with curl or wget
 */
export function getDownloadCommand (url: string, opt?: { out?: string }): ShellCommand {
	const cmd: string = getDownloader();
	if (cmd) {
		let args: string[] = (cmd === "curl") ? [ "-L" ] // -L allows curl to follow URL redirect.
			: [ "--progress=bar:force" ,"--show-progress" ]; // wget automatically follows up to 20 URL redirect.
		args.push(url);
		if (opt?.out) {
			if (cmd === "curl") {
				args.push(`-o ${opt.out}`);
			} else {
				args.push(`-O ${opt.out}`);
			}
		}
		return { cmd, args };
	}
	return null;
}

/**
 * Default folder name for the installationn of pvs
 */
export const pvsFolderName: string = "pvs-7.1.0";

/**
 * Utility function for executing commands in the shell
 */
export function execShellCommand (req: ShellCommand, opt?: {
	stdOut?: (res: string) => void,
	stdErr?: (res: string) => void,
	callback?: (success: boolean) => void
}): ChildProcess {
	if (req?.cmd) {
		const cmd: string = req.args ? `${req.cmd} ${req.args?.join(" ")}` : req.cmd;
		const shellProcess: ChildProcess = exec(cmd, { cwd: req?.cwd });
		
		// spawn(req.cmd, req.args || [], { cwd: req?.cwd });
        shellProcess.stdout.setEncoding("utf8");
        shellProcess.stderr.setEncoding("utf8");

		shellProcess.stdout.on("data", async (data: string) => {
			opt?.stdOut(data);
		});
		shellProcess.stderr.on("data", (data: string) => {
			opt?.stdErr(data);
		});
		shellProcess.once("error", (err: Error) => {
			opt?.stdErr(`Error: ${JSON.stringify(err, null, " ")}`);
			opt?.callback(false);
		});
		shellProcess.once("exit", (code: number, signal: string) => {
			// code === 0 means success
			opt?.callback(code === 0);
		});
		shellProcess.on("message", (message: any) => {
			console.log(message);
		});
		return shellProcess;
	}
	return null;
}

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
 export function findTheoryName(fileContent: string, line: number): string | null {
	if (fileContent) {
		let txt = fileContent.replace(commentRegexp, ""); // this removes all commented text
		const regexp: RegExp = new RegExp(theoryRegexp);
		let candidates: string[] = [];

		// check that line number is not before keyword begin -- if so adjust line number otherwise regexp won't find theory name
		const matchFirstTheory: RegExpMatchArray = regexp.exec(txt)
		if (matchFirstTheory && matchFirstTheory.length > 1) {
			const theoryName: string = matchFirstTheory[1];

			const matchEnd: RegExpMatchArray = endTheoryOrDatatypeRegexp(theoryName).exec(txt);
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
		let match: RegExpMatchArray = regexp.exec(txt);
		const maxIterations: number = 64;
		// while (match) {
		for (let i = 0; i < maxIterations && match; i++) {
			if (match.length > 1 && match[1]) {
				const theoryName: string = match[1];
				const matchEnd: RegExpMatchArray = endTheoryOrDatatypeRegexp(theoryName).exec(txt);
				if (matchEnd && matchEnd.length) {
					// found theory end, the definition is not in this theory
					const endIndex: number = matchEnd.index + matchEnd[0].length;
					txt = txt.slice(endIndex);
					// need to create a new regexp when txt is updated
					match = new RegExp(regexp).exec(txt);
					// candidates = [ theoryName ].concat(candidates);
				} else {
					// theory end not found, this is a good candidate
					candidates = [ theoryName ].concat(candidates);
					match = regexp.exec(txt);
				}
				// console.log("match", { theoryName, candidates });
			} else {
				match = regexp.exec(txt);
			}
		}
		if (candidates.length > 0) {
			return candidates[0];
		}
	}
	return null;
};


/**
 * @function listTheoryNames
 * @description Utility function, returns a list of all theories in the given file
 * @param fileContent The text where the theory should be searched 
 * @returns string[] the list of theories in the given text
 */
export function listTheoryNames (fileContent: string): string[] {
	const ans: string[] = [];
	if (fileContent) {
		let txt = fileContent.replace(commentRegexp, "");
		const regexp: RegExp = theoryRegexp;
		let match: RegExpMatchArray = new RegExp(regexp).exec(txt);
		while (match) {
			if (match.length > 1 && match[1]) {
				const theoryName: string = match[1];

				const matchEnd: RegExpMatchArray = endTheoryOrDatatypeRegexp(theoryName).exec(txt);
				if (matchEnd && matchEnd.length) {
					const endIndex: number = matchEnd.index + matchEnd[0].length;
					txt = txt.slice(endIndex);
					// need to create a new regexp when txt is updated
					match = new RegExp(regexp).exec(txt);
					ans.push(theoryName);
				} else {
					match = regexp.exec(txt);
				}
			} else {
				match = regexp.exec(txt);
			}
		}
	}
	return ans;
};

/**
 * Utility function, returns the list of theories defined in a given pvs file
 * @param fname Path to a pvs file
 */
export async function listTheoriesInFile (fname: string, opt?: { content?: string }): Promise<TheoryDescriptor[]> {
	// console.log(`listing theories in file ${fname}`);
	opt = opt || {};
	if (fname) {
		const fileName: string = getFileName(fname);
		const fileExtension: string = getFileExtension(fname);
		const contextFolder: string = getContextFolder(fname);
		const fileContent: string = (opt.content) ? opt.content : await readFile(fname);
		if (fileContent) {
			const response: TheoryDescriptor[] = listTheories({ fileName, fileExtension, contextFolder, fileContent });
			// console.dir(response);
			return response;
		}
	}
	return null;
}


/**
 * Utility function, returns the list of theories defined in a given pvs file
 * @param fname Path to a pvs file
 */
export async function mapTheoriesInFile (fname: string): Promise<{ [ key: string ]: TheoryDescriptor }> {
	if (fname) {
		const fileName: string = getFileName(fname);
		const fileExtension: string = getFileExtension(fname);
		const contextFolder: string = getContextFolder(fname);
		const fileContent: string = await readFile(fname);
		if (fileContent) {
			const response: TheoryDescriptor[] = listTheories({ fileName, fileExtension, contextFolder, fileContent });
			const theoryMap: { [ key: string ]: TheoryDescriptor } = {};
			for (const i in response) {
				theoryMap[ response[i].theoryName ] = response[i];
			}
			return theoryMap;
		}
	}
	return null;
}


/**
 * Utility function, finds all theories in a given file
 * @param desc Descriptor indicating filename, file extension, context folder, and file content
 */
export function listTheories(desc: { fileName: string, fileExtension: string, contextFolder: string, fileContent: string, prelude?: boolean }): TheoryDescriptor[] {
	// console.log(`[language-utils] Listing theorems in file ${desc.fileName}${desc.fileExtension}`);
	let ans: TheoryDescriptor[] = [];
	if (desc && desc.fileContent) {
		let txt: string = desc.fileContent.replace(commentRegexp, "");
		// console.log(txt);
		const start: number = Date.now();
		const regexp: RegExp = theoryRegexp;
		// let lastIndex: number = 0;
		let match: RegExpMatchArray = new RegExp(regexp).exec(txt);
		let lineOffset: number = 0;
		while (match) {
			// console.log(`[language-utils] Found ${match[0]}`);
			if (match.length > 1 && match[1]) {
				const theoryName: string = match[1];

				const matchEnd: RegExpMatchArray = endTheoryOrDatatypeRegexp(theoryName).exec(txt);
				if (matchEnd && matchEnd.length) {
					const endIndex: number = matchEnd.index + matchEnd[0].length;
					const fullClip = txt.slice(0, endIndex);

					const clipStart = txt.slice(0, match.index);
					const lines: string[] = clipStart.split("\n"); 
					const line: number = lines.length + lineOffset;
					const character: number = 0;

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
				} else {
					match = regexp.exec(txt);
				}
			} else {
				match = regexp.exec(txt);
			}
			// console.log(match);
		}
		const stats: number = Date.now() - start;
		// console.log(`[languageUtils] listTheories(${desc.fileName}) completed in ${stats}ms`);
	}
	return ans;
}

/**
 * Utility function, returns the list of theories defined in a given pvs file
 * @param fname Path to a pvs file
 */
export async function listTheoremsInFile (fname: string, opt?: { content?: string }): Promise<FormulaDescriptor[]> {
	opt = opt || {};
	if (fname) {
		const fileName: string = getFileName(fname);
		const fileExtension: string = getFileExtension(fname);
		const contextFolder: string = getContextFolder(fname);
		const fileContent: string = (opt.content) ? opt.content : await readFile(fname);
		if (fileContent) {
			const response: FormulaDescriptor[] = await listTheorems({ fileName, fileExtension, contextFolder, fileContent });
			return response;
		}
	}
	return null;
};



/**
 * Utility function, changes the proof status from 'proved' ro 'unchecked' if the file content has changed
 */
 export function getActualProofStatus (desc: ProofDescriptor, shasum: string): ProofStatus {
	if (desc?.info) {
		if (shasum === desc.info.shasum) {
			return desc.info.status;
		} else {
			return (desc.info.status === "proved") ? "unchecked"
				: desc?.proofTree?.rules?.length ? "unfinished" 
				: "untried";
		}
	}
	return "untried";
}	

/**
 * Utility function, returns the status of the proof for the theorem indicated in the fuction arguments
 */
export async function getProofStatus (desc: { 
	fileName: string, 
	fileExtension: string, 
	contextFolder: string, 
	theoryName: string, 
	formulaName: string
}): Promise<ProofStatus> {
	if (desc) {
		let status: ProofStatus = "untried";
		// check if the .jprf file contains the proof status
		const jprf_file: string = desc2fname({
			fileName: desc.fileName, 
			fileExtension: ".jprf", 
			contextFolder: desc.contextFolder
		});
		const proofFile: ProofFile = await readJprfProofFile(jprf_file);
		if (proofFile) {
			const proofDescriptors: ProofDescriptor[] = proofFile[`${desc.theoryName}.${desc.formulaName}`];
			if (proofDescriptors && proofDescriptors.length && proofDescriptors[0] && proofDescriptors[0].info) {
				if (desc.fileExtension === ".tccs") {
					status = proofDescriptors[0].info.status; // for tccs we choose not to adjust the status because tccs are automatically generated by pvs and status of the formula is visible to the user as a comment -- we don't want to confuse them
				} else {
					// compute shasum for the file, and check it with the shasum saved in the proof descriptor. If the two differ, then the file has changed and the proof status is not valid anymore
					const shasum: string = await shasumFile(desc);
					status = getActualProofStatus(proofDescriptors[0], shasum);
				}
			}
		}
		return status;
	}
	return null;
}

/**
 * Utility function, returns the proof descriptor for a given formula
 * @param formula 
 */
export async function getProofDescriptor (formula: PvsFormula): Promise<ProofDescriptor> {
	if (formula) {
		// check if the .jprf file contains the proof status
		const jprf_file: string = desc2fname({
			fileName: formula.fileName, 
			fileExtension: ".jprf", 
			contextFolder: formula.contextFolder
		});
		const proofFile: ProofFile = await readJprfProofFile(jprf_file);
		if (proofFile) {
			const proofDescriptors: ProofDescriptor[] = proofFile[`${formula.theoryName}.${formula.formulaName}`];
			if (proofDescriptors && proofDescriptors.length) {
				return proofDescriptors[0];
			}
		}
	}
	return null;
}

/**
 * Utility function, updates the proof descriptor for a given formula
 * @param formula 
 */
export async function saveProofDescriptor (formula: PvsFormula, newDesc: ProofDescriptor, opt?: { saveProofTree?: boolean }): Promise<boolean> {
	if (formula && newDesc) {
		opt = opt || {};
		const fname: string = desc2fname({
			fileName: formula.fileName,
			fileExtension: ".jprf",
			contextFolder: formula.contextFolder
		});
		const key: string = `${formula.theoryName}.${formula.formulaName}`;
		let fdesc: ProofFile = await readJprfProofFile(fname);
		// check if file contains a proof for the given formula
		if (fdesc && fdesc[key] && fdesc[key].length) {
			// we are updating only the default proof, this might be changed in the future
			const pdesc: ProofDescriptor = fdesc[key][0];
			if (pdesc.info) {
				pdesc.info.shasum = newDesc.info.shasum ? newDesc.info.shasum : pdesc.info.shasum;
				pdesc.info.prover = newDesc.info.prover ? newDesc.info.prover : pdesc.info.prover;
				pdesc.info.status = newDesc.info.status ? newDesc.info.status : pdesc.info.status;
				pdesc.info.date = newDesc.info.date ? newDesc.info.date : pdesc.info.date;
			}
			if (opt.saveProofTree) {
				pdesc.proofTree = newDesc.proofTree ? newDesc.proofTree : pdesc.proofTree;
			}
		} else {
			fdesc[key] = fdesc[key] || [ newDesc ];
		}
		// write descriptor to file
		const newContent: string = JSON.stringify(fdesc, null, " ");
		return await writeFile(fname, newContent);		
	}
	return false;
}

/**
 * Utility function, saves the sketchpad with proof clips for a given formula
 * @param formula 
 */
export async function saveSketchpad (formula: PvsFormula, clips: ProofNode[]): Promise<boolean> {
	if (formula) {
		const fname: string = desc2fname({
			fileName: formula.fileName,
			fileExtension: ".jprf",
			contextFolder: formula.contextFolder
		});
		const key: string = `${formula.theoryName}.${formula.formulaName}`;
		let fdesc: ProofFile = await readJprfProofFile(fname);
		// check if file contains a proof for the given formula
		if (fdesc && fdesc[key] && fdesc[key].length) {
			// update clips for default proof
			const pdesc: ProofDescriptor = fdesc[key][0];
			pdesc.clips = clips || [];
			// write descriptor to file
			const newContent: string = JSON.stringify(fdesc, null, " ");
			return await writeFile(fname, newContent);		
		}
	}
	return false;
}

/**
 * Utility function, opens the sketchpad with proof clips for a given formula
 * @param formula 
 */
export async function openSketchpad (formula: PvsFormula): Promise<ProofNode[] | null> {
	if (formula) {
		const fname: string = desc2fname({
			fileName: formula.fileName,
			fileExtension: ".jprf",
			contextFolder: formula.contextFolder
		});
		const key: string = `${formula.theoryName}.${formula.formulaName}`;
		let fdesc: ProofFile = await readJprfProofFile(fname);
		// check if file contains a proof for the given formula
		if (fdesc && fdesc[key] && fdesc[key].length) {
			// update clips for default proof
			const pdesc: ProofDescriptor = fdesc[key][0];
			return pdesc.clips;
		}
	}
	return null;
}

/**
 * Utility function, returns the date (day and time) a given proof was saved
 * @param desc 
 */
export async function getProofDate (desc: { 
	fileName: string, 
	fileExtension: string, 
	contextFolder: string, 
	theoryName: string, 
	formulaName: string
}): Promise<string | null> {
	if (desc) {
		// check if the .jprf file contains the date
		const jprf_file: string = desc2fname({
			fileName: desc.fileName, 
			fileExtension: ".jprf", 
			contextFolder: desc.contextFolder
		});
		const proofFile: ProofFile = await readJprfProofFile(jprf_file);
		if (proofFile) {
			const proofDescriptors: ProofDescriptor[] = proofFile[`${desc.theoryName}.${desc.formulaName}`];
			if (proofDescriptors && proofDescriptors.length && proofDescriptors[0] && proofDescriptors[0].info) {
				return proofDescriptors[0].info.date;
			}
		}
	}
	return null;
}


/**
 * Reads the content of a .jprf file
 * @param fname Name of the prooflite file
 * @param opt Optionals
 *               - quiet (boolean): if true, the function will not print any message to the console. 
 */
 export async function readJprfProofFile (fname: string, opt?: { quiet?: boolean }): Promise<ProofFile> {
	opt = opt || {};
	let proofFile: ProofFile = {};
	fname = fname.replace("file://", "");
	fname = tildeExpansion(fname);
	const content: string = await readFile(fname);
	if (content) {
		try {
			proofFile = JSON.parse(content);
		} catch (jsonError) {
			if (!opt.quiet) {
				console.error(`[fs-utils] Error: Unable to parse proof file ${fname}`, jsonError.message);
				console.error(`[fs-utils] Storing corrupted file content to ${fname}.err`);
			}
			// create a backup copy of the corrupted jprf file, because it might get over-written
			await renameFile(fname, `${fname}.err`);
			await writeFile(`${fname}.err.msg`, jsonError.message);
		} finally {
			return proofFile;
		}
	}
	return proofFile;
}

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
export async function updateProoflite (fname: string, formulaName: string, newProoflite: string): Promise<boolean> {
	if (fname && formulaName) {
		fname = decodeURIComponents(fname);
		const success: boolean = await fileExists(fname);
		if (!success) {
			writeFile(fname, "");
		}
		const content: string = await readFile(fname);

		// group 1 is the header (this group can be null)
		// group 2 is the prooflite script
		const formula: string = formulaName.replace(/\?/g, "\\?");
		const regex: RegExp = new RegExp(`(%-*\\s%\\s*@formula\\s*:\\s*${formula}\\s[\\w\\W\\s]+%-*)?\\s*\\b(${formula}\\s*:\\s*PROOF\\b[\\s\\w\\W]+\\bQED\\b\\s*${formula}\\b\\s*)`, "g");
		if (regex.test(content)) {
			let newContent: string =  newProoflite + "\n\n\n" + content.replace(regex, "").trim();
			// update content
			return await writeFile(fname, newContent.trim());
		} else {
			const newContent: string = newProoflite + "\n\n\n" + content.trim();
			return await writeFile(fname, newContent.trim());
		}
	}
	return false;
}
/**
 * Utility function, checks if a prooflite script is present in a given file
 * @param fname Name of the prooflite file
 * @param formulaName name of the prooflite script
 */
export async function containsProoflite (fname: string, formulaName: string): Promise<boolean> {
	if (fname && formulaName) {
		fname = decodeURIComponents(fname);
		const success: boolean = await fileExists(fname);
		if (success) {
			const content: string = await readFile(fname);
			if (content) {
				// group 1 is the header (this group can be null)
				// group 2 is the prooflite script
				const formula: string = formulaName.replace(/\?/g, "\\?");
				const regex: RegExp = new RegExp(`\\b(${formula}\\s*:\\s*PROOF\\b[\\s\\w\\W]+\\bQED\\b\\s*${formula}\\b\\s*)`, "g");
				return regex.test(content);
			}
		}
	}
	return false;
}
/**
 * Utility function, returns the prooflite script without tags
 * @param fname Name of the prooflite file
 * @param formulaName name of the prooflite script
 */
export async function readProoflite (fname: string, formulaName: string): Promise<string | null> {
	if (fname && formulaName) {
		fname = decodeURIComponents(fname);
		const success: boolean = await fileExists(fname);
		if (success) {
			const content: string = await readFile(fname);
			if (content) {
				// group 1 is the header (this group can be null)
				// group 2 is the prooflite script (with tags)
				// group 3 is the prooflite script (without tags)
				const formula: string = formulaName.replace(/\?/g, "\\?");
				const regex: RegExp = new RegExp(`\\s*\\b(${formula}\\s*:\\s*PROOF\\b([\\s\\w\\W]+)\\bQED\\b\\s*${formula}\\b\\s*)`, "g");
				const match: RegExpMatchArray = regex.exec(content);
				if (match && match.length > 2) {
					return match[2].trim();
				}
			}
		}
	}
	return null;
}
/**
 * Utility function, saves a prooflite script for a given formula in the given file
 * @param fname Name of the prooflite file
 * @param formulaName name of the prooflite
 * @param script The prooflite script to be saved in the file
 */
export async function saveProoflite (fname: string, formulaName: string, script: string): Promise<boolean> {
	if (fname && formulaName && script) {
		fname = decodeURIComponents(fname);
		return await updateProoflite(fname, formulaName, script);
	}
	return false;
}

export async function getProofliteScript (desc: { 
	fileName: string, 
	fileExtension: string, 
	contextFolder: string, 
	theoryName: string, 
	formulaName: string 
}): Promise<string> {
	const fname: string = path.join(desc.contextFolder, `${desc.fileName}${desc.fileExtension}`);
	const txt: string = await readFile(fname);
	const matchProoflite: RegExpMatchArray = proofliteRegexp(desc).exec(txt);
	return matchProoflite ? matchProoflite[0] : null;
}
export async function getProofLitePosition (desc: { formula: PvsFormula, proofFile: FileDescriptor }): Promise<number> {
	if (desc && desc.formula && desc.proofFile) {
		const fname: string = desc2fname(desc.proofFile);
		const txt: string = await readFile(fname);
		const matchProoflite: RegExpMatchArray = proofliteDeclRegexp(desc.formula).exec(txt);
		if (matchProoflite) {
			const slice: string = txt.slice(0, matchProoflite.index);
			const lines: string[] = slice.split("\n");
			return lines.length;
		}
	}
	return 0;
}

/**
 * Utility function, returns the list of theorems defined in a given pvs file
 * @param desc Descriptor indicating filename, file extension, context folder, file content, and whether the file in question is the prelude (flag prelude)
 */
 export async function listTheorems (desc: { fileName: string, fileExtension: string, contextFolder: string, fileContent: string, prelude?: boolean, cache?: { theories?: TheoryDescriptor[] } }): Promise<FormulaDescriptor[]> {
	if (desc && desc.fileContent) {
		const theories: TheoryDescriptor[] = (desc.cache && desc.cache.theories) ? desc.cache.theories : listTheories(desc);
		const boundaries: { theoryName: string, from: number, to: number }[] = []; // slices txt to the boundaries of the theories
		if (theories) {
			const start: number = Date.now();
			const fileContent: string = desc.fileContent.replace(commentRegexp, ""); // first, remove all comments
			const slices: string[] = fileContent.split("\n");
			for (let i = 0; i < theories.length; i++) {
				boundaries.push({
					theoryName: theories[i].theoryName,
					from: theories[i].position.line,
					to: (i + 1 < theories.length) ? theories[i + 1].position.line : slices.length
				});
			}
			const formulaDescriptors: FormulaDescriptor[] = [];
			for (let i = 0; i < boundaries.length; i++) {
				const content: string = slices.slice(boundaries[i].from - 1, boundaries[i].to - 1).join("\n");
				if (content && content.trim()) {
					const regex: RegExp = theoremRegexp;
					let match: RegExpMatchArray = null;
					while (match = regex.exec(content)) {
						if (match.length > 1 && match[1]) {
							const formulaName: string = match[1];
							const slice: string = content.slice(0, match.index);
							const offset: number = (slice) ? slice.split("\n").length : 0;
							const line: number = boundaries[i].from + offset - 1;
							const isTcc: boolean = desc.fileExtension === ".tccs";
							let status: ProofStatus = "untried";
							// if (isTcc) {
							// 	const matchStatus: RegExpMatchArray = tccStatusRegExp.exec(slice);
							// 	if (matchStatus && matchStatus.length > 1 && matchStatus[1]) {
							// 		status = <ProofStatus> matchStatus[1];
							// 	}
							// } else {
							if (desc.prelude) {
								status = "proved";
							} else {
								// check if the .jprf file contains the proof status
								const theoryName: string = boundaries[i].theoryName;
								status = await getProofStatus({
									fileName: desc.fileName, 
									fileExtension: desc.fileExtension, 
									contextFolder: desc.contextFolder,
									formulaName,
									theoryName
								});
							}
							const fdesc: FormulaDescriptor = {
								fileName: desc.fileName,
								fileExtension: desc.fileExtension,
								contextFolder: desc.contextFolder,
								theoryName: boundaries[i].theoryName,
								formulaName,
								position: { line, character: 0 },
								status,
								isTcc
							}
							formulaDescriptors.push(fdesc);
						}
					}
				} else {
					console.error("Error while finding theory names :/");
				}
			}
			const stats: number = Date.now() - start;
			// console.log(`[languageUtils] listTheorems(${desc.fileName}) completed in ${stats}ms`);
			return formulaDescriptors;
		}
	}
	return [];
}

/**
 * @function findFormulaName
 * @description Utility function, finds the name of a theorem that immediately preceeds a given line
 * @param fileContent The text where the theory should be searched 
 * @param line The line in the document where search should end
 * @returns { string | null } The theory name if any is found, null otherwise
 */
export function findFormulaName(fileContent: string, line: number): string | null {
	if (fileContent) {
		const txt: string = fileContent.replace(commentRegexp, "");
		let text: string = txt.split("\n").slice(0, line + 1).join("\n");
		let candidates: string[] = [];
		// (?:\%.*\s)* removes comments
		const regexp: RegExp = formulaRegexp;
		let match: RegExpMatchArray = null;
		while(match = regexp.exec(text)) {
			if (match && match.length > 1 && match[1]) {
				candidates.push(match[1]);
			}
		}
		if (candidates.length > 0) {
			return candidates[candidates.length - 1];
		}
	}
	return null;
};

/**
 * @function findProofObligation
 * @description Utility function, finds the line of a proof obligation
 * @param txt The text where the proof obligation should be searched 
 * @returns { string | null } The theory name if any is found, null otherwise
 */
export function findProofObligation(formulaName: string, txt: string): number {
	const formula: string = formulaName.replace("?", "\\?");
	const regexp: RegExp = new RegExp(`\\b${formula}:\\s*OBLIGATION\\b`, "g");
	let match: RegExpMatchArray = regexp.exec(txt);
	if (match) {
		const trim: string = txt.substr(0, match.index);
		if (trim && trim.length > 0) {
			return trim.split("\n").length;
		}
	}
	return 0;
};



/**
 * Lists all theorems in a given context folder
 */
 export async function getContextDescriptor (contextFolder: string, opt?: { listTheorems?: boolean, includeTccs?: boolean, prelude?: boolean }): Promise<PvsContextDescriptor> {
	// console.log(`[language-utils] Generating context descriptor for ${contextFolder}...`);
	const response: PvsContextDescriptor = {
		fileDescriptors: {},
		contextFolder
	};
	const fileList: FileList = await listPvsFiles(contextFolder);
	if (fileList) {
		for (let i in fileList.fileNames) {
			const fname: string = path.join(contextFolder, fileList.fileNames[i]);
			// console.log(`[language-utils] Processing file ${fname}`);
			const desc: PvsFileDescriptor = await getFileDescriptor(fname, opt);
			response.fileDescriptors[fname] = desc;
		}
	}
	// console.log("[language-utils] Done");
	return response;
}

export async function getFileDescriptor (fname: string, opt?: { listTheorems?: boolean, includeTccs?: boolean, prelude?: boolean }): Promise<PvsFileDescriptor> {
	opt = opt || {};
	opt.listTheorems = (opt.listTheorems !== undefined) ? opt.listTheorems : true;
	const start: number = Date.now();
	const contextFolder: string = getContextFolder(fname);
	const fileName: string = getFileName(fname);
	const fileExtension: string = getFileExtension(fname);
	const response: PvsFileDescriptor = {
		theories: [],
		contextFolder,
		fileName,
		fileExtension
	};
	const pvsFileContent: string = await readFile(fname);
	const tccsFileContent: string = (opt.includeTccs) ? await readFile(path.join(contextFolder, `${fileName}.tccs`)) : null;
	// console.log(`[languageUtils.getFileDescriptor] listTheories(${fileName})`);
	const theories: TheoryDescriptor[] = listTheories({ fileName, fileExtension, contextFolder, fileContent: pvsFileContent });
	if (theories) {
		// if (opt.listTheorems) { console.log(`[languageUtils.getFileDescriptor] listTheorems(${fileName})`);	}
		const lemmas: FormulaDescriptor[] = 
			(opt.listTheorems) 
				? await listTheorems({ fileName, fileExtension, contextFolder, fileContent: pvsFileContent, prelude: opt?.prelude, cache: { theories } })
					: [];
		const tccs: FormulaDescriptor[] = 
			(opt.listTheorems && fileExtension !== ".tccs" && tccsFileContent) 
				? await listTheorems({ fileName, fileExtension: ".tccs", contextFolder, fileContent: tccsFileContent, prelude: opt?.prelude })
					: [];
		const descriptors: FormulaDescriptor[] = lemmas.concat(tccs);
		// console.log(`[language-utils] Processing ${theories.length} theories`);
		for (let i = 0; i < theories.length; i++) {
			const theoryName: string = theories[i].theoryName;
			// console.log(`[language-utils] Processing theory ${theoryName}`);
			const position: Position = theories[i].position;
			const theoryDescriptor: TheoryDescriptor = {
				fileName, fileExtension, contextFolder, theoryName, position, 
				theorems: (descriptors && descriptors.length) ? descriptors.filter((desc: FormulaDescriptor) => {
					return desc.theoryName === theoryName;
				}) : []
			}
			// console.log(`[language-utils] Done`);
			response.theories.push(theoryDescriptor);
		}
	}
	const stats: number = Date.now() - start;
	// console.log(`[languageUtils.getFileDescriptor] File descriptor for ${fname} created in ${stats}ms`);
	return response;
}

export async function renameFormulaInProofFile (formula: PvsFormula, newInfo: { newFormulaName: string, newShasum?: string }): Promise<boolean> {
	if (formula && newInfo && newInfo.newFormulaName) {
		const fname: string = desc2fname({
			fileName: formula.fileName,
			fileExtension: ".jprf",
			contextFolder: formula.contextFolder
		});
		const fdesc: ProofFile = await readJprfProofFile(fname);
		// check if file contains a proof for the given formula
		const key: string = `${formula.theoryName}.${formula.formulaName}`;
		if (fdesc && fdesc[key] && fdesc[key].length) {
			const newKey: string = `${formula.theoryName}.${newInfo.newFormulaName}`;
			// we are updating only the default proof, this might be changed in the future
			const pdesc: ProofDescriptor = fdesc[key][0];
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
			const keys: string[] = Object.keys(fdesc);
			for (let i = 0; i < keys.length; i++) {
				if (fdesc[keys[i]] && fdesc[keys[i]].length && fdesc[keys[i]][0].info) {
					fdesc[keys[i]][0].info.shasum = newInfo.newShasum;
				}
			}
			// add new key
			fdesc[newKey] = [ pdesc ];			
			// write to file
			const newContent: string = JSON.stringify(fdesc, null, " ");
			return await writeFile(fname, newContent);
		}
	}
	return false;
}
export async function renameTheoryInProofFile (theory: PvsTheory, newInfo: { newTheoryName: string, newShasum?: string }): Promise<boolean> {
	if (theory && newInfo && newInfo.newTheoryName) {
		const fname: string = desc2fname({
			fileName: theory.fileName,
			fileExtension: ".jprf",
			contextFolder: theory.contextFolder
		});
		const fdesc: ProofFile = await readJprfProofFile(fname);
		// update all proofs
		// check if file contains a proof for the given formula
		if (fdesc) {
			const keys: string[] = Object.keys(fdesc);
			if (keys && keys.length) {
				const newFdesc: ProofFile = {};
				for (let i = 0; i < keys.length; i++) {
					const key: string = keys[i];
					// we are updating only the default proof, this might be changed in the future
					const pdesc: ProofDescriptor = fdesc[key][0];
					if (pdesc.info) {
						pdesc.info.theory = newInfo.newTheoryName;
						pdesc.info.shasum = newInfo.newShasum || pdesc.info.shasum;
					}
					const newKey: string = `${newInfo.newTheoryName}.${pdesc.info.formula}`;
					if (pdesc.proofTree) {
						pdesc.proofTree.name = newKey;
					}
					newFdesc[newKey] = [ pdesc ];
				}	
				// write to file
				const newContent: string = JSON.stringify(newFdesc, null, " ");
				const newFname: string = desc2fname({
					fileName: (theory.fileName === theory.theoryName) ? newInfo.newTheoryName : theory.fileName,
					fileExtension: ".jprf",
					contextFolder: theory.contextFolder		
				});
				const fileAlreadyExists: boolean = await fileExists(newFname);
				const success: boolean = (fileAlreadyExists) ? await writeFile(fname, newContent)
					: await writeFile(newFname, newContent);
				if (success && !fileAlreadyExists && fname !== newFname) {
					deleteFile(fname);
				}
				return success;
			}
		}
	}
	return false;
}

export interface WorkspaceSummaryItem extends PvsTheory {
	ok: number,
	miss: number,
	total: number,
	ms: number	
}
export type WorkspaceSummary = {
	total: number, 
	contextFolder: string,
	theories: WorkspaceSummaryItem[]
}
export function makeWorkspaceSummary (desc: WorkspaceSummary): string {
	const header: string = "Proof summary";
	const workspaceName: string = getContextFolderName(desc.contextFolder);
	let ans: string = `${header} for workspace ${workspaceName}\n`;
	let nProved: number = 0;
	let nMissed: number = 0;
	let totTime: number = 0;
	let libraries: { [name: string]: boolean } = {};
	for (let i = 0; i < desc.theories.length; i++) {
		const theory: WorkspaceSummaryItem = desc.theories[i];
		libraries[theory.contextFolder] = true;

		const points: number = (64 - theory.theoryName.length) > 0 ? 64 - theory.theoryName.length : 0;
		const overall: string = (theory.miss) ? `${icons.sparkles} partly proved [${theory.ok}/${theory.total}]`
			: `${icons.checkmark}  fully proved [${theory.ok}/${theory.total}]`;
		const spaces: number = (20 - overall.length) > 0 ? 20 - overall.length : 0;
		nProved += theory.ok;
		nMissed += theory.miss;
		ans += `\n\t${theory.theoryName}` + ".".repeat(points) + " " + overall + " ".repeat(spaces) + `(${(+theory.ms / 1000).toFixed(3)} s)`;
		totTime += theory.ms;
	}
	ans += `\n\nWorkspace ${workspaceName} totals: ${desc.total} formulas, ${nProved + nMissed} attempted, ${nProved} succeeded (${(+totTime / 1000).toFixed(3)} s)`;
// 	ans += `\n
// *** Grand Totals: ${nProved} proofs / ${desc.total} formulas. Missed: ${nMissed} formulas.
// *** Number of libraries: ${Object.keys(libraries).length}`;	
	return ans;
}


export interface TheorySummaryItem {
	theoryName: string, 
	formulaName: string, 
	status: ProofStatus, 
	ms: number 
}
export type TheorySummary = { 
	total: number, 
	tccsOnly?: boolean, 
	theoryName: string,
	theorems: TheorySummaryItem[]
}
export function makeTheorySummary (desc: TheorySummary): string {
	const header: string = desc.tccsOnly ? "TCCs summary" : "Proof summary";
	let ans: string = `${header} for theory ${desc.theoryName}\n`;
	let nProved: number = 0;
	let totTime: number = 0;
	let importChainFlag: boolean = false;
	for (let i = 0; i < desc.theorems.length; i++) {
		if (desc.theorems[i].theoryName !== desc.theoryName && !importChainFlag) {
			importChainFlag = true;
			ans += `\n\t%-- importchain`;
		}
		const formulaName: string = desc.theorems[i].formulaName;
		const status: ProofStatus = desc.theorems[i].status;
		const ms: number = desc.theorems[i].ms;

		const points: number = (64 - formulaName.length) > 0 ? 64 - formulaName.length : 0;
		const spaces: number = (20 - status.length) > 0 ? 20 - status.length : 0;

		if (isProved(status)) { nProved++; }
		totTime += ms;

		ans += `\n\t${formulaName}` + ".".repeat(points) + getIcon(status) + " " + status + " ".repeat(spaces) + `(${(+ms / 1000)} s)`;
	}
	ans += `\n\nTheory ${desc.theoryName} totals: ${desc.total} formulas, ${desc.theorems.length} attempted, ${nProved} succeeded (${+(totTime / 1000).toFixed(3)} s)`;
	return ans;
}

export function decodePvsLibraryPath (pvsLibraryPath: string): string[] {
	const libs: string[] = (pvsLibraryPath) ? pvsLibraryPath.split(":").map((elem: string) => {
		return elem.trim();
	}) : [];
	return libs.filter((elem: string) => {
		return elem !== "";
	}).map((elem: string) => {
		return elem.endsWith("/") ? elem : `${elem}/`;
	});
}
export function createPvsLibraryPath (libs: string[]): string {
	if (libs && libs.length) {
		return libs.filter((elem: string) => {
			return elem.trim() !== "";
		}).map((elem: string) => {
			return elem.trim().endsWith("/") ? elem.trim() : `${elem.trim()}/`;
		}).join(":");
	}
	return "";
}

/**
 * Utility function, appends a proof summary at the end of a given file
 * @param fname Name of the summary file
 * @param summary The summary to be appended
 */
export async function appendSummary (fname: string, summary: string): Promise<boolean> {
	if (fname && summary) {
		const content: string = await readFile(fname);
		const newContent: string = (content && content.trim()) ? content + `\n\n${summary}` : summary;
		return await writeFile(fname, newContent);
	}
	return false;
}
/**
 * Utility function, removes a proof summary from a given file
 * @param fname Name of the summary file
 * @param theoryName name of the theory whose summary should be removed
 */
export async function removeSummary (fname: string, theoryName: string): Promise<boolean> {
	if (fname && theoryName) {
		fname = decodeURIComponents(fname);
		const success: boolean = await fileExists(fname);
		if (success) {
			const content: string = await readFile(fname);
			if (content) {
				const regex: RegExp = new RegExp(`\\bProof summary for theory ${theoryName}\\s[\\s\\w\\W]+\\bTheory ${theoryName}\\s.*`, "g");
				const newContent: string = content.replace(regex, "");
				return await writeFile(fname, newContent);
			}
		}
	}
	return false;
}
/**
 * Utility function, checks if a summary is present in a given file
 * @param fname Name of the summary file
 * @param theoryName name of the theory whose summary should be removed
 */
export async function containsSummary (fname: string, theoryName: string): Promise<boolean> {
	if (fname && theoryName) {
		fname = decodeURIComponents(fname);
		const success: boolean = await fileExists(fname);
		if (success) {
			const content: string = await readFile(fname);
			if (content) {
				const regex: RegExp = new RegExp(`\\bProof summary for theory ${theoryName}\\s[\\s\\w\\W]+\\bTheory ${theoryName}\\s.*`, "g");
				return regex.test(content);
			}
		}
	}
	return false;
}
/**
 * Utility function, saves a summary for a given theory in the given file
 * @param fname Name of the summary file
 * @param theoryName name of the theory whose summary should be saved
 * @param summary The summary to be saved in the file
 */
export async function saveSummary (fname: string, theoryName: string, summary: string): Promise<boolean> {
	if (fname && theoryName && summary) {
		fname = decodeURIComponents(fname);
		const success: boolean = await fileExists(fname);
		if (success) {
			// deleted any previous version of the summary
			await removeSummary(fname, theoryName);
		}
		// append the new summary
		return await appendSummary(fname, summary);
	}
	return false;
}

/**
 * Utility function, creates the command for downloading the list of pvs versions
 */
export function lsPvsVersions (): string {
	const osName: { version?: string, error?: string } = getOs();
	if (osName && osName.version) {
		const preferredVersion: string = "7.1.0";//"ge7a69672"; //"g762f82aa"; //"ga3f9dbb7";//"g03fe2100";
		const shellCommand: ShellCommand = getDownloadCommand(pvsDownloadUrl);
		if (shellCommand?.cmd) {
			let lsCommand: string = `${shellCommand.cmd} ${shellCommand.args?.join(" ") }`;
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

/**
 * Utility function, parses the result of lsPvsVersions
 */
 export function parseLsPvsVersions (str: string): PvsDownloadDescriptor[] {
	 if (str) {
		const res: string = str?.toLocaleString().trim();
		const elems: string[] = res?.split("\n");
		const versions: PvsDownloadDescriptor[] = elems?.map((fileName: string) => {
			const match: RegExpMatchArray = /pvs-?([\d\.\-]+)\-\w+/.exec(fileName);
			const version: string = (match && match.length > 1) ? match[1].replace(/\-/g,".") : null;
			const url: string = (pvsDownloadUrl.endsWith("/") ? pvsDownloadUrl : pvsDownloadUrl + "/") + fileName;
			return { url, fileName, version };
		});
		return versions;
	}
	return null;
}
