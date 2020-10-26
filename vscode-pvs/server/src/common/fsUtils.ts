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
import { FileDescriptor, FileList} from '../common/serverInterface';
import { execSync } from 'child_process';
import * as crypto from 'crypto';


export const HOME_DIR: string = require('os').homedir();
export const MAX_RECURSION: number = 4;
// nodeJS does not support tilde expansion for the home folder
export function tildeExpansion(path: string): string {
	if (path && (path.startsWith("~/") || path === "~")) {
		path = path.replace("~", HOME_DIR);
	}
	return path;
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
export async function readFile(fname: string): Promise<string> {
	if (fname) {
		fname = fname.replace("file://", "");
		fname = tildeExpansion(fname);
		try {
			const exists: boolean = await fileExists(fname);
			if (exists) {
				const data: Buffer = fs.readFileSync(fname);
				if (data) {
					return data.toLocaleString();
				}
				return "";
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
		if (contextFolder && fs.existsSync(contextFolder)) {
			execSync(`rm -r ${contextFolder}`);
		}
	} catch (deleteError) {
		return false;
	}
	return true;
}
export function deleteBinFiles(binFolder: string, opt?: { removePvsbinFolder?: boolean }): boolean {
	opt = opt || {};
	try {
		if (fs.existsSync(binFolder)) {
			if (opt.removePvsbinFolder) {
				deleteFolder(binFolder);
			} else {
				execSync(`rm ${binFolder}/*.bin 2> /dev/null`); // 2> /dev/null suppresses 'file not found' messages
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
export async function createFolder(path: string): Promise<void> {
	if (!fs.existsSync(path)){
		fs.mkdirSync(path);
	}
}
export async function writeFile(fname: string, content: string, opt?: { append?: boolean }): Promise<boolean> {
	opt = opt || {};
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
			fs.writeFileSync(fname, content);
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
export function getFileName(fname: string): string {
	if (fname) {
		fname = fname.replace("file://", "");
		fname = fname.includes("/") ? fname.split("/").slice(-1)[0] : fname;
		fname = fname.includes(".") ? fname.split(".").slice(0, -1).join(".") : fname;
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
	if (txt) {
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

export function getOs (): string {
	if (process.platform === 'linux' || process.platform === 'freebsd' || process.platform === 'openbsd' || process.platform === 'sunos' || process.platform === 'aix') {
		return 'Linux';
	} else if (process.platform === 'darwin') {
		return 'MacOSX';
	}
	return process.platform;
}

export function decodeURIComponents (desc) {
	if (desc) {
		if (typeof desc === "string") {
			return decodeURIComponent(desc);
		}
		// else
		const keys: string[] = [ "fileName", "fileExtension", "contextFolder"];
		for (let i = 0; i < keys.length; i++) {
			if (desc[keys[i]] && typeof desc[keys[i]] === "string") {
				desc[keys[i]] = decodeURIComponent(desc[keys[i]]);
			}
		}
	}
	return desc;
}

// constants
export const logFileExtension: string = ".pr";

export function getDownloader (): string {
	const candidates: string[] = [ "curl", "wget" ];
	for (let i = 0; i < candidates.length; i++) {
		if (execSync(`which ${candidates[i]}`)) {
			return candidates[i];
		}
	}
	return null;
} 

export function getSourceControl (): "git" | null {
	const res: Buffer = execSync(`which git`);
	if (res) {
		const ans: string = res.toLocaleString();
		if (ans.trim().endsWith("git")) {
			return "git";
		}
	}
	return null;
}

export function cloneCommand (url: string, opt?: { update?: boolean, basePath?: string, branch?: string }): string {
	opt = opt || {};
	let gitCommand: string = (opt.update) ? `cd nasalib && git pull` : `git clone ${url} nasalib`;
	if (opt.basePath) {
		gitCommand = `cd "${opt.basePath}" && ` + gitCommand;
	}
	if (opt.branch && !opt.update) {
		gitCommand += ` -b "${opt.branch}"`;
	}
	return gitCommand;
}

export function downloadCommand (url: string, opt?: { out?: string }): string {
	opt = opt || {};
	const downloader: string = getDownloader();
	if (downloader) {
		const cmd: string = (downloader === "curl") ? `${downloader} -L ` : downloader; // -L allows curl to follow redirect. wget automatically follows up to 20 redirect. Redirects may happen when downloading files from github.
		return opt.out ? `${cmd} -o ${opt.out} ${url}` : `${cmd} ${url}`;
	}
	return null;
}

export const pvsFolderName: string = "pvs-7.1.0";

export async function getNodeJsVersion (): Promise<{ version?: string, error?: string }> {
	const cmd: string = "node --version";
	try {
		const buf: Buffer = execSync(cmd);
		if (buf) {
			const info: string = buf.toLocaleString();
			console.log(`[pvs-server] ${cmd}\n `, info);
			const match: RegExpMatchArray = /(v?[\d\.]+)/g.exec(info);
			if (match && match.length > 1) {
				return { version: match[1] };
			} else {
				return { error: info };
			}
		} else {
			console.log("[pvs-server] Missing dependency: node (please download node from https://nodejs.org/)");
		}
	} catch (error) {
		console.log("[pvs-server]", error);
		return { error };
	}
	return null;
}