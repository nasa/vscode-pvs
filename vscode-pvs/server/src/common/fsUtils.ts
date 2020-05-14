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
import { FileList, ProofFile } from '../common/serverInterface';

const HOME_DIR: string = require('os').homedir();
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
export async function readFile(fname: string): Promise<string | null> {
	if (fname) {
		fname = fname.replace("file://", "");
		fname = tildeExpansion(fname);
		const exists: boolean = await fileExists(fname);
		if (exists) {
			try {
				const data: Buffer = fs.readFileSync(fname);
				if (data) {
					return data.toLocaleString();
				}
				return null;
				// return new Promise<string>((resolve, reject) => {
				// 	fs.readFile(path, (error: NodeJS.ErrnoException, data: Buffer) => {
				// 		if (data) {
				// 			resolve(data.toString('utf8'));
				// 		} else {
				// 			resolve(null);
				// 		}
				// 	});
				// });
			} catch (error) {
				console.error(`[fs-utils] Error while reading file ${fname}`, error);
				return null;
			}
		}
	}
	return null;
}
// TODO: move this function to languageUtils
export async function readProofFile (fname: string): Promise<ProofFile> {
	let proofFile: ProofFile = null;
	fname = fname.replace("file://", "");
	fname = tildeExpansion(fname);
	try {
		// check if a jprf file exists with the proof
		const fnameExists: boolean = await fileExists(fname);
		if (fnameExists) {
			const content: string = await readFile(fname);
			if (content) {
				proofFile = JSON.parse(content);
			}
		}
	} catch (jsonError) {
		console.warn(`[fs-utils] Warning: unable to parse proof file ${fname}`, jsonError);
	} finally {
		return proofFile;
	}
}
export function deleteFile(fname: string): boolean {
	try {
		fs.unlinkSync(fname);
	} catch (deleteError) {
		return false;
	}
	return true;
}
export function deleteFolder(contextFolder: string): boolean {
	try {
		if (fs.existsSync(contextFolder)) {
			execSync(`rm -r ${contextFolder}`);
		}
	} catch (deleteError) {
		return false;
	}
	return true;
}
export function deletePvsCache(contextFolder: string): Promise<boolean> {
	try {
		// console.log(`Deleting cache for context ${contextFolder}`);
		if (contextFolder) {
			contextFolder = tildeExpansion(contextFolder);
			// console.log(`Deleting cache for context ${contextFolder}`);
			const cacheFolder: string = path.join(contextFolder, "pvsbin");
			deleteFolder(cacheFolder);
			// const pvsBinFiles: string[] = fs.readdirSync(cacheFolder);
			// // console.log(pvsBinFiles);
			// pvsBinFiles.forEach(file => {
			// 	deleteFile(path.join(cacheFolder, file));
			// });
			// console.log(`removing ${path.join(contextFolder, ".pvscontext")}`);
			deleteFile(path.join(contextFolder, ".pvscontext"));
			// console.log(`reading folder ${contextFolder}`);
			const tccFiles: string[] = fs.readdirSync(contextFolder);
			// console.log(tccFiles);
			if (tccFiles) {
				// console.log(tccFiles);
				tccFiles.filter(name => {
					// console.log(name);
					return name.endsWith(".tccs");
				}).forEach(file => {
					// console.log(`deleting ${file}`);
					deleteFile(path.join(cacheFolder, file));
				});
			}
		}
	} catch (deleteError) {
		return Promise.resolve(false);
	}
	return Promise.resolve(true);
}
export async function createFolder(path: string): Promise<void> {
	if (!fs.existsSync(path)){
		fs.mkdirSync(path);
	}
}
export async function writeFile(fname: string, content: string): Promise<boolean> {
	if (fname) {
		try {
			fname = tildeExpansion(fname);
			fs.writeFileSync(fname, content);
		} catch (error) {
			console.error(`[fs-utils] Error while writing file ${fname}`, error);
			return false;
		}
	}
	return true;
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
export function isPvsFile(fname: string): boolean {
	if (fname) {
		return fname.endsWith('.pvs') || fname.endsWith('.tccs') || fname.endsWith('.ppe') || fname.endsWith('.pr')
				|| fname.endsWith('.hpvs');
	}
	return false;
}
export async function fileExists(fname: string): Promise<boolean> {
	return pathExists(fname);
}
export async function dirExists(contextFolder: string): Promise<boolean> {
	return pathExists(contextFolder);
}
export async function pathExists(path: string): Promise<boolean> {
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

import * as crypto from 'crypto';
import { execSync } from 'child_process';

export function get_fresh_id(): string {
	// This may be overkill, a simple call to random is probably good enough.
	return crypto.createHash('sha256').update(Math.random().toString(36)).digest('hex');
}

export function fname2desc (fname: string): { fileName: string, fileExtension: string, contextFolder: string } {
	const fileName: string = getFileName(fname);
	const fileExtension: string = getFileExtension(fname);
	const contextFolder: string = getContextFolder(fname);
	return { fileName, fileExtension, contextFolder };
}

export function desc2fname (desc: { fileName: string, fileExtension: string, contextFolder: string }): string {
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
		const keys: string[] = Object.keys(desc);
		for (let i = 0; i < keys.length; i++) {
			if (typeof desc[keys[i]] === "string") {
				desc[keys[i]] = decodeURIComponent(desc[keys[i]]);
			}
		}
	}
	return desc;
}


// constants
export const pvsbinFolder: string = "pvsbin";
export const logFileExtension: string = ".pr";
