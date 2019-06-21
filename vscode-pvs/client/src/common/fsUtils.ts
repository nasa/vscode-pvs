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
import { FileList } from '../common/serverInterface';

const HOME_DIR: string = require('os').homedir();
// nodeJS does not support tilde expansion for the home folder
export function tildeExpansion(path: string): string {
	if (path && path.startsWith("~")) {
		path = path.replace("~", HOME_DIR);
	}
	return path;
}
export async function stat(fileName: string): Promise<fs.Stats> {
	if (fileName) {
		fileName = tildeExpansion(fileName);
		return new Promise<fs.Stats>((resolve, reject) => {
			fs.stat(fileName, (error, stat) => {
				// ignore errors for now
				resolve(stat);
			});
		});
	}
	return null;
};
export async function readDir(pvsContextFolder: string): Promise<string[]> {
	if (pvsContextFolder) {
		pvsContextFolder = tildeExpansion(pvsContextFolder);
		return new Promise<string[]>((resolve, reject) => {
			fs.readdir(pvsContextFolder, (error, children) => {
				// ignore errors for now
				resolve(children || []);
			});
		});
	}
	return null;
}
export async function readFile(path: string): Promise<string | null> {
	if (path) {
		path = tildeExpansion(path);
		try {
			return await fs.readFileSync(path).toString('utf8');
		} catch (fileReadError) {
			console.error(fileReadError);
		}
	}
	return null;
}
export async function deletePvsCache(pvsContextFolder: string): Promise<boolean> {
	try {
		if (pvsContextFolder) {
			const cacheFolder: string = path.join(pvsContextFolder, "pvsbin");
			const fileList: string[] = await fs.readdirSync(cacheFolder);
			fileList.forEach(file => {
				fs.unlinkSync(path.join(cacheFolder, file));
			});
		}
	} catch (deleteError) {
		return false;
	}
	return true;
}
export async function writeFile(path: string, content: string): Promise<void> {
	if (path) {
		await fs.writeFileSync(path, content);
	}
}
export function getFilename(fileName: string, opt?: { removeFileExtension?: boolean }): string {
	if (fileName) {
		opt = opt || {};
		const pathlessFileName = fileName.includes("/") ? fileName.split("/").slice(-1)[0] : fileName;
		if (opt.removeFileExtension &&
				(pathlessFileName.endsWith(".pvs") || pathlessFileName.endsWith(".tccs"))) {
			return pathlessFileName.split(".").slice(0, -1).join(".");
		}
		return pathlessFileName;
	}
	return null;
}
export function removeFileExtension(fileName: string): string {
	if (fileName) {
		return fileName.split(".").slice(0, -1).join(".");
	}
	return null;
}
export function getFileExtension(fileName: string): string {
	if (fileName) {
		return `.${fileName.split(".").slice(-1).join(".")}`;
	}
	return null;
}
export function getContextFolder(path: string): string {
	if (path) {
		return path.split("/").slice(0, -1).join("/").replace("file://", "");
	}
	return null;
}
export function isPvsFile(fileName: string): boolean {
	if (fileName) {
		return fileName.endsWith('.pvs') || fileName.endsWith('.tccs');
	}
	return false;
}
export async function fileExists(path: string): Promise<boolean> {
	if (path) {
		path = tildeExpansion(path);
		return await fs.existsSync(path);
	}
	return null;
}
export function dirExists(path: string): boolean {
	let ans: boolean = false;
	if (path) {
		path = tildeExpansion(path);
		try {
			ans = fs.existsSync(path);
		} catch (readError) {
			console.error(readError);
		} finally {
			return ans;
		}
	}
	return false;
}
/**
 * Utility function, returns the list of pvs files contained in a given folder
 * @param folder Path to a folder containing pvs files.
 * @returs List of pvs files, as a structure FileList. Null if the folder does not exist.
 */
export async function listPvsFiles (folder: string): Promise<FileList> {
	if (folder) {
		const children: string[] = await readDir(folder);
		const fileList: FileList = {
			fileNames: children.filter((fileName) => {
				return fileName.endsWith(".pvs") 
						&& !fileName.startsWith("."); // this second part is necessary to filter out temporary files created by pvs
			}),
			pvsContextFolder: folder
		};
		return fileList;
	}
	return null;
}
