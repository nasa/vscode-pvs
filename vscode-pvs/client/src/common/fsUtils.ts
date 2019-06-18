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

export async function stat(file: string): Promise<fs.Stats> {
	return new Promise<fs.Stats>((resolve, reject) => {
		fs.stat(file, (error, stat) => {
			// ignore errors for now
			resolve(stat);
		});
	});
};

export async function readDir(pvsContextFolder: string): Promise<string[]> {
	return new Promise<string[]>((resolve, reject) => {
		fs.readdir(pvsContextFolder, (error, children) => {
			// ignore errors for now
			resolve(children || []);
		});
	});
}

export async function readFile(path: string): Promise<string | null> {
	try {
		return await fs.readFileSync(path).toString('utf8');
	} catch (fileReadError) {
		console.error(fileReadError);
	}
	return null;
}
export async function deletePvsCache(contextFolder: string): Promise<boolean> {
	try {
		const cacheFolder: string = path.join(contextFolder, "pvsbin");
		const fileList: string[] = await fs.readdirSync(cacheFolder);
		fileList.forEach(file => {
			fs.unlinkSync(path.join(cacheFolder, file));
		});
	} catch (deleteError) {
		return false;
	}
	return true;
}
export async function writeFile(path: string, content: string): Promise<void> {
	return await fs.writeFileSync(path, content);
}

export async function fileExists(path: string): Promise<boolean> {
	return await fs.existsSync(path);
}

export function getFilename(fileName: string, opt?: { removeFileExtension?: boolean }): string {
	opt = opt || {};
	const pathlessFileName = fileName.includes("/") ? fileName.split("/").slice(-1)[0] : fileName;
	if (opt.removeFileExtension &&
			(pathlessFileName.endsWith(".pvs") || pathlessFileName.endsWith(".tccs"))) {
		return pathlessFileName.split(".").slice(0, -1).join(".");
	}
	return pathlessFileName;
}
export function removeFileExtension(fileName: string): string {
	return fileName.split(".").slice(0, -1).join(".");
}
export function getFileExtension(fileName: string): string {
	return `.${fileName.split(".").slice(-1).join(".")}`;
}
export function getContextFolder(path: string): string {
	return path.split("/").slice(0, -1).join("/").replace("file://", "");
}
export function isPvsFile(fileName: string): boolean {
	return fileName.endsWith('.pvs') || fileName.endsWith('.tccs');
}
// export function rmDir(path: string) {
// 	return rimraf.sync(path);
// }
export function dirExists(path: string): boolean {
	let ans: boolean = false;
	try {
		ans = fs.existsSync(path);
	} catch (readError) {
		console.error(readError);
	} finally {
		return ans;
	}
}

/**
 * Utility function, returns the list of pvs files contained in a given folder
 * @param folder Path to a folder
 */
export async function listPvsFiles (folder: string): Promise<FileList> {
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
