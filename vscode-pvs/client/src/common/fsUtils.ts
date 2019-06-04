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

export async function readFile(path: string): Promise<string> {
	return await fs.readFileSync(path).toString('utf8');
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

export async function writeFileSync(path: string, content: string): Promise<void> {
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
export function getPathname(path: string): string {
	return path.split("/").slice(0, -1).join("/").replace("file://", "");
}
export function isPvsFile(fileName: string): boolean {
	return fileName.endsWith('.pvs') || fileName.endsWith('.tccs');
}
// export function rmDir(path: string) {
// 	return rimraf.sync(path);
// }
export function dirExists(path: string) {
	return fs.existsSync(path);
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
