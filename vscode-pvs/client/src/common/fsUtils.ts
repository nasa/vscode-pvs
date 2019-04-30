import * as fs from 'fs';
// import * as rimraf from 'rimraf';

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

export async function writeFileSync(path: string, content: string): Promise<void> {
	return await fs.writeFileSync(path, content);
}

export async function fileExists(path: string): Promise<boolean> {
	return await fs.existsSync(path);
}

export function getFilename(fileName: string, opt?: { removeFileExtension?: boolean }) {
	opt = opt || {};
	const pathlessFileName = fileName.includes("/") ? fileName.split("/").slice(-1)[0] : fileName;
	if (opt.removeFileExtension &&
			(pathlessFileName.endsWith(".pvs") || pathlessFileName.endsWith(".tccs"))) {
		return pathlessFileName.split(".").slice(0, -1).join(".");
	}
	return pathlessFileName;
}
export function getPathname(path: string) {
	return path.split("/").slice(0, -1).join("/").replace("file://", "");
}
export function isPvsFile(fileName: string) {
	return fileName.endsWith('.pvs') || fileName.endsWith('.tccs');
}
// export function rmDir(path: string) {
// 	return rimraf.sync(path);
// }
export function dirExists(path: string) {
	return fs.existsSync(path);
}