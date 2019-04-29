import * as fs from 'fs';

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