import * as path from 'path';

// -- flags to control what is displayed
const VERBOSE: boolean = true;

// sandbox folder with the examples
export const sandboxExamples: string = path.join(__dirname, "sandbox");

// load configuration indicating the installation folder of pvs
export const configFile: string = path.join(__dirname, "test.config");
console.info(`Loading configuration file ${configFile}`);

// utility functions
export function label(l: string): void {
	if (VERBOSE) {
		console.info(`\n==== ${l} ====`);
	}
}
export function log(...args:any) {
	if (VERBOSE) {
		console.log();
		console.log(args);
	}
}
export function dir(...args:any) {
	if (VERBOSE) {
		console.log();
		console.dir(args, { depth: null });
	}
}