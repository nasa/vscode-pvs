import * as fsUtils from "../server/src/common/fsUtils";
import * as test from "./test-constants";
import { PvsResponse, PvsResult } from "../server/src/common/pvs-gui";
import { PvsProxy } from '../server/src/pvsProxy'; // XmlRpcSystemMethods
import { label, configFile, sandboxExamples,
	stever, steverFiles, pillbox, pillboxFiles, pvsioweb, pvsiowebFiles, pvsiowebFolders,
	dependable_plus_safe } from './test-utils';
import * as os from 'os';
import * as path from 'path';

//----------------------------
//   Test cases for parser
//----------------------------
describe("pvs-parser", () => {
	let pvsProxy: PvsProxy = null;
	beforeAll(async () => {
		const config: string = await fsUtils.readFile(configFile);
		const content: { pvsPath: string } = JSON.parse(config);
		// console.log(content);
		const pvsPath: string = content.pvsPath;
		// log("Activating xmlrpc proxy...");
		pvsProxy = new PvsProxy(pvsPath, { externalServer: true });
		await pvsProxy.activate({ debugMode: false, showBanner: false }); // this will also start pvs-server

		// delete pvsbin files and .pvscontext
		await fsUtils.cleanBin(sandboxExamples);
		await fsUtils.cleanBin(stever);
		await fsUtils.cleanBin(pillbox);
		for (let i = 0; i < pvsiowebFolders.length; i++) {
			await fsUtils.cleanBin(path.join(pvsioweb, pvsiowebFolders[i]));
		}
		await fsUtils.cleanBin(dependable_plus_safe);

		console.log("\n----------------------");
		console.log("test-parser");
		console.log("----------------------");
	});
	afterAll(async () => {
		await pvsProxy.killPvsServer();
		await pvsProxy.killPvsProxy();
		// delete pvsbin files and .pvscontext
		await fsUtils.cleanBin(sandboxExamples);
		await fsUtils.cleanBin(stever);
		await fsUtils.cleanBin(pillbox);
		for (let i = 0; i < pvsiowebFolders.length; i++) {
			await fsUtils.cleanBin(path.join(pvsioweb, pvsiowebFolders[i]));
		}
		await fsUtils.cleanBin(dependable_plus_safe);
	});

	it(`can parse file`, async () => {
		label(`can parse file`);
		// Need to clear-theories, in case rerunning with the same server.
		await pvsProxy.lisp("(clear-theories t)");

		const response: PvsResponse = await pvsProxy.parseFile({ fileName: "sqrt", fileExtension: ".pvs", contextFolder: sandboxExamples }, { test: true });
		// console.dir(response);
		expect(response).toBeDefined();
		expect(response.result).toBeDefined();
		// on MacOS, stats are not provided because we are using the Emacs interface to interact with the parser
		if (os.platform() !== "darwin") {
			expect(response.result).toEqual(test.parse1_result);
		}
		expect(response.error).not.toBeDefined();
	}, 4000);

	it(`can report parse errors`, async () => {
		label(`can report parse errors`);

		const response: PvsResult = await pvsProxy.parseFile({ fileName: "lib0", fileExtension: ".pvs", contextFolder: sandboxExamples }, { test: true });
		// console.dir(response);
		const exp = {
			code: 1,
			message: 'Parser error',
			data: {
				error_string: 'Found \'[#\' when expecting \'END\'\nIn file /home/owre/vscode-pvs/server/test/sandbox/lib0.pvs (line 3, col 14)',
				file_name: '/home/owre/vscode-pvs/server/test/sandbox/lib0.pvs',
				place: [3, 14, 3, 14]
			}
		};
		expect(response.error).toBeDefined();
		expect(response.error.data).toBeDefined();
		// legacy apis provide only the position of the error (as opposed to the range) because they use the Emacs interface for the parser
		// if (os.platform() === "darwin") {
			expect(response.error.data.place).toEqual(exp.data.place.slice(0, 2));
		// } else {
		// 	expect(response.error.data.place).toEqual(exp.data.place);
		// }
		expect(response.error.data.error_string.startsWith(exp.data.error_string.split("\n")[0])).toBeTruthy();
	}, 100000);

	it(`can parse file with inline declarations`, async () => {
		label(`can parse file with inline declarations`);
		// Need to clear-theories, in case rerunning with the same server.
		await pvsProxy.lisp("(clear-theories t)");

		const response: PvsResponse = await pvsProxy.parseFile({ fileName: "test", fileExtension: ".pvs", contextFolder: sandboxExamples }, { test: true });
		// console.dir(response);
		expect(response).toBeDefined();
	}, 100000);

	it(`can parse file when filename contains '.'`, async () => {
		label(`can parse file when filename contains '.'`);

		let response: PvsResponse = await pvsProxy.parseFile({ fileName: "alaris2lnewmodes.pump", fileExtension: ".pvs", contextFolder: sandboxExamples }, { test: true });
		// console.dir(response);
		expect(response).toBeDefined();
		expect(response.result).toBeDefined();
		// on MacOS, stats are not provided because we are using the Emacs interface to interact with the parser
		if (os.platform() !== "darwin") {
			expect(response.result).toEqual(test.parse2_result);
		}
		expect(response.error).not.toBeDefined();
	}, 100000);

	it(`can parse files in folders whose name contains utf8 symbols`, async () => {
		label(`can parse files in folders whose name contains utf8 symbols`);

		const response: PvsResponse = await pvsProxy.parseFile({
			fileName: "helloworld", 
			fileExtension: ".pvs", 
			contextFolder: dependable_plus_safe
		}, { test: true });
		// console.dir(response);
		expect(response).toBeDefined();
		expect(response.result).toBeDefined();
		// on MacOS, stats are not provided because we are using the Emacs interface to interact with the parser
		if (os.platform() !== "darwin") {
			expect(response.result).toEqual(test.parse2_result);
		}
		expect(response.error).not.toBeDefined();
	}, 100000);

	it(`is robust when asked to parse file that does not exist / is not readable`, async () => {
		label(`is robust when asked to parse file that does not exist / is not readable`);

		let response: PvsResponse = await pvsProxy.parseFile({
			fileName: "foo", 
			fileExtension: ".pvs", 
			contextFolder: sandboxExamples
		}, { test: true });
		// console.dir(response);
		expect(response).toBeDefined();
		expect(response.result).not.toBeDefined();
		expect(response.error).toBeDefined();
	}, 100000);


	//-----------------------
	// additional test cases
	//-----------------------
	for (let i = 0; i < steverFiles.length; i++) {
		it(`can parse stever/${steverFiles[i]}.pvs`, async () => {
			label(`can parse stever/${steverFiles[i]}.pvs`);
			// Need to clear-theories, in case rerunning with the same server.
			await pvsProxy.lisp("(clear-theories t)");

			const response: PvsResponse = await pvsProxy.parseFile({
				fileName: steverFiles[i],
				fileExtension: ".pvs", 
				contextFolder: stever
			}, { test: true });
			// console.dir(response);
			expect(response).toBeDefined();
			expect(response.result).toBeDefined();
			expect(response.error).not.toBeDefined();
		}, 8000);
	}

	for (let i = 0; i < pillboxFiles.length; i++) {
		it(`can parse pillboxv7/${pillboxFiles[i]}.pvs`, async () => {
			label(`can parse pillboxv7/${pillboxFiles[i]}.pvs`);
			// Need to clear-theories, in case rerunning with the same server.
			await pvsProxy.lisp("(clear-theories t)");

			const response: PvsResponse = await pvsProxy.parseFile({
				fileName: pillboxFiles[i],
				fileExtension: ".pvs", 
				contextFolder: pillbox
			}, { test: true });
			// console.dir(response);
			expect(response).toBeDefined();
			expect(response.result).toBeDefined();
			expect(response.error).not.toBeDefined();
		}, 8000);
	}

	for (let i = 0; i < pvsiowebFiles.length; i++) {
		it(`can parse pvsioweb/${pvsiowebFiles[i]}.pvs`, async () => {
			label(`can parse pvsioweb/${pvsiowebFiles[i]}.pvs`);
			// Need to clear-theories, in case rerunning with the same server.
			await pvsProxy.lisp("(clear-theories t)");

			const response: PvsResponse = await pvsProxy.parseFile({
				fileName: pvsiowebFiles[i],
				fileExtension: ".pvs", 
				contextFolder: pvsioweb
			}, { test: true });
			// console.dir(response);
			if (pvsiowebFiles[i].endsWith("MDNumberpad")) {
				expect(response.error).toBeDefined(); // theory 'limits' declared in twice in the same workspace
			} else {
				expect(response.result).toBeDefined();
				expect(response.error).not.toBeDefined();
			}
		}, 8000);
	}

});

