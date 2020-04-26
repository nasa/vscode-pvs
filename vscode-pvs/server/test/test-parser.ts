// import { ContextDiagnostics } from "./server/pvsProcess";
//import { PvsFindDeclaration, PvsParserResponse, PvsTypecheckerResponse, XmlRpcResponse } from "./server/common/serverInterface";
import * as fsUtils from "../src/common/fsUtils";
import * as test from "./test-constants";
import { PvsResponse, PvsResult } from "../src/common/pvs-gui";
import { PvsProxy } from '../src/pvsProxy'; // XmlRpcSystemMethods
import { label, log, dir, configFile, sandboxExamples, safeSandboxExamples, 
	stever, steverFiles, pillbox, pillboxFiles, pvsioweb, pvsiowebFiles } from './test-utils';

//----------------------------
//   Test cases for parser
//----------------------------
describe("pvs-parser", () => {
	let pvsProxy: PvsProxy = null;
	beforeAll(async () => {
		const config: string = await fsUtils.readFile(configFile);
		const content: { pvsPath: string } = JSON.parse(config);
		log(content);
		const pvsPath: string = content.pvsPath;
		// log("Activating xmlrpc proxy...");
		pvsProxy = new PvsProxy(pvsPath, { externalServer: test.EXTERNAL_SERVER });
		await pvsProxy.activate({ debugMode: true, showBanner: false }); // this will also start pvs-server

		// delete pvsbin files
		await fsUtils.deletePvsCache(sandboxExamples);
	});
	afterAll(async () => {
		// delete pvsbin files
		await fsUtils.deletePvsCache(sandboxExamples);

		if (!test.EXTERNAL_SERVER) {
			// kill pvs server & proxy
			console.log(" killing pvs server...")
			await pvsProxy.killPvsServer();
		}
		await pvsProxy.killPvsProxy();
	});

	it(`can parse file`, async () => {
		label(`can parse file`);
		// Need to clear-theories, in case rerunning with the same server.
		await pvsProxy.lisp("(clear-theories t)");

		const response: PvsResponse = await pvsProxy.parseFile({ fileName: "sqrt", fileExtension: ".pvs", contextFolder: sandboxExamples }, { test: true });
		dir(response); // set VERBOSE to true in test-utils if you want to see the output
		expect(response).toBeDefined();
		expect(response.result).toBeDefined();
		expect(response.result).toEqual(test.parse1_result);
		expect(response.error).not.toBeDefined();
	}, 4000);

	it(`can report parse errors`, async () => {
		label(`can report parse errors`);

		const response: PvsResult = await pvsProxy.parseFile({ fileName: "lib0", fileExtension: ".pvs", contextFolder: sandboxExamples }, { test: true });
		dir(response);
		const exp = {
		code: 1,
		message: 'Parser error',
		data:
		{
			error_string: 'Found \'[#\' when expecting \'END\'\nIn file /home/owre/vscode-pvs/server/test/sandbox/lib0.pvs (line 3, col 14)',
			file_name: '/home/owre/vscode-pvs/server/test/sandbox/lib0.pvs',
			place: [3, 14, 3, 14]
		}
		};
		expect(response.error).toBeDefined();
		expect(response.error.data).toBeDefined();
		expect(response.error.data.place).toEqual(exp.data.place);
		expect(response.error.data.error_string.startsWith(exp.data.error_string.split("\n")[0])).toBeTruthy();
	}, 100000);

	it(`can parse file with inline declarations`, async () => {
		label(`can parse file with inline declarations`);
		// Need to clear-theories, in case rerunning with the same server.
		await pvsProxy.lisp("(clear-theories t)");

		const response: PvsResponse = await pvsProxy.parseFile({ fileName: "test", fileExtension: ".pvs", contextFolder: sandboxExamples }, { test: true });
		dir(response); // set VERBOSE to true in test-utils if you want to see the output
		expect(response).toBeDefined();
	}, 100000);

	it(`can parse file when filename contains '.'`, async () => {
		label(`can parse file when filename contains '.'`);

		let response: PvsResponse = await pvsProxy.parseFile({ fileName: "alaris2lnewmodes.pump", fileExtension: ".pvs", contextFolder: sandboxExamples }, { test: true });
		dir(response);
		expect(response).toBeDefined();
		expect(response.result).toBeDefined();
		expect(response.result.length).toEqual(1);
		expect(response.result).toEqual(test.parse2_result);
		expect(response.error).not.toBeDefined();
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
			dir(response); // set VERBOSE to true in test-utils if you want to see the output
			expect(response).toBeDefined();
			expect(response.result).toBeDefined();
			expect(response.error).not.toBeDefined();
		}, 8000);
	}

	for (let i = 0; i < pillboxFiles.length; i++) {
		it(`can parse pillbox/${pillboxFiles[i]}.pvs`, async () => {
			label(`can parse pillbox/${pillboxFiles[i]}.pvs`);
			// Need to clear-theories, in case rerunning with the same server.
			await pvsProxy.lisp("(clear-theories t)");

			const response: PvsResponse = await pvsProxy.parseFile({
				fileName: pillboxFiles[i],
				fileExtension: ".pvs", 
				contextFolder: pillbox
			}, { test: true });
			dir(response); // set VERBOSE to true in test-utils if you want to see the output
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
			dir(response); // set VERBOSE to true in test-utils if you want to see the output
			expect(response).toBeDefined();
			expect(response.result).toBeDefined();
			expect(response.error).not.toBeDefined();
		}, 8000);
	}
});

