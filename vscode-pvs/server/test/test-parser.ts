// import { ContextDiagnostics } from "./server/pvsProcess";
//import { PvsFindDeclaration, PvsParserResponse, PvsTypecheckerResponse, XmlRpcResponse } from "./server/common/serverInterface";
import * as fsUtils from "./server/common/fsUtils";
import * as test from "./test-constants";
import { PvsResponse, PvsResult } from "./server/common/pvs-gui";
import { PvsProxy } from './server/pvsProxy'; // XmlRpcSystemMethods
import { label, log, dir, configFile, sandboxExamples, safeSandboxExamples } from './test-utils';

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
		await pvsProxy.activate({ debugMode: true }); // this will also start pvs-server

		// delete pvsbin files
		await fsUtils.deletePvsCache(sandboxExamples);
	});
	afterAll(async () => {
		// delete pvsbin files
		await fsUtils.deletePvsCache(sandboxExamples);

		if (test.EXTERNAL_SERVER) {
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

		const response: PvsResponse = await pvsProxy.parseFile({ fileName: "sqrt", fileExtension: ".pvs", contextFolder: sandboxExamples });
		// dir(response); // Uncomment this to see the whole ans
		expect(response).not.toBeNull();
		expect(response.result).toEqual(test.parse1_result);
	}, 100000);

	// it(`can parse multiple files in parallel`, async () => {
	// 	label(`can parse multiple files in parallel`);
	// 	// Need to clear-theories, in case rerunning with the same server.
	// 	await pvsProxy.lisp("(clear-theories t)");

	// 	// async calls
	// 	pvsProxy.parseFile({ fileName: "alaris2lnewmodes", fileExtension: ".pvs", contextFolder: safeSandboxExamples });
	// 	pvsProxy.parseFile({ fileName: "alaris2lnewmodes_pump", fileExtension: ".pvs", contextFolder: safeSandboxExamples });
	// 	pvsProxy.parseFile({ fileName: "alaris2lnewmodes_types_and_constants", fileExtension: ".pvs", contextFolder: safeSandboxExamples });

	// 	const response: PvsResponse = await pvsProxy.parseFile({ fileName: "sqrt", fileExtension: ".pvs", contextFolder: sandboxExamples });
	// 	// dir(response); // Uncomment this to see the whole ans
	// 	expect(response).not.toBeNull();
	// 	expect(response.result).toEqual(test.parse1_result);
	// }, 100000);

	it(`can report parse errors`, async () => {
		label(`can report parse errors`);

		const response: PvsResult = await pvsProxy.parseFile({ fileName: "lib0", fileExtension: ".pvs", contextFolder: sandboxExamples });
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

		const response: PvsResponse = await pvsProxy.parseFile({ fileName: "test", fileExtension: ".pvs", contextFolder: sandboxExamples });
		dir(response); // Uncomment this to see the whole ans
		expect(response).not.toBeNull();
	}, 100000);

	it(`can parse file when filename contains '.'`, async () => {
		label(`can parse file when filename contains '.'`);

		let response: PvsResponse = await pvsProxy.parseFile({ fileName: "alaris2lnewmodes.pump", fileExtension: ".pvs", contextFolder: sandboxExamples });
		dir(response);
		expect(response).not.toBeNull();
		expect(response.result).not.toBeNull();
		expect(response.result.length).toEqual(1);
		expect(response.result).toEqual(test.parse2_result);
	}, 100000);

});

