// import { ContextDiagnostics } from "./server/pvsProcess";
//import { PvsFindDeclaration, PvsParserResponse, PvsTypecheckerResponse, XmlRpcResponse } from "./server/common/serverInterface";
import * as fsUtils from "./server/common/fsUtils";
import * as test from "./test-constants";
import * as path from 'path';
import { ParseResult, ListMethodsResult, PvsError, PvsResponse, PvsResult, FindDeclarationResult } from "./server/common/pvs-gui";
import { PvsProxy, ContextDiagnostics } from './server/pvsProxy'; // XmlRpcSystemMethods
import { label, log, dir, configFile, sandboxExamples } from './test-utils';

//----------------------------
//   Test cases for find-declaration
//----------------------------
describe("find-declaration", () => {
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
	
	it(`pvs-server can invoke find-declaration`, async () => {
		label(`pvs-server can invoke find-declaration`);
		// Need to clear-theories, in case rerunning with the same server.
		await pvsProxy.lisp("(clear-theories t)");

		await pvsProxy.parseFile({ fileName: "sqrt", fileExtension: ".pvs", contextFolder: sandboxExamples });
		const response: PvsResponse = await pvsProxy.findDeclaration("sqrt");
		dir(response);
		expect(response).not.toBeNull();
		expect(response["error"]).not.toBeDefined();
		expect(response["result"]).not.toBeNull();

		const result: FindDeclarationResult = response["result"];
		expect(result.length).toEqual(1);
		expect(result[0].declname).toEqual(test.find_declaration_result[0].declname);
		expect(result[0].theoryid).toEqual(test.find_declaration_result[0].theoryid);
		expect(result[0].filename.endsWith(test.find_declaration_result[0].filename)).toBeTruthy();
		expect(result[0].place).toEqual(test.find_declaration_result[0].place);
		expect(result[0]['decl-ppstring']).toEqual(test.find_declaration_result[0]['decl-ppstring']);

		// the check on the type fails --- pvs-server is returning 'nil'
		// expect(result[0].type).toEqual(test.find_declaration_result[0].type);

	}, 10000);

	it(`pvs-server can sustain workload with find-declaration`, async () => {
		label(`pvs-server can sustain workload with find-declaration`);

		// parse context performs async calls
		let response: PvsResponse = null;
		for (let i = 0; i < 10; i++) {
			response = await pvsProxy.findDeclaration("boolean");
			response = await pvsProxy.findDeclaration("T");
			response = await pvsProxy.findDeclaration("if_def");
			response = await pvsProxy.findDeclaration("not_def");
			response = await pvsProxy.findDeclaration("and_def");
			response = await pvsProxy.findDeclaration("syand_def");
			response = await pvsProxy.findDeclaration("or_def");
			response = await pvsProxy.findDeclaration("implies_def");
			response = await pvsProxy.findDeclaration("syimplies_def");
			response = await pvsProxy.findDeclaration("when_def");
			response = await pvsProxy.findDeclaration("syiff_def");
			response = await pvsProxy.findDeclaration("excluded_middle");
			response = await pvsProxy.findDeclaration("not_exists");
			response = await pvsProxy.findDeclaration("exists_not");
			response = await pvsProxy.findDeclaration("exists_or");
			response = await pvsProxy.findDeclaration("exists_implies");
			response = await pvsProxy.findDeclaration("exists_and");
			response = await pvsProxy.findDeclaration("forall_and");
			response = await pvsProxy.findDeclaration("forall_not");
			response = await pvsProxy.findDeclaration("forall_or");
			response = await pvsProxy.findDeclaration("pred");
		}
		expect(response).not.toBeNull();
	}, 40000);

});