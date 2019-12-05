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
describe("find-declaration / term-at", () => {
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
	
	//-----------------------------------------
	// test cases for find-declaration
	//-----------------------------------------

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

	}, 10000);

	it(`pvs-server can sustain workload with find-declaration`, async () => {
		label(`pvs-server can sustain workload with find-declaration`);

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
		expect(response.result).toBeDefined();
		expect(response.error).not.toBeDefined();

	}, 40000);

	it(`pvs-server can perform multiple find-declaration in parallel`, async () => {
		label(`pvs-server can perform multiple find-declaration in parallel`);

		pvsProxy.findDeclaration("boolean");
		pvsProxy.findDeclaration("T");
		pvsProxy.findDeclaration("if_def");
		pvsProxy.findDeclaration("not_def");
		pvsProxy.findDeclaration("and_def");
		pvsProxy.findDeclaration("syand_def");
		pvsProxy.findDeclaration("or_def");
		pvsProxy.findDeclaration("implies_def");
		pvsProxy.findDeclaration("syimplies_def");
		pvsProxy.findDeclaration("when_def");
		pvsProxy.findDeclaration("syiff_def");
		pvsProxy.findDeclaration("excluded_middle");
		pvsProxy.findDeclaration("not_exists");
		pvsProxy.findDeclaration("exists_not");
		pvsProxy.findDeclaration("exists_or");
		pvsProxy.findDeclaration("exists_implies");
		pvsProxy.findDeclaration("exists_and");
		pvsProxy.findDeclaration("forall_and");
		pvsProxy.findDeclaration("forall_not");
		pvsProxy.findDeclaration("forall_or");

		const response: PvsResponse = await pvsProxy.findDeclaration("pred");
		expect(response).not.toBeNull();
		expect(response.result).toBeDefined();
		expect(response.error).not.toBeDefined();

	}, 40000);

	it(`pvs-server returns a full and well-formed filename in find-declaration`, async () => {
		label(`pvs-server returns a full and well-formed filename in find-declaration`);

		const response: PvsResponse = await pvsProxy.findDeclaration("boolean");
		expect(response).not.toBeNull();
		expect(response.result).toBeDefined();
		expect(response.result[0].filename).toContain("/"); // a simple way to that filename includes a path is to check that there is at least one path separator
		expect(response.result[0].filename).not.toContain("//"); // consecutive double slashes should not be present
		expect(response.result[0].filename).not.toContain("~"); // tilde should be expanded
		expect(response.result[0].filename).toMatch(/\/.*/); // path should be absolute, i.e., start with /

	}, 40000);

	it(`pvs-server can execute find-declaration while parsing`, async () => {
		label(`pvs-server can execute find-declaration while parsing`);

		// async call
		pvsProxy.parseFile({ fileName: "alaris2lnewmodes", fileExtension: ".pvs", contextFolder: sandboxExamples });
		
		const response: PvsResponse = await pvsProxy.findDeclaration("boolean");
		expect(response).not.toBeNull();
		expect(response.result).toBeDefined();
		expect(response.error).not.toBeDefined();
	}, 40000);

	//-----------------------------------------
	// test cases for term-at
	//-----------------------------------------
	it(`pvs-server can invoke term-at`, async () => {
		label(`pvs-server can invoke term-at`);
		// Need to clear-theories, in case rerunning with the same server.
		await pvsProxy.lisp("(clear-theories t)");

		const fname: string = path.join(sandboxExamples, "sqrt.pvs");
		const response: PvsResponse = await pvsProxy.pvsRequest("term-at", [ fname, `(23 2)`, 't' ]); 
		dir(response);
		expect(response).not.toBeNull();
		expect(response["result"]).not.toBeNull();
		expect(response["error"]).not.toBeDefined();

	}, 10000);

});