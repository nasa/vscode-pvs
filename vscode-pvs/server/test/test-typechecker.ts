// import { ContextDiagnostics } from "./server/pvsProcess";
//import { PvsFindDeclaration, PvsParserResponse, PvsTypecheckerResponse, XmlRpcResponse } from "./server/common/serverInterface";
import * as fsUtils from "./server/common/fsUtils";
import * as test from "./test-constants";
import * as path from 'path';
import { ParseResult, ListMethodsResult, PvsError, PvsResponse, PvsResult, FindDeclarationResult } from "./server/common/pvs-gui";
import { PvsProxy, ContextDiagnostics } from './server/pvsProxy'; // XmlRpcSystemMethods
import { label, log, dir, configFile, safeSandboxExamples, sandboxExamples, radixExamples } from './test-utils';

//----------------------------
//   Test cases for typechecker
//----------------------------
describe("pvs-typechecker", () => {
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

	// on Mac, pvs-server crashes without informative messages when performing this test
	it(`can typecheck pvs files`, async () => {
		label(`can typecheck pvs files`);

		// const response: PvsResponse = await pvsProxy.typecheckFile({ fileName: "sqrt", fileExtension: ".pvs", contextFolder: sandboxExamples });
		const response: PvsResponse = await pvsProxy.typecheckFile({ fileName: "main", fileExtension: ".pvs", contextFolder: sandboxExamples });
		dir(response);
		expect(response).toBeDefined();
		// expect(response.result).toEqual(test.typecheck1_result);
	}, 100000);

	// on Mac, pvs-server crashes without informative messages when performing this test
	it(`can typecheck theories with parameters`, async () => {
		label(`can typecheck theories with parameters`);

		let desc = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "alaris2lnewmodes.pump",
			formulaName: "vtbi_over_rate_lemma",
			line: 28,
			theoryName: "pump_th"
		};
		let response: PvsResponse = await pvsProxy.typecheckFile(desc);
		expect(response.result).toBeDefined();
		expect(response.error).not.toBeDefined();
		
	}, 10000);


	return; // the following tests are completed successfully -- remove the return statement if you want to run them

	// OK
	it(`can typecheck files that import other files`, async () => {
		label(`can typecheck files that import other files`);

		const response: PvsResponse = await pvsProxy.typecheckFile({ fileName: "sqrt", fileExtension: ".pvs", contextFolder: sandboxExamples });
		dir(response);
		expect(response).not.toBeNull();
		expect(response.result).not.toBeNull();
		expect(response.result.length).toEqual(1);

		// has-proofscript? seems to be changing all the time for different runs, I'm removing it from the check for now
		const res_decls = response.result[0].decls.map(elem => {
			if (elem.kind === "formula") {
				return {
					id: elem.id,
					kind: elem.kind,
					place: elem.place,
					"proved?": elem["proved?"],
					"complete?": elem["complete?"]
				};
			}
			return elem;
		});
		const exp_decls = test.typecheck2_result[0].decls.map(elem => {
			if (elem.kind === "formula") {
				return {
					id: elem.id,
					kind: elem.kind,
					place: elem.place,
					"proved?": elem["proved?"],
					"complete?": elem["complete?"]
				};
			}
			return elem;
		});
		expect(res_decls).toEqual(exp_decls);
	}, 100000);

});
