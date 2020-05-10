import * as fsUtils from "../server/src/common/fsUtils";
import * as test from "./test-constants";
import { PvsResponse } from "../server/src/common/pvs-gui";
import { PvsProxy } from '../server/src/pvsProxy'; // XmlRpcSystemMethods
import { label, log, configFile, sandboxExamples } from './test-utils';

//----------------------------
//   Test cases for proofScript
//----------------------------
describe("proofScript", () => {
	let pvsProxy: PvsProxy = null;
	beforeAll(async () => {
		const config: string = await fsUtils.readFile(configFile);
		const content: { pvsPath: string } = JSON.parse(config);
		log(content);
		const pvsPath: string = content.pvsPath;
		// log("Activating xmlrpc proxy...");
		pvsProxy = new PvsProxy(pvsPath, { externalServer: test.EXTERNAL_SERVER });
		await pvsProxy.activate({ debugMode: false, showBanner: false }); // this will also start pvs-server

		// delete pvsbin files and .pvscontext
		await fsUtils.deletePvsCache(sandboxExamples);

		console.log("\n----------------------");
		console.log("test-proofscript");
		console.log("----------------------");
	});
	afterAll(async () => {
		await pvsProxy.killPvsServer();
		await pvsProxy.killPvsProxy();
		// delete pvsbin files and .pvscontext
		await fsUtils.deletePvsCache(sandboxExamples);
	});	

	it(`can provide proof scripts`, async () => {
		label(`can provide proof scripts`);

		const desc = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "alaris2lnewmodes",
			formulaName: "check_chev_fup_permission",
			theoryName: "alaris_th"
		};
		let response: PvsResponse = await pvsProxy.proofScript(desc);
		expect(response.result).toBeDefined();
		expect(response.error).not.toBeDefined();
	}, 60000);
	
	it(`returns a well-formed proof empty script when the proof file is not available`, async () => {
		label(`returns a well-formed empty proof script when the proof file is not available`);

		// a well-formed response for formula sqrt_0 is in the form /;;; Proof sqrt_0(\-\d+)? for formula sqrt.sqrt_0\n(""\s*)/
		const desc = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sqrt",
			formulaName: "sqrt_0",
			theoryName: "sqrt"
		};
		let response: PvsResponse = await pvsProxy.proofScript(desc);
		// console.dir(response);
		expect(response.result).not.toBeDefined();
		expect(response.error).toBeDefined();
		expect(response.error.data.error_string).toMatch(/(.*) does not have a proof/);

		const desc1 = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "alari2lnewmodes.pump.pvs",
			formulaName: "vtbi_over_rate_lemma",
			theoryName: "pump_th"
		}
		response = await pvsProxy.proofScript(desc);
		// console.dir(response);
		expect(response.result).not.toBeDefined();
		expect(response.error).toBeDefined();
		expect(response.error.data.error_string).toMatch(/(.*) does not have a proof/);
	});

	it(`can show proof script`, async () => {
		label(`show proof script`);

		const desc = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			formulaName: "sq_neg",
			theoryName: "sq"
		};
		const response: PvsResponse = await pvsProxy.proofScript(desc);
		expect(response.error).not.toBeDefined();
		expect(response.result).toBeDefined();
		const proof_header: string = response.result.split("\n")[0];
		expect(proof_header).toEqual(`;;; Proof sq_neg-1 for formula sq.sq_neg`);
	});
	
});