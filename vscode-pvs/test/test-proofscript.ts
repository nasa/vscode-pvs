// import { ContextDiagnostics } from "./server/pvsProcess";
//import { PvsFindDeclaration, PvsParserResponse, PvsTypecheckerResponse, XmlRpcResponse } from "./server/common/serverInterface";
import * as fsUtils from "../server/src/common/fsUtils";
import * as test from "./test-constants";
import { PvsResponse } from "../server/src/common/pvs-gui";
import { PvsProxy } from '../server/src/pvsProxy'; // XmlRpcSystemMethods
import { label, log, configFile, sandboxExamples, safeSandboxExamples, radixExamples } from './test-utils';
import * as path from 'path';

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
		await pvsProxy.activate({ debugMode: true, showBanner: false }); // this will also start pvs-server

		// delete pvsbin files
		await fsUtils.deletePvsCache(sandboxExamples);
	});
	afterAll(async () => {
		// delete pvsbin files
		// await fsUtils.deletePvsCache(sandboxExamples);

		if (!test.EXTERNAL_SERVER) {
			// kill pvs server & proxy
			console.log(" killing pvs server...")
			await pvsProxy.killPvsServer();
		}
		await pvsProxy.killPvsProxy();
	});	

	// on Linux, pvs-server fails with the following error:
	// { code: 1, message: '"No methods applicable for generic function #<standard-generic-function all-declarations> with args (nil) of classes (null)"' }
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
	
	// on Linux, this test fails for alaris2lnewmodes.pump.pvs with the following error message: 
	// There is garbage at the end of your file or string:
	// Line 1:     alari2lnewmodes.pump
	//                            ^
	//  (line 1, col 15)
	// on Mac, pvs-server returns the following message: '"No methods applicable for generic function #<standard-generic-function id> with args (nil) of classes (null)"' }) not to be defined.
	it(`returns a well-formed proof script when the proof file is not available`, async () => {
		label(`returns a well-formed proof script when the proof file is not available`);

		// a well-formed response for formula sqrt_0 is in the form /;;; Proof sqrt_0(\-\d+)? for formula sqrt.sqrt_0\n(""\s*)/
		let fname: string = path.join(sandboxExamples, "sqrt.pvs");
		let response: PvsResponse = await pvsProxy.pvsRequest('proof-script', [ fname, "sqrt_0" ]);
		// console.dir(response);
		expect(response.result).not.toBeDefined();
		expect(response.error).toBeDefined();
		expect(response.error.data.error_string).toMatch(/(.*) does not have a proof/);

		fname = path.join(sandboxExamples, "alari2lnewmodes.pump.pvs");
		response = await pvsProxy.pvsRequest('proof-script', [ fname, "vtbi_over_rate_lemma" ]);
		// console.dir(response);
		expect(response.result).not.toBeDefined();
		expect(response.error).toBeDefined();
		expect(response.error.data.error_string).toMatch(/(.*) does not have a proof/);
	});

	// return; // the following tests are completed successfully on Linux -- remove the return statement if you want to run them	

	// OK 
	it(`can show proof script`, async () => {
		label(`show proof script`);
		const fname: string = path.join(sandboxExamples, "sq.pvs");

		const response: PvsResponse = await pvsProxy.pvsRequest('proof-script', [ fname, "sq_neg" ]);
		expect(response.error).not.toBeDefined();
		expect(response.result).toBeDefined();
		const proof_header: string = response.result.split("\n")[0];
		expect(proof_header).toEqual(`;;; Proof sq_neg-1 for formula sq.sq_neg`);
	});
	
});