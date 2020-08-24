import * as fsUtils from "../server/src/common/fsUtils";
import { PvsResponse, PvsResult } from "../server/src/common/pvs-gui";
import { PvsProxy } from '../server/src/pvsProxy';
import { configFile, tacticalsExamples } from './test-utils';
import { PvsFormula } from "../server/src/common/serverInterface";

//----------------------------
//   Test cases for prover
//----------------------------
describe("pvs-prover", () => {
	let pvsProxy: PvsProxy = null;
	beforeAll(async () => {
		const config: string = await fsUtils.readFile(configFile);
		const content: { pvsPath: string } = JSON.parse(config);
		// log(content);
		const pvsPath: string = content.pvsPath;
		// log("Activating xmlrpc proxy...");
		pvsProxy = new PvsProxy(pvsPath, { externalServer: true });
		await pvsProxy.activate({ debugMode: false, showBanner: false }); // this will also start pvs-server

		// delete pvsbin files and .pvscontext
		await fsUtils.cleanBin(tacticalsExamples);

		console.log("\n----------------------");
		console.log("test-tacticals");
		console.log("----------------------");
	});
	afterAll(async () => {
		await pvsProxy.killPvsServer();
		await pvsProxy.killPvsProxy();
		// delete pvsbin files and .pvscontext
		await fsUtils.cleanBin(tacticalsExamples);
	});

	// utility function, quits the prover if the prover status is active
	const quitProverIfActive = async (): Promise<void> => {
		// quit prover if prover status is active
		const proverStatus: PvsResult = await pvsProxy.getProverStatus();
		expect(proverStatus.result).toBeDefined();
		expect(proverStatus.error).not.toBeDefined();
		console.log(proverStatus);
		if (proverStatus && proverStatus.result !== "inactive") {
			await pvsProxy.proofCommand({ cmd: 'quit' });
		}
	}

	// this test case fails (prev-cmd and commentary provide incorrect information for the first command)
	it(`can execute tactical 'then'`, async () => {
		await quitProverIfActive();

		const request: PvsFormula = {
			contextFolder: tacticalsExamples,
			fileExtension: '.pvs',
			fileName: 'foo',
			formulaName: 'foo1',
			theoryName: 'foo'
		};
		let response: PvsResponse = await pvsProxy.proveFormula(request);
		// console.dir(response, { depth: null });
		expect(response.error).not.toBeDefined();
		expect(response.result).toBeDefined();

		const skosimp_response: PvsResponse = await pvsProxy.proofCommand({ cmd: "(skosimp*)" });
		console.dir(skosimp_response, { depth: null });
		expect(skosimp_response.result[0]["prev-cmd"][0]).toEqual("skosimp*");

		const split_response: PvsResponse = await pvsProxy.proofCommand({ cmd: "(split)" });
		console.dir(split_response, { depth: null });
		expect(split_response.result[0]["prev-cmd"][0]).toEqual("split");

		await pvsProxy.proofCommand({ cmd: "(quit)" });

		// re-run the proof commands with the tactical
		response = await pvsProxy.proveFormula(request);
		response = await pvsProxy.proofCommand({ cmd: "(then (skosimp*) (split))" });
		console.dir(response, { depth: null });	
		
		expect(response.result.length).toEqual(2);

		expect(response.result[1]["prev-cmd"][0]).toEqual("skosimp*");
		expect(response.result[1].label).toEqual(skosimp_response.result[0].label);
		expect(response.result[1]["num-subgoals"]).toEqual(skosimp_response.result[0]["num-subgoals"]);
		expect(response.result[1].commentary).toEqual(skosimp_response.result[0].commentary);
		expect(response.result[1].sequent).toEqual(skosimp_response.result[0].sequent);

		expect(response.result[0]["prev-cmd"][0]).toEqual("split");
		expect(response.result[0].label).toEqual(split_response.result[0].label);
		expect(response.result[0]["num-subgoals"]).toEqual(split_response.result[0]["num-subgoals"]);
		expect(response.result[0].commentary).toEqual(split_response.result[0].commentary);
		expect(response.result[0].sequent).toEqual(split_response.result[0].sequent);

	});


});
