import * as fsUtils from "../server/src/common/fsUtils";
import { PvsResponse, PvsResult } from "../server/src/common/pvs-gui";
import { PvsProxy } from '../server/src/pvsProxy';
import { configFile, tacticalsExamples } from './test-utils';
import { PvsFormula } from "../server/src/common/serverInterface";
import * as path from 'path';
import { execSync } from "child_process";

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

	// this test requires nasalib and patch-20291231-server-output
	fit(`can handle branches with propax`, async () => {
		// Need to clear-theories, in case rerunning with the same server.
		await pvsProxy.lisp("(clear-theories t)");

		const baseFolder: string = path.join(__dirname, "pvscontext");

		// remove folder if present and replace it with the content of the zip file
		const contextFolder: string = path.join(baseFolder, "nasalib-monitors");
		fsUtils.deleteFolder(contextFolder);
		execSync(`cd ${baseFolder} && unzip nasalib-monitors.zip`);

		let response: PvsResponse = await pvsProxy.proveFormula({
			fileName: "trace",
			fileExtension: ".pvs",
			contextFolder: path.join(contextFolder, "Fret_MLTL"),
			theoryName: "trace",
			formulaName: "null_null_always_satisfaction"
		});
		expect(response.result).toBeDefined();
		expect(response.error).not.toBeDefined();
		// console.dir(response);

		const cmds: string[] = `(skeep)(fretex)(iff)(split)(flatten)(inst -1 "0")(skeep)(inst 2 "n-1")(case "i > n-1")(expand "Trace_equiv")(inst -3 "n-1")(assert)(flatten)(assert)(expand "last_atom")`.replace(/\)/g, ")\n").split("\n");
		for (let i = 0; i < cmds.length; i++) {
			const cmd: string = cmds[i];
			console.log(cmd);
			response = await pvsProxy.proofCommand({ cmd });
			console.dir(response);
		}

		expect(response.result[0]["prev-cmd"]).toEqual(`(expand "last_atom")`);
	}, 20000);

	it(`provides correct prev-cmd when branch closes (test 1)`, async () => {
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

		response = await pvsProxy.proofCommand({ cmd: "(skosimp*)" });
		expect(response.result[0]["prev-cmd"]).toEqual("(skosimp*)");
		// console.dir(response);

		response = await pvsProxy.proofCommand({ cmd: "(split)" });
		expect(response.result[0]["prev-cmd"]).toEqual("(split)");
		// console.dir(response);

		response = await pvsProxy.proofCommand({ cmd: "(flatten)" });
		expect(response.result[0]["prev-cmd"]).toEqual("(flatten)");
		// console.dir(response);

		response = await pvsProxy.proofCommand({ cmd: "(grind)" });
		expect(response.result[0]["prev-cmd"]).toEqual("(grind)");
		expect(response.result[1]["prev-cmd"]).toEqual("(grind)");
		console.dir(response);

	});

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
		// console.dir(skosimp_response, { depth: null });
		expect(skosimp_response.result[0]["prev-cmd"][0]).toEqual("skosimp*");

		const split_response: PvsResponse = await pvsProxy.proofCommand({ cmd: "(split)" });
		// console.dir(split_response, { depth: null });
		expect(split_response.result[0]["prev-cmd"][0]).toEqual("split");

		await pvsProxy.proofCommand({ cmd: "(quit)" });

		// re-run the proof commands with the tactical
		response = await pvsProxy.proveFormula(request);
		response = await pvsProxy.proofCommand({ cmd: "(then (skosimp*) (split))" });
		console.dir(response, { depth: null });	
		
		// pvs-server should returns an ordered array of sequents for glassbox tactics such as (then (skosimp*) (split))
		// - each sequent is the result of the execution of a proof command 
		// - sequent in position n is the sequent obtained from the first proof command, (skosimp*) in the considered example
		// - sequent in position 0 is the sequent obtained from the last proof command, (split) in the considered example
		// - if the last proof command produces more than sequent, only the first sequent is returned (the active sequent)
		// - all sequents must have a label that identifies the subgoal in the proof tree where the sequent belongs
		// - all sequents must have a field "prev-cmd" of type string, indicating the command that produced the executed command 
		expect(response.result.length).toEqual(2); // the sequent for (skosimp*) and the active sequent

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
