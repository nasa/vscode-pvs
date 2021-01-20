import * as fsUtils from "../server/src/common/fsUtils";
import { PvsResponse, PvsResult } from "../server/src/common/pvs-gui";
import { PvsProxy } from '../server/src/pvsProxy';
import { configFile, tacticalsExamples } from './test-utils';
import { PvsFormula } from "../server/src/common/serverInterface";
import * as path from 'path';
import { execSync } from "child_process";
import * as languageUtils from '../server/src/common/languageUtils';
import { expect } from 'chai';

//----------------------------
//   Test cases for prover
//----------------------------
describe("pvs-prover", () => {
	let pvsProxy: PvsProxy = null;
	before(async () => {
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
	after(async () => {
		await pvsProxy.killPvsServer();
		await pvsProxy.killPvsProxy();
		// delete pvsbin files and .pvscontext
		await fsUtils.cleanBin(tacticalsExamples);
	});

	// utility function, quits the prover if the prover status is active
	const quitProverIfActive = async (): Promise<void> => {
		// quit prover if prover status is active
		const proverStatus: PvsResult = await pvsProxy.getProverStatus();
		expect(proverStatus.result).not.to.be.undefined;
		expect(proverStatus.error).to.be.undefined;
		// console.log(proverStatus);
		if (proverStatus && proverStatus.result !== "inactive") {
			await pvsProxy.proofCommand({ cmd: 'quit' });
		}
	}

	// this test requires nasalib and patch-20291231-server-output
	it(`can execute glassbox proof for eventually_nulltiming`, async () => {
		await quitProverIfActive();

		// Need to clear-theories, in case rerunning with the same server.
		await pvsProxy.lisp("(clear-theories t)");

		const baseFolder: string = path.join(__dirname, "pvscontext");

		// remove folder if present and replace it with the content of the zip file
		const contextFolder: string = path.join(baseFolder, "monitors");
		fsUtils.deleteFolder(contextFolder);
		execSync(`cd ${baseFolder} && unzip nasalib-monitors-1.zip`);

		// await fsUtils.cleanBin(contextFolder, { keepTccs: true, recursive: true }); // cleaning pvsbin and .pvscontext does not help

		let response: PvsResponse = await pvsProxy.proveFormula({
			fileName: "trace",
			fileExtension: ".pvs",
			contextFolder: path.join(contextFolder, "Fret_MLTL"),
			theoryName: "trace",
			formulaName: "eventually_nulltiming"
		});
		expect(response.result).not.to.be.undefined;
		expect(response.error).to.be.undefined;
		// console.dir(response);

		const cmd: string = `(then (skeep) (expand "check_fretish_req_semantics") (expand* "Timing_fun"))`;
		response = await pvsProxy.proofCommand({ cmd });
		// console.log(cmd);
		// console.dir(response);

		expect(response.error).to.be.undefined;
		expect(response.result).not.to.be.undefined;
	}).timeout(20000);

	// this test requires nasalib and patch-20291231-server-output
	it(`can handle branches with propax`, async () => {
		await quitProverIfActive();

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
		expect(response.result).not.to.be.undefined;
		expect(response.error).to.be.undefined;
		// console.dir(response);

		const cmds: string[] = `(skeep)(fretex)(iff)(split)(flatten)(inst -1 "0")(skeep)(inst 2 "n-1")(case "i > n-1")(expand "Trace_equiv")(inst -3 "n-1")(assert)(flatten)(assert)(expand "last_atom")`.replace(/\)/g, ")\n").split("\n").filter(cmd => { return cmd && cmd.trim() !== ""; });
		for (let i = 0; i < cmds.length; i++) {
			const cmd: string = cmds[i];
			response = await pvsProxy.proofCommand({ cmd });
			// console.log(`cmd: ${cmd}`);
			// console.dir(response);
		}

		expect(response.result[0]["prev-cmd"]).to.deep.equal(`(expand "last_atom")`);
	}).timeout(20000);

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
		expect(response.error).to.be.undefined;
		expect(response.result).not.to.be.undefined;

		response = await pvsProxy.proofCommand({ cmd: "(skosimp*)" });
		expect(response.result[0]["prev-cmd"]).to.deep.equal("(skosimp*)");
		// console.dir(response);

		response = await pvsProxy.proofCommand({ cmd: "(split)" });
		expect(response.result[0]["prev-cmd"]).to.deep.equal("(split)");
		// console.dir(response);

		response = await pvsProxy.proofCommand({ cmd: "(flatten)" });
		expect(response.result[0]["prev-cmd"]).to.deep.equal("(flatten)");
		// console.dir(response);

		response = await pvsProxy.proofCommand({ cmd: "(grind)" });
		expect(response.result[0]["prev-cmd"]).to.deep.equal("(grind)");
		expect(languageUtils.branchComplete(response.result[0], request.formulaName, `1`)).to.equal(true);
		// console.dir(response);

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
		expect(response.error).to.be.undefined;
		expect(response.result).not.to.be.undefined;

		const skosimp_response: PvsResponse = await pvsProxy.proofCommand({ cmd: "(skosimp*)" });
		// console.dir(skosimp_response, { depth: null });
		expect(skosimp_response.result[0]["prev-cmd"]).to.deep.equal("(skosimp*)");

		const split_response: PvsResponse = await pvsProxy.proofCommand({ cmd: "(split)" });
		// console.dir(split_response, { depth: null });
		expect(split_response.result[0]["prev-cmd"]).to.deep.equal("(split)");

		await pvsProxy.proofCommand({ cmd: "(quit)" });

		// re-run the proof commands with the tactical
		response = await pvsProxy.proveFormula(request);
		response = await pvsProxy.proofCommand({ cmd: "(then (skosimp*) (split))" });
		await pvsProxy.proofCommand({ cmd: "(quit)" });
		// console.dir(response, { depth: null });
		
		// pvs-server should returns an ordered array of sequents for glassbox tactics such as (then (skosimp*) (split))
		// - each sequent is the result of the execution of a proof command 
		// - sequent in position 0 is the sequent obtained from the first proof command, (skosimp*) in the considered example
		// - sequent in position n is the sequent obtained from the last proof command, n = 1 and the command is (split) in the considered example
		// - the last sequent is the active sequent
		// - all sequents must have a label that identifies the subgoal in the proof tree where the sequent belongs
		// - all sequents must have a field "prev-cmd" of type string, indicating the command that produced the executed command 
		expect(response.result.length).to.equal(4); // two commands + two postpone for the two branches

		expect(response.result[0]["prev-cmd"]).to.deep.equal("(skosimp*)");
		expect(response.result[0].label).to.deep.equal(skosimp_response.result[0].label);
		expect(response.result[0]["num-subgoals"]).to.deep.equal(skosimp_response.result[0]["num-subgoals"]);
		expect(response.result[0].commentary).to.deep.equal(skosimp_response.result[0].commentary);
		expect(response.result[0].sequent).to.deep.equal(skosimp_response.result[0].sequent);

		expect(response.result[1]["prev-cmd"]).to.deep.equal("(split)");
		expect(response.result[1].label).to.deep.equal(split_response.result[0].label);
		expect(response.result[1]["num-subgoals"]).to.deep.equal(split_response.result[0]["num-subgoals"]);
		expect(response.result[1].commentary).to.deep.equal(split_response.result[0].commentary);
		expect(response.result[1].sequent).to.deep.equal(split_response.result[0].sequent);

		expect(response.result[3].label).to.deep.equal(`foo1.1`);
	});


});
