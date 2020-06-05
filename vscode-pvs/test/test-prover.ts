import * as fsUtils from "../server/src/common/fsUtils";
import * as test from "./test-constants";
import { PvsResponse } from "../server/src/common/pvs-gui";
import { PvsProxy } from '../server/src/pvsProxy'; // XmlRpcSystemMethods
import { label, configFile, sandboxExamples, safeSandboxExamples, radixExamples } from './test-utils';


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
		pvsProxy = new PvsProxy(pvsPath, { externalServer: test.EXTERNAL_SERVER });
		await pvsProxy.activate({ debugMode: false, showBanner: false }); // this will also start pvs-server

		// delete pvsbin files and .pvscontext
		await fsUtils.deletePvsCache(safeSandboxExamples);
		await fsUtils.deletePvsCache(sandboxExamples);
		await fsUtils.deletePvsCache(radixExamples);

		console.log("\n----------------------");
		console.log("test-prover");
		console.log("----------------------");
	});
	afterAll(async () => {
		await pvsProxy.killPvsServer();
		await pvsProxy.killPvsProxy();
		// delete pvsbin files and .pvscontext
		await fsUtils.deletePvsCache(safeSandboxExamples);
		await fsUtils.deletePvsCache(sandboxExamples);
		await fsUtils.deletePvsCache(radixExamples);		
	});

	it(`can discharge tccs`, async () => {
		label(`can discharge tccs`);

		const response: PvsResponse = await pvsProxy.proveTccs({
			fileName: "sq",
			fileExtension: ".pvs",
			contextFolder: sandboxExamples
		});
		// console.dir(response);
		expect(response.error).not.toBeDefined();
		expect(response.result).toBeDefined();
		expect(response.result.totals).toEqual(2);
		expect(response.result.proved).toEqual(2);
		expect(response.result.unproved).toEqual(0);
		expect(response.result.subsumed).toEqual(0);
		expect(response.result.simplified).toEqual(0);
	}, 4000);

	it(`can start prover session`, async () => {
		label(`can start prover session`);

		const desc = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "alaris2lnewmodes",
			formulaName: "check_chev_fup_permission",
			theoryName: "alaris_th"
		};
		let response: PvsResponse = await pvsProxy.proveFormula(desc);
		expect(response.result).toBeDefined();
		expect(response.error).not.toBeDefined();

		response = await pvsProxy.proofCommand({ cmd: 'quit' });
		expect(response.result).toEqual({ result: 'Unfinished' });
	}, 60000);

	it(`can start interactive proof session when the formula has already been proved`, async () => {
		label(`can start interactive proof session when the formula has already been proved`);

		const desc = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			formulaName: "sq_neg",
			theoryName: "sq",
			rerun: false
		};

		let response: PvsResponse = await pvsProxy.proveFormula(desc);
		// console.dir(response);
		expect(response.result.label).toEqual(test.sq_neg_prove_formula.label);
		expect(response.result.sequent).toEqual(test.sq_neg_prove_formula.sequent);

		try {
			// send proof command (skosimp*)
			response = await pvsProxy.proofCommand({ cmd: '(skosimp*)'});
			// console.dir(response);
			expect(response.result.label).toEqual(test.sq_neg_proof_command_skosimp_star.label);
			expect(response.result.action).toEqual(test.sq_neg_proof_command_skosimp_star.action);
			expect(response.result.sequent).toEqual(test.sq_neg_proof_command_skosimp_star.sequent);

			// send proof command (expand "sq")
			response = await pvsProxy.proofCommand({ cmd: '(expand "sq")'});
			// console.dir(response);
			expect(response.result.label).toEqual(test.sq_neg_expand.label);
			expect(response.result.action).toEqual(test.sq_neg_expand.action);
			expect(response.result.sequent).toEqual(test.sq_neg_expand.sequent);

			// send proof command (assert) to complete the proof
			response = await pvsProxy.proofCommand({ cmd: '(assert)'});
			// console.dir(response);
			expect(response.result).toEqual({ result: 'Q.E.D.' });

			// try to re-start the proof
			response = await pvsProxy.proveFormula(desc);
			// console.dir(response);
			expect(response.result.label).toEqual(test.sq_neg_prove_formula.label);
			expect(response.result.sequent).toEqual(test.sq_neg_prove_formula.sequent);

			// send proof command (skosimp*)
			response = await pvsProxy.proofCommand({ cmd: '(skosimp*)'});
			// console.dir(response);
			expect(response.result.label).toEqual(test.sq_neg_proof_command_skosimp_star.label);
			expect(response.result.action).toEqual(test.sq_neg_proof_command_skosimp_star.action);
			expect(response.result.sequent).toEqual(test.sq_neg_proof_command_skosimp_star.sequent);
		}
		finally {
			// quit the proof attempt
			await pvsProxy.proofCommand({ cmd: 'quit'});
		}

	}, 4000);
	
	it(`can start a prover session and quit the prover session`, async () => {
		label(`can start a prover session and quit the prover session`);

		const desc = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			formulaName: "sq_neg",
			theoryName: "sq"
		};
		let response: PvsResponse = await pvsProxy.proveFormula(desc);
		expect(response.result.sequent).toEqual(test.sq_neg_prove_formula.sequent);

		response = await pvsProxy.proofCommand({ cmd: 'quit' });
		expect(response.result).toEqual({ result: 'Unfinished' });
	}, 20000);
	
	it(`returns proverStatus = inactive when a prover session is not active`, async () => {
		label(`returns proverStatus = inactive when a prover session is not active`);

		const proverStatus: PvsResponse = await pvsProxy.proverStatus();
		expect(proverStatus.result).toEqual("inactive");
	}, 4000);

	it(`returns proverStatus = active when a prover session is active`, async () => {
		label(`returns proverStatus = active when a prover session is active`);

		const desc = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			formulaName: "sq_times",
			theoryName: "sq",
			rerun: false
		};

		// start prover session
		await pvsProxy.proveFormula(desc);
		// check prover status
		const proverStatus: PvsResponse = await pvsProxy.proverStatus();
		expect(proverStatus.result).toEqual("active");

		// quit the proof attempt
		await pvsProxy.proofCommand({ cmd: 'quit'});
	}, 4000);

	it(`can invoke prove-formula on theories with parameters`, async () => {
		label(`can invoke prove-formula on theories with parameters`);

		const desc = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "alaris2lnewmodes",
			formulaName: "check_chev_fup_permission",
			theoryName: "alaris_th" // pump_th exists, but check_chev_fup_permission is in alaris_th
		};
		let response: PvsResponse = await pvsProxy.proveFormula(desc);
		expect(response.result).toBeDefined();
		expect(response.error).not.toBeDefined();

		response = await pvsProxy.proofCommand({ cmd: 'quit' });
		expect(response.result).toEqual({ result: 'Unfinished' });
	}, 60000);	

	it(`returns proverStatus = inactive after quitting a prover session`, async () => {
		label(`returns proverStatus = inactive after quitting a prover session`);

		const desc = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			formulaName: "sq_times",
			theoryName: "sq",
			rerun: false
		};

		// start prover session
		await pvsProxy.proveFormula(desc);
		// quit the proof attempt
		await pvsProxy.proofCommand({ cmd: 'quit'});
		// check prover status
		const proverStatus: PvsResponse = await pvsProxy.proverStatus();
		expect(proverStatus.result).toEqual("inactive");
	}, 4000);

	it(`can start prover sessions in theories with parameters`, async () => {
		label(`can start prover sessions in theories with parameters`);

		const desc = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "alaris2lnewmodes.pump",
			formulaName: "vtbi_over_rate_lemma",
			theoryName: "pump_th"
		};
		// await pvsProxy.typecheckFile(desc); // typechecking, if needed, should be performed automatically by prove-formula
		let response: PvsResponse = await pvsProxy.proveFormula(desc);
		expect(response.result).toBeDefined();

		response = await pvsProxy.proofCommand({ cmd: 'quit' });
		expect(response.result).toEqual({ result: 'Unfinished' });
	}, 10000);

	it(`reports typecheck error when the prove command is executed but the theory does not typecheck`, async () => {
		label(`reports typecheck error when the prove command is executed but the theory does not typecheck`);

		const desc = {
			contextFolder: radixExamples,
			fileExtension: ".pvs",
			fileName: "mergesort-test",
			formulaName: "merge_size",
			theoryName: "mergesort_1"
		};
		let response: PvsResponse = await pvsProxy.proveFormula(desc);
		expect(response.result).not.toBeDefined();
		expect(response.error).toBeDefined();
	}, 2000);

	// the rationale for the following test case is to check that the following use case:
	// the user has defined formula l in file f1, and another formula with the same name l in file f2;
	// f1 typechecks correctly; f2 does not typecheck; the user tries to prove formula l in f2;
	// pvs-server should not start the proof and return a typecheck error
	it(`is able to distinguish theories with the same name that are stored in different files in the same context`, async () => {
		label(`is able to distinguish theories with the same name that are stored in different files in the same context`);

		// this version of the theory does not typecheck, so the prover should report error
		let desc = {
			contextFolder: radixExamples,
			fileExtension: ".pvs",
			fileName: "mergesort-test",
			formulaName: "merge_size",
			theoryName: "mergesort"
		};
		let response: PvsResponse = await pvsProxy.proveFormula(desc);
		// https://linkprotect.cudasvc.com/url?a=https%3a%2f%2fconsole.info&c=E,1,NoM5mYjxGeDV1TOA0t-WiaDqmrw8n1-kQHjHMTR6JZpVPSkgCt_XpTIuw7teYXJmPFlYrfOH8WI0FLyzi2Uhs4Jh4xh7tDl6uDnClWqO40Xj7sqi8a0,&typo=1('After proveFormula');
		expect(response.result).not.toBeDefined();
		expect(response.error).toBeDefined();
		// the following command should have no effect
		response = await pvsProxy.proofCommand({ cmd: 'quit' });
		expect(response.result).not.toBeDefined();
		expect(response.error).toBeDefined();

		// this version of the theory, on the other hand, typechecks correctly, so the prover should correctly start a prover session
		desc = {
			contextFolder: radixExamples,
			fileExtension: ".pvs",
			fileName: "mergesort",
			formulaName: "merge_size",
			theoryName: "mergesort"
		};
		response = await pvsProxy.proveFormula(desc);
		expect(response.result).not.toBeDefined();
		expect(response.error).toBeDefined();

		response = await pvsProxy.proofCommand({ cmd: 'quit' });
		// expect(response.result).toEqual({ result: 'Unfinished' });
		// expect(response.error).not.toBeDefined();
		expect(response.result).not.toBeDefined();
		expect(response.error).toBeDefined();

	}, 2000);

	it(`reports error when the prove command is executed but the theory does not exist`, async () => {
		label(`reports error when the prove command is executed but the theory does not exist`);

		let desc = {
			contextFolder: radixExamples,
			fileExtension: ".pvs",
			fileName: "mergesort-test",
			formulaName: "merge_size",
			theoryName: "mergesort_2"
		};
		let response: PvsResponse = await pvsProxy.proveFormula(desc);
		expect(response.result).not.toBeDefined();
		expect(response.error).toBeDefined();
	}, 2000);

	it(`reports error when the prove command is executed but the formula does not exist`, async () => {
		label(`reports error when the prove command is executed but the formula does not exist`);

		const desc = {
			contextFolder: radixExamples,
			fileExtension: ".pvs",
			fileName: "mergesort-test",
			formulaName: "mm",
			theoryName: "mergesort_1"
		};
		let response: PvsResponse = await pvsProxy.proveFormula(desc);
		expect(response.result).not.toBeDefined();
		expect(response.error).toBeDefined();
	}, 2000);

	it(`is robust to mistyped / malformed prover commands`, async () => {
		label(`is robust to mistyped / malformed prover commands`);

		const desc = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			formulaName: "sq_neg",
			theoryName: "sq",
			rerun: false
		};

		let response: PvsResponse = await pvsProxy.proveFormula(desc);
		// console.dir(response);
		expect(response.result.label).toEqual(test.sq_neg_prove_formula.label);
		expect(response.result.sequent).toEqual(test.sq_neg_prove_formula.sequent);

		// send proof command (skosimp*)
		response = await pvsProxy.proofCommand({ cmd: '(sko)'});
		// console.dir(response);
		expect(response.result.commentary).toBeDefined();
		expect(response.result.commentary[0].endsWith("not a valid prover command")).toBeTruthy();

		response = await pvsProxy.proofCommand({ cmd: '(sko'});
		// console.dir(response);
		expect(response.result.commentary).toBeDefined();
		// https://linkprotect.cudasvc.com/url?a=https%3a%2f%2fconsole.info&c=E,1,AT7dakqqXVu-FHSlFH9Hrjw1zlt284-jW7jXrzA6uc3Seyz6RugQ4bcG35MiOg69oMQWw6ry09NL58ETaefyoyb0cWxWkQkZKNd2clvLrSnA3SQTA0MD&typo=1(response.result.commentary);
		expect(response.result.commentary[0]).toContain("eof encountered");

		// quit the proof attempt
		await pvsProxy.proofCommand({ cmd: 'quit'});
	});

	it(`can start prover session while parsing files in other contexts`, async () => {
		label(`can start prover session while parsing files in other contexts`);

		// async call to the parser in context safesandbox
		pvsProxy.parseFile({ fileName: "alaris2lnewmodes", fileExtension: ".pvs", contextFolder: safeSandboxExamples });

		// call to prove-formula in sandbox, while the parser is running in the other context
		const desc = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "alaris2lnewmodes.pump",
			formulaName: "vtbi_over_rate_lemma",
			theoryName: "pump_th"
		};
		let response: PvsResponse = await pvsProxy.proveFormula(desc);
		expect(response.result).toBeDefined();
		expect(response.error).not.toBeDefined();

		response = await pvsProxy.proofCommand({ cmd: 'quit' });
		expect(response.result).toEqual({ result: 'Unfinished' });
	}, 60000);

	//-----------------------------------------------
	//--- the following test fail on Mac and Linux
	//-----------------------------------------------

	// on Mac and Linux, the following test fails and the prover crashes into Lisp
	// to activate the test case, change 'xit(...)' to 'it(...)'
	it(`is robust to prover commands with incorrect arguments`, async () => {
		label(`is robust to prover commands with incorrect arguments`);

		const desc = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			formulaName: "sq_neg",
			theoryName: "sq",
			rerun: false
		};

		let response: PvsResponse = await pvsProxy.proveFormula(desc);
		// console.dir(response);
		expect(response.result.label).toEqual(test.sq_neg_prove_formula.label);
		expect(response.result.sequent).toEqual(test.sq_neg_prove_formula.sequent);

		response = await pvsProxy.proofCommand({ cmd: '(expand "as <")'});
		// console.dir(response);
		expect(response.result.commentary).toBeDefined();
		//expect(response.result.commentary.startsWith("Found 'AS' when expecting 'EXPR'")).toBeTrue();

		// https://linkprotect.cudasvc.com/url?a=https%3a%2f%2fconsole.info&c=E,1,uoDpzuRE51RlQA1ExGIoQ8MktvIbqQctW4uzJY1w5MgnCqUOvQAvptgATnlEiq3KdlnuiTRokFGmsAroekZRP978OpCh3AqqQWrGsG4xTo3306Cb1sBdMJwCzQ,,&typo=1(response.result.commentary);
		// quit the proof attempt
		await pvsProxy.proofCommand({ cmd: 'quit'});
	});

	// on Mac and Linux, pvs-server fails with the following error:  { code: 1, message: '"No methods applicable for generic function #<standard-generic-function all-declarations> with args (nil) of classes (null)"' }
	// to activate the test case, change 'xit(...)' to 'it(...)'
	it(`prove-formula is robust to invocations with incorrect theory names`, async () => {
		label(`prove-formula is robust to invocations with incorrect theory names`);

		const desc = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "alaris2lnewmodes",
			formulaName: "check_chev_fup_permission",
			theoryName: "pump_th" // pump_th exists, but check_chev_fup_permission is in alaris_th
		};
		let response: PvsResponse = await pvsProxy.proveFormula(desc);
		expect(response.result).not.toBeDefined();
		expect(response.error).toBeDefined();
		expect(response.error.message.startsWith("Typecheck-error")).toBeTrue();

		response = await pvsProxy.proofCommand({ cmd: 'quit' });
		//console.dir(response);
		expect(response.result).not.toBeDefined();
		expect(response.error).toBeDefined();
		expect(response.error.message.startsWith('Proof-command error')).toBeTrue();
	}, 60000);

});
