import * as fsUtils from "../server/src/common/fsUtils";
import * as test from "./test-constants";
import { PvsResponse, PvsResult } from "../server/src/common/pvs-gui";
import { PvsProxy } from '../server/src/pvsProxy'; // XmlRpcSystemMethods
import { configFile, sandboxExamples, safeSandboxExamples, radixExamples } from './test-utils';
import { PvsFormula, PvsProofCommand } from "../server/src/common/serverInterface";
import * as path from 'path';

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
		await fsUtils.cleanBin(safeSandboxExamples);
		await fsUtils.cleanBin(sandboxExamples);
		await fsUtils.cleanBin(radixExamples);

		console.log("\n----------------------");
		console.log("test-prover");
		console.log("----------------------");
	});
	afterAll(async () => {
		await pvsProxy.killPvsServer();
		await pvsProxy.killPvsProxy();
		// delete pvsbin files and .pvscontext
		await fsUtils.cleanBin(safeSandboxExamples);
		await fsUtils.cleanBin(sandboxExamples);
		await fsUtils.cleanBin(radixExamples);
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

	// @Sam: this first test fails intermittently.
	//       It seems that pvs returns a response before it's ready to accept a proof command (if a delay is introduced before sending the command request then the test succeeds)
	//       There is also a problem with the prover status: sometimes pvs returns the following error:
	//            'Value #<unknown object of type number 3 @\n' +
	//            '        #x107000000100223> is not of a type which can be encoded by encode-json.'
	//       This error usually occurs when the server is restarted, during the first prover session  
	it(`can start a proof and step proof commands`, async () => { // to run all tests, change fit(...) into it(...)
		const proverStatus: PvsResult = await pvsProxy.pvsRequest('prover-status'); // await pvsProxy.getProverStatus();
		expect(proverStatus.result).toBeDefined();
		expect(proverStatus.error).not.toBeDefined();
		console.log(proverStatus);
		
		if (proverStatus && proverStatus.result !== "inactive") {
			await pvsProxy.proofCommand({ cmd: 'quit' });
		}
		const baseFolder: string = path.join(__dirname, "proof-explorer");
		const request: PvsProofCommand = {
			contextFolder: path.join(baseFolder, "foo"),
			fileExtension: '.pvs',
			fileName: 'foo',
			formulaName: 'foo1',
			theoryName: 'foo_th',
			cmd: "(skosimp*)"
		};
		let response: PvsResponse = await pvsProxy.proveFormula(request);
		// console.dir(response, { depth: null });
		
		expect(response.result).toBeDefined();
		expect(response.error).not.toBeDefined();

		response = await pvsProxy.proofCommand({ cmd: '(skosimp*)' });
		expect(response.result).toBeDefined();
		expect(response.error).not.toBeDefined();	
	});

	//----- the tests below this line are completed successfully

	it(`can discharge tccs`, async () => {
		await quitProverIfActive();
		
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
		await quitProverIfActive();

		const desc: PvsFormula = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "alaris2lnewmodes",
			formulaName: "check_chev_fup_permission",
			theoryName: "alaris_th"
		};
		let response: PvsResponse = await pvsProxy.proveFormula(desc);
		console.log(response);
		// expect(response.result).toBeDefined();
		// expect(response.error).not.toBeDefined();

		response = await pvsProxy.proofCommand({ cmd: 'quit' });
		// console.dir(response);
		// expect(response.result[0].commentary[0]).toEqual('Unfinished');
	}, 60000);

	it(`can start interactive proof session when the formula has already been proved`, async () => {
		await quitProverIfActive();
		
		const desc: PvsFormula = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			formulaName: "sq_neg",
			theoryName: "sq"
		};

		let response: PvsResponse = await pvsProxy.proveFormula(desc);
		// console.dir(response);
		expect(response.result[0].label).toEqual(test.sq_neg_prove_formula.label);
		expect(response.result[0].sequent.succedents).toBeDefined();

		try {
			// send proof command (skosimp*)
			response = await pvsProxy.proofCommand({ cmd: '(skosimp*)'});
			// console.dir(response);
			expect(response.result[0].label).toEqual(test.sq_neg_proof_command_skosimp_star.label);
			expect(response.result[0].action).toEqual(test.sq_neg_proof_command_skosimp_star.action);
			expect(response.result[0].sequent).toBeDefined();

			// send proof command (expand "sq")
			response = await pvsProxy.proofCommand({ cmd: '(expand "sq")'});
			// console.dir(response);
			expect(response.result[0].label).toEqual(test.sq_neg_expand.label);
			expect(response.result[0].action).toEqual(test.sq_neg_expand.action);
			expect(response.result[0].sequent).toBeDefined();

			// send proof command (assert) to complete the proof
			response = await pvsProxy.proofCommand({ cmd: '(assert)'});
			// console.dir(response);
			expect(response.result[0].commentary[0]).toEqual('Q.E.D.');

			// try to re-start the proof
			response = await pvsProxy.proveFormula(desc);
			// console.dir(response);
			expect(response.result[0].label).toEqual(test.sq_neg_prove_formula.label);
			expect(response.result[0].sequent).toBeDefined();

			// send proof command (skosimp*)
			response = await pvsProxy.proofCommand({ cmd: '(skosimp*)'});
			// console.dir(response);
			expect(response.result[0].label).toEqual(test.sq_neg_proof_command_skosimp_star.label);
			expect(response.result[0].action).toEqual(test.sq_neg_proof_command_skosimp_star.action);
			expect(response.result[0].sequent).toBeDefined();
		}
		finally {
			// quit the proof attempt
			await pvsProxy.proofCommand({ cmd: 'quit'});
		}

	}, 4000);
	
	it(`can start a prover session and quit the prover session`, async () => {
		await quitProverIfActive();
		
		const desc: PvsFormula = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			formulaName: "sq_neg",
			theoryName: "sq"
		};
		let response: PvsResponse = await pvsProxy.proveFormula(desc);
		expect(response.result[0].sequent).toBeDefined();;

		response = await pvsProxy.proofCommand({ cmd: 'quit' });
		// console.dir(response);
		expect(response.result[0].commentary[0]).toEqual('Unfinished');
	}, 20000);
	
	it(`returns proverStatus = inactive when a prover session is not active`, async () => {
		const proverStatus: PvsResponse = await pvsProxy.getProverStatus();
		expect(proverStatus.result).toEqual("inactive");
	}, 4000);

	it(`returns proverStatus = active when a prover session is active`, async () => {
		const desc: PvsFormula = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			formulaName: "sq_times",
			theoryName: "sq"
		};

		// start prover session
		await pvsProxy.proveFormula(desc);
		// check prover status
		const proverStatus: PvsResponse = await pvsProxy.getProverStatus();
		expect(proverStatus.result).toEqual("active");

		// quit the proof attempt
		await pvsProxy.proofCommand({ cmd: 'quit'});
	}, 4000);

	it(`can invoke prove-formula on theories with parameters`, async () => {
		const desc: PvsFormula = {
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
		expect(response.result[0].commentary[0]).toEqual('Unfinished');
	}, 60000);	

	it(`returns proverStatus = inactive after quitting a prover session`, async () => {
		const desc: PvsFormula = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			formulaName: "sq_times",
			theoryName: "sq"
		};

		// start prover session
		await pvsProxy.proveFormula(desc);
		// quit the proof attempt
		await pvsProxy.proofCommand({ cmd: 'quit'});
		// check prover status
		const proverStatus: PvsResponse = await pvsProxy.getProverStatus();
		expect(proverStatus.result).toEqual("inactive");
	}, 4000);

	it(`can start prover sessions in theories with parameters`, async () => {
		const desc: PvsFormula = {
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
		expect(response.result[0].commentary[0]).toEqual('Unfinished');
	}, 10000);

	it(`reports typecheck error when the prove command is executed but the theory does not typecheck`, async () => {
		const desc: PvsFormula = {
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
		// this version of the theory does not typecheck, so the prover should report error
		let desc: PvsFormula = {
			contextFolder: radixExamples,
			fileExtension: ".pvs",
			fileName: "mergesort-test",
			formulaName: "merge_size",
			theoryName: "mergesort"
		};
		let response: PvsResponse = await pvsProxy.proveFormula(desc);
		expect(response.result).not.toBeDefined();
		expect(response.error).toBeDefined();
		// the following command should have no effect
		response = await pvsProxy.proofCommand({ cmd: 'quit' });
		expect(response.result[0].commentary[0]).toEqual("No change on: quit");
		expect(response.error).toBeDefined();

		// this other version of the theory, on the other hand, typechecks correctly
		// pvs should report a typecheck error because two theories with the same name are in the same context folder
		desc = {
			contextFolder: radixExamples,
			fileExtension: ".pvs",
			fileName: "mergesort",
			formulaName: "merge_size",
			theoryName: "mergesort"
		};
		response = await pvsProxy.proveFormula(desc);
		// console.dir(response);
		expect(response.result).not.toBeDefined();
		expect(response.error).toBeDefined();
		expect(response.error.data.error_string).toContain("has been declared previously");
	}, 2000);

	it(`reports error when trying to prove a theory that does not exist`, async () => {
		let desc: PvsFormula = {
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
		const desc: PvsFormula = {
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
		const desc: PvsFormula = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			formulaName: "sq_neg",
			theoryName: "sq"
		};

		let response: PvsResponse = await pvsProxy.proveFormula(desc);
		// console.dir(response);
		expect(response.result[0].label).toEqual(test.sq_neg_prove_formula.label);
		expect(response.result[0].sequent).toBeDefined();

		// send proof command (skosimp*)
		response = await pvsProxy.proofCommand({ cmd: '(sko)'});
		// console.dir(response);
		expect(response.result[0].commentary).toBeDefined();
		expect(response.result[0].commentary[0].endsWith("not a valid prover command")).toBeTruthy();

		response = await pvsProxy.proofCommand({ cmd: '(sko'});
		// console.dir(response);
		expect(response.result[0].commentary).toBeDefined();
		// console.dir(response.result[0].commentary);
		expect(response.result[0].commentary[0]).toContain("No change on: (sko");

		// quit the proof attempt
		await pvsProxy.proofCommand({ cmd: 'quit'});
	});

	// // the following test is disabled because pvs does not support parallel processing of files
	// xit(`can start prover session while parsing files in other contexts`, async () => {
	// 	// async call to the parser in context safesandbox
	// 	pvsProxy.parseFile({ fileName: "alaris2lnewmodes", fileExtension: ".pvs", contextFolder: safeSandboxExamples });

	// 	// call to prove-formula in sandbox, while the parser is running in the other context
	// 	const desc: PvsFormula = {
	// 		contextFolder: sandboxExamples,
	// 		fileExtension: ".pvs",
	// 		fileName: "alaris2lnewmodes.pump",
	// 		formulaName: "vtbi_over_rate_lemma",
	// 		theoryName: "pump_th"
	// 	};
	// 	let response: PvsResponse = await pvsProxy.proveFormula(desc);
	// 	expect(response.result).toBeDefined();
	// 	expect(response.error).not.toBeDefined();

	// 	response = await pvsProxy.proofCommand({ cmd: 'quit' });
	// 	expect(response.result).toEqual({ result: 'Unfinished' });
	// }, 60000);

	//-----------------------------------------------
	//--- the following test fail on Mac and Linux
	//-----------------------------------------------

	// on Mac and Linux, the following test fails and the prover crashes into Lisp
	// to activate the test case, change 'xit(...)' to 'it(...)'
	it(`is robust to prover commands with incorrect arguments`, async () => {
		const desc: PvsFormula = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			formulaName: "sq_neg",
			theoryName: "sq"
		};

		let response: PvsResponse = await pvsProxy.proveFormula(desc);
		// console.dir(response);
		expect(response.result[0].label).toEqual(test.sq_neg_prove_formula.label);
		expect(response.result[0].sequent).toBeDefined();

		response = await pvsProxy.proofCommand({ cmd: '(expand "as <")'});
		// console.dir(response);
		expect(response.result[0].commentary).toBeDefined();
		//expect(response.result.commentary.startsWith("Found 'AS' when expecting 'EXPR'")).toBeTrue();

		// quit the proof attempt
		await pvsProxy.proofCommand({ cmd: 'quit'});
	});

	// on Mac and Linux, pvs-server fails with the following error:  { code: 1, message: '"No methods applicable for generic function #<standard-generic-function all-declarations> with args (nil) of classes (null)"' }
	// to activate the test case, change 'xit(...)' to 'it(...)'
	it(`prove-formula is robust to invocations with incorrect theory names`, async () => {
		const desc: PvsFormula = {
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
		expect(response.result[0].commentary[0]).toContain("No change on: quit");
		expect(response.error).toBeDefined();
		expect(response.error.message).toContain('Proof-command error');
	}, 60000);


	fit(`can interrupt prover commands`, async () => {
		await quitProverIfActive();

		const desc: PvsFormula = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "alaris2lnewmodes",
			formulaName: "check_chev_fup_permission",
			theoryName: "alaris_th"
		};
		await pvsProxy.proveFormula(desc);
		pvsProxy.proofCommand({ cmd: '(grind)' }); // async call

		let response: PvsResponse = await new Promise((resolve, reject) => {
			setTimeout(async () => {
				let response: PvsResponse = await pvsProxy.pvsRequest("interrupt");
				console.dir(response);
				resolve(response);
			}, 1000);
		});
		expect(response.result).toBeDefined();
		expect(response.result[0].label).toBeDefined();
		expect(response.result[0].sequent).toBeDefined();
	}, 20000);
});
