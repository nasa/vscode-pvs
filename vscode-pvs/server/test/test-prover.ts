// import { ContextDiagnostics } from "./server/pvsProcess";
//import { PvsFindDeclaration, PvsParserResponse, PvsTypecheckerResponse, XmlRpcResponse } from "./server/common/serverInterface";
import * as fsUtils from "./server/common/fsUtils";
import * as test from "./test-constants";
import * as path from 'path';
import { PvsResponse } from "./server/common/pvs-gui";
import { PvsProxy } from './server/pvsProxy'; // XmlRpcSystemMethods
import { label, log, dir, configFile, sandboxExamples, safeSandboxExamples, radixExamples } from './test-utils';


//----------------------------
//   Test cases for prover
//----------------------------
describe("pvs-prover", () => {
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
	
	it(`pvs-server can start a prover session and quit the prover session`, async () => {
		label(`pvs-server can start a prover session and quit the prover session`);

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

	it(`pvs-server can start interactive proof session when the formula has already been proved`, async () => {
		label(`pvs-server can start interactive proof session when the formula has already been proved`);

		const desc = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			formulaName: "sq_neg",
			theoryName: "sq",
			rerun: false
		};

		let response: PvsResponse = await pvsProxy.proveFormula(desc);
		console.dir(response);
		expect(response.result.label).toEqual(test.sq_neg_prove_formula.label);
		expect(response.result.sequent).toEqual(test.sq_neg_prove_formula.sequent);

		// send proof command (skosimp*)
		response = await pvsProxy.proofCommand({ cmd: '(skosimp*)'});
		console.dir(response);
		expect(response.result).toEqual(test.sq_neg_proof_command_skosimp_star);

		// send proof command (expand "sq")
		response = await pvsProxy.proofCommand({ cmd: '(expand "sq")'});
		console.dir(response);
		expect(response.result).toEqual(test.sq_neg_expand);

		// send proof command (assert) to complete the proof
		response = await pvsProxy.proofCommand({ cmd: '(assert)'});
		console.dir(response);
		expect(response.result).toEqual({ result: 'Q.E.D.' });

		// try to re-start the proof
		response = await pvsProxy.proveFormula(desc);
		console.dir(response);
		expect(response.result.label).toEqual(test.sq_neg_prove_formula.label);
		expect(response.result.sequent).toEqual(test.sq_neg_prove_formula.sequent);

		// send proof command (skosimp*)
		response = await pvsProxy.proofCommand({ cmd: '(skosimp*)'});
		console.dir(response);
		expect(response.result).toEqual(test.sq_neg_proof_command_skosimp_star);

		// quit the proof attempt
		await pvsProxy.proofCommand({ cmd: 'quit'});

	}, 20000);

	it(`pvs-server returns proverStatus = inactive when a prover session is not active`, async () => {
		label(`pvs-server returns proverStatus = inactive when a prover session is not active`);

		const proverStatus: PvsResponse = await pvsProxy.proverStatus();
		expect(proverStatus.result).toEqual("inactive");
	}, 10000);


	it(`pvs-server returns proverStatus = active when a prover session is active`, async () => {
		label(`pvs-server returns proverStatus = active when a prover session is active`);

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
	}, 10000);

	it(`pvs-server returns proverStatus = inactive after quitting a prover session`, async () => {
		label(`pvs-server returns proverStatus = inactive after quitting a prover session`);

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
	}, 10000);
	

	it(`pvs-server can generate .tcc file content`, async () => {
		label(`pvs-server can generate the .tcc file content`);
		const response: PvsResponse = await pvsProxy.showTccs({ fileName: "sqrt", fileExtension: ".pvs", theoryName: "sqrt", contextFolder: sandboxExamples });
		dir("response", response);
		expect(response.error).not.toBeDefined();
		expect(response.result).not.toBeNull();
	});
	
	// discharge tccs
	it(`pvs-server can discharge tccs`, async () => {
		label(`pvs-server can discharge tccs`);
		const fname: string = path.join(sandboxExamples, "sq.pvs");

		const response: PvsResponse = await pvsProxy.pvsRequest('prove-tccs', [ fname ]);
		dir("response", response);
		expect(response.error).not.toBeDefined();
		expect(response.result).toBeDefined();
		expect(response.result.totals).toEqual(2);
		expect(response.result.proved).toEqual(2);
		expect(response.result.unproved).toEqual(0);
		expect(response.result.subsumed).toEqual(0);
		expect(response.result.simplified).toEqual(0);
	}, 4000);

	// show proof when proof script is available 
	it(`pvs-server can show proof script`, async () => {
		label(`pvs-server show proof script`);
		const fname: string = path.join(sandboxExamples, "sq.pvs");

		const response: PvsResponse = await pvsProxy.pvsRequest('proof-script', [ fname, "sq_neg" ]);
		dir("response", response);
		expect(response.result).toBeDefined();
		expect(response.error).not.toBeDefined();
		expect(response.result).toEqual(`;;; Proof sq_neg-1 for formula sq.sq_neg\n("" (skosimp*) (postpone))`);
	});


	// show proof when proof script is NOT available 
	it(`pvs-server returns a well-formed proof script when the proof file is not available`, async () => {
		label(`pvs-server returns a well-formed proof script when the proof file is not available`);

		// a well-formed response for formula sqrt_0 is in the form /;;; Proof sqrt_0(\-\d+)? for formula sqrt.sqrt_0\n(""\s*)/
		const fname: string = path.join(sandboxExamples, "sqrt.pvs");

		const response: PvsResponse = await pvsProxy.pvsRequest('proof-script', [ fname, "sqrt_0" ]);
		dir("response", response);
		expect(response.error).not.toBeDefined();
		expect(response.result).toBeDefined();
		expect(response.result).toMatch(/;;; Proof sqrt_0(\-\d+)? for formula sqrt.sqrt_0\n(""\s*)/);
	});

	// prover session in theories with parameters
	it(`pvs-server can start prover sessions in theories with parameters`, async () => {
		label(`pvs-server can start prover sessions in theories with parameters`);

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

	// start prover session while parsing
	it(`pvs-server can start prover session while parsing files in other contexts`, async () => {
		label(`pvs-server can start prover session while parsing files in other contexts`);

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
	}, 10000);

	// prover session is not started if theory does not typecheck
	it(`pvs-server reports typecheck error when the prove command is executed but the theory does not typecheck`, async () => {
		label(`pvs-server reports typecheck error when the prove command is executed but the theory does not typecheck`);

		const desc = {
			contextFolder: radixExamples,
			fileExtension: ".pvs",
			fileName: "mergesort-test.pvs",
			formulaName: "merge_size",
			theoryName: "mergesort_1"
		};
		let response: PvsResponse = await pvsProxy.proveFormula(desc);
		expect(response.result).not.toBeDefined();
		expect(response.error).toBeDefined();
	}, 2000);
	
	// prover session is not started if theory does not exist
	it(`pvs-server reports error when the prove command is executed but the theory does not exist`, async () => {
		label(`pvs-server reports error when the prove command is executed but the theory does not exist`);

		let desc = {
			contextFolder: radixExamples,
			fileExtension: ".pvs",
			fileName: "mergesort-test.pvs",
			formulaName: "merge_size",
			theoryName: "mergesort_2"
		};
		let response: PvsResponse = await pvsProxy.proveFormula(desc);
		expect(response.result).not.toBeDefined();
		expect(response.error).toBeDefined();
	}, 2000);

	// prover session is not started if the formula does not exist
	it(`pvs-server reports error when the prove command is executed but the formula does not exist`, async () => {
		label(`pvs-server reports error when the prove command is executed but the formula does not exist`);

		const desc = {
			contextFolder: radixExamples,
			fileExtension: ".pvs",
			fileName: "mergesort-test.pvs",
			formulaName: "mm",
			theoryName: "mergesort_1"
		};
		let response: PvsResponse = await pvsProxy.proveFormula(desc);
		expect(response.result).not.toBeDefined();
		expect(response.error).toBeDefined();
	}, 2000);

	// prover is able to distinguish theories with the same name that are stored in different files in the same context
	it(`pvs-server is able to distinguish theories with the same name that are stored in different files in the same context`, async () => {
		label(`pvs-server is able to distinguish theories with the same name that are stored in different files in the same context`);

		// this version of the theory does not typecheck, so the prover should report error
		let desc = {
			contextFolder: radixExamples,
			fileExtension: ".pvs",
			fileName: "mergesort-test.pvs",
			formulaName: "merge_size",
			theoryName: "mergesort"
		};
		let response: PvsResponse = await pvsProxy.proveFormula(desc);
		expect(response.result).not.toBeDefined();
		expect(response.error).toBeDefined();
		// the following command should have no effect
		response = await pvsProxy.proofCommand({ cmd: 'quit' });
		expect(response.result).not.toBeDefined();
		expect(response.error).toBeDefined();


		// this version of the theory typechecks correctly, so the prover should correctly start a prover session
		desc = {
			contextFolder: radixExamples,
			fileExtension: ".pvs",
			fileName: "mergesort.pvs",
			formulaName: "merge_size",
			theoryName: "mergesort"
		};
		response = await pvsProxy.proveFormula(desc);
		expect(response.result).toBeDefined();
		expect(response.error).not.toBeDefined();

		response = await pvsProxy.proofCommand({ cmd: 'quit' });
		expect(response.result).toEqual({ result: 'Unfinished' });
		expect(response.error).toBeDefined();

	}, 2000);
	
	
});