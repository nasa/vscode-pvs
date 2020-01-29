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
	

	// on MacOs, pvs-server returns the following message: '"Value #<unknown object of type number 12 @ #x70000001003fc> is not of a type which can be encoded by encode-json."'
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
	


	// on Mac, pvs-server returns the following message: '"No methods applicable for generic function #<standard-generic-function id> with args (nil) of classes (null)"' }) not to be defined.
	it(`returns a well-formed proof script when the proof file is not available`, async () => {
		label(`returns a well-formed proof script when the proof file is not available`);

		// a well-formed response for formula sqrt_0 is in the form /;;; Proof sqrt_0(\-\d+)? for formula sqrt.sqrt_0\n(""\s*)/
		const fname: string = path.join(sandboxExamples, "sqrt.pvs");

		const response: PvsResponse = await pvsProxy.pvsRequest('proof-script', [ fname, "sqrt_0" ]);
		dir("response", response);
		expect(response.error).not.toBeDefined();
		expect(response.result).toBeDefined();
		expect(response.result).toMatch(/;;; Proof sqrt_0(\-\d+)? for formula sqrt.sqrt_0\n(""\s*)/);
	});


	// on Mac, pvs-server returns the following error: { code: 1, message: '"the assertion oplace failed."' },
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
	}, 10000);

	// on Mac, pvs-server does not send a response back to the proxy, and pvs shows an error #<pvs-error @ #x1008b7da82> [condition type: pvs-error]
	it(`reports typecheck error when the prove command is executed but the theory does not typecheck`, async () => {
		label(`reports typecheck error when the prove command is executed but the theory does not typecheck`);

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

	
	// on Mac, pvs-server does not send a response back to the proxy, and pvs shows an error #<pvs-error @ #x1008828e62> [condition type: pvs-error]
	it(`reports error when the prove command is executed but the theory does not exist`, async () => {
		label(`reports error when the prove command is executed but the theory does not exist`);

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


	// on Mac, pvs-server does not send a response back to the proxy, and pvs shows an error #<pvs-error @ #x1008b7da22> [condition type: pvs-error]
	it(`reports error when the prove command is executed but the formula does not exist`, async () => {
		label(`reports error when the prove command is executed but the formula does not exist`);

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
		expect(response.result).toBeDefined();
		expect(response.error).not.toBeDefined();

		response = await pvsProxy.proofCommand({ cmd: 'quit' });
		expect(response.result).toEqual({ result: 'Unfinished' });
		expect(response.error).toBeDefined();

	}, 2000);

	// the following test fails because pvs-server is not reading the proof command (quit) after sending a malformed proof command (sko
	it(`is robust to incorrect / malformed prover commands`, async () => {
		label(`is robust to incorrect / malformed prover commands`);

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
		response = await pvsProxy.proofCommand({ cmd: '(sko)'});
		expect(response.result.commentary).toBeDefined();
		expect(response.results.commentary[0].endsWith("not a valid prover command")).toBeTruthy();

		response = await pvsProxy.proofCommand({ cmd: '(sko'});
		expect(response.result.commentary).toBeDefined();
		expect(response.results.commentary[0].endsWith("not a valid prover command")).toBeTruthy();

		// quit the proof attempt
		await pvsProxy.proofCommand({ cmd: 'quit'});
	});

	return; // the following tests are completed successfully -- remove the return statement if you want to run them

	// OK
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

	}, 4000);
	
	// OK
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
	
	// OK
	it(`returns proverStatus = inactive when a prover session is not active`, async () => {
		label(`returns proverStatus = inactive when a prover session is not active`);

		const proverStatus: PvsResponse = await pvsProxy.proverStatus();
		expect(proverStatus.result).toEqual("inactive");
	}, 4000);


	//OK
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

	// OK
	it(`can generate .tcc file content`, async () => {
		label(`can generate the .tcc file content`);
		const response: PvsResponse = await pvsProxy.showTccs({ fileName: "sqrt", fileExtension: ".pvs", theoryName: "sqrt", contextFolder: sandboxExamples });
		dir("response", response);
		expect(response.error).not.toBeDefined();
		expect(response.result).not.toBeNull();
	});
	
	// OK
	it(`can discharge tccs`, async () => {
		label(`can discharge tccs`);
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

	// OK 
	it(`can show proof script`, async () => {
		label(`show proof script`);
		const fname: string = path.join(sandboxExamples, "sq.pvs");

		const response: PvsResponse = await pvsProxy.pvsRequest('proof-script', [ fname, "sq_neg" ]);
		dir("response", response);
		expect(response.error).not.toBeDefined();
		expect(response.result).toBeDefined();
		const proof_header: string = response.result.split("\n")[0];
		expect(proof_header).toEqual(`;;; Proof sq_neg-1 for formula sq.sq_neg`);
	});

	// OK
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

});