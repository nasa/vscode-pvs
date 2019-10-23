// import { ContextDiagnostics } from "./server/pvsProcess";
//import { PvsFindDeclaration, PvsParserResponse, PvsTypecheckerResponse, XmlRpcResponse } from "./server/common/serverInterface";
import * as fsUtils from "./server/common/fsUtils";
import * as test from "./test-constants";
import * as path from 'path';
import { ParseResult, ListMethodsResult, PvsError, PvsResponse, PvsResult, FindDeclarationResult } from "./server/common/pvs-gui";
import { PvsProxy, ContextDiagnostics } from './server/pvsProxy'; // XmlRpcSystemMethods
import { label, log, dir, configFile, sandboxExamples } from './test-utils';
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
		pvsProxy = new PvsProxy(pvsPath, { externalServer: false });
		await pvsProxy.activate({ debugMode: true });

		// delete pvsbin files
		await fsUtils.deletePvsCache(sandboxExamples);
	});
	afterAll(async () => {
		// delete pvsbin files
		await fsUtils.deletePvsCache(sandboxExamples);
	});
	it(`pvs-server can start a prover session and quit the prover session`, async () => {
		label(`pvs-server can start a prover session and quit the prover session`);

		const desc = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			formulaName: "sq_neg",
			line: 12,
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
			line: 12,
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

		let proverStatus: PvsResponse = await pvsProxy.proverStatus();
		expect(proverStatus.result).toEqual("inactive");
	}, 10000);


	it(`pvs-server returns proverStatus = active when a prover session is active`, async () => {
		label(`pvs-server returns proverStatus = active when a prover session is active`);

		const desc = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			formulaName: "sq_times",
			line: 18,
			theoryName: "sq",
			rerun: false
		};

		// start prover session
		await pvsProxy.proveFormula(desc);
		// check prover status
		const proverStatus: PvsResponse = await pvsProxy.proverStatus();
		expect(proverStatus.result).toEqual("active");
	}, 10000);

	it(`pvs-server returns proverStatus = inactive after quitting a prover session`, async () => {
		label(`pvs-server returns proverStatus = inactive after quitting a prover session`);

		const desc = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			formulaName: "sq_times",
			line: 18,
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
		expect(response.result).toBeDefined();
		expect(response.error).not.toBeDefined();
	}, 4000);


	// show proof
	it(`pvs-server can show proof script`, async () => {
		label(`pvs-server show proof script`);
		const fname: string = path.join(sandboxExamples, "sq.pvs");

		const response: PvsResponse = await pvsProxy.pvsRequest('proof-script', [ fname, "sq_neg" ]);
		dir("response", response);
		expect(response.result).toBeDefined();
		expect(response.error).not.toBeDefined();
	});

	it(`pvs-server can start prover sessions in theories with parameters`, async () => {
		label(`pvs-server can start prover sessions in theories with parameters`);

		const desc = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "alaris2lnewmodes.pump",
			formulaName: "vtbi_over_rate_lemma",
			line: 28,
			theoryName: "pump_th"
		};
		await pvsProxy.typecheckFile(desc);
		let response: PvsResponse = await pvsProxy.proveFormula(desc);
		expect(response.result).toBeDefined();

		response = await pvsProxy.proofCommand({ cmd: 'quit' });
		expect(response.result).toEqual({ result: 'Unfinished' });
	}, 10000);

});