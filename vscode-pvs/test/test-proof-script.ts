import * as fsUtils from "../server/src/common/fsUtils";
import * as test from "./test-constants";
import { PvsResponse, PvsResult } from "../server/src/common/pvs-gui";
import { PvsProxy } from '../server/src/pvsProxy'; // XmlRpcSystemMethods
import { label, log, configFile, sandboxExamples } from './test-utils';
import { ProofDescriptor, ProofFile, PvsFormula, PvsTheory } from "../server/src/common/serverInterface";
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
		pvsProxy = new PvsProxy(pvsPath, { externalServer: true });
		await pvsProxy.activate({ debugMode: false, showBanner: false }); // this will also start pvs-server

		// delete pvsbin files and .pvscontext
		await fsUtils.cleanBin(sandboxExamples);

		console.log("\n----------------------");
		console.log("test-proof-script");
		console.log("----------------------");
	});
	afterAll(async () => {
		await pvsProxy.killPvsServer();
		await pvsProxy.killPvsProxy();
		// delete pvsbin files and .pvscontext
		setTimeout(async () => {
			await fsUtils.cleanBin(sandboxExamples);
		}, 600);
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

	it(`can provide pvs proof scripts`, async () => {
		label(`can provide pvs proof scripts`);

		const desc = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			formulaName: "sq_neg",
			theoryName: "sq"
		};
		const response: PvsResponse = await pvsProxy.getDefaultProofScript(desc);
		expect(response.error).not.toBeDefined();
		expect(response.result).toBeDefined();
		const proof_header: string = response.result.split("\n")[0];
		expect(proof_header).toEqual(`;;; Proof sq_neg-1 for formula sq.sq_neg`);
	});
	
	it(`returns a well-formed empty pvs proof script when the proof file is not available`, async () => {
		label(`returns a well-formed empty pvs proof script when the proof file is not available`);

		// a well-formed response for formula sqrt_0 is in the form /;;; Proof sqrt_0(\-\d+)? for formula sqrt.sqrt_0\n(""\s*)/
		const desc = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sqrt",
			formulaName: "sqrt_0",
			theoryName: "sqrt"
		};
		let response: PvsResponse = await pvsProxy.getDefaultProofScript(desc);
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
		response = await pvsProxy.getDefaultProofScript(desc1);
		// console.dir(response);
		expect(response.result).not.toBeDefined();
		expect(response.error).toBeDefined();
		expect(response.error.data.error_string).toMatch(/(.*) does not have a proof/);
	});
	
	fit(`can load & save vscode-pvs proof file`, async () => {
		await quitProverIfActive();

		const formula: PvsFormula = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			formulaName: "sq_plus_eq_0",
			theoryName: "sq"
		};
		let response: PvsResponse = await pvsProxy.proveFormula(formula);
		response = await pvsProxy.proofCommand({ cmd: "(grind)" });
		response = await pvsProxy.proofCommand({ cmd: "(quit)" });
		console.dir(response);
		// response = await pvsProxy.storeLastProofAndSave(formula);
		const fullTheoryName: string = path.join(formula.contextFolder, formula.fileName + ".pvs" + "#" + formula.theoryName);
		response = await pvsProxy.pvsRequest("store-last-attempted-proof", [ formula.formulaName, fullTheoryName ]);
		if (response && response.result) {
			response = await pvsProxy.pvsRequest("change-context", [ formula.contextFolder ]);
			response = await pvsProxy.pvsRequest("save-all-proofs", [ fullTheoryName ]);
		}
		expect(response.result).toBeDefined();
		console.dir(response);
	});

	it(`can load vscode-pvs proof descriptors`, async () => {
		label(`can load vscode-pvs proof descriptors`);

		const fname: string = fsUtils.desc2fname({
			contextFolder: sandboxExamples,
			fileExtension: ".jprf",
			fileName: "sq"
		});
		fsUtils.deleteFile(fname);

		const desc = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			formulaName: "sq_plus_eq_0",
			theoryName: "sq"
		};
		const proofDescriptor: ProofDescriptor = await pvsProxy.openProof(desc);
		expect(proofDescriptor).toBeDefined();
		expect(proofDescriptor.info.formula).toEqual(test.sq_plus_eq_0_desc.info.formula);
		expect(proofDescriptor.info.theory).toEqual(test.sq_plus_eq_0_desc.info.theory);
		expect(proofDescriptor.info.status).toEqual(test.sq_plus_eq_0_desc.info.status);
		expect(proofDescriptor.info.shasum).toEqual(test.sq_plus_eq_0_desc.info.shasum);
		expect(proofDescriptor.proofTree).toEqual(test.sq_plus_eq_0_desc.proofTree);
		// console.dir(proofDescriptor, { depth: null });

		fsUtils.deleteFile(fname);
	});

	// it(`can save new vscode-pvs proof descriptors`, async () => {
	// 	label(`can save new vscode-pvs proof descriptors`);

	// 	const fname: string = fsUtils.desc2fname({
	// 		contextFolder: sandboxExamples,
	// 		fileExtension: ".jprf",
	// 		fileName: "sq"
	// 	});
	// 	fsUtils.deleteFile(fname);

	// 	const desc = {
	// 		contextFolder: sandboxExamples,
	// 		fileExtension: ".pvs",
	// 		fileName: "sq",
	// 		formulaName: "sq_plus_eq_0",
	// 		theoryName: "sq"
	// 	};

	// 	// update proof descriptor
	// 	const success: boolean = await pvsProxy.saveProof({
	// 		fileName: desc.fileName,
	// 		fileExtension: desc.fileExtension,
	// 		theoryName: desc.theoryName,
	// 		formulaName: desc.formulaName,
	// 		contextFolder: desc.contextFolder,
	// 		proofDescriptor: test.sq_plus_eq_0_desc_new
	// 	});
	// 	expect(success).toBeTrue();

	// 	const newProofDescriptor: ProofDescriptor = await pvsProxy.openProof(desc);
	// 	expect(newProofDescriptor).toBeDefined();
	// 	expect(newProofDescriptor.info.formula).toEqual(test.sq_plus_eq_0_desc_new.info.formula);
	// 	expect(newProofDescriptor.info.theory).toEqual(test.sq_plus_eq_0_desc_new.info.theory);
	// 	expect(newProofDescriptor.info.status).toEqual(test.sq_plus_eq_0_desc_new.info.status);
	// 	expect(newProofDescriptor.info.shasum).toEqual(test.sq_plus_eq_0_desc_new.info.shasum);
	// 	expect(newProofDescriptor.proofTree).toEqual(test.sq_plus_eq_0_desc_new.proofTree);

	// 	fsUtils.deleteFile(fname);
	// });

	// it(`can update existing vscode-pvs proof descriptors`, async () => {
	// 	label(`can update existing vscode-pvs proof descriptors`);

	// 	const fname: string = fsUtils.desc2fname({
	// 		contextFolder: sandboxExamples,
	// 		fileExtension: ".jprf",
	// 		fileName: "sq"
	// 	});
	// 	fsUtils.deleteFile(fname);

	// 	const desc = {
	// 		contextFolder: sandboxExamples,
	// 		fileExtension: ".pvs",
	// 		fileName: "sq",
	// 		formulaName: "sq_plus_eq_0",
	// 		theoryName: "sq"
	// 	};
	// 	const proofDescriptor: ProofDescriptor = await pvsProxy.openProof(desc);
	// 	expect(proofDescriptor).toBeDefined();
	// 	expect(proofDescriptor.info.formula).toEqual(test.sq_plus_eq_0_desc.info.formula);
	// 	expect(proofDescriptor.info.theory).toEqual(test.sq_plus_eq_0_desc.info.theory);
	// 	expect(proofDescriptor.info.status).toEqual(test.sq_plus_eq_0_desc.info.status);
	// 	expect(proofDescriptor.info.shasum).toEqual(test.sq_plus_eq_0_desc.info.shasum);
	// 	expect(proofDescriptor.proofTree).toEqual(test.sq_plus_eq_0_desc.proofTree);
	// 	// console.dir(proofDescriptor, { depth: null });

	// 	// update proof descriptor
	// 	const success: boolean = await pvsProxy.saveProof({
	// 		fileName: desc.fileName,
	// 		fileExtension: desc.fileExtension,
	// 		theoryName: desc.theoryName,
	// 		formulaName: desc.formulaName,
	// 		contextFolder: desc.contextFolder,
	// 		proofDescriptor: test.sq_plus_eq_0_desc_new
	// 	});
	// 	expect(success).toBeTrue();

	// 	const newProofDescriptor: ProofDescriptor = await pvsProxy.openProof(desc);
	// 	expect(newProofDescriptor).toBeDefined();
	// 	expect(newProofDescriptor.info.formula).toEqual(test.sq_plus_eq_0_desc_new.info.formula);
	// 	expect(newProofDescriptor.info.theory).toEqual(test.sq_plus_eq_0_desc_new.info.theory);
	// 	expect(newProofDescriptor.info.status).toEqual(test.sq_plus_eq_0_desc_new.info.status);
	// 	expect(newProofDescriptor.info.shasum).toEqual(test.sq_plus_eq_0_desc_new.info.shasum);
	// 	expect(newProofDescriptor.proofTree).toEqual(test.sq_plus_eq_0_desc_new.proofTree);

	// 	fsUtils.deleteFile(fname);
	// });

	it(`can generate backup files if vscode-pvs proof file is corrupted`, async () => {
		label(`can generate backup files if vscode-pvs proof file is corrupted`);

		// create a corrupted jprf file
		const fname: string = fsUtils.desc2fname({
			contextFolder: sandboxExamples,
			fileExtension: ".jprf",
			fileName: "sq"
		});
		const content: string = await fsUtils.readFile(fsUtils.desc2fname({
			contextFolder: sandboxExamples,
			fileExtension: ".jprf.corr",
			fileName: "sq"
		}));
		await fsUtils.writeFile(fname, content);

		// try to load a proof from the corrupted file
		const desc: PvsFormula = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			formulaName: "sq_plus_eq_0",
			theoryName: "sq"
		};
		const proofDescriptor: ProofDescriptor = await pvsProxy.openProof(desc, { quiet: true });
		expect(proofDescriptor).toBeDefined();

		const backup: string = await fsUtils.readFile(`${fname}.err`);
		expect(backup).toBeDefined();
		expect(backup).toEqual(content);
		// console.log(backup);

		const msg: string = await fsUtils.readFile(`${fname}.err.msg`);
		expect(msg).toBeDefined();
		expect(msg).toEqual("Unexpected token : in JSON at position 36");
		// console.log(msg);

		fsUtils.deleteFile(fname);
		fsUtils.deleteFile(`${fname}.err`);
		fsUtils.deleteFile(`${fname}.err.msg`);
	});

	it(`edit-proof-at and get-default-proof-script are equivalent`, async () => {
		const desc: PvsFormula = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			formulaName: "sq_neg",
			theoryName: "sq"
		};
		const fname: string = fsUtils.desc2fname(desc);
		const line: number = 12;
		await pvsProxy.changeContext(desc);
		await pvsProxy.typecheckFile(desc);
		const response1: PvsResponse = await pvsProxy.lisp(`(edit-proof-at "${fname}" nil ${line} "pvs" "${desc.fileName}${desc.fileExtension}" 0 nil)`);
		const response2: PvsResponse = await pvsProxy.getDefaultProofScript(desc);
		// const response2: PvsResponse = await pvsProxy.lisp(`(get-default-proof-script "${desc.theoryName}" "${desc.formulaName}")`);
		// console.dir(response1.result);
		// console.dir(response2.result);
		expect(response1.result).toEqual(response2.result);
	});

	it(`can provide default proofs for tccs`, async () => {
		const desc: PvsFormula = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			formulaName: "sq_TCC1",
			theoryName: "sq"
		};
		await pvsProxy.changeContext(desc);
		// const response: PvsResponse = await pvsProxy.lisp(`(get-default-proof-script "${desc.theoryName}" "${desc.formulaName}")`);
		const response: PvsResponse = await pvsProxy.getDefaultProofScript(desc);
		// console.dir(response);
		expect(response.error).not.toBeDefined();
		expect(response.result).toBeDefined();
		expect(response.result).toContain(`("" (then (ground)))`)
		
		// console.dir(response.result);
	});
});