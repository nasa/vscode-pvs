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
		// console.log(proverStatus);
		if (proverStatus && proverStatus.result !== "inactive") {
			await pvsProxy.proofCommand({ cmd: 'quit' });
		}
	}

	it(`can open default pvs proof script`, async () => {
		const formula: PvsFormula = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			formulaName: "sq_neg",
			theoryName: "sq"
		};
		const response: PvsResponse = await pvsProxy.getDefaultProofScript(formula);
		// console.dir(response);
		expect(response.error).not.toBeDefined();
		expect(response.result).toBeDefined();
		const proof_header: string = response.result.split("\n")[0];
		expect(proof_header).toEqual(`;;; Proof sq_neg-1 for formula sq.sq_neg`);
	});
	
	it(`returns a well-formed empty pvs proof script when the proof file is not available`, async () => {
		// a well-formed response for formula sqrt_0 is in the form /;;; Proof sqrt_0(\-\d+)? for formula sqrt.sqrt_0\n(""\s*)/
		const formula: PvsFormula = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sqrt",
			formulaName: "sqrt_0",
			theoryName: "sqrt"
		};
		const response: PvsResponse = await pvsProxy.getDefaultProofScript(formula);
		// console.dir(response);
		expect(response.result).not.toBeDefined();
		expect(response.error).toBeDefined();
		expect(response.error.data.error_string).toMatch(/(.*) does not have a proof/);

		const formula1: PvsFormula = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "alari2lnewmodes.pump",
			formulaName: "vtbi_over_rate_lemma",
			theoryName: "pump_th"
		}
		const response1: PvsResponse = await pvsProxy.getDefaultProofScript(formula1);
		// console.dir(response);
		expect(response1.result).not.toBeDefined();
		expect(response1.error).toBeDefined();
		expect(response1.error.data.error_string).toMatch(/(.*) does not have a proof/);

		const formula3: PvsFormula = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			formulaName: "sq_plus_pos",
			theoryName: "sq"
		};
		await pvsProxy.changeContext(formula3);
		await pvsProxy.typecheckFile(formula3);
		const response3: PvsResponse = await pvsProxy.getDefaultProofScript(formula3);
		// const response3: PvsResponse = await pvsProxy.lisp(`(get-default-proof-script "${formula3.theoryName}" "${formula3.formulaName}")`);
		// console.dir(response3.result);

		expect(response3.result).not.toBeDefined();
		expect(response3.error).toBeDefined();
		expect(response3.error.data.error_string).toMatch(/(.*) does not have a proof/);
	});
	
	it(`can load & save vscode-pvs proof files`, async () => {
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
		// console.dir(response);
		response = await pvsProxy.storeLastAttemptedProof(formula);
		const fullTheoryName: string = path.join(formula.contextFolder, formula.fileName + ".pvs" + "#" + formula.theoryName);
		response = await pvsProxy.pvsRequest("store-last-attempted-proof", [ formula.formulaName, fullTheoryName, 't' ]);
		if (response && response.result) {
			response = await pvsProxy.pvsRequest("change-context", [ formula.contextFolder ]);
			response = await pvsProxy.pvsRequest("save-all-proofs", [ fullTheoryName ]);
		}
		expect(response.result).toBeDefined();
		// console.dir(response);
	});

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
		const formula: PvsFormula = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			formulaName: "sq_plus_eq_0",
			theoryName: "sq"
		};
		const proofDescriptor: ProofDescriptor = await pvsProxy.openProofFile({
			contextFolder: formula.contextFolder,
			fileName: formula.fileName,
			fileExtension: ".jprf"
		}, formula, { quiet: true });
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

	it(`can provide default proof script for tccs`, async () => {
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
		expect(response.result).toContain(`("" (subtype-tcc))`)
		// console.dir(response.result);
	});

});