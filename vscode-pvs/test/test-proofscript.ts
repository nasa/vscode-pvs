import * as fsUtils from "../server/src/common/fsUtils";
import * as test from "./test-constants";
import { PvsResponse } from "../server/src/common/pvs-gui";
import { PvsProxy } from '../server/src/pvsProxy'; // XmlRpcSystemMethods
import { label, log, configFile, sandboxExamples } from './test-utils';
import { ProofDescriptor } from "../server/src/common/serverInterface";

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
		pvsProxy = new PvsProxy(pvsPath, { externalServer: test.EXTERNAL_SERVER });
		await pvsProxy.activate({ debugMode: false, showBanner: false }); // this will also start pvs-server

		// delete pvsbin files and .pvscontext
		await fsUtils.cleanBin(sandboxExamples);

		console.log("\n----------------------");
		console.log("test-proofscript");
		console.log("----------------------");
	});
	afterAll(async () => {
		await pvsProxy.killPvsServer();
		await pvsProxy.killPvsProxy();
		// delete pvsbin files and .pvscontext
		await fsUtils.cleanBin(sandboxExamples);
	});	

	it(`can provide pvs proof scripts`, async () => {
		label(`can provide pvs proof scripts`);

		const desc = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			formulaName: "sq_neg",
			theoryName: "sq"
		};
		const response: PvsResponse = await pvsProxy.proofScript(desc);
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
		let response: PvsResponse = await pvsProxy.proofScript(desc);
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
		response = await pvsProxy.proofScript(desc1);
		// console.dir(response);
		expect(response.result).not.toBeDefined();
		expect(response.error).toBeDefined();
		expect(response.error.data.error_string).toMatch(/(.*) does not have a proof/);
	});
	
	it(`can generate vscode-pvs proof file`, async () => {
		label(`can generate vscode-pvs proof file`);

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
		const proofDescriptor: ProofDescriptor = await pvsProxy.loadProof(desc); // this will trigger the generation of the proof file
		expect(proofDescriptor).toBeDefined();

		const jprf: string = await fsUtils.readFile(fname);
		expect(jprf).toBeDefined();
		// console.dir(proofDescriptor, { depth: null });

		const fname_copy: string = fsUtils.desc2fname({
			contextFolder: sandboxExamples,
			fileExtension: ".jprf.bak",
			fileName: "sq"
		});
		const jprf_copy: string = await fsUtils.readFile(fname_copy);
		expect(jprf).toEqual(jprf_copy);

		fsUtils.deleteFile(fsUtils.desc2fname({
			contextFolder: sandboxExamples,
			fileExtension: ".jprf",
			fileName: "sq"
		}));
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
		const proofDescriptor: ProofDescriptor = await pvsProxy.loadProof(desc);
		expect(proofDescriptor).toBeDefined();
		expect(proofDescriptor.info.formula).toEqual(test.sq_plus_eq_0_desc.info.formula);
		expect(proofDescriptor.info.theory).toEqual(test.sq_plus_eq_0_desc.info.theory);
		expect(proofDescriptor.info.status).toEqual(test.sq_plus_eq_0_desc.info.status);
		expect(proofDescriptor.info.shasum).toEqual(test.sq_plus_eq_0_desc.info.shasum);
		expect(proofDescriptor.proofTree).toEqual(test.sq_plus_eq_0_desc.proofTree);
		// console.dir(proofDescriptor, { depth: null });

		fsUtils.deleteFile(fname);
	});

	it(`can save new vscode-pvs proof descriptors`, async () => {
		label(`can save new vscode-pvs proof descriptors`);

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

		// update proof descriptor
		const success: boolean = await pvsProxy.saveProof({
			fileName: desc.fileName,
			fileExtension: desc.fileExtension,
			theoryName: desc.theoryName,
			formulaName: desc.formulaName,
			contextFolder: desc.contextFolder,
			proofDescriptor: test.sq_plus_eq_0_desc_new
		});
		expect(success).toBeTrue();

		const newProofDescriptor: ProofDescriptor = await pvsProxy.loadProof(desc);
		expect(newProofDescriptor).toBeDefined();
		expect(newProofDescriptor.info.formula).toEqual(test.sq_plus_eq_0_desc_new.info.formula);
		expect(newProofDescriptor.info.theory).toEqual(test.sq_plus_eq_0_desc_new.info.theory);
		expect(newProofDescriptor.info.status).toEqual(test.sq_plus_eq_0_desc_new.info.status);
		expect(newProofDescriptor.info.shasum).toEqual(test.sq_plus_eq_0_desc_new.info.shasum);
		expect(newProofDescriptor.proofTree).toEqual(test.sq_plus_eq_0_desc_new.proofTree);

		fsUtils.deleteFile(fname);
	});

	it(`can update existing vscode-pvs proof descriptors`, async () => {
		label(`can update existing vscode-pvs proof descriptors`);

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
		const proofDescriptor: ProofDescriptor = await pvsProxy.loadProof(desc);
		expect(proofDescriptor).toBeDefined();
		expect(proofDescriptor.info.formula).toEqual(test.sq_plus_eq_0_desc.info.formula);
		expect(proofDescriptor.info.theory).toEqual(test.sq_plus_eq_0_desc.info.theory);
		expect(proofDescriptor.info.status).toEqual(test.sq_plus_eq_0_desc.info.status);
		expect(proofDescriptor.info.shasum).toEqual(test.sq_plus_eq_0_desc.info.shasum);
		expect(proofDescriptor.proofTree).toEqual(test.sq_plus_eq_0_desc.proofTree);
		// console.dir(proofDescriptor, { depth: null });

		// update proof descriptor
		const success: boolean = await pvsProxy.saveProof({
			fileName: desc.fileName,
			fileExtension: desc.fileExtension,
			theoryName: desc.theoryName,
			formulaName: desc.formulaName,
			contextFolder: desc.contextFolder,
			proofDescriptor: test.sq_plus_eq_0_desc_new
		});
		expect(success).toBeTrue();

		const newProofDescriptor: ProofDescriptor = await pvsProxy.loadProof(desc);
		expect(newProofDescriptor).toBeDefined();
		expect(newProofDescriptor.info.formula).toEqual(test.sq_plus_eq_0_desc_new.info.formula);
		expect(newProofDescriptor.info.theory).toEqual(test.sq_plus_eq_0_desc_new.info.theory);
		expect(newProofDescriptor.info.status).toEqual(test.sq_plus_eq_0_desc_new.info.status);
		expect(newProofDescriptor.info.shasum).toEqual(test.sq_plus_eq_0_desc_new.info.shasum);
		expect(newProofDescriptor.proofTree).toEqual(test.sq_plus_eq_0_desc_new.proofTree);

		fsUtils.deleteFile(fname);
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
		const desc = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			formulaName: "sq_plus_eq_0",
			theoryName: "sq"
		};
		const proofDescriptor: ProofDescriptor = await pvsProxy.loadProof(desc, { quiet: true });
		expect(proofDescriptor).toBeDefined();
		expect(proofDescriptor).toEqual(test.sq_plus_eq_0_desc); // the prf file content should be loaded when the jprf file is corrupted

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
});