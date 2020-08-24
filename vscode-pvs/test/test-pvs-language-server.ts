import * as fsUtils from "../server/src/common/fsUtils";
import { label, configFile } from './test-utils';
import * as path from 'path';
import { PvsLanguageServer } from '../server/src/pvsLanguageServer'
import { ProofDescriptor, ProofFile } from "../server/src/common/serverInterface";
import { execSync } from "child_process";
import * as constants from './test-constants';
import { PvsResult } from "../server/src/common/pvs-gui";
//----------------------------
//   Test cases for pvs language server
//----------------------------

describe("pvs-language-server", () => {
	let server: PvsLanguageServer = new PvsLanguageServer();
	beforeAll(async () => {
		const config: string = await fsUtils.readFile(configFile);
		const content: { pvsPath: string } = JSON.parse(config);
		// console.log(content);
		const pvsPath: string = content.pvsPath;
		await server.startPvsServer({ pvsPath });

		console.log("\n----------------------");
		console.log("test-pvs-language-server");
		console.log("----------------------");
	});
	afterAll(async () => {
	});

	// utility function, quits the prover if the prover status is active
	const quitProverIfActive = async (): Promise<void> => {
		// quit prover if prover status is active
		const proverStatus: PvsResult = await server.getPvsProxy().getProverStatus();
		expect(proverStatus.result).toBeDefined();
		expect(proverStatus.error).not.toBeDefined();
		console.log(proverStatus);
		if (proverStatus && proverStatus.result !== "inactive") {
			await server.getPvsProxy().proofCommand({ cmd: 'quit' });
		}
	}
	
	// OK
	it(`can load pvs proof (.prj)`, async () => {
		// remove alaris folder if present and replace it with the content of the zip file
		const baseFolder: string = path.join(__dirname, "pvs-language-server");
		// fsUtils.deleteFolder(path.join(baseFolder, "alaris2l"));
		// execSync(`cd ${path.join(__dirname, "pvscontext")} && unzip alaris2l-show-tccs-error.zip`);
		execSync(`cd ${path.join(baseFolder, "sq")} && rm -f sq.jprf`);

		const desc: ProofDescriptor = await server.loadProof({
			fileName: "sq", 
			fileExtension: ".pvs", 
			contextFolder: path.join(baseFolder, "sq"),
			theoryName: "sq",
			formulaName: "triangle_rectangle"
		});
		// console.dir(desc, { depth: null });
		expect(desc.info.theory).toEqual("sq");
		expect(desc.info.formula).toEqual("triangle_rectangle");
		expect(desc.info.status).toEqual("untried");
		expect(desc.info.prover).toContain("PVS");
		expect(desc.info.shasum).toEqual("90d0630453df76b0a749b92ac10e7e51b0c59e2cb0e3711bb009a7b4191b802a");

		const fname: string = path.join(baseFolder, "sq", "sq.jprf");
		const jprf: ProofFile = JSON.parse(await fsUtils.readFile(fname));
		// console.dir(jprf, { depth: null });
		const proof: ProofDescriptor = jprf["sq.triangle_rectangle"][0];
		expect(proof.proofTree).toEqual(constants.triangle_rectangle.proofTree);
		expect(proof.info.formula).toEqual(constants.triangle_rectangle.info.formula);
		expect(proof.info.shasum).toEqual(constants.triangle_rectangle.info.shasum);
		expect(proof.info.status).toEqual(constants.triangle_rectangle.info.status);
		expect(proof.info.theory).toEqual(constants.triangle_rectangle.info.theory);
		expect(proof.info.prover).toContain("PVS");

		// remove jprf file
		execSync(`cd ${path.join(baseFolder, "sq")} && rm -f sq.jprf`);
	}, 20000);
	

	fit(`can typecheck nasalib-monitors/trace.pvs (nasalib-monitors.zip)`, async () => {
		await quitProverIfActive();

		await server.getPvsProxy().loadPatchesAndLibraries();

		// remove folder if present and replace it with the content of the zip file
		const contextFolder: string = "/Users/pmasci/Work/gitlab/pvs-experimental/monitors/"; //path.join(baseFolder, "nasalib-monitors"); // "/Users/pmasci/Work/gitlab/pvs-experimental/monitors/";
		// fsUtils.deleteFolder(contextFolder);
		// execSync(`cd ${baseFolder} && unzip nasalib-monitors.zip`);

		// let response: PvsResponse = await pvsProxy.setNasalibPath({ useXmlrpc: true });
		// console.dir(response);
		// response = await pvsProxy.loadPvsPatches({ useXmlrpc: true });
		// console.dir(response);
		// response = await pvsProxy.loadPvsLibraryPath(null, { useXmlrpc: true });
		// console.dir(response);

		await server.proveFormulaRequest({
			fileName: "trace",
			fileExtension: ".pvs",
			contextFolder: path.join(contextFolder, "Fret_MLTL"),
			theoryName: "trace",
			formulaName: "null_null_always_satisfaction"
		});

		await server.proofCommandRequest({
			fileName: "trace",
			fileExtension: ".pvs",
			contextFolder: path.join(contextFolder, "Fret_MLTL"),
			theoryName: "trace",
			formulaName: "null_null_always_satisfaction",
			cmd: `(skeep)`
		});
		await server.proofCommandRequest({
			fileName: "trace",
			fileExtension: ".pvs",
			contextFolder: path.join(contextFolder, "Fret_MLTL"),
			theoryName: "trace",
			formulaName: "null_null_always_satisfaction",
			cmd: `(fretex)`
		});
		// response = await pvsProxy.proofCommand({ cmd: `(skeep)(fretex)(iff)(split)(flatten)(inst -1 "0")(skeep)(inst 2 "n-1")(case "i > n-1")(expand "Trace_equiv")(inst -3 "n-1")(assert)(flatten)(assert)(expand "last_atom")(expand "always")(split)(inst -1 "i")(split)(expand "Trace_equiv")(inst -2 "i")(flatten)(hide -2 -3 -4 -5 -7)(expand "post_condition_atom")(assert)(typepred "i")(assert)(expand "nth")(typepred "i")(grind)(expand "nth")(grind)(inst -1 "i")(expand "nth")(grind)(expand "nth")(typepred "i")(grind)(expand "length")(grind)(flatten)(skeep)(expand "always")(skeep)(typepred "i_1")(inst -2 "i_1")(expand "nth")(assert)(typepred "i")(grind)` });

		// remove folder 
		// fsUtils.deleteFolder(path.join(baseFolder, "type_theory"));
	}, 20000);

});

