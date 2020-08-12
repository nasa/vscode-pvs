import * as fsUtils from "../server/src/common/fsUtils";
import { label, configFile } from './test-utils';
import * as path from 'path';
import { PvsLanguageServer } from '../server/src/pvsLanguageServer'
import { ProofDescriptor, ProofFile } from "../server/src/common/serverInterface";
import { execSync } from "child_process";
import * as constants from './test-constants';
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

	// OK
	it(`can load pvs proof (.prj)`, async () => {
		label(`can load pvs proof (.prj)`);

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

});

