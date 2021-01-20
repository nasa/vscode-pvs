import * as fsUtils from "../server/src/common/fsUtils";
import { configFile } from './test-utils';
import * as path from 'path';
import { PvsLanguageServer } from '../server/src/pvsLanguageServer'
import { ProofDescriptor, PvsFormula } from "../server/src/common/serverInterface";
import { execSync } from "child_process";
import { PvsResult } from "../server/src/common/pvs-gui";
import { expect } from 'chai';

//----------------------------
//   Test cases for pvs language server
//----------------------------

describe("pvs-language-server", () => {
	let server: PvsLanguageServer = new PvsLanguageServer();
	before(async () => {
		const config: string = await fsUtils.readFile(configFile);
		const content: { pvsPath: string } = JSON.parse(config);
		// console.log(content);
		const pvsPath: string = content.pvsPath;
		await server.startPvsServer({ pvsPath });

		console.log("\n----------------------");
		console.log("test-pvs-language-server");
		console.log("----------------------");
	});
	after(async () => {
	});

	// utility function, quits the prover if the prover status is active
	const quitProverIfActive = async (): Promise<void> => {
		// quit prover if prover status is active
		const proverStatus: PvsResult = await server.getPvsProxy().getProverStatus();
		expect(proverStatus.result).not.to.be.undefined;
		expect(proverStatus.error).to.be.undefined;
		console.log(proverStatus);
		if (proverStatus && proverStatus.result !== "inactive") {
			await server.getPvsProxy().proofCommand({ cmd: 'quit' });
		}
	}
	
	// OK
	it(`can load and save pvs proof (.prj)`, async () => {
		// remove alaris folder if present and replace it with the content of the zip file
		const baseFolder: string = path.join(__dirname, "pvs-language-server");
		// fsUtils.deleteFolder(path.join(baseFolder, "alaris2l"));
		// execSync(`cd ${path.join(__dirname, "pvscontext")} && unzip alaris2l-show-tccs-error.zip`);
		execSync(`cd ${path.join(baseFolder, "sq")} && rm -f sq.jprf`);

		const formula: PvsFormula = {
			fileName: "sq", 
			fileExtension: ".pvs", 
			contextFolder: path.join(baseFolder, "sq"),
			theoryName: "sq",
			formulaName: "triangle_rectangle"
		};
		const desc: ProofDescriptor = await server.getPvsProxy().openProofFile({
			contextFolder: formula.contextFolder,
			fileName: formula.fileName,
			fileExtension: ".prf"
		}, formula);
		// console.dir(desc, { depth: null });
		expect(desc.info.theory).to.deep.equal("sq");
		expect(desc.info.formula).to.deep.equal("triangle_rectangle");
		expect(desc.info.status).to.deep.equal("untried");
		expect(desc.info.prover).to.contain("PVS");
		expect(desc.info.shasum).to.deep.equal("90d0630453df76b0a749b92ac10e7e51b0c59e2cb0e3711bb009a7b4191b802a");

	}).timeout(20000);

});

