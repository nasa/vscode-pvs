import * as fsUtils from "../server/src/common/fsUtils";
import * as test from "./test-constants";
import { PvsResponse } from "../server/src/common/pvs-gui";
import { PvsProxy } from '../server/src/pvsProxy'; // XmlRpcSystemMethods
import { label, configFile, sandboxExamples } from './test-utils';
import * as os from 'os';
import * as path from 'path';
import { execSync } from "child_process";

//----------------------------
//   Test cases for checking behavior of pvs with corrupted .pvscontext
//----------------------------
describe(".pvscontext", () => {
	let pvsProxy: PvsProxy = null;
	beforeAll(async () => {
		const config: string = await fsUtils.readFile(configFile);
		const content: { pvsPath: string } = JSON.parse(config);
		// console.log(content);
		const pvsPath: string = content.pvsPath;
		// log("Activating xmlrpc proxy...");
		pvsProxy = new PvsProxy(pvsPath, { externalServer: test.EXTERNAL_SERVER });
		await pvsProxy.activate({ debugMode: false, showBanner: false }); // this will also start pvs-server

		console.log("\n----------------------");
		console.log("test-pvscontext");
		console.log("----------------------");
	});
	afterAll(async () => {
		await pvsProxy.killPvsServer();
		await pvsProxy.killPvsProxy();
	});

	it(`can typecheck baxter example`, async () => {
		label(`can typecheck baxter example`);
		// Need to clear-theories, in case rerunning with the same server.
		await pvsProxy.lisp("(clear-theories t)");

		// remove baxter folder if present and replace it with the content of the zip file
		const baseFolder: string = path.join(__dirname, "test.pvscontext");
		fsUtils.deleteFolder(path.join(baseFolder, "baxter"));
		execSync(`cd ${path.join(__dirname, "test.pvscontext")} && unzip baxter-with-pvscontext.zip`);

		const response: PvsResponse = await pvsProxy.typecheckFile({
			fileName: "baxterSigmaSpectrum", 
			fileExtension: ".pvs", 
			contextFolder: path.join(__dirname, "test.pvscontext", "baxter")
		});
		// console.dir(response);
		expect(response).toBeDefined();
		expect(response.result).toBeDefined();
		expect(response.error).not.toBeDefined();

		// remove baxter folder 
		fsUtils.deleteFolder(path.join(baseFolder, "baxter"));
	}, 4000);

	it(`can typecheck pillboxv7 example`, async () => {
		label(`can typecheck pillboxv7 example`);
		// Need to clear-theories, in case rerunning with the same server.
		await pvsProxy.lisp("(clear-theories t)");

		// remove pillboxv7 folder if present and replace it with the content of the zip file
		const baseFolder: string = path.join(__dirname, "test.pvscontext");
		fsUtils.deleteFolder(path.join(baseFolder, "pillboxv7"));
		execSync(`cd ${path.join(__dirname, "test.pvscontext")} && unzip pillboxv7-with-pvscontext.zip`);

		const response: PvsResponse = await pvsProxy.typecheckFile({
			fileName: "firstpillchecks", 
			fileExtension: ".pvs", 
			contextFolder: path.join(__dirname, "test.pvscontext", "pillboxv7")
		});
		// console.dir(response);
		expect(response).toBeDefined();
		expect(response.result).toBeDefined();
		expect(response.error).not.toBeDefined();

		// remove pillboxv7 folder 
		fsUtils.deleteFolder(path.join(baseFolder, "pillboxv7"));
	}, 4000);

});

