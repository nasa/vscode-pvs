import * as fsUtils from "../server/src/common/fsUtils";
import * as test from "./test-constants";
import { PvsResponse, PvsResult } from "../server/src/common/pvs-gui";
import { PvsProxy } from '../server/src/pvsProxy';
import { label, log, dir, configFile, sandboxExamples } from './test-utils';

//----------------------------
//   Test cases for parser
//----------------------------
describe("pvs-proxy", () => {
	let pvsProxy: PvsProxy = null;
	beforeAll(async () => {
		const config: string = await fsUtils.readFile(configFile);
		const content: { pvsPath: string } = JSON.parse(config);
		// console.dir(content);
		const pvsPath: string = content.pvsPath;
		// log("Activating xmlrpc proxy...");
		pvsProxy = new PvsProxy(pvsPath, { externalServer: test.EXTERNAL_SERVER });
		await pvsProxy.activate({ debugMode: false, showBanner: false }); // this will also start pvs-server

		// delete pvsbin files and .pvscontext
		await fsUtils.deletePvsCache(sandboxExamples);

		console.log("\n----------------------");
		console.log("test-workspace");
		console.log("----------------------");
	});
	afterAll(async () => {
		await pvsProxy.killPvsServer();
		await pvsProxy.killPvsProxy();
		// delete pvsbin files and .pvscontext
		await fsUtils.deletePvsCache(sandboxExamples);
	});

	it(`can tell what is the current context and returns a well-formed path`, async () => {
		label(`can tell what is the current context and returns a well-formed path`);
		// Need to clear-theories, in case rerunning with the same server.
		await pvsProxy.lisp("(clear-theories t)");

		const current: PvsResponse = await pvsProxy.currentContext();
		expect(current).not.toBeNull();
		expect(current.result).not.toContain("~"); // tilde should be expanded
		expect(current.result).toMatch(/\/.*/); // path should be absolute, therefore it should start with /
	});

	it(`can change context`, async () => {
		label(`can change context`);
		// Need to clear-theories, in case rerunning with the same server.
		await pvsProxy.lisp("(clear-theories t)");

		const home: PvsResponse = await pvsProxy.changeContext("~");
		// console.dir(home);
		expect(home).not.toBeNull();
		expect(home.result).not.toContain("~"); // tilde should be expanded
		expect(home.result).toMatch(/\/.*/); // path should be absolute, therefore it should start with /
	});

	// it(`change-context is equivalent to change-workspace`, async () => {
	// 	label(`change-context is equivalent to change-workspace`);
	// 	// Need to clear-theories, in case rerunning with the same server.
	// 	await pvsProxy.lisp("(clear-theories t)");

	// 	const home1: PvsResponse = await pvsProxy.pvsRequest("change-context", [ "~" ]);
	// 	const home2: PvsResponse = await pvsProxy.changeContext("~");
	// 	expect(home1.result).toEqual(home2.result);
	// });

});

