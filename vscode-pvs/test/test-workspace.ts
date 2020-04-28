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
		log(content);
		const pvsPath: string = content.pvsPath;
		// log("Activating xmlrpc proxy...");
		pvsProxy = new PvsProxy(pvsPath, { externalServer: test.EXTERNAL_SERVER });
		await pvsProxy.activate({ debugMode: true, showBanner: false }); // this will also start pvs-server

		// delete pvsbin files
		await fsUtils.deletePvsCache(sandboxExamples);
	});
	afterAll(async () => {
		// delete pvsbin files
		await fsUtils.deletePvsCache(sandboxExamples);

		if (!test.EXTERNAL_SERVER) {
			// kill pvs server & proxy
			console.log(" killing pvs server...")
			await pvsProxy.killPvsServer();
		}
		await pvsProxy.killPvsProxy();
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

	// return; // the following tests are completed successfully -- remove the return statement if you want to run them

	// OK
	it(`can change context`, async () => {
		label(`can change context`);
		// Need to clear-theories, in case rerunning with the same server.
		await pvsProxy.lisp("(clear-theories t)");

		const home: PvsResponse = await pvsProxy.changeContext("~");
		expect(home).not.toBeNull();
		expect(home.result).not.toContain("~"); // tilde should be expanded
		expect(home.result).toMatch(/\/.*/); // path should be absolute, therefore it should start with /
	});

	// OK
	it(`change context is equivalent to change-workspace`, async () => {
		label(`change context is equivalent to change-workspace`);
		// Need to clear-theories, in case rerunning with the same server.
		await pvsProxy.lisp("(clear-theories t)");

		const home1: PvsResponse = await pvsProxy.pvsRequest("change-context", [ "~" ]);
		const home2: PvsResponse = await pvsProxy.changeContext("~");
		expect(home1.result).toEqual(home2.result);
	});

});

