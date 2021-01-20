import * as fsUtils from "../server/src/common/fsUtils";
import { PvsResponse } from "../server/src/common/pvs-gui";
import { PvsProxy } from '../server/src/pvsProxy';
import { label, configFile, sandboxExamples } from './test-utils';
import { expect } from 'chai';

//----------------------------
//   Test cases for parser
//----------------------------
describe("pvs-proxy", () => {
	let pvsProxy: PvsProxy = null;
	before(async () => {
		const config: string = await fsUtils.readFile(configFile);
		const content: { pvsPath: string } = JSON.parse(config);
		// console.dir(content);
		const pvsPath: string = content.pvsPath;
		// log("Activating xmlrpc proxy...");
		pvsProxy = new PvsProxy(pvsPath, { externalServer: true });
		await pvsProxy.activate({ debugMode: false, showBanner: false }); // this will also start pvs-server

		// delete pvsbin files and .pvscontext
		await fsUtils.cleanBin(sandboxExamples);

		console.log("\n----------------------");
		console.log("test-workspace");
		console.log("----------------------");
	});
	after(async () => {
		await pvsProxy.killPvsServer();
		await pvsProxy.killPvsProxy();
		// delete pvsbin files and .pvscontext
		await fsUtils.cleanBin(sandboxExamples);
	});

	it(`can tell what is the current context and returns a well-formed path`, async () => {
		label(`can tell what is the current context and returns a well-formed path`);
		// Need to clear-theories, in case rerunning with the same server.
		await pvsProxy.lisp("(clear-theories t)");

		const current: PvsResponse = await pvsProxy.currentContext();
		expect(current).not.to.equal(null);
		expect(current.result).not.to.contain("~"); // tilde should be expanded
		expect(current.result).to.match(/\/.*/); // path should be absolute, therefore it should start with /
	});

	it(`can change context`, async () => {
		label(`can change context`);
		// Need to clear-theories, in case rerunning with the same server.
		await pvsProxy.lisp("(clear-theories t)");

		const home: PvsResponse = await pvsProxy.changeContext("~");
		// console.dir(home);
		expect(home).not.to.equal(null);
		expect(home.result).not.to.contain("~"); // tilde should be expanded
		expect(home.result).to.match(/\/.*/); // path should be absolute, therefore it should start with /
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

