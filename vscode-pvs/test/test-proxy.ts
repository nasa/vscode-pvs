import * as fsUtils from "../server/src/common/fsUtils";
import { PvsResponse } from "../server/src/common/pvs-gui";
import { PvsProxy } from '../server/src/pvsProxy'; // XmlRpcSystemMethods
import { label, configFile, sandboxExamples } from './test-utils';
import { expect } from 'chai'

//----------------------------
//   Test cases for pvs-proxy
//----------------------------
describe("pvs-proxy", () => {
	let pvsProxy: PvsProxy = null;
	before(async () => {
		const config: string = await fsUtils.readFile(configFile);
		const content: { pvsPath: string } = JSON.parse(config);
		// console.log(content);
		const pvsPath: string = content.pvsPath;
		// log("Activating xmlrpc proxy...");
		pvsProxy = new PvsProxy(pvsPath, { externalServer: true });
		await pvsProxy.activate({ debugMode: false, showBanner: false }); // this will also start pvs-server

		// delete pvsbin files and .pvscontext
		await fsUtils.cleanBin(sandboxExamples);

		console.log("\n----------------------");
		console.log("test-proxy");
		console.log("----------------------");
	});
	after(async () => {
		await pvsProxy.killPvsServer();
		await pvsProxy.killPvsProxy();
		// delete pvsbin files and .pvscontext
		await fsUtils.cleanBin(sandboxExamples);
	});

	//-----------------------------------
	// console.info("Starting test case execution");
	//-----------------------------------

	it(`can be started correctly`, async () => {
		label(`can be started correctly`);
		expect(pvsProxy).not.to.equal(null);

		const success: boolean = await pvsProxy.testServerConnectivity();
		expect(success).to.equal(true);
	});

	it(`can list xml-rpc system methods`, async () => {
		label(`can list xml-rpc system methods`);
		const ans: { error: { code: string, message: string, stack: string }, result: string[] } = await pvsProxy.listSystemMethods();
		expect(ans).not.to.equal(null);
		expect(ans.error).to.equal(null);
		expect(ans.result).not.to.equal(null);
		expect(ans.result.length).to.equal(4);
		expect(ans.result.filter((mth: string) => {
			return mth === "pvs.request";
		}).length).to.equal(1);
	});

	it(`can list pvs-server methods`, async () => {
		label(`can list pvs-server methods`);
		const response: PvsResponse = await pvsProxy.listMethodsRequest();
		// console.log(response);
		const methods: string[] = [
			'add-pvs-library',
			'change-context',
			'change-workspace',
			'find-declaration',
			'help',
			'interrupt',
			'lisp',
			'list-client-methods',
			'list-methods',
			'names-info',
			'parse',
			'proof-command',
			'proof-script',
			'proof-status',
			'prove-formula',
			'prove-tccs',
			'prover-status',
			'reset',
			'save-all-proofs',
			'show-tccs',
			'store-last-attempted-proof',
			'term-at',
			'typecheck'
		];
		expect(response).not.to.equal(null);
		expect(response.result).to.deep.equal(methods);
		// console.dir(response.result);
	});

	it(`knows client methods`, async () => {
		label(`knows client methods`);
		const response: PvsResponse = await pvsProxy.listClientMethods();
		// console.log(response);
		// console.info("listClientMethods: ans = ", ans);
		expect(response.result).to.deep.equal(pvsProxy.client_methods);
	});

});

