import * as fsUtils from "../server/src/common/fsUtils";
import * as test from "./test-constants";
import { PvsResponse } from "../server/src/common/pvs-gui";
import { PvsProxy } from '../server/src/pvsProxy'; // XmlRpcSystemMethods
import { label, configFile, sandboxExamples } from './test-utils';


//----------------------------
//   Test cases for pvs-proxy
//----------------------------
describe("pvs-proxy", () => {
	let pvsProxy: PvsProxy = null;
	beforeAll(async () => {
		const config: string = await fsUtils.readFile(configFile);
		const content: { pvsPath: string } = JSON.parse(config);
		// console.log(content);
		const pvsPath: string = content.pvsPath;
		// log("Activating xmlrpc proxy...");
		pvsProxy = new PvsProxy(pvsPath, { externalServer: test.EXTERNAL_SERVER });
		await pvsProxy.activate({ debugMode: false, showBanner: false }); // this will also start pvs-server

		// delete pvsbin files and .pvscontext
		await fsUtils.deletePvsCache(sandboxExamples);

		console.log("\n----------------------");
		console.log("test-proxy");
		console.log("----------------------");
	});
	afterAll(async () => {
		await pvsProxy.killPvsServer();
		await pvsProxy.killPvsProxy();
		// delete pvsbin files and .pvscontext
		await fsUtils.deletePvsCache(sandboxExamples);
	});

	//-----------------------------------
	// console.info("Starting test case execution");
	//-----------------------------------

	it(`can be started correctly`, async () => {
		label(`can be started correctly`);
		expect(pvsProxy).not.toBeNull();

		const success: boolean = await pvsProxy.testServerConnectivity();
		expect(success).toBeTruthy();
	});

	it(`can list xml-rpc system methods`, async () => {
		label(`can list xml-rpc system methods`);
		const ans: { error: { code: string, message: string, stack: string }, result: string[] } = await pvsProxy.listSystemMethods();
		expect(ans).not.toBeNull();
		expect(ans.error).toBeNull();
		expect(ans.result).not.toBeNull();
		expect(ans.result.length).toEqual(4);
		expect(ans.result.filter((mth: string) => {
			return mth === "pvs.request";
		}).length).toEqual(1);
	});

	it(`can list pvs-server methods`, async () => {
		label(`can list pvs-server methods`);
		const response: PvsResponse = await pvsProxy.listMethodsRequest();
		// console.log(response);
		const mths: string[] = [
			'change-context',      'change-workspace',
			'find-declaration',    'help',
			'interrupt',           'lisp',
			'list-client-methods', 'list-methods',
			'names-info',          'parse',
			'proof-command',       'proof-script',
			'proof-status',        'prove-formula',
			'prove-tccs',          'prover-status',
			'reset',               'show-tccs',
			'term-at',             'typecheck'
		];
		expect(response).not.toBeNull();
		expect(response.result).toEqual(mths);
	});

	it(`knows client methods`, async () => {
		label(`knows client methods`);
		const response: PvsResponse = await pvsProxy.listClientMethods();
		// console.log(response);
		// console.info("listClientMethods: ans = ", ans);
		expect(response.result).toEqual(pvsProxy.client_methods);
	});

});

