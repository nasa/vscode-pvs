// import { ContextDiagnostics } from "./server/pvsProcess";
//import { PvsFindDeclaration, PvsParserResponse, PvsTypecheckerResponse, XmlRpcResponse } from "./server/common/serverInterface";
import * as fsUtils from "../server/src/common/fsUtils";
import * as test from "./test-constants";
import { PvsResponse } from "../server/src/common/pvs-gui";
import { PvsProxy } from '../server/src/pvsProxy'; // XmlRpcSystemMethods
import { label, log, dir, configFile, safeSandboxExamples, sandboxExamples, radixExamples,
	stever, steverFiles, pillbox, pillboxFiles, pvsioweb, pvsiowebFiles } from './test-utils';
import * as path from 'path';
import * as os from 'os';

//----------------------------
//   Test cases for typechecker
//----------------------------
describe("pvs-typechecker", () => {
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

		await pvsProxy.killPvsServer();
		await pvsProxy.killPvsProxy();
	});

	it(`can typecheck files`, async () => {
		label(`can typecheck files`);

		const response: PvsResponse = await pvsProxy.typecheckFile({ fileName: "sqrt", fileExtension: ".pvs", contextFolder: sandboxExamples });
		dir(response);
		expect(response).not.toBeNull();
		expect(response.result).not.toBeNull();

		// on MacOS, stats are not provided because we are using the Emacs interface to interact with the parser
		if (os.platform() !== "darwin") {
			// has-proofscript? seems to be changing all the time for different runs, I'm removing it from the check for now
			const res_decls = response.result[0].decls.map(elem => {
				if (elem.kind === "formula") {
					return {
						id: elem.id,
						kind: elem.kind,
						place: elem.place,
						"proved?": elem["proved?"],
						"complete?": elem["complete?"]
					};
				}
				return elem;
			});
			expect(res_decls).toEqual(test.typecheck2_result[0].decls.map(elem => {
				if (elem.kind === "formula") {
					return {
						id: elem.id,
						kind: elem.kind,
						place: elem.place,
						"proved?": elem["proved?"],
						"complete?": elem["complete?"]
					};
				}
				return elem;
			}));
		}
	}, 100000);

	// on Mac, pvs-server crashes without informative messages when performing this test
	it(`can typecheck theories with parameters`, async () => {
		label(`can typecheck theories with parameters`);

		let desc = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "alaris2lnewmodes.pump",
			formulaName: "vtbi_over_rate_lemma",
			line: 28,
			theoryName: "pump_th"
		};
		let response: PvsResponse = await pvsProxy.typecheckFile(desc);
		expect(response.result).toBeDefined();
		expect(response.error).not.toBeDefined();
		
	}, 10000);

	// on Mac, pvs-server crashes without informative messages when performing this test
	it(`can typecheck pvs files that import other files`, async () => {
		label(`can typecheck pvs files that import other files`);

		// const response: PvsResponse = await pvsProxy.typecheckFile({ fileName: "sqrt", fileExtension: ".pvs", contextFolder: sandboxExamples });
		const response: PvsResponse = await pvsProxy.typecheckFile({ fileName: "main", fileExtension: ".pvs", contextFolder: sandboxExamples });
		dir(response);
		expect(response).toBeDefined();
		// expect(response.result).toEqual(test.typecheck1_result);
	}, 100000);

	// return; // the following tests are completed successfully -- remove the return statement if you want to run them

	// on Linux, this test fails for alaris2lnewmodes
	// the error reported is "No methods applicable for generic function #<standard-generic-function id> with args (#<IMPORTING pump_th..."
	it(`can generate .tcc file content`, async () => {
		label(`can generate the .tcc file content`);
		const response: PvsResponse = await pvsProxy.showTccs({ 
			fileName: "sqrt", 
			fileExtension: ".pvs", 
			theoryName: "sqrt", 
			contextFolder: sandboxExamples 
		});
		dir("response", response);
		expect(response.error).not.toBeDefined();
		expect(response.result).not.toBeNull();

		const response1: PvsResponse = await pvsProxy.showTccs({
			fileName:"alaris2lnewmodes", 
			fileExtension:".pvs", 
			theoryName: "alaris_th", 
			contextFolder: sandboxExamples
		});
		dir("response", response1);
		expect(response1.error).not.toBeDefined();
		expect(response1.result).not.toBeNull();
	}, 20000);

	// OK
	it(`can discharge tccs`, async () => {
		label(`can discharge tccs`);
		const fname: string = path.join(sandboxExamples, "sq.pvs");

		const response: PvsResponse = await pvsProxy.pvsRequest('prove-tccs', [ fname ]);
		dir("response", response);
		expect(response.error).not.toBeDefined();
		expect(response.result).toBeDefined();
		expect(response.result.totals).toEqual(2);
		expect(response.result.proved).toEqual(2);
		expect(response.result.unproved).toEqual(0);
		expect(response.result.subsumed).toEqual(0);
		expect(response.result.simplified).toEqual(0);
	}, 4000);
	
	//-----------------------
	// additional test cases
	//-----------------------

	for (let i = 0; i < steverFiles.length; i++) {
		it(`can parse stever/${steverFiles[i]}.pvs`, async () => {
			label(`can parse stever example ${steverFiles[i]}`);
			// Need to clear-theories, in case rerunning with the same server.
			await pvsProxy.lisp("(clear-theories t)");

			const response: PvsResponse = await pvsProxy.typecheckFile({
				fileName: steverFiles[i],
				fileExtension: ".pvs", 
				contextFolder: stever
			});
			dir(response); // set VERBOSE to true in test-utils if you want to see the output
			expect(response).toBeDefined();
			expect(response.error).not.toBeDefined();
			
		}, 20000);

	}
	for (let i = 0; i < pillboxFiles.length; i++) {
		it(`can parse pillbox/${pillboxFiles[i]}.pvs`, async () => {
			label(`can parse pillbox example ${pillboxFiles[i]}`);
			// Need to clear-theories, in case rerunning with the same server.
			await pvsProxy.lisp("(clear-theories t)");

			const response: PvsResponse = await pvsProxy.typecheckFile({
				fileName: pillboxFiles[i],
				fileExtension: ".pvs", 
				contextFolder: pillbox
			});
			dir(response); // set VERBOSE to true in test-utils if you want to see the output
			expect(response).toBeDefined();
			expect(response.error).not.toBeDefined();
			
		}, 20000);
	}

	for (let i = 0; i < pvsiowebFiles.length; i++) {
		it(`can parse pvsioweb/${pvsiowebFiles[i]}.pvs`, async () => {
			label(`can parse pvsioweb example ${pvsiowebFiles[i]}`);
			// Need to clear-theories, in case rerunning with the same server.
			await pvsProxy.lisp("(clear-theories t)");

			const response: PvsResponse = await pvsProxy.typecheckFile({
				fileName: pvsiowebFiles[i],
				fileExtension: ".pvs", 
				contextFolder: pvsioweb
			});
			dir(response); // set VERBOSE to true in test-utils if you want to see the output
			expect(response).toBeDefined();
			expect(response.error).not.toBeDefined();

		}, 60000);
	}


});
