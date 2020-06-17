import * as fsUtils from "../server/src/common/fsUtils";
import * as test from "./test-constants";
import { PvsResponse } from "../server/src/common/pvs-gui";
import { PvsProxy } from '../server/src/pvsProxy'; // XmlRpcSystemMethods
import { label, configFile } from './test-utils';
import * as path from 'path';
import { execSync } from "child_process";
import { PvsIoProxy } from '../server/src/pvsioProxy';

//----------------------------
//   Test cases for checking behavior of pvs with corrupted .pvscontext
//----------------------------
describe("pvs", () => {
	let pvsProxy: PvsProxy = null;
	let pvsioProxy: PvsIoProxy = null;
	beforeAll(async () => {
		const config: string = await fsUtils.readFile(configFile);
		const content: { pvsPath: string } = JSON.parse(config);
		// console.log(content);
		const pvsPath: string = content.pvsPath;
		// log("Activating xmlrpc proxy...");
		pvsProxy = new PvsProxy(pvsPath, { externalServer: test.EXTERNAL_SERVER });
		await pvsProxy.activate({ debugMode: false, showBanner: false }); // this will also start pvs-server

		pvsioProxy = new PvsIoProxy(pvsPath);

		console.log("\n----------------------");
		console.log("test-pvscontext");
		console.log("----------------------");
	});
	afterAll(async () => {
		await pvsProxy.killPvsServer();
		await pvsProxy.killPvsProxy();
	});

	// OK
	it(`can show tccs for alaris_th`, async () => {
		label(`can show tccs for alaris_th`);
		// Need to clear-theories, in case rerunning with the same server.
		await pvsProxy.lisp("(clear-theories t)");

		// remove alaris folder if present and replace it with the content of the zip file
		const baseFolder: string = path.join(__dirname, "pvscontext");
		fsUtils.deleteFolder(path.join(baseFolder, "alaris2l"));
		execSync(`cd ${path.join(__dirname, "pvscontext")} && unzip alaris2l-show-tccs-error.zip`);

		const response: PvsResponse = await pvsProxy.generateTccs({
			fileName: "alaris2lnewmodes", 
			fileExtension: ".pvs", 
			contextFolder: path.join(baseFolder, "alaris2l"),
			theoryName: "alaris_th"
		});
		// console.dir(response);
		expect(response).toBeDefined();
		expect(response.result).toBeDefined();
		expect(response.error).not.toBeDefined();

		// remove alaris folder 
		fsUtils.deleteFolder(path.join(baseFolder, "alaris2l"));
	}, 20000);

	// OK
	it(`can find typecheck error in ICEcoordinator.pvs (wrong field type)`, async () => {
		label(`can find typecheck error in ICEcoordinator.pvs (wrong field type)`);
		// Need to clear-theories, in case rerunning with the same server.
		await pvsProxy.lisp("(clear-theories t)");

		// remove pillboxv7 folder if present and replace it with the content of the zip file
		const baseFolder: string = path.join(__dirname, "pvscontext");
		fsUtils.deleteFolder(path.join(baseFolder, "pvsICEipandvs2"));
		execSync(`cd ${baseFolder} && unzip pvsICEipandvs2-wrong-field.zip`);

		const response: PvsResponse = await pvsProxy.typecheckFile({
			fileName: "main", 
			fileExtension: ".pvs", 
			contextFolder: path.join(baseFolder, "pvsICEipandvs2")
		});
		// console.dir(response);
		expect(response).toBeDefined();
		expect(response.result).not.toBeDefined();
		expect(response.error).toBeDefined();

		// remove pillboxv7 folder 
		fsUtils.deleteFolder(path.join(baseFolder, "pvsICEipandvs2"));
	}, 10000);

	// FAILS: crashed into Lisp
	it(`can typecheck strings defined in pillboxv7`, async () => {
		label(`can typecheck strings defined in pillboxv7`);
		// Need to clear-theories, in case rerunning with the same server.
		await pvsProxy.lisp("(clear-theories t)");

		// remove pillboxv7 folder if present and replace it with the content of the zip file
		const baseFolder: string = path.join(__dirname, "pvscontext");
		fsUtils.deleteFolder(path.join(baseFolder, "pillboxv7"));
		execSync(`cd ${baseFolder} && unzip pillboxv7-errors-with-strings.zip`);

		const response: PvsResponse = await pvsProxy.typecheckFile({
			fileName: "main", 
			fileExtension: ".pvs", 
			contextFolder: path.join(baseFolder, "pillboxv7")
		});
		// console.dir(response);
		expect(response).toBeDefined();
		expect(response.result).toBeDefined();
		expect(response.error).not.toBeDefined();

		// remove pillboxv7 folder 
		fsUtils.deleteFolder(path.join(baseFolder, "pillboxv7"));
	}, 10000);

	// FAIL: returns wrong error message
	it(`returns the correct error message ('limits' theory defined twice in the same context) when typechecking baxterSigmaSpectrum.pvs`, async () => {
		label(`returns the correct error message ('limits' theory defined twice in the same context) when typechecking baxterSigmaSpectrum.pvs`);
		// Need to clear-theories, in case rerunning with the same server.
		await pvsProxy.lisp("(clear-theories t)");

		// remove baxter folder if present and replace it with the content of the zip file
		const baseFolder: string = path.join(__dirname, "pvscontext");
		fsUtils.deleteFolder(path.join(baseFolder, "baxter"));
		execSync(`cd ${path.join(__dirname, "pvscontext")} && unzip baxter-two-theory-limits.zip`);

		const response: PvsResponse = await pvsProxy.typecheckFile({
			fileName: "baxterSigmaSpectrum", 
			fileExtension: ".pvs", 
			contextFolder: path.join(baseFolder, "baxter")
		});
		// console.dir(response);
		expect(response).toBeDefined();
		expect(response.result).not.toBeDefined();
		expect(response.error).toBeDefined();
		// console.dir(response.error);
		expect(response.error.data).toBeDefined();
		expect(response.error.data.error_string).toBeDefined();
		expect(response.error.data.error_string).toMatch(/\blimits\b/g);

		// remove baxter folder 
		fsUtils.deleteFolder(path.join(baseFolder, "baxter"));
	}, 10000);

	// FAIL: crashes into Lisp
	it(`can typecheck lists defined in pillboxv7`, async () => {
		label(`can typecheck lists defined in pillboxv7`);
		// Need to clear-theories, in case rerunning with the same server.
		await pvsProxy.lisp("(clear-theories t)");

		// remove pillboxv7 folder if present and replace it with the content of the zip file
		const baseFolder: string = path.join(__dirname, "pvscontext");
		fsUtils.deleteFolder(path.join(baseFolder, "pillboxv7"));
		execSync(`cd ${baseFolder} && unzip pillboxv7-errors-with-lists.zip`);

		const response: PvsResponse = await pvsProxy.typecheckFile({
			fileName: "firstpillchecks", 
			fileExtension: ".pvs", 
			contextFolder: path.join(baseFolder, "pillboxv7")
		});
		// console.dir(response);
		expect(response).toBeDefined();
		expect(response.result).toBeDefined();
		expect(response.error).not.toBeDefined();

		// remove pillboxv7 folder 
		fsUtils.deleteFolder(path.join(baseFolder, "pillboxv7"));
	}, 10000);

	// FAIL: crashes into Lisp
	it(`can typecheck datatypes in type_theory`, async () => {
		label(`can typecheck datatypes in type_theory`);
		// Need to clear-theories, in case rerunning with the same server.
		await pvsProxy.lisp("(clear-theories t)");

		// remove folder if present and replace it with the content of the zip file
		const baseFolder: string = path.join(__dirname, "pvscontext");
		fsUtils.deleteFolder(path.join(baseFolder, "type_theory"));
		execSync(`cd ${baseFolder} && unzip type-theory-error-with-datatypes.zip`);

		const response: PvsResponse = await pvsProxy.typecheckFile({
			fileName: "basics", 
			fileExtension: ".pvs", 
			contextFolder: path.join(baseFolder, "type_theory")
		});
		// console.dir(response);
		expect(response).toBeDefined();
		expect(response.result).toBeDefined();
		expect(response.error).not.toBeDefined();

		// remove folder 
		fsUtils.deleteFolder(path.join(baseFolder, "type_theory"));
	}, 10000);


	// FAIL: crashes into Lisp
	it(`can typecheck datatypes in trace`, async () => {
		label(`can typecheck datatypes in trace`);
		// Need to clear-theories, in case rerunning with the same server.
		await pvsProxy.lisp("(clear-theories t)");

		// remove folder if present and replace it with the content of the zip file
		const baseFolder: string = path.join(__dirname, "pvscontext");
		fsUtils.deleteFolder(path.join(baseFolder, "trace"));
		execSync(`cd ${baseFolder} && unzip trace.zip`);

		let response: PvsResponse = await pvsProxy.parseFile({
			fileName: "trace", 
			fileExtension: ".pvs", 
			contextFolder: path.join(baseFolder, "trace")
		});
		// console.dir(response);
		response = await pvsProxy.parseFile({
			fileName: "CONDITIONS", 
			fileExtension: ".pvs", 
			contextFolder: path.join(baseFolder, "trace")
		});
		// console.dir(response);
		response = await pvsProxy.typecheckFile({
			fileName: "CONDITIONS", 
			fileExtension: ".pvs", 
			contextFolder: path.join(baseFolder, "trace")
		});
		// console.dir(response);
		expect(response).toBeDefined();
		expect(response.result).toBeDefined();
		expect(response.error).not.toBeDefined();

		// remove folder 
		fsUtils.deleteFolder(path.join(baseFolder, "trace"));
	}, 10000);

	// it(`can evalute ground expressions`, async () => {
	// 	label(`can evalute ground expressions`);
	// 	// Need to clear-theories, in case rerunning with the same server.
	// 	await pvsProxy.lisp("(clear-theories t)");

	// 	// remove folder if present and replace it with the content of the zip file
	// 	const baseFolder: string = path.join(__dirname, "pvscontext");
	// 	fsUtils.deleteFolder(path.join(baseFolder, "pvsICEipandvs3"));
	// 	execSync(`cd ${baseFolder} && unzip pvsio-error-with-strings.zip`);

	// 	await pvsioProxy.startEvaluator({
	// 		fileName: "main", 
	// 		fileExtension: ".pvs", 
	// 		contextFolder: path.join(baseFolder, "pvsICEipandvs3"),
	// 		theoryName: "main"
	// 	});
	// 	const response: PvsResponse = await pvsioProxy.evaluateExpression({
	// 		fileName: "main", 
	// 		fileExtension: ".pvs", 
	// 		contextFolder: path.join(baseFolder, "pvsICEipandvs3"),
	// 		theoryName: "main",
	// 		cmd: "print_state(istate0)"
	// 	});
	// 	console.dir(response);
	// 	expect(response).toBeDefined();
	// 	// expect(response.result).toBeDefined();
	// 	// expect(response.error).not.toBeDefined();

	// 	// remove pillboxv7 folder 
	// 	fsUtils.deleteFolder(path.join(baseFolder, "pvsICEipandvs2"));
	// }, 10000);


});

