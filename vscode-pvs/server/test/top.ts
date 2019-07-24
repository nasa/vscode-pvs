import { PvsProcess } from "./server/pvsProcess";
import { PvsResponseType, PvsVersionInfoResponseType, JsonType, ProofNodeType, ProofObjectType, PvsListProofStrategiesResponseType, StrategyDescriptor, PvsFindDeclarationResponseType, PvsListDeclarationsResponseType, PvsDeclarationType, PvsParserResponse, PvsTypecheckerResponse } from "./server/common/serverInterface";
import * as fsUtils from "./server/common/fsUtils";
import * as test from "./test-costants";
import * as path from 'path';
import { XmlRpcProxy, XmlRpcResponse, XmlRpcSystemMethods } from './server/common/xmlrpcProvider';

const VERBOSE: boolean = true;

function log (data: any, opt?: { force?: boolean, stringify?: boolean, expanded?: boolean, label?: string }): void {
	opt = opt || {};
	if (VERBOSE || opt.force) {
		if (opt.label) {
			console.info(`\n==== ${opt.label} ====`);
		}
		if (opt.stringify) {
			if (opt.expanded) {
				console.log("\n", JSON.stringify(data, null, " "));
			} else {
				console.log("\n", JSON.stringify(data));
			}
		} else {
			console.log("\n", data);
		}
	}
}

function label (l: string): void {
	if (VERBOSE) {
		console.info(`\n==== ${l} ====`);
	}
}

describe("PvsProcess", async () => {

	const accord: string = "~/Work/sandbox/nasalib/ACCoRD";
	const nasalib: string = "~/Work/sandbox/nasalib";

	const pvsExamples: string = "~/git/PVS/src/interface/";

	// testPvsProcess("~/Work/pvs-snapshots/pvs-7.0-851/");
	// testPvsProcess("~/Work/pvs-snapshots/pvs-7.0.962");

	testPvsServer("~/Work/pvs-snapshots/pvs-7.0.962");

	//-----------------------------------

	function testPvsServer(pvsPath: string) {
		log(`PvsServer`);
		
		// beforeAll
		const serverProxy: XmlRpcProxy = new XmlRpcProxy();
		const proc: PvsProcess = new PvsProcess({ pvsPath, processType: "xml-rpc-server" });

		beforeAll(async () => {
		});

		beforeEach(async () => {
			const success: boolean = await proc.pvs({ xmlRpcServer: { port: 22334 }});
			await serverProxy.activate();
		});

		afterEach(async () => {
			await serverProxy.kill();
			await proc.kill();
		});

		it(`[PvsServer] can create XML-RPC server`, async () => {
			label(`[PvsServer] can create XML-RPC server`);
			expect(serverProxy).toBeDefined();
		});

		it(`[PvsServer] can list xml-rpc system methods`, async () => {
			label(`[PvsServer] can list xml-rpc system methods`);
			const ans: XmlRpcSystemMethods = await serverProxy.listSystemMethods();
			expect(ans).toBeDefined();
			expect(ans.error).toBeNull();
			expect(ans.res).toBeDefined();
			log(ans);
		});

		it(`[PvsServer] can list pvs-server methods`, async () => {
			label(`[PvsServer] can list pvs-server methods`);
			const ans: XmlRpcResponse = await serverProxy.listMethods();
			expect(ans).toBeDefined();
			expect(ans.error).toBeNull();
			expect(ans.res).toBeDefined();
			log(ans);
		});

		it(`[PvsServer] knows client methods`, async () => {
			label(`[PvsServer] has client methods`);
			const ans: XmlRpcResponse = await serverProxy.clientMethods();
			expect(ans.error).toBeNull();
			expect(ans.res).toBeDefined();
			expect(ans.res.jsonrpc_result).toBeDefined();
			expect(ans.res.jsonrpc_result.result).toEqual(serverProxy.client_methods);
			log(ans);
		});

		it(`[PvsServer] can identify current context`, async () => {
			label(`[PvsServer] can identify current context`);
			const ans: XmlRpcResponse = await serverProxy.currentContext();
			expect(ans.error).toBeNull();
			expect(ans.res).toBeDefined();
			expect(ans.res.jsonrpc_result).toBeDefined();
			expect(ans.res.jsonrpc_result.result).toEqual(ans.res.context);
			log(ans);
		});

		it(`[PvsServer] can change context`, async () => {
			label(`[PvsServer] can change context`);

			let ans: XmlRpcResponse = await serverProxy.changeContext(accord);
			expect(ans.error).toBeNull();
			expect(ans.res).toBeDefined();
			log(ans);
			// console.log(`1st: `, ans.res);
			expect(ans.res.jsonrpc_result).toBeDefined();
			expect(ans.res.jsonrpc_result.result).toBeDefined();
			expect(ans.res.jsonrpc_result.error).not.toBeDefined();
			expect(fsUtils.normalizePath(ans.res.context)).toEqual(fsUtils.normalizePath(ans.res.jsonrpc_result.result));

			ans = await serverProxy.currentContext();
			// console.log(`2nd: `, ans.res);
			expect(ans.res).toBeDefined();
			log(ans);
			expect(ans.res.jsonrpc_result).toBeDefined();
			expect(ans.res.jsonrpc_result.result).toBeDefined();
			expect(ans.res.jsonrpc_result.error).not.toBeDefined();
			expect(fsUtils.normalizePath(ans.res.jsonrpc_result.result)).toEqual(fsUtils.normalizePath(accord));
			expect(ans.res.context).toEqual(ans.res.jsonrpc_result.result);

			ans = await serverProxy.changeContext("..");
			expect(ans.error).toBeNull();
			expect(ans.res).toBeDefined();
			log(ans.res);
			// console.log(`3rd: `, ans.res);
			ans = await serverProxy.currentContext();
			expect(ans.res).toBeDefined();
			expect(ans.res.jsonrpc_result).toBeDefined();
			expect(ans.res.jsonrpc_result.result).toBeDefined();
			expect(ans.res.jsonrpc_result.error).not.toBeDefined();
			expect(ans.res.context).toEqual(ans.res.jsonrpc_result.result);
			expect(fsUtils.normalizePath(ans.res.jsonrpc_result.result)).toEqual(fsUtils.normalizePath(nasalib));
			log(ans);
		});

		it(`[PvsServer] can typecheck file`, async () => {
			label(`[PvsServer] can typecheck file`);

			let ans: XmlRpcResponse = await serverProxy.changeContext(pvsExamples);
			ans = await serverProxy.typecheckFile("sqrt");
			log(ans, { stringify: true });
			expect(ans).toBeDefined();
			expect(ans.res).toBeDefined();
		}, 100000);

		// it(`[PvsServer] ` + "can parse file", async () => {
		// 	// const proc: PvsProcess = new PvsProcess({ pvsPath, processType: "xml-rpc-server" });
		// 	// const success: boolean = await proc.pvs({ xmlRpcServer: { port: 22334 }});
		// 	// const serverProxy: XmlRpcProxy = new XmlRpcProxy();
		// 	// let ans: XmlRpcResponse = await serverProxy.changeContext("~/Work/sandbox/nasalib/ACCoRD");
		// 	let ans: XmlRpcResponse = await serverProxy.parseFile("~/Work/sandbox/nasalib/ACCoRD/bands_3D");
		// 	// console.log(ans);
		// 	// console.log(ans.res);
		// 	// let ans: XmlRpcResponse = await serverProxy.typecheck("~/Work/sandbox/nasalib/ACCoRD/bands_3D.pvs");
		// 	// let ans: XmlRpcResponse = await serverProxy.typecheck("sqrt");
		// 	// console.log(ans.res);
		// 	// expect(ans.error).toBeNull();
		// 	expect(ans.res).toBeDefined();
		// 	console.log(ans.res);

		// 	await serverProxy.kill();
		// 	// await proc.kill();
		// });

	}

	//-----------------------------------

	function testPvsProcess(pvsPath: string) {
		log(`PvsProcess ${pvsPath}`);
		it(`[${pvsPath}] ` + "constructor can create a pvs process", async () => {
			const proc: PvsProcess = new PvsProcess({ pvsPath });
			const success: boolean = await proc.pvs();
			expect(success).toBeDefined();
			const versionInfo: PvsResponseType = await proc.getPvsVersionInfo();
			expect(versionInfo).toBeDefined();
			expect(versionInfo.res).toBeDefined();
			log(versionInfo.res, { label: "constructor can create a pvs process" });
		});

		it(`[${pvsPath}] ` + "constructor can set context", async () => {
			const contextFolder: string = "~/Work/";
			const proc: PvsProcess = new PvsProcess({ pvsPath, contextFolder });
			await proc.pvs();
			const ctx: string = await proc.getContextFolder();
			log(`Current context: ${ctx}`, { label: "constructor can set context" });
			expect(ctx).toEqual(fsUtils.tildeExpansion(contextFolder));
		});

		it(`[${pvsPath}] ` + "changeContext() can change context", async () => {
			const proc: PvsProcess = new PvsProcess({ pvsPath });
			await proc.pvs();
			const ctx: string = await proc.getContextFolder();
			log(`Current context: ${ctx}`, { label: "changeContext() can change context" });
			await proc.changeContext("..");
			const new_ctx: string = await proc.getContextFolder();
			log(`New context: ${new_ctx}`, { label: "changeContext() can change context" });
			expect(new_ctx).not.toEqual(ctx);
			const sup: string = `${ctx.split("/").slice(0, -2).join("/")}/`;
			expect(new_ctx).toEqual(sup);
		});

		it(`[${pvsPath}] ` + "restart() can restart pvs and keep same context folder", async () => {
			const proc: PvsProcess = new PvsProcess({ pvsPath });
			await proc.pvs();
			const ctx: string = await proc.getContextFolder();
			log(`Current context: ${ctx}`, { label: "restart() can restart pvs and keep same context folder" });
			await proc.restart();
			const ctx_after_restart: string = await proc.getContextFolder();
			log(`Context after restart: ${ctx_after_restart}`);
			expect(ctx_after_restart).toEqual(ctx);
		});

		it(`[${pvsPath}] ` + "getProcessID() can provide process ID", async () => {
			const proc: PvsProcess = new PvsProcess({ pvsPath });
			await proc.pvs();
			const pid: string = await proc.getProcessID();
			log(`Process ID: ${pid}`, { label: "getProcessID() can provide process ID" });
			expect(pid).toBeDefined();
		});

		it(`[${pvsPath}] ` + "kill() can kill a pvs process", async () => {
			const proc: PvsProcess = new PvsProcess({ pvsPath });
			await proc.pvs();
			const pid: string = await proc.getProcessID();
			log(`Process ID: ${pid}`, { label: "kill() can kill a pvs process" });
			expect(pid).toBeDefined();
			const killed_pid: string = await proc.kill();
			log(`Killed process ID: ${killed_pid}`, { label: "kill() can kill a pvs process" });
			expect(killed_pid).toBeDefined();
			expect(killed_pid).toEqual(pid);
		});

		it(`[${pvsPath}] ` + "getPvsPath can provide the path to the pvs executable", async () => {
			const proc: PvsProcess = new PvsProcess({ pvsPath });
			await proc.pvs();
			const _pvsPath: string = await proc.getPvsPath();
			log(`PVS path: ${_pvsPath}`, { label: "getPvsPath can provide the path to the pvs executable" });
			expect(_pvsPath).toBeDefined();
		});

		it(`[${pvsPath}] ` + "getPvsLibraryPath can provide library path", async () => {
			const proc: PvsProcess = new PvsProcess({ pvsPath });
			await proc.pvs();
			const lib: string = await proc.getPvsLibraryPath();
			log(`Library path: ${lib}`, { label: "getPvsLibraryPath can provide library path" });
			expect(lib).toBeDefined();
			expect(lib).toEqual(path.join(fsUtils.tildeExpansion(pvsPath), "lib"))
		});

		it(`[${pvsPath}] ` + "prf2json() can translate .prf files into JSON objects", async () => {
			test.proof_tactics.forEach(tactic => {
				const res: ProofObjectType = PvsProcess.prf2json(tactic, "test");
				expect(res).toBeDefined();
				expect(res.proof).toBeDefined();
				expect(res.proof.id).toBeDefined();
				expect(res.proof.type).toEqual("root");
				expect(res.proof.children).toBeDefined();
				log(res, { label: "prf2json() can translate .prf files into JSON objects" });
			});
		});

		it(`[${pvsPath}] ` + "getPvsVersionInfo() can print pvs version info as a JSON objects", async () => {
			const proc: PvsProcess = new PvsProcess({ pvsPath });
			await proc.pvs();
			const version: PvsVersionInfoResponseType = await proc.getPvsVersionInfo();
			log(version, { label: "getPvsVersionInfo() can print pvs version info as a JSON objects" });
			expect(version).toBeDefined();
			expect(version.res).toBeDefined();
			expect(version.res.pvsVersion).toBeDefined();
			expect(version.res.lispVersion).toBeDefined();
		});

		it(`[${pvsPath}] ` + "listProofStrategies() can print proof strategies", async () => {
			const proc: PvsProcess = new PvsProcess({ pvsPath });
			await proc.pvs();
			const strategies: PvsListProofStrategiesResponseType = await proc.listProofStrategies();
			expect(strategies).toBeDefined();
			expect(strategies.res).toBeDefined();
			log(strategies.res, { label: "listProofStrategies() can print proof strategies" });
			strategies.res.forEach((strategy: StrategyDescriptor) => {
				expect(strategy.description).toBeDefined();
				expect(strategy.name).toBeDefined();
			});
		});

		it(`[${pvsPath}] ` + "findDeclaration() can find declarations from the prelude", async () => {
			const proc: PvsProcess = new PvsProcess({ pvsPath });
			await proc.pvs();
			const declarations: PvsFindDeclarationResponseType = await proc.findDeclaration("real");
			expect(declarations).toBeDefined();
			expect(declarations.res).toBeDefined();
			log(declarations.res, { label: "findDeclaration() can find declarations from the prelude" });
			expect(declarations.res.length).toEqual(1);
			declarations.res.forEach((decl: PvsDeclarationType) => {
				expect(decl.symbolDeclaration).toBeDefined();
				expect(decl.symbolDeclarationFile).toBeDefined();
				expect(decl.symbolDeclarationRange).toBeDefined();
				expect(decl.symbolName).toBeDefined();
				expect(decl.theoryName).toBeDefined();
			});
		});

		it(`[${pvsPath}] ` + "findDeclaration() can list all declarations in a given theory", async () => {
			const proc: PvsProcess = new PvsProcess({ pvsPath });
			await proc.pvs();
			const declarations: PvsListDeclarationsResponseType = await proc.listDeclarations({ theoryName: "reals" });
			expect(declarations).toBeDefined();
			expect(declarations.res).toBeDefined();
			log(declarations.res, { label: "findDeclaration() can list all declarations in a given theory" });
			expect(declarations.res.length).not.toEqual(0);
			declarations.res.forEach((decl: PvsDeclarationType) => {
				expect(decl.symbolDeclaration).toBeDefined();
				expect(decl.symbolDeclarationFile).toBeDefined();
				expect(decl.symbolDeclarationRange).toBeDefined();
				expect(decl.symbolName).toBeDefined();
				expect(decl.theoryName).toBeDefined();
			});
		});

		it(`[${pvsPath}] ` + "parseFile() can parse a pvs file", async () => {
			const proc: PvsProcess = new PvsProcess({ pvsPath });
			await proc.pvs();
			const ans: PvsParserResponse = await proc.parseFile({ contextFolder: "~/Work/sandbox/nasalib/ACCoRD", fileName: "bands_3D", fileExtension: ".pvs" });
			expect(ans).toBeDefined();
			expect(ans.error).toBeNull();
			expect(ans.res).toBeDefined();
			log(ans, { label: "parseFile() can parse a pvs file" });
		});

		it(`[${pvsPath}] ` + "typecheckFile() can typecheck a pvs file", async () => {
			const proc: PvsProcess = new PvsProcess({ pvsPath });
			await proc.pvs();
			const ans: PvsTypecheckerResponse = pvsPath.endsWith(".962") ?
							await proc.typecheckFile({ contextFolder: "~/Work/sandbox/examples", fileName: "alaris2lnewmodes.types_and_constants.pvs", fileExtension: ".pvs" })
							: await proc.typecheckFile({ contextFolder: "~/Work/sandbox/examples", fileName: "alaris2lnewmodes.types_and_constants", fileExtension: ".pvs" });
			expect(ans).toBeDefined();
			expect(ans.error).toBeNull();
			log(ans, { label: "typecheckFile() can typecheck a pvs file" });
		}, 10000);
	};

});
