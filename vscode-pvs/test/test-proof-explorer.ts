import * as fsUtils from "../server/src/common/fsUtils";
import { configFile } from './test-utils';
import * as path from 'path';
import { execSync } from "child_process";
import { PvsProofExplorer } from "../server/src/providers/pvsProofExplorer";
import { ProofDescriptor, ProofNodeX, PvsProofCommand } from "../server/src/common/serverInterface";
import { PvsLanguageServer } from "../server/src/pvsLanguageServer";

//----------------------------
//   Test cases for checking behavior of pvs with corrupted .pvscontext
//----------------------------
describe("proof-explorer", () => {
	let server: PvsLanguageServer = new PvsLanguageServer();
	beforeAll(async () => {
		const config: string = await fsUtils.readFile(configFile);
		const content: { pvsPath: string } = JSON.parse(config);
		// console.log(content);
		const pvsPath: string = content.pvsPath;
		await server.startPvsServer({ pvsPath }, { verbose: false, debugMode: false });

		console.log("\n----------------------");
		console.log("test-proof-explorer");
		console.log("----------------------");
	});
	afterAll(async () => {
	});

	const baseFolder: string = path.join(__dirname, "proof-explorer");
	const contextFolder: string = path.join(baseFolder, "foo")

	// OK
	it(`can be started when the proof is not present in the proof file`, async () => {
		// remove alaris folder if present and replace it with the content of the zip file
		fsUtils.deleteFolder(contextFolder);
		execSync(`cd ${baseFolder} && unzip foo.zip`);

		const request = {
			contextFolder,
			fileExtension: '.pvs',
			fileName: 'foo',
			formulaName: 'foo1',
			line: 8,
			theoryName: 'foo_th'
		}

		await server.typecheckFile(request);
		const proofExplorer: PvsProofExplorer = server.getProofExplorer();
		const pdesc: ProofDescriptor = await proofExplorer.loadProofRequest(request);

		// console.dir(response);
		expect(pdesc).toBeDefined();
		expect(pdesc.info.theory).toEqual("foo_th");
		expect(pdesc.info.formula).toEqual("foo1");
		expect(pdesc.info.status).toEqual("untried");
		expect(pdesc.info.prover).toBeDefined();
		expect(pdesc.info.shasum).toEqual("b474a6797e7f46760fd69a07ce715cfe856b1e9d37ddbe87a944fcd3bb260f35");

		const root: ProofNodeX = proofExplorer.getProofX();
		expect(root).toBeDefined();
		expect(root.branch).toEqual("");
		expect(root.name).toEqual(pdesc.info.formula);
		expect(root.type).toEqual("root");
		expect(root.parent).toEqual(root.id);
		// console.dir(root, { depth: null });

		// remove test folder 
		fsUtils.deleteFolder(path.join(baseFolder, "foo"));
	}, 6000);

	it(`can step single proof commands`, async () => {
		// remove alaris folder if present and replace it with the content of the zip file
		fsUtils.deleteFolder(contextFolder);
		execSync(`cd ${baseFolder} && unzip foo.zip`);

		const request: PvsProofCommand = {
			contextFolder,
			fileExtension: '.pvs',
			fileName: 'foo',
			formulaName: 'foo1',
			theoryName: 'foo_th',
			cmd: "(skosimp*)"
		};
		await server.proveFormulaRequest(request);

		const proofExplorer: PvsProofExplorer = server.getProofExplorer();
		let root: ProofNodeX = proofExplorer.getProofX();
		expect(root.name).toEqual(request.formulaName);
		expect(root.rules).toEqual([]);

		await proofExplorer.proofCommandRequest(request);
		root = proofExplorer.getProofX();
		expect(root.name).toEqual(request.formulaName);
		expect(root.rules[0].name).toEqual(request.cmd);
		expect(root.rules[0].type).toEqual("proof-command");
		expect(root.rules[0].parent).toEqual(root.id);
	});
	it(`can step a series of proof commands`, async () => {
		const request: PvsProofCommand = {
			contextFolder,
			fileExtension: '.pvs',
			fileName: 'foo',
			formulaName: 'foo1',
			theoryName: 'foo_th',
			cmd: `(assert)(grind)(case "x!1 > 0")(postpone)(grind)`
		};
		const proofExplorer: PvsProofExplorer = server.getProofExplorer();

		await proofExplorer.proofCommandRequest(request);
		const root: ProofNodeX = proofExplorer.getProofX();
		// console.dir(root, { depth: null });
		expect(root.rules.length).toEqual(4);
		expect(root.rules[0].name).toEqual("(skosimp*)");
		expect(root.rules[1].name).toEqual("(assert)");
		expect(root.rules[2].name).toEqual("(grind)");
		expect(root.rules[3].name).toEqual(`(case "x!1 > 0")`);
		expect(root.rules[3].rules.length).toEqual(2);
		expect(root.rules[3].rules[0].name).toEqual("(1)");
		expect(root.rules[3].rules[0].rules.length).toEqual(0); // postpone is never added to the proof
		expect(root.rules[3].rules[1].name).toEqual("(2)");
		expect(root.rules[3].rules[1].rules.length).toEqual(1);
		expect(root.rules[3].rules[1].rules[0].name).toEqual("(grind)"); // this will close branch 2

		const activeNode: ProofNodeX = proofExplorer.getActiveNode();
		// console.dir(activeNode);
		expect(activeNode.name).toEqual("(1)");
		expect(proofExplorer.ghostNodeIsActive()).toBeTrue();
	});
	it(`can perform (undo)`, async () => {
		const request: PvsProofCommand = {
			contextFolder,
			fileExtension: '.pvs',
			fileName: 'foo',
			formulaName: 'foo1',
			theoryName: 'foo_th',
			cmd: `(undo)`
		};
		const proofExplorer: PvsProofExplorer = server.getProofExplorer();
		await proofExplorer.proofCommandRequest(request);

		// the proof structure should be unchanged, and the active node should be `(case "x!1 > 0")`
		const root: ProofNodeX = proofExplorer.getProofX();
		// console.dir(root, { depth: null });
		expect(root.rules.length).toEqual(4);
		expect(root.rules[0].name).toEqual("(skosimp*)");
		expect(root.rules[1].name).toEqual("(assert)");
		expect(root.rules[2].name).toEqual("(grind)");
		expect(root.rules[3].name).toEqual(`(case "x!1 > 0")`);
		expect(root.rules[3].rules.length).toEqual(2);
		expect(root.rules[3].rules[0].name).toEqual("(1)");
		expect(root.rules[3].rules[0].rules.length).toEqual(0); // postpone is never added to the proof
		expect(root.rules[3].rules[1].name).toEqual("(2)");
		expect(root.rules[3].rules[1].rules.length).toEqual(1);
		expect(root.rules[3].rules[1].rules[0].name).toEqual("(grind)"); // this will close branch 2

		const activeNode: ProofNodeX = proofExplorer.getActiveNode();
		// console.dir(activeNode);
		expect(activeNode.name).toEqual(`(case "x!1 > 0")`);
		expect(proofExplorer.ghostNodeIsActive()).toBeFalse();

		// remove test folder 
		fsUtils.deleteFolder(path.join(baseFolder, "foo"));
	}, 6000);
	it(`can perform (undo undo)`, async () => {
		const request: PvsProofCommand = {
			contextFolder,
			fileExtension: '.pvs',
			fileName: 'foo',
			formulaName: 'foo1',
			theoryName: 'foo_th',
			cmd: `(undo undo)`
		};
		const proofExplorer: PvsProofExplorer = server.getProofExplorer();
		await proofExplorer.proofCommandRequest(request);

		// the proof structure should be unchanged, and the active node should be (1)
		let root: ProofNodeX = proofExplorer.getProofX();
		// console.dir(root, { depth: null });
		expect(root.rules.length).toEqual(4);
		expect(root.rules[0].name).toEqual("(skosimp*)");
		expect(root.rules[1].name).toEqual("(assert)");
		expect(root.rules[2].name).toEqual("(grind)");
		expect(root.rules[3].name).toEqual(`(case "x!1 > 0")`);
		expect(root.rules[3].rules.length).toEqual(2);
		expect(root.rules[3].rules[0].name).toEqual("(1)");
		expect(root.rules[3].rules[0].rules.length).toEqual(0); // postpone is never added to the proof
		expect(root.rules[3].rules[1].name).toEqual("(2)");
		expect(root.rules[3].rules[1].rules.length).toEqual(1);
		expect(root.rules[3].rules[1].rules[0].name).toEqual("(grind)"); // this will close branch 2

		const activeNode: ProofNodeX = proofExplorer.getActiveNode();
		// console.dir(activeNode);
		expect(proofExplorer.ghostNodeIsActive()).toBeTrue();
		expect(activeNode.name).toEqual(`ghost`);

		// can attach a node at the ghost position
		request.cmd = "(all-typepreds)";
		await proofExplorer.proofCommandRequest(request);
		// the proof structure should be unchanged, and the active node should be (1)
		root = proofExplorer.getProofX();
		expect(root.rules[3].rules[0].name).toEqual("(1)");
		expect(root.rules[3].rules[0].rules.length).toEqual(1); // postpone is never added to the proof
		expect(root.rules[3].rules[0].rules[0].name).toEqual("(all-typepreds)");
		expect(proofExplorer.ghostNodeIsActive()).toBeTrue();
		expect(activeNode.name).toEqual(`ghost`);
	}, 6000);
	it(`can automatically trim branches if proof structure has changed`, async () => {
		const request: PvsProofCommand = {
			contextFolder,
			fileExtension: '.pvs',
			fileName: 'foo',
			formulaName: 'foo1',
			theoryName: 'foo_th',
			cmd: `(undo)(case "x!1 > 2")` // the second command will generate two proof branches, so all-typepreds should be trimmed
		};
		const proofExplorer: PvsProofExplorer = server.getProofExplorer();
		await proofExplorer.proofCommandRequest(request);

		// the proof structure should be unchanged, and the active node should be (1)
		let root: ProofNodeX = proofExplorer.getProofX();
		// console.dir(root, { depth: null });
		expect(root.rules.length).toEqual(4);
		expect(root.rules[0].name).toEqual("(skosimp*)");
		expect(root.rules[1].name).toEqual("(assert)");
		expect(root.rules[2].name).toEqual("(grind)");
		expect(root.rules[3].name).toEqual(`(case "x!1 > 0")`);
		expect(root.rules[3].rules.length).toEqual(2);
		expect(root.rules[3].rules[0].name).toEqual("(1)");
		expect(root.rules[3].rules[0].rules.length).toEqual(1); // the first proof branch is automatically proved by pvs
		expect(root.rules[3].rules[0].rules[0].name).toEqual(`(case "x!1 > 2")`);
		expect(root.rules[3].rules[1].name).toEqual("(2)");
		expect(root.rules[3].rules[1].rules.length).toEqual(1);
		expect(root.rules[3].rules[1].rules[0].name).toEqual("(grind)"); // this will close branch 2

		let activeNode: ProofNodeX = proofExplorer.getActiveNode();
		expect(proofExplorer.ghostNodeIsActive()).toBeTrue();
		expect(activeNode.name).toEqual(`(1.1)`);

		// this sequence will trim again the node
		request.cmd = `(undo)(case "x!1 > 1")`;
		await proofExplorer.proofCommandRequest(request);

		// the proof structure should be unchanged, and the active node should be (1)
		root = proofExplorer.getProofX();
		// console.dir(root, { depth: null });
		expect(root.rules.length).toEqual(4);
		expect(root.rules[0].name).toEqual("(skosimp*)");
		expect(root.rules[1].name).toEqual("(assert)");
		expect(root.rules[2].name).toEqual("(grind)");
		expect(root.rules[3].name).toEqual(`(case "x!1 > 0")`);
		expect(root.rules[3].rules.length).toEqual(2);
		expect(root.rules[3].rules[0].name).toEqual("(1)");
		expect(root.rules[3].rules[0].rules.length).toEqual(1); // the first proof branch is automatically proved by pvs
		expect(root.rules[3].rules[0].rules[0].name).toEqual(`(case "x!1 > 1")`); // <<<<<
		expect(root.rules[3].rules[1].name).toEqual("(2)");
		expect(root.rules[3].rules[1].rules.length).toEqual(1);
		expect(root.rules[3].rules[1].rules[0].name).toEqual("(grind)"); // this will close branch 2

		activeNode = proofExplorer.getActiveNode();
		expect(proofExplorer.ghostNodeIsActive()).toBeTrue();
		expect(activeNode.name).toEqual(`(1.1)`);

		// remove test folder 
		fsUtils.deleteFolder(path.join(baseFolder, "foo"));
	}, 6000);
});

