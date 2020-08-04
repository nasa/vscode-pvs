import * as fsUtils from "../server/src/common/fsUtils";
import { configFile } from './test-utils';
import * as path from 'path';
import { PvsProofExplorer } from "../server/src/providers/pvsProofExplorer";
import { ProofNodeX, PvsProofCommand } from "../server/src/common/serverInterface";
import { PvsLanguageServer } from "../server/src/pvsLanguageServer";
import { PvsResponse } from "../server/src/common/pvs-gui";

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
		await server.startPvsServer({ pvsPath, externalServer: false }, { verbose: false, debugMode: false });

		console.log("\n----------------------");
		console.log("test-proof-explorer");
		console.log("----------------------");
	});
	afterAll(async () => {
	});

	const baseFolder: string = path.join(__dirname, "proof-explorer");
	const contextFolder: string = path.join(baseFolder, "foo")

	// remove alaris folder if present and replace it with the content of the zip file
	// fsUtils.deleteFolder(contextFolder);
	// execSync(`cd ${baseFolder} && unzip foo.zip`);

	const request: PvsProofCommand = {
		contextFolder,
		fileExtension: '.pvs',
		fileName: 'foo',
		formulaName: 'foo1',
		theoryName: 'foo_th',
		cmd: ""
	};
	const request2: PvsProofCommand = {
		contextFolder,
		fileExtension: '.pvs',
		fileName: 'foo',
		formulaName: 'foo2',
		theoryName: 'foo_th',
		cmd: ""
	};
	const request2a: PvsProofCommand = {
		contextFolder,
		fileExtension: '.pvs',
		fileName: 'foo',
		formulaName: 'foo2a',
		theoryName: 'foo_th',
		cmd: ""
	};
	const request5: PvsProofCommand = {
		contextFolder,
		fileExtension: '.pvs',
		fileName: 'foo',
		formulaName: 'foo5',
		theoryName: 'foo_th',
		cmd: ""
	};

	it(`can step single proof commands`, async () => {
		await server.getPvsProxy().quitProofIfInProver();

		await server.proveFormulaRequest(request);

		const proofExplorer: PvsProofExplorer = server.getProofExplorer();
		let root: ProofNodeX = proofExplorer.getProofX();
		expect(root.name).toEqual(request.formulaName);
		expect(root.rules.length).toEqual(0);

		request.cmd = "(skosimp*)";
		await proofExplorer.proofCommandRequest(request);
		root = proofExplorer.getProofX();
		// console.dir(root);
		expect(root.name).toEqual(request.formulaName);
		expect(root.rules[0].name).toEqual("(skosimp*)");
		expect(root.rules[0].type).toEqual("proof-command");
		expect(root.rules[0].parent).toEqual(root.id);
	});

	it(`can step a series of proof commands`, async () => {
		request.cmd = `(assert)(grind)(case "x!1 > 0")(postpone)(grind)`;
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
		request.cmd = "(undo)";
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
	}, 6000);
	it(`can perform (undo undo)`, async () => {
		request.cmd = "(undo undo)";
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
		request.cmd = `(undo)(case "x!1 > 2")`; // the second command will generate two proof branches, so all-typepreds should be trimmed
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
	}, 6000);
	it(`can start another proof when a prover session has already started`, async () => {
		await server.proveFormulaRequest(request5);

		const proofExplorer: PvsProofExplorer = server.getProofExplorer();
		let root: ProofNodeX = proofExplorer.getProofX();

		expect(root.name).toEqual(request5.formulaName);
		expect(root.rules.length).toEqual(2);
		expect(root.rules[0].name).toEqual("(skosimp*)");
		expect(root.rules[1].name).toEqual("(grind)");
	});
	it(`can delete a proof and display the correct active node`, async () => {
		const proofExplorer: PvsProofExplorer = server.getProofExplorer();
		let root: ProofNodeX = proofExplorer.getProofX();
		expect(proofExplorer.isActive({ id: root.id, name: root.name }));

		proofExplorer.deleteNodeX({ action: "delete-node", selected: { id: root.id, name: root.name }});

		root = proofExplorer.getProofX();
		expect(root.name).toEqual(request5.formulaName);
		expect(root.rules.length).toEqual(0);
		expect(proofExplorer.ghostNodeIsActive()).toBeTrue();
		expect(proofExplorer.isPending({ id: root.id, name: root.name }));
	});
	it(`can automatically trim branches at the beginning of a proof, if proof structure has changed`, async () => {
		await server.proveFormulaRequest(request2);

		const proofExplorer: PvsProofExplorer = server.getProofExplorer();
		let root: ProofNodeX = proofExplorer.getProofX();
		expect(root.name).toEqual(request2.formulaName);
		expect(root.rules[0].name).toEqual("(skosimp*)");

		expect(proofExplorer.ghostNodeIsActive()).not.toBeTrue();
		let activeNode: ProofNodeX = proofExplorer.getActiveNode();
		expect(activeNode.name).toEqual(`(skosimp*)`);

		request2.cmd = "(grind)";
		await proofExplorer.proofCommandRequest(request2);

		expect(proofExplorer.ghostNodeIsActive()).toBeTrue();

		root = proofExplorer.getProofX();
		expect(root.rules[0].name).toEqual("(grind)");
		expect(root.rules[0].rules[0].name).toEqual("(1)");
		activeNode = proofExplorer.getActiveNode();
		expect(activeNode.name).toEqual(`(1)`);
	});
	it(`can trim branches with active nodes and correctly re-position the active node`, async () => {
		await server.proveFormulaRequest(request2a);

		const proofExplorer: PvsProofExplorer = server.getProofExplorer();
		let root: ProofNodeX = proofExplorer.getProofX();
		expect(root.name).toEqual(request2a.formulaName);
		expect(root.rules[0].name).toEqual("(grind)");

		expect(proofExplorer.ghostNodeIsActive()).not.toBeTrue();
		let activeNode: ProofNodeX = proofExplorer.getActiveNode();
		expect(activeNode.name).toEqual(`(grind)`);

		request2.cmd = "(skosimp*)";
		await proofExplorer.proofCommandRequest(request2);

		expect(proofExplorer.ghostNodeIsActive()).toBeFalse();

		root = proofExplorer.getProofX();
		expect(root.rules.length).toEqual(2);
		expect(root.rules[0].name).toEqual(`(skosimp*)`);
		expect(root.rules[1].name).toEqual(`(grind)`);
		activeNode = proofExplorer.getActiveNode();
		expect(activeNode.name).toEqual(`(grind)`);

		proofExplorer.trimNodeX({ action: "trim-node", selected: { id: root.rules[0].id, name: root.rules[0].name }});
		root = proofExplorer.getProofX();
		// console.dir(root, { depth: null });
		expect(root.rules.length).toEqual(1);
		expect(proofExplorer.ghostNodeIsActive()).toBeTrue();
		activeNode = proofExplorer.getActiveNode();
		expect(activeNode.name).toEqual(`ghost`);

	});
			// // remove test folder 
			// fsUtils.deleteFolder(path.join(baseFolder, "foo"));

});

