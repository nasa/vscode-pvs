import * as fsUtils from "../server/src/common/fsUtils";
import { configFile, sandboxExamples } from './test-utils';
import * as path from 'path';
import { PvsProofExplorer } from "../server/src/providers/pvsProofExplorer";
import { ProofNodeX, PvsFormula, PvsProofCommand, SequentDescriptor } from "../server/src/common/serverInterface";
import { PvsLanguageServer } from "../server/src/pvsLanguageServer";
import { PvsResponse, PvsResult } from "../server/src/common/pvs-gui";
import { expect } from 'chai';

//----------------------------
//   Test cases for checking behavior of pvs with corrupted .pvscontext
//----------------------------
describe("proof-explorer", () => {
	let server: PvsLanguageServer = new PvsLanguageServer();
	before(async () => {
		const config: string = await fsUtils.readFile(configFile);
		const content: { pvsPath: string } = JSON.parse(config);
		// console.log(content);
		const pvsPath: string = content.pvsPath;
		await server.startPvsServer({ pvsPath, externalServer: true }, { verbose: false, debugMode: false });

		console.log("\n----------------------");
		console.log("test-proof-explorer");
		console.log("----------------------");
	});
	after(async () => {
	});

	const baseFolder: string = path.join(__dirname, "proof-explorer");
	const contextFolder: string = path.join(baseFolder, "foo")

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

	// the following groupd of tests needs to be performed together -- don't use fit() to enable just one of them
	it(`can step single proof commands`, async () => {
		const proverStatus: PvsResult = await server.getPvsProxy().pvsRequest('prover-status'); // await pvsProxy.getProverStatus();		
		if (proverStatus && proverStatus.result !== "inactive") {
			await server.getPvsProxy().proofCommand({ cmd: 'quit' });
		}
		// await server.getPvsProxy().quitProofIfInProver();

		await server.proveFormulaRequest(request);

		const proofExplorer: PvsProofExplorer = server.getProofExplorer();
		let root: ProofNodeX = proofExplorer.getProofX();
		expect(root.name).to.deep.equal(request.formulaName);
		expect(root.rules.length).to.equal(0);

		request.cmd = "(skosimp*)";
		await proofExplorer.proofCommandRequest(request);
		root = proofExplorer.getProofX();
		// console.dir(root);
		expect(root.name).to.deep.equal(request.formulaName);
		expect(root.rules[0].name).to.deep.equal("(skosimp*)");
		expect(root.rules[0].type).to.deep.equal("proof-command");
		expect(root.rules[0].parent).to.deep.equal(root.id);
	});
	it(`can step a series of proof commands`, async () => {
		request.cmd = `(assert)(grind)(case "x!1 > 0")(postpone)(grind)`;
		const proofExplorer: PvsProofExplorer = server.getProofExplorer();

		await proofExplorer.proofCommandRequest(request);
		const root: ProofNodeX = proofExplorer.getProofX();
		// console.dir(root, { depth: null });
		expect(root.rules.length).to.equal(4);
		expect(root.rules[0].name).to.deep.equal("(skosimp*)");
		expect(root.rules[1].name).to.deep.equal("(assert)");
		expect(root.rules[2].name).to.deep.equal("(grind)");
		expect(root.rules[3].name).to.deep.equal(`(case "x!1 > 0")`);
		expect(root.rules[3].rules.length).to.equal(2);
		expect(root.rules[3].rules[0].name).to.deep.equal("(1)");
		expect(root.rules[3].rules[0].rules.length).to.equal(0); // postpone is never added to the proof
		expect(root.rules[3].rules[1].name).to.deep.equal("(2)");
		expect(root.rules[3].rules[1].rules.length).to.equal(1);
		expect(root.rules[3].rules[1].rules[0].name).to.deep.equal("(grind)"); // this will close branch 2

		const activeNode: ProofNodeX = proofExplorer.getActiveNode();
		// console.dir(activeNode);
		expect(activeNode.name).to.deep.equal("(1)");
		expect(proofExplorer.ghostNodeIsActive()).to.equal(true);
	});
	it(`can perform (undo)`, async () => {
		request.cmd = "(undo)";
		const proofExplorer: PvsProofExplorer = server.getProofExplorer();
		await proofExplorer.proofCommandRequest(request);

		// the proof structure should be unchanged, and the active node should be `(case "x!1 > 0")`
		const root: ProofNodeX = proofExplorer.getProofX();
		// console.dir(root, { depth: null });
		expect(root.rules.length).to.equal(4);
		expect(root.rules[0].name).to.deep.equal("(skosimp*)");
		expect(root.rules[1].name).to.deep.equal("(assert)");
		expect(root.rules[2].name).to.deep.equal("(grind)");
		expect(root.rules[3].name).to.deep.equal(`(case "x!1 > 0")`);
		expect(root.rules[3].rules.length).to.equal(2);
		expect(root.rules[3].rules[0].name).to.deep.equal("(1)");
		expect(root.rules[3].rules[0].rules.length).to.equal(0); // postpone is never added to the proof
		expect(root.rules[3].rules[1].name).to.deep.equal("(2)");
		expect(root.rules[3].rules[1].rules.length).to.equal(1);
		expect(root.rules[3].rules[1].rules[0].name).to.deep.equal("(grind)"); // this will close branch 2

		const activeNode: ProofNodeX = proofExplorer.getActiveNode();
		// console.dir(activeNode);
		expect(activeNode.name).to.deep.equal(`(case "x!1 > 0")`);
		expect(proofExplorer.ghostNodeIsActive()).to.equal(false);
	}).timeout(6000);
	it(`can perform (undo undo)`, async () => {
		request.cmd = "(undo undo)";
		const proofExplorer: PvsProofExplorer = server.getProofExplorer();
		await proofExplorer.proofCommandRequest(request);

		// the proof structure should be unchanged, and the active node should be (1)
		let root: ProofNodeX = proofExplorer.getProofX();
		// console.dir(root, { depth: null });
		expect(root.rules.length).to.equal(4);
		expect(root.rules[0].name).to.deep.equal("(skosimp*)");
		expect(root.rules[1].name).to.deep.equal("(assert)");
		expect(root.rules[2].name).to.deep.equal("(grind)");
		expect(root.rules[3].name).to.deep.equal(`(case "x!1 > 0")`);
		expect(root.rules[3].rules.length).to.equal(2);
		expect(root.rules[3].rules[0].name).to.deep.equal("(1)");
		expect(root.rules[3].rules[0].rules.length).to.equal(0); // postpone is never added to the proof
		expect(root.rules[3].rules[1].name).to.deep.equal("(2)");
		expect(root.rules[3].rules[1].rules.length).to.equal(1);
		expect(root.rules[3].rules[1].rules[0].name).to.deep.equal("(grind)"); // this will close branch 2

		const activeNode: ProofNodeX = proofExplorer.getActiveNode();
		// console.dir(activeNode);
		expect(proofExplorer.ghostNodeIsActive()).to.equal(true);
		expect(activeNode.name).to.deep.equal(`ghost`);

		// can attach a node at the ghost position
		request.cmd = "(all-typepreds)";
		await proofExplorer.proofCommandRequest(request);
		// the proof structure should be unchanged, and the active node should be (1)
		root = proofExplorer.getProofX();
		expect(root.rules[3].rules[0].name).to.deep.equal("(1)");
		expect(root.rules[3].rules[0].rules.length).to.equal(1); // postpone is never added to the proof
		expect(root.rules[3].rules[0].rules[0].name).to.deep.equal("(all-typepreds)");
		expect(proofExplorer.ghostNodeIsActive()).to.equal(true);
		expect(activeNode.name).to.deep.equal(`ghost`);
	}).timeout(6000);
	it(`can automatically trim branches if proof structure has changed`, async () => {
		request.cmd = `(undo)(case "x!1 > 2")`; // the second command will generate two proof branches, so all-typepreds should be trimmed
		const proofExplorer: PvsProofExplorer = server.getProofExplorer();
		await proofExplorer.proofCommandRequest(request);

		// the proof structure should be unchanged, and the active node should be (1)
		let root: ProofNodeX = proofExplorer.getProofX();
		// console.dir(root, { depth: null });
		expect(root.rules.length).to.equal(4);
		expect(root.rules[0].name).to.deep.equal("(skosimp*)");
		expect(root.rules[1].name).to.deep.equal("(assert)");
		expect(root.rules[2].name).to.deep.equal("(grind)");
		expect(root.rules[3].name).to.deep.equal(`(case "x!1 > 0")`);
		expect(root.rules[3].rules.length).to.equal(2);
		expect(root.rules[3].rules[0].name).to.deep.equal("(1)");
		expect(root.rules[3].rules[0].rules.length).to.equal(1); // the first proof branch is automatically proved by pvs
		expect(root.rules[3].rules[0].rules[0].name).to.deep.equal(`(case "x!1 > 2")`);
		expect(root.rules[3].rules[1].name).to.deep.equal("(2)");
		expect(root.rules[3].rules[1].rules.length).to.equal(1);
		expect(root.rules[3].rules[1].rules[0].name).to.deep.equal("(grind)"); // this will close branch 2

		let activeNode: ProofNodeX = proofExplorer.getActiveNode();
		expect(proofExplorer.ghostNodeIsActive()).to.equal(true);
		expect(activeNode.name).to.deep.equal(`(1.1)`);

		// this sequence will trim again the node
		request.cmd = `(undo)(case "x!1 > 1")`;
		await proofExplorer.proofCommandRequest(request);

		// the proof structure should be unchanged, and the active node should be (1)
		root = proofExplorer.getProofX();
		// console.dir(root, { depth: null });
		expect(root.rules.length).to.equal(4);
		expect(root.rules[0].name).to.deep.equal("(skosimp*)");
		expect(root.rules[1].name).to.deep.equal("(assert)");
		expect(root.rules[2].name).to.deep.equal("(grind)");
		expect(root.rules[3].name).to.deep.equal(`(case "x!1 > 0")`);
		expect(root.rules[3].rules.length).to.equal(2);
		expect(root.rules[3].rules[0].name).to.deep.equal("(1)");
		expect(root.rules[3].rules[0].rules.length).to.equal(1); // the first proof branch is automatically proved by pvs
		expect(root.rules[3].rules[0].rules[0].name).to.deep.equal(`(case "x!1 > 1")`); // <<<<<
		expect(root.rules[3].rules[1].name).to.deep.equal("(2)");
		expect(root.rules[3].rules[1].rules.length).to.equal(1);
		expect(root.rules[3].rules[1].rules[0].name).to.deep.equal("(grind)"); // this will close branch 2

		activeNode = proofExplorer.getActiveNode();
		expect(proofExplorer.ghostNodeIsActive()).to.equal(true);
		expect(activeNode.name).to.deep.equal(`(1.1)`);
	}).timeout(6000);

	//-----
	it(`can start another proof when a prover session has already started`, async () => {
		await server.getPvsProxy().proofCommand({ cmd: "(quit)" });
		const response: PvsResponse = await server.proveFormula(request5);

		// console.log(response);
		const proofExplorer: PvsProofExplorer = server.getProofExplorer();

		const result: SequentDescriptor[] = response.result;
		// load initial sequent in proof explorer
		proofExplorer.loadInitialSequent(result[0]);

		let root: ProofNodeX = proofExplorer.getProofX();
		// console.dir(root);

		const success: boolean = await proofExplorer.openProofRequest({
			contextFolder: request5.contextFolder,
			fileName: request5.fileName,
			fileExtension: ".jprf"
		}, request5);
		expect(success).to.equal(true);

		root = proofExplorer.getProofX();
		// console.dir(root);

		expect(root.name).to.deep.equal(request5.formulaName);
		expect(root.rules.length).to.equal(2);
		expect(root.rules[0].name).to.deep.equal("(skosimp*)");
		expect(root.rules[1].name).to.deep.equal("(grind)");

		const initial_tooltip: string = proofExplorer.getTooltip({ selected: root.rules[0] });
		expect(initial_tooltip).to.contain(request5.formulaName);

	});

	it(`can delete a proof and display the correct active node`, async () => {
		const proofExplorer: PvsProofExplorer = server.getProofExplorer();
		let root: ProofNodeX = proofExplorer.getProofX();
		expect(proofExplorer.isActive({ id: root.id, name: root.name }));

		proofExplorer.deleteNodeX({ action: "delete-node", selected: { id: root.id, name: root.name }});

		root = proofExplorer.getProofX();
		expect(root.name).to.deep.equal(request5.formulaName);
		expect(root.rules.length).to.equal(0);
		expect(proofExplorer.ghostNodeIsActive()).to.equal(true);
		expect(proofExplorer.isPending({ id: root.id, name: root.name }));
	});

	it(`can automatically trim branches at the beginning of a proof, if proof structure has changed`, async () => {
		await server.getPvsProxy().proofCommand({ cmd: "(quit)" });
		await server.proveFormulaRequest(request2);
		const proofExplorer: PvsProofExplorer = server.getProofExplorer();

		const success: boolean = await proofExplorer.openProofRequest({
			contextFolder: request2.contextFolder,
			fileName: request2.fileName,
			fileExtension: ".jprf"
		}, request2);
		expect(success).to.equal(true);

		let root: ProofNodeX = proofExplorer.getProofX();
		expect(root.name).to.deep.equal(request2.formulaName);
		expect(root.rules[0].name).to.deep.equal("(skosimp*)");

		expect(proofExplorer.ghostNodeIsActive()).not.to.equal(true);
		let activeNode: ProofNodeX = proofExplorer.getActiveNode();
		expect(activeNode.name).to.deep.equal(`(skosimp*)`);

		request2.cmd = "(grind)";
		await proofExplorer.proofCommandRequest(request2);

		expect(proofExplorer.ghostNodeIsActive()).to.equal(true);

		root = proofExplorer.getProofX();
		expect(root.rules[0].name).to.deep.equal("(grind)");
		expect(root.rules[0].rules[0].name).to.deep.equal("(1)");
		activeNode = proofExplorer.getActiveNode();
		expect(activeNode.name).to.deep.equal(`(1)`);
	});

	it(`can trim branches with active nodes and correctly re-position the active node`, async () => {
		await server.getPvsProxy().proofCommand({ cmd: "(quit)" });
		await server.proveFormulaRequest(request2a);
		const proofExplorer: PvsProofExplorer = server.getProofExplorer();

		const success: boolean = await proofExplorer.openProofRequest({
			contextFolder: request2a.contextFolder,
			fileName: request2a.fileName,
			fileExtension: ".jprf"
		}, request2a);
		expect(success).to.equal(true);

		let root: ProofNodeX = proofExplorer.getProofX();
		expect(root.name).to.deep.equal(request2a.formulaName);
		expect(root.rules[0].name).to.deep.equal("(grind)");

		expect(proofExplorer.ghostNodeIsActive()).not.to.equal(true);
		let activeNode: ProofNodeX = proofExplorer.getActiveNode();
		expect(activeNode.name).to.deep.equal(`(grind)`);

		request2.cmd = "(skosimp*)";
		await proofExplorer.proofCommandRequest(request2);

		expect(proofExplorer.ghostNodeIsActive()).to.equal(false);

		root = proofExplorer.getProofX();
		expect(root.rules.length).to.equal(2);
		expect(root.rules[0].name).to.deep.equal(`(skosimp*)`);
		expect(root.rules[1].name).to.deep.equal(`(grind)`);
		activeNode = proofExplorer.getActiveNode();
		expect(activeNode.name).to.deep.equal(`(grind)`);

		proofExplorer.trimNodeX({ action: "trim-node", selected: { id: root.rules[0].id, name: root.rules[0].name }});
		root = proofExplorer.getProofX();
		// console.dir(root, { depth: null });
		expect(root.rules.length).to.equal(1);
		expect(proofExplorer.ghostNodeIsActive()).to.equal(true);
		activeNode = proofExplorer.getActiveNode();
		expect(activeNode.name).to.deep.equal(`ghost`);

	});

	it(`can save current proof`, async () => {
		await server.getPvsProxy().quitProof();

		const formula: PvsFormula = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			theoryName: "sq",
			formulaName: "sq_neg"
		};

		await server.proveFormulaRequest(formula, { autorun: true });
		const res: { success: boolean, msg?: string } = await server.getProofExplorer().quitProofAndSave();
		// console.dir(res);
		expect(res.success).to.equal(true);
	});

	it(`can start a proof, then interrupt, quit and save current proof`, async () => {
		let proverStatus: PvsResult = await server.getPvsProxy().pvsRequest('prover-status'); // await pvsProxy.getProverStatus();		
		// console.dir(proverStatus);
		if (proverStatus && proverStatus.result !== "inactive") {
			await server.getPvsProxy().proofCommand({ cmd: 'quit' });
		}

		const formula: PvsFormula = {
			contextFolder: sandboxExamples,
			fileExtension: ".pvs",
			fileName: "sq",
			theoryName: "sq",
			formulaName: "sq_neg"
		};

		await server.proveFormulaRequest(formula);
		await server.getPvsProxy().proofCommand({ cmd: "skosimp*" });
		await server.getPvsProxy().interruptProver();
		const res: { success: boolean, msg?: string } = await server.getProofExplorer().quitProofAndSave();
		// console.dir(res);
		expect(res.success).to.equal(true);

		// try to start other proofs, to double check that everything is still working fine
		await server.getPvsProxy().proofCommand({ cmd: 'quit' });
		const pvsResponse: PvsResponse = await server.proveFormula(request);
		expect(pvsResponse.error).to.be.undefined;
		expect(pvsResponse.result).not.to.be.undefined;

		await server.proveFormulaRequest(request5);
		const proofExplorer: PvsProofExplorer = server.getProofExplorer();
		const success: boolean = await proofExplorer.openProofRequest({
			contextFolder: request5.contextFolder,
			fileName: request5.fileName,
			fileExtension: ".jprf"
		}, request5);
		expect(success).to.equal(true);
	});

	// fit(`can prove omega_2D_continuous without triggering stack overflow`, async () => {
	// 	let proverStatus: PvsResult = await server.getPvsProxy().pvsRequest('prover-status'); // await pvsProxy.getProverStatus();		
	// 	// console.dir(proverStatus);
	// 	if (proverStatus && proverStatus.result !== "inactive") {
	// 		await server.getPvsProxy().proofCommand({ cmd: 'quit' });
	// 	}

    //     const formula: PvsFormula = {
    //         contextFolder: path.join(__dirname, "nasalib/ACCoRD"),
    //         fileExtension: ".pvs",
    //         fileName: "omega_2D",
    //         theoryName: "omega_2D",
    //         formulaName: "omega_2D_continuous"
    //     };

	// 	await server.proveFormulaRequest(formula, { autorun: true, externalServer: true });
	// 	await server.
	// 	// const res: { success: boolean, msg?: string } = await server.getProofExplorer().quitProofAndSave();
	// 	// console.dir(res);
	// 	// expect(res.success).toBeTrue();
	// }, 80000);
});

