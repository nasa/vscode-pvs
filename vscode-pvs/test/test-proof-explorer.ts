import * as fsUtils from "../server/src/common/fsUtils";
import { configFile, label, sandboxExamples } from './test.utils';
import * as path from 'path';
import { PvsProofExplorer } from "../server/src/providers/pvsProofExplorer";
import { ProofNodeX, ProofStatus, PvsFormula, PvsProofCommand, /*, SequentDescriptor*/ 
PvsProofState} from "../server/src/common/serverInterface";
import { PvsLanguageServer, PvsServerDescriptor } from "../server/src/pvsLanguageServer";
import { PvsResponse, PvsResult } from "../server/src/common/pvs-gui";
import { expect } from 'chai';
import { getProofId, PvsProxy } from "../server/src/pvsProxy";
import { isQEDProofState } from "../server/src/common/languageUtils";

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
        const desc: PvsServerDescriptor = {
            pvsPath,
            pvsLibraryPath: '',
            contextFolder: '~',
            externalServer: true,
            webSocketPort: 23456,
            remote: {}
        };
        await server.startPvsServer(desc, { verbose: false, debugMode: false });

        console.log("\n----------------------");
        console.log("test-proof-explorer");
        console.log("----------------------");
    });
    after(async () => {
        await server.getPvsProxy().killPvsServer();
        await server.getPvsProxy().killPvsProxy();
        // delete pvsbin files and .pvscontext
        await fsUtils.cleanBin(contextFolder);
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

    const helloWorldFolder: string = path.join(__dirname, "helloworld");
    const foo: PvsProofCommand = {
        contextFolder: helloWorldFolder,
        fileExtension: '.pvs',
        fileName: 'helloworld',
        formulaName: 'foo',
        theoryName: 'helloworld',
        cmd: ""
    }

    // note: these tests need to be performed together -- be mindful when skipping them, because this may cause other tests down the line to fail
    it(`can step single proof commands`, async () => {
        // label(`can step single proof commands`);

        await server.proveFormulaRequest(request);
        const proofExplorer: PvsProofExplorer = server.getProofExplorer();
        let root: ProofNodeX = proofExplorer.getProofX();
        // console.dir(root, { depth: null });
        expect(root.name).to.deep.equal(request.formulaName);
        expect(root.rules.length).to.equal(0);

        request.cmd = "(skosimp*)";
        await proofExplorer.proofCommandRequest(request);
        root = proofExplorer.getProofX();
        // console.dir(root);
        expect(root.name).to.deep.equal(request.formulaName);
        expect(root.rules[0].name.toLowerCase()).to.deep.equal("(skosimp*)");
        expect(root.rules[0].type).to.deep.equal("proof-command");
        expect(root.rules[0].parent).to.deep.equal(root.id);
    });

    // at this point, the proof should contain one command (skosimp*), try to send a new command that can will be added to the proof tree
    it(`can append valid proof commands`, async () => {
        // label(`can append valid proof commands`);

        request.cmd = "(assert)";
        const proofExplorer: PvsProofExplorer = server.getProofExplorer();

        await proofExplorer.proofCommandRequest(request);
        const root: ProofNodeX = proofExplorer.getProofX()

        // console.dir(root);
        expect(root.name).to.deep.equal(request.formulaName);
        expect(root.rules[0].name.toLowerCase()).to.deep.equal("(skosimp*)");
        expect(root.rules[0].type).to.deep.equal("proof-command");
        expect(root.rules[0].parent).to.deep.equal(root.id);
        expect(root.rules[1].name.toLowerCase()).to.deep.equal("(assert)");
        expect(root.rules[1].type).to.deep.equal("proof-command");
        expect(root.rules[1].parent).to.deep.equal(root.id);
    });

    // at this point, the proof should contain two commands (skosimp*)(assert), try to send a new command that won't be added to the proof tree, e.g., because it's not applicable (lift-if)
    it(`understands PVS reporting no change in the proof tree`, async () => {
        // label(`understands PVS reporting no change in the proof tree`);

        request.cmd = "(lift-if)";
        const proofExplorer: PvsProofExplorer = server.getProofExplorer();

        await proofExplorer.proofCommandRequest(request);
        const root: ProofNodeX = proofExplorer.getProofX()

        // console.dir(root);
        expect(root.name).to.deep.equal(request.formulaName);
        expect(root.rules[0].name.toLowerCase()).to.deep.equal("(skosimp*)");
        expect(root.rules[0].type).to.deep.equal("proof-command");
        expect(root.rules[0].parent).to.deep.equal(root.id);
        expect(root.rules[1].name.toLowerCase()).to.deep.equal("(assert)");
        expect(root.rules[1].type).to.deep.equal("proof-command");
        expect(root.rules[1].parent).to.deep.equal(root.id);
        expect(root.rules.length).to.equal(2);
    });

    // at this point, the proof contains two commands (skosimp*)(assert), try to send a new command that won't be added to the proof tree, e.g., because it's not applicable (lift-if)
    it(`understands PVS reporting ill formed rule`, async () => {
        // label(`understands PVS reporting ill formed rule`);

        request.cmd = "(dada)";
        const proofExplorer: PvsProofExplorer = server.getProofExplorer();

        await proofExplorer.proofCommandRequest(request);
        const root: ProofNodeX = proofExplorer.getProofX()

        // console.dir(root);
        expect(root.name).to.deep.equal(request.formulaName);
        expect(root.rules[0].name.toLowerCase()).to.deep.equal("(skosimp*)");
        expect(root.rules[0].type).to.deep.equal("proof-command");
        expect(root.rules[0].parent).to.deep.equal(root.id);
        expect(root.rules[1].name.toLowerCase()).to.deep.equal("(assert)");
        expect(root.rules[1].type).to.deep.equal("proof-command");
        expect(root.rules[1].parent).to.deep.equal(root.id);
        expect(root.rules.length).to.equal(2);
    });
    
    it(`can step a series of proof commands`, async () => {
        // label(`can step a series of proof commands`);

        request.cmd = `(assert)(grind)(case "x!1 > 0")(postpone)(grind)`;
        const proofExplorer: PvsProofExplorer = server.getProofExplorer();

        await proofExplorer.proofCommandRequest(request);
        const root: ProofNodeX = proofExplorer.getProofX();
        // console.dir(root, { depth: null });
        expect(root.rules.length).to.equal(4);
        expect(root.rules[0].name.toLowerCase()).to.deep.equal("(skosimp*)");
        expect(root.rules[1].name.toLowerCase()).to.deep.equal("(assert)");
        expect(root.rules[2].name.toLowerCase()).to.deep.equal("(grind)");
        expect(root.rules[3].name.toLowerCase()).to.deep.equal(`(case "x!1 > 0")`);
        expect(root.rules[3].rules.length).to.equal(2);
        expect(root.rules[3].rules[0].name).to.deep.equal("(1)");
        expect(root.rules[3].rules[0].rules.length).to.equal(0); // postpone is never added to the proof
        expect(root.rules[3].rules[1].name).to.deep.equal("(2)");
        expect(root.rules[3].rules[1].rules.length).to.equal(1);
        expect(root.rules[3].rules[1].rules[0].name.toLowerCase()).to.deep.equal("(grind)"); // this will close branch 2

        const activeNode: ProofNodeX = proofExplorer.getActiveNode();
        // console.dir(activeNode);
        expect(activeNode.name).to.deep.equal("(1)");
        expect(proofExplorer.ghostNodeIsActive()).to.equal(true);
    });

    it(`can perform (undo)`, async () => {
        // label(`can perform (undo)`);
        request.cmd = "(undo)";
        const proofExplorer: PvsProofExplorer = server.getProofExplorer();
        await proofExplorer.proofCommandRequest(request);

        // the proof structure should be unchanged, and the active node should be `(case "x!1 > 0")`
        const root: ProofNodeX = proofExplorer.getProofX();
        // console.dir(root, { depth: null });
        expect(root.rules.length).to.equal(4);
        expect(root.rules[0].name.toLowerCase()).to.deep.equal("(skosimp*)");
        expect(root.rules[1].name.toLowerCase()).to.deep.equal("(assert)");
        expect(root.rules[2].name.toLowerCase()).to.deep.equal("(grind)");
        expect(root.rules[3].name.toLowerCase()).to.deep.equal(`(case "x!1 > 0")`);
        expect(root.rules[3].rules.length).to.equal(2);
        expect(root.rules[3].rules[0].name).to.deep.equal("(1)");
        expect(root.rules[3].rules[0].rules.length).to.equal(0); // postpone is never added to the proof
        expect(root.rules[3].rules[1].name).to.deep.equal("(2)");
        expect(root.rules[3].rules[1].rules.length).to.equal(1);
        expect(root.rules[3].rules[1].rules[0].name.toLowerCase()).to.deep.equal("(grind)"); // this will close branch 2

        const activeNode: ProofNodeX = proofExplorer.getActiveNode();
        // console.dir(activeNode);
        expect(activeNode.name.toLowerCase()).to.deep.equal(`(case "x!1 > 0")`);
        expect(proofExplorer.ghostNodeIsActive()).to.equal(false);
    }).timeout(6000);

    it(`can perform (undo undo)`, async () => {
        // label(`can perform (undo undo)`);
        request.cmd = "(undo undo)";
        const proofExplorer: PvsProofExplorer = server.getProofExplorer();
        await proofExplorer.proofCommandRequest(request);

        // the proof structure should be unchanged, and the active node should be (1)
        let root: ProofNodeX = proofExplorer.getProofX();
        // console.dir(root, { depth: null });
        expect(root.rules.length).to.equal(4);
        expect(root.rules[0].name.toLowerCase()).to.deep.equal("(skosimp*)");
        expect(root.rules[1].name.toLowerCase()).to.deep.equal("(assert)");
        expect(root.rules[2].name.toLowerCase()).to.deep.equal("(grind)");
        expect(root.rules[3].name.toLowerCase()).to.deep.equal(`(case "x!1 > 0")`);
        expect(root.rules[3].rules.length).to.equal(2);
        expect(root.rules[3].rules[0].name.toLowerCase()).to.deep.equal("(1)");
        expect(root.rules[3].rules[0].rules.length).to.equal(0); // postpone is never added to the proof
        expect(root.rules[3].rules[1].name.toLowerCase()).to.deep.equal("(2)");
        expect(root.rules[3].rules[1].rules.length).to.equal(1);
        expect(root.rules[3].rules[1].rules[0].name.toLowerCase()).to.deep.equal("(grind)"); // this will close branch 2

        const activeNode: ProofNodeX = proofExplorer.getActiveNode();
        // console.dir(activeNode);
        expect(proofExplorer.ghostNodeIsActive()).to.equal(true);
        expect(activeNode.name).to.deep.equal(`ghost`);

        // can attach a node at the ghost position
        request.cmd = "(all-typepreds)";
        await proofExplorer.proofCommandRequest(request);
        // the proof structure should be unchanged, and the active node should be (1)
        root = proofExplorer.getProofX();
        expect(root.rules[3].rules[0].name.toLowerCase()).to.deep.equal("(1)");
        expect(root.rules[3].rules[0].rules.length).to.equal(1); // postpone is never added to the proof
        expect(root.rules[3].rules[0].rules[0].name.toLowerCase()).to.deep.equal("(all-typepreds)");
        expect(proofExplorer.ghostNodeIsActive()).to.equal(true);
        expect(activeNode.name).to.deep.equal(`ghost`);
    }).timeout(6000);

    it(`can automatically trim branches if proof structure has changed`, async () => {
        // label(`can automatically trim branches if proof structure has changed`);
        request.cmd = `(undo)(case "x!1 > 2")`; // the second command will generate two proof branches, so all-typepreds should be trimmed
        const proofExplorer: PvsProofExplorer = server.getProofExplorer();
        await proofExplorer.proofCommandRequest(request);

        // the proof structure should be unchanged, and the active node should be (1)
        let root: ProofNodeX = proofExplorer.getProofX();
        // console.dir(root, { depth: null });
        expect(root.rules.length).to.equal(4);
        expect(root.rules[0].name.toLowerCase()).to.deep.equal("(skosimp*)");
        expect(root.rules[1].name.toLowerCase()).to.deep.equal("(assert)");
        expect(root.rules[2].name.toLowerCase()).to.deep.equal("(grind)");
        expect(root.rules[3].name.toLowerCase()).to.deep.equal(`(case "x!1 > 0")`);
        expect(root.rules[3].rules.length).to.equal(2);
        expect(root.rules[3].rules[0].name.toLowerCase()).to.deep.equal("(1)");
        expect(root.rules[3].rules[0].rules.length).to.equal(1); // the first proof branch is automatically proved by pvs
        expect(root.rules[3].rules[0].rules[0].name.toLowerCase()).to.deep.equal(`(case "x!1 > 2")`);
        expect(root.rules[3].rules[1].name.toLowerCase()).to.deep.equal("(2)");
        expect(root.rules[3].rules[1].rules.length).to.equal(1);
        expect(root.rules[3].rules[1].rules[0].name.toLowerCase()).to.deep.equal("(grind)"); // this will close branch 2

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
        expect(root.rules[0].name.toLowerCase()).to.deep.equal("(skosimp*)");
        expect(root.rules[1].name.toLowerCase()).to.deep.equal("(assert)");
        expect(root.rules[2].name.toLowerCase()).to.deep.equal("(grind)");
        expect(root.rules[3].name.toLowerCase()).to.deep.equal(`(case "x!1 > 0")`);
        expect(root.rules[3].rules.length).to.equal(2);
        expect(root.rules[3].rules[0].name.toLowerCase()).to.deep.equal("(1)");
        expect(root.rules[3].rules[0].rules.length).to.equal(1); // the first proof branch is automatically proved by pvs
        expect(root.rules[3].rules[0].rules[0].name.toLowerCase()).to.deep.equal(`(case "x!1 > 1")`); // <<<<<
        expect(root.rules[3].rules[1].name.toLowerCase()).to.deep.equal("(2)");
        expect(root.rules[3].rules[1].rules.length).to.equal(1);
        expect(root.rules[3].rules[1].rules[0].name.toLowerCase()).to.deep.equal("(grind)"); // this will close branch 2

        activeNode = proofExplorer.getActiveNode();
        expect(proofExplorer.ghostNodeIsActive()).to.equal(true);
        expect(activeNode.name).to.deep.equal(`(1.1)`);
    }).timeout(6000);

    //-----
    it(`can start another proof when a prover session has already started`, async () => {
        // label(`can start another proof when a prover session has already started`);
        const response: PvsResponse | null = await server.proveFormula(request5);

        // console.log(response);
        const proofExplorer: PvsProofExplorer = server.getProofExplorer();

        const result/*: SequentDescriptor[]*/ = response?.result;
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
        expect(root.rules[0].name.toLowerCase()).to.deep.equal("(skosimp*)");
        expect(root.rules[1].name.toLowerCase()).to.deep.equal("(grind)");

        // const initial_tooltip: string = proofExplorer.getTooltip({ selected: root.rules[0] });
        // expect(initial_tooltip).to.contain(request5.formulaName);

    });

    it(`can delete a proof and display the correct active node`, async () => {
        // label(`can delete a proof and display the correct active node`);
        const proofExplorer: PvsProofExplorer = server.getProofExplorer();
        let root: ProofNodeX = proofExplorer.getProofX();
        expect(proofExplorer.isActive({ id: root.id, name: root.name }));

        proofExplorer.deleteNodeX({ action: "delete-node", selected: { id: root.id, name: root.name } });

        root = proofExplorer.getProofX();
        expect(root.name).to.deep.equal(request5.formulaName);
        expect(root.rules.length).to.equal(0);
        expect(proofExplorer.ghostNodeIsActive()).to.equal(true);
        expect(proofExplorer.isPending({ id: root.id, name: root.name }));
    });

    it(`can automatically trim branches at the beginning of a proof, if proof structure has changed`, async () => {
        // label(`can automatically trim branches at the beginning of a proof, if proof structure has changed`);
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
        expect(root.rules[0].name.toLowerCase()).to.deep.equal("(skosimp*)");

        expect(proofExplorer.ghostNodeIsActive()).not.to.equal(true);
        let activeNode: ProofNodeX = proofExplorer.getActiveNode();
        expect(activeNode.name).to.deep.equal(`(skosimp*)`);

        request2.cmd = "(grind)";
        await proofExplorer.proofCommandRequest(request2);

        expect(proofExplorer.ghostNodeIsActive()).to.equal(true);

        root = proofExplorer.getProofX();
        expect(root.rules[0].name.toLowerCase()).to.deep.equal("(grind)");
        expect(root.rules[0].rules[0].name.toLowerCase()).to.deep.equal("(1)");
        activeNode = proofExplorer.getActiveNode();
        expect(activeNode.name).to.deep.equal(`(1)`);
    });

    it(`can trim branches with active nodes and correctly re-position the active node`, async () => {
        // label(`can trim branches with active nodes and correctly re-position the active node`);
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
        expect(root.rules[0].name.toLowerCase()).to.deep.equal("(grind)");

        expect(proofExplorer.ghostNodeIsActive()).not.to.equal(true);
        let activeNode: ProofNodeX = proofExplorer.getActiveNode();
        expect(activeNode.name).to.deep.equal(`(grind)`);

        request2.cmd = "(skosimp*)";
        await proofExplorer.proofCommandRequest(request2);

        expect(proofExplorer.ghostNodeIsActive()).to.equal(false);

        root = proofExplorer.getProofX();
        expect(root.rules.length).to.equal(2);
        expect(root.rules[0].name.toLowerCase()).to.deep.equal(`(skosimp*)`);
        expect(root.rules[1].name.toLowerCase()).to.deep.equal(`(grind)`);
        activeNode = proofExplorer.getActiveNode();
        expect(activeNode.name.toLowerCase()).to.deep.equal(`(grind)`);

        proofExplorer.trimNodeX({ action: "trim-node", selected: { id: root.rules[0].id, name: root.rules[0].name } });
        root = proofExplorer.getProofX();
        // console.dir(root, { depth: null });
        expect(root.rules.length).to.equal(1);
        expect(proofExplorer.ghostNodeIsActive()).to.equal(true);
        activeNode = proofExplorer.getActiveNode();
        expect(activeNode.name).to.deep.equal(`ghost`);

    });

    it(`can save current proof`, async () => {
        // label(`can save current proof`);
        await server.getPvsProxy().quitAllProofs();

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
    }).timeout(4000);

    it(`can start a proof, then interrupt, quit and save current proof`, async () => {
        // label(`can start a proof, then interrupt, quit and save current proof`);
        let proverStatus: PvsResult = await server.getPvsProxy().pvsRequest('prover-status'); // await pvsProxy.getProverStatus();		
        // console.dir(proverStatus);
        if (proverStatus && proverStatus.result !== "inactive") {
            await server.getPvsProxy().quitAllProofs();
        }

        const formula: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "sq",
            theoryName: "sq",
            formulaName: "sq_neg"
        };

        await server.proveFormulaRequest(formula);
        const proofExplorer1: PvsProofExplorer = server.getProofExplorer();
        await server.getPvsProxy().proofCommand({ proofId: proofExplorer1.getProofId(), cmd: "skosimp*" });
        await server.getPvsProxy().interruptProver(proofExplorer1.getProofId());
        const res: { success: boolean, msg?: string } = await server.getProofExplorer().quitProofAndSave();
        // console.dir(res);
        expect(res.success).to.equal(true);

        // try to start other proofs, to double check that everything is still working fine
        await server.getPvsProxy().proofCommand({ proofId: proofExplorer1.getProofId(), cmd: 'quit' });
        const pvsResponse: PvsResponse | null = await server.proveFormula(request);
        expect(pvsResponse).not.to.be.null;
        expect(pvsResponse?.error).to.be.undefined;
        expect(pvsResponse?.result).not.to.be.undefined;

        await server.proveFormulaRequest(request5);
        const proofExplorer: PvsProofExplorer = server.getProofExplorer();
        const success: boolean = await proofExplorer.openProofRequest({
            contextFolder: request5.contextFolder,
            fileName: request5.fileName,
            fileExtension: ".jprf"
        }, request5);
        expect(success).to.equal(true);
    });

    it(`can prove omega_2D_continuous without triggering stack overflow`, async () => {
        // label(`can prove omega_2D_continuous without triggering stack overflow`);
    	let proverStatus: PvsResult = await server.getPvsProxy().pvsRequest('prover-status'); // await pvsProxy.getProverStatus();		
    	// console.dir(proverStatus);
        const proofExplorer: PvsProofExplorer = server.getProofExplorer();

    	if (proverStatus && proverStatus.result !== "inactive") {
            await server.getPvsProxy().proofCommand({ proofId: proofExplorer.getProofId(), cmd: "quit" });
    	}

        const formula: PvsFormula = {
            contextFolder: path.join(__dirname, "nasalib/ACCoRD"),
            fileExtension: ".pvs",
            fileName: "omega_2D",
            theoryName: "omega_2D",
            formulaName: "omega_2D_continuous"
        };

    	await server.proveFormulaRequest(formula, { autorun: true, externalServer: true });
    	const res: { success: boolean, msg?: string } = await server.getProofExplorer().quitProofAndSave();
    	// console.dir(res);
    	expect(res.success).to.be.true;
    }).timeout(80000);

    it(`can handle trivial proofs`, async () => {
        // label(`can handle trivial proofs`);

        let proverStatus: PvsResult = await server.getPvsProxy().pvsRequest('prover-status'); // await pvsProxy.getProverStatus();		
        // console.dir(proverStatus);
        if (proverStatus && proverStatus.result !== "inactive") {
            await server.getPvsProxy().quitAllProofs();
        }
        await server.proveFormulaRequest(foo);
        const proofExplorer: PvsProofExplorer = server.getProofExplorer();
        const fmla: PvsFormula = proofExplorer.getFormula();
        // console.dir(fmla, { depth: null});
        expect(fmla).not.to.be.undefined;
        const node: ProofNodeX = proofExplorer.getActiveNode();
        // console.dir(node, { depth: null});
        const status: ProofStatus = proofExplorer.getProofStatus();
        // console.dir(status, { depth: null});
        expect(node).not.to.be.undefined;
    }).timeout(80000);

    it(`can handle corner cases with (propax)`, async () => {
        const pvsProxy: PvsProxy = server.getPvsProxy();
        let proverStatus: PvsResult = await pvsProxy?.pvsRequest('prover-status'); // await pvsProxy.getProverStatus();		
        // quit all proofs, in case some were still active
        if (proverStatus && proverStatus.result !== "inactive") {
            await pvsProxy?.quitAllProofs();
        }

        const formula: PvsFormula = {
            contextFolder: path.join(__dirname, "nasalib/ACCoRD"),
            fileExtension: ".pvs",
            fileName: "cd2d_shape",
            theoryName: "cd2d_shape",
            formulaName: "cd2d_haz_correctness"
        };

        // start new proof
        await server.proveFormulaRequest(formula);
        const proofExplorer: PvsProofExplorer = server.getProofExplorer();

        const cmds: string[] = [
            `(SKEEP)`,
            `(BETA)`,
            `(LEMMA "cd2d_haz_soundness")`,
            `(INSTEEP)`,
            `(BETA)`,
            `(LEMMA "cd2d_haz_completeness")`,
            `(INSTEEP)`,
            `(BETA)`,
            `(SPLIT 1)`
            // (PROPAX)(PROPAX)
        ]
        for (let i = 0; i < cmds.length; i++) {
            console.log(`>>> Sending command ${cmds[i]}`);
            request.cmd = cmds[i];
            await proofExplorer.proofCommandRequest(request);
        }

        const root: ProofNodeX = proofExplorer.getProofX();
        expect(root.name).to.deep.equal(formula.formulaName);

        const status: ProofStatus = proofExplorer.getProofStatus();
        expect(status).to.be.deep.equal("proved");
        console.log({ status });

        console.dir({ rules: root.rules });
        expect(root.rules.length).to.be.equal(cmds.length);
        for (let i = 0; i < cmds.length; i++) {
            expect(root.rules[i].name).to.be.deep.equal(cmds[i])
        }
    }).timeout(80000);
});

