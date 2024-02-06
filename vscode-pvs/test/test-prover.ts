import * as fsUtils from "../server/src/common/fsUtils";
import * as test from "./test-constants";
import { PvsResponse, PvsResult } from "../server/src/common/pvs-gui";
import { PvsProxy } from '../server/src/pvsProxy'; // XmlRpcSystemMethods
import { configFile, sandboxExamples, safeSandboxExamples, radixExamples, helloworldExamples } from './test-utils';
import { PvsFormula, PvsProofCommand } from "../server/src/common/serverInterface";
import * as path from 'path';
import { execSync } from "child_process";
import { expect } from 'chai';

//----------------------------
//   Test cases for prover --- 	THESE TESTS REQUIRE NASALIB
//----------------------------
describe("pvs-prover", () => {
    let pvsProxy: PvsProxy | undefined = undefined;
    before(async () => {
        const config: string = await fsUtils.readFile(configFile);
        const content: { pvsPath: string } = JSON.parse(config);
        // log(content);
        const pvsPath: string = content.pvsPath;
        pvsProxy = new PvsProxy(pvsPath, { externalServer: true });
        await pvsProxy?.activate({ debugMode: false, showBanner: false }); // this will also start pvs-server

        // delete pvsbin files and .pvscontext
        await fsUtils.cleanBin(safeSandboxExamples);
        await fsUtils.cleanBin(sandboxExamples);
        await fsUtils.cleanBin(radixExamples);

        console.log("\n----------------------");
        console.log("test-prover");
        console.log("----------------------");
    });
    after(async () => {
        await pvsProxy?.killPvsServer();
        await pvsProxy?.killPvsProxy();
        // delete pvsbin files and .pvscontext
        await fsUtils.cleanBin(safeSandboxExamples);
        await fsUtils.cleanBin(sandboxExamples);
        await fsUtils.cleanBin(radixExamples);
    });

    // utility function, quits the prover if the prover status is active
    // const quitProverIfActive = async (): Promise<void> => {
    //     let proverStatus: PvsResult = await pvsProxy?.pvsRequest('prover-status'); // await pvsProxy?.getProverStatus();		
    //     // console.dir(proverStatus);
    //     if (proverStatus && proverStatus.result !== "inactive") {
    //         await pvsProxy?.proofCommand({ proofId: prfid, cmd: 'quit' });
    //     }

    //     // // quit prover if prover status is active
    //     // const proverStatus: PvsResult = await pvsProxy?.getProverStatus();
    //     // expect(proverStatus.result).not.to.be.undefined;
    //     // expect(proverStatus.error).to.be.undefined;
    //     // // console.log(proverStatus);
    //     // if (proverStatus && proverStatus.result !== "inactive") {
    //     // 	await pvsProxy?.proofCommand({ cmd: 'quit' });
    //     // }
    // }

    // @Sam: this first test fails intermittently.
    //       It seems that pvs returns a response before it's ready to accept a proof command (if a delay is introduced before sending the command request then the test succeeds)
    //       There is also a problem with the prover status: sometimes pvs returns the following error:
    //            'Value #<unknown object of type number 3 @\n' +
    //            '        #x107000000100223> is not of a type which can be encoded by encode-json.'
    //       This error usually occurs when the server is restarted, during the first prover session  
    it(`can start a proof and step proof commands`, async () => {
        const baseFolder: string = path.join(__dirname, "proof-explorer");
        const request: PvsProofCommand = {
            contextFolder: path.join(baseFolder, "foo"),
            fileExtension: '.pvs',
            fileName: 'foo',
            formulaName: 'foo1',
            theoryName: 'foo_th',
            cmd: "(skosimp*)"
        };

        let response: PvsResponse | undefined = await pvsProxy?.proveFormula(request);
        expect(response).not.to.be.undefined;

        expect(response?.result).not.to.be.undefined;
        expect(response?.error).to.be.undefined;

        let prfid: string = response?.result.id;

        response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: '(skosimp*)' });
        expect(response?.result).not.to.be.undefined;
        expect(response?.error).to.be.undefined;

        response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: '(quit)' });
        expect(response?.result).not.to.be.undefined;
        expect(response?.error).to.be.undefined;
    }).timeout(10000);

    it(`can handle unicode characters`, async () => {
        //await quitProverIfActive();

        const formula: PvsFormula = {
            contextFolder: helloworldExamples,
            fileExtension: ".pvs",
            fileName: "dummy",
            theoryName: "dummy",
            formulaName: "withUnicode"
        };

        let response: PvsResponse | undefined = await pvsProxy?.proveFormula(formula);
        expect(response).not.to.be.undefined;

        let prfid: string = response?.result.id;

        response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: '(expand "≥")' });
        expect(response?.error).to.be.undefined;
        expect(response?.result).not.to.be.undefined;

        response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: '(quit)' });
        expect(response?.result).not.to.be.undefined;
        expect(response?.error).to.be.undefined;
    }).timeout(10000);

    //----- the tests below this line are completed successfully
    it(`can start prover session`, async () => {

        const desc: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "alaris2lnewmodes",
            formulaName: "check_chev_fup_permission",
            theoryName: "alaris_th"
        };
        let response: PvsResponse | undefined = await pvsProxy?.proveFormula(desc);
        expect(response).not.to.be.undefined;
        expect(response?.result).not.to.be.undefined;
        expect(response?.error).to.be.undefined;

        let prfid: string = response?.result.id;

        response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: '(quit)' });
        expect(response?.result).not.to.be.undefined;
        expect(response?.error).to.be.undefined;
    }).timeout(60000);

    it(`can start interactive proof session when the formula has already been proved`, async () => {
        const desc: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "sq",
            formulaName: "sq_neg",
            theoryName: "sq"
        };

        let response: PvsResponse | undefined = await pvsProxy?.proveFormula(desc);
        expect(response).not.to.be.undefined;
        expect(response?.result.label).to.deep.equal(test.sq_neg_prove_formula.label);
        expect(response?.result.sequent.succedents).not.to.be.undefined;

        let prfid: string = response?.result.id;
        let prfid1: string = "";

        try {
            // send proof command (skosimp*)
            response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: '(skosimp*)' });
            expect(response?.result.sequent).not.to.be.undefined;
            expect(response?.result["prev-cmd"].toLowerCase()).to.deep.equal("(skosimp*)");

            // send proof command (expand "sq")
            response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: '(expand "sq")' });
            expect(response?.result.sequent).not.to.be.undefined;
            expect(response?.result["prev-cmd"].toLowerCase()).to.deep.equal('(expand "sq")');

            // send proof command (assert) to complete the proof
            response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: '(assert)' });
            expect(response?.result.status).to.equal('proved');
            expect(response?.result.commentary).to.contain('Q.E.D.');

            // try to re-start the proof
            response = await pvsProxy?.proveFormula(desc);
            expect(response?.result.label).to.deep.equal(test.sq_neg_prove_formula.label);
            expect(response?.result.sequent).not.to.be.undefined;
            prfid1 = response?.result.id;
            // send proof command (skosimp*)
            response = await pvsProxy?.proofCommand({ proofId: prfid1, cmd: '(skosimp*)' });
            // console.dir(response);
            expect(response?.result.sequent).not.to.be.undefined;
        }
        finally {
            // quit the proof attempt
            await pvsProxy?.proofCommand({ proofId: prfid1, cmd: '(quit)' });
        }

    }).timeout(10000);

    it(`can start a prover session and quit the prover session`, async () => {
        await pvsProxy?.pvsRequest('quit-all-proof-sessions');
        const desc: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "sq",
            formulaName: "sq_neg",
            theoryName: "sq"
        };
        let response: PvsResponse | undefined = await pvsProxy?.proveFormula(desc);
        expect(response).not.to.be.undefined;
        expect(response?.result.sequent).not.to.be.undefined;

        let prfid: string = response?.result.id;

        response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: '(quit)' });
        expect(response?.result.status).to.equal('quit');
    }).timeout(20000);

    it(`returns proverStatus = inactive when a prover session is not active`, async () => {
        await pvsProxy?.pvsRequest('quit-all-proof-sessions');
        const proverStatus: PvsResponse | undefined = await pvsProxy?.getProverStatus();
        expect(proverStatus).not.to.be.undefined;
        expect(proverStatus?.result).to.equal(null);
    }).timeout(4000);

    it(`returns proverStatus = active when a prover session is active`, async () => {
        //await quitProverIfActive();
        const desc: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "sq",
            formulaName: "sq_times",
            theoryName: "sq"
        };

        // start prover session
        let response: PvsResponse | undefined = await pvsProxy?.proveFormula(desc);
        expect(response).not.to.be.undefined;
        // check prover status
        let prfid = response?.result.id;

        const proverStatus: PvsResponse | undefined = await pvsProxy?.getProverStatus(prfid);
        expect(proverStatus).not.to.be.undefined;
        expect(proverStatus?.result[0].status).to.equal("active");

        // quit the proof attempt
        await pvsProxy?.proofCommand({ proofId: prfid, cmd: 'quit' });
    }).timeout(4000);

    it(`can invoke prove-formula on theories with parameters`, async () => {
        await pvsProxy?.pvsRequest('quit-all-proof-sessions');
        const desc: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "alaris2lnewmodes",
            formulaName: "check_chev_fup_permission",
            theoryName: "alaris_th" // pump_th exists, but check_chev_fup_permission is in alaris_th
        };
        let response: PvsResponse | undefined = await pvsProxy?.proveFormula(desc);
        expect(response).not.to.be.undefined;
        // console.dir(response);
        expect(response?.result).not.to.be.undefined;
        expect(response?.error).to.be.undefined;

        let prfid: string = response?.result.id;

        response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: 'quit' });
        expect(response?.result.status).to.equal('quit');
        const proverStatus: PvsResult | undefined = await pvsProxy?.getProverStatus(prfid);
        expect(proverStatus).not.to.be.undefined;
        if (proverStatus != undefined){
            expect(proverStatus.result).not.to.be.undefined;
            expect(proverStatus.error).to.be.undefined;
            if (proverStatus.result != undefined )
                expect(proverStatus.result[0].status).to.equal("quit");
        }
    }).timeout(60000);

    it(`returns proverStatus = inactive after quitting a prover session`, async () => {
        //await quitProverIfActive();
        const desc: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "sq",
            formulaName: "sq_times",
            theoryName: "sq"
        };

        // start prover session
        let response: PvsResponse | undefined = await pvsProxy?.proveFormula(desc);
        expect(response).not.to.be.undefined;
        let prfid: string = response?.result.id;
        // quit the proof attempt
        await pvsProxy?.proofCommand({ proofId: prfid, cmd: 'quit' });
        // check prover status
        const proverStatus: PvsResponse | undefined = await pvsProxy?.getProverStatus(prfid);
        expect(proverStatus?.result[0].status).to.equal("quit");
    }).timeout(4000);

    it(`can start prover sessions in theories with parameters`, async () => {
        //await quitProverIfActive();
        const desc: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "alaris2lnewmodes.pump",
            formulaName: "vtbi_over_rate_lemma",
            theoryName: "pump_th"
        };
        // await pvsProxy?.typecheckFile(desc); // typechecking, if needed, should be performed automatically by prove-formula
        let response: PvsResponse | undefined = await pvsProxy?.proveFormula(desc);
        expect(response).not.to.be.undefined;
        expect(response?.result).not.to.be.undefined;

        let prfid: string = response?.result.id;

        response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: 'quit' });
        const proverStatus: PvsResult | undefined = await pvsProxy?.getProverStatus(prfid);
        expect(proverStatus).not.to.be.undefined;
        if (proverStatus != undefined){
            expect(proverStatus.result).not.to.be.undefined;
            expect(proverStatus.error).to.be.undefined;
            if (proverStatus.result != undefined )
                expect(proverStatus.result[0].status).to.equal("quit");
        }
    }).timeout(10000);

    it(`reports typecheck error when the prove command is executed but the theory does not typecheck`, async () => {
        //await quitProverIfActive();
        const desc: PvsFormula = {
            contextFolder: radixExamples,
            fileExtension: ".pvs",
            fileName: "mergesort-test",
            formulaName: "merge_size",
            theoryName: "mergesort_1"
        };
        let response: PvsResponse | undefined = await pvsProxy?.proveFormula(desc);
        expect(response).not.to.be.undefined;
        expect(response?.result).to.be.undefined;
        expect(response?.error).not.to.be.undefined;
    }).timeout(10000);

    // the rationale for the following test case is to check that the following use case:
    // the user has defined formula l in file f1, and another formula with the same name l in file f2;
    // f1 typechecks correctly; f2 does not typecheck; the user tries to prove formula l in f2;
    // pvs-server should not start the proof and return a typecheck error
    it(`is able to distinguish theories with the same name that are stored in different files in the same context`, async () => {
        //await quitProverIfActive();
        // this version of the theory does not typecheck, so the prover should report error
        let desc: PvsFormula = {
            contextFolder: radixExamples,
            fileExtension: ".pvs",
            fileName: "mergesort-test",
            formulaName: "merge_size",
            theoryName: "mergesort"
        };
        let response: PvsResponse | undefined = await pvsProxy?.proveFormula(desc);
        expect(response).not.to.be.undefined;
        expect(response?.result).to.be.undefined;
        expect(response?.error).not.to.be.undefined;
        expect(response?.error.data.error_string).not.to.be.undefined;

        // let prfid: string = response?.result.id;
        // the following command should have no effect
        // response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: 'quit' });
        // expect(response?.result[0].commentary[0]).to.deep.equal("No change on: quit");
        // expect(response?.error).not.to.be.undefined;

        // this other version of the theory, on the other hand, typechecks correctly
        // pvs should report a typecheck error because two theories with the same name are in the same context folder
        desc = {
            contextFolder: radixExamples,
            fileExtension: ".pvs",
            fileName: "mergesort",
            formulaName: "merge_size",
            theoryName: "mergesort"
        };
        response = await pvsProxy?.proveFormula(desc);
        // console.dir(response);
        expect(response?.result).to.be.undefined;
        expect(response?.error).not.to.be.undefined;
        expect(response?.error.data.error_string).to.contain("has been declared previously");
    }).timeout(10000);

    it(`reports error when trying to prove a theory that does not exist`, async () => {
        //await quitProverIfActive();
        let desc: PvsFormula = {
            contextFolder: radixExamples,
            fileExtension: ".pvs",
            fileName: "mergesort-test",
            formulaName: "merge_size",
            theoryName: "mergesort_2"
        };
        let response: PvsResponse | undefined = await pvsProxy?.proveFormula(desc);
        expect(response).not.to.be.undefined;
        expect(response?.result).to.be.undefined;
        expect(response?.error).not.to.be.undefined;
    }).timeout(10000);

    it(`reports error when the prove command is executed but the formula does not exist`, async () => {
        //await quitProverIfActive();
        const desc: PvsFormula = {
            contextFolder: radixExamples,
            fileExtension: ".pvs",
            fileName: "mergesort-test",
            formulaName: "mm",
            theoryName: "mergesort_1"
        };
        let response: PvsResponse | undefined = await pvsProxy?.proveFormula(desc);
        expect(response).not.to.be.undefined;
        expect(response?.result).to.be.undefined;
        expect(response?.error).not.to.be.undefined;
    }).timeout(10000);

    it(`is robust to mistyped / malformed prover commands`, async () => {
        //await quitProverIfActive();
        const desc: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "sq",
            formulaName: "sq_neg",
            theoryName: "sq"
        };

        let response: PvsResponse | undefined = await pvsProxy?.proveFormula(desc);
        expect(response).not.to.be.undefined;
        //console.dir(response);
        expect(response?.result.label).to.deep.equal(test.sq_neg_prove_formula.label);
        expect(response?.result.sequent).not.to.be.undefined;

        let prfid: string = response?.result.id;

        // send proof command (skosimp*)
        response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: '(sko)' });
        expect(response?.result.commentary).not.to.be.undefined;
        expect(response?.result.commentary[0].startsWith("Error: Ill-formed rule or strategy, substituting (skip): (SKO)")).to.equal(true);

        response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: '(sko' });
        // console.dir(response);
        expect(response?.result[0].commentary).not.to.be.undefined;
        // console.dir(response?.result[0].commentary);
        expect(response?.result[0].commentary[0]).to.contain("No change on: (sko");

        // quit the proof attempt
        await pvsProxy?.proofCommand({ proofId: prfid, cmd: 'quit' });
    });

    // // the following test is disabled because pvs does not support parallel processing of files
    it(`can start prover session while parsing files in other contexts`, async () => {
        // async call to the parser in context safesandbox
        pvsProxy?.parseFile({ fileName: "alaris2lnewmodes", fileExtension: ".pvs", contextFolder: safeSandboxExamples });

        // call to prove-formula in sandbox, while the parser is running in the other context
        const desc: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "alaris2lnewmodes.pump",
            formulaName: "vtbi_over_rate_lemma",
            theoryName: "pump_th"
        };
        let response: PvsResponse | undefined = await pvsProxy?.proveFormula(desc);
        expect(response).not.to.be.undefined;
        expect(response?.result).not.to.be.undefined;
        expect(response?.error).to.be.undefined;

        let prfid: string = response?.result.id;

        response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: 'quit' });
        expect(response?.result.status).to.equal("quit");
    }).timeout(60000);

    //-----------------------------------------------
    //--- the following test fail on Mac and Linux
    //-----------------------------------------------

    // the following test fails after QED, with the following error
    // 	Error: the assertion
    //        (or (equalp (car scr-old) "")
    //            (and (stringp (car scr-old))
    //                 (char= (char (car scr-old) 0) #\;)))
    //        failed.
    //   [condition type: simple-error]
    it(`supports glassbox tactics`, async () => {
        //await quitProverIfActive();

        const desc: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "sq",
            formulaName: "sq_neg",
            theoryName: "sq"
        };

        let response: PvsResponse | undefined = await pvsProxy?.proveFormula(desc);
        expect(response).not.to.be.undefined;
        // console.dir(response);
        expect(response?.result.label).to.deep.equal(test.sq_neg_prove_formula.label);
        expect(response?.result.sequent).not.to.be.undefined;

        let prfid: string = response?.result.id;

        response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: '(then (skosimp*)(grind))' });
        expect(response?.error).to.be.undefined;
        expect(response?.result).not.to.be.undefined;
        // console.dir(response);

        // quit the proof attempt
        await pvsProxy?.proofCommand({ proofId: prfid, cmd: 'quit' });
    });

    // on Mac and Linux, the following test fails when executed **during the first** prover session
    // to activate the test case, change 'xit(...)' to 'it(...)'
    it(`is robust to prover commands with incorrect arguments`, async () => {
        //await quitProverIfActive();

        const desc: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "sq",
            formulaName: "sq_neg",
            theoryName: "sq"
        };

        let response: PvsResponse | undefined = await pvsProxy?.proveFormula(desc);
        expect(response).not.to.be.undefined;
        // console.dir(response);
        expect(response?.result.label).to.deep.equal(test.sq_neg_prove_formula.label);
        expect(response?.result.sequent).not.to.be.undefined;

        let prfid: string = response?.result.id;

        await pvsProxy?.proofCommand({ proofId: prfid, cmd: '(skosimp*)' });
        //response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: '(typepred "a!1)' });

        response = await pvsProxy?.pvsRequest('proof-command', [prfid, '(skosimp*)']);
        //console.dir(response);
        expect(response?.result).not.to.be.undefined;
        expect(response?.error).to.be.undefined;

        //response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: '(expand "as <")' });
        response = await pvsProxy?.pvsRequest('proof-command', [prfid, '(expand "as <")']);
        // console.log('response = ', response);
        expect(response?.error).to.be.undefined;
        expect(response?.result).not.to.be.undefined;
        // quit the proof attempt
        await pvsProxy?.proofCommand({ proofId: prfid, cmd: 'quit' });
    });

    // on Mac and Linux, pvs-server fails with the following error:  { code: 1, message: '"No methods applicable for generic function #<standard-generic-function all-declarations> with args (nil) of classes (null)"' }
    // to activate the test case, change 'xit(...)' to 'it(...)'
    it(`prove-formula is robust to invocations with incorrect theory names`, async () => {
        const desc: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "alaris2lnewmodes",
            formulaName: "check_chev_fup_permission",
            theoryName: "pump_th" // pump_th exists, but check_chev_fup_permission is in alaris_th
        };
        let response: PvsResponse | undefined = await pvsProxy?.proveFormula(desc);
        expect(response).not.to.be.undefined;
        expect(response?.result).to.be.undefined;
        expect(response?.error).not.to.be.undefined;
        expect(response?.error.message.startsWith("Typecheck error")).to.equal(true);

        //response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: 'quit' });
        //console.dir(response);
        //expect(response?.result.commentary[0]).to.contain("No change on: quit");
        //expect(response?.error).not.to.be.undefined;
        //expect(response?.error.message).to.contain('Proof-command error');
    }).timeout(60000);

    it(`can interrupt prover commands`, async () => {
        //await quitProverIfActive();

        const desc: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "alaris2lnewmodes",
            formulaName: "check_chev_fup_permission",
            theoryName: "alaris_th"
        };
        let pfResp: PvsResponse | undefined = await pvsProxy?.proveFormula(desc);
        expect(pfResp).not.to.be.undefined;
        let prfid: string = pfResp?.result.id;

        setTimeout(async () => {
            // console.log('Trying to interrupt');
            let intResp: PvsResponse | undefined = await pvsProxy?.pvsInterrupt(prfid);
            //console.log('intResp = ', intResp);
        }, 2000);
        await pvsProxy?.proofCommand({ proofId: prfid, cmd: '(skosimp*)' });
        let response: PvsResponse | undefined = await pvsProxy?.proofCommand({ proofId: prfid, cmd: '(grind)' });
        expect(response).not.to.be.undefined;
        expect(response?.result).not.to.be.undefined;
        expect(response?.result.label).not.to.be.undefined;
        expect(response?.result.sequent).not.to.be.undefined;
        //expect(response?.result.status).to.equal("interrupted");
        expect(response?.result["prev-cmd"].toLowerCase()).to.deep.equal("(skosimp*)");
        // console.dir(response?.result);
    }).timeout(5000);

    it(`can prove omega_2D_continuous without triggering stack overflow`, async () => {
        //await quitProverIfActive();

        const formula: PvsFormula = {
            contextFolder: path.join(__dirname, "nasalib/ACCoRD"),
            fileExtension: ".pvs",
            fileName: "omega_2D",
            theoryName: "omega_2D",
            formulaName: "omega_2D_continuous"
        };

        let response: PvsResponse | undefined = await pvsProxy?.proveFormula(formula);
        expect(response).not.to.be.undefined;
        //console.dir(response);
        let prfid: string = response?.result.id;
        const cmds: string[] = [
            `(skosimp*)`,
            `(lemma "curried_min_is_cont_2D")`,
            `(inst - "(LAMBDA (t: real, v: Vect2): IF (B <= t AND t <= T) THEN horiz_dist_scaf(s!1)(t,v) ELSE 0 ENDIF)" "B" "T")`,
            `(ground)`,
            `(expand "continuous?")`,
            `(expand "continuous?")`,
            `(expand "continuous_at?")`,
            `(skosimp*)`,
            `(inst - "x!1")`,
            `(inst - "epsilon!1")`,
            `(skosimp*)`,
            `(inst + "delta!1")`,
            `(skosimp*)`,
            `(inst - "y!1")`,
            `(expand "member")`,
            `(expand "ball")`,
            `(lemma "omega_2D_min_rew")`,
            `(inst-cp - "s!1" "x!1")`,
            `(inst - "s!1" "y!1")`,
            `(assert)`,
            `(case "{r: real | EXISTS (t: Lookahead): horiz_dist_scaf(s!1)(t, y!1) = r} = {r: real | EXISTS (t_1: (LAMBDA (x: real): B <= x AND x <= T)): r = horiz_dist_scaf(s!1)(t_1, y!1)} AND {r: real | EXISTS (t: Lookahead): horiz_dist_scaf(s!1)(t, x!1) = r} = {r: real | EXISTS (t_1: (LAMBDA (x: real): B <= x AND x <= T)): r = horiz_dist_scaf(s!1)(t_1, x!1)}")`,
            `(flatten)`,
            `(assert)`
        ]
        for (let i = 0; i < cmds.length; i++) {
            response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: cmds[i] });
        }
        //console.dir(response);
        expect(response?.error).to.be.undefined;
        expect(response?.result).not.to.be.undefined;
    }).timeout(80000);

    // PVS-8.0 tests
    it(`The quit command stores the proof`, async () => {
        const formula: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "test_prover_quit_proof",
            formulaName: "dummy",
            theoryName: "test_prover_quit_proof"
        };

        let pfResp: PvsResponse | undefined = await pvsProxy?.proveFormula(formula);
        expect(pfResp).not.to.be.undefined;

        let prfid: string = pfResp?.result.id;
        let now = Date.now();
        let proofCommand: string = "(comment \"" + now + "\")" ;
        let response: PvsResponse | undefined = await pvsProxy?.proofCommand({ proofId: prfid, cmd: proofCommand });
        expect(response).not.to.be.undefined;
        expect(response?.error).to.be.undefined;

        response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: "(quit)" });
        expect(response).not.to.be.undefined;
        expect(response?.error).to.be.undefined;

        // check that the proof just quitted is stored as one of the proofs of the formula
        response = await pvsProxy?.getAllProofScripts(formula);
        expect(response).not.to.be.undefined;
        let proofsAfterQuit: Array<any> = response?.result;
        let found : boolean = false;
        for(let i=0; i<proofsAfterQuit.length; i++){
            if (proofsAfterQuit[i].id === prfid) {
                expect(proofsAfterQuit[i].script[1][1]==now).to.be.equal(true);
            }
        }
    });

    it(`The quit command does not modifies the default proof for the declaration (if it had a default proof)`, async () => {
        const formula: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "test_prover_quit_proof",
            formulaName: "dummy",
            theoryName: "test_prover_quit_proof"
        };
        let defaultProofBeforeQuit: PvsResponse | undefined = await pvsProxy?.getDefaultProofScript(formula);
        expect(defaultProofBeforeQuit).not.to.be.undefined;
        expect(defaultProofBeforeQuit?.error).to.be.undefined;

        let pfResp: PvsResponse | undefined = await pvsProxy?.proveFormula(formula);
        expect(pfResp).not.to.be.undefined;

        let prfid: string = pfResp?.result.id;
        let now = Date.now();
        let proofCommand: string = "(comment \"" + now + "\")" ;
        let response: PvsResponse | undefined = await pvsProxy?.proofCommand({ proofId: prfid, cmd: proofCommand });
        expect(response).not.to.be.undefined;
        expect(response?.error).to.be.undefined;

        response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: "(quit)" });
        expect(response).not.to.be.undefined;
        expect(response?.error).to.be.undefined;

        // check that default proof is not changed
        let defaultProofAfterQuit: PvsResponse | undefined = await pvsProxy?.getDefaultProofScript(formula);
        expect(defaultProofAfterQuit).not.to.be.undefined;
        expect(defaultProofAfterQuit?.error).to.be.undefined;
        expect(defaultProofBeforeQuit?.result).to.be.equal(defaultProofAfterQuit?.result);
    });

    it(`The quit command does not modifies the default proof for a declaration with no proofs`, async () => {
        const formula: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "test_prover_quit_proof",
            formulaName: "dummy_no_proof",
            theoryName: "test_prover_quit_proof"
        };
        let defaultProofBeforeQuit: PvsResponse | undefined = await pvsProxy?.getDefaultProofScript(formula);
        expect(defaultProofBeforeQuit).not.to.be.undefined;
        expect(defaultProofBeforeQuit?.error).not.to.be.undefined;
        expect(defaultProofBeforeQuit?.error?.data.error_string.endsWith(`${formula.formulaName} does not have a proof`)).to.equal(true);

        let pfResp: PvsResponse | undefined = await pvsProxy?.proveFormula(formula);
        expect(pfResp).not.to.be.undefined;

        let prfid: string = pfResp?.result.id;
        let now = Date.now();
        let proofCommand: string = "(comment \"" + now + "\")" ;
        let response: PvsResponse | undefined = await pvsProxy?.proofCommand({ proofId: prfid, cmd: proofCommand });
        expect(response).not.to.be.undefined;
        expect(response?.error).to.be.undefined;

        response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: "(quit)" });
        expect(response).not.to.be.undefined;
        expect(response?.error).to.be.undefined;

        // check that default proof is not changed
        let defaultProofAfterQuit: PvsResponse | undefined = await pvsProxy?.getDefaultProofScript(formula);
        expect(defaultProofAfterQuit).not.to.be.undefined;
        expect(defaultProofAfterQuit?.error).to.be.undefined;
        expect(defaultProofAfterQuit?.result.endsWith("(\"\" (POSTPONE))")).to.equal(true);
    });

    it(`The quit command stores the proof, but it does not modify the prf file.`, async () => {

        const formula: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "test_prover_quit_proof",
            formulaName: "dummy",
            theoryName: "test_prover_quit_proof"
        };
        let proofsInPrfFileBeforeQuit: PvsResponse | undefined = await pvsProxy?.getProofScriptsInPrfFile(formula.contextFolder + "/" + formula.fileName);
        expect(proofsInPrfFileBeforeQuit).not.to.be.undefined;
        expect(proofsInPrfFileBeforeQuit?.error).to.be.undefined;

        let pfResp: PvsResponse | undefined = await pvsProxy?.proveFormula(formula);
        expect(pfResp).not.to.be.undefined;

        let prfid: string = pfResp?.result.id;
        let now = Date.now();
        let proofCommand: string = "(comment \"" + now + "\")" ;
        let response: PvsResponse | undefined = await pvsProxy?.proofCommand({ proofId: prfid, cmd: proofCommand });
        expect(response).not.to.be.undefined;
        expect(response?.error).to.be.undefined;

        response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: "(quit)" });
        expect(response).not.to.be.undefined;
        expect(response?.error).to.be.undefined;

        // check that the prf file is not modified
        let proofsInPrfFileAfterQuit: PvsResponse | undefined = await pvsProxy?.getProofScriptsInPrfFile(formula.contextFolder + "/" + formula.fileName);
        expect(proofsInPrfFileAfterQuit).not.to.be.undefined;
        expect(proofsInPrfFileAfterQuit?.error).to.be.undefined;
        expect(proofsInPrfFileBeforeQuit?.result).to.be.deep.equal(proofsInPrfFileAfterQuit?.result);
    });

});
