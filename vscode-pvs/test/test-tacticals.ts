import * as fsUtils from "../server/src/common/fsUtils";
import { PvsResponse, PvsResult } from "../server/src/common/pvs-gui";
import { PvsProxy } from '../server/src/pvsProxy';
import { configFile, tacticalsExamples } from './test-utils';
import { PvsFormula } from "../server/src/common/serverInterface";
import * as path from 'path';
import { execSync } from "child_process";
import * as languageUtils from '../server/src/common/languageUtils';
import { expect } from 'chai';

//----------------------------
//   Test cases for prover
//----------------------------
describe("pvs-prover", () => {
    let pvsProxy: PvsProxy | undefined = undefined;
    before(async () => {
        const config: string = await fsUtils.readFile(configFile);
        const content: { pvsPath: string } = JSON.parse(config);
        // log(content);
        const pvsPath: string = content.pvsPath;
        // log("Activating xmlrpc proxy...");
        pvsProxy = new PvsProxy(pvsPath, { externalServer: true });
        await pvsProxy?.activate({ debugMode: false, showBanner: false }); // this will also start pvs-server

        // delete pvsbin files and .pvscontext
        await fsUtils.cleanBin(tacticalsExamples);

        console.log("\n----------------------");
        console.log("test-tacticals");
        console.log("----------------------");
    });
    after(async () => {
        await pvsProxy?.killPvsServer();
        await pvsProxy?.killPvsProxy();
        // delete pvsbin files and .pvscontext
        await fsUtils.cleanBin(tacticalsExamples);
    });

    // utility function, quits the prover if the prover status is active
    const quitProverIfActive = async (): Promise<void> => {
        await pvsProxy?.pvsRequest('quit-all-proof-sessions');
    }

    // this test requires nasalib and patch-20291231-server-output
    it(`can execute glassbox proof for eventually_nulltiming`, async () => {
        await quitProverIfActive();

        // Need to clear-theories, in case rerunning with the same server.
        await pvsProxy?.lisp("(clear-theories t)");

        const baseFolder: string = path.join(__dirname, "pvscontext");

        // remove folder if present and replace it with the content of the zip file
        const contextFolder: string = path.join(baseFolder, "monitors");
        fsUtils.deleteFolder(contextFolder);
        execSync(`cd ${baseFolder} && unzip nasalib-monitors-1.zip`);

        // await fsUtils.cleanBin(contextFolder, { keepTccs: true, recursive: true }); // cleaning pvsbin and .pvscontext does not help

        let response: PvsResponse | undefined = await pvsProxy?.proveFormula({
            fileName: "trace",
            fileExtension: ".pvs",
            contextFolder: path.join(contextFolder, "Fret_MLTL"),
            theoryName: "trace",
            formulaName: "eventually_nulltiming"
        });
        expect(response).not.to.be.undefined;
        expect(response?.result).not.to.be.undefined;
        expect(response?.error).to.be.undefined;
        // console.dir(response);
        let prfid: string = response?.result.id;

        const cmd: string = `(then (skeep) (expand "check_fretish_req_semantics") (expand* "Timing_fun"))`;
        response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: cmd });
        // console.log(cmd);
        // console.dir(response);

        expect(response?.error).to.be.undefined;
        expect(response?.result).not.to.be.undefined;
    }).timeout(20000);

    // this test requires nasalib and patch-20291231-server-output
    it(`can handle branches with propax`, async () => {
        //await quitProverIfActive();

        // Need to clear-theories, in case rerunning with the same server.
        //await pvsProxy?.lisp("(clear-theories t)");

        const baseFolder: string = path.join(__dirname, "pvscontext");

        // remove folder if present and replace it with the content of the zip file
        const contextFolder: string = path.join(baseFolder, "nasalib-monitors");
        fsUtils.deleteFolder(contextFolder);
        execSync(`cd ${baseFolder} && unzip nasalib-monitors.zip`);

        let response: PvsResponse | undefined = await pvsProxy?.proveFormula({
            fileName: "trace",
            fileExtension: ".pvs",
            contextFolder: path.join(contextFolder, "Fret_MLTL"),
            theoryName: "trace",
            formulaName: "null_null_always_satisfaction"
        });
        expect(response).not.to.be.undefined;
        expect(response?.result).not.to.be.undefined;
        expect(response?.error).to.be.undefined;
        // console.dir(response);
        let prfid: string = response?.result.id;

        const cmds: string[] = `(skeep)(fretex)(iff)(split)(flatten)(inst -1 "0")(skeep)(inst 2 "n-1")(case "i > n-1")(expand "Trace_equiv")(inst -3 "n-1")(assert)(flatten)(assert)(expand "last_atom")`.replace(/\)/g, ")\n").split("\n").filter(cmd => { return cmd && cmd.trim() !== ""; });
        for (let i = 0; i < cmds.length; i++) {
            const cmd: string = cmds[i];
            response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: cmd });
            //console.log(`cmd: ${cmd}`);
            //console.dir(response);
        }

        expect(response?.result["prev-cmd"].toLowerCase()).to.deep.equal(`(case "i > n-1")`);
    }).timeout(20000);

    it(`provides correct prev-cmd when branch closes (test 1)`, async () => {
        await quitProverIfActive();

        const request: PvsFormula = {
            contextFolder: tacticalsExamples,
            fileExtension: '.pvs',
            fileName: 'foo',
            formulaName: 'foo1',
            theoryName: 'foo'
        };
        let response: PvsResponse | undefined = await pvsProxy?.proveFormula(request);
        expect(response).not.to.be.undefined;
        // console.dir(response, { depth: null });
        expect(response?.error).to.be.undefined;
        expect(response?.result).not.to.be.undefined;
        let prfid: string = response?.result.id;

        response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: "(skosimp*)" });
        expect(response?.result["prev-cmd"].toLowerCase()).to.deep.equal("(skosimp*)");
        // console.dir(response);

        response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: "(split)" });
        expect(response?.result["prev-cmd"].toLowerCase()).to.deep.equal("(split)");
        // console.dir(response);

        response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: "(flatten)" });
        expect(response?.result["prev-cmd"].toLowerCase()).to.deep.equal("(flatten)");
        // console.dir(response);

        response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: "(grind)" });
        expect(response?.result["prev-cmd"].toLowerCase()).to.deep.equal("(split)");
        expect(languageUtils.branchComplete(response?.result, request.formulaName, `1`)).to.equal(true);
        // console.dir(response);

    });

    // this test case fails (prev-cmd and commentary provide incorrect information for the first command)
    it(`can execute tactical 'then'`, async () => {
        await quitProverIfActive();

        const request: PvsFormula = {
            contextFolder: tacticalsExamples,
            fileExtension: '.pvs',
            fileName: 'foo',
            formulaName: 'foo1',
            theoryName: 'foo'
        };
        let response: PvsResponse | undefined = await pvsProxy?.proveFormula(request);
        expect(response).not.to.be.undefined;
        //console.dir(response, { depth: null });
        expect(response?.error).to.be.undefined;
        expect(response?.result).not.to.be.undefined;
        let prfid: string = response?.result.id;

        const skosimp_response: PvsResponse | undefined = await pvsProxy?.proofCommand({ proofId: prfid, cmd: "(skosimp*)" });
        expect(skosimp_response).not.to.be.undefined;
        //console.dir(skosimp_response, { depth: null });
        expect(skosimp_response?.result["prev-cmd"].toLowerCase()).to.deep.equal("(skosimp*)");

        const split_response: PvsResponse | undefined = await pvsProxy?.proofCommand({ proofId: prfid, cmd: "(split)" });
        expect(split_response).not.to.be.undefined;
        //console.dir(split_response, { depth: null });
        expect(split_response?.result["prev-cmd"].toLowerCase()).to.deep.equal("(split)");

        await pvsProxy?.proofCommand({ proofId: prfid, cmd: "(quit)" });

        // re-run the proof commands with the tactical
        response = await pvsProxy?.proveFormula(request);
        prfid = response?.result.id;
        response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: "(then (skosimp*) (split))" });
        await pvsProxy?.proofCommand({ proofId: prfid, cmd: "(quit)" });
        //console.dir(response, { depth: null });

        expect(response?.result["prev-cmd"].toLowerCase()).to.deep.equal("(split)");
        expect(response?.result.label).to.deep.equal(split_response?.result.label);
        expect(response?.result["num-subgoals"]).to.deep.equal(split_response?.result["num-subgoals"]);
        expect(response?.result.commentary.slice(0, 2)).to.deep.equal(skosimp_response?.result.commentary.slice(0, 2));
        expect(response?.result.commentary.slice(3, 4)).to.deep.equal(split_response?.result.commentary.slice(0, 1));
        expect(response?.result.sequent).to.deep.equal(split_response?.result.sequent);

        expect(response?.result["prev-cmd"].toLowerCase()).to.deep.equal("(split)");
        // expect(response?.result.label).to.deep.equal(split_response?.result.label);
        // expect(response?.result["num-subgoals"]).to.deep.equal(split_response?.result["num-subgoals"]);
        // expect(response?.result.commentary).to.deep.equal(split_response?.result.commentary);
        // expect(response?.result.sequent).to.deep.equal(split_response?.result.sequent);

        expect(response?.result.label).to.deep.equal(`foo1.1`);
    }).timeout(10000);


});
