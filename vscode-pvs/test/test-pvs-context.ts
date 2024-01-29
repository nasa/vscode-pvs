import * as fsUtils from "../server/src/common/fsUtils";
import { PvsResponse, PvsResult } from "../server/src/common/pvs-gui";
import { PvsProxy } from '../server/src/pvsProxy'; // XmlRpcSystemMethods
import { configFile } from './test-utils';
import * as path from 'path';
import { execSync } from "child_process";
//import { PvsIoProxy } from '../server/src/pvsioProxy';
import { expect } from 'chai';

//----------------------------
//   Test cases for checking behavior of pvs with corrupted .pvscontext
//----------------------------
describe("pvs", () => {
    let pvsProxy: PvsProxy | undefined = undefined;
    //    let pvsioProxy: PvsIoProxy = null;

    let baseFolder: string = path.join(__dirname, "pvscontext");
    const cleanAll = () => {
        fsUtils.deleteFolder(path.join(baseFolder, "monitors"));
        fsUtils.deleteFolder(path.join(baseFolder, "baxter"));
        fsUtils.deleteFolder(path.join(baseFolder, "type_theory"));
        fsUtils.deleteFolder(path.join(baseFolder, "alaris2l"));
        fsUtils.deleteFolder(path.join(baseFolder, "pvsICEipandvs2"));
        fsUtils.deleteFolder(path.join(baseFolder, "trace"));
        fsUtils.deleteFolder(path.join(baseFolder, "pillboxv7"));
        fsUtils.deleteFolder(path.join(baseFolder, "nasalib-monitors"));
        fsUtils.deleteFolder(path.join(baseFolder, "nasalib-monitors-stack-limit-error"));
    }

    before(async () => {
        const config: string = await fsUtils.readFile(configFile);
        const content: { pvsPath: string } = JSON.parse(config);
        // console.log(content);
        const pvsPath: string = content.pvsPath;
        // log("Activating xmlrpc proxy...");
        pvsProxy = new PvsProxy(pvsPath, { externalServer: true });
        await pvsProxy?.activate({ debugMode: false, showBanner: false }); // this will also start pvs-server

        //pvsioProxy = new PvsIoProxy(pvsPath);

        console.log("\n----------------------");
        console.log("test-pvscontext");
        console.log("----------------------");
    });
    after(async () => {
        await pvsProxy?.quitAllProofs();
        await pvsProxy?.killPvsServer();
        await pvsProxy?.killPvsProxy();
        await new Promise<void>((resolve, reject) => {
            setTimeout(() => {
                cleanAll();
                resolve();
            }, 1500);
        })
    });

    // this test fails -- grind does not terminate
    // this test case requires pvs-experimental/monitors in the pvs-library-path
    // or alternatively folder vscode-pvs/test/pvs-context/nasalib-monitors-stack-limit-error in the library path
    xit(`can prove null_null_after_satisfaction_ft (nasalib-monitors-stack-limit-error.zip)`, async () => {
        await pvsProxy?.quitAllProofs();

        // Need to clear-theories, in case rerunning with the same server.
        await pvsProxy?.emptyAllWorkspaces();

        // remove folder if present and replace it with the content of the zip file
        const contextFolder: string = path.join(baseFolder, "nasalib-monitors-stack-limit-error");
        fsUtils.deleteFolder(contextFolder);
        execSync(`cd ${baseFolder} && unzip nasalib-monitors-stack-limit-error.zip`);

        let response: PvsResponse | undefined = await pvsProxy?.proveFormula({
            fileName: "trace",
            fileExtension: ".pvs",
            contextFolder: path.join(contextFolder, "Fret_MLTL"),
            theoryName: "trace",
            formulaName: "null_null_after_satisfaction_ft"
        });
        expect(response).not.to.be.undefined;
        expect(response?.result).not.to.be.undefined;
        expect(response?.error).to.be.undefined;
        // console.dir(response);
        let prfid: string = response?.result.id;
        const commands: string[] = [
            '(skeep)',
            '(fretex)',
            '(iff)',
            '(split)',
            '(flatten)',
            '(split)',
            '(inst -1 "0")',
            '(expand "nth")',
            '(expand "after")',
            '(inst -1 "0")',
            '(expand "nth")',
            '(flatten)',
            '(skeep)',
            '(skeep)',
            '(case "n-1 < i")',
            '(inst 2 "n-1")',
            '(expand "Trace_equiv")',
            '(inst -6 "n-1")',
            '(grind)'
        ]
        for (let i = 0; i < commands.length; i++) {
            response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: commands[i] });
        }
        expect(response?.result).not.to.be.undefined;
        expect(response?.error).to.be.undefined;
    }).timeout(300000);

    it(`can typecheck nasalib-monitors/trace.pvs (nasalib-monitors.zip)`, async () => {
        // await quitProverIfActive();

        // Need to clear-theories, in case rerunning with the same server.
        await pvsProxy?.emptyAllWorkspaces();

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
        expect(undefined).not.to.be.undefined;
        expect(response?.result).not.to.be.undefined;
        expect(response?.error).to.be.undefined;
        // console.dir(response);
        let prfid: string = response?.result.id;

        response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: `(skeep)` });
        response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: `(fretex)` });
        // response = await pvsProxy?.proofCommand({ cmd: `(skeep)(fretex)(iff)(split)(flatten)(inst -1 "0")(skeep)(inst 2 "n-1")(case "i > n-1")(expand "Trace_equiv")(inst -3 "n-1")(assert)(flatten)(assert)(expand "last_atom")(expand "always")(split)(inst -1 "i")(split)(expand "Trace_equiv")(inst -2 "i")(flatten)(hide -2 -3 -4 -5 -7)(expand "post_condition_atom")(assert)(typepred "i")(assert)(expand "nth")(typepred "i")(grind)(expand "nth")(grind)(inst -1 "i")(expand "nth")(grind)(expand "nth")(typepred "i")(grind)(expand "length")(grind)(flatten)(skeep)(expand "always")(skeep)(typepred "i_1")(inst -2 "i_1")(expand "nth")(assert)(typepred "i")(grind)` });
        // console.dir(response);
        expect(response?.result).not.to.be.undefined;
        expect(response?.error).to.be.undefined;
    }).timeout(300000);

    it(`identified typecheck errors for datatypes in type_theory (type-theory-error-with-datatypes.zip)`, async () => {
        await pvsProxy?.quitAllProofs();

        // Need to clear-theories, in case rerunning with the same server.
        await pvsProxy?.emptyAllWorkspaces();

        // remove folder if present and replace it with the content of the zip file
        fsUtils.deleteFolder(path.join(baseFolder, "type_theory"));
        execSync(`cd ${baseFolder} && unzip type-theory-error-with-datatypes.zip`);

        const response: PvsResponse | undefined = await pvsProxy?.typecheckFile({
            fileName: "basics",
            fileExtension: ".pvs",
            contextFolder: path.join(baseFolder, "type_theory")
        });
        // console.dir(response);
        expect(response).not.to.be.undefined;
        expect(response?.result).to.be.undefined;
        expect(response?.error).not.to.be.undefined;

    }).timeout(10000);

    it(`identifies typecheck errors when processing baxterSigmaSpectrum.pvs`, async () => {
        await pvsProxy?.quitAllProofs();

        // Need to clear-theories, in case rerunning with the same server.
        await pvsProxy?.emptyAllWorkspaces();

        // remove baxter folder if present and replace it with the content of the zip file
        fsUtils.deleteFolder(path.join(baseFolder, "baxter"));
        execSync(`cd ${path.join(__dirname, "pvscontext")} && unzip baxter-two-theory-limits.zip`);

        const response: PvsResponse | undefined = await pvsProxy?.typecheckFile({
            fileName: "baxterSigmaSpectrum",
            fileExtension: ".pvs",
            contextFolder: path.join(baseFolder, "baxter")
        });
        // console.dir(response, { depth: null });
        expect(response).not.to.be.undefined;
        expect(response?.result).not.to.be.undefined;
        expect(response?.error).to.be.undefined;
        // console.dir(response?.error);
        //expect(response?.error.data).not.to.be.undefined;
        //expect(response?.error.data.error_string).not.to.be.undefined;
        // expect(response?.error.data.error_string).toMatch(/\blimits\b/g);

    }).timeout(10000);

    //-- all tests below this line are completed successfully

    it(`can show tccs for alaris_th`, async () => {
        await pvsProxy?.quitAllProofs();

        // Need to clear-theories, in case rerunning with the same server.
        await pvsProxy?.emptyAllWorkspaces();

        // remove alaris folder if present and replace it with the content of the zip file
        fsUtils.deleteFolder(path.join(baseFolder, "alaris2l"));
        execSync(`cd ${path.join(__dirname, "pvscontext")} && unzip alaris2l-show-tccs-error.zip`);

        const response: PvsResponse | undefined = await pvsProxy?.generateTccsFile({
            fileName: "alaris2lnewmodes",
            fileExtension: ".pvs",
            contextFolder: path.join(baseFolder, "alaris2l"),
            theoryName: "alaris_th"
        });
        // console.dir(response);
        expect(response).not.to.be.undefined;
        expect(response?.result).not.to.be.undefined;
        expect(response?.error).to.be.undefined;

    }).timeout(20000);

    it(`can typecheck datatypes in trace`, async () => {
        await pvsProxy?.quitAllProofs();

        // Need to clear-theories, in case rerunning with the same server.
        await pvsProxy?.emptyAllWorkspaces();

        // remove folder if present and replace it with the content of the zip file
        fsUtils.deleteFolder(path.join(baseFolder, "trace"));
        execSync(`cd ${baseFolder} && unzip trace.zip`);

        let response: PvsResponse | undefined | null = await pvsProxy?.parseFile({
            fileName: "trace",
            fileExtension: ".pvs",
            contextFolder: path.join(baseFolder, "trace")
        });
        // console.dir(response);
        expect(response).not.to.be.undefined;
        expect(response).not.to.be.null;
        response = await pvsProxy?.parseFile({
            fileName: "CONDITIONS",
            fileExtension: ".pvs",
            contextFolder: path.join(baseFolder, "trace")
        });
        expect(response).not.to.be.undefined;
        expect(response).not.to.be.null;
        // console.dir(response);
        response = await pvsProxy?.typecheckFile({
            fileName: "CONDITIONS",
            fileExtension: ".pvs",
            contextFolder: path.join(baseFolder, "trace")
        });
        // console.dir(response);
        expect(response).not.to.be.undefined;
        expect(response?.result).not.to.be.undefined;
        expect(response?.error).to.be.undefined;

    }).timeout(10000);

    // this test case fails with the assertion error "the assertion (directory-p path) failed."
    it(`ignores non-existing folders indicated in pvs-library-path`, async () => {
        await pvsProxy?.quitAllProofs();

        // Need to clear-theories, in case rerunning with the same server.
        await pvsProxy?.emptyAllWorkspaces();

        // remove folder if present and replace it with the content of the zip file
        const contextFolder: string = path.join(baseFolder, "nasalib-monitors");
        fsUtils.deleteFolder(contextFolder);
        execSync(`cd ${baseFolder} && unzip nasalib-monitors.zip`);

        await pvsProxy?.lisp(`(push "~/non/existing/path/" *pvs-library-path*)`);

        let response: PvsResponse | undefined = await pvsProxy?.proveFormula({
            fileName: "trace",
            fileExtension: ".pvs",
            contextFolder: path.join(contextFolder, "Fret_MLTL"),
            theoryName: "trace",
            formulaName: "null_null_always_satisfaction"
        });
        expect(response?.result).not.to.be.undefined;
        expect(response?.error).to.be.undefined;
    }).timeout(40000);


    it(`can find typecheck error in ICEcoordinator.pvs (wrong field type)`, async () => {
        await pvsProxy?.quitAllProofs();

        // Need to clear-theories, in case rerunning with the same server.
        await pvsProxy?.emptyAllWorkspaces();

        // remove folder if present and replace it with the content of the zip file
        fsUtils.deleteFolder(path.join(baseFolder, "pvsICEipandvs2"));
        execSync(`cd ${baseFolder} && unzip pvsICEipandvs2-wrong-field.zip`);

        const response: PvsResponse | undefined = await pvsProxy?.typecheckFile({
            fileName: "main",
            fileExtension: ".pvs",
            contextFolder: path.join(baseFolder, "pvsICEipandvs2")
        });
        // console.dir(response);
        expect(response).not.to.be.undefined;
        expect(response?.result).to.be.undefined;
        expect(response?.error).not.to.be.undefined;

    }).timeout(10000);

    it(`can typecheck strings defined in pillboxv7`, async () => {
        await pvsProxy?.quitAllProofs();

        // Need to clear-theories, in case rerunning with the same server.
        await pvsProxy?.emptyAllWorkspaces();

        // remove pillboxv7 folder if present and replace it with the content of the zip file
        fsUtils.deleteFolder(path.join(baseFolder, "pillboxv7"));
        execSync(`cd ${baseFolder} && unzip pillboxv7-errors-with-strings.zip`);

        const response: PvsResponse | undefined = await pvsProxy?.typecheckFile({
            fileName: "main",
            fileExtension: ".pvs",
            contextFolder: path.join(baseFolder, "pillboxv7")
        });
        // the following timeout will give time to pvs to write pvsbin and .pvscontext, which are going to be deleted
        await new Promise<void>((resolve, reject) => {
            setTimeout(() => {
                fsUtils.deleteFolder(path.join(baseFolder, "pillboxv7"));
                resolve();
            }, 8000);
        });
        // console.dir(response);
        expect(response).not.to.be.undefined;
        expect(response?.result).not.to.be.undefined;
        expect(response?.error).to.be.undefined;

    }).timeout(40000);

    it(`can run find-declaration without hitting breaks`, async () => {
        await pvsProxy?.quitAllProofs();

        // Need to clear-theories, in case rerunning with the same server.
        await pvsProxy?.emptyAllWorkspaces();

        fsUtils.deleteFolder(path.join(baseFolder, "data"));
        execSync(`cd ${baseFolder} && unzip data.zip`);

        const dataFolder: string = path.join(baseFolder, "data");
        const TypeCheckPVS: string = fsUtils.desc2fname({
            fileName: "TypeCheck",
            fileExtension: ".pvs",
            contextFolder: dataFolder
        });
        const LiteralPVS: string = fsUtils.desc2fname({
            fileName: "Literal",
            fileExtension: ".pvs",
            contextFolder: dataFolder
        });
        let response: PvsResponse | undefined = undefined;
        const externalServer: boolean = false;

        response = await pvsProxy?.lisp(`(change-workspace "${dataFolder}" t)`);
        response = await pvsProxy?.lisp(`(parse-file "${TypeCheckPVS}" nil)`);
        response = await pvsProxy?.lisp(`(change-workspace "${dataFolder}" t)`);
        response = await pvsProxy?.lisp(`(typecheck-file "${TypeCheckPVS}" nil nil nil nil t)`);
        response = await pvsProxy?.lisp(`(change-workspace "${dataFolder}" t)`);
        response = await pvsProxy?.lisp(`(parse-file "${LiteralPVS}" nil)`);
        response = await pvsProxy?.lisp(`(change-workspace "${dataFolder}" t)`);
        response = await pvsProxy?.lisp(`(typecheck-file "${LiteralPVS}" nil nil nil nil t)`);

        response = await pvsProxy?.lisp(`(find-declaration "PrimitiveValue")`);
        // console.dir(response);

        expect(response).not.to.be.undefined;
        expect(response?.result).not.to.be.undefined;
        expect(response?.error).to.be.undefined;
    }).timeout(20000);

    it(`can typecheck lists defined in pillboxv7 and can handle corrupt prf file without breaking into lisp`, async () => {
        await pvsProxy?.quitAllProofs();

        // Need to clear-theories, in case rerunning with the same server.
        await pvsProxy?.emptyAllWorkspaces();

        // remove pillboxv7 folder if present and replace it with the content of the zip file
        fsUtils.deleteFolder(path.join(baseFolder, "pillboxv7"));
        execSync(`cd ${baseFolder} && unzip pillboxv7-errors-with-lists.zip`);

        const response: PvsResponse | undefined = await pvsProxy?.typecheckFile({
            fileName: "firstpillchecks",
            fileExtension: ".pvs",
            contextFolder: path.join(baseFolder, "pillboxv7")
        });
        // console.dir(response);
        expect(response?.result).not.to.be.undefined;
        expect(response?.error).to.be.undefined;

    }).timeout(20000);

});

