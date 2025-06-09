import * as fsUtils from "../server/src/common/fsUtils";
import { PvsResponse, PvsResult } from "../server/src/common/pvs-gui";
import { PvsProxy } from '../server/src/pvsProxy'; // XmlRpcSystemMethods
import {
    label, configFile, sandboxExamples,
    stever, steverFiles, pillbox, pillboxFiles, pvsioweb, pvsiowebFiles, pvsiowebFolders,
    dependable_plus_safe
} from './test-utils';
import * as path from 'path';
import { expect } from 'chai';

//----------------------------
//   Test cases for parser
//----------------------------
describe("pvs-parser", () => {
    let pvsProxy: PvsProxy | undefined = undefined;
    before(async () => {
        const config: string = await fsUtils.readFile(configFile);
        const content: { pvsPath: string } = JSON.parse(config);
        // console.log(content);
        const pvsPath: string = content.pvsPath;
        // log("Activating xmlrpc proxy...");
        // NOTE: we need to use the lisp interface because the server breaks with parse
        pvsProxy = new PvsProxy(pvsPath, { externalServer: false });
        await pvsProxy?.activate({ debugMode: false, showBanner: false }); // this will also start pvs-server

        // delete pvsbin files and .pvscontext
        await fsUtils.cleanBin(sandboxExamples);
        await fsUtils.cleanBin(stever);
        await fsUtils.cleanBin(pillbox);
        for (let i = 0; i < pvsiowebFolders.length; i++) {
            await fsUtils.cleanBin(path.join(pvsioweb, pvsiowebFolders[i]));
        }
        await fsUtils.cleanBin(dependable_plus_safe);

        console.log("\n----------------------");
        console.log("test-parser");
        console.log("----------------------");
    });
    after(async () => {
        await pvsProxy?.killPvsServer();
        await pvsProxy?.killPvsProxy();
        // delete pvsbin files and .pvscontext
        await fsUtils.cleanBin(sandboxExamples);
        await fsUtils.cleanBin(stever);
        await fsUtils.cleanBin(pillbox);
        for (let i = 0; i < pvsiowebFolders.length; i++) {
            await fsUtils.cleanBin(path.join(pvsioweb, pvsiowebFolders[i]));
        }
        await fsUtils.cleanBin(dependable_plus_safe);
    });

    // it(`can be started correctly`, async () => {
    //     label(`can be started correctly`);
    //     expect(pvsProxy).not.to.equal(null);

    //     const success: boolean = await pvsProxy?.testServerConnectivity();
    //     expect(success).to.equal(true);
    // });

    it(`can parse file`, async () => {
        label(`can parse file`);
        // Need to clear-theories, in case rerunning with the same server.
        await pvsProxy?.emptyAllWorkspaces();
        const response: PvsResponse | null | undefined = await pvsProxy?.parseFile({ fileName: "sqrt", fileExtension: ".pvs", contextFolder: sandboxExamples }, { test: true });
        //console.dir(response);
        expect(response).not.to.be.undefined;
        expect(response).not.to.be.null;
        expect(response?.result).not.to.be.undefined;
        expect(response?.error).to.be.undefined;
    }).timeout(4000);

    it(`can report parse errors`, async () => {
        label(`can report parse errors`);

        const response: PvsResult | undefined | null = await pvsProxy?.parseFile({ fileName: "lib0", fileExtension: ".pvs", contextFolder: sandboxExamples }, { test: true });
        //console.dir(response);
        const exp = {
            code: 1,
            message: 'Parser error',
            data: {
                error_string: 'Found \'[#\' when expecting \'END\'\nIn file /home/owre/vscode-pvs/server/test/sandbox/lib0.pvs (line 3, col 14)',
                file_name: '/home/owre/vscode-pvs/server/test/sandbox/lib0.pvs',
                place: [3, 14, 3, 14]
            }
        };
        expect(response).not.to.be.undefined;
        expect(response).not.to.be.null;
        expect(response?.error).not.to.be.undefined;
        expect(response?.error.data).not.to.be.undefined;
        expect(response?.error.data.place).to.deep.equal(exp.data.place);
        expect(response?.error.data.error_string.startsWith(exp.data.error_string.split("\n")[0])).to.equal(true);
    }).timeout(100000);

    it(`can parse file with inline declarations`, async () => {
        label(`can parse file with inline declarations`);
        // Need to clear-theories, in case rerunning with the same server.
        await pvsProxy?.emptyAllWorkspaces();

        const response: PvsResponse | undefined | null = await pvsProxy?.parseFile({ fileName: "test", fileExtension: ".pvs", contextFolder: sandboxExamples }, { test: true });
        // console.dir(response);
        expect(response).not.to.be.undefined;
        expect(response).not.to.be.null;
    }).timeout(100000);

    it(`can parse file when filename contains '.'`, async () => {
        label(`can parse file when filename contains '.'`);

        let response: PvsResponse | undefined | null = await pvsProxy?.parseFile({ fileName: "alaris2lnewmodes.pump", fileExtension: ".pvs", contextFolder: sandboxExamples }, { test: true });
        // console.dir(response);
        expect(response).not.to.be.null;
        expect(response).not.to.be.undefined;
        expect(response?.result).not.to.be.undefined;
        expect(response?.error).to.be.undefined;
    }).timeout(100000);

    it(`can parse files in folders whose name contains utf8 symbols`, async () => {
        label(`can parse files in folders whose name contains utf8 symbols`);

        const response: PvsResponse | undefined | null = await pvsProxy?.parseFile({
            fileName: "helloworld",
            fileExtension: ".pvs",
            contextFolder: dependable_plus_safe
        }, { test: true });
        // console.dir(response);
        expect(response).not.to.be.undefined;
        expect(response).not.to.be.null;
        expect(response?.result).not.to.be.undefined;
        expect(response?.error).to.be.undefined;
    }).timeout(100000);

    it(`is robust when asked to parse file that does not exist / is not readable`, async () => {
        label(`is robust when asked to parse file that does not exist / is not readable`);

        let response: PvsResponse | undefined | null = await pvsProxy?.parseFile({
            fileName: "foo",
            fileExtension: ".pvs",
            contextFolder: sandboxExamples
        }, { test: true });
        // console.dir(response);
        expect(response).not.to.be.undefined;
        expect(response).not.to.be.null;
        expect(response?.result).to.be.undefined;
        expect(response?.error).not.to.be.undefined;
    }).timeout(100000);


    //-----------------------
    // additional test cases
    //-----------------------
    for (let i = 0; i < steverFiles.length; i++) {
        it(`can parse stever/${steverFiles[i]}.pvs`, async () => {
            label(`can parse stever/${steverFiles[i]}.pvs`);
            // Need to clear-theories, in case rerunning with the same server.
            await pvsProxy?.emptyAllWorkspaces();

            const response: PvsResponse | undefined | null = await pvsProxy?.parseFile({
                fileName: steverFiles[i],
                fileExtension: ".pvs",
                contextFolder: stever
            }, { test: true });
            //console.dir(response);
            expect(response).not.to.be.undefined;
            expect(response).not.to.be.null;
            expect(response?.result).not.to.be.undefined;
            expect(response?.error).to.be.undefined;
        }).timeout(8000);
    }

    for (let i = 0; i < pillboxFiles.length; i++) {
        it(`can parse pillboxv7/${pillboxFiles[i]}.pvs`, async () => {
            label(`can parse pillboxv7/${pillboxFiles[i]}.pvs`);
            // Need to clear-theories, in case rerunning with the same server.
            await pvsProxy?.emptyAllWorkspaces();

            const response: PvsResponse | undefined | null = await pvsProxy?.parseFile({
                fileName: pillboxFiles[i],
                fileExtension: ".pvs",
                contextFolder: pillbox
            }, { test: true });
            // console.dir(response);
            expect(response).not.to.be.undefined;
            expect(response).not.to.be.null;
            expect(response?.result).not.to.be.undefined;
            expect(response?.error).to.be.undefined;
        }).timeout(8000);
    }

    for (let i = 0; i < pvsiowebFiles.length; i++) {
        it(`can parse pvsioweb/${pvsiowebFiles[i]}.pvs`, async () => {
            label(`can parse pvsioweb/${pvsiowebFiles[i]}.pvs`);
            // Need to clear-theories, in case rerunning with the same server.
            await pvsProxy?.emptyAllWorkspaces();

            const response: PvsResponse | undefined | null = await pvsProxy?.parseFile({
                fileName: pvsiowebFiles[i],
                fileExtension: ".pvs",
                contextFolder: pvsioweb
            }, { test: true });
            expect(response).not.to.be.undefined;
            expect(response).not.to.be.null;
            //console.dir(response);
            if (pvsiowebFiles[i].endsWith("baxter/limitsss") ||
                (pvsiowebFiles[i].endsWith("baxter/constantsss"))) {
                console.dir(response, { depth: null });
                expect(response?.error).not.to.be.undefined; // theory 'limits' declared in twice in the same workspace
            } else {
                expect(response?.result).not.to.be.undefined;
                expect(response?.error).to.be.undefined;
            }
        }).timeout(8000);
    }

});

