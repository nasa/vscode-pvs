import * as fsUtils from "../server/src/common/fsUtils";
import { PvsResponse } from "../server/src/common/pvs-gui";
import { PvsProxy } from '../server/src/pvsProxy'; // XmlRpcSystemMethods
import {
    configFile, sandboxExamples,
    stever, steverFiles, pillbox, pillboxFiles, pvsioweb, pvsiowebFiles, pvsiowebFolders,
    dependable_plus_safe,
    mValueExamples,
    label
} from './test-utils';
import * as path from 'path';
import { expect } from 'chai';

//----------------------------
//   Test cases for typechecker
//----------------------------
describe("pvs-typechecker", () => {
    let pvsProxy: PvsProxy | undefined = undefined;
    before(async () => {
        const config: string = await fsUtils.readFile(configFile);
        const content: { pvsPath: string } = JSON.parse(config);
        // console.log(content);
        const pvsPath: string = content.pvsPath;
        // log("Activating xmlrpc proxy...");
        pvsProxy = new PvsProxy(pvsPath, { externalServer: true });
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
        console.log("test-typechecker");
        console.log("----------------------");
    });
    after(async () => {
        await pvsProxy?.killPvsServer();
        await pvsProxy?.killPvsProxy();
        // delete pvsbin files and .pvscontext
        await new Promise<void>((resolve, reject) => {
            setTimeout(async () => {
                try {
                    await fsUtils.cleanBin(sandboxExamples);
                    await fsUtils.cleanBin(stever);
                    await fsUtils.cleanBin(pillbox);
                    for (let i = 0; i < pvsiowebFolders.length; i++) {
                        await fsUtils.cleanBin(path.join(pvsioweb, pvsiowebFolders[i]));
                    }
                    await fsUtils.cleanBin(dependable_plus_safe);
                } catch (err) {
                    console.log('cleanBin error: ', err);
                }
                resolve();
            }, 1500);
        });
    });

    it(`can typecheck files`, async () => {
        label(`can typecheck files`);
        const response: PvsResponse | undefined = await pvsProxy?.typecheckFile({ fileName: "sqrt", fileExtension: ".pvs", contextFolder: sandboxExamples });
        // console.dir(response);
        expect(response).not.to.equal(undefined);
        expect(response?.result).not.to.equal(undefined);
    }).timeout(100000);

    it(`can report typecheck errors in imported files`, async () => {
        label(`can report typecheck errors in imported files`);
        const response: PvsResponse | undefined = await pvsProxy?.typecheckFile({ fileName: "test", fileExtension: ".pvs", contextFolder: sandboxExamples });
        // console.dir(response);
        expect(response).not.to.be.undefined;
        expect(response?.result).not.to.equal(null);
        expect(response?.error.data.error_string).to.include("Incompatible types");
        expect(response?.error.data.file_name).to.equal(path.join(sandboxExamples, "test.pvs"));
        expect(response?.error.data.place).to.deep.equal([15, 31, 15, 38]);
        // console.dir(response.error.data);
    }).timeout(100000);

    it(`can typecheck theories with parameters`, async () => {
        label(`can typecheck theories with parameters`);
        let desc = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "alaris2lnewmodes.pump",
            formulaName: "vtbi_over_rate_lemma",
            line: 28,
            theoryName: "pump_th"
        };

        let response: PvsResponse | undefined = await pvsProxy?.typecheckFile(desc);
        expect(response).not.to.be.undefined;
        expect(response?.result).not.to.be.undefined;
        expect(response?.error).to.be.undefined;

    }).timeout(10000);

    it(`can typecheck pvs files that import other files`, async () => {
        label(`can typecheck pvs files that import other files`);
        // const response: PvsResponse | undefined = await pvsProxy?.typecheckFile({ fileName: "sqrt", fileExtension: ".pvs", contextFolder: sandboxExamples });
        const response: PvsResponse | undefined = await pvsProxy?.typecheckFile({ fileName: "main", fileExtension: ".pvs", contextFolder: sandboxExamples });
        console.dir(response);
        expect(response).not.to.be.undefined;
        //expect(response.result).toEqual(test.typecheck1_result);
    }).timeout(10000);

    it(`can generate .tcc file content`, async () => {
        label(`can generate .tcc file content`);
        const response: PvsResponse | undefined = await pvsProxy?.generateTccsFile({
            fileName: "sqrt",
            fileExtension: ".pvs",
            theoryName: "sqrt",
            contextFolder: sandboxExamples
        });
        // console.log("response", response);
        expect(response).not.to.be.undefined;
        expect(response?.error).to.be.undefined;
        expect(response?.result).not.to.equal(null);

        const response1: PvsResponse | undefined = await pvsProxy?.generateTccsFile({
            fileName: "alaris2lnewmodes",
            fileExtension: ".pvs",
            theoryName: "alaris_th",
            contextFolder: sandboxExamples
        });
        // console.log("response1", response1);
        expect(response1?.error).to.be.undefined;
        expect(response1?.result).not.to.equal(null);
    }).timeout(10000);

    it(`can typecheck files in folders whose name contains utf8 symbols`, async () => {
        label(`can typecheck files in folders whose name contains utf8 symbols`);

        await pvsProxy?.emptyAllWorkspaces();

        const response: PvsResponse | undefined = await pvsProxy?.typecheckFile({
            fileName: "helloworld",
            fileExtension: ".pvs",
            contextFolder: dependable_plus_safe
        });
        expect(response).not.to.be.undefined;
        expect(response?.result).not.to.be.undefined;
        expect(response?.error).to.be.undefined;
    }).timeout(100000);

    //-----------------------
    // additional test cases
    //-----------------------
    for (let i = 0; i < steverFiles.length; i++) {
        it(`can typecheck stever/${steverFiles[i]}.pvs`, async () => {
        label(`can typecheck stever/${steverFiles[i]}.pvs`);
            // Need to clear-theories, in case rerunning with the same server.
            await pvsProxy?.emptyAllWorkspaces();

            const response: PvsResponse | undefined = await pvsProxy?.typecheckFile({
                fileName: steverFiles[i],
                fileExtension: ".pvs",
                contextFolder: stever
            });
            // console.dir(response);
            expect(response).not.to.be.undefined;
            expect(response?.error).to.be.undefined;

        }).timeout(40000);

    }
    for (let i = 0; i < pillboxFiles.length; i++) {
        it(`can typecheck pillbox/${pillboxFiles[i]}.pvs`, async () => {
        label(`can typecheck pillbox/${pillboxFiles[i]}.pvs`);
            // Need to clear-theories, in case rerunning with the same server.
            await pvsProxy?.emptyAllWorkspaces();

            const response: PvsResponse | undefined = await pvsProxy?.typecheckFile({
                fileName: pillboxFiles[i],
                fileExtension: ".pvs",
                contextFolder: pillbox
            });
            // console.dir(response);
            expect(response).not.to.be.undefined;
            expect(response?.error).to.be.undefined;

        }).timeout(40000);
    }
    for (let i = 0; i < pvsiowebFiles.length; i++) {
        //(let i = 0; i < pvsiowebFiles.length; i++) {
        // 34, 45 signal; 0, len ok if 44 excluded
        it(`can typecheck pvsioweb/${pvsiowebFiles[i]}.pvs`, async () => {
        label(`can typecheck pvsioweb/${pvsiowebFiles[i]}.pvs`);
            // Need to clear-theories, in case rerunning with the same server.
            await pvsProxy?.emptyAllWorkspaces();
            if (i != 50) {
                const response: PvsResponse | undefined = await pvsProxy?.typecheckFile({
                    fileName: pvsiowebFiles[i],
                    fileExtension: ".pvs",
                    contextFolder: pvsioweb
                });
                expect(response).not.to.be.undefined;
                if (pvsiowebFiles[i].endsWith("MDNumberpaddd")) {
                    expect(response?.error).not.to.be.undefined; // theory 'limits' declared in twice in the same workspace
                } else {
                    if (!response?.result) {
                        console.log(`[test tc] ${i}: ${pvsiowebFiles[i]}`);
                        console.dir(response);
                    }
                    expect(response?.result).not.to.be.undefined;
                    expect(response?.error).to.be.undefined;
                }
            }

        }).timeout(60000);
    }


});
