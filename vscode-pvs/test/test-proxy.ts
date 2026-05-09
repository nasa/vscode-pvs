import * as fsUtils from "../server/src/common/fsUtils";
import { PvsResponse } from "../server/src/common/pvs-gui";
import { PvsFormula } from "../server/src/common/serverInterface";
import { DEFAULT_PVS_WEBSOCKET_PORT, PvsProxy } from '../server/src/pvsProxy'; // XmlRpcSystemMethods
import { label, configFile, sandboxExamples } from './test.utils';
import { expect } from 'chai'

import * as path from 'path';
export const helloworldExamples: string = path.join(__dirname, "helloworld");


//----------------------------
//   Test cases for pvs-proxy
//   NOTE: to run this test, please manually launch an instance of the PVS server (./pvs -raw -port 23456)
//----------------------------
describe("pvs-proxy", () => {
    let pvsProxy: PvsProxy;
    before(async () => {
        const config: string = await fsUtils.readFile(configFile);
        const content: { pvsPath: string } = JSON.parse(config);
        // console.log(content);
        const pvsPath: string = content.pvsPath;
        // log("Activating xmlrpc proxy...");
        pvsProxy = new PvsProxy(pvsPath, {
            externalServer: true,
            webSocketPort: DEFAULT_PVS_WEBSOCKET_PORT
        });
        await pvsProxy?.activate({ debugMode: false, showBanner: false }); // this will also start pvs-server

        // delete pvsbin files and .pvscontext
        await fsUtils.cleanBin(sandboxExamples);

        console.log("\n----------------------");
        console.log("test-proxy");
        console.log("NOTE: prior to running this test, an instance of the PVS server needs to be launched manually (./pvs -raw -port 23456)")
        console.log("----------------------");
    });
    after(async () => {
        await pvsProxy?.killPvsServer();
        await pvsProxy?.killPvsProxy();
        // delete pvsbin files and .pvscontext
        await fsUtils.cleanBin(sandboxExamples);
    });

    //-----------------------------------
    // console.info("Starting test case execution");
    //-----------------------------------

    it(`can be started correctly`, async () => {
        label(`can be started correctly`);
        expect(pvsProxy).not.to.equal(null);

        const success: boolean = await pvsProxy?.testServerConnectivity();
        expect(success).to.equal(true);
    });

    // it(`can list xml-rpc system methods`, async () => {
    //     label(`can list xml-rpc system methods`);
    //     const ans: string[] = await pvsProxy?.listSystemMethods();
    //     expect(ans).not.to.equal(null);
    //     // expect(ans.error).to.equal(null);
    //     // expect(ans.result).not.to.equal(null);
    //     // expect(ans.result.length).to.equal(4);
    //     // expect(ans.result.filter((mth: string) => {
    //     // return mth === "pvs.request";
    //     return '';
    // });

    it(`can list pvs-server methods`, async () => {
        label(`can list pvs-server methods`);
        const response: PvsResponse = await pvsProxy?.listMethodsRequest();
        const methods: string[] = [
            'add-prover-hook',
            'add-pvs-library',
            'all-proofs-of-formula',
            'change-context',
            'change-workspace',
            'clear-workspace',
            'collect-theory-usings',
            'delete-proof-of-formula',
            'find-declaration',
            'get-proof-scripts',
            'help',
            'interrupt',
            'interrupt-proof',
            'latex-importchain',
            'latex-pvs-file',
            'latex-theory',
            'lisp',
            'list-client-methods',
            'list-methods',
            'mark-proof-as-default',
            'names-info',
            'parse',
            'proof-command',
            'proof-help',
            'proof-script',
            'proof-status',
            'prove-formula',
            'prove-tccs',
            'prover-status',
            'pvsio-eval',
            'pvsio-start',
            'quit-all-proof-sessions',
            'reset',
            'save-all-proofs',
            'show-tccs',
            'term-at',
            'typecheck'
        ];
        // console.dir({ response, methods });
        expect(response).not.to.equal(null);
        expect(response).not.to.be.undefined;
        expect(response?.result).to.deep.equal(methods);
    });

    it(`knows client methods`, async () => {
        label(`knows client methods`);
        const response: PvsResponse = await pvsProxy?.listClientMethods();
        // console.log(response);
        // console.info("listClientMethods: ans = ", ans);
        expect(response).not.to.be.undefined;
        expect(response?.result).to.deep.equal(pvsProxy?.client_methods);
    });

    it(`knows info method`, async () => {
        label(`knows info method`);
        const response: PvsResponse = await pvsProxy?.lisp('(progn (pvs::pvs-message "i know info") (* 6 7))');
        // console.log(response);
        // console.info("listClientMethods: ans = ", ans);
        expect(response).not.to.be.undefined;
        expect(response).not.to.equal(null);
    });

    it(`clear-theories`, async () => {
        label(`clear-theories`);
        const response: PvsResponse = await pvsProxy?.lisp('(clear-theories t)');
        // console.log(response);
        // console.info("listClientMethods: ans = ", ans);
        expect(response).not.to.be.undefined;
        expect(response).not.to.equal(null);
    });

    /**
     * This test fails intermittently with the following error
        error: {
            code: -32700,
            message: 'PVS Error',
            data: 'Lock on package COMMON-LISP violated when interning assert while in package PVS.\n' +
                'See also:\n' +
                '  The SBCL Manual, Node "Package Locks"\n' +
                '  The ANSI Standard, Section 11.1.2.1.2'
        }
     */
    it(`getProofScriptsInPrfFile returns a valid result.`, async () => {
        const formula: PvsFormula = {
            contextFolder: helloworldExamples,
            fileExtension: ".pvs",
            fileName: "helloworld",
            formulaName: "sqrt2",
            theoryName: "helloworld"
        };
        const res: PvsResponse = await pvsProxy?.getProofScriptsInPrfFile(formula.contextFolder + "/" + formula.fileName);
        console.dir({ res }, { depth: null });
        expect(res).not.to.be.undefined;
        expect(res?.error).to.be.undefined;
    });


    /**
     * This test fails intermittently with the following errors
        error: {
            code: -32700,
            message: 'PVS Error',
            data: 'Lock on package COMMON-LISP violated when interning assert while in package PVS.\n' +
            'See also:\n' +
            '  The SBCL Manual, Node "Package Locks"\n' +
            '  The ANSI Standard, Section 11.1.2.1.2'
        }
        
        error: {
                code: -32700,
                message: 'PVS Error',
                data: 'The value\n  NIL\nis not of type\n  PVS::CONTEXT-ENTRY'
            },
            id: 'f2bbc74e980d6d473617656fcecdefc7dcc9e3b90e22525e7b0c5a6694b36037',
            jsonrpc: '2.0'
        }
     */
    it(`getDefaultProofScript returns the intended error nessage when the proof file is not available`, async () => {
        const formula: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "sqrt",
            formulaName: "sqrt_0",
            theoryName: "sqrt"
        };
        const response: PvsResponse = await pvsProxy?.getDefaultProofScript(formula);
        console.dir(response, { depth: null });
        expect(response).not.to.be.undefined;
        expect(response?.result).to.be.undefined;
        expect(response?.error).not.to.be.undefined;
        expect(response?.error?.message).to.deep.equal("Proof script error");
        expect(response?.error?.data?.error_string).to.match(/(.*) does not have a proof/);
    });
});
