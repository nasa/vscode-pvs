import * as fsUtils from "../server/src/common/fsUtils";
import { PvsResponse } from "../server/src/common/pvs-gui";
import { PvsProxy } from '../server/src/pvsProxy'; // XmlRpcSystemMethods
import { label, configFile, sandboxExamples } from './test-utils';
import { expect } from 'chai'

//----------------------------
//   Test cases for pvs-proxy
//----------------------------
describe("pvs-proxy", () => {
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

        console.log("\n----------------------");
        console.log("test-proxy");
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

        const success: boolean | undefined = await pvsProxy?.testServerConnectivity();
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
        const response: PvsResponse | undefined = await pvsProxy?.listMethodsRequest();
        const methods: string[] = [
            'add-pvs-library',
            'all-proofs-of-formula',
            'change-context',
            'change-workspace',
            'clear-workspace',
            'delete-proof-of-formula',
            'find-declaration',
            'get-proof-scripts',
            'help',
            'interrupt',
            'interrupt-proof',
            'lisp',
            'list-client-methods',
            'list-methods',
            'names-info',
            'parse',
            'proof-command',
            'proof-help',
            'proof-script',
            'proof-status',
            'prove-formula',
            'prove-tccs',
            'prover-status',
            'quit-all-proof-sessions',
            'reset',
            'save-all-proofs',
            'show-tccs',
            'store-last-attempted-proof',
            'term-at',
            'typecheck'
        ];
        expect(response).not.to.equal(null);
        expect(response).not.to.be.undefined;
        expect(response?.result).to.deep.equal(methods);
    });

    it(`knows client methods`, async () => {
        label(`knows client methods`);
        const response: PvsResponse | undefined = await pvsProxy?.listClientMethods();
        // console.log(response);
        // console.info("listClientMethods: ans = ", ans);
        expect(response).not.to.be.undefined;
        expect(response?.result).to.deep.equal(pvsProxy?.client_methods);
    });

    it(`knows info method`, async () => {
        label(`knows info method`);
        const response: PvsResponse | undefined = await pvsProxy?.lisp('(progn (pvs::pvs-message "i know info") (* 6 7))');
        // console.log(response);
        // console.info("listClientMethods: ans = ", ans);
        expect(response).not.to.be.undefined;
        expect(response).not.to.equal(null);
    });

    it(`clear-theories`, async () => {
        label(`clear-theories`);
        const response: PvsResponse | undefined = await pvsProxy?.lisp('(clear-theories t)');
        // console.log(response);
        // console.info("listClientMethods: ans = ", ans);
        expect(response).not.to.be.undefined;
        expect(response).not.to.equal(null);
    });
});
