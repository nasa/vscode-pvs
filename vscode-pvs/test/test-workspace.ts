import * as fsUtils from "../server/src/common/fsUtils";
import { PvsResponse } from "../server/src/common/pvs-gui";
import { PvsProxy } from '../server/src/pvsProxy';
import { mValue2Examples, configFile, sandboxExamples, pigeonhole } from './test-utils';
import { expect } from 'chai';
import * as path from "path";
import { commentRegexp, theoremRegexp } from "../server/src/common/languageUtils";

//----------------------------
//   Test cases for parser
//----------------------------
describe("pvs-proxy", () => {
    let pvsProxy: PvsProxy | undefined = undefined;
    before(async () => {
        const config: string = await fsUtils.readFile(configFile);
        const content: { pvsPath: string } = JSON.parse(config);
        // console.dir(content);
        const pvsPath: string = content.pvsPath;
        // log("Activating xmlrpc proxy...");
        pvsProxy = new PvsProxy(pvsPath, { externalServer: true });
        await pvsProxy?.activate({ debugMode: false, showBanner: false }); // this will also start pvs-server

        // delete pvsbin files and .pvscontext
        await fsUtils.cleanBin(sandboxExamples);

        console.log("\n----------------------");
        console.log("test-workspace");
        console.log("----------------------");
    });
    after(async () => {
        await pvsProxy?.killPvsServer();
        await pvsProxy?.killPvsProxy();
        // delete pvsbin files and .pvscontext
        await fsUtils.cleanBin(sandboxExamples);
    });

    it(`can tell what is the current context and returns a well-formed path`, async () => {
        // Need to clear-theories, in case rerunning with the same server.
        await pvsProxy?.lisp("(clear-theories t)");

        const current: PvsResponse | undefined = await pvsProxy?.currentContext();
        //console.log(current);
        expect(current).not.to.equal(null);
        expect(current).not.to.be.undefined;        
        expect(current?.result).not.to.contain("~"); // tilde should be expanded
        expect(current?.result).to.match(/\/.*/); // path should be absolute, therefore it should start with /
    });

    it(`can change context`, async () => {
        // Need to clear-theories, in case rerunning with the same server.
        await pvsProxy?.emptyAllWorkspaces();

        const home: PvsResponse | undefined = await pvsProxy?.changeContext("~");
        expect(home).not.to.equal(null);
        expect(home).not.to.be.undefined;
        expect(home?.result).not.to.contain("~"); // tilde should be expanded
        expect(home?.result).to.match(/\/.*/); // path should be absolute, therefore it should start with /
    });

    xit(`can find the correct theory name`, async () => {
        // Need to clear-theories, in case rerunning with the same server.
        await pvsProxy?.emptyAllWorkspaces();

        const fname: string = path.join(mValue2Examples, "value.pvs");
        const content: string = await fsUtils.readFile(fname);
        // console.log('content = ', content);
        let theoryName: string | null = "";
        theoryName = fsUtils.findTheoryName(content, 50);
        expect(theoryName).to.equal("test");
        theoryName = fsUtils.findTheoryName(content, 36);
        expect(theoryName).to.equal("value");
        theoryName = fsUtils.findTheoryName(content, 12);
        expect(theoryName).to.equal("HashTable");
        theoryName = fsUtils.findTheoryName(content, 3);
        expect(theoryName).to.equal("HashTableType");
    });

    it(`can find swap_injective and pigeonhole lemmas`, async () => {
        // Need to clear-theories, in case rerunning with the same server.
        await pvsProxy?.lisp("(clear-theories t)");

        const fname: string = path.join(pigeonhole, "pigeonhole.pvs");
        let content: string = await fsUtils.readFile(fname);
        content = content.replace(commentRegexp, "");

        let lemmas: string[] = [];
        const regex: RegExp = new RegExp(theoremRegexp);
        let match: RegExpMatchArray | null = null;
        while (match = regex.exec(content)) {
            if (match.length > 1 && match[1]) {
                const formulaName: string = match[1];
                lemmas.push(formulaName)
            }
        }
        expect(lemmas).to.deep.equal([
            "swap_injective", "pigeonhole"
        ]);
    });
    // it(`change-context is equivalent to change-workspace`, async () => {
    // 	label(`change-context is equivalent to change-workspace`);
    // 	// Need to clear-theories, in case rerunning with the same server.
    // 	await pvsProxy?.lisp("(clear-theories t)");

    // 	const home1: PvsResponse | undefined = await pvsProxy?.pvsRequest("change-context", [ "~" ]);
    // 	const home2: PvsResponse | undefined = await pvsProxy?.changeContext("~");
    // 	expect(home1.result).toEqual(home2.result);
    // });

});

