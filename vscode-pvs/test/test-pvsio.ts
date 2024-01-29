import * as fsUtils from "../server/src/common/fsUtils";
import { PvsIoProxy } from '../server/src/pvsioProxy'; // XmlRpcSystemMethods
import { EvalExpressionRequest } from "../server/src/common/serverInterface";
import { PvsResult } from "../server/src/common/pvs-gui";
import { configFile, pvsioExamples } from './test-utils';
import { expect } from 'chai';

//----------------------------
//   Test cases for pvsio
//----------------------------
describe("pvsio", () => {
    let pvsioProxy: PvsIoProxy | undefined = undefined;
    before(async () => {
        const config: string = await fsUtils.readFile(configFile);
        const content: { pvsPath: string } = JSON.parse(config);
        // log(content);
        const pvsPath: string = content.pvsPath;
        // log("Activating xmlrpc proxy...");
        pvsioProxy = new PvsIoProxy(pvsPath, {
            // connection: this.connection, 
            // pvsLibraryPath: this.pvsLibraryPath
        });

        // console.log("\n----------------------");
        // console.log("test-pvsio");
        // console.log("----------------------");
    });
    after(async () => {
        await fsUtils.cleanBin(pvsioExamples);
    });

    const removeWhiteSpace = (str: any): string => {
        if (str && typeof str === "string") {
            return str.replace(/\s/g, "");
        }
        return str;
    }

    it(`can evalute tuples`, async () => {
        // (0, 1, 2, 0)
        const req1: EvalExpressionRequest = {
            contextFolder: pvsioExamples,
            fileName: "bug_tuple",
            fileExtension: ".pvs",
            theoryName: "bug_tuple",
            expr: "LET a = (2,0,2,0), b = f(a) IN b; quit;Y"
        };
        let res1: PvsResult | undefined = await pvsioProxy?.evalExpression(req1);
        expect(res1).not.to.be.undefined;

        console.log({ res1 });
        expect(removeWhiteSpace(res1?.result)).to.be.deep.equal(removeWhiteSpace("(0, 1, 2, 0)"));

        const req2: EvalExpressionRequest = {
            contextFolder: pvsioExamples,
            fileName: "bug_tuple",
            fileExtension: ".pvs",
            theoryName: "bug_tuple",
            expr: "LET a = (2,0,2,0), b = f(a) IN (a,b)`2 = (0,1,2,0); quit;Y"
        };
        let res2: PvsResult | undefined = await pvsioProxy?.evalExpression(req2);
        // console.log({ res2 });
        expect(res2).not.to.be.undefined;
        expect(res2?.result).to.be.deep.equal("TRUE");

    }).timeout(6000);
});
