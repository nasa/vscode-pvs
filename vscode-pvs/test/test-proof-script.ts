import * as fsUtils from "../server/src/common/fsUtils";
import { PvsResponse, PvsResult } from "../server/src/common/pvs-gui";
import { PvsProxy } from '../server/src/pvsProxy'; // XmlRpcSystemMethods
import { label, log, configFile, sandboxExamples, helloworldExamples } from './test-utils';
import { ProofDescriptor, ProofFile, PvsFormula, PvsTheory } from "../server/src/common/serverInterface";
import * as path from 'path';
import { expect } from 'chai';

//----------------------------
//   Test cases for proofScript
//----------------------------
describe("proofScript", () => {
    let pvsProxy: PvsProxy | undefined = undefined;
    before(async () => {
        const config: string = await fsUtils.readFile(configFile);
        const content: { pvsPath: string } = JSON.parse(config);
        log(content);
        const pvsPath: string = content.pvsPath;
        // log("Activating xmlrpc proxy...");
        pvsProxy = new PvsProxy(pvsPath, { externalServer: true });
        await pvsProxy?.activate({ debugMode: false, showBanner: false }); // this will also start pvs-server

        // delete pvsbin files and .pvscontext
        await fsUtils.cleanBin(sandboxExamples);

        console.log("\n----------------------");
        console.log("test-proof-script");
        console.log("----------------------");
    });
    after(async () => {
        await pvsProxy?.killPvsServer();
        await pvsProxy?.killPvsProxy();
        // delete pvsbin files and .pvscontext
        setTimeout(async () => {
            await fsUtils.cleanBin(sandboxExamples);
        }, 600);
    });

    // utility function, quits the prover if the prover status is active
    const quitProverIfActive = async (): Promise<void> => {
        await pvsProxy?.pvsRequest('quit-all-proof-sessions');
    }

    it(`can open default pvs proof script`, async () => {
        const formula: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "sq",
            formulaName: "sq_neg",
            theoryName: "sq"
        };
        const response: PvsResponse | undefined = await pvsProxy?.getDefaultProofScript(formula);
        //console.dir(response);
        expect(response).not.to.be.undefined;
        expect(response?.error).to.be.undefined;
        expect(response?.result).not.to.be.undefined;
        const proof_header: string = response?.result.split("\n")[0];
        expect(proof_header).to.deep.equal(`;;; Proof sq_neg-1 for formula sq.sq_neg`);
    });

    it(`returns a well-formed empty pvs proof script when the proof file is not available`, async () => {
        // a well-formed response for formula sqrt_0 is in the form /;;; Proof sqrt_0(\-\d+)? for formula sqrt.sqrt_0\n(""\s*)/
        const formula: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "sqrt",
            formulaName: "sqrt_0",
            theoryName: "sqrt"
        };
        const response: PvsResponse | undefined = await pvsProxy?.getDefaultProofScript(formula);
        // console.dir(response);
        expect(response).not.to.be.undefined;
        expect(response?.result).to.be.undefined;
        expect(response?.error).not.to.be.undefined;
        expect(response?.error.data.error_string).to.match(/(.*) does not have a proof/);

        const formula1: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "alaris2lnewmodes.pump",
            formulaName: "vtbi_over_rate_lemma",
            theoryName: "pump_th"
        }
        const response1: PvsResponse | undefined = await pvsProxy?.getDefaultProofScript(formula1);
        // console.dir(response1);
        expect(response1).not.to.be.undefined;
        expect(response1?.result).to.be.undefined;
        expect(response1?.error).not.to.be.undefined;
        expect(response1?.error.data.error_string).to.match(/(.*) does not have a proof/);

        const formula3: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "sq",
            formulaName: "sq_plus_pos",
            theoryName: "sq"
        };
        await pvsProxy?.changeContext(formula3);
        await pvsProxy?.typecheckFile(formula3);
        const response3: PvsResponse | undefined = await pvsProxy?.getDefaultProofScript(formula3);
        // const response3: PvsResponse | undefined = await pvsProxy?.lisp(`(get-default-proof-script "${formula3.theoryName}" "${formula3.formulaName}")`);
        // console.dir(response3?.result);
        expect(response3).not.to.be.undefined;
        expect(response3?.result).to.be.undefined;
        expect(response3?.error).not.to.be.undefined;
        expect(response3?.error.data.error_string).to.match(/(.*) does not have a proof/);
    }).timeout(10000);

    it(`can load & save vscode-pvs proof files`, async () => {
        await quitProverIfActive();

        const formula: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "sq",
            formulaName: "sq_plus_eq_0",
            theoryName: "sq"
        };
        let response: PvsResponse | undefined = await pvsProxy?.proveFormula(formula);
        expect(response).not.to.be.undefined;
        if(response != undefined){
            let prfid: string = response?.result.id;
            response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: "(grind)" });
            expect(response).not.to.be.undefined;
            response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: "(quit)" });
            expect(response).not.to.be.undefined;
            // console.dir(response);
            response = await pvsProxy?.storeLastAttemptedProof(formula);
            expect(response).not.to.be.undefined;
            expect(response?.result).not.to.be.undefined;
            // #TODO @M3 add check to see the prf file has been saved with a new proof with the id above
        }
    });

    // this test fails: save proof saves an empty proof in the prf file for vtbi_over_rate_lemma.
    // additionally, pvs-server takes 100% of the cpu at the end of this test
    it(`saves only the current proof, and leaves all other proofs untouched`, async () => {
        await quitProverIfActive();
        const formula1: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "alaris2lnewmodes.pump",
            formulaName: "vtbi_over_rate_lemma",
            theoryName: "pump_th"
        }
        let response1: PvsResponse | undefined = await pvsProxy?.getDefaultProofScript(formula1);
        // console.dir(response1);
        expect(response1?.result).to.be.undefined;
        expect(response1?.error).not.to.be.undefined;
        expect(response1?.error.data.error_string).to.match(/(.*) does not have a proof/);

        await quitProverIfActive();
        const formula: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "sq",
            formulaName: "sq_plus_eq_0",
            theoryName: "sq"
        };
        let response: PvsResponse | undefined = await pvsProxy?.proveFormula(formula);
        expect(response).not.to.be.undefined;

        if(response != undefined){

            let prfid: string = response.result.id;

            response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: "(grind)" });
            expect(response).not.to.be.undefined;
            response = await pvsProxy?.proofCommand({ proofId: prfid, cmd: "(quit)" });
            expect(response).not.to.be.undefined;
            // console.dir(response);
            response = await pvsProxy?.storeLastAttemptedProof(formula);
            expect(response).not.to.be.undefined;
    
            response1 = await pvsProxy?.getDefaultProofScript(formula1);
            expect(response1).not.to.be.undefined;
            // console.dir(response1);
            expect(response1?.result).to.be.undefined;
            expect(response1?.error).not.to.be.undefined;
            expect(response1?.error.data.error_string).to.match(/(.*) does not have a proof/);
    
        }
    });

    it(`can generate backup files if vscode-pvs proof file is corrupted`, async () => {
        label(`can generate backup files if vscode-pvs proof file is corrupted`);

        // create a corrupted jprf file
        const fname: string = fsUtils.desc2fname({
            contextFolder: sandboxExamples,
            fileExtension: ".jprf",
            fileName: "sq"
        });
        const content: string = await fsUtils.readFile(fsUtils.desc2fname({
            contextFolder: sandboxExamples,
            fileExtension: ".jprf.corr",
            fileName: "sq"
        }));
        await fsUtils.writeFile(fname, content);

        // try to load a proof from the corrupted file
        const formula: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "sq",
            formulaName: "sq_plus_eq_0",
            theoryName: "sq"
        };
        const proofDescriptor: ProofDescriptor | undefined = await pvsProxy?.openProofFile({
            contextFolder: formula.contextFolder,
            fileName: formula.fileName,
            fileExtension: ".jprf"
        }, formula, { quiet: true });
        expect(proofDescriptor).not.to.be.undefined;

        const backup: string = await fsUtils.readFile(`${fname}.err`);
        expect(backup).not.to.be.undefined;
        expect(backup).to.deep.equal(content);
        // console.log(backup);

        const msg: string = await fsUtils.readFile(`${fname}.err.msg`);
        expect(msg).not.to.be.undefined;
        expect(msg).to.deep.equal("Expected ',' or ']' after array element in JSON at position 36");
        // console.log(msg);

        fsUtils.deleteFile(fname);
        fsUtils.deleteFile(`${fname}.err`);
        fsUtils.deleteFile(`${fname}.err.msg`);
    });

    it(`edit-proof-at and get-default-proof-script are equivalent`, async () => {
        const desc: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "sq",
            formulaName: "sq_neg",
            theoryName: "sq"
        };
        const fname: string = fsUtils.desc2fname(desc);
        const line: number = 12;
        await pvsProxy?.changeContext(desc);
        await pvsProxy?.typecheckFile(desc);
        const cmd: string = `(edit-proof-at "${fname}" nil ${line} "pvs" "${desc.fileName}${desc.fileExtension}" 0 nil)`;
        const response1: PvsResponse | undefined = await pvsProxy?.lisp(cmd);
        const response2: PvsResponse | undefined = await pvsProxy?.getDefaultProofScript(desc);
        // const response2: PvsResponse | undefined = await pvsProxy?.lisp(`(get-default-proof-script "${desc.theoryName}" "${desc.formulaName}")`);
        //console.dir(response1?.result);
        //console.dir(response2.result);
        expect(response1).not.to.be.undefined;
        expect(response2).not.to.be.undefined;
        expect(response1?.result).to.deep.equal(response2?.result);
    });

    it(`can provide default proof script for tccs`, async () => {
        const desc: PvsFormula = {
            contextFolder: sandboxExamples,
            fileExtension: ".pvs",
            fileName: "sq",
            formulaName: "sq_TCC1",
            theoryName: "sq"
        };
        await pvsProxy?.changeContext(desc);
        // const response: PvsResponse | undefined = await pvsProxy?.lisp(`(get-default-proof-script "${desc.theoryName}" "${desc.formulaName}")`);
        const response: PvsResponse | undefined = await pvsProxy?.getDefaultProofScript(desc);
        // console.dir(response);
        expect(response).not.to.be.undefined;
        expect(response?.error).to.be.undefined;
        expect(response?.result).not.to.be.undefined;
        expect(response?.result.toLowerCase()).to.contain(`("" (subtype-tcc))`)
        // console.dir(response.result);
    });

    it(`can open default pvs proof script when the formula has no proof script saved in the prf file`, async () => {
        const formula: PvsFormula = {
            contextFolder: helloworldExamples,
            fileExtension: ".pvs",
            fileName: "helloworld",
            theoryName: "helloworld",
            formulaName: "dummy"
        };
        // const response: PvsResponse | undefined = await pvsProxy?.getDefaultProofScript(formula);
        let response: PvsResponse | undefined = await pvsProxy?.lisp(`(change-workspace "${formula.contextFolder}")`);
        response = await pvsProxy?.lisp(`(typecheck-file "${fsUtils.desc2fname(formula)}" nil nil nil nil t)`);
        response = await pvsProxy?.lisp(`(get-default-proof-script "helloworld" "dummy")`);
        // console.log(response);
        expect(response).not.to.be.undefined;
        expect(response?.result.toLowerCase()).to.equal('nil');
    });

    it(`can open proofs with comments`, async () => {
        const formula: PvsFormula = {
            contextFolder: helloworldExamples,
            fileExtension: ".pvs",
            fileName: "dummy",
            theoryName: "dummy",
            formulaName: "withComments"
        };
        // const response: PvsResponse | undefined = await pvsProxy?.getDefaultProofScript(formula);
        let response: PvsResponse | undefined = await pvsProxy?.lisp(`(change-workspace "${formula.contextFolder}")`);
        response = await pvsProxy?.lisp(`(typecheck-file "${fsUtils.desc2fname(formula)}" nil nil nil nil t)`);
        let lisp: PvsResponse | undefined = await pvsProxy?.lisp(`#+allegro "allegro" #+sbcl "sbcl"`);
        expect(lisp).not.to.be.undefined;
        const proofDescriptor: ProofDescriptor | undefined = await pvsProxy?.openProofFile({
            contextFolder: formula.contextFolder,
            fileName: formula.fileName,
            fileExtension: ".prf"
        }, formula, { quiet: true });
        expect(proofDescriptor).not.to.be.undefined;
        expect(proofDescriptor?.origin).to.deep.equal(".prf");
        if (lisp?.result == 'allegro') {
            expect(proofDescriptor?.proofTree).to.be.deep.equal({
                name: 'dummy.withComments',
                rules: [
                    {
                        name: '(skosimp*)',
                        rules: [],
                        type: 'proof-command',
                        branch: ''
                    },
                    {
                        name: '(comment "Suppose proc(alpha)=p!1, but this is impossible.")',
                        rules: [],
                        type: 'proof-command',
                        branch: ''
                    },
                    {
                        name: '(case "proc!1(alpha!1)=p!1")',
                        rules: [
                            {
                                name: '1',
                                rules: [
                                    {
                                        name: '(propax)',
                                        rules: [],
                                        type: 'proof-command',
                                        branch: '1'
                                    }
                                ],
                                type: 'proof-branch',
                                branch: '1'
                            },
                            {
                                name: '2',
                                rules: [
                                    {
                                        name: '(postpone)',
                                        rules: [],
                                        type: 'proof-command',
                                        branch: '2'
                                    }
                                ],
                                type: 'proof-branch',
                                branch: '2'
                            }
                        ],
                        type: 'proof-command',
                        branch: ''
                    }
                ],
                type: 'root',
                branch: ''
            });
        } else {
            expect(proofDescriptor?.proofTree).to.be.deep.equal({
                name: 'dummy.withComments',
                rules: [
                    {
                        name: '(SKOSIMP*)',
                        rules: [],
                        type: 'proof-command',
                        branch: ''
                    },
                    {
                        name: '(COMMENT "Suppose proc(alpha)=p!1, but this is impossible.")',
                        rules: [],
                        type: 'proof-command',
                        branch: ''
                    },
                    {
                        name: '(CASE "proc!1(alpha!1)=p!1")',
                        rules: [
                            {
                                name: '1',
                                rules: [
                                    {
                                        name: '(PROPAX)',
                                        rules: [],
                                        type: 'proof-command',
                                        branch: '1'
                                    }
                                ],
                                type: 'proof-branch',
                                branch: '1'
                            },
                            {
                                name: '2',
                                rules: [
                                    {
                                        name: '(POSTPONE)',
                                        rules: [],
                                        type: 'proof-command',
                                        branch: '2'
                                    }
                                ],
                                type: 'proof-branch',
                                branch: '2'
                            }
                        ],
                        type: 'proof-command',
                        branch: ''
                    }
                ],
                type: 'root',
                branch: ''
            });
        }
    });
});
