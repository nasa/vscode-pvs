"use strict";
/**
 * @module serverInterface
 * @author Paolo Masci
 * @date 2019.06.18
 * @copyright
 * Copyright 2019 United States Government as represented by the Administrator
 * of the National Aeronautics and Space Administration. All Rights Reserved.
 *
 * Disclaimers
 *
 * No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY
 * WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY,
 * INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE
 * WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM
 * INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR
 * FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO
 * THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER,
 * CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT
 * OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY
 * OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.
 * FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES
 * REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE,
 * AND DISTRIBUTES IT "AS IS."
 *
 * Waiver and Indemnity: RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS
 * AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND
 * SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF
 * THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES,
 * EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM
 * PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT
 * SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED
 * STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY
 * PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE
 * REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL
 * TERMINATION OF THIS AGREEMENT.
 **/
Object.defineProperty(exports, "__esModule", { value: true });
exports.PvsDocKind = exports.quickFixAddImportingCommand = exports.quickFixReplaceCommand = exports.NASALibGithubFile = exports.NASALibGithubBranch = exports.NASALibUrl = exports.pvsDownloadUrl = exports.pvsUrl = exports.sriUrl = exports.serverEvent = exports.serverRequest = exports.cliSessionType = exports.PVS_LIBRARY_FILES = exports.PRELUDE_FILE = exports.ProofDescriptor = void 0;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
class ProofDescriptor {
    constructor(info, origin, proofTree) {
        this.info = info;
        this.origin = origin;
        this.proofTree = proofTree;
    }
    isEmpty() {
        return !this.proofTree || !this.proofTree.rules || this.proofTree.rules.length === 0;
    }
}
exports.ProofDescriptor = ProofDescriptor;
exports.PRELUDE_FILE = "*prelude*";
exports.PVS_LIBRARY_FILES = {
    "*prelude*": "prelude.pvs",
    "character_adt": "character_adt.pvs",
    "lift_adt": "lift_adt.pvs",
    "list_adt": "list_adt.pvs",
    "ordstruct_adt": "ordstruct_adt.pvs",
    "union_adt": "union_adt.pvs"
};
;
;
;
;
exports.cliSessionType = {
    pvsioEvaluator: "pvs.pvsio-evaluator",
    proveFormula: "pvs.prove-formula"
};
;
;
;
;
;
;
;
;
exports.serverRequest = {
    typecheckFile: "pvs.typecheck-file",
    proveFormula: "pvs.prove-formula",
    autorunFormula: "pvs.autorun-formula",
    autorunFormulaFromJprf: "pvs.autorun-formula-from-jprf",
    showProofLite: "pvs.show-prooflite",
    proofCommand: "pvs.proof-command",
    evalExpression: "pvs.eval-expression",
    evaluatorCommand: "pvs.evaluator-command",
    parseFile: "pvs.parse-file",
    parseFileWithFeedback: "pvs.parse-file.with-feedback",
    parseWorkspace: "pvs.parse-workspace",
    parseWorkspaceWithFeedback: "pvs.parse-workspace.with-feedback",
    typecheckWorkspace: "pvs.typecheck-workspace",
    typecheckWorkspaceWithFeedback: "pvs.typecheck-workspace.with-feedback",
    listContext: "pvs.list-context",
    generateTccs: "pvs.generate-tccs",
    dumpPvsFiles: "pvs.dump-pvs-files",
    undumpPvsFiles: "pvs.undump-pvs-files",
    showTccs: "pvs.show-tccs",
    generateTheorySummary: "pvs.generate-theory-summary",
    showTheorySummary: "pvs.show-theory-summary",
    generateWorkspaceSummary: "pvs.generate-workspace-summary",
    showWorkspaceSummary: "pvs.show-workspace-summary",
    startPvsServer: "pvs.start-pvs-server",
    stopPvsServer: "pvs.stop-pvs-server",
    rebootPvsServer: "pvs.reboot-pvs-server",
    hp2pvs: "pvs.hp-to-pvs-file",
    startEvaluator: "pvs.start-evaluator",
    quitProof: "pvs.quit-proof",
    quitEvaluator: "pvs.quit-evaluator",
    clearWorkspace: "pvs.clear-workspace",
    findSymbolDeclaration: "pvs.find-symbol-declaration",
    pvsDoc: "pvs.pvs-doc",
    saveProof: "pvs.save-proof",
    viewPreludeFile: "pvs.view-prelude-file",
    statusProofChain: "pvs.status-proofchain",
    getContextDescriptor: "pvs.get-context-descriptor",
    getFileDescriptor: "pvs.get-file-descriptor",
    getGatewayConfig: "pvs.get-cli-descriptor",
    getImportChainTheorems: "pvs.get-importchain-theorems",
    getTheorems: "pvs.get-theorems",
    getTccs: "pvs.get-tccs",
    cancelOperation: "pvs.cancel-operation",
    proverCommand: "pvs.prover-command",
    search: "pvs.search",
    listVersionsWithProgress: "pvs.list-versions-with-progress",
    // downloadPvs: "pvs.download-pvs",
    downloadLicensePage: "pvs.download-license-page",
    downloadWithProgress: "pvs.download-with-progress",
    installWithProgress: "pvs.install-with-progress",
    openFileWithExternalApp: "pvs.open-file-with-external-app",
    getNasalibDownloader: "pvs.get-nasalib-downloader",
    downloadNasalib: "pvs.download-nasalib",
};
exports.serverEvent = {
    typecheckFileResponse: "pvs.response.typecheck-file",
    typecheckWorkspaceResponse: "pvs.response.typecheck-workspace",
    proveFormulaResponse: "pvs.response.prove-formula",
    autorunFormulaResponse: "pvs.response.autorun-formula",
    loadProofResponse: "pvs.response.load-proof",
    // showProofLiteResponse: "pvs.response.show-prooflite",
    proofCommandResponse: "pvs.response.proof-command",
    // evalExpressionResponse: "pvs.response.eval-expression",
    evaluatorCommandResponse: "pvs.response.evaluator-command",
    parseFileResponse: "pvs.response.parse-file",
    listContextResponse: "pvs.response.list-context",
    generateTccsResponse: "pvs.response.generate-tccs",
    showTccsResponse: "pvs.response.show-tccs",
    showTheorySummaryResponse: "pvs.response.show-theory-summary",
    startEvaluatorResponse: "pvs.response.start-evaluator",
    hp2pvsResponse: "pvs.response.hp-to-pvs-file",
    quitEvaluatorResponse: "pvs.response.quit-evaluator",
    quitProofResponse: "pvs.response.quit-proof",
    findSymbolDeclarationResponse: "pvs.response.find-symbol-declaration",
    viewPreludeFileResponse: "pvs.response.view-prelude-file",
    getContextDescriptorResponse: "pvs.response.get-context-descriptor",
    getFileDescriptorResponse: "pvs.response.get-file-descriptor",
    getGatewayConfigResponse: "pvs.response.get-cli-descriptor",
    getImportChainTheoremsResponse: "pvs.response.get-importchain-theorems",
    getTheoremsResponse: "pvs.response.get-theorems",
    getTccsResponse: "pvs.response.get-tccs",
    // downloadPvsResponse: "pvs.response.download-pvs",
    downloadLicensePageResponse: "pvs.response.download-license-page",
    setNasalibPathResponse: "pvs.response.set-nasalib-path",
    pvsServerReady: "pvs.response.pvs-server-ready",
    workspaceEvent: "pvs.workspace-event",
    contextUpdate: "pvs.event.context-update",
    // proofStateUpdate: "pvs.event.proof-state",
    QED: "pvs.event.qed",
    evaluatorStateUpdate: "pvs.event.evaluator-state",
    workspaceStats: "pvs.event.workspace-stats",
    // saveProofEvent: "pvs.event.save-proof",
    quitProofDontSaveEvent: "pvs.event.quit-dont-save-proof",
    saveProofForceQuitEvent: "pvs.event.save-then-quit",
    // closeDontSaveEvent: "pvs.event.close-dont-save-proof",
    serverModeUpdateEvent: "pvs.event.server-mode-update",
    // querySaveProof: "pvs.query.save-proof?",
    // querySaveProofResponse: "pvs.query.response.save-proof",
    // proverForwardResponse: "pvs.response.prover-forward",
    proofNodeUpdate: "pvs.event.proof-node-update",
    // proofEditEvent: "pvs.event.proof-edit-event",
    // proofExecEvent: "pvs.event.proof-exec-event",
    proverEvent: "pvs.prover-event",
    clipboardEvent: "pvs.event.clipboard-event",
    // loadProofStructureEvent: "pvs.event.load-proof-structure",
    // startProofEvent: "pvs.event.start-proof",
    pvsServerFail: "pvs.event.server-fail",
    pvsVersionInfo: "pvs.event.version-info",
    pvsNotFound: "pvs.event.pvs-not-found",
    pvsIncorrectVersion: "pvs.event.pvs-incorrect-version",
    profilerData: "pvs.event.profiler-data"
};
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
// useful constants
exports.sriUrl = "www.csl.sri.com";
exports.pvsUrl = "pvs.csl.sri.com";
// export const pvsSnapshotsUrl: string = `http://${sriUrl}/users/owre/drop/pvs-snapshots/`;
exports.pvsDownloadUrl = `https://${exports.pvsUrl}/downloads/`;
// export const allegroLicenseUrl: string = `http://${pvsUrl}/cgi-bin/downloadlic.cgi?file=pvs-6.0-ix86_64-Linux-allegro.tgz`; //`https://pvs.csl.sri.com/download.shtml`;
exports.NASALibUrl = "https://github.com/nasa/pvslib";
exports.NASALibGithubBranch = "master";
exports.NASALibGithubFile = `https://github.com/nasa/pvslib/archive/${exports.NASALibGithubBranch}.zip`;
;
;
;
// quick-fix interface
exports.quickFixReplaceCommand = "vscode-pvs.quick-fix-replace-command";
exports.quickFixAddImportingCommand = "vscode-pvs.quick-fix-add-importing-command";
;
/**
 * Kinds of documentation that can be exported with vscode-pvs
 */
var PvsDocKind;
(function (PvsDocKind) {
    PvsDocKind["embedded"] = "Embedded in the pvs file";
    PvsDocKind["html"] = "HTML";
    PvsDocKind["latex"] = "LaTex";
})(PvsDocKind = exports.PvsDocKind || (exports.PvsDocKind = {}));
;
;
//# sourceMappingURL=serverInterface.js.map