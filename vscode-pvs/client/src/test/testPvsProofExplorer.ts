import * as vscode from 'vscode';
import { ProofNode, ProofNodeType, serverRequest, PvsVersionDescriptor, ProofDescriptor, ProofStatus, serverEvent, ProofTree } from '../common/serverInterface';
import { VSCodePvsProofExplorer, ProofItem } from '../views/vscodePvsProofExplorer';

/**
 * Utility class for running test cases for proof-explorer
 */
export class TestVSCodePvsProofExplorer extends VSCodePvsProofExplorer {
	test (): void {
		// show dialog with progress
		vscode.window.withProgress({
			location: vscode.ProgressLocation.Notification,
			cancellable: true
		}, (progress, token) => {
			return new Promise(async (resolve, reject) => {
				progress.report({ increment: -1, message: `Regression test for module 'proof-explorer'` });
				token.onCancellationRequested(() => {
					// dispose of the dialog
					resolve(null);
				});
				let test: Promise<void> = Promise.resolve();
				let totals: number = 0;
				let failed: string[] = [];

				// can load proof descriptor
				// test.then(() => {
				// 	return new Promise((resolve, reject) => {
				// 		progress.report({
				// 			increment: 1 / ++totals,
				// 			message: "Can load proof descriptor"
				// 		});
				// 		const desc: ProofDescriptor = {
				// 			info: {
				// 				theory: "th",
				// 				formula: "fmla",
				// 				status: "unfinished",
				// 				prover: "PVS 7.1.0 (International Allegro CL Enterprise Edition)",
				// 				shasum: "bb6f5bfc76488e53c9392a48fb11496f8c04aaacb1cf2be812bd6e1820efcdc8"
				// 			}
				// 		};
				// 		this.loadProofDescriptor(desc);
				// 		if (this.root !== null 
				// 			&& this.root.name === desc.info.formula
				// 			&& this.root.getProofStatus() === desc.info.status
				// 			&& this.ghostNode !== null
				// 			&& this.ghostNode.parent === this.root
				// 			&& !this.ghostNode.isActive()) {
				// 			// all ok
				// 		} else {
				// 			failed.push("Failed to load proof descriptor");
				// 		}
				// 		resolve();
				// 	});
				// });

				// // can add proof commands when tree is empty
				// // before: .
				// // after: [0] skosimp*
				// let skosimp: ProofItem = null;
				// test = test.then(() => {
				// 	return new Promise(async (resolve, reject) => {
				// 		progress.report({
				// 			increment: 1 / ++totals,
				// 			message: "Can add proof commands when tree is empty"
				// 		});
				// 		skosimp = await this.appendNode({ selected: this.root, elem: "skosimp*" });
				// 		if (this.root && this.root.children && this.root.children.length === 1
				// 			&& this.root.children[0] === skosimp
				// 			&& skosimp.name === "(skosimp*)"
				// 			&& skosimp.children && skosimp.children.length === 0
				// 			&& !skosimp.isVisited()) {
				// 			// all ok
				// 		} else {
				// 			failed.push("Failed to add proof commands when tree is empty");
				// 		}

				// 		resolve();
				// 	});
				// });

				// // can append proof commands
				// // before: [0] skosimp*
				// // after: [0] skosimp*
				// //        [1] split
				// let split: ProofItem = null;
				// test = test.then(() => {
				// 	return new Promise(async (resolve, reject) => {
				// 		progress.report({
				// 			increment: 1 / ++totals,
				// 			message: "Can append proof commands"
				// 		});

				// 		split = await this.appendNode({ selected: skosimp, elem: "split" });
				// 		if (split !== null && this.root.children && this.root.children.length === 2
				// 			&& this.root.children[1] === split
				// 			&& split.name === "(split)"
				// 			&& split.children && split.children.length === 0
				// 			&& !split.isVisited()) {
				// 			// all ok
				// 		} else {
				// 			failed.push("Failed to append proof commands");
				// 		}

				// 		resolve();
				// 	});
				// });

				// // can prepend proof commands
				// // before: [0] skosimp*
				// //         [1] split
				// // after: [0] skeep
				// //        [1] skosimp*
				// //        [2] split
				// let skeep: ProofItem = null;
				// test = test.then(() => {
				// 	return new Promise(async (resolve, reject) => {
				// 		progress.report({
				// 			increment: 1 / ++totals,
				// 			message: "Can prepend proof commands"
				// 		});
				// 		skeep = await this.appendNode({ selected: this.root, elem: "skeep" });
				// 		if (this.root && this.root.children && this.root.children.length === 3
				// 			&& this.root.children[0] === skeep
				// 			&& skeep.name === "(skeep)"
				// 			&& skeep.children && skeep.children.length === 0
				// 			&& !skeep.isVisited()) {
				// 			// all ok
				// 		} else {
				// 			failed.push("Failed to prepend proof commands");
				// 		}

				// 		resolve();
				// 	});
				// });

				// // can add proof branches
				// // before: [0] skeep
				// //         [1] skosimp*
				// //         [2] split
				// // after: [0] skeep
				// //        [1] skosimp*
				// //        [2] split
				// //        [2.0] (1)
				// //        [2.1] (2)
				// let branch1: ProofItem = null;
				// let branch2: ProofItem = null;
				// test = test.then(() => {
				// 	return new Promise((resolve, reject) => {
				// 		progress.report({
				// 			increment: 1 / ++totals,
				// 			message: "Can add proof branches"
				// 		});
				// 		branch1 = this.appendBranch({ selected: split });
				// 		branch2 = this.appendBranch({ selected: split });
				// 		if (this.root && this.root.children && this.root.children.length === 3
				// 			&& branch1 !== null && branch1.name === "(1)"
				// 			&& branch2 !== null && branch2.name === "(2)"
				// 			&& split.children && split.children.length === 2) {
				// 			// all ok
				// 		} else {
				// 			failed.push("Failed to add proof branches");
				// 		}
				// 		resolve();
				// 	});
				// });

				// // can append proof commands in branches
				// // before: [0] skeep
				// //         [1] skosimp*
				// //         [2] split
				// //         [2.1] (1)
				// //         [2.2] (2)
				// // after: [0] skeep
				// //        [1] skosimp*
				// //        [2] split
				// //        [2.0] (1)
				// //        [2.0.0] (assert)
				// //        [2.1] (2)
				// let assert: ProofItem = null;
				// test = test.then(() => {
				// 	return new Promise(async (resolve, reject) => {
				// 		progress.report({
				// 			increment: 1 / ++totals,
				// 			message: "can append proof commands in branches"
				// 		});
				// 		assert = await this.appendNode({ selected: branch1, elem: "assert" });
				// 		if (this.root && this.root.children && this.root.children.length === 3
				// 			&& this.root.children[2].children 
				// 			&& this.root.children[2].children.length
				// 			&& this.root.children[2].children[0] 
				// 			&& this.root.children[2].children[0].children
				// 			&& this.root.children[2].children[0].children.length
				// 			&& this.root.children[2].children[0].children[0] === assert
				// 			&& assert.name === "(assert)"
				// 			&& assert.children && assert.children.length === 0
				// 			&& !assert.isVisited()) {
				// 			// all ok
				// 		} else {
				// 			failed.push("Failed to append proof commands in branches");
				// 		}

				// 		resolve();
				// 	});
				// });


				// // run all tests
				// await Promise.resolve(test);

				// // print summary
				// const summary: string = `Test summary for 'proof-explorer': success: ${totals - failed.length}, fail: ${failed.length}, totals: ${totals}`;
				// if (failed.length === 0) {
				// 	vscode.window.showInformationMessage(summary);
				// } else {
				// 	vscode.window.showWarningMessage(summary);
				// 	for (let i = 0; i < failed.length; i++) {
				// 		vscode.window.showErrorMessage(failed[i]);
				// 	}
				// }
				// resolve();
			});
		});
	}
}