/**
 * @module process-decl
 * @description generates nasalib-lookup-table.ts based on the content of nasalib-decls.json
 * @author Paolo Masci
 * @date 2021.01.10
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

import { LookUpTable, LookUpTableStats, PvsTheory } from '../../common/serverInterface';
import * as fsUtils from '../../common/fsUtils';
import * as fs from 'fs';

const NASALIB_VERSION: string = "NASALib v7.1.0 (11/05/20)";

interface MathObjects { types: number, definitions: number, lemmas: number };
interface Decl { line: number, character: number, identifier: string, theoryName: string };
interface DeclsOverview {
    contextFolder: string,
    fileName: string,
    fileExtension: ".pvs",
    declarations: {
        theories: Decl[],
        types: Decl[],
        functions: Decl[],
        formulas: Decl[],
        locals: Decl[]
    },
    "math-objects": MathObjects,
    "parse-time": { ms: number }
};

// input file
const fname_input: string = "nasalib-decls.json";
// output files, created in the common folder
const fname_ts_output: string = "../../common/nasalib-lookup-table.ts";
const fname_json_output: string = "../../common/nasalib-lookup-table.json";

// this function generates a json file (nasalib-decls.json) containing info on nasalib theories. 
// The function is executed manually before distributing a vscode-pvs release.
// see Makefile target 'build-nasalib-decls'
async function run () {
    const content: string = await fsUtils.readFile(fname_input);
    const declsOverview: DeclsOverview[] = JSON.parse(content);
    // console.log(info);

    // for each declaration, store info about the pvs file, the location of the decl, and the theory name
    // an array of pvs theories is used because the same declaration names can be declared in different theories
    const folders: { [folderName: string]: PvsTheory[] } = {};
    const theories: { [theoryName: string]: PvsTheory[] } = {};
    const types: { [typeName: string]: PvsTheory[] } = {};
    const functions: { [typeName: string]: PvsTheory[] } = {};
    const formulas: { [typeName: string]: PvsTheory[] } = {};
    const stats: LookUpTableStats = {
        version: NASALIB_VERSION,
        folders: 0,
        theories: 0,
        formulas: 0,
        functions: 0,
        types: 0
    };
    for (let i = 0; i < declsOverview?.length; i++) {
        // overview reflects the content of a pvs file
        const overview: DeclsOverview = declsOverview[i];
        if (overview.fileName !== "top") {
            // for each theory declaration, store info about the pvs file, the location of the decl, and the theory name
            for (let j = 0; j < overview?.declarations?.theories?.length; j++) {
                const elem: Decl = overview.declarations.theories[j];
                const name: string = elem.identifier;
                theories[name] = theories[name] || [];
                theories[name].push({
                    contextFolder: overview.contextFolder,
                    fileName: overview.fileName,
                    fileExtension: overview.fileExtension,
                    theoryName: elem.identifier,
                    line: elem.line,
                    character: elem.character
                });
                // store the information also in the folder view
                folders[overview.contextFolder] = folders[overview.contextFolder] || [];
                folders[overview.contextFolder].push({
                    contextFolder: overview.contextFolder,
                    fileName: overview.fileName,
                    fileExtension: overview.fileExtension,
                    theoryName: elem.identifier,
                    line: elem.line,
                    character: elem.character
                });
            }
            stats.folders = Object.keys(folders).length;
            stats.theories = Object.keys(theories).length;
            // for each type declaration, store info about the pvs file, the location of the decl, and the theory name
            for (let j = 0; j < overview?.declarations?.types?.length; j++) {
                const elem: Decl = overview.declarations.types[j];
                const name: string = elem.identifier;
                types[name] = types[name] || [];
                types[name].push({
                    contextFolder: overview.contextFolder,
                    fileName: overview.fileName,
                    fileExtension: overview.fileExtension,
                    theoryName: elem.theoryName,
                    line: elem.line,
                    character: elem.character
                });
            }
            stats.types = Object.keys(types).length;
            // for each function declaration, store info about the pvs file, the location of the decl, and the theory name
            for (let j = 0; j < overview?.declarations?.functions?.length; j++) {
                const info: Decl = overview.declarations.functions[j];
                const name: string = info.identifier;
                // sanity check
                functions[name] = functions[name] || [];
                const theory: PvsTheory = {
                    contextFolder: overview.contextFolder,
                    fileName: overview.fileName,
                    fileExtension: overview.fileExtension,
                    theoryName: info.theoryName,
                    line: info.line,
                    character: info.character
                };
                // console.log(functions[name].length, name, theory);
                // for some reasons functions[name].push(theory) does not work
                functions[name] = [ theory ].concat(functions[name]);
            }
            stats.functions = Object.keys(functions).length;
            // for each formula declaration, store info about the pvs file, the location of the decl, and the theory name
            for (let j = 0; j < overview?.declarations?.formulas?.length; j++) {
                const elem: Decl = overview.declarations.formulas[j];
                const name: string = elem.identifier;
                formulas[name] = formulas[name] || [];
                formulas[name].push({
                    contextFolder: overview.contextFolder,
                    fileName: overview.fileName,
                    fileExtension: overview.fileExtension,
                    theoryName: elem.theoryName,
                    line: elem.line,
                    character: elem.character
                });
            }
            stats.formulas = Object.keys(formulas).length;
        }
    }
    // const nasalib: LookUpTable = { stats, folders, theories, types, functions, formulas };
    const nasalibUnsorted: LookUpTable = {
        stats, 
        folders,
        theories, 
        types, 
        functions, 
        formulas
    };
    const nasalib: LookUpTable = {
        stats, 
        folders: {},
        theories: {}, 
        types: {}, 
        functions: {}, 
        formulas: {}
    };
    // sort everything by theory name
    const keys: string[] = Object.keys(nasalib);
    for (let k = 0; k < keys.length; k++) {
        const key: string = keys[k];
        if (key !== "stats") {
            // nasalib[key] = nasalibUnsorted[key].sort((a: PvsTheory, b: PvsTheory) => {
            //     return a.theoryName < b.theoryName ? -1 : 1;
            // });;
            let l1key: string[] = Object.keys(nasalibUnsorted[key]).sort((a, b) => { return a < b ? -1 : 1; });
            for (let i = 0; i < l1key.length; i++) {
                nasalib[key][l1key[i]] = nasalibUnsorted[key][l1key[i]].sort((a: PvsTheory, b: PvsTheory) => {
                    return (a.theoryName && b.theoryName) && a.theoryName.toLowerCase() < b.theoryName.toLowerCase() ? -1 : 1;
                });;
            }        
        }
    }
    console.log("nasalib", nasalib.stats);
    // create file content
    const json: string = JSON.stringify(nasalib, null, " ");
    const txt: string = `/**
* File automatically generated by vscode-pvs
*/\n
import { LookUpTable } from './serverInterface';
export const nasalib_lookup_table: LookUpTable = ${json};`;
    // delete any previous versions of the files
    fsUtils.deleteFile(fname_ts_output);
    fsUtils.deleteFile(fname_json_output);
    // write output files
    fs.writeFileSync(fname_ts_output, txt);
    fs.writeFileSync(fname_json_output, json);
    console.log(`output files ${fname_ts_output} and ${fname_json_output} created in ../../common!`);
}

run();
