import { LookUpTable, PvsTheory } from '../../common/serverInterface';
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
// output files
const fname_ts_output: string = "nasalib-lookup-table.ts";
const fname_json_output: string = "nasalib-lookup-table.json";

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
    const stats: { [key: string]: number | string } = {
        version: NASALIB_VERSION
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
    const nasalib: LookUpTable = { stats, folders, theories, types, functions, formulas };
    console.log("nasalib", nasalib.stats);
    // create file content
    const json: string = JSON.stringify(nasalib, null, " ");
    const txt: string = `/**
* File automatically generated by vscode-pvs
*/\n
import { LookUpTable } from '../../common/serverInterface';
export const nasalib_lookup_table: LookUpTable = ${json};`;
    // write output files
    fs.writeFileSync(fname_ts_output, txt);
    fs.writeFileSync(fname_json_output, json);
    console.log(`output files ${fname_ts_output} and ${fname_json_output} created!`);
}

run();
