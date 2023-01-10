/**
 * @module Pvs2HtmlCli
 * @author Paolo Masci
 * @date 2022.12.06
 * @desc Command-line version of pvs2html
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

import { getContextFolder } from "./common/fsUtils";
import { PvsDocDescriptor } from './common/serverInterface';
import { Pvs2Html, Pvs2HtmlSettings } from "./pvs2html";

// help message
const helpMessage: string = `pvs-doc-cli: documentation generator for PVS specification files
Usage:
    pvs2html-cli <pvs-file>

Options:
    -out <folder> (folder where the documentation will be generated, default is <current folder>/out )
    -ctx <folder> (folder containing the input files, default is <current folder>)
    -help         (shows this help message)
`;

// cli interface options
export interface PvsDocCliOptions extends Pvs2HtmlSettings { };

/**
 * Utility function used to process command line interface arguments
 */
function parseCliArgs (args: string[]): PvsDocCliOptions {
    const options: PvsDocCliOptions = {};
    console.log("------------------------");
    if (args && args.length) {
        for (let i = 0; i < args.length; i++) {
            if (args[i] === "-out" || args[i] === "--out" || args[i] === "-output" || args[i] === "--output") {
                if (i + 1 < args.length) {
                    i++;
                    options.outputFolder = args[i];
                    console.log(`** Output folder: ${options.outputFolder}`);
                } else {
                    console.warn(`[pvs2html-cli] Warning: ${args[i]} option provided without indicating output folder (skipping option)}`);
                }
            } else if (args[i] === "-readme" || args[i] === "--readme") {
                if (i + 1 < args.length) {
                    i++;
                    options.readmeFile = args[i];
                    console.log(`** Readme file: ${options.readmeFile}`);
                } else {
                    console.warn(`[pvs2html-cli] Warning: ${args[i]} option provided without indicating the README.md file.`);
                }
            } else if (args[i] === "-help" || args[i] === "--help") {
                console.log(helpMessage);
            } else {
                if (args[i].endsWith(".pvs")) {
                    options.inputFile = args[i];
                    options.inputFolder = getContextFolder(options.inputFile);
                    console.log(`** Input file: ${options.inputFile}`);
                } else {
                    options.inputFolder = args[i];
                    console.log(`** Location of input files: ${options.inputFolder}`);
                }
            }
        }
    } else {
        console.log(helpMessage);
    }
    console.log("------------------------");
    return options;
}

/**
 * Main function for invoking pvs2html
 */
async function generateDocFiles () {
    const worker: Pvs2Html = new Pvs2Html();
    console.log(process.argv)
    const options: PvsDocCliOptions = parseCliArgs(process.argv.slice(2));
    if (options?.inputFolder) {
        const desc: PvsDocDescriptor | null = await worker.run(options); // the first two args are 'node' and 'pvs2html.js'
        if (desc?.mainFile) {
            console.log("------------------------");
            console.log(`Documentation files ready at ${desc.mainFile}`);
            console.log("------------------------");
        }
    }
}

generateDocFiles();
