/**
 * @module PvsPackageManager
 * @author Paolo Masci
 * @date 2019.10.24
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

import { execSync } from 'child_process';
import * as os from 'os';
import * as fsUtils from '../common/fsUtils';
import { pvsSnapshotsUrl, PvsDownloadDescriptor, allegroLicenseUrl, NasalibDownloadDescriptor, nasalibFile } from '../common/serverInterface';

export class PvsPackageManager {

    /**
     * Provides the list pvs versions available for this machine's operating system at SRI's pvs-snapshots website.
     * The list is ordered by version number (the most recent version is in position 0).
     */
    static async listDownloadableVersions (): Promise<PvsDownloadDescriptor[]> {
        const osName: string = fsUtils.getOs();
        const lsCommand: string = `${fsUtils.downloadCommand(pvsSnapshotsUrl)} | grep -oE '(http.*\.tgz)\"' | sed 's/"$//' | grep ${osName} | grep allegro`;
        const ls: Buffer = execSync(lsCommand);
        if (ls) {
            const res: string = ls.toLocaleString();
            const elems: string[] = res.split("\n");
            const versions: PvsDownloadDescriptor[] = elems.map(url => {
                const components: string[] = url.split("/");
                const fileName: string = components.slice(-1)[0];
                const match: RegExpMatchArray = /pvs([\d\.\-]+)\-\w+/.exec(fileName);
                const version: string = (match && match.length > 1) ? match[1].replace(/\-/g,".") : null;
                return { url, fileName, version };
            });
            return versions;
        }
        return null;
    }

    /**
     * Downloads a pvs version from SRI's pvs-snapshots website.
     */
    static async downloadPvsExecutable (desc: PvsDownloadDescriptor): Promise<string> {
        const fname: string = `${os.tmpdir()}/${desc.fileName}`;
        const downloadCommand: string = fsUtils.downloadCommand(desc.url, { out: fname });
        const dnl: Buffer = execSync(downloadCommand);
        if (dnl) {
            return fname;
        }
        return null;
    }

    /**
     * Downloads the pvs license page from SRI's pvs-snapshots website.
     */
    static async downloadPvsLicensePage (): Promise<string> {
        const downloadCommand: string = fsUtils.downloadCommand(allegroLicenseUrl);
        const dnl: Buffer = execSync(downloadCommand);
        if (dnl) {
            return dnl.toLocaleString()
                    .replace(`<body>`, `<body style="color: black;background-color: white;">`)
                    .replace(`action="../cgi-bin/download.cgi">`, `action="../cgi-bin/download.cgi" style="display: none;">`);
        }
        return null;
    }

    /**
     * Downloads nasalib from github.
     */
    static async downloadNasalib (desc: NasalibDownloadDescriptor): Promise<string> {
        const url: string = nasalibFile;
        const fname: string = `${os.tmpdir()}/nasalib7.zip`;
        const downloadCommand: string = fsUtils.downloadCommand(url, { out: fname });
        fsUtils.deleteFile(fname);
        const dnl: Buffer = execSync(downloadCommand);
        if (dnl) {
            return fname;
        }
        return null;
    }

}