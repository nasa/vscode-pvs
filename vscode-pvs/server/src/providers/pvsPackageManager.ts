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

import { ChildProcess, execSync } from 'child_process';
import * as os from 'os';
import * as fsUtils from '../common/fsUtils';
import {
    serverRequest, DownloadWithProgressRequest, DownloadWithProgressResponse, 
    InstallWithProgressRequest, InstallWithProgressResponse, NASALibDownloader, 
    NASALibDownloaderRequest, ShellCommand, ListVersionsWithProgressRequest, 
    ListVersionsWithProgressResponse
} from '../common/serverInterface';
import * as path from 'path';
import { Connection } from 'vscode-languageserver';
import { shellCommandToString } from '../common/fsUtils';
import { colorText, PvsColor } from '../common/colorUtils';

/**
 * Server-side implementation of package manager class for downloding/installing PVS
 */
export class PvsPackageManager {

    /**
     * Process workers for download and installation tasks
     */
    protected static installProcess: ChildProcess;
    protected static downloadProcess: ChildProcess;

    /**
     * List of approved patches for pvs
     */
    static readonly approvedPatches: { fname: string, description: string }[] = [
        { fname: "patch-20210511.lisp", description: "Adds support for unicode in server commands" },
        { fname: "patch-20210706.lisp", description: "Fix for 'the assertion (or (null ex) (place ex)) failed'" },
        { fname: "patch-20210707.lisp", description: "Fix for prover not starting on judgements that are trivially true" },
        { fname: "patch-20210715.lisp", description: "Fix for pvs-server breaking into lisp when imported library cannot be found" },
        { fname: "patch-20210715a.lisp", description: "Fix for pvs-server breaking into lisp when opening a corrupted prf file" }
    ];

    /**
     * Installs PVS and provides progress feedback to the front-end.
     * The installation will be located in the target folder indicated in req
     */
    static async installWithProgress (connection: Connection, req: InstallWithProgressRequest): Promise<InstallWithProgressResponse> {
        let success: boolean = false;
        // if the request contains a cancellation token, then kill the download task
        if (req.cancellationToken) {
            PvsPackageManager.installProcess?.kill();
            if (req.cancellationToken) {
                return;
            }
        }
        if (connection && req?.targetFolder && req?.shellCommand) {
            const tmpFolder: string = req?.saveAndRestore ? path.join(os.tmpdir(), "iwp") : null;
            let restoreFolder: boolean = false;

            // check if there's a sub-folder of targetFolder to be saved and restored after install (e.g., nasalib)
            if (req?.saveAndRestore && fsUtils.folderExists(req.saveAndRestore)) {
                const res: InstallWithProgressResponse = {
                    progressInfo: true,
                    stdOut: `Saving ${req.saveAndRestore}`
                };
                connection?.sendNotification(serverRequest.installWithProgress, { req, res });
                fsUtils.deleteFolder(tmpFolder);
                restoreFolder = fsUtils.moveFolder(req.saveAndRestore, tmpFolder);
            }
            // check if the client has requested a clean install
            if (req.cleanTarget) {
                fsUtils.deleteFolder(req.targetFolder);
            }
            
            // exec the shell command with progress
            const cmd: InstallWithProgressResponse = {
                progressInfo: true,
                stdOut: shellCommandToString(req.shellCommand) + "\n"
            };
            connection?.sendNotification(serverRequest.installWithProgress, { req, res: cmd });
            success = await new Promise ((resolve, reject) => {
                PvsPackageManager.installProcess = fsUtils.execShellCommand(req.shellCommand, {
                    stdOut: (out: string) => {
                        if (!req.shellCommand.quiet) {
                            const res: InstallWithProgressResponse = {
                                progressInfo: true,
                                stdOut: out
                            };
                            connection?.sendNotification(serverRequest.installWithProgress, { req, res });
                        }
                    },
                    stdErr: (err: string) => {
                        if (!req.shellCommand.quiet) {
                            const res: InstallWithProgressResponse = {
                                progressInfo: true,
                                stdErr: err
                            };
                            connection?.sendNotification(serverRequest.installWithProgress, { req, res });
                        }
                    },
                    callback: (success: boolean) => {
                        resolve(success);
                    }
                })
            });
            // if the installation is successful, run post-installation scripts provided by the client
            if (success && req.installScript) {
                const cmd: InstallWithProgressResponse = {
                    progressInfo: true,
                    stdOut: req.installScript + "\n"
                };
                connection?.sendNotification(serverRequest.installWithProgress, { req, res: cmd });    
                success = await new Promise ((resolve, reject) => {
                    PvsPackageManager.installProcess = fsUtils.execShellCommand({ cmd: req.installScript }, {
                        stdOut: (out: string) => {
                            // if (!req.installScript.quiet) {
                            //     const res: InstallWithProgressResponse = {
                            //         progressInfo: true,
                            //         stdOut: out
                            //     };
                            //     connection?.sendNotification(serverRequest.installWithProgress, { req, res });
                            // }
                        },
                        stdErr: (err: string) => {
                            // if (!req.installScript.quiet) {
                                // const res: InstallWithProgressResponse = {
                                //     progressInfo: true,
                                //     stdErr: err
                                // };
                                // connection?.sendNotification(serverRequest.installWithProgress, { req, res });
                            // }
                        },
                        callback: (success: boolean) => {
                            resolve(success);
                        }
                    })
                });
            }
            // restore folder if necessary
            if (restoreFolder) {
                success = fsUtils.moveFolder(tmpFolder, req.saveAndRestore);
                if (success) {
                    const res: InstallWithProgressResponse = {
                        progressInfo: true,
                        stdOut: `Restoring ${req.saveAndRestore}`
                    };
                    connection?.sendNotification(serverRequest.installWithProgress, { req, res });
                }
            }
            // all done when we get to this point!
            if (success) {
                const res: InstallWithProgressResponse = {
                    progressInfo: true,
                    stdOut: colorText(`Done!`, PvsColor.green)
                };
                connection?.sendNotification(serverRequest.installWithProgress, { req, res });
            }
        }
        return { success };
    }

    /**
     * Provides the list pvs versions available for this machine's operating system at SRI's pvs-snapshots website.
     * The list is ordered by version number (the most recent version is in position 0).
     */
    static async listDownloadableVersionsWithProgress (connection: Connection, req: ListVersionsWithProgressRequest): Promise<ListVersionsWithProgressResponse> {
        const cmd: string = fsUtils.lsPvsVersions();

        const res: DownloadWithProgressResponse = {
            progressInfo: true,
            stdOut: cmd + "\n"
        };
        connection?.sendNotification(serverRequest.listVersionsWithProgress, { req, res });
        try {
            const ls: Buffer = execSync(cmd);
            if (ls) {
                const out: string = ls.toLocaleString().trim();
                const res: DownloadWithProgressResponse = {
                    progressInfo: true,
                    stdOut: out + "\n"
                };
                connection?.sendNotification(serverRequest.listVersionsWithProgress, { req, res });
                return { versions: [ out ] };
                // return fsUtils.parseLsPvsVersions(res);
            }
        } catch (error) {
            const res: DownloadWithProgressResponse = {
                progressInfo: true,
                stdErr: error + "\n"
            };
            connection?.sendNotification(serverRequest.listVersionsWithProgress, { req, res });
            return null;
            // unable to list versions
        }
        return null;
    }

    /**
     * Downloads PVS installation files and provides progress feedback to the front-end.
     * The file will be downloaded in the pvs base folder indicated in req
     */
    static async downloadWithProgress (connection: Connection, req: DownloadWithProgressRequest): Promise<DownloadWithProgressResponse> {
        // if the request contains a cancellation token, then kill the download task
        if (req.cancellationToken) {
            PvsPackageManager.downloadProcess?.kill();
            if (req.cancellationToken) {
                return;
            }
        }
        if (connection && req?.url && req?.baseFolder) {
            const fileNameOnDisk: string = fsUtils.getFileName(req.url, { keepExtension: true });
            await fsUtils.createFolder(req.baseFolder);
            const fname: string = path.join(req.baseFolder, fileNameOnDisk);
            const cmd: ShellCommand = req?.shellCommand || fsUtils.getDownloadCommand(req.url, { out: fname });
            // delete fname in temporary folder before downloading
            fsUtils.deleteFile(fname);
            // send progress info to show command and args
            const res: DownloadWithProgressResponse = {
                progressInfo: true,
                stdOut: shellCommandToString(cmd) + "\n"
            };
            connection?.sendNotification(serverRequest.downloadWithProgress, { req, res });
            const success: boolean = await new Promise ((resolve, reject) => {
                PvsPackageManager.downloadProcess = fsUtils.execShellCommand(cmd, {
                    stdOut: (out: string) => {
                        const res: DownloadWithProgressResponse = {
                            progressInfo: true,
                            stdOut: out
                        };
                        connection?.sendNotification(serverRequest.downloadWithProgress, { req, res });
                    },
                    stdErr: (err: string) => {
                        const res: DownloadWithProgressResponse = {
                            progressInfo: true,
                            stdErr: err
                        };
                        connection?.sendNotification(serverRequest.downloadWithProgress, { req, res });
                    },
                    callback: (success: boolean) => {
                        resolve(success);
                    }
                });
            });
            return { success, fname };
        }
        return { success: false };
    }

    /**
     * Utility function, installs required pvs patches
     */
    static async installPvsPatches (desc: { pvsPath: string }): Promise<boolean> {
        if (desc?.pvsPath && fsUtils.folderExists(desc?.pvsPath)) {
            const targetFolder: string = path.join(desc.pvsPath, "pvs-patches");
            fsUtils.createFolder(targetFolder);
            let successSummary: boolean = true;
            for (let i = 0; i < PvsPackageManager.approvedPatches.length; i++) {
                const fileName: string = PvsPackageManager.approvedPatches[i].fname;
                const fname: string = path.join(targetFolder, fileName);
                if (!fsUtils.fileExists(fname)) {
                    const src: string = path.join(__dirname, "..", "..", "..", "pvs-patches", fileName);
                    const fileContent: string = await fsUtils.readFile(src);
                    const success: boolean = fileContent && await fsUtils.writeFile(fname, fileContent);
                    if (!success) {
                        console.log(`[pvs-package-manager] Warning: could not install patch ${PvsPackageManager.approvedPatches[i]?.fname} (${PvsPackageManager.approvedPatches[i]?.description})`);
                    }
                    successSummary = successSummary && success;
                }
            }
            return successSummary;
        }
        return false;
    }

    /**
     * Returns the available nasalib downloader (preferred is git clone, alternative is zip)
     * @param pvsPath pvs installation path
     * @returns {string} the path where nasalib is installed
     */
    static async getNasalibDownloader (opt?: NASALibDownloaderRequest): Promise<NASALibDownloader> {
        return (opt?.preferred === "git") ? fsUtils.getSourceControl() || fsUtils.getDownloader()
            : fsUtils.getDownloader() || fsUtils.getSourceControl();
    }

    /**
     * Downloads the pvs license page from SRI's pvs-snapshots website.
     */
    static async downloadPvsLicensePage (): Promise<string> {
        // const downloadCommand: string = fsUtils.downloadCommand(allegroLicenseUrl);
        // const dnl: Buffer = execSync(downloadCommand);
        // if (dnl) {
        //     const res: string = dnl.toLocaleString()
        //             .replace(`<body>`, `<body style="color: black;background-color: white;">`)
        //             .replace(`action="../cgi-bin/download.cgi">`, `action="../cgi-bin/download.cgi" style="display: none;">`);
        //     await fsUtils.writeFile("~/Work/allegro.html", res);
        //     return res;
        // }
        // return null;
        return PvsPackageManager.allegroLicense; // we are using the cached version of the page to prevent issues in the case the license server is down
    }

    protected static allegroLicense: string = `
<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
<head>

<link rel="shortcut icon" href="http://pvs.csl.sri.com/images/pvslogo.ico" />
<link rel="icon" href="http://pvs.csl.sri.com/images/pvslogo16.gif" />
<link rel="stylesheet" href="http://pvs.csl.sri.com/fm-style.css" type="text/css" />
<title>PVS&trade; License Agreement - Allegro Versions</title>
</head>

<body style="color: black;background-color: white;">

<!-- Google Analytics bit -->
<script src="http://www.google-analytics.com/urchin.js" type="text/javascript">
</script>
<script type="text/javascript">
_uacct = "UA-2830572-1";
urchinTracker();
</script>

<table class="centered-small">
  <tr>
    <th> <a href="http://pvs.csl.sri.com/index.shtml">Home</a> </th>
    <th> &bull; </th>
    <th> <a href="http://pvs.csl.sri.com/introduction.shtml">Intro</a> </th>
    <th> &bull; </th>
    <th> <a href="http://pvs.csl.sri.com/announcements.shtml">Announce</a> </th>
    <th> &bull; </th>
    <th> <a href="http://pvs.csl.sri.com/faq.shtml">FAQ</a> </th>
    <th> &bull; </th>
    <th> <a href="http://pvs.csl.sri.com/documentation.shtml">Docs</a> </th>
    <th> &bull; </th>
    <th> <a href="http://pvs.csl.sri.com/download.shtml">Download</a> </th>
    <th> &bull; </th>
    <th> <a href="http://pvs.csl.sri.com/mailing-lists.shtml">Mail</a> </th>
    <th> &bull; </th>
    <th> <a href="http://pvs.csl.sri.com/status.shtml">Status</a> </th>
    <th> &bull; </th>
    <th> <a href="http://pvs.csl.sri.com/reporting-bugs.shtml">Bugs</a> </th>
    <th> &bull; </th>
    <th> <a href="http://pvs.csl.sri.com/user-links.shtml">Users</a> </th>
    <th> &bull; </th>
    <th> <a href="http://pvs.csl.sri.com/links.shtml">Related</a> </th>
    <th> &bull; </th>
    <th> <a href="http://fm.csl.sri.com">FM&nbsp;Tools</a> </th>
  </tr>
</table>

<img class="rfloat" src="http://pvs.csl.sri.com/images/" alt="" />
<h1>PVS&trade; License Agreement - Allegro Versions</h1>

<p>Please read and click "Accept" button below.</p>

<h2>SRI INTERNATIONAL END-USE LICENSE AGREEMENT SOFTWARE PROGRAM:
PVS&trade; ALLEGRO VERSIONS </h2>

<h3>GRANT</h3>

<p> By clicking "Accept" below, SRI International ("SRI") grants you a
royalty-free, non-exclusive, non-transferable license under the Licensed IP
to use its accompanying, above referenced software program ("Software") and
related documentation ("Documentation") upon the terms and conditions set
forth below:</p>

<h4>Definitions</h4>

<p> "Licensed Software" means the Software, and any larger applications in
which such Software is incorporated.  </p>

<p> "Licensed IP" means all patent claims, copyright, and trade secrets
owned or freely licensable by SRI that are embodied in or necessarily
practiced by the Software.  </p>

<p> "Non-Commercial Purposes" means usage of Licensed Software (a) by an
official governmental agency, strictly for non-commercial, public benefit
purposes, or (b) solely for non-commercial research purposes in exchange for
which no financially valuable consideration (including but not limited to
sales or license revenue, service revenue, and advertising revenue, and
whether in the form of monetary, equity, or other forms of consideration) is
received, except for research funding received from an official government
agency sponsor, a non-profit foundation, or similar research funding
organization.  </p>

<p> <b>You may:</b> use or copy the Licensed Software solely for
Non-Commercial Purposes. </p>

<p> <b>You may not:</b> modify the Software, or decompile, disassemble, or
otherwise copy the Software in modified form; or publish, distribute, or
transfer copies of the Licensed Software.  These examples are by way of
illustration, not limitation; except as expressly set forth above (under
"You may"), no other license rights are implied or otherwise granted under
this License Agreement.  </p>

<h4>TITLE</h4>

<p> SRI retains title, ownership rights, and intellectual property rights in
and to the Software and Documentation.  The Software is protected by the
copyright laws of the United States and international copyright treaties.
</p>


<h4>DISCLAIMER OF WARRANTY</h4>

<p> Since this Software is a research tool still in the development stage
and is provided free of charge, the Software and Documentation are provided
on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, INCLUDING WITHOUT
LIMITATION THE WARRANTIES OF MERCHANTABILITY OR FITNESS FOR ANY PARTICULAR
PURPOSE OR THAT THE USE OF THE SOFTWARE OR DOCUMENTATION WILL NOT INFRINGE
ANY PATENTS, COPYRIGHTS, TRADEMARKS, OR OTHER RIGHTS.  The entire risk as to
the quality and performance of the Software and/or Documentation is borne by
you.  Should the Software and/or Documentation prove defective, you and not
SRI assume the entire cost of any services and repair.  This disclaimer of
warranty constitutes an essential part of the agreement.  </p>


<h4>LIMITATION OF LIABILITY</h4>

<p> UNDER NO CIRCUMSTANCES AND UNDER NO LEGAL THEORY, TORT, CONTRACT, OR
OTHERWISE, SHALL SRI, SRI'S AGENTS, OFFICERS, ASSISTANTS AND EMPLOYEES
EITHER IN THEIR INDIVIDUAL CAPACITIES OR BY REASON OF THEIR RELATIONSHIP TO
SRI AND SRI'S SUCCESSORS, BE HELD LIABLE TO YOU OR ANY OTHER PERSON FOR ANY
INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES OF ANY CHARACTER
INCLUDING, WITHOUT LIMITATION, DAMAGES FOR LOSS OF GOODWILL, WORK STOPPAGE,
COMPUTER FAILURE OR MALFUNCTION, OR ANY AND ALL OTHER COMMERCIAL DAMAGES OR
LOSSES.  IN NO EVENT SHALL SRI BE LIABLE FOR COSTS OF PROCUREMENT OF
SUBSTITUTE OR REPLACEMENT PRODUCTS OR SERVICES.  IN NO EVENT SHALL SRI'S
LIABILITY TO YOU OR ANY THIRD PARTY EXCEED THE AMOUNT PAID BY YOU UNDER THIS
AGREEMENT, IF ANY.</p>

<h4>NO SUPPORT, NO UPDATES</h4>

<p> This license does not grant you any right to any Software support,
enhancements or updates.  </p>

<h4>TERMINATION</h4>

<p> This license will terminate automatically if you fail to comply with the
limitations described above.  On termination, you must destroy all copies of
the Software.  </p>

<h4>EXPORT RESTRICTIONS</h4>

<p> Each party hereby acknowledges that the rights and obligations of this
Agreement are subject to the laws and regulations of the United States
relating to the export of products and technical information.  Without
limitation, each party shall comply with all such applicable laws and
regulations.  </p>

<p> Use of PVS outside the USA may be subject to US Government export
licensing requirements.  The requirements are dependent on the use to be
made of PVS.  If your use of PVS will concern something other than research,
or if the results will be restricted for proprietary or national security
reasons, then please provide explicit details in an end-use statement and
send to: </p>

<p>
Computer Science Laboratory<br/>
SRI International<br/>
Menlo Park CA 94025 USA<br/>
Email: <a href="mailto:pvs-sri@csl.sri.com">pvs-sri@csl.sri.com</a><br/>
Phone: +1 (650) 859-3333<br/>
Fax: +1 (650) 859-2844<br/>
</p>

<h4>U.S. GOVERNMENT RESTRICTED RIGHTS</h4>

<p> If the Software or any accompanying documentation is used or acquired by
or on behalf of any unit, division or agency of the United States
Government, this provision applies.  The Software and any accompanying
documentation is provided with RESTRICTED RIGHTS.  The use, modification,
reproduction, release, display, duplication or disclosure thereof by or on
behalf of any unit, division or agency of the Government is subject to the
restrictions set forth in subdivision (c)(1) of the Commercial Computer
Software-Restricted Rights clause at 48 CFR 52.227-19 and the restrictions
set forth in the Rights in Technical Data-Non-commercial Items clause set
forth in 48 CFR 252.227-7013.  The contractor/manufacturer of the Software
and accompanying documentation is SRI International, 333 Ravenswood Avenue,
Menlo Park, California 94025, USA.  </p>

<h4>MISCELLANEOUS</h4>

<p> This agreement represents the complete agreement concerning this license
between the parties and supersedes all prior agreement and representations
between them.  It may be amended only by a writing executed by both parties.
If any provision of this Agreement is held to be unenforceable for any
reason, such provision shall be reformed only to the extent necessary to
make it enforceable.  This agreement shall be governed by and construed in
accordance with the laws of the State of California, without regard to the
conflicts of law principles thereof, and shall not be governed by the United
Nations convention on contracts for the international sale of goods.  </p>

<p> <strong>NOTE:</strong> Although not part of the agreement, we appreciate
it if you will inform us at <a
href="mailto:pvs-bugs@csl.sri.com">pvs-bugs@csl.sri.com</a> of any bugs that
you discover, and notify us at <a
href="mailto:pvs-sri@csl.sri.com">pvs-sri@csl.sri.com</a> of interesting
applications you have made of PVS and of any papers or reports describing
your use of the system.  </p>

<p>
<center>
<form method=get action="../cgi-bin/download.cgi" style="display: none;">
<input type="hidden" name="file" value="pvs-6.0-ix86_64-Linux-allegro.tgz">
<center><i> <b>I have read the terms of the PVS license and
<input type=submit name="accept" value="I accept"> or <input type="submit" name="reject" value="I DO NOT accept"> the license terms</b> </i>
</form>
</center>
</p>

<table class="centered-small">
  <tr>
    <th> <a href="http://pvs.csl.sri.com/index.shtml">Home</a> </th>
    <th> &bull; </th>
    <th> <a href="http://pvs.csl.sri.com/introduction.shtml">Intro</a> </th>
    <th> &bull; </th>
    <th> <a href="http://pvs-wiki.csl.sri.com">Wiki</a> </th>
    <th> &bull; </th>
<!--    <th> <a href="http://pvs.csl.sri.com/announcements.shtml">Announce</a> </th> 
    <th> &bull; </th> -->
    <th> <a href="http://pvs.csl.sri.com/documentation.shtml">Docs</a> </th>
    <th> &bull; </th>
    <th> <a href="http://pvs-wiki.csl.sri.com/index.php/FAQ">FAQ</a> </th>
    <th> &bull; </th>
    <th> <a href="http://pvs.csl.sri.com/download.shtml">Download</a> </th>
    <th> &bull; </th>
    <th> <a href="http://pvs.csl.sri.com/reporting-bugs.shtml">Bugs</a> </th>
    <th> &bull; </th>
    <th> <a href="http://pvs.csl.sri.com/mailing-lists.shtml">Mail</a> </th>
    <th> &bull; </th>
<!--    <th> <a href="http://pvs.csl.sri.com/status.shtml">Status</a> </th>
    <th> &bull; </th>
    <th> <a href="http://pvs.csl.sri.com/user-links.shtml">Users</a> </th>
    <th> &bull; </th>
    <th> <a href="http://pvs.csl.sri.com/links.shtml">Related</a> </th>
    <th> &bull; </th> -->
    <th> <a href="http://fm.csl.sri.com">FM&nbsp;Tools</a> </th>
  </tr>
</table>
</body>
</html>
    `;

}