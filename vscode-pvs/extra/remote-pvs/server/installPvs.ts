import { spawn, ChildProcess } from 'child_process';
import fs from 'fs';
import path from 'path';
import { logger } from './utils';

function installPvs(baseDir: string): Promise<void> {
    return new Promise((resolve, reject) => {
        const commands = `
        sudo apt-get update \
        && sudo apt-get -y dist-upgrade \
        && sudo apt-get -y install apt-utils git sbcl curl sudo wget unzip \
        && DEBIAN_FRONTEND=noninteractive sudo apt-get install -y build-essential \
        && DEBIAN_FRONTEND=noninteractive sudo apt-get install -y emacs \
        && mkdir -p ${baseDir} \
        && cd ${baseDir} \
        && git clone https://github.com/SRI-CSL/PVS.git \
        && cd PVS \
        && ./configure \
        && make \
        `;
        // && wget https://github.com/nasa/pvslib/archive/v8.0.zip \
        // && unzip v8.0.zip \
        // && mv pvslib-8.0 nasalib \
        // && rm v8.0.zip \
        // && echo "export PVS_LIBRARY_PATH=${baseDir}/PVS/nasalib" >> ~/.bashrc \
        

        const process: ChildProcess = spawn('sh', ['-c', commands], {
            stdio: 'inherit',
            shell: true
        });

        process.on('error', (error: Error) => {
            console.error(`Error: ${error.message}`);
            reject(error);
        });

        process.on('close', (code: number | null) => {
            if (code !== 0) {
                logger.info(`Process exited with code ${code}`);
                reject(new Error(`Process exited with code ${code}`));
            } else {
                logger.info('PVS installed successfully');
                resolve();
            }
        });
    });
}

export const installPvsMain = async (pvspath: string) => {
    if (!isPvsInstalled(pvspath)) {
        logger.info(`PVS not found at ${pvspath}. Reinstalling...`);
        const dir_name = path.dirname(pvspath);
        deleteDirectory(pvspath);
        await installPvs(dir_name);
      } else {
        logger.info('PVS is already installed.');
      }
}

// Function to check if PVS is installed
function isPvsInstalled(dir: string): boolean {
    return fs.existsSync(path.join(dir, 'pvs'));
  }
  
  // Function to delete a directory
  function deleteDirectory(dir: string): void {
    if (fs.existsSync(dir)) {
        fs.rmSync(dir, { recursive: true, force: true });
        logger.info(`Deleted directory: ${dir}`);
    }
  }