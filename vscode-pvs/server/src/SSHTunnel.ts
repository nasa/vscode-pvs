import { spawn, exec, ChildProcess } from 'child_process';

export class SSHTunnel {
    private localPort: number;
    private remotePort: number;
    private sshUser: string;
    private sshHost: string;
    private privateKeyPath: string;
    tunnelProcess: ChildProcess | null = null;

    constructor(localPort: number, remotePort: number, sshUser: string, sshHost: string, privateKeyPath: string) {
        this.localPort = localPort;
        this.remotePort = remotePort;
        this.sshUser = sshUser;
        this.sshHost = sshHost;
        this.privateKeyPath = privateKeyPath;
    }

    create(): Promise<string> {
        return new Promise((resolve, reject) => {
            const sshCommand = 'ssh';
            const sshArgs = [
                '-L',
                `${this.localPort}:localhost:${this.remotePort}`,
                '-i', this.privateKeyPath,
                '-N',
                `${this.sshUser}@${this.sshHost}`
            ];

            this.tunnelProcess = spawn(sshCommand, sshArgs);
            this.tunnelProcess.on('error', (err: Error) => {
                reject(new Error(`Failure at SSH tunnel: ${err.message}`));
            });

            // this.tunnelProcess.on('exit', (code: number | null) => {
            //     if (code === 0) {
            //         resolve('SSH tunnel created successfully');
            //     } else {
            //         reject(new Error(`SSH tunnel failed to start with exit code ${code}`));
            //     }
            // });
            setTimeout(() => {
                if (this.tunnelProcess && !this.tunnelProcess.killed) {
                    resolve('SSH tunnel created successfully');
                } else {
                    reject(new Error('SSH tunnel failed to start'));
                }
            }, 1000);

            if (this.tunnelProcess.stderr) {
                this.tunnelProcess.stderr.on('data', (data: Buffer) => {
                    console.error(`SSH Error: ${data.toString()}`);
                    resolve('SSH tunnel created successfully');
                });
            }
        });
    }

    destroy(): Promise<string> {
        // return new Promise((resolve, reject) => {
        //     const cmd = [
        //         'ssh',
        //         '-L',
        //         `${this.localPort}:localhost:${this.remotePort}`,
        //         '-i', this.privateKeyPath,
        //         '-N', '-f',
        //         `${this.sshUser}@${this.sshHost}`
        //     ];
        //     const killCommand = `pkill -f "${cmd.join(' ')}"`;

        //     exec(killCommand, (error, stdout, stderr) => {
        //         if (error) {
        //             reject(new Error(`Failed to destroy SSH tunnel: ${error.message}`));
        //         } else {
        //             resolve('SSH tunnel destroyed successfully');
        //         }
        //     });
        // });
        return new Promise((resolve, reject) => {
            if (this.tunnelProcess) {
                this.tunnelProcess.kill('SIGKILL');
                this.tunnelProcess.on('close', (code: number | null) => {
                    this.tunnelProcess = null;
                    resolve(`SSH tunnel destroyed with exit code ${code}`);
                });
            } else {
                resolve('No active SSH tunnel to destroy');
            }
        });
    }
}