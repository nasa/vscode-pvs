import { spawn, exec } from 'child_process';

export class SSHTunnel {
    private localPort: number;
    private remoteHost: string;
    private remotePort: number;
    private sshUser: string;
    private sshHost: string;
    privateKeyPath: string;

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
                '-N', '-f',
                `${this.sshUser}@${this.sshHost}`
            ];

            const tunnelProcess = spawn(sshCommand, sshArgs);
            tunnelProcess.on('error', (err: Error) => {
                reject(new Error(`Failed to start SSH tunnel: ${err.message}`));
            });

            tunnelProcess.on('exit', (code: number | null) => {
                if (code === 0) {
                    resolve('SSH tunnel created successfully');
                } else {
                    reject(new Error(`SSH tunnel failed to start with exit code ${code}`));
                }
            });

            if (tunnelProcess.stderr) {
                tunnelProcess.stderr.on('data', (data: Buffer) => {
                    console.error(`SSH Error: ${data.toString()}`);
                });
            }
        });
    }

    destroy(): Promise<string> {
        return new Promise((resolve, reject) => {
            const cmd = [
                'ssh',
                '-L',
                `${this.localPort}:localhost:${this.remotePort}`,
                '-i', this.privateKeyPath,
                '-N', '-f',
                `${this.sshUser}@${this.sshHost}`
            ];
            const killCommand = `pkill -f "${cmd.join(' ')}"`;

            exec(killCommand, (error, stdout, stderr) => {
                if (error) {
                    reject(new Error(`Failed to destroy SSH tunnel: ${error.message}`));
                } else {
                    resolve('SSH tunnel destroyed successfully');
                }
            });
        });
    }
}