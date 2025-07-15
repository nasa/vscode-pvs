import path from "path";
import * as fs from 'fs';
import * as net from 'net';
import winston from 'winston';
import { CommandLineArgs } from "./types";
import crypto from 'crypto';
import { parse } from 'ts-command-line-args';
import * as os from 'os';

export function fileExistsCheck(fname: string): boolean {
	return pathExists(fname);
}
export function pathExists(path: string): boolean {
	if (path) {
		let ans: boolean = false;
		path = tildeExpansion(path);
		try {
			ans = fs.existsSync(path);
		} catch (readError) {
		} finally {
			return ans;
		}
	}
	return false;
}
export function tildeExpansion(pathName: string): string {
	if (pathName) {
		if (pathName.startsWith("~/") || pathName === "~") {
            const HOME_DIR: string = require('os').homedir();
			pathName = pathName.replace("~", HOME_DIR);
		}
		return path.normalize(pathName);
	}
	return pathName;
}



export function findAvailablePort(startPort: number = 23456): Promise<number> {
  return new Promise((resolve, reject) => {
    function tryPort(port: number): void {
      const server: net.Server = net.createServer();

      server.once('error', (err: NodeJS.ErrnoException) => {
        if (err.code === 'EADDRINUSE') {
          tryPort(port + 1);
        } else {
          reject(err);
        }
      });

      server.once('listening', () => {
        server.close(() => {
          resolve(port);
        });
      });

      server.listen(port);
    }

    tryPort(startPort);
  });
}

export function forceLocale (): void {
	process.env["ACL_LOCALE"] = "en_US.UTF-8";
	process.env["LC_ALL"] = "en_US.UTF-8";
	process.env["LANG"] = "en_US.UTF-8";
}

// Setup winston logger
export const logger = winston.createLogger({
  level: 'info',
  format: winston.format.simple(),
  transports: [
    new winston.transports.Console({
      format: winston.format.simple(),
    }),
    new winston.transports.File({ filename: 'server.log', maxFiles: 5, maxsize: 5242880 }) // 5MB file size limit
  ]
});

export const secretKey: string = crypto.randomBytes(64).toString('hex');

export const parseArgs = parse<CommandLineArgs>({
  dataPath: {
    type: String,
    defaultValue: '/tmp/pvs-sync-server',
    description: 'Path to store server data.'
  },
  port: {
    type: Number,
    defaultValue: 8080,
    description: 'Port for the WebSocket server.'
  },
  pvsPath: {
    type: String,
    defaultValue: path.join(os.homedir(), 'pvs8', 'PVS'),
    description: 'Full path to the PVS8 installation directory.'
  },
  time: {
    type: Number,
    optional: true
  },
});
