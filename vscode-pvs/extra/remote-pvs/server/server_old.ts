import WebSocket from 'ws';
import { v4 as uuidv4 } from 'uuid';
import jwt from 'jsonwebtoken';
import fs from 'fs';
import path from 'path';
import crypto from 'crypto';
import * as os from 'os';
import { installPvsMain } from './installPvs';
import { PvsProcess } from './pvsProcess';

interface ClientMessage {
  type: string;
  token: string;
  workspacePaths?: string[];
  libPaths?: string[];
  function?: string;
  method?: string;
  params?: string[];
  jsonrpc?: string;
  id?: string;
}

interface ProcessDescription {
  pvs?: PvsProcess;
  ws?: WebSocket;
  ws_interrupt?: WebSocket;
}

interface ServerMessage {
  type: string;
  token?: string;
  syncPathsResponse?: {
    workspacePaths?: Record<string, string>;
    libPaths?: Record<string, string>;
    id?: string;
  };
  portActiveResponse?: {
    port?: number;
    code?: number;
  };
  functionResponse?: {
    functionName: string;
    output: string;
    id: string;
  };
}

interface PathMap {
  workspace: Record<string, string>;
  lib: Record<string, string>;
}

interface CommandLineArgs {
  dataPath?: string;
  pvsPath?: string;
}

const secretKey: string = crypto.randomBytes(64).toString('hex');

function parseArgs(args: string[]): CommandLineArgs {
  const params: CommandLineArgs = {};
  for (let i = 0; i < args.length; i++) {
    if (args[i] === '-data' && i + 1 < args.length) {
      params.dataPath = args[i + 1];
      i++;
    } else if (args[i] === '-pvspath' && i + 1 < args.length) {
      params.pvsPath = args[i + 1];
      i++;
    }
  }
  return params;
}

const args = parseArgs(process.argv.slice(2));
const basePath: string = args.dataPath || '/tmp/pvs-sync-server';
const clientFolders: Map<string, PathMap> = new Map();
const processes: Map<string, ProcessDescription> = new Map();

const pvsPath: string = args.pvsPath || path.join(os.homedir(), 'pvs8', 'PVS');

console.log(`Server initialized with PVS path: ${pvsPath}`);
installPvsMain(pvsPath);

const wss = new WebSocket.Server({ port: 8080 });
console.log('WebSocket server started on port 8080');

if (fs.existsSync(basePath)) {
  fs.rmSync(basePath, { recursive: true });
}
fs.mkdirSync(basePath, { recursive: true });

const createClientFolders = (data: ClientMessage, ws: WebSocket): string => {
  let clientId = '';
  if (!data.token) {
    clientId = uuidv4();
    const token = jwt.sign({ clientId }, secretKey, { expiresIn: '24h' });
    const response: ServerMessage = { type: 'token', token };
    ws.send(JSON.stringify(response));
    createClientDirectory(clientId);
  } else {
    try {
      const decoded = jwt.verify(data.token, secretKey) as jwt.JwtPayload;
      clientId = decoded.clientId;
      createClientDirectory(clientId);
    } catch (error) {
      console.log("Invalid token, generating new token");
      clientId = uuidv4();
      const token = jwt.sign({ clientId }, secretKey, { expiresIn: '24h' });
      const response: ServerMessage = { type: 'token', token };
      ws.send(JSON.stringify(response));
      createClientDirectory(clientId);
    }
  }
  return clientId;
};

wss.on('connection', (ws: WebSocket) => {
  let clientId: string = '';
  ws.on('message', async (message: WebSocket.Data) => {
    let data: ClientMessage = JSON.parse(message.toString());
    clientId = createClientFolders(data, ws);
    let pvsProc: PvsProcess;
    let client: ProcessDescription | undefined;
    let port: number | undefined;
    let prev: ProcessDescription;

    if (data.type === 'connect-interrupt') {
      console.log(`Received Connect Interuppt Request from ${clientId}`);
      client = processes.get(clientId);
      if (client && client.pvs) {
        pvsProc = client.pvs;
      } else {
        pvsProc = new PvsProcess(pvsPath, clientId, ws);
      }
      const activationCode = await pvsProc.activate();
      console.log(`PVS process activated for client ${clientId} with code ${activationCode}`);
      port = pvsProc.getReportedServerPort();
      const ws_to_pvs_interrupt = new WebSocket(`ws://localhost:${port}`);

      ws_to_pvs_interrupt.on("open", () => {
        console.log(`Interrupt connection opened to PVS Server on port ${port} for client ${clientId}`);
        const response: ServerMessage = {
          type: 'port-active-interrupt',
          portActiveResponse: { port, code: activationCode }
        };
        ws.send(JSON.stringify(response));
        console.log(response);
      });

      ws_to_pvs_interrupt.on('message', (msg: string) => {
        const obj = JSON.parse(msg);
        if (obj.id && ("result" in obj || "error" in obj || "method" in obj)) {
          ws.send(msg);
          console.log(msg);
        }
      });

      prev = processes.get(clientId) || {};
      processes.set(clientId, { ...prev, pvs: pvsProc, ws_interrupt: ws_to_pvs_interrupt });
    } else if (data.type === 'connect') {
      console.log(`Connection request received from client ${clientId}`);
      client = processes.get(clientId);
      if (client && client.pvs) {
        pvsProc = client.pvs;
      } else {
        pvsProc = new PvsProcess(pvsPath, clientId, ws);
      }
      const activationCode = await pvsProc.activate();
      port = pvsProc.getReportedServerPort();
      console.log(`Attempting to connect WebSocket to PVS server on port ${port}`);

      try {
        const ws_to_pvs_Server = new WebSocket(`ws://localhost:${port}`);
        ws_to_pvs_Server.on("open", () => {
          console.log(`Connection opened to PVS Server on port ${port} for client ${clientId}`);
          prev = processes.get(clientId) || {};
          processes.set(clientId, { ...prev, pvs: pvsProc, ws: ws_to_pvs_Server });
          handlePathSync(clientId, data.workspacePaths || [], data.libPaths || [], ws);
          const responsePort: ServerMessage = {
            type: 'port-active',
            portActiveResponse: { port, code: activationCode }
          };
          ws.send(JSON.stringify(responsePort));
          console.log(responsePort);
        });

        ws_to_pvs_Server.on('message', (msg: string) => {
          const obj = JSON.parse(msg);
          if (obj.id && ("result" in obj || "error" in obj || "method" in obj)) {
            console.log(`Forwarding message from PVS server to client ${clientId}`);
            ws.send(msg);
            console.log(msg);
          }
        });
      } catch (err) {
        console.error(`Error connecting to PVS server for client ${clientId}:`, err);
      }
    } else if (data.type === 'path-sync') {
      console.log(`Path sync request received from client ${clientId}`);
      handlePathSync(clientId, data.workspacePaths || [], data.libPaths || [], ws, data.id || '');
    } else if (data.type === 'invoke-pvs-process-method') {
      const proc = processes.get(clientId);
      let out = '';
      if (proc && data.function && proc.pvs && data.id) {
        if (data.function === "getLispInterfaceOutput") {
          out = proc.pvs.getLispInterfaceOutput();
        } else if (data.function === "clearLispInterfaceOutput") {
          proc.pvs.clearLispInterfaceOutput();
        }
      }
      if (out && data.id) {
        const response: ServerMessage = {
          type: 'function-response',
          functionResponse: { functionName: data.function || '', output: out, id: data.id }
        };
        ws.send(JSON.stringify(response));
      }
    } else if (data.type === 'call-pvs') {
      console.log(`Call PVS request received from client ${clientId}`);
      if ('id' in data && 'method' in data) {
        const params = data.params || [];
        const req = { method: data.method, params: params, jsonrpc: "2.0", id: data.id };
        const jsonReq: string = JSON.stringify(req, null, " ");
        client = processes.get(clientId);
        if (client && client.ws) {
          console.log(`Sending request to PVS server for client ${clientId}`);
          client.ws.send(jsonReq);
        } else {
          console.log(`No process found for client ${clientId}`);
        }
      }
    } else if (data.type === 'call-pvs-interrupt') {
      if ('id' in data && 'method' in data) {
        const params = data.params || [];
        const req = { method: data.method, params: params, jsonrpc: "2.0", id: data.id };
        const jsonReq: string = JSON.stringify(req, null, " ");
        client = processes.get(clientId);
        if (client && client.ws_interrupt) {
          client.ws_interrupt.send(jsonReq);
        } else {
          console.log(`No interrupt process found for client ${clientId}`);
        }
      }
    }
  });

  ws.on('close', async () => {
    console.log(`Connection closed for client ${clientId}`);
    const proc = processes.get(clientId);
    if (proc) {
      if (proc.ws) {
        proc.ws.close();
        proc.ws.terminate();
        proc.ws.removeAllListeners();
      }
      if (proc.ws_interrupt) {
        proc.ws_interrupt.close();
        proc.ws_interrupt.terminate();
        proc.ws_interrupt.removeAllListeners();
      }
      if (proc.pvs) {
        await proc.pvs.kill();
        delete proc.pvs;
      }
      processes.delete(clientId);
    }
  });
});

function createClientDirectory(clientId: string): void {
  const clientDir = path.join(basePath, clientId);
  if (!fs.existsSync(clientDir)) {
    fs.mkdirSync(clientDir, { recursive: true });
    fs.mkdirSync(path.join(clientDir, 'workspace'), { recursive: true });
    fs.mkdirSync(path.join(clientDir, 'lib'), { recursive: true });
  }
}

function handlePathSync(clientId: string, workspacePaths: string[], libPaths: string[], ws: WebSocket, id?: string): void {
  const workspace = processPaths(clientId, workspacePaths, 'workspace');
  const lib = processPaths(clientId, libPaths, 'lib');

  const response: ServerMessage = {
    type: 'return-path-sync',
    syncPathsResponse: { workspacePaths: workspace, libPaths: lib , id: id||'' }
  };
  ws.send(JSON.stringify(response));
  console.log(response);
}

function processPaths(clientId: string, paths: string[], type: 'workspace' | 'lib'): Record<string, string> {
  const clientDir = path.join(basePath, clientId, type);
  const pathMap = clientFolders.get(clientId) || { workspace: {}, lib: {} };
  const pathsResult: Record<string, string> = {};

  paths.forEach(localPath => {
    if (pathMap[type][localPath]) {
      if (!fs.existsSync(pathMap[type][localPath])) {
        fs.mkdirSync(pathMap[type][localPath], { recursive: true });
      }
    } else {
      const hashedPath = hashPath(localPath);
      pathMap[type][localPath] = path.join(clientDir, hashedPath);
      fs.mkdirSync(pathMap[type][localPath], { recursive: true });
    }
    pathsResult[localPath] = pathMap[type][localPath];
    const clientProc = processes.get(clientId);
    if (type === 'lib' && clientProc && clientProc.ws) {
      console.log(`Adding path : ${pathsResult[localPath]} to pvs process`);
      addLibraryToPVS(pathsResult[localPath], clientProc.ws);
    }
  });

  clientFolders.set(clientId, pathMap);
  return pathsResult;
}

function hashPath(localPath: string): string {
  return crypto.createHash('sha256').update(localPath).digest('hex');
}

function addLibraryToPVS(path: string, ws_to_pvs: WebSocket) {
  const req = { method: 'add-pvs-library', params: [path], jsonrpc: "2.0" };
  const jsonReq: string = JSON.stringify(req, null, " ");
  ws_to_pvs.send(jsonReq);
}