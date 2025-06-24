import WebSocket from 'ws';
import fs from 'fs';
import path from 'path';
import * as os from 'os';
import { installPvsMain } from './installPvs';
import { PvsProcess } from './pvsProcess';
import { ClientMessage, ProcessDescription, ServerMessage, PathMap } from './types';
import { parseArgs, logger } from './utils';
import { exit } from 'process';
import { retrieveClientId, handlePathSync } from './helper';

export const basePath: string = parseArgs.dataPath || '/tmp/pvs_sync_server';
const clientFolders: Map<string, PathMap> = new Map();
const processes: Map<string, ProcessDescription> = new Map();
const pvsPath: string = parseArgs.pvsPath || path.join(os.homedir(), 'pvs8', 'PVS');
const connectionInfo = new WeakMap<WebSocket, { clientId: string, workspacePath: string }>();

async function initializeServer() {
  logger.info("---> Starting Remote PVS Server <---");
  logger.info("-- -> Arguments: ");
  logger.info(`-- --> pvs path: ${pvsPath} `);
  logger.info(`-- --> data path: ${basePath} `);
  try {
    await installPvsMain(pvsPath);
    logger.info(`Server initialized with PVS path: ${pvsPath}`);
  } catch (error) {
    logger.error('Error initializing PVS:', error);
    exit(1);
  }

  let wss: WebSocket.Server;
  try {
    const port = parseArgs.port || 8080;
    wss = new WebSocket.Server({ port, host: 'localhost' });
    logger.info(`Server started on port ${port} at localhost`);
  } catch (err) {
    logger.error('Error starting WebSocket server:', err);
    exit(1);
  }

  if (fs.existsSync(basePath)) {
    fs.rmSync(basePath, { recursive: true });
  }
  fs.mkdirSync(basePath, { recursive: true });

  wss.on('connection', handleConnection);
}

function handleConnection(ws: WebSocket) {
  ws.on('message', async (message: WebSocket.Data) => {
    const data: ClientMessage = JSON.parse(message.toString());
    let { clientId, workspacePath } = connectionInfo.get(ws) || { clientId: '', workspacePath: '' };
    if (clientId === '') {
      clientId = retrieveClientId(data, ws);
      workspacePath = data.workspace || '';
      connectionInfo.set(ws, { clientId, workspacePath });
    }

    switch (data.type) {
      case 'connect-interrupt':
        await handleConnectInterrupt(ws, data, clientId, workspacePath);
        break;
      case 'connect':
        await handleConnect(ws, data, clientId, workspacePath);
        break;
      case 'path-sync':
        handlePathSyncRequest(ws, data, clientId, workspacePath);
        break;
      case 'invoke-pvs-process-method':
        handleInvokePvsProcessMethod(ws, data, clientId, workspacePath);
        break;
      case 'call-pvs':
        handleCallPvs(ws, data, clientId, workspacePath);
        break;
      case 'call-pvs-interrupt':
        handleCallPvsInterrupt(ws, data, clientId, workspacePath);
        break;
      default:
        logger.warn(`Unknown message type received: ${data.type}`);
    }
  });

  ws.on('close', () => handleClose(ws));
}

async function handleConnectInterrupt(ws: WebSocket, data: ClientMessage, clientId: string, workspacePath: string) {
  logger.info(`Received Connect Interrupt Request from ${clientId}`);
  let pvsProc: PvsProcess;
  const client = processes.get(JSON.stringify([workspacePath, clientId]));

  if (client && client.pvs) {
    pvsProc = client.pvs;
  } else {
    pvsProc = new PvsProcess(pvsPath, clientId, ws);
  }

  const activationCode = await pvsProc.activate();
  logger.info(`PVS process activated for client ${clientId} with code ${activationCode}`);
  const port = pvsProc.getReportedServerPort();
  const ws_to_pvs_interrupt = new WebSocket(`ws://localhost:${port}`);

  ws_to_pvs_interrupt.on("open", () => {
    logger.info(`Interrupt connection opened to PVS Server on port ${port} for client ${clientId}`);
    const response: ServerMessage = {
      type: 'port-active-interrupt',
      portActiveResponse: { port, code: activationCode }
    };
    ws.send(JSON.stringify(response));
  });

  ws_to_pvs_interrupt.on('message', (msg: string) => {
    const obj = JSON.parse(msg);
    if (obj.id && ("result" in obj || "error" in obj || "method" in obj)) {
      ws.send(msg);
    }
  });

  const prev = processes.get(JSON.stringify([workspacePath, clientId])) || {};
  processes.set(JSON.stringify([workspacePath, clientId]), { ...prev, pvs: pvsProc, ws_interrupt: ws_to_pvs_interrupt });
}

async function handleConnect(ws: WebSocket, data: ClientMessage, clientId: string, workspacePath: string) {
  logger.info(`Connection request received from client ${clientId}`);
  let pvsProc: PvsProcess;
  const client = processes.get(JSON.stringify([workspacePath, clientId]));

  if (client && client.pvs) {
    pvsProc = client.pvs;
  } else {
    pvsProc = new PvsProcess(pvsPath, clientId, ws);
  }

  const activationCode = await pvsProc.activate();
  const port = pvsProc.getReportedServerPort();
  logger.info(`Attempting to connect WebSocket to PVS server on port ${port}`);

  try {
    const ws_to_pvs_Server = new WebSocket(`ws://localhost:${port}`);
    ws_to_pvs_Server.on("open", () => {
      logger.info(`Connection opened to PVS Server on port ${port} for client ${clientId}`);
      const prev = processes.get(JSON.stringify([workspacePath, clientId])) || {};
      processes.set(JSON.stringify([workspacePath, clientId]), { ...prev, pvs: pvsProc, ws: ws_to_pvs_Server });
      const responsePort: ServerMessage = {
        type: 'port-active',
        portActiveResponse: { port, code: activationCode }
      };
      ws.send(JSON.stringify(responsePort));
      handlePathSync(clientId, data.workspacePaths || [], data.libPaths || [], ws, clientFolders, processes, workspacePath);
    });

    ws_to_pvs_Server.on('message', (msg: string) => {
      const obj = JSON.parse(msg);
      if (obj.id && ("result" in obj || "error" in obj || "method" in obj)) {
        logger.info(`Forwarding message from PVS server to client ${clientId} : ${msg}`);
        ws.send(msg);
      }
    });
  } catch (err) {
    logger.error(`Error connecting to PVS server for client ${clientId}:`, err);
  }
}

function handlePathSyncRequest(ws: WebSocket, data: ClientMessage, clientId: string, workspacePath: string) {
  logger.info(`Path sync request received from client ${clientId}`);
  handlePathSync(clientId, data.workspacePaths || [], data.libPaths || [], ws, clientFolders, processes, workspacePath, data.id || '');
}

function handleInvokePvsProcessMethod(ws: WebSocket, data: ClientMessage, clientId: string, workspacePath: string) {
  const proc = processes.get(JSON.stringify([workspacePath, clientId]));
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
}

function handleCallPvs(ws: WebSocket, data: ClientMessage, clientId: string, workspacePath: string) {
  logger.info(`Call PVS request received from client ${clientId} : ${JSON.stringify(data)}`);
  if ('id' in data && 'method' in data) {
    const params = data.params || [];
    const req = { method: data.method, params: params, jsonrpc: "2.0", id: data.id };
    const jsonReq: string = JSON.stringify(req, null, " ");
    const client = processes.get(JSON.stringify([workspacePath, clientId]));
    if (client && client.ws) {
      logger.info(`Sending request to PVS server for client ${clientId}`);
      client.ws.send(jsonReq);
    } else {
      logger.info(`No process found for client ${clientId}`);
    }
  }
}

function handleCallPvsInterrupt(ws: WebSocket, data: ClientMessage, clientId: string, workspacePath: string) {
  if ('id' in data && 'method' in data) {
    const params = data.params || [];
    const req = { method: data.method, params: params, jsonrpc: "2.0", id: data.id };
    const jsonReq: string = JSON.stringify(req, null, " ");
    const client = processes.get(JSON.stringify([workspacePath, clientId]));
    if (client && client.ws_interrupt) {
      client.ws_interrupt.send(jsonReq);
    } else {
      logger.info(`No interrupt process found for client ${clientId}`);
    }
  }
}

async function handleClose(ws: WebSocket) {
  const info = connectionInfo.get(ws);
  if (!info) {
    logger.warn('Connection closed for unknown client');
    return;
  }

  const { clientId, workspacePath } = info;
  logger.info(`Connection closed for client ${clientId}`);

  try {
    const proc = processes.get(JSON.stringify([workspacePath, clientId]));
    if (proc) {
      if (proc.ws) {
        proc.ws.close();
        proc.ws.terminate();
        proc.ws.removeAllListeners();
        proc.ws = undefined;
        delete proc.ws;
      }
      if (proc.ws_interrupt) {
        proc.ws_interrupt.close();
        proc.ws_interrupt.terminate();
        proc.ws_interrupt.removeAllListeners();
        proc.ws_interrupt = undefined;
        delete proc.ws_interrupt;
      }
      if (proc.pvs) {
        await proc.pvs.kill();
        proc.pvs = undefined;
        delete proc.pvs;
      }
      processes.delete(JSON.stringify([workspacePath, clientId]));
    }

    // Clean up the connection info
    connectionInfo.delete(ws);
  } catch (err) {
    console.log("Error while cleanup:", err);
  }
}

initializeServer().catch(error => {
  logger.error('Failed to initialize server:', error);
  exit(1);
});
