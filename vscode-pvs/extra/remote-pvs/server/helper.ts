import { ClientMessage, PathMap, ProcessDescription, ServerMessage } from "./types";
import WebSocket from 'ws';
import { logger, secretKey } from "./utils";
import { v4 as uuidv4 } from 'uuid';
import jwt from 'jsonwebtoken';
import { basePath } from "./server";
import path from 'path';
import fs from 'fs';
import crypto from 'crypto';

export const retrieveClientId = (data: ClientMessage, ws: WebSocket): string => {
    let clientId = '';
    if (!data.token_str) {
        clientId = uuidv4();
        const token = jwt.sign({ clientId }, secretKey);
        const response: ServerMessage = { type: 'send-token', token_str: token };
        logger.info(`Sending new token to client ${clientId}`);
        ws.send(JSON.stringify(response));
        createClientDirectory(clientId);
    } else {
        try {
            const decoded = jwt.verify(data.token_str, secretKey) as jwt.JwtPayload;
            clientId = decoded.clientId;
            createClientDirectory(clientId);
        } catch (error) {
            logger.info("Invalid token, generating new token");
            clientId = uuidv4();
            const token = jwt.sign({ clientId }, secretKey);
            const response: ServerMessage = { type: 'send-token', token_str: token };
            ws.send(JSON.stringify(response));
            logger.info(`Sending new token to client ${clientId}`);
            createClientDirectory(clientId);
        }
    }
    return clientId;
};

const createClientDirectory = (clientId: string): void => {
    const clientDir = path.join(basePath, clientId);
    if (!fs.existsSync(clientDir)) {
        fs.mkdirSync(clientDir, { recursive: true });
        fs.mkdirSync(path.join(clientDir, 'workspace'), { recursive: true });
        fs.mkdirSync(path.join(clientDir, 'lib'), { recursive: true });
    }
}

export function handlePathSync(clientId: string, workspacePaths: string[], libPaths: string[], ws: WebSocket, clientFolders: Map<string, PathMap>,processes: Map<string, ProcessDescription>,workspacepath: string,id?: string): void {
    const workspace = processPaths(clientId, workspacePaths, 'workspace',clientFolders,processes,workspacepath);
    const lib = processPaths(clientId, libPaths, 'lib',clientFolders,processes,workspacepath);
  
    const response: ServerMessage = {
      type: 'return-path-sync',
      syncPathsResponse: { workspacePaths: workspace, libPaths: lib , id: id||'' }
    };
    ws.send(JSON.stringify(response));
}
  
function processPaths(clientId: string, paths: string[], type: 'workspace' | 'lib',clientFolders: Map<string, PathMap>,processes: Map<string, ProcessDescription>,workspacepath: string): Record<string, string> {
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
    const clientProc = processes.get(JSON.stringify([workspacepath, clientId]));
    if (type === 'lib' && clientProc && clientProc.ws) {
      logger.info(`Adding path : ${pathsResult[localPath]} to pvs process`);
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