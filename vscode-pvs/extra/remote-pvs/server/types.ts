import { PvsProcess } from "./pvsProcess";
import WebSocket from 'ws';

export interface ClientMessage {
  type: string;
  token_str: string;
  workspacePaths?: string[];
  libPaths?: string[];
  function?: string;
  method?: string;
  params?: string[];
  jsonrpc?: string;
  id?: string;
  workspace?: string;
}

export interface ProcessDescription {
  pvs?: PvsProcess;
  ws?: WebSocket;
  ws_interrupt?: WebSocket;
}

export interface ServerMessage {
  type: string;
  token_str?: string;
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
  unzipDataResponse?: {
    success: boolean;
    msg: string;
    id?: string;
  }
}

export interface PathMap {
  workspace: Record<string, string>;
  lib: Record<string, string>;
}

export interface CommandLineArgs {
  dataPath: string;
  port: number;
  pvsPath: string;
  time?: number;
}
