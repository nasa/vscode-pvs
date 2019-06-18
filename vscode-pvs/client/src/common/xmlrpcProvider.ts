/**
 * @module xmlrpcProvider
 * @author Paolo Masci
 * @date 2019.06.18
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

// import { Connection } from "vscode-languageserver";
import { ConnectionError } from "vscode-jsonrpc";

// - the PVS server accepts XML-RPC messages; the only method that can be invoked is pvs.request.
// - the parameter passed to pvs.request is a string encoding of a JSON-RPC message (see https://www.jsonrpc.org/specification)
// - the JSON-RPC message indicates the command to be executed by pvs, e.g., reset, typecheck, etc.

// '{"mode":"lisp","context":"~\\/Downloads\\/bugs\\/","jsonrpc_result":{
// 		"result":["change-context","help","lisp","list-client-methods","list-methods","names-info",
// 					"proof-command","prove-formula","reset","typecheck"
// 				],"jsonrpc":"2.0","id":"0"}}'

const xmlrpc = require("xmlrpc");

export interface XmlRpcError {
	code: string,
	message: string,
	stack: string
}
export interface PvsJsonError {
	code: number,
	message: string,
	data: any
}
export interface PvsJsonResponse {
	jsonrpc: string,
	id: string,
	result?: any,
	error?: PvsJsonError
}
export interface RpcPvsResponse {
	mode: string,
	context: string,
	jsonrpc_result?: PvsJsonResponse,
	"xmlrpc-error"?: string
}
export interface XmlRpcResponse {
	error: XmlRpcError,
	res: RpcPvsResponse
}

export class XmlRpcProxy {
	serverPort: number;
	client: any;
	server: any;
	constructor () {
		this.serverPort = 22334;
		this.client = xmlrpc.createClient({
			host: "localhost", port: 22334, path: "/RPC2"
		});
		this.server = xmlrpc.createServer({
			host: "localhost", port: 9090
		});
		this.server.on('info', (res) => {
			console.log("info");
		});
		this.server.on('warning', (res) => {
			console.log("warning");
		});
		this.server.on('debug', (res) => {
			console.log("debug");
		});
		this.server.on('buffer', (res) => {
			console.log("buffer");
		});
		this.server.on('yes-no', (res) => {
			console.log("yes-no");
		});
		this.server.on('dialog', (res) => {
			console.log("dialog");
		});
	}
    listMethods(): Promise<XmlRpcResponse> {
		return new Promise((resolve, reject) => {
			this.client.methodCall("system.listMethods", [ ], (error: XmlRpcError, value: string) => {
				const res: RpcPvsResponse = JSON.parse(value);
				resolve({ error, res });
			});
		});
    }
    methodHelp(methodName: string): Promise<XmlRpcResponse> {
		return new Promise((resolve, reject) => {
			this.client.methodCall("system.methodHelp", [ methodName ], (error: XmlRpcError, value: string) => {
				const res: RpcPvsResponse = JSON.parse(value);
				resolve({ error, res });
			});
		});
	}
	pvsRequest(method: string, args?: string[]): Promise<XmlRpcResponse> {
        let req = {
            jsonrpc: "2.0",
			method: method,
			id: new Date().getTime()
        };
        let jsonReq: string = JSON.stringify(req);//.replace(/"/g,'\\"');
		return new Promise((resolve, reject) => {
			this.client.methodCall("pvs.request", [ jsonReq ], (error: XmlRpcError, value: string) => {
				const res: RpcPvsResponse = JSON.parse(value);
				resolve({ error, res });
			});
		});
    }
	typecheck(fileName: string): Promise<XmlRpcResponse> {
        let req = {
            jsonrpc: "2.0",
			method: "typecheck",
			params: [ fileName ],
			id: new Date().getTime()
        };
        let jsonReq: string = JSON.stringify(req);//.replace(/"/g,'\\"');
		return new Promise((resolve, reject) => {
			this.client.methodCall("pvs.request", [ jsonReq, "http://localhost:9090" ], (error: XmlRpcError, value: string) => {
				const res: RpcPvsResponse = JSON.parse(value);
				resolve({ error, res });
			});
		});
    }
	changeContext(contextFolder: string): Promise<XmlRpcResponse> {
        let req = {
            jsonrpc: "2.0",
			method: "change-context",
			params: [ contextFolder ],
			id: new Date().getTime()
        };
        let jsonReq: string = JSON.stringify(req);//.replace(/"/g,'\\"');
		return new Promise((resolve, reject) => {
			this.client.methodCall("pvs.request", [ jsonReq, "http://localhost:9090" ], (error: XmlRpcError, value: string) => {
				const res: RpcPvsResponse = JSON.parse(value);
				resolve({ error, res });
			});
		});
	}
	lisp(cmd: string): Promise<XmlRpcResponse> {
        let req = {
            jsonrpc: "2.0",
			method: "lisp",
			params: [ cmd ],
			id: new Date().getTime()
        };
        let jsonReq: string = JSON.stringify(req);//.replace(/"/g,'\\"');
		return new Promise((resolve, reject) => {
			this.client.methodCall("pvs.request", [ jsonReq, "http://localhost:9090" ], (error: XmlRpcError, value: string) => {
				const res: RpcPvsResponse = JSON.parse(value);
				resolve({ error, res });
			});
		});
	}
}



function xmlRequest(jsonrpc: string): string {
    return '<?xml version="1.0"?>'
            + '\n<methodCall>'
            + '\n<methodName>pvs.request</methodName>'
            + '\n<params>'
            + '\n<param>'
            // + '\n<value><array><data>'
            + '\n<value><string>"' + jsonrpc + '"</string></value>'
            // + '\n</data></array></value>'
            + '\n</param>'
            + '\n</params>'
            + '\n</methodCall>';
};

class XmlRpcClient {
    // {
    //     "jsonrpc":"2.0",
    //     "id":"typecheck-file /home/nia/Work/nasagitlab/sandbox/examples/sri.pvs",
    //     "method":"typecheck",
    //     "params":"/home/nia/Work/nasa-gitlab/sandbox/examples/sri.pvs"
    // }      
    typecheck(fileName: string): string {
        let req = {
            jsonrpc: "2.0",
            id: "typecheck-file " + fileName,
            method: "typecheck",
            params: fileName
        };

        // let req = {
        //     jsonrpc: "2.0",
        //     method: "typecheck",
        //     params: "[ " + fileName + " ]",
        //     id: 0
        // };
        let str: string = JSON.stringify(req);
        let ans: string = xmlRequest(str); //encodeURIComponent ?
        return ans;
    }
    // {
    //     "jsonrpc":"2.0",
    //     "id":"list-methods",
    //     "method":"list-methods"
    //  }
    listMethods() {
        let req = {
            jsonrpc: "2.0",
			method: "list-methods",
			id: "list-methods"
        };
        let str: string = JSON.stringify(req);//.replace(/"/g,'\\"');
        let ans: string = xmlRequest(str);
        return ans;
    }
}

const pvsJson = {
    "$schema": "http://json-schema.org/schema#",
    "description": "JSON schema for PVS GUIs",
    "definitions": {
	"pvs.request": {
	    "description": "This is an XML-RPC request, not really JSON",
	    "type": "array",
	    "items": [ {"$ref": "#/definitions/pvs-json-request"},
		       {"schema": {"format": "uri"}} ]
	},
	"pvs.response": {
	    "description": "The non-error response to a pvs.request. Note that it may still be a pvs-json-error",
	    "type": "object",
	    "required": [ "mode", "context", "jsonrpc_result" ],
	    "properties": {
		"mode": {
		    "enum": ["lisp", "prover", "evaluator"]
		},
		"context": {
		    "description": "Pathname for the current PVS context",
		    "type": "string"
		},
		"jsonrpc_result": {
		    "oneOf": [{"$ref": "#/definitions/pvs-json-response"},
			      {"$ref": "#/definitions/pvs.error"}]
		}
	    }
	},
	"pvs.error": {
	    "description": "An XML-RPC response in case of an error where id is not available - o.w. a pvs.response is given",
	    "type": "object",
	    "required": [ "mode", "context", "xmlrpc-error" ],
	    "properties": {
		"mode": {
		    "enum": ["lisp", "prover", "evaluator"]
		},
		"context": {
		    "description": "Pathname for the current PVS context",
		    "type": "string"
		},
		"xmlrpc-error": {
		    "type": "string"
		}
	    }
	},
	"pvs-json-request": {
	    "type": "object",
	    "required": [ "jsonrpc", "method" ],
	    "properties": {
            "jsonrpc": {
                "description": "JSON-RPC version",
                "enum": ["2.0"]
            },
            "id": {
                "description": "A unique ID",
                "type": "string"
            }
	    },
	    "oneOf": [
		{"$ref": "#/definitions/list-methods-request"},
		{"$ref": "#/definitions/list-client-methods-request"},
		{"$ref": "#/definitions/help-request"},
		{"$ref": "#/definitions/change-context-request"},
		{"$ref": "#/definitions/typecheck-request"},
		{"$ref": "#/definitions/prove-formula-request"},
		{"$ref": "#/definitions/proof-command-request"},
		{"$ref": "#/definitions/names-info-request"},
		{"$ref": "#/definitions/lisp-request"}
	    ]
	},
	"pvs-json-response": {
	    "type": "object",
	    "required": [ "jsonrpc", "id" ],
	    "properties": {
		"jsonrpc": { "enum": ["2.0"]
		},
		"id": { "type": "string" }
	    },
	    "oneOf": [
		{"$ref": "#/definitions/pvs-json-error"},
		{"$ref": "#/definitions/pvs-json-result"}
	    ]
	},
	"pvs-json-error": {
	    "type": "object",
	    "required": [ "error" ],
	    "properties": {
		"error": {
		    "type": "object",
		    "required": [ "code", "message" ],
		    "properties": {
			"code": { "type": "integer"},
			"message": { "type": "string"},
			"data": {}
		    }
		}
	    }
	},
	"pvs-json-result": {
	    "type": "object",
	    "required": [ "result" ],
	    "anyOf": [
		{"$ref": "#/definitions/list-methods-result"},
		{"$ref": "#/definitions/list-client-methods-result"},
		{"$ref": "#/definitions/help-result"},
		{"$ref": "#/definitions/change-context-result"},
		{"$ref": "#/definitions/typecheck-result"},
		{"$ref": "#/definitions/prove-formula-result"},
		{"$ref": "#/definitions/proof-command-result"},
		{"$ref": "#/definitions/names-info-result"},
		{"$ref": "#/definitions/lisp-result"}
	    ]
	},
	"list-methods-request": {
	    "type": "object",
	    "properties": {
		"method": { "enum": ["list-methods"] }
	    }
	},
	"list-methods-result": {
	    "oneOf": [{"$ref": "#/definitions/string-array-result"}]
	},
	"list-client-methods-request": {
	    "type": "object",
	    "properties": {
		"method": { "enum": ["list-client-methods"] }
	    }
	},
	"list-client-methods-result": {
	    "oneOf": [{"$ref": "#/definitions/string-array-result"}]
	},
	"help-request": {
	    "type": "object",
	    "properties": {
		"method": { "enum": ["help"] },
		"params": { "type": "string" }
	    },
	    "required": ["params"]
	},
	"help-result": {
	    "type": "object",
	    "properties": {
		"result": { "type" : "object" }
	    }
	},
	"change-context-request": {
	    "type": "object",
	    "properties": {
		"method": { "enum": ["change-context"] },
		"params": { "type": "string" }
	    }
	},
	"change-context-result": {
	    "type": "object",
	    "properties": {
		"result": { "type" : "string" }
	    }
	},
	"typecheck-request": {
	    "type": "object",
	    "properties": {
		"method": { "enum": ["typecheck"] },
		"params": { "type": "string" }
	    }
	},
	"typecheck-result": {
	    "type": "object",
	    "properties": {
		"result": {
		    "type" : "array",
		    "items": {
			"type": "object",
			"properties": {
			    "id": {"type": "string"},
			    "decls": {
				"type": "array",
				"description": "an array of declarations in the given theory",
				"items": {
				    "type": "object",
				    "oneOf": [{"$ref": "#definitions/importing"},
					      {"$ref": "#definitions/typed-decl"},
					      {"$ref": "#definitions/formula-decl"}]
				}
			    }
			}
		    }
		}
	    }
	},
	"importing": {
	    "type": "object",
	    "properties": {
		"kind": {"enum": ["importing"]},
		"importing": {"type": "string"},
		"place": {"$ref": "#/definitions/place"}
	    }
	},
	"typed-decl": {
	    "type": "object",
	    "properties": {
		"id": {"type": "string"},
		"kind": {"enum": ["module", "type", "expr", "datatype",
				  "library", "judgement", "conversion",
				  "auto-rewrite"]},
		"type": {"type": "string"},
		"place": {"$ref": "#/definitions/place"}
	    }
	},
	"formula-decl": {
	    "type": "object",
	    "properties": {
		"id": {"type": "string"},
		"kind": {"enum": ["formula"]},
		"place": {"$ref": "#/definitions/place"}
	    }
	},
	"prove-formula-request": {
	    "type": "object",
	    "properties": {
		"method": { "enum": ["prove-formula"] },
		"params": {
		    "type" : "array",
		    "minItems": 1,
		    "maxItems": 1,
		    "items": { "type": "string" }
		}
	    }
	},
	"prove-formula-result": {
	    "type": "object",
	    "properties": {
		"result": {
		    "type" : "object"
		}
	    }
	},
	"proof-command-request": {
	    "type": "object",
	    "properties": {
		"method": { "enum": ["proof-command"] },
		"params": {
		    "type" : "array",
		    "minItems": 1,
		    "maxItems": 1,
		    "items": { "type": "string" }
		}
	    }
	},
	"proof-command-result": {
	    "type": "object",
	    "properties": {
		"result": {
		    "type" : "object"
		}
	    }
	},
	"proofstate": {
	    "type": "object",
	    "required": [ "current-goal", "tree-delta" ],
	    "properties": {
		"children": { "type": "array",
			      "items": {"$ref": "#/definitions/proofstate-node"}},
		"current-goal": { "type": "string" },
		"tree-delta": { "type": "string" }
	    }
	},
	"proofstate-node": {
	    "type": "object",
	    "required": [ "label", "sequent" ],
	    "properties": {
		"commentary": { "type": "array",
				"items": "string" },
		"action": { "type": "string" },
		"num-subgoals": { "type": "integer" },
		"label": { "type": "string" },
		"comment": { "type": "string" },
		"sequent": {
		    "type": "object",
		    "properties": {
			"antecedents": {"$ref": "#/definitions/s-formulas"},
			"succedents": {"$ref": "#/definitions/s-formulas"},
			"hidden-antecedents": {"$ref": "#/definitions/s-formulas"},
			"hidden-succedents": {"$ref": "#/definitions/s-formulas"}
		    }
		}
	    }
	},
	"s-formulas": {
	    "type": "array",
	    "items": {
		"type": "object",
		"required": ["labels", "changed", "formula"],
		"properties": {
		    "labels": {
			"type": "array",
			"minItems": 1,
			"items": {"type": "string"}
		    },
		    "changed": {"type": "boolean"},
		    "formula": {"type": "string"}
		}
	    }
	},
	"names-info-request": {
	    "type": "object",
	    "properties": {
		"method": { "enum": ["names-info"] },
		"params": {
		    "type" : "array",
		    "minItems": 1,
		    "maxItems": 1,
		    "items": { "type": "string" }
		}
	    },
	    "required": ["params"]
	},
	"names-info-result": {
	    "type": "array",
	    "items": {
		"type": "object",
		"properties": {
		    "result": {
			"type": "object",
			"properties": {
			    "id": {"type": "string"},
			    "place": {"$ref": "#/definitions/place"},
			    "decl": {"type": "string"},
			    "decl-file": {"type": "string"},
			    "decl-place": {"$ref": "#/definitions/place"}
			}
		    }
		}
	    }
	},
	"lisp-request": {
	    "type": "object",
	    "properties": {
		"method": { "enum": ["lisp"] },
		"params": {
		    "type" : "array",
		    "minItems": 1,
		    "maxItems": 1,
		    "items": { "type": "string" }
		}
	    },
	    "required": ["params"]
	},
	"lisp-result": {
	    "oneOf": [{"$ref": "#/definitions/string-result"}]
	},
	"string-result": {
	    "type": "object",
	    "properties": {
		"result": { "type" : "string" }
	    }
	},
	"string-array-result": {
	    "type": "object",
	    "properties": {
		"result": {
		    "type" : "array",
		    "minItems": 1,
		    "items": { "type": "string" }
		}
	    }
	},
	"place": {
	    "type": "array",
	    "minItems": 2,
	    "maxItems": 4,
	    "items": {"type": "integer"}
	}
    },
    "oneOf": [
	{"$ref": "#/definitions/pvs.request"},
	{"$ref": "#/definitions/pvs.response"},
	{"$ref": "#/definitions/pvs.error"},
	{"$ref": "#/definitions/pvs-json-request"},
	{"$ref": "#/definitions/pvs-json-response"}
    ]
}