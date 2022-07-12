import * as vscode from 'vscode';
import * as utils from "../common/languageUtils";
import { LanguageClient } from 'vscode-languageclient';

interface SnippetDescriptor {
    description: string,
    prefix: string,
    scope: string,
    body: string[]
};
interface SnippetMap { [key: string]: SnippetDescriptor };
const snippetMap: {[key: string]: SnippetMap } = {
    theory: {
        "Autocomplete theory declaration": {
            "description": "Autocomplete theory declaration",
            "prefix": "theory",
            "scope": "pvs",
            "body": [
                "${TM_CURRENT_LINE/\\s*(\\b[\\w\\W\\s]+)\\s*:(\\s*\\w+)//g}THEORY",
                "  BEGIN $0",
                "    $2",
                "  END ${TM_CURRENT_LINE/\\s*(\\b\\w+)(\\b[\\w\\W\\s]*):(\\s*\\w+)/$1/g}"
            ]
        },
        "Theory template": {
            "description": "Theory template",
            "prefix": "theory-template",
            "scope": "pvs",
            "body": [
                "${1:theoryName}: THEORY",
                "  BEGIN $0",
                "    $2",
                "  END $1"
            ]
        }
    },
    datatypes: {
        "Autocomplete datatype declaration": {
            "description": "Autocomplete datatype declaration",
            "prefix": "datatype",
            "scope": "pvs",
            "body": [
                "${TM_CURRENT_LINE/\\s*(\\b[\\w\\W\\s]+)\\s*:(\\s*\\w+)//g}DATATYPE",
                "  BEGIN $0",
                "    $2",
                "  END ${TM_CURRENT_LINE/\\s*(\\b\\w+)(\\b[\\w\\W\\s]*):(\\s*\\w+)/$1/g}"
            ]
        },
        "Datatype template": {
            "description": "datatype template",
            "prefix": "datatype-template",
            "scope": "pvs",
            "body": [
                "${1:datatypeName}: DATATYPE",
                "  BEGIN $0",
                "    $2",
                "  END $1"
            ]
        }
    },
    conditionals: {
        "if-then-else template": {
            "description": "if-then-else template",
            "prefix": "if-then-else",
            "scope": "pvs",
            "body": [
                "IF ${1:cond} THEN ${2:expr1} ELSE ${3:expr2} ENDIF"
            ]
        },
        "cond block template": {
            "description": "cond template",
            "prefix": "cond",
            "scope": "pvs",
            "body": [
                "COND",
                "  ${1:expression} -> ${2:action}",
                "ENDCOND"
            ]
        }
    }
};

export class VSCodePvsSnippetsProvider implements vscode.CompletionItemProvider {
    protected client: LanguageClient;
    protected context: vscode.ExtensionContext;

    protected theoryCompletionMap: { [key: string]: vscode.CompletionItem[] } = {};

    constructor (client: LanguageClient) {
		this.client = client;
    }

    /**
     * Registers the snippets provider
     * @param context Extension context
     */
    activate (context: vscode.ExtensionContext) {
        this.context = context;
        vscode.languages.registerCompletionItemProvider({ scheme: 'file', language: 'pvs' }, this);//, metaData?: DocumentSymbolProviderMetadata);
        
        const keys: string[] = Object.keys(snippetMap);
        for (let k = 0; k < keys.length; k++) {
            const map: SnippetMap = snippetMap[keys[k]];
            const names: string[] = Object.keys(map);
            for (let i = 0; i < names.length; i++) {
                this.theoryCompletionMap[keys[k]] = this.theoryCompletionMap[keys[k]] || [];
                this.theoryCompletionMap[keys[k]].push({
                    label: map[names[i]].prefix,
                    detail: map[names[i]].description,
                    insertText: new vscode.SnippetString(map[names[i]].body.join("\n")),
                    kind: vscode.CompletionItemKind.Snippet
                });
            }
        }
    }
    /**
     * Standard LSP method of the snippets provider
     */
    provideCompletionItems(document: vscode.TextDocument, position: vscode.Position, token: vscode.CancellationToken, context: vscode.CompletionContext): vscode.ProviderResult<vscode.CompletionItem[] | vscode.CompletionList> {
        let ans: vscode.CompletionItem[] = [];
        const txt: string = document.getText();

        const lines: string[] = txt.split("\n");
        if (lines && lines.length > position.line) {
            const lineText: string = lines[position.line];
            const currentInput: string = lineText.substring(0, position.character).trim();
            // let lastWord: string = currentInput.substring(currentInput.lastIndexOf(" ") + 1);
            // lastWord = (lastWord.includes(":")) ? lastWord.substring(lastWord.lastIndexOf(":") + 1).trim() : lastWord;
            // lastWord = (lastWord.includes("`")) ? lastWord.substring(lastWord.lastIndexOf("`") + 1).trim() : lastWord;

            const theoryDeclared: boolean = new RegExp(utils.isense.theoryDeclaration).test(txt);
            const declaration: boolean = new RegExp(utils.isense.declaration).test(currentInput);
            if (!theoryDeclared) {
                ans = ans.concat(this.theoryCompletionMap["theory"]);
            } 
            if (declaration) {
                ans = ans.concat(this.theoryCompletionMap["datatypes"]);
            }
            ans = ans.concat(this.theoryCompletionMap["conditionals"]);
        }
        return ans;
    }
}
