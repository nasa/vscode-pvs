import { Connection, TextDocument } from 'vscode-languageserver';
import { listTheoremsInFile, TheoremDescriptor, TheoremList } from '../common/languageUtils';
import * as fsUtils from '../common/fsUtils';


export class PvsStatusUpdater {
    readonly THEOREM_LIST_UPDATE_INTERVAL: number = 4000;
    private timers: {[key: string]: NodeJS.Timer } = {};
    constructor (connection: Connection) {
        if (connection) {
            connection.onDidSaveTextDocument(async (evt) => {
                if (evt && evt.textDocument && evt.textDocument.uri) {
                    const uri: string = evt.textDocument.uri;
                    const theorems: TheoremDescriptor[] = await listTheoremsInFile(uri);
                    const pvsContextFolder: string = fsUtils.getPathname(uri);
                    const fileName: string = fsUtils.getFilename(uri);
                    const theoremList: TheoremList = { theorems, fileName, pvsContextFolder };
                    connection.sendNotification('pvs.context.theorems.update', theoremList);
                }
            });
        }
        // this.timers["theorem-list"] = setTimeout(() => {
        //     if (connection) {
        //         utils.findTheorems
        //         connection.sendNotification('context.theorem-list.update', )
        //     }
        // }, this.THEOREM_LIST_UPDATE_INTERVAL);
    }
}