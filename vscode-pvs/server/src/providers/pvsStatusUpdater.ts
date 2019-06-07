import { Connection, TextDocument } from 'vscode-languageserver';
import * as utils from '../common/languageUtils';
import { TheoriesMap } from '../common/serverInterface';
import * as fsUtils from '../common/fsUtils';


export class PvsStatusUpdater {
    readonly THEOREM_LIST_UPDATE_INTERVAL: number = 4000;
    private timers: {[key: string]: NodeJS.Timer } = {};
    constructor (connection: Connection) {
        if (connection) {
            connection.onDidSaveTextDocument(async (evt) => {
                if (evt && evt.textDocument && evt.textDocument.uri) {
                    const uri: string = evt.textDocument.uri;
                    const contextFolder: string = fsUtils.getContextFolder(uri);
                    const theoriesMap: TheoriesMap = await utils.listTheorems(contextFolder);
                    connection.sendNotification('pvs.context.theories-status.update', theoriesMap);
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