/**
 * This file defines the data structures used by the prototype builder plugin to store/share information about the prototypes
 * Information is conceptually split in two elements: 'io' and 'web'
 * - io elements are for pvsio-related elements (e.g., theory name, pvs file name, etc)
 * - web elements are for web-related elements (e.g., picture, widgets, etc.)
 * File attributes is information to be stored in files
 * Data attributes extends file attributes with information necessary to complete the loading of a prototype (e.g., base64 data of the picture image, current context folder)
 */
export enum IoFileAttribute {
    description = "description",
    mainFile = "mainFile",
    mainModule = "mainModule",
    initialState = "initialState",
    tickFunction = "tickFunction",
    tickInterval = "tickInterval",
    toStringFunction = "toStringFunction"
};
export enum WebFileAttribute {
    pictureFile = "pictureFile",
    pictureWidth = "pictureWidth",
    pictureHeight = "pictureHeight",
    widgets = "widgets"
}
export enum DataAttribute {
    contextFolder = "contextFolder",
    pictureData = "pictureData"
};
export type PVSioWebDataAttribute = IoFileAttribute | WebFileAttribute | DataAttribute;
export interface ValueLabel { value: string, label?: string };
export declare interface IoFile {
    version?: string, // file version
    description?: ValueLabel, // brief description of the prototype
    mainFile?: ValueLabel, // name of the main file, including extension
    mainModule?: ValueLabel, // main function -- this is a theory in PVS
    initialState?: ValueLabel // initial state, constant literal or function call
    tickFunction?: ValueLabel, // tick function name
    tickInterval?: ValueLabel // tick interval
    toStringFunction?: ValueLabel // name of print function for converting states returned by the server in json format
}
export declare interface IoData extends IoFile {
    contextFolder: string
}
export declare interface WebFile {
    version?: string, // file version
    pictureFile?: string, // file name, including extension
    pictureWidth?: number,
    pictureHeight?: number,
    widgets?: any
}
export declare interface PVSioWebFile extends IoFile, WebFile {};
export declare interface WebData extends WebFile {
    pictureData?: string
}
export declare interface PrototypeData extends IoData, WebData {};

/**
 * Utility function, returns io attributes
 */
export function getIoFile (data: PrototypeData): IoFile {
    if (data) {
        const ans: IoFile = { };
        for (let key in IoFileAttribute) {
            ans[key] = data[key];
        }
        return ans;
    }
    return null;
}
/**
 * Utility function, returns web attributes
 */
export function getWebFile (data: PrototypeData): WebFile {
    if (data) {
        const ans: WebFile = { };
        for (let key in WebFileAttribute) {
            ans[key] = data[key];
        }
        return ans;
    }
    return null;
}

/**
 * Prototype Builder events
 */
export enum PrototypeBuilderEvents {
    // initial event, indicates that the plugin is ready to be used
    DidActivatePlugin = "DidActivatePlugin",
    DidLoadPrototypeData = "DidLoadPrototypeData",

    // file menu
    NewPrototype = "NewPrototype",
    SavePrototype = "SavePrototype",
    SaveAs = "SaveAs",
    OpenPrototype = "OpenPrototype",

    // run menu
    PauseSimulation = "PauseSimulation",
    RebootSimulation = "RebootSimulation",

    // switch events
    DidSwitchToSimulatorView = "DidSwitchToSimulatorView",
    DidSwitchToBuilderView = "DidSwitchToBuilderView",
    DidSwitchToSettingsView = "DidSwitchToSettingsView",

    // change events
    DidChangePicture = "DidChangePicture",
    DidUpdateWidgets = "DidUpdateWidgets",
    DidUpdateSettings = "DidUpdateSettings",
    DidRemovePicture = "DidRemovePicture"
};

/**
 * Event data produced by prototype builder
 */
export interface DidChangePictureEventData extends PrototypeData {
    new: Picture,
    old: Picture
};
export interface DidRemovePictureEventData extends PrototypeData {
    old: Picture
};

/**
 * Utility function for translating tick frequency in ms
 * When the frequency is given as a string, it may include units (ms or s)
 */
export function getFrequency (freq: string | number): number {
    if (freq) {
        if (typeof freq === "string") {
            return freq.endsWith("ms") ? parseFloat(freq)
                : freq.endsWith("s") ? 1000 * parseFloat(freq)
                : +freq;
        }
        return freq;
    }
    return 0;
}
/**
 * Picture file descriptor
 */
export interface Picture {
    fileName: string,
    fileExtension: string,
    fileContent: string
};
/**
 * Picture data
 */
export type PictureData = Picture & PictureSize;

/**
 * Name used to identify the whiteboard used when no picture is loaded
 */
export const whiteboardFile: string = "whiteboard.gif";


/**
 * Constants and data structures for pictures
 */
export const MIN_WIDTH: number = 800; //px
export const MIN_HEIGHT: number = 600; //px
export interface PictureSize {
    width: number,
    height: number
}
export const DEFAULT_PICTURE_SIZE: PictureSize = { width: MIN_WIDTH, height: MIN_HEIGHT };

/**
 * Default settings
 */
export const defaultIoSettings: IoFile = {
    description: { label: "Description", value: "" },
    mainModule: { label: "Theory Name", value: "" },
    mainFile: { label: "Main File", value: "" },
    initialState: { label: "Initial State", value: "init" },
    tickFunction: { label: "Tick Function (optional)", value: "" },
    tickInterval: { label: "Tick Interval (optional)", value: "" },
    toStringFunction: { label: "toString Function (optional)", value: "" }
};
export const defaultWebSettings: WebFile = {
    pictureFile: "",
    pictureWidth: DEFAULT_PICTURE_SIZE.width,
    pictureHeight: DEFAULT_PICTURE_SIZE.height
};
export interface SettingsOptions {
    contextFolder?: string,
    io?: IoFile,
    web?: WebFile
};