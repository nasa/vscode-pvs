import * as path from 'path';

// -- flags to control what is displayed
const VERBOSE: boolean = false;

// folders with the example specifications
export const helloworldExamples: string = path.join(__dirname, "helloworld");
export const sandboxExamples: string = path.join(__dirname, "sandbox");
export const safeSandboxExamples: string = path.join(__dirname, "safesandbox");
export const radixExamples: string = path.join(__dirname, "radix_sort_7");
export const dependable_plus_safe: string = path.join(__dirname, "dependable+safe");
export const tacticalsExamples: string = path.join(__dirname, "tacticals");
export const pvsioExamples: string = path.join(__dirname, "pvsio");
export const mValueExamples: string = path.join(__dirname, "m-value");
export const mValue2Examples: string = path.join(__dirname, "m-value2");
export const pigeonhole: string = path.join(__dirname, "shankar", "pigeonhole");

export const stever: string = path.join(__dirname, "stever"); // examples courtesy of steve reeves (waikato university)
export const steverFiles: string[] = [
	"BCtheory",
	"types_and_constants",
	"etherlite_language",
	"PVSSingle",
	"FInalPVSDoubleandSIngle",
	"steve_analysis",
	"MDNumberpad",
	"pvsioweb_utils"
];
export const pillbox: string = path.join(__dirname, "pillboxv7"); // examples courtesy of michael harrison (newcastle university) and paolo masci (NIA/NASA LaRC)
export const pillboxFiles: string[] = [
	"firstpillchecks",
	"firstpillmodel",
	"pillboxchecks",
	"pilldispenser.types_and_constants",
	"pilldispensermodes"
];
export const pvsioweb: string = path.join(__dirname, "pvsioweb"); // examples from the pvsioweb distribution
export const pvsiowebFolders: string[] = [
	"alaris", "baxter", "calc", "data_entry", "fcu", "giip", "gpca", "ice", "mt32"
];
export const pvsiowebFiles: string[] = [
	"alaris/alarisGH_AsenaCC",
	"alaris/alarisPC8100PumpModule",
	"alaris/limits",
	"alaris/alarisGP",
	"alaris/constants",

	"baxter/baxterSigmaSpectrum",
	"baxter/constants",
	"baxter/limits",
	"baxter/MDNumberpad",

	"calc/display",
	"calc/utils",

	"data_entry/arcomedical",
	"data_entry/bbraun_space",
	"data_entry/constants",
	"data_entry/emucharts_MedtronicMinimed530GSafe_th",
	"data_entry/emucharts_MedtronicMinimed530G_th",
	"data_entry/emucharts_NikiT34",
	"data_entry/emucharts_prototype_2016_11_21_th",
	"data_entry/limits",
	"data_entry/medfusion3500",
	"data_entry/zimed",

	"fcu/consistency",
	"fcu/FCUDataEntry",
	"fcu/FCU_properties",
	"fcu/emucharts_fcusoftware",
	"fcu/limits",
	"fcu/emucharts_FCU_th",
	"fcu/reversibility",

	"giip/basal_profiles",
	"giip/GIIP",

	"gpca/AlarmingComponent",
	"gpca/drugLib",
	"gpca/reference_model_th",
	"gpca/GPCA_Controller",
	"gpca/refined_model_th",
	"gpca/GPCA_Hardware_emulation",
	"gpca/GPCA_Language",
	"gpca/simulatorConstants",
	"gpca/GPCA_Limits",
	"gpca/StartInfusionEvents",
	"gpca/GPCA_Simulink_Controller_Bridge",
	"gpca/types_and_constants",
	"gpca/gpcaUI",
	"gpca/utility_functions",
	"gpca/main",

	"ice/main",
	"ice/patient_monitor",
	"ice/pump_rc",

	"mt32/CT64",
	"mt32/main",
	"mt32/MT32"
];

// load configuration indicating the installation folder of pvs
export const configFile: string = path.join(__dirname, "test.config");
console.info(`Loading configuration file ${configFile}`);

// utility functions
export function label(l: string): void {
	if (VERBOSE) {
		console.info(`\n==== ${l} ====`);
	}
}
export function log(...args:any) {
	if (VERBOSE) {
		console.log();
		console.log(args);
	}
}
export function dir(...args:any) {
	if (VERBOSE) {
		console.log();
		console.dir(args, { depth: null });
	}
}