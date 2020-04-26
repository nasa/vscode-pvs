import * as path from 'path';

// -- flags to control what is displayed
const VERBOSE: boolean = false;

// folders with the example specifications
export const sandboxExamples: string = path.join(__dirname, "sandbox");
export const safeSandboxExamples: string = path.join(__dirname, "safesandbox");
export const radixExamples: string = path.join(__dirname, "radix_sort_7");
export const stever: string = path.join(__dirname, "stever"); // examples courtesy of steve reeves (waikato university)
export const steverFiles: string[] = [
	"BCtheory",
	"types_and_constants",
	"etherlite_language",
	"PVSSingle",
	"FInalPVSDoubleandSIngle",
	"steve_analysis"
];
export const pillbox: string = path.join(__dirname, "pillboxv7"); // examples courtesy of michael harrison (newcastle university) and paolo masci (NIA/NASA LaRC)
export const pillboxFiles: string[] = [
	"firstpillchecks",
	"pillboxchecks",
	"firstpillmodel",
	"main",
	"pilldispensermodes",
	"pilldispenser.types_and_constants"
];
export const pvsioweb: string = path.join(__dirname, "pvsioweb"); // examples from the pvsioweb distribution
export const pvsiowebFiles: string[] = [
	"alaris2lnewmodes.pump",
	"drugLib",
	"alaris2lnewmodes",
	"alaris2lnewmodes.types_and_constants",
	"alarisGH_AsenaCC",
	"alarisGP",
	"limits",
	"alarisPC8100PumpModule",
	"main",
	"medfusion3500",
	"arcomedical",
	"MT32",
	"patient_monitor",
	"pillboxchecks",
	"pilldispensermodes",
	"basal_profiles",
	"pilldispenser.types_and_constants",
	"baxterSigmaSpectrum",
	"pump_rc",
	"bbraun_space",
	"reference_model_th",
	"consistency",
	"refined_model_th",
	"constants",
	"reversibility",
	"conversions",
	"simulatorConstants",
	"CT64",
	"StartInfusionEvents",
	"display",
	"emucharts_fcusoftware",
	"emucharts_FCU_th",
	"emucharts_MedtronicMinimed530GSafe_th",
	"emucharts_MedtronicMinimed530G_th",
	"emucharts_NikiT34",
	"emucharts_prototype_2016_11_21_th",
	"FCUDataEntry",
	"FCU_properties",
	"GIIP",
	"GPCA_Hardware_emulation",
	"types_and_constants",
	"GPCA_Simulink_Controller_Bridge",
	"gpcaUI",
	"utility_functions",
	"utils",
	"zimed"
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