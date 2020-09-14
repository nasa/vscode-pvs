import * as pvsgui from "../server/src/common/pvs-gui.d";
import { ProofDescriptor } from "../server/src/common/serverInterface";

export const parse1_result: pvsgui.ParseResult =
    [ { id: 'sqrt',
    decls: 
     [ { importing: 'more_real_props',
         kind: 'importing',
         place: [ 11, 0, 11, 25 ] },
       { importing: 'sq', kind: 'importing', place: [ 14, 2, 14, 14 ] },
       { id: 'sqrt_exists',
         kind: 'formula',
         'proved?': null,
         'complete?': null,
         'has-proofscript?': null,
         place: [ 19, 2, 21, 21 ] },
       { id: 'sqrt',
         kind: 'expr',
         type: 'nil',
         place: [ 23, 2, 23, 34 ] },
       { id: 'sqrt_pos',
         kind: 'judgement',
         type: 'nil',
         place: [ 25, 2, 25, 68 ] },
       { id: 'sqrt_nnr',
         kind: 'judgement',
         type: 'nil',
         place: [ 26, 2, 26, 76 ] },
       { id: 'sqrt_0',
         kind: 'formula',
         'proved?': null,
         'complete?': null,
         'has-proofscript?': null,
         place: [ 30, 2, 30, 42 ] },
       { id: 'sqrt_1',
         kind: 'formula',
         'proved?': null,
         'complete?': null,
         'has-proofscript?': null,
         place: [ 31, 2, 31, 42 ] },
       { id: 'sqrt_eq_0',
         kind: 'formula',
         'proved?': null,
         'complete?': null,
         'has-proofscript?': null,
         place: [ 32, 2, 32, 60 ] },
       { id: 'sqrt_lem',
         kind: 'formula',
         'proved?': null,
         'complete?': null,
         'has-proofscript?': null,
         place: [ 36, 2, 36, 66 ] },
       { id: 'sqrt_def',
         kind: 'formula',
         'proved?': null,
         'complete?': null,
         'has-proofscript?': null,
         place: [ 38, 2, 38, 58 ] },
       { id: 'sqrt_square',
         kind: 'formula',
         'proved?': null,
         'complete?': null,
         'has-proofscript?': null,
         place: [ 40, 2, 40, 52 ] },
       { id: 'sqrt_sq',
         kind: 'formula',
         'proved?': null,
         'complete?': null,
         'has-proofscript?': null,
         place: [ 42, 2, 42, 61 ] },
       { id: 'sqrt_sq_abs',
         kind: 'formula',
         'proved?': null,
         'complete?': null,
         'has-proofscript?': null,
         place: [ 44, 2, 44, 51 ] },
       { id: 'sq_sqrt',
         kind: 'formula',
         'proved?': null,
         'complete?': null,
         'has-proofscript?': null,
         place: [ 46, 2, 46, 59 ] },
       { id: 'sqrt_times',
         kind: 'formula',
         'proved?': null,
         'complete?': null,
         'has-proofscript?': null,
         place: [ 48, 2, 48, 70 ] },
       { id: 'sqrt_div',
         kind: 'formula',
         'proved?': null,
         'complete?': null,
         'has-proofscript?': null,
         place: [ 50, 2, 51, 74 ] },
       { id: 'sqrt_lt',
         kind: 'formula',
         'proved?': null,
         'complete?': null,
         'has-proofscript?': null,
         place: [ 55, 2, 55, 66 ] },
       { id: 'sqrt_le',
         kind: 'formula',
         'proved?': null,
         'complete?': null,
         'has-proofscript?': null,
         place: [ 57, 2, 57, 68 ] },
       { id: 'sqrt_gt',
         kind: 'formula',
         'proved?': null,
         'complete?': null,
         'has-proofscript?': null,
         place: [ 59, 2, 59, 66 ] },
       { id: 'sqrt_ge',
         kind: 'formula',
         'proved?': null,
         'complete?': null,
         'has-proofscript?': null,
         place: [ 61, 2, 61, 68 ] },
       { id: 'sqrt_eq',
         kind: 'formula',
         'proved?': null,
         'complete?': null,
         'has-proofscript?': null,
         place: [ 63, 2, 63, 66 ] },
       { id: 'sqrt_less',
         kind: 'formula',
         'proved?': null,
         'complete?': null,
         'has-proofscript?': null,
         place: [ 65, 2, 65, 62 ] },
       { id: 'sqrt_more',
         kind: 'formula',
         'proved?': null,
         'complete?': null,
         'has-proofscript?': null,
         place: [ 67, 2, 67, 74 ] } ] } ]

export const parse2_result: pvsgui.ParseResult =
    [ { id: 'pump_th', decls: 
	[ { id: 'maxrate', kind: 'expr', type: 'nil', place: [ 3, 14, 3, 20 ] },
	  { id: 'maxinfuse', kind: 'expr', type: 'nil', place: [ 4, 14, 4, 20 ] },
	  { id: 'infusemin', kind: 'expr', type: 'nil', place: [ 5, 14, 5, 61 ] },
	  { id: 'timeout', kind: 'expr', type: 'nil', place: [ 7, 14, 7, 17 ] },
	  { id: 'shorttimeout', kind: 'expr', type: 'nil', place: [ 8, 18, 8, 21 ] },
	  { id: 'maxtime', kind: 'expr', type: 'nil', place: [ 9, 14, 9, 53 ] },
	  { id: 'bat_max', kind: 'expr', type: 'nil', place: [ 10, 13, 10, 56 ] },
	  { id: 'bat_min', kind: 'expr', type: 'nil', place: [ 11, 12, 11, 51 ] },
	  { id: 'timeoutLTmaxtime', kind: 'formula', 'proved?': null, 'complete?': null,
            'has-proofscript?': null, place: [ 15, 4, 15, 50 ] },
	  { id: 'shorttimeoutLTtimeout', kind: 'formula', 'proved?': null, 'complete?': null,
            'has-proofscript?': null, place: [ 16, 4, 16, 60 ] },
	  { id: 'maxinfuseLEQmaxtime', kind: 'formula', 'proved?': null, 'complete?': null,
            'has-proofscript?': null, place: [ 17, 4, 17, 56 ] },
	  { importing: 'types_and_constants_th[maxrate, maxinfuse, infusemin, timeout, shorttimeout, maxtime, bat_max, bat_min]',
            kind: 'importing', place: [ 19, 2, 19, 115 ] },
	  { id: 'vtbi_over_rate_lemma', kind: 'formula', 'proved?': null, 'complete?': null,
            'has-proofscript?': null, place: [ 27, 2, 28, 64 ] },
	  { id: 'vtbi_over_time_lemma', kind: 'formula', 'proved?': null, 'complete?': null,
            'has-proofscript?': null, place: [ 30, 2, 31, 64 ] },
	  { id: 'pump', kind: 'type', place: [ 36, 2, 55, 4 ] },
	  { id: 'init?', kind: 'expr', type: 'nil', place: [ 60, 2, 63, 72 ] },
	  { id: 'mains_switch', kind: 'expr', type: 'nil', place: [ 66, 2, 68, 27 ] },
	  { id: 'connect_set', kind: 'expr', type: 'nil', place: [ 69, 2, 70, 34 ] },
	  { id: 'on', kind: 'expr', type: 'nil', place: [ 72, 2, 81, 11 ] },
	  { id: 'per_start', kind: 'expr', type: 'nil', place: [ 83, 2, 83, 91 ] },
	  { id: 'start', kind: 'expr', type: 'nil', place: [ 85, 2, 96, 11 ] },
	  { id: 'per_pause', kind: 'expr', type: 'nil', place: [ 98, 2, 98, 62 ] },
	  { id: 'pause', kind: 'expr', type: 'nil', place: [ 100, 2, 103, 17 ] },
	  { id: 'per_tick', kind: 'expr', type: 'nil', place: [ 107, 2, 107, 99 ] },
	  { id: 'tick_case_infuse_and_infusionrateLvtbi', kind: 'expr', type: 'nil', place: [ 109, 2, 126, 42 ] },
	  { id: 'tick_case_infuse_and_infusionrateGEvtbi_NOTkvoflag', kind: 'expr', type: 'nil', place: [ 129, 2, 145, 42 ] },
	  { id: 'tick_case_infuse_and_infusionrateGEvtbi_kvoflag', kind: 'expr', type: 'nil', place: [ 147, 2, 161, 42 ] },
	  { id: 'tick', kind: 'expr', type: 'nil', place: [ 163, 2, 169, 15 ] },
	  { id: 'resetElapsed', kind: 'expr', type: 'nil', place: [ 172, 2, 172, 55 ] },
	  { id: 'clearVolumeinfused', kind: 'expr', type: 'nil', place: [ 174, 2, 174, 82 ] },
	  { id: 'clearkvoflag', kind: 'expr', type: 'nil', place: [ 176, 2, 176, 94 ] },
	  { id: 'maxvtbi', kind: 'expr', type: 'nil', place: [ 178, 2, 183, 69 ] },
	  { id: 'maxhvtbi', kind: 'expr', type: 'nil', place: [ 186, 2, 193, 57 ] },
	  { id: 'zerovtbi', kind: 'expr', type: 'nil', place: [ 195, 2, 199, 57 ] },
	  { id: 'zerohvtbi', kind: 'expr', type: 'nil', place: [ 201, 2, 204, 58 ] },
	  { id: 'reset', kind: 'expr', type: 'nil', place: [ 206, 2, 213, 48 ] },
	  { id: 'modvtbi', kind: 'expr', type: 'nil', place: [ 215, 2, 222, 66 ] },
	  { id: 'modhvtbi', kind: 'expr', type: 'nil', place: [ 224, 2, 229, 67 ] },
	  { id: 'pbsvtbi', kind: 'expr', type: 'nil', place: [ 231, 2, 239, 57 ] },
	  { id: 'aug_vtbi', kind: 'expr', type: 'nil', place: [ 240, 1, 247, 56 ] },
	  { id: 'pbshvtbi', kind: 'expr', type: 'nil', place: [ 250, 2, 257, 57 ] },
	  { id: 'aug_hvtbi', kind: 'expr', type: 'nil', place: [ 259, 1, 266, 56 ] },
	  { id: 'pivtbi', kind: 'expr', type: 'nil', place: [ 269, 2, 277, 58 ] },
	  { id: 'pihvtbi', kind: 'expr', type: 'nil', place: [ 279, 2, 285, 70 ] },
	  { id: 'mbsvtbi', kind: 'expr', type: 'nil', place: [ 287, 2, 294, 53 ] },
	  { id: 'mbshvtbi', kind: 'expr', type: 'nil', place: [ 296, 2, 302, 52 ] },
	  { id: 'mdvtbi', kind: 'expr', type: 'nil', place: [ 304, 2, 311, 53 ] },
	  { id: 'mdhvtbi', kind: 'expr', type: 'nil', place: [ 313, 2, 319, 52 ] },
	  { id: 'modtime', kind: 'expr', type: 'nil', place: [ 323, 2, 328, 62 ] },
	  { id: 'zerotime', kind: 'expr', type: 'nil', place: [ 330, 2, 332, 44 ] },
	  { id: 'maxitime', kind: 'expr', type: 'nil', place: [ 334, 2, 338, 52 ] },
	  { id: 'pbstime', kind: 'expr', type: 'nil', place: [ 340, 2, 345, 51 ] },
	  { id: 'aug_time', kind: 'expr', type: 'nil', place: [ 347, 2, 351, 27 ] },
	  { id: 'pitime', kind: 'expr', type: 'nil', place: [ 354, 2, 359, 52 ] },
	  { id: 'mbstime', kind: 'expr', type: 'nil', place: [ 361, 2, 367, 52 ] },
	  { id: 'mdtime', kind: 'expr', type: 'nil', place: [ 369, 2, 375, 52 ] },
	  { id: 'zerorate', kind: 'expr', type: 'nil', place: [ 377, 2, 377, 82 ] },
	  { id: 'maximrate', kind: 'expr', type: 'nil', place: [ 381, 2, 381, 106 ] },
	  { id: 'pbsrate', kind: 'expr', type: 'nil', place: [ 383, 2, 388, 53 ] },
	  { id: 'pirate', kind: 'expr', type: 'nil', place: [ 390, 2, 395, 53 ] },
	  { id: 'aug_rate', kind: 'expr', type: 'nil', place: [ 397, 2, 402, 28 ] },
	  { id: 'mbsrate', kind: 'expr', type: 'nil', place: [ 405, 2, 411, 53 ] },
	  { id: 'mdrate', kind: 'expr', type: 'nil', place: [ 413, 2, 419, 53 ] },
	  { id: 'modratevtbi', kind: 'expr', type: 'nil', place: [ 421, 2, 426, 87 ] },
	  { id: 'modvtbitime', kind: 'expr', type: 'nil', place: [ 428, 2, 433, 98 ] } ] } ];

export const typecheck1_result: pvsgui.ParseResult =
[ { id: 'more_real_props',
decls: 
 [ { id: 'sqrt_1_lt',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 15, 2, 15, 47 ] },
   { id: 'sqrt_1_le',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 17, 2, 17, 49 ] },
   { id: 'abs_div',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 20, 2, 20, 49 ] },
   { id: 'abs_abs',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 22, 2, 22, 37 ] },
   { id: 'abs_square',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 24, 2, 24, 38 ] },
   { id: 'nzreal_expt',
     kind: 'judgement',
     type: 'nzreal',
     place: [ 26, 2, 26, 64 ] },
   { id: 'nzreal_exp_TCC1',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': true,
     place: null },
   { id: 'nzreal_exp',
     kind: 'judgement',
     type: 'nzreal',
     place: [ 28, 2, 28, 60 ] },
   { id: 'expt_of_mult',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 30, 2, 30, 62 ] },
   { id: 'expt_of_div',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 32, 2, 32, 65 ] },
   { id: 'expt_of_inv',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 34, 2, 34, 56 ] },
   { id: 'expt_of_abs',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 36, 2, 36, 54 ] },
   { id: 'abs_of_expt_inv',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 38, 2, 38, 69 ] },
   { id: 'mult_expt_TCC1',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': true,
     place: null },
   { id: 'mult_expt',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 41, 2, 41, 49 ] },
   { id: 'div_expt_TCC1',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': true,
     place: null },
   { id: 'div_expt',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 43, 2, 43, 48 ] },
   { id: 'inv_expt',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 45, 2, 45, 42 ] },
   { id: 'abs_expt_TCC1',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': true,
     place: null },
   { id: 'abs_expt',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 47, 2, 47, 41 ] },
   { id: 'expt_lt1_bound1',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 53, 2, 53, 44 ] },
   { id: 'expt_lt1_bound2',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 55, 2, 55, 44 ] },
   { id: 'expt_gt1_bound1',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 57, 2, 57, 44 ] },
   { id: 'expt_gt1_bound2',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 59, 2, 59, 44 ] },
   { id: 'large_expt',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 61, 2, 61, 75 ] },
   { id: 'small_expt',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 63, 2, 63, 75 ] },
   { id: 'normalize_left_plus',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 72, 2, 72, 56 ] },
   { id: 'normalize_right_plus',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 73, 2, 73, 57 ] },
   { id: 'normalize_left_minus',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 75, 2, 75, 57 ] },
   { id: 'normalize_right_minus',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 76, 2, 76, 58 ] },
   { id: 'normalize_both_plus',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 78, 2, 78, 72 ] },
   { id: 'normalize_both_minus',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 79, 2, 79, 73 ] },
   { id: 'lt_times_lt_pos3',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 84, 2, 84, 71 ] },
   { id: 'gt_times_gt_pos3',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 86, 2, 86, 71 ] },
   { id: 'shift_minus',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 90, 2, 90, 35 ] },
   { id: 'move_minus_right',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 92, 2, 92, 53 ] },
   { id: 'move_minus_left',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 94, 2, 94, 52 ] },
   { id: 'both_sides_times3',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 98, 2, 98, 56 ] },
   { id: 'both_sides_times4',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 100, 2, 100, 56 ] },
   { id: 'both_sides_times1a',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 102, 2, 102, 53 ] },
   { id: 'both_sides_times2a',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 104, 2, 104, 53 ] },
   { id: 'both_sides_times3a',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 106, 2, 106, 53 ] },
   { id: 'both_sides_times4a',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 108, 2, 108, 53 ] },
   { id: 'floor_is',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 110, 2, 110, 48 ] },
   { id: 'ceiling_is',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 111, 2, 111, 52 ] },
   { id: 'sq_nonneg',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 113, 2, 113, 27 ] },
   { id: 'lt1_prop',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 115, 2, 115, 40 ] },
   { id: 'le1_prop',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 117, 2, 117, 45 ] },
   { id: 'gt1_prop',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 119, 2, 119, 39 ] },
   { id: 'ge1_prop',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 121, 2, 121, 42 ] },
   { id: 'exp_plus_TCC1',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': true,
     place: null },
   { id: 'exp_plus_TCC2',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': true,
     place: null },
   { id: 'exp_plus',
     kind: 'formula',
     'proved?': null,
     'complete?': null,
     'has-proofscript?': null,
     place: [ 123, 2, 123, 46 ] } ] } ];

     export const typecheck2_result: pvsgui.ParseResult =
     [ { id: 'sqrt',
     decls: 
      [ { importing: 'more_real_props',
          kind: 'importing',
          place: [ 11, 0, 11, 25 ] },
        { importing: 'sq', kind: 'importing', place: [ 14, 2, 14, 14 ] },
        { id: 'sqrt_exists',
          kind: 'formula',
          'proved?': null,
          'complete?': null,
          'has-proofscript?': null,
          place: [ 19, 2, 21, 21 ] },
        { id: 'sqrt_TCC1',
          kind: 'formula',
          'proved?': null,
          'complete?': null,
          'has-proofscript?': true,
          place: null },
        { id: 'sqrt',
          kind: 'expr',
          type: '[nnx: nonneg_real -> {nnz | nnz * nnz = nnx}]',
          place: [ 23, 2, 23, 34 ] },
        { id: 'sqrt_pos',
          kind: 'formula',
          'proved?': null,
          'complete?': null,
          'has-proofscript?': true,
          place: null },
        { id: 'sqrt_pos',
          kind: 'judgement',
          type: 'posreal',
          place: [ 25, 2, 25, 68 ] },
        { id: 'sqrt_nnr',
          kind: 'judgement',
          type: 'nonneg_real',
          place: [ 26, 2, 26, 76 ] },
        { id: 'sqrt_0',
          kind: 'formula',
          'proved?': null,
          'complete?': null,
          'has-proofscript?': null,
          place: [ 30, 2, 30, 42 ] },
        { id: 'sqrt_1',
          kind: 'formula',
          'proved?': null,
          'complete?': null,
          'has-proofscript?': null,
          place: [ 31, 2, 31, 42 ] },
        { id: 'sqrt_eq_0',
          kind: 'formula',
          'proved?': null,
          'complete?': null,
          'has-proofscript?': null,
          place: [ 32, 2, 32, 60 ] },
        { id: 'sqrt_lem',
          kind: 'formula',
          'proved?': null,
          'complete?': null,
          'has-proofscript?': null,
          place: [ 36, 2, 36, 66 ] },
        { id: 'sqrt_def',
          kind: 'formula',
          'proved?': null,
          'complete?': null,
          'has-proofscript?': null,
          place: [ 38, 2, 38, 58 ] },
        { id: 'sqrt_square',
          kind: 'formula',
          'proved?': null,
          'complete?': null,
          'has-proofscript?': null,
          place: [ 40, 2, 40, 52 ] },
        { id: 'sqrt_sq',
          kind: 'formula',
          'proved?': null,
          'complete?': null,
          'has-proofscript?': null,
          place: [ 42, 2, 42, 61 ] },
        { id: 'sqrt_sq_abs',
          kind: 'formula',
          'proved?': null,
          'complete?': null,
          'has-proofscript?': null,
          place: [ 44, 2, 44, 51 ] },
        { id: 'sq_sqrt',
          kind: 'formula',
          'proved?': null,
          'complete?': null,
          'has-proofscript?': null,
          place: [ 46, 2, 46, 59 ] },
        { id: 'sqrt_times',
          kind: 'formula',
          'proved?': null,
          'complete?': null,
          'has-proofscript?': null,
          place: [ 48, 2, 48, 70 ] },
        { id: 'sqrt_div_TCC1',
          kind: 'formula',
          'proved?': null,
          'complete?': null,
          'has-proofscript?': true,
          place: null },
        { id: 'sqrt_div_TCC2',
          kind: 'formula',
          'proved?': null,
          'complete?': null,
          'has-proofscript?': true,
          place: null },
        { id: 'sqrt_div',
          kind: 'formula',
          'proved?': null,
          'complete?': null,
          'has-proofscript?': null,
          place: [ 50, 2, 51, 74 ] },
        { id: 'sqrt_lt',
          kind: 'formula',
          'proved?': null,
          'complete?': null,
          'has-proofscript?': null,
          place: [ 55, 2, 55, 66 ] },
        { id: 'sqrt_le',
          kind: 'formula',
          'proved?': null,
          'complete?': null,
          'has-proofscript?': null,
          place: [ 57, 2, 57, 68 ] },
        { id: 'sqrt_gt',
          kind: 'formula',
          'proved?': null,
          'complete?': null,
          'has-proofscript?': null,
          place: [ 59, 2, 59, 66 ] },
        { id: 'sqrt_ge',
          kind: 'formula',
          'proved?': null,
          'complete?': null,
          'has-proofscript?': null,
          place: [ 61, 2, 61, 68 ] },
        { id: 'sqrt_eq',
          kind: 'formula',
          'proved?': null,
          'complete?': null,
          'has-proofscript?': null,
          place: [ 63, 2, 63, 66 ] },
        { id: 'sqrt_less',
          kind: 'formula',
          'proved?': null,
          'complete?': null,
          'has-proofscript?': null,
          place: [ 65, 2, 65, 62 ] },
        { id: 'sqrt_more',
          kind: 'formula',
          'proved?': null,
          'complete?': null,
          'has-proofscript?': null,
          place: [ 67, 2, 67, 74 ] } ] } ];

export const prf: string[] = [
    `(""
    (skeep)
    (lemma "sin2_cos2")
    (inst?)
    (both-sides-f 1 "sq")
    (("1"
      (both-sides "-" "sq(cos(acos(sig)))" -1)
      (("1"
        (assert)
        (replaces -1)
        (rewrite "cos_acos")
        (rewrite "sq_sqrt"))
       ("2" (iff) (ground))))
     ("2"
      (case "FORALL (zz,qq:nnreal): sq(zz) = sq(qq) IMPLIES zz=qq")
      (("1"
        (invoke (inst - "%1" "%2") (! 1 1) (! 1 2))
        (("1" (assert) (assert)) ("2" (assert))
         ("3" (assert) (lemma "sin_ge_0") (inst?) (assert))))
       ("2"
        (hide-all-but 1)
        (skeep)
        (case "sqrt(sq(zz)) = sqrt(sq(qq))")
        (("1" (assert)) ("2" (replaces -1))))))))`,

    `(""
    (case "FORALL (zz,qq:nnreal): sq(zz) = sq(qq) IMPLIES zz=qq")
    (("1"
      (label "igz" -1)
      (hide "igz")
      (skeep)
      (skoletin :var "AA")
      (assert)
      (expand "xyz2spherical")
      (assert)
      (lift-if)
      (split -)
      (("1"
        (flatten)
        (assert)
        (case "x = 0 AND y=0")
        (("1"
          (flatten)
          (assert)
          (flatten)
          (replaces -5)
          (assert)
          (replace -1)
          (replace -2)
          (assert)
          (expand "^" + 1)
          (expand "expt")
          (assert)
          (lemma "sqrt_sq")
          (inst - "z")
          (split -)
          (("1"
            (expand "expt")
            (expand "expt")
            (expand "sq")
            (replaces -1)
            (assert)
            (expand "spherical2xyz")
            (assert)
            (rewrite "cos_pi")
            (rewrite "sin_0")
            (rewrite "sin_pi")
            (rewrite "cos_0")
            (assert))
           ("2" (propax))))
         ("2"
          (hide-all-but (-1 1))
          (case "NOT x^2+y^2=0")
          (("1" (assert))
           ("2"
            (case "NOT sq(x)+sq(y)=0")
            (("1" (grind))
             ("2"
              (lemma "sq_eq_0")
              (inst-cp - "x")
              (inst - "y")
              (grind))))))))
       ("2"
        (flatten)
        (split -1)
        (("1"
          (flatten)
          (assert)
          (hide -3)
          (lift-if)
          (split -)
          (("1" (ground))
           ("2"
            (flatten)
            (hide 1)
            (replaces -1)
            (assert)
            (case "x = 0 AND y = 0")
            (("1"
              (flatten)
              (replaces -1)
              (replaces -1)
              (assert)
              (case-replace "sqrt(z^2) = -z")
              (("1"
                (expand "spherical2xyz")
                (rewrite "cos_pi")
                (assert)
                (rewrite "sin_pi")
                (assert))
               ("2"
                (hide-all-but (1 2))
                (rewrite "expt_x2")
                (rewrite "sq" :dir rl)
                (assert))
               ("3" (hide-all-but 1) (grind))))
             ("2"
              (hide (2 3))
              (case "NOT x^2+y^2=0")
              (("1" (assert))
               ("2"
                (case "NOT sq(x)+sq(y)=0")
                (("1" (grind))
                 ("2"
                  (lemma "sq_eq_0")
                  (inst-cp - "x")
                  (inst - "y")
                  (ground))))))))))
         ("2"
          (flatten)
          (hide 2)
          (assert)
          (replaces -1)
          (assert)
          (expand "spherical2xyz")
          (name "R" "sqrt(x^2+y^2+z^2)")
          (("1"
            (replace -1)
            (case "R > 0")
            (("1"
              (case "x/=0 OR y/=0")
              (("1"
                (hide -4)
                (assert)
                (split +)
                (("1"
                  (rewrite "cos_atan2")
                  (assert)
                  (lift-if)
                  (assert)
                  (split +)
                  (("1" (propax))
                   ("2"
                    (flatten)
                    (rewrite "sin_acos_ecef")
                    (("1"
                      (case "sqrt(1 - sq(z / R)) * (1 / sqrt(1 + sq(y / x))) * R = abs(x)")
                      (("1"
                        (split +)
                        (("1"
                          (flatten)
                          (expand "abs")
                          (expand "sq")
                          (lift-if)
                          (ground))
                         ("2"
                          (flatten)
                          (expand "abs")
                          (expand "sq")
                          (lift-if)
                          (ground))))
                       ("2"
                        (hide 3)
                        (reveal "igz")
                        (invoke (inst - "%1" "%2") (! 1 1) (! 1 2))
                        (assert)
                        (hide 2)
                        (rewrite "sq_times")
                        (rewrite "sq_times")
                        (rewrite "sq_div")
                        (rewrite "sq_div")
                        (("1"
                          (rewrite "sq_div")
                          (rewrite "sq_sqrt")
                          (("1"
                            (field)
                            (replaces -2 1 :dir rl)
                            (rewrite "sq_sqrt")
                            (("1" (grind))
                             ("2"
                              (typepred "sq(x)+sq(y)+sq(z)")
                              (grind))))
                           ("2"
                            (assert)
                            (lemma "nnreal_div_posreal_is_nnreal")
                            (inst?)
                            (("1" (assert))
                             ("2" (lemma "sq_eq_0") (inst?) (assert))))))
                         ("2"
                          (flatten)
                          (lemma "sqrt_eq_0")
                          (inst - "1+sq(y)/sq(x)")
                          (assert))
                         ("3"
                          (lemma "nnreal_div_posreal_is_nnreal")
                          (inst?)
                          (("1" (assert))
                           ("2" (lemma "sq_eq_0") (inst?) (assert))))))
                       ("3" (assert))))
                     ("2"
                      (rewrite "abs_div")
                      (expand "abs" + 2)
                      (cross-mult 1)
                      (lemma "sq_le")
                      (inst?)
                      (assert)
                      (hide 2)
                      (case "FORALL (rr:real): 1*rr = rr")
                      (("1"
                        (rewrite -1)
                        (hide -1)
                        (replaces -2 :dir rl)
                        (rewrite "sq_sqrt")
                        (("1" (typepred "sq(x)+sq(y)") (grind))
                         ("2" (typepred "sq(x)+sq(y)+sq(z)") (grind))))
                       ("2" (assert))))))))
                 ("2"
                  (rewrite "sin_acos_ecef")
                  (("1"
                    (rewrite "sin_atan2")
                    (lift-if)
                    (assert)
                    (split +)
                    (("1"
                      (flatten)
                      (assert)
                      (case "sqrt(1 - sq(z / R)) * R = abs(y)")
                      (("1" (expand "abs") (lift-if) (ground))
                       ("2"
                        (hide 2)
                        (reveal "igz")
                        (invoke (inst - "%1" "%2") (! 1 1) (! 1 2))
                        (assert)
                        (hide 2)
                        (delabel "igz")
                        (rewrite "sq_times")
                        (rewrite "sq_div")
                        (replace -1)
                        (assert)
                        (field)
                        (replaces -4 1 :dir rl)
                        (rewrite "sq_sqrt")
                        (("1" (assert) (grind))
                         ("2" (typepred "sq(y)+sq(z)") (grind))))))
                     ("2"
                      (flatten)
                      (case "sqrt(1 - sq(z / R)) * (y / abs(x) / sqrt(1 + sq(y / x))) * R = y")
                      (("1"
                        (split +)
                        (("1" (flatten) (expand "abs") (lift-if) (assert))
                         ("2" (flatten) (expand "abs") (assert))))
                       ("2"
                        (hide 3)
                        (reveal "igz")
                        (case "sqrt(1 - sq(z / R)) * (abs(y) / abs(x) / sqrt(1 + sq(y / x))) * R = abs(y)")
                        (("1"
                          (name "d" "abs(y)")
                          (replace -1)
                          (expand "abs" -1)
                          (replaces -1 :dir rl)
                          (lift-if)
                          (ground))
                         ("2"
                          (hide 2)
                          (invoke (inst - "%1" "%2") (! 1 1) (! 1 2))
                          (("1"
                            (assert)
                            (hide 1)
                            (delabel "igz")
                            (rewrite "sq_times")
                            (rewrite "sq_div")
                            (rewrite "sq_times")
                            (rewrite "sq_div")
                            (rewrite "sq_div")
                            (("1"
                              (rewrite "sq_div")
                              (rewrite "sq_sqrt")
                              (rewrite "sq_sqrt")
                              (("1"
                                (field)
                                (case "sq(R) = sq(x)+sq(y)+sq(z)")
                                (("1" (replaces -1) (grind))
                                 ("2"
                                  (replace -2 1 :dir rl)
                                  (rewrite "sq_sqrt")
                                  (("1" (grind))
                                   ("2"
                                    (typepred "sq(x)+sq(y)+sq(z)")
                                    (grind))))))
                               ("2"
                                (lemma "nnreal_div_posreal_is_nnreal")
                                (inst?)
                                (("1" (assert))
                                 ("2"
                                  (lemma "sq_eq_0")
                                  (inst?)
                                  (assert))))))
                             ("2"
                              (flatten)
                              (lemma "sqrt_eq_0")
                              (inst - "1+sq(y)/sq(x)")
                              (assert))
                             ("3"
                              (lemma "nnreal_div_posreal_is_nnreal")
                              (inst?)
                              (("1" (assert))
                               ("2"
                                (lemma "sq_eq_0")
                                (inst?)
                                (assert))))))
                           ("2"
                            (rewrite "nnreal_times_nnreal_is_nnreal")
                            (rewrite "nnreal_times_nnreal_is_nnreal")
                            (cross-mult 1))))))
                       ("3" (assert)) ("4" (assert))))))
                   ("2"
                    (rewrite "abs_div")
                    (cross-mult 1)
                    (expand "abs" 1 2)
                    (assert)
                    (lemma "sq_le")
                    (inst?)
                    (assert)
                    (replace -3 1 :dir rl)
                    (rewrite "sq_sqrt")
                    (("1" (typepred "sq(x)+sq(y)") (grind))
                     ("2" (typepred "sq(x)+sq(y)+sq(z)") (grind))))))
                 ("3" (rewrite "cos_acos") (assert))))
               ("2"
                (flatten)
                (assert)
                (replace -1)
                (replace -2)
                (assert))))
             ("2"
              (assert)
              (lemma "sqrt_pos")
              (assert)
              (lemma "sq_eq_0")
              (typepred "sq(x)")
              (typepred "sq(y)")
              (typepred "sq(z)")
              (split -)
              (("1" (flatten) (inst - "x") (grind))
               ("2" (inst - "y") (grind)) ("3" (inst - "z") (grind))))))
           ("2" (hide 3) (typepred "sq(x)+sq(y)+sq(z)") (grind))))))))
     ("2" (hide 2) (skeep) (both-sides-f -1 "sqrt") (replaces -1))))`
];

export const find_declaration_result: pvsgui.FindDeclarationResult =
    [ { declname: 'sqrt',
	'type': '[nnx: nonneg_real -> {nnz | nnz * nnz = nnx}]',
	theoryid: 'sqrt',
	filename: 'sqrt.pvs',
	place: [ 23, 2, 23, 34 ],
	'decl-ppstring': 'sqrt(nnx): {nnz | nnz * nnz = nnx}' },
    ];

export const proof_script_result: string =
  ';;; Proof sq_ge-1 for formula sq.sq_ge\n(""\n (skosimp)\n (ground)\n (("1"\n   (expand "sq")\n   (case "forall (x,y:real): x>=y iff not x<y")\n   (("1" (auto-rewrite -1) (grind-arith)) ("2" (skosimp) (ground))))\n  ("2" (grind-arith))))'

export const seq1_result: any =
    { commentary: 
      [ '\nQ.E.D.\n',
	'\nInstalling rewrite rule sets.singleton_rew (all instances)',
	'\nsq_0 :  \n\n  |-------\n{1}   sq(0) = 0\n' ],
      label: 'sq_0',
      sequent: 
      { succedents: 
	[ { labels: [ 1 ],
            changed: 'true',
            formula: 'sq(0) = 0',
            'names-info': 
            [ { id: '=',
		place: [ 1, 6, 1, 7 ],
		decl: '=: [T, T -> boolean]',
		'decl-file': 'prelude.pvs',
		'decl-place': [ 61, 2, 61, 22 ] },
              { id: 'sq',
		place: [ 1, 0, 1, 2 ],
		decl: 'sq(a): nonneg_real = a * a',
		'decl-file': 'sq.pvs',
		'decl-place': [ 10, 2, 10, 26 ] } ] } ] } };

export const seq2_result: any =
    { commentary: [ '\nsq_0 :  \n\n  |-------\n{1}   TRUE\n' ],
      action: 'Expanding the definition of sq,',
      'num-subgoals': 1,
      label: 'sq_0',
      'prev-cmd': [ 'expand', 'sq' ],
      sequent: 
      { succedents: 
	[ { labels: [ 1 ],
            changed: 'true',
            formula: 'TRUE',
            'names-info': 
            [ { id: 'TRUE',
		place: [ 1, 0, 1, 4 ],
		decl: 'TRUE: bool',
		'decl-file': 'prelude.pvs',
    'decl-place': [ 48, 9, 48, 19 ] } ] } ] } };

export const show_tccs_result: any =
    [ { id: 'sq_TCC1',
	theory: 'sq',
	comment: [ '% Subtype TCC generated (at line 10, column 23) for  a * a\n    % expected type  nonneg_real' ],
	'from-decl': 'sq',
	definition: 'FORALL (a: real): a * a >= 0',
	proved: true },
      { id: 'sq_div_TCC1',
	theory: 'sq',
	comment: [ '% Subtype TCC generated (at line 32, column 54) for  sq(d)\n    % expected type  nznum' ],
	'from-decl': 'sq_div',
	definition: 'FORALL (d: real): d /= 0 IMPLIES sq(d) /= 0',
  proved: true } ] ;

export const prove_tccs_result: any =
  { totals: 2, proved: 2, unproved: 0, subsumed: 0, simplified: 0 };
  
export const prove_formula_sqrt_0 = { commentary: 
     [ '\nInstalling rewrite rule sets.singleton_rew (all instances)',
       '\nsqrt_0 :  \n\n  |-------\n{1}   sqrt(0) = 0\n' ],
    label: 'sqrt_0',
    sequent: 
     { succedents: 
        [ { labels: [ 1 ],
            changed: 'true',
            formula: 'sqrt(0) = 0',
            'names-info': 
             [ { id: '=',
                 place: [ 1, 8, 1, 9 ],
                 decl: '=: [T, T -> boolean]',
                 'decl-file': 'prelude.pvs',
                 'decl-place': [ 61, 2, 61, 22 ] },
               { id: 'sqrt',
                 place: [ 1, 0, 1, 4 ],
                 decl: 'sqrt(nnx): {nnz | nnz * nnz = nnx}',
                 'decl-file': 'sqrt.pvs',
                 'decl-place': [ 23, 2, 23, 34 ] } ] } ] } };

export const sq_neg_prove_formula = { 
    commentary: 
     [ '\nInstalling rewrite rule sets.singleton_rew (all instances)',
       '\nsq_neg :  \n\n  |-------\n{1}   FORALL (a: real): sq(-a) = sq(a)\n' ],
    label: 'sq_neg',
    sequent: 
     { succedents: 
        [ { labels: [ 1 ],
            changed: 'true',
            formula: 'FORALL (a: real): sq(-a) = sq(a)',
            'names-info': 
             [ { id: 'a',
                 place: [ 1, 30, 1, 31 ],
                 decl: 'a: real',
                 'decl-file': 'sq.pvs',
                 'decl-place': null },
               { id: 'sq',
                 place: [ 1, 27, 1, 29 ],
                 decl: 'sq(a): nonneg_real = a * a',
                 'decl-file': 'sq.pvs',
                 'decl-place': [ 10, 2, 10, 26 ] },
               { id: '=',
                 place: [ 1, 25, 1, 26 ],
                 decl: '=: [T, T -> boolean]',
                 'decl-file': 'prelude.pvs',
                 'decl-place': [ 61, 2, 61, 22 ] },
               { id: 'a',
                 place: [ 1, 22, 1, 23 ],
                 decl: 'a: real',
                 'decl-file': 'sq.pvs',
                 'decl-place': null },
               { id: '-',
                 place: [ 1, 21, 1, 22 ],
                 decl: '-: [numfield -> numfield]',
                 'decl-file': 'prelude.pvs',
                 'decl-place': [ 1803, 2, 1803, 27 ] },
               { id: 'sq',
                 place: [ 1, 18, 1, 20 ],
                 decl: 'sq(a): nonneg_real = a * a',
                 'decl-file': 'sq.pvs',
                 'decl-place': [ 10, 2, 10, 26 ] },
               { id: 'a',
                 place: [ 1, 8, 1, 9 ],
                 decl: 'a: real',
                 'decl-file': 'sq.pvs',
                 'decl-place': null },
               { id: 'real',
                 place: [ 1, 11, 1, 15 ],
                 decl: 'real: TYPE+ FROM number_field',
                 'decl-file': 'prelude.pvs',
                 'decl-place': [ 1850, 2, 1850, 39 ] } ] } ] } };

export const sq_neg_proof_command_skosimp_star = { 
action: 'Repeatedly Skolemizing and flattening,',
'num-subgoals': 1,
label: 'sq_neg',
'prev-cmd': [ 'skosimp*' ],
sequent: 
 { succedents: 
    [ { labels: [ 1 ],
        changed: 'true',
        formula: 'sq(-a!1) = sq(a!1)',
        'names-info': 
         [ { id: 'a!1',
             place: [ 1, 14, 1, 17 ],
             decl: 'Skolem Constant: real',
             'decl-file': null,
             'decl-place': null },
           { id: 'sq',
             place: [ 1, 11, 1, 13 ],
             decl: 'sq(a): nonneg_real = a * a',
             'decl-file': 'sq.pvs',
             'decl-place': [ 10, 2, 10, 26 ] },
           { id: '=',
             place: [ 1, 9, 1, 10 ],
             decl: '=: [T, T -> boolean]',
             'decl-file': 'prelude.pvs',
             'decl-place': [ 61, 2, 61, 22 ] },
           { id: 'a!1',
             place: [ 1, 4, 1, 7 ],
             decl: 'Skolem Constant: real',
             'decl-file': null,
             'decl-place': null },
           { id: '-',
             place: [ 1, 3, 1, 4 ],
             decl: '-: [numfield -> numfield]',
             'decl-file': 'prelude.pvs',
             'decl-place': [ 1803, 2, 1803, 27 ] },
           { id: 'sq',
             place: [ 1, 0, 1, 2 ],
             decl: 'sq(a): nonneg_real = a * a',
             'decl-file': 'sq.pvs', ////// <<<< Note: the latest version of PVS correctly provides a full path
             'decl-place': [ 10, 2, 10, 26 ] } ] } ] } };

export const sq_neg_expand = { commentary: 
  [ '\nsq_neg :  \n\n  |-------\n{1}   -a!1 * -a!1 = a!1 * a!1\n' ],
 action: 'Expanding the definition of sq,',
 'num-subgoals': 1,
 label: 'sq_neg',
 'prev-cmd': [ 'expand', 'sq' ],
 sequent: 
  { succedents: 
     [ { labels: [ 1 ],
         changed: 'true',
         formula: '-a!1 * -a!1 = a!1 * a!1',
         'names-info': 
          [ { id: '*',
              place: [ 1, 18, 1, 19 ],
              decl: '*: [numfield, numfield -> numfield]',
              'decl-file': 'prelude.pvs',
              'decl-place': [ 1801, 8, 1801, 43 ] },
            { id: 'a!1',
              place: [ 1, 20, 1, 23 ],
              decl: 'Skolem Constant: real',
              'decl-file': null,
              'decl-place': null },
            { id: 'a!1',
              place: [ 1, 14, 1, 17 ],
              decl: 'Skolem Constant: real',
              'decl-file': null,
              'decl-place': null },
            { id: '=',
              place: [ 1, 12, 1, 13 ],
              decl: '=: [T, T -> boolean]',
              'decl-file': 'prelude.pvs',
              'decl-place': [ 61, 2, 61, 22 ] },
            { id: '*',
              place: [ 1, 5, 1, 6 ],
              decl: '*: [numfield, numfield -> numfield]',
              'decl-file': 'prelude.pvs',
              'decl-place': [ 1801, 8, 1801, 43 ] },
            { id: 'a!1',
              place: [ 1, 8, 1, 11 ],
              decl: 'Skolem Constant: real',
              'decl-file': null,
              'decl-place': null },
            { id: '-',
              place: [ 1, 7, 1, 8 ],
              decl: '-: [numfield -> numfield]',
              'decl-file': 'prelude.pvs',
              'decl-place': [ 1803, 2, 1803, 27 ] },
            { id: 'a!1',
              place: [ 1, 1, 1, 4 ],
              decl: 'Skolem Constant: real',
              'decl-file': null,
              'decl-place': null },
            { id: '-',
              place: [ 1, 0, 1, 1 ],
              decl: '-: [numfield -> numfield]',
              'decl-file': 'prelude.pvs',
              'decl-place': [ 1803, 2, 1803, 27 ] } ] } ] } };

export const triangle_rectangle_prj: string = `
;;; Proof triangle_rectangle-1 for formula sq.triangle_rectangle
(""
 (skolem 1 ("a" "b" "c"))
 (flatten)
 (case "sq(a)>=0" "sq(b)>=0" "sq(c)>=0")
 (("1"
   (case "sq(a) <= sq(c)")
   (("1"
     (case "sq(b) <= sq(c)")
     (("1"
       (lemma "sq_neg_pos_le")
       (inst-cp -1 "a" "c")
       (inst -1 "b" "c")
       (ground))
      ("2" (hide 2 -1) (ground))))
    ("2" (hide 2 -1) (ground))))
  ("2" (rewrite "sq_pos")) ("3" (rewrite "sq_pos"))
  ("4" (rewrite "sq_pos"))))
`;

export const triangle_rectangle: ProofDescriptor = new ProofDescriptor({
    theory: 'sq',
    formula: 'triangle_rectangle',
    status: 'untried',
    prover: 'PVS 7.x',
    shasum: '90d0630453df76b0a749b92ac10e7e51b0c59e2cb0e3711bb009a7b4191b802a'
  }, {
    name: 'sq.triangle_rectangle',
    rules: [
      {
        name: '(skolem 1 ("a" "b" "c"))',
        rules: [],
        type: 'proof-command',
        branch: ''
      },
      {
        name: '(flatten)',
        rules: [],
        type: 'proof-command',
        branch: ''
      },
      {
        name: '(case "sq(a)>=0" "sq(b)>=0" "sq(c)>=0")',
        rules: [
          {
            name: '1',
            rules: [
              {
                name: '(case "sq(a) <= sq(c)")',
                rules: [
                  {
                    name: '1',
                    rules: [
                      {
                        name: '(case "sq(b) <= sq(c)")',
                        rules: [
                          {
                            name: '1',
                            rules: [
                              {
                                name: '(lemma "sq_neg_pos_le")',
                                rules: [],
                                type: 'proof-command',
                                branch: '1.1.1'
                              },
                              {
                                name: '(inst-cp -1 "a" "c")',
                                rules: [],
                                type: 'proof-command',
                                branch: '1.1.1'
                              },
                              {
                                name: '(inst -1 "b" "c")',
                                rules: [],
                                type: 'proof-command',
                                branch: '1.1.1'
                              },
                              {
                                name: '(ground)',
                                rules: [],
                                type: 'proof-command',
                                branch: '1.1.1'
                              }
                            ],
                            type: 'proof-branch',
                            branch: '1.1.1'
                          },
                          {
                            name: '2',
                            rules: [
                              {
                                name: '(hide 2 -1)',
                                rules: [],
                                type: 'proof-command',
                                branch: '1.1.2'
                              },
                              {
                                name: '(ground)',
                                rules: [],
                                type: 'proof-command',
                                branch: '1.1.2'
                              }
                            ],
                            type: 'proof-branch',
                            branch: '1.1.2'
                          }
                        ],
                        type: 'proof-command',
                        branch: '1.1'
                      }
                    ],
                    type: 'proof-branch',
                    branch: '1.1'
                  },
                  {
                    name: '2',
                    rules: [
                      {
                        name: '(hide 2 -1)',
                        rules: [],
                        type: 'proof-command',
                        branch: '1.2'
                      },
                      {
                        name: '(ground)',
                        rules: [],
                        type: 'proof-command',
                        branch: '1.2'
                      }
                    ],
                    type: 'proof-branch',
                    branch: '1.2'
                  }
                ],
                type: 'proof-command',
                branch: '1'
              }
            ],
            type: 'proof-branch',
            branch: '1'
          },
          {
            name: '2',
            rules: [
              {
                name: '(rewrite "sq_pos")',
                rules: [],
                type: 'proof-command',
                branch: '2'
              }
            ],
            type: 'proof-branch',
            branch: '2'
          },
          {
            name: '3',
            rules: [
              {
                name: '(rewrite "sq_pos")',
                rules: [],
                type: 'proof-command',
                branch: '3'
              }
            ],
            type: 'proof-branch',
            branch: '3'
          },
          {
            name: '4',
            rules: [
              {
                name: '(rewrite "sq_pos")',
                rules: [],
                type: 'proof-command',
                branch: '4'
              }
            ],
            type: 'proof-branch',
            branch: '4'
          }
        ],
        type: 'proof-command',
        branch: ''
      }
    ],
    type: 'root',
    branch: ''
});

export const sq_plus_eq_0_desc: ProofDescriptor = new ProofDescriptor({
  theory: 'sq',
  formula: 'sq_plus_eq_0',
  status: 'untried',
  prover: 'PVS 7.1.0 (Allegro CL Enterprise Edition)',
  shasum: '90d0630453df76b0a749b92ac10e7e51b0c59e2cb0e3711bb009a7b4191b802a'
  }, {
    name: 'sq.sq_plus_eq_0',
    rules: [
      {
        name: '(skosimp)',
        rules: [],
        type: 'proof-command',
        branch: ''
      },
      {
        name: '(ground)',
        rules: [
          {
            name: '1',
            rules: [
              {
                name: '(grind-with-lemmas :theories ("sq" "real_props") :if-match all :lazy-match? nil :lemmas "sq_eq_0")',
                rules: [],
                type: 'proof-command',
                branch: '1'
              }
            ],
            type: 'proof-branch',
            branch: '1'
          },
          {
            name: '2',
            rules: [
              {
                name: '(grind-with-lemmas :theories ("sq" "real_props") :if-match all :lazy-match? nil :lemmas "sq_eq_0")',
                rules: [],
                type: 'proof-command',
                branch: '2'
              }
            ],
            type: 'proof-branch',
            branch: '2'
          },
          {
            name: '3',
            rules: [
              {
                name: '(grind-with-lemmas :theories ("sq" "real_props") :if-match all :lazy-match? nil :lemmas "sq_eq_0")',
                rules: [],
                type: 'proof-command',
                branch: '3'
              }
            ],
            type: 'proof-branch',
            branch: '3'
          }
        ],
        type: 'proof-command',
        branch: ''
      }
    ],
    type: 'root',
    branch: ''
});

export const sq_plus_eq_0_desc_new: ProofDescriptor = new ProofDescriptor({
  theory: 'sq',
  formula: 'sq_plus_eq_0',
  status: 'proved', // update status
  prover: 'PVS 7.1.0 (Allegro CL Enterprise Edition)',
  shasum: '90d0630453df76b0a749b92ac10e7e51b0c59e2cb0e3711bb009a7b4191b802a'
  }, {
    name: 'sq.sq_plus_eq_0',
    rules: [
      {
        name: '(skosimp*)', // updated proof command
        rules: [],
        type: 'proof-command',
        branch: ''
      },
      {
        name: '(ground)',
        rules: [
          {
            name: '1',
            rules: [
              {
                name: '(grind-with-lemmas :theories ("sq" "real_props") :if-match all :lazy-match? nil :lemmas "sq_eq_0")',
                rules: [],
                type: 'proof-command',
                branch: '1'
              }
            ],
            type: 'proof-branch',
            branch: '1'
          },
          {
            name: '2',
            rules: [
              {
                name: '(grind-with-lemmas :theories ("sq" "real_props") :if-match all :lazy-match? nil :lemmas "sq_eq_0")',
                rules: [],
                type: 'proof-command',
                branch: '2'
              }
            ],
            type: 'proof-branch',
            branch: '2'
          },
          {
            name: '3',
            rules: [
              {
                name: '(grind-with-lemmas :theories ("sq" "real_props") :if-match all :lazy-match? nil :lemmas "sq_eq_0")',
                rules: [],
                type: 'proof-command',
                branch: '3'
              }
            ],
            type: 'proof-branch',
            branch: '3'
          }
        ],
        type: 'proof-command',
        branch: ''
      }
    ],
    type: 'root',
    branch: ''
});
