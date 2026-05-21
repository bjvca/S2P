
{/******************************************************************************
						Title; Space to Place Project in Malawi
						Purpose: Prodcue estimation results 
********************************************************************************/
	}
	
*Run master and data cleaning do files 
do Master.do 
*do "Data cleaning and preparation.do" 

use "$processed/estimation_data_v2", clear 

*----------------------------------------
* Table 1: ITT Effects on total fertilizer use
*----------------------------------------      
eststo clear

* Preferred pre-treatment controls for the fertilizer-use table.
*
* Changes relative to the original specification:
*   1. agro_vis and ext_srv are dropped because they can be affected by
*      treatment and are therefore bad controls in the preferred ITT
*      specification.
*   2. hh_educ, slope, soil_str, and seed_typ_num are entered with factor
*      notation because they are categorical variables, not continuous scales.
*   3. dist_agro is time to the nearest agro-dealer in minutes, not distance in
*      kilometers.
*   4. Two implausibly large tobacco observations (farmer_id F_546 and F_387)
*      are excluded explicitly because they dominate the all-crops means.
local fert_controls hh_size hh_age i.hh_educ dist_agro plot_siz ///
    i.slope i.soil_str i.seed_typ_num

* Sentinel values should be treated as missing before the adjusted regressions
* are estimated. These replacements are applied only within this analysis run.
replace dist_agro = . if dist_agro == 999
replace plot_siz = . if plot_siz == 999
drop if inlist(farmer_id, "F_546", "F_387")

** All crops
reg total_qty_fert i.treat_num , cluster(cluster_id)
eststo r1
quietly summarize total_qty_fert if e(sample) & treat_num == 1
estadd scalar ctrl_mean = r(mean)
estadd local controls "No"

* Original adjusted regression kept for documentation only.
*
* Feedback to the original author:
*   - agro_vis and ext_srv are post-treatment controls and should not appear in
*     the preferred ITT specification;
*   - hh_educ, slope, soil_str, and seed_typ_num were passed linearly even
*     though they are categorical;
*   - the table note described hh_educ as years and dist_agro as kilometers,
*     which does not match the questionnaire.
*
* reg total_qty_fert i.treat_num hh_size hh_age hh_educ dist_agro plot_siz ///
*     agro_vis ext_srv slope soil_str seed_typ_num, cluster(cluster_id)
reg total_qty_fert i.treat_num `fert_controls', cluster(cluster_id)
eststo r2
quietly summarize total_qty_fert if e(sample) & treat_num == 1
estadd scalar ctrl_mean = r(mean)
estadd local controls "Yes"

** Main crops (Maize, main_crp==1)
reg total_qty_fert i.treat_num if main_crp==1, cluster(cluster_id)
eststo r3
quietly summarize total_qty_fert if e(sample) & treat_num == 1
estadd scalar ctrl_mean = r(mean)
estadd local controls "No"

* Original adjusted maize regression kept for documentation only.
*
* reg total_qty_fert i.treat_num hh_size hh_age hh_educ dist_agro plot_siz ///
*     agro_vis ext_srv slope soil_str seed_typ_num if main_crp==1, ///
*     cluster(cluster_id)
reg total_qty_fert i.treat_num `fert_controls' if main_crp==1, ///
    cluster(cluster_id)
eststo r4
quietly summarize total_qty_fert if e(sample) & treat_num == 1
estadd scalar ctrl_mean = r(mean)
estadd local controls "Yes"



*------------------------------------------------------------*
* Fertilizer nutrient use per hectare (kg/ha)
*------------------------------------------------------------*

gen N_kgha = total_N/plot_siz
label variable N_kgha "Nitrogen applied (kg/ha)"

gen K_kgha = total_K/plot_siz
label variable K_kgha "Potassium applied (kg/ha)"

gen P_kgha = total_P/plot_siz
label variable P_kgha "Phosphorus applied (kg/ha)"

gen totalnutrient_kgha = total_nutrient/plot_siz
label variable totalnutrient_kgha "Total nutrients applied (kg/ha)"



** Export results
label variable total_qty_fert "Total fertilizer use (kg)"
label variable treat_num    "Treatment group"
label variable hh_size        "Household size"
label variable hh_age         "Household head age (years)"
label variable hh_educ        "Household head education level"
label variable dist_agro      "Time to agro-dealer (minutes)"
label variable plot_siz       "Plot size (acres)"
label variable agro_vis       "Visited agro-dealer (Yes=1)"
label variable ext_srv        "Access to extension services (Yes=1)"
label variable slope          "Plot slope"
label variable soil_str       "Soil structure"
label variable seed_typ_num   "Seed type used"

esttab, star(* 0.10 ** 0.05 *** 0.01) b(%10.2f)
esttab using "$results/Table xx Fertilizer use amount.tex", se replace label b(%10.2f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    keep(2.treat_num 3.treat_num) order(2.treat_num 3.treat_num) ///
    coeflabels(2.treat_num "Treatment one" 3.treat_num "Treatment two") ///
    stats(ctrl_mean controls N, labels("Control mean" "Pre-treatment controls" "Number of observations")) ///
    collabels(none) nomtitles nonumbers nogaps nonotes compress


*----------------------------------------
* Table 2: ITT Effects on fertilizer nutrient use
*----------------------------------------
eststo clear

** Main crops only -	 test plot
reg N_kgha i.treat_num if main_crp==1, cluster(cluster_id)
eststo r1
reg N_kgha i.treat_num hh_size hh_age hh_educ dist_agro plot_siz agro_vis ext_srv slope soil_str seed_typ if main_crp==1, cluster(cluster_id)
eststo r2

reg P_kgha i.treat_num if main_crp==1, cluster(cluster_id)
eststo r3
reg P_kgha i.treat_num hh_size hh_age hh_educ dist_agro plot_siz agro_vis ext_srv slope soil_str seed_typ if main_crp==1, cluster(cluster_id)
eststo r4

reg K_kgha i.treat_num if main_crp==1, cluster(cluster_id)
eststo r5
reg K_kgha i.treat_num hh_size hh_age hh_educ dist_agro plot_siz agro_vis ext_srv slope soil_str seed_typ if main_crp==1, cluster(cluster_id)
eststo r6

reg totalnutrient_kgha i.treat_num if main_crp==1, cluster(cluster_id)
eststo r7
reg totalnutrient_kgha i.treat_num hh_size hh_age hh_educ dist_agro plot_siz agro_vis ext_srv slope soil_str seed_typ if main_crp==1, cluster(cluster_id)
eststo r8

** Export results
esttab, star(* 0.10 ** 0.05 *** 0.01) b(%10.2f)
esttab using "$results/Table XX - Fertilizer use nutrient amount.tex", se replace label b(%10.2f) ///
    star(* 0.10 ** 0.05 *** 0.01) stats(N, fmt(%9.0g %9.2g %9.2f) label("Number of Obs.")) nogaps nonotes compress
*----------------------------------------
* Table 3: ITT Effects on AIP fertilizer use
*----------------------------------------
eststo clear

** All crops
reg total_qty_fert_AIP i.treat_num, cluster(cluster_id)
eststo r1
reg total_qty_fert_AIP i.treat_num hh_size hh_age hh_educ dist_agro plot_siz agro_vis ext_srv slope soil_str seed_typ, cluster(cluster_id)
eststo r2

** Main crops (Maize)
reg total_qty_fert_AIP i.treat_num if main_crp==1, cluster(cluster_id)
eststo r3
reg total_qty_fert_AIP i.treat_num hh_size hh_age hh_educ dist_agro plot_siz agro_vis ext_srv slope soil_str seed_typ if main_crp==1, cluster(cluster_id)
eststo r4

** Export results
esttab, star(* 0.10 ** 0.05 *** 0.01) b(%10.2f)
esttab using "$results/fertilizer_use_totalAIP.tex", se replace label b(%10.2f) ///
    star(* 0.10 ** 0.05 *** 0.01) stats(N, fmt(%9.0g %9.2g %9.2f) label("Number of Obs.")) nogaps nonotes compress

	
*----------------------------------------
* Table 4: Effect of the program on yield
*----------------------------------------
eststo clear

reg lnyield i.treat_num, cluster(cluster_id)
eststo r1
reg lnyield i.treat_num hh_size hh_age hh_educ dist_agro plot_siz agro_vis ext_srv slope soil_str seed_typ, cluster(cluster_id)
eststo r2

** Export results
esttab, star(* 0.10 ** 0.05 *** 0.01) b(%10.2f)
esttab using "$results/maize_yield.tex", se replace label b(%10.2f) ///
    star(* 0.10 ** 0.05 *** 0.01) stats(N, fmt(%9.0g %9.2g %9.2f) label("Number of Obs.")) nogaps nonotes compress

	
**--------------------------------------------------**
** Food security variables
**--------------------------------------------------**
* Define value labels for feed_diff
label define feed_diff_lbl ///
    1 "Never" ///
    2 "Seldom" ///
    3 "Sometimes" ///
    4 "Often" ///
    5 "Very often (nearly always)", replace

* Attach labels
label values feed_diff feed_diff_lbl
label variable feed_diff "Frequency of feed difficulties"

* Construct binary indicators
capture drop food_insecurity
gen food_insecurity = (feed_diff > 1) if !missing(feed_diff)
label variable food_insecurity "Any food insecurity (Seldom or more frequent)"

capture drop extreme_foodinsecurity
gen extreme_foodinsecurity = (feed_diff > 3) if !missing(feed_diff)
label variable extreme_foodinsecurity "Extreme food insecurity (Often or nearly always)"

*-------------------------------------------------
* Table 5: Effect of the program on Food security
*-------------------------------------------------

eststo clear 
set more off 

reg food_insecurity i.treat_num, cluster(cluster_id)
eststo r1
reg food_insecurity i.treat_num hh_size hh_age hh_educ dist_agro plot_siz agro_vis ext_srv slope soil_str seed_typ, cluster(cluster_id)
eststo r2
reg extreme_foodinsecurity i.treat_num, cluster(cluster_id)
eststo r3
reg extreme_foodinsecurity i.treat_num hh_size hh_age hh_educ dist_agro plot_siz agro_vis ext_srv slope soil_str seed_typ, cluster(cluster_id)
eststo r4
** Export results
esttab, star(* 0.10 ** 0.05 *** 0.01) b(%10.2f)
esttab using "$results/maize_yield.tex", se replace label b(%10.2f) ///
    star(* 0.10 ** 0.05 *** 0.01) stats(N, fmt(%9.0g %9.2g %9.2f) label("Number of Obs."))


*======================================================
** Application Error 
*======================================================

** Regional Recommendations 

*======================================================
** Farm expenditures  
*======================================================
eststo clear
label define treat_lbl 1 "Control" 2 "Treatment one" 3 "Treatment two"
label values treat_num treat_lbl

local covariates hh_size hh_age hh_educ dist_agro plot_siz agro_vis ext_srv slope soil_str seed_typ 

* --- Column 1: ln_total_farm_exp, no controls ---
eststo r1: reg ln_total_farm_exp i.treat_num, cluster(cluster_id)
summ w_total_farm_exp if e(sample) & treat_num == 1
estadd scalar ctrl_mean = r(mean)
estadd local controls "No"

* --- Column 2: ln_total_farm_exp, with controls ---
eststo r2: reg ln_total_farm_exp i.treat_num `covariates', cluster(cluster_id)
summ w_total_farm_exp if e(sample) & treat_num == 1
estadd scalar ctrl_mean = r(mean)
estadd local controls "Yes"

* --- Column 3: ln_total_farm_exp_exc_fert, no controls ---
eststo r3: reg ln_total_farm_exp_exc_fert i.treat_num, cluster(cluster_id)
summ w_total_farm_exp_exc_fert if e(sample) & treat_num == 1
estadd scalar ctrl_mean = r(mean)
estadd local controls "No"

* --- Column 4: ln_total_farm_exp_exc_fert, with controls ---
eststo r4: reg ln_total_farm_exp_exc_fert i.treat_num `covariates', cluster(cluster_id)
summ w_total_farm_exp_exc_fert if e(sample) & treat_num == 1
estadd scalar ctrl_mean = r(mean)
estadd local controls "Yes"


* ---- EXPORT TABLE ----
esttab using "$results/total_farm_exp.tex", replace se label b(%10.2f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    keep(2.treat_num 3.treat_num _cons) ///
    stats(controls ctrl_mean N, ///
        fmt(%9s %9.2f %9.0g) ///
        label("Control variables" "Control group mean" "Number of observations"))
eststo clear


*======================================================
** Farm profits  
*======================================================

* --- Column 1: no controls ---
eststo r1: reg ln_farm_profits i.treat_num, cluster(cluster_id)
summ farm_profits if e(sample) & treat_num == 1
estadd scalar ctrl_mean = r(mean)
estadd local controls "No"

* --- Column 2: with controls ---
eststo r2: reg ln_farm_profits i.treat_num `covariates', cluster(cluster_id)
summ farm_profits if e(sample) & treat_num == 1
estadd scalar ctrl_mean = r(mean)
estadd local controls "Yes"

* ---- EXPORT TABLE ----
esttab using "$results/farm_profits.tex", replace se label b(%10.2f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    keep(2.treat_num 3.treat_num _cons) ///
    stats(controls ctrl_mean N, ///
        fmt(%9s %9.2f %9.0g) ///
        label("Control variables" "Control group mean" "Number of observations"))
		

*======================================================
** SNM practices
*======================================================
*==============================================================*
* TABLE A: green_inco, fresh_app, mat_farm_app, dairy_app
*==============================================================*

local outcomes1 green_inco fresh_app mat_farm_app dairy_app

* row labels
label var green_inco   "Green legume"
label var fresh_app    "Fresh vegetative"
label var mat_farm_app "Farm materials"
label var dairy_app    "Dairy adoption"

eststo clear

foreach y of local outcomes1 {

    * No controls
    eststo: probit `y' i.treat_num, cluster(cluster_id)
    summ `y' if e(sample) & treat_num == 1
    estadd scalar ctrl_mean = r(mean)
    estadd local controls "No"

    * With controls
    eststo: probit `y' i.treat_num `covariates', cluster(cluster_id)
    summ `y' if e(sample) & treat_num == 1
    estadd scalar ctrl_mean = r(mean)
    estadd local controls "Yes"
}

* Column titles
local mtitles1
foreach y of local outcomes1 {
    local mtitles1 `"`mtitles1' "No ctrls" "Ctrls""'
}

* Group headers
local mgroups1 `"Green_legume" "Fresh_vegetative" "Farm_materials" "Dairy_adoption"'

* Pattern for 4 outcomes → 4×“1 0”
local pattern1 ""
foreach y of local outcomes1 {
    local pattern1 "`pattern1' 1 0"
}

* Export TABLE A
esttab using "$results/SNM_practices1.tex", replace ///
    se label b(%10.2f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    keep(2.treat_num 3.treat_num _cons) ///
    mtitles(`mtitles1') ///
    mgroups(`mgroups1', pattern(`pattern1') span ///
            prefix(\multicolumn{@span}{c}{) suffix(})) ///
    stats(controls ctrl_mean N, ///
        fmt(%9s %9.2f %9.0g) ///
        label("Control variables" "Control group mean" "Number of observations"))
*==============================================================*
* TABLE B: comp_app, mbeya_app, till_app, ridge_use, pit_use
*==============================================================*

local outcomes2 comp_app mbeya_app till_app ridge_use pit_use

* Optional labels
label var comp_app   "Compost use"
label var mbeya_app  "Mbeya fertilizer"
label var till_app   "Tillage"
label var ridge_use  "Ridge use"
label var pit_use    "Pit planting"

eststo clear

foreach y of local outcomes2 {

    * No controls
    eststo: probit `y' i.treat_num, cluster(cluster_id)
    summ `y' if e(sample) & treat_num == 1
    estadd scalar ctrl_mean = r(mean)
    estadd local controls "No"

    * With controls
    eststo: probit `y' i.treat_num `covariates', cluster(cluster_id)
    summ `y' if e(sample) & treat_num == 1
    estadd scalar ctrl_mean = r(mean)
    estadd local controls "Yes"
}

* Column titles
local mtitles2
foreach y of local outcomes2 {
    local mtitles2 `"`mtitles2' "No ctrls" "Ctrls""'
}

* Group headers
local mgroups2 `"Compost_use" "Mbeya_fertilizer" "Tillage" "Ridge_use" "Pit_planting"'

* Pattern: 5 outcomes → 5×“1 0”
local pattern2 ""
foreach y of local outcomes2 {
    local pattern2 "`pattern2' 1 0"
}

* Export TABLE B
esttab using "$results/SNM_practices2.tex", replace ///
    se label b(%10.2f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    keep(2.treat_num 3.treat_num _cons) ///
    mtitles(`mtitles2') ///
    mgroups(`mgroups2', pattern(`pattern2') span ///
            prefix(\multicolumn{@span}{c}{) suffix(})) ///
    stats(controls ctrl_mean N, ///
        fmt(%9s %9.2f %9.0g) ///
        label("Control variables" "Control group mean" "Number of observations"))

		
		
		
*-------------------------------------------------
** 
*--------------------------------------------------


{/******************************************************************************
						Application Error  
********************************************************************************/
	}
*Run master do file
do Master.do 

use "$processed/estimation_data_v2", clear 

*------------------------------------------------------------*
* Fertilizer nutrient use per hectare (kg/ha)
*------------------------------------------------------------*

gen N_kgha = total_N/plot_siz
label variable N_kgha "Nitrogen applied (kg/ha)"

gen K_kgha = total_K/plot_siz
label variable K_kgha "Potassium applied (kg/ha)"

gen P_kgha = total_P/plot_siz
label variable P_kgha "Phosphorus applied (kg/ha)"

gen totalnutrient_kgha = total_nutrient/plot_siz
label variable totalnutrient_kgha "Total nutrients applied (kg/ha)"

*******************************************************
* 1. ACTUAL N, P, K
*******************************************************

gen actual_N = N_kgha
gen actual_P = P_kgha
gen actual_K = K_kgha

label var actual_N "Actual Nitrogen applied (Kg/Ha)"
label var actual_P "Actual Phosphorus applied (Kg/Ha)"
label var actual_K "Actual Potassium applied (Kg/Ha)"


*******************************************************
* 2. INITIALIZE TOTAL RECOMMENDED N, P, K
*******************************************************

gen rec_N_total = CR_PD_N_def if treat_num==1 // Assume that the the CR_PD_K_def id the gap between the actual and the recommended amount 
gen rec_P_total = CR_PD_P_def if treat_num==1
gen rec_K_total = CR_PD_K_def if treat_num==1

*******************************************************
* 4. TREATMENT GROUP (treat_num==2 | 3)
*******************************************************

replace rec_N_total = TR_N_Req if treat_num==2 | treat_num==3
replace rec_P_total = TR_P2O5_Corr_Req if treat_num==2 | treat_num==3
replace rec_K_total = TR_K2O_Req if treat_num==2 | treat_num==3


*******************************************************
* 5. LABEL TOTAL RECOMMENDED
*******************************************************

label var rec_N_total "Total Recommended Nitrogen (Kg/Ha)"
label var rec_P_total "Total Recommended Phosphorus (Kg/Ha)"
label var rec_K_total "Total Recommended Potassium (Kg/Ha)"

sum rec_N_total rec_P_total rec_K_total  actual_N actual_P actual_K

*******************************************************
* 6. ABSOLUTE APPLICATION ERROR
*******************************************************

gen abs_error_N = abs(actual_N - rec_N_total)
gen abs_error_P = abs(actual_P - rec_P_total)
gen abs_error_K = abs(actual_K - rec_K_total)

label var abs_error_N "|Actual - Recommended| Nitrogen (Kg/Ha)"
label var abs_error_P "|Actual - Recommended| Phosphorus (Kg/Ha)"
label var abs_error_K "|Actual - Recommended| Potassium (Kg/Ha)"



sum abs_error_N abs_error_P abs_error_K
bys treat_num: sum abs_error_N




*---------------------------------------------------------------------
* Table 6: Effect of the program on fertilizer application error 
*----------------------------------------------------------------------

eststo clear 
set more off 

reg abs_error_N i.treat_num, cluster(cluster_id)
eststo r1
reg abs_error_N i.treat_num hh_size hh_age hh_educ dist_agro plot_siz agro_vis ext_srv slope soil_str seed_typ, cluster(cluster_id)
eststo r2
reg abs_error_P i.treat_num, cluster(cluster_id)
eststo r3
reg abs_error_P i.treat_num hh_size hh_age hh_educ dist_agro plot_siz agro_vis ext_srv slope soil_str seed_typ, cluster(cluster_id)
eststo r4
reg abs_error_K i.treat_num, cluster(cluster_id)
eststo r5
reg abs_error_K i.treat_num hh_size hh_age hh_educ dist_agro plot_siz agro_vis ext_srv slope soil_str seed_typ, cluster(cluster_id)
eststo r6

** Export results
esttab, star(* 0.10 ** 0.05 *** 0.01) b(%10.2f)
esttab using "$results/maize_yield.tex", se replace label b(%10.2f) ///
    star(* 0.10 ** 0.05 *** 0.01) stats(N, fmt(%9.0g %9.2g %9.2f) label("Number of Obs."))


esttab using "$results/maize_yield.rtf", se replace label b(%10.2f) ///
    star(* 0.10 ** 0.05 *** 0.01) stats(N, fmt(%9.0g %9.2g %9.2f) label("Number of Obs."))





	
********************************************
*** Application Error - Alternative specification using the total use of inorganic fertilizer *-------------------------------------

*** Recommended Nutrient value for the treatment groups

********************************************************************************
* FULL STATA CODE (SECOND VERSION): Recommended N, P, K (kg/ha)
********************************************************************************

*-----------------------------------------------*
* 1. Convert string variables to numeric
*-----------------------------------------------*

use "$processed/estimation_data_v2", clear 

*------------------------------------------------------------*
* Fertilizer nutrient use per hectare (kg/ha)
*------------------------------------------------------------*

gen N_kgha = total_N/plot_siz
label variable N_kgha "Nitrogen applied (kg/ha)"

gen K_kgha = total_K/plot_siz
label variable K_kgha "Potassium applied (kg/ha)"

gen P_kgha = total_P/plot_siz
label variable P_kgha "Phosphorus applied (kg/ha)"

gen totalnutrient_kgha = total_nutrient/plot_siz
label variable totalnutrient_kgha "Total nutrients applied (kg/ha)"

*******************************************************
* 1. ACTUAL N, P, K
*******************************************************

gen actual_N = N_kgha
gen actual_P = P_kgha
gen actual_K = K_kgha

label var actual_N "Actual Nitrogen applied (Kg/Ha)"
label var actual_P "Actual Phosphorus applied (Kg/Ha)"
label var actual_K "Actual Potassium applied (Kg/Ha)"

********************************************************************************
* CLEAN FERTILIZER STRING VARIABLES AND GENERATE RECOMMENDED N, P, K
* Assumes fertilizer values are stored like: 280.0Kg/Ha
********************************************************************************

*-----------------------------*
* 1. Create cleaned numeric variables
*-----------------------------*

gen npk141420_num = real(subinstr(lower(trim(TR_PLANTINGNPK1414204S2M)), "kg/ha", "", .))
gen can_num       = real(subinstr(lower(trim(TR_TOPDRESSCalciumAmmoniumNitra)), "kg/ha", "", .))
gen potsul_num    = real(subinstr(lower(trim(TR_TOPDRESSPotassiumSulphate)), "kg/ha", "", .))
gen npk23105_num  = real(subinstr(lower(trim(TR_PLANTINGNPK231056S1Zn)), "kg/ha", "", .))
gen npk81815_num  = real(subinstr(lower(trim(TR_PLANTINGNPK818156S01B)), "kg/ha", "", .))
gen sop_num       = real(subinstr(lower(trim(TR_TOPDRESSSOP)), "kg/ha", "", .))
gen mop_num       = real(subinstr(lower(trim(TR_TOPDRESSMOP)), "kg/ha", "", .))
gen calcitic_num  = real(subinstr(lower(trim(TR_SOILCORRECTIONCALCITICLIME)), "kg/ha", "", .))
gen dolomitic_num = real(subinstr(lower(trim(TR_SOILCORRECTIONDOLOMITICLIME)), "kg/ha", "", .))
gen urea_num      = real(subinstr(lower(trim(TR_TOPDRESSUrea)), "kg/ha", "", .))
gen map_num       = real(subinstr(lower(trim(TR_SOILCORRECTIONMAPTECHNICALG)), "kg/ha", "", .))
gen npk152316_num = real(subinstr(lower(trim(TR_PLANTINGNPK1523166S05Zn0)), "kg/ha", "", .))
gen manure_num    = real(subinstr(lower(trim(TR_SOILCORRECTIONMANURECOMPOST)), "kg/ha", "", .))

*-----------------------------*
* 2. Replace missings with zero
*-----------------------------*

foreach var in npk141420_num can_num potsul_num npk23105_num npk81815_num ///
               sop_num mop_num calcitic_num dolomitic_num urea_num ///
               map_num npk152316_num manure_num {
    replace `var' = 0 if missing(`var')
}

*-----------------------------*
* 3. Generate recommended nutrient amounts
*-----------------------------*

* Nitrogen (N)
gen N_recommended = ///
    0.14*npk141420_num + ///
    0.26*can_num + ///
    0.23*npk23105_num + ///
    0.08*npk81815_num + ///
    0.46*urea_num + ///
    0.12*map_num + ///
    0.15*npk152316_num

* Phosphorus (P) as elemental P
* P2O5 -> P conversion factor = 0.44
gen P_recommended = ///
    (0.14*0.44)*npk141420_num + ///
    (0.10*0.44)*npk23105_num + ///
    (0.18*0.44)*npk81815_num + ///
    (0.61*0.44)*map_num + ///
    (0.23*0.44)*npk152316_num

* Potassium (K) as elemental K
* K2O -> K conversion factor = 0.83
gen K_recommended = ///
    (0.20*0.83)*npk141420_num + ///
    (0.50*0.83)*potsul_num + ///
    (0.05*0.83)*npk23105_num + ///
    (0.15*0.83)*npk81815_num + ///
    (0.50*0.83)*sop_num + ///
    (0.60*0.83)*mop_num + ///
    (0.16*0.83)*npk152316_num

*-----------------------------*
* 4. Label variables
*-----------------------------*

label var npk141420_num "Cleaned amount of NPK 14-14-20 (kg/ha)"
label var can_num       "Cleaned amount of CAN (kg/ha)"
label var potsul_num    "Cleaned amount of Potassium Sulphate (kg/ha)"
label var npk23105_num  "Cleaned amount of NPK 23-10-5 (kg/ha)"
label var npk81815_num  "Cleaned amount of NPK 8-18-15 (kg/ha)"
label var sop_num       "Cleaned amount of SOP (kg/ha)"
label var mop_num       "Cleaned amount of MOP (kg/ha)"
label var calcitic_num  "Cleaned amount of Calcitic lime (kg/ha)"
label var dolomitic_num "Cleaned amount of Dolomitic lime (kg/ha)"
label var urea_num      "Cleaned amount of Urea (kg/ha)"
label var map_num       "Cleaned amount of MAP technical grade (kg/ha)"
label var npk152316_num "Cleaned amount of NPK 15-23-16 (kg/ha)"
label var manure_num    "Cleaned amount of Manure/Compost (kg/ha)"

label var N_recommended "Recommended nitrogen (kg/ha)"
label var P_recommended "Recommended phosphorus (kg/ha, elemental P)"
label var K_recommended "Recommended potassium (kg/ha, elemental K)"

*-----------------------------*
* 5. Optional oxide forms
*-----------------------------*

gen P2O5_recommended = ///
    0.14*npk141420_num + ///
    0.10*npk23105_num + ///
    0.18*npk81815_num + ///
    0.61*map_num + ///
    0.23*npk152316_num

gen K2O_recommended = ///
    0.20*npk141420_num + ///
    0.50*potsul_num + ///
    0.05*npk23105_num + ///
    0.15*npk81815_num + ///
    0.50*sop_num + ///
    0.60*mop_num + ///
    0.16*npk152316_num

label var P2O5_recommended "Recommended phosphate (kg/ha as P2O5)"
label var K2O_recommended  "Recommended potash (kg/ha as K2O)"

*-----------------------------*
* 6. Quick checks
*-----------------------------*

summ npk141420_num can_num potsul_num npk23105_num npk81815_num ///
     sop_num mop_num urea_num map_num npk152316_num ///
     N_recommended P_recommended K_recommended ///
     P2O5_recommended K2O_recommended


sum  N_recommended P_recommended K_recommended ///
     P2O5_recommended K2O_recommended

bys treat_num: sum  N_recommended P_recommended K_recommended ///
     P2O5_recommended K2O_recommended

	 
		
********************************************************************************
* CLEAN CONTROL-GROUP FERTILIZER STRING VARIABLES AND GENERATE
* RECOMMENDED N, P, K
* Assumes values are stored like: 280.0Kg/Ha
********************************************************************************
********************************************************************************
* CONTROL GROUP: CLEAN FERTILIZER VARIABLES AND GENERATE RECOMMENDED N, P, K
* This code handles both string and numeric source variables.
********************************************************************************

*-----------------------------*
* 1. Create short numeric versions
*-----------------------------*

capture confirm string variable CR_PLANTINGNPK231056S1ZnK
if _rc == 0 {
    gen c_npk23105_num = real(subinstr(lower(trim(CR_PLANTINGNPK231056S1ZnK)), "kg/ha", "", .))
}
else {
    gen c_npk23105_num = CR_PLANTINGNPK231056S1ZnK
}

capture confirm string variable CR_PLANTINGNPK818156S01B
if _rc == 0 {
    gen c_npk81815_num = real(subinstr(lower(trim(CR_PLANTINGNPK818156S01B)), "kg/ha", "", .))
}
else {
    gen c_npk81815_num = CR_PLANTINGNPK818156S01B
}

capture confirm string variable CR_TOPDRESSCalciumAmmoniumNitra
if _rc == 0 {
    gen c_can_num = real(subinstr(lower(trim(CR_TOPDRESSCalciumAmmoniumNitra)), "kg/ha", "", .))
}
else {
    gen c_can_num = CR_TOPDRESSCalciumAmmoniumNitra
}

capture confirm string variable CR_TopdressBlend_UreaKgHa
if _rc == 0 {
    gen c_urea_num = real(subinstr(lower(trim(CR_TopdressBlend_UreaKgHa)), "kg/ha", "", .))
}
else {
    gen c_urea_num = CR_TopdressBlend_UreaKgHa
}

capture confirm string variable CR_TOPDRESSMOPkgha
if _rc == 0 {
    gen c_mop_num = real(subinstr(lower(trim(CR_TOPDRESSMOPkgha)), "kg/ha", "", .))
}
else {
    gen c_mop_num = CR_TOPDRESSMOPkgha
}

capture confirm string variable CR_SOILCORRECTIONCALCITICLIME
if _rc == 0 {
    gen c_calcitic_num = real(subinstr(lower(trim(CR_SOILCORRECTIONCALCITICLIME)), "kg/ha", "", .))
}
else {
    gen c_calcitic_num = CR_SOILCORRECTIONCALCITICLIME
}

capture confirm string variable CR_SOILCORRECTIONDOLOMITICLIME
if _rc == 0 {
    gen c_dolomitic_num = real(subinstr(lower(trim(CR_SOILCORRECTIONDOLOMITICLIME)), "kg/ha", "", .))
}
else {
    gen c_dolomitic_num = CR_SOILCORRECTIONDOLOMITICLIME
}

*-----------------------------*
* 2. Replace missing with zero
*-----------------------------*

foreach var in c_npk23105_num c_npk81815_num c_can_num c_urea_num ///
               c_mop_num c_calcitic_num c_dolomitic_num {
    replace `var' = 0 if missing(`var')
}

*-----------------------------*
* 3. Generate recommended nutrients
*-----------------------------*

* Nitrogen (N)
* Assumptions:
* - NPK 23:10:5 contains 23% N
* - NPK 8:18:15 contains 8% N
* - CAN contains 26% N
* - Blend_Urea is treated as Urea with 46% N
gen c_N_recommended = ///
    0.23*c_npk23105_num + ///
    0.08*c_npk81815_num + ///
    0.26*c_can_num + ///
    0.46*c_urea_num

* Phosphorus (P) as elemental P
* Convert from P2O5 to P using 0.44
gen c_P_recommended = ///
    (0.10*0.44)*c_npk23105_num + ///
    (0.18*0.44)*c_npk81815_num

* Potassium (K) as elemental K
* Convert from K2O to K using 0.83
gen c_K_recommended = ///
    (0.05*0.83)*c_npk23105_num + ///
    (0.15*0.83)*c_npk81815_num + ///
    (0.60*0.83)*c_mop_num

*-----------------------------*
* 4. Optional oxide-form nutrients
*-----------------------------*

gen c_P2O5_recommended = ///
    0.10*c_npk23105_num + ///
    0.18*c_npk81815_num

gen c_K2O_recommended = ///
    0.05*c_npk23105_num + ///
    0.15*c_npk81815_num + ///
    0.60*c_mop_num

*-----------------------------*
* 5. Labels
*-----------------------------*

label var c_npk23105_num  "Control NPK 23-10-5 amount (kg/ha)"
label var c_npk81815_num  "Control NPK 8-18-15 amount (kg/ha)"
label var c_can_num       "Control CAN amount (kg/ha)"
label var c_urea_num      "Control blend/urea amount (kg/ha)"
label var c_mop_num       "Control MOP amount (kg/ha)"
label var c_calcitic_num  "Control calcitic lime amount (kg/ha)"
label var c_dolomitic_num "Control dolomitic lime amount (kg/ha)"

label var c_N_recommended    "Control recommended nitrogen (kg/ha)"
label var c_P_recommended    "Control recommended phosphorus (kg/ha, elemental P)"
label var c_K_recommended    "Control recommended potassium (kg/ha, elemental K)"
label var c_P2O5_recommended "Control recommended phosphate (kg/ha as P2O5)"
label var c_K2O_recommended  "Control recommended potash (kg/ha as K2O)"

*-----------------------------*
* 6. Quick checks
*-----------------------------*

summ c_npk23105_num c_npk81815_num c_can_num c_urea_num c_mop_num ///
     c_N_recommended c_P_recommended c_K_recommended ///
     c_P2O5_recommended c_K2O_recommended

list CR_PLANTINGNPK231056S1ZnK c_npk23105_num ///
     CR_TOPDRESSCalciumAmmoniumNitra c_can_num ///
     CR_TopdressBlend_UreaKgHa c_urea_num ///
     c_N_recommended c_P_recommended c_K_recommended in 1/10
********************************************************************************



*** Recommended rates of N, P, K based on fertilizer use 

tab treat_num, nol 

gen N_recomm_fertuse_kgha=N_recommended if treat_num==2|treat_num==3
replace N_recomm_fertuse_kgha=c_N_recommended if treat_num==1

gen P_recomm_fertuse_kgha=P_recommended if treat_num==2|treat_num==3
replace P_recomm_fertuse_kgha=c_P_recommended if treat_num==1

gen K_recomm_fertuse_kgha=K_recommended if treat_num==2|treat_num==3
replace K_recomm_fertuse_kgha=c_K_recommended if treat_num==1

gen P2O5_recomm_fertuse_kgha=P2O5_recommended if treat_num==2|treat_num==3
replace P2O5_recomm_fertuse_kgha=c_P2O5_recommended if treat_num==1

ge K2O_recomm_fertuse_kgha=K2O_recommended if treat_num==2|treat_num==3
replace K2O_recomm_fertuse_kgha=c_K2O_recommended if treat_num==1


*bys treat_num: sum  N_recommended P_recommended K_recommended ///
*     P2O5_recommended K2O_recommended

	 

*******************************************************
* ABSOLUTE APPLICATION ERROR
*******************************************************

gen abs_error_N2 = abs(actual_N - N_recomm_fertuse_kgha)
gen abs_error_P2 = abs(actual_P - P_recomm_fertuse_kgha)
gen abs_error_K2 = abs(actual_K - K_recomm_fertuse_kgha)
gen abs_error_P2O5 = abs(actual_P - P2O5_recomm_fertuse_kgha)
gen abs_error_K2O = abs(actual_K - K2O_recomm_fertuse_kgha)



label var abs_error_N2 "|Actual - Recommended| Nitrogen (Kg/Ha)"
label var abs_error_P2 "|Actual - Recommended| Phosphorus (Kg/Ha)"
label var abs_error_K2 "|Actual - Recommended| Potassium (Kg/Ha)"



sum abs_error_N2 abs_error_P2 abs_error_K2
bys treat_num: sum abs_error_N2




*---------------------------------------------------------------------
* Table 6: Effect of the program on fertilizer application error 
*----------------------------------------------------------------------

eststo clear 
set more off 

reg abs_error_N2 i.treat_num, cluster(cluster_id)
eststo r1
reg abs_error_N2 i.treat_num hh_size hh_age hh_educ dist_agro plot_siz agro_vis ext_srv slope soil_str seed_typ, cluster(cluster_id)
eststo r2
reg abs_error_P2 i.treat_num, cluster(cluster_id)
eststo r3
reg abs_error_P2 i.treat_num hh_size hh_age hh_educ dist_agro plot_siz agro_vis ext_srv slope soil_str seed_typ, cluster(cluster_id)
eststo r4
reg abs_error_K2 i.treat_num, cluster(cluster_id)
eststo r5
reg abs_error_K2 i.treat_num hh_size hh_age hh_educ dist_agro plot_siz agro_vis ext_srv slope soil_str seed_typ, cluster(cluster_id)
eststo r6


** Export results
esttab, star(* 0.10 ** 0.05 *** 0.01) b(%10.2f)
esttab using "$results/maize_yield2.tex", se replace label b(%10.2f) ///
    star(* 0.10 ** 0.05 *** 0.01) stats(N, fmt(%9.0g %9.2g %9.2f) label("Number of Obs."))


esttab using "$results/maize_yield2.rtf", se replace label b(%10.2f) ///
    star(* 0.10 ** 0.05 *** 0.01) stats(N, fmt(%9.0g %9.2g %9.2f) label("Number of Obs."))

eststo clear 

reg abs_error_P2O5 i.treat_num, cluster(cluster_id)
eststo r7
reg abs_error_P2O5 i.treat_num hh_size hh_age hh_educ dist_agro plot_siz agro_vis ext_srv slope soil_str seed_typ, cluster(cluster_id)
eststo r8
reg abs_error_K2O i.treat_num, cluster(cluster_id)
eststo r9
reg abs_error_K2O i.treat_num hh_size hh_age hh_educ dist_agro plot_siz agro_vis ext_srv slope soil_str seed_typ, cluster(cluster_id)
eststo r10

esttab, star(* 0.10 ** 0.05 *** 0.01) b(%10.2f)
esttab using "$results/maize_yield2.tex", se replace label b(%10.2f) ///
    star(* 0.10 ** 0.05 *** 0.01) stats(N, fmt(%9.0g %9.2g %9.2f) label("Number of Obs."))


esttab using "$results/maize_yield2.rtf", se replace label b(%10.2f) ///
    star(* 0.10 ** 0.05 *** 0.01) stats(N, fmt(%9.0g %9.2g %9.2f) label("Number of Obs."))


	
	
** Squaring it and give the more weight for the larger the gap 
** Differetate between under and over shooting 

	
