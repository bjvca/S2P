
{/******************************************************************************
						Title; Space to Place Project in Malawi
						Purpose: Prodcue estimation results 
********************************************************************************/
	}
	
*Run master and data cleaning do files 
do Master.do 
*do "Data cleaning and preparation.do" 

use "$processed/estimation_data", clear 

* Convert cluster_id from string to numeric
* Create a numeric cluster_id variable
encode cluster_id, gen(cluster_id_num1)


*----------------------------------------
* Table 1: ITT Effects on total fertilizer use
*----------------------------------------
local varlist_used hh_size hh_age hh_educ hh_gender feed_diff dist_agro ///
                    get_rec ease_rec buy_rec buy_other vou_en agro_vis ext_srv times_ext ///
                    plot_siz slope soil_str seed_typ folll_rec got_voucher vou_en dist_plot main_crp int_crpp Perc_main qty_used bags_Mcrp
       
	   
** All crops
reg total_qty_fert i.treat_num , cluster(cluster_id)
eststo r1

reg total_qty_fert i.treat_num hh_size hh_age hh_educ dist_agro plot_siz agro_vis ext_srv slope soil_str seed_typ , cluster(cluster_id)
eststo r2

** Main crops (Maize, main_crp==5)
reg total_qty_fert i.treat_num if main_crp==5, cluster(cluster_id)
eststo r3

reg total_qty_fert i.treat_num hh_size hh_age hh_educ dist_agro plot_siz agro_vis ext_srv slope soil_str seed_typ if main_crp==5, cluster(cluster_id)
eststo r4

** Export results
label variable total_qty_fert "Total fertilizer use (kg)"
label variable treat_num    "Treatment group"
label variable hh_size        "Household size"
label variable hh_age         "Household head age (years)"
label variable hh_educ        "Household head education (years)"
label variable dist_agro      "Distance to agro-dealer (km)"
label variable plot_siz       "Plot size (acres)"
label variable agro_vis       "Visited agro-dealer (Yes=1)"
label variable ext_srv        "Access to extension services (Yes=1)"
label variable slope          "Plot slope"
label variable soil_str       "Soil structure"
label variable seed_typ       "Seed type used"

esttab, star(* 0.10 ** 0.05 *** 0.01) b(%10.2f)
esttab using "$OUTDIR/Table xx Fertilizer use amount.tex", se replace label b(%10.2f) ///
    star(* 0.10 ** 0.05 *** 0.01) stats(N, fmt(%9.0g %9.2g %9.2f) label("Number of Obs.")) ///
    nogaps nonotes compress


*----------------------------------------
* Table 2: ITT Effects on fertilizer nutrient use
*----------------------------------------
eststo clear

** Main crops only
reg total_N i.treat_num if main_crp==5, cluster(cluster_id)
eststo r1
reg total_N i.treat_num hh_size hh_age hh_educ dist_agro plot_siz agro_vis ext_srv slope soil_str seed_typ if main_crp==5, cluster(cluster_id)
eststo r2

reg total_P i.treat_num if main_crp==5, cluster(cluster_id)
eststo r3
reg total_P i.treat_num hh_size hh_age hh_educ dist_agro plot_siz agro_vis ext_srv slope soil_str seed_typ if main_crp==5, cluster(cluster_id)
eststo r4

reg total_K i.treat_num if main_crp==5, cluster(cluster_id)
eststo r5
reg total_K i.treat_num hh_size hh_age hh_educ dist_agro plot_siz agro_vis ext_srv slope soil_str seed_typ if main_crp==5, cluster(cluster_id)
eststo r6

reg total_nutrient i.treat_num if main_crp==5, cluster(cluster_id)
eststo r7
reg total_nutrient i.treat_num hh_size hh_age hh_educ dist_agro plot_siz agro_vis ext_srv slope soil_str seed_typ if main_crp==5, cluster(cluster_id)
eststo r8

** Export results
esttab, star(* 0.10 ** 0.05 *** 0.01) b(%10.2f)
esttab using "$OUTDIR/Table XX - Fertilizer use nutrient amount.tex", se replace label b(%10.2f) ///
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
reg total_qty_fert_AIP i.treat_num if main_crp==5, cluster(cluster_id)
eststo r3
reg total_qty_fert_AIP i.treat_num hh_size hh_age hh_educ dist_agro plot_siz agro_vis ext_srv slope soil_str seed_typ if main_crp==5, cluster(cluster_id)
eststo r4

** Export results
esttab, star(* 0.10 ** 0.05 *** 0.01) b(%10.2f)
esttab using "$OUTDIR/fertilizer_use_totalAIP.tex", se replace label b(%10.2f) ///
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
esttab using "$OUTDIR/maize_yield.tex", se replace label b(%10.2f) ///
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
esttab using "$OUTDIR/maize_yield.tex", se replace label b(%10.2f) ///
    star(* 0.10 ** 0.05 *** 0.01) stats(N, fmt(%9.0g %9.2g %9.2f) label("Number of Obs."))


*======================================================
** Application Error 
*======================================================

** Regional Recommendations 


	
	
