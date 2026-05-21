
{/******************************************************************************
						Title; Space to Place Project in Malawi
						Purpose: Clean and prepare the dataset for analysis 
********************************************************************************/
	}
*Run master do file
do Master.do 

use "$processed/endline_labeled", clear 

*---------------------------------------------
* Rename plot-related variables for clarity
*---------------------------------------------
*---------------------------------------------
* Rename plot and input-related variables
*---------------------------------------------
rename (test_plotplot_siz test_plotslope test_plotsoil_str test_plotdist_plot ///
        test_plotmain_crp test_plotint_crpp test_plotcrp_cropp test_plotperc_main ///
        test_plotseed_typ test_plotseed_bag test_plotseed_var test_plotseed_sourc ///
        test_plotseed_rec_times test_plotqty_used test_plotprice_seed test_plotseed_ob_aip ///
        test_plotobt_aip test_plotpay_meth test_plotsatis_qual test_plotsatis_yield ///
        test_plotsatis_dis test_plotsatis_drought test_plotsatis_avail ///
        test_plotweed_time test_plotpest_use test_plotttl_cost_pest test_plotgreen_inco ///
        test_plotcrp_prec test_plotfresh_app test_plotmat_farm_app test_plotdairy_app ///
        test_plotcomp_app test_plotmbeya_app test_plottill_app test_plotridge_use ///
        test_plotpit_use test_plotbags_mcrp test_plotmain_sell test_plotbags_sell ///
        test_plotprc_main test_plotother_exp) ///
       (plot_siz slope soil_str dist_plot main_crp int_crpp crp_cropp Perc_main ///
        seed_typ seed_bag seed_var seed_sourc seed_rec_times qty_used price_seed ///
        seed_ob_AIP obt_AIP pay_meth satis_qual satis_yield satis_dis satis_drought satis_avail ///
        weed_time pest_use ttl_cost_pest green_inco crp_prec fresh_app mat_farm_app dairy_app ///
        comp_app mbeya_app till_app ridge_use pit_use bags_Mcrp main_sell bags_sell prc_main other_exp)

*---------------------------------------------
* Fertilizer use cleaning (per type)
*---------------------------------------------
foreach v in map dap urea can npk14142 npk23105 npk81815 npk15231 sop mop calcitic dolomiti potassiu other {
   gen fert_`v' = .
   replace fert_`v' = test_plotfertilizer_list`v'
   la var fert_`v' "Uses fertilizer type: `v' on test plot"
}

clonevar repeat_count = test_plotfertilizer_repeat_count
label define fertype 1 "UREA" 2 "NPK23:10:5" 3 "CAN" 4 "NPK8:18:15" 5 "Other"

*---------------------------------------------
* Repeat fertilizer groups
*---------------------------------------------
forvalues r = 1/4 {
    
    * Fertilizer type variable (string → numeric with labels)
    gen ferti`r' = .
    replace ferti`r' = 1 if test_plot`r'fertilizer_name == 3 
    replace ferti`r' = 2 if test_plot`r'fertilizer_name == 6 
    replace ferti`r' = 3 if test_plot`r'fertilizer_name == 4 
    replace ferti`r' = 4 if test_plot`r'fertilizer_name == 7 
    replace ferti`r' = 5 if test_plot`r'fertilizer_name == 14 
    label values ferti`r' fertype
    label var ferti`r' "Fertilizer type, repeat `r'"


    * Create numeric versions of the variables
    clonevar fert_time`r'    = test_plot`r'fert_time
    clonevar app_method`r'   = test_plot`r'fert_app_meth
    clonevar weather`r'      = test_plot`r'weather
    clonevar qty_fert`r'     = test_plot`r'qty_fert
    clonevar fert_cost`r'    = test_plot`r'fert_cost
    
    * Binary Yes/No for ob_AIP
    clonevar ob_AIP`r' = test_plot`r'ob_aip
	
    clonevar qty_fert_AIP`r' = test_plot`r'qty_fert_aip
    
    * Binary Yes/No for fert_us
    clonevar fert_us`r' = test_plot`r'fert_us
      
    * Numeric for your_qunt_us
    gen your_qunt_us`r' = test_plot`r'vour_qnt_us
    
    * Binary Yes/No for fert_sat
    clonevar fert_sat`r' = test_plot`r'fert_sat
  	
	* Fertilizer type labels
	label values ferti`r' fertype
}


*---------------------------------------------------------
* . Aggregates across all repeats
*---------------------------------------------------------
egen total_qty_fert = rowtotal(qty_fert1 qty_fert2 qty_fert3 qty_fert4)
egen total_qty_fert_AIP = rowtotal(qty_fert_AIP1 qty_fert_AIP2 qty_fert_AIP3 qty_fert_AIP4)
egen total_fert_cost = rowtotal(fert_cost1 fert_cost2 fert_cost3 fert_cost4)

label var total_qty_fert       "Total fertilizer quantity in Kg"
label var total_qty_fert_AIP   "Total fertilizer quantity from AIP in Kg"
label var total_fert_cost      "Total fertilizer cost"

*---------------------------------------------
* Fertilizer nutrients (Extended: N, P2O5, K2O, S, Mg, Ca)
*---------------------------------------------
capture drop N1-N4 P1-P4 K1-K4 S1-S4 Mg1-Mg4 Ca1-Ca4 ///
              total_N total_P2O5 total_K2O total_S total_Mg total_Ca total_nutrient

forvalues r = 1/4 {

    gen N`r'  = .
    gen P`r'  = .      // P2O5
    gen K`r'  = .      // K2O
    gen S`r'  = .
    gen Mg`r' = .
    gen Ca`r' = .

    *--------------------------------------------------
    * 1. UREA (46% N)
    *--------------------------------------------------
    replace N`r'  = qty_fert`r' * 0.46 if ferti`r'==1
    replace P`r'  = 0 if ferti`r'==1
    replace K`r'  = 0 if ferti`r'==1
    replace S`r'  = 0 if ferti`r'==1
    replace Mg`r' = 0 if ferti`r'==1
    replace Ca`r' = 0 if ferti`r'==1

    *--------------------------------------------------
    * 2. NPK 23:10:5  (+ assume 6S if applicable)
    *--------------------------------------------------
    replace N`r'  = qty_fert`r' * 0.23 if ferti`r'==2
    replace P`r'  = qty_fert`r' * 0.10 if ferti`r'==2   // P2O5
    replace K`r'  = qty_fert`r' * 0.05 if ferti`r'==2   // K2O
    replace S`r'  = qty_fert`r' * 0.06 if ferti`r'==2   // 6% Sulphur
    replace Mg`r' = 0 if ferti`r'==2
    replace Ca`r' = 0 if ferti`r'==2

    *--------------------------------------------------
    * 3. CAN (27% N, ~8% Ca typical)
    *--------------------------------------------------
    replace N`r'  = qty_fert`r' * 0.27 if ferti`r'==3
    replace P`r'  = 0 if ferti`r'==3
    replace K`r'  = 0 if ferti`r'==3
    replace S`r'  = 0 if ferti`r'==3
    replace Mg`r' = 0 if ferti`r'==3
    replace Ca`r' = qty_fert`r' * 0.08 if ferti`r'==3

    *--------------------------------------------------
    * 4. NPK 8:18:15
    *--------------------------------------------------
    replace N`r'  = qty_fert`r' * 0.08 if ferti`r'==4
    replace P`r'  = qty_fert`r' * 0.18 if ferti`r'==4
    replace K`r'  = qty_fert`r' * 0.15 if ferti`r'==4
    replace S`r'  = 0 if ferti`r'==4
    replace Mg`r' = 0 if ferti`r'==4
    replace Ca`r' = 0 if ferti`r'==4

    *--------------------------------------------------
    * 5. Other (leave missing)
    *--------------------------------------------------
    replace N`r'  = . if ferti`r'==5
    replace P`r'  = . if ferti`r'==5
    replace K`r'  = . if ferti`r'==5
    replace S`r'  = . if ferti`r'==5
    replace Mg`r' = . if ferti`r'==5
    replace Ca`r' = . if ferti`r'==5
}

*---------------------------------------------
* Totals across applications
*---------------------------------------------
egen total_N    = rowtotal(N1 N2 N3 N4)
egen total_P2O5 = rowtotal(P1 P2 P3 P4)
egen total_K2O  = rowtotal(K1 K2 K3 K4)
egen total_S    = rowtotal(S1 S2 S3 S4)
egen total_Mg   = rowtotal(Mg1 Mg2 Mg3 Mg4)
egen total_Ca   = rowtotal(Ca1 Ca2 Ca3 Ca4)

gen total_nutrient = total_N + total_P2O5 + total_K2O + total_S + total_Mg + total_Ca

*---------------------------------------------
* Labels
*---------------------------------------------
label var total_N    "Total Nitrogen applied (kg)"
label var total_P2O5 "Total Phosphorus (P2O5) applied (kg)"
label var total_K2O  "Total Potassium (K2O) applied (kg)"
label var total_S    "Total Sulphur applied (kg)"
label var total_Mg   "Total Magnesium applied (kg)"
label var total_Ca   "Total Calcium applied (kg)"
label var total_nutrient "Total nutrient applied (N+P2O5+K2O+S+Mg+Ca) kg"

sum total_N total_P2O5 total_K2O total_S total_Mg total_Ca total_nutrient

*---------------------------------------------
* Treatment variable
*---------------------------------------------
encode treat, gen(treat_num) 
label list treat_num


*--------------------------------------------- 
* Clean/convert key variables
*--------------------------------------------- 

recode hh_gender (2=0)
label define hh_gender 1 "Male" 0 "Female"
label values hh_gender hh_gender

gen seed_typ_num = .
replace seed_typ_num = 1 if seed_typ == 3 
replace seed_typ_num = 2 if seed_typ == 2 
replace seed_typ_num = 3 if seed_typ == 1 
replace seed_typ_num = 4 if seed_typ == .  
replace seed_typ_num = 5 if seed_typ == 4 
*rename seed_typ_num seed_typ
label define seed_typ 1 "Improved reused" 2 "Improved newly acquired" ///
					 3 "Indigenous/Local" 4 "n/a" 5 "Unknown"
label values seed_typ seed_typ_num

recode seed_typ (.=5)
	


*---------------------------------------------
*Yield - maize 
*---------------------------------------------
gen maize_area = . 
replace maize_area = plot_siz if main_crp==1 & int_crpp==0
replace maize_area = plot_siz * Perc_main/100 if main_crp==1 & int_crpp==1

gen yield_maize = . 
replace yield_maize = bags_Mcrp / maize_area if maize_area > 0 & main_crp==1
label variable yield_maize "Maize yield (kg/acre)"

gen lnyield = . 
replace lnyield = log(yield_maize) if yield_maize > 0
label variable lnyield "Log of maize yield (kg/acre)"

*---------------------------------------------
* Farm expenditures - maize 
*---------------------------------------------

egen total_farm_exp = rowtotal(price_seed ttl_cost_pest test_plotttl_exp fert_cost1 fert_cost2 fert_cost3 fert_cost4) if main_crp==1
egen total_farm_exp_exc_fert = rowtotal(price_seed ttl_cost_pest test_plotttl_exp) if main_crp==1
	
	*Winzorizing the prices 
	foreach v of varlist total_farm_exp total_farm_exp_exc_fert {

		* Create winsorized version
		gen w_`v' = `v'

		* Loop over each treatment group
		levelsof treat_num, local(groups)
		foreach g of local groups {

			* Compute 1st and 99th percentiles within group g
			_pctile `v' if treat_num == `g', p(1 99)
			local p1 = r(r1)
			local p99 = r(r2)

			* Apply winsorization for this group only
			replace w_`v' = `p1' if treat_num == `g' & w_`v' < `p1' & w_`v' != .
			replace w_`v' = `p99' if treat_num == `g' & w_`v' > `p99' & w_`v' != .
		}

		* Label variable
		label var w_`v' "`v' - Winsorized 1–99% within treat_num groups"
	}

gen ln_total_farm_exp=log(w_total_farm_exp) if w_total_farm_exp_exc_fert>0
gen ln_total_farm_exp_exc_fert=log(w_total_farm_exp_exc_fert) if w_total_farm_exp_exc_fert>0

*---------------------------------------------
* Value of production - maize
*---------------------------------------------
* Winzorizing bags_Mcrp
	* Compute percentiles only for maize
	_pctile bags_Mcrp if main_crp == 1, p(1 99)
	local p1  = r(r1)
	local p99 = r(r2)

	* Create new variable only for maize
	gen bags_Mcrp_maiz = bags_Mcrp if main_crp == 1

	* Apply winsorization only among maize
	replace bags_Mcrp_maiz = `p1'  if main_crp == 1 & bags_Mcrp_maiz < `p1'
	replace bags_Mcrp_maiz = `p99' if main_crp == 1 & bags_Mcrp_maiz > `p99'
	rename bags_Mcrp_maiz w_bags_Mcrp_maiz

	* Label
	label var w_bags_Mcrp_maiz "Maize bags (winsorized 1–99% among maize only)"

gen w_value_production = 1050*w_bags_Mcrp_maiz if main_crp==1

gen ln_value_production=log(w_value_production) if main_crp==1

*======================================================
** Farm profits  
*======================================================

gen farm_profits = w_value_production - w_total_farm_exp
gen ln_farm_profits = log(farm_profits)


*---------------------------------------------
* Save cleaned dataset
*---------------------------------------------

* Convert cluster_id from string to numeric
encode cluster_id, gen(cluster_id_num1)	
rename soil_id Barcode

save "$processed/estimation_data_v1", replace
*---------------------------------------------

*Importing the recommendation files 

	import excel using "$raw/Recommendations_Treated_Group_CN.xlsx", first clear

	save "$raw/Recommendations_Treated_Group_CN.dta", replace 

	import excel using "$raw/Recommendations_Control_Group_FES_V2.xlsx", first clear

	save "$raw/Recommendations_Control_Group_FES_V2.dta", replace 
	
*Cleaning and Preparing the IDs from the recommendation files for merging 
	*Treated 
	use "$raw/Recommendations_Treated_Group_CN.dta", clear  
	rename Barcode Barcode_Original //keeping the Barcode variable as a variable to merge with the main data 
	gen Barcode = substr(Barcode_Original, 1, strrpos(Barcode_Original, "-") - 1) //removing the crop code from original Barcode variable
	order Crop YieldTarget Barcode*
	duplicates list Barcode // One duplicate found Barcodes = FWM-ML-51836-16 FWM-ML-51836-96 become 5186 when the crop code is droppped but all other recomndation file values are the same. 
	drop if Barcode_Original == "FWM-ML-51836-96" //dropping one of them - for now the one with Barcode_Original FWM-ML-51836-96
	rename * TR_*
	rename TR_Barcode Barcode 
	isid Barcode
	save "$processed/Recommendations_Treated", replace 
	
	*Control 
	use "$raw/Recommendations_Control_Group_FES_V2.dta", clear  
	duplicates list Barcode //there are two Barcode repeated four times each, two of the four are the same in values of the other vars and the remaining two fo the four are also the same each other in values of the other vars 
	duplicates drop Barcode, force //For now, we are dropping the duplicates keeping one of them. 
	rename * CR_* 
	rename CR_Barcode Barcode 
	isid Barcode
	save "$processed/Recommendations_Control", replace 
	
	*Appending Treated and Control recomndation files 
	use  using "$processed/Recommendations_Treated", clear 
	gen treat_status_from_R = 1
	label var treat_status_from_R "Treatment status based on the recommendation file"
	append using "$processed/Recommendations_Control"
	replace treat_status_from_R=2 if treat_status_from_R==.
	order Barcode treat_status_from_R
	save "$processed/Recommendations", replace 

	

*Merging main and recomndation files 
	use "$processed/estimation_data_v1", clear 
	duplicates tag Barcode , gen(dup)
	list farmer_id Barcode if dup==1 // Duplicates in Barcode in the main data 126 
	list farmer_id Barcode if dup>1 // No Barcode in the main data 98
	merge m:1 Barcode using "$processed/Recommendations"
	rename _merge Merging_Recommendation
	save "$processed/estimation_data_v2", replace 
	


	
	

