
{/******************************************************************************
						Title; Space to Place Project in Malawi
						Purpose: Clean and prepare the dataset for analysis 
********************************************************************************/
	}
*Run master do file
do Master.do 

clear
import delimited using "$raw\endline.csv", ///
    clear stringcols(_all) varnames(1) case(lower)
	

*---------------------------------------------
* Label household variables
*---------------------------------------------
la var hh_size "Household size"
la var hh_age "Age of household head"
la var hh_educ "Education level of head"
la var hh_gender "Gender of household head"
la var ttl_area "Land area last season (acres)"
la var ttl_area_t_1 "Land area previous season (acres)"
la var aip_rec "Received AIP last season"
la var aip_rec_t_1 "Received AIP previous season"
la var feed_diff "Difficulty feeding family"
la var dist_agro "Time to nearest agro-dealer (min)"
la var is_fw "Nearest shop Farmers World?"
la var dist_fws "Time to Farmers World shop (min)"
la var rem_test "Soil sample taken last year?"
la var get_rec "Received fertilizer recommendations?"
la var ease_rec "Ease of understanding recommendations"
la var folll_rec "Followed recommendations?"
la var got_voucher "Received voucher for fertilizer"
la var buy_rec "Bought recommended fertilizer"
la var redeem "Redeemed voucher at Farmers World"
la var reco_av "Recommended fertilizer available?"
la var buy_other "Bought different fertilizer?"
la var vou_en "Voucher value enough?"
la var agro_vis "Visited by Agronaut (FW extension)"
la var times_vis "Number of Agronaut visits"
la var ext_srv "Got extension from other providers?"
la var other_ext "Other extension provider(s)"
la var times_ext "Number of visits by other providers"

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
* Label plot variables
*---------------------------------------------
la var plot_siz "Size of plot (acres)"
la var slope "Slope of plot"
la var soil_str "Soil type/texture"
la var dist_plot "Time from household to plot (min)"
la var main_crp "Main crop planted"
la var int_crpp "Plot inter-cropped?"
la var crp_cropp "Inter-crop(s) planted"
la var Perc_main "% area allocated to main crop"
la var seed_typ "Type of seed/planting material"
la var seed_bag "Seed in sealed bag with tag?"
la var seed_var "Seed/variety name"
la var seed_sourc "Source of seed"
la var seed_rec_times "Times this seed used"
la var qty_used "Quantity of seed planted (kg)"
la var price_seed "Total amount paid for seed"
la var seed_ob_AIP "Seed obtained via AIP?"
la var obt_AIP "Quantity of seed from AIP (kg)"
la var pay_meth "Payment method for seed"
la var satis_qual "Satisfaction with seed quality"
la var satis_yield "Satisfaction with yield"
la var satis_dis "Satisfaction with disease resistance"
la var satis_drought "Satisfaction with drought tolerance"
la var satis_avail "Satisfaction with seed availability/access"
la var weed_time "Times weeded plot"
la var pest_use "Used pesticides/herbicides/fungicides?"
la var ttl_cost_pest "Total cost of pesticides (MWK)"
la var green_inco "Early green manure incorporation?"
la var crp_prec "Crop grown previous season"
la var fresh_app "Applied fresh vegetative material?"
la var mat_farm_app "Applied farmyard manure?"
la var dairy_app "Applied dairy/poultry manure?"
la var comp_app "Applied compost?"
la var mbeya_app "Applied Mbeya fertilizer?"
la var till_app "Applied minimum tillage?"
la var ridge_use "Used ridges/check dams/soil protection?"
la var pit_use "Used pit planting techniques?"
la var bags_Mcrp "Harvested main crop (kg)"
la var main_sell "Sold any harvested main crop?"
la var bags_sell "Quantity sold of main crop (kg)"
la var prc_main "Price of main crop sold (per kg)"
la var other_exp "Other plot expenses (labour, irrigation, etc)"
la var yield_fert "Preference: short-term yield vs. long-term soil fertility"
la var org_amen   "Prefer organic soil amendments if same price"
la var pay_hgh    "WTP above 190,000 MK for fertilizer that improves soil health"
la var gen_fert   "WTP above 150,000 MK/acre for customized vs. standard blend"
la var sub_form   "Preferred subsidy format"
la var spend      "Allocation of extra MK 150,000 (rank spending categories)"
la var seas_red   "Willingness to reduce/stop chemical fertilizer use for soil fertility"
la var rank_imp   "Rank importance of factors for farm productivity"
la var trade_yld  "Accept 10% lower yield now for 15% higher yields over 3 years"
la var w_pref     "Preference choice"
la var rel_acc    "Prefer subsidized fertilizer vs. guaranteed extension services"



*---------------------------------------------
* Fertilizer use cleaning (per type)
*---------------------------------------------
foreach v in map dap urea can npk14142 npk23105 npk81815 npk15231 sop mop calcitic dolomiti potassiu other {
   gen fert_`v' = .
    replace fert_`v' = 1 if test_plotfertilizer_list`v' == "True"
    replace fert_`v' = 0 if test_plotfertilizer_list`v' == "False"
	replace fert_`v' = . if test_plotfertilizer_list`v' == "n/a"
    label define yesno 0 "No" 1 "Yes", replace
    label values fert_`v' yesno
    la var fert_`v' "Uses fertilizer type: `v'"
}

gen repeat_count = real(test_plotfertilizer_repeat_count)
label define fertype 1 "UREA" 2 "NPK23:10:5" 3 "CAN" 4 "NPK8:18:15" 5 "Other"

*---------------------------------------------
* Repeat fertilizer groups
*---------------------------------------------
forvalues r = 1/4 {
    
    * Fertilizer type variable (string → numeric with labels)
    gen ferti`r' = .
    replace ferti`r' = 1 if test_plotfertilizer_repeat`r'ferti == "UREA"
    replace ferti`r' = 2 if test_plotfertilizer_repeat`r'ferti == "NPK23:10:5"
    replace ferti`r' = 3 if test_plotfertilizer_repeat`r'ferti == "CAN"
    replace ferti`r' = 4 if test_plotfertilizer_repeat`r'ferti == "NPK8:18:15"
    replace ferti`r' = 5 if lower(test_plotfertilizer_repeat`r'ferti) == "other"
    label values ferti`r' fertype
    label var ferti`r' "Fertilizer type, repeat `r'"
    
    * figure out starting column index for this repeat
    local start = cond(`r'==1,130, cond(`r'==2,141, cond(`r'==3,152,163)))
    
    * Create numeric versions of the variables
    gen fert_time`r'    = real(v`start')
    gen app_method`r'   = real(v`=`start'+1')
    gen weather`r'      = real(v`=`start'+2')
    gen qty_fert`r'     = real(v`=`start'+3')
    gen fert_cost`r'    = real(v`=`start'+4')
    
    * Binary Yes/No for ob_AIP
    gen ob_AIP`r' = .
    replace ob_AIP`r' = 1 if inlist(lower(v`=`start'+5'), "yes","true","1")
    replace ob_AIP`r' = 0 if inlist(lower(v`=`start'+5'), "no","false","0")
    label values ob_AIP`r' yesno
    
    gen qty_fert_AIP`r' = real(v`=`start'+6')
    
    * Binary Yes/No for fert_us
    gen fert_us`r' = .
    replace fert_us`r' = 1 if inlist(lower(v`=`start'+7'), "yes","true","1")
    replace fert_us`r' = 0 if inlist(lower(v`=`start'+7'), "no","false","0")
    label values fert_us`r' yesno
    
    * Numeric for your_qunt_us
    gen your_qunt_us`r' = real(v`=`start'+8')
    
    * Binary Yes/No for fert_sat
    gen fert_sat`r' = .
    replace fert_sat`r' = 1 if inlist(lower(v`=`start'+9'), "yes","true","1")
    replace fert_sat`r' = 0 if inlist(lower(v`=`start'+9'), "no","false","0")
    label values fert_sat`r' yesno
	
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
* Fertilizer nutrients
*---------------------------------------------
capture drop N1 N2 N3 N4 P1 P2 P3 P4 K1 K2 K3 K4 total_N total_P total_K total_nutrient
forvalues r = 1/4 {
    gen N`r' = .
    gen P`r' = .
    gen K`r' = .

    * UREA
    replace N`r' = qty_fert`r' * 0.46  if ferti`r'==1
    replace P`r' = qty_fert`r' * 0     if ferti`r'==1
    replace K`r' = qty_fert`r' * 0     if ferti`r'==1

    * NPK23:10:5
    replace N`r' = qty_fert`r' * 0.23  if ferti`r'==2
    replace P`r' = qty_fert`r' * 0.10  if ferti`r'==2
    replace K`r' = qty_fert`r' * 0.05  if ferti`r'==2

    * CAN
    replace N`r' = qty_fert`r' * 0.27  if ferti`r'==3
    replace P`r' = qty_fert`r' * 0     if ferti`r'==3
    replace K`r' = qty_fert`r' * 0     if ferti`r'==3

    * NPK8:18:15
    replace N`r' = qty_fert`r' * 0.08  if ferti`r'==4
    replace P`r' = qty_fert`r' * 0.18  if ferti`r'==4
    replace K`r' = qty_fert`r' * 0.15  if ferti`r'==4

    * Other (leave missing)
    replace N`r' = .  if ferti`r'==5
    replace P`r' = .  if ferti`r'==5
    replace K`r' = .  if ferti`r'==5
}

egen total_N = rowtotal(N1 N2 N3 N4)
egen total_P = rowtotal(P1 P2 P3 P4)
egen total_K = rowtotal(K1 K2 K3 K4)
gen total_nutrient = total_N + total_P + total_K

label var total_N "Total Nitrogen applied (kg)"
label var total_P "Total Phosphorus applied (kg)"
label var total_K "Total Potassium applied (kg)"
label var total_nutrient "Total nutrient applied (N+P+K) in kg"

sum total_N total_P total_K total_nutrient

*---------------------------------------------
* Treatment variable
*---------------------------------------------
encode treat, gen(treat_num) 
label list treat_num


*--------------------------------------------- 
* Clean/convert key variables
*--------------------------------------------- 
local varlist_used hh_size hh_age hh_educ hh_gender feed_diff dist_agro ///
                   get_rec ease_rec buy_rec buy_other vou_en agro_vis ext_srv times_ext ///
                   plot_siz slope soil_str seed_typ folll_rec got_voucher vou_en dist_plot main_crp int_crpp Perc_main qty_used bags_Mcrp

foreach v of local varlist_used {
    capture confirm string variable `v'
    if !_rc {
        replace `v' = lower(trim(`v'))

        *-------------------------
        * Special cases
        *-------------------------
        if "`v'" == "hh_gender" {
            gen `v'_num = .
            replace `v'_num = 1 if inlist(`v',"male","1")
            replace `v'_num = 0 if inlist(`v',"female","0")
            drop `v'
            rename `v'_num `v'
            label define hh_gender 1 "Male" 0 "Female"
            label values `v' hh_gender
        }

        else if ("`v'" == "seed_typ") {
            gen seed_typ_num = .
            replace seed_typ_num = 1 if strpos(`v',"improved_ reused")>0
            replace seed_typ_num = 2 if strpos(`v',"improved_newly")>0
            replace seed_typ_num = 3 if strpos(`v',"indigenous")>0 | strpos(`v',"local")>0
            replace seed_typ_num = 4 if inlist(`v',"n/a","na","missing")
            replace seed_typ_num = 5 if strpos(`v',"unknown")>0
            drop `v'
            rename seed_typ_num seed_typ
            label define seed_typ 1 "Improved reused" 2 "Improved newly acquired" ///
                                 3 "Indigenous/Local" 4 "n/a" 5 "Unknown"
            label values seed_typ seed_typ
        }

        else if "`v'" == "main_crp" {
            encode `v', gen(`v'_num)
            drop `v'
            rename `v'_num `v'
        }

        *-------------------------
        * General yes/no/numeric cleanup
        *-------------------------
        else {
            gen `v'_num = .
            replace `v'_num = 1 if `v' == "yes"
            replace `v'_num = 0 if `v' == "no"
            replace `v'_num = . if inlist(`v',"n/a","na","missing","")
            replace `v'_num = real(`v') if missing(`v'_num) & regexm(`v',"^[0-9.]+$")
            drop `v'
            rename `v'_num `v'
        }
    }
}

*---------------------------------------------
* Maize yield calculations
*---------------------------------------------
gen maize_area = . 
replace maize_area = plot_siz if main_crp==5 & int_crpp==0
replace maize_area = plot_siz * Perc_main/100 if main_crp==5 & int_crpp==1

gen yield_maize = . 
replace yield_maize = bags_Mcrp / maize_area if maize_area > 0 & main_crp==5
label variable yield_maize "Maize yield (kg/acre)"

gen lnyield = . 
replace lnyield = log(yield_maize) if yield_maize > 0
label variable lnyield "Log of maize yield (kg/acre)"

*---------------------------------------------
* Save cleaned dataset
*---------------------------------------------
save "$processed/estimation_data", replace
*---------------------------------------------

