version 17
clear all
set more off

* Generate the product-level recommendation-compliance table.
*
* This script mirrors code/R/14_table9_product_compliance.R. Product
* compliance is defined as applying the specific product or product group
* included in the treatment recommendation. The estimand is T2 minus T1 among
* treated households with valid treatment recommendation records.
*
* Run from the replication_package folder:
*   stata -b do code/stata/14_table9_product_compliance.do

local root "`c(pwd)'"
local data_endline "`root'/../endline/data/public/clear_merged_data.csv"
local out_tables "`root'/output/tables"
local out_logs "`root'/output/logs"

capture mkdir "`root'/output"
capture mkdir "`out_tables'"
capture mkdir "`out_logs'"

tempfile results exclusions prod_diag

program define post_spec
    syntax varname, Label(string) Controls(string) Handle(name)

    local n = e(N)
    local clusters = e(N_clust)

    quietly summarize `varlist' if e(sample) & treat == "T1"
    local t1_mean = r(mean)

    quietly summarize `varlist' if e(sample) & treat == "T2"
    local t2_mean = r(mean)

    local coef = _b[treat_t2]
    local se = _se[treat_t2]

    if missing(`se') | `se' == 0 {
        local p = .
    }
    else {
        local p = 2 * ttail(e(df_r), abs(`coef' / `se'))
    }

    post `handle' ("`varlist'") ("`label'") ("`controls'") (`n') (`clusters') ///
        (`t1_mean') (`t2_mean') (`coef') (`se') (`p')
end

import delimited using "`data_endline'", clear varnames(1) stringcols(_all)
keep if inlist(treat, "T1", "T2")

local n_start = _N
drop if inlist(farmer_id, "F_546", "F_387")
local n_dropped = `n_start' - _N

foreach v in cluster_id_num hh_size hh_age dist_agro plot_siz seed_typ_num ///
    total_qty_fert tr_n_req {
    capture destring `v', replace force
}

rename cluster_id_num cluster_id
rename tr_n_req TR_N_Req

replace dist_agro = . if dist_agro == 999
replace plot_siz = . if plot_siz == 999
replace total_qty_fert = . if total_qty_fert == 999
replace hh_educ = "" if trim(hh_educ) == ""
replace slope = "" if trim(slope) == ""
replace soil_str = "" if trim(soil_str) == ""
replace soil_str = "Other/unknown" if soil_str == "5"

encode hh_educ, gen(hh_educ_cat)
encode slope, gen(slope_cat)
encode soil_str, gen(soil_str_cat)

gen treat_t2 = treat == "T2"

local recommendation_pairs ///
    `"tr_plantingnpk231056s1zn rec_npk23105"' ///
    `"tr_topdressurea rec_urea"' ///
    `"tr_topdresscalciumammoniumnitra rec_can"' ///
    `"tr_plantingnpk1414204s2m rec_npk141420"' ///
    `"tr_plantingnpk818156s01b rec_npk81815"' ///
    `"tr_plantingnpk1523166s05zn0 rec_npk152316"' ///
    `"tr_soilcorrectionmaptechnicalg rec_map"' ///
    `"tr_topdressmop rec_mop"' ///
    `"tr_topdresssop rec_sop"' ///
    `"tr_topdresspotassiumsulphate rec_potassium_sulphate"' ///
    `"tr_soilcorrectioncalciticlime rec_calcitic_lime"' ///
    `"tr_soilcorrectiondolomiticlime rec_dolomitic_lime"'

foreach pair of local recommendation_pairs {
    gettoken source rest : pair
    gettoken target rest : rest
    gen `target'_amt = real(subinstr(lower(trim(`source')), "kg/ha", "", .))
    gen `target' = `target'_amt > 0 if !missing(`target'_amt)
    replace `target' = 0 if missing(`target')
}

gen rec_potassium_product = rec_mop | rec_sop | rec_potassium_sulphate
gen rec_lime = rec_calcitic_lime | rec_dolomitic_lime

gen act_npk23105 = trim(test_plotfertilizer_listnpk23105) == "Yes"
gen act_urea = trim(test_plotfertilizer_listurea) == "Yes"
gen act_can = trim(test_plotfertilizer_listcan) == "Yes"
gen act_npk141420 = trim(test_plotfertilizer_listnpk14142) == "Yes"
gen act_npk81815 = trim(test_plotfertilizer_listnpk81815) == "Yes"
gen act_npk152316 = trim(test_plotfertilizer_listnpk15231) == "Yes"
gen act_map = trim(test_plotfertilizer_listmap) == "Yes"
gen act_potassium_product = trim(test_plotfertilizer_listmop) == "Yes" ///
    | trim(test_plotfertilizer_listsop) == "Yes" ///
    | trim(test_plotfertilizer_listpotassiu) == "Yes"
gen act_lime = trim(test_plotfertilizer_listcalcitic) == "Yes" ///
    | trim(test_plotfertilizer_listdolomiti) == "Yes"

local products npk23105 urea can npk141420 npk81815 potassium_product lime map

egen rec_product_count = rowtotal(rec_npk23105 rec_urea rec_can rec_npk141420 ///
    rec_npk81815 rec_potassium_product rec_lime rec_map)

gen valid_product = !missing(TR_N_Req) & !missing(total_qty_fert) & rec_product_count > 0

local applied_terms
local missed_terms
local exact_terms
foreach p of local products {
    gen rec_and_act_`p' = rec_`p' & act_`p'
    gen rec_and_not_act_`p' = rec_`p' & !act_`p'
    gen rec_diff_act_`p' = rec_`p' != act_`p'
    gen applied_if_rec_`p' = .
    replace applied_if_rec_`p' = act_`p' if rec_`p'

    local applied_terms `applied_terms' rec_and_act_`p'
    local missed_terms `missed_terms' rec_and_not_act_`p'
    local exact_terms `exact_terms' rec_diff_act_`p'
}

egen recommended_products_applied_count = rowtotal(`applied_terms')
egen recommended_products_missed_count = rowtotal(`missed_terms')
egen product_set_difference_count = rowtotal(`exact_terms')

gen any_recommended_product_applied = recommended_products_applied_count > 0
gen share_recommended_products_applied = recommended_products_applied_count / rec_product_count
gen all_recommended_products_applied = recommended_products_missed_count == 0
gen exact_recommended_product_bundle = product_set_difference_count == 0

tempname ex_handle
postfile `ex_handle' str70 reason double n using "`exclusions'", replace
post `ex_handle' ("Treatment-arm observations before exclusions") (`n_start')
post `ex_handle' ("Dropped implausible fertilizer records: F_546 and F_387") (`n_dropped')
quietly count if missing(TR_N_Req)
post `ex_handle' ("Missing treatment recommendation record") (r(N))
quietly count if missing(total_qty_fert)
post `ex_handle' ("Missing/sentinel total fertilizer quantity") (r(N))
quietly count if !missing(TR_N_Req) & rec_product_count == 0
post `ex_handle' ("No positive product recommendation in parsed fields") (r(N))
quietly count if valid_product
post `ex_handle' ("Usable product-compliance sample") (r(N))
postclose `ex_handle'

local controls hh_size hh_age i.hh_educ_cat dist_agro plot_siz ///
    i.slope_cat i.soil_str_cat i.seed_typ_num

tempname handle
postfile `handle' str45 outcome str55 label str3 controls double n clusters ///
    t1_mean t2_mean t2_minus_t1 t2_minus_t1_se t2_minus_t1_p using "`results'", replace

local outcomes any_recommended_product_applied share_recommended_products_applied ///
    all_recommended_products_applied exact_recommended_product_bundle ///
    applied_if_rec_npk23105 applied_if_rec_urea applied_if_rec_can ///
    applied_if_rec_npk141420 applied_if_rec_npk81815 ///
    applied_if_rec_potassium_product applied_if_rec_lime applied_if_rec_map

local label1 "Any recommended product applied"
local label2 "Share of recommended products applied"
local label3 "All recommended products applied"
local label4 "Exact recommended product bundle applied"
local label5 "Recommended NPK 23:10:5 applied"
local label6 "Recommended urea applied"
local label7 "Recommended CAN applied"
local label8 "Recommended NPK 14:14:20 applied"
local label9 "Recommended NPK 8:18:15 applied"
local label10 "Recommended potassium product applied"
local label11 "Recommended lime applied"
local label12 "Recommended MAP applied"

local i = 1
foreach y of local outcomes {
    local label "`label`i''"

    quietly regress `y' treat_t2 if valid_product, vce(cluster cluster_id)
    post_spec `y', label("`label'") controls("No") handle(`handle')

    quietly regress `y' treat_t2 `controls' if valid_product, vce(cluster cluster_id)
    post_spec `y', label("`label'") controls("Yes") handle(`handle')

    local ++i
}

postclose `handle'

use "`results'", clear
export delimited using "`out_logs'/table9_product_compliance_stata.csv", replace

preserve
    keep if controls == "Yes"

    gen str16 t1_mean_fmt = cond(missing(t1_mean), "", strtrim(string(t1_mean, "%9.3f")))
    gen str16 coef_fmt = cond(missing(t2_minus_t1), "", strtrim(string(t2_minus_t1, "%9.3f")))
    gen str16 se_fmt = cond(missing(t2_minus_t1_se), "", strtrim(string(t2_minus_t1_se, "%9.3f")))
    gen str16 n_fmt = cond(missing(n), "", strtrim(string(n, "%9.0f")))

    gen str12 star = ""
    replace star = "\\sym{***}" if t2_minus_t1_p < 0.01
    replace star = "\\sym{**}" if t2_minus_t1_p >= 0.01 & t2_minus_t1_p < 0.05
    replace star = "\\sym{*}" if t2_minus_t1_p >= 0.05 & t2_minus_t1_p < 0.10

    gen str28 coef_cell = coef_fmt + star

    file open tex using "`out_tables'/table9_product_compliance_stata.tex", write replace
    file write tex "{" _n
    file write tex "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" _n
    file write tex "\begin{tabular}{lccc}" _n
    file write tex "\toprule" _n
    file write tex "Outcome & T1 mean & T2 $-$ T1 & N \\" _n
    file write tex "\midrule" _n
    forvalues j = 1/`=_N' {
        file write tex "`=label[`j']' & `=t1_mean_fmt[`j']' & `=coef_cell[`j']' & `=n_fmt[`j']' \\" _n
        file write tex "& & (`=se_fmt[`j']') & \\" _n
    }
    file write tex "\bottomrule" _n
    file write tex "\end{tabular}" _n
    file write tex "}" _n
    file close tex
restore

tempname diag_handle
postfile `diag_handle' str30 product str55 label double recommended_n applied_n ///
    applied_rate t1_recommended_n t1_applied_rate t2_recommended_n t2_applied_rate ///
    using "`prod_diag'", replace

local dlabel1 "Recommended NPK 23:10:5 applied"
local dlabel2 "Recommended urea applied"
local dlabel3 "Recommended CAN applied"
local dlabel4 "Recommended NPK 14:14:20 applied"
local dlabel5 "Recommended NPK 8:18:15 applied"
local dlabel6 "Recommended potassium product applied"
local dlabel7 "Recommended lime applied"
local dlabel8 "Recommended MAP applied"

local i = 1
foreach p of local products {
    local label "`dlabel`i''"
    quietly count if valid_product & rec_`p'
    local recommended_n = r(N)
    quietly count if valid_product & rec_`p' & act_`p'
    local applied_n = r(N)
    quietly summarize act_`p' if valid_product & rec_`p'
    local applied_rate = r(mean)
    quietly count if valid_product & rec_`p' & treat == "T1"
    local t1_recommended_n = r(N)
    quietly summarize act_`p' if valid_product & rec_`p' & treat == "T1"
    local t1_applied_rate = r(mean)
    quietly count if valid_product & rec_`p' & treat == "T2"
    local t2_recommended_n = r(N)
    quietly summarize act_`p' if valid_product & rec_`p' & treat == "T2"
    local t2_applied_rate = r(mean)

    post `diag_handle' ("`p'") ("`label'") (`recommended_n') (`applied_n') ///
        (`applied_rate') (`t1_recommended_n') (`t1_applied_rate') ///
        (`t2_recommended_n') (`t2_applied_rate')
    local ++i
}
postclose `diag_handle'

use "`prod_diag'", clear
export delimited using "`out_logs'/table9_product_compliance_product_diagnostics_stata.csv", replace

use "`exclusions'", clear
export delimited using "`out_logs'/table9_product_compliance_exclusions_stata.csv", replace
