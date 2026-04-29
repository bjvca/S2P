version 17
clear all
set more off

* Generate the recommendation-compliance and application-error table.
*
* This script mirrors code/R/13_table8_application_compliance.R. The estimand is
* T2 minus T1 among treated households with valid treatment recommendation
* records. The control-group shadow recommendations are deliberately not used in
* the paper table because they come from a different recommendation pipeline and
* are affected by the lab-calibration concern.
*
* Input:
*   ../endline/data/public/clear_merged_data.csv
*
* Outputs:
*   output/tables/table8_application_compliance_stata.tex
*   output/logs/table8_application_compliance_stata.csv
*   output/logs/table8_application_compliance_exclusions_stata.csv
*
* Run from the replication_package folder:
*   stata -b do code/stata/13_table8_application_compliance.do

local root "`c(pwd)'"
local data_endline "`root'/../endline/data/public/clear_merged_data.csv"
local out_tables "`root'/output/tables"
local out_logs "`root'/output/logs"

capture mkdir "`root'/output"
capture mkdir "`out_tables'"
capture mkdir "`out_logs'"

tempfile results exclusions

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

* Match the fertilizer-use cleaning decision.
drop if inlist(farmer_id, "F_546", "F_387")
local n_after_implausible = _N

foreach v in cluster_id_num hh_size hh_age dist_agro plot_siz seed_typ_num ///
    total_qty_fert total_N total_P total_K tr_n_req {
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

* Clean recommendation product quantities. Blank cells mean zero for that
* product when a valid recommendation record exists.
local recommendation_pairs ///
    `"tr_plantingnpk1414204s2m rec_npk141420"' ///
    `"tr_topdresscalciumammoniumnitra rec_can"' ///
    `"tr_topdresspotassiumsulphate rec_potassium_sulphate"' ///
    `"tr_plantingnpk231056s1zn rec_npk23105"' ///
    `"tr_plantingnpk818156s01b rec_npk81815"' ///
    `"tr_topdresssop rec_sop"' ///
    `"tr_topdressmop rec_mop"' ///
    `"tr_topdressurea rec_urea"' ///
    `"tr_soilcorrectionmaptechnicalg rec_map"' ///
    `"tr_plantingnpk1523166s05zn0 rec_npk152316"'

foreach pair of local recommendation_pairs {
    gettoken source rest : pair
    gettoken target rest : rest
    gen `target' = real(subinstr(lower(trim(`source')), "kg/ha", "", .))
    replace `target' = 0 if missing(`target')
}

* Recommended product-grade nutrients. P and K are P2O5 and K2O because actual
* total_P and total_K are built from fertilizer label grades in those units.
gen rec_N_kgha = ///
    0.14 * rec_npk141420 + ///
    0.26 * rec_can + ///
    0.23 * rec_npk23105 + ///
    0.08 * rec_npk81815 + ///
    0.46 * rec_urea + ///
    0.12 * rec_map + ///
    0.15 * rec_npk152316

gen rec_P2O5_kgha = ///
    0.14 * rec_npk141420 + ///
    0.10 * rec_npk23105 + ///
    0.18 * rec_npk81815 + ///
    0.61 * rec_map + ///
    0.23 * rec_npk152316

gen rec_K2O_kgha = ///
    0.20 * rec_npk141420 + ///
    0.50 * rec_potassium_sulphate + ///
    0.05 * rec_npk23105 + ///
    0.15 * rec_npk81815 + ///
    0.50 * rec_sop + ///
    0.60 * rec_mop + ///
    0.16 * rec_npk152316

gen actual_N_kgha = total_N / plot_siz
gen actual_P2O5_kgha = total_P / plot_siz
gen actual_K2O_kgha = total_K / plot_siz

gen abs_error_N_kgha = abs(actual_N_kgha - rec_N_kgha)
gen abs_error_P2O5_kgha = abs(actual_P2O5_kgha - rec_P2O5_kgha)
gen abs_error_K2O_kgha = abs(actual_K2O_kgha - rec_K2O_kgha)

gen shortfall_N_kgha = max(rec_N_kgha - actual_N_kgha, 0)
gen shortfall_P2O5_kgha = max(rec_P2O5_kgha - actual_P2O5_kgha, 0)
gen shortfall_K2O_kgha = max(rec_K2O_kgha - actual_K2O_kgha, 0)

gen valid_application = !missing(TR_N_Req) & !missing(plot_siz) & plot_siz > 0 ///
    & !missing(total_qty_fert) & !missing(actual_N_kgha) ///
    & !missing(actual_P2O5_kgha) & !missing(actual_K2O_kgha)

tempname ex_handle
postfile `ex_handle' str60 reason double n using "`exclusions'", replace
post `ex_handle' ("Treatment-arm observations before exclusions") (`n_start')
post `ex_handle' ("Dropped implausible fertilizer records: F_546 and F_387") (2)
quietly count if missing(TR_N_Req)
post `ex_handle' ("Missing treatment recommendation record") (r(N))
quietly count if missing(plot_siz) | plot_siz <= 0
post `ex_handle' ("Missing/sentinel plot size") (r(N))
quietly count if missing(total_qty_fert)
post `ex_handle' ("Missing/sentinel total fertilizer quantity") (r(N))
quietly count if valid_application
post `ex_handle' ("Usable recommendation-compliance sample") (r(N))
postclose `ex_handle'

local controls hh_size hh_age i.hh_educ_cat dist_agro plot_siz ///
    i.slope_cat i.soil_str_cat i.seed_typ_num

tempname handle
postfile `handle' str30 outcome str45 label str3 controls double n clusters ///
    t1_mean t2_mean t2_minus_t1 t2_minus_t1_se t2_minus_t1_p using "`results'", replace

local outcomes rec_N_kgha actual_N_kgha abs_error_N_kgha shortfall_N_kgha ///
    rec_P2O5_kgha actual_P2O5_kgha abs_error_P2O5_kgha shortfall_P2O5_kgha ///
    rec_K2O_kgha actual_K2O_kgha abs_error_K2O_kgha shortfall_K2O_kgha

local label1 "Recommended N"
local label2 "Actual N applied"
local label3 "Absolute N application error"
local label4 "N shortfall"
local label5 "Recommended P$_2$O$_5$"
local label6 "Actual P$_2$O$_5$ applied"
local label7 "Absolute P$_2$O$_5$ application error"
local label8 "P$_2$O$_5$ shortfall"
local label9 "Recommended K$_2$O"
local label10 "Actual K$_2$O applied"
local label11 "Absolute K$_2$O application error"
local label12 "K$_2$O shortfall"

local i = 1
foreach y of local outcomes {
    local label "`label`i''"

    quietly regress `y' treat_t2 if valid_application, vce(cluster cluster_id)
    post_spec `y', label("`label'") controls("No") handle(`handle')

    quietly regress `y' treat_t2 `controls' if valid_application, vce(cluster cluster_id)
    post_spec `y', label("`label'") controls("Yes") handle(`handle')

    local ++i
}

postclose `handle'

use "`results'", clear
export delimited using "`out_logs'/table8_application_compliance_stata.csv", replace

preserve
    keep if controls == "Yes"

    gen str16 t1_mean_fmt = cond(missing(t1_mean), "", strtrim(string(t1_mean, "%9.2f")))
    gen str16 coef_fmt = cond(missing(t2_minus_t1), "", strtrim(string(t2_minus_t1, "%9.2f")))
    gen str16 se_fmt = cond(missing(t2_minus_t1_se), "", strtrim(string(t2_minus_t1_se, "%9.2f")))
    gen str16 n_fmt = cond(missing(n), "", strtrim(string(n, "%9.0f")))

    gen str12 star = ""
    replace star = "\\sym{***}" if t2_minus_t1_p < 0.01
    replace star = "\\sym{**}" if t2_minus_t1_p >= 0.01 & t2_minus_t1_p < 0.05
    replace star = "\\sym{*}" if t2_minus_t1_p >= 0.05 & t2_minus_t1_p < 0.10

    gen str28 coef_cell = coef_fmt + star

    file open tex using "`out_tables'/table8_application_compliance_stata.tex", write replace
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

use "`exclusions'", clear
export delimited using "`out_logs'/table8_application_compliance_exclusions_stata.csv", replace
