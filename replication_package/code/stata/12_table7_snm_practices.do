version 17
clear all
set more off

* Generate the soil nutrient management (SNM) practice table.
*
* This script mirrors code/R/12_table7_snm_practices.R. It replaces the old
* probit/esttab SNM output with linear probability ITT estimates so the table
* reports interpretable percentage-point effects.
*
* Input:
*   ../endline/data/public/clear_merged_data.csv
*
* Outputs:
*   output/tables/table7_snm_practices_stata.tex
*   output/logs/table7_snm_practices_stata.csv
*   output/logs/table7_snm_response_diagnostics_stata.csv
*
* Run from the replication_package folder:
*   stata -b do code/stata/12_table7_snm_practices.do

local root "`c(pwd)'"
local data_endline "`root'/../endline/data/public/clear_merged_data.csv"
local out_tables "`root'/output/tables"
local out_logs "`root'/output/logs"

capture mkdir "`root'/output"
capture mkdir "`out_tables'"
capture mkdir "`out_logs'"

tempfile results response_diag

program define post_spec
    syntax varname, Label(string) Controls(string) Handle(name)

    local n = e(N)
    local clusters = e(N_clust)

    quietly summarize `varlist' if e(sample) & treat == "C"
    local control_mean = r(mean)

    local t1_coef = _b[2.treat_num]
    local t1_se = _se[2.treat_num]
    local t2_coef = _b[3.treat_num]
    local t2_se = _se[3.treat_num]

    if missing(`t1_se') | `t1_se' == 0 {
        local t1_p = .
    }
    else {
        local t1_p = 2 * ttail(e(df_r), abs(`t1_coef' / `t1_se'))
    }

    if missing(`t2_se') | `t2_se' == 0 {
        local t2_p = .
    }
    else {
        local t2_p = 2 * ttail(e(df_r), abs(`t2_coef' / `t2_se'))
    }

    quietly test 2.treat_num = 3.treat_num
    local p_equal = r(p)

    post `handle' ("`varlist'") ("`label'") ("`controls'") (`n') (`clusters') ///
        (`control_mean') (`t1_coef') (`t1_se') (`t1_p') ///
        (`t2_coef') (`t2_se') (`t2_p') (`p_equal')
end

import delimited using "`data_endline'", clear varnames(1) stringcols(_all)
keep if inlist(treat, "C", "T1", "T2")

foreach v in treat_num cluster_id_num hh_size hh_age dist_agro plot_siz seed_typ_num {
    destring `v', replace force
}

rename cluster_id_num cluster_id

replace dist_agro = . if dist_agro == 999
replace plot_siz = . if plot_siz == 999
replace hh_age = . if hh_age == 999
replace hh_educ = "" if trim(hh_educ) == ""
replace slope = "" if trim(slope) == ""
replace soil_str = "" if trim(soil_str) == ""
replace soil_str = "Other/unknown" if soil_str == "5"

encode hh_educ, gen(hh_educ_cat)
encode slope, gen(slope_cat)
encode soil_str, gen(soil_str_cat)

local outcomes green_inco fresh_app mat_farm_app dairy_app comp_app ///
    mbeya_app till_app ridge_use pit_use
local label1 "Green legume incorporation"
local label2 "Fresh vegetative material"
local label3 "Farmyard manure"
local label4 "Dairy or poultry manure"
local label5 "Compost"
local label6 "Mbeya fertilizer"
local label7 "Minimum tillage"
local label8 "Ridges/check dams"
local label9 "Pit planting"

foreach y of local outcomes {
    gen `y'_bin = .
    replace `y'_bin = 1 if trim(`y') == "Yes"
    replace `y'_bin = 0 if trim(`y') == "No"
}

local controls hh_size hh_age i.hh_educ_cat dist_agro plot_siz ///
    i.slope_cat i.soil_str_cat i.seed_typ_num

tempname handle
postfile `handle' str30 outcome str40 label str3 controls double n clusters ///
    control_mean t1_coef t1_se t1_p t2_coef t2_se t2_p p_equal using "`results'", replace

local i = 1
foreach y of local outcomes {
    local label "`label`i''"

    quietly regress `y'_bin i.treat_num, vce(cluster cluster_id)
    post_spec `y'_bin, label("`label'") controls("No") handle(`handle')

    quietly regress `y'_bin i.treat_num `controls', vce(cluster cluster_id)
    post_spec `y'_bin, label("`label'") controls("Yes") handle(`handle')

    local ++i
}

postclose `handle'

use "`results'", clear
export delimited using "`out_logs'/table7_snm_practices_stata.csv", replace

preserve
    keep if controls == "Yes"

    gen str16 control_fmt = cond(missing(control_mean), "", strtrim(string(control_mean, "%9.3f")))
    gen str16 t1_coef_fmt = cond(missing(t1_coef), "", strtrim(string(t1_coef, "%9.3f")))
    gen str16 t2_coef_fmt = cond(missing(t2_coef), "", strtrim(string(t2_coef, "%9.3f")))
    gen str16 t1_se_fmt = cond(missing(t1_se), "", strtrim(string(t1_se, "%9.3f")))
    gen str16 t2_se_fmt = cond(missing(t2_se), "", strtrim(string(t2_se, "%9.3f")))
    gen str16 p_equal_fmt = cond(missing(p_equal), "", strtrim(string(p_equal, "%9.3f")))
    gen str16 n_fmt = cond(missing(n), "", strtrim(string(n, "%9.0f")))

    gen str12 t1_star = ""
    replace t1_star = "\\sym{***}" if t1_p < 0.01
    replace t1_star = "\\sym{**}" if t1_p >= 0.01 & t1_p < 0.05
    replace t1_star = "\\sym{*}" if t1_p >= 0.05 & t1_p < 0.10

    gen str12 t2_star = ""
    replace t2_star = "\\sym{***}" if t2_p < 0.01
    replace t2_star = "\\sym{**}" if t2_p >= 0.01 & t2_p < 0.05
    replace t2_star = "\\sym{*}" if t2_p >= 0.05 & t2_p < 0.10

    gen str28 t1_cell = t1_coef_fmt + t1_star
    gen str28 t2_cell = t2_coef_fmt + t2_star

    file open tex using "`out_tables'/table7_snm_practices_stata.tex", write replace
    file write tex "{" _n
    file write tex "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" _n
    file write tex "\begin{tabular}{lccccc}" _n
    file write tex "\toprule" _n
    file write tex "Outcome & Control mean & T1 $-$ Control & T2 $-$ Control & p-value: T2 = T1 & N \\" _n
    file write tex "\midrule" _n
    forvalues j = 1/`=_N' {
        file write tex "`=label[`j']' & `=control_fmt[`j']' & `=t1_cell[`j']' & `=t2_cell[`j']' & `=p_equal_fmt[`j']' & `=n_fmt[`j']' \\" _n
        file write tex "& & (`=t1_se_fmt[`j']') & (`=t2_se_fmt[`j']') & & \\" _n
    }
    file write tex "\bottomrule" _n
    file write tex "\end{tabular}" _n
    file write tex "}" _n
    file close tex
restore

import delimited using "`data_endline'", clear varnames(1) stringcols(_all)
keep if inlist(treat, "C", "T1", "T2")

tempname diag_handle
postfile `diag_handle' str30 outcome str40 label double n_yes n_no n_blank using "`response_diag'", replace

local i = 1
foreach y of local outcomes {
    local label "`label`i''"

    quietly count if trim(`y') == "Yes"
    local n_yes = r(N)
    quietly count if trim(`y') == "No"
    local n_no = r(N)
    quietly count if trim(`y') == ""
    local n_blank = r(N)

    post `diag_handle' ("`y'") ("`label'") (`n_yes') (`n_no') (`n_blank')
    local ++i
}

postclose `diag_handle'

use "`response_diag'", clear
export delimited using "`out_logs'/table7_snm_response_diagnostics_stata.csv", replace
