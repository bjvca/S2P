version 17
clear all
set more off

* Audit farm expenditure and profit outcomes from the public endline file.
*
* This is a diagnostic script, not yet a manuscript-table replacement. It
* mirrors code/R/10_expenditure_profit_audit.R and documents whether the profit
* conclusions depend on dropping zero or negative profit observations.
*
* Input:
*   ../endline/data/public/clear_merged_data.csv
*
* Outputs:
*   output/tables/expenditure_profit_audit_stata.tex
*   output/logs/expenditure_profit_audit_stata.csv
*   output/logs/expenditure_profit_sample_diagnostics_stata.csv
*
* Run from the replication_package folder:
*   stata -b do code/stata/10_expenditure_profit_audit.do

local root "`c(pwd)'"
local data_endline "`root'/../endline/data/public/clear_merged_data.csv"
local out_tables "`root'/output/tables"
local out_logs "`root'/output/logs"

capture mkdir "`root'/output"
capture mkdir "`out_tables'"
capture mkdir "`out_logs'"

tempfile results diagnostics

program define post_spec
    syntax varname, Controls(string) Handle(name)

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

    post `handle' ("`varlist'") ("`controls'") (`n') (`clusters') ///
        (`control_mean') (`t1_coef') (`t1_se') (`t1_p') ///
        (`t2_coef') (`t2_se') (`t2_p') (`p_equal')
end

import delimited using "`data_endline'", clear varnames(1) stringcols(_all)
keep if inlist(treat, "C", "T1", "T2") & main_crp == "MAIZE"

foreach v in treat_num cluster_id_num ln_total_farm_exp ///
    ln_total_farm_exp_exc_fert ln_farm_profits farm_profits ///
    w_total_farm_exp w_total_farm_exp_exc_fert hh_size hh_age ///
    dist_agro plot_siz seed_typ_num {
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

gen ln_total_farm_exp_positive = log(w_total_farm_exp) if w_total_farm_exp > 0
gen ln_total_farm_exp_exc_fert_positive = log(w_total_farm_exp_exc_fert) ///
    if w_total_farm_exp_exc_fert > 0
gen asinh_farm_profits = asinh(farm_profits)

local controls hh_size hh_age i.hh_educ_cat dist_agro plot_siz ///
    i.slope_cat i.soil_str_cat i.seed_typ_num

local outcomes ln_total_farm_exp ln_total_farm_exp_positive ///
    ln_total_farm_exp_exc_fert ln_farm_profits asinh_farm_profits ///
    farm_profits

tempname handle
postfile `handle' str40 outcome str3 controls double n clusters ///
    control_mean t1_coef t1_se t1_p t2_coef t2_se t2_p p_equal using "`results'", replace

foreach y of local outcomes {
    quietly regress `y' i.treat_num, vce(cluster cluster_id)
    post_spec `y', controls("No") handle(`handle')

    quietly regress `y' i.treat_num `controls', vce(cluster cluster_id)
    post_spec `y', controls("Yes") handle(`handle')
}

postclose `handle'

use "`results'", clear
export delimited using "`out_logs'/expenditure_profit_audit_stata.csv", replace

preserve
    keep if controls == "Yes"

    gen byte order = .
    replace order = 1 if outcome == "ln_total_farm_exp"
    replace order = 2 if outcome == "ln_total_farm_exp_positive"
    replace order = 3 if outcome == "ln_total_farm_exp_exc_fert"
    replace order = 4 if outcome == "ln_farm_profits"
    replace order = 5 if outcome == "asinh_farm_profits"
    replace order = 6 if outcome == "farm_profits"
    sort order

    gen str44 outcome_label = ""
    replace outcome_label = "Log expenditure, current coding" if outcome == "ln_total_farm_exp"
    replace outcome_label = "Log expenditure, positive total" if outcome == "ln_total_farm_exp_positive"
    replace outcome_label = "Log expenditure excluding fertilizer" if outcome == "ln_total_farm_exp_exc_fert"
    replace outcome_label = "Log profits, positive profits only" if outcome == "ln_farm_profits"
    replace outcome_label = "Asinh profits" if outcome == "asinh_farm_profits"
    replace outcome_label = "Profits, levels" if outcome == "farm_profits"

    gen str16 t1_coef_fmt = cond(missing(t1_coef), "", ///
        strtrim(string(cond(abs(t1_coef) < 0.005, 0, t1_coef), "%9.2f")))
    gen str16 t2_coef_fmt = cond(missing(t2_coef), "", ///
        strtrim(string(cond(abs(t2_coef) < 0.005, 0, t2_coef), "%9.2f")))
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

    gen str20 t1_cell = t1_coef_fmt + t1_star
    gen str20 t2_cell = t2_coef_fmt + t2_star

    file open tex using "`out_tables'/expenditure_profit_audit_stata.tex", write replace
    file write tex "{\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" _n
    file write tex "\begin{tabular}{lcccc}" _n
    file write tex "\toprule" _n
    file write tex "Outcome & N & T1 & T2 & p-value: T2 = T1 \\" _n
    file write tex "\midrule" _n
    forvalues i = 1/`=_N' {
        file write tex "`=outcome_label[`i']' & `=n_fmt[`i']' & `=t1_cell[`i']' & `=t2_cell[`i']' & `=p_equal_fmt[`i']' \\" _n
    }
    file write tex "\bottomrule" _n
    file write tex "\end{tabular}}" _n
    file close tex
restore

import delimited using "`data_endline'", clear varnames(1) stringcols(_all)
keep if inlist(treat, "C", "T1", "T2") & main_crp == "MAIZE"

foreach v in w_total_farm_exp w_total_farm_exp_exc_fert farm_profits ///
    ln_total_farm_exp ln_farm_profits {
    destring `v', replace force
}

gen ln_total_farm_exp_positive = log(w_total_farm_exp) if w_total_farm_exp > 0

tempname diag_handle
postfile `diag_handle' str40 variable str2 treat double n nonmissing positive ///
    mean median using "`diagnostics'", replace

local diagnostic_vars w_total_farm_exp w_total_farm_exp_exc_fert farm_profits ///
    ln_total_farm_exp ln_total_farm_exp_positive ln_farm_profits

foreach v of local diagnostic_vars {
    foreach arm in C T1 T2 {
        quietly count if treat == "`arm'"
        local n = r(N)

        quietly count if treat == "`arm'" & !missing(`v')
        local nonmissing = r(N)

        quietly count if treat == "`arm'" & `v' > 0 & !missing(`v')
        local positive = r(N)

        quietly summarize `v' if treat == "`arm'", detail
        local mean = r(mean)
        local median = r(p50)

        post `diag_handle' ("`v'") ("`arm'") (`n') (`nonmissing') ///
            (`positive') (`mean') (`median')
    }
}

postclose `diag_handle'

use "`diagnostics'", clear
export delimited using "`out_logs'/expenditure_profit_sample_diagnostics_stata.csv", replace
