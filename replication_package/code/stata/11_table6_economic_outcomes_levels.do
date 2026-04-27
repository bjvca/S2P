version 17
clear all
set more off

* Generate the level economic-outcome table for the manuscript.
*
* This script mirrors code/R/11_table6_economic_outcomes_levels.R. The public
* endline file does not contain household-level revenue for all crops. It
* contains production and cost information for the sampled/test plot's main
* crop. The table therefore reports:
*   1. all sampled main crops; and
*   2. the maize-main-crop subsample.
*
* Value of production is imputed as harvested quantity times the crop-specific
* median observed sale price among sellers of that crop. Costs are the sum of
* seed, pesticide, other sampled-plot expenses, and fertilizer costs. Profits
* are value of production minus those costs.
*
* Input:
*   ../endline/data/public/clear_merged_data.csv
*
* Outputs:
*   output/tables/table6_economic_outcomes_levels_stata.tex
*   output/logs/table6_economic_outcomes_levels_stata.csv
*   output/logs/table6_economic_price_diagnostics_stata.csv
*   output/logs/table6_economic_sample_diagnostics_stata.csv
*
* Run from the replication_package folder:
*   stata -b do code/stata/11_table6_economic_outcomes_levels.do

local root "`c(pwd)'"
local data_endline "`root'/../endline/data/public/clear_merged_data.csv"
local out_tables "`root'/output/tables"
local out_logs "`root'/output/logs"

capture mkdir "`root'/output"
capture mkdir "`out_tables'"
capture mkdir "`out_logs'"

tempfile results price_diag sample_diag

program define post_spec
    syntax varname, Sample(string) Label(string) Handle(name)

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

    post `handle' ("`sample'") ("`label'") ("`varlist'") (`n') (`clusters') ///
        (`control_mean') (`t1_coef') (`t1_se') (`t1_p') ///
        (`t2_coef') (`t2_se') (`t2_p') (`p_equal')
end

import delimited using "`data_endline'", clear varnames(1) stringcols(_all)
keep if inlist(treat, "C", "T1", "T2")

foreach v in treat_num cluster_id_num hh_size hh_age dist_agro plot_siz ///
    maize_area seed_typ_num price_seed ttl_cost_pest test_plotttl_exp ///
    fert_cost1 fert_cost2 fert_cost3 fert_cost4 prc_main bags_Mcrp {
    destring `v', replace force
}

rename cluster_id_num cluster_id

replace dist_agro = . if dist_agro == 999
replace plot_siz = . if plot_siz == 999
replace maize_area = . if maize_area == 999
replace hh_age = . if hh_age == 999
replace hh_educ = "" if trim(hh_educ) == ""
replace slope = "" if trim(slope) == ""
replace soil_str = "" if trim(soil_str) == ""
replace soil_str = "Other/unknown" if soil_str == "5"

encode hh_educ, gen(hh_educ_cat)
encode slope, gen(slope_cat)
encode soil_str, gen(soil_str_cat)

* Sum monetary input costs. The missing option keeps cost_total missing when all
* components are missing, while treating item-level missing components as zero
* when at least one cost component is observed.
egen cost_total = rowtotal(price_seed ttl_cost_pest test_plotttl_exp ///
    fert_cost1 fert_cost2 fert_cost3 fert_cost4), missing

* Crop prices are observed only for sellers. Use crop-level medians after
* dropping nonpositive prices; this makes the valuation less sensitive to
* extreme unit-value errors than means.
gen prc_main_positive = prc_main if prc_main > 0 & !missing(prc_main)
egen median_price = median(prc_main_positive), by(main_crp)

gen value_production = bags_Mcrp * median_price
gen profits = value_production - cost_total

gen value_per_acre_all = value_production / plot_siz
gen cost_per_acre_all = cost_total / plot_siz
gen profit_per_acre_all = profits / plot_siz

gen value_per_acre_maize = value_production / maize_area
gen cost_per_acre_maize = cost_total / maize_area
gen profit_per_acre_maize = profits / maize_area

preserve
    keep if main_crp != ""
    collapse ///
        (count) n = main_crp ///
        (count) production_nonmissing = bags_Mcrp ///
        (count) price_imputed = median_price ///
        (count) value_nonmissing = value_production ///
        (count) sellers = prc_main_positive ///
        (median) median_price = prc_main_positive ///
        (mean) mean_price = prc_main_positive ///
        (min) min_price = prc_main_positive ///
        (max) max_price = prc_main_positive, by(main_crp)
    export delimited using "`out_logs'/table6_economic_price_diagnostics_stata.csv", replace
restore

tempname handle
postfile `handle' str30 sample str30 label str30 outcome double n clusters ///
    control_mean t1_coef t1_se t1_p t2_coef t2_se t2_p p_equal using "`results'", replace

local controls hh_size hh_age i.hh_educ_cat dist_agro plot_siz ///
    i.slope_cat i.soil_str_cat i.seed_typ_num

quietly regress value_production i.treat_num `controls' ///
    if main_crp != "" & !missing(value_production, cost_total), vce(cluster cluster_id)
post_spec value_production, sample("All sampled main crops") label("Value of production") handle(`handle')

quietly regress cost_total i.treat_num `controls' ///
    if main_crp != "" & !missing(value_production, cost_total), vce(cluster cluster_id)
post_spec cost_total, sample("All sampled main crops") label("Total costs") handle(`handle')

quietly regress profits i.treat_num `controls' ///
    if main_crp != "" & !missing(value_production, cost_total), vce(cluster cluster_id)
post_spec profits, sample("All sampled main crops") label("Profits") handle(`handle')

quietly regress value_per_acre_all i.treat_num `controls' ///
    if main_crp != "" & !missing(value_production, cost_total), vce(cluster cluster_id)
post_spec value_per_acre_all, sample("All sampled main crops") label("Value per acre") handle(`handle')

quietly regress cost_per_acre_all i.treat_num `controls' ///
    if main_crp != "" & !missing(value_production, cost_total), vce(cluster cluster_id)
post_spec cost_per_acre_all, sample("All sampled main crops") label("Costs per acre") handle(`handle')

quietly regress profit_per_acre_all i.treat_num `controls' ///
    if main_crp != "" & !missing(value_production, cost_total), vce(cluster cluster_id)
post_spec profit_per_acre_all, sample("All sampled main crops") label("Profits per acre") handle(`handle')

quietly regress value_production i.treat_num `controls' ///
    if main_crp == "MAIZE" & !missing(value_production, cost_total), vce(cluster cluster_id)
post_spec value_production, sample("Maize main crop") label("Value of production") handle(`handle')

quietly regress cost_total i.treat_num `controls' ///
    if main_crp == "MAIZE" & !missing(value_production, cost_total), vce(cluster cluster_id)
post_spec cost_total, sample("Maize main crop") label("Total costs") handle(`handle')

quietly regress profits i.treat_num `controls' ///
    if main_crp == "MAIZE" & !missing(value_production, cost_total), vce(cluster cluster_id)
post_spec profits, sample("Maize main crop") label("Profits") handle(`handle')

quietly regress value_per_acre_maize i.treat_num `controls' ///
    if main_crp == "MAIZE" & !missing(value_production, cost_total), vce(cluster cluster_id)
post_spec value_per_acre_maize, sample("Maize main crop") label("Value per acre") handle(`handle')

quietly regress cost_per_acre_maize i.treat_num `controls' ///
    if main_crp == "MAIZE" & !missing(value_production, cost_total), vce(cluster cluster_id)
post_spec cost_per_acre_maize, sample("Maize main crop") label("Costs per acre") handle(`handle')

quietly regress profit_per_acre_maize i.treat_num `controls' ///
    if main_crp == "MAIZE" & !missing(value_production, cost_total), vce(cluster cluster_id)
post_spec profit_per_acre_maize, sample("Maize main crop") label("Profits per acre") handle(`handle')

postclose `handle'

tempname diag_handle
postfile `diag_handle' str30 sample double n clusters using "`sample_diag'", replace

quietly count if main_crp != "" & !missing(value_production, cost_total)
local n_all = r(N)
quietly levelsof cluster_id if main_crp != "" & !missing(value_production, cost_total), local(cluster_all)
local clusters_all : word count `cluster_all'
post `diag_handle' ("All sampled main crops") (`n_all') (`clusters_all')

quietly count if main_crp == "MAIZE" & !missing(value_production, cost_total)
local n_maize = r(N)
quietly levelsof cluster_id if main_crp == "MAIZE" & !missing(value_production, cost_total), local(cluster_maize)
local clusters_maize : word count `cluster_maize'
post `diag_handle' ("Maize main crop") (`n_maize') (`clusters_maize')

postclose `diag_handle'

preserve
    use "`sample_diag'", clear
    export delimited using "`out_logs'/table6_economic_sample_diagnostics_stata.csv", replace
restore

use "`results'", clear
export delimited using "`out_logs'/table6_economic_outcomes_levels_stata.csv", replace

preserve
    gen byte order = _n

    gen str16 control_fmt = cond(missing(control_mean), "", ///
        strtrim(string(cond(abs(control_mean) < 0.5, 0, control_mean), "%12.0fc")))
    gen str16 t1_coef_fmt = cond(missing(t1_coef), "", ///
        strtrim(string(cond(abs(t1_coef) < 0.5, 0, t1_coef), "%12.0fc")))
    gen str16 t2_coef_fmt = cond(missing(t2_coef), "", ///
        strtrim(string(cond(abs(t2_coef) < 0.5, 0, t2_coef), "%12.0fc")))
    gen str16 t1_se_fmt = cond(missing(t1_se), "", strtrim(string(t1_se, "%12.0fc")))
    gen str16 t2_se_fmt = cond(missing(t2_se), "", strtrim(string(t2_se, "%12.0fc")))
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

    file open tex using "`out_tables'/table6_economic_outcomes_levels_stata.tex", write replace
    file write tex "{" _n
    file write tex "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" _n
    file write tex "\begin{tabular}{lccccc}" _n
    file write tex "\toprule" _n
    file write tex "Outcome & Control mean & T1 & T2 & p-value: T2 = T1 & N \\" _n
    file write tex "\midrule" _n
    file write tex "\multicolumn{6}{l}{\textit{All sampled main crops}} \\" _n
    forvalues i = 1/6 {
        file write tex "`=label[`i']' & `=control_fmt[`i']' & `=t1_cell[`i']' & `=t2_cell[`i']' & `=p_equal_fmt[`i']' & `=n_fmt[`i']' \\" _n
        file write tex "& & (`=t1_se_fmt[`i']') & (`=t2_se_fmt[`i']') & & \\" _n
    }
    file write tex "\addlinespace" _n
    file write tex "\multicolumn{6}{l}{\textit{Maize main crop}} \\" _n
    forvalues i = 7/12 {
        file write tex "`=label[`i']' & `=control_fmt[`i']' & `=t1_cell[`i']' & `=t2_cell[`i']' & `=p_equal_fmt[`i']' & `=n_fmt[`i']' \\" _n
        file write tex "& & (`=t1_se_fmt[`i']') & (`=t2_se_fmt[`i']') & & \\" _n
    }
    file write tex "\bottomrule" _n
    file write tex "\end{tabular}" _n
    file write tex "}" _n
    file close tex
restore
