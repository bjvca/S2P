version 17
clear all
set more off

* Reproduce the first fertilizer-use impact table from the public endline file.
*
* Input:
*   ../endline/data/public/clear_merged_data.csv
*
* Outputs:
*   output/tables/table2_fertilizer_use_stata.tex
*   output/logs/table2_fertilizer_use_stata.csv
*
* Run from the replication_package folder:
*   stata -b do code/stata/06_table2_fertilizer_use.do
*
* This script follows the preferred specification used in the R replication
* code:
*   - Columns (1) and (3): unadjusted ITT.
*   - Columns (2) and (4): adjusted ITT with pre-treatment controls only.
*   - Education level, slope, soil structure, and seed type enter as
*     categorical controls.
*   - agronaut visits and extension contact are excluded from the preferred
*     adjusted specification because they can be affected by treatment.

local root "`c(pwd)'"
local data_endline "`root'/../endline/data/public/clear_merged_data.csv"
local out_tables "`root'/output/tables"
local out_logs "`root'/output/logs"

capture mkdir "`root'/output"
capture mkdir "`out_tables'"
capture mkdir "`out_logs'"

tempfile results

program define post_spec
    syntax, Label(string) Sample(string) Controls(string) Handle(name)

    local n = e(N)
    local clusters = e(N_clust)

    quietly summarize total_qty_fert if e(sample) & treat == "C"
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

    post `handle' ("`label'") ("`sample'") ("`controls'") (`n') (`clusters') ///
        (`control_mean') (`t1_coef') (`t1_se') (`t1_p') ///
        (`t2_coef') (`t2_se') (`t2_p')
end

import delimited using "`data_endline'", clear varnames(1) stringcols(_all)
keep if inlist(treat, "C", "T1", "T2")

* Numeric variables arrive from CSV as strings. Convert only the fields used by
* the fertilizer-use table and leave the questionnaire text variables untouched.
foreach v in treat_num cluster_id_num total_qty_fert hh_size hh_age dist_agro plot_siz seed_typ_num {
    destring `v', replace force
}

rename cluster_id_num cluster_id

* Restrict sentinel values to missing before estimating the adjusted columns.
replace dist_agro = . if dist_agro == 999
replace plot_siz = . if plot_siz == 999

* Blank strings and one stray soil code in the CSV need to be normalized before
* encoding categorical controls.
replace hh_educ = "" if trim(hh_educ) == ""
replace slope = "" if trim(slope) == ""
replace soil_str = "" if trim(soil_str) == ""
replace soil_str = "Other/unknown" if soil_str == "5"

encode hh_educ, gen(hh_educ_cat)
encode slope, gen(slope_cat)
encode soil_str, gen(soil_str_cat)

label variable hh_educ_cat "Household head education level"
label variable dist_agro "Time to agro-dealer (minutes)"
label variable plot_siz "Plot size (acres)"
label variable slope_cat "Plot slope"
label variable soil_str_cat "Soil structure"
label variable seed_typ_num "Seed type used"

local fert_controls hh_size hh_age i.hh_educ_cat dist_agro plot_siz ///
    i.slope_cat i.soil_str_cat i.seed_typ_num

tempname handle
postfile `handle' str3 column str12 sample str3 controls double n clusters ///
    control_mean t1_coef t1_se t1_p t2_coef t2_se t2_p using "`results'", replace

quietly regress total_qty_fert i.treat_num, vce(cluster cluster_id)
post_spec, label("(1)") sample("All crops") controls("No") handle(`handle')

quietly regress total_qty_fert i.treat_num `fert_controls', vce(cluster cluster_id)
post_spec, label("(2)") sample("All crops") controls("Yes") handle(`handle')

quietly regress total_qty_fert i.treat_num if main_crp == "MAIZE", vce(cluster cluster_id)
post_spec, label("(3)") sample("Maize only") controls("No") handle(`handle')

quietly regress total_qty_fert i.treat_num `fert_controls' if main_crp == "MAIZE", ///
    vce(cluster cluster_id)
post_spec, label("(4)") sample("Maize only") controls("Yes") handle(`handle')

postclose `handle'

use "`results'", clear
export delimited using "`out_logs'/table2_fertilizer_use_stata.csv", replace

gen str16 control_mean_fmt = cond(missing(control_mean), "", ///
    strtrim(string(cond(abs(control_mean) < 0.0005, 0, control_mean), "%9.2f")))
gen str16 t1_coef_fmt = cond(missing(t1_coef), "", ///
    strtrim(string(cond(abs(t1_coef) < 0.0005, 0, t1_coef), "%9.2f")))
gen str16 t2_coef_fmt = cond(missing(t2_coef), "", ///
    strtrim(string(cond(abs(t2_coef) < 0.0005, 0, t2_coef), "%9.2f")))
gen str16 t1_se_fmt = cond(missing(t1_se), "", ///
    strtrim(string(cond(abs(t1_se) < 0.0005, 0, t1_se), "%9.2f")))
gen str16 t2_se_fmt = cond(missing(t2_se), "", ///
    strtrim(string(cond(abs(t2_se) < 0.0005, 0, t2_se), "%9.2f")))
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

file open tex using "`out_tables'/table2_fertilizer_use_stata.tex", write replace
file write tex "{\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" _n
file write tex "\begin{tabular}{l*{4}{c}}" _n
file write tex "\toprule" _n
file write tex "&\multicolumn{2}{c}{All crops} & \multicolumn{2}{c}{Maize only} \\" _n
file write tex "\cmidrule(lr){2-3}\cmidrule(lr){4-5}" _n
file write tex "& (1) & (2) & (3) & (4) \\" _n
file write tex "\midrule" _n
file write tex "Treatment one & `=t1_cell[1]' & `=t1_cell[2]' & `=t1_cell[3]' & `=t1_cell[4]' \\" _n
file write tex "& (`=t1_se_fmt[1]') & (`=t1_se_fmt[2]') & (`=t1_se_fmt[3]') & (`=t1_se_fmt[4]') \\" _n
file write tex "Treatment two & `=t2_cell[1]' & `=t2_cell[2]' & `=t2_cell[3]' & `=t2_cell[4]' \\" _n
file write tex "& (`=t2_se_fmt[1]') & (`=t2_se_fmt[2]') & (`=t2_se_fmt[3]') & (`=t2_se_fmt[4]') \\" _n
file write tex "Control mean & `=control_mean_fmt[1]' & `=control_mean_fmt[2]' & `=control_mean_fmt[3]' & `=control_mean_fmt[4]' \\" _n
file write tex "\midrule" _n
file write tex "Pre-treatment controls & `=controls[1]' & `=controls[2]' & `=controls[3]' & `=controls[4]' \\" _n
file write tex "Number of observations & `=n_fmt[1]' & `=n_fmt[2]' & `=n_fmt[3]' & `=n_fmt[4]' \\" _n
file write tex "\bottomrule" _n
file write tex "\end{tabular}}" _n
file close tex
