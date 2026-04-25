version 17
clear all
set more off

* Reproduce the nutrient-use table from the public endline file.
*
* Input:
*   ../endline/data/public/clear_merged_data.csv
*
* Outputs:
*   output/tables/table4_nutrient_use_stata.tex
*   output/logs/table4_nutrient_use_stata.csv
*
* Run from the replication_package folder:
*   stata -b do code/stata/08_table4_nutrient_use.do

local root "`c(pwd)'"
local data_endline "`root'/../endline/data/public/clear_merged_data.csv"
local out_tables "`root'/output/tables"
local out_logs "`root'/output/logs"

capture mkdir "`root'/output"
capture mkdir "`out_tables'"
capture mkdir "`out_logs'"

tempfile results

program define post_spec
    syntax varname, Outcome(string) Controls(string) Handle(name)

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

    post `handle' ("`outcome'") ("`controls'") (`n') (`clusters') ///
        (`control_mean') (`t1_coef') (`t1_se') (`t1_p') ///
        (`t2_coef') (`t2_se') (`t2_p') (`p_equal')
end

import delimited using "`data_endline'", clear varnames(1) stringcols(_all)
keep if inlist(treat, "C", "T1", "T2") & main_crp == "MAIZE"

foreach v in treat_num cluster_id_num total_N total_P total_K total_nutrient ///
    plot_siz hh_size hh_age dist_agro seed_typ_num {
    destring `v', replace force
}

rename cluster_id_num cluster_id

gen N_kgha = total_N / plot_siz
gen P_kgha = total_P / plot_siz
gen K_kgha = total_K / plot_siz
gen totalnutrient_kgha = total_nutrient / plot_siz

replace dist_agro = . if dist_agro == 999
replace plot_siz = . if plot_siz == 999
replace hh_educ = "" if trim(hh_educ) == ""
replace slope = "" if trim(slope) == ""
replace soil_str = "" if trim(soil_str) == ""
replace soil_str = "Other/unknown" if soil_str == "5"

encode hh_educ, gen(hh_educ_cat)
encode slope, gen(slope_cat)
encode soil_str, gen(soil_str_cat)

local fert_controls hh_size hh_age i.hh_educ_cat dist_agro plot_siz ///
    i.slope_cat i.soil_str_cat i.seed_typ_num

tempname handle
postfile `handle' str24 outcome str3 controls double n clusters ///
    control_mean t1_coef t1_se t1_p t2_coef t2_se t2_p p_equal using "`results'", replace

foreach y in N_kgha P_kgha K_kgha totalnutrient_kgha {
    quietly regress `y' i.treat_num, vce(cluster cluster_id)
    post_spec `y', outcome("`y'") controls("No") handle(`handle')

    quietly regress `y' i.treat_num `fert_controls', vce(cluster cluster_id)
    post_spec `y', outcome("`y'") controls("Yes") handle(`handle')
}

postclose `handle'

use "`results'", clear
export delimited using "`out_logs'/table4_nutrient_use_stata.csv", replace

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
gen str16 p_equal_fmt = cond(missing(p_equal), "", strtrim(string(p_equal, "%9.3f")))

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

file open tex using "`out_tables'/table4_nutrient_use_stata.tex", write replace
file write tex "{\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" _n
file write tex "\begin{tabular}{l*{8}{c}}" _n
file write tex "\toprule" _n
file write tex "& \multicolumn{8}{c}{Nutrient application on maize plots (kg/ha)} \\" _n
file write tex "\cmidrule(lr){2-9}" _n
file write tex "& \multicolumn{2}{c}{Nitrogen} & \multicolumn{2}{c}{Phosphorus} & \multicolumn{2}{c}{Potassium} & \multicolumn{2}{c}{Total nutrients} \\" _n
file write tex "\cmidrule(lr){2-3} \cmidrule(lr){4-5} \cmidrule(lr){6-7} \cmidrule(lr){8-9}" _n
file write tex "& (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\" _n
file write tex "\midrule" _n
file write tex "Treatment one & `=t1_cell[1]' & `=t1_cell[2]' & `=t1_cell[3]' & `=t1_cell[4]' & `=t1_cell[5]' & `=t1_cell[6]' & `=t1_cell[7]' & `=t1_cell[8]' \\" _n
file write tex "& (`=t1_se_fmt[1]') & (`=t1_se_fmt[2]') & (`=t1_se_fmt[3]') & (`=t1_se_fmt[4]') & (`=t1_se_fmt[5]') & (`=t1_se_fmt[6]') & (`=t1_se_fmt[7]') & (`=t1_se_fmt[8]') \\" _n
file write tex "Treatment two & `=t2_cell[1]' & `=t2_cell[2]' & `=t2_cell[3]' & `=t2_cell[4]' & `=t2_cell[5]' & `=t2_cell[6]' & `=t2_cell[7]' & `=t2_cell[8]' \\" _n
file write tex "& (`=t2_se_fmt[1]') & (`=t2_se_fmt[2]') & (`=t2_se_fmt[3]') & (`=t2_se_fmt[4]') & (`=t2_se_fmt[5]') & (`=t2_se_fmt[6]') & (`=t2_se_fmt[7]') & (`=t2_se_fmt[8]') \\" _n
file write tex "p-value: T2 = T1 & `=p_equal_fmt[1]' & `=p_equal_fmt[2]' & `=p_equal_fmt[3]' & `=p_equal_fmt[4]' & `=p_equal_fmt[5]' & `=p_equal_fmt[6]' & `=p_equal_fmt[7]' & `=p_equal_fmt[8]' \\" _n
file write tex "Control mean & `=control_mean_fmt[1]' & `=control_mean_fmt[2]' & `=control_mean_fmt[3]' & `=control_mean_fmt[4]' & `=control_mean_fmt[5]' & `=control_mean_fmt[6]' & `=control_mean_fmt[7]' & `=control_mean_fmt[8]' \\" _n
file write tex "\midrule" _n
file write tex "Pre-treatment controls & `=controls[1]' & `=controls[2]' & `=controls[3]' & `=controls[4]' & `=controls[5]' & `=controls[6]' & `=controls[7]' & `=controls[8]' \\" _n
file write tex "Number of observations & `=n_fmt[1]' & `=n_fmt[2]' & `=n_fmt[3]' & `=n_fmt[4]' & `=n_fmt[5]' & `=n_fmt[6]' & `=n_fmt[7]' & `=n_fmt[8]' \\" _n
file write tex "\bottomrule" _n
file write tex "\end{tabular}}" _n
file close tex
