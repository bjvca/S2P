# Resolve the replication-package root whether this script is run from the
# package directory itself or invoked from elsewhere with Rscript.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg) > 0) {
  this_file <- normalizePath(sub("^--file=", "", file_arg[1]), mustWork = TRUE)
  replication_root <- normalizePath(dirname(this_file), mustWork = TRUE)
} else {
  replication_root <- normalizePath(getwd(), mustWork = TRUE)
}

scripts <- c(
  "code/R/01_table1_balance.R",
  "code/R/02_sample_flow_attrition.R",
  "code/R/03_sample_flow_visuals.R",
  "code/R/04_attrition_diagnostics.R",
  "code/R/05_table_first_stage.R",
  "code/R/06_table2_fertilizer_use_audit.R",
  "code/R/07_table3_aip_fertilizer_use.R",
  "code/R/07g_aip_substitution_unified.R",
  "code/R/08_table4_nutrient_use.R",
  "code/R/09_table5_maize_yield.R",
  "code/R/10_expenditure_profit_audit.R",
  "code/R/11_table6_economic_outcomes_levels.R",
  "code/R/11b_table6_economic_robustness.R",
  "code/R/12_table7_snm_practices.R",
  "code/R/13_table8_application_compliance.R",
  "code/R/14_table9_product_compliance.R"
  # "code/R/15_multiple_testing_summary.R"  # First-draft MHT diagnostic — disabled
  # 2026-05-07. Multiple-testing strategy (Anderson indices vs Holm vs
  # Romano-Wolf, family definitions) to be revisited with co-authors before
  # re-enabling. Output is not currently used by the manuscript.
)

for (script in scripts) {
  message("Running ", script)
  source(file.path(replication_root, script))
}
