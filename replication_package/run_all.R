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
  "code/R/06_table2_fertilizer_use_audit.R"
)

for (script in scripts) {
  message("Running ", script)
  source(file.path(replication_root, script))
}
