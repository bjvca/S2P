replication_root <- normalizePath(getwd(), mustWork = TRUE)

scripts <- c(
  "code/R/01_table1_balance.R",
  "code/R/02_sample_flow_attrition.R"
)

for (script in scripts) {
  message("Running ", script)
  source(file.path(replication_root, script))
}
