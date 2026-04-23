replication_root <- normalizePath(getwd(), mustWork = TRUE)

scripts <- c(
  "code/R/01_table1_balance.R"
)

for (script in scripts) {
  message("Running ", script)
  source(file.path(replication_root, script))
}
