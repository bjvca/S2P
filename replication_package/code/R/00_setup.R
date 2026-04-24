# Shared setup for R replication scripts.
#
# Each table script is written so it can be run either through run_all.R or
# directly with Rscript. The block below resolves the replication-package root
# in both cases and defines common input/output folders.
if (!exists("replication_root")) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    this_file <- normalizePath(sub("^--file=", "", file_arg[1]), mustWork = TRUE)
    replication_root <- normalizePath(file.path(dirname(this_file), "..", ".."), mustWork = TRUE)
  } else {
    replication_root <- normalizePath(getwd(), mustWork = TRUE)
  }
}

dir_code <- file.path(replication_root, "code")
dir_data <- file.path(replication_root, "data", "analysis")
dir_output <- file.path(replication_root, "output")
dir_tables <- file.path(dir_output, "tables")
dir_logs <- file.path(dir_output, "logs")
dir_figures <- file.path(dir_output, "figures")

# Create output folders on demand. Generated outputs are committed only when
# they correspond to tables/figures used in the manuscript.
for (d in c(dir_output, dir_tables, dir_logs, dir_figures)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# Formatting helpers for LaTeX tables. Values that round to zero are forced to
# print as 0.000 rather than -0.000.
fmt_num <- function(x, digits = 3) {
  x[!is.na(x) & abs(x) < 0.5 * 10^(-digits)] <- 0
  ifelse(is.na(x), "", sprintf(paste0("%.", digits, "f"), x))
}

fmt_p <- function(x, digits = 3) {
  x[!is.na(x) & abs(x) < 0.5 * 10^(-digits)] <- 0
  ifelse(is.na(x), "", sprintf(paste0("%.", digits, "f"), x))
}
