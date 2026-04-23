# Copy generated table fragments into the Overleaf manuscript repository.
#
# Overleaf does not run R/Stata during LaTeX compilation. The replication
# package is therefore the source of truth for statistical outputs, while the
# paper repository receives generated .tex fragments that Overleaf can input.
#
# Run after Rscript run_all.R:
#   Rscript code/R/sync_outputs_to_paper.R

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

paper_root <- normalizePath(file.path(replication_root, "..", "paper"), mustWork = TRUE)

source_tables <- file.path(replication_root, "output", "tables")
target_tables <- file.path(paper_root, "tables")

if (!dir.exists(source_tables)) {
  stop("Missing generated tables folder: ", source_tables)
}

if (!dir.exists(target_tables)) {
  dir.create(target_tables, recursive = TRUE)
}

table_files <- list.files(source_tables, pattern = "\\.tex$", full.names = TRUE)
if (length(table_files) == 0) {
  stop("No generated .tex tables found in ", source_tables)
}

copied <- file.copy(
  from = table_files,
  to = file.path(target_tables, basename(table_files)),
  overwrite = TRUE
)

if (!all(copied)) {
  failed <- basename(table_files)[!copied]
  stop("Failed to copy generated tables: ", paste(failed, collapse = ", "))
}

message("Copied generated tables to ", target_tables)
for (f in basename(table_files)) {
  message("  - ", f)
}
