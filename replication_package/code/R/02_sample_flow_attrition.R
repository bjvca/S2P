# Sample-flow and attrition audit for the S2P paper.
#
# Purpose:
#   1. Compare the original sampled baseline frame to the realized endline file.
#   2. Document contact and interview realization by treatment arm.
#   3. Show how many endline observations successfully merge to the
#      recommendation/administrative data.
#   4. Flag off-sample "using only" records created by the Stata merge.
#
# This script reads:
#   - replication_package/data/analysis/baseline_balance_sample.csv
#   - endline/data/public/clear_merged_data.csv
#
# The endline file is kept in the main repository rather than duplicated inside
# the replication package. That keeps the replication package lean while still
# letting this audit script reproduce the sample-flow numbers used in the paper.

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

source(file.path(replication_root, "code", "R", "00_setup.R"))

# Resolve the repository root from the replication-package root.
dir_repo <- normalizePath(file.path(replication_root, ".."), mustWork = TRUE)

baseline_path <- file.path(dir_data, "baseline_balance_sample.csv")
endline_path <- file.path(dir_repo, "endline", "data", "public", "clear_merged_data.csv")

if (!file.exists(baseline_path)) {
  stop("Missing baseline sample file: ", baseline_path)
}

if (!file.exists(endline_path)) {
  stop("Missing endline merged file: ", endline_path)
}

baseline <- read.csv(baseline_path, stringsAsFactors = FALSE)
endline <- read.csv(endline_path, stringsAsFactors = FALSE)

# Harmonize treatment labels and impose the experimental arm order used in the
# paper. The baseline sample is the original sampled frame from which the RCT
# was drawn. The endline file should contain one row per targeted endline case,
# plus some off-sample records introduced from the recommendation merge.
arm_levels <- c("C", "T1", "T2")
baseline$treat <- trimws(baseline$treat)
endline$treat <- trimws(endline$treat)

# The Stata merge created one merge-status variable with the standard codes:
#   Master only (1) = present in endline master only
#   Using only (2)  = present only in recommendation/admin file
#   Matched (3)     = present in both files
# The "Using only" rows do not belong in the endline analysis sample and are
# summarized separately as a data-quality diagnostic.
if (!"Merging_Recommendation" %in% names(endline)) {
  stop("Expected Merging_Recommendation in endline merged file.")
}

using_only <- endline[endline$Merging_Recommendation == "Using only (2)", , drop = FALSE]
endline_master <- endline[endline$treat %in% arm_levels, , drop = FALSE]

# Sanity checks. If these fail, the merged file changed in a way that requires
# revisiting the sample-flow logic.
if (nrow(baseline) != 2034) {
  warning("Baseline sampled frame has ", nrow(baseline), " rows; expected 2034.")
}

if (nrow(endline_master) + nrow(using_only) != nrow(endline)) {
  warning("Merged file contains rows outside the treatment-arm sample and outside Using only (2).")
}

# Helper that counts rows satisfying a logical condition within each arm.
count_by_arm <- function(data, treat_var, condition) {
  out <- setNames(numeric(length(arm_levels)), arm_levels)
  for (arm in arm_levels) {
    subset_arm <- data[data[[treat_var]] == arm, , drop = FALSE]
    out[arm] <- sum(condition(subset_arm), na.rm = TRUE)
  }
  out
}

# Baseline sampled counts by arm. These are the denominators for attrition from
# the original sampled frame to the realized endline target file.
baseline_n <- table(factor(baseline$treat, levels = arm_levels))

# Endline target records by arm. These are the rows in the endline master file,
# irrespective of whether the farmer was found or interviewed.
endline_target_n <- table(factor(endline_master$treat, levels = arm_levels))

# Contact and interview realization. In the questionnaire:
#   q4a = 1 if the farmer was found, 0 if not found.
#   checkq9 captures whether the interview proceeded after the consent screen.
found_n <- count_by_arm(endline_master, "treat", function(df) df$q4a == 1 | df$q4a == "1")
not_found_n <- count_by_arm(endline_master, "treat", function(df) df$q4a == 0 | df$q4a == "0")

interview_completed_n <- if ("checkq9" %in% names(endline_master)) {
  count_by_arm(endline_master, "treat", function(df) df$checkq9 == "Yes" | df$checkq9 == 1 | df$checkq9 == "1")
} else {
  setNames(rep(NA_real_, length(arm_levels)), arm_levels)
}

interview_refused_n <- if ("checkq9" %in% names(endline_master)) {
  count_by_arm(endline_master, "treat", function(df) df$checkq9 == "No" | df$checkq9 == 0 | df$checkq9 == "0")
} else {
  setNames(rep(NA_real_, length(arm_levels)), arm_levels)
}

# Merge quality by arm within the endline master file.
matched_n <- count_by_arm(
  endline_master,
  "treat",
  function(df) df$Merging_Recommendation == "Matched (3)"
)

master_only_n <- count_by_arm(
  endline_master,
  "treat",
  function(df) df$Merging_Recommendation == "Master only (1)"
)

# Assemble the main audit table. Rates are reported relative to the baseline
# sampled frame and, separately, relative to the realized endline target file.
sample_flow <- data.frame(
  treatment = arm_levels,
  baseline_sample_n = as.numeric(baseline_n[arm_levels]),
  endline_target_n = as.numeric(endline_target_n[arm_levels]),
  found_n = as.numeric(found_n[arm_levels]),
  not_found_n = as.numeric(not_found_n[arm_levels]),
  interview_completed_n = as.numeric(interview_completed_n[arm_levels]),
  interview_refused_n = as.numeric(interview_refused_n[arm_levels]),
  matched_recommendation_n = as.numeric(matched_n[arm_levels]),
  unmatched_endline_n = as.numeric(master_only_n[arm_levels]),
  stringsAsFactors = FALSE
)

sample_flow$endline_target_rate <- sample_flow$endline_target_n / sample_flow$baseline_sample_n
sample_flow$found_rate <- sample_flow$found_n / sample_flow$baseline_sample_n
sample_flow$interview_completed_rate <- sample_flow$interview_completed_n / sample_flow$baseline_sample_n
sample_flow$matched_recommendation_rate_baseline <- sample_flow$matched_recommendation_n / sample_flow$baseline_sample_n
sample_flow$matched_recommendation_rate_endline <- sample_flow$matched_recommendation_n / sample_flow$endline_target_n

# Summarize the off-sample records created by the merge. These rows should not
# enter treatment-effect estimation but are important to document because they
# reveal how many recommendation/admin records lack an endline counterpart.
using_only_summary <- data.frame(
  metric = c(
    "using_only_rows",
    "using_only_rows_with_missing_treat",
    "using_only_rows_with_nonmissing_treat"
  ),
  value = c(
    nrow(using_only),
    sum(trimws(using_only$treat) == "" | is.na(using_only$treat)),
    sum(trimws(using_only$treat) %in% arm_levels)
  ),
  stringsAsFactors = FALSE
)

write.csv(sample_flow,
          file.path(dir_logs, "sample_flow_attrition.csv"),
          row.names = FALSE)

write.csv(using_only_summary,
          file.path(dir_logs, "sample_flow_merge_diagnostics.csv"),
          row.names = FALSE)

# Build a compact LaTeX table for the manuscript. The current recommendation is
# to use the appendix for the full sample-flow table and summarize the main
# sample realization in the text, but generating the table now keeps that option
# open.
latex_lines <- c(
  "\\begin{tabular}{lrrrrr}",
  "\\hline",
  "Treatment arm & Baseline sample & Endline target & Found & Interview completed & Matched to soil test data \\\\",
  "\\hline"
)

for (i in seq_len(nrow(sample_flow))) {
  row <- sample_flow[i, ]
  latex_lines <- c(
    latex_lines,
    sprintf(
      "%s & %s & %s & %s & %s & %s \\\\",
      row$treatment,
      fmt_num(row$baseline_sample_n, 0),
      fmt_num(row$endline_target_n, 0),
      fmt_num(row$found_n, 0),
      fmt_num(row$interview_completed_n, 0),
      fmt_num(row$matched_recommendation_n, 0)
    )
  )
}

latex_lines <- c(
  latex_lines,
  "\\hline",
  "\\end{tabular}"
)

writeLines(latex_lines, file.path(dir_tables, "sample_flow_attrition.tex"))

message("Wrote sample-flow audit to: ", file.path(dir_logs, "sample_flow_attrition.csv"))
message("Wrote merge diagnostics to: ", file.path(dir_logs, "sample_flow_merge_diagnostics.csv"))
message("Wrote LaTeX table to: ", file.path(dir_tables, "sample_flow_attrition.tex"))
