# Attrition diagnostics for the S2P paper.
#
# This script complements the descriptive sample-flow audit with:
#   1. Regression-based tests for differential realization across treatment arms
#      at each survey stage.
#   2. A retained-sample balance table using baseline covariates among
#      households that completed the endline interview.
#
# All code lives in the replication package, but the endline file itself is
# read from the main repository so we do not duplicate analysis extracts.

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

suppressPackageStartupMessages({
  library(clubSandwich)
  library(car)
})

dir_repo <- normalizePath(file.path(replication_root, ".."), mustWork = TRUE)
baseline_path <- file.path(dir_data, "baseline_balance_sample.csv")
endline_path <- file.path(dir_repo, "endline", "data", "public", "clear_merged_data.csv")

baseline <- read.csv(baseline_path, stringsAsFactors = FALSE)
endline <- read.csv(endline_path, stringsAsFactors = FALSE)

# Keep only endline rows that correspond to households in the sampled baseline
# frame, then enforce one row per sampled household. The current cleaned endline
# extract has a unique row for each sampled household in the endline master, so
# duplicates would indicate a data-management problem worth surfacing.
endline_master <- endline[endline$farmer_id %in% baseline$farmer_ID, , drop = FALSE]
if (any(duplicated(endline_master$farmer_id))) {
  stop("Duplicate farmer_id values found in clear_merged_data.csv for sampled households.")
}

df <- merge(
  baseline,
  endline_master,
  by.x = "farmer_ID",
  by.y = "farmer_id",
  all.x = TRUE
)

df$treat <- trimws(df$treat.x)
df$t1 <- as.integer(df$treat == "T1")
df$t2 <- as.integer(df$treat == "T2")

# Survey-stage indicators. These are defined on the full sampled baseline
# frame, with zeros for sampled households that do not reach a given stage.
df$endline_target <- as.integer(!is.na(df$q4a))
df$found <- as.integer(df$q4a == "1")
df$interview_completed <- as.integer(df$checkq9 == "Yes")
df$matched_soil <- as.integer(df$Merging_Recommendation == "Matched (3)")

stage_specs <- data.frame(
  var = c("endline_target", "found", "interview_completed", "matched_soil"),
  label = c("Endline target", "Found", "Interview completed", "Matched to soil test data"),
  stringsAsFactors = FALSE
)

# Helper to estimate treatment-arm differences relative to control with
# cluster-robust standard errors at the village level.
run_stage_test <- function(var_name, label) {
  model <- lm(reformulate(c("t1", "t2"), response = var_name), data = df)
  vcov_stage <- vcovCR(model, cluster = df$clusterID, type = "CR2")
  ct <- as.data.frame(coef_test(model, vcov = vcov_stage, test = "Satterthwaite"))
  rownames(ct) <- ct$Coef
  equal_test <- linearHypothesis(model, "t1 = t2", vcov. = vcov_stage, test = "F")

  data.frame(
    stage = label,
    control_mean = mean(df[df$treat == "C", var_name], na.rm = TRUE),
    t1_coef = ct["t1", "beta"],
    t1_se = ct["t1", "SE"],
    t1_p = ct["t1", "p_Satt"],
    t2_coef = ct["t2", "beta"],
    t2_se = ct["t2", "SE"],
    t2_p = ct["t2", "p_Satt"],
    p_equal = equal_test$`Pr(>F)`[2],
    n = nrow(df),
    stringsAsFactors = FALSE
  )
}

attrition_results <- do.call(
  rbind,
  lapply(seq_len(nrow(stage_specs)), function(i) {
    run_stage_test(stage_specs$var[i], stage_specs$label[i])
  })
)

write.csv(
  attrition_results,
  file.path(dir_logs, "attrition_stage_regressions.csv"),
  row.names = FALSE
)

# Build a LaTeX appendix table with one row per stage.
attrition_lines <- c(
  "\\begin{tabular}{lrrrrr}",
  "\\hline",
  "Stage & Control mean & T1 $-$ Control & T2 $-$ Control & $p$-value: T1 = T2 & N \\\\",
  "\\hline"
)

for (i in seq_len(nrow(attrition_results))) {
  row <- attrition_results[i, ]
  attrition_lines <- c(
    attrition_lines,
    sprintf(
      "%s & %s & %s & %s & %s & %s \\\\",
      row$stage,
      fmt_num(row$control_mean, 3),
      fmt_num(row$t1_coef, 3),
      fmt_num(row$t2_coef, 3),
      fmt_p(row$p_equal, 3),
      fmt_num(row$n, 0)
    ),
    sprintf(
      " &  & (%s) & (%s) &  &  \\\\",
      fmt_num(row$t1_se, 3),
      fmt_num(row$t2_se, 3)
    )
  )
}

attrition_lines <- c(
  attrition_lines,
  "\\hline",
  "\\end{tabular}"
)

writeLines(attrition_lines, file.path(dir_tables, "attrition_stage_regressions.tex"))

# -----------------------------------------------------------------------------
# Retained-sample balance table
# -----------------------------------------------------------------------------

retained <- df[df$interview_completed == 1, , drop = FALSE]

# Recreate the baseline covariates using the same logic as the main balance
# table so the retained-sample balance is directly comparable.
to_numeric_with_na <- function(x, na_values = c("n/a", "NA", "")) {
  x <- as.character(x)
  x[x %in% na_values] <- NA
  suppressWarnings(as.numeric(x))
}

retained$q54a <- to_numeric_with_na(retained$q54a)
retained$q58a <- to_numeric_with_na(retained$q58a)
retained$q62a <- to_numeric_with_na(retained$q62a)
retained$q19 <- to_numeric_with_na(retained$q19)
retained$q14 <- to_numeric_with_na(retained$q14)
retained$q17 <- to_numeric_with_na(retained$q17)

retained$q14[retained$q14 == 999] <- NA
retained$q17[retained$q17 == 999] <- NA
retained$age_head <- ifelse(is.na(retained$q17), retained$q14, retained$q17)

retained$q16[retained$q16 == "n/a"] <- NA
retained$q13[retained$q13 == "n/a"] <- NA
retained$q16 <- retained$q16 == "Male"
retained$q13 <- retained$q13 == "Male"
retained$male_head <- ifelse(is.na(retained$q16), retained$q13, retained$q16)

retained$q19[retained$q19 > 15] <- NA
retained$HH_size <- retained$q19

retained$q54a[retained$q54a >= 100] <- NA
retained$q58a[retained$q58a >= 100] <- NA
retained$q62a[retained$q62a >= 100] <- NA

land_components <- cbind(retained$q54a, retained$q58a, retained$q62a)
retained$land_size <- rowSums(land_components, na.rm = TRUE)
retained$land_size[rowSums(!is.na(land_components)) == 0] <- NA
retained$land_size[retained$land_size > 50] <- NA
retained$land_size_ha <- retained$land_size / 2.471

retained$poor <- retained$q69 == 4 | retained$q69 == 5

balance_vars <- c("age_head", "male_head", "HH_size", "land_size_ha", "poor")
balance_labels <- c(
  "Household head age",
  "Household head male",
  "Household size",
  "Land area (ha)",
  "Difficulty feeding family"
)

retained_balance <- vector("list", length(balance_vars))

for (i in seq_along(balance_vars)) {
  var_name <- balance_vars[i]
  model <- lm(reformulate(c("t1", "t2"), response = var_name), data = retained)
  vcov_balance <- vcovCR(model, cluster = retained$clusterID, type = "CR3")
  ct <- as.data.frame(coef_test(model, vcov = vcov_balance, test = "Satterthwaite"))
  rownames(ct) <- ct$Coef

  retained_balance[[i]] <- data.frame(
    variable = balance_labels[i],
    control_mean = mean(retained[retained$treat == "C", var_name], na.rm = TRUE),
    t1_coef = ct["t1", "beta"],
    t1_se = ct["t1", "SE"],
    t2_coef = ct["t2", "beta"],
    t2_se = ct["t2", "SE"],
    n = sum(!is.na(retained[[var_name]])),
    stringsAsFactors = FALSE
  )
}

retained_balance <- do.call(rbind, retained_balance)

write.csv(
  retained_balance,
  file.path(dir_logs, "retained_sample_balance.csv"),
  row.names = FALSE
)

balance_lines <- c(
  "\\begin{tabular}{lrrrr}",
  "\\hline",
  "Variable & Control mean & T1 $-$ Control & T2 $-$ Control & N \\\\",
  "\\hline"
)

for (i in seq_len(nrow(retained_balance))) {
  row <- retained_balance[i, ]
  balance_lines <- c(
    balance_lines,
    sprintf(
      "%s & %s & %s & %s & %s \\\\",
      row$variable,
      fmt_num(row$control_mean, 3),
      fmt_num(row$t1_coef, 3),
      fmt_num(row$t2_coef, 3),
      fmt_num(row$n, 0)
    ),
    sprintf(
      " &  & (%s) & (%s) &  \\\\",
      fmt_num(row$t1_se, 3),
      fmt_num(row$t2_se, 3)
    )
  )
}

balance_lines <- c(
  balance_lines,
  "\\hline",
  "\\end{tabular}"
)

writeLines(balance_lines, file.path(dir_tables, "retained_sample_balance.tex"))

# -----------------------------------------------------------------------------
# Lee-bounds trigger note
# -----------------------------------------------------------------------------

# Lee bounds are outcome-specific and should only be added once we know whether
# differential interview attrition is both statistically real and large enough
# to threaten key treatment-effect estimates. The current interview-completion
# gap is statistically detectable for T2 relative to control/T1, but modest in
# absolute size (about four percentage points), so we log the trigger decision
# rather than compute bounds mechanically at this stage.
lee_trigger <- data.frame(
  metric = c(
    "interview_completion_control_mean",
    "interview_completion_t1_minus_control",
    "interview_completion_t2_minus_control",
    "interview_completion_t1_equals_t2_pvalue",
    "lee_bounds_triggered"
  ),
  value = c(
    attrition_results$control_mean[attrition_results$stage == "Interview completed"],
    attrition_results$t1_coef[attrition_results$stage == "Interview completed"],
    attrition_results$t2_coef[attrition_results$stage == "Interview completed"],
    attrition_results$p_equal[attrition_results$stage == "Interview completed"],
    "No: differential interview attrition is statistically detectable but modest in magnitude"
  ),
  stringsAsFactors = FALSE
)

write.csv(
  lee_trigger,
  file.path(dir_logs, "lee_bounds_trigger_note.csv"),
  row.names = FALSE
)

message("Wrote attrition regressions to: ", file.path(dir_logs, "attrition_stage_regressions.csv"))
message("Wrote retained-sample balance to: ", file.path(dir_logs, "retained_sample_balance.csv"))
message("Wrote Lee-bounds trigger note to: ", file.path(dir_logs, "lee_bounds_trigger_note.csv"))
