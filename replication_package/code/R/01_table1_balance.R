# Reproduce Table 1: baseline balance.
#
# Input:
#   data/analysis/baseline_balance_sample.csv
#
# This CSV is a de-identified extract of the 2022 source-study sample selected
# for S2P. It keeps only the raw fields needed to reconstruct the five baseline
# covariates in the PAP balance table and the village-level treatment assignment.
#
# Output:
#   output/tables/table1_balance.tex
#   output/logs/table1_balance_comparison.csv
#
# The displayed contrasts are arm-specific differences relative to control:
#   T1 - Control and T2 - Control.
# This differs from the original PAP table's additive parameterization, where
# the third column was T2 - T1. The comparison log therefore transforms the old
# saved object where possible and ignores quantities that cannot be recovered
# from rounded additive estimates.

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
  library(lmtest)
  library(nnet)
})

balance_sample_path <- file.path(dir_data, "baseline_balance_sample.csv")
if (!file.exists(balance_sample_path)) {
  stop("Missing input data: ", balance_sample_path)
}

sampling_frame <- read.csv(balance_sample_path, stringsAsFactors = FALSE)

# Required raw variables from the 2022 survey extract:
#   q13/q16: respondent/household-head sex
#   q14/q17: respondent/household-head age
#   q19: household size
#   q54a/q58a/q62a: land components used to construct land size
#   q69: food-security/poverty proxy used for the "poor" indicator
#   treat/clusterID: S2P treatment arm and village cluster
required_vars <- c(
  "treat", "clusterID", "q13", "q14", "q16", "q17", "q19",
  "q54a", "q58a", "q62a", "q69"
)
missing_vars <- setdiff(required_vars, names(sampling_frame))
if (length(missing_vars) > 0) {
  stop("Missing required variables: ", paste(missing_vars, collapse = ", "))
}

sampling_frame$treatment <- sampling_frame$treat
sampling_frame$village <- sampling_frame$clusterID

# The original survey uses strings such as "n/a" and sentinel values such as
# 999. This helper converts numeric survey fields while preserving missingness.
to_numeric_with_na <- function(x, na_values = c("n/a", "NA", "")) {
  x <- as.character(x)
  x[x %in% na_values] <- NA
  suppressWarnings(as.numeric(x))
}

sampling_frame$q54a <- to_numeric_with_na(sampling_frame$q54a)
sampling_frame$q54a[sampling_frame$q54a >= 100] <- NA

sampling_frame$q58a <- to_numeric_with_na(sampling_frame$q58a)
sampling_frame$q58a[sampling_frame$q58a >= 100] <- NA

sampling_frame$q62a <- to_numeric_with_na(sampling_frame$q62a)
sampling_frame$q62a[sampling_frame$q62a >= 100] <- NA

# Land size follows sampling/sample_frame.R: sum the three land components,
# remove implausible totals above 50 acres, and convert acres to hectares.
sampling_frame$land_size <- rowSums(
  cbind(sampling_frame$q54a, sampling_frame$q58a, sampling_frame$q62a),
  na.rm = TRUE
)
sampling_frame$land_size[sampling_frame$land_size > 50] <- NA
sampling_frame$land_size_ha <- sampling_frame$land_size / 2.471

sampling_frame$q19 <- to_numeric_with_na(sampling_frame$q19)
sampling_frame$q19[sampling_frame$q19 > 15] <- NA

# Head age uses household-head age when reported and respondent age otherwise.
sampling_frame$q17 <- to_numeric_with_na(sampling_frame$q17)
sampling_frame$q17[sampling_frame$q17 == 999] <- NA

sampling_frame$q14 <- to_numeric_with_na(sampling_frame$q14)
sampling_frame$q14[sampling_frame$q14 == 999] <- NA

sampling_frame$age_head <- ifelse(
  is.na(sampling_frame$q17),
  sampling_frame$q14,
  sampling_frame$q17
)

sampling_frame$q16[sampling_frame$q16 == "n/a"] <- NA
sampling_frame$q16 <- sampling_frame$q16 == "Male"
sampling_frame$q13[sampling_frame$q13 == "n/a"] <- NA
sampling_frame$q13 <- sampling_frame$q13 == "Male"

sampling_frame$male_head <- ifelse(
  is.na(sampling_frame$q16),
  sampling_frame$q13,
  sampling_frame$q16
)

sampling_frame$HH_size <- sampling_frame$q19
sampling_frame$poor <- sampling_frame$q69 == 4 | sampling_frame$q69 == 5

# df_balance mirrors the historical balance_2022.Rdata shape:
# columns 1-2: control mean and SD
# columns 3-5: first treatment contrast, SE, p-value
# columns 6-8: second treatment contrast, SE, p-value
# column 9: regression N
df_balance <- array(NA, dim = c(8, 9))
balance_vars <- c("age_head", "male_head", "HH_size", "land_size_ha", "poor")

# Keep the original additive indicators for the joint C/T1 and T1/T2 tests, but
# use mutually exclusive arm indicators for the displayed balance contrasts.
sampling_frame$treatment1 <- sampling_frame$treatment == "T1" |
  sampling_frame$treatment == "T2"
sampling_frame$treatment2 <- sampling_frame$treatment == "T2"
sampling_frame$t1_arm <- sampling_frame$treatment == "T1"
sampling_frame$t2_arm <- sampling_frame$treatment == "T2"

for (i in seq_along(balance_vars)) {
  y <- as.numeric(unlist(sampling_frame[balance_vars[i]]))

  df_balance[i, 1] <- round(mean(y[sampling_frame$treatment == "C"], na.rm = TRUE), 3)
  df_balance[i, 2] <- round(sd(y[sampling_frame$treatment == "C"], na.rm = TRUE), 3)

  fit <- lm(
    as.formula(paste(balance_vars[i], "t1_arm+t2_arm", sep = "~")),
    data = sampling_frame
  )
  vcov_cluster <- vcovCR(fit, cluster = sampling_frame$village, type = "CR3")
  coefs <- coef_test(fit, vcov_cluster)

  df_balance[i, 3] <- round(coefs$beta[2], 3)
  df_balance[i, 4] <- round(coefs$SE[2], 3)
  df_balance[i, 5] <- coefs$p_Satt[2]
  df_balance[i, 6] <- round(coefs$beta[3], 3)
  df_balance[i, 7] <- round(coefs$SE[3], 3)
  df_balance[i, 8] <- coefs$p_Satt[3]
  df_balance[i, 9] <- nobs(fit)
}

# Joint test across all three arms, matching the original PAP implementation.
complete_balance <- na.omit(
  sampling_frame[, c("treatment", balance_vars)]
)
null_mod <- multinom(treatment ~ 1, data = complete_balance, trace = FALSE)
alt_mod <- multinom(
  treatment ~ age_head + male_head + HH_size + land_size_ha + poor,
  data = sampling_frame,
  trace = FALSE
)
lr_res <- lrtest(alt_mod, null_mod)
df_balance[6, 1] <- round(lr_res[2, 4], 3)
df_balance[6, 2] <- round(lr_res[2, 5], 3)

# Pairwise balance tests used in the PAP table notes.
mod1 <- lm(
  (treatment == "T1") ~ age_head + male_head + HH_size + land_size_ha + poor,
  data = sampling_frame[sampling_frame$treatment %in% c("T1", "C"), ]
)
test_res <- linearHypothesis(
  mod1,
  c("age_head=0", "male_headTRUE=0", "HH_size=0", "land_size_ha=0", "poorTRUE=0")
)
df_balance[7, 1] <- round(test_res[2, 5], 3)
df_balance[7, 2] <- round(test_res[2, 6], 3)

mod1 <- lm(
  (treatment == "T2") ~ age_head + male_head + HH_size + land_size_ha + poor,
  data = sampling_frame[sampling_frame$treatment %in% c("T1", "T2"), ]
)
test_res <- linearHypothesis(
  mod1,
  c("age_head=0", "male_headTRUE=0", "HH_size=0", "land_size_ha=0", "poorTRUE=0")
)
df_balance[8, 1] <- round(test_res[2, 5], 3)
df_balance[8, 2] <- round(test_res[2, 6], 3)

reference_path <- file.path(dir_data, "balance_2022.Rdata")
if (file.exists(reference_path)) {
  reference_balance <- readRDS(reference_path)

  # The old reference object stores T2 - T1 in column 6. For coefficient values
  # only, T2 - Control can be reconstructed as (T1 - Control) + (T2 - T1).
  # The corresponding SE/p-value cannot be reconstructed from the rounded saved
  # object because the covariance between coefficients is not stored.
  additive_reference_as_arm_contrasts <- reference_balance
  additive_reference_as_arm_contrasts[1:5, 6] <-
    reference_balance[1:5, 3] + reference_balance[1:5, 6]
  comparison <- df_balance - additive_reference_as_arm_contrasts
  comparison[1:5, c(7, 8)] <- NA
  comparison_path <- file.path(dir_logs, "table1_balance_comparison.csv")
  write.csv(comparison, comparison_path, row.names = FALSE)
  if (max(abs(comparison), na.rm = TRUE) > 1.1e-3) {
    warning("Generated balance table differs from transformed balance_2022.Rdata; see ", comparison_path)
  } else {
    message("Generated balance table matches transformed balance_2022.Rdata for comparable quantities")
  }
}

row_labels <- c(
  "Household head age (years)",
  "Household head is male (1=yes)",
  "Household size (number)",
  "Land area (ha)",
  "Had difficulties feeding family in last year (1=yes)"
)

# Write the LaTeX fragment included by the manuscript.
table_rows <- character(0)
for (i in seq_along(row_labels)) {
  table_rows <- c(
    table_rows,
    paste0(
      row_labels[i], " & ",
      fmt_num(df_balance[i, 1]), " & ",
      fmt_num(df_balance[i, 3]), " & ",
      fmt_num(df_balance[i, 6]), " & ",
      as.integer(df_balance[i, 9]), " \\\\"
    ),
    paste0(
      " & (", fmt_num(df_balance[i, 2]), ") & (",
      fmt_num(df_balance[i, 4]), ") & (",
      fmt_num(df_balance[i, 7]), ") & \\\\"
    )
  )
}

tex <- c(
  "% Generated by replication_package/code/R/01_table1_balance.R",
  "\\begin{tabular}{lcccc}",
  "\\hline\\hline",
  " & Control mean & T1 - Control & T2 - Control & N \\\\",
  "\\cline{2-5}",
  table_rows,
  "\\hline",
  paste0(
    "Multinomial test (p-value) & ",
    fmt_num(df_balance[6, 1]), " & (", fmt_p(df_balance[6, 2]), ") & & \\\\"
  ),
  paste0(
    "F-test C/T1 (p-value) & ",
    fmt_num(df_balance[7, 1]), " & (", fmt_p(df_balance[7, 2]), ") & & \\\\"
  ),
  paste0(
    "F-test T1/T2 (p-value) & ",
    fmt_num(df_balance[8, 1]), " & (", fmt_p(df_balance[8, 2]), ") & & \\\\"
  ),
  "\\hline\\hline",
  "\\end{tabular}"
)

out_path <- file.path(dir_tables, "table1_balance.tex")
writeLines(tex, out_path)

message("Wrote ", out_path)
