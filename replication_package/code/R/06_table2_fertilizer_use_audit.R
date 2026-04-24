# Independent audit of the first main fertilizer-use table in the paper.
#
# This script does not rely on coauthor table-generation code. It starts from
# the public cleaned analysis extract and asks what the paper table appears to
# be estimating. The goal is to catch hidden sample definitions and undocumented
# cleaning decisions before we write the final fertilizer-use section.
#
# Findings at the time this script was written:
#   - Column (1) in the paper is reproducible from total_qty_fert using the
#     full treatment-coded endline master sample (N = 1953), with non-interview
#     observations carrying zero fertilizer use.
#   - Column (3) is reproducible from the same outcome restricted to households
#     whose main crop is maize (N = 1626).
#   - Column (4) is approximately reproducible from the interviewed maize-main-
#     crop sample with the stated controls.
#   - Column (2) is not reproducible from the public merged file under the same
#     control set, which suggests an undocumented cleaning or outcome-construction
#     rule in the current paper workflow. The diagnostics written here are meant
#     to isolate that discrepancy rather than hide it.

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
})

dir_repo <- normalizePath(file.path(replication_root, ".."), mustWork = TRUE)
paper_root <- file.path(dir_repo, "paper")
endline_path <- file.path(dir_repo, "endline", "data", "public", "clear_merged_data.csv")

df <- read.csv(endline_path, stringsAsFactors = FALSE)
df <- subset(df, treat %in% c("C", "T1", "T2"))

df$t1 <- as.integer(df$treat == "T1")
df$t2 <- as.integer(df$treat == "T2")

controls <- c(
  "hh_size",
  "hh_age",
  "hh_educ",
  "dist_agro",
  "plot_siz",
  "agro_vis",
  "ext_srv",
  "slope",
  "soil_str",
  "seed_typ_num"
)

run_spec <- function(data, outcome, rhs_terms, sample_label, column_label) {
  formula <- reformulate(rhs_terms, response = outcome)
  model <- lm(formula, data = data)
  vcov_stage <- vcovCR(model, cluster = data$cluster_id_num, type = "CR2")
  ct <- as.data.frame(coef_test(model, vcov = vcov_stage, test = "Satterthwaite"))
  rownames(ct) <- ct$Coef

  data.frame(
    column = column_label,
    sample = sample_label,
    outcome = outcome,
    controls = paste(rhs_terms[-c(1, 2)], collapse = ", "),
    n = nrow(data),
    control_mean = mean(data[data$treat == "C", outcome], na.rm = TRUE),
    intercept = ct["(Intercept)", "beta"],
    intercept_se = ct["(Intercept)", "SE"],
    t1 = ct["t1", "beta"],
    t1_se = ct["t1", "SE"],
    t2 = ct["t2", "beta"],
    t2_se = ct["t2", "SE"],
    stringsAsFactors = FALSE
  )
}

# Candidate specifications inferred from the paper table itself.
spec_results <- list()

spec_results[[1]] <- run_spec(
  data = df,
  outcome = "total_qty_fert",
  rhs_terms = c("t1", "t2"),
  sample_label = "Full treatment-coded endline master sample",
  column_label = "Independent col1"
)

spec_results[[2]] <- run_spec(
  data = subset(df, checkq9 == "Yes"),
  outcome = "total_qty_fert",
  rhs_terms = c("t1", "t2", controls),
  sample_label = "Interview-complete sample",
  column_label = "Independent col2"
)

spec_results[[3]] <- run_spec(
  data = subset(df, main_crp == "MAIZE"),
  outcome = "total_qty_fert",
  rhs_terms = c("t1", "t2"),
  sample_label = "Main crop is maize",
  column_label = "Independent col3"
)

spec_results[[4]] <- run_spec(
  data = subset(df, checkq9 == "Yes" & main_crp == "MAIZE"),
  outcome = "total_qty_fert",
  rhs_terms = c("t1", "t2", controls),
  sample_label = "Interview-complete sample, main crop is maize",
  column_label = "Independent col4"
)

spec_results <- do.call(rbind, spec_results)

# Enter the current paper values explicitly so the discrepancy is documented
# in machine-readable form.
paper_values <- data.frame(
  column = c("Paper col1", "Paper col2", "Paper col3", "Paper col4"),
  sample = c(
    "Reported N = 1953",
    "Reported N = 1869",
    "Reported N = 1626",
    "Reported N = 1626"
  ),
  outcome = "Table 2 paper values",
  controls = c("No", "Yes", "No", "Yes"),
  n = c(1953, 1869, 1626, 1626),
  control_mean = c(78.46, 132.30, 83.36, 22.29),
  intercept = c(78.46, 132.30, 83.36, 22.29),
  intercept_se = c(5.84, 72.77, 5.59, 10.66),
  t1 = c(150.57, 52.77, 3.37, 2.99),
  t1_se = c(147.71, 145.49, 6.87, 6.56),
  t2 = c(249.05, 147.35, 28.60, 35.07),
  t2_se = c(217.73, 156.70, 6.01, 5.61),
  stringsAsFactors = FALSE
)

audit_results <- rbind(paper_values, spec_results)

write.csv(
  audit_results,
  file.path(dir_logs, "table2_fertilizer_use_audit.csv"),
  row.names = FALSE
)

# Document the outliers that drive the all-crops controlled specification.
interviewed <- subset(df, checkq9 == "Yes")
outliers <- interviewed[order(interviewed$total_qty_fert, decreasing = TRUE), c(
  "farmer_id",
  "treat",
  "main_crp",
  "total_qty_fert",
  "plot_siz",
  "maize_area",
  "qty_used",
  "ferti1",
  "ferti2",
  "ferti3",
  "ferti4"
)]
outliers <- outliers[seq_len(min(25, nrow(outliers))), ]

write.csv(
  outliers,
  file.path(dir_logs, "table2_fertilizer_outliers.csv"),
  row.names = FALSE
)

# Sensitivity of the all-crops controlled coefficient to simple trimming/capping.
sensitivity_caps <- c(1000, 1050, 1500, 2000, 5000, 10000, 25000, 50000, 100000)
sensitivity <- vector("list", length(sensitivity_caps))

for (i in seq_along(sensitivity_caps)) {
  cap <- sensitivity_caps[i]
  d_cap <- interviewed
  d_cap$y_cap <- pmin(d_cap$total_qty_fert, cap)
  model <- lm(reformulate(c("t1", "t2", controls), response = "y_cap"), data = d_cap)
  vcov_cap <- vcovCR(model, cluster = d_cap$cluster_id_num, type = "CR2")
  ct <- as.data.frame(coef_test(model, vcov = vcov_cap, test = "Satterthwaite"))
  rownames(ct) <- ct$Coef

  sensitivity[[i]] <- data.frame(
    rule = paste0("cap_at_", cap),
    n = nrow(d_cap),
    t1 = ct["t1", "beta"],
    t1_se = ct["t1", "SE"],
    t2 = ct["t2", "beta"],
    t2_se = ct["t2", "SE"],
    stringsAsFactors = FALSE
  )
}

sensitivity <- do.call(rbind, sensitivity)

write.csv(
  sensitivity,
  file.path(dir_logs, "table2_fertilizer_sensitivity.csv"),
  row.names = FALSE
)

# Write a compact LaTeX audit table for quick inspection.
audit_lines <- c(
  "\\begin{tabular}{lrrrr}",
  "\\hline",
  "Specification & N & T1 & T2 & Outcome/sample interpretation \\\\",
  "\\hline"
)

for (i in seq_len(nrow(spec_results))) {
  row <- spec_results[i, ]
  label <- switch(
    row$column,
    "Independent col1" = "Independent col. (1)",
    "Independent col2" = "Independent col. (2)",
    "Independent col3" = "Independent col. (3)",
    "Independent col4" = "Independent col. (4)"
  )
  interp <- switch(
    row$column,
    "Independent col1" = "total_qty_fert, full 1953 sample",
    "Independent col2" = "total_qty_fert, interviewed sample + controls",
    "Independent col3" = "total_qty_fert, maize-main-crop sample",
    "Independent col4" = "total_qty_fert, maize-main-crop interviewed sample + controls"
  )
  audit_lines <- c(
    audit_lines,
    sprintf(
      "%s & %s & %s & %s & %s \\\\",
      label,
      fmt_num(row$n, 0),
      fmt_num(row$t1, 2),
      fmt_num(row$t2, 2),
      interp
    )
  )
}

audit_lines <- c(
  audit_lines,
  "\\hline",
  "\\end{tabular}"
)

writeLines(audit_lines, file.path(dir_tables, "table2_fertilizer_use_audit.tex"))

message("Wrote fertilizer-use audit to: ", file.path(dir_logs, "table2_fertilizer_use_audit.csv"))
message("Wrote fertilizer-use outlier log to: ", file.path(dir_logs, "table2_fertilizer_outliers.csv"))
message("Wrote fertilizer-use sensitivity log to: ", file.path(dir_logs, "table2_fertilizer_sensitivity.csv"))
