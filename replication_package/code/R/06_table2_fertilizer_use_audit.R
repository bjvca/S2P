# Generate the first main fertilizer-use table for the manuscript.
#
# This script replaces the earlier "audit-only" version with a table generator
# that uses the specification we can defend in the paper:
#   - unadjusted ITT columns for all crops and maize only;
#   - adjusted columns that include pre-treatment household and plot covariates
#     only;
#   - categorical covariates entered as factors rather than as ad hoc numeric
#     scores.
#
# The earlier version of this script treated the controlled specification too
# loosely. We keep the original control block below, commented out, because the
# mistakes are substantive and should remain visible in the code review trail.
#
# Mistakes in the earlier approach:
#   1. It included agro_vis and ext_srv, which are treatment-affected and
#      therefore inappropriate as preferred controls in an ITT regression.
#   2. It passed hh_educ, slope, soil_str, and seed_typ_num as if they were
#      continuous, even though the questionnaire and the exported CSV make clear
#      that they are categorical.
#
# Original control block kept for documentation only:
# mistaken_controls <- c(
#   "hh_size",
#   "hh_age",
#   "hh_educ",
#   "dist_agro",
#   "plot_siz",
#   "agro_vis",
#   "ext_srv",
#   "slope",
#   "soil_str",
#   "seed_typ_num"
# )
#
# Original mistaken formula kept for documentation only:
# lm(
#   total_qty_fert ~ t1 + t2 + hh_size + hh_age + hh_educ + dist_agro +
#     plot_siz + agro_vis + ext_srv + slope + soil_str + seed_typ_num,
#   data = df
# )

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
endline_path <- file.path(dir_repo, "endline", "data", "public", "clear_merged_data.csv")

df <- read.csv(endline_path, stringsAsFactors = FALSE)
df <- subset(df, treat %in% c("C", "T1", "T2"))

df$treat_num <- factor(df$treat_num, levels = c("C", "T1", "T2"))
df$main_maize <- df$main_crp == "MAIZE"

# Sentinel values and blank strings are exported from Stata as literal values
# in the CSV. They need to be normalized before the adjusted specification is
# estimated in R.
df$dist_agro[df$dist_agro == 999] <- NA
df$plot_siz[df$plot_siz == 999] <- NA
df$hh_educ[df$hh_educ == ""] <- NA
df$slope[df$slope == ""] <- NA
df$soil_str[df$soil_str == ""] <- NA
df$soil_str[df$soil_str == "5"] <- "Other/unknown"

# The questionnaire records education level, slope, soil structure, and seed
# type as categories. Enter them as factors rather than imposing a linear score.
df$hh_educ <- factor(df$hh_educ)
df$slope <- factor(df$slope)
df$soil_str <- factor(df$soil_str)
df$seed_typ_num <- factor(df$seed_typ_num)

preferred_controls <- c(
  "hh_size",
  "hh_age",
  "hh_educ",
  "dist_agro",
  "plot_siz",
  "slope",
  "soil_str",
  "seed_typ_num"
)

fit_spec <- function(data, rhs_terms, sample_label, column_label) {
  vars_needed <- c("total_qty_fert", "treat_num", "cluster_id_num", rhs_terms)
  vars_needed <- unique(vars_needed)
  data <- data[complete.cases(data[, vars_needed]), ]

  model <- lm(reformulate(rhs_terms, response = "total_qty_fert"), data = data)
  cluster_count <- length(unique(data$cluster_id_num))
  vcov_stage <- vcovCR(model, cluster = data$cluster_id_num, type = "CR1S")
  ct <- as.data.frame(coef_test(model, vcov = vcov_stage, test = "naive-t"))
  rownames(ct) <- ct$Coef

  t1_beta <- ct["treat_numT1", "beta"]
  t1_se <- ct["treat_numT1", "SE"]
  t2_beta <- ct["treat_numT2", "beta"]
  t2_se <- ct["treat_numT2", "SE"]

  # Match Stata's clustered t reference by using G-1 degrees of freedom.
  df_cluster <- cluster_count - 1
  t1_p <- 2 * pt(abs(t1_beta / t1_se), df = df_cluster, lower.tail = FALSE)
  t2_p <- 2 * pt(abs(t2_beta / t2_se), df = df_cluster, lower.tail = FALSE)

  data.frame(
    column = column_label,
    sample = sample_label,
    n = nrow(data),
    clusters = cluster_count,
    control_mean = mean(data$total_qty_fert[data$treat == "C"], na.rm = TRUE),
    t1 = t1_beta,
    t1_se = t1_se,
    t1_p = t1_p,
    t2 = t2_beta,
    t2_se = t2_se,
    t2_p = t2_p,
    uses_controls = ifelse(length(rhs_terms) > 1, "Yes", "No"),
    stringsAsFactors = FALSE
  )
}

star_code <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.01) return("\\sym{***}")
  if (p_value < 0.05) return("\\sym{**}")
  if (p_value < 0.10) return("\\sym{*}")
  ""
}

fmt_coef <- function(beta, p_value) {
  paste0(fmt_num(beta, 2), star_code(p_value))
}

spec_results <- rbind(
  fit_spec(
    data = df,
    rhs_terms = "treat_num",
    sample_label = "All crops",
    column_label = "(1)"
  ),
  fit_spec(
    data = df,
    rhs_terms = c("treat_num", preferred_controls),
    sample_label = "All crops",
    column_label = "(2)"
  ),
  fit_spec(
    data = subset(df, main_maize),
    rhs_terms = "treat_num",
    sample_label = "Maize only",
    column_label = "(3)"
  ),
  fit_spec(
    data = subset(df, main_maize),
    rhs_terms = c("treat_num", preferred_controls),
    sample_label = "Maize only",
    column_label = "(4)"
  )
)

write.csv(
  spec_results,
  file.path(dir_logs, "table2_fertilizer_use.csv"),
  row.names = FALSE
)

table_lines <- c(
  "{",
  "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
  "\\begin{tabular}{l*{4}{c}}",
  "\\toprule",
  "&\\multicolumn{2}{c}{All crops} & \\multicolumn{2}{c}{Maize only} \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
  "& (1) & (2) & (3) & (4) \\\\",
  "\\midrule",
  sprintf(
    "Treatment one & %s & %s & %s & %s \\\\",
    fmt_coef(spec_results$t1[1], spec_results$t1_p[1]),
    fmt_coef(spec_results$t1[2], spec_results$t1_p[2]),
    fmt_coef(spec_results$t1[3], spec_results$t1_p[3]),
    fmt_coef(spec_results$t1[4], spec_results$t1_p[4])
  ),
  sprintf(
    "& (%s) & (%s) & (%s) & (%s) \\\\",
    fmt_num(spec_results$t1_se[1], 2),
    fmt_num(spec_results$t1_se[2], 2),
    fmt_num(spec_results$t1_se[3], 2),
    fmt_num(spec_results$t1_se[4], 2)
  ),
  sprintf(
    "Treatment two & %s & %s & %s & %s \\\\",
    fmt_coef(spec_results$t2[1], spec_results$t2_p[1]),
    fmt_coef(spec_results$t2[2], spec_results$t2_p[2]),
    fmt_coef(spec_results$t2[3], spec_results$t2_p[3]),
    fmt_coef(spec_results$t2[4], spec_results$t2_p[4])
  ),
  sprintf(
    "& (%s) & (%s) & (%s) & (%s) \\\\",
    fmt_num(spec_results$t2_se[1], 2),
    fmt_num(spec_results$t2_se[2], 2),
    fmt_num(spec_results$t2_se[3], 2),
    fmt_num(spec_results$t2_se[4], 2)
  ),
  sprintf(
    "Control mean & %s & %s & %s & %s \\\\",
    fmt_num(spec_results$control_mean[1], 2),
    fmt_num(spec_results$control_mean[2], 2),
    fmt_num(spec_results$control_mean[3], 2),
    fmt_num(spec_results$control_mean[4], 2)
  ),
  "\\midrule",
  sprintf(
    "Pre-treatment controls & %s & %s & %s & %s \\\\",
    spec_results$uses_controls[1],
    spec_results$uses_controls[2],
    spec_results$uses_controls[3],
    spec_results$uses_controls[4]
  ),
  sprintf(
    "Number of observations & %s & %s & %s & %s \\\\",
    fmt_num(spec_results$n[1], 0),
    fmt_num(spec_results$n[2], 0),
    fmt_num(spec_results$n[3], 0),
    fmt_num(spec_results$n[4], 0)
  ),
  "\\bottomrule",
  "\\end{tabular}",
  "}"
)

writeLines(table_lines, file.path(dir_tables, "table2_fertilizer_use.tex"))

message("Wrote fertilizer-use table to: ", file.path(dir_tables, "table2_fertilizer_use.tex"))
message("Wrote fertilizer-use log to: ", file.path(dir_logs, "table2_fertilizer_use.csv"))
