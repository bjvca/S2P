# Generate the maize-yield impact table for the manuscript.
#
# This script audits the existing yield table and replaces the stale Stata table
# input with the same specification discipline used for the fertilizer tables:
#   - mutually exclusive Treatment 1 and Treatment 2 assignment indicators;
#   - unadjusted and preferred adjusted ITT columns;
#   - pre-treatment controls only in the adjusted column;
#   - education level, slope, soil structure, and seed type entered as factors;
#   - village-clustered standard errors;
#   - a direct test of whether Treatment 2 differs from Treatment 1.
#
# The earlier Stata table included agro_vis and ext_srv as controls. Those are
# not used in the preferred specification because they can respond to treatment.

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
df <- subset(df, treat %in% c("C", "T1", "T2") & main_crp == "MAIZE")

df$treat_num <- factor(df$treat_num, levels = c("C", "T1", "T2"))

# Normalize sentinel values and blanks before estimating adjusted columns.
df$dist_agro[df$dist_agro == 999] <- NA
df$plot_siz[df$plot_siz == 999] <- NA
df$hh_age[df$hh_age == 999] <- NA
df$hh_educ[df$hh_educ == ""] <- NA
df$slope[df$slope == ""] <- NA
df$soil_str[df$soil_str == ""] <- NA
df$soil_str[df$soil_str == "5"] <- "Other/unknown"

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

fit_spec <- function(data, rhs_terms, column_label) {
  vars_needed <- c("lnyield", "yield_maize", "treat_num", "cluster_id_num", rhs_terms)
  vars_needed <- unique(vars_needed)
  data <- data[complete.cases(data[, vars_needed]), ]

  model <- lm(reformulate(rhs_terms, response = "lnyield"), data = data)
  cluster_count <- length(unique(data$cluster_id_num))
  vcov_stage <- vcovCR(model, cluster = data$cluster_id_num, type = "CR1S")
  ct <- as.data.frame(coef_test(model, vcov = vcov_stage, test = "naive-t"))
  rownames(ct) <- ct$Coef

  t1_beta <- ct["treat_numT1", "beta"]
  t1_se <- ct["treat_numT1", "SE"]
  t2_beta <- ct["treat_numT2", "beta"]
  t2_se <- ct["treat_numT2", "SE"]

  df_cluster <- cluster_count - 1
  t1_p <- 2 * pt(abs(t1_beta / t1_se), df = df_cluster, lower.tail = FALSE)
  t2_p <- 2 * pt(abs(t2_beta / t2_se), df = df_cluster, lower.tail = FALSE)
  diff_beta <- t2_beta - t1_beta
  diff_se <- sqrt(
    vcov_stage["treat_numT2", "treat_numT2"] +
      vcov_stage["treat_numT1", "treat_numT1"] -
      2 * vcov_stage["treat_numT2", "treat_numT1"]
  )
  p_equal <- 2 * pt(abs(diff_beta / diff_se), df = df_cluster, lower.tail = FALSE)

  data.frame(
    column = column_label,
    n = nrow(data),
    clusters = cluster_count,
    control_mean_log = mean(data$lnyield[data$treat == "C"], na.rm = TRUE),
    control_mean_level = mean(data$yield_maize[data$treat == "C"], na.rm = TRUE),
    t1 = t1_beta,
    t1_se = t1_se,
    t1_p = t1_p,
    t2 = t2_beta,
    t2_se = t2_se,
    t2_p = t2_p,
    p_equal = p_equal,
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
  fit_spec(df, "treat_num", "(1)"),
  fit_spec(df, c("treat_num", preferred_controls), "(2)")
)

write.csv(
  spec_results,
  file.path(dir_logs, "table5_maize_yield.csv"),
  row.names = FALSE
)

top_yields <- subset(
  df,
  !is.na(yield_maize),
  select = c(farmer_id, treat, main_crp, bags_Mcrp, maize_area, yield_maize, lnyield)
)
top_yields <- top_yields[order(-top_yields$yield_maize), ]
write.csv(
  head(top_yields, 25),
  file.path(dir_logs, "table5_maize_yield_top_values.csv"),
  row.names = FALSE
)

sensitivity_results <- rbind(
  cbind(
    scenario = "preferred_adjusted",
    fit_spec(df, c("treat_num", preferred_controls), "(2)")
  ),
  cbind(
    scenario = "drop_top_yield_F_3183",
    fit_spec(subset(df, farmer_id != "F_3183"), c("treat_num", preferred_controls), "(2)")
  ),
  cbind(
    scenario = "drop_yield_above_10000",
    fit_spec(subset(df, yield_maize <= 10000 | is.na(yield_maize)), c("treat_num", preferred_controls), "(2)")
  ),
  cbind(
    scenario = "drop_yield_above_5000",
    fit_spec(subset(df, yield_maize <= 5000 | is.na(yield_maize)), c("treat_num", preferred_controls), "(2)")
  ),
  cbind(
    scenario = "drop_maize_area_below_0.1",
    fit_spec(subset(df, maize_area >= 0.1 | is.na(maize_area)), c("treat_num", preferred_controls), "(2)")
  )
)
write.csv(
  sensitivity_results,
  file.path(dir_logs, "table5_maize_yield_sensitivity.csv"),
  row.names = FALSE
)

table_lines <- c(
  "{",
  "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
  "\\begin{tabular}{l*{2}{c}}",
  "\\toprule",
  "& \\multicolumn{2}{c}{Log maize yield} \\\\",
  "\\cmidrule(lr){2-3}",
  "& (1) & (2) \\\\",
  "\\midrule",
  sprintf(
    "Treatment one & %s & %s \\\\",
    fmt_coef(spec_results$t1[1], spec_results$t1_p[1]),
    fmt_coef(spec_results$t1[2], spec_results$t1_p[2])
  ),
  sprintf(
    "& (%s) & (%s) \\\\",
    fmt_num(spec_results$t1_se[1], 2),
    fmt_num(spec_results$t1_se[2], 2)
  ),
  sprintf(
    "Treatment two & %s & %s \\\\",
    fmt_coef(spec_results$t2[1], spec_results$t2_p[1]),
    fmt_coef(spec_results$t2[2], spec_results$t2_p[2])
  ),
  sprintf(
    "& (%s) & (%s) \\\\",
    fmt_num(spec_results$t2_se[1], 2),
    fmt_num(spec_results$t2_se[2], 2)
  ),
  sprintf(
    "p-value: T2 = T1 & %s & %s \\\\",
    fmt_num(spec_results$p_equal[1], 3),
    fmt_num(spec_results$p_equal[2], 3)
  ),
  sprintf(
    "Control mean, log yield & %s & %s \\\\",
    fmt_num(spec_results$control_mean_log[1], 2),
    fmt_num(spec_results$control_mean_log[2], 2)
  ),
  sprintf(
    "Control mean, yield & %s & %s \\\\",
    fmt_num(spec_results$control_mean_level[1], 1),
    fmt_num(spec_results$control_mean_level[2], 1)
  ),
  "\\midrule",
  sprintf(
    "Pre-treatment controls & %s & %s \\\\",
    spec_results$uses_controls[1],
    spec_results$uses_controls[2]
  ),
  sprintf(
    "Number of observations & %s & %s \\\\",
    fmt_num(spec_results$n[1], 0),
    fmt_num(spec_results$n[2], 0)
  ),
  "\\bottomrule",
  "\\end{tabular}",
  "}"
)

writeLines(table_lines, file.path(dir_tables, "table5_maize_yield.tex"))

message("Wrote maize-yield table to: ", file.path(dir_tables, "table5_maize_yield.tex"))
message("Wrote maize-yield log to: ", file.path(dir_logs, "table5_maize_yield.csv"))
message("Wrote top-yield diagnostics to: ", file.path(dir_logs, "table5_maize_yield_top_values.csv"))
message("Wrote yield sensitivity checks to: ", file.path(dir_logs, "table5_maize_yield_sensitivity.csv"))
