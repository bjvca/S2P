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

fit_spec <- function(data, rhs_terms, column_label, outcome = "lnyield",
                     ctrl_mean_var = NULL) {
  if (is.null(ctrl_mean_var)) ctrl_mean_var <- outcome
  vars_needed <- unique(c(outcome, ctrl_mean_var, "lnyield", "yield_maize",
                          "treat_num", "cluster_id_num", rhs_terms))
  data <- data[complete.cases(data[, vars_needed]), ]

  model <- lm(reformulate(rhs_terms, response = outcome), data = data)
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
    outcome = outcome,
    n = nrow(data),
    clusters = cluster_count,
    control_mean = mean(data[[ctrl_mean_var]][data$treat == "C"], na.rm = TRUE),
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

# Panel A: total kg harvested (levels, winsorized)
spec_results_kg <- fit_spec(df, "treat_num", "(2)", "w_bags_Mcrp_maiz")

# Panel B: log yield (kg/acre)
spec_results_log <- fit_spec(df, "treat_num", "(2)", "lnyield", "yield_maize")

spec_results <- rbind(spec_results_kg, spec_results_log)

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

sens_rows <- c(
  "preferred_adjusted",
  "drop_top_yield_F_3183",
  "drop_yield_above_10000",
  "drop_yield_above_5000",
  "drop_maize_area_below_0.1"
)
sens_labels <- c(
  "Preferred adjusted specification",
  "Drop largest yield observation",
  "Drop yield above 10,000 kg/acre",
  "Drop yield above 5,000 kg/acre",
  "Drop maize area below 0.1 acre"
)
sens <- sensitivity_results[match(sens_rows, sensitivity_results$scenario), ]
sensitivity_table_lines <- c(
  "{",
  "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  "Scenario & N & T1 & T2 & p-value: T2 = T1 \\\\",
  "\\midrule",
  paste0(
    sens_labels,
    " & ",
    fmt_num(sens$n, 0),
    " & ",
    mapply(fmt_coef, sens$t1, sens$t1_p),
    " & ",
    mapply(fmt_coef, sens$t2, sens$t2_p),
    " & ",
    fmt_num(sens$p_equal, 3),
    " \\\\"
  ),
  "\\bottomrule",
  "\\end{tabular}",
  "}"
)
writeLines(
  sensitivity_table_lines,
  file.path(dir_tables, "table5_maize_yield_sensitivity.tex")
)

fmt_panel_row <- function(r, is_log) {
  pct <- if (is_log) {
    fmt_num(100 * (exp(r$t2) - 1), 1)
  } else {
    fmt_num(100 * r$t2 / r$control_mean, 1)
  }
  list(
    sprintf(
      "%s & %s & %s & %s & %s & %s \\\\",
      fmt_num(r$control_mean, ifelse(is_log, 2, 0)),
      fmt_coef(r$t1, r$t1_p),
      fmt_coef(r$t2, r$t2_p),
      pct,
      fmt_num(r$p_equal, 3),
      fmt_num(r$n, 0)
    ),
    sprintf(
      "& (%s) & (%s) & & & \\\\",
      fmt_num(r$t1_se, ifelse(is_log, 2, 0)),
      fmt_num(r$t2_se, ifelse(is_log, 2, 0))
    )
  )
}

table_lines <- c(
  "{",
  "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
  "\\begin{tabular}{rccccc}",
  "\\toprule",
  "Ctrl. mean & T1 $-$ C & T2 $-$ C & T2 \\% effect & $p$: T2 = T1 & N \\\\",
  "\\midrule",
  "\\multicolumn{6}{c}{\\textit{Panel A: Total kg harvested}} \\\\",
  unlist(lapply(seq_len(nrow(spec_results_kg)), function(i)
    fmt_panel_row(spec_results_kg[i, ], is_log = FALSE))),
  "\\midrule",
  "\\multicolumn{6}{c}{\\textit{Panel B: Log yield (kg/acre)}} \\\\",
  unlist(lapply(seq_len(nrow(spec_results_log)), function(i)
    fmt_panel_row(spec_results_log[i, ], is_log = TRUE))),
  "\\bottomrule",
  "\\end{tabular}",
  "}"
)

writeLines(table_lines, file.path(dir_tables, "table5_maize_yield.tex"))

message("Wrote maize-yield table to: ", file.path(dir_tables, "table5_maize_yield.tex"))
message("Wrote maize-yield log to: ", file.path(dir_logs, "table5_maize_yield.csv"))
message("Wrote top-yield diagnostics to: ", file.path(dir_logs, "table5_maize_yield_top_values.csv"))
message("Wrote yield sensitivity checks to: ", file.path(dir_logs, "table5_maize_yield_sensitivity.csv"))
message("Wrote yield sensitivity table to: ", file.path(dir_tables, "table5_maize_yield_sensitivity.tex"))
