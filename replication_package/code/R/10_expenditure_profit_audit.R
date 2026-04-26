# Audit farm expenditure and profit outcomes.
#
# This script does not yet replace the manuscript expenditure/profit tables.
# It documents the main specification choices that need to be settled before
# those tables are submission-ready:
#   - the current log total-expenditure variable is only defined when
#     non-fertilizer expenditure is positive;
#   - log profit drops households with non-positive profits;
#   - the old Stata table includes agro_vis and ext_srv, which can respond to
#     treatment and are therefore excluded from the preferred adjusted audit.

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

# Alternative outcome codings used only for audit diagnostics.
df$ln_total_farm_exp_positive <- ifelse(
  df$w_total_farm_exp > 0,
  log(df$w_total_farm_exp),
  NA
)
df$ln_total_farm_exp_exc_fert_positive <- ifelse(
  df$w_total_farm_exp_exc_fert > 0,
  log(df$w_total_farm_exp_exc_fert),
  NA
)
df$asinh_farm_profits <- asinh(df$farm_profits)

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

fit_spec <- function(data, outcome, rhs_terms, controls_label) {
  vars_needed <- c(outcome, "treat_num", "cluster_id_num", rhs_terms)
  vars_needed <- unique(vars_needed)
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
    outcome = outcome,
    controls = controls_label,
    n = nrow(data),
    clusters = cluster_count,
    control_mean = mean(data[[outcome]][data$treat == "C"], na.rm = TRUE),
    t1 = t1_beta,
    t1_se = t1_se,
    t1_p = t1_p,
    t2 = t2_beta,
    t2_se = t2_se,
    t2_p = t2_p,
    p_equal = p_equal,
    stringsAsFactors = FALSE
  )
}

outcomes <- c(
  "ln_total_farm_exp",
  "ln_total_farm_exp_positive",
  "ln_total_farm_exp_exc_fert",
  "ln_farm_profits",
  "asinh_farm_profits",
  "farm_profits"
)

audit_results <- do.call(
  rbind,
  lapply(outcomes, function(outcome) {
    rbind(
      fit_spec(df, outcome, "treat_num", "No"),
      fit_spec(df, outcome, c("treat_num", preferred_controls), "Yes")
    )
  })
)

write.csv(
  audit_results,
  file.path(dir_logs, "expenditure_profit_audit.csv"),
  row.names = FALSE
)

diagnostic_vars <- c(
  "w_total_farm_exp",
  "w_total_farm_exp_exc_fert",
  "farm_profits",
  "ln_total_farm_exp",
  "ln_total_farm_exp_positive",
  "ln_farm_profits"
)

diagnostics <- do.call(
  rbind,
  lapply(diagnostic_vars, function(v) {
    do.call(
      rbind,
      lapply(split(df[[v]], df$treat), function(x) {
        data.frame(
          variable = v,
          n = length(x),
          nonmissing = sum(!is.na(x)),
          positive = sum(x > 0, na.rm = TRUE),
          mean = mean(x, na.rm = TRUE),
          median = median(x, na.rm = TRUE),
          stringsAsFactors = FALSE
        )
      })
    )
  })
)
diagnostics$treat <- rep(names(split(df$treat, df$treat)), length(diagnostic_vars))
diagnostics <- diagnostics[, c("variable", "treat", "n", "nonmissing", "positive", "mean", "median")]

write.csv(
  diagnostics,
  file.path(dir_logs, "expenditure_profit_sample_diagnostics.csv"),
  row.names = FALSE
)

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

preferred <- subset(audit_results, controls == "Yes")
preferred <- preferred[match(outcomes, preferred$outcome), ]
labels <- c(
  "Log expenditure, current coding",
  "Log expenditure, positive total",
  "Log expenditure excluding fertilizer",
  "Log profits, positive profits only",
  "Asinh profits",
  "Profits, levels"
)

table_lines <- c(
  "{",
  "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  "Outcome & N & T1 & T2 & p-value: T2 = T1 \\\\",
  "\\midrule",
  paste0(
    labels,
    " & ",
    fmt_num(preferred$n, 0),
    " & ",
    mapply(fmt_coef, preferred$t1, preferred$t1_p),
    " & ",
    mapply(fmt_coef, preferred$t2, preferred$t2_p),
    " & ",
    fmt_num(preferred$p_equal, 3),
    " \\\\"
  ),
  "\\bottomrule",
  "\\end{tabular}",
  "}"
)

writeLines(table_lines, file.path(dir_tables, "expenditure_profit_audit.tex"))

message("Wrote expenditure/profit audit to: ", file.path(dir_logs, "expenditure_profit_audit.csv"))
message("Wrote expenditure/profit diagnostics to: ", file.path(dir_logs, "expenditure_profit_sample_diagnostics.csv"))
message("Wrote expenditure/profit audit table to: ", file.path(dir_tables, "expenditure_profit_audit.tex"))
