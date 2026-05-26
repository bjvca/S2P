# AIP substitution table for the manuscript: four panels (binary receipt,
# test-plot AIP, random-plot AIP, two-plot AIP), each in the same row format
# as existing tables (All crops / Maize only, with and without controls).
#
# Panel A — Binary AIP receipt indicator (aip_rec).
#   All crops:  full sample.
#   Maize only: subset to main_crp == "MAIZE".
#
# Panel B — AIP kg applied on the test plot (total_qty_fert_AIP).
#   All crops:  full sample.
#   Maize only: subset to main_crp == "MAIZE".
#
# Panel C — AIP kg applied on the random plot (sum of rnd_plot{1-3}qty_fert_aipr).
#   All crops:  restricted to other_plots = Yes (random plot exists).
#   Maize only: restricted to main_crp == "MAIZE" AND rnd_plotmain_crpr == "MAIZE"
#               (both test and random plots are maize, so the comparison is
#               apples-to-apples with the test-plot maize result).
#
# Panel D — Two-plot AIP kg (test + random, no no_plots multiplier).
#   All crops:  test_aip + rnd_aip on full sample.
#   Maize only: ifelse(main_maize, test_aip, 0) + ifelse(rnd_is_maize, rnd_aip, 0)
#               on full sample.

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

df$dist_agro[df$dist_agro == 999] <- NA
df$plot_siz[df$plot_siz == 999] <- NA
df$hh_educ[df$hh_educ == ""] <- NA
df$slope[df$slope == ""] <- NA
df$soil_str[df$soil_str == ""] <- NA
df$soil_str[df$soil_str == "5"] <- "Other/unknown"

df$hh_educ <- factor(df$hh_educ)
df$slope <- factor(df$slope)
df$soil_str <- factor(df$soil_str)
df$seed_typ_num <- factor(df$seed_typ_num)

coerce_binary <- function(x) {
  if (is.numeric(x)) {
    out <- x; out[!(out %in% c(0, 1)) & !is.na(out)] <- NA; return(out)
  }
  out <- rep(NA_real_, length(x))
  yes_codes <- c("1", "yes", "Yes", "YES", "y", "Y", "TRUE", "True", "true")
  no_codes  <- c("0", "no",  "No",  "NO",  "n", "N", "FALSE", "False", "false", "2")
  out[x %in% yes_codes] <- 1
  out[x %in% no_codes]  <- 0
  out
}

df$aip_rec_bin <- coerce_binary(df$aip_rec)
df$other_plots_bin <- coerce_binary(df$other_plots)
df$rnd_is_maize <- df$rnd_plotmain_crpr == "MAIZE"
df$rnd_is_maize[is.na(df$rnd_is_maize)] <- FALSE

df$test_aip <- df$total_qty_fert_AIP
df$test_aip[is.na(df$test_aip) & df$aip_rec_bin == 0] <- 0

rnd_cols <- c("rnd_plot1qty_fert_aipr", "rnd_plot2qty_fert_aipr", "rnd_plot3qty_fert_aipr")
df$rnd_aip <- rowSums(df[, rnd_cols], na.rm = TRUE)
all_rnd_na <- rowSums(is.na(df[, rnd_cols])) == length(rnd_cols)
df$rnd_aip[all_rnd_na] <- 0

# AIP is capped at 100 kg per household (one standard package: ~50 kg NPK +
# ~50 kg urea). Plot-level reports above 100 kg are recall errors and
# household totals above 100 kg are infeasible. Cap accordingly.
df$test_aip <- pmin(df$test_aip, 100)
df$rnd_aip  <- pmin(df$rnd_aip,  100)

# Two-plot, all crops, capped at 100 kg (the household ceiling)
df$two_plot_aip_all <- pmin(df$test_aip + df$rnd_aip, 100)

# Two-plot, maize only (zero out non-maize contributions), then capped at 100
df$two_plot_aip_maize <- pmin(
  ifelse(df$main_maize,    df$test_aip, 0) +
  ifelse(df$rnd_is_maize,  df$rnd_aip,  0),
  100
)

preferred_controls <- c(
  "hh_size", "hh_age", "hh_educ", "dist_agro",
  "plot_siz", "slope", "soil_str", "seed_typ_num"
)

fit_one <- function(data, response, rhs_terms, sample_label, uses_controls, panel_label) {
  vars_needed <- c(response, "treat_num", "cluster_id_num", rhs_terms)
  vars_needed <- unique(vars_needed)
  data <- data[complete.cases(data[, vars_needed]), ]

  model <- lm(reformulate(rhs_terms, response = response), data = data)
  cluster_count <- length(unique(data$cluster_id_num))
  vcov_stage <- vcovCR(model, cluster = data$cluster_id_num, type = "CR1S")
  ct <- as.data.frame(coef_test(model, vcov = vcov_stage, test = "naive-t"))
  rownames(ct) <- ct$Coef

  t1_beta <- ct["treat_numT1", "beta"]; t1_se <- ct["treat_numT1", "SE"]
  t2_beta <- ct["treat_numT2", "beta"]; t2_se <- ct["treat_numT2", "SE"]

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
    panel = panel_label,
    sample = sample_label,
    uses_controls = uses_controls,
    n = nrow(data),
    control_mean = mean(data[[response]][data$treat == "C"], na.rm = TRUE),
    t1 = t1_beta, t1_se = t1_se, t1_p = t1_p,
    t2 = t2_beta, t2_se = t2_se, t2_p = t2_p,
    p_equal = p_equal,
    stringsAsFactors = FALSE
  )
}

# Each panel produces 4 rows: All-crops No, All-crops Yes, Maize-only No, Maize-only Yes
panel_rows <- function(response, all_crops_data, maize_data, panel_label) {
  rbind(
    fit_one(all_crops_data, response, "treat_num",                       "All crops",  "No",  panel_label),
    fit_one(all_crops_data, response, c("treat_num", preferred_controls), "All crops",  "Yes", panel_label),
    fit_one(maize_data,     response, "treat_num",                       "Maize only", "No",  panel_label),
    fit_one(maize_data,     response, c("treat_num", preferred_controls), "Maize only", "Yes", panel_label)
  )
}

all_results <- rbind(
  panel_rows("aip_rec_bin", df,                                            subset(df, main_maize),
             "Panel A: Binary AIP receipt indicator"),
  panel_rows("test_aip",    df,                                            subset(df, main_maize),
             "Panel B: AIP fertilizer applied on the test plot (kg)"),
  panel_rows("rnd_aip",     subset(df, other_plots_bin == 1),              subset(df, main_maize & rnd_is_maize),
             "Panel C: AIP fertilizer applied on the random plot (kg)"),
  panel_rows("two_plot_aip_all", df, df,
             "Panel D: Two-plot AIP fertilizer total (kg)")
)
# Override Panel D maize-only rows: restrict sample to maize test plots and
# use the maize-conditioned outcome (test_aip + rnd_aip*rnd_is_maize).
panel_d_maize <- rbind(
  fit_one(subset(df, main_maize), "two_plot_aip_maize", "treat_num",                       "Maize only", "No",  "Panel D: Two-plot AIP fertilizer total (kg)"),
  fit_one(subset(df, main_maize), "two_plot_aip_maize", c("treat_num", preferred_controls), "Maize only", "Yes", "Panel D: Two-plot AIP fertilizer total (kg)")
)
panel_d_maize_idx <- which(all_results$panel == "Panel D: Two-plot AIP fertilizer total (kg)" &
                           all_results$sample == "Maize only")
all_results[panel_d_maize_idx, ] <- panel_d_maize

write.csv(all_results,
          file.path(dir_logs, "table_aip_substitution.csv"),
          row.names = FALSE)

# LaTeX rendering
star_code <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.01) return("\\sym{***}")
  if (p_value < 0.05) return("\\sym{**}")
  if (p_value < 0.10) return("\\sym{*}")
  ""
}

fmt_coef <- function(beta, p_value, digits = 2) {
  paste0(fmt_num(beta, digits), star_code(p_value))
}

outcome_row <- function(label, r, digits) {
  c(
    sprintf(
      "%s & %s & %s & %s & %s & %s \\\\",
      label,
      fmt_num(r$control_mean, digits),
      fmt_coef(r$t1, r$t1_p, digits),
      fmt_coef(r$t2, r$t2_p, digits),
      fmt_num(r$p_equal, 3),
      fmt_num(r$n, 0)
    ),
    sprintf(
      "& & (%s) & (%s) & & \\\\",
      fmt_num(r$t1_se, digits),
      fmt_num(r$t2_se, digits)
    )
  )
}

get_row <- function(panel_str, sample_str) {
  subset(all_results, grepl(panel_str, panel) & sample == sample_str & uses_controls == "No")
}

rows_tex <- c(
  outcome_row("AIP receipt (0/1)",          get_row("^Panel A", "All crops"), 3),
  "\\midrule",
  outcome_row("AIP on test plot (kg)",       get_row("^Panel B", "All crops"), 2),
  "\\midrule",
  outcome_row("AIP on random plot (kg)",     get_row("^Panel C", "All crops"), 2),
  "\\midrule",
  outcome_row("Two-plot AIP total (kg)",     get_row("^Panel D", "All crops"), 2)
)

table_lines <- c(
  "{",
  "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  "Outcome & Control mean & T1 $-$ Control & T2 $-$ Control & p-value: T2 = T1 & N \\\\",
  "\\midrule",
  rows_tex,
  "\\bottomrule",
  "\\end{tabular}",
  "}"
)

writeLines(table_lines,
           file.path(dir_tables, "table_aip_substitution.tex"))

message("Wrote AIP substitution table to: ", file.path(dir_tables, "table_aip_substitution.tex"))
message("Wrote AIP substitution log to: ", file.path(dir_logs, "table_aip_substitution.csv"))
