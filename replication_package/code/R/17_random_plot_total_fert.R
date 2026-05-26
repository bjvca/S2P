# Test whether the voucher-induced fertilizer increase on the soil-test plot
# extends to the household's other plots, or is concentrated on the treated
# plot. Mirrors the AIP-substitution Panel C structure but applies to TOTAL
# fertilizer (any source), not AIP-specific fertilizer.

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
df <- subset(df, !(farmer_id %in% c("F_546", "F_387")))

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
df$other_plots_bin <- coerce_binary(df$other_plots)
df$rnd_is_maize <- df$rnd_plotmain_crpr == "MAIZE"
df$rnd_is_maize[is.na(df$rnd_is_maize)] <- FALSE

# Sum of all fertilizer kg on the random plot across slots.
rnd_slots <- c("rnd_plot1qty_fertr", "rnd_plot2qty_fertr",
               "rnd_plot3qty_fertr", "rnd_plot4qty_fertr")
present_slots <- rnd_slots[rnd_slots %in% names(df)]
for (col in present_slots) {
  x <- suppressWarnings(as.numeric(df[[col]]))
  x[x >= 999] <- NA  # 999 / sentinel and other recall artefacts
  df[[col]] <- x
}
df$rnd_total_fert <- rowSums(df[, present_slots], na.rm = TRUE)
all_na <- rowSums(is.na(df[, present_slots])) == length(present_slots)
df$rnd_total_fert[all_na] <- NA_real_

# Households that answered "never used fertilizer" on the random plot have all
# quantity slots missing (the repeat block never fires). Recode as true zeros.
rnd_never <- tolower(trimws(as.character(df$rnd_plotfertilizer_listrnever)))
df$rnd_total_fert[rnd_never == "yes" & is.na(df$rnd_total_fert)] <- 0

df$rnd_any_fert <- as.integer(df$rnd_total_fert > 0)

# Count-scaled estimator: multiply observed random-plot kg by the total number
# of non-test plots. Under random plot selection within each household this is
# an unbiased Horvitz-Thompson estimate of total non-test household fertilizer.
n_plots <- suppressWarnings(as.numeric(df$no_plots))
n_plots[n_plots <= 0 | n_plots > 99] <- NA
df$n_nontest_plots <- n_plots
df$rnd_total_fert_scaled <- df$rnd_total_fert * df$n_nontest_plots

rnd_siz <- suppressWarnings(as.numeric(df$rnd_plotplot_sizr))
rnd_siz[rnd_siz >= 999 | rnd_siz <= 0] <- NA
df$rnd_plot_siz <- rnd_siz
df$rnd_fert_per_acre <- df$rnd_total_fert / df$rnd_plot_siz

preferred_controls <- c(
  "hh_size", "hh_age", "hh_educ", "dist_agro",
  "plot_siz", "slope", "soil_str", "seed_typ_num"
)

fit_one <- function(data, rhs_terms, sample_label, uses_controls,
                    outcome = "rnd_total_fert") {
  vars_needed <- c(outcome, "treat_num", "cluster_id_num", rhs_terms)
  vars_needed <- unique(vars_needed)
  data <- data[complete.cases(data[, vars_needed]), ]

  model <- lm(reformulate(rhs_terms, response = outcome), data = data)
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
    sample = sample_label,
    uses_controls = uses_controls,
    outcome = outcome,
    n = nrow(data),
    control_mean = mean(data[[outcome]][data$treat == "C"], na.rm = TRUE),
    t1 = t1_beta, t1_se = t1_se, t1_p = t1_p,
    t2 = t2_beta, t2_se = t2_se, t2_p = t2_p,
    p_equal = p_equal,
    stringsAsFactors = FALSE
  )
}

results_bin <- rbind(
  fit_one(subset(df, other_plots_bin == 1),      "treat_num",                       "All crops",  "No",  "rnd_any_fert"),
  fit_one(subset(df, other_plots_bin == 1),      c("treat_num", preferred_controls), "All crops",  "Yes", "rnd_any_fert"),
  fit_one(subset(df, main_maize & rnd_is_maize), "treat_num",                       "Maize only", "No",  "rnd_any_fert"),
  fit_one(subset(df, main_maize & rnd_is_maize), c("treat_num", preferred_controls), "Maize only", "Yes", "rnd_any_fert")
)

results <- rbind(
  fit_one(subset(df, other_plots_bin == 1),      "treat_num",                       "All crops",  "No"),
  fit_one(subset(df, other_plots_bin == 1),      c("treat_num", preferred_controls), "All crops",  "Yes"),
  fit_one(subset(df, main_maize & rnd_is_maize), "treat_num",                       "Maize only", "No"),
  fit_one(subset(df, main_maize & rnd_is_maize), c("treat_num", preferred_controls), "Maize only", "Yes")
)

results_acre <- rbind(
  fit_one(subset(df, other_plots_bin == 1),      "treat_num",                       "All crops",  "No",  "rnd_fert_per_acre"),
  fit_one(subset(df, other_plots_bin == 1),      c("treat_num", preferred_controls), "All crops",  "Yes", "rnd_fert_per_acre"),
  fit_one(subset(df, main_maize & rnd_is_maize), "treat_num",                       "Maize only", "No",  "rnd_fert_per_acre"),
  fit_one(subset(df, main_maize & rnd_is_maize), c("treat_num", preferred_controls), "Maize only", "Yes", "rnd_fert_per_acre")
)

results_scaled <- rbind(
  fit_one(subset(df, other_plots_bin == 1),      "treat_num",                       "All crops",  "No",  "rnd_total_fert_scaled"),
  fit_one(subset(df, other_plots_bin == 1),      c("treat_num", preferred_controls), "All crops",  "Yes", "rnd_total_fert_scaled"),
  fit_one(subset(df, main_maize & rnd_is_maize), "treat_num",                       "Maize only", "No",  "rnd_total_fert_scaled"),
  fit_one(subset(df, main_maize & rnd_is_maize), c("treat_num", preferred_controls), "Maize only", "Yes", "rnd_total_fert_scaled")
)

out_dir <- file.path(replication_root, "output", "logs")
write.csv(rbind(results_bin, results, results_acre, results_scaled),
          file.path(out_dir, "table_random_plot_total_fert.csv"), row.names = FALSE)

# ── table writer ──────────────────────────────────────────────────────────────

star_code <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("\\sym{***}")
  if (p < 0.05) return("\\sym{**}")
  if (p < 0.10) return("\\sym{*}")
  ""
}

fmt_num <- function(x, digits) formatC(round(x, digits), format = "f", digits = digits)

fmt_row <- function(r) {
  c(
    sprintf("%s & %s & %s%s & %s%s & %s & %s \\\\",
            r$sample,
            fmt_num(r$control_mean, 1),
            fmt_num(r$t1, 1), star_code(r$t1_p),
            fmt_num(r$t2, 1), star_code(r$t2_p),
            fmt_num(r$p_equal, 2),
            fmt_num(r$n, 0)),
    sprintf("& & (%s) & (%s) & & \\\\",
            fmt_num(r$t1_se, 1), fmt_num(r$t2_se, 1))
  )
}

pref_bin    <- subset(results_bin,    uses_controls == "Yes")
pref_kg     <- subset(results,        uses_controls == "Yes")
pref_acre   <- subset(results_acre,   uses_controls == "Yes")
pref_scaled <- subset(results_scaled, uses_controls == "Yes")

fmt_row_bin <- function(r) {
  c(
    sprintf("%s & %s & %s%s & %s%s & %s & %s \\\\",
            r$sample,
            fmt_num(r$control_mean, 2),
            fmt_num(r$t1, 2), star_code(r$t1_p),
            fmt_num(r$t2, 2), star_code(r$t2_p),
            fmt_num(r$p_equal, 2),
            fmt_num(r$n, 0)),
    sprintf("& & (%s) & (%s) & & \\\\",
            fmt_num(r$t1_se, 2), fmt_num(r$t2_se, 2))
  )
}

table_lines <- c(
  "{",
  "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
  "\\begin{tabular}{lrrrrr}",
  "\\toprule",
  "Sample & Control mean & T1 $-$ Control & T2 $-$ Control & p-value: T2 = T1 & N \\\\",
  "\\midrule",
  "\\multicolumn{6}{l}{\\textit{Panel A: Any fertilizer used (proportion)}} \\\\",
  unlist(lapply(seq_len(nrow(pref_bin)),  function(i) fmt_row_bin(pref_bin[i, ]))),
  "\\midrule",
  "\\multicolumn{6}{l}{\\textit{Panel B: Total kg applied}} \\\\",
  unlist(lapply(seq_len(nrow(pref_kg)),   function(i) fmt_row(pref_kg[i, ]))),
  "\\midrule",
  "\\multicolumn{6}{l}{\\textit{Panel C: Kg per acre}} \\\\",
  unlist(lapply(seq_len(nrow(pref_acre)), function(i) fmt_row(pref_acre[i, ]))),
  "\\midrule",
  "\\multicolumn{6}{l}{\\textit{Panel D: Count-scaled total non-test household fertilizer (kg)}} \\\\",
  unlist(lapply(seq_len(nrow(pref_scaled)), function(i) fmt_row(pref_scaled[i, ]))),
  "\\bottomrule",
  "\\end{tabular}",
  "}"
)

out_tables <- file.path(replication_root, "output", "tables")
writeLines(table_lines, file.path(out_tables, "table_rnd_plot_total_fert.tex"))
message("Wrote random-plot fertilizer table to: ",
        file.path(out_tables, "table_rnd_plot_total_fert.tex"))
