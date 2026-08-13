# Generate the nutrient-use table for the manuscript.
#
# Nutrient outcomes are measured in kg/acre and restricted to households
# whose main crop is maize. Plot size (plot_siz) is already recorded in
# acres in the raw data, so nutrient application rates are simply total kg
# divided by plot_siz -- no unit conversion is needed or applied. (An
# earlier version of this script multiplied plot_siz by 0.40468564224 to
# convert acres to hectares and reported kg/ha; that conversion has been
# removed so the table matches the acre-based convention used throughout
# the rest of the paper.) The adjusted columns use the preferred
# pre-treatment controls only and enter categorical controls as factors.

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


df <- load_estimation_data()
df <- subset(df, treat %in% c("C", "T1", "T2") & main_crp == "MAIZE")

df$treat_num <- factor(df$treat_num, levels = c("C", "T1", "T2"))

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

# Per-acre nutrient application rates: total kg of nutrient applied divided
# directly by plot size in acres (plot_siz is already in acres in the raw
# data -- no acre-to-hectare conversion). Variable names keep the historical
# "_kgha" suffix so downstream scripts that key on these names by string
# (e.g. 15_multiple_testing_summary.R for table8's analogous variables) are
# unaffected; only the units of the VALUES have changed, from kg/ha to
# kg/acre.
df$N_kgha <- df$total_N / df$plot_siz
df$P_kgha <- df$total_P / df$plot_siz
df$K_kgha <- df$total_K / df$plot_siz
df$totalnutrient_kgha <- df$total_nutrient / df$plot_siz

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

fit_spec <- function(data, outcome, rhs_terms, outcome_label, controls_label) {
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
    outcome_label = outcome_label,
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

outcomes <- data.frame(
  var = c("N_kgha", "P_kgha", "K_kgha", "totalnutrient_kgha"),
  label = c(
    "Nitrogen (kg/acre)",
    "Phosphorus (kg/acre)",
    "Potassium (kg/acre)",
    "Total nutrients (kg/acre)"
  ),
  stringsAsFactors = FALSE
)

spec_results <- do.call(
  rbind,
  lapply(seq_len(nrow(outcomes)), function(i) {
    rbind(
      fit_spec(df, outcomes$var[i], "treat_num", outcomes$label[i], "No"),
      fit_spec(df, outcomes$var[i], c("treat_num", preferred_controls), outcomes$label[i], "Yes")
    )
  })
)

write.csv(
  spec_results,
  file.path(dir_logs, "table4_nutrient_use.csv"),
  row.names = FALSE
)

preferred_results <- subset(spec_results, controls == "No")

table_lines <- c(
  "{",
  "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  "Outcome & Control mean & T1 $-$ Control & T2 $-$ Control & p-value: T2 = T1 & N \\\\",
  "\\midrule",
  unlist(lapply(seq_len(nrow(preferred_results)), function(i) {
    c(
      sprintf(
        "%s & %s & %s & %s & %s & %s \\\\",
        preferred_results$outcome_label[i],
        fmt_num(preferred_results$control_mean[i], 2),
        fmt_coef(preferred_results$t1[i], preferred_results$t1_p[i]),
        fmt_coef(preferred_results$t2[i], preferred_results$t2_p[i]),
        fmt_num(preferred_results$p_equal[i], 3),
        fmt_num(preferred_results$n[i], 0)
      ),
      sprintf(
        "& & (%s) & (%s) & & \\\\",
        fmt_num(preferred_results$t1_se[i], 2),
        fmt_num(preferred_results$t2_se[i], 2)
      )
    )
  })),
  "\\bottomrule",
  "\\end{tabular}",
  "}"
)

writeLines(table_lines, file.path(dir_tables, "table4_nutrient_use.tex"))

message("Wrote nutrient-use table to: ", file.path(dir_tables, "table4_nutrient_use.tex"))
message("Wrote nutrient-use log to: ", file.path(dir_logs, "table4_nutrient_use.csv"))
