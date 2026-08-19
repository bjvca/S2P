# Generate the first main fertilizer-use table for the manuscript.
#
# This script replaces the earlier "audit-only" version with a table generator
# that uses the specifications we can defend in the paper:
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
#   3. It left in two implausibly large tobacco observations (farmer_id F_546
#      and F_387) that dominated the all-crops treatment means.
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


df <- load_estimation_data()
df <- subset(df, treat %in% c("C", "T1", "T2"))

# Drop two implausibly large tobacco observations that dominate the all-crops
# fertilizer results. These cases do not affect the maize-only columns because
# neither household's main crop is maize.
df <- subset(df, !(farmer_id %in% c("F_546", "F_387")))

df$treat_num <- factor(df$treat_num, levels = c("C", "T1", "T2"))
df$main_maize <- df$main_crp == "MAIZE"
df$any_fert    <- as.integer(df$total_qty_fert > 0)
df$fert_per_acre <- df$total_qty_fert / df$plot_siz

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

fit_spec <- function(data, rhs_terms, sample_label, column_label,
                     outcome = "total_qty_fert") {
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

  # Match Stata's clustered t reference by using G-1 degrees of freedom.
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
    sample = sample_label,
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
    sample_label = "All soil-test plots",
    column_label = "(1)"
  ),
  fit_spec(
    data = df,
    rhs_terms = c("treat_num", preferred_controls),
    sample_label = "All soil-test plots",
    column_label = "(2)"
  ),
  fit_spec(
    data = subset(df, main_maize),
    rhs_terms = "treat_num",
    sample_label = "Maize soil-test plots",
    column_label = "(3)"
  ),
  fit_spec(
    data = subset(df, main_maize),
    rhs_terms = c("treat_num", preferred_controls),
    sample_label = "Maize soil-test plots",
    column_label = "(4)"
  )
)

spec_results_acre <- rbind(
  fit_spec(
    data = df,
    rhs_terms = "treat_num",
    sample_label = "All soil-test plots",
    column_label = "(1)",
    outcome = "fert_per_acre"
  ),
  fit_spec(
    data = df,
    rhs_terms = c("treat_num", preferred_controls),
    sample_label = "All soil-test plots",
    column_label = "(2)",
    outcome = "fert_per_acre"
  ),
  fit_spec(
    data = subset(df, main_maize),
    rhs_terms = "treat_num",
    sample_label = "Maize soil-test plots",
    column_label = "(3)",
    outcome = "fert_per_acre"
  ),
  fit_spec(
    data = subset(df, main_maize),
    rhs_terms = c("treat_num", preferred_controls),
    sample_label = "Maize soil-test plots",
    column_label = "(4)",
    outcome = "fert_per_acre"
  )
)

spec_results_bin <- rbind(
  fit_spec(df,                    "treat_num", "All soil-test plots",  "(1)", "any_fert"),
  fit_spec(df,                    c("treat_num", preferred_controls), "All soil-test plots",  "(2)", "any_fert"),
  fit_spec(subset(df, main_maize),"treat_num", "Maize soil-test plots","(3)", "any_fert"),
  fit_spec(subset(df, main_maize),c("treat_num", preferred_controls), "Maize soil-test plots","(4)", "any_fert")
)

spec_results_bin$outcome  <- "any_fert"
spec_results_acre$outcome <- "fert_per_acre"
spec_results$outcome      <- "total_qty_fert"
write.csv(
  rbind(spec_results_bin, spec_results, spec_results_acre),
  file.path(dir_logs, "table2_fertilizer_use.csv"),
  row.names = FALSE
)

preferred_results_bin  <- subset(spec_results_bin,  uses_controls == "No")
preferred_results      <- subset(spec_results,      uses_controls == "No")
preferred_results_acre <- subset(spec_results_acre, uses_controls == "No")

make_rows <- function(results) {
  unlist(lapply(seq_len(nrow(results)), function(i) {
    c(
      sprintf(
        "%s & %s & %s & %s & %s & %s \\\\",
        results$sample[i],
        fmt_num(results$control_mean[i], 2),
        fmt_coef(results$t1[i], results$t1_p[i]),
        fmt_coef(results$t2[i], results$t2_p[i]),
        fmt_num(results$p_equal[i], 3),
        fmt_num(results$n[i], 0)
      ),
      sprintf(
        "& & (%s) & (%s) & & \\\\",
        fmt_num(results$t1_se[i], 2),
        fmt_num(results$t2_se[i], 2)
      )
    )
  }))
}

table_lines <- c(
  "{",
  "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
  "\\begin{tabular}{lccccc}",
  "\\hline\\hline",
  "Sample & Control mean & T1 $-$ Control & T2 $-$ Control & p-value: T2 = T1 & N \\\\",
  "\\hline",
  "\\addlinespace[6pt]",
  "\\multicolumn{6}{c}{\\textit{Panel A: Any fertilizer used (proportion)}} \\\\",
  "\\hline",
  make_rows(preferred_results_bin),
  "\\addlinespace[6pt]",
  "\\multicolumn{6}{c}{\\textit{Panel B: Total kg applied}} \\\\",
  "\\hline",
  make_rows(preferred_results),
  "\\addlinespace[6pt]",
  "\\multicolumn{6}{c}{\\textit{Panel C: Kg per acre}} \\\\",
  "\\hline",
  make_rows(preferred_results_acre),
  "\\hline\\hline",
  "\\end{tabular}",
  "}"
)

writeLines(table_lines, file.path(dir_tables, "table2_fertilizer_use.tex"))

message("Wrote fertilizer-use table to: ", file.path(dir_tables, "table2_fertilizer_use.tex"))
message("Wrote fertilizer-use log to: ", file.path(dir_logs, "table2_fertilizer_use.csv"))
