# Generate the nutrient-use table for the manuscript.
#
# The table follows the Stata analysis structure: nutrient outcomes are
# measured in kg/ha and restricted to households whose main crop is maize. The
# adjusted columns use the preferred pre-treatment controls only and enter
# categorical controls as factors.

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
df$N_kgha <- df$total_N / df$plot_siz
df$P_kgha <- df$total_P / df$plot_siz
df$K_kgha <- df$total_K / df$plot_siz
df$totalnutrient_kgha <- df$total_nutrient / df$plot_siz

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
  label = c("Nitrogen", "Phosphorus", "Potassium", "Total nutrients"),
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

cols <- seq_len(nrow(spec_results))

table_lines <- c(
  "{",
  "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
  "\\begin{tabular}{l*{8}{c}}",
  "\\toprule",
  "& \\multicolumn{8}{c}{Nutrient application on maize plots (kg/ha)} \\\\",
  "\\cmidrule(lr){2-9}",
  "& \\multicolumn{2}{c}{Nitrogen} & \\multicolumn{2}{c}{Phosphorus} & \\multicolumn{2}{c}{Potassium} & \\multicolumn{2}{c}{Total nutrients} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7} \\cmidrule(lr){8-9}",
  "& (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\\\",
  "\\midrule",
  paste0(
    "Treatment one & ",
    paste(mapply(fmt_coef, spec_results$t1[cols], spec_results$t1_p[cols]), collapse = " & "),
    " \\\\"
  ),
  paste0(
    "& (",
    paste(fmt_num(spec_results$t1_se[cols], 2), collapse = ") & ("),
    ") \\\\"
  ),
  paste0(
    "Treatment two & ",
    paste(mapply(fmt_coef, spec_results$t2[cols], spec_results$t2_p[cols]), collapse = " & "),
    " \\\\"
  ),
  paste0(
    "& (",
    paste(fmt_num(spec_results$t2_se[cols], 2), collapse = ") & ("),
    ") \\\\"
  ),
  paste0(
    "Control mean & ",
    paste(fmt_num(spec_results$control_mean[cols], 2), collapse = " & "),
    " \\\\"
  ),
  "\\midrule",
  paste0(
    "Pre-treatment controls & ",
    paste(spec_results$controls[cols], collapse = " & "),
    " \\\\"
  ),
  paste0(
    "Number of observations & ",
    paste(fmt_num(spec_results$n[cols], 0), collapse = " & "),
    " \\\\"
  ),
  "\\bottomrule",
  "\\end{tabular}",
  "}"
)

writeLines(table_lines, file.path(dir_tables, "table4_nutrient_use.tex"))

message("Wrote nutrient-use table to: ", file.path(dir_tables, "table4_nutrient_use.tex"))
message("Wrote nutrient-use log to: ", file.path(dir_logs, "table4_nutrient_use.csv"))
