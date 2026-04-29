# Generate the recommendation-compliance and application-error table.
#
# Application error is only cleanly defined for households in Treatment 1 and
# Treatment 2 that have actual treatment recommendation records. The control
# group has shadow recommendation variables, but those are generated through a
# different recommendation pipeline and are affected by the lab-calibration
# issue discussed in the manuscript. For that reason, the paper table reports
# the treated-arm contrast: recommendation plus voucher (T2) minus
# recommendation only (T1).
#
# Nutrient units follow the fertilizer-grade units in the cleaned actual-use
# variables. The Stata cleaning code constructs total_P and total_K using the
# product-grade percentages in NPK labels, so these are P2O5 and K2O rather than
# elemental P and K. This table therefore compares actual and recommended N,
# P2O5, and K2O in kg/ha.

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

df <- read.csv(endline_path, stringsAsFactors = FALSE, check.names = FALSE)
df <- subset(df, treat %in% c("T1", "T2"))
n_before_case_exclusions <- nrow(df)

# Keep this table aligned with the fertilizer-use cleaning decision. These two
# tobacco observations are implausible aggregate fertilizer-use records and
# dominate fertilizer/nutrient application outcomes if retained.
df <- subset(df, !(farmer_id %in% c("F_546", "F_387")))
n_dropped_implausible <- n_before_case_exclusions - nrow(df)

# Exported sentinel values and blanks need to be normalized before either
# controls or actual kg/ha outcomes are constructed.
df$dist_agro[df$dist_agro == 999] <- NA
df$plot_siz[df$plot_siz == 999] <- NA
df$total_qty_fert[df$total_qty_fert == 999] <- NA
df$hh_educ[df$hh_educ == ""] <- NA
df$slope[df$slope == ""] <- NA
df$soil_str[df$soil_str == ""] <- NA
df$soil_str[df$soil_str == "5"] <- "Other/unknown"

df$treat_num <- factor(df$treat, levels = c("T1", "T2"))
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

parse_kgha <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  suppressWarnings(as.numeric(gsub("[^0-9.+-]", "", x)))
}

zero_if_missing <- function(x) {
  x[is.na(x)] <- 0
  x
}

# Clean product recommendation rates. Blank product cells mean that this
# product was not part of the recommended bundle, conditional on the farmer
# having a valid recommendation record.
df$rec_npk141420 <- zero_if_missing(parse_kgha(df$TR_PLANTINGNPK1414204S2M))
df$rec_can <- zero_if_missing(parse_kgha(df$TR_TOPDRESSCalciumAmmoniumNitra))
df$rec_potassium_sulphate <- zero_if_missing(parse_kgha(df$TR_TOPDRESSPotassiumSulphate))
df$rec_npk23105 <- zero_if_missing(parse_kgha(df$TR_PLANTINGNPK231056S1Zn))
df$rec_npk81815 <- zero_if_missing(parse_kgha(df$TR_PLANTINGNPK818156S01B))
df$rec_sop <- zero_if_missing(parse_kgha(df$TR_TOPDRESSSOP))
df$rec_mop <- zero_if_missing(parse_kgha(df$TR_TOPDRESSMOP))
df$rec_urea <- zero_if_missing(parse_kgha(df$TR_TOPDRESSUrea))
df$rec_map <- zero_if_missing(parse_kgha(df$TR_SOILCORRECTIONMAPTECHNICALG))
df$rec_npk152316 <- zero_if_missing(parse_kgha(df$TR_PLANTINGNPK1523166S05Zn0))

# Product-grade nutrient requirements implied by the recommended products.
# P and K are kept as P2O5 and K2O because actual total_P and total_K are built
# from fertilizer label grades in the same units.
df$rec_N_kgha <-
  0.14 * df$rec_npk141420 +
  0.26 * df$rec_can +
  0.23 * df$rec_npk23105 +
  0.08 * df$rec_npk81815 +
  0.46 * df$rec_urea +
  0.12 * df$rec_map +
  0.15 * df$rec_npk152316

df$rec_P2O5_kgha <-
  0.14 * df$rec_npk141420 +
  0.10 * df$rec_npk23105 +
  0.18 * df$rec_npk81815 +
  0.61 * df$rec_map +
  0.23 * df$rec_npk152316

df$rec_K2O_kgha <-
  0.20 * df$rec_npk141420 +
  0.50 * df$rec_potassium_sulphate +
  0.05 * df$rec_npk23105 +
  0.15 * df$rec_npk81815 +
  0.50 * df$rec_sop +
  0.60 * df$rec_mop +
  0.16 * df$rec_npk152316

df$actual_N_kgha <- df$total_N / df$plot_siz
df$actual_P2O5_kgha <- df$total_P / df$plot_siz
df$actual_K2O_kgha <- df$total_K / df$plot_siz

df$abs_error_N_kgha <- abs(df$actual_N_kgha - df$rec_N_kgha)
df$abs_error_P2O5_kgha <- abs(df$actual_P2O5_kgha - df$rec_P2O5_kgha)
df$abs_error_K2O_kgha <- abs(df$actual_K2O_kgha - df$rec_K2O_kgha)

df$shortfall_N_kgha <- pmax(df$rec_N_kgha - df$actual_N_kgha, 0)
df$shortfall_P2O5_kgha <- pmax(df$rec_P2O5_kgha - df$actual_P2O5_kgha, 0)
df$shortfall_K2O_kgha <- pmax(df$rec_K2O_kgha - df$actual_K2O_kgha, 0)

# A valid compliance observation needs a treatment recommendation record,
# usable actual fertilizer application data, and a non-sentinel plot size.
valid_application <- !is.na(df$TR_N_Req) &
  !is.na(df$plot_siz) &
  df$plot_siz > 0 &
  !is.na(df$total_qty_fert) &
  complete.cases(df[, c("actual_N_kgha", "actual_P2O5_kgha", "actual_K2O_kgha")])

df_valid <- df[valid_application, ]

exclusion_log <- data.frame(
  reason = c(
    "Treatment-arm observations before exclusions",
    "Dropped implausible fertilizer records: F_546 and F_387",
    "Missing treatment recommendation record",
    "Missing/sentinel plot size",
    "Missing/sentinel total fertilizer quantity",
    "Usable recommendation-compliance sample"
  ),
  n = c(
    n_before_case_exclusions,
    n_dropped_implausible,
    sum(is.na(df$TR_N_Req)),
    sum(is.na(df$plot_siz) | df$plot_siz <= 0),
    sum(is.na(df$total_qty_fert)),
    nrow(df_valid)
  )
)
write.csv(
  exclusion_log,
  file.path(dir_logs, "table8_application_compliance_exclusions.csv"),
  row.names = FALSE
)

fit_spec <- function(data, outcome, rhs_terms, outcome_label, controls_label) {
  vars_needed <- unique(c(outcome, "treat_num", "cluster_id_num", rhs_terms))
  data <- data[complete.cases(data[, vars_needed]), ]

  model <- lm(reformulate(rhs_terms, response = outcome), data = data)
  cluster_count <- length(unique(data$cluster_id_num))
  vcov_stage <- vcovCR(model, cluster = data$cluster_id_num, type = "CR1S")
  ct <- as.data.frame(coef_test(model, vcov = vcov_stage, test = "naive-t"))
  rownames(ct) <- ct$Coef

  t2_beta <- ct["treat_numT2", "beta"]
  t2_se <- ct["treat_numT2", "SE"]
  df_cluster <- cluster_count - 1
  t2_p <- 2 * pt(abs(t2_beta / t2_se), df = df_cluster, lower.tail = FALSE)

  data.frame(
    outcome = outcome,
    outcome_label = outcome_label,
    controls = controls_label,
    n = nrow(data),
    clusters = cluster_count,
    t1_mean = mean(data[[outcome]][data$treat == "T1"], na.rm = TRUE),
    t2_mean = mean(data[[outcome]][data$treat == "T2"], na.rm = TRUE),
    t2_minus_t1 = t2_beta,
    t2_minus_t1_se = t2_se,
    t2_minus_t1_p = t2_p,
    stringsAsFactors = FALSE
  )
}

outcomes <- data.frame(
  var = c(
    "rec_N_kgha",
    "actual_N_kgha",
    "abs_error_N_kgha",
    "shortfall_N_kgha",
    "rec_P2O5_kgha",
    "actual_P2O5_kgha",
    "abs_error_P2O5_kgha",
    "shortfall_P2O5_kgha",
    "rec_K2O_kgha",
    "actual_K2O_kgha",
    "abs_error_K2O_kgha",
    "shortfall_K2O_kgha"
  ),
  label = c(
    "Recommended N",
    "Actual N applied",
    "Absolute N application error",
    "N shortfall",
    "Recommended P$_2$O$_5$",
    "Actual P$_2$O$_5$ applied",
    "Absolute P$_2$O$_5$ application error",
    "P$_2$O$_5$ shortfall",
    "Recommended K$_2$O",
    "Actual K$_2$O applied",
    "Absolute K$_2$O application error",
    "K$_2$O shortfall"
  ),
  stringsAsFactors = FALSE
)

results <- do.call(
  rbind,
  lapply(seq_len(nrow(outcomes)), function(i) {
    rbind(
      fit_spec(df_valid, outcomes$var[i], "treat_num", outcomes$label[i], "No"),
      fit_spec(
        df_valid,
        outcomes$var[i],
        c("treat_num", preferred_controls),
        outcomes$label[i],
        "Yes"
      )
    )
  })
)

write.csv(
  results,
  file.path(dir_logs, "table8_application_compliance.csv"),
  row.names = FALSE
)

recommendation_diagnostics <- aggregate(
  cbind(rec_N_kgha, rec_P2O5_kgha, rec_K2O_kgha, actual_N_kgha,
        actual_P2O5_kgha, actual_K2O_kgha) ~ treat,
  data = df_valid,
  FUN = function(x) c(n = length(x), mean = mean(x), sd = sd(x), max = max(x))
)
write.csv(
  recommendation_diagnostics,
  file.path(dir_logs, "table8_application_compliance_diagnostics.csv"),
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

preferred <- subset(results, controls == "Yes")

table_lines <- c(
  "{",
  "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "Outcome & T1 mean & T2 $-$ T1 & N \\\\",
  "\\midrule",
  unlist(lapply(seq_len(nrow(preferred)), function(i) {
    c(
      sprintf(
        "%s & %s & %s & %s \\\\",
        preferred$outcome_label[i],
        fmt_num(preferred$t1_mean[i], 2),
        fmt_coef(preferred$t2_minus_t1[i], preferred$t2_minus_t1_p[i]),
        fmt_num(preferred$n[i], 0)
      ),
      sprintf(
        "& & (%s) & \\\\",
        fmt_num(preferred$t2_minus_t1_se[i], 2)
      )
    )
  })),
  "\\bottomrule",
  "\\end{tabular}",
  "}"
)

writeLines(table_lines, file.path(dir_tables, "table8_application_compliance.tex"))

message("Wrote application-compliance table to: ", file.path(dir_tables, "table8_application_compliance.tex"))
message("Wrote application-compliance log to: ", file.path(dir_logs, "table8_application_compliance.csv"))
