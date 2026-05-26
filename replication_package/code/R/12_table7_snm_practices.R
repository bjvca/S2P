# Generate the soil nutrient management (SNM) practice table.
#
# The original manuscript inputs for SNM practices were produced by probit
# regressions and displayed latent-index coefficients. For binary adoption
# outcomes, those coefficients are not directly interpretable as percentage
# point treatment effects. This script instead reports linear probability ITT
# estimates, matching the treatment-effect scale used in the balance,
# attrition, and first-stage tables.
#
# Outcomes are measured for the sampled/test plot. Blank survey responses are
# treated as missing; "Yes" is coded 1 and "No" is coded 0. The manuscript table
# reports the preferred adjusted specification with pre-treatment controls only.
# The CSV log stores both unadjusted and adjusted estimates for auditability.

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

outcomes <- data.frame(
  var = c(
    "green_inco",
    "fresh_app",
    "mat_farm_app",
    "dairy_app",
    "comp_app",
    "mbeya_app",
    "till_app",
    "ridge_use",
    "pit_use"
  ),
  label = c(
    "Green legume incorporation",
    "Fresh vegetative material",
    "Farmyard manure",
    "Dairy or poultry manure",
    "Compost",
    "Mbeya fertilizer",
    "Minimum tillage",
    "Ridges/check dams",
    "Pit planting"
  ),
  stringsAsFactors = FALSE
)

recode_yes_no <- function(x) {
  out <- rep(NA_real_, length(x))
  out[trimws(x) == "Yes"] <- 1
  out[trimws(x) == "No"] <- 0
  out
}

for (v in outcomes$var) {
  df[[paste0(v, "_bin")]] <- recode_yes_no(df[[v]])
}

fit_spec <- function(data, outcome, outcome_label, rhs_terms, controls_label) {
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
    label = outcome_label,
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

results <- do.call(
  rbind,
  lapply(seq_len(nrow(outcomes)), function(i) {
    outcome <- paste0(outcomes$var[i], "_bin")
    rbind(
      fit_spec(df, outcome, outcomes$label[i], "treat_num", "No"),
      fit_spec(df, outcome, outcomes$label[i], c("treat_num", preferred_controls), "Yes")
    )
  })
)

write.csv(
  results,
  file.path(dir_logs, "table7_snm_practices.csv"),
  row.names = FALSE
)

response_diagnostics <- do.call(
  rbind,
  lapply(seq_len(nrow(outcomes)), function(i) {
    raw <- df[[outcomes$var[i]]]
    data.frame(
      outcome = outcomes$var[i],
      label = outcomes$label[i],
      n_yes = sum(trimws(raw) == "Yes", na.rm = TRUE),
      n_no = sum(trimws(raw) == "No", na.rm = TRUE),
      n_blank = sum(trimws(raw) == "", na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
)
write.csv(
  response_diagnostics,
  file.path(dir_logs, "table7_snm_response_diagnostics.csv"),
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
  paste0(fmt_num(beta, 3), star_code(p_value))
}

preferred <- subset(results, controls == "No")

table_lines <- c(
  "{",
  "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  "Outcome & Control mean & T1 $-$ Control & T2 $-$ Control & p-value: T2 = T1 & N \\\\",
  "\\midrule",
  unlist(lapply(seq_len(nrow(preferred)), function(i) {
    c(
      sprintf(
        "%s & %s & %s & %s & %s & %s \\\\",
        preferred$label[i],
        fmt_num(preferred$control_mean[i], 3),
        fmt_coef(preferred$t1[i], preferred$t1_p[i]),
        fmt_coef(preferred$t2[i], preferred$t2_p[i]),
        fmt_num(preferred$p_equal[i], 3),
        fmt_num(preferred$n[i], 0)
      ),
      sprintf(
        "& & (%s) & (%s) & & \\\\",
        fmt_num(preferred$t1_se[i], 3),
        fmt_num(preferred$t2_se[i], 3)
      )
    )
  })),
  "\\bottomrule",
  "\\end{tabular}",
  "}"
)

writeLines(table_lines, file.path(dir_tables, "table7_snm_practices.tex"))

message("Wrote SNM-practices table to: ", file.path(dir_tables, "table7_snm_practices.tex"))
message("Wrote SNM-practices log to: ", file.path(dir_logs, "table7_snm_practices.csv"))
message("Wrote SNM-response diagnostics to: ", file.path(dir_logs, "table7_snm_response_diagnostics.csv"))
