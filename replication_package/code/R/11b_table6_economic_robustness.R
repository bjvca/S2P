# Generate a compact maize-profit robustness table for the appendix.
#
# The main text economic table should stand on transparent level outcomes:
# value of production, costs, and profits. This appendix table checks whether
# the maize profit result is being driven by extreme harvest or cost values,
# and whether the sign changes under alternative outcome scales. It does not
# replace the main economic table.

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
df$maize_area[df$maize_area == 999] <- NA
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

row_total_na0 <- function(data, vars) {
  x <- data[, vars]
  all_missing <- rowSums(!is.na(x)) == 0
  out <- rowSums(x, na.rm = TRUE)
  out[all_missing] <- NA_real_
  out
}

winsorize <- function(x, probs = c(0.01, 0.99)) {
  q <- quantile(x, probs = probs, na.rm = TRUE, names = FALSE)
  pmin(pmax(x, q[1]), q[2])
}

cost_vars <- c(
  "price_seed",
  "ttl_cost_pest",
  "test_plotttl_exp",
  "fert_cost1",
  "fert_cost2",
  "fert_cost3",
  "fert_cost4"
)

df$cost_total <- row_total_na0(df, cost_vars)

maize_prices <- subset(df, !is.na(prc_main) & prc_main > 0)$prc_main
maize_price_median <- median(maize_prices, na.rm = TRUE)
price_q <- quantile(maize_prices, probs = c(0.05, 0.95), na.rm = TRUE, names = FALSE)
maize_price_winsor_mean <- mean(pmin(pmax(maize_prices, price_q[1]), price_q[2]), na.rm = TRUE)

df$value_med <- df$bags_Mcrp * maize_price_median
df$profit_med <- df$value_med - df$cost_total

df$bags_Mcrp_w <- winsorize(df$bags_Mcrp)
df$cost_total_w <- winsorize(df$cost_total)

df$value_med_wyield <- df$bags_Mcrp_w * maize_price_median
df$profit_med_wyield <- df$value_med_wyield - df$cost_total
df$profit_med_wyield_cost <- df$value_med_wyield - df$cost_total_w

df$value_winmean <- df$bags_Mcrp * maize_price_winsor_mean
df$profit_winmean <- df$value_winmean - df$cost_total

df$ln_profit_med_positive <- NA_real_
positive_profit <- !is.na(df$profit_med) & df$profit_med > 0
df$ln_profit_med_positive[positive_profit] <- log(df$profit_med[positive_profit])

fit_spec <- function(data, outcome, label, control_mean_label) {
  vars_needed <- unique(c(outcome, "treat_num", "cluster_id_num", preferred_controls))
  data <- data[complete.cases(data[, vars_needed]), ]

  model <- lm(reformulate(c("treat_num", preferred_controls), response = outcome), data = data)
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
    specification = label,
    control_mean_label = control_mean_label,
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

robustness_results <- do.call(
  rbind,
  list(
    fit_spec(df, "profit_med", "Level profits, baseline valuation", "MWK"),
    fit_spec(df, "profit_med_wyield", "Level profits, winsorized harvest quantity", "MWK"),
    fit_spec(df, "profit_med_wyield_cost", "Level profits, winsorized harvest and costs", "MWK"),
    fit_spec(df, "ln_profit_med_positive", "Log profits, positive-profit sample only", "log(MWK)")
  )
)

write.csv(
  robustness_results,
  file.path(dir_logs, "table6_economic_profit_robustness.csv"),
  row.names = FALSE
)

price_diagnostics <- data.frame(
  price_statistic = c(
    "Observed maize seller prices (N)",
    "Observed maize seller-price median",
    "Observed maize seller-price mean",
    "Observed maize seller-price winsorized mean (5-95)"
  ),
  value = c(
    length(maize_prices),
    maize_price_median,
    mean(maize_prices, na.rm = TRUE),
    maize_price_winsor_mean
  )
)
write.csv(
  price_diagnostics,
  file.path(dir_logs, "table6_economic_maize_price_diagnostics.csv"),
  row.names = FALSE
)

top_outliers <- df[order(-abs(df$profit_med)), c(
  "farmer_id",
  "treat",
  "bags_Mcrp",
  "prc_main",
  "value_med",
  "cost_total",
  "profit_med",
  "plot_siz",
  "maize_area"
)]
write.csv(
  head(top_outliers, 25),
  file.path(dir_logs, "table6_economic_maize_top_profit_observations.csv"),
  row.names = FALSE
)

star_code <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.01) return("\\sym{***}")
  if (p_value < 0.05) return("\\sym{**}")
  if (p_value < 0.10) return("\\sym{*}")
  ""
}

fmt_num_or_money <- function(x, unit_label) {
  if (is.na(x)) return("")
  if (unit_label == "MWK") {
    return(formatC(round(x), format = "f", digits = 0, big.mark = ","))
  }
  formatC(x, format = "f", digits = 3)
}

fmt_coef <- function(beta, p_value, unit_label) {
  paste0(fmt_num_or_money(beta, unit_label), star_code(p_value))
}

table_lines <- c(
  "{",
  "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  "Robustness check & Control mean & T1 & T2 & p-value: T2 = T1 & N \\\\",
  "\\midrule",
  unlist(lapply(seq_len(nrow(robustness_results)), function(i) {
    unit_label <- robustness_results$control_mean_label[i]
    c(
      sprintf(
        "%s & %s & %s & %s & %s & %s \\\\",
        robustness_results$specification[i],
        fmt_num_or_money(robustness_results$control_mean[i], unit_label),
        fmt_coef(robustness_results$t1[i], robustness_results$t1_p[i], unit_label),
        fmt_coef(robustness_results$t2[i], robustness_results$t2_p[i], unit_label),
        fmt_num(robustness_results$p_equal[i], 3),
        fmt_num(robustness_results$n[i], 0)
      ),
      sprintf(
        "& & (%s) & (%s) & & \\\\",
        fmt_num_or_money(robustness_results$t1_se[i], unit_label),
        fmt_num_or_money(robustness_results$t2_se[i], unit_label)
      )
    )
  })),
  "\\bottomrule",
  "\\end{tabular}",
  "}"
)

writeLines(table_lines, file.path(dir_tables, "table6_economic_profit_robustness.tex"))

message("Wrote economic robustness table to: ", file.path(dir_tables, "table6_economic_profit_robustness.tex"))
message("Wrote economic robustness log to: ", file.path(dir_logs, "table6_economic_profit_robustness.csv"))
