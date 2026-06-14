# Generate level economic-outcome tables for the manuscript.
#
# The public endline file does not contain household-level revenue for all crops.
# It contains production and cost information for the sampled/test plot's main
# crop. This script therefore reports two samples:
#   - all sampled main crops: all observations with a nonmissing sampled-plot
#     main crop and production/cost information;
#   - maize main crop: the subsample whose sampled-plot main crop is maize.
#
# Value of production is constructed as harvested quantity times a crop-specific
# median unit price computed from sellers of that crop. This avoids conditioning
# the outcome on selling the crop, but it also means the "all crops" value
# measure is an imputed-value measure, not observed sales revenue. The script
# writes price and sample diagnostics so this choice remains auditable.

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

# Farmer-private fertilizer cost. The endline fertilizer-cost question (Q68)
# elicits the total cost of the fertilizer *applied* to the plot, regardless of
# source. Respondents report voucher-sourced fertilizer at its full commercial
# value (median ~2,200 MWK/kg) even though they paid nothing for it, so the raw
# fert_cost field charges treatment-2 households for the voucher transfer and is
# not a farmer-private cost. We net the voucher-funded kilograms (vour_qnt_us)
# out of each fertilizer slot, valued at that slot's own reported price per kg,
# leaving only fertilizer the household actually purchased. AIP-sourced kilograms
# are retained: they are reported at the subsidized price the household actually
# paid (median ~362 MWK/kg), so they already reflect genuine out-of-pocket cost.
for (r in 1:4) {
  qty  <- df[[paste0("test_plot", r, "qty_fert")]]
  cost <- df[[paste0("fert_cost", r)]]
  vour <- df[[paste0("test_plot", r, "vour_qnt_us")]]
  vour[is.na(vour)] <- 0
  own_frac <- ifelse(!is.na(qty) & qty > 0, pmax(qty - vour, 0) / qty, NA_real_)
  df[[paste0("fert_cost_own", r)]] <- ifelse(is.na(cost), NA_real_, cost * own_frac)
}

cost_vars <- c(
  "price_seed",
  "ttl_cost_pest",
  "test_plotttl_exp",
  "fert_cost_own1",
  "fert_cost_own2",
  "fert_cost_own3",
  "fert_cost_own4"
)

df$cost_total <- row_total_na0(df, cost_vars)

# Prices are only observed for sellers. Use crop medians after dropping nonpositive
# prices; medians are robust to the large unit-value outliers in the raw field.
price_data <- subset(
  df,
  main_crp != "" & !is.na(main_crp) & !is.na(prc_main) & prc_main > 0
)
crop_prices <- aggregate(
  prc_main ~ main_crp,
  price_data,
  function(x) {
    c(
      sellers = length(x),
      median_price = median(x, na.rm = TRUE),
      mean_price = mean(x, na.rm = TRUE),
      min_price = min(x, na.rm = TRUE),
      max_price = max(x, na.rm = TRUE)
    )
  }
)
crop_prices <- data.frame(
  main_crp = crop_prices$main_crp,
  sellers = crop_prices$prc_main[, "sellers"],
  median_price = crop_prices$prc_main[, "median_price"],
  mean_price = crop_prices$prc_main[, "mean_price"],
  min_price = crop_prices$prc_main[, "min_price"],
  max_price = crop_prices$prc_main[, "max_price"],
  row.names = NULL
)
df <- merge(df, crop_prices[, c("main_crp", "median_price")], by = "main_crp", all.x = TRUE)

df$value_production <- df$bags_Mcrp * df$median_price
df$profits <- df$value_production - df$cost_total

df$value_per_acre_all <- df$value_production / df$plot_siz
df$cost_per_acre_all <- df$cost_total / df$plot_siz
df$profit_per_acre_all <- df$profits / df$plot_siz

df$value_per_acre_maize <- df$value_production / df$maize_area
df$cost_per_acre_maize <- df$cost_total / df$maize_area
df$profit_per_acre_maize <- df$profits / df$maize_area

price_coverage_data <- subset(df, main_crp != "" & !is.na(main_crp))
price_coverage <- aggregate(
  cbind(
    n = !is.na(main_crp) & main_crp != "",
    production_nonmissing = !is.na(bags_Mcrp),
    price_imputed = !is.na(median_price),
    value_nonmissing = !is.na(value_production)
  ) ~ main_crp,
  price_coverage_data,
  sum
)
price_coverage <- merge(price_coverage, crop_prices, by = "main_crp", all.x = TRUE)
write.csv(
  price_coverage,
  file.path(dir_logs, "table6_economic_price_diagnostics.csv"),
  row.names = FALSE
)

fit_spec <- function(data, outcome, sample_label, rhs_terms) {
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
    sample = sample_label,
    outcome = outcome,
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

all_sample <- subset(
  df,
  main_crp != "" & !is.na(main_crp) & !is.na(value_production) & !is.na(cost_total)
)
maize_sample <- subset(all_sample, main_crp == "MAIZE")

outcomes <- data.frame(
  sample = c(rep("All sampled main crops", 6), rep("Maize main crop", 6)),
  outcome = c(
    "value_production",
    "cost_total",
    "profits",
    "value_per_acre_all",
    "cost_per_acre_all",
    "profit_per_acre_all",
    "value_production",
    "cost_total",
    "profits",
    "value_per_acre_maize",
    "cost_per_acre_maize",
    "profit_per_acre_maize"
  ),
  label = rep(
    c(
      "Value of production",
      "Total costs",
      "Profits",
      "Value per acre",
      "Costs per acre",
      "Profits per acre"
    ),
    2
  ),
  stringsAsFactors = FALSE
)

results <- do.call(
  rbind,
  lapply(seq_len(nrow(outcomes)), function(i) {
    data_i <- if (outcomes$sample[i] == "Maize main crop") maize_sample else all_sample
    fit_spec(
      data = data_i,
      outcome = outcomes$outcome[i],
      sample_label = outcomes$sample[i],
      rhs_terms = "treat_num"
    )
  })
)
results$label <- outcomes$label
results <- results[, c("sample", "label", setdiff(names(results), c("sample", "label")))]

write.csv(
  results,
  file.path(dir_logs, "table6_economic_outcomes_levels.csv"),
  row.names = FALSE
)

sample_diagnostics <- rbind(
  data.frame(
    sample = "All sampled main crops",
    potential_n = nrow(all_sample),
    potential_clusters = length(unique(all_sample$cluster_id_num)),
    analysis_n_min = min(results$n[results$sample == "All sampled main crops"]),
    analysis_n_max = max(results$n[results$sample == "All sampled main crops"]),
    analysis_clusters_min = min(results$clusters[results$sample == "All sampled main crops"]),
    analysis_clusters_max = max(results$clusters[results$sample == "All sampled main crops"]),
    stringsAsFactors = FALSE
  ),
  data.frame(
    sample = "Maize main crop",
    potential_n = nrow(maize_sample),
    potential_clusters = length(unique(maize_sample$cluster_id_num)),
    analysis_n_min = min(results$n[results$sample == "Maize main crop"]),
    analysis_n_max = max(results$n[results$sample == "Maize main crop"]),
    analysis_clusters_min = min(results$clusters[results$sample == "Maize main crop"]),
    analysis_clusters_max = max(results$clusters[results$sample == "Maize main crop"]),
    stringsAsFactors = FALSE
  )
)
write.csv(
  sample_diagnostics,
  file.path(dir_logs, "table6_economic_sample_diagnostics.csv"),
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
  paste0(fmt_money(beta), star_code(p_value))
}

fmt_money <- function(x) {
  ifelse(
    is.na(x),
    "",
    formatC(round(x), format = "f", digits = 0, big.mark = ",")
  )
}

make_rows <- function(sample_name) {
  x <- subset(results, sample == sample_name)
  rows <- unlist(lapply(seq_len(nrow(x)), function(i) {
    c(
      sprintf(
        "%s & %s & %s & %s & %s & %s \\\\",
        x$label[i],
        fmt_money(x$control_mean[i]),
        fmt_coef(x$t1[i], x$t1_p[i]),
        fmt_coef(x$t2[i], x$t2_p[i]),
        fmt_num(x$p_equal[i], 3),
        fmt_money(x$n[i])
      ),
      sprintf(
        "& & (%s) & (%s) & & \\\\",
        fmt_money(x$t1_se[i]),
        fmt_money(x$t2_se[i])
      )
    )
  }))
  rows
}

table_lines <- c(
  "{",
  "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  "Outcome & Control mean & T1 & T2 & p-value: T2 = T1 & N \\\\",
  "\\midrule",
  "\\multicolumn{6}{l}{\\textit{Maize main crop}} \\\\",
  make_rows("Maize main crop"),
  "\\addlinespace",
  "\\multicolumn{6}{l}{\\textit{All sampled main crops}} \\\\",
  make_rows("All sampled main crops"),
  "\\bottomrule",
  "\\end{tabular}",
  "}"
)

writeLines(table_lines, file.path(dir_tables, "table6_economic_outcomes_levels.tex"))

message("Wrote economic outcomes table to: ", file.path(dir_tables, "table6_economic_outcomes_levels.tex"))
message("Wrote economic outcomes log to: ", file.path(dir_logs, "table6_economic_outcomes_levels.csv"))
message("Wrote economic price diagnostics to: ", file.path(dir_logs, "table6_economic_price_diagnostics.csv"))
