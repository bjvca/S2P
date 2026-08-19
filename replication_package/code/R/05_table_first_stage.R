# Treatment-fidelity / first-stage table for the S2P paper.
#
# This script constructs intermediate outcomes from the completed endline
# interviews and estimates treatment-arm differences relative to the control
# group. The purpose is not IV first-stage estimation. It is to document
# whether the intervention components were actually delivered and acted upon.
#
# Several questionnaire items are conditional follow-ups. For the main table
# we therefore define "realized uptake" outcomes on the full interviewed
# sample. For example, the row on recommendation comprehension is coded as one
# only when the respondent both received the recommendation and reported that it
# was easy or very easy to understand. Households that did not receive the
# recommendation are coded as zero rather than dropped. This keeps the table on
# a common denominator and avoids comparing incomparable conditional samples
# across arms.

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
  library(car)
})


df <- load_estimation_data()

# Restrict to treatment-coded households with a usable endline interview.
df <- subset(df, treat %in% c("C", "T1", "T2") & checkq9 == "Yes")

if (any(is.na(df$cluster_id_num))) {
  stop("cluster_id_num contains missing values in the interviewed sample.")
}

df$t1 <- as.integer(df$treat == "T1")
df$t2 <- as.integer(df$treat == "T2")

yes_indicator <- function(x) {
  as.integer(x == "Yes")
}

# Realized-uptake outcomes on the full interviewed sample.
df$received_recommendation <- yes_indicator(df$get_rec)
df$received_and_easy <- as.integer(
  df$get_rec == "Yes" &
    df$ease_rec %in% c("Easy to understand", "Very easy to understand")
)
df$received_and_followed <- as.integer(
  df$get_rec == "Yes" & df$folll_rec == "Yes"
)
df$agronaut_visited <- yes_indicator(df$agro_vis)
df$received_voucher <- yes_indicator(df$got_voucher)
df$redeemed_voucher <- yes_indicator(df$redeem)
df$voucher_enough <- yes_indicator(df$vou_en)

outcome_specs <- data.frame(
  var = c(
    "agronaut_visited",
    "received_recommendation",
    "received_voucher"
  ),
  label = c(
    "Agronaut visited household",
    "Received recommendation",
    "Received voucher"
  ),
  stringsAsFactors = FALSE
)

run_first_stage <- function(var_name, label) {
  model <- lm(reformulate(c("t1", "t2"), response = var_name), data = df)
  vcov_stage <- vcovCR(model, cluster = df$cluster_id_num, type = "CR2")
  ct <- as.data.frame(coef_test(model, vcov = vcov_stage, test = "Satterthwaite"))
  rownames(ct) <- ct$Coef
  equal_test <- linearHypothesis(model, "t1 = t2", vcov. = vcov_stage, test = "F")

  data.frame(
    outcome = label,
    control_mean = mean(df[df$treat == "C", var_name], na.rm = TRUE),
    t1_mean = mean(df[df$treat == "T1", var_name], na.rm = TRUE),
    t1_coef = ct["t1", "beta"],
    t1_se = ct["t1", "SE"],
    t1_p = ct["t1", "p_Satt"],
    t2_mean = mean(df[df$treat == "T2", var_name], na.rm = TRUE),
    t2_coef = ct["t2", "beta"],
    t2_se = ct["t2", "SE"],
    t2_p = ct["t2", "p_Satt"],
    p_equal = equal_test$`Pr(>F)`[2],
    n = nrow(df),
    stringsAsFactors = FALSE
  )
}

first_stage_results <- do.call(
  rbind,
  lapply(seq_len(nrow(outcome_specs)), function(i) {
    run_first_stage(outcome_specs$var[i], outcome_specs$label[i])
  })
)

df_received <- subset(df, treat %in% c("T1", "T2") & get_rec == "Yes")
easy_cond_model <- lm(received_and_easy ~ t2, data = df_received)
easy_cond_vcov <- vcovCR(easy_cond_model, cluster = df_received$cluster_id_num, type = "CR2")
easy_cond_test <- as.data.frame(coef_test(easy_cond_model, vcov = easy_cond_vcov, test = "Satterthwaite"))
rownames(easy_cond_test) <- easy_cond_test$Coef

easy_conditional <- data.frame(
  outcome = "Found recommendation easy/very easy, conditional on receipt",
  control_mean = NA_real_,
  t1_mean = mean(df_received$received_and_easy[df_received$treat == "T1"], na.rm = TRUE),
  t1_coef = NA_real_,
  t1_se = NA_real_,
  t1_p = NA_real_,
  t2_mean = mean(df_received$received_and_easy[df_received$treat == "T2"], na.rm = TRUE),
  t2_coef = easy_cond_test["t2", "beta"],
  t2_se = easy_cond_test["t2", "SE"],
  t2_p = easy_cond_test["t2", "p_Satt"],
  p_equal = easy_cond_test["t2", "p_Satt"],
  n = nrow(df_received),
  stringsAsFactors = FALSE
)

followed_cond_model <- lm(received_and_followed ~ t2, data = df_received)
followed_cond_vcov <- vcovCR(followed_cond_model, cluster = df_received$cluster_id_num, type = "CR2")
followed_cond_test <- as.data.frame(coef_test(followed_cond_model, vcov = followed_cond_vcov, test = "Satterthwaite"))
rownames(followed_cond_test) <- followed_cond_test$Coef

followed_conditional <- data.frame(
  outcome = "Followed recommendation, conditional on receipt",
  control_mean = NA_real_,
  t1_mean = mean(df_received$received_and_followed[df_received$treat == "T1"], na.rm = TRUE),
  t1_coef = NA_real_,
  t1_se = NA_real_,
  t1_p = NA_real_,
  t2_mean = mean(df_received$received_and_followed[df_received$treat == "T2"], na.rm = TRUE),
  t2_coef = followed_cond_test["t2", "beta"],
  t2_se = followed_cond_test["t2", "SE"],
  t2_p = followed_cond_test["t2", "p_Satt"],
  p_equal = followed_cond_test["t2", "p_Satt"],
  n = nrow(df_received),
  stringsAsFactors = FALSE
)

insert_after <- match("Received voucher", first_stage_results$outcome)
tail_rows <- if (insert_after < nrow(first_stage_results)) {
  first_stage_results[(insert_after + 1):nrow(first_stage_results), ]
} else {
  first_stage_results[FALSE, ]
}
first_stage_results <- rbind(
  first_stage_results[seq_len(insert_after), ],
  easy_conditional,
  followed_conditional,
  tail_rows
)

df_voucher <- subset(df, treat == "T2" & got_voucher == "Yes")
redeemed_conditional <- data.frame(
  outcome = "Redeemed voucher, conditional on receipt",
  control_mean = NA_real_,
  t1_mean = NA_real_,
  t1_coef = NA_real_,
  t1_se = NA_real_,
  t1_p = NA_real_,
  t2_mean = mean(df_voucher$redeemed_voucher, na.rm = TRUE),
  t2_coef = NA_real_,
  t2_se = NA_real_,
  t2_p = NA_real_,
  p_equal = NA_real_,
  n = nrow(df_voucher),
  stringsAsFactors = FALSE
)

df_redeemed <- subset(df, treat == "T2" & redeem == "Yes")
voucher_enough_conditional <- data.frame(
  outcome = "Voucher enough for full recommended purchase, conditional on redemption",
  control_mean = NA_real_,
  t1_mean = NA_real_,
  t1_coef = NA_real_,
  t1_se = NA_real_,
  t1_p = NA_real_,
  t2_mean = mean(df_redeemed$voucher_enough, na.rm = TRUE),
  t2_coef = NA_real_,
  t2_se = NA_real_,
  t2_p = NA_real_,
  p_equal = NA_real_,
  n = nrow(df_redeemed),
  stringsAsFactors = FALSE
)

voucher_insert_after <- match("Followed recommendation, conditional on receipt", first_stage_results$outcome)
tail_rows <- if (voucher_insert_after < nrow(first_stage_results)) {
  first_stage_results[(voucher_insert_after + 1):nrow(first_stage_results), ]
} else {
  first_stage_results[FALSE, ]
}
first_stage_results <- rbind(
  first_stage_results[seq_len(voucher_insert_after), ],
  redeemed_conditional,
  voucher_enough_conditional,
  tail_rows
)

write.csv(
  first_stage_results,
  file.path(dir_logs, "table_first_stage.csv"),
  row.names = FALSE
)

table_lines <- c(
  "\\begin{tabular}{lrrrrr}",
  "\\hline\\hline",
  "Outcome & Control & T1 & T2 & $p$-value: T1 = T2 & N \\\\",
  "\\hline"
)

for (i in seq_len(nrow(first_stage_results))) {
  row <- first_stage_results[i, ]
  if (i == 1) {
    table_lines <- c(
      table_lines,
      "\\multicolumn{6}{c}{\\textit{Panel A: Delivery among interviewed households}} \\\\",
      "\\hline"
    )
  }
  if (row$outcome == "Found recommendation easy/very easy, conditional on receipt") {
    table_lines <- c(
      table_lines,
      "\\hline",
      "\\multicolumn{6}{c}{\\textit{Panel B: Recommendation comprehension and adherence, conditional on receipt}} \\\\",
      "\\hline"
    )
    row$outcome <- "Found recommendation easy/very easy"
  }
  if (row$outcome == "Followed recommendation, conditional on receipt") {
    row$outcome <- "Followed recommendation"
  }
  if (row$outcome == "Redeemed voucher, conditional on receipt") {
    table_lines <- c(
      table_lines,
      "\\hline",
      "\\multicolumn{6}{c}{\\textit{Panel C: Voucher implementation, T2 only}} \\\\",
      "\\hline"
    )
  }
  table_lines <- c(
    table_lines,
    sprintf(
      "%s & %s & %s & %s & %s & %s \\\\",
      row$outcome,
      fmt_num(row$control_mean, 3),
      fmt_num(row$t1_mean, 3),
      fmt_num(row$t2_mean, 3),
      fmt_p(row$p_equal, 3),
      fmt_num(row$n, 0)
    )
  )
}

table_lines <- c(
  table_lines,
  "\\hline\\hline",
  "\\end{tabular}"
)

writeLines(table_lines, file.path(dir_tables, "table_first_stage.tex"))

# Conditional descriptive rates used in the manuscript text.
conditional_stats <- data.frame(
  metric = c(
    "easy_given_received_T1",
    "easy_given_received_T2",
    "followed_given_received_T1",
    "followed_given_received_T2",
    "redeemed_given_voucher_T2",
    "voucher_enough_given_redeemed_T2"
  ),
  value = c(
    mean(df$received_and_easy[df$treat == "T1" & df$get_rec == "Yes"], na.rm = TRUE),
    mean(df$received_and_easy[df$treat == "T2" & df$get_rec == "Yes"], na.rm = TRUE),
    mean(df$received_and_followed[df$treat == "T1" & df$get_rec == "Yes"], na.rm = TRUE),
    mean(df$received_and_followed[df$treat == "T2" & df$get_rec == "Yes"], na.rm = TRUE),
    mean(df$redeemed_voucher[df$treat == "T2" & df$got_voucher == "Yes"], na.rm = TRUE),
    mean(df$voucher_enough[df$treat == "T2" & df$redeem == "Yes"], na.rm = TRUE)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  conditional_stats,
  file.path(dir_logs, "table_first_stage_conditional_stats.csv"),
  row.names = FALSE
)

message("Wrote treatment-fidelity table to: ", file.path(dir_tables, "table_first_stage.tex"))
message("Wrote supporting conditional rates to: ", file.path(dir_logs, "table_first_stage_conditional_stats.csv"))
