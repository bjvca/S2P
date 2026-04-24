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

dir_repo <- normalizePath(file.path(replication_root, ".."), mustWork = TRUE)
endline_path <- file.path(dir_repo, "endline", "data", "public", "clear_merged_data.csv")

df <- read.csv(endline_path, stringsAsFactors = FALSE)

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
    "received_recommendation",
    "received_and_easy",
    "received_and_followed",
    "agronaut_visited",
    "received_voucher",
    "redeemed_voucher",
    "voucher_enough"
  ),
  label = c(
    "Received recommendation",
    "Received recommendation and found it easy/very easy",
    "Received recommendation and followed it",
    "Agronaut visited household",
    "Received voucher",
    "Redeemed voucher",
    "Voucher enough for full recommended purchase"
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
    t1_coef = ct["t1", "beta"],
    t1_se = ct["t1", "SE"],
    t1_p = ct["t1", "p_Satt"],
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

write.csv(
  first_stage_results,
  file.path(dir_logs, "table_first_stage.csv"),
  row.names = FALSE
)

table_lines <- c(
  "\\begin{tabular}{lrrrrr}",
  "\\hline",
  "Outcome & Control mean & T1 $-$ Control & T2 $-$ Control & $p$-value: T1 = T2 & N \\\\",
  "\\hline"
)

for (i in seq_len(nrow(first_stage_results))) {
  row <- first_stage_results[i, ]
  table_lines <- c(
    table_lines,
    sprintf(
      "%s & %s & %s & %s & %s & %s \\\\",
      row$outcome,
      fmt_num(row$control_mean, 3),
      fmt_num(row$t1_coef, 3),
      fmt_num(row$t2_coef, 3),
      fmt_p(row$p_equal, 3),
      fmt_num(row$n, 0)
    ),
    sprintf(
      " &  & (%s) & (%s) &  &  \\\\",
      fmt_num(row$t1_se, 3),
      fmt_num(row$t2_se, 3)
    )
  )
}

table_lines <- c(
  table_lines,
  "\\hline",
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
