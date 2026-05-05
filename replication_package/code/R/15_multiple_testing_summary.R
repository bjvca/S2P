# Multiple-testing diagnostics for the preferred manuscript contrasts.
#
# The main tables report raw p-values because the outcome families describe
# different steps in the causal chain. This script adds a transparent appendix
# diagnostic: within each family, it applies Holm adjustments to the preferred
# contrasts used in the paper.
source(file.path(if (exists("replication_root")) replication_root else getwd(), "code", "R", "00_setup.R"))

read_log <- function(file) {
  path <- file.path(dir_logs, file)
  if (!file.exists(path)) stop("Missing input log: ", path)
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

keep_preferred <- function(df) {
  if (!"controls" %in% names(df)) return(df)
  df[df$controls == "Yes", , drop = FALSE]
}

rows <- list()

add_family <- function(family, outcome, contrast, raw_p) {
  ok <- !is.na(raw_p)
  if (!any(ok)) return(invisible(NULL))
  rows[[length(rows) + 1L]] <<- data.frame(
    family = family,
    outcome = outcome[ok],
    contrast = contrast[ok],
    raw_p = as.numeric(raw_p[ok]),
    stringsAsFactors = FALSE
  )
}

# First-stage outcomes: the relevant diagnostic is whether the voucher arm
# differs from the recommendation-only arm for delivery, comprehension, and
# self-reported follow-through margins.
first_stage <- read_log("table_first_stage.csv")
first_stage_keep <- first_stage[first_stage$outcome %in% c(
  "Agronaut visited household",
  "Received recommendation",
  "Received recommendation and found it easy/very easy",
  "Received recommendation and followed it",
  "Voucher enough for full recommended purchase"
), , drop = FALSE]
add_family(
  "Delivery and adherence",
  first_stage_keep$outcome,
  rep("T2 = T1", nrow(first_stage_keep)),
  first_stage_keep$p_equal
)

# Product-compliance outcomes are defined only for treated households with
# recommendation records; the preferred contrast is T2 minus T1.
product <- keep_preferred(read_log("table9_product_compliance.csv"))
product_keep <- product[product$outcome %in% c(
  "any_recommended_product_applied",
  "share_recommended_products_applied",
  "all_recommended_products_applied",
  "exact_recommended_product_bundle",
  "applied_if_rec_npk23105",
  "applied_if_rec_urea",
  "applied_if_rec_potassium_product"
), , drop = FALSE]
add_family(
  "Product compliance",
  product_keep$outcome_label,
  rep("T2 - T1", nrow(product_keep)),
  product_keep$t2_minus_t1_p
)

# Nutrient/application compliance is also a treated-household comparison. We
# retain the interpretable implementation margins emphasized in the text.
application <- keep_preferred(read_log("table8_application_compliance.csv"))
application_keep <- application[application$outcome %in% c(
  "actual_N_kgha", "abs_error_N_kgha", "shortfall_N_kgha",
  "actual_P2O5_kgha", "abs_error_P2O5_kgha", "shortfall_P2O5_kgha",
  "actual_K2O_kgha", "abs_error_K2O_kgha", "shortfall_K2O_kgha"
), , drop = FALSE]
add_family(
  "Nutrient compliance",
  application_keep$outcome_label,
  rep("T2 - T1", nrow(application_keep)),
  application_keep$t2_minus_t1_p
)

# Fertilizer and nutrient use are ITT outcomes. The preferred inferential
# contrast for treatment impact is T2 versus the control group in adjusted
# specifications; the tables also report the incremental voucher test.
fert_use <- read_log("table2_fertilizer_use.csv")
fert_use_keep <- fert_use[fert_use$uses_controls == "Yes" & fert_use$sample %in% c("All crops", "Maize only"), , drop = FALSE]
add_family(
  "Fertilizer and nutrient use",
  paste0("Total fertilizer use: ", fert_use_keep$sample),
  rep("T2 - control", nrow(fert_use_keep)),
  fert_use_keep$t2_p
)
nutrient <- keep_preferred(read_log("table4_nutrient_use.csv"))
add_family(
  "Fertilizer and nutrient use",
  paste0("Maize nutrient use: ", nutrient$outcome_label),
  rep("T2 - control", nrow(nutrient)),
  nutrient$t2_p
)

# Production and economic outcomes. Maize-main-crop economic outcomes are the
# preferred welfare-relevant economic panel because price support is strongest.
yield <- read_log("table5_maize_yield.csv")
yield_keep <- yield[yield$uses_controls == "Yes", , drop = FALSE]
add_family(
  "Production and economic outcomes",
  rep("Log maize yield", nrow(yield_keep)),
  rep("T2 - control", nrow(yield_keep)),
  yield_keep$t2_p
)
econ <- read_log("table6_economic_outcomes_levels.csv")
econ_keep <- econ[econ$sample == "Maize main crop", , drop = FALSE]
add_family(
  "Production and economic outcomes",
  econ_keep$label,
  rep("T2 - control", nrow(econ_keep)),
  econ_keep$t2_p
)

# Secondary SNM practices are reported in the appendix and interpreted as
# supporting evidence, not as the primary basis for the paper's claims.
snm <- keep_preferred(read_log("table7_snm_practices.csv"))
add_family(
  "Secondary SNM practices",
  snm$label,
  rep("T2 - control", nrow(snm)),
  snm$t2_p
)

mht <- do.call(rbind, rows)
mht$holm_p <- ave(mht$raw_p, mht$family, FUN = function(p) p.adjust(p, method = "holm"))
mht <- mht[order(mht$family, mht$holm_p, mht$raw_p), ]

write.csv(mht, file.path(dir_logs, "multiple_testing_summary.csv"), row.names = FALSE)

escape_latex <- function(x) {
  # Labels in the regression logs already use LaTeX notation for nutrients
  # (for example, P$_2$O$_5$), so only escape characters that can break a table
  # in plain text labels.
  x <- gsub("&", "\\\\&", x)
  x <- gsub("%", "\\\\%", x)
  x <- gsub("#", "\\\\#", x)
  x
}

fmt_p_mht <- function(x) {
  ifelse(x < 0.001, "$<0.001$", sprintf("%.3f", x))
}

compact <- do.call(rbind, lapply(split(mht, mht$family), function(df) {
  sig <- df$outcome[df$holm_p < 0.05]
  marginal <- df$outcome[df$holm_p >= 0.05 & df$raw_p < 0.05]
  not_sig <- df$outcome[df$raw_p >= 0.05]
  interpretation <- if (length(sig) == 0) {
    "No outcome survives Holm adjustment."
  } else {
    paste0("Survives Holm: ", paste(sig, collapse = "; "), ".")
  }
  if (length(marginal) > 0) {
    interpretation <- paste0(
      interpretation,
      " Raw-only: ", paste(marginal, collapse = "; "), "."
    )
  }
  data.frame(
    family = unique(df$family),
    tests = nrow(df),
    contrast = paste(unique(df$contrast), collapse = "; "),
    interpretation = interpretation,
    stringsAsFactors = FALSE
  )
}))

compact <- compact[match(c(
  "Delivery and adherence",
  "Product compliance",
  "Nutrient compliance",
  "Fertilizer and nutrient use",
  "Production and economic outcomes",
  "Secondary SNM practices"
), compact$family), ]

tex <- c(
  "\\begin{tabularx}{\\linewidth}{p{0.20\\linewidth}cp{0.18\\linewidth}X}",
  "\\toprule",
  "Family & Tests & Contrast & Multiplicity-adjusted interpretation \\\\",
  "\\midrule"
)

for (i in seq_len(nrow(compact))) {
  tex <- c(tex, paste0(
    escape_latex(compact$family[i]), " & ",
    compact$tests[i], " & ",
    escape_latex(compact$contrast[i]), " & ",
    escape_latex(compact$interpretation[i]), " \\\\"
  ))
  if (i < nrow(compact)) tex <- c(tex, "\\addlinespace")
}

tex <- c(tex, "\\bottomrule", "\\end{tabularx}")
writeLines(tex, file.path(dir_tables, "multiple_testing_summary.tex"))
