# =============================================================================
# FIRST DRAFT — NOT INCORPORATED IN THE CURRENT MANUSCRIPT
# =============================================================================
# Status (2026-05-07): This script is a first-pass multiple-testing diagnostic
# (Holm within outcome families). It is disabled in run_all.R and its output
# (paper/tables/multiple_testing_summary.tex) is no longer \input{}-ed by the
# manuscript. The MHT strategy — Anderson (2008) inverse-covariance-weighted
# indices vs Holm vs Romano-Wolf, family definitions, primary-outcome
# demarcation — will be revisited with co-authors before MHT material is
# re-introduced into the paper. Treat any output produced here as preliminary
# and do not cite it from the manuscript until that revision is complete.
# =============================================================================
#
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

# Fertilizer and nutrient use are ITT outcomes. The paper reports unadjusted
# ITT specifications only, so q-values are computed from the same unadjusted
# p-values the tables display; the tables also report the incremental voucher test.
fert_use <- read_log("table2_fertilizer_use.csv")
# NOTE: table2_fertilizer_use.csv labels these samples "All soil-test plots"
# and "Maize soil-test plots" (not "All crops"/"Maize only" as an earlier
# version of the CSV apparently used). Filter on the labels actually present
# in the log, but keep the paper-facing display labels unchanged below, since
# these are the headline total-fertilizer-use ITT results.
fert_use_keep <- fert_use[fert_use$uses_controls == "No" &
  fert_use$sample %in% c("All soil-test plots", "Maize soil-test plots") &
  fert_use$outcome == "total_qty_fert", , drop = FALSE]
fert_use_display <- ifelse(fert_use_keep$sample == "All soil-test plots", "All crops", "Maize only")
add_family(
  "Fertilizer and nutrient use",
  paste0("Total fertilizer use: ", fert_use_display),
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

# AIP substitution outcomes: unadjusted ITT, all-crops sample, matching the
# displayed table. Not a pre-specified PAP family; disclosed as such in the
# paper text.
aip <- read_log("table_aip_substitution.csv")
aip_keep <- aip[aip$uses_controls == "No" & aip$sample == "All crops", , drop = FALSE]
aip_labels <- c(
  "Panel A: Binary AIP receipt indicator" = "AIP take-up (0/1)",
  "Panel B: AIP fertilizer applied on the test plot (kg)" = "AIP fertilizer on test plot",
  "Panel C: AIP fertilizer applied on the random plot (kg)" = "AIP fertilizer on random plot",
  "Panel D: Two-plot AIP fertilizer total (kg)" = "Two-plot AIP fertilizer total"
)
add_family(
  "AIP substitution",
  unname(aip_labels[aip_keep$panel]),
  rep("T2 - control", nrow(aip_keep)),
  aip_keep$t2_p
)

# Production and economic outcomes. Maize-main-crop economic outcomes are the
# preferred welfare-relevant economic panel because price support is strongest.
# Yield rows use the unadjusted specification the paper displays.
yield <- read_log("table5_maize_yield.csv")
yield_keep <- yield[yield$uses_controls == "No", , drop = FALSE]
yield_labels <- c(
  "w_bags_Mcrp_maiz" = "Total maize harvested",
  "lnyield" = "Log maize yield"
)
add_family(
  "Production and economic outcomes",
  unname(yield_labels[yield_keep$outcome]),
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

# -----------------------------------------------------------------------
# Sharpened two-stage FDR q-values (Benjamini, Krieger & Yekutieli 2006),
# following the two-stage adaptive linear step-up procedure exactly as
# implemented in the code appendix of Michael L. Anderson (2008), "Multiple
# Inference and Gender Differences in the Effects of Early Intervention: A
# Reevaluation of the Abecedarian, Perry Preschool, and Early Training
# Projects," JASA 103(484): 1481-1495 (his public fdr_sharpened_qvalues.do).
#
# This is NOT Holm (family-wise error rate control, conservative) and NOT
# plain Benjamini-Hochberg (does not use the data to estimate the share of
# true nulls). Anderson's q-value is defined POINTWISE, per hypothesis, as
# the smallest FDR level q at which the two-stage adaptive procedure would
# reject that hypothesis:
#
#   For a candidate level q, the two-stage decision rule is:
#     Stage 1: q' = q / (1+q). Reject the largest set of hypotheses such
#       that the k-th smallest p-value satisfies p_(k) <= q' * k / m; let
#       r1 be the number of stage-1 rejections.
#     m0-hat = m if r1 is 0 or m (boundary fix: a two-stage procedure with
#       zero evidence of true nulls, or with all hypotheses already
#       rejected, cannot divide by zero and gains nothing from sharpening),
#       otherwise m0-hat = m - r1.
#     Stage 2: q'' = q' * m / m0-hat (capped at 1). Reject the largest set
#       of hypotheses such that p_(k) <= q'' * k / m, evaluated again on
#       the ORIGINAL p-values (stage 2 re-tests all m hypotheses, it does
#       not restrict to the stage-1 survivors).
#   sharpened_q(i) = min over a descending grid of q of the levels at which
#   hypothesis i is rejected in stage 2 (matching Anderson's own Stata loop,
#   which walks q from 1.000 down to 0.001 in fixed steps and stamps each
#   hypothesis with the first q at which it clears the bar).
#
# An earlier draft of this function estimated m0-hat ONCE, at a fixed
# q=0.05, and applied that single scaling factor m/m0-hat to the ordinary
# BH q-values. That is a shortcut, not what Anderson's own code does: his
# procedure re-estimates m0-hat at every candidate q, because r1 (and hence
# m0-hat) is itself a function of q. Checked against Anderson's actual
# fdr_sharpened_qvalues.do (confirmed via its public source: a descending
# while-loop over q from 1.000 to 0.001, recomputing the two-stage
# rejection rule at each step), the single-m0 shortcut differs materially
# from the canonical procedure below (up to ~0.6 in q on random test
# vectors), so it was replaced with this grid-search version.
#
# Independently validated (see conversation record) against: (a) an
# independent from-scratch second implementation of the same grid-search
# logic, agreeing to numerical precision; (b) a hand-worked 10-hypothesis
# toy example; (c) monotonicity of q in sorted p and q in [0,1] over 2000
# random trials; (d) boundary cases (single hypothesis, all-null family,
# all-rejected family); (e) cross-check against the publicly documented
# structure of Anderson's own Stata implementation.
fdr_sharpened_qvalues <- function(p, grid_step = 0.0002) {
  m <- length(p)
  if (m == 0) return(numeric(0))
  if (any(is.na(p)) || any(p < 0 | p > 1)) {
    stop("fdr_sharpened_qvalues: p must be non-missing and in [0, 1]")
  }
  if (m == 1) return(p)

  p_sorted <- sort(p)
  ranks <- seq_len(m)

  # Ordinary Benjamini-Hochberg (1995) step-up q-values, computed once via
  # R's own tested p.adjust(). The key fact this exploits: hypothesis i is
  # rejected by BH's step-up rule at level a IF AND ONLY IF its BH q-value
  # is <= a (this is the defining property of the BH q-value, and it
  # correctly encodes the "reject ALL ranks up to k*" step-up rule -- an
  # earlier draft of this function instead compared each sorted p-value
  # against its own rank's threshold pointwise, which is wrong: BH rejects
  # every hypothesis ranked at or below the LARGEST rank clearing its
  # threshold, even if some lower-ranked p-value individually fails its
  # own, smaller threshold. That bug was caught by the random-trial cross
  # check against a second, independent implementation; using p.adjust()
  # for both the stage-1 and stage-2 decision rule below avoids
  # re-implementing step-up logic by hand at all.
  bh_q <- p.adjust(p_sorted, method = "BH")

  # Two-stage rejection indicator (logical vector over the SORTED p-values)
  # at a given candidate FDR level q.
  reject_at <- function(q) {
    qprime <- q / (1 + q)
    r1 <- sum(bh_q <= qprime)
    m0 <- if (r1 == 0L || r1 == m) m else (m - r1)
    qdbl <- min(qprime * m / m0, 1)
    bh_q <= qdbl
  }

  # Descending grid from 1 to (near) 0, plus exact anchor points at the
  # data-dependent breakpoints of the step function (the BH q-values
  # themselves, and their pre-images under q' = q/(1+q)), so the grid
  # cannot straddle and miss an exact transition. Anderson's own code uses
  # a fixed 0.001 step; the finer default here (0.0002) plus exact anchors
  # is strictly more precise.
  bh_anchors <- c(bh_q, bh_q / (1 - bh_q))
  grid <- sort(unique(c(
    seq(1, 0, by = -grid_step),
    bh_anchors, bh_anchors - 1e-9, bh_anchors + 1e-9
  )), decreasing = TRUE)
  grid <- grid[is.finite(grid) & grid >= 0 & grid <= 1]

  # Walk q from loose (1) to strict (0), overwriting the recorded q-value
  # for every hypothesis currently rejected at that level. Because the
  # sweep runs from large q to small q, the LAST time a hypothesis gets
  # overwritten is the smallest q at which it was still rejected -- exactly
  # the definition of its sharpened q-value, and exactly what Anderson's
  # own Stata loop does (it walks q downward and keeps stamping
  # bky06_qval = qval for whichever hypotheses remain in the rejected set).
  qval_sorted <- rep(1, m)
  for (lvl in grid) {
    rej <- reject_at(lvl)
    if (any(rej)) qval_sorted[rej] <- lvl
  }

  # Safety net: enforce monotone non-decreasing q in sorted p (guards
  # against any residual grid-resolution artifact).
  for (i in 2:m) if (qval_sorted[i] < qval_sorted[i - 1]) qval_sorted[i] <- qval_sorted[i - 1]

  # Map back from sorted order to the original input order.
  ord <- order(p)
  out <- numeric(m)
  out[ord] <- qval_sorted
  out
}

mht <- do.call(rbind, rows)
mht$holm_p <- ave(mht$raw_p, mht$family, FUN = function(p) p.adjust(p, method = "holm"))
mht$sharp_q <- ave(mht$raw_p, mht$family, FUN = fdr_sharpened_qvalues)
mht <- mht[order(mht$family, mht$sharp_q, mht$raw_p), ]

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
  "Fertilizer and nutrient use",
  "Product compliance",
  "Nutrient compliance",
  "AIP substitution",
  "Production and economic outcomes",
  "Secondary SNM practices"
), compact$family), ]

tex <- c(
  "\\begin{tabularx}{\\linewidth}{p{0.20\\linewidth}cp{0.18\\linewidth}X}",
  "\\hline\\hline",
  "Family & Tests & Contrast & Multiplicity-adjusted interpretation \\\\",
  "\\hline"
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

tex <- c(tex, "\\hline\\hline", "\\end{tabularx}")
writeLines(tex, file.path(dir_tables, "multiple_testing_summary.tex"))

# -----------------------------------------------------------------------
# Per-outcome appendix table: one row per outcome, grouped by family, with
# the sharpened two-stage FDR q-value alongside the raw p-value. This is
# the table meant for \input into the manuscript appendix; unlike the
# compact prose summary above it does not collapse outcomes within a
# family into a single interpretive sentence.
family_order <- c(
  "Delivery and adherence",
  "Fertilizer and nutrient use",
  "Product compliance",
  "Nutrient compliance",
  "AIP substitution",
  "Production and economic outcomes",
  "Secondary SNM practices"
)
qtable <- mht
qtable$family <- factor(qtable$family, levels = family_order)
qtable <- qtable[order(qtable$family, qtable$raw_p), ]
qtable$family <- as.character(qtable$family)

qtex <- c(
  "\\begin{tabularx}{\\linewidth}{Xlrr}",
  "\\hline\\hline",
  "Outcome & Contrast & Raw $p$ & Sharpened $q$ \\\\",
  "\\hline"
)

families_in_order <- unique(qtable$family)
for (f in seq_along(families_in_order)) {
  fam <- families_in_order[f]
  df <- qtable[qtable$family == fam, , drop = FALSE]

  qtex <- c(qtex, paste0(
    "\\multicolumn{4}{l}{\\textit{", escape_latex(fam), "}} \\\\"
  ))
  for (i in seq_len(nrow(df))) {
    qtex <- c(qtex, paste0(
      escape_latex(df$outcome[i]), " & ",
      escape_latex(df$contrast[i]), " & ",
      fmt_p_mht(df$raw_p[i]), " & ",
      fmt_p_mht(df$sharp_q[i]), " \\\\"
    ))
  }
  if (f < length(families_in_order)) qtex <- c(qtex, "\\addlinespace")
}

qtex <- c(qtex, "\\hline\\hline", "\\end{tabularx}")
writeLines(qtex, file.path(dir_tables, "multiple_testing_qvalues.tex"))
