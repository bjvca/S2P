# Generate the product-level recommendation-compliance table.
#
# This table asks whether farmers applied the specific fertilizer products that
# appeared in their treatment recommendation, not only whether they moved closer
# to the recommended nutrient totals. The main estimand is T2 minus T1 among
# treated households with valid treatment recommendation records.
#
# Product compliance is measured as product-set overlap. The main outcomes use
# binary indicators for applying the recommended product at all. Quantity-based
# product-rate compliance is kept in the diagnostic log because actual use is
# reported as total kg on the test plot, while recommendations are kg/ha.

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

# Keep case exclusions aligned with the fertilizer and nutrient compliance
# tables. These observations have implausible fertilizer quantities.
df <- subset(df, !(farmer_id %in% c("F_546", "F_387")))
n_dropped_implausible <- n_before_case_exclusions - nrow(df)

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

is_yes <- function(x) {
  trimws(as.character(x)) == "Yes"
}

product_specs <- data.frame(
  product = c(
    "npk23105",
    "urea",
    "can",
    "npk141420",
    "npk81815",
    "potassium_product",
    "lime",
    "map"
  ),
  label = c(
    "Recommended NPK 23:10:5 applied",
    "Recommended urea applied",
    "Recommended CAN applied",
    "Recommended NPK 14:14:20 applied",
    "Recommended NPK 8:18:15 applied",
    "Recommended potassium product applied",
    "Recommended lime applied",
    "Recommended MAP applied"
  ),
  stringsAsFactors = FALSE
)

# Recommendation indicators. Potassium products are grouped because the product
# recommendation can name MOP, SOP, or potassium sulphate, while actual survey
# responses also split potassium products across closely related categories.
df$rec_npk23105 <- parse_kgha(df$TR_PLANTINGNPK231056S1Zn) > 0
df$rec_urea <- parse_kgha(df$TR_TOPDRESSUrea) > 0
df$rec_can <- parse_kgha(df$TR_TOPDRESSCalciumAmmoniumNitra) > 0
df$rec_npk141420 <- parse_kgha(df$TR_PLANTINGNPK1414204S2M) > 0
df$rec_npk81815 <- parse_kgha(df$TR_PLANTINGNPK818156S01B) > 0
df$rec_npk152316 <- parse_kgha(df$TR_PLANTINGNPK1523166S05Zn0) > 0
df$rec_map <- parse_kgha(df$TR_SOILCORRECTIONMAPTECHNICALG) > 0
df$rec_mop <- parse_kgha(df$TR_TOPDRESSMOP) > 0
df$rec_sop <- parse_kgha(df$TR_TOPDRESSSOP) > 0
df$rec_potassium_sulphate <- parse_kgha(df$TR_TOPDRESSPotassiumSulphate) > 0
df$rec_potassium_product <- df$rec_mop | df$rec_sop | df$rec_potassium_sulphate
df$rec_lime <- parse_kgha(df$TR_SOILCORRECTIONCALCITICLIME) > 0 |
  parse_kgha(df$TR_SOILCORRECTIONDOLOMITICLIME) > 0

rec_cols <- grep("^rec_", names(df), value = TRUE)
for (v in rec_cols) df[[v]][is.na(df[[v]])] <- FALSE

# Actual product-use indicators from the survey product-list questions. These
# are less sensitive to repeat ordering than the repeat-level fertilizer names.
df$act_npk23105 <- is_yes(df$test_plotfertilizer_listnpk23105)
df$act_urea <- is_yes(df$test_plotfertilizer_listurea)
df$act_can <- is_yes(df$test_plotfertilizer_listcan)
df$act_npk141420 <- is_yes(df$test_plotfertilizer_listnpk14142)
df$act_npk81815 <- is_yes(df$test_plotfertilizer_listnpk81815)
df$act_npk152316 <- is_yes(df$test_plotfertilizer_listnpk15231)
df$act_map <- is_yes(df$test_plotfertilizer_listmap)
df$act_potassium_product <- is_yes(df$test_plotfertilizer_listmop) |
  is_yes(df$test_plotfertilizer_listsop) |
  is_yes(df$test_plotfertilizer_listpotassiu)
df$act_lime <- is_yes(df$test_plotfertilizer_listcalcitic) |
  is_yes(df$test_plotfertilizer_listdolomiti)

act_cols <- grep("^act_", names(df), value = TRUE)
for (v in act_cols) df[[v]][is.na(df[[v]])] <- FALSE

product_order <- product_specs$product
rec_matrix <- as.data.frame(lapply(product_order, function(p) df[[paste0("rec_", p)]]))
names(rec_matrix) <- product_order
act_matrix <- as.data.frame(lapply(product_order, function(p) df[[paste0("act_", p)]]))
names(act_matrix) <- product_order

valid_product <- df$get_rec == "Yes" &
  !is.na(df$TR_N_Req) &
  !is.na(df$total_qty_fert) &
  rowSums(rec_matrix) > 0

df_valid <- df[valid_product, ]
rec_valid <- rec_matrix[valid_product, , drop = FALSE]
act_valid <- act_matrix[valid_product, , drop = FALSE]

df_valid$any_recommended_product_applied <- as.numeric(rowSums(rec_valid & act_valid) > 0)
df_valid$share_recommended_products_applied <- rowSums(rec_valid & act_valid) / rowSums(rec_valid)
df_valid$all_recommended_products_applied <- as.numeric(rowSums(rec_valid & !act_valid) == 0)
df_valid$exact_recommended_product_bundle <- as.numeric(rowSums(rec_valid != act_valid) == 0)

for (p in product_order) {
  # Product-specific outcomes are defined only among households for whom that
  # product or product group was recommended.
  out <- ifelse(rec_valid[[p]], as.numeric(act_valid[[p]]), NA_real_)
  df_valid[[paste0("applied_if_rec_", p)]] <- out
}

exclusion_log <- data.frame(
  reason = c(
    "Treatment-arm observations before exclusions",
    "Dropped implausible fertilizer records: F_546 and F_387",
    "Missing treatment recommendation record",
    "Did not report receiving recommendation",
    "Missing/sentinel total fertilizer quantity",
    "No positive product recommendation in parsed fields",
    "Usable product-compliance sample"
  ),
  n = c(
    n_before_case_exclusions,
    n_dropped_implausible,
    sum(is.na(df$TR_N_Req)),
    sum(!is.na(df$TR_N_Req) & df$get_rec != "Yes"),
    sum(is.na(df$total_qty_fert)),
    sum(!is.na(df$TR_N_Req) & rowSums(rec_matrix) == 0),
    nrow(df_valid)
  )
)
write.csv(
  exclusion_log,
  file.path(dir_logs, "table9_product_compliance_exclusions.csv"),
  row.names = FALSE
)

fit_spec <- function(data, outcome, rhs_terms, outcome_label, controls_label) {
  vars_needed <- unique(c(outcome, "treat_num", "cluster_id_num", rhs_terms))
  data <- data[complete.cases(data[, vars_needed]), ]
  data <- droplevels(data)

  if (nrow(data) == 0 || length(unique(data$treat_num)) < 2) {
    return(data.frame(
      outcome = outcome,
      outcome_label = outcome_label,
      controls = controls_label,
      n = nrow(data),
      clusters = length(unique(data$cluster_id_num)),
      t1_mean = mean(data[[outcome]][data$treat == "T1"], na.rm = TRUE),
      t2_mean = mean(data[[outcome]][data$treat == "T2"], na.rm = TRUE),
      t2_minus_t1 = NA_real_,
      t2_minus_t1_se = NA_real_,
      t2_minus_t1_p = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  usable_rhs_terms <- rhs_terms
  if (length(rhs_terms) > 1) {
    usable_rhs_terms <- rhs_terms[sapply(rhs_terms, function(v) {
      if (v == "treat_num") return(TRUE)
      if (is.factor(data[[v]]) || is.character(data[[v]])) {
        return(length(unique(data[[v]])) >= 2)
      }
      stats::var(data[[v]], na.rm = TRUE) > 0
    })]
  }

  model <- lm(reformulate(usable_rhs_terms, response = outcome), data = data)
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

outcomes <- rbind(
  data.frame(
    var = c(
      "any_recommended_product_applied",
      "share_recommended_products_applied",
      "all_recommended_products_applied",
      "exact_recommended_product_bundle"
    ),
    label = c(
      "Applied at least one recommended product",
      "Share of recommended products applied",
      "Applied all recommended products",
      "Applied exact recommended product set"
    ),
    stringsAsFactors = FALSE
  ),
  data.frame(
    var = paste0("applied_if_rec_", product_specs$product),
    label = product_specs$label,
    stringsAsFactors = FALSE
  )
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
  file.path(dir_logs, "table9_product_compliance.csv"),
  row.names = FALSE
)

product_diagnostics <- do.call(
  rbind,
  lapply(seq_len(nrow(product_specs)), function(i) {
    p <- product_specs$product[i]
    keep <- rec_valid[[p]]
    data.frame(
      product = p,
      label = product_specs$label[i],
      recommended_n = sum(keep),
      applied_n = sum(keep & act_valid[[p]]),
      applied_rate = mean(act_valid[[p]][keep]),
      t1_recommended_n = sum(keep & df_valid$treat == "T1"),
      t1_applied_rate = mean(act_valid[[p]][keep & df_valid$treat == "T1"]),
      t2_recommended_n = sum(keep & df_valid$treat == "T2"),
      t2_applied_rate = mean(act_valid[[p]][keep & df_valid$treat == "T2"]),
      stringsAsFactors = FALSE
    )
  })
)
write.csv(
  product_diagnostics,
  file.path(dir_logs, "table9_product_compliance_product_diagnostics.csv"),
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
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "Outcome & T1 mean & T2 $-$ T1 & N \\\\",
  "\\midrule"
)

summary_rows <- preferred[preferred$outcome %in% c(
  "any_recommended_product_applied",
  "share_recommended_products_applied",
  "all_recommended_products_applied",
  "exact_recommended_product_bundle"
), ]
product_rows <- preferred[!(preferred$outcome %in% summary_rows$outcome), ]

format_rows <- function(rows) {
  unlist(lapply(seq_len(nrow(rows)), function(i) {
    c(
      sprintf(
        "%s & %s & %s & %s \\\\",
        rows$outcome_label[i],
        fmt_num(rows$t1_mean[i], 3),
        fmt_coef(rows$t2_minus_t1[i], rows$t2_minus_t1_p[i]),
        fmt_num(rows$n[i], 0)
      ),
      sprintf(
        "& & (%s) & \\\\",
        fmt_num(rows$t2_minus_t1_se[i], 3)
      )
    )
  }))
}

table_lines <- c(
  table_lines,
  "\\multicolumn{4}{c}{\\textit{Panel A: Product-set compliance summary}} \\\\",
  format_rows(summary_rows),
  "\\midrule",
  "\\multicolumn{4}{c}{\\textit{Panel B: Product-specific compliance, conditional on product recommendation}} \\\\",
  format_rows(product_rows),
  "\\bottomrule",
  "\\end{tabular}",
  "}"
)

writeLines(table_lines, file.path(dir_tables, "table9_product_compliance.tex"))

message("Wrote product-compliance table to: ", file.path(dir_tables, "table9_product_compliance.tex"))
message("Wrote product-compliance log to: ", file.path(dir_logs, "table9_product_compliance.csv"))
