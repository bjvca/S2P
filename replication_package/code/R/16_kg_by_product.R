## 16_kg_by_product.R
## ---------------------------------------------------------------------------
## Per-household kg of each fertilizer product on the test plot, by treatment
## arm, alongside the mean recommended kg/ha (T1 + T2 households with valid
## recommendation records). Outputs:
##   output/logs/table_kg_by_product.csv   -- applied kg per HH by arm
##   output/logs/table_kg_recommended.csv  -- recommended kg/ha by product
## ---------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(data.table)
})

here <- function(...) file.path("/home/claude/workspace/S2P", ...)
d <- fread(here("endline/data/public/clear_merged_data.csv"))
d <- d[!is.na(treat) & treat %in% c("C", "T1", "T2")]

## ---- Applied kg per HH by product, on the test plot ------------------------
slots <- 1:4
long <- rbindlist(lapply(slots, function(i) {
  data.table(
    treat = d$treat,
    name  = d[[paste0("test_plot", i, "fertilizer_name")]],
    qty   = suppressWarnings(as.numeric(d[[paste0("test_plot", i, "qty_fert")]]))
  )
}))
long <- long[!is.na(qty) & qty > 0 & qty < 999 & nzchar(name)]

long[, prod := fcase(
  grepl("23:10:5",  name, fixed = TRUE), "NPK_23105",
  grepl("8:18:15",  name, fixed = TRUE), "NPK_81815",
  grepl("15:23:16", name, fixed = TRUE), "NPK_152316",
  grepl("14:14:20", name, fixed = TRUE), "NPK_141420",
  grepl("^UREA$",   name),               "Urea",
  grepl("CAN",      name, fixed = TRUE), "CAN",
  grepl("SOP|Potassium Sulphate|Sulphate of Potash", name), "K_product",
  grepl("DAP",      name, fixed = TRUE), "DAP",
  grepl("MOP",      name, fixed = TRUE), "K_product",
  grepl("Lime",     name, ignore.case = TRUE), "Lime",
  grepl("MAP",      name, fixed = TRUE), "MAP",
  default = "Other"
)]

N_arm <- d[, .N, by = treat]
agg <- long[, .(kg_total = sum(qty)), by = .(treat, prod)]
agg <- merge(agg, N_arm, by = "treat")
agg[, kg_per_hh := kg_total / N]
applied <- dcast(agg, prod ~ treat, value.var = "kg_per_hh", fill = 0)
applied[, delta_T2_minus_C := T2 - C]
applied <- applied[order(-T2)]

out_dir <- here("replication_package/output/logs")
fwrite(applied, file.path(out_dir, "table_kg_by_product.csv"))

cat("Applied kg per HH by product (test plot):\n")
print(applied, digits = 3)

## ---- Recommended kg/ha by product (T1 + T2 only) ---------------------------
parse_kgha <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  suppressWarnings(as.numeric(gsub("[^0-9.+-]", "", x)))
}

rec_vars <- c(
  NPK_23105_basal      = "TR_PLANTINGNPK231056S1Zn",
  NPK_81815_basal_alt  = "TR_PLANTINGNPK818156S01B",
  NPK_152316_basal_alt = "TR_PLANTINGNPK1523166S05Zn0",
  NPK_141420_basal_alt = "TR_PLANTINGNPK1414204S2M",
  CAN_topdress         = "TR_TOPDRESSCalciumAmmoniumNitra",
  Urea_topdress        = "TR_TOPDRESSUrea",
  MOP_topdress         = "TR_TOPDRESSMOP",
  SOP_topdress         = "TR_TOPDRESSSOP",
  Potassium_sulphate   = "TR_TOPDRESSPotassiumSulphate",
  Lime_calcitic        = "TR_SOILCORRECTIONCALCITICLIME",
  Lime_dolomitic       = "TR_SOILCORRECTIONDOLOMITICLIME",
  MAP                  = "TR_SOILCORRECTIONMAPTECHNICALG"
)
have <- rec_vars[rec_vars %in% names(d)]

rec <- rbindlist(lapply(seq_along(have), function(i) {
  x <- parse_kgha(d[treat %in% c("T1","T2")][[have[i]]])
  data.table(
    product               = names(have)[i],
    variable              = have[i],
    N_with_recommendation = sum(!is.na(x) & x > 0),
    mean_kgha_recommended = round(mean(x[x > 0], na.rm = TRUE), 2),
    median_kgha_recommended = round(median(x[x > 0], na.rm = TRUE), 2)
  )
}))
fwrite(rec, file.path(out_dir, "table_kg_recommended.csv"))

cat("\nRecommended kg/ha by product (T1 + T2):\n")
print(rec)
