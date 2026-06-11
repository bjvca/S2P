## ratio_response_T2.R
## ---------------------------------------------------------------------------
## Within-T2 test of whether farmers tilt the urea : NPK 23:10:5 ratio toward
## the recommendation. If they do not, slope ~ 0 settles Joachim's concern.
## ---------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(data.table)
  library(sandwich)
  library(lmtest)
})

here <- function(...) file.path("/home/claude/workspace/S2P", ...)
replication_root <- here("replication_package")
source(file.path(replication_root, "code", "R", "00_setup.R"))
d <- as.data.table(load_estimation_data())
d <- d[!is.na(treat) & treat %in% c("C", "T1", "T2")]

slots <- 1:4
long <- rbindlist(lapply(slots, function(i) {
  data.table(
    row_id  = seq_len(nrow(d)),
    treat   = d$treat,
    cluster = d$cluster_id_num,
    name    = d[[paste0("test_plot", i, "fertilizer_name")]],
    qty     = suppressWarnings(as.numeric(d[[paste0("test_plot", i, "qty_fert")]]))
  )
}))
long <- long[!is.na(qty) & qty > 0 & qty < 999 & nzchar(name)]
long[, prod := fcase(
  grepl("23:10:5", name, fixed = TRUE), "NPK_23105",
  grepl("^UREA$",  name),               "Urea",
  default = "Other"
)]

agg <- long[, .(kg = sum(qty)), by = .(row_id, prod)]
wide <- dcast(agg, row_id ~ prod, value.var = "kg", fill = 0)

d[, row_id := seq_len(.N)]
d <- merge(d, wide, by = "row_id", all.x = TRUE)
for (v in c("Urea","NPK_23105")) if (!v %in% names(d)) d[[v]] <- 0
d[is.na(Urea), Urea := 0]
d[is.na(NPK_23105), NPK_23105 := 0]

parse_kgha <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  suppressWarnings(as.numeric(gsub("[^0-9.+-]", "", x)))
}
d[, rec_urea_kgha := parse_kgha(TR_TOPDRESSUrea)]
d[, rec_npk_kgha  := parse_kgha(TR_PLANTINGNPK231056S1Zn)]

t2 <- d[treat == "T2" & (Urea + NPK_23105) > 0 &
        !is.na(rec_urea_kgha) & !is.na(rec_npk_kgha) &
        (rec_urea_kgha + rec_npk_kgha) > 0]

t2[, urea_share_applied := Urea / (Urea + NPK_23105)]
t2[, urea_share_rec     := rec_urea_kgha / (rec_urea_kgha + rec_npk_kgha)]

cat("=== Sample (T2 with both rec'd and any urea/NPK applied) ===\n")
cat("N =", nrow(t2), "\n\n")
cat("Recommended urea share (urea / (urea + NPK 23:10:5)) summary:\n")
print(summary(t2$urea_share_rec))
cat("\nApplied urea share summary:\n")
print(summary(t2$urea_share_applied))
cat("\nVariation in recommended share: SD =",
    round(sd(t2$urea_share_rec), 3), "\n\n")

run <- function(form, data, cl) {
  m <- lm(form, data = data)
  vc <- vcovCL(m, cluster = cl)
  ct <- coeftest(m, vc)
  list(m = m, ct = ct, n = nobs(m))
}

cat("=== Regression 1: applied urea share on recommended urea share ===\n")
r1 <- run(urea_share_applied ~ urea_share_rec, t2, t2$cluster)
print(r1$ct); cat("N =", r1$n, "\n\n")

t2[, rec_N := parse_kgha(TR_N_Req)]
t2[, rec_P := parse_kgha(TR_P2O5_Req)]
t2b <- t2[!is.na(rec_N) & !is.na(rec_P) & (rec_N + rec_P) > 0]
t2b[, rec_N_share := rec_N / (rec_N + rec_P)]

cat("=== Regression 2: applied urea share on recommended N share (N / (N + P2O5)) ===\n")
cat("Rec N share summary:\n"); print(summary(t2b$rec_N_share))
r2 <- run(urea_share_applied ~ rec_N_share, t2b, t2b$cluster)
print(r2$ct); cat("N =", r2$n, "\n\n")

cat("=== Regression 3: kg urea on recommended urea kg/ha (controlling for total kg applied) ===\n")
t2[, total_kg := Urea + NPK_23105]
r3 <- run(Urea ~ rec_urea_kgha + total_kg, t2, t2$cluster)
print(r3$ct); cat("N =", r3$n, "\n\n")

cat("=== Bin check: mean applied urea share by tercile of recommended urea share ===\n")
t2[, rec_terc := cut(urea_share_rec,
                     breaks = quantile(urea_share_rec, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                     include.lowest = TRUE, labels = c("low","mid","high"))]
print(t2[, .(N = .N,
             mean_rec_share = round(mean(urea_share_rec), 3),
             mean_applied_share = round(mean(urea_share_applied), 3)),
         by = rec_terc][order(rec_terc)])
