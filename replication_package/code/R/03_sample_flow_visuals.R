# Visual and tabular presentation options for sample flow and attrition.
#
# This script turns the audit outputs from 02_sample_flow_attrition.R into:
#   1. A main-text retention plot in percentages by treatment arm and stage.
#   2. A manuscript table that combines counts and percentages.
#   3. An appendix-style decomposition bar chart showing where sample losses occur.
#
# The percentages are always expressed relative to the original sampled baseline
# frame. That keeps denominators constant across stages and makes treatment-arm
# differences easy to compare.

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

sample_flow_path <- file.path(dir_logs, "sample_flow_attrition.csv")
if (!file.exists(sample_flow_path)) {
  stop("Missing sample-flow audit output: ", sample_flow_path,
       ". Run 02_sample_flow_attrition.R first.")
}

sample_flow <- read.csv(sample_flow_path, stringsAsFactors = FALSE)

arm_levels <- c("C", "T1", "T2")
sample_flow <- sample_flow[match(arm_levels, sample_flow$treatment), ]

stage_labels <- c(
  "Baseline\nsampled",
  "Endline\ntarget",
  "Found",
  "Interview\ncompleted",
  "Matched to\nsoil test data"
)

stage_counts <- rbind(
  sample_flow$baseline_sample_n,
  sample_flow$endline_target_n,
  sample_flow$found_n,
  sample_flow$interview_completed_n,
  sample_flow$matched_recommendation_n
)
rownames(stage_counts) <- stage_labels
colnames(stage_counts) <- sample_flow$treatment

stage_pct <- sweep(stage_counts, 2, sample_flow$baseline_sample_n, "/") * 100

# -----------------------------------------------------------------------------
# Main-text figure: retention plot
# -----------------------------------------------------------------------------

retention_png <- file.path(dir_figures, "sample_flow_retention_plot.png")
png(retention_png, width = 1600, height = 1000, res = 180)

old_mar <- par("mar")
par(mar = c(7, 6, 3, 5))

x <- seq_along(stage_labels)
cols <- c("C" = "#4E79A7", "T1" = "#F28E2B", "T2" = "#59A14F")
ltys <- c("C" = 1, "T1" = 1, "T2" = 1)
pchs <- c("C" = 16, "T1" = 17, "T2" = 15)

plot(
  x, stage_pct[, "C"],
  type = "n",
  xaxt = "n",
  xlim = c(min(x), max(x) + 0.95),
  ylim = c(60, 102),
  xlab = "",
  ylab = "Percent of baseline sampled households",
  las = 1
)
axis(1, at = x, labels = FALSE)
usr <- par("usr")
text(
  x = x,
  y = usr[3] - 2.8,
  labels = stage_labels,
  cex = 0.85,
  xpd = NA
)
abline(h = seq(60, 100, by = 10), col = "grey90", lwd = 1)

for (arm in arm_levels) {
  lines(x, stage_pct[, arm], lwd = 3, col = cols[[arm]], lty = ltys[[arm]])
  points(x, stage_pct[, arm], pch = pchs[[arm]], col = cols[[arm]], cex = 1.3)
}

# Label only the final-stage percentages to keep the figure readable.
for (arm in arm_levels) {
  text(
    x[length(x)] + 0.12,
    stage_pct[length(stage_labels), arm],
    labels = sprintf("%s: %.1f%%", arm, stage_pct[length(stage_labels), arm]),
    col = cols[[arm]],
    pos = 4,
    cex = 0.95
  )
}

legend(
  "bottomleft",
  legend = c("Control", "Treatment 1", "Treatment 2"),
  col = cols[arm_levels],
  pch = pchs[arm_levels],
  lwd = 3,
  bty = "n",
  cex = 0.95
)

title("Sample retention by treatment arm and survey stage")
par(mar = old_mar)
dev.off()

# -----------------------------------------------------------------------------
# Table: counts plus percentages by arm
# -----------------------------------------------------------------------------

fmt_npct <- function(n, denom) {
  sprintf("%s (%.1f\\%%)", fmt_num(n, 0), 100 * n / denom)
}

table_lines <- c(
  "\\begin{tabular}{lccc}",
  "\\hline",
  "Stage & Control & Treatment 1 & Treatment 2 \\\\",
  "\\hline"
)

for (i in seq_len(nrow(stage_counts))) {
  stage <- rownames(stage_counts)[i]
  table_lines <- c(
    table_lines,
    sprintf(
      "%s & %s & %s & %s \\\\",
      stage,
      fmt_npct(stage_counts[i, "C"], sample_flow$baseline_sample_n[sample_flow$treatment == "C"]),
      fmt_npct(stage_counts[i, "T1"], sample_flow$baseline_sample_n[sample_flow$treatment == "T1"]),
      fmt_npct(stage_counts[i, "T2"], sample_flow$baseline_sample_n[sample_flow$treatment == "T2"])
    )
  )
}

table_lines <- c(
  table_lines,
  "\\hline",
  "\\end{tabular}"
)

writeLines(table_lines, file.path(dir_tables, "sample_flow_counts_pct.tex"))

# -----------------------------------------------------------------------------
# Appendix figure: decomposition of sample losses
# -----------------------------------------------------------------------------

loss_components <- rbind(
  matched = sample_flow$matched_recommendation_n,
  interviewed_unmatched = sample_flow$interview_completed_n - sample_flow$matched_recommendation_n,
  found_not_interviewed = sample_flow$found_n - sample_flow$interview_completed_n,
  not_found = sample_flow$not_found_n,
  not_in_endline_target = sample_flow$baseline_sample_n - sample_flow$endline_target_n
)
colnames(loss_components) <- sample_flow$treatment
loss_pct <- sweep(loss_components, 2, sample_flow$baseline_sample_n, "/") * 100

decomp_png <- file.path(dir_figures, "sample_flow_decomposition_bar.png")
png(decomp_png, width = 1600, height = 1000, res = 180)

old_mar <- par("mar")
par(mar = c(5, 6, 3, 8), xpd = NA)

bar_cols <- c(
  matched = "#59A14F",
  interviewed_unmatched = "#76B7B2",
  found_not_interviewed = "#EDC948",
  not_found = "#E15759",
  not_in_endline_target = "#B07AA1"
)

barplot(
  loss_pct,
  beside = FALSE,
  col = bar_cols[rownames(loss_pct)],
  ylim = c(0, 100),
  names.arg = c("Control", "Treatment 1", "Treatment 2"),
  ylab = "Percent of baseline sampled households",
  las = 1
)
abline(h = seq(0, 100, by = 10), col = "grey90", lwd = 1)

legend(
  "topright",
  inset = c(-0.27, 0),
  legend = c(
    "Matched to soil test data",
    "Interviewed but unmatched",
    "Found but not interviewed",
    "Not found",
    "Not in endline target"
  ),
  fill = bar_cols[c("matched", "interviewed_unmatched", "found_not_interviewed", "not_found", "not_in_endline_target")],
  bty = "n",
  cex = 0.95
)

title("Where sample loss occurs by treatment arm")
par(mar = old_mar)
dev.off()

message("Wrote retention plot to: ", retention_png)
message("Wrote counts-plus-percentages table to: ", file.path(dir_tables, "sample_flow_counts_pct.tex"))
message("Wrote decomposition bar chart to: ", decomp_png)
