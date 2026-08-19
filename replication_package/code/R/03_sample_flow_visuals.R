# Visual and tabular presentation options for sample flow and attrition.
#
# This script turns the audit outputs from 02_sample_flow_attrition.R into:
#   1. A main-text retention plot in percentages by treatment arm and stage.
#   2. A manuscript table that combines counts and percentages.
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
  "Interview\ncompleted",
  "Matched to\nsoil test data"
)

stage_counts <- rbind(
  sample_flow$baseline_sample_n,
  sample_flow$endline_target_n,
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

suppressPackageStartupMessages({ library(ggplot2) })

arm_names <- c("C" = "Control", "T1" = "Treatment 1", "T2" = "Treatment 2")
arm_cols <- c("Control" = "#4E79A7", "Treatment 1" = "#F28E2B", "Treatment 2" = "#59A14F")

plot_df <- do.call(rbind, lapply(arm_levels, function(arm) {
  data.frame(
    stage = factor(stage_labels, levels = stage_labels),
    arm = arm_names[[arm]],
    pct = stage_pct[, arm],
    stringsAsFactors = FALSE
  )
}))
end_df <- plot_df[plot_df$stage == stage_labels[length(stage_labels)], ]
end_df$label <- sprintf("%s: %.1f%%", names(arm_names)[match(end_df$arm, arm_names)], end_df$pct)

g_ret <- ggplot(plot_df, aes(x = stage, y = pct, colour = arm, group = arm, shape = arm)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  geom_text(data = end_df, aes(label = label), hjust = 0, nudge_x = 0.08,
            size = 3.4, show.legend = FALSE) +
  scale_colour_manual(values = arm_cols) +
  scale_shape_manual(values = c("Control" = 16, "Treatment 1" = 17, "Treatment 2" = 15)) +
  scale_y_continuous(limits = c(60, 102), breaks = seq(60, 100, by = 10)) +
  scale_x_discrete(expand = expansion(add = c(0.15, 0.95))) +
  labs(x = NULL, y = "Percent of baseline sampled households",
       colour = NULL, shape = NULL) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.14, 0.18),
        legend.background = element_blank())

ggsave(retention_png, g_ret, width = 6.5, height = 4.2, dpi = 200)

# -----------------------------------------------------------------------------
# Table: counts plus percentages by arm
# -----------------------------------------------------------------------------

fmt_npct <- function(n, denom) {
  sprintf("%s (%.1f\\%%)", fmt_num(n, 0), 100 * n / denom)
}

table_lines <- c(
  "\\begin{tabular}{lcccc}",
  "\\hline\\hline",
  "Stage & Control & Treatment 1 & Treatment 2 & Total \\\\",
  "\\hline"
)

total_baseline_n <- sum(sample_flow$baseline_sample_n)

for (i in seq_len(nrow(stage_counts))) {
  stage <- rownames(stage_counts)[i]
  table_lines <- c(
    table_lines,
    sprintf(
      "%s & %s & %s & %s & %s \\\\",
      stage,
      fmt_npct(stage_counts[i, "C"], sample_flow$baseline_sample_n[sample_flow$treatment == "C"]),
      fmt_npct(stage_counts[i, "T1"], sample_flow$baseline_sample_n[sample_flow$treatment == "T1"]),
      fmt_npct(stage_counts[i, "T2"], sample_flow$baseline_sample_n[sample_flow$treatment == "T2"]),
      fmt_npct(sum(stage_counts[i, ]), total_baseline_n)
    )
  )
}

table_lines <- c(
  table_lines,
  "\\hline\\hline",
  "\\end{tabular}"
)

writeLines(table_lines, file.path(dir_tables, "sample_flow_counts_pct.tex"))

message("Wrote retention plot to: ", retention_png)
message("Wrote counts-plus-percentages table to: ", file.path(dir_tables, "sample_flow_counts_pct.tex"))
