## soil_balance_tables.R
## Generates LaTeX pairwise balance tables for soil chemistry across T1, T2, C
## Output: ../results/soil_balance_tables.tex

# --- Load data ---
soil <- read.csv("../data/raw/harmonized_soil_data.csv", stringsAsFactors = FALSE)
linkfile <- read.csv("../../listing/data/clean/linkfile.csv", stringsAsFactors = FALSE)

# Merge treatment assignment
soil <- merge(soil, linkfile[, c("soil_sample_ID", "treat")],
              by.x = "barcode_base", by.y = "soil_sample_ID", all.x = TRUE)
soil <- soil[soil$treat %in% c("T1", "T2", "C"), ]

# --- Define variables ---
vars <- c("pH", "phosphorus_ppm", "potassium_ppm", "calcium_ppm",
          "magnesium_ppm", "iron_ppm", "manganese_ppm", "copper_ppm",
          "zinc_ppm", "boron_ppm", "cec", "total_nitrogen_pct")

var_labels <- c("pH", "Phosphorus (ppm)", "Potassium (ppm)", "Calcium (ppm)",
                "Magnesium (ppm)", "Iron (ppm)", "Manganese (ppm)", "Copper (ppm)",
                "Zinc (ppm)", "Boron (ppm)", "CEC (meq/100g)", "Total N (\\%)")

# --- Pairwise Welch t-tests ---
run_pair <- function(df, g1, g2, vars) {
  d1 <- df[df$treat == g1, ]
  d2 <- df[df$treat == g2, ]
  results <- vector("list", length(vars))
  for (i in seq_along(vars)) {
    v <- vars[i]
    x <- as.numeric(d1[[v]])
    y <- as.numeric(d2[[v]])
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
    if (length(x) < 2 || length(y) < 2) next
    tt <- t.test(x, y)
    sp <- sqrt(((length(x) - 1) * var(x) + (length(y) - 1) * var(y)) /
               (length(x) + length(y) - 2))
    d <- if (sp > 0) (mean(x) - mean(y)) / sp else 0
    results[[i]] <- data.frame(
      variable = v,
      mean1 = mean(x), mean2 = mean(y), diff = mean(x) - mean(y),
      t_stat = unname(tt$statistic), df = unname(tt$parameter),
      p_value = tt$p.value, cohens_d = d,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, results)
}

pairs <- list(
  list(g1 = "T1", g2 = "T2", lab = "same lab: Cropnuts"),
  list(g1 = "T1", g2 = "C",  lab = "different labs: Cropnuts vs.\\ FES"),
  list(g1 = "T2", g2 = "C",  lab = "different labs: Cropnuts vs.\\ FES")
)

# --- Group sizes ---
n_t1 <- sum(soil$treat == "T1")
n_t2 <- sum(soil$treat == "T2")
n_c  <- sum(soil$treat == "C")

# --- Format helpers ---
fmt_num <- function(x, digits = 3) formatC(x, format = "f", digits = digits)

fmt_p <- function(p) {
  if (p < 0.001) return("$<$0.001")
  formatC(p, format = "f", digits = 3)
}

sig_stars <- function(p) {
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  if (p < 0.10) return("\\dag")
  ""
}

# --- Build LaTeX ---
lines <- c(
  "\\documentclass[12pt]{article}",
  "\\usepackage{booktabs}",
  "\\usepackage{caption}",
  "\\usepackage{geometry}",
  "\\geometry{margin=1in}",
  "\\begin{document}",
  ""
)

for (pair in pairs) {
  res <- run_pair(soil, pair$g1, pair$g2, vars)
  g1 <- pair$g1; g2 <- pair$g2
  n1 <- if (g1 == "T1") n_t1 else if (g1 == "T2") n_t2 else n_c
  n2 <- if (g2 == "T1") n_t1 else if (g2 == "T2") n_t2 else n_c

  lines <- c(lines,
    "\\begin{table}[htbp]",
    "\\centering",
    sprintf("\\caption{Soil chemistry balance: %s vs.\\ %s (%s; $n$: %s\\,=\\,%d, %s\\,=\\,%d)}",
            g1, g2, pair$lab, g1, n1, g2, n2),
    sprintf("\\label{tab:balance_%s_%s}", tolower(g1), tolower(g2)),
    "\\small",
    "\\begin{tabular}{lrrrrrrrl}",
    "\\toprule",
    sprintf("Variable & %s mean & %s mean & Diff & $t$ & df & $p$-value & Cohen's $d$ & \\\\",
            g1, g2),
    "\\midrule"
  )

  for (i in seq_len(nrow(res))) {
    r <- res[i, ]
    label <- var_labels[match(r$variable, vars)]
    stars <- sig_stars(r$p_value)
    line <- sprintf("%s & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
                    label,
                    fmt_num(r$mean1),
                    fmt_num(r$mean2),
                    fmt_num(r$diff),
                    fmt_num(r$t_stat, 2),
                    fmt_num(r$df, 0),
                    fmt_p(r$p_value),
                    fmt_num(r$cohens_d),
                    stars)
    lines <- c(lines, line)
  }

  lines <- c(lines,
    "\\bottomrule",
    "\\multicolumn{9}{l}{\\footnotesize $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$; $^{\\dag}p<0.10$. Welch's $t$-test (unequal variances).} \\\\",
    "\\end{tabular}",
    "\\end{table}",
    ""
  )
}

lines <- c(lines, "\\end{document}")

# --- Write output ---
outfile <- "../results/soil_balance_tables.tex"
writeLines(lines, outfile)
cat("Written to", normalizePath(outfile), "\n")
