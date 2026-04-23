# Create a human-readable codebook for the baseline balance dataset.
#
# Input files:
#   data/analysis/baseline_balance_sample.csv
#   data/questionnaires/baseline_questionnaire.xls
#
# Outputs:
#   docs/baseline_balance_codebook.md
#   docs/baseline_balance_codebook.pdf
#
# The codebook is intentionally scoped to the dataset included in the
# replication package. It does not document private raw baseline fields that
# are not needed for the paper's baseline balance table.

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
  library(readxl)
})

dir_docs <- file.path(replication_root, "docs")
if (!dir.exists(dir_docs)) dir.create(dir_docs, recursive = TRUE)

data_path <- file.path(dir_data, "baseline_balance_sample.csv")
questionnaire_path <- file.path(replication_root, "data", "questionnaires", "baseline_questionnaire.xls")

if (!file.exists(data_path)) stop("Missing data file: ", data_path)
if (!file.exists(questionnaire_path)) stop("Missing questionnaire file: ", questionnaire_path)

baseline <- read.csv(data_path, stringsAsFactors = FALSE)
survey <- read_excel(questionnaire_path, sheet = "survey")
choices <- read_excel(questionnaire_path, sheet = "choices")

balance_fields <- c(
  "farmer_ID", "treat", "clusterID",
  "q13", "q14", "q16", "q17", "q19",
  "q54a", "q58a", "q62a", "q69"
)

field_map <- data.frame(
  variable = balance_fields,
  role = c(
    "Anonymized farmer identifier",
    "S2P experimental arm",
    "Village/cluster identifier",
    "Input for household-head sex",
    "Input for household-head age",
    "Input for household-head sex",
    "Input for household-head age",
    "Household size",
    "Input for land area",
    "Input for land area",
    "Input for land area",
    "Input for food-needs difficulty"
  ),
  stringsAsFactors = FALSE
)

survey_named <- survey[!is.na(survey$name), ]
survey_subset <- survey_named[survey_named$name %in% balance_fields, ]
survey_subset <- survey_subset[match(intersect(balance_fields, survey_subset$name), survey_subset$name), ]

get_label <- function(var) {
  custom <- switch(
    var,
    farmer_ID = "Anonymized farmer identifier in the replication package extract",
    treat = "S2P treatment assignment: C, T1, or T2",
    clusterID = "Village/cluster identifier used for clustered standard errors",
    NULL
  )
  if (!is.null(custom)) return(custom)

  if (var %in% survey_named$name) {
    out <- survey_named[survey_named$name == var, "label::English (en)", drop = TRUE][1]
    if (!is.na(out)) return(out)
  }
  ""
}

get_type <- function(var) {
  custom <- switch(
    var,
    farmer_ID = "string",
    treat = "categorical",
    clusterID = "integer",
    NULL
  )
  if (!is.null(custom)) return(custom)

  if (var %in% survey_named$name) {
    out <- survey_named[survey_named$name == var, "type", drop = TRUE][1]
    if (!is.na(out)) return(out)
  }
  ""
}

get_constraint <- function(var) {
  if (var %in% survey_named$name) {
    out <- survey_named[survey_named$name == var, "constraint", drop = TRUE][1]
    if (!is.na(out)) return(out)
  }
  ""
}

get_relevant <- function(var) {
  if (var %in% survey_named$name) {
    out <- survey_named[survey_named$name == var, "relevant", drop = TRUE][1]
    if (!is.na(out)) return(out)
  }
  ""
}

clean_cell <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- gsub("\\|", "\\\\|", x)
  x <- gsub("\n", " ", x)
  x
}

write_table <- function(rows) {
  header <- names(rows)
  out <- c(
    paste0("| ", paste(header, collapse = " | "), " |"),
    paste0("| ", paste(rep("---", length(header)), collapse = " | "), " |")
  )
  for (i in seq_len(nrow(rows))) {
    out <- c(out, paste0("| ", paste(clean_cell(rows[i, ]), collapse = " | "), " |"))
  }
  out
}

variable_table <- data.frame(
  Variable = balance_fields,
  Type = vapply(balance_fields, get_type, character(1)),
  Label = vapply(balance_fields, get_label, character(1)),
  Role = field_map$role,
  Constraint = vapply(balance_fields, get_constraint, character(1)),
  Relevant = vapply(balance_fields, get_relevant, character(1)),
  stringsAsFactors = FALSE
)

choice_rows <- choices[
  choices[["list name"]] %in% c("q13", "q69"),
  c("list name", "name", "label")
]
choice_rows <- choice_rows[!is.na(choice_rows$name), ]
names(choice_rows) <- c("List", "Value", "Label")

derived_table <- data.frame(
  Derived_variable = c("age_head", "male_head", "HH_size", "land_size_ha", "poor"),
  Construction = c(
    "Household-head age q17 where reported; respondent age q14 otherwise. Values 999 are treated as missing.",
    "Household-head sex q16 where reported; respondent sex q13 otherwise. Indicator equals 1 for Male.",
    "Household size q19. Values above 15 are treated as outliers in the original balance-table construction.",
    "Sum of q54a, q58a, and q62a, after setting values >= 100 to missing, divided by 2.471 to convert acres to hectares.",
    "Indicator equal to 1 when q69 is 4 (Often) or 5 (Very often, nearly always)."
  ),
  Used_in_table = c(
    "Household head age (years)",
    "Household head is male (1=yes)",
    "Household size (number)",
    "Land area (ha)",
    "Had difficulties meeting food needs in last year (1=yes)"
  ),
  stringsAsFactors = FALSE
)

dataset_summary <- data.frame(
  Item = c(
    "Rows",
    "Treatment arms",
    "Clusters",
    "Rows per cluster"
  ),
  Value = c(
    as.character(nrow(baseline)),
    paste(names(table(baseline$treat)), as.integer(table(baseline$treat)), sep = "=", collapse = "; "),
    as.character(length(unique(baseline$clusterID))),
    paste(range(as.integer(table(baseline$clusterID))), collapse = " to ")
  ),
  stringsAsFactors = FALSE
)

md <- c(
  "---",
  "title: \"Baseline Balance Dataset Codebook\"",
  "subtitle: \"Space to Place (S2P) replication package\"",
  "date: \"April 2026\"",
  "geometry: margin=1in",
  "---",
  "",
  "# Scope",
  "",
  "This codebook documents `data/analysis/baseline_balance_sample.csv`, the de-identified baseline extract used to reproduce Table 1 of the S2P paper. The extract comes from the 2022 source-study data used to construct the S2P sampling frame. It is not the full raw baseline dataset.",
  "",
  "The file contains only the fields needed to reconstruct the five baseline covariates used in the balance table, plus treatment assignment and cluster identifiers. Names, phone numbers, GPS coordinates, and other private raw fields are excluded.",
  "",
  "# Dataset Summary",
  "",
  write_table(dataset_summary),
  "",
  "# Variables In The Replication Dataset",
  "",
  write_table(variable_table),
  "",
  "# Choices For Categorical Variables",
  "",
  write_table(choice_rows),
  "",
  "# Derived Balance Variables",
  "",
  write_table(derived_table),
  "",
  "# Notes On Cleaning Rules",
  "",
  "- Ages use `999` as a don't-know code and are recoded to missing.",
  "- Household-head age and sex use respondent age/sex when the respondent is the household head and household-head-specific fields are therefore not asked.",
  "- Household size values above 15 are treated as outliers in the original balance-table construction, even though the questionnaire allowed values up to 40 or 999.",
  "- Crop acreage variables are top-coded by setting values greater than or equal to 100 to missing before summing maize, groundnut, and soybean acreage.",
  "- The original balance-table code used `rowSums(..., na.rm = TRUE)` for land size. If all crop acreage components are missing, this returns zero. This should be treated as a coding issue to review before finalizing the replication package.",
  "",
  "# Relationship To Table 1",
  "",
  "The replication script `code/R/01_table1_balance.R` uses these fields to construct `age_head`, `male_head`, `HH_size`, `land_size_ha`, and `poor`, then estimates balance regressions with standard errors clustered at the village/cluster level. The manuscript table reports control means and treatment-arm differences relative to control."
)

md_path <- file.path(dir_docs, "baseline_balance_codebook.md")
pdf_path <- file.path(dir_docs, "baseline_balance_codebook.pdf")

writeLines(md, md_path)

pandoc_cmd <- sprintf(
  "pandoc %s -o %s --pdf-engine=pdflatex",
  shQuote(md_path),
  shQuote(pdf_path)
)
status <- system(pandoc_cmd)
if (status != 0) {
  stop("pandoc failed with status ", status)
}

message("Wrote ", md_path)
message("Wrote ", pdf_path)
