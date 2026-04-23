# S2P Replication Package

This folder is the working replication package for the S2P paper. It should contain only the code and data needed to reproduce tables, figures, and statistics reported in the paper.

## Current Scope

The package currently reproduces Table 1, the baseline balance table, using the saved object:

```text
data/analysis/balance_2022.Rdata
```

This object is copied from `sampling/balance_2022.Rdata`. It contains the balance-table results used in the PAP and current manuscript. It is enough to reproduce the displayed table, but not enough to audit the construction from household-level baseline microdata.

## Structure

```text
code/R/            R scripts for paper outputs
code/stata/        Stata scripts for cross-checks where available
data/analysis/     analysis datasets or compact table inputs
output/tables/     generated LaTeX tables
output/logs/       logs and diagnostics
run_all.R          runs all current replication scripts
```

## Running

From this folder:

```bash
Rscript run_all.R
```

The current output is:

```text
output/tables/table1_balance.tex
```

To copy generated paper-ready tables into the Overleaf manuscript repository:

```bash
Rscript code/R/sync_outputs_to_paper.R
```

This copies generated `.tex` tables from:

```text
replication_package/output/tables/
```

to:

```text
paper/tables/
```

The manuscript should include synced tables using paths such as:

```latex
\input{tables/table1_balance.tex}
```

The Stata version of Table 1 can be run from the same folder if Stata is installed:

```bash
stata -b do code/stata/01_table1_balance.do
```

It writes:

```text
output/tables/table1_balance_stata.tex
output/logs/table1_balance_stata_matrix.csv
```

The Stata script is intended as an independent cross-check. Its basic coefficients,
means, standard deviations, and sample sizes should match the R output. Some p-values
may differ because the R script uses `clubSandwich` CR3/Satterthwaite standard errors
for the row-level balance regressions.

## Rules

- Include only data needed to reproduce paper outputs.
- Do not include private raw data, GPS-identifiable raw exports, phone numbers, names, device metadata, or unredacted ODK metadata.
- Prefer final analysis datasets over raw data when raw data cannot be made public.
- Each paper table or figure should have one script with a stable name, for example `02_table2_fertilizer_use.R`.
- Scripts should write outputs to `output/tables/` or `output/figures/`, not into the manuscript folder.

## Missing Inputs

To audit and reproduce the remaining paper tables, this package still needs:

- the cleaned analysis dataset used for the current results tables;
- the Stata or R scripts used by coauthors to generate `Table 2.tex`, `Table 3.tex`, `Table 4.tex`, `Table 5.tex`, `total_farm_exp.tex`, `farm_profits.tex`, and `SNM_practices*.tex`;
- a variable dictionary or codebook for treatment variables, outcomes, controls, cluster identifiers, and sample restrictions.
