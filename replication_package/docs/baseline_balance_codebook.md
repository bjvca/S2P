---
title: "Baseline Balance Dataset Codebook"
subtitle: "Space to Place (S2P) replication package"
date: "April 2026"
geometry: margin=1in
---

# Scope

This codebook documents `data/analysis/baseline_balance_sample.csv`, the de-identified baseline extract used to reproduce Table 1 of the S2P paper. The extract comes from the 2022 source-study data used to construct the S2P sampling frame. It is not the full raw baseline dataset.

The file contains only the fields needed to reconstruct the five baseline covariates used in the balance table, plus treatment assignment and cluster identifiers. Names, phone numbers, GPS coordinates, and other private raw fields are excluded.

# Dataset Summary

| Item | Value |
| --- | --- |
| Rows | 2034 |
| Treatment arms | C=612; T1=720; T2=702 |
| Clusters | 113 |
| Rows per cluster | 18 to 18 |

# Variables In The Replication Dataset

| Variable | Type | Label | Role | Constraint | Relevant |
| --- | --- | --- | --- | --- | --- |
| farmer_ID | string | Anonymized farmer identifier in the replication package extract | Anonymized farmer identifier |  |  |
| treat | categorical | S2P treatment assignment: C, T1, or T2 | S2P experimental arm |  |  |
| clusterID | integer | Village/cluster identifier used for clustered standard errors | Village/cluster identifier |  |  |
| q13 | select_one q13 | Q13. What is your SEX? | Input for household-head sex |  |  |
| q14 | integer | Q14. What is your age? | Input for household-head age | . >=15 and .<=99 or .=999 |  |
| q16 | select_one q13 | Q16. Sex of the household head: | Input for household-head sex |  | ${q12}!='1' |
| q17 | integer | Q17. Age of the Household Head: | Input for household-head age | . >=15 and .<=99 or .=999 | ${q12}!='1' |
| q19 | integer | Q19. How big is the household? | Household size | . >=0 and .<=40 or .=999 |  |
| q54a | decimal | Q54a. How many acres of maize did you plant? | Input for land area | . >=0.1 and .< 5000 or .=999 | ${q53}='Yes' |
| q58a | decimal | Q58a. How many acres of groundnuts did you grow? | Input for land area | . >=0.1 and .< 5000 or .=999 | ${q58}='Yes' |
| q62a | decimal | Q62a. How many acres of soybean did you plant? | Input for land area | . >=0.01 and .< 1500 | ${q62}='Yes' |
| q69 | select_one q69 | Q69. In the last year: how often did your household have problems satisfying food needs of the household? | Input for food-needs difficulty |  |  |

# Choices For Categorical Variables

| List | Value | Label |
| --- | --- | --- |
| q13 | Male | Male |
| q13 | Female | Female |
| q69 | 1 | Never |
| q69 | 2 | Seldom |
| q69 | 3 | Sometimes |
| q69 | 4 | Often |
| q69 | 5 | Very often, nearly always |

# Derived Balance Variables

| Derived_variable | Construction | Used_in_table |
| --- | --- | --- |
| age_head | Household-head age q17 where reported; respondent age q14 otherwise. Values 999 are treated as missing. | Household head age (years) |
| male_head | Household-head sex q16 where reported; respondent sex q13 otherwise. Indicator equals 1 for Male. | Household head is male (1=yes) |
| HH_size | Household size q19. Values above 15 are treated as outliers in the original balance-table construction. | Household size (number) |
| land_size_ha | Sum of q54a, q58a, and q62a, after setting values >= 100 to missing, divided by 2.471 to convert acres to hectares. | Land area (ha) |
| poor | Indicator equal to 1 when q69 is 4 (Often) or 5 (Very often, nearly always). | Had difficulties meeting food needs in last year (1=yes) |

# Notes On Cleaning Rules

- Ages use `999` as a don't-know code and are recoded to missing.
- Household-head age and sex use respondent age/sex when the respondent is the household head and household-head-specific fields are therefore not asked.
- Household size values above 15 are treated as outliers in the original balance-table construction, even though the questionnaire allowed values up to 40 or 999.
- Crop acreage variables are top-coded by setting values greater than or equal to 100 to missing before summing maize, groundnut, and soybean acreage.
- If all crop acreage components are missing after top-coding, land area is set to missing. This corrects the original `rowSums(..., na.rm = TRUE)` behavior, which would otherwise return zero for all-missing rows.
- A cleaning diagnostics file is written to `output/logs/table1_balance_cleaning_diagnostics.csv` when Table 1 is regenerated.

# Relationship To Table 1

The replication script `code/R/01_table1_balance.R` uses these fields to construct `age_head`, `male_head`, `HH_size`, `land_size_ha`, and `poor`, then estimates balance regressions with standard errors clustered at the village/cluster level. The manuscript table reports control means and treatment-arm differences relative to control.
