# Project: S2P paper revision + blog post

## Objective
Finalise `paper/S2PIEpaper.tex` for submission and revise blog post for publication. Core experiment: T1 = soil-test recommendation only; T2 = recommendation + fungible voucher (~$100 AIP-equivalent). Central finding: voucher raises fertilizer use and yield but not recommendation compliance; product mix mirrors AIP bundle, not soil-test prescription.

## Current Status
Blog post fully revised in response to Ruth Stewart feedback (all 4 points addressed, GitHub `62a2a4b`). Co-authors pushed changes to Overleaf (now at `40c66c7`) with a programme rename (AIP→FISP) and two factual corrections. Awaiting Bjorn's confirmation on whether changes are deliberate before acting.

## Completed Tasks
- [x] All table notes switched to "unadjusted ITT"
- [x] Fixed `de2024expecting` bib entry
- [x] Blog factual fixes (yield 33%→19%, affiliations, potassium share, lime rate, 8→5 takeaways)
- [x] Blog: all four Ruth Stewart points addressed (recommendation-null para rewrite, credibility claim removed, AIP institutional-prior added to paras 25 & 27, generalizability caveats in paras 8 & 23)
- [x] Wiki: external literature section added to `ict-agricultural-extension.md` (4 papers)
- [x] Pulled Overleaf changes (commit `40c66c7`) — stashed .aux/.log/.PROGRESS.md artifacts

## In Progress
- [ ] Awaiting Bjorn confirmation on co-author Overleaf changes (see Next Steps)

## Next Steps
- [ ] **DECISION NEEDED — AIP→FISP rename:** Co-authors replaced every "AIP" with "FISP" throughout the paper and updated the description to "Farm Input Subsidy Programme (FISP) from 2020 to 2025" (implying the programme has reverted to FISP name as of 2025/26). If confirmed: blog also needs a full AIP→FISP sweep (AIP appears ~12 times there).
- [ ] **DECISION NEEDED — District correction:** "Lilongwe" → "Kasungu" in the four study districts. Confirm this is correct.
- [ ] **DECISION NEEDED — Intervention timing:** "September and October 2024" → "October and November 2024". Confirm this is correct.
- [ ] **Paper abstract (line 113):** "maize yield rises by roughly a third" is stale (old adjusted-spec 33%) — fix to "19 percent" (total production) or "44 percent" (log yield per acre); confirm which to use.
- [ ] Push replication package R script changes to outer GitHub (06, 08, 09 scripts + output logs/tex files are unstaged)
- [ ] Lee (2009) trimming bounds for T2 retention differential — still unaddressed
- [ ] AEA registration post-dates rollout — not yet addressed
- [ ] Five §4-deleted PAP deviations still have no in-paper home — referee risk

## Decisions / Notes
- **Co-author Overleaf changes (commit `40c66c7`):** Three edits — (1) AIP→FISP rename throughout, "from 2020 to 2025" added, tense changes; (2) district Lilongwe→Kasungu; (3) timing Sep/Oct→Oct/Nov 2024. All need confirmation before propagating to blog.
- **Working blog file is `blog/blog_S2P_final.docx`** — GitHub latest = `62a2a4b`; still uses "AIP" throughout pending FISP decision
- **New file in GitHub:** `blog/blog_post_final_2.docx` was pushed by a co-author (pulled in `a28f515`) — unclear if this is a revised version; check contents
- **Nyondo et al. (2025):** Key citation — Malawi cluster RCT, no effect on fertilizer quantities, attributed to AIP/FISP. DOI: 10.1016/j.foodpol.2025.102850
- **Paper abstract "roughly a third"** — stale; unadjusted results are 19% (Panel A) or 44% (Panel B log yield/acre); body uses 19%
- **No controls in any reported regression.** All tables use unadjusted ITT only.
- **No em dashes** anywhere in paper. Hard constraint.
- **No Co-Authored-By trailer** in any commit message.
- **Nested git structure.** Outer GitHub at `/workspace/S2P`; inner Overleaf at `paper/`. Always pull both before pushing. Stash .aux/.log before pulling Overleaf.

## Issues / Blockers
- Paper abstract still has stale yield figure ("roughly a third")
- AIP→FISP rename decision pending — blog and paper need to be in sync
- Five §4-deleted PAP deviations have no in-paper home — open referee risk
- AEA registration post-dates rollout — not yet addressed
- Lee bounds for T2 retention differential not yet computed

## Key Files
- `paper/S2PIEpaper.tex` — main manuscript; Overleaf HEAD = `40c66c7` (AIP→FISP changes by co-authors)
- `paper/s2p_bib.bib` — bibliography
- `blog/blog_S2P_final.docx` — latest blog; all Ruth edits applied; GitHub = `62a2a4b`; still uses AIP
- `blog/blog_post_final_2.docx` — pushed by co-author (pulled `a28f515`); contents unknown
- `replication_package/code/R/06_table2_fertilizer_use_audit.R` — unstaged changes
- `replication_package/code/R/08_table4_nutrient_use.R` — unstaged changes
- `replication_package/code/R/09_table5_maize_yield.R` — unstaged changes
- `wiki/articles/ict-agricultural-extension.md` — updated with external literature section (4 papers)
