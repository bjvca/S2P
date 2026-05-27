# Project: S2P paper revision + blog post

## Objective
Finalise `paper/S2PIEpaper.tex` for submission and revise blog post for publication. Core experiment: T1 = soil-test recommendation only; T2 = recommendation + fungible voucher (~$100 AIP-equivalent). Central finding: voucher raises fertilizer use and yield but not recommendation compliance; product mix mirrors AIP bundle, not soil-test prescription.

## Current Status
Paper is compiled and pushed to Overleaf (commit `f79613e`). Blog post editorial pass is partially complete — factual fixes applied, stylistic rewrites suggested but not yet implemented. Latest blog version is `blog/blog_S2P_final.docx` (uploaded by Bjorn 2026-05-27).

## Completed Tasks
- [x] Switched ALL table notes and body text to "unadjusted ITT" — removed all "preferred adjusted specification" language throughout paper
- [x] Fixed `de2024expecting` bib entry: `@Article` (no journal) → `@Unpublished` with `note = {Mimeo}`
- [x] Fixed Table 6 note: "preferred adjusted ITT" → "Unadjusted ITT effects"
- [x] Fixed compliance tables 8 and 9 notes: removed full adjusted-controls description
- [x] Fixed appendix robustness table notes (profit and yield sensitivity)
- [x] Blog: yield figure 33% → 19% (3 occurrences: lede, takeaways, body)
- [x] Blog: potassium share "around 70%" → "roughly two-thirds"
- [x] Blog: removed unverified lime application rate (950 kg/ha)
- [x] Blog: fixed 4 author affiliations (Thomas → Agrifood Innovation and Resilience Unit; Jonathan → Solidaridad Southern Africa; David → School of Public Health, Washington University in St. Louis; Bjorn → MTI Unit)
- [x] Blog: clarified 18+19 kg product split comes from separate product-level regressions
- [x] Blog: condensed 8 key takeaways → 5
- [x] All blog changes pushed to GitHub outer repo

## In Progress
- [ ] Blog stylistic rewrites — suggestions made but not yet applied to `blog_S2P_final.docx`

## Next Steps
- [ ] **Paper abstract (line 113):** "maize yield rises by roughly a third" is the stale 33% adjusted-spec figure — fix to "19 percent" (total production) or "44 percent" (log yield per acre); confirm with Bjorn which to use
- [ ] **Blog `blog_S2P_final.docx`:** Apply agreed stylistic rewrites (see Decisions/Notes below for the list of pending suggestions)
- [ ] **Blog:** Decide whether to apply Ukraine/Hormuz fertilizer-price addition
- [ ] Push replication package R script changes to outer GitHub (06, 08, 09 scripts + output logs/tex files are unstaged)
- [ ] Lee (2009) trimming bounds for T2 retention differential — still unaddressed
- [ ] AEA registration post-dates rollout — not yet addressed
- [ ] Five §4-deleted PAP deviations still have no in-paper home — referee risk

## Decisions / Notes
- **Working blog file is now `blog/blog_S2P_final.docx`** — Bjorn uploaded this 2026-05-27; supersedes `blog_post_draft_all.docx`
- **Paper abstract "roughly a third"** — stale; comes from old adjusted spec (exp(0.291)−1 ≈ 34%); unadjusted results are 19% (Panel A, total production) or 44% (Panel B, log yield per acre); paper body uses 19%
- **Pending blog suggestion 1:** "alternative version of AIP" → "an extension of AIP" (preferred wording; implies additive not substitutive, consistent with data showing partial rather than full crowd-out)
- **Pending blog suggestion 2:** Potassium bullet split into two: (a) prescribed to two-thirds, often in place of urea, compliance zero; (b) mirror image — urea prescribed to <10% of households yet largest single component of voucher increment
- **Pending blog suggestion 3:** Joachim JD5 fix — "pulled away from the distinctive components" → "not pulled toward the distinctive components" (farmers didn't use less recommended products relative to control; they just didn't use more)
- **Pending blog suggestion 4:** Implications paragraph rewrite — reframe from supply-side-only problem to need for complementary innovations on both supply (stocking, package sizes, blue-spoon dosing tools) and demand (soil health training, behavioral nudges, trust, credit/risk)
- **Pending blog suggestion 5:** Fertilizer price sentence — optionally tie to Ukraine war (gas markets, urea) and Strait of Hormuz (Gulf LNG) as concrete drivers of price volatility
- **No controls in any reported regression.** All tables use unadjusted ITT only.
- **No em dashes** anywhere in paper. Hard constraint.
- **No Co-Authored-By trailer** in any commit message.
- **Nested git structure.** Outer GitHub at `/workspace/S2P`; inner Overleaf at `paper/`. `paper/` is gitignored in outer repo. Pull GitHub before pushing from workspace (`git pull origin master` from `/workspace/S2P`). Pull Overleaf before pushing (`git stash && git pull --rebase origin master && git stash pop` from `paper/`).

## Issues / Blockers
- Paper abstract still has stale yield figure ("roughly a third")
- Five §4-deleted PAP deviations have no in-paper home — open referee risk
- AEA registration post-dates rollout — not yet addressed
- Lee bounds for T2 retention differential not yet computed

## Key Files
- `paper/S2PIEpaper.tex` — main manuscript; Overleaf HEAD = `f79613e`
- `paper/s2p_bib.bib` — bibliography; `de2024expecting` is now `@Unpublished`
- `blog/blog_S2P_final.docx` — **latest blog version** (uploaded by Bjorn 2026-05-27)
- `blog/blog_post_draft_all.docx` — earlier working version (factual fixes applied)
- `blog/blog_post_draft_all.txt` — plain-text mirror of above
- `replication_package/code/R/06_table2_fertilizer_use_audit.R` — unstaged changes
- `replication_package/code/R/08_table4_nutrient_use.R` — unstaged changes
- `replication_package/code/R/09_table5_maize_yield.R` — unstaged changes
