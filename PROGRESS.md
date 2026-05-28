# Project: S2P paper revision + blog post

## Objective
Finalise `paper/S2PIEpaper.tex` for submission and revise blog post for publication. Core experiment: T1 = soil-test recommendation only; T2 = recommendation + fungible voucher (~$100 AIP-equivalent). Central finding: voucher raises fertilizer use and yield but not recommendation compliance; product mix mirrors AIP bundle, not soil-test prescription.

## Current Status
Blog post has been substantially revised in response to expert feedback from Ruth Stewart (received 2026-05-28). All four of Ruth's points addressed and pushed to GitHub (latest commit: `62a2a4b`). Paper abstract still has stale yield figure.

## Completed Tasks
- [x] Switched ALL table notes and body text to "unadjusted ITT" — removed all "preferred adjusted specification" language throughout paper
- [x] Fixed `de2024expecting` bib entry: `@Article` → `@Unpublished` with `note = {Mimeo}`
- [x] Fixed Table 6, 8, 9 and appendix robustness table notes
- [x] Blog factual fixes: yield 33%→19%, potassium share, affiliations, lime rate removed, 8→5 takeaways, product split clarification
- [x] All prior blog changes pushed to GitHub outer repo
- [x] Blog: rewrote "recommendation alone" paragraph — replaced "consistent with the broader literature" framing with account anchored in Nyondo (2025) Malawi precedent and voucher arm as direct test of AIP-entrenchment mechanism (Ruth point 1)
- [x] Blog: removed "credible, well-resourced commercial agronomy service" credibility claim entirely (Ruth point 2)
- [x] Blog para 25: added AIP institutional-prior sentence naming programme-shaped expectations as more fundamental than supply-demand constraints (Ruth points 3 & 4)
- [x] Blog para 27: added context-specificity caveat — binding constraint may be institutional prior, not value chain links (Ruth points 3 & 4)
- [x] Blog para 8 (key takeaway 5): scoped AI-tools caveat to "similar settings" (Ruth generalizability sweep)
- [x] Blog para 23 (Tanzania contrast): added sentence that Malawi null may be context-specific, not universal property of voucher+recommendation (Ruth generalizability sweep)
- [x] Wiki: added external literature section to `articles/ict-agricultural-extension.md` with full entries for Fabregas et al. (2025), Islam & Beg (2021), Maertens et al. (2023), Ayalew et al. (2022) — all relevant to S2P information-null framing

## In Progress
- [ ] Nothing actively in progress

## Next Steps
- [ ] **Paper abstract (line 113):** "maize yield rises by roughly a third" is stale (old adjusted-spec 33%) — fix to "19 percent" (total production) or "44 percent" (log yield per acre); confirm with Bjorn which to use
- [ ] **Blog:** Decide whether remaining pending stylistic suggestions still apply (see Decisions/Notes)
- [ ] Push replication package R script changes to outer GitHub (06, 08, 09 scripts + output logs/tex files are unstaged)
- [ ] Lee (2009) trimming bounds for T2 retention differential — still unaddressed
- [ ] AEA registration post-dates rollout — not yet addressed
- [ ] Five §4-deleted PAP deviations still have no in-paper home — referee risk

## Decisions / Notes
- **Working blog file is `blog/blog_S2P_final.docx`** — all Ruth edits applied here; GitHub latest = `62a2a4b`
- **Ruth Stewart feedback (2026-05-28):** Four points, all addressed. Core: null result is Malawi/AIP-specific, not a universal information-failure finding; supply-demand framing secondary to institutional prior; generalizability caveats needed throughout.
- **Nyondo et al. (2025) Food Policy:** Key citation — Malawi cluster RCT, soil-test advice raises complementary practices but does NOT move fertilizer quantities; authors attribute this to AIP. Our voucher arm is the direct confirmatory test. DOI: 10.1016/j.foodpol.2025.102850.
- **Paper abstract "roughly a third"** — stale; comes from old adjusted spec; unadjusted results are 19% (Panel A, total production) or 44% (Panel B, log yield per acre); paper body uses 19%
- **Pending blog suggestion (from earlier session):** Potassium bullet split into two, Joachim JD5 fix ("not pulled toward" rather than "pulled away from"), Ukraine/Hormuz sentence — these were suggested but not yet confirmed as wanted. Check with Bjorn.
- **No controls in any reported regression.** All tables use unadjusted ITT only.
- **No em dashes** anywhere in paper. Hard constraint.
- **No Co-Authored-By trailer** in any commit message.
- **Nested git structure.** Outer GitHub at `/workspace/S2P`; inner Overleaf at `paper/`. `paper/` is gitignored in outer repo. Pull GitHub before pushing from workspace. Pull Overleaf before pushing from `paper/`.

## Issues / Blockers
- Paper abstract still has stale yield figure ("roughly a third")
- Five §4-deleted PAP deviations have no in-paper home — open referee risk
- AEA registration post-dates rollout — not yet addressed
- Lee bounds for T2 retention differential not yet computed

## Key Files
- `paper/S2PIEpaper.tex` — main manuscript; Overleaf HEAD = `f79613e`
- `paper/s2p_bib.bib` — bibliography
- `blog/blog_S2P_final.docx` — **latest blog version**; all Ruth edits applied; GitHub = `62a2a4b`
- `replication_package/code/R/06_table2_fertilizer_use_audit.R` — unstaged changes
- `replication_package/code/R/08_table4_nutrient_use.R` — unstaged changes
- `replication_package/code/R/09_table5_maize_yield.R` — unstaged changes
- `wiki/articles/ict-agricultural-extension.md` — updated with external literature section (4 papers)
