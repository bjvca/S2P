# Project: S2P paper (soil tests and vouchers, fertilizer subsidy reform in Malawi)

## Objective
Finalise and publish the S2P paper: a cluster-RCT of soil-test recommendations (T1) and recommendation + subsidy-equivalent voucher (T2) against a control group in central Malawi. Two tracks run in parallel: journal submission, and the IFPRI Discussion Paper series.

## Current Status
Journal: submitted. Bjorn confirms the anonymized version was sent. The identified PDF committed as "uploaded new version that is sent to journal" (outer repo `dd3ef5c`) carries the corrected Holden 2019 entry, so the post-reference-audit snapshot is what went in. The file-replacement email drafted earlier is no longer needed.

IFPRI DP: reviewed, publication recommended. A 25-point response (`paper/DP_Response.tex`, Overleaf `1327f67`, 17 Nov 2026) is complete, every point marked done and applied to the manuscript. `paper/DP_WorkingVersion.tex` holds the knitr-generated working version.

Versions reconciled 18 Nov 2026: the DP revisions were adopted into `paper/S2PIEpaper.tex`, the anonymized version regenerated from it, both PDFs rebuilt clean (no errors, no undefined references), and the root `S2PIEpaper.pdf` that the paper's "Latest version" link points to refreshed. Journal, DP, and public versions now hold the same content.

## Completed Tasks
- [x] Full reference audit (56 entries): 3 hallucinated entries fixed (holden2019input, mather2018input, burke2017agricultural), 5 wrong titles/venues, Kilic et al. updated to published JAE 2015, text correction "Central Malawi" to "Zambia"
- [x] MHT appendix (sharpened FDR q-values, Table A.7), AIP substitution moved to Appendix 3, conclusion restructured, conceptual framework theory-first, arm vocabulary unified (T1/T2)
- [x] All tables restyled (double rules, centered Panel headings, stars); figures 3/4 matched on shared ggplot theme_classic; retention figure regenerated from current data
- [x] Anonymized version `paper/S2PIEpaper_anon.tex/pdf` created, authors and GitHub link removed, IFPRI IRB number withheld
- [x] AI-use declaration added to the Note section of both versions
- [x] DP review: all 25 reviewer points addressed. Substantive ones: lime reclassified as soil conditioner (illustrative list now uses CAN), fertilizer budget m stated explicitly as predetermined at the point of bundle choice with the advice-arm evidence reported, H2 reworded from credit to liquidity constraints, additionality turned into a formal hypothesis, Table 6 moved to the appendix, per-acre effects reported, soil-test cost excluded from the cost-effectiveness arithmetic

## In Progress
- [ ] DP response complete and ready for the series editor, if it has not gone already. The revisions it describes are in `DP_WorkingVersion.tex` only.

## Next Steps
- [ ] Push the reconciled version: `paper/` to Overleaf and the root PDF to GitHub. Committed locally, not yet pushed.
- [ ] Two flags Thomas left in the source: the AEA registry URL at line 357 needs confirming (socialsciencregistry.org was blocked from the machine he drafted on), and the Chichewa figure caption at line 389 needs Edwin's check before release.
- [ ] `paper/DP_WorkingVersion.tex` is now redundant with `S2PIEpaper.tex`. Delete it, or keep it as the DP-series copy and accept that edits must be made twice.
- [ ] Decide whether to push back on the trimmed AI declaration. A co-author removed "statistical programming" from the list on 26 Oct (Overleaf `2009670`); the current text reads "drafting, editing, and reference management", which understates the R work.
- [ ] Anonymized version still carries the Malawi NCRSH protocol number (P.08/24/917) while withholding the IFPRI IRB number. Traceable to the study if anonymity matters.
- [ ] Parked: heterogeneity idea (baseline-AIP x treatment interactions) as a mechanism test for the fungible-voucher framing

## Related Work
- **Fertilizer window proposal** (Malawi, Kenya, Rwanda; partial funding expected). Malawi stream: Ministry advisory on the World Bank Soil Health Pilot, a constant-spending simulation of the compound/urea split with a district-level targeting rule, and AI-generated voice-call recommendations in Chichewa during 2027/28. Staff: Duchoslav, De Weerdt, Van Campenhout, Assefa.
- **2027 PORB (Policy AoW2, Agricultural Policy Repurposing, Malawi, $54,000, 1 knowledge product).** KPI drafted 11 Nov 2026 for Ruth Hill: a costed assessment of expanding the FISP package beyond its two current fertilizer products, valuing the yield and revenue gains from adding soil amendments net of procurement and distribution costs. Framed to sit beside, not duplicate, the fertilizer-window compound/urea work.

## Decisions / Notes
- Programme naming: the paper uses both AIP (79 occurrences) and FISP (8), each spelled out on first use
- Em dashes: do not strip the co-authors' existing ones, do not write new ones
- Reference audits run automatically in any final pass
- Oxford spelling: -ize endings with programme/behaviour/labour
- Per-acre caveat framing: plot area is a farmer-reported endline variable, so a treatment effect on reporting is possible; this is measurement, not randomization imbalance. Levels estimates stay primary
- No controls in any reported table; unadjusted ITT throughout
- No Co-Authored-By trailer in commit messages
- Nested git structure: outer GitHub at `/workspace/S2P`, inner Overleaf at `paper/`. Pull --rebase both before pushing; stash build artifacts first
- amsmath is not loaded; the paper loads amssymb only

## Issues / Blockers
- None. The three-way version fork is resolved.
