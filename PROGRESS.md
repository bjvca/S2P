# Project: S2P paper (soil tests and vouchers, fertilizer subsidy reform in Malawi)

## Objective
Publish the S2P paper: a cluster-RCT of soil-test recommendations (T1) and recommendation plus subsidy-equivalent voucher (T2) against a control group in central Malawi. Two tracks run in parallel, a journal submission and the IFPRI Discussion Paper series.

## Current Status
All three versions of the paper now hold identical content. The DP reviewer revisions were merged into the main source on 18 Nov 2026, the anonymized version regenerated from it, both PDFs rebuilt clean, and both repos pushed (Overleaf `470fd46`, GitHub `96eda02`). The DP response and the clean manuscript are compiled and ready to send to the IFPRI series editor.

## Completed Tasks
- [x] Reference audit of all 56 entries: 3 hallucinated entries fixed (holden2019input, mather2018input, burke2017agricultural), 5 wrong titles or venues, Kilic et al. updated to published JAE 2015, text correction "Central Malawi" to "Zambia"
- [x] MHT appendix with sharpened FDR q-values (Table A.7), AIP substitution moved to Appendix 3, conclusion restructured, conceptual framework made theory-first, arm vocabulary unified on T1/T2
- [x] All tables restyled (double rules, centered Panel headings, stars); figures 3 and 4 matched on a shared ggplot theme_classic; retention figure regenerated from current data
- [x] Anonymized version created: authors, GitHub link and funding acknowledgement removed, IFPRI IRB number withheld
- [x] AI-use declaration added to the Note section of both versions
- [x] Journal submission confirmed as the anonymized, post-audit version
- [x] DP review completed, publication recommended, all 25 reviewer points addressed
- [x] DP revisions merged from `DP_WorkingVersion.tex` into `S2PIEpaper.tex`, anonymized version regenerated, both PDFs rebuilt with no errors and no undefined references
- [x] Root `S2PIEpaper.pdf` refreshed so the paper's "Latest version" GitHub link serves current content
- [x] Both repos pushed

## In Progress
- [ ] Nothing. Work is at a clean stopping point.

## Next Steps
- [ ] Verify `https://www.socialscienceregistry.org/trials/14293` resolves to AEARCTR-0014293. Thomas could not reach the registry from his machine and flagged the URL at `paper/S2PIEpaper.tex:357`. Section 3 now asserts it, so it needs checking before the DP is published.
- [ ] Ask Edwin to check the Chichewa figure caption flagged at `paper/S2PIEpaper.tex:389`.
- [ ] Send to the IFPRI series editor: `paper/DP_Response.pdf` and `paper/S2PIEpaper.pdf` (identified version, not the anonymized one).
- [ ] Decide what to do with `paper/DP_WorkingVersion.tex`, now redundant with the main source. Every future edit otherwise has to be made twice.
- [ ] Decide whether to push back on the trimmed AI declaration. A co-author removed "statistical programming" on 26 Oct (Overleaf `2009670`), leaving "drafting, editing, and reference management", which understates the R work.
- [ ] Decide whether the anonymized version should also withhold the Malawi NCRSH number (P.08/24/917). It currently carries it while withholding the IFPRI IRB number.
- [ ] Parked: heterogeneity idea, baseline-AIP by treatment interactions as a mechanism test for the fungible-voucher framing.

## Decisions / Notes
- The merge direction was DP_WorkingVersion into the main source, confirmed safe: identical preamble, title block, bibliography, AI declaration and FISP naming, so it was the current paper plus revisions rather than an older fork.
- Regenerating the anonymized version mechanically would have leaked the AEA registry number, which Thomas's new Section 3 text states in the body. It is withheld there too.
- Substantive DP revisions: lime reclassified as a soil conditioner (illustrative list now uses CAN), the fertilizer budget m stated explicitly as predetermined at the point of bundle choice with the advice-arm evidence reported, H2 reworded from credit to liquidity constraints, additionality turned into a formal hypothesis, Table 6 moved to the appendix, per-acre effects reported, soil-test cost excluded from the cost-effectiveness arithmetic.
- Em dashes: do not strip the co-authors' existing ones, do not write new ones.
- Reference audits run automatically in any final pass.
- Oxford spelling: -ize endings with programme, behaviour, labour.
- Per-acre caveat framing: plot area is a farmer-reported endline variable, so a treatment effect on reporting is possible. This is measurement, not randomization imbalance, and levels estimates stay primary.
- No controls in any reported table; unadjusted ITT throughout.
- No Co-Authored-By trailer in commit messages.
- Nested git structure: outer GitHub at `/workspace/S2P`, inner Overleaf at `paper/`. Pull --rebase both before pushing, stashing build artifacts first.
- amsmath is not loaded; the paper loads amssymb only.

## Related Work
- **Fertilizer window proposal** (Malawi, Kenya, Rwanda; partial funding expected). Malawi stream: Ministry advisory on the World Bank Soil Health Pilot, a constant-spending simulation of the compound/urea split with a district-level targeting rule, and AI-generated voice-call recommendations in Chichewa during 2027/28. Staff: Duchoslav, De Weerdt, Van Campenhout, Assefa.
- **2027 PORB** (Policy AoW2, Agricultural Policy Repurposing, Malawi, $54,000, 1 knowledge product). KPI drafted 11 Nov 2026 for Ruth Hill: a costed assessment of expanding the FISP package beyond its two current fertilizer products, valuing yield and revenue gains from adding soil amendments net of procurement and distribution costs. Framed to sit beside, not duplicate, the fertilizer-window compound/urea work.

## Issues / Blockers
- None blocking. The three-way version fork between journal, DP and public GitHub copies is resolved.

## Key Files
- `paper/S2PIEpaper.tex` - main source, authoritative, now carries the DP revisions
- `paper/S2PIEpaper.pdf` - compiled identified version, send this to IFPRI
- `paper/S2PIEpaper_anon.tex` / `.pdf` - blinded version for the journal; regenerated by reapplying six anonymizing edits to the main source
- `paper/DP_Response.tex` / `.pdf` - 25-point response to the DP reviewer, all points marked done
- `paper/DP_WorkingVersion.tex` - knitr output where Thomas made the revisions; now redundant
- `paper/s2p_bib.bib` - bibliography, post-audit
- `S2PIEpaper.pdf` (repo root) - what the paper's "Latest version" link serves on GitHub
- `replication_package/code/R/` - table and figure generators (03 sample flow, 04 attrition diagnostics, figure_voucher_breakeven)
- `policy_brief/` - two-page brief from July 2026, already committed
