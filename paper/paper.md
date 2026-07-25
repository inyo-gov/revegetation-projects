---
title: "Revegetation Monitoring in Owens Valley, California: A Living Open Repository for Type E Transfer and 1999 Plan Sites"
authors:
  - name: "Inyo County Water Department"
    affiliation: "Inyo County Water Department, Bishop, California, USA"
date: today
format:
  html:
    output-file: paper.html
keywords:
  - revegetation
  - Owens Valley
  - line-point intercept
  - Type E transfer
  - 1999 Revegetation Plan
  - open science
  - reproducible workflows
  - LADWP
  - Inyo County
abstract: |
  Revegetation monitoring under the Inyo–Los Angeles Long Term Water Agreement spans multiple plans, decades of field work, and a messy stack of workbooks, GIS layers, and partner transfers. We describe a public, version-controlled repository that consolidates two active programs—(1) Laws Type E transfer sites under the 2003 plan (abandoned irrigated agriculture managed toward precipitation-dependent native communities) and (2) 1991 EIR / 1999 Revegetation Plan mitigation sites resurveyed in 2025—into Quarto dashboards, downloadable analysis-ready tables, and documented pipelines. Cover and composition goals are assessed with plan-specific rules; a sustainability (recruitment) criterion in the 1999 Plan remains incompletely operationalized and is now being explored with remote-sensing methods proposed by LADWP. The repository is intentionally a living system: raw sources, known gaps (missing segment denominators, tallied vs distance-tagged intercepts, incomplete metadata), methods decisions, and rendered reports stay in one place so staff, Technical Group partners, students, and the public can follow—and challenge—the path from field hits to compliance statements. This paper is the citable narrative companion to that working site.
---

::: {.callout-important}
## Draft / sandbox — living manuscript

This paper is a **living draft** kept open for review. Figures and site pages may include prototype or sparsely labeled outputs. It is not a peer-reviewed final product and does not replace plan text or Technical Group decisions.
:::

## Introduction

### Why a living repository

Agency revegetation data often live as spreadsheets on shared drives, emailed PDFs, and GIS layers whose folder paths are institutional memory. That pattern is closed by default even when the work is publicly funded and informs public decisions [@Snow2026; @Nandi2025]. ICWD’s response is a **living repository**: one GitHub project that holds source extracts, processing code, known issues, interactive reports, and downstream products—updated as surveys arrive and methods sharpen—rather than a one-off static paper dump [@Baker2025].

This manuscript documents that repository for Owens Valley revegetation monitoring. The **canonical analytics** remain the Quarto site ([inyo-gov.github.io/revegetation-projects](https://inyo-gov.github.io/revegetation-projects/)); this paper is the peer-citable overview of purpose, data sources, methods, and open problems.

### Regulatory and ecological context

Owens Valley revegetation obligations arise from groundwater and land-use impacts addressed in the 1991 EIR and subsequent plans under the Inyo–Los Angeles Long Term Water Agreement [@InLA1991]. Two programs dominate the present repository:

1. **Laws Type E transfer (2003 plan).** Parcels LAW090, LAW094, LAW095, and LAW118/129 were removed from Type E irrigated agriculture and are managed toward a **precipitation-dependent natural community**. Dryland succession toward a stable late-successional (climax) state can take decades; annual goal snapshots are milestones, not proofs of endpoint. Companion **reference sites** inform ATTO/ERNA capping rules.

2. **1991 EIR / 1999 Revegetation Plan.** Mitigation sites (e.g., IND105, IND123, IND131N/S, BLK16E, TIN054, BGP160E/W, BIS097, LAW118) use permanent line-point layouts. Table 2 goals set cover and composition targets with 80% CI rules; the Plan also requires that **≥25% of vegetation cover** be natural recruits ≥3 years old [@LADWP1999]—a sustainability test historically under-measured in LPI alone.

LADWP and ICWD share field and remote-sensing responsibilities; recent Technical discussions (2026) proposed aerial segmentation/classification and interannual overlap rules to score recruitment against the 25% criterion, with **2022** high-resolution imagery + LiDAR as a baseline and forthcoming **2024 3DEP LiDAR** as a near-term comparison layer.

### Data management challenges

1. **Heterogeneous sources** — LADWP workbooks, ICWD resurvey sheets, AGOL/GIS posts and polygons, historical master workbooks, reference-parcel CSVs; formats and column semantics differ by year and crew.
2. **Denominator ambiguity** — 1999-plan cover needs `n_possible_hits`; field sheets often omit total points. GIS post-to-post length / 0.5 m is the best available denominator when tape totals are missing.
3. **Protocol drift** — Intended first-hit every 0.5 m vs **segment tallies** without distance tags (noted for some 2017/2025 work) blocks individual tracking along transects.
4. **Geometry incompleteness** — Missing posts, incomplete segment lengths (e.g., TIN054), layout revisions LADWP may propose.
5. **Split criteria** — Cover/composition can be “met” while sustainability is unassessed or fails (IND105 aerial pilot).
6. **Cross-repo GIS** — Parcel inventory polygons also live in `mitigation-spatial`; this repo owns hits, denominators, and reports.

### Objectives

1. Document both monitoring programs and their goal logic in one place.  
2. Catalog primary inputs, processed products, and known gaps.  
3. Describe the reproducible Quarto/R(/Python) pipeline and live dashboards.  
4. Record open method questions (sustainability RS, field metadata checklist, layout sync).  
5. Position the repository as a template for transparent revegetation analytics under multi-party agreements.

## Data Description

### Type E transfer sites (2003)

| Item | Detail |
|------|--------|
| Sites | LAW090, LAW094, LAW095, LAW118/129 |
| Design | Fixed **200** intercepts per transect (hits/200 × 100) |
| Goals | Cover, richness, species ≥3 hits, per-transect floor, grass presence (see live report) |
| Reference | Separate reference-parcel surveys for ATTO/ERNA thresholds |
| Live report | [Type E transfer revegetation](https://inyo-gov.github.io/revegetation-projects/) |

### 1999 Plan sites (2025 resurvey)

| Item | Detail |
|------|--------|
| Sites | IND105, IND123, IND131N/S, BLK16E, TIN054, BGP160E/W, BIS097, LAW118 |
| Design | Variable segment lengths; cover = hits / (`length_m` / 0.5) × 100 |
| Goals | Upper 80% CI of segment cover ≥ 90% of Table 2 cover; richness ≥ 75% of Table 2 species; sustainability (≥25% recruit cover) **not** scored in the 2025 LPI page |
| Primary workbook | `Re_Veg Parcels 2025 IND_BLK_TIN_BGP_BIS_LAW.xlsx` (species hits only; staff %COV/%COMP ignored) |
| Denominators | `data/transect_segment_denominators.csv` |
| Live report | [1999 Plan monitoring (2025)](https://inyo-gov.github.io/revegetation-projects/reveg1999plan.html) |

### Spatial layers

- Parcel boundaries: `mitigation_sites.shp`, `mitigation_view_polygons.geojson` (LAW118), `TIN054.json`
- Transect posts: `Revegetation_Transects91.geojson`, parcel-specific GeoJSON/CSV; segment **lines** built post-to-post for maps
- Related inventory: [`mitigation-spatial`](https://github.com/inyo-gov/mitigation-spatial)

### Processed products (examples)

- `data/processed/reveg1999plan_hits_long.csv` — long hits + cover  
- `data/processed/reveg1999plan_parcel_metadata.csv`  
- `output/` / `docs/` downloads — Laws compliance tables, GeoJSON, clean zip bundles  

Exact file lists evolve with each survey; the repository README and report download sections are authoritative.

## Methods

### Type E transfer pipeline

Ingest multi-year LADWP/ICWD transect CSVs and workbooks; filter to allowable perennial species; compute parcel- and transect-level metrics against 2003 goals; render interactive maps and tables (`index.qmd`, `reference.qmd`).

### 1999 Plan pipeline

1. Parse hit counts from the 2025 workbook (`code/parse_reveg1999plan_workbook.R`).  
2. Join GIS denominators (`code/build_transect_segment_denominators.py`).  
3. Apply perennial filters consistent with the LADWP master workbook.  
4. Assess cover (upper 80% CI vs 90% of Table 2) and composition (75% of species goal).  
5. Map boundaries, segment lines, and posts; historical cover from `LADWP 1991 EIR REVEGETATION DATA-MASTER.xlsm` (`reveg1999plan.qmd`).

### Sustainability (recruitment) — proposed aerial workflow

LADWP (LA watershed resources staff, 2026) demonstrated on IND105:

1. Segment/classify high-res 4-band + LiDAR → bare / live / dead.  
2. Interannual overlap: no (or &lt;10%) overlap with prior live cover → recruit area; age inferred from imagery gap.  
3. Metric: recruit live area / **total live cover** vs ≥25%.  
4. **Caveats (LA watershed resources staff):** annuals inflate recruitment; areas tagged as recruits in 2017 but empty in 2022 should be dropped from the 25% share (annual signal assumption; perennial mortality is an alternative). Lateral growth ≠ recruitment on LPI alone.

**Site priority (s-ri):** IND105 chosen as low-hanging fruit (cover/comp already met); IND123 of interest to push to full complete with sustainability demonstrated.

### Software

R (tidyverse, sf, Quarto) with Python for denominator builds; site theme PeerJ-inspired; HTML primary, DOCX/PDF export available for reports [@Allaire2024; @Wickham2019].

## Results (repository as product)

The primary result is an **operational open analytics stack**, not a single year’s table:

- Live dashboards for Type E transfer and 1999 Plan sites (goal tables, maps, species/segment summaries).  
- Explicit incomplete status where denominators or assessments fail.  
- TOC cues (⚠️) for sites meeting cover/composition but not yet demonstrating sustainability.  
- Version history of method changes (e.g., GIS denominators, transect lines on maps, Type E transfer framing).

Quantitative compliance numbers for the current survey year are published on the site and should be cited from the rendered report or release tag, not frozen only in this manuscript.

## Discussion

### Living documentation vs closed files

Keeping sources, issues, and dashboards together reduces the “who has the real sheet?” problem and supports Technical Group review, public records responses, and student reuse [@Snow2026]. Gaps (TIN054 lengths, tally vs LPI, Meredith 2023 plot protocol not yet located) stay visible as tracked work, not silent footnotes.

### Succession and forecasting

Type E transfer sites are multi-decadal ecological projects. Annual goal checks should sit beside succession thinking and, eventually, forecasting against baselines (2022 imagery/LiDAR; 2024 3DEP; concurrent field).

### Broader applicability

Agencies with multi-plan revegetation or restoration monitoring can reuse: (1) Quarto report-as-dashboard pattern; (2) explicit denominator and perennial filters; (3) separating cover/composition from sustainability; (4) linking field LPI to remote-sensing recruit tests when LPI cannot age plants.

### Limitations

Not all raw workbooks are (or should be) public on day one; some geometry remains incomplete; sustainability RS methods are pilot-stage; field metadata checklists are still needed for each transect.

## Data and code availability

- Repository: [github.com/inyo-gov/revegetation-projects](https://github.com/inyo-gov/revegetation-projects)  
- Site: [inyo-gov.github.io/revegetation-projects](https://inyo-gov.github.io/revegetation-projects/)  
- About / open science: [about page](https://inyo-gov.github.io/revegetation-projects/about.html)  
- Related GIS inventory: [mitigation-spatial](https://github.com/inyo-gov/mitigation-spatial)  

Archival DOI releases (e.g., Zenodo) can be minted from tagged GitHub versions as datasets stabilize. **Preferred workflow:** enable Zenodo’s GitHub integration once, then publish an annual GitHub Release (`vYYYY.1.0`); Zenodo creates a **version DOI** under a stable **concept DOI** with no manual file upload. See [`docs/ZENODO.md`](../docs/ZENODO.md), `.zenodo.json`, and `CITATION.cff`.

## Acknowledgments

Field crews, LADWP monitoring and remote-sensing staff (including LA watershed resources staff and partners in 2026 sustainability discussions), and ICWD colleagues who maintain the monitoring network.

## References
