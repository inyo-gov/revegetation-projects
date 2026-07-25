# Revegetation Projects Monitoring

**Inyo County Water Department**

Monitoring data analysis and reporting for revegetation parcels in Inyo County, California:

- **Laws parcels (2003 plan):** LAW090, LAW094, LAW095, LAW118, LAW129 — lands removed from irrigation
- **1991 EIR / 1999 plan parcels (2025 resurvey):** IND105, IND123, IND131N/S, BLK16E, TIN054, BGP160E/W, BIS097, LAW118

## Overview

This repository contains monitoring data analysis for revegetation projects in the Laws area and for 1991 EIR mitigation parcels under the 1999 Revegetation Plan. The Laws analysis tracks progress toward revegetation goals established in the 2003 plan. The 1999-plan page reports August 2025 point-intercept resurveys on historical transect layouts. Both use interactive Quarto reports with maps and downloadable datasets.

**Cross-repo note:** Mitigation site inventory and polygon boundaries live in `[mitigation-spatial](https://github.com/inyo-gov/mitigation-spatial)`. Transect hits, segment denominators, and reveg monitoring reports live here. Day-to-day status stays in a local `WORKLOG.md` (gitignored; backed up elsewhere). AGOL / start-point geometry tasks: `[mitigation-spatial/WORKLOG.md](https://github.com/inyo-gov/mitigation-spatial/blob/main/WORKLOG.md)`.

### Draft / sandbox pages

STM, NMDS, and papers are labeled **(draft)** in the nav. They stay public on purpose (open science) but are a **sandbox**—not adopted policy or certified compliance. Full **2003-plan amendment draft language** is gitignored (`amendment/`); the public site has only [goals & caps summary](https://inyo-gov.github.io/revegetation-projects/amendment_summary.html). Shared disclaimer: `_includes/sandbox_disclaimer.qmd`.

`.gitignore` keeps a **site + reproducibility** set (Quarto sources, parsers, processed tables, GIS needed to re-render). Manifest: `[data/SITE_INPUTS.md](data/SITE_INPUTS.md)`. Meeting notes, correspondence, full amendment drafts, LADWP deck images, `laws2026` drops, and large raw zips stay local.

## Interactive Data Portal

**🌐 [View Live Analysis & Interactive Maps](https://inyo-gov.github.io/revegetation-projects/)**


| Section                | Page                                                                                                  | Content                                         |
| ---------------------- | ----------------------------------------------------------------------------------------------------- | ----------------------------------------------- |
| Type E transfer (2003) | [Revegetation sites](https://inyo-gov.github.io/revegetation-projects/)                               | Goal attainment, compliance tables              |
| Type E transfer (2003) | [Reference sites](https://inyo-gov.github.io/revegetation-projects/reference.html)                    | ATTO/ERNA thresholds                            |
| Type E transfer (2003) | [STM (draft)](https://inyo-gov.github.io/revegetation-projects/stm.html)                              | Gravelly loam ESD + ABAG abandoned-ag state     |
| Type E transfer (2003) | [NMDS (draft)](https://inyo-gov.github.io/revegetation-projects/nmds.html)                            | Reference vs reveg composition ordination       |
| Type E transfer (2003) | [Goals & caps (summary)](https://inyo-gov.github.io/revegetation-projects/amendment_summary.html)     | Shared direction + ATTO/ERNA caps open question |
| 1999 Plan              | [1999 Plan monitoring](https://inyo-gov.github.io/revegetation-projects/reveg1999plan.html)           | 1991 EIR parcel resurvey                        |
| Papers                 | [Data paper (draft)](https://inyo-gov.github.io/revegetation-projects/paper.html)                     | PeerJ-style living-repository narrative         |
| Papers                 | [STM / NMDS for Type E (draft)](https://inyo-gov.github.io/revegetation-projects/paper_stm_nmds.html) | ABAG state + Bray–Curtis ordination framing     |


**Manuscripts:** `paper/paper.md` (living repository) and `paper/stm_nmds_type_e.md` (STM/NMDS). Keep the Quarto site as the source of truth for current numbers; update papers when methods or survey years change.

**Zenodo (annual DOI versions):** Enable GitHub↔Zenodo once, then each `gh release` / tagged release auto-mints a version DOI. Metadata: `.zenodo.json`, `CITATION.cff`. Steps: `[docs/ZENODO.md](docs/ZENODO.md)`.

```bash
cd paper && quarto render                 # both → docs/paper.html + docs/paper_stm_nmds.html
cd paper && quarto render paper.md --to docx
```

Navbar: **Type E transfer** dropdown (sites, reference, goals/caps summary, STM, NMDS) + **1999 Plan monitoring** + **About** + **Papers**. Site theme is PeerJ-inspired (charcoal, compact nav, numbered figures/tables).

**Report exports (Word / PDF):**

```bash
quarto render reveg1999plan.qmd --to docx
quarto render reveg1999plan.qmd --to pdf
quarto render index.qmd --to docx
quarto render reference.qmd --to docx
```

Cross-references use Quarto `@fig-…` / `@tbl-…` labels (Figure / Table prefixes).

## 1999 Plan Revegetation Monitoring (2025)

August 2025 field data for **10 parcels** from the resurvey workbook (LAW118 also appears in the Laws report). Source workbook: `Re_Veg Parcels 2025 IND_BLK_TIN_BGP_BIS_LAW.xlsx` (local copy; not yet committed to git).

**Analysis approach:**

- Parse **species hit counts only** — staff `%COV`, `%COMP`, accuracy, and other calculated workbook columns are ignored.
- Join segment denominators from `data/transect_segment_denominators.csv` (built from 1991 transect datasheets and `blk16e_transects.geojson`).
- Compute cover in the pipeline: `percent_cover = hits / n_possible_hits × 100`, where `n_possible_hits = segment length (m) / 0.5 m` from GIS post-to-post distances (not 15 m sub-segments from the 1991 datasheets).

**Key files:**


| File                                               | Purpose                                               |
| -------------------------------------------------- | ----------------------------------------------------- |
| `reveg1999plan.qmd`                                | Quarto report (metadata, map, segment/species tables) |
| `code/parse_reveg1999plan_workbook.R`              | Parse workbook, join denominators, write CSVs         |
| `code/build_transect_segment_denominators.py`      | Rebuild segment length / intercept lookup             |
| `data/transect_segment_denominators.csv`           | Per-segment `length_m` and `n_possible_hits`          |
| `data/processed/reveg1999plan_hits_long.csv`       | Long-format hits + computed cover                     |
| `data/processed/reveg1999plan_parcel_metadata.csv` | Survey date, quad, parcel code, acres                 |


**Known gaps:**

- **TIN054:** all 16 segments lack denominators in the lookup (`NEEDS_ACQUISITION`); hits are parsed but cover is NA until segment lengths are acquired.
- **IND105 / IND131:** possible conflict between 1991 datasheet layout (15 m segments) and 1999 PDF layout (~100 m) — flagged in denominator notes; confirm against 2025 field layout.

**Render the 1999-plan page:**

```bash
quarto render reveg1999plan.qmd
```



## Laws Parcels — 2025 Goal Attainment Status



### Revegetation Goals

1. **Perennial Cover ≥ 10%**: Parcel-average native perennial cover must be 10% or greater
2. **Species with ≥3 Hits ≥ 6**: At least six perennial species must have at least 3 hits within each parcel
3. **Species Richness ≥ 10**: Each parcel must have at least 10 distinct perennial species
4. **Transect Cover ≥ 2%**: Each individual transect must have at least 2% perennial cover
5. **Grass Species Present**: At least one grass species must be present on each parcel



### 2025 Compliance Summary


| Parcel         | Overall Compliance | Cover (≥10%) | Species ≥3 Hits (≥6) | Richness (≥10) | Transect Coverage | Grass Present |
| -------------- | ------------------ | ------------ | -------------------- | -------------- | ----------------- | ------------- |
| **LAW090**     | ✅ **Yes**          | ✅ 10.3%      | ✅ 6 species          | ✅ 13 species   | ✅ 31/31           | ✅ Present     |
| **LAW094**     | ❌ No               | ❌ 8.0%       | ✅ 7 species          | ✅ 13 species   | ✅ 21/21           | ✅ Present     |
| **LAW095**     | ❌ No               | ❌ 5.8%       | ❌ 4 species          | ✅ 13 species   | ✅ 21/21           | ✅ Present     |
| **LAW129_118** | ❌ No               | ❌ 9.4%       | ❌ 5 species          | ❌ 6 species    | ❌ 19/20           | ❌ None        |


**Legend**: ✅ Goal attained | ❌ Goal not attained

### Key Findings

- **LAW090** is the only parcel meeting all revegetation goals in 2025
- **LAW129_118** shows the most challenges, failing 4 out of 5 goals
- **LAW094** and **LAW095** are close to compliance, primarily failing the 10% cover requirement



### ATTO/ERNA Policy Caps

The analysis includes policy-based capping of ATTO and ERNA species based on reference parcel averages:

- **LAW90/94/95 Group**: ATTO 0.19%, ERNA10 0.28% (combined 0.47%)
- **LAW118/129 Group**: ATTO 2.78%, ERNA10 3.24% (combined 6.02%)

These caps ensure revegetation projects don't exceed reference parcel conditions for these species.

## Repository Structure

```
├── _targets.R              # Reproducible data processing pipeline (Laws analysis)
├── _quarto.yml             # Site config — Laws, 1999-plan, Reference pages
├── index.qmd               # Laws 90/94/95/118/129 report
├── reveg1999plan.qmd       # 1991 EIR / 1999-plan 2025 resurvey report
├── reference.qmd           # Reference parcel analysis
├── code/                   # R/Python scripts and data loaders
│   ├── parse_reveg1999plan_workbook.R
│   ├── build_transect_segment_denominators.py
│   └── transect_loader.R
├── data/
│   ├── raw/               # Original data files (Excel, CSV)
│   ├── processed/         # Cleaned and analyzed datasets
│   ├── gis/               # Spatial data (shapefiles, GeoJSON)
│   └── transect_segment_denominators.csv
├── docs/                  # Rendered website files
├── output/                # Analysis outputs and summaries
├── www/                   # Website assets (logos, images)
└── README.md              # This file
```



## Data Downloads



### Laws monitoring (2003 plan)

- **[Compliance Summary](https://github.com/inyo-gov/revegetation-projects/blob/main/data/processed/full_summary.csv)** - Complete goal attainment analysis
- **[Reference Parcel Data](https://github.com/inyo-gov/revegetation-projects/blob/main/data/processed/reference_parcel_summary.csv)** - ATTO/ERNA threshold analysis
- **[Transect-Species Data](https://github.com/inyo-gov/revegetation-projects/blob/main/data/processed/transect_species_data.csv)** - Detailed species cover by transect
- **[Spatial Data](https://github.com/inyo-gov/revegetation-projects/tree/main/data/gis)** - Parcel boundaries and transect locations



### 1999-plan monitoring (2025 resurvey)

- **[Hits long format](https://github.com/inyo-gov/revegetation-projects/blob/main/data/processed/reveg1999plan_hits_long.csv)** - Species hits by parcel/segment with computed cover
- **[Parcel metadata](https://github.com/inyo-gov/revegetation-projects/blob/main/data/processed/reveg1999plan_parcel_metadata.csv)** - Survey dates and workbook metadata
- **[Segment denominators](https://github.com/inyo-gov/revegetation-projects/blob/main/data/transect_segment_denominators.csv)** - Segment lengths and intercept counts for cover calculation



### Additional Resources

- **[Data Processing Pipeline](https://inyo-gov.github.io/revegetation-projects/data_process.html)** - Technical documentation
- **[Reference Analysis](https://inyo-gov.github.io/revegetation-projects/reference.html)** - Species threshold methodology



## Reproducing the Analysis



### Prerequisites

- R (≥ 4.0)
- RStudio (recommended)
- Git



### Setup Instructions

1. **Clone the Repository**:
  ```bash
   git clone https://github.com/inyo-gov/revegetation-projects.git
   cd revegetation-projects
  ```
2. **Install Required R Packages**:
  ```r
   install.packages(c("targets", "tidyverse", "sf", "DT", "leaflet", "quarto"))
  ```
3. **Run the Analysis Pipeline**:
  ```r
   library(targets)
   tar_make()  # Builds all analysis targets
  ```
4. **Render the Website**:
  ```bash
   quarto render                    # full site
   quarto render reveg1999plan.qmd # 1999-plan page only
  ```



### Key R Packages

- `targets` - Reproducible data pipeline
- `tidyverse` - Data manipulation and visualization  
- `sf` - Spatial data processing
- `DT` - Interactive tables
- `leaflet` - Interactive maps
- `quarto` - Dynamic document generation



## Contributing

1. **Fork the Repository** on GitHub
2. **Create a Feature Branch**: `git checkout -b feature-name`
3. **Make Changes** and test locally
4. **Commit Changes**: `git commit -m "Descriptive message"`
5. **Push to Fork**: `git push origin feature-name`
6. **Open a Pull Request** with detailed description



## Contact & Support

- **Repository**: [github.com/inyo-gov/revegetation-projects](https://github.com/inyo-gov/revegetation-projects)
- **Live Website**: [inyo-gov.github.io/revegetation-projects](https://inyo-gov.github.io/revegetation-projects)
- **Issues**: [Report bugs or request features](https://github.com/inyo-gov/revegetation-projects/issues)



## License

This project is developed by the Inyo County Water Department for public land management and environmental monitoring purposes.

---

*Last updated: June 2026*