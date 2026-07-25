# Site + reproducibility inputs

Files needed to **serve GitHub Pages** (`docs/`) and to **re-render** the Quarto site from source. Everything else should stay local (see root `.gitignore`).

## Quarto sources (root)

| File | Page |
|------|------|
| `index.qmd` | Type E revegetation |
| `reference.qmd` | Reference / ATTO–ERNA |
| `amendment_summary.qmd` | Goals & caps summary |
| `stm.qmd` / `nmds.qmd` | STM / NMDS drafts |
| `reveg1999plan.qmd` | 1999 Plan 2025 |
| `about.qmd` | About |
| `_quarto.yml`, `styles.css`, `inyo_logo.png` | Site chrome |
| `_includes/sandbox_disclaimer.qmd` | Shared draft banner |
| `paper/` | Living papers → `docs/paper*.html` |

## Code (tracked)

| Script | Role |
|--------|------|
| `code/nmds_laws.R` | NMDS page |
| `code/parse_reveg1999plan_workbook.R` | 1999 hits (or load processed) |
| `code/parse_reveg1999_master_workbook.R` | Master trends (or load processed) |
| `code/reveg1999plan_maps.R` | 1999 maps |
| `code/read_reference_parcels_excel.R` | Optional reference Excel path |
| `code/build_transect_segment_denominators.py` | Regenerate denominators |

## Data required to re-render

### Always

- `data/species.csv`
- `data/cap_scenarios.csv`
- `data/allowable_species_by_group.csv` (if used)
- `data/TypeE_Transfer_SppList.xlsx`
- `data/LawsRevegetationData_SummaryTable_2025_ForICWD092525.xlsx`
- `data/transect_segment_denominators.csv`
- `data/processed/*` site tables (reference summaries, 1999 hits/metadata, NMDS scores, ESD attrs)
- `data/raw/reveg/LAW118_129_reveg2025_e.csv`
- `data/raw/reference/law090_094_095_reference_parcel_long_format.csv`
- `data/raw/reference/law118_129_reference_parcel_long_format.csv`
- `data/raw/reveg/LADWP_ReferenceParcel_LAW090_094_095_2025_Data_2025.xlsx` (reference 2025 block)
- `data/processed/reveg1999_master_*.csv` and `reveg1999plan_*.csv` (loaders prefer these)
- Optional source workbooks (also trackable for full reparse): `data/raw/LADWP 1991 EIR REVEGETATION DATA-MASTER.xlsm`, `data/raw/reveg/Re_Veg Parcels 2025 IND_BLK_TIN_BGP_BIS_LAW.xlsx`

### GIS

- `data/gis/LA_parcels_rasterizedd.*` (shapefile set)
- `data/gis/laws_reveg_transects.csv`
- `data/gis/mitigation_view_polygons.geojson`
- `data/gis/mit_points/mitigation_sites.*`
- `data/gis/Revegetation_Transects91.geojson`
- `data/gis/TIN054.json`, `data/gis/TIN054_transect_posts.csv`
- `data/gis/IND105.*` (optional if posts already in denom CSV)
- `data/gis/blk16e_transects.geojson` (if used)
- `data/gis/LAW118_129_startpoints_2025_for_ladwp.gpkg` (optional)

### Docs artifacts for Pages

- Rendered `docs/*.html`, `docs/site_libs/`, download CSVs/geojsons written by Quarto
- `docs/reference_parcels.geojson`
- Quarto figure dirs under `docs/*_files/` (kept; root `*_files/` ignored)

## Explicitly local (ignored)

Amendment draft language (`amendment/`), meeting notes, correspondence, LADWP deck images, `laws2026` drops, large raw zips, one-off export/QA scripts, `output/`, and `WORKLOG.md` (backed up elsewhere).

LADWP sustainability deck figures are **optional** at render time (skipped if missing); previously rendered HTML may still show them from `docs/` assets if present locally.
