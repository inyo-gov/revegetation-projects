# Laws Revegetation Projects Monitoring

**Inyo County Water Department**  
*Land Removed from Irrigation: Laws Parcels 090, 094, 095, 118, and 129*

## Overview

This repository contains comprehensive monitoring data analysis for revegetation projects on lands removed from irrigation in the Laws area of Inyo County, California. The analysis tracks progress toward revegetation goals established in the 2003 plan and provides interactive data visualization and reporting tools.

## 2025 Goal Attainment Status

### Revegetation Goals
1. **Perennial Cover ≥ 10%**: Parcel-average native perennial cover must be 10% or greater
2. **Species with ≥3 Hits ≥ 6**: At least six perennial species must have at least 3 hits within each parcel  
3. **Species Richness ≥ 10**: Each parcel must have at least 10 distinct perennial species
4. **Transect Cover ≥ 2%**: Each individual transect must have at least 2% perennial cover
5. **Grass Species Present**: At least one grass species must be present on each parcel

### 2025 Compliance Summary

| Parcel | Overall Compliance | Cover (≥10%) | Species ≥3 Hits (≥6) | Richness (≥10) | Transect Coverage | Grass Present |
|--------|-------------------|--------------|---------------------|----------------|------------------|---------------|
| **LAW090** | ✅ **Yes** | ✅ 10.1% | ✅ 6 species | ✅ 13 species | ✅ 31/31 | ✅ Present |
| **LAW094** | ❌ No | ❌ 7.8% | ✅ 7 species | ✅ 13 species | ✅ 21/21 | ✅ Present |
| **LAW095** | ❌ No | ❌ 5.7% | ❌ 4 species | ✅ 13 species | ✅ 21/21 | ✅ Present |
| **LAW129_118** | ❌ No | ❌ 5.8% | ❌ 5 species | ❌ 6 species | ❌ 19/20 | ❌ None |

**Legend**: ✅ Goal attained | ❌ Goal not attained

### Key Findings
- **LAW090** is the only parcel meeting all revegetation goals in 2025
- **LAW129_118** shows the most challenges, failing 4 out of 5 goals
- **LAW094** and **LAW095** are close to compliance, primarily failing the 10% cover requirement

### ATTO/ERNA Policy Caps
The analysis includes policy-based capping of ATTO and ERNA species based on reference parcel averages:

- **LAW90/94/95 Group**: ATTO 0.06%, ERNA10 0.32% (combined 0.38%)
- **LAW118/129 Group**: ATTO 2.79%, ERNA10 4.28% (combined 7.07%)

These caps ensure revegetation projects don't exceed reference parcel conditions for these species.

## Interactive Data Portal

**🌐 [View Live Analysis & Interactive Maps](https://inyo-gov.github.io/revegetation-projects/)**

The website provides:
- Interactive compliance tables with filtering and search
- Reference parcel analysis with ATTO/ERNA species thresholds  
- Historical data visualization and trend analysis
- Downloadable datasets in multiple formats (CSV, GeoJSON, Shapefile)
- Comprehensive data processing pipeline documentation

## Repository Structure

```
├── _targets.R              # Reproducible data processing pipeline
├── code/                   # R functions and data loading scripts
├── data/                   # Raw and processed monitoring data
│   ├── raw/               # Original data files (Excel, CSV)
│   ├── processed/         # Cleaned and analyzed datasets
│   └── gis/               # Spatial data (shapefiles, GeoJSON)
├── docs/                  # Rendered website files
├── output/                # Analysis outputs and summaries
├── www/                   # Website assets (logos, images)
├── *.qmd                  # Quarto documents for analysis and reporting
└── README.md              # This file
```

## Data Downloads

### Primary Datasets
- **[Compliance Summary](https://github.com/inyo-gov/revegetation-projects/blob/main/data/processed/full_summary.csv)** - Complete goal attainment analysis
- **[Reference Parcel Data](https://github.com/inyo-gov/revegetation-projects/blob/main/data/processed/reference_parcel_summary.csv)** - ATTO/ERNA threshold analysis
- **[Transect-Species Data](https://github.com/inyo-gov/revegetation-projects/blob/main/data/processed/transect_species_data.csv)** - Detailed species cover by transect
- **[Spatial Data](https://github.com/inyo-gov/revegetation-projects/tree/main/data/gis)** - Parcel boundaries and transect locations

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
   quarto render
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

*Last updated: September 2025*