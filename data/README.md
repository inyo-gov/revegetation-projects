# Revegetation Data Package

This data package contains all raw data files used in the revegetation compliance analysis for Inyo County Water Department (ICWD).

## Data Sources and Collection Information

### 1. 2025 Revegetation Data
- **File**: `LawsRevegetationData_SummaryTable_2025_ForICWD092525.xlsx`
- **Source**: Los Angeles Department of Water and Power (LADWP)
- **Collection Method**: Vegetation surveys using point-intercept method
- **Survey Period**: 2025
- **Areas Covered**: LAW090, LAW094, LAW095
- **Data Format**: Excel file with 'Species List Data' sheet containing raw hit counts
- **Notes**: Cover values are raw hits that need conversion (hits * 1/200 * 100)

### 2. LAW118/129 Data
- **File**: `LAW118_129_reveg2025_e.csv`
- **Source**: Los Angeles Department of Water and Power (LADWP)
- **Collection Method**: Vegetation surveys using point-intercept method
- **Survey Period**: 2025
- **Areas Covered**: LAW118, LAW129
- **Data Format**: CSV with unique header structure (metadata in first 3 rows)
- **Notes**: Combined as single management unit for analysis

### 3. TypeE Species Lists
- **File**: `TypeE_Transfer_SppList.xlsx`
- **Source**: Los Angeles Department of Water and Power (LADWP)
- **Purpose**: Defines allowable species for compliance calculations
- **Sheets**: 
  - `TypeE_List_90_94_95`: Species allowed for LAW090/094/095
  - `TypeE_List_118_129`: Species allowed for LAW118/129
- **Notes**: Based on 2003 Plan species lists

### 4. Species Attributes Database
- **File**: `species.csv`
- **Source**: Inyo County Water Department (ICWD)
- **Purpose**: Species characteristics and metadata
- **Fields**: Code, Species, Genus, CommonName, Phreatophyte, Lifecycle, Lifeform, WeedyInOV, Noxious, Toxic
- **Notes**: Master species database with plant characteristics

### 5. Historical Data
- **Files**: `laws_reveg_data_2022.csv`, `laws_reveg_data_2024.csv`
- **Source**: Inyo County Water Department (ICWD)
- **Collection Method**: Vegetation surveys using point-intercept method
- **Survey Periods**: 2022, 2024
- **Areas Covered**: All revegetation parcels
- **Data Format**: CSV with standardized format
- **Notes**: Used for trend analysis and comparison

## Data Processing Notes

### Transect Counts
- **LAW090 (2025)**: 30 transects
- **All Other Parcels**: 20 transects each
- **LAW118/129**: Combined as single management unit

### Capping Rules Applied
- **ERNA10 & ATTO**: 0.3% cap for LAW90/94/95, 2% cap for LAW118/129
- **ATPO**: 3% cap for LAW118/129 only, no cap for LAW90/94/95

### Data Quality
- All data has undergone QA/QC checks
- Missing values and outliers identified and documented
- Transect counts verified against field records

## File Structure

```
data/
├── README.md                                    # This file
├── species.csv                                  # Species attributes database
├── allowable_species_by_group.csv              # Processed allowable species lists
├── LawsRevegetationData_SummaryTable_2025_ForICWD092525.xlsx  # 2025 LADWP data
├── TypeE_Transfer_SppList.xlsx                 # Allowable species lists
├── raw/
│   └── reveg/
│       ├── LAW118_129_reveg2025_e.csv          # LAW118/129 2025 data
│       ├── laws_reveg_data_2022.csv            # 2022 historical data
│       └── laws_reveg_data_2024.csv            # 2024 historical data
└── revegetation_data_package.zip             # Complete data package
```

## Contact Information

For questions about this data package, contact:
- **Inyo County Water Department**
- **Data Analysis**: Revegetation Compliance Program
- **Last Updated**: October 2025

## Data Use Agreement

This data is provided for revegetation compliance analysis and monitoring purposes. Users should cite ICWD and LADWP as data sources when using this information.
