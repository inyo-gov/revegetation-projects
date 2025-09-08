

# _targets.R file for revegetation data processing pipeline
library(targets)
library(tarchetypes)

# Source functions
source("code/functions.R")

# Set target options
tar_option_set(
  packages = c("readr", "dplyr", "tidyr", "stringr", "purrr", "here", "janitor", "sf", "DT", "ggplot2", "readxl"),
  format = "rds",
  error = "continue"
)

# Define the pipeline
list(
  # Raw data inputs
  tar_target(
    name = raw_species,
    command = read.csv(here("data", "species.csv")),
    description = "Species attributes lookup table"
  ),
  
  tar_target(
    name = raw_parcels_shp,
    command = st_read(here('data','gis', 'LA_parcels_rasterizedd.shp'), quiet = TRUE),
    description = "Parcel boundaries shapefile"
  ),
  
  tar_target(
    name = raw_transects_xy,
    command = st_read(here('data','gis', 'laws_reveg_xy.shp'), quiet = TRUE) %>%
      mutate(transect = as.character(transect)) %>%
      mutate(x = sf::st_coordinates(.)[,1], y = sf::st_coordinates(.)[,2]) %>%
      st_drop_geometry(),
    description = "Transect XY coordinates"
  ),
  
  # Raw field data by year
  tar_target(
    name = raw_reveg_2022,
    command = read_csv(here('data','raw', 'reveg','laws_reveg_data_2022.csv')) %>% 
      mutate(year = 2022, transect = as.character(transect), hits = as.integer(hits), year = as.integer(year)),
    description = "2022 revegetation field data"
  ),
  
  tar_target(
    name = raw_reveg_2023,
    command = read_csv(here('data','raw', 'reveg','laws118_reveg_data_2023.csv')) %>% 
      mutate(year = 2023, transect = as.character(transect), hits = as.integer(hits), year = as.integer(year)),
    description = "2023 revegetation field data"
  ),
  
  tar_target(
    name = raw_reveg_2024,
    command = read_csv(here('data','raw', 'reveg','laws_reveg_data_2024.csv')) %>% 
      mutate(year = 2024, transect = as.character(transect), hits = as.integer(hits), year = as.integer(year)),
    description = "2024 revegetation field data"
  ),
  
  tar_target(
    name = raw_reveg_2025_118129,
    command = parse_reveg_totals_simple(here('data','raw','reveg','LAW118_129_reveg2025_e.csv')) %>% 
      mutate(transect = as.character(transect), hits = as.integer(hits), year = as.integer(year)),
    description = "2025 LAW118/129 revegetation field data"
  ),
  
  tar_target(
    name = raw_reveg_2025_909495,
    command = read_excel(here('data','raw', 'reveg','LADWP_ReferenceParcel_LAW090_094_095_2025_Data_2025.xlsx'), 
                        sheet = "LAW090,094,095Data") %>% 
      mutate(year = as.integer(Year), 
             transect = as.character(Transect), 
             hits = as.integer(Hits),
             parcel = Parcel,
             species = Species) %>%
      select(parcel, year, transect, species, hits),
    description = "2025 LAW90/94/95 revegetation field data"
  ),
  
  # Raw reference data
  tar_target(
    name = raw_ref_909495,
    command = bind_rows(
      # 2022-2024 data from original source
      read_csv(here('data','raw','reference','law090_094_095_reference_parcel_long_format.csv')) %>%
        clean_names() %>% 
        select(-date,-green, -live, -x1) %>% 
        rename(transect = tran_name, hits = hit) %>% 
        mutate(transect = as.character(transect)) %>% 
        left_join(raw_species, by = c("species" = "Code")) %>% 
        mutate(percent_cover = hits*.5) %>% 
        select(parcel, transect, species, hits, year, CommonName, Lifecycle, Lifeform, percent_cover),
      # 2025 data from Excel file
      read_excel(here('data','raw','reveg','LADWP_ReferenceParcel_LAW090_094_095_2025_Data_2025.xlsx'), 
                sheet = "ReferenceParcelData") %>%
        mutate(
          year = as.integer(Year),
          transect = as.character(Transect),
          hits = as.integer(Green + Live),
          parcel = Parcel,
          species = Species
        ) %>%
        left_join(raw_species, by = c("species" = "Code")) %>% 
        mutate(percent_cover = hits*.5) %>% 
        select(parcel, transect, species, hits, year, CommonName, Lifecycle, Lifeform, percent_cover)
    ),
    description = "LAW90/94/95 reference parcel data 2022-2025 (includes 2025 data from Excel)"
  ),
  
  tar_target(
    name = raw_ref_118129,
    command = bind_rows(
      # 2022-2023 data from original source
      read_csv(here('data','raw','reference','law118_129_reference_parcel_long_format.csv')) %>%
        clean_names() %>% 
        select(-date,-green, -live, -x1) %>% 
        rename(transect = tran_name, hits = hit) %>% 
        mutate(transect = as.character(transect)) %>% 
        left_join(raw_species, by = c("species" = "Code")) %>% 
        mutate(percent_cover = hits*.5) %>% 
        select(parcel, transect, species, hits, year, CommonName, Lifecycle, Lifeform, percent_cover),
      # 2025 data from processed output
      read_csv(here('output','2025','law118_129_reference_parcel_long_format_2025.csv')) %>%
        mutate(
          transect = as.character(transect),
          hits = as.integer(hits),
          year = as.integer(year)
        ) %>%
        left_join(raw_species, by = c("species" = "Code")) %>% 
        mutate(percent_cover = hits*.5) %>% 
        select(parcel, transect, species, hits, year, CommonName, Lifecycle, Lifeform, percent_cover)
    ),
    description = "LAW118/129 reference parcel data 2022-2025 (includes processed 2025 data)"
  ),
  
  tar_target(
    name = raw_ref_2025,
    command = read_excel(here('data','raw', 'reveg','LADWP_ReferenceParcel_LAW090_094_095_2025_Data_2025.xlsx'), 
                        sheet = "ReferenceParcelData") %>% 
      mutate(year = as.integer(Year), 
             transect = as.character(Transect), 
             hits = as.integer(Green + Live),
             parcel = Parcel,
             species = Species) %>%
      select(parcel, year, transect, species, hits),
    description = "2025 LAW90/94/95 reference parcel data from Excel"
  ),
  
  tar_target(
    name = raw_ref_2025_law909495,
    command = read_excel(here('data','raw', 'reveg','LADWP_ReferenceParcel_LAW090_094_095_2025_Data_2025.xlsx'), 
                        sheet = "ReferenceParcelData") %>% 
      mutate(year = as.integer(Year), 
             transect = as.character(Transect), 
             hits = as.integer(Green + Live),
             parcel = Parcel,
             species = Species) %>%
      select(parcel, year, transect, species, hits),
    description = "2025 LAW90/94/95 reference parcel data (separate target for clarity)"
  ),
  

  
  # Combined datasets
  tar_target(
    name = reveg_data_combined,
    command = bind_rows(raw_reveg_2022, raw_reveg_2023, raw_reveg_2024, raw_reveg_2025_118129, raw_reveg_2025_909495) %>%
      mutate(parcel_aggregate = ifelse(parcel %in% c("LAW118", "LAW129"), "LAW129_118", parcel)),
    description = "Combined revegetation data across all years"
  ),
  
  tar_target(
    name = ref_data_combined,
    command = bind_rows(raw_ref_909495, raw_ref_118129),
    description = "Combined reference parcel data (raw_ref_118129 now includes 2025 data)"
  ),
  
  # Main processing pipeline
  tar_target(
    name = r0_data,
    command = reveg_data_combined %>%
      left_join(raw_species, by = c("species" = "Code")) %>%
      mutate(percent_cover = hits * 0.5),
    description = "Revegetation data with species attributes"
  ),
  
  tar_target(
    name = r2_data,
    command = r0_data %>%
      mutate(parcel_original = parcel, parcel = parcel_aggregate) %>%
      mutate(transect_unique = paste(parcel_original, transect, sep = "_")),
    description = "Main analysis dataset with LAW118/129 aggregated"
  ),
  
  tar_target(
    name = r3_data,
    command = r2_data %>% mutate(tcov = hits*1/200*100),
    description = "Analysis dataset with transect cover calculations (aggregated parcels)"
  ),
  
  # Functional type summaries
  tar_target(
    name = species_hits_summary,
    command = summarise_reveg_to_transect(r3_data, possible_hits = 200),
    description = "Species hits summary by transect"
  ),
  
  tar_target(
    name = species_richness_summary,
    command = r3_data %>%
      filter(Lifecycle == 'Perennial') %>%
      group_by(parcel, year, species) %>%
      summarise(species_hits = sum(hits), .groups = 'drop') %>%
      group_by(parcel, year) %>%
      summarise(sp_richness = n_distinct(species), .groups = 'drop'),
    description = "Species richness by parcel and year"
  ),
  
  # Parcel-level summaries
  tar_target(
    name = parcel_sum_all,
    command = summarise_to_parcel(species_hits_summary),
    description = "Parcel summary including all species"
  ),
  
  tar_target(
    name = species_hits_summary_filtered,
    command = summarise_reveg_to_transect(
      r3_data %>% filter(!(species %in% c("ATTO", "ERNA10"))), 
      possible_hits = 200
    ),
    description = "Species hits summary by transect (excluding ATTO/ERNA)"
  ),
  
  tar_target(
    name = parcel_sum_filtered,
    command = summarise_to_parcel(species_hits_summary_filtered),
    description = "Parcel summary excluding ATTO and ERNA"
  ),
  
  # Comprehensive compliance table with all ATTO/ERNA scenarios
  tar_target(
    name = compliance_comprehensive_2025,
    command = {
      # Get the capped data
      capped_data <- reveg_capped_2025
      
      # Calculate parcel-level summaries
      parcel_summary <- capped_data %>%
        group_by(parcel, year, reference_group) %>%
        summarise(
          # Perennial cover metrics
          perennial_cover_original = mean(original_perennial_cover, na.rm = TRUE),
          perennial_cover_capped = mean(adjusted_perennial_cover, na.rm = TRUE),
          perennial_cover_full = mean(full_perennial_cover, na.rm = TRUE),
          
          # ATTO/ERNA contributions
          atto_contribution_avg = mean(atto_contribution, na.rm = TRUE),
          erna_contribution_avg = mean(erna_contribution, na.rm = TRUE),
          
          # Transect counts
          n_transects = n_distinct(transect_unique),
          
          # Transects meeting 2% threshold for each scenario
          transects_2pct_original = sum(original_perennial_cover >= 2, na.rm = TRUE),
          transects_2pct_capped = sum(adjusted_perennial_cover >= 2, na.rm = TRUE),
          transects_2pct_full = sum(full_perennial_cover >= 2, na.rm = TRUE),
          
          .groups = 'drop'
        ) %>%
        mutate(
          # Calculate compliance percentages
          pct_2pct_original = round((transects_2pct_original / n_transects) * 100, 1),
          pct_2pct_capped = round((transects_2pct_capped / n_transects) * 100, 1),
          pct_2pct_full = round((transects_2pct_full / n_transects) * 100, 1),
          
          # Round cover values
          across(c(perennial_cover_original, perennial_cover_capped, perennial_cover_full,
                   atto_contribution_avg, erna_contribution_avg), ~round(., 2))
        )
      
      # Add compliance status for each scenario
      compliance_status <- parcel_summary %>%
        mutate(
          # Original compliance (no ATTO/ERNA)
          compliance_original = case_when(
            perennial_cover_original >= 10 & pct_2pct_original >= 80 ~ "Compliant",
            TRUE ~ "Non-compliant"
          ),
          
          # Capped ATTO/ERNA compliance
          compliance_capped = case_when(
            perennial_cover_capped >= 10 & pct_2pct_capped >= 80 ~ "Compliant",
            TRUE ~ "Non-compliant"
          ),
          
          # Full ATTO/ERNA compliance
          compliance_full = case_when(
            perennial_cover_full >= 10 & pct_2pct_full >= 80 ~ "Compliant",
            TRUE ~ "Non-compliant"
          )
        )
      
      compliance_status
    },
    description = "Comprehensive compliance table showing all ATTO/ERNA scenarios for 2025"
  ),
  
  # Species metrics
  tar_target(
    name = species_attributes,
    command = raw_species,
    description = "Species attributes and classifications"
  ),
  
  # Comprehensive species summary across all years and parcels
  tar_target(
    name = species_summary_all_years,
    command = {
      # Get all species data with attributes
      species_data <- r3_data %>%
        select(parcel, year, species, hits, percent_cover, transect_unique, CommonName, Lifecycle, Lifeform) %>%
        # Calculate annual average cover per species per parcel
        group_by(parcel, year, species, CommonName, Lifecycle, Lifeform) %>%
        summarise(
          annual_avg_cover = mean(percent_cover, na.rm = TRUE),
          total_hits = sum(hits, na.rm = TRUE),
          n_transects = n_distinct(transect_unique),
          .groups = 'drop'
        ) %>%
        # Calculate summary statistics across all years for each species-parcel combination
        group_by(parcel, species, CommonName, Lifecycle, Lifeform) %>%
        summarise(
          years_detected = n_distinct(year),
          years_available = n_distinct(r3_data$year), # Total years in dataset
          min_annual_avg_cover = min(annual_avg_cover, na.rm = TRUE),
          max_annual_avg_cover = max(annual_avg_cover, na.rm = TRUE),
          mean_annual_avg_cover = mean(annual_avg_cover, na.rm = TRUE),
          total_hits_all_years = sum(total_hits, na.rm = TRUE),
          total_transects_all_years = sum(n_transects, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        # Only include species that were detected at least once
        filter(total_hits_all_years > 0) %>%
        # Round numeric columns
        mutate(
          across(c(min_annual_avg_cover, max_annual_avg_cover, mean_annual_avg_cover), ~round(., 2))
        ) %>%
        # Reorder columns for better presentation
        select(
          parcel, species, CommonName, Lifecycle, Lifeform,
          years_detected, years_available,
          min_annual_avg_cover, max_annual_avg_cover, mean_annual_avg_cover,
          total_hits_all_years, total_transects_all_years
        ) %>%
        # Sort by parcel, then by mean cover (descending)
        arrange(parcel, desc(mean_annual_avg_cover))
      
      species_data
    },
    description = "Comprehensive species summary showing average cover per parcel across all years with min-max ranges"
  ),
  
  tar_target(
    name = grass_species_counts,
    command = r3_data %>%
      filter(Lifeform == 'Grass', Lifecycle == 'Perennial') %>%
      group_by(parcel, year, species) %>%
      summarise(total_hits = sum(hits), .groups = 'drop') %>%
      group_by(parcel, year) %>%
      summarise(grass_species = n(), .groups = 'drop'),
    description = "Grass species counts by parcel and year"
  ),
  
  # Transect analysis
  tar_target(
    name = transect_cover_details,
    command = summarize_transect_cover_filtered(r3_data),
    description = "Detailed transect cover analysis"
  ),
  
  tar_target(
    name = transect_summary_overall,
    command = transect_cover_details %>%
      group_by(parcel, year) %>%
      summarise(
        transects_at_2_percent = paste0(sum(meets_2_percent == "Yes"), "/", n_distinct(transect_unique)),
        .groups = 'drop'
      ),
    description = "Overall transect summary by parcel and year"
  ),
  
  # Reference parcel analysis
  tar_target(
    name = reference_parcel_summary,
    command = summarize_reference_parcels(ref_data_combined),
    description = "Reference parcel summary statistics"
  ),
  
  # ATTO/ERNA analysis in reference parcels
  tar_target(
    name = reference_atto_erna_analysis,
    command = {
      # Calculate ATTO and ERNA contribution to reference parcels
      # First, calculate cover per transect for ATTO/ERNA
      atto_erna_by_transect <- ref_data_combined %>%
        filter(species %in% c("ATTO", "ERNA10")) %>%
        group_by(parcel, year, transect, species) %>%
        summarise(
          transect_hits = sum(hits, na.rm = TRUE),
          transect_cover = transect_hits * 0.5,  # Convert hits to percent cover
          .groups = 'drop'
        )
      
      # Calculate total perennial cover per transect
      perennial_by_transect <- ref_data_combined %>%
        filter(Lifecycle == 'Perennial') %>%
        group_by(parcel, year, transect) %>%
        summarise(
          transect_perennial_hits = sum(hits, na.rm = TRUE),
          transect_perennial_cover = transect_perennial_hits * 0.5,
          .groups = 'drop'
        )
      
      # Join and calculate averages
      # Use full_join to include all transects, even those without ATTO/ERNA (zero hits)
      atto_erna_by_transect %>%
        full_join(perennial_by_transect, by = c("parcel", "year", "transect")) %>%
        # Fill NA values for transects without ATTO/ERNA with zeros
        mutate(
          transect_hits = ifelse(is.na(transect_hits), 0, transect_hits),
          transect_cover = ifelse(is.na(transect_cover), 0, transect_cover),
          species = ifelse(is.na(species), "ATTO", species)  # Fill species for zero-hit rows
        ) %>%
        # Calculate relative contribution per transect
        mutate(
          relative_cover_transect = (transect_cover / transect_perennial_cover) * 100
        ) %>%
        # Average across transects for each parcel-year-species combination
        group_by(parcel, year, species) %>%
        summarise(
          total_hits = sum(transect_hits, na.rm = TRUE),
          avg_cover = mean(transect_cover, na.rm = TRUE),
          avg_perennial_cover = mean(transect_perennial_cover, na.rm = TRUE),
          avg_relative_cover = mean(relative_cover_transect, na.rm = TRUE),
          n_transects = n(),
          .groups = 'drop'
        ) %>%
        # Round for display
        mutate(
          avg_cover = round(avg_cover, 1),
          avg_relative_cover = round(avg_relative_cover, 1)
        ) %>%
        arrange(parcel, year, species)
    },
    description = "ATTO/ERNA contribution analysis in reference parcels by year"
  ),
  
  # Revegetation data with ATTO/ERNA capping based on reference thresholds
  tar_target(
    name = reveg_capped_2025,
    command = {
      # Get reference thresholds for ATTO/ERNA capping from reference parcels
      reference_thresholds <- reference_atto_erna_analysis %>%
        filter(year == 2025) %>%
        mutate(reference_group = case_when(
          parcel %in% c('LAW012', 'LAW024', 'LAW028', 'LAW048', 'LAW049', 'LAW091', 'LAW093', 'LAW117', 'LAW130', 'LAW134') ~ 'LAW90/94/95',
          parcel %in% c('LAW029', 'LAW039', 'LAW069', 'LAW104', 'LAW119', 'PLC202', 'PLC219', 'PLC227', 'PLC230') ~ 'LW118/129',
          TRUE ~ 'Other'
        )) %>%
        filter(reference_group != 'Other') %>%
        group_by(species, reference_group) %>%
        summarise(
          cap_threshold = mean(avg_cover, na.rm = TRUE),
          .groups = 'drop'
        )
      
      # Get 2025 revegetation data
      reveg_2025 <- r3_data %>%
        filter(year == 2025) %>%
        mutate(
          # Determine reference group for each reveg parcel
          reference_group = case_when(
            parcel %in% c('LAW090', 'LAW094', 'LAW095') ~ 'LAW90/94/95',
            parcel %in% c('LAW118', 'LAW129') ~ 'LW118/129',
            TRUE ~ 'Other'
          )
        ) %>%
        filter(reference_group != 'Other')
      
      # Calculate capped ATTO/ERNA cover
      reveg_capped <- reveg_2025 %>%
        left_join(reference_thresholds, by = c('species', 'reference_group')) %>%
        mutate(
          # Calculate transect cover (hits/200*100)
          transect_cover = hits * 1/200 * 100,
          
          # Apply capping for ATTO/ERNA
          capped_cover = case_when(
            species == 'ATTO' ~ pmin(transect_cover, cap_threshold),
            species == 'ERNA10' ~ pmin(transect_cover, cap_threshold),
            TRUE ~ transect_cover
          ),
          
          # Calculate allowed addition (difference between original and capped)
          allowed_addition = transect_cover - capped_cover,
          
          # Set to 0 if no capping occurred
          allowed_addition = ifelse(allowed_addition < 0, 0, allowed_addition)
        ) %>%
        select(-cap_threshold)
      
      # Calculate perennial cover summaries by transect
      transect_summary <- reveg_capped %>%
        group_by(parcel, year, transect_unique, reference_group) %>%
        summarise(
          # Original perennial cover (excluding ATTO/ERNA)
          original_perennial_cover = sum(
            ifelse(species %in% c('ATTO', 'ERNA10'), 0, 
                   ifelse(Lifecycle == 'Perennial', transect_cover, 0)), 
            na.rm = TRUE
          ),
          
          # Capped ATTO/ERNA cover
          capped_atto_cover = sum(
            ifelse(species == 'ATTO', capped_cover, 0), 
            na.rm = TRUE
          ),
          
          capped_erna_cover = sum(
            ifelse(species == 'ERNA10', capped_cover, 0), 
            na.rm = TRUE
          ),
          
          # Total adjusted perennial cover
          adjusted_perennial_cover = original_perennial_cover + capped_atto_cover + capped_erna_cover,
          
          # Full ATTO/ERNA cover (uncapped)
          full_atto_cover = sum(
            ifelse(species == 'ATTO', transect_cover, 0), 
            na.rm = TRUE
          ),
          
          full_erna_cover = sum(
            ifelse(species == 'ERNA10', transect_cover, 0), 
            na.rm = TRUE
          ),
          
          # Full perennial cover (including uncapped ATTO/ERNA)
          full_perennial_cover = original_perennial_cover + full_atto_cover + full_erna_cover,
          
          # ATTO/ERNA contribution proportions
          atto_contribution = ifelse(adjusted_perennial_cover > 0, 
                                    (capped_atto_cover / adjusted_perennial_cover) * 100, 0),
          erna_contribution = ifelse(adjusted_perennial_cover > 0, 
                                    (capped_erna_cover / adjusted_perennial_cover) * 100, 0),
          
          .groups = 'drop'
        ) %>%
        mutate(
          across(c(original_perennial_cover, capped_atto_cover, capped_erna_cover, 
                   adjusted_perennial_cover, full_atto_cover, full_erna_cover, 
                   full_perennial_cover, atto_contribution, erna_contribution), 
                 ~round(., 2))
        )
      
      transect_summary
    },
    description = "2025 revegetation data with ATTO/ERNA capping based on reference parcel thresholds"
  ),
  
  # Metadata
  tar_target(
    name = reveg_metadata,
    command = reveg_data_combined |> 
      group_by(parcel, year) %>%
      summarise(
        n_species = n_distinct(species),
        n_transects = n_distinct(paste(parcel, transect, sep = "_")),
        .groups = 'drop'
      ) %>% 
      arrange(parcel, year, n_species, n_transects),
    description = "Revegetation data metadata summary"
  ),
  
  # MotherDuck Integration
  tar_target(
    name = motherduck_push,
    command = {
      # This target pushes all key datasets to MotherDuck
      # Note: Requires MotherDuck authentication token
      
      # Load required packages
      if (!require(duckdb, quietly = TRUE)) {
        message("DuckDB not available, skipping MotherDuck push")
        return(NULL)
      }
      
      # Connect to MotherDuck (commented out for safety)
      # con <- dbConnect(duckdb(), "md:?motherduck_token=YOUR_TOKEN_HERE")
      
      # For now, just return a message
      message("MotherDuck integration ready - uncomment connection code and add token")
      return("MotherDuck push target created")
    },
    description = "Push all processed data to MotherDuck backend"
  ),
  
  # Comprehensive compliance table for all years
  tar_target(
    name = compliance_comprehensive_all,
    command = {
      # Get reference thresholds for ATTO/ERNA capping
      reference_thresholds <- reference_atto_erna_analysis %>%
        filter(year == 2025) %>%
        mutate(reference_group = case_when(
          parcel %in% c('LAW012', 'LAW024', 'LAW028', 'LAW048', 'LAW049', 'LAW091', 'LAW093', 'LAW117', 'LAW130', 'LAW134') ~ 'LAW90/94/95',
          parcel %in% c('LAW029', 'LAW039', 'LAW069', 'LAW104', 'LAW119', 'PLC202', 'PLC219', 'PLC227', 'PLC230') ~ 'LW118/129',
          TRUE ~ 'Other'
        )) %>%
        filter(reference_group != 'Other') %>%
        group_by(species, reference_group) %>%
        summarise(
          cap_threshold = mean(avg_cover, na.rm = TRUE),
          .groups = 'drop'
        )
      
      # Get all parcel summaries
      parcel_sum_filtered_wide <- parcel_sum_filtered %>%
        select(parcel, year, Cover, n.transects) %>%
        rename(cover_original = Cover)
      
      parcel_sum_all_wide <- parcel_sum_all %>%
        select(parcel, year, Cover) %>%
        rename(cover_full = Cover)
      
      # Calculate capped cover (original + allowed ATTO/ERNA)
      capped_covers <- parcel_sum_all_wide %>%
        left_join(parcel_sum_filtered_wide, by = c('parcel', 'year')) %>%
        # Assign reference groups to revegetation parcels
        mutate(reference_group = case_when(
          parcel %in% c('LAW090', 'LAW094', 'LAW095') ~ 'LAW90/94/95',
          parcel == 'LAW129_118' ~ 'LW118/129',
          TRUE ~ 'Other'
        )) %>%
        filter(reference_group != 'Other') %>%
        # Calculate allowed ATTO/ERNA addition (use mean, not sum)
        left_join(
          reference_thresholds %>% 
            group_by(reference_group) %>% 
            summarise(allowed_atto_erna = mean(cap_threshold), .groups = 'drop'),
          by = 'reference_group'
        ) %>%
        mutate(
          cover_capped = pmin(cover_original + allowed_atto_erna, cover_full)
        )
      
      # Get other compliance metrics from existing data
      # Use target names directly instead of tar_load()
      
      # Species with ≥3 hits
      species_3hit_counts <- r2_data %>%
        filter(Lifecycle == 'Perennial') %>%
        group_by(parcel, year, species) %>%
        summarise(species_hits = sum(hits), .groups = 'drop') %>%
        filter(species_hits >= 3) %>%
        group_by(parcel, year) %>%
        summarise(count_3hit_species = n(), .groups = 'drop')
      
      # Species richness
      species_richness <- r2_data %>%
        filter(Lifecycle == 'Perennial') %>%
        group_by(parcel, year) %>%
        summarise(sp_richness = n_distinct(species), .groups = 'drop')
      
      # Transect coverage (≥2%)
      transect_coverage <- r3_data %>%
        filter(Lifecycle == 'Perennial') %>%
        # Handle merged parcels with overlapping transect names
        mutate(
          unique_transect_id = case_when(
            parcel == 'LAW129_118' & parcel_original == 'LAW118' ~ paste0(parcel, '_', as.numeric(transect) + 20),
            parcel == 'LAW129_118' & parcel_original == 'LAW129' ~ paste0(parcel, '_', transect),
            TRUE ~ paste0(parcel, '_', transect)
          )
        ) %>%
        group_by(parcel, year, unique_transect_id) %>%
        summarise(total_tcov = sum(tcov, na.rm = TRUE), .groups = 'drop') %>%
        mutate(meets_2_percent = total_tcov >= 2) %>%
        group_by(parcel, year) %>%
        summarise(
          transects_at_2_percent = paste0(sum(meets_2_percent), "/", n()),
          transect_status_symbol = ifelse(sum(meets_2_percent) == n(), "✓", "✗"),
          .groups = 'drop'
        )
      
      # Grass presence - ensure all parcels have a row even if 0 grass species
      grass_presence <- r3_data %>%
        group_by(parcel, year) %>%
        summarise(
          grass_species = sum(Lifeform == 'Grass', na.rm = TRUE),
          .groups = 'drop'
        )
      
      # Combine all data
      capped_covers %>%
        left_join(species_3hit_counts, by = c('parcel', 'year')) %>%
        left_join(species_richness, by = c('parcel', 'year')) %>%
        left_join(transect_coverage, by = c('parcel', 'year')) %>%
        left_join(grass_presence, by = c('parcel', 'year')) %>%
        # Add compliance indicators
        mutate(
          # Cover compliance for each scenario
          cover_status_original = paste0(ifelse(cover_original >= 10, "✓", "✗"), " ", round(cover_original, 1), "%"),
          cover_status_capped = paste0(ifelse(cover_capped >= 10, "✓", "✗"), " ", round(cover_capped, 1), "%"),
          cover_status_full = paste0(ifelse(cover_full >= 10, "✓", "✗"), " ", round(cover_full, 1), "%"),
          
          # Other compliance indicators
          species_3hit_status = paste0(ifelse(count_3hit_species >= 6, "✓", "✗"), " ", count_3hit_species),
          richness_status = paste0(ifelse(sp_richness >= 10, "✓", "✗"), " ", sp_richness),
          grass_status = paste0(ifelse(grass_species >= 1, "✓", "✗"), " ", grass_species)
        ) %>%
        # Use the pre-calculated transect status symbol
        mutate(
          transect_status = paste0(transect_status_symbol, " ", transects_at_2_percent)
        ) %>%
        # Add overall compliance summary based on capped column (main compliance indicator)
        mutate(
          compliance_summary = case_when(
            cover_capped >= 10 & 
            count_3hit_species >= 6 & 
            sp_richness >= 10 & 
            transect_status_symbol == "✓" & 
            grass_species >= 1 ~ "Yes",
            TRUE ~ "No"
          )
        ) %>%
        select(
          parcel, year, reference_group,
          cover_status_full, cover_status_capped, cover_status_original,
          species_3hit_status, richness_status, transect_status, grass_status,
          compliance_summary
        )
    },
    description = "Comprehensive compliance table showing all ATTO/ERNA scenarios for all years"
  )
)