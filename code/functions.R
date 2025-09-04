# functions.R - Data processing functions for revegetation analysis

# Parse wide-format revegetation data
parse_reveg_totals_simple <- function(path, add_year = 2025) {
  raw <- readr::read_csv(path, col_names = FALSE, show_col_types = FALSE)
  
  # Find header rows by label in column 1
  r_parcel   <- which(raw[[1]] == "Parcel")[1]
  r_transect <- which(raw[[1]] == "Transect")[1]
  r_bearing  <- which(raw[[1]] == "Bearing")[1]  # may be NA if not present
  if (is.na(r_parcel) || is.na(r_transect)) {
    stop("Couldn't find 'Parcel' and 'Transect' rows in column 1.")
  }
  
  # Data columns are every column except the first (species names)
  data_cols <- 2:ncol(raw)
  
  # Species rows start after the last header row (Parcel/Transect/Bearing)
  species_start <- max(c(r_parcel, r_transect, ifelse(is.na(r_bearing), -Inf, r_bearing))) + 1
  species_rows  <- species_start:nrow(raw)
  
  # Build per-column metadata (parcel / transect / bearing)
  meta <- tibble(
    col_id   = paste0("col", seq_along(data_cols)),
    col_idx  = data_cols,
    parcel   = as.character(unlist(raw[r_parcel,   data_cols], use.names = FALSE)),
    transect = as.character(unlist(raw[r_transect, data_cols], use.names = FALSE)),
    bearing  = if (!is.na(r_bearing))
                 as.character(unlist(raw[r_bearing, data_cols], use.names = FALSE))
               else NA_character_
  ) %>%
    mutate(
      parcel = str_trim(replace_na(parcel, "")),
      transect = str_trim(replace_na(transect, "")),
      bearing = str_trim(replace_na(bearing, ""))
    )
  
  # Slice the species block and assign temporary unique column names
  block <- raw[species_rows, c(1, data_cols), drop = FALSE]
  colnames(block) <- c("species", meta$col_id)
  
  # Tidy: long by column id, then join metadata
  out <- block %>%
    filter(!is.na(species), species != "") %>%
    pivot_longer(
      cols = -species,
      names_to = "col_id",
      values_to = "hits",
      values_transform = list(hits = as.numeric)
    ) %>%
    left_join(meta, by = "col_id") %>%
    filter(parcel != "", transect != "") %>%
    transmute(
      parcel,
      transect = as.character(transect),
      species  = as.character(species),
      hits     = replace_na(hits, 0),
      bearing  = if (all(is.na(bearing))) NULL else bearing,
      year     = add_year
    )
  
  out
}

# Summarize revegetation data to transect level
summarise_reveg_to_transect <- function(x, possible_hits){
  # at the transect level sum cover at the lifecycle, lifeform level
  # Use transect_unique if available, otherwise use transect
  transect_col <- if("transect_unique" %in% colnames(x)) "transect_unique" else "transect"
  
  tran.sums <- x %>%
    group_by(parcel,year,!!sym(transect_col),Lifecycle,Lifeform)%>% 
    summarise(Cover=(sum(hits)/possible_hits) * 100, .groups = 'drop') # sum cover (total number of hits) and divide by 200 possible hits - multiply it by 100 to scale from 0-100.
  
  # summarise for each e.g. lifecycle/lifeform annual/perennial grass
  
  # create new dataframe with summary of all lifecycle cover
  # this will be joined to the wide dataframe below
  tran.tlc <- tran.sums %>%
    group_by(parcel,year, !!sym(transect_col)) %>% # sum all cover across all lifecycle/lifeform
    summarise(tot.live.cover = sum(Cover), .groups = 'drop')
  
  # spread the perennial lifeforms into columns
  pft.wide<-tran.sums %>% filter(Lifecycle == 'Perennial') %>% select(-Lifecycle) %>%  spread(Lifeform,Cover)
  # change na to 0
  pft.wide[is.na(pft.wide)] <- 0
  
  # create total cover variable
  pft.wide <- pft.wide %>%
    mutate(Cover= Grass + Herb + Herb_Shrub + Shrub  + Tree)
  
  # now join the tlc from above
  pft.wide.wtot.cov <- pft.wide %>%
    left_join(tran.tlc, by = c("parcel", transect_col, 'year'))
  
  # make a few more columns for percentage of the total for each functional group
  full <- pft.wide.wtot.cov %>%
    mutate(pShrubTran = Shrub / Cover,
           pGrassTran = Grass / Cover,
           pHerbTran = Herb / Cover) %>%
    mutate_if(is.numeric, ~replace_na(., 0))
  
  return(full)
}

# Summarize transects to parcel level
summarise_to_parcel <- function(x) {
  # Use transect_unique if available, otherwise use transect
  transect_col <- if("transect_unique" %in% colnames(x)) "transect_unique" else "transect"
  
  p <- x %>%
    group_by(parcel, year) %>%
    summarise(
      SD_Cover = sd(Cover, na.rm = TRUE),
      n.transects = n_distinct(!!sym(transect_col)),
      SE_Cover = SD_Cover / sqrt(n.transects),
      CI_95_Cover_Lower = mean(Cover, na.rm = TRUE) - qt(0.975, df = n.transects - 1) * SE_Cover,
      CI_95_Cover_Upper = mean(Cover, na.rm = TRUE) + qt(0.975, df = n.transects - 1) * SE_Cover,
      Cover = mean(Cover, na.rm = TRUE),
      Shrub = mean(Shrub, na.rm = TRUE),
      Herb = mean(Herb, na.rm = TRUE),
      Grass = mean(Grass, na.rm = TRUE),
      Tree = mean(Tree, na.rm = TRUE),
      TLC = mean(tot.live.cover, na.rm = TRUE),
      .groups = 'drop'
    )
  
  return(p)
}

# Summarize transect cover with 2% threshold
summarize_transect_cover_filtered <- function(data) {
  # Use transect_unique if available, otherwise use transect
  transect_col <- if("transect_unique" %in% colnames(data)) "transect_unique" else "transect"
  
  data %>%
    filter(Veg_Type != "weed", Lifecycle == 'Perennial') %>%
    group_by(parcel, year, !!sym(transect_col)) %>%
    summarise(
      total_hits = sum(hits, na.rm = TRUE),
      total_tcov = sum(tcov, na.rm = TRUE),
      meets_2_percent = ifelse(total_tcov >= 2, "Yes", "No"),
      .groups = 'drop'
    )
}

# Summarize reference parcels
summarize_reference_parcels <- function(ref_data) {
  # Calculate the total cover for each transect
  transect_summary <- ref_data %>%
    group_by(parcel, year, transect) %>%
    summarise(Total_Cover = sum(hits, na.rm = TRUE) * 0.5, .groups = 'drop')  # Convert hits to percent cover
  
  # Aggregate at the parcel-year level to calculate the mean, SD, and SE
  parcel_summary <- transect_summary %>%
    group_by(parcel, year) %>%
    summarise(
      Mean_Cover = mean(Total_Cover, na.rm = TRUE),
      SD_Cover = sd(Total_Cover, na.rm = TRUE),
      SE_Cover = SD_Cover / sqrt(n()),
      .groups = 'drop'
    )
  
  return(parcel_summary)
}

# Generate parcel summary table for compliance
generate_parcel_summary_table <- function(r3_data, species_data) {
  # Use transect_unique if available, otherwise use transect
  transect_col <- if("transect_unique" %in% colnames(r3_data)) "transect_unique" else "transect"
  
  # Calculate count_grass_species
  count_grass_species <- r3_data %>% 
    filter(year != 2022, Lifeform == 'Grass') %>%
    select(parcel, !!sym(transect_col), species, Genus, Species, Lifecycle, CommonName) %>%
    arrange(parcel, !!sym(transect_col), Genus) %>%
    group_by(parcel) %>%
    summarise(grass_species = n(), .groups = 'drop')
  
  # Calculate species_hit
  species_hit <- r3_data %>% 
    filter(Lifecycle == 'Perennial') %>%
    group_by(parcel,species) %>%
    summarise(species_hits = sum(hits), .groups = 'drop') %>%
    filter(species_hits >= 3) %>% ungroup()
  
  # Calculate species_rich
  species_rich <- r3_data %>% 
    filter(Lifecycle == 'Perennial') %>%
    group_by(parcel,species) %>%
    summarise(species_hits = sum(hits), .groups = 'drop')
  
  # Calculate count_hit_species_per_parcel
  count_hit_species_per_parcel <- species_hit %>%
    group_by(parcel) %>%
    summarise(count_3hit_species = n(), .groups = 'drop')
  
  # Calculate species_richness_parcel
  species_richness_parcel <- species_rich %>%
    group_by(parcel) %>%
    summarise(sp_richness = n_distinct(species), .groups = 'drop')
  
  # Calculate transect_cover_summary
  transect_cover_summary <- summarize_transect_cover_filtered(r3_data)
  
  # Calculate transect_summary
  transect_summary <- transect_cover_summary %>%
    filter(year != 2022) %>%
    group_by(parcel) %>%
    summarise(
      transects_at_2_percent = paste0(sum(meets_2_percent == "Yes"), "/", n()),
      .groups = 'drop'
    )
  
  # Calculate parcel_sum (assuming summarise_to_parcel is available and `full` is derived from `r3_data`)
  full_data <- summarise_reveg_to_transect(r3_data, possible_hits = 200)
  parcel_sum <- summarise_to_parcel(full_data)
  
  # Create the final summary_table
  summary_table <- parcel_sum %>%
    filter(year != 2022) %>%
    left_join(count_hit_species_per_parcel, by = 'parcel') %>%
    left_join(count_grass_species, by = 'parcel') %>%
    left_join(transect_summary, by = 'parcel') %>%
    left_join(species_richness_parcel, by = 'parcel')
  
  return(summary_table)
}

# Bubble map function for spatial visualization
bubble_maps_with_parcels <- function(data, parcels) {
  ggplot(data) +
    # Adding parcel boundaries with no fill and a black outline
    geom_sf(data = parcels, fill = NA, color = "black", size = 0.5) +
    
    # Add text labels for parcels using the PCL field
    geom_sf_text(data = parcels, aes(label = PCL), size = 3, color = "black", fontface = "bold") +
    
    # Mapping the size of the points to the absolute percent cover and coloring based on the percent change
    geom_sf(aes(size = abs(percent_cover), 
                color = case_when(
                  percent_cover > 0 ~ "positive",
                  percent_cover < 0 ~ "negative",
                  percent_cover == 0 ~ "zero"
                ), geometry = geometry)) +  
    coord_sf() +
    
    # Adjust the size breaks to have more granular control over point sizes
    scale_size_binned(breaks = c(0, 2, 4, 6, 8, 10), range = c(0.1, 6)) +
    
    # Apply color scale for three categories: red for negative, white for zero, green for positive
    scale_color_manual(
      values = c("negative" = "red", "zero" = "white", "positive" = "green"),
      guide = guide_legend(override.aes = list(size = 5))
    ) +
    
    # Facet by species and year_compare to see changes per species over years
    facet_grid(species ~ year_compare) +
    
    # Move legend to the bottom and adjust its layout
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_blank(),
      legend.position = "bottom",
      legend.box = "vertical",
      strip.text.y = element_text(angle = 0, hjust = 0.5)
    )
}

# Calculate cover differences between years
calculate_cover_difference <- function(data) {
  # Remove geometry temporarily to handle pivoting
  data_no_geometry <- st_drop_geometry(data)
  
  # Group by necessary columns and calculate the mean percent_cover
  cover_diff_data <- data_no_geometry %>%
    group_by(parcel, transect, species, year) %>%
    summarise(percent_cover = mean(percent_cover, na.rm = TRUE), .groups = 'drop') %>%
    
    # Pivot wider to compare 2022 and 2024 data
    pivot_wider(
      names_from = year,
      values_from = percent_cover,
      names_prefix = "cover_"
    ) %>%
    
    # Replace NAs with 0 in both cover_2022 and cover_2024
    mutate(cover_2022 = replace_na(cover_2022, 0),
           cover_2024 = replace_na(cover_2024, 0)) %>%
    
    # Calculate the difference between 2024 and 2022
    mutate(cover_diff = cover_2024 - cover_2022) %>%
    
    # Ensure unique records
    distinct(parcel, transect, species, cover_2022, cover_2024, cover_diff)
  
  # Join geometry back into the dataset, ensuring no duplicates are created
  result_with_geometry <- left_join(cover_diff_data,
                                    distinct(select(data, parcel, transect, geometry)),
                                    by = c("parcel", "transect"))
  
  return(result_with_geometry)
}

