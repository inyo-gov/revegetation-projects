# Standardized Transect Loader Function
# This function loads and standardizes all transect data sources

load_all_transects <- function() {
  library(sf)
  library(dplyr)
  library(targets)
  
  # Initialize empty list to store all transect datasets
  all_transect_sources <- list()
  
  # 1. Load 2003 Reveg Projects (from targets)
  tryCatch({
    tar_load(raw_transects_xy)
    
    # Filter out old LAW118/129 transects (will be replaced with 2025 data)
    transects_2003 <- raw_transects_xy %>%
      filter(!(parcel %in% c("LAW118", "LAW129"))) %>%
      st_as_sf(coords = c("x", "y"), crs = 26911) %>%  # NAD83 UTM Zone 11N
      st_transform(4326) %>%  # Convert to WGS84
      mutate(
        source_file = "raw_transects_xy",
        project_type = "2003 Reveg Projects",
        parcel = parcel,
        transect = as.character(transect),
        bearing = bearing,
        popup_text = paste0(
          "<b>2003 Transect ", transect, "</b><br>",
          "Parcel: ", parcel, "<br>",
          "Bearing: ", bearing, "°<br>",
          "Type: 2003 Reveg Projects"
        )
      )
    
    all_transect_sources[["2003_projects"]] <- transects_2003
    message("✓ Loaded 2003 revegetation projects: ", nrow(transects_2003), " transects")
  }, error = function(e) {
    message("✗ Error loading 2003 projects: ", e$message)
  })
  
  # 2. Load LAW118/129 2025 transects
  tryCatch({
    law118_129_2025 <- st_read("data/gis/LAW118_129_transect_start_2025.geojson", quiet = TRUE) %>%
      st_transform(4326) %>%
      rename(
        parcel = Parcel,
        transect = Transect,
        bearing = Bearing
      ) %>%
      mutate(
        source_file = "LAW118_129_transect_start_2025.geojson",
        project_type = "2003 Reveg Projects",
        popup_text = paste0(
          "<b>2025 Transect ", transect, "</b><br>",
          "Parcel: ", parcel, "<br>",
          "Bearing: ", bearing, "°<br>",
          "Type: 2003 Reveg Projects (2025)"
        )
      )
    
    all_transect_sources[["law118_129_2025"]] <- law118_129_2025
    message("✓ Loaded LAW118/129 2025 transects: ", nrow(law118_129_2025), " transects")
  }, error = function(e) {
    message("✗ Error loading LAW118/129 2025: ", e$message)
  })
  
  # 3. Load 1991 Revegetation Projects
  tryCatch({
    reveg_1991 <- st_read("data/gis/Revegetation_Transects91_view.geojson", quiet = TRUE) %>%
      st_transform(4326) %>%
      mutate(
        source_file = "Revegetation_Transects91_view.geojson",
        project_type = "1991 Reveg Projects",
        parcel = parcel,
        transect = transct,  # Map transct to transect
        bearing = brng_dg,   # Map brng_dg to bearing
        popup_text = paste0(
          "<b>1991 Transect ", transct, "</b><br>",
          "Parcel: ", parcel, "<br>",
          "Bearing: ", brng_dg, "°<br>",
          "Type: 1991 Reveg Projects"
        )
      )
    
    all_transect_sources[["1991_projects"]] <- reveg_1991
    message("✓ Loaded 1991 revegetation projects: ", nrow(reveg_1991), " transects")
  }, error = function(e) {
    message("✗ Error loading 1991 projects: ", e$message)
  })
  
  # 4. Load TIN054 transects
  tryCatch({
    tin054 <- st_read("data/gis/TIN054_tpost_transects.json", quiet = TRUE) %>%
      st_transform(4326) %>%
      mutate(
        source_file = "TIN054_tpost_transects.json",
        project_type = "1991 Reveg Projects",
        parcel = "TIN054",
        transect = paste0("TIN054_", row_number()),
        bearing = 0,
        popup_text = paste0(
          "<b>TIN054 Transect ", transect, "</b><br>",
          "Parcel: TIN054<br>",
          "Type: 1991 Reveg Projects"
        )
      )
    
    all_transect_sources[["tin054"]] <- tin054
    message("✓ Loaded TIN054 transects: ", nrow(tin054), " transects")
  }, error = function(e) {
    message("✗ Error loading TIN054: ", e$message)
  })
  
  # 5. Load BLK16E transects
  tryCatch({
    blk16e <- st_read("data/gis/blk16e_transects.geojson", quiet = TRUE) %>%
      st_transform(4326) %>%
      mutate(
        source_file = "blk16e_transects.geojson",
        project_type = "1991 Reveg Projects",
        parcel = parcel,
        transect = transect,
        bearing = ifelse(is.na(bearing_deg), 0, bearing_deg),
        popup_text = paste0(
          "<b>BLK16E Transect ", transect, "</b><br>",
          "Parcel: ", parcel, "<br>",
          "Bearing: ", bearing, "°<br>",
          "Type: 1991 Reveg Projects"
        )
      )
    
    all_transect_sources[["blk16e"]] <- blk16e
    message("✓ Loaded BLK16E transects: ", nrow(blk16e), " transects")
  }, error = function(e) {
    message("✗ Error loading BLK16E: ", e$message)
  })
  
  # 6. Load IND105 transects
  tryCatch({
    ind105 <- st_read("data/gis/IND105.shp", quiet = TRUE) %>%
      st_transform(4326) %>%
      mutate(
        source_file = "IND105.shp",
        project_type = "1991 Reveg Projects",
        parcel = parcel,
        transect = ifelse("transect_p" %in% colnames(.), transect_p, 
                         ifelse("transect_n" %in% colnames(.), transect_n, paste0("IND105_", row_number()))),
        bearing = ifelse("bearing_de" %in% colnames(.), bearing_de, 0),
        popup_text = paste0(
          "<b>IND105 Transect ", transect, "</b><br>",
          "Parcel: ", parcel, "<br>",
          "Bearing: ", bearing, "°<br>",
          "Type: 1991 Reveg Projects"
        )
      )
    
    all_transect_sources[["ind105"]] <- ind105
    message("✓ Loaded IND105 transects: ", nrow(ind105), " transects")
  }, error = function(e) {
    message("✗ Error loading IND105: ", e$message)
  })
  
  # Combine all transect sources
  if (length(all_transect_sources) > 0) {
    # Get common columns across all datasets
    common_cols <- Reduce(intersect, lapply(all_transect_sources, colnames))
    
    # Ensure we have the essential columns
    essential_cols <- c("parcel", "transect", "bearing", "project_type", "popup_text", "source_file", "geometry")
    available_cols <- intersect(essential_cols, common_cols)
    
    # Combine all datasets with geometry validation
    combined_transects <- do.call(rbind, lapply(all_transect_sources, function(x) {
      # Ensure geometry is valid and convert to points if needed
      x_valid <- x %>%
        select(all_of(available_cols)) %>%
        st_make_valid() %>%
        st_cast("POINT")  # Ensure all geometries are points
      
      return(x_valid)
    }))
    
    # Summary statistics
    summary_stats <- combined_transects %>%
      st_drop_geometry() %>%
      group_by(project_type, source_file) %>%
      summarise(
        n_transects = n(),
        n_parcels = n_distinct(parcel),
        .groups = 'drop'
      )
    
    message("\n📊 Transect Loading Summary:")
    print(summary_stats)
    
    return(combined_transects)
  } else {
    message("✗ No transect data could be loaded")
    return(st_sf(
      parcel = character(),
      transect = character(),
      bearing = numeric(),
      project_type = character(),
      popup_text = character(),
      source_file = character(),
      geometry = st_sfc(),
      crs = 4326
    ))
  }
}
