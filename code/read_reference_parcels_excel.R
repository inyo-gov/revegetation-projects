# Function to read reference parcel data from Excel file
# Based on the Google Sheets function but adapted for Excel format

library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)

read_reference_parcels_excel <- function(file_path, sheets = NULL) {
  
  # Get sheet names if not provided
  if (is.null(sheets)) {
    sheets <- excel_sheets(file_path)
  }
  
  parse_one_sheet <- function(sheet_name) {
    # Read raw data without column names
    raw <- read_excel(file_path, sheet = sheet_name, col_names = FALSE, na = c("", "NA"))
    
    # Extract parcel name from row 2, column 1
    parcel <- as.character(raw[2, 1])
    if (is.na(parcel) || parcel == "") {
      parcel <- sheet_name  # fallback to sheet name
    }
    
    # Find transect row (row 5 based on structure)
    r_tran <- 5
    if (raw[r_tran, 1] != "Transect") {
      # Try to find it dynamically
      r_tran <- which(raw[[1]] == "Transect")[1]
      if (is.na(r_tran)) {
        warning(paste("Could not find 'Transect' header in sheet", sheet_name))
        return(tibble())
      }
    }
    
    # Find species row (row 6 based on structure)
    r_species <- 6
    if (raw[r_species, 1] != "Species") {
      # Try to find it dynamically
      r_species <- which(raw[[1]] == "Species")[1]
      if (is.na(r_species)) {
        warning(paste("Could not find 'Species' header in sheet", sheet_name))
        return(tibble())
      }
    }
    
    # Read transect labels from transect row
    tran_row <- as.character(unlist(raw[r_tran, ], use.names = FALSE))
    tran_row <- str_trim(tran_row)
    
    # Find positions of transect labels (pattern: number + letter, e.g., "1d", "2d", "3e", "3A", "15A")
    # Accept both lowercase and uppercase letters
    tran_pos <- which(str_detect(tran_row, "^[0-9]+[a-zA-Z]$"))
    if (length(tran_pos) == 0) {
      warning(paste("No valid transect labels found in sheet", sheet_name))
      return(tibble())
    }
    
    # Read species data (rows below "Species")
    data_start <- r_species + 1
    if (data_start > nrow(raw)) {
      warning(paste("No data rows found in sheet", sheet_name))
      return(tibble())
    }
    
    dat <- raw[data_start:nrow(raw), , drop = FALSE]
    species <- as.character(dat[[1]])
    
    # Remove empty rows and summary rows
    bad_rows <- is.na(species) | species == "" |
      str_detect(str_to_lower(species), "^(total(\\s+sub)?|sum|%|percent|cross|notes)")
    
    dat <- dat[!bad_rows, , drop = FALSE]
    species <- species[!bad_rows]
    
    if (nrow(dat) == 0) {
      warning(paste("No valid species data found in sheet", sheet_name))
      return(tibble())
    }
    
    # Convert data to numeric
    dat_num <- as.data.frame(
      lapply(dat, function(x) suppressWarnings(as.numeric(as.character(x)))),
      stringsAsFactors = FALSE
    )
    
    # Build output: for each transect, extract green and live values
    out <- map_dfr(tran_pos, function(p) {
      tr_label <- str_to_lower(tran_row[p])  # Normalize to lowercase
      
      # Green values are in column p, Live values are in column p+1
      green_col <- p
      live_col <- p + 1
      
      green <- if (green_col <= ncol(dat_num)) dat_num[[green_col]] else rep(0, nrow(dat_num))
      live <- if (live_col <= ncol(dat_num)) dat_num[[live_col]] else rep(0, nrow(dat_num))
      
      # Replace NA with 0
      green <- replace_na(green, 0)
      live <- replace_na(live, 0)
      
      tibble(
        parcel = parcel,
        transect = tr_label,
        species = species,
        green = as.numeric(green),
        live = as.numeric(live),
        hits = green + live,
        year = 2025L
      )
    })
    
    return(out)
  }
  
  # Process all sheets
  result <- map_dfr(sheets, parse_one_sheet, .id = "sheet")
  
  return(result)
}

# Test function on the reference parcels file
if (FALSE) {  # Set to TRUE to run test
  test_data <- read_reference_parcels_excel("data/raw/reference/Reference Parcels_LAW_PLC_2025.xlsx")
  print("Sample of parsed data:")
  print(head(test_data, 20))
  print(paste("Total rows:", nrow(test_data)))
  print("Parcels found:")
  print(unique(test_data$parcel))
  print("Transects per parcel:")
  print(test_data %>% group_by(parcel) %>% summarise(n_transects = n_distinct(transect), .groups = 'drop'))
}
