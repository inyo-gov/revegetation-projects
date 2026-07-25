# Parse 2025 field workbook for 1991 EIR / 1999-plan revegetation parcels.
# Raw species hits + survey metadata only — no staff %COV / %COMP / accuracy rows.

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(sf)

REVEG1999PLAN_SKIP_COL0 <- regex(
  paste(
    "^quad$", "^parcel$", "^species$",
    "%\\s*live", "live acres", "standard dev", "coef of var",
    "^accuracy$", "^stdv$", "^cv$", "^acc$",
    "\\[prn\\]", "^total$", "^%cov$", "^%comp$", "^abs\\.acres$",
    sep = "|"
  ),
  ignore_case = TRUE
)

norm_segment_key <- function(x) {
  x |>
    str_trim() |>
    str_replace_all("_", "-") |>
    str_replace("A3\\.20", "A3.2") |>
    str_replace("2\\.\\.2", "2.2")
}

is_species_code <- function(x) {
  s <- str_trim(as.character(x))
  !is.na(s) &
    s != "" &
    str_detect(s, "^[A-Z][A-Z0-9]{1,10}$")
}

is_segment_column <- function(x) {
  s <- norm_segment_key(x)
  !is.na(s) &
    s != "" &
    !str_detect(s, regex("^total$|^%cov$|^%comp$|^abs\\.acres$", ignore_case = TRUE)) &
    !str_detect(s, regex("^abs\\.acres$", ignore_case = TRUE)) &
    str_detect(s, "^[A-Z0-9.]+-[A-Z0-9.]+$|^[0-9]+[ns]-[0-9]+[ns]$")
}

parse_survey_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  if (is.numeric(x) && !is.na(x) && x > 30000 && x < 60000) {
    return(as.Date(x, origin = "1899-12-30"))
  }
  s <- str_trim(as.character(x))
  if (is.na(s) || s == "") {
    return(as.Date(NA))
  }
  if (str_detect(s, "^\\d{5}(\\.\\d+)?$")) {
    return(as.Date(as.numeric(s), origin = "1899-12-30"))
  }
  if (!str_detect(s, "\\d{4}|/\\d{1,2}/|^[0-9]{4}-")) {
    return(as.Date(NA))
  }
  s <- str_replace(s, "/0225$", "/2025")
  d <- suppressWarnings(as.Date(s, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%Y-%m-%d %H:%M:%OS")))
  if (is.na(d) && str_detect(s, "^\\d{1,2}/\\d{1,2}/\\d{4}")) {
    parts <- as.integer(str_split_fixed(s, "/", 3))
    d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", parts[3], parts[1], parts[2])))
  }
  as.Date(d)
}

find_workbook_path <- function(path = NULL) {
  if (!is.null(path) && file.exists(path)) {
    return(normalizePath(path))
  }
  candidates <- c(
    "Re_Veg Parcels 2025 IND_BLK_TIN_BGP_BIS_LAW.xlsx",
    "data/raw/reveg/Re_Veg Parcels 2025 IND_BLK_TIN_BGP_BIS_LAW.xlsx",
    "data/Re_Veg Parcels 2025 IND_BLK_TIN_BGP_BIS_LAW (1).xlsx"
  )
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0) {
    stop(
      "2025 revegetation workbook not found. Expected one of:\n",
      paste("  ", candidates, collapse = "\n")
    )
  }
  normalizePath(found[1])
}

parse_reveg1999plan_sheet <- function(raw, sheet_name) {
  header <- as.character(unlist(raw[1, ], use.names = FALSE))
  seg_idx <- which(vapply(header, is_segment_column, logical(1)))
  if (length(seg_idx) == 0) {
    warning("No segment columns in sheet ", sheet_name)
    return(list(hits = tibble(), metadata = tibble()))
  }
  segment_keys <- norm_segment_key(header[seg_idx])

  quad <- str_trim(as.character(raw[3, 1]))
  parcel_code <- str_trim(as.character(raw[3, 2]))
  if (is.na(quad) || quad == "") {
    quad <- NA_character_
  }
  if (is.na(parcel_code) || parcel_code == "") {
    parcel_code <- sheet_name
  }

  survey_date <- as.Date(NA)

  hit_rows <- list()

  for (i in seq_len(nrow(raw))) {
    col0 <- raw[i, 1]
    col0_chr <- str_trim(as.character(col0))
    species <- raw[i, 3]

    if (!is.na(col0_chr) && col0_chr != "") {
      if (str_detect(col0_chr, REVEG1999PLAN_SKIP_COL0)) {
        next
      }
      d <- parse_survey_date(col0)
      if (!is.na(d) && is.na(survey_date)) {
        survey_date <- d
      }
    }

    if (!is_species_code(species)) {
      next
    }

    vals <- raw[i, seg_idx]
    for (j in seq_along(seg_idx)) {
      hits <- suppressWarnings(as.numeric(vals[[j]]))
      if (is.na(hits)) {
        hits <- 0
      }
      hit_rows[[length(hit_rows) + 1]] <- tibble(
        parcel = sheet_name,
        quad = quad,
        parcel_code = parcel_code,
        segment_key = segment_keys[j],
        species = str_trim(as.character(species)),
        hits = hits
      )
    }
  }

  hits <- if (length(hit_rows) == 0) {
    tibble()
  } else {
    bind_rows(hit_rows)
  }

  metadata <- tibble(
    parcel = sheet_name,
    quad = quad,
    parcel_code = parcel_code,
    survey_date = survey_date,
    n_segment_columns = length(segment_keys),
    n_species_rows = if (nrow(hits) > 0) n_distinct(hits$species) else 0L
  )

  list(hits = hits, metadata = metadata)
}

parse_reveg1999plan_workbook <- function(path = NULL, survey_year = 2025L) {
  path <- find_workbook_path(path)
  sheets <- excel_sheets(path)

  parsed <- lapply(sheets, function(s) {
    raw <- read_excel(path, sheet = s, col_names = FALSE, na = c("", "NA"))
    parse_reveg1999plan_sheet(raw, s)
  })
  names(parsed) <- sheets

  hits <- bind_rows(lapply(parsed, `[[`, "hits")) |>
    mutate(survey_year = survey_year, source_file = basename(path))

  metadata <- bind_rows(lapply(parsed, `[[`, "metadata")) |>
    mutate(survey_year = survey_year, source_file = basename(path))

  list(hits = hits, metadata = metadata, source_file = path)
}

load_transect_denominators <- function(
    path = "data/transect_segment_denominators.csv") {
  read_csv(path, show_col_types = FALSE) |>
    mutate(
      segment_key = norm_segment_key(segment_key),
      across(c(length_m, n_possible_hits, intercept_spacing_m), as.numeric)
    ) |>
    filter(source != "NEEDS_ACQUISITION")
}

join_reveg1999plan_hits <- function(hits, denominators) {
  hits |>
    mutate(segment_key = norm_segment_key(segment_key)) |>
    left_join(
      denominators |>
        select(parcel, segment_key, length_m, n_possible_hits, intercept_spacing_m, source),
      by = c("parcel", "segment_key"),
      suffix = c("", "_denom")
    ) |>
    rename(denominator_source = source) |>
    mutate(
      percent_cover = if_else(
        !is.na(n_possible_hits) & n_possible_hits > 0,
        pmin(100, hits / n_possible_hits * 100),
        NA_real_
      )
    )
}

reveg1999_species_attributes <- function(species_attr) {
  species_attr |>
    select(Code, Lifecycle, WeedyInOV, Provenance) |>
    rename(species = Code) |>
    mutate(
      is_native = !is.na(Provenance) & Provenance == "Native",
      is_weedy = !is.na(WeedyInOV) & WeedyInOV > 0,
      is_perennial = grepl("Perennial", Lifecycle, ignore.case = TRUE),
      counts_for_cover = is_native & !is_weedy & is_perennial
    )
}

reveg1999_hits_for_cover <- function(hits, species_attr) {
  hits |>
    left_join(reveg1999_species_attributes(species_attr), by = "species") |>
    mutate(
      hits_for_cover = if_else(counts_for_cover %in% TRUE, hits, 0L)
    )
}

summarise_reveg1999_segment_cover <- function(hits, species_attr) {
  reveg1999_hits_for_cover(hits, species_attr) |>
    group_by(parcel, segment_key) |>
    summarise(
      total_hits = sum(hits_for_cover, na.rm = TRUE),
      length_m = first(length_m),
      n_possible_hits = first(n_possible_hits),
      segment_cover_pct = if_else(
        !is.na(n_possible_hits) & n_possible_hits > 0,
        pmin(100, sum(hits_for_cover, na.rm = TRUE) / first(n_possible_hits) * 100),
        NA_real_
      ),
      .groups = "drop"
    )
}

load_reveg1999_transect_posts <- function() {
  pts91 <- st_read("data/gis/Revegetation_Transects91.geojson", quiet = TRUE) |>
    st_transform(4326) |>
    mutate(
      parcel = if_else(parcel == "Hines south", "Hines South", parcel),
      station = str_trim(as.character(transct))
    ) |>
    select(parcel, station)

  if (file.exists("data/gis/IND105.shp")) {
    ind105 <- st_read("data/gis/IND105.shp", quiet = TRUE) |>
      st_transform(4326) |>
      mutate(parcel = "IND105", station = str_trim(as.character(transect_p))) |>
      select(parcel, station)
    pts91 <- bind_rows(pts91, ind105)
  }

  blk_path <- "data/gis/blk16e_transects.geojson"
  if (file.exists(blk_path)) {
    blk <- st_read(blk_path, quiet = TRUE) |>
      st_transform(4326) |>
      filter(parcel == "BLK16E") |>
      mutate(station = str_trim(as.character(transect))) |>
      select(parcel, station)
    pts91 <- bind_rows(pts91, blk)
  }

  tin_posts_path <- "data/gis/TIN054_transect_posts.csv"
  if (file.exists(tin_posts_path)) {
    tin <- readr::read_csv(
      tin_posts_path,
      col_types = readr::cols(
        parcel = readr::col_character(),
        station = readr::col_character(),
        lon = readr::col_double(),
        lat = readr::col_double()
      ),
      show_col_types = FALSE
    ) |>
      mutate(
        parcel = "TIN054",
        station = str_trim(as.character(station))
      ) |>
      st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
      select(parcel, station)
    pts91 <- bind_rows(pts91, tin)
  }

  pts91 |>
    distinct(parcel, station, .keep_all = TRUE)
}

build_reveg1999_segment_lines <- function(segment_cover, posts_sf) {
  posts <- posts_sf |>
    st_drop_geometry() |>
    mutate(
      x = st_coordinates(posts_sf)[, 1],
      y = st_coordinates(posts_sf)[, 2]
    ) |>
    select(parcel, station, x, y)

  seg_lines <- segment_cover |>
    filter(!is.na(n_possible_hits)) |>
    mutate(
      segment_beg = vapply(strsplit(segment_key, "-", fixed = TRUE), `[`, character(1), 1),
      segment_end = vapply(strsplit(segment_key, "-", fixed = TRUE), `[`, character(1), 2)
    ) |>
    left_join(
      posts |> rename(segment_beg = station, x_beg = x, y_beg = y),
      by = c("parcel", "segment_beg")
    ) |>
    left_join(
      posts |> rename(segment_end = station, x_end = x, y_end = y),
      by = c("parcel", "segment_end")
    ) |>
    filter(!is.na(x_beg), !is.na(x_end)) |>
    rowwise() |>
    mutate(
      geometry = st_sfc(
        st_linestring(
          matrix(c(x_beg, y_beg, x_end, y_end), ncol = 2, byrow = TRUE),
          dim = "XY"
        ),
        crs = 4326
      ),
      popup_text = paste0(
        "<b>", parcel, " ", segment_key, "</b><br>",
        "Length: ", round(length_m, 1), " m<br>",
        "Hits: ", total_hits, " / ", n_possible_hits, "<br>",
        "Cover: ", round(segment_cover_pct, 1), "%"
      )
    ) |>
    ungroup() |>
    st_as_sf()

  seg_lines
}

build_reveg1999_segment_points <- function(segment_cover, posts_sf) {
  seg <- segment_cover |>
    filter(!is.na(n_possible_hits)) |>
    mutate(
      segment_beg = vapply(strsplit(segment_key, "-", fixed = TRUE), `[`, character(1), 1),
      segment_end = vapply(strsplit(segment_key, "-", fixed = TRUE), `[`, character(1), 2)
    )

  at_station <- bind_rows(
    seg |>
      transmute(
        parcel,
        station = segment_beg,
        segment_key,
        segment_cover_pct,
        total_hits,
        n_possible_hits,
        length_m
      ),
    seg |>
      transmute(
        parcel,
        station = segment_end,
        segment_key,
        segment_cover_pct,
        total_hits,
        n_possible_hits,
        length_m
      )
  ) |>
    group_by(parcel, station) |>
    slice_max(segment_cover_pct, n = 1, with_ties = FALSE) |>
    ungroup()

  posts_sf |>
    inner_join(at_station, by = c("parcel", "station")) |>
    mutate(
      label = station,
      popup_text = paste0(
        "<b>", parcel, " ", station, "</b><br>",
        "Segment: ", segment_key, "<br>",
        "Length: ", round(length_m, 1), " m<br>",
        "Hits: ", total_hits, " / ", n_possible_hits, "<br>",
        "Cover: ", round(segment_cover_pct, 1), "%"
      )
    )
}

load_reveg1999plan_data <- function(
    workbook_path = NULL,
    denominators_path = "data/transect_segment_denominators.csv",
    prefer_processed = TRUE,
    processed_dir = "data/processed") {
  hits_path <- file.path(processed_dir, "reveg1999plan_hits_long.csv")
  meta_path <- file.path(processed_dir, "reveg1999plan_parcel_metadata.csv")
  missing_denoms <- if (file.exists(denominators_path)) {
    read_csv(denominators_path, show_col_types = FALSE) |>
      filter(source == "NEEDS_ACQUISITION")
  } else {
    tibble()
  }

  # Prefer committed processed tables so the site renders without the workbook.
  if (
    prefer_processed &&
      is.null(workbook_path) &&
      file.exists(hits_path) &&
      file.exists(meta_path)
  ) {
    return(list(
      hits = read_csv(hits_path, show_col_types = FALSE),
      metadata = read_csv(meta_path, show_col_types = FALSE),
      missing_denominators = missing_denoms,
      source_file = hits_path
    ))
  }

  parsed <- parse_reveg1999plan_workbook(workbook_path)
  denom <- load_transect_denominators(denominators_path)
  hits_joined <- join_reveg1999plan_hits(parsed$hits, denom)

  list(
    hits = hits_joined,
    metadata = parsed$metadata,
    missing_denominators = missing_denoms,
    source_file = parsed$source_file
  )
}

write_reveg1999plan_outputs <- function(
    data,
    out_dir = "data/processed") {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv(data$hits, file.path(out_dir, "reveg1999plan_hits_long.csv"))
  write_csv(data$metadata, file.path(out_dir, "reveg1999plan_parcel_metadata.csv"))
  invisible(data)
}
