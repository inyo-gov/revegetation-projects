# Parse LADWP 1991 EIR revegetation master workbook (longitudinal cover + compliance)

library(readxl)
library(dplyr)
library(readr)
library(tibble)
library(purrr)

REVEG1999_MASTER_SHEETS <- c(
  "IND105", "IND123", "IND131N", "IND131S",
  "BLK016E", "TIN054", "BIS097", "BGP160", "BGP160W", "LAW118"
)

REVEG1999_MASTER_PARCEL <- c(
  IND105 = "IND105", IND123 = "IND123", IND131N = "IND131N", IND131S = "IND131S",
  BLK016E = "BLK16E", TIN054 = "TIN054", BIS097 = "BIS097",
  BGP160 = "BGP160E", BGP160W = "BGP160W", LAW118 = "LAW118"
)

reveg1999_ci80_margin <- function(values) {
  x <- as.numeric(values)
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 2) {
    return(list(n = n, mean = if (n == 1) x[[1]] else NA_real_, stdev = NA_real_, ci80 = NA_real_))
  }
  m <- mean(x)
  s <- stats::sd(x)
  t90 <- stats::qt(0.9, df = n - 1)
  ci <- t90 * s / sqrt(n)
  list(n = n, mean = m, stdev = s, ci80 = ci)
}

parse_reveg1999_master_sheet <- function(path, sheet) {
  raw <- read_excel(path, sheet = sheet, col_names = FALSE)
  if (nrow(raw) < 2 || ncol(raw) < 3) {
    return(list(transect = tibble(), summary = tibble()))
  }

  years <- suppressWarnings(as.integer(unlist(raw[1, 3:ncol(raw)])))
  year_cols <- which(!is.na(years)) + 2L
  years <- years[!is.na(years)]

  transect_rows <- seq_len(nrow(raw))
  transect_rows <- transect_rows[
    !is.na(raw[[1]][transect_rows]) &
      !trimws(as.character(raw[[1]][transect_rows])) %in% c("", "Average", "Site")
  ]
  transect_rows <- transect_rows[
    !grepl("^(Average|Stdev|CI80|Upper|Target|Below|Species|Site)", raw[[1]][transect_rows], ignore.case = TRUE)
  ]
  transect_rows <- transect_rows[!is.na(raw[[2]][transect_rows])]

  parcel <- unname(REVEG1999_MASTER_PARCEL[[sheet]])

  transect_long <- map_dfr(seq_along(transect_rows), function(i) {
    r <- transect_rows[[i]]
    map_dfr(seq_along(year_cols), function(j) {
      col <- year_cols[[j]]
      val <- suppressWarnings(as.numeric(raw[[col]][[r]]))
      tibble(
        parcel = parcel,
        sheet = sheet,
        transect = as.character(raw[[2]][[r]]),
        survey_year = years[[j]],
        cover_pct = val
      )
    })
  }) |>
    filter(!is.na(cover_pct))

  summary_long <- map_dfr(seq_along(year_cols), function(j) {
    col <- year_cols[[j]]
    yr <- years[[j]]
    vals <- transect_long |> filter(survey_year == yr) |> pull(cover_pct)
    stats <- reveg1999_ci80_margin(vals)
    avg_row <- which(trimws(as.character(raw[[1]])) == "Average")
    stdev_row <- which(trimws(as.character(raw[[1]])) == "Stdev")
    ci_row <- which(trimws(as.character(raw[[1]])) == "CI80")
    upper_row <- which(trimws(as.character(raw[[1]])) == "Upper CI80")
    tc_row <- which(trimws(as.character(raw[[1]])) == "Target Cover")
    cm_row <- which(trimws(as.character(raw[[1]])) == "Target Cover  met?")
    sr_row <- which(trimws(as.character(raw[[1]])) == "Species Richness")
    tsr_row <- which(trimws(as.character(raw[[1]])) == "Target SR")
    sm_row <- which(trimws(as.character(raw[[1]])) == "Target SR met?")

    sheet_mean <- if (length(avg_row)) suppressWarnings(as.numeric(raw[[col]][avg_row[[1]]])) else NA_real_
    sheet_stdev <- if (length(stdev_row)) suppressWarnings(as.numeric(raw[[col]][stdev_row[[1]]])) else NA_real_
    sheet_ci80 <- if (length(ci_row)) suppressWarnings(as.numeric(raw[[col]][ci_row[[1]]])) else NA_real_
    sheet_upper <- if (length(upper_row)) suppressWarnings(as.numeric(raw[[col]][upper_row[[1]]])) else NA_real_
    target_cover <- if (length(tc_row)) suppressWarnings(as.numeric(raw[[col]][tc_row[[1]]])) else NA_real_
    cover_met_sheet <- if (length(cm_row)) suppressWarnings(as.numeric(raw[[col]][cm_row[[1]]])) else NA_real_
    species_richness <- if (length(sr_row)) suppressWarnings(as.numeric(raw[[col]][sr_row[[1]]])) else NA_real_
    target_sr <- if (length(tsr_row)) suppressWarnings(as.numeric(raw[[col]][tsr_row[[1]]])) else NA_real_
    sr_met_sheet <- if (length(sm_row)) suppressWarnings(as.numeric(raw[[col]][sm_row[[1]]])) else NA_real_

    tibble(
      parcel = parcel,
      sheet = sheet,
      survey_year = yr,
      n_transects = stats$n,
      mean_cover_pct = coalesce(sheet_mean, stats$mean),
      stdev_cover_pct = coalesce(sheet_stdev, stats$stdev),
      ci80_margin = coalesce(sheet_ci80, stats$ci80),
      upper_ci80_pct = coalesce(sheet_upper, stats$mean + stats$ci80),
      target_cover_pct = target_cover,
      cover_met = if_else(!is.na(cover_met_sheet), cover_met_sheet >= 1, upper_ci80_pct >= target_cover_pct),
      species_richness = species_richness,
      target_species_richness = target_sr,
      species_richness_met = if_else(!is.na(sr_met_sheet), sr_met_sheet >= 1, species_richness >= target_sr)
    )
  }) |>
    filter(n_transects > 0)

  list(transect = transect_long, summary = summary_long)
}

parse_reveg1999_master_workbook <- function(
    path = "data/raw/LADWP 1991 EIR REVEGETATION DATA-MASTER.xlsm") {
  if (!file.exists(path)) {
    stop("Master workbook not found: ", path)
  }

  sheets <- intersect(REVEG1999_MASTER_SHEETS, excel_sheets(path))
  parsed <- map(sheets, ~ parse_reveg1999_master_sheet(path, .x))
  names(parsed) <- sheets

  list(
    transect_cover = bind_rows(map(parsed, "transect")),
    parcel_summary = bind_rows(map(parsed, "summary"))
  )
}

load_reveg1999_master_data <- function(
    path = "data/raw/LADWP 1991 EIR REVEGETATION DATA-MASTER.xlsm",
    prefer_processed = TRUE,
    processed_dir = "data/processed") {
  cover_path <- file.path(processed_dir, "reveg1999_master_transect_cover.csv")
  summary_path <- file.path(processed_dir, "reveg1999_master_parcel_summary.csv")

  # Prefer committed processed tables when the master workbook is absent or skipped.
  if (
    prefer_processed &&
      file.exists(cover_path) &&
      file.exists(summary_path) &&
      (identical(path, "data/raw/LADWP 1991 EIR REVEGETATION DATA-MASTER.xlsm") ||
        !file.exists(path))
  ) {
    return(list(
      transect_cover = read_csv(cover_path, show_col_types = FALSE),
      parcel_summary = read_csv(summary_path, show_col_types = FALSE),
      source_file = cover_path
    ))
  }

  parse_reveg1999_master_workbook(path)
}

write_reveg1999_master_outputs <- function(
    data,
    out_dir = "data/processed") {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv(data$transect_cover, file.path(out_dir, "reveg1999_master_transect_cover.csv"))
  write_csv(data$parcel_summary, file.path(out_dir, "reveg1999_master_parcel_summary.csv"))
  invisible(data)
}
