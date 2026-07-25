# Laws Type E — NMDS helpers (adapted from stm/code/nmds_functions.R)
# Species hits/cover → parcel–year relative composition → Bray–Curtis → metaMDS

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(janitor)
  library(vegan)
  library(ggplot2)
})

filter_rare_species <- function(comm, min_occ = 2) {
  keep <- colSums(comm > 0) >= min_occ
  comm[, keep, drop = FALSE]
}

run_nmds <- function(comm, k = 2, trymax = 50, seed = 42) {
  set.seed(seed)
  vegan::metaMDS(
    comm,
    distance = "bray",
    k = k,
    trymax = trymax,
    autotransform = FALSE,
    noshare = FALSE
  )
}

#' Build relative community matrix from long table with columns:
#' parcel, year, species, cover (absolute cover or hits proportional within parcel–year)
#' Optional columns role, reference_group are carried into meta (first value per parcel–year).
build_relative_community_laws <- function(long_df, min_total = 0) {
  by_py <- long_df %>%
    filter(!is.na(species), !is.na(cover), cover > 0) %>%
    group_by(parcel, year, species) %>%
    summarise(
      cover = sum(cover, na.rm = TRUE),
      role = dplyr::first(as.character(role)),
      reference_group = dplyr::first(as.character(reference_group)),
      .groups = "drop"
    )

  sample_meta <- by_py %>%
    group_by(parcel, year) %>%
    summarise(
      role = dplyr::first(role),
      reference_group = dplyr::first(reference_group),
      .groups = "drop"
    ) %>%
    mutate(sample = paste(parcel, year, sep = "_"))

  wide <- by_py %>%
    mutate(sample = paste(parcel, year, sep = "_")) %>%
    select(sample, parcel, year, species, cover) %>%
    pivot_wider(names_from = species, values_from = cover, values_fill = 0)

  meta <- wide %>%
    select(sample, parcel, year) %>%
    left_join(sample_meta %>% select(sample, role, reference_group), by = "sample") %>%
    as.data.frame()
  comm_raw <- wide %>%
    select(-sample, -parcel, -year) %>%
    as.data.frame()
  rownames(comm_raw) <- meta$sample

  rs <- rowSums(comm_raw)
  keep <- rs > min_total
  comm_raw <- comm_raw[keep, , drop = FALSE]
  meta <- meta[keep, , drop = FALSE]

  comm_rel <- as.data.frame(sweep(comm_raw, 1, rowSums(comm_raw), "/"))
  comm_rel <- comm_rel[, colSums(comm_rel) > 0, drop = FALSE]
  list(comm = comm_rel, meta = meta, raw = comm_raw)
}

#' Load Laws Type E reveg + reference composition for ordination
load_laws_composition_long <- function(repo_root = ".") {
  root <- normalizePath(repo_root)

  # --- Reveg LAW090/094/095 (hits already as cover units in workbook) ---
  xlsx <- file.path(root, "data/LawsRevegetationData_SummaryTable_2025_ForICWD092525.xlsx")
  reveg_909495 <- read_excel(xlsx, sheet = "Species List Data") %>%
    clean_names() %>%
    transmute(
      parcel = as.character(parcel),
      year = as.numeric(year),
      species = str_trim(as.character(species)),
      cover = as.numeric(cover),
      role = "reveg",
      reference_group = NA_character_
    ) %>%
    filter(year >= 2022, year <= 2025, !is.na(cover), cover > 0)

  # --- Reveg LAW118/129 2025 wide CSV → long (combine as LAW118/129) ---
  raw <- read_csv(
    file.path(root, "data/raw/reveg/LAW118_129_reveg2025_e.csv"),
    col_names = FALSE,
    show_col_types = FALSE
  )
  parcel_names <- as.character(unlist(raw[1, -1]))
  transect_numbers <- as.character(unlist(raw[2, -1]))
  species_block <- raw[4:nrow(raw), ]
  rows <- list()
  for (i in seq_len(nrow(species_block))) {
    sp <- str_trim(as.character(species_block[[i, 1]]))
    for (j in 2:ncol(species_block)) {
      h <- suppressWarnings(as.numeric(species_block[[i, j]]))
      if (!is.na(h) && h > 0) {
        rows[[length(rows) + 1]] <- data.frame(
          parcel = "LAW118/129",
          year = 2025,
          species = sp,
          cover = h,
          role = "reveg",
          reference_group = NA_character_,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  reveg_118 <- bind_rows(rows)

  # --- Reference long CSVs (hits = percent cover at transect) ---
  spp <- read_csv(file.path(root, "data/species.csv"), show_col_types = FALSE)
  ref_paths <- c(
    file.path(root, "data/raw/reference/law090_094_095_reference_parcel_long_format.csv"),
    file.path(root, "data/raw/reference/law118_129_reference_parcel_long_format.csv")
  )
  # 2025 corrected long if present
  corr <- file.path(root, "data/law118_129_reference_parcel_long_format_2025_corrected(in).csv")
  if (file.exists(corr)) {
    ref_paths <- c(ref_paths, corr)
  }

  ref_list <- lapply(ref_paths, function(p) {
    if (!file.exists(p)) {
      return(NULL)
    }
    d <- read_csv(p, show_col_types = FALSE) %>% clean_names()
    # column variants
    hit_col <- if ("hit" %in% names(d)) "hit" else if ("hits" %in% names(d)) "hits" else NA
    if (is.na(hit_col) || !"parcel" %in% names(d) || !"species" %in% names(d) || !"year" %in% names(d)) {
      return(NULL)
    }
    d %>%
      transmute(
        parcel = as.character(parcel),
        year = as.numeric(year),
        species = str_trim(as.character(species)),
        cover = as.numeric(.data[[hit_col]]),
        role = "reference",
        reference_group = NA_character_
      ) %>%
      filter(!is.na(cover), cover > 0)
  })
  ref <- bind_rows(ref_list)

  # attach reference group from summary
  summ <- read_csv(
    file.path(root, "data/processed/reference_parcel_summary.csv"),
    show_col_types = FALSE
  ) %>%
    distinct(parcel, reference_group)
  ref <- ref %>%
    select(-reference_group) %>%
    left_join(summ, by = "parcel")

  bind_rows(reveg_909495, reveg_118, ref) %>%
    mutate(
      role = factor(role, levels = c("reference", "reveg")),
      parcel_label = parcel
    )
}

scores_with_meta <- function(nmds, meta) {
  sc <- as.data.frame(vegan::scores(nmds, display = "sites"))
  sc$sample <- rownames(sc)
  # Prefer cbind when row order matches (metaMDS preserves community row order)
  if (!is.null(meta$sample) && identical(as.character(meta$sample), sc$sample)) {
    dplyr::bind_cols(meta, sc[, c("NMDS1", "NMDS2"), drop = FALSE])
  } else {
    dplyr::left_join(meta, sc, by = "sample")
  }
}
