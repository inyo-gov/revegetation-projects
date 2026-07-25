# Static and interactive maps + 1999-plan goal assessment for reveg1999plan.qmd

library(dplyr)
library(sf)
library(tibble)
library(ggplot2)

REVEG1999_PARCEL_SITES <- tibble::tribble(
  ~parcel,     ~site_name,
  "IND105",    "Independence 105 Revegetation",
  "IND123",    "Independence 123 Revegetation",
  "IND131N",   "Independence 131 North Revegetation",
  "IND131S",   "Independence 131 South Revegetation",
  "BLK16E",    "Blackrock 16E Revegetation",
  "TIN054",    "Tinemaha 54 Revegetation",
  "BIS097",    "Bishop Area Revegetation",
  "BGP160E",   "Big Pine Area Revegetation, 160 Acres",
  "BGP160W",   "Big Pine Area Revegetation, 20 Acres",
  "LAW118",    "Laws 129/118 Irrigated Revegetation"
)

# Table 2, 2021-22 Mitigation Projects and Status (1999 Revegetation Plan)
REVEG1999_PARCEL_GOALS <- tibble::tribble(
  ~parcel,     ~cover_goal_pct, ~species_composition_goal, ~goal_label,
  "IND105",    17.0,            4L,                        "Independence 105",
  "IND123",    17.0,            4L,                        "Independence 123",
  "IND131N",   17.0,            4L,                        "Independence 131N",
  "IND131S",   17.0,            4L,                        "Independence 131S",
  "BLK16E",    34.0,            6L,                        "Blackrock 16E",
  "TIN054",    33.0,            3L,                        "Tinemaha 54",
  "BIS097",    15.0,            12L,                       "Bishop Area 120",
  "BGP160E",   17.7,            10L,                       "Big Pine 160",
  "BGP160W",   17.7,            10L,                       "Big Pine 160",
  "LAW118",    11.5,            11L,                       "Laws 118"
)

# LADWP compliance uses 90% of Table 2 cover goal and 75% of species goal
REVEG1999_COVER_GOAL_FACTOR <- 0.9
REVEG1999_SPECIES_GOAL_FACTOR <- 0.75

REVEG1999_MITIGATION_VIEW_PATH <- "data/gis/mitigation_view_polygons.geojson"

# PCL codes in mitigation_view_polygons.geojson -> 1999-plan parcel IDs (LAW118 only)
REVEG1999_VIEW_POLYGON_PCL <- c(
  LAW118 = "LAW118"
)

# Named AGOL polygons in mitigation_sites.shp (no transect buffering)
REVEG1999_MITIGATION_SITES_PARCELS <- c(
  "IND105", "IND123", "IND131N", "IND131S", "BLK16E", "BGP160E", "BGP160W", "BIS097"
)
REVEG1999_MITIGATION_SITES_PATH <- "data/gis/mit_points/mitigation_sites.shp"

REVEG1999_TIN054_BOUNDARY_PATH <- "data/gis/TIN054.json"
REVEG1999_TIN054_POSTS_PATH <- "data/gis/TIN054_transect_posts.csv"

reveg1999_parcel_site_name <- function(parcel_id) {
  REVEG1999_PARCEL_SITES$site_name[
    match(parcel_id, REVEG1999_PARCEL_SITES$parcel)
  ]
}

load_reveg1999_mitigation_view <- function() {
  if (!file.exists(REVEG1999_MITIGATION_VIEW_PATH)) {
    return(NULL)
  }
  st_read(REVEG1999_MITIGATION_VIEW_PATH, quiet = TRUE) |>
    st_transform(4326) |>
    st_zm(drop = TRUE, what = "ZM")
}

boundary_from_mitigation_view <- function(poly_sf, parcel_id, source_label) {
  if (is.null(poly_sf) || nrow(poly_sf) == 0) {
    return(NULL)
  }

  site_name <- reveg1999_parcel_site_name(parcel_id)
  acres_val <- if ("acres" %in% names(poly_sf)) {
    sum(as.numeric(poly_sf$acres), na.rm = TRUE)
  } else {
    as.numeric(st_area(st_transform(st_union(poly_sf), 26911))) / 4046.86
  }

  poly_sf |>
    summarise(
      parcel = parcel_id,
      site_name = site_name,
      acres = acres_val,
      boundary_source = source_label,
      geometry = st_union(geometry),
      .groups = "drop"
    ) |>
    st_as_sf()
}

load_mitigation_site_boundaries <- function(parcel_ids = REVEG1999_MITIGATION_SITES_PARCELS) {
  if (!file.exists(REVEG1999_MITIGATION_SITES_PATH)) {
    return(NULL)
  }

  ms <- st_read(REVEG1999_MITIGATION_SITES_PATH, quiet = TRUE) |>
    st_transform(4326) |>
    st_zm(drop = TRUE, what = "ZM")

  site_rows <- REVEG1999_PARCEL_SITES |>
    filter(.data$parcel %in% parcel_ids)

  matched <- ms |>
    filter(.data$Name %in% site_rows$site_name) |>
    mutate(
      parcel = site_rows$parcel[match(.data$Name, site_rows$site_name)]
    )

  if (nrow(matched) == 0) {
    return(NULL)
  }

  matched |>
    group_by(.data$parcel) |>
    summarise(
      site_name = first(.data$Name),
      acres = sum(as.numeric(.data$Acres), na.rm = TRUE),
      boundary_source = REVEG1999_MITIGATION_SITES_PATH,
      geometry = st_union(.data$geometry),
      .groups = "drop"
    ) |>
    st_as_sf()
}

load_reveg1999_parcel_boundaries_from_view <- function(mitigation_view) {
  bound <- lapply(names(REVEG1999_VIEW_POLYGON_PCL), function(parcel_id) {
    if (parcel_id %in% c("TIN054", REVEG1999_MITIGATION_SITES_PARCELS)) {
      return(NULL)
    }

    pcl <- REVEG1999_VIEW_POLYGON_PCL[[parcel_id]]
    poly <- mitigation_view |> filter(.data$PCL == pcl)
    if (nrow(poly) == 0) {
      return(NULL)
    }

    boundary_from_mitigation_view(poly, parcel_id, REVEG1999_MITIGATION_VIEW_PATH)
  })

  bound <- bound[!vapply(bound, is.null, logical(1))]

  if (length(bound) == 0) {
    return(NULL)
  }
  do.call(rbind, bound)
}

load_reveg1999_tin054_boundary <- function() {
  if (!file.exists(REVEG1999_TIN054_BOUNDARY_PATH)) {
    return(NULL)
  }

  tin_all <- st_read(REVEG1999_TIN054_BOUNDARY_PATH, quiet = TRUE) |>
    st_transform(4326)
  tin_poly <- tin_all |>
    filter(
      if ("annotation.type" %in% names(tin_all)) {
        .data$`annotation.type` == "area"
      } else {
        sf::st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")
      }
    )
  if (nrow(tin_poly) == 0) {
    return(NULL)
  }

  tin_poly |>
    summarise(
      parcel = "TIN054",
      site_name = "Tinemaha 54 experimental plot",
      acres = as.numeric(st_area(st_transform(st_union(geometry), 26911))) / 4046.86,
      boundary_source = REVEG1999_TIN054_BOUNDARY_PATH,
      geometry = st_union(geometry),
      .groups = "drop"
    ) |>
    st_as_sf()
}

load_reveg1999_parcel_boundaries_from_mitigation_sites <- function() {
  if (!file.exists(REVEG1999_MITIGATION_SITES_PATH)) {
    return(NULL)
  }

  sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(TRUE), add = TRUE)

  ms <- st_read(REVEG1999_MITIGATION_SITES_PATH, quiet = TRUE) |>
    st_transform(4326)

  matched <- ms |>
    filter(.data$Name %in% REVEG1999_PARCEL_SITES$site_name) |>
    mutate(
      parcel = REVEG1999_PARCEL_SITES$parcel[
        match(.data$Name, REVEG1999_PARCEL_SITES$site_name)
      ]
    ) |>
    st_zm(drop = TRUE, what = "ZM")

  matched |>
    group_by(parcel) |>
    summarise(
      site_name = first(Name),
      acres = sum(as.numeric(Acres), na.rm = TRUE),
      boundary_source = "mitigation_sites.shp",
      geometry = st_union(geometry),
      .groups = "drop"
    ) |>
    st_as_sf()
}

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
  list(n = n, mean = m, stdev = s, ci80 = t90 * s / sqrt(n))
}

load_reveg1999_parcel_boundaries <- function() {
  empty_boundaries <- st_sf(
    parcel = character(),
    site_name = character(),
    acres = numeric(),
    boundary_source = character(),
    geometry = st_sfc(crs = 4326)
  )

  mitigation_view <- load_reveg1999_mitigation_view()
  bound <- if (!is.null(mitigation_view)) {
    load_reveg1999_parcel_boundaries_from_view(mitigation_view)
  } else {
    NULL
  }

  if (is.null(bound) || nrow(bound) == 0) {
    bound <- empty_boundaries
  }

  tin <- load_reveg1999_tin054_boundary()
  if (!is.null(tin)) {
    bound <- bound |> filter(.data$parcel != "TIN054")
    bound <- rbind(bound, tin)
  }

  mit_sites <- load_mitigation_site_boundaries()
  if (!is.null(mit_sites)) {
    bound <- bound |> filter(!.data$parcel %in% mit_sites$parcel)
    bound <- rbind(bound, mit_sites)
  }

  bound
}

compute_reveg1999_parcel_metrics <- function(hits, species_attr) {
  hits_sp <- reveg1999_hits_for_cover(hits, species_attr)

  seg <- hits_sp |>
    filter(!is.na(n_possible_hits)) |>
    group_by(parcel, segment_key) |>
    summarise(
      n_possible_hits = first(n_possible_hits),
      length_m = first(length_m),
      hits_all = sum(hits, na.rm = TRUE),
      hits_live_native_perennial = sum(hits_for_cover, na.rm = TRUE),
      .groups = "drop"
    )

  species_stats <- hits_sp |>
    filter(!is.na(n_possible_hits)) |>
    group_by(parcel, species) |>
    summarise(
      hits = sum(hits, na.rm = TRUE),
      hits_for_cover = sum(hits_for_cover, na.rm = TRUE),
      counts_for_cover = any(counts_for_cover %in% TRUE),
      .groups = "drop"
    )

  seg_with_cover <- seg |>
    mutate(
      segment_live_native_cover_pct = if_else(
        n_possible_hits > 0,
        pmin(100, hits_live_native_perennial / n_possible_hits * 100),
        NA_real_
      )
    )

  parcel_stats <- seg_with_cover |>
    group_by(parcel) |>
    summarise(
      n_segments = n(),
      total_intercepts = sum(n_possible_hits, na.rm = TRUE),
      total_hits_all = sum(hits_all, na.rm = TRUE),
      total_hits_live_native_perennial = sum(hits_live_native_perennial, na.rm = TRUE),
      live_native_cover_pct = if_else(
        sum(n_possible_hits, na.rm = TRUE) > 0,
        pmin(
          100,
          sum(hits_live_native_perennial, na.rm = TRUE) /
            sum(n_possible_hits, na.rm = TRUE) * 100
        ),
        NA_real_
      ),
      total_cover_pct = if_else(
        sum(n_possible_hits, na.rm = TRUE) > 0,
        pmin(100, sum(hits_all, na.rm = TRUE) / sum(n_possible_hits, na.rm = TRUE) * 100),
        NA_real_
      ),
      mean_segment_length_m = mean(length_m, na.rm = TRUE),
      mean_segment_cover_pct = mean(segment_live_native_cover_pct, na.rm = TRUE),
      stdev_segment_cover_pct = stats::sd(segment_live_native_cover_pct, na.rm = TRUE),
      n_segments_for_ci = sum(!is.na(segment_live_native_cover_pct)),
      .groups = "drop"
    ) |>
    mutate(
      ci80_margin = if_else(
        n_segments_for_ci >= 2,
        stats::qt(0.9, n_segments_for_ci - 1) * stdev_segment_cover_pct / sqrt(n_segments_for_ci),
        NA_real_
      ),
      upper_ci80_pct = if_else(
        !is.na(mean_segment_cover_pct) & !is.na(ci80_margin),
        mean_segment_cover_pct + ci80_margin,
        NA_real_
      )
    )

  parcel_stats |>
    left_join(
      species_stats |>
        group_by(parcel) |>
        summarise(
          n_species = n_distinct(species),
          n_live_native_perennial_species = n_distinct(species[counts_for_cover %in% TRUE]),
          n_species_3plus = n_distinct(species[hits_for_cover >= 3 & (counts_for_cover %in% TRUE)]),
          .groups = "drop"
        ),
      by = "parcel"
    ) |>
    left_join(REVEG1999_PARCEL_GOALS, by = "parcel") |>
    mutate(
      target_cover_pct = cover_goal_pct * REVEG1999_COVER_GOAL_FACTOR,
      target_species_richness = species_composition_goal * REVEG1999_SPECIES_GOAL_FACTOR,
      cover_met = !is.na(upper_ci80_pct) & upper_ci80_pct >= target_cover_pct,
      composition_met = n_species_3plus >= target_species_richness,
      overall_met = cover_met & composition_met
    )
}

fix_parcel_metrics_denominators <- function(metrics, hits) {
  missing <- hits |>
    group_by(parcel) |>
    summarise(
      segments_missing_denom = n_distinct(segment_key[is.na(n_possible_hits)]),
      .groups = "drop"
    )
  metrics |>
    left_join(missing, by = "parcel") |>
    mutate(
      segments_missing_denom = coalesce(segments_missing_denom, 0L),
      assessment_complete = segments_missing_denom == 0L,
      overall_met = if_else(assessment_complete, overall_met, NA)
    )
}

plot_reveg1999_parcel_map <- function(
    parcel,
    boundaries,
    segment_points,
    metrics_row = NULL,
    segment_lines = NULL) {
  boundary <- boundaries |> filter(.data$parcel == .env$parcel)
  points <- segment_points |> filter(.data$parcel == .env$parcel)
  lines <- if (!is.null(segment_lines) && nrow(segment_lines) > 0) {
    segment_lines |> filter(.data$parcel == .env$parcel)
  } else {
    NULL
  }

  if (nrow(boundary) == 0 && nrow(points) == 0) {
    return(
      ggplot() +
        annotate(
          "text", x = 0, y = 0,
          label = paste("No GIS geometry for", parcel),
          size = 4
        ) +
        theme_void()
    )
  }

  has_boundary <- nrow(boundary) > 0
  has_lines <- !is.null(lines) && nrow(lines) > 0

  subtitle <- if (!is.null(metrics_row) && nrow(metrics_row) == 1) {
    paste0(
      "Live native perennial cover: ",
      round(metrics_row$live_native_cover_pct, 1), "% (upper 80% CI ",
      if_else(is.na(metrics_row$upper_ci80_pct), "—", paste0(round(metrics_row$upper_ci80_pct, 1), "%")),
      "; target ", round(metrics_row$target_cover_pct, 1), "%) · ",
      metrics_row$n_species_3plus, " species ≥3 hits (target ",
      metrics_row$target_species_richness, ")"
    )
  } else {
    NULL
  }

  p <- ggplot()
  if (has_boundary) {
    p <- p +
      geom_sf(data = boundary, fill = "#f0f4f8", color = "#4a5568", linewidth = 0.7)
  }
  if (has_lines) {
    p <- p +
      geom_sf(
        data = lines,
        color = "#64748b",
        linewidth = 0.55,
        alpha = 0.85
      )
  }
  p <- p +
    theme_minimal(base_size = 11) +
    labs(
      title = parcel,
      subtitle = subtitle,
      caption = if (has_boundary) {
        "Grey fill = parcel boundary · grey lines = transect segments · colored points = 2025 posts"
      } else {
        "No parcel boundary polygon · grey lines = transect segments · colored points = 2025 posts"
      }
    ) +
    theme(
      panel.grid = element_line(color = "grey90", linewidth = 0.2),
      plot.title = element_text(face = "bold")
    )

  if (nrow(points) > 0) {
    p <- p +
      geom_sf(
        data = points,
        aes(color = segment_cover_pct),
        size = 2.8,
        stroke = 0.25
      ) +
      scale_color_viridis_c(
        name = "Segment\ncover %",
        option = "C",
        limits = c(0, 100),
        na.value = "grey70"
      )
  }

  if (nrow(points) > 0) {
    label_coords <- sf::st_coordinates(points)
    points_labels <- points |>
      mutate(
        label_x = label_coords[, 1],
        label_y = label_coords[, 2]
      )
    p <- p +
      ggrepel::geom_text_repel(
        data = points_labels,
        aes(x = .data$label_x, y = .data$label_y, label = .data$label),
        size = 2.4,
        color = "black",
        max.overlaps = Inf,
        min.segment.length = 0,
        box.padding = 0.15,
        point.padding = 0.25,
        segment.size = 0.2,
        seed = 42,
        inherit.aes = FALSE
      )
  }

  map_crs <- if (has_boundary) sf::st_crs(boundary) else sf::st_crs(points)
  map_geoms <- c(
    if (has_boundary) list(st_geometry(boundary)),
    if (has_lines) list(st_geometry(lines)),
    if (nrow(points) > 0) list(st_geometry(points))
  )
  map_bb <- sf::st_bbox(do.call(c, map_geoms))

  p +
    coord_sf(
      xlim = c(map_bb[["xmin"]], map_bb[["xmax"]]),
      ylim = c(map_bb[["ymin"]], map_bb[["ymax"]]),
      expand = TRUE,
      clip = "off",
      datum = map_crs
    ) +
    theme(plot.margin = margin(5.5, 15, 5.5, 5.5))
}

plot_reveg1999_parcel_cover_history <- function(
    parcel,
    master_summary,
    metrics_row = NULL) {
  goals <- REVEG1999_PARCEL_GOALS |>
    filter(.data$parcel == .env$parcel)

  if (nrow(goals) != 1) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 0, label = paste("No cover goals for", parcel), size = 4) +
        theme_void()
    )
  }

  cover_goal <- goals$cover_goal_pct[[1]]
  target_90 <- cover_goal * REVEG1999_COVER_GOAL_FACTOR

  plot_data <- master_summary |>
    filter(.data$parcel == .env$parcel, !is.na(.data$mean_cover_pct)) |>
    transmute(
      survey_year,
      mean_cover_pct,
      upper_ci80_pct,
      is_2025 = FALSE
    )

  if (!is.null(metrics_row) && nrow(metrics_row) == 1 && isTRUE(metrics_row$assessment_complete)) {
    plot_data <- bind_rows(
      plot_data,
      tibble(
        survey_year = 2025L,
        mean_cover_pct = metrics_row$mean_segment_cover_pct,
        upper_ci80_pct = metrics_row$upper_ci80_pct,
        is_2025 = TRUE
      )
    )
  }

  if (nrow(plot_data) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 0, label = paste("No cover history for", parcel), size = 4) +
        theme_void()
    )
  }

  y_max <- max(
    c(plot_data$upper_ci80_pct, plot_data$mean_cover_pct, cover_goal, target_90),
    na.rm = TRUE
  )

  ggplot(plot_data, aes(x = .data$survey_year, y = .data$mean_cover_pct)) +
    geom_hline(
      yintercept = cover_goal,
      linetype = "dashed",
      color = "grey55",
      linewidth = 0.45
    ) +
    geom_hline(
      yintercept = target_90,
      linetype = "solid",
      color = "#c0392b",
      linewidth = 0.45
    ) +
    geom_linerange(
      aes(ymin = .data$mean_cover_pct, ymax = .data$upper_ci80_pct),
      color = "grey40",
      linewidth = 0.55,
      na.rm = TRUE
    ) +
    geom_line(color = "#2563eb", linewidth = 0.7) +
    geom_point(
      aes(shape = .data$is_2025),
      size = 2.6,
      color = "#2563eb"
    ) +
    scale_shape_manual(values = c(`TRUE` = 17, `FALSE` = 19), guide = "none") +
    scale_x_continuous(breaks = sort(unique(plot_data$survey_year))) +
    coord_cartesian(ylim = c(0, y_max * 1.08), clip = "off") +
    labs(
      title = paste0(parcel, " — cover through time"),
      subtitle = paste0(
        "Mean cover with upper 80% CI\n",
        "Dashed = Table 2 goal (", cover_goal, "%) · red = 90% compliance threshold (",
        round(target_90, 2), "%)"
      ),
      x = "Survey year",
      y = "Mean cover (%)",
      caption = "Historical years from LADWP master workbook; 2025 = live native perennial segment mean."
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold"),
      plot.margin = margin(5.5, 10, 5.5, 5.5)
    )
}

build_reveg1999_overview_map <- function(
    boundaries,
    segment_points,
    metrics,
    segment_lines = NULL) {
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("leaflet package required")
  }

  pal <- leaflet::colorNumeric(
    palette = c("#ffffcc", "#41b6c4", "#253494"),
    domain = segment_points$segment_cover_pct
  )

  metrics <- metrics |>
    mutate(
      popup = paste0(
        "<b>", parcel, "</b><br>",
        "Live native perennial cover: ",
        if_else(is.na(live_native_cover_pct), "—", paste0(round(live_native_cover_pct, 1), "%")),
        " (upper 80% CI ",
        if_else(is.na(upper_ci80_pct), "—", paste0(round(upper_ci80_pct, 1), "%")),
        "; target ", target_cover_pct, "%)<br>",
        "Species ≥3 hits: ", n_species_3plus, " (target ", target_species_richness, ")<br>",
        "Goals met: ",
        if_else(is.na(overall_met), "incomplete data", if_else(overall_met, "Yes", "No"))
      )
    )

  point_coords <- sf::st_coordinates(segment_points)
  segment_points <- segment_points |>
    mutate(
      lon = point_coords[, 1],
      lat = point_coords[, 2]
    )

  boundaries <- boundaries |>
    left_join(metrics |> select(parcel, popup), by = "parcel")

  bb <- sf::st_bbox(boundaries)

  m <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 8)) |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::addPolygons(
      data = boundaries,
      fillColor = "#cbd5e0",
      fillOpacity = 0.35,
      color = "#2d3748",
      weight = 2,
      popup = ~popup
    )

  if (!is.null(segment_lines) && nrow(segment_lines) > 0) {
    m <- m |>
      leaflet::addPolylines(
        data = segment_lines,
        color = "#475569",
        weight = 2,
        opacity = 0.85,
        popup = ~popup_text
      )
  }

  m |>
    leaflet::addCircleMarkers(
      data = segment_points,
      lng = ~lon,
      lat = ~lat,
      radius = 4,
      stroke = TRUE,
      weight = 1,
      fillOpacity = 0.9,
      color = ~pal(segment_cover_pct),
      fillColor = ~pal(segment_cover_pct),
      popup = ~popup_text
    ) |>
    leaflet::addLegend(
      "bottomright",
      pal = pal,
      values = segment_points$segment_cover_pct,
      title = "Segment cover %"
    ) |>
    leaflet::fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
}
