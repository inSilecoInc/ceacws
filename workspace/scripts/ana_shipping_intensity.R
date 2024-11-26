# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Shipping density
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ana_shipping_intensity_density <- function(input_files, output_path, bbox = c(-80, 40, -50, 70)) {
  # output_path <- "workspace/data/analyzed/shipping_intensity_density-1.0.0/"
  # # dir.create(output_path)
  # input_path <- "workspace/data/harvested/shipping_ais-1.0.0/processed"
  # input_files <- input_path
  input_files <- unlist(input_files)

  # Get vessel type
  ship_info <- dir(input_files, pattern = "*csv", full.names = TRUE) |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()

  # Prepare for parallel processing
  future::plan(future::multisession, workers = parallel::detectCores() - 3)

  # List of Parquet files
  files <- list.files(input_files, pattern = "*.parquet", full.names = TRUE)

  # Process each Parquet file in parallel and write directly to output
  furrr::future_map(files, function(file) {
    # Read data from the Parquet file
    dat <- arrow::read_parquet(file)

    # Apply bbox filter if provided
    if (!is.null(bbox)) {
      dat <- dat |>
        dplyr::filter(
          latitude >= bbox[2], latitude <= bbox[4],
          longitude >= bbox[1], longitude <= bbox[3]
        )
    }

    # Filename
    filename <- basename(file) |>
      tools::file_path_sans_ext() |>
      stringr::str_split("_") |>
      unlist()
    filename <- glue::glue("shipping_intensity_density_{filename[3]}_{filename[4]}")

    # Trackline processing
    create_full_tracklines(dat, group_by_type = TRUE, ship_info = ship_info) |>
      calculate_ship_density(
        resolution = 0.01,
        output_path = file.path(output_path, filename),
        ntype = "ntype"
      )
  }, .options = furrr::furrr_options(seed = TRUE))
  # plot(log(dat[[1]]), col = viridis::magma(100), maxcell = 100000000)
}

ana_shipping_night_light_density <- function(input_files, output_path, bbox = c(-80, 40, -50, 70)) {
  # output_path <- "workspace/data/analyzed/shipping_night_light_intensity_density-1.0.0/"
  # # dir.create(output_path)
  # input_path <- "workspace/data/harvested/shipping_ais-1.0.0/processed"
  # input_files <- input_path
  input_files <- unlist(input_files)

  # Get vessel type
  ship_info <- dir(input_files, pattern = "*csv", full.names = TRUE) |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()

  # Prepare for parallel processing
  future::plan(future::multisession, workers = parallel::detectCores() - 3)

  # List of Parquet files
  files <- list.files(input_files, pattern = "*.parquet", full.names = TRUE)

  # Process each Parquet file in parallel and write directly to output
  furrr::future_map(files, function(file) {
    # Read data from the Parquet file
    dat <- arrow::read_parquet(file)

    # Apply bbox filter if provided
    if (!is.null(bbox)) {
      dat <- dat |>
        dplyr::filter(
          latitude >= bbox[2], latitude <= bbox[4],
          longitude >= bbox[1], longitude <= bbox[3]
        )
    }

    # Filename
    filename <- basename(file) |>
      tools::file_path_sans_ext() |>
      stringr::str_split("_") |>
      unlist()
    filename <- glue::glue("shipping_night_light_intensity_density_{filename[3]}_{filename[4]}")

    # Trackline processing
    create_full_tracklines(
      dat,
      time_filter = "night",
      group_by_type = TRUE,
      ship_info = ship_info
    ) |>
      calculate_ship_density(
        resolution = 0.01,
        output_path = file.path(output_path, filename),
        ntype = "ntype"
      )
  }, .options = furrr::furrr_options(seed = TRUE))
  # plot(log(dat[[1]]), col = viridis::magma(100), maxcell = 100000000)
}

# ------------------------------
# Functions
# ------------------------------
create_full_tracklines <- function(points, time_filter = "all", group_by_type = FALSE, ship_info = NULL) {
  # Filter points by time of day
  if (time_filter != "all") {
    points <- points |>
      dplyr::filter(day_or_night == time_filter)
  }

  # Create tracklines
  tracklines <- points |>
    dplyr::group_by(mmsi, track_id) |>
    dplyr::summarise(
      start_time = min(ymd_hms, na.rm = TRUE),
      end_time = max(ymd_hms, na.rm = TRUE),
      month = lubridate::month(min(start_time)),
      year = lubridate::year(min(start_time)),
      avg_sog = mean(sog, na.rm = TRUE),
      max_sog = max(sog, na.rm = TRUE),
      n_pos = dplyr::n(),
      elapsed_hours = sum(time_diff, na.rm = TRUE) / 60, # Convert minutes to hours
      geometry = list(sf::st_linestring(as.matrix(cbind(longitude, latitude)))),
      .groups = "drop"
    ) |>
    dplyr::filter(n_pos > 1) |>
    sf::st_as_sf(crs = 4326) |>
    dplyr::mutate(
      track_length_km = units::set_units(sf::st_length(geometry), "km")
    )

  if (group_by_type) {
    tracklines <- dplyr::left_join(
      tracklines,
      ship_info[, c("mmsi", "ntype")],
      by = "mmsi"
    ) |>
      dplyr::relocate(mmsi, track_id, ntype)
  }

  return(tracklines)
}

# Function to calculate ship density using rasterization
calculate_ship_density <- function(tracklines, output_path = NULL, resolution = 0.01, ntype = NULL) {
  # Create a blank raster with specified resolution (e.g., ~1km² at the equator)
  bbox <- terra::ext(tracklines) # Bounding box of the data
  grid <- terra::rast(
    ext = bbox,
    resolution = resolution,
    crs = terra::crs(tracklines)
  )

  # Rasterize: Count unique MMSI values in each grid cell
  density_raster <- terra::rasterize(
    x = tracklines,
    y = grid,
    fun = "count",
    field = NA,
    by = ntype
  )

  # Extract the number of days in the month from the tracklines
  ndays <- difftime(
    max(tracklines$end_time, na.rm = TRUE),
    min(tracklines$start_time, na.rm = TRUE),
    units = "days"
  ) |>
    as.numeric() |>
    round(0)

  # Normalize by the number of days in the month
  density_raster <- density_raster / ndays

  # Write the raster to file
  if (!is.null(output_path)) {
    if (!is.null(ntype)) {
      # Write each raster to file
      ntypes <- names(density_raster)
      purrr::walk2(
        terra::split(density_raster, ntypes),
        ntypes,
        ~ terra::writeRaster(
          .x,
          filename =
            glue::glue("{output_path}_{tolower(stringr::str_replace_all(.y, pattern = ' |/', replacement = '_'))}.tif"),
          overwrite = TRUE,
          gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
        )
      )
    } else {
      terra::writeRaster(
        density_raster,
        filename = glue::glue("{output_path}_all_vessels.tif"),
        overwrite = TRUE,
        gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
      )
    }
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Shipping density
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ana_shipping_intensity_occupancy <- function(input_files, output_path, bbox = c(-80, 40, -50, 70)) {
  output_path <- "workspace/data/analyzed/shipping_intensity_occupancy-1.0.0/"
  # dir.create(output_path)
  input_path <- "workspace/data/harvested/shipping_ais-1.0.0/processed"
  input_files <- input_path
  input_files <- unlist(input_files)

  # Data
  files <- dir(input_files, pattern = "*.parquet", full.names = TRUE)
  # Points
  dat <- arrow::read_parquet(files[1])

  # bbox filter
  if (!is.null(bbox)) {
    dat <- dat |>
      dplyr::filter(
        latitude >= bbox[2], latitude <= bbox[4],
        longitude >= bbox[1], longitude <= bbox[3]
      )
  }

  # Trackline processing
  system.time({
    dat2 <- create_consecutive_tracklines(dat[1:100000, ])
  }) # |>
  dat3 <- calculate_ship_density(dat2, resolution = 0.01)
  # plot(log(dat2), col = viridis::magma(100))
}

ana_shipping_light_occupancy <- function(input_files, output_path, bbox = c(-80, 40, -50, 70)) {

}

# ------------------------------
# Functions
# ------------------------------
create_consecutive_tracklines <- function(points, time_filter = "all", group_by_type = FALSE) {
  # Filter points by time of day
  if (time_filter != "all") {
    points <- points |>
      dplyr::filter(day_or_night == time_filter)
  }

  # Group points by MMSI, track_id, and optionally boat type
  grouping_vars <- c()

  # Create tracklines between consecutive points
  points <- dat # [1:1000000, ]
  system.time({
    tracklines <- points |>
      dplyr::group_by(mmsi, track_id) |>
      dplyr::arrange(ymd_hms) |>
      dplyr::mutate(
        next_lon = dplyr::lead(longitude),
        next_lat = dplyr::lead(latitude),
        next_time = dplyr::lead(ymd_hms)
      ) |>
      dplyr::filter(!is.na(next_lon) & !is.na(next_lat)) |>
      dplyr::mutate(
        geometry = list(sf::st_linestring(as.matrix(cbind(c(longitude, next_lon), c(latitude, next_lat))))),
        time_diff_hours = as.numeric(difftime(next_time, ymd_hms, units = "hours"))
      ) |>
      dplyr::ungroup() |>
      sf::st_as_sf(crs = 4326) # |>
  })

  dplyr::mutate(
    segment_length_km = units::set_units(sf::st_length(geometry), "km")
  )

  return(tracklines)
}


# points <- dat # [1:1000000, ]
# system.time({
#   tracklines <- points |>
#     dplyr::group_by(mmsi, track_id) |>
#     dplyr::arrange(ymd_hms) |>
#     dplyr::mutate(
#       next_lon = dplyr::lead(longitude),
#       next_lat = dplyr::lead(latitude),
#       next_time = dplyr::lead(ymd_hms)
#     ) |>
#     dplyr::filter(!is.na(next_lon) & !is.na(next_lat)) |>
#     dplyr::ungroup()

#   # Vectorized construction of LINESTRING geometries
#   line_coords <- purrr::pmap(
#     list(tracklines$longitude, tracklines$latitude, tracklines$next_lon, tracklines$next_lat),
#     ~ matrix(c(..1, ..3, ..2, ..4), ncol = 2, byrow = FALSE)
#   )
# })

# # Convert to LINESTRING geometries
# points$geometry <- sf::st_sfc(
#   lapply(line_coords, sf::st_linestring),
#   crs = 4326
# )

# # Calculate additional fields
# points <- points |>
#   dplyr::mutate(
#     time_diff_hours = as.numeric(difftime(next_time, ymd_hms, units = "hours")),
#     segment_length_km = units::set_units(sf::st_length(geometry), "km")
#   )

# # Convert to sf object
# tracklines <- sf::st_as_sf(points, crs = 4326) |>
#   dplyr::select(-next_lon, -next_lat, -next_time)
