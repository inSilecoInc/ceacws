# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Shipping density
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ana_shipping_intensity_density <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/shipping_intensity_density-1.0.0/"
  # # dir.create(output_path)
  # input_path <- "workspace/data/harvested/shipping_ais-1.0.0/processed"
  # input_files <- c(
  #   input_path,
  #   "workspace/data/harvested/aoi-1.0.0/processed/aoi.gpkg",
  #   "workspace/data/harvested/aoi-1.0.0/processed/grid.tif"
  # )
  input_files <- unlist(input_files)

  # AOI & grid
  grid_path <- input_files[basename(input_files) == "grid.tif"]
  aoi <- sf::st_read(input_files[basename(input_files) == "aoi.gpkg"], quiet = TRUE)
  input_files <- input_files[!basename(input_files) %in% c("grid.tif", "aoi.gpkg")]

  # Get vessel type
  ship_info <- dir(input_files, pattern = "*csv", full.names = TRUE) |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()

  # Bounding box
  bbox <- sf::st_bbox(aoi)

  # Prepare for parallel processing
  future::plan(future::multisession, workers = parallel::detectCores() - 3)

  # List of Parquet files
  files <- list.files(input_files, pattern = "*.parquet", full.names = TRUE)

  # Process each Parquet file in parallel and write directly to output
  furrr::future_map(files, function(file) {
    # Read data from the Parquet file
    dat <- arrow::read_parquet(file)

    # Apply bbox filter if provided
    dat <- dat |>
      dplyr::filter(
        latitude >= bbox["ymin"], latitude <= bbox["ymax"],
        longitude >= bbox["xmin"], longitude <= bbox["xmax"]
      )

    # Filename
    filename <- basename(file) |>
      tools::file_path_sans_ext() |>
      stringr::str_split("_") |>
      unlist()
    filename <- glue::glue("shipping_intensity_density_{filename[3]}_{filename[4]}")

    # Trackline processing
    create_full_tracklines(dat, group_by_type = TRUE, ship_info = ship_info) |>
      calculate_ship_density(
        grid_path = grid_path,
        output_path = file.path(output_path, filename),
        ntype = "ntype"
      )
  }, .options = furrr::furrr_options(seed = TRUE))
  # plot(log(dat[[1]]), col = viridis::magma(100), maxcell = 100000000)
}

ana_shipping_night_light_density <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/shipping_night_light_intensity_density-1.0.0/"
  # # dir.create(output_path)
  # input_path <- "workspace/data/harvested/shipping_ais-1.0.0/processed"
  # input_files <- c(
  #   input_path,
  #   "workspace/data/harvested/aoi-1.0.0/processed/aoi.gpkg",
  #   "workspace/data/harvested/aoi-1.0.0/processed/grid.tif"
  # )
  input_files <- unlist(input_files)

  # AOI & grid
  grid_path <- input_files[basename(input_files) == "grid.tif"]
  aoi <- sf::st_read(input_files[basename(input_files) == "aoi.gpkg"], quiet = TRUE)
  input_files <- input_files[!basename(input_files) %in% c("grid.tif", "aoi.gpkg")]

  # Get vessel type
  ship_info <- dir(input_files, pattern = "*csv", full.names = TRUE) |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()


  # Bounding box
  bbox <- sf::st_bbox(aoi)

  # Prepare for parallel processing
  future::plan(future::multisession, workers = parallel::detectCores() - 3)

  # List of Parquet files
  files <- list.files(input_files, pattern = "*.parquet", full.names = TRUE)

  # Process each Parquet file in parallel and write directly to output
  furrr::future_map(files, function(file) {
    # Read data from the Parquet file
    dat <- arrow::read_parquet(file)

    # Apply bbox filter if provided
    dat <- dat |>
      dplyr::filter(
        latitude >= bbox["ymin"], latitude <= bbox["ymax"],
        longitude >= bbox["xmin"], longitude <= bbox["xmax"]
      )

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
        grid_path,
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
calculate_ship_density <- function(tracklines, grid_path, output_path = NULL, ntype = NULL) {
  # Get grid
  grid <- terra::rast(grid_path)

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
# Shipping occupancy
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ana_shipping_intensity_occupancy <- function(input_files, output_path) {
  output_path <- "workspace/data/analyzed/shipping_intensity_occupancy-1.0.0/"
  # dir.create(output_path)
  input_path <- "workspace/data/harvested/shipping_ais-1.0.0/processed"
  input_files <- c(
    input_path,
    "workspace/data/harvested/aoi-1.0.0/processed/aoi.gpkg",
    "workspace/data/harvested/aoi-1.0.0/processed/grid.tif"
  )
  input_files <- unlist(input_files)

  # AOI & grid
  grid_path <- input_files[basename(input_files) == "grid.tif"]
  aoi <- sf::st_read(input_files[basename(input_files) == "aoi.gpkg"], quiet = TRUE)
  input_files <- input_files[!basename(input_files) %in% c("grid.tif", "aoi.gpkg")]

  # Get vessel type
  ship_info <- dir(input_files, pattern = "*csv", full.names = TRUE) |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()

  # Bounding box
  bbox <- sf::st_bbox(aoi)

  # Prepare for parallel processing
  future::plan(future::multisession, workers = parallel::detectCores() - 3)

  # List of Parquet files
  files <- list.files(input_files, pattern = "*.parquet", full.names = TRUE)

  # Process each Parquet file in parallel and write directly to output
  furrr::future_map(files, function(file) {
    # Read data from the Parquet file
    dat <- arrow::read_parquet(file)

    # Apply bbox filter if provided
    dat <- dat |>
      dplyr::filter(
        latitude >= bbox["ymin"], latitude <= bbox["ymax"],
        longitude >= bbox["xmin"], longitude <= bbox["xmax"]
      )

    # Filename
    filename <- basename(file) |>
      tools::file_path_sans_ext() |>
      stringr::str_split("_") |>
      unlist()
    filename <- glue::glue("shipping_intensity_occupancy_{filename[3]}_{filename[4]}")

    # Trackline processing
    create_consecutive_tracklines(dat, group_by_type = TRUE, ship_info = ship_info) |>
      calculate_ship_occupancy(
        grid_path = grid_path,
        output_path = file.path(output_path, filename),
        ntype = "ntype"
      )
  }, .options = furrr::furrr_options(seed = TRUE))
  # plot(log(dat[[1]]), col = viridis::magma(100), maxcell = 100000000)
}

ana_shipping_light_occupancy <- function(input_files, output_path) {

}

# ------------------------------
# Functions
# ------------------------------
create_consecutive_tracklines <- function(points, time_filter = "all", group_by_type = FALSE, ship_info = NULL) {
  # Filter points by time of day
  if (time_filter != "all") {
    points <- points |>
      dplyr::filter(day_or_night == time_filter)
  }

  # Create tracklines between consecutive points
  # system.time({
  tracklines <- points |>
    dplyr::group_by(mmsi, track_id) |>
    dplyr::arrange(ymd_hms) |>
    dplyr::mutate(
      next_lon = dplyr::lead(longitude),
      next_lat = dplyr::lead(latitude),
      next_time = dplyr::lead(ymd_hms)
    ) |>
    dplyr::filter(!is.na(next_lon) & !is.na(next_lat) & !is.na(time_diff)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      geometry = construct_wkt_lines(longitude, latitude, next_lon, next_lat) |>
        sf::st_as_sfc(crs = 4326),
      time_diff_hours = as.numeric(difftime(next_time, ymd_hms, units = "hours"))
    ) |>
    sf::st_as_sf() |>
    dplyr::mutate(
      segment_length_km = units::set_units(sf::st_length(geometry), "km")
    ) |>
    dplyr::select(mmsi, date, time_diff_hours, segment_length_km) |>
    dplyr::filter(time_diff_hours > 0)
  # })

  if (group_by_type) {
    tracklines <- dplyr::left_join(
      tracklines,
      ship_info[, c("mmsi", "ntype")],
      by = "mmsi"
    ) |>
      dplyr::relocate(mmsi, ntype)
  }


  return(tracklines)
}


Rcpp::cppFunction('
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector construct_wkt_lines(NumericVector lon, NumericVector lat,
                                     NumericVector lon_next, NumericVector lat_next) {
  int n = lon.size();
  CharacterVector wkts(n);

  for (int i = 0; i < n; ++i) {
    if (NumericVector::is_na(lon[i]) || NumericVector::is_na(lat[i]) ||
        NumericVector::is_na(lon_next[i]) || NumericVector::is_na(lat_next[i])) {
      wkts[i] = NA_STRING; // Handle missing values
    } else {
      std::ostringstream oss;
      oss << "LINESTRING("
          << lon[i] << " " << lat[i] << ", "
          << lon_next[i] << " " << lat_next[i] << ")";
      wkts[i] = oss.str();
    }
  }

  return wkts;
}
')

# ------------------------------------------------------------
# Function to calculate ship occupancy (hours per cell)
calculate_ship_occupancy <- function(tracklines, grid_path, output_path = NULL, ntype = NULL) {
  # Load the grid
  grid <- stars::read_stars(grid_path)

  # Get the raster grid as polygons
  grdp <- sf::st_as_sf(grid, as_points = FALSE, merge = FALSE) |>
    dplyr::mutate(id = seq_len(dplyr::n()))

  # Intersections
  tmp <- file.path(output_path, "tmp")
  process_intersections(
    grdp,
    tracklines,
    grid_chunk_size = 10000,
    trackline_chunk_size = 10000,
    export_path = tmp
  )

  # Load intersection results
  dat <- dir(tmp, full.names = TRUE, pattern = ".parquet") |>
    lapply(arrow::read_parquet) |>
    dplyr::bind_rows()

  # Join to grid
  newgrid <- dplyr::left_join(grdp, dat, by = "id")

  # Aggregate time (in hours) for each grid cell
  grid <- terra::rast(grid_path)
  occupancy <- terra::rasterize(
    x = newgrid,
    y = grid,
    fun = sum,
    field = "time_spent",
    by = ntype
  )

  # Write the raster to file
  if (!is.null(output_path)) {
    if (!is.null(ntype)) {
      # Write each raster to file for different vessel types
      ntypes <- names(occupancy)
      purrr::walk2(
        terra::split(occupancy, ntypes),
        ntypes,
        ~ terra::writeRaster(
          .x,
          filename = glue::glue(
            "{output_path}_{tolower(stringr::str_replace_all(.y, pattern = ' |/', replacement = '_'))}.tif"
          ),
          overwrite = TRUE,
          gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
        )
      )
    } else {
      # Write the combined raster to file
      terra::writeRaster(
        occupancy,
        filename = glue::glue("{output_path}_all_vessels.tif"),
        overwrite = TRUE,
        gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
      )
    }
  }
}


# Function to process intersections in chunks and save incrementally
process_intersections <- function(grid, tracklines, grid_chunk_size = 100000, trackline_chunk_size = 100000, export_path) {
  # Function to split an sf object into chunks
  split_sf <- function(sf_object, chunk_size) {
    split(sf_object, ceiling(seq_len(nrow(sf_object)) / chunk_size))
  }

  # Create an output directory if not exists
  dir.create(export_path, showWarnings = FALSE)

  # Split the grid into chunks
  grid_chunks <- split_sf(grid, grid_chunk_size)

  # Split the tracklines into chunks
  trackline_chunks <- split_sf(tracklines, trackline_chunk_size)

  # Initialize a chunk counter for filenames
  chunk_counter <- 1

  # Process each grid chunk
  for (i in seq_along(grid_chunks)) {
    cat("Processing grid chunk", i, "of", length(grid_chunks), "\n")

    # Subset the current grid chunk
    grid_chunk <- grid_chunks[[i]]

    # Process each trackline chunk
    for (j in seq_along(trackline_chunks)) {
      cat("  Processing trackline chunk", j, "of", length(trackline_chunks), "\n")

      # Subset the current trackline chunk
      trackline_chunk <- trackline_chunks[[j]]

      # Identify intersecting grid cells
      grd_uid <- sf::st_intersects(trackline_chunk, grid_chunk) |>
        unlist() |>
        unique()

      # If no intersecting cells, skip this trackline chunk
      if (length(grd_uid) == 0) next

      # Run intersections
      int <- sf::st_intersection(grid_chunk[grd_uid, ], trackline_chunk) |>
        dplyr::mutate(
          cut_segment_km = units::set_units(sf::st_length(geometry), "km"),
          pct_seg = cut_segment_km / segment_length_km,
          time_spent = time_diff_hours * pct_seg
        ) |>
        sf::st_drop_geometry() |>
        dplyr::group_by(id, ntype) |>
        dplyr::summarise(time_spent = as.numeric(sum(time_spent)), .groups = "drop")

      # Write results to a Parquet file (incremental append)
      chunk_path <- file.path(
        export_path,
        sprintf("%s_chunk_%04d.parquet", gsub("\\.parquet$", "", "occupancy"), chunk_counter)
      )
      arrow::write_parquet(int, chunk_path)

      # Increment the chunk counter
      chunk_counter <- chunk_counter + 1
    }
  }
}
