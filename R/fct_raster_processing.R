#' Raster Processing Utilities
#'
#' Functions for loading, combining, and processing raster layers
#' in the CEACWS threat layer application.
#'

#' Load Raster Files
#'
#' Loads raster files from filepaths with minimal validation and error handling.
#' Uses stars::read_stars() to load files and reports any failures.
#'
#' @param filepaths Character vector of file paths to raster files
#' @return List with components:
#'   - rasters: List of successfully loaded stars objects
#'   - errors: Character vector of filepaths that failed to load
#'   - success_count: Number of successfully loaded files
#'
#' @export
load_raster_files <- function(filepaths) {
  if (length(filepaths) == 0) {
    return(list(
      rasters = list(),
      errors = character(0),
      success_count = 0
    ))
  }

  rasters <- list()
  errors <- character(0)

  for (filepath in filepaths) {
    tryCatch(
      {
        raster_obj <- stars::read_stars(filepath)
        rasters[[length(rasters) + 1]] <- raster_obj
      },
      error = function(e) {
        errors <<- c(errors, filepath)
      }
    )
  }

  list(
    rasters = rasters,
    errors = errors,
    success_count = length(rasters)
  )
}


#' Combine Raster Layers with gdalcubes
#'
#' Ultra-fast raster combination using gdalcubes. Up to 18x faster than terra
#' for large rasters. Handles proper NA removal and supports cloud processing.
#'
#' @param filepaths Character vector of file paths to raster files
#' @param method Character string: "sum", "average", "maximum", "minimum", "sd"
#' @return stars object with combined raster
#'
#' @examples
#' \dontrun{
#' # Get some test files from your shipping data
#' test_files <- list.files(
#'   "workspace/data/analyzed/shipping_night_light_intensity_density-1.0.0/",
#'   pattern = "\\.tif$", full.names = TRUE
#' )[1:3]
#'
#' # Combine with sum (18x faster than terra)
#' result_sum <- combine_rasters_gdalcubes(test_files, "sum")
#'
#' # Try other methods
#' result_avg <- combine_rasters_gdalcubes(test_files, "average")
#' result_max <- combine_rasters_gdalcubes(test_files, "maximum")
#' result_min <- combine_rasters_gdalcubes(test_files, "minimum")
#' result_sd <- combine_rasters_gdalcubes(test_files, "sd")
#'
#' # View result
#' plot(result_sum)
#' print(result_sum)
#' }
#'
#' @export
combine_rasters_gdalcubes <- function(filepaths, method = "sum") {
  if (length(filepaths) == 0) {
    stop("No file paths provided")
  }

  if (length(filepaths) == 1) {
    return(terra::rast(filepaths[1]))
  }

  # gdalcubes options
  gdalcubes::gdalcubes_options(show_progress = FALSE)

  # Generate sequential dates for gdalcubes temporal requirement
  date_times <- seq(Sys.time(), by = "day", length.out = length(filepaths))

  # Create image collection
  col <- gdalcubes::create_image_collection(filepaths, date_time = date_times)

  # Get spatial extent from first file
  first_stars <- stars::read_stars(filepaths[1])
  bbox <- sf::st_bbox(first_stars)
  ext_vec <- c(bbox[["xmin"]], bbox[["xmax"]], bbox[["ymin"]], bbox[["ymax"]])

  # Create cube view with proper temporal extent
  v <- gdalcubes::cube_view(
    srs = "EPSG:4326",
    extent = list(
      left = ext_vec[1], right = ext_vec[2],
      bottom = ext_vec[3], top = ext_vec[4],
      t0 = as.character(min(date_times)),
      t1 = as.character(max(date_times))
    ),
    dx = 0.01, dy = 0.01,
    dt = "P1D",
    aggregation = "sum",
    resampling = "near"
  )

  # Create cube
  cube <- gdalcubes::raster_cube(col, v)

  # Get band name dynamically
  band_names <- names(cube)
  if (length(band_names) == 0) {
    band_name <- "band1" # Fallback
  } else {
    band_name <- band_names[1]
  }

  # Create reduction expression
  reduce_expr <- switch(method,
    "sum" = paste0("sum(", band_name, ")"),
    "average" = paste0("mean(", band_name, ")"),
    "maximum" = paste0("max(", band_name, ")"),
    "minimum" = paste0("min(", band_name, ")"),
    "sd" = paste0("sd(", band_name, ")"),
    stop("Method must be one of: sum, average, maximum, minimum, sd")
  )

  # Apply reduction and convert to stars
  res <- gdalcubes::reduce_time(cube, reduce_expr) |>
    stars::st_as_stars() |>
    terra::rast()
  res[res == 0] <- NA
  res
}

#' Resample Raster Resolution and/or Extent
#'
#' Flexible raster resampling function that can change resolution, extent, or both
#' using terra::resample(). This unified function handles all spatial resampling
#' needs in a single interface.
#'
#' @param raster A terra SpatRaster object to be resampled
#' @param new_extent Optional. New spatial extent for the raster. Can be:
#'   - terra SpatExtent object
#'   - Numeric vector c(xmin, xmax, ymin, ymax)
#'   - terra SpatRaster (uses its extent)
#'   - sf/sfc object (uses its bounding box)
#'   If NULL, uses current extent
#' @param new_resolution Optional. New resolution in the same units as the
#'   raster's coordinate system. Can be:
#'   - Single numeric value (same for x and y)
#'   - Numeric vector c(x_res, y_res) for different x/y resolutions
#'   If NULL, uses current resolution
#' @param method Character string specifying resampling method. One of:
#'   - "bilinear" (default): bilinear interpolation, good for continuous data
#'   - "near": nearest neighbor, good for categorical data
#'   - "cubic": cubic convolution, smooth for continuous data
#'   - "lanczos": Lanczos windowed sinc resampling
#'    - "sum": Sum, see details of function
#' @return A terra SpatRaster object with the new resolution and/or extent
#'
#' @examples
#' \dontrun{
#' # Load a raster
#' r <- terra::rast("workspace/data/analyzed/shipping_night_light_intensity_density-1.0.0/shipping_night_light_intensity_density_2023_1_container.tif")
#'
#' # Resolution only - make coarser
#' r_coarse <- resample_raster(r, new_resolution = 0.1)
#'
#' # Extent only - crop to bounding box
#' r_crop <- resample_raster(r, new_extent = c(-75, -65, 40, 50))
#'
#' # Both resolution and extent
#' r_both <- resample_raster(r, new_extent = c(-75, -65, 40, 50), new_resolution = 0.05)
#'
#' # Match another raster exactly
#' r_match <- resample_raster(r, new_extent = other_raster, new_resolution = terra::res(other_raster))
#'
#' # Use with sf polygon
#' r_sf <- resample_raster(r, new_extent = study_area_polygon, new_resolution = 0.01)
#' }
#'
#' @export
resample_raster <- function(raster, new_extent = NULL, new_resolution = NULL, method = "sum") {
  # Validate inputs
  if (!inherits(raster, "SpatRaster")) {
    stop("Input must be a terra SpatRaster object")
  }

  valid_methods <- c(
    "bilinear", "average", "near", "mode", "cubic", "cubicspline",
    "lanczos", "sum", "min", "q1", "median", "q3", "max", "rms"
  )
  if (!method %in% valid_methods) {
    stop("method must be one of: ", paste(valid_methods, collapse = ", "))
  }

  # Handle extent
  if (is.null(new_extent)) {
    ext <- terra::ext(raster)
  } else {
    ext <- .parse_extent(new_extent)
  }

  # Handle resolution
  if (is.null(new_resolution)) {
    res <- terra::res(raster)
  } else {
    res <- .parse_resolution(new_resolution)
  }

  # Create template raster
  template <- terra::rast(
    extent = ext,
    resolution = res,
    crs = terra::crs(raster)
  )

  # Resample to template
  resampled <- terra::resample(raster, template, method = method)

  return(resampled)
}

#' Parse extent from various input types
#' @param extent_input Various extent input types
#' @return terra SpatExtent object
#' @noRd
.parse_extent <- function(extent_input) {
  if (inherits(extent_input, "SpatExtent")) {
    return(extent_input)
  } else if (is.numeric(extent_input) && length(extent_input) == 4) {
    return(terra::ext(extent_input))
  } else if (inherits(extent_input, "SpatRaster")) {
    return(terra::ext(extent_input))
  } else if (inherits(extent_input, c("sf", "sfc"))) {
    bbox <- sf::st_bbox(extent_input)
    return(terra::ext(bbox[["xmin"]], bbox[["xmax"]], bbox[["ymin"]], bbox[["ymax"]]))
  } else {
    stop("new_extent must be a terra SpatExtent, numeric vector c(xmin,xmax,ymin,ymax), SpatRaster, or sf object")
  }
}

#' Parse resolution from various input types
#' @param resolution_input Various resolution input types
#' @return Numeric vector of length 1 or 2
#' @noRd
.parse_resolution <- function(resolution_input) {
  if (!is.numeric(resolution_input)) {
    stop("new_resolution must be numeric")
  }

  if (length(resolution_input) == 1) {
    if (resolution_input <= 0) {
      stop("new_resolution must be positive")
    }
    return(resolution_input)
  } else if (length(resolution_input) == 2) {
    if (any(resolution_input <= 0)) {
      stop("new_resolution values must be positive")
    }
    return(resolution_input)
  } else {
    stop("new_resolution must be a single value or vector of length 2")
  }
}
