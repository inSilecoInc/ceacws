ana_offshore_petroleum_platform_monthly <- function(input_files, output_path, bbox = c(-80, 40, -40, 70), resolution = 0.01) {
  # output_path <- "workspace/data/analyzed/offshore_petroleum_platform_monthly-1.0.0/"
  # # dir.create(output_path)
  # input_files <- c(
  #   "workspace/data/harvested/viirs_night_fire-1.0.0/processed/viirs_night_fire_nightly.gpkg"
  # )
  input_files <- unlist(input_files)

  # Data
  dat <- sf::st_read(input_files, quiet = TRUE) |>
    dplyr::filter(likely_flare)

  # Create an sf polygon for the bounding box
  bbox <- sf::st_bbox(c(
    xmin = bbox[1], ymin = bbox[2],
    xmax = bbox[3], ymax = bbox[4]
  ), crs = 4326) |>
    sf::st_as_sfc() |>
    sf::st_as_sf()

  # Create grid
  grid <- terra::rast(
    ext = terra::ext(bbox),
    resolution = resolution,
    crs = terra::crs(bbox)
  )

  # Filter data
  dat <- dat[sf::st_within(dat, bbox, sparse = FALSE), ]

  # Group data
  dat <- dat |>
    dplyr::mutate(
      month = lubridate::floor_date(lubridate::ymd_hms(date), unit = "month")
    )

  # Rasterize and export
  # Process data month by month
  dat |>
    dplyr::group_by(month) |>
    dplyr::group_split() |>
    purrr::walk(function(monthly_sf) {
      # Extract the month
      current_month <- unique(monthly_sf$month)

      # Rasterize the data and sum total_heat for the current month
      r <- terra::rasterize(
        monthly_sf,
        grid,
        field = "total_heat",
        fun = sum,
        background = 0,
        na.rm = TRUE
      )

      # Save raster for the current month
      output_file <- file.path(output_path, paste0("offshore_petroleum_platform_", current_month, ".tif"))
      terra::writeRaster(
        r,
        output_file,
        overwrite = TRUE,
        gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
      )
    })
}


ana_offshore_petroleum_platform_annual <- function(input_files, output_path, bbox = c(-80, 40, -40, 70), resolution = 0.01) {
  output_path <- "workspace/data/analyzed/offshore_petroleum_platform_annual-1.0.0/"
  # dir.create(output_path)
  input_files <- c(
    "workspace/data/harvested/viirs_night_fire_annual-1.0.0/processed/viirs_night_fire_annual.gpkg"
  )
  input_files <- unlist(input_files)

  # Data
  dat <- sf::st_read(input_files, quiet = TRUE)

  # Create an sf polygon for the bounding box
  bbox <- sf::st_bbox(c(
    xmin = bbox[1], ymin = bbox[2],
    xmax = bbox[3], ymax = bbox[4]
  ), crs = 4326) |>
    sf::st_as_sfc() |>
    sf::st_as_sf()

  # Create grid
  grid <- terra::rast(
    ext = terra::ext(bbox),
    resolution = resolution,
    crs = terra::crs(bbox)
  )

  # Rasterize and export
  dat |>
    dplyr::group_by(year) |>
    dplyr::group_split() |>
    purrr::walk(function(yearly_sf) {
      # Extract the month
      current_year <- unique(yearly_sf$year)

      # Rasterize the data and sum total_heat for the current month
      r <- terra::rasterize(
        yearly_sf,
        grid,
        field = "avg_temp_k",
        fun = sum,
        background = 0,
        na.rm = TRUE
      )

      # Save raster for the current month
      output_file <- file.path(output_path, paste0("offshore_petroleum_platform_", current_year, ".tif"))
      terra::writeRaster(
        r,
        output_file,
        overwrite = TRUE,
        gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
      )
    })
}
