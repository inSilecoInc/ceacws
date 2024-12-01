ana_offshore_petroleum_platform_monthly <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/offshore_petroleum_platform_monthly-1.0.0/"
  # # dir.create(output_path)
  # input_files <- c(
  #   "workspace/data/harvested/viirs_night_fire-1.0.0/processed/viirs_night_fire_nightly.gpkg",
  #   "workspace/data/harvested/aoi-1.0.0/processed/aoi.gpkg",
  #   "workspace/data/harvested/aoi-1.0.0/processed/grid.tif"
  # )
  input_files <- unlist(input_files)

  grid <- terra::rast(input_files[basename(input_files) == "grid.tif"])
  aoi <- sf::st_read(input_files[basename(input_files) == "aoi.gpkg"], quiet = TRUE)
  input_files <- input_files[!basename(input_files) %in% c("grid.tif", "aoi.gpkg")]

  # Data
  dat <- sf::st_read(input_files, quiet = TRUE) |>
    dplyr::filter(likely_flare)

  # Filter data
  dat <- dat[sf::st_within(dat, aoi, sparse = FALSE), ]

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


ana_offshore_petroleum_platform_annual <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/offshore_petroleum_platform_annual-1.0.0/"
  # # dir.create(output_path)
  # input_files <- c(
  #   "workspace/data/harvested/viirs_night_fire_annual-1.0.0/processed/viirs_night_fire_annual.gpkg",
  #   "workspace/data/harvested/aoi-1.0.0/processed/aoi.gpkg",
  #   "workspace/data/harvested/aoi-1.0.0/processed/grid.tif"
  # )
  input_files <- unlist(input_files)

  grid <- terra::rast(input_files[basename(input_files) == "grid.tif"])
  aoi <- sf::st_read(input_files[basename(input_files) == "aoi.gpkg"], quiet = TRUE)
  input_files <- input_files[!basename(input_files) %in% c("grid.tif", "aoi.gpkg")]


  # Data
  dat <- sf::st_read(input_files, quiet = TRUE)

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
