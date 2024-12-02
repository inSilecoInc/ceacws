ana_ship_light_detection <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/ship_light_detection-1.0.0/"
  # # dir.create(output_path)
  # input_files <- c(
  #   "workspace/data/harvested/viirs_boat_detection-1.0.0/processed/viirs_boat_detection.gpkg",
  #   "workspace/data/harvested/aoi-1.0.0/processed/aoi.gpkg",
  #   "workspace/data/harvested/aoi-1.0.0/processed/grid.tif"
  # )
  input_files <- unlist(input_files)

  grid <- terra::rast(input_files[basename(input_files) == "grid.tif"])
  aoi <- sf::st_read(input_files[basename(input_files) == "aoi.gpkg"], quiet = TRUE)
  input_files <- input_files[!basename(input_files) %in% c("grid.tif", "aoi.gpkg")]

  # Data
  dat <- sf::st_read(input_files, quiet = TRUE)

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
        field = "radiance",
        fun = sum,
        background = 0,
        na.rm = TRUE
      ) |>
        terra::mask(aoi)

      # Save raster for the current month
      output_file <- file.path(output_path, paste0("ship_light_detection_", current_month, ".tif"))
      terra::writeRaster(
        r,
        output_file,
        overwrite = TRUE,
        gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
      )
    })
}
