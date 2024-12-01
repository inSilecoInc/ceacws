ana_night_light_monthly <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/night_light_monthly-1.0.0/"
  # # dir.create(output_path)
  # input_files <- c(
  #   "workspace/data/harvested/viirs_nighttime_light-1.0.0/processed",
  #   "workspace/data/harvested/aoi-1.0.0/processed/aoi.gpkg",
  #   "workspace/data/harvested/aoi-1.0.0/processed/grid.tif"
  # )
  input_files <- unlist(input_files)

  grid <- terra::rast(input_files[basename(input_files) == "grid.tif"])
  aoi <- sf::st_read(input_files[basename(input_files) == "aoi.gpkg"], quiet = TRUE)
  input_files <- input_files[!basename(input_files) %in% c("grid.tif", "aoi.gpkg")] |>
    dir(full.names = TRUE, pattern = ".tif")

  # Data
  for (i in seq_len(length(input_files))) {
    # Resample
    dat <- terra::rast(input_files[i]) |>
      terra::resample(grid) |>
      terra::mask(aoi)

    # Export
    current_month <- basename(input_files[i]) |>
      tools::file_path_sans_ext() |>
      stringr::str_split("_") |>
      unlist()
    current_month <- lubridate::ymd(paste(current_month[4], current_month[5], "01", sep = "-"))
    output_file <- file.path(output_path, paste0("night_light_", current_month, ".tif"))
    terra::writeRaster(
      dat,
      output_file,
      overwrite = TRUE,
      gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
    )
  }
}
