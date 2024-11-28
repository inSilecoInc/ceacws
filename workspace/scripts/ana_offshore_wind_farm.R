ana_offshore_wind_farm <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/offshore_wind_farm-1.0.0/"
  # # dir.create(output_path)
  # input_files <- c(
  #   "workspace/data/harvested/offshore_wind_can-1.0.0/processed/offshore_wind_can.gpkg",
  #   "workspace/data/harvested/offshore_wind_nfl-1.0.0/processed/offshore_wind_nfl.gpkg",
  #   "workspace/data/harvested/offshore_wind_ns-1.0.0/processed/offshore_wind_ns.gpkg",
  #   "workspace/data/harvested/offshore_wind_usa-1.0.0/processed/offshore_wind_usa.gpkg"
  # )
  input_files <- unlist(input_files)

  grid <- terra::rast(input_files[basename(input_files) == "grid.tif"])
  aoi <- sf::st_read(input_files[basename(input_files) == "aoi.gpkg"], quiet = TRUE)
  input_files <- input_files[!basename(input_files) %in% c("grid.tif", "aoi.gpkg")]

  # Data
  dat <- lapply(input_files, sf::st_read, quiet = TRUE) |>
    dplyr::bind_rows() |>
    sf::st_union() |>
    sf::st_as_sf()

  # AOI & Grid
  dat <- terra::rasterize(
    x = dat,
    y = grid,
    fun = "count",
    field = NA
  )
  dat <- terra::mask(dat, aoi)

  # Export
  terra::writeRaster(
    dat,
    filename = file.path(output_path, "offshore_wind_farm.tif"),
    overwrite = TRUE,
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
  )

  # # Export
  # sf::st_write(
  #   dat,
  #   dsn = file.path(output_path, "offshore_wind_farm.gpkg"),
  #   quiet = TRUE,
  #   delete_dsn = TRUE
  # )
}
