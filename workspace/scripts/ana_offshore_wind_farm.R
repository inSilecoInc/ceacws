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

  # Data
  dat <- lapply(input_files, sf::st_read, quiet = TRUE) |>
    dplyr::bind_rows() |>
    sf::st_union()

  # Export
  sf::st_write(
    dat,
    dsn = file.path(output_path, "offshore_wind_farm.gpkg"),
    quiet = TRUE,
    delete_dsn = TRUE
  )
}
