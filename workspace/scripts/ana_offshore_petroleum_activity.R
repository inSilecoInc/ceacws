ana_offshore_petroleum_activity <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/offshore_petroleum_activity-1.0.0/"
  # # dir.create(output_path)
  # input_files <- c(
  #   "workspace/data/harvested/offshore_petroleum_nfl-1.0.0/processed/offshore_petroleum_nfl.gpkg",
  #   "workspace/data/harvested/offshore_petroleum_ns-1.0.0/processed/offshore_petroleum_ns.gpkg"
  # )
  input_files <- unlist(input_files)

  # Data
  dat <- lapply(input_files, sf::st_read, quiet = TRUE) |>
    dplyr::bind_rows() |>
    sf::st_transform(32198) |>
    dplyr::mutate(geom_type = sf::st_geometry_type(geom))
  uid <- dat$geom_type == "POINT"
  pts <- dat[uid, ]
  poly <- dat[!uid, ]

  # Buffered points
  pts <- sf::st_buffer(pts, dist = 500)

  # Combine and transform back to CRS 4326
  dat <- dplyr::bind_rows(pts, poly) |>
    sf::st_transform(4326) |>
    dplyr::select(-geom_type)

  # Export
  sf::st_write(
    dat,
    dsn = file.path(output_path, "offshore_petroleum_activity.gpkg"),
    quiet = TRUE,
    delete_dsn = TRUE
  )
}
