prc_offshore_wind_can <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/offshore_wind_can-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/offshore_wind_can-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "NS_pfda.zip",
  #     "NFL_pla.zip"
  #   )
  # )

  input_files <- unlist(input_files)

  # Data
  tmp <- file.path(output_path, "tmp")

  ## NS pdfa
  input_files[stringr::str_detect(input_files, "NS_pfda.zip")] |>
    archive::archive_extract(tmp)
  ns <- sf::st_read(file.path(tmp, "NS_pfda/Wind_Areas_Feb29_amalgamated.shp"), quiet = TRUE) |>
    sf::st_transform(4326) |>
    dplyr::mutate(
      dataset = "wind_can_ns",
      uid = sprintf("wind_can_ns_%04d", dplyr::row_number())
    ) |>
    dplyr::select(dataset, uid)

  ## NFL pla
  input_files[stringr::str_detect(input_files, "NFL_pla.zip")] |>
    archive::archive_extract(tmp)
  nfl <- sf::st_read(file.path(tmp, "NFL_pla/osw_preliminary_areas_eee_zones_preliminaires.shp"), quiet = TRUE) |>
    sf::st_transform(4326) |>
    sf::st_zm() |>
    dplyr::mutate(
      dataset = "wind_can_nfl",
      uid = sprintf("wind_can_nfl_%04d", dplyr::row_number())
    ) |>
    dplyr::select(dataset, uid)


  # Bind
  wind_can <- dplyr::bind_rows(ns, nfl) |>
    dplyr::mutate(classification = "planning area")

  # Export
  sf::st_write(
    wind_can,
    dsn = file.path(output_path, "offshore_wind_can.gpkg"),
    quiet = TRUE,
    delete_dsn = TRUE
  )

  # Remove unzipped files
  fs::dir_delete(tmp)
}
