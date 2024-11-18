prc_offshore_wind_nfl <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/offshore_wind_nfl-1.0.0/processed/"
  # dir.create(output_path)
  # input_path <- "workspace/data/harvested/offshore_wind_nfl-1.0.0/raw/"
  # input_files <- file.path(input_path, "focus_area_nl_shapefile.zip")

  input_files <- unlist(input_files)

  # Data
  tmp <- file.path(output_path, "tmp")

  ## NS pdfa
  input_files[stringr::str_detect(input_files, "focus_area_nl_shapefile.zip")] |>
    archive::archive_extract(tmp)
  nfl <- sf::st_read(file.path(tmp, "RA_OSW_NL_Focus_Area_Le_Secteur_dintervention_ER_TNL.shp"), quiet = TRUE) |>
    sf::st_transform(4326) |>
    sf::st_zm() |>
    dplyr::mutate(
      dataset = "wind_nfl",
      uid = sprintf("wind_nfl_%04d", dplyr::row_number())
    ) |>
    dplyr::select(dataset, uid)


  # Export
  sf::st_write(
    nfl,
    dsn = file.path(output_path, "offshore_wind_nfl.gpkg"),
    quiet = TRUE,
    delete_dsn = TRUE
  )

  # Remove unzipped files
  fs::dir_delete(tmp)
}
