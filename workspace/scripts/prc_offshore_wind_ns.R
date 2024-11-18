prc_offshore_wind_ns <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/offshore_wind_ns-1.0.0/processed/"
  # dir.create(output_path)
  # input_path <- "workspace/data/harvested/offshore_wind_ns-1.0.0/raw/"
  # input_files <- file.path(input_path, "study_area_ns_shapefile.zip")

  input_files <- unlist(input_files)

  # Data
  tmp <- file.path(output_path, "tmp")

  ## NS pdfa
  input_files[stringr::str_detect(input_files, "study_area_ns_shapefile.zip")] |>
    archive::archive_extract(tmp)
  dat <- sf::st_read(file.path(tmp, "Study_Area_NS_NÉ_Zone_détude.shp"), quiet = TRUE) |>
    sf::st_transform(4326) |>
    dplyr::mutate(
      dataset = "wind_ns",
      uid = sprintf("wind_ns_%04d", dplyr::row_number())
    ) |>
    dplyr::select(dataset, uid)


  # Export
  sf::st_write(
    dat,
    dsn = file.path(output_path, "offshore_wind_ns.gpkg"),
    quiet = TRUE,
    delete_dsn = TRUE
  )

  # Remove unzipped files
  fs::dir_delete(tmp)
}
