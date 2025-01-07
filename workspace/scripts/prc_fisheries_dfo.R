prc_fisheries_dfo <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/fisheries_dfo-1.0.0/processed/"
  # # dir.create(output_path)
  # input_path <- "workspace/data/harvested/fisheries_dfo-1.0.0/raw/"
  # input_files <- list(
  #   file.path(input_path, "Codes_engin.csv"),
  #   file.path(input_path, "Codes_especes.csv"),
  #   file.path(input_path, "Version_totale_20102014.csv"),
  #   file.path(input_path, "Version_totale_20002004.csv"),
  #   file.path(input_path, "Version_totale_20152019.csv"),
  #   file.path(input_path, "Version_totale_20052009.csv"),
  #   file.path(input_path, "Version_totale_20202024.csv")
  # )
  input_files <- unlist(input_files)

  # Gear
  input_files[stringr::str_detect(input_files, "Codes_engin.csv")] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names() |>
    vroom::vroom_write(file.path(output_path, "gear.csv"), delim = ",")


  # Species
  input_files[stringr::str_detect(input_files, "Codes_especes.csv")] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names() |>
    vroom::vroom_write(file.path(output_path, "species.csv"), delim = ",")

  # Logbooks
  input_files[stringr::str_detect(input_files, "Version_totale")][1] |>
    lapply(function(x) {
      dat <- vroom::vroom(x, progress = FALSE, show_col_types = FALSE) |>
        janitor::clean_names() |>
        dplyr::rename(longitude = longit_gis, latitude = latit_gis) |>
        dplyr::filter(!is.na(latitude) & !is.na(longitude)) |>
        dplyr::mutate(
          date_cap = lubridate::as_date(date_cap),
          date_deb = lubridate::as_date(date_deb),
          date = dplyr::if_else(is.na(date_cap), date_deb, date_cap),
          year = lubridate::year(date),
          weight_kg = dplyr::if_else(un_mes == "P", pd_deb * 0.453592, pd_deb)
        )
    }) |>
    dplyr::bind_rows() |>
    dplyr::distinct() |>
    arrow::write_parquet(file.path(output_path, "logbooks.parquet"))
}
