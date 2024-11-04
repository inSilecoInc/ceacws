prc_neec <- function(input_files, output_path) {
  input_files <- unlist(input_files)
  # NEEC
  neec <- input_files[stringr::str_detect(input_files, "neec.csv")] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::filter(!dplyr::if_all(dplyr::everything(), ~ is.na(.x)))

  # Substances
  substances <- input_files[stringr::str_detect(input_files, "sheen_properties.csv")] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(
      chemical_state = tolower(chemical_state),
      oil_sheen = tolower(oil_sheen)
    )

  # Export
  vroom::vroom_write(neec, file.path(output_path, "neec.csv"), delim = ",")
  vroom::vroom_write(substances, file.path(output_path, "substances.csv"), delim = ",")
}
