prc_neec <- function(input_files, output_path) {
  input_files <- c(
    "/Users/davidbeauchesne/Desktop/data/neec-1.0.0/neec.csv",
    "/Users/davidbeauchesne/Desktop/data/neec-1.0.0/neec_taxonomy.csv"
  )

  # NEEC
  neec <- input_files[stringr::str_detect(input_files, "neec.csv")] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::filter(!dplyr::if_all(dplyr::everything(), ~ is.na(.x)))

  # Taxonomy
  taxonomy <- input_files[stringr::str_detect(input_files, "neec_taxonomy.csv")] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()

  # Export
  vroom::vroom_write(neec, file.path(output_path, "neec.csv"), delim = ",")
  vroom::vroom_write(taxonomy, file.path(output_path, "neec_taxonomy.csv"), delim = ",")
}
