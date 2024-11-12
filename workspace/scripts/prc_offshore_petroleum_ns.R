prc_offshore_petroleum_ns <- function(input_files, output_path) {
  input_files <- unlist(input_files)

  # Function to load xls files from this source
  load_xls <- function(filepath) {
    ext <- tools::file_ext(filepath)
    suppressMessages({
      # Column names
      if (ext == "xlsx") {
        header_rows <- readxl::read_xlsx(filepath, range = readxl::cell_rows(1:2), col_names = FALSE)
      } else if (ext == "xls") {
        header_rows <- readxl::read_xls(filepath, range = readxl::cell_rows(1:2), col_names = FALSE)
      } else {
        cli::cli_abort("Unsupported file type")
      }

      col_names <- paste(
        ifelse(is.na(header_rows[1, ]), "", header_rows[1, ]),
        ifelse(is.na(header_rows[2, ]), "", header_rows[2, ]),
        sep = "_"
      ) |>
        stringr::str_replace("_$", "") # Remove trailing underscores from combined names

      # Data
      if (ext == "xlsx") {
        dat <- readxl::read_xlsx(filepath, skip = 2, col_names = FALSE)
      } else if (ext == "xls") {
        dat <- readxl::read_xls(filepath, skip = 2, col_names = FALSE)
      }

      # Process
      stats::setNames(dat, col_names) |>
        janitor::clean_names() |>
        dplyr::select(-latitude_d, -m, -s, -longitude_d, -m_2, -s_2) |>
        dplyr::filter(!is.na(latitude_decimal)) |>
        dplyr::slice(-dplyr::n()) |>
        dplyr::mutate(latitude_decimal = as.numeric(latitude_decimal)) |>
        sf::st_as_sf(coords = c("longitude_decimal", "latitude_decimal"), crs = 4267) |>
        sf::st_transform(4326)
    })
  }

  # Data
  ## Call for bids
  tmp <- file.path(output_path, "tmp")
  input_files[stringr::str_detect(input_files, "call_for_bids")] |>
    archive::archive_extract(tmp)
  bids <- sf::st_read(file.path(tmp, "CFB NS22-1 Parcels.shp"), quiet = TRUE) |>
    janitor::clean_names() |>
    dplyr::mutate(type = "Call for bids") |>
    sf::st_transform(4326)

  ## Production licences
  production_licences <- input_files[stringr::str_detect(input_files, "production_licenses")] |>
    load_xls() |>
    dplyr::mutate(type = "Production licence")

  ## Significant discovery areas
  significant_discovery_areas <- input_files[stringr::str_detect(input_files, "significant_discovery_areas")] |>
    load_xls() |>
    dplyr::mutate(type = "Significant discovery areas")

  ## Significant discovery licenses
  significant_discovery_licenses <- input_files[stringr::str_detect(input_files, "significant_discovery_licenses")] |>
    load_xls() |>
    dplyr::mutate(type = "Significant discovery licenses")

  # Bind
  offshore_petroleum_ns <- dplyr::bind_rows(
    bids,
    production_licences,
    significant_discovery_areas,
    significant_discovery_licenses
  )

  # Export
  sf::st_write(
    offshore_petroleum_ns,
    dsn = file.path(output_path, "offshore_petroleum_ns.gpkg"),
    quiet = TRUE,
    delete_dsn = TRUE
  )

  # Remove unzipped files
  fs::dir_delete(tmp)
}
