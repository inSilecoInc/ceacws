prc_nasp <- function(input_files, output_path) {
  # Import
  dat <- vroom::vroom(input_files[[1]], progress = FALSE, show_col_types = FALSE)

  # Apply formatting to make data usable
  colnames(dat) <- tolower(colnames(dat))
  dat <- dat |>
    dplyr::filter(!is.na(latitude)) |>
    unique() |>
    dplyr::mutate(date = lubridate::dmy(date))

  # Transform coordinates and create spatial object
  format_coord <- function(x, type = "latitude") {
    # Basic transformations
    x <- tolower(x) |>
      stringr::str_replace_all(" ", "") |>
      stringr::str_replace_all("w", "") |>
      stringr::str_replace_all("n", "") |>
      stringr::str_replace_all("'", "") |>
      stringr::str_replace_all('"', "") |>
      stringr::str_replace_all("/", "") |>
      stringr::str_replace_all(",", "\\.") |>
      stringr::str_replace_all(":", "") |>
      stringr::str_replace_all("\\.", "") |>
      stringr::str_replace_all(">", "")

    if (type == "latitude") {
      # Transform, the formula is: Dd = DD + MM.MM/60
      dd <- as.numeric(substr(x, 1, 2))
      mm <- as.numeric(glue::glue("{substr(x, 3, 4)}.{substr(x, 5, nchar(x))}")) / 60
      x <- dd + mm
    } else if (type == "longitude") {
      # Some values are > 100. Values < 100 thus all need to start with a 0
      iid <- as.numeric(substr(x, 1, 1)) > 1
      x[iid] <- glue::glue("0{x[iid]}")

      # Transform, the formula is: Dd = DD + MM.MM/60
      dd <- as.numeric(substr(x, 1, 3))
      mm <- as.numeric(glue::glue("{substr(x, 4, 5)}.{substr(x, 6, nchar(x))}")) / 60
      x <- -(dd + mm)
    }

    x
  }

  dat <- dat |>
    dplyr::mutate(
      latitude = format_coord(latitude, "latitude"),
      longitude = format_coord(longitude, "longitude")
    ) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  # Export
  sf::st_write(dat, dsn = file.path(output_path, "nasp.gpkg"), quiet = TRUE, delete_dsn = TRUE)
}
