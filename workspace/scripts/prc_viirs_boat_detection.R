prc_viirs_boat_detection <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/viirs_boat_detection-1.0.0/processed/"
  # # dir.create(output_path)
  # input_path <- "workspace/data/harvested/viirs_boat_detection-1.0.0/raw/"
  # input_files <- input_path

  input_files <- unlist(input_files)

  # Data
  files <- dir(input_files, pattern = "*.csv.gz", full.names = TRUE)

  # Plan for parallel processing with furrr
  future::plan(future::multisession, workers = parallel::detectCores() - 3)

  # Process files in parallel
  dat <- furrr::future_map_dfr(
    files,
    process_vbd_file,
    .options = furrr::furrr_options(seed = TRUE)
  ) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

  # Export
  sf::st_write(
    dat,
    dsn = file.path(output_path, "viirs_boat_detection.gpkg"),
    quiet = TRUE,
    delete_dsn = TRUE
  )
}

process_vbd_file <- function(file) {
  arrow::read_csv_arrow(file) |>
    dplyr::filter(
      QF_Detect == 1, # Retain only high-confidence detections
      Rad_DNB > 0 # Ensure radiance is positive
      # Uncomment the following lines to include SMI and SHI filters
      # SMI > 0.1, # Filter out detections with low Signal-to-Mean Index
      # SHI > 0.75 # Filter out detections with low Signal-to-High Index
    ) |>
    dplyr::select(
      lat = Lat_DNB, lon = Lon_DNB, date = Date_Mscan, radiance = Rad_DNB
    ) |>
    dplyr::mutate(
      date = lubridate::force_tz(date, tzone = "UTC") # Ensure UTC timestamps
    ) |>
    tidyr::drop_na() # Drop rows with missing values
}
