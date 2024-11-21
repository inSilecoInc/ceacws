prc_viirs_night_fire <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/viirs_night_fire-1.0.0/processed/"
  # dir.create(output_path)
  # input_path <- "workspace/data/harvested/viirs_night_fire-1.0.0/raw/"
  # input_files <- input_path
  input_files <- unlist(input_files)

  # Data
  files <- dir(input_files, pattern = "*.csv", full.names = TRUE)

  # Plan for parallel processing with furrr
  future::plan(future::multisession, workers = parallel::detectCores() - 3)

  # Process files in parallel
  dat <- furrr::future_map_dfr(
    files,
    process_vnf_file,
    .options = furrr::furrr_options(seed = TRUE)
  ) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

  # Export
  sf::st_write(
    dat,
    dsn = file.path(output_path, "viirs_night_fire.gpkg"),
    quiet = TRUE,
    delete_dsn = TRUE
  )
}


process_vnf_file <- function(file) {
  # Specify the columns to be imported to avoid memory overhead.
  required_columns <- c(
    "QF_Detect", "Lat_GMTCO", "Lon_GMTCO", "Date_Mscan",
    "Rad_DNB", "Rad_M07", "Rad_M08", "Rad_M10", "Rad_M11",
    "Rad_M12", "Rad_M13", "Rad_M14", "Rad_M15", "Rad_M16",
    "QF1_DNB", "QF1_M07", "QF1_M08", "QF1_M10", "QF1_M11",
    "QF1_M12", "QF1_M13", "QF1_M14", "QF1_M15", "QF1_M16",
    "id_iremitter", "Lat_iremitter", "Lon_iremitter",
    "Type_iremitter", "Category_iremitter"
  )

  # Define flags to retain
  retain_flags <- c(
    1, 2, 4, 8, # High radiance thresholds
    16, 32, 64, # Scatterplot outliers, DNB local maxima
    128, 65536, # Cluster detections
    2048, 4096, 8192, 262144, # Tophat filtered
    524288 # M14 local thermal anomaly
  )
  retain_mask <- Reduce(bitwOr, retain_flags)

  # Manually specify a bounding box around Canada
  # Canadian EEZ roughly as (-141, 40, -50, 85)
  bbox <- c(-141, 40, -50, 85)

  # Read the file
  dat <- arrow::read_csv_arrow(file, col_select = dplyr::all_of(required_columns)) |>
    # Ensure QF_Detect is an integer
    dplyr::mutate(QF_Detect = as.integer(QF_Detect)) |>
    # Apply bitwise filtering for quality flags
    dplyr::filter(
      (bitwAnd(QF_Detect, retain_mask) != 0),
      Rad_DNB > 0,
      Date_Mscan != 999999,
      Lat_GMTCO >= bbox[2], Lat_GMTCO <= bbox[4], # Latitude filter
      Lon_GMTCO >= bbox[1], Lon_GMTCO <= bbox[3] # Longitude filter
    ) |>
    dplyr::rename(lat = Lat_GMTCO, lon = Lon_GMTCO, date = Date_Mscan) |>
    janitor::clean_names() |>
    # tidyr::drop_na() |>
    dplyr::mutate(
      date = lubridate::force_tz(lubridate::ymd_hms(date), tzone = "UTC")
    )

  return(dat)
}
