prc_viirs_night_fire_nightly <- function(input_files, output_path) {
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
    process_vnf_nightly_file,
    .options = furrr::furrr_options(seed = TRUE)
  ) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

  # Export
  sf::st_write(
    dat,
    dsn = file.path(output_path, "viirs_night_fire_nightly.gpkg"),
    quiet = TRUE,
    delete_dsn = TRUE
  )
}


process_vnf_nightly_file <- function(file) {
  # Specify the columns to be imported to avoid memory overhead.
  # required_columns <- c(
  #   "QF_Detect", "Lat_GMTCO", "Lon_GMTCO", "Date_Mscan",
  #   "Rad_DNB", "Rad_M07", "Rad_M08", "Rad_M10", "Rad_M11",
  #   "Rad_M12", "Rad_M13", "Rad_M14", "Rad_M15", "Rad_M16",
  #   "QF1_DNB", "QF1_M07", "QF1_M08", "QF1_M10", "QF1_M11",
  #   "QF1_M12", "QF1_M13", "QF1_M14", "QF1_M15", "QF1_M16",
  #   "id_iremitter", "Lat_iremitter", "Lon_iremitter",
  #   "Type_iremitter", "Category_iremitter"
  # )

  required_columns <- c(
    "QF_Detect", "Lat_GMTCO", "Lon_GMTCO", "Date_Mscan",
    "Temp_primary", "ESF_primary", "RHI_primary", "Area_primary",
    "Temp_secondary", "ESF_secondary", "Area_secondary", "RHI_secondary",
    "id_iremitter", "Type_iremitter", "Category_iremitter"
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
      Date_Mscan != 999999,
      Temp_primary != 999999,
      Lat_GMTCO >= bbox[2], Lat_GMTCO <= bbox[4],
      Lon_GMTCO >= bbox[1], Lon_GMTCO <= bbox[3]
    ) |>
    dplyr::rename(lat = Lat_GMTCO, lon = Lon_GMTCO, date = Date_Mscan) |>
    janitor::clean_names() |>
    # tidyr::drop_na() |>
    dplyr::mutate(
      date = lubridate::force_tz(lubridate::ymd_hms(date), tzone = "UTC"),
      rhi_secondary = dplyr::if_else(rhi_secondary == 999999.000, NA_real_, rhi_secondary),
      total_heat = dplyr::coalesce(rhi_primary, 0) + dplyr::coalesce(rhi_secondary, 0),
      likely_flare = temp_primary >= 1450
    )

  return(dat)
}


prc_viirs_night_fire_annual <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/viirs_night_fire_annual-1.0.0/processed/"
  # dir.create(output_path)
  # input_path <- "workspace/data/harvested/viirs_night_fire_annual-1.0.0/raw/"
  # input_files <- input_path
  input_files <- unlist(input_files)

  # Files
  files <- dir(input_files, full.names = TRUE, pattern = ".xlsx")

  # Data
  nm <- c("flares_upstream", "flares_downstream_oil", "flares_downstream_gas")
  dat <- lapply(files, function(x) {
    sht <- readxl::excel_sheets(x)
    sht <- sht[sht %in% c(
      "flares_downstream_gas", "flares_gas_downstream", "flare_downstream_gas", "flare_gas_downstream",
      "flares downstream gas", "flares gas downstream", "flare downstream gas", "flare gas downstream",
      "flares_downstream_oil", "flares_oil_downstream", "flare_downstream_oil", "flare_oil_downstream",
      "flares downstream oil", "flares oil downstream", "flare downstream oil", "flare oil downstream",
      "flares_upstream", "flares upstream", "flare upstream", "flare downstream"
    )]
    lapply(sht, function(y) readxl::read_excel(x, sheet = y)) |>
      dplyr::bind_rows() |>
      janitor::clean_names() |>
      process_column_names()
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(avg_temp_k = dplyr::if_else(is.na(avg_temp_k), avg_temp, avg_temp_k)) |>
    dplyr::filter(country == "Canada") |>
    dplyr::select(
      latitude, longitude, avg_temp_k, detection_frequency,
      clear_obs, type, bcm, year
    ) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  # Export
  sf::st_write(
    dat,
    dsn = file.path(output_path, "viirs_night_fire_annual.gpkg"),
    quiet = TRUE,
    delete_dsn = TRUE
  )
}

process_column_names <- function(df) {
  # Extract the year from column names
  suffixes <- stringr::str_extract(names(df), "_\\d{4}$") # Match 4-digit year at the end
  unique_years <- unique(na.omit(suffixes)) # Remove NAs and duplicates

  if (length(unique_years) != 1) {
    stop("Multiple or no unique years found in column names")
  }

  year <- stringr::str_replace(unique_years, "_", "") |>
    as.integer()
  names(df) <- stringr::str_remove(names(df), "_\\d{4}$") # Remove the year suffix from column names

  # Add the year column
  df$year <- year

  return(df)
}


# -----
# Atlantic
prc_viirs_night_fire_annual_atlantic <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/viirs_night_fire_annual-1.0.0/processed/"
  # dir.create(output_path)
  # input_path <- "workspace/data/harvested/viirs_night_fire_annual-1.0.0/raw/"
  # input_files <- input_path
  input_files <- unlist(input_files)

  # Files
  files <- dir(input_files, full.names = TRUE, pattern = ".xlsx")

  # Data
  nm <- c("flares_upstream", "flares_downstream_oil", "flares_downstream_gas")
  dat <- lapply(files, function(x) {
    sht <- readxl::excel_sheets(x)
    sht <- sht[sht %in% c(
      "flares_downstream_gas", "flares_gas_downstream", "flare_downstream_gas", "flare_gas_downstream",
      "flares downstream gas", "flares gas downstream", "flare downstream gas", "flare gas downstream",
      "flares_downstream_oil", "flares_oil_downstream", "flare_downstream_oil", "flare_oil_downstream",
      "flares downstream oil", "flares oil downstream", "flare downstream oil", "flare oil downstream",
      "flares_upstream", "flares upstream", "flare upstream", "flare downstream"
    )]
    lapply(sht, function(y) readxl::read_excel(x, sheet = y)) |>
      dplyr::bind_rows() |>
      janitor::clean_names() |>
      process_column_names()
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(avg_temp_k = dplyr::if_else(is.na(avg_temp_k), avg_temp, avg_temp_k)) |>
    dplyr::select(
      latitude, longitude, avg_temp_k, detection_frequency,
      clear_obs, type, bcm, year
    ) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  # Export
  sf::st_write(
    dat,
    dsn = file.path(output_path, "viirs_night_fire_annual_atlantic.gpkg"),
    quiet = TRUE,
    delete_dsn = TRUE
  )
}


prc_viirs_night_fire_nightly_atlantic <- function(input_files, output_path) {
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
    process_vnf_nightly_file_atlantic,
    .options = furrr::furrr_options(seed = TRUE)
  ) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

  # Export
  sf::st_write(
    dat,
    dsn = file.path(output_path, "viirs_night_fire_nightly_atlantic.gpkg"),
    quiet = TRUE,
    delete_dsn = TRUE
  )
}


process_vnf_nightly_file_atlantic <- function(file) {
  # Specify the columns to be imported to avoid memory overhead.
  # required_columns <- c(
  #   "QF_Detect", "Lat_GMTCO", "Lon_GMTCO", "Date_Mscan",
  #   "Rad_DNB", "Rad_M07", "Rad_M08", "Rad_M10", "Rad_M11",
  #   "Rad_M12", "Rad_M13", "Rad_M14", "Rad_M15", "Rad_M16",
  #   "QF1_DNB", "QF1_M07", "QF1_M08", "QF1_M10", "QF1_M11",
  #   "QF1_M12", "QF1_M13", "QF1_M14", "QF1_M15", "QF1_M16",
  #   "id_iremitter", "Lat_iremitter", "Lon_iremitter",
  #   "Type_iremitter", "Category_iremitter"
  # )

  required_columns <- c(
    "QF_Detect", "Lat_GMTCO", "Lon_GMTCO", "Date_Mscan",
    "Temp_primary", "ESF_primary", "RHI_primary", "Area_primary",
    "Temp_secondary", "ESF_secondary", "Area_secondary", "RHI_secondary",
    "id_iremitter", "Type_iremitter", "Category_iremitter"
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
  bbox <- c(-97, -54, 46, 66.5)

  # Read the file
  dat <- arrow::read_csv_arrow(file, col_select = dplyr::all_of(required_columns)) |>
    # Ensure QF_Detect is an integer
    dplyr::mutate(QF_Detect = as.integer(QF_Detect)) |>
    # Apply bitwise filtering for quality flags
    dplyr::filter(
      (bitwAnd(QF_Detect, retain_mask) != 0),
      Date_Mscan != 999999,
      Temp_primary != 999999,
      Lat_GMTCO >= bbox[2], Lat_GMTCO <= bbox[4],
      Lon_GMTCO >= bbox[1], Lon_GMTCO <= bbox[3]
    ) |>
    dplyr::rename(lat = Lat_GMTCO, lon = Lon_GMTCO, date = Date_Mscan) |>
    janitor::clean_names() |>
    # tidyr::drop_na() |>
    dplyr::mutate(
      date = lubridate::force_tz(lubridate::ymd_hms(date), tzone = "UTC"),
      rhi_secondary = dplyr::if_else(rhi_secondary == 999999.000, NA_real_, rhi_secondary),
      total_heat = dplyr::coalesce(rhi_primary, 0) + dplyr::coalesce(rhi_secondary, 0),
      likely_flare = temp_primary >= 1450
    )

  return(dat)
}
