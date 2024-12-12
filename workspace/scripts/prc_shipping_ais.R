prc_shipping_ais_tracklines <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/shipping_ais-1.0.0/processed/"
  # # dir.create(output_path)
  # input_path <- "workspace/data/harvested/shipping_ais-1.0.0/raw/"
  # input_files <- file.path(input_path, "2023AIS.zip")
  input_files <- unlist(input_files)

  # Data
  tmp <- file.path(output_path, "tmp")
  archive::archive_extract(input_files, tmp)
  parquet_db <- file.path(tmp, "parquet_dataset")
  try(archive::archive_extract(file.path(tmp, "OneDrive_1_3-13-2024.zip"), parquet_db))

  # Ship info
  ship_info <- vroom::vroom(
    file.path(tmp, "static_data_2023.csv"),
    progress = FALSE,
    show_col_types = FALSE,
    col_types = vroom::cols(
      Cnt_MMSI = vroom::col_skip()
    )
  )

  # Parquet files
  future::plan(future::multisession, workers = parallel::detectCores() - 3)

  # List of parquet files
  files <- list.files(parquet_db, full.names = TRUE)

  # Apply processing in parallel
  tracklines <- furrr::future_map(files, function(file) {
    process_parquet(file) |>
      day_night() |>
      create_tracklines(ship_info) |>
      post_process_tracklines()
  },
  .options = furrr::furrr_options(seed = TRUE)
  )

  # # Split by MMSI and process each group in parallel
  # tracklines <- dat |>
  #   dplyr::group_split(MMSI) |>
  #   furrr::future_map_dfr(create_tracklines)

  # Bind
  tracklines <- purrr::map(tracklines, sf::st_as_sf) |>
    dplyr::bind_rows() |>
    sf::st_as_sf()

  # Export
  sf::st_write(
    tracklines,
    dsn = file.path(output_path, "shipping_ais_tracklines.gpkg"),
    quiet = TRUE,
    delete_dsn = TRUE
  )

  # Remove unzipped files
  fs::dir_delete(tmp)
}


process_parquet <- function(file) {
  arrow::read_parquet(file, col_select = c("MMSI", "Latitude", "Longitude", "SOG", "YMD_HMS")) |>
    dplyr::filter(
      MMSI >= 201000000 & MMSI <= 775999999,
      SOG >= 0.1 & SOG <= 100
    ) |>
    dplyr::mutate(
      YMD_HMS = lubridate::force_tz(YMD_HMS, tzone = "UTC")
    )
}

day_night <- function(dat) {
  # Create unique combinations of date, Latitude, and Longitude (rounded)
  dat <- dat |>
    dplyr::mutate(
      date = lubridate::as_date(YMD_HMS),
      lat = round(Latitude, 2),
      lon = round(Longitude, 2)
    )
  unique_points <- dat |>
    dplyr::distinct(date, lat, lon)

  # Calculate sunrise/sunset for each unique point
  sunlight <- suncalc::getSunlightTimes(
    data = unique_points,
    keep = c("sunrise", "sunset"),
    tz = "UTC"
  ) |>
    dplyr::select(date, lat, lon, sunrise, sunset)

  # Join back to original data
  dat <- dat |>
    dplyr::left_join(sunlight, by = c("date", "lat", "lon")) |>
    dplyr::mutate(
      day_or_night = dplyr::if_else(YMD_HMS >= sunrise & YMD_HMS <= sunset, "day", "night")
    ) |>
    dplyr::select(-lat, -lon, -sunrise, -sunset)

  return(dat)
}

create_tracklines <- function(dat, ship_info) {
  dat <- dat |>
    dplyr::arrange(MMSI, YMD_HMS) |>
    dplyr::group_by(MMSI) |>
    dplyr::mutate(
      dist_miles = c(NA, geodist::geodist(
        cbind(Longitude, Latitude),
        sequential = TRUE,
        measure = "geodesic"
      )),
      dist_miles = dist_miles / 1609.34,
      time_diff = as.numeric(difftime(YMD_HMS, dplyr::lag(YMD_HMS), units = "mins")),
      new_track = dplyr::coalesce(dist_miles > 50 | time_diff > 300, FALSE),
      track_id = cumsum(new_track)
    ) |>
    dplyr::group_by(MMSI, track_id) |>
    dplyr::mutate(
      day_night_segment = cumsum(day_or_night != dplyr::lag(day_or_night, default = dplyr::first(day_or_night)))
    ) |>
    dplyr::ungroup()

  # Summarize into tracklines)
  tracklines <- dat |>
    dplyr::group_by(MMSI, track_id, day_night_segment) |>
    dplyr::summarise(
      start_time = min(YMD_HMS, na.rm = TRUE),
      end_time = max(YMD_HMS, na.rm = TRUE),
      avg_sog = mean(SOG, na.rm = TRUE),
      max_sog = max(SOG, na.rm = TRUE),
      n_pos = dplyr::n(),
      elapsed_hours = as.numeric(difftime(max(YMD_HMS), min(YMD_HMS), units = "hours")) |>
        units::set_units("hours"),
      month = lubridate::month(min(YMD_HMS)),
      year = lubridate::year(min(YMD_HMS)),
      daytime = dplyr::case_when(
        all(day_or_night == "day") ~ "day",
        all(day_or_night == "night") ~ "night",
        .default = "mixed"
      ),
      geometry = list(sf::st_linestring(as.matrix(cbind(Longitude, Latitude)))),
      .groups = "drop"
    ) |>
    dplyr::filter(n_pos > 1) |>
    sf::st_as_sf(crs = 4326) |>
    dplyr::mutate(
      track_length_km = units::set_units(sf::st_length(geometry), "km")
    )

  # Add ship info
  tracklines <- tracklines |>
    dplyr::left_join(ship_info, by = "MMSI") |>
    janitor::clean_names()

  return(tracklines)
}

post_process_tracklines <- function(tracklines) {
  # Add ship type information to tracklines
  tracklines <- tracklines |>
    dplyr::mutate(
      avg_speed_kmh = track_length_km / elapsed_hours,
      spoofing_flags = dplyr::case_when(
        avg_speed_kmh < units::set_units(0.3, "knots") ~ TRUE, # Speeds below 0.3 knots
        track_length_km < units::set_units(100, "meters") ~ TRUE, # Track length less than 100 meters
        avg_speed_kmh > units::set_units(100, "km/h") ~ TRUE, # Speeds exceeding 100 km/h
        ntype %in% c("PLEASURE VESSELS", "OTHERS/SPECIAL SHIPS") & avg_speed_kmh > units::set_units(30, "km/h") ~ TRUE,
        ntype %in% c("FISHING", "TUGS/PORT") & avg_speed_kmh > units::set_units(35, "km/h") ~ TRUE,
        avg_speed_kmh > units::set_units(40, "km/h") ~ TRUE, # Other ships over 40 km/h,
        is.na(day_night_segment) ~ TRUE,
        elapsed_hours == units::set_units(0, "hours") ~ TRUE,
        .default = FALSE
      )
    ) |>
    dplyr::filter(!spoofing_flags)

  return(tracklines)
}

prc_shipping_ais_points <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/shipping_ais-1.0.0/processed/"
  # # dir.create(output_path)
  # input_path <- "workspace/data/harvested/shipping_ais-1.0.0/raw/"
  # input_files <- file.path(input_path, "2023AIS.zip")
  input_files <- unlist(input_files)

  # Data
  tmp <- file.path(output_path, "tmp")
  archive::archive_extract(input_files, tmp)
  parquet_db <- file.path(tmp, "parquet_dataset")
  try(archive::archive_extract(file.path(tmp, "OneDrive_1_3-13-2024.zip"), parquet_db))

  # Load ship info
  ship_info <- vroom::vroom(
    file.path(tmp, "static_data_2023.csv"),
    progress = FALSE,
    show_col_types = FALSE,
    col_types = vroom::cols(Cnt_MMSI = vroom::col_skip())
  )
  write.csv(ship_info, file = file.path(output_path, "vessel_static_information.csv"), row.names = FALSE)

  # Prepare for parallel processing
  future::plan(future::multisession, workers = parallel::detectCores() - 3)

  # List of parquet files
  files <- list.files(parquet_db, full.names = TRUE)[1:8]

  # Process each parquet file in parallel and write directly to output
  furrr::future_map(files, function(file) {
    processed_points <- process_parquet(file) |>
      day_night() |>
      create_track_ids(ship_info) |>
      post_process_points()

    # Extract the year and month from the file or data
    split_name <- basename(file) |>
      strsplit("_") |>
      unlist() |>
      tolower()
    year <- split_name[3]
    month <- split_name[2]
    month <- lubridate::ymd(paste("2023", month, "01")) |>
      lubridate::month()

    # Export the processed points directly to a Parquet file
    arrow::write_parquet(
      processed_points,
      file.path(output_path, glue::glue("shipping_ais_{year}_{month}.parquet"))
    )
  }, .options = furrr::furrr_options(seed = TRUE))

  # Clean up temporary files
  fs::dir_delete(tmp)
}


create_track_ids <- function(dat, ship_info) {
  dat <- dat |>
    dplyr::arrange(MMSI, YMD_HMS) |>
    dplyr::group_by(MMSI) |>
    dplyr::mutate(
      dist_miles = c(NA, geodist::geodist(
        cbind(Longitude, Latitude),
        sequential = TRUE,
        measure = "geodesic"
      )) / 1609.34,
      time_diff = as.numeric(difftime(YMD_HMS, dplyr::lag(YMD_HMS), units = "mins")),
      new_track = dplyr::coalesce(dist_miles > 50 | time_diff > 300, FALSE),
      track_id = cumsum(new_track),
      day_night_segment = cumsum(day_or_night != dplyr::lag(day_or_night, default = dplyr::first(day_or_night)))
    ) |>
    dplyr::ungroup()

  # Join ship information
  dat <- dat |>
    dplyr::left_join(ship_info[, c("MMSI", "NTYPE")], by = "MMSI") |>
    janitor::clean_names()

  return(dat)
}

post_process_points <- function(points) {
  # Group by track_id and compute track-level metrics
  points <- points |>
    dplyr::group_by(track_id) |>
    dplyr::mutate(
      # Calculate total track length in kilometers
      track_length_km = sum(dist_miles * 1.60934, na.rm = TRUE), # Convert miles to kilometers and sum
      # Compute total elapsed time in hours based on time_diff
      elapsed_hours = sum(time_diff, na.rm = TRUE) / 60, # Convert minutes to hours
      avg_speed_kmh = track_length_km / elapsed_hours
    ) |>
    # Add spoofing flags at the track level
    dplyr::mutate(
      spoofing_flags = dplyr::case_when(
        avg_speed_kmh < (0.3 * 1.852) ~ TRUE, # Speeds below 0.3 knots
        track_length_km < 0.1 ~ TRUE, # Track length less than 100 meters
        avg_speed_kmh > (100 * 1.852) ~ TRUE, # Speeds exceeding 100 knots
        ntype %in% c("PLEASURE VESSELS", "OTHERS/SPECIAL SHIPS") & avg_speed_kmh > (30 * 1.852) ~ TRUE,
        ntype %in% c("FISHING", "TUGS/PORT") & avg_speed_kmh > (35 * 1.852) ~ TRUE,
        avg_speed_kmh > (40 * 1.852) ~ TRUE, # Other ships over 40 knots
        .default = FALSE
      )
    ) |>
    # Ungroup and filter out flagged tracks
    dplyr::ungroup() |>
    dplyr::filter(!spoofing_flags) |>
    dplyr::select(-spoofing_flags, -track_length_km, -elapsed_hours, -avg_speed_kmh, -ntype)

  return(points)
}
