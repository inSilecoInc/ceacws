ana_shipping_intensity_density <- function(input_files, output_path) {

}

ana_shipping_intensity_occupancy <- function(input_files, output_path) {

}


ana_shipping_light_density <- function(input_files, output_path) {

}

ana_shipping_light_occupancy <- function(input_files, output_path) {

}





create_full_tracklines <- function(points, time_filter = "all", group_by_type = FALSE) {
  # Filter points by time of day
  if (time_filter != "all") {
    points <- points |>
      dplyr::filter(day_or_night == time_filter)
  }

  # Group points by MMSI, track_id, and optionally boat type
  grouping_vars <- c("MMSI", "track_id")
  if (group_by_type) {
    grouping_vars <- c(grouping_vars, "ntype") # Assuming 'ntype' is the boat type column
  }

  # Create tracklines
  tracklines <- points |>
    dplyr::group_by(across(all_of(grouping_vars))) |>
    dplyr::summarise(
      start_time = min(YMD_HMS, na.rm = TRUE),
      end_time = max(YMD_HMS, na.rm = TRUE),
      avg_sog = mean(SOG, na.rm = TRUE),
      max_sog = max(SOG, na.rm = TRUE),
      n_pos = dplyr::n(),
      elapsed_hours = sum(time_diff, na.rm = TRUE) / 60, # Convert minutes to hours
      geometry = sf::st_linestring(as.matrix(cbind(Longitude, Latitude))),
      .groups = "drop"
    ) |>
    dplyr::filter(n_pos > 1) |>
    sf::st_as_sf(crs = 4326) |>
    dplyr::mutate(
      track_length_km = units::set_units(sf::st_length(geometry), "km")
    )

  return(tracklines)
}


create_consecutive_tracklines <- function(points, time_filter = "all", group_by_type = FALSE) {
  # Filter points by time of day
  if (time_filter != "all") {
    points <- points |>
      dplyr::filter(day_or_night == time_filter)
  }

  # Group points by MMSI, track_id, and optionally boat type
  grouping_vars <- c("MMSI", "track_id")
  if (group_by_type) {
    grouping_vars <- c(grouping_vars, "ntype") # Assuming 'ntype' is the boat type column
  }

  # Create tracklines between consecutive points
  tracklines <- points |>
    dplyr::group_by(across(all_of(grouping_vars))) |>
    dplyr::arrange(YMD_HMS) |>
    dplyr::mutate(
      next_lon = dplyr::lead(Longitude),
      next_lat = dplyr::lead(Latitude),
      next_time = dplyr::lead(YMD_HMS)
    ) |>
    dplyr::filter(!is.na(next_lon) & !is.na(next_lat)) |>
    dplyr::mutate(
      geometry = sf::st_linestring(as.matrix(cbind(c(Longitude, next_lon), c(Latitude, next_lat)))),
      time_diff_hours = as.numeric(difftime(next_time, YMD_HMS, units = "hours"))
    ) |>
    dplyr::ungroup() |>
    sf::st_as_sf(crs = 4326) |>
    dplyr::mutate(
      segment_length_km = units::set_units(sf::st_length(geometry), "km")
    )

  return(tracklines)
}
