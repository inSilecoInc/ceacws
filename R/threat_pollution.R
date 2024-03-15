#' Script to generate the threat layer associated with ocean pollution
#'
#' @export
threat_pollution <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # NEEC data
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  neec <- pipedat::importdat("77f8d683", "format")[["neec_cws-77f8d683.csv"]]

  # ----------------------------------------------------------------
  # Filter NEEC data based on specified constraints
  # Remove empty coordinates
  neec <- neec |>
    dplyr::filter(!is.na(latitude))

  # ----------------------------------------------------------------
  # Use data from “Notification Date Time” when not available from “Incident Local Date Time” field
  neec <- neec |>
    dplyr::mutate(
      datetime = ifelse(!is.na(incident_date_time), incident_date_time, notification_date_time),
      date = as.Date(datetime, format = "%Y-%m-%d %H:%M"),
      year = format(date, "%Y")
    )

  # ----------------------------------------------------------------
  # Remove records containing "covid", "istop", "sipps"
  neec <- neec |>
    dplyr::filter(!stringr::str_detect(tolower(incident_name), "covid")) |>
    dplyr::filter(!stringr::str_detect(tolower(incident_name), "istop")) |>
    dplyr::filter(!stringr::str_detect(tolower(incident_name), "sipps"))

  # ----------------------------------------------------------------
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # TO VERIFY
  # Set all NA quantities to 1
  neec <- neec |>
    dplyr::mutate(quantity = ifelse(is.na(quantity), 1, quantity))
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  # ----------------------------------------------------------------
  # Select greatest quantity for individual spills with multiple substances
  neec <- neec |>
    dplyr::group_by(incident_name) |>
    dplyr::filter(quantity == max(quantity, na.rm = TRUE)) |>
    dplyr::ungroup()

  # ----------------------------------------------------------------
  # Based on email exhange with Robert Ronconi on 2024-02-23
  # Remove Incident Name containing "MVI" or "furnace oil"
  # This is to remove a few dozen incidents that generally won't impact marine birds
  neec <- neec |>
    dplyr::filter(!stringr::str_detect(tolower(incident_name), "mvi")) |>
    dplyr::filter(!stringr::str_detect(tolower(incident_name), "furnace oil"))

  # ----------------------------------------------------------------
  # Based on email exhange with Robert Ronconi on 2024-02-23
  # Keep all incidents within 1km of the coastline
  # Coastal outline
  tmp <- here::here("tmp")
  chk_create(tmp)
  getdat <- function(country) {
    raster::getData("GADM", country = country, level = 0, path = tmp) |>
      sf::st_as_sf() |>
      sf::st_simplify(dTolerance = 600, preserveTopology = FALSE) |>
      sf::st_make_valid()
  }
  can <- getdat("CAN")
  usa <- getdat("USA")
  terre <- sf::st_union(can, usa) |>
    sf::st_transform(32198) |> # To get units in meters
    smoothr::fill_holes(threshold = units::set_units(1000, km^2))

  # 1km buffer inland
  terre_buf <- sf::st_buffer(terre, -1000) |>
    sf::st_transform(4326)

  # Filter data spatially
  neec <- neec |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  iid <- sf::st_disjoint(terre_buf, neec) |> unlist()
  neec <- neec[iid, ]

  # Remove downloaded data
  fs::dir_delete(tmp)
  # ----------------------------------------------------------------
  # To explore:
  # Impact to Waterbody field.  Consider if we should treat “actual” vs. “potential” spills differently.  Data on “potential” spills only started being recorded in April 2021 onward (no differentiation before that).  Some “Potential” spills may report very large amounts of substance that was not actually spilled (e.g.  Incident Number NL-20200611-03045-20 was a disabled vessel with 400000 L of fuel on board, but nothing spilled).  These “Potential” spills still represent risk even if no true pollution occurred.

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # NASP data
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  nasp <- pipedat::importdat("376f0891", "format")[[1]]

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # ISTOP data
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load & Filter ISTOP data based on specified constraints
  istop <- pipedat::importdat("48ea8a05", "format")[[1]] |>
    dplyr::filter(!is.na(date)) |>
    dplyr::distinct() |>
    dplyr::filter(category %in% c("1A", "1B", "B"))

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Threat layer TODO
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  neec
  nasp
  istop
}
