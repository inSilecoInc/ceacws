ana_petroleum_pollution_incidents_neec <- function(input_files, output_path) {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # NEEC data
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  input_files <- unlist(input_files)
  neec <- input_files[stringr::str_detect(input_files, "neec.csv")] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE)

  substances <- input_files[stringr::str_detect(input_files, "substances.csv")] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE)
  # ----------------------------------------------------------------
  # Filter NEEC data based on specified constraints
  # Remove empty coordinates
  neec <- neec |>
    dplyr::filter(!is.na(latitude))

  # ----------------------------------------------------------------
  # Use data from “Notification Date Time” when not available from “Incident Local Date Time” field
  neec <- neec |>
    dplyr::mutate(
      datetime = ifelse(!is.na(incident_local_date_time_local_time), incident_local_date_time_local_time, notification_date_time),
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
    dplyr::mutate(
      quantity = ifelse(is.na(quantity_converted), 1, quantity_converted),
      quantity = ifelse(quantity < 1, 1, quantity)
    )
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
  # Group by substance type
  neec <- neec |>
    dplyr::left_join(substances[, c("name", "chemical_state", "oil_sheen")], by = c("substance" = "name")) |>
    dplyr::mutate(
      chemical_state = ifelse(is.na(chemical_state), "unknown", chemical_state),
      oil_sheen = ifelse(is.na(oil_sheen), "unknown", oil_sheen)
    )

  # ----------------------------------------------------------------
  # # Based on email exhange with Robert Ronconi on 2024-02-23
  # # Keep all incidents within 1km of the coastline
  # # Coastal outline
  fetch_data <- function(url) {
    # Temporary file
    tmp <- tempfile(fileext = ".json")

    # Download file
    curl::curl_download(url, tmp)

    # Import
    sf::st_read(tmp, quiet = TRUE)
  }

  coastlines <- c(
    "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_USA_0.json",
    "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_CAN_0.json"
  ) |>
    lapply(fetch_data)

  terre <- sf::st_union(coastlines[[1]], coastlines[[2]]) |>
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

  # ----------------------------------------------------------------
  # Function to categorize a numeric vector into n categories by percentile
  categorize_by_percentile <- function(x) {
    # Calculate quantiles for dividing into n categories
    probs <- c(0.0, 0.7, 0.9, 1.0)
    breaks <- quantile(x, probs = probs, na.rm = TRUE)

    # Use cut to assign each value in x to a percentile category
    categories <- cut(x, breaks = breaks, include.lowest = TRUE, labels = paste0("Q", 1:(length(probs) - 1)))

    return(categories)
  }

  # Categorize quantities
  neec <- neec |>
    dplyr::mutate(quantity_category = categorize_by_percentile(neec$quantity))

  # Select columns that are going to be useful for the next step only to reduce file size
  neec <- neec |>
    dplyr::select(date, year, quantity, quantity_category, chemical_state, oil_sheen)

  # Export partial data for use in report (this should be revisited)
  sf::st_write(neec, dsn = file.path(output_path, "neec_prep.gpkg"), quiet = TRUE, delete_dsn = TRUE)
  # ----------------------------------------------------------------
  # To explore:
  # Impact to Waterbody field.  Consider if we should treat “actual” vs. “potential” spills differently.  Data on “potential” spills only started being recorded in April 2021 onward (no differentiation before that).  Some “Potential” spills may report very large amounts of substance that was not actually spilled (e.g.  Incident Number NL-20200611-03045-20 was a disabled vessel with 400000 L of fuel on board, but nothing spilled).  These “Potential” spills still represent risk even if no true pollution occurred.
}

ana_petroleum_pollution_incidents_nasp <- function(input_files, output_path) {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # NASP data
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  input_files <- unlist(input_files)
  nasp <- input_files[stringr::str_detect(input_files, "nasp.gpkg")] |>
    sf::st_read(quiet = TRUE) |>
    dplyr::filter(!is.na(volume_l)) |>
    dplyr::rename(geometry = geom)

  # Export
  sf::st_write(nasp, dsn = file.path(output_path, "nasp_prep.gpkg"), quiet = TRUE, delete_dsn = TRUE)
}

ana_petroleum_pollution_incidents_istop <- function(input_files, output_path) {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # ISTOP data
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load & Filter ISTOP data based on specified constraints
  istop <- input_files[stringr::str_detect(unlist(input_files), "istop.gpkg")] |>
    sf::st_read(quiet = TRUE) |>
    dplyr::filter(!is.na(date)) |>
    dplyr::distinct() |>
    dplyr::filter(category %in% c("1A", "1B", "B")) |>
    dplyr::rename(geometry = geom) |>
    dplyr::mutate(
      area = sf::st_area(geometry),
      area = units::set_units(area, km^2),
      area = as.numeric(area)
    )

  # Export partial data for use in report (this should be revisited)
  sf::st_write(istop, dsn = file.path(output_path, "istop_prep.gpkg"), quiet = TRUE, delete_dsn = TRUE)
}

ana_petroleum_pollution_incidents_neec_diffusive <- function(input_files, output_path, threshold = .05, decay = 2, increment = 100, cellsize = 1000) {
  # Using diffusive model function to generate threat layer for each dataset
  # https://github.com/EffetsCumulatifsNavigation/ceanav/blob/main/R/fnc_diffusive_model.R
  neec <- sf::st_read(unlist(input_files), quiet = TRUE)

  # NEEC
  neec_tmp <- neec |>
    dplyr::mutate(
      quantity = as.numeric(as.factor(quantity_category)),
      quantity = quantity / max(quantity)
    ) |>
    dplyr::rename(geometry = geom) |>
    sf::st_transform(3348)
  neec_threat <- diffusive_model(
    dat = neec_tmp,
    field = "quantity",
    threshold = threshold,
    globalmaximum = min(neec_tmp$quantity),
    decay = decay,
    increment = increment,
    cellsize = cellsize
  )

  # Export
  neec_threat |>
    terra::rast() |>
    terra::writeRaster(
      file.path(output_path, "petroleum_pollution_incidents_neec.tif"),
      filetype = "COG",
      gdal = c("COMPRESS=LZW", "TILED=YES", "OVERVIEW_RESAMPLING=AVERAGE"),
      overwrite = TRUE
    )
}

ana_petroleum_pollution_incidents_nasp_diffusive <- function(input_files, output_path, threshold = .05, decay = 2, increment = 100, cellsize = 1000) {
  # Using diffusive model function to generate threat layer for each dataset
  # https://github.com/EffetsCumulatifsNavigation/ceanav/blob/main/R/fnc_diffusive_model.R
  nasp <- sf::st_read(unlist(input_files), quiet = TRUE)

  # NASP
  nasp_tmp <- nasp |>
    dplyr::mutate(
      volume_l = log(volume_l + 1),
      volume_l = volume_l / max(volume_l)
    ) |>
    dplyr::rename(geometry = geom) |>
    sf::st_transform(3348)
  nasp_threat <- diffusive_model(
    dat = nasp_tmp,
    field = "volume_l",
    threshold = threshold,
    globalmaximum = min(nasp_tmp$volume_l),
    decay = decay,
    increment = increment,
    cellsize = cellsize
  )

  # Export
  nasp_threat |>
    terra::rast() |>
    terra::writeRaster(
      file.path(output_path, "petroleum_pollution_incidents_nasp.tif"),
      filetype = "COG",
      gdal = c("COMPRESS=LZW", "TILED=YES", "OVERVIEW_RESAMPLING=AVERAGE"),
      overwrite = TRUE
    )
}

ana_petroleum_pollution_incidents_istop_diffusive <- function(input_files, output_path, threshold = .05, decay = 2, increment = 100, cellsize = 1000) {
  # Using diffusive model function to generate threat layer for each dataset
  # https://github.com/EffetsCumulatifsNavigation/ceanav/blob/main/R/fnc_diffusive_model.R
  istop <- sf::st_read(unlist(input_files), quiet = TRUE)

  # ISTOP
  istop_tmp <- istop |>
    dplyr::mutate(
      area = log(area + 1),
      area = area / max(area)
    ) |>
    dplyr::rename(geometry = geom) |>
    sf::st_transform(3348)
  istop_threat <- diffusive_model(
    dat = istop_tmp,
    field = "area",
    threshold = threshold,
    globalmaximum = min(istop_tmp$area),
    decay = decay,
    increment = increment,
    cellsize = cellsize
  )

  # Export
  istop_threat |>
    terra::rast() |>
    terra::writeRaster(
      file.path(output_path, "petroleum_pollution_incidents_istop.tif"),
      filetype = "COG",
      gdal = c("COMPRESS=LZW", "TILED=YES", "OVERVIEW_RESAMPLING=AVERAGE"),
      overwrite = TRUE
    )
}
