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
  # fs::dir_delete(tmp)
  # ----------------------------------------------------------------
  # To explore:
  # Impact to Waterbody field.  Consider if we should treat “actual” vs. “potential” spills differently.  Data on “potential” spills only started being recorded in April 2021 onward (no differentiation before that).  Some “Potential” spills may report very large amounts of substance that was not actually spilled (e.g.  Incident Number NL-20200611-03045-20 was a disabled vessel with 400000 L of fuel on board, but nothing spilled).  These “Potential” spills still represent risk even if no true pollution occurred.

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # NASP data
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  nasp <- pipedat::importdat("376f0891", "format")[[1]] |>
    dplyr::filter(!is.na(volume_l)) |>
    dplyr::rename(geometry = geom)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # ISTOP data
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load & Filter ISTOP data based on specified constraints
  istop <- pipedat::importdat("48ea8a05", "format")[[1]] |>
    dplyr::filter(!is.na(date)) |>
    dplyr::distinct() |>
    dplyr::filter(category %in% c("1A", "1B", "B")) |>
    dplyr::rename(geometry = geom) |>
    dplyr::mutate(
      area = sf::st_area(geometry),
      area = units::set_units(area, km^2),
      area = as.numeric(area)
    )

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Threat layer - diffusive model
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Using diffusive model function to generate threat layer for each dataset
  # https://github.com/EffetsCumulatifsNavigation/ceanav/blob/main/R/fnc_diffusive_model.R
  # Parameters
  threshold <- .05
  decay <- 2
  increment <- 100

  # NEEC
  neec_tmp <- neec |>
    dplyr::mutate(
      quantity = log(quantity + 1),
      quantity = quantity / max(quantity)
    ) |>
    sf::st_transform(3348)
  neec_threat <- diffusive(
    dat = neec_tmp,
    field = "quantity",
    threshold = threshold,
    globalmaximum = min(neec_tmp$quantity),
    decay = decay,
    increment = increment
  )

  # NASP
  nasp_tmp <- nasp |>
    dplyr::mutate(
      volume_l = log(volume_l + 1),
      volume_l = volume_l / max(volume_l)
    ) |>
    sf::st_transform(3348)
  nasp_threat <- diffusive(
    dat = nasp_tmp,
    field = "volume_l",
    threshold = threshold,
    globalmaximum = min(nasp_tmp$volume_l),
    decay = decay,
    increment = increment
  )

  # ISTOP
  istop_tmp <- istop |>
    dplyr::mutate(
      area = log(area + 1),
      area = area / max(area)
    ) |>
    sf::st_transform(3348)
  istop_threat <- diffusive(
    dat = istop_tmp,
    field = "area",
    threshold = threshold,
    globalmaximum = min(istop_tmp$area),
    decay = decay,
    increment = increment
  )

  # Export
  out <- here::here("output", "threats")
  pipedat::chk_create(out)
  pipedat::masterwrite(neec_threat, here::here(out, "pollution_neec"))
  pipedat::masterwrite(nasp_threat, here::here(out, "pollution_nasp"))
  pipedat::masterwrite(istop_threat, here::here(out, "pollution_istop"))


  # # ----
  # KERNEL EXPLORATION
  # make_kernel <- function(dat, sig, wts) {
  #   # Window
  #   global_parameters()
  #   bbox <- unlist(param$bbox$base)[c("xmin", "xmax", "ymin", "ymax")] |>
  #     pipedat::bbox_poly(param$crs) |>
  #     sf::st_transform(3348) |>
  #     sf::st_bbox()
  #   wdw <- spatstat.geom::owin(xrange = c(bbox["xmin"], bbox["xmax"]), yrange = c(bbox["ymin"], bbox["ymax"]))

  #   # Points
  #   xy <- sf::st_transform(dat, 3348) |>
  #     sf::st_coordinates()
  #   pts <- spatstat.geom::ppp(
  #     x = xy[, "X"],
  #     y = xy[, "Y"],
  #     window = wdw,
  #     marks = dat[, wts, drop = TRUE]
  #   )

  #   # Kernel
  #   kern <- spatstat.explore::density.ppp(
  #     x = pts,
  #     sigma = sig,
  #     # weights = log(pts$marks + 1),
  #     weights = pts$marks,
  #     edge = TRUE,
  #     kernel = "gaussian"
  #   )

  #   # Spatial objet and mask to study area
  #   kern |>
  #     terra::rast() |>
  #     stars::st_as_stars() |>
  #     sf::st_set_crs(3348) #|>
  #   # pipedat::masteringrid()
  # }

  # x <- make_kernel(neec, 10000, "quantity")
  # plot(x)
}
