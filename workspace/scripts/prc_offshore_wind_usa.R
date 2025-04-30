prc_offshore_wind_usa <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/offshore_wind_usa-1.0.0/processed/"
  # dir.create(output_path)
  # input_path <- "workspace/data/harvested/offshore_wind_usa-1.0.0/raw/"
  # input_files <- file.path(input_path, c(
  #   "boem_renewable_energy_geodatabase.zip",
  #   "Offshore_Wind_-_Project_Phase_Areas_Proposed.gpkg",
  #   "Offshore_Wind_-_Substations_Proposed_or_Installed.gpkg",
  #   "Offshore_Wind_-_Turbine_Locations_Proposed_or_Installed.gpkg",
  #   "Offshore_Wind_-_Cable_Interconnections_Proposed_or_Installed.gpkg",
  #   "Offshore_Wind_-_Ocean_Observing_Devices_Proposed_or_Installed.gpkg",
  #   "Offshore_Wind_-_Export_Cable_Corridors_Proposed.gpkg",
  #   "Offshore_Wind_-_Cable_Landings_Proposed_or_Installed.gpkg",
  #   "Offshore_Wind_-_Project_Inter-array_Cables_Proposed_or_Installed.gpkg"
  # ))

  input_files <- unlist(input_files)

  # Data
  tmp <- file.path(output_path, "tmp")
  boem <- list()

  ## BEOM Geodatabase
  input_files[tools::file_ext(input_files) == "zip"] |>
    archive::archive_extract(tmp)
  boem_layers <- sf::st_layers(file.path(tmp, "leases_planning_areas_feb_2025.gdb"))[[1]]

  ### Leases
  suppressWarnings({
    boem[["leases"]] <- sf::st_read(
      file.path(tmp, "leases_planning_areas_feb_2025.gdb"),
      layer = boem_layers[stringr::str_detect(boem_layers, "Wind_Leases")],
      quiet = TRUE
    ) |>
      sf::st_transform(4326) |>
      sf::st_zm() |>
      dplyr::mutate(
        dataset = "wind_usa",
        classification = "lease",
        type = tolower(LEASE_TYPE),
        date_start = lubridate::dmy(LEASE_DATE),
        date_start = dplyr::case_when(
          is.na(date_start) ~ lubridate::mdy(LEASE_DATE),
          .default = date_start
        ),
        lease_term_years = stringr::str_trim(LEASE_TERM, side = "both"),
        lease_term_years = as.numeric(gsub(" years", "", tolower(lease_term_years))),
        date_end = format(date_start + lubridate::years(lease_term_years), "%d-%m-%Y")
      ) |>
      dplyr::select(dataset, classification, type, lease_number = LEASE_NUMBER, date_start, date_end)
  })

  ### Planning areas
  boem[["planning_areas"]] <- sf::st_read(
    file.path(tmp, "leases_planning_areas_feb_2025.gdb"),
    layer = boem_layers[stringr::str_detect(boem_layers, "Wind_Planning_Areas")],
    quiet = TRUE
  ) |>
    sf::st_transform(4326) |>
    sf::st_zm() |>
    dplyr::mutate(
      dataset = "wind_usa",
      classification = "planning area",
      status = AREA_STATUS
    ) |>
    dplyr::select(dataset, classification, status, protraction_number = PROTRACTION_NUMBER)

  ### MHK leases and planning areas
  boem[["mhk_leases_and_planning_areas"]] <- sf::st_read(
    file.path(tmp, "leases_planning_areas_feb_2025.gdb"),
    layer = boem_layers[stringr::str_detect(boem_layers, "BOEM_MHKLeasesandPlanningAreas")],
    quiet = TRUE
  ) |>
    sf::st_transform(4326) |>
    sf::st_zm() |>
    dplyr::mutate(
      dataset = "wind_usa",
      classification = "mhk leases and planning areas",
      type = tolower(LEASE_TYPE),
      date_start = lubridate::dmy(LEASE_DATE),
      date_start = dplyr::case_when(
        is.na(date_start) ~ lubridate::mdy(LEASE_DATE),
        .default = date_start
      ),
      lease_term_years = stringr::str_trim(LEASE_TERM, side = "both"),
      lease_term_years = as.numeric(gsub(" years", "", tolower(lease_term_years))),
      date_end = format(date_start + lubridate::years(lease_term_years), "%d-%m-%Y")
    ) |>
    dplyr::select(
      dataset, classification, type, date_start, date_end,
      lease_number = LEASE_NUMBER, protraction_number = PROTRACTION_NUMBER
    )

  ## Project phase areas proposed
  boem[["project_phase_areas_proposed"]] <- sf::st_read(
    input_files[stringr::str_detect(input_files, "Project_Phase_Areas_Proposed")],
    quiet = TRUE
  ) |>
    sf::st_transform(4326) |>
    dplyr::mutate(
      dataset = "wind_usa",
      classification = "project phase areas proposed"
    ) |>
    dplyr::select(dataset, classification, lease_number = LEASE_NUMBER)

  ## Substations
  boem[["substations"]] <- sf::st_read(
    input_files[stringr::str_detect(input_files, "Substations")],
    quiet = TRUE
  ) |>
    sf::st_transform(4326) |>
    dplyr::mutate(
      dataset = "wind_usa",
      classification = "substations",
      status = STATUS,
      location = LOCATION_TYPE
    ) |>
    dplyr::select(dataset, classification, status, location, lease_number = LEASE_NUMBER)


  ## Turbine_Locations
  boem[["turbine_locations"]] <- sf::st_read(
    input_files[stringr::str_detect(input_files, "Turbine_Locations")],
    quiet = TRUE
  ) |>
    sf::st_transform(4326) |>
    dplyr::mutate(
      dataset = "wind_usa",
      classification = "turbine locations",
      status = STATUS
    ) |>
    dplyr::select(dataset, classification, status, lease_number = LEASE_NUMBER)

  ## Cable_Interconnections
  boem[["cable_interconnections"]] <- sf::st_read(
    input_files[stringr::str_detect(input_files, "Cable_Interconnections")],
    quiet = TRUE
  ) |>
    sf::st_transform(4326) |>
    dplyr::mutate(
      dataset = "wind_usa",
      classification = "cable interconnections",
      status = STATUS
    ) |>
    dplyr::select(dataset, classification, status, lease_number = LEASE_NUMBER)

  ## Ocean_Observing_Devices
  boem[["ocean_observing_devices"]] <- sf::st_read(
    input_files[stringr::str_detect(input_files, "Ocean_Observing_Devices")],
    quiet = TRUE
  ) |>
    sf::st_transform(4326) |>
    dplyr::mutate(
      dataset = "wind_usa",
      classification = "ocean observing devices",
      status = STATUS
    ) |>
    dplyr::select(dataset, classification, status, lease_number = LEASE_NUMBER)

  ## Export_Cable_Corridors
  boem[["export_cable_corridors"]] <- sf::st_read(
    input_files[stringr::str_detect(input_files, "Export_Cable_Corridors")],
    quiet = TRUE
  ) |>
    sf::st_transform(4326) |>
    dplyr::mutate(
      dataset = "wind_usa",
      classification = "export cable corridors",
      status = STATUS
    ) |>
    dplyr::select(dataset, classification, status, lease_number = LEASE_NUMBER)

  ## Cable_Landings
  boem[["cable_landings"]] <- sf::st_read(
    input_files[stringr::str_detect(input_files, "Cable_Landings")],
    quiet = TRUE
  ) |>
    sf::st_transform(4326) |>
    dplyr::mutate(
      dataset = "wind_usa",
      classification = "cable landings",
      status = STATUS
    ) |>
    dplyr::select(dataset, classification, status, lease_number = LEASE_NUMBER)

  ## Project_Inter-array_Cables
  boem[["project_inter-array_cables"]] <- sf::st_read(
    input_files[stringr::str_detect(input_files, "Project_Inter-array_Cables")],
    quiet = TRUE
  ) |>
    sf::st_transform(4326) |>
    dplyr::mutate(
      dataset = "wind_usa",
      classification = "project inter-array cables",
      status = STATUS
    ) |>
    dplyr::select(dataset, classification, status, lease_number = LEASE_NUMBER)

  ## Make sure geoometries are all the same
  boem <- lapply(boem, function(x) {
    x |> sf::st_set_geometry("geom")
  })

  # Bind
  boem <- dplyr::bind_rows(boem) |>
    dplyr::mutate(
      uid = sprintf("wind_usa_lease_%04d", dplyr::row_number()),
      status = tolower(status)
    ) |>
    dplyr::select(
      uid, dataset, classification, type, date_start,
      date_end, lease_number, protraction_number
    ) |>
    sf::st_make_valid()

  # Export
  sf::st_write(
    boem,
    dsn = file.path(output_path, "offshore_wind_usa.gpkg"),
    quiet = TRUE,
    delete_dsn = TRUE
  )

  # Remove unzipped files
  fs::dir_delete(tmp)
}
