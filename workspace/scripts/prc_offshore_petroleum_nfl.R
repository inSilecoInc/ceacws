prc_offshore_petroleum_nfl <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/offshore_petroleum_nfl-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/offshore_petroleum_nfl-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "active_exploration_licenses.zip",
  #     "active_significant_discovery_licenses.zip",
  #     "active_production_licenses.zip",
  #     "production_installations.zip",
  #     "well_info_summary.xlsx",
  #     "delineation_wells.zip",
  #     "development_wells.zip",
  #     "dual_classified_wells.zip",
  #     "exploration_wells.zip",
  #     "eastern_newfoundland_nl23_cfb01.zip",
  #     "south_eastern_newfoundland_nl23_cfb02.zip",
  #     "eastern_newfoundland_nl24_cfb01.zip",
  #     "labrador_south_nl02_ls.zip",
  #     "eastern_newfoundland_nl06_en.zip",
  #     "north_eastern_newfoundland_nl01_nen.zip",
  #     "southern_newfoundland_nl01_sn.zip"
  #   )
  # )

  # Utility functions
  format_dates1 <- function(dat, dataset, classification) {
    dat |>
      dplyr::mutate(
        dplyr::across(
          dplyr::ends_with("_dte"),
          ~ stringr::str_replace(.x, "\\t\\r\\n\\t\\r\\n\\t\\r\\n\\t\\r\\n\\t\\r\\n", NA_character_)
        )
      ) |>
      dplyr::mutate(
        dataset = dataset,
        classification = classification,
        start_date = lubridate::mdy(spud_date),
        end_date = dplyr::case_when(
          !is.na(re5term_dte) ~ re5term_dte,
          !is.na(re4term_dte) ~ re4term_dte,
          !is.na(re3term_dte) ~ re3term_dte,
          !is.na(re2term_dte) ~ re2term_dte,
          !is.na(re1term_dte) ~ re1term_dte,
          .default = term_date
        ) |>
          lubridate::mdy()
      ) |>
      dplyr::select(dataset, classification, start_date, end_date, status = well_status)
  }

  format_dates2 <- function(dat, dataset, classification) {
    dat |>
      dplyr::mutate(
        dataset = dataset,
        classification = classification,
        start_date = lubridate::mdy(date_posted),
        end_date = lubridate::ymd(paste0(cfb_clse_dte, "-01-01"))
      ) |>
      dplyr::select(dataset, classification, start_date, end_date, status)
  }

  format_dates3 <- function(dat, dataset, classification) {
    dat |>
      dplyr::mutate(
        dataset = dataset,
        classification = classification,
        start_date = lubridate::mdy(cfb_open_dte),
        end_date = lubridate::mdy(cfb_clse_dte)
      ) |>
      dplyr::select(dataset, classification, start_date, end_date, status)
  }

  convert_dms_to_decimal <- function(dms) {
    # Use regular expressions to extract degrees, minutes, and seconds
    matches <- stringr::str_match(dms, "([0-9]+)° ([0-9]+)' ([0-9\\.]+)\"?")

    # Convert the extracted components to numeric
    degrees <- as.numeric(matches[, 2])
    minutes <- as.numeric(matches[, 3])
    seconds <- as.numeric(matches[, 4])

    # Calculate decimal degrees
    decimal_degrees <- degrees + minutes / 60 + seconds / 3600

    return(decimal_degrees)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Data
  input_files <- unlist(input_files)

  ## Unzip shapefiles
  zip <- input_files[tools::file_ext(input_files) == "zip"]
  tmp <- file.path(output_path, "tmp")
  out <- file.path(tmp, tools::file_path_sans_ext(basename(zip)))
  dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
  for (i in seq_len(length(zip))) archive::archive_extract(zip[i], out[i])

  # Load shapefiles
  ## bbox to remove erroneous point locations
  bbox <- sf::st_bbox(c(xmin = -100, xmax = -20, ymin = 35, ymax = 85), crs = 4326) |>
    sf::st_as_sfc()

  dat <- lapply(out, function(x) {
    x <- dir(x, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE) |>
      sf::st_read(quiet = TRUE) |>
      sf::st_transform(4326) |>
      sf::st_zm(drop = TRUE, what = "ZM") |>
      janitor::clean_names()
    id <- suppressMessages(suppressWarnings(
      sf::st_within(x, bbox, sparse = FALSE) |>
        as.logical()
    ))
    x[id, ]
  })
  names(dat) <- tools::file_path_sans_ext(basename(zip))

  # Data processing
  ## active_exploration_licenses
  dat[["active_exploration_licenses"]] <- dat[["active_exploration_licenses"]] |>
    dplyr::mutate(
      dataset = "active_exploration_licenses",
      classification = "exploration licenses",
      start_date = lubridate::mdy(effective),
      end_date = lubridate::mdy(period2exp),
      status = "Active"
    ) |>
    dplyr::select(dataset, classification, status, start_date, end_date)

  ## active_significant_discovery_licenses
  dat[["active_significant_discovery_licenses"]] <- dat[["active_significant_discovery_licenses"]] |>
    dplyr::mutate(
      dataset = "active_significant_discovery_licenses",
      classification = "significant discovery licenses",
      start_date = lubridate::mdy(effective),
      status = "Active"
    ) |>
    dplyr::select(dataset, classification, status, start_date)

  ## active_production_licenses
  dat[["active_production_licenses"]] <- dat[["active_production_licenses"]] |>
    dplyr::mutate(
      dataset = "active_production_licenses",
      classification = "production licenses",
      start_date = lubridate::mdy(effective),
      status = "Active"
    ) |>
    dplyr::select(dataset, classification, status, start_date)


  # production_installations
  dat[["production_installations"]] <- dat[["production_installations"]] |>
    dplyr::mutate(
      dataset = "production_installations",
      classification = "production",
      start_date = lubridate::mdy(first_oil)
    ) |>
    dplyr::rename(status = curr_status) |>
    dplyr::select(dataset, classification, status, start_date)


  # delineation_wells
  dat[["delineation_wells"]] <- dat[["delineation_wells"]] |>
    format_dates1("delineation_wells", "delineation")


  # development_wells
  dat[["development_wells"]] <- dat[["development_wells"]] |>
    format_dates1("development_wells", "development")

  # dual_classified_wells
  dat[["dual_classified_wells"]] <- dat[["dual_classified_wells"]] |>
    format_dates1("dual_classified_wells", "exploration / delineation")


  # exploration_wells
  dat[["exploration_wells"]] <- dat[["exploration_wells"]] |>
    format_dates1("exploration_wells", "exploration")


  # eastern_newfoundland_nl23_cfb01
  dat[["eastern_newfoundland_nl23_cfb01"]] <- dat[["eastern_newfoundland_nl23_cfb01"]] |>
    format_dates3("eastern_newfoundland_nl23_cfb01", "call for bids")

  # south_eastern_newfoundland_nl23_cfb02
  dat[["south_eastern_newfoundland_nl23_cfb02"]] <- dat[["south_eastern_newfoundland_nl23_cfb02"]] |>
    format_dates3("south_eastern_newfoundland_nl23_cfb02", "call for bids")

  # eastern_newfoundland_nl24_cfb01
  dat[["eastern_newfoundland_nl24_cfb01"]] <- dat[["eastern_newfoundland_nl24_cfb01"]] |>
    format_dates3("eastern_newfoundland_nl24_cfb01", "call for bids")

  # labrador_south_nl02_ls
  dat[["labrador_south_nl02_ls"]] <- dat[["labrador_south_nl02_ls"]] |>
    format_dates2("labrador_south_nl02_ls", "sector")

  # eastern_newfoundland_nl06_en
  dat[["eastern_newfoundland_nl06_en"]] <- dat[["eastern_newfoundland_nl06_en"]] |>
    format_dates2("eastern_newfoundland_nl06_en", "sector")

  # north_eastern_newfoundland_nl01_nen
  dat[["north_eastern_newfoundland_nl01_nen"]] <- dat[["north_eastern_newfoundland_nl01_nen"]] |>
    format_dates2("north_eastern_newfoundland_nl01_nen", "sector")

  # southern_newfoundland_nl01_sn
  dat[["southern_newfoundland_nl01_sn"]] <- dat[["southern_newfoundland_nl01_sn"]] |>
    format_dates2("southern_newfoundland_nl01_sn", "sector")

  # Bind dat
  dat <- dplyr::bind_rows(dat)

  # Well info summary (xlsx file)
  xlsx <- input_files[tools::file_ext(input_files) == "xlsx"]
  suppressWarnings({
    suppressMessages({
      wells_info <- readxl::read_xlsx(xlsx, skip = 3) |>
        dplyr::slice(-dplyr::n()) |>
        janitor::clean_names() |>
        dplyr::mutate(
          start_date = dplyr::case_when(
            !is.na(as.numeric(spud_date)) ~
              lubridate::as_date(as.numeric(spud_date), origin = "1899-12-30"),
            .default = lubridate::dmy(spud_date)
          ),
          end_date = dplyr::case_when(
            !is.na(as.numeric(well_termination_date)) ~
              lubridate::as_date(as.numeric(well_termination_date), origin = "1899-12-30"),
            .default = lubridate::dmy(well_termination_date)
          )
        ) |>
        dplyr::select(
          well_number,
          classification, start_date, end_date,
          status = well_status,
          latitude = surface_lat_nad_83, longitude = surface_long_nad_83
        ) |>
        dplyr::group_by(well_number, status, classification) |>
        dplyr::arrange(start_date) |>
        dplyr::summarize(
          start_date = min(start_date),
          end_date = max(end_date),
          latitude = convert_dms_to_decimal(latitude[!is.na(latitude)]),
          longitude = -convert_dms_to_decimal(longitude[!is.na(longitude)])
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(dataset = "well_info_summary") |>
        data.frame() |>
        sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
        dplyr::select(dataset, classification, start_date, end_date, status) |>
        dplyr::mutate(
          classification = tolower(classification),
          classification = stringr::str_replace(
            classification,
            "development/delineation",
            "development / delineation"
          )
        ) |>
        dplyr::filter(!is.na(status))
    })
  })

  # Bind
  offshore_petroleum_nfl <- dplyr::bind_rows(dat, wells_info) |>
    dplyr::mutate(uid = sprintf("petroleum_nfl_%04d", dplyr::row_number())) |>
    tidyr::separate_rows(classification, sep = " / ") |>
    dplyr::mutate(
      subtype = dplyr::case_when(
        status %in% c("Abandoned Oil Well", "Suspended Oil Well", "Oil Well") ~ "Oil Well",
        status %in% c("Abandoned Oil Show", "Suspended Oil Show", "Oil Show") ~ "Oil Show",
        status %in% c("Abandoned Oil Producer", "Suspended Oil Producer", "Oil Producer") ~ "Oil Producer",
        status %in% c("Abandoned Gas Well", "Suspended Gas Well", "Gas Well") ~ "Gas Well",
        status %in% c("Abandoned Gas Show", "Suspended Gas Show", "Gas Show") ~ "Gas Show",
        status %in% c("Abandoned Gas Injector", "Suspended Gas Injector", "Gas Injector") ~ "Gas Injector",
        status %in% c("Abandoned Water Injector", "Suspended Water Injector", "Water Injector") ~ "Water Injector",
        status %in% c(
          "Abandoned Oil and Gas Well", "Suspended Oil and Gas Well", "Oil and Gas Well"
        ) ~ "Oil and Gas Well",
        status %in% c(
          "Abandoned Oil and Gas Show", "Suspended Oil and Gas Show", "Oil and Gas Show"
        ) ~ "Oil and Gas Show",
        status %in% "Production Operations" ~ "Production Operations",
        .default = NA_character_
      ),
      status = dplyr::case_when(
        status %in% c(
          "Abandoned Oil Well", "Abandoned Gas Well", "Abandoned",
          "Abandoned Oil and Gas Well", "Abandoned Oil Show",
          "Abandoned Oil and Gas Show", "Abandoned Gas Show",
          "Abandoned Gas Injector", "Abandoned Oil Producer",
          "Abandoned Water Injector"
        ) ~ "Abandoned",
        status %in% c(
          "Suspended Oil Well", "Suspended Oil Show",
          "Suspended Oil and Gas Well", "Suspended",
          "Suspended Oil Producer", "Suspended Water Injector"
        ) ~ "Suspended",
        status %in% c(
          "Oil Producer", "Water Injector", "Gas Injector",
          "Production Operations", "Active"
        ) ~ "Active",
        status == "Drilling" ~ "Drilling",
        status == "Disposal Well" ~ "Disposal",
        status == "Closed" ~ "Closed",
        status == "Off Station" ~ "Off Station",
        .default = NA_character_
      )
    ) |>
    dplyr::relocate(uid)

  # Export
  sf::st_write(
    offshore_petroleum_nfl,
    dsn = file.path(output_path, "offshore_petroleum_nfl.gpkg"),
    quiet = TRUE,
    delete_dsn = TRUE
  )

  # Remove unzipped files
  fs::dir_delete(tmp)
}
