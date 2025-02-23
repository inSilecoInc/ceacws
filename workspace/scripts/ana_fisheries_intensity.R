ana_fisheries_intensity <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/fisheries_intensity-1.0.0/"
  # # dir.create(output_path)
  # input_files <- c(
  #   "workspace/data/harvested/fisheries_dfo-1.0.0/processed/logbooks.parquet",
  #   "workspace/data/harvested/fisheries_dfo-1.0.0/processed/gear.csv",
  #   "workspace/data/harvested/fisheries_dfo-1.0.0/processed/species.csv",
  #   "workspace/data/harvested/aoi-1.0.0/processed/aoi_fisheries.gpkg",
  #   "workspace/data/harvested/aoi-1.0.0/processed/grid_fisheries.tif"
  # )
  input_files <- unlist(input_files)

  # Area of interest & Grid
  grid <- terra::rast(input_files[basename(input_files) == "grid_fisheries.tif"])
  aoi <- sf::st_read(input_files[basename(input_files) == "aoi_fisheries.gpkg"], quiet = TRUE)

  # =============================================================================================================
  # Data preparation
  # Gear
  gear <- vroom::vroom(input_files[basename(input_files) == "gear.csv"], progress = FALSE, show_col_types = FALSE) |>
    dplyr::select(codes, description_anglais) |>
    dplyr::mutate(gear_type = dplyr::case_when(
      codes %in% c(4, 5, 27, 28, 29, 30, 34, 35, 36, 41, 42, 43, 48) ~ "gillnet",
      codes %in% c(37, 38, 39, 40, 51, 52) ~ "longline",
      codes %in% c(10, 11, 12, 16) ~ "bottom trawl",
      codes %in% c(13, 14, 15, 17, 18) ~ "midwater trawl",
      codes %in% c(9, 19) ~ "shrimp trawl",
      codes %in% 31 ~ "purse seine",
      .default = NA_character_
    ))

  # Species environment traits
  # ------------------------------------------------------------------------------------------------------------
  # Environments:
  #   Grouped as demersal:
  #   - Bathydemersal: Living and/or feeding on or near the bottom, below 200 m
  #   - Benthic: Living and feeding on the bottom
  #   - Demersal: Living and/or feeding on or near the bottom, between 0 and 200 m
  #   - Benthopelagic: Living and/or feeding on or near the bottom, as well as in midwater, between and 200 m
  #
  #   Grouped as pelagic
  #   - Bathypelagic: Occurring mainly in open water below 200 m, not feeding on benthic organisms
  #   - Reef-associated: living and/or feeding on or near reefs, between 0 and 200 m;
  #   - Coastal: living on or near the coastline;
  #   - Pelagic: Occurring mainly in the water column between 0 and 200 m, not feeding on benthic organisms
  # ------------------------------------------------------------------------------------------------------------
  traits <- vroom::vroom(
    "https://raw.githubusercontent.com/Ecosystem-Assessments/Species_Traits/refs/heads/master/Data/SpeciesTraits/Environment.csv",
    progress = FALSE, show_col_types = FALSE
  ) |>
    dplyr::rename(species = `...1`) |>
    dplyr::select(-terrestrial) |>
    tidyr::pivot_longer(-species, names_to = "environment", values_to = "present") |>
    dplyr::filter(present == 1) |>
    dplyr::select(-present) |>
    dplyr::mutate(environment = dplyr::case_when(
      environment %in% c("demersal", "bathydemersal", "benthic") ~ "demersal",
      environment %in% c("benthopelagic", "bathypelagic", "reef-associated", "pelagic", "coastal") ~ "pelagic",
      .default = NA_character_
    )) |>
    dplyr::distinct() |>
    dplyr::bind_rows(
      data.frame(
        species = c(
          "Anarhichadidae",
          "Cottidae",
          "Katsuwonus pelamis",
          "Lamna nasus, Lamna cornubica",
          "Makaira nigricans",
          "Myxina glutinosa",
          "Pectinidae",
          "Pleuronectes americanus",
          "Pleuronectes ferrugineus",
          "Pleuronectiformes",
          "Raja sp",
          "Raja spp.",
          "Sebastes sp",
          "Squaliformes",
          "Tetrapterus albidus",
          "Thunnus albacares",
          "Commun crab (fishing licence)",
          "Exploratory scallop (fishing licence)",
          "Unspecified groundfish",
          "Unspecified pelagic fish",
          "Unspecified tuna"
        ),
        environment = c(
          "demersal",
          "demersal",
          "pelagic",
          "pelagic",
          "pelagic",
          "demersal",
          "demersal",
          "demersal",
          "demersal",
          "demersal",
          "demersal",
          "demersal",
          "demersal",
          "demersal",
          "demersal",
          "pelagic",
          "demersal",
          "demersal",
          "demersal",
          "pelagic",
          "pelagic"
        )
      )
    )

  # Species
  species <- vroom::vroom(
    input_files[basename(input_files) == "species.csv"],
    progress = FALSE, show_col_types = FALSE
  ) |>
    dplyr::select(esp_stat, da_esp, dl_esp) |>
    dplyr::mutate(dl_esp = dplyr::if_else(
      is.na(dl_esp), da_esp, dl_esp
    )) |>
    dplyr::left_join(traits, by = c("dl_esp" = "species"))

  # Logbooks
  logbooks <- arrow::read_parquet(input_files[basename(input_files) == "logbooks.parquet"]) |>
    # Add `prespvis` to list if target species is important
    dplyr::select(
      date, jr_mouil, eff_hre, engin, nb_engin, prof, cl_prof,
      prespvis, prespcap, pds_vif, latitude, longitude
    ) |>
    # Set nb_engin to 1 if `nb_engin == 0` | `is.na(nb_engin)`
    dplyr::mutate(nb_engin = dplyr::case_when(
      nb_engin == 0 ~ 1,
      is.na(nb_engin) ~ 1,
      .default = nb_engin
    )) |>
    # Adjust fishing effort if NA or 0 to gear soak time in days
    dplyr::mutate(
      eff_hre = eff_hre / 24,
      effort_days = dplyr::case_when(
        is.na(eff_hre) ~ jr_mouil,
        eff_hre == 0 ~ jr_mouil,
        .default = eff_hre
      )
    ) |>
    # Adjust depth using mean of depth classes if unavailable in `prof`
    dplyr::mutate(
      depth_fathoms = dplyr::case_when(
        !is.na(prof) ~ prof, # Use prof directly in fathoms if available
        cl_prof == 1 ~ 12.5,
        cl_prof == 2 ~ 37.5,
        cl_prof == 3 ~ 62.5,
        cl_prof == 4 ~ 87.5,
        cl_prof == 5 ~ 112.5,
        cl_prof == 6 ~ 137.5,
        cl_prof == 7 ~ 162.5,
        cl_prof == 8 ~ 187.5,
        cl_prof == 9 ~ 225,
        cl_prof == 0 ~ 275,
        .default = NA_real_
      ),
    ) |>
    dplyr::select(
      date, effort_days, engin, nb_engin, depth_fathoms,
      prespvis, prespcap, pds_vif, latitude, longitude
    ) |>
    dplyr::left_join(species, by = c("prespvis" = "esp_stat"), relationship = "many-to-many") |>
    dplyr::left_join(gear, by = c("engin" = "codes"), relationship = "many-to-many") |>
    dplyr::filter(!is.na(da_esp), !prespvis %in% c(0, 999)) |>
    # Reclassify longlines based on target  species
    dplyr::mutate(gear_type = dplyr::case_when(
      gear_type == "longline" & environment == "demersal" ~ "demersal longline",
      gear_type == "longline" & environment == "pelagic" ~ "pelagic longline",
      .default = gear_type
    )) |>
    # Add periods to the data
    dplyr::mutate(
      month = lubridate::month(date),
      period = dplyr::case_when(
        lubridate::year(date) >= 1999 & lubridate::year(date) <= 2005 ~ "2000-2005",
        lubridate::year(date) >= 2006 & lubridate::year(date) <= 2010 ~ "2006-2010",
        lubridate::year(date) >= 2011 & lubridate::year(date) <= 2015 ~ "2011-2015",
        lubridate::year(date) >= 2016 & lubridate::year(date) <= 2021 ~ "2016-2020",
        .default = NA_character_
      )
    )
  # =============================================================================================================

  # =============================================================================================================
  # Fishing intensity

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Effort metrics
  tmp <- logbooks |>
    dplyr::select(
      date, period, month, effort_days, engin, nb_engin, depth_fathoms,
      prespvis, dl_esp, latitude, longitude, gear_type
    ) |>
    dplyr::distinct()

  effort <- list(
    # for gillnets slightly different data processing that I do not wish to apply to the logbooks data upfront
    gillnet_effort_gear_days(tmp),
    fishing_effort_gear(tmp, "demersal longline"),
    fishing_effort_gear(tmp, "pelagic longline"),
    fishing_effort_gear_days(tmp, "bottom trawl"),
    fishing_effort_gear_days(tmp, "midwater trawl"),
    fishing_effort_gear_days(tmp, "shrimp trawl"),
    fishing_effort_gear_days(tmp, "purse seine")
  ) |>
    dplyr::bind_rows() |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    sf::st_transform(sf::st_crs(aoi))

  # ------------------------------------------------------
  # Effort intensity per gear type
  group_rasterize_export(
    sf_object = effort,
    grouping_vars = c("gear_type", "period", "month"),
    # grouping_vars = c("gear_type"),
    fun = "sum",
    field = "intensity",
    grid = grid,
    aoi = aoi,
    dataset_name = "fisheries_effort",
    output_path = output_path
  )

  # -------------------------------------------------------
  # Effort gear species
  target_species <- effort |>
    sf::st_drop_geometry() |>
    dplyr::group_by(prespvis, dl_esp) |>
    dplyr::summarize(n = dplyr::n()) |>
    dplyr::filter(n > 5000)
  effort_species <- effort |>
    dplyr::filter(prespvis %in% target_species$prespvis)

  group_rasterize_export(
    sf_object = effort_species,
    grouping_vars = c("gear_type", "dl_esp", "period", "month"),
    # grouping_vars = c("gear_type", "dl_esp"),
    fun = "sum",
    field = "intensity",
    grid = grid,
    aoi = aoi,
    dataset_name = "fisheries_effort",
    output_path = output_path
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Landings target metrics
  tmp <- logbooks |>
    dplyr::filter(prespvis == prespcap) |>
    dplyr::group_by(
      date, period, month, engin, prespvis, dl_esp,
      latitude, longitude, gear_type
    ) |>
    dplyr::summarize(pds_vif = sum(pds_vif)) |>
    dplyr::ungroup() |>
    dplyr::distinct()

  landings_target <- list(
    # for gillnets slightly different data processing that I do not wish to apply to the logbooks data upfront
    fishing_landings(tmp, "gillnet"),
    fishing_landings(tmp, "demersal longline"),
    fishing_landings(tmp, "pelagic longline"),
    fishing_landings(tmp, "bottom trawl"),
    fishing_landings(tmp, "midwater trawl"),
    fishing_landings(tmp, "shrimp trawl"),
    fishing_landings(tmp, "purse seine")
  ) |>
    dplyr::bind_rows() |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    sf::st_transform(sf::st_crs(aoi))

  # -------------------------------------------------------
  # Landings target gear
  group_rasterize_export(
    sf_object = landings_target,
    grouping_vars = c("gear_type", "period", "month"),
    # grouping_vars = c("gear_type"),
    fun = "sum",
    field = "intensity",
    grid = grid,
    aoi = aoi,
    dataset_name = "fisheries_landings_target",
    output_path = output_path
  )

  # -------------------------------------------------------
  # Landings target gear species
  landings_target_species <- landings_target |>
    dplyr::filter(prespvis %in% target_species$prespvis)

  group_rasterize_export(
    sf_object = landings_target_species,
    grouping_vars = c("gear_type", "dl_esp", "period", "month"),
    # grouping_vars = c("gear_type", "dl_esp"),
    fun = "sum",
    field = "intensity",
    grid = grid,
    aoi = aoi,
    dataset_name = "fisheries_landings_target",
    output_path = output_path
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Landings target bycatch metrics
  tmp <- logbooks |>
    dplyr::group_by(date, period, month, engin, prespvis, dl_esp, latitude, longitude, gear_type) |>
    dplyr::summarize(pds_vif = sum(pds_vif)) |>
    dplyr::ungroup() |>
    dplyr::distinct()

  landings_bycatch <- list(
    # for gillnets slightly different data processing that I do not wish to apply to the logbooks data upfront
    fishing_landings(tmp, "gillnet"),
    fishing_landings(tmp, "demersal longline"),
    fishing_landings(tmp, "pelagic longline"),
    fishing_landings(tmp, "bottom trawl"),
    fishing_landings(tmp, "midwater trawl"),
    fishing_landings(tmp, "shrimp trawl"),
    fishing_landings(tmp, "purse seine")
  ) |>
    dplyr::bind_rows() |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    sf::st_transform(sf::st_crs(aoi))

  # -------------------------------------------------------
  # Landings target bycatch gear
  group_rasterize_export(
    sf_object = landings_bycatch,
    grouping_vars = c("gear_type", "period", "month"),
    # grouping_vars = c("gear_type"),
    fun = "sum",
    field = "intensity",
    grid = grid,
    aoi = aoi,
    dataset_name = "fisheries_landings_bycatch",
    output_path = output_path
  )

  # -------------------------------------------------------
  # Landings target gear species
  landings_bycatch_species <- landings_bycatch |>
    dplyr::filter(prespvis %in% target_species$prespvis)

  group_rasterize_export(
    sf_object = landings_bycatch_species,
    grouping_vars = c("gear_type", "dl_esp", "period", "month"),
    # grouping_vars = c("gear_type", "dl_esp"),
    fun = "sum",
    field = "intensity",
    grid = grid,
    aoi = aoi,
    dataset_name = "fisheries_landings_bycatch",
    output_path = output_path
  )


  # =============================================================================================================
  # Gear table
  gear_summary <- dplyr::left_join(
    effort |>
      sf::st_drop_geometry() |>
      dplyr::group_by(gear_type) |>
      dplyr::summarise(
        n_event_effort = dplyr::n(),
        effort = sum(intensity)
      ),
    landings_target |>
      sf::st_drop_geometry() |>
      dplyr::group_by(gear_type) |>
      dplyr::summarise(
        n_event_landings_target = dplyr::n(),
        landings_target = sum(intensity)
      ),
    by = "gear_type"
  ) |>
    dplyr::left_join(
      landings_bycatch |>
        sf::st_drop_geometry() |>
        dplyr::group_by(gear_type) |>
        dplyr::summarise(
          n_event_landings_bycatch = dplyr::n(),
          landings_bycatch = sum(intensity)
        ),
      by = "gear_type"
    )
  vroom::vroom_write(gear_summary, file.path(output_path, "gear_summary.csv"), delim = ",")

  # Species table
  gear_species_summary <- dplyr::left_join(
    effort |>
      sf::st_drop_geometry() |>
      dplyr::group_by(gear_type, prespvis, dl_esp) |>
      dplyr::summarise(
        n_event_effort = dplyr::n(),
        effort = sum(intensity)
      ),
    landings_target |>
      sf::st_drop_geometry() |>
      dplyr::group_by(gear_type, prespvis, dl_esp) |>
      dplyr::summarise(
        n_event_landings_target = dplyr::n(),
        landings_target = sum(intensity)
      ),
    by = c("gear_type", "prespvis", "dl_esp")
  ) |>
    dplyr::left_join(
      landings_bycatch |>
        sf::st_drop_geometry() |>
        dplyr::group_by(gear_type, prespvis, dl_esp) |>
        dplyr::summarise(
          n_event_landings_bycatch = dplyr::n(),
          landings_bycatch = sum(intensity)
        ),
      by = c("gear_type", "prespvis", "dl_esp")
    )

  vroom::vroom_write(gear_species_summary, file.path(output_path, "gear_species_summary.csv"), delim = ",")
}


fishing_effort_gear_days <- function(logbooks, gear) {
  logbooks |>
    dplyr::filter(gear_type %in% gear, !is.na(effort_days)) |>
    dplyr::mutate(intensity = nb_engin * effort_days)
}

fishing_effort_gear <- function(logbooks, gear) {
  logbooks |>
    dplyr::filter(gear_type == gear) |> # , !is.na(effort_days)) |> # !! Remove effort days?
    dplyr::mutate(intensity = nb_engin)
}

gillnet_effort_gear_days <- function(logbooks) {
  logbooks |>
    dplyr::filter(gear_type == "gillnet", !is.na(effort_days)) |>
    # Modify effort to 1h if depth > 200 meters
    dplyr::mutate(effort_days = dplyr::case_when(
      depth_fathoms > (200 / 1.8288) ~ 1,
      is.na(depth_fathoms) ~ 1,
      .default = effort_days
    )) |>
    # Calculate intensity based on number of gear per days fished
    dplyr::mutate(intensity = nb_engin * effort_days)
}

fishing_landings <- function(logbooks, gear) {
  logbooks |>
    dplyr::filter(gear_type %in% gear, !is.na(pds_vif)) |>
    dplyr::mutate(intensity = pds_vif)
}
