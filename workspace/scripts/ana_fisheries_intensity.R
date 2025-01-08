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

  # Gear
  gear <- vroom::vroom(input_files[basename(input_files) == "gear.csv"], progress = FALSE, show_col_types = FALSE) |>
    dplyr::select(codes, description_anglais) |>
    dplyr::mutate(gear_type = dplyr::case_when(
      codes %in% c(4, 5, 27, 28, 29, 30, 34, 35, 36, 41, 42, 43, 48) ~ "gillnet",
      codes %in% c(37, 38, 39, 40, 51, 52) ~ "longline",
      codes %in% c(9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19) ~ "trawl",
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
    dplyr::distinct()

  # Species
  species <- vroom::vroom(
    input_files[basename(input_files) == "species.csv"],
    progress = FALSE, show_col_types = FALSE
  ) |>
    dplyr::select(esp_stat, da_esp, dl_esp) # |>
  # dplyr::left_join(traits, by = c("dl_esp" = "species"))

  # Logbooks
  logbooks <- arrow::read_parquet(input_files[basename(input_files) == "logbooks.parquet"]) |>
    # Add `prespvis` to list if target species is important
    dplyr::select(date, jr_mouil, eff_hre, engin, nb_engin, prof, cl_prof, prespvis, latitude, longitude) |>
    dplyr::distinct() |>
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
    dplyr::select(date, effort_days, engin, nb_engin, depth_fathoms, prespvis, latitude, longitude) |>
    dplyr::left_join(species, by = c("prespvis" = "esp_stat"), relationship = "many-to-many") |>
    dplyr::left_join(gear, by = c("engin" = "codes"), relationship = "many-to-many") |>
    dplyr::filter(!is.na(da_esp), !prespvis %in% c(0, 999))

  # Intensity per gear type
  intensity <- list(
    gillnet_intensity(logbooks),
    longline_intensty(logbooks),
    trawl_intensity(logbooks),
    purse_seine_intensity(logbooks)
  ) |>
    dplyr::bind_rows()

  # Gear table
  gear_summary <- intensity |>
    dplyr::group_by(gear_type, description_anglais) |>
    dplyr::summarise(
      n_event = dplyr::n(),
      intensity = sum(intensity)
    )
  vroom::vroom_write(gear_summary, file.path(output_path, "gear_summary.csv"), delim = ",")

  # Species table
  gear_species_summary <- intensity |>
    dplyr::group_by(gear_type, prespvis, da_esp, dl_esp) |>
    dplyr::summarise(
      n_event = dplyr::n(),
      intensity = sum(intensity)
    )
  vroom::vroom_write(gear_species_summary, file.path(output_path, "gear_species_summary.csv"), delim = ",")

  # Spatial object
  intensity <- intensity |>
    dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date)) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    sf::st_transform(sf::st_crs(aoi))

  # Rasterize and export
  group_rasterize_export(
    intensity,
    grouping_vars = c("gear_type", "year", "month"),
    fun = "sum",
    field = "intensity",
    grid = grid,
    aoi = aoi,
    dataset_name = "fisheries_intensity",
    output_path = output_path
  )
}

gillnet_intensity <- function(logbooks) {
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

longline_intensty <- function(logbooks) {
  logbooks |>
    dplyr::filter(gear_type == "longline", !is.na(effort_days)) |>
    # TODO: classify between pelagic and demersal using target species
    dplyr::mutate(intensity = nb_engin * effort_days)
}

trawl_intensity <- function(logbooks) {
  logbooks |>
    dplyr::filter(gear_type == "trawl", !is.na(effort_days)) |>
    # TODO: classify between pelagic and demersal using target species
    dplyr::mutate(intensity = nb_engin * effort_days)
}

purse_seine_intensity <- function(logbooks) {
  logbooks |>
    dplyr::filter(gear_type == "purse seine", !is.na(effort_days)) |>
    dplyr::mutate(intensity = nb_engin * effort_days)
}
