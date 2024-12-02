ana_night_light_monthly <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/night_light_monthly-1.0.0/"
  # # dir.create(output_path)
  # input_files <- c(
  #   "workspace/data/harvested/viirs_nighttime_light-1.0.0/processed",
  #   "workspace/data/harvested/aoi-1.0.0/processed/aoi.gpkg",
  #   "workspace/data/harvested/aoi-1.0.0/processed/grid.tif"
  # )
  input_files <- unlist(input_files)

  grid <- terra::rast(input_files[basename(input_files) == "grid.tif"])
  aoi <- sf::st_read(input_files[basename(input_files) == "aoi.gpkg"], quiet = TRUE)
  input_files <- input_files[!basename(input_files) %in% c("grid.tif", "aoi.gpkg")] |>
    dir(full.names = TRUE, pattern = ".tif")

  # Get necessary data to mask only along the coast
  # Coastal outline
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
    "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_CAN_0.json",
    "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_GRL_0.json"
  ) |>
    lapply(fetch_data)

  terre <- sf::st_union(coastlines[[1]], coastlines[[2]]) |>
    sf::st_union(coastlines[[3]]) |>
    sf::st_transform(32198) |> # To get units in meters
    smoothr::fill_holes(threshold = units::set_units(1000, km^2)) |>
    sf::st_buffer(1000) |>
    sf::st_transform(4326)

  # Coastline
  coast <- sf::st_intersection(terre, aoi)

  # Data
  for (i in seq_len(length(input_files))) {
    # Resample
    dat <- terra::rast(input_files[i]) |>
      terra::resample(grid) |>
      terra::mask(coast)

    # Export
    current_month <- basename(input_files[i]) |>
      tools::file_path_sans_ext() |>
      stringr::str_split("_") |>
      unlist()
    current_month <- lubridate::ymd(paste(current_month[4], current_month[5], "01", sep = "-"))
    output_file <- file.path(output_path, paste0("night_light_", current_month, ".tif"))
    terra::writeRaster(
      dat,
      output_file,
      overwrite = TRUE,
      gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
    )
  }
}
