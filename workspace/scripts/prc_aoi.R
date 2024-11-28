prc_aoi <- function(input_files, output_path) {
  # Bounding box
  bbox <- c(-80, 40, -40, 70)
  bbox <- sf::st_bbox(c(
    xmin = bbox[1], ymin = bbox[2],
    xmax = bbox[3], ymax = bbox[4]
  ), crs = 4326) |>
    sf::st_as_sfc() |>
    sf::st_as_sf()

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
    smoothr::fill_holes(threshold = units::set_units(1000, km^2))

  # 5km buffer inland
  terre_buf <- sf::st_buffer(terre, -5000) |>
    sf::st_transform(4326)

  # AOI
  aoi <- sf::st_difference(bbox, terre_buf)

  # Export partial data for use in report (this should be revisited)
  sf::st_write(
    aoi,
    dsn = file.path(output_path, "aoi.gpkg"),
    quiet = TRUE,
    delete_dsn = TRUE
  )

  # Create grid
  bbox <- terra::ext(aoi)
  grid <- terra::rast(
    ext = bbox,
    resolution = 0.01,
    crs = terra::crs(aoi)
  )
  terra::values(grid) <- 1
  grid <- terra::mask(grid, aoi)
  terra::writeRaster(
    grid,
    filename = file.path(output_path, "grid.tif"),
    overwrite = TRUE,
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
  )
}
