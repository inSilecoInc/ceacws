#' Script to get the area of interest for the assessment
#'
#' @export
get_aoi <- function() {
  global_parameters()
  bbox <- unlist(param$bbox$base)[c("xmin", "xmax", "ymin", "ymax")] |>
    pipedat::bbox_poly(param$crs)

  # Land
  land <- sf::st_read("data/basemap/usa.gpkg", quiet = TRUE) |>
    sf::st_union(sf::st_read("data/basemap/canada_full.gpkg", quiet = TRUE)) |>
    sf::st_union(sf::st_read("data/basemap/greenland.gpkg", quiet = TRUE)) |>
    sf::st_transform(param$crs)

  # Remove land
  aoi <- sf::st_difference(bbox, land)

  # Make grid & export
  pipedat::pipegrid(aoi, cellsize = 0.01, crs = param$crs)
}
