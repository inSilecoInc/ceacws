#' Generate a Spatial Grid Template
#'
#' Creates a rasterized grid within a specified area of interest (AOI) based on a defined cell size and coordinate reference system (CRS).
#'
#' @param aoi An area of interest for the grid, which can be provided as an `sf` or `sfc` object, bounding box (`bbox`), or a numeric vector. If provided as a bounding box or numeric vector, it will be converted into an `sf` object.
#' @param cellsize Numeric. The cell size for the grid in the units of the specified CRS.
#' @param crs Numeric. Coordinate reference system (CRS) specified as an EPSG code. Defaults to 4326 (WGS84).
#'
#' @details
#' The function first converts the area of interest (`aoi`) into an `sf` object if it is provided in another supported format (e.g., bounding box, numeric vector).
#' It then uses this `sf` object to create a grid with the specified cell size, rasterized using the `stars` package.
#'
#' @return A rasterized grid (`stars` object) representing the spatial grid template within the specified AOI.
#'
#' @examples
#' \dontrun{
#' # Example 1: Using an sf polygon as AOI
#' library(sf)
#' poly <- st_as_sf(st_sfc(st_polygon(list(rbind(
#'   c(-100, 40), c(-100, 45), c(-95, 45), c(-95, 40), c(-100, 40)
#' )))), crs = 4326)
#' grid <- make_grid(poly, cellsize = 0.1)
#'
#' # Example 2: Using a bounding box as AOI
#' bbox <- c(xmin = -100, ymin = 40, xmax = -95, ymax = 45)
#' grid <- make_grid(bbox, cellsize = 0.1)
#' }
#'
#' @importFrom sf st_as_sf st_bbox st_as_sfc
#' @importFrom stars st_rasterize
#' @export
make_grid <- function(aoi, cellsize, crs = 4326) {
  # Create grid template
  if ("sfc" %in% class(aoi)) {
    aoi <- sf::st_as_sf(aoi, crs = crs)
  } else if (any(c("bbox", "numeric", "integer") %in% class(aoi))) {
    aoi <- sf::st_bbox(aoi, crs = crs) |>
      sf::st_as_sfc() |>
      sf::st_as_sf()
  }
  grd <- stars::st_rasterize(aoi, dx = cellsize, dy = cellsize)
  grd
}
