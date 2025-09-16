#' selectionMap
#'
#' @description A function to select a map.
#'
#' @param geoms a sf object.
#' @param layer a specific layer.
#' @param set_view a logical. Should the view be (re)set.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

base_map <- function() {
    leaflet::leaflet(height = 2000) |>
        leafem::addMouseCoordinates() |>
        leaflet::addTiles() |>
        leaflet::addProviderTiles("Esri.OceanBasemap", group = "OceaBasemap") |>
        leaflet::addProviderTiles("OpenStreetMap", group = "OpenStreetMap") |>
        leaflet::addLayersControl(
            baseGroups = c("OpenStreetMap", "Ocean Basemap"),
            position = "bottomleft"
        ) |>
        leaflet::fitBounds(
            lng1 = map_bbox$lng1,
            lat1 = map_bbox$lat1,
            lng2 = map_bbox$lng2,
            lat2 = map_bbox$lat2
        )
}

set_view_to_city <- function(city, map) {
    bb <- osmdata::getbb(city)
    if (!is.na(bb[1, 1])) {
        # https://github.com/r-spatial/sf/issues/572
        map |>
            leaflet::clearBounds() |>
            # leaflet::flyToBounds( # funky but not very useful
            leaflet::fitBounds(
                lng1 = bb[1, 1],
                lat1 = bb[2, 1],
                lng2 = bb[1, 2],
                lat2 = bb[2, 2]
            )
    } else {
        map
    }
}

reset_view <- function(map) {
    map |>
        leaflet::clearBounds() |>
        leaflet::fitBounds(
            lng1 = map_bbox$lng1,
            lat1 = map_bbox$lat1,
            lng2 = map_bbox$lng2,
            lat2 = map_bbox$lat2
        )
}
