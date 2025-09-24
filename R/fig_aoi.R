# #' Figure of study area
# #'
# #' @export

# fig_aoi <- function(aoi) {
#   # out <- here::here("figures", "aoi")
#   # chk_create(out)
#   # png(
#   #   here::here(out, "aoi.png"),
#   #   res = param$figures$resolution,
#   #   width = param$figures$width,
#   #   height = param$figures$height,
#   #   units = "mm",
#   #   pointsize = param$figures$pointsize
#   # )

#   # aoi <- sf::st_read(here::here("project-data/aoi/aoi.gpkg"), quiet = TRUE)
#   # can <- sf::st_read(here::here("project-data/basemap/canada_full.gpkg"), quiet = TRUE)
#   # usa <- sf::st_read(here::here("project-data/basemap/usa.gpkg"), quiet = TRUE)
#   # gl <- sf::st_read(here::here("project-data/basemap/greenland.gpkg"), quiet = TRUE)
#   bbox <- sf::st_bbox(aoi)
#   par(family = "serif", mar = c(.5, .5, .5, .5))
#   plot(
#     sf::st_geometry(aoi),
#     lwd = 2,
#     border = "#000000BB",
#     col = "#376e7d88",
#     xlim = c(bbox$xmin, bbox$xmax),
#     ylim = c(bbox$ymin, bbox$ymax),
#     axes = FALSE
#   )
#   # plot(sf::st_geometry(can), lwd = .5, col = "#00000088", add = TRUE)
#   # plot(sf::st_geometry(usa), lwd = .5, col = "#00000088", add = TRUE)
#   # plot(sf::st_geometry(gl), lwd = .5, col = "#00000088", add = TRUE)
#   box()
#   # dev.off()
# }
