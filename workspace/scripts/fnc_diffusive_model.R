#' Diffusive model
#'
#' Function to evaluate the influence area of a stressor using a passive diffusive model
#'
#' @param dat sf object with points characterizing stressor intensity
#' @param field character, name of field containing stresseor intensity
#' @param threshold numeric, minimum threshold in percent of the global maximum at which to stop the diffusive model
#' @param globalmaximum global maximum value of the stressor in the study area
#' @param decay percent by which the value of the stressor is reduced when diffusing to the adjacent cells
#' @param increment numeric, distance between successive steps in the model. Units are in the units of the spatial object provided.
#'
#' @keywords cumulative footprint
#'
#' @export
#'
#' @details
#'

diffusive_model <- function(dat, field, threshold, globalmaximum, decay, increment) {
  # -----
  val <- res <- list()
  for (i in 1:nrow(dat)) {
    val[[i]] <- seq(
      from = as.numeric(dat[i, field, drop = TRUE]),
      to = as.numeric(globalmaximum * (threshold / 100)),
      by = -(decay / 100)
    )
    nVal <- length(val[[i]])
    temp <- dplyr::select(dat[i, ], geometry)


    rings <- circles <- list()
    if (nVal > 1) {
      # Buffers and concentric circles for passive diffusion
      circles[[1]] <- rings[[1]] <- sf::st_buffer(temp, increment)
      for (j in 2:nVal) {
        circles[[j]] <- sf::st_buffer(circles[[(j - 1)]], increment)
        rings[[j]] <- sf::st_difference(circles[[j]], circles[[(j - 1)]])
      }

      # Add intensity values and rasterize
      res[[i]] <- dplyr::bind_rows(rings) |>
        dplyr::mutate(intensity = val[[i]])
    } else if (nVal == 1) {
      res[[i]] <- sf::st_buffer(temp, increment) |>
        dplyr::mutate(intensity = val[[i]])
    }
  }

  # Combine and rasterize
  r <- as(
    stars::read_stars("project-data/grid/grid.tif"),
    "Raster"
  )
  res <- res |>
    dplyr::bind_rows() |>
    sf::st_transform(sf::st_crs(r)) |>
    fasterize::fasterize(
      r,
      field = "intensity",
      fun = "sum"
    ) |>
    stars::st_as_stars() |>
    mask_aoi()

  # Return
  res
}
