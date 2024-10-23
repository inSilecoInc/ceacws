#' Check if folder exists and create if not
#'
#' @param path path of folder to use as output, create if it does not already exist
#'
#' @export
chk_create <- function(path) {
  if (!file.exists(path)) dir.create(path, recursive = TRUE)
}

# ------------------------------------------------------------------------------
#' Create a timestamp
#'
#' @export
timestamp <- function() format(Sys.time(), format = "%Y-%m-%d")


# ------------------------------------------------------------------------------
#' Create polygon from bbox
#'
#' @param bbox bounding box to spatially subset the queried data, if applicable. The bounding box should be of the form `c(xmin, ymin, xmax, ymax)`",
#' @param crs spatial projection of bounding box argument, defaults to epsg: 4326",
#'
#' @export
bbox_poly <- function(bbox, crs = 4326) {
  sf::st_bbox(bbox, crs = sf::st_crs(crs)) |>
    sf::st_as_sfc()
}

# ------------------------------------------------------------------------------
#' Function to mask stars object with sf polygon
#'
#' @param dat stars object to mask using sf polygon
#' @param aoi sf polygonused to mask the stars
#'
#' @export
mask_sf <- function(dat, aoi) {
  tmp <- dat
  tmp[[1]][] <- NA
  aoi$val_ras <- 1
  aoi_mask <- stars::st_rasterize(aoi["val_ras"], template = tmp)
  stars::st_dimensions(aoi_mask) <- stars::st_dimensions(dat)
  dat * aoi_mask
}



# ------------------------------------------------------------------------------
#' Imports and export data in or from global R environment
#'
#' These functions are used to import or export data of various format in R
#'
#' @param path path to disk of the file to import or export. Do not specify the extension if exporting using `masterwrite()`.
#' @param obj object to write to disk
#'
#' @return This function returns objects in the global R environment
#'
#' @name masterload
#'
#' @examples
#' \dontrun{
#' masterload("path")
#' }
#' @export
masterload <- function(path) {
  # Identify extension of the file to import
  ext <- tools::file_ext(path)
  name <- tools::file_path_sans_ext(basename(path))

  # Import
  dat <- switch(ext,
    "gpkg" = sf::st_read(path, quiet = TRUE),
    "geojson" = sf::st_read(path, quiet = TRUE),
    "shp" = sf::st_read(path, quiet = TRUE),
    "tif" = stars::read_stars(path, quiet = TRUE),
    "csv" = vroom::vroom(path)
  )

  # Identify which data were loaded
  msgInfo("Imported file:", glue::glue("{name}.{ext}"))

  # -----
  invisible(dat)
}

#' @name masterload
#' @export
masterwrite <- function(obj, path) {
  # Identify extension to use for object export
  ext <- make_extension(obj)

  # WARNING: Soft transition to `terra` to export COGs
  write_ras <- function(obj, path) {
    if (inherits(obj, "stars")) {
      obj <- as(obj, "Raster") |>
        terra::rast()
    }
    obj |>
      terra::writeRaster(
        filename = path,
        filetype = "COG",
        gdal = c("COMPRESS=LZW", "TILED=YES", "OVERVIEW_RESAMPLING=AVERAGE"),
        overwrite = TRUE
      )
  }

  # Export
  switch(ext,
    "gpkg" = sf::st_write(obj, glue::glue("{path}.{ext}"), quiet = TRUE, append = FALSE),
    # "tif" = stars::write_stars(obj, glue::glue("{path}.{ext}"), quiet = TRUE),
    "tif" = write_ras(obj, glue::glue("{path}.{ext}")),
    "csv" = vroom::vroom_write(obj, glue::glue("{path}.{ext}"), delim = ","),
    "yaml" = yaml::write_yaml(obj, glue::glue("{path}.{ext}"), column.major = FALSE),
    "bib" = RefManageR::WriteBib(obj, file = glue::glue("{path}.{ext}"), verbose = FALSE)
  )

  # Identify which data were loaded
  msgInfo("Exported file:", glue::glue("{basename(path)}.{ext}"))
}

#' @name masterload
#' @export
make_extension <- function(obj) {
  cls <- class(obj)
  dplyr::case_when(
    "sf" %in% cls ~ "gpkg",
    "stars" %in% cls ~ "tif",
    "SpatRaster" %in% cls ~ "tif",
    (("matrix" %in% cls | "data.frame" %in% cls) & (!"stars" %in% cls & !"sf" %in% cls)) ~ "csv",
    "list" %in% cls ~ "yaml",
    "BibEntry" %in% cls ~ "bib"
  )
}

#' Creates a raster study grid
#'
#' This function is used to create a raster study grid that will serve as a template for data extractions
#'
#' @param aoi object of class sf or sfc representing the area of interest, or the bounding box of the area to consider. The bounding box should be either a `bbox` class object from the `sf` package or of the form `c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)`.
#' @param crs object of class crs; coordinate reference system of the target of the target grid in case argument x is missing, if x is not missing, its crs is inherited.
#' @param cellsize target grid cell size, in the units of the crs selected.
#'
#' @return a tif file and a csv file
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cellsize <- 1
#' crs <- 4326
#' bb <- c(xmin = -45, ymin = -45, xmax = 45, ymax = 45)
#' bbox <- sf::st_bbox(bb, crs = crs)
#' aoi <- sf::st_as_sfc(bbox, crs = crs)
#' make_grid(bb, crs, cellsize)
#' make_grid(bbox, crs, cellsize)
#' make_grid(aoi, crs, cellsize)
#' }
make_grid <- function(aoi, crs = 4326, cellsize) {
  # Create grid template
  if ("sfc" %in% class(aoi)) {
    aoi <- sf::st_as_sf(aoi, crs = crs)
  } else if (any(c("bbox", "numeric", "integer") %in% class(aoi))) {
    aoi <- sf::st_bbox(aoi, crs = crs) |>
      sf::st_as_sfc() |>
      sf::st_as_sf()
  }
  grd <- stars::st_rasterize(aoi, dx = cellsize, dy = cellsize)

  # Export
  ## Raster grid
  out <- here::here("project-data", "grid")
  chk_create(out)
  masterwrite(grd, here::here(out, "grid"))

  ## Polygon of area of interest
  out <- here::here("project-data", "aoi")
  chk_create(out)
  masterwrite(aoi, dsn = here::here(out, "aoi"))
}









# # ------------------------------------------------------------------------------
# # Clean global environment
# clean <- function() {
#   objs <- ls(envir = globalenv())
#   rm(list = objs, pos = ".GlobalEnv")
# }
