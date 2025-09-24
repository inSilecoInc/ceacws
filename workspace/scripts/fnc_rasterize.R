#' Group Rasterize and Export Spatial Data
#'
#' This function takes an `sf` object (points, lines, or polygons), groups it based on specified variables, rasterizes each group onto a provided grid, masks the rasterized data by an area of interest (AOI), and exports the masked rasters to a specified folder. The output filenames are constructed using the dataset name and the grouping variable values.
#'
#' @param sf_object An `sf` object (points, lines, or polygons) to be rasterized.
#' @param grouping_vars A character vector of column names in `dat` to group the data.
#' @param fun from `terra::rasterize`, summarizing function for when there are multiple geometries in one cell. For lines and polygons you can only use ‘"min"’, ‘"max"’, ‘"mean"’, ‘"count"’ and ‘"sum"’ For points you can use any function that returns a single number; for example ‘mean’, ‘length’ (to get a count), ‘min’ or ‘max’
#' @param field from `terra::rasterize` character or numeric. If ‘field’ is a character, it should a variable name in ‘x’. If ‘field’ is numeric it typically is a single number or a vector of length ‘nrow(x)’. The values are recycled to ‘nrow(x)’
#' @param norm numeric, value to use to normalize raster values
#' @param grid A `SpatRaster` object defining the grid to rasterize onto.
#' @param aoi A polygon of class `sf` or `SpatVector` defining the area of interest to mask the rasters.
#' @param dataset_name A string specifying the name of the dataset, used in the output filenames.
#' @param output_path A string specifying the folder path where the output rasters will be saved. The folder must exist prior to function execution.
#'
#' @return Invisibly returns `NULL`. The function writes raster files to the `output_path` and outputs a message upon successful completion.
#'
#' @details
#' The function rasterizes each group using `terra::rasterize()`, masks the raster using `terra::mask()`, and writes the resulting rasters to the output directory using `terra::writeRaster()`.
#'
#' The filenames for each raster are constructed using `file.path()` and include the dataset name combined with the grouping variable values, separated by underscores (`_`).
#'
#' @examples
#' \dontrun{
#' group_rasterize_export(
#'   sf_object = my_sf_data,
#'   grouping_vars = c("species", "year"),
#'   grid = my_grid,
#'   aoi = my_aoi,
#'   dataset_name = "habitat_density",
#'   output_path = "output_folder/"
#' )
#' }
#'
#' @export
group_rasterize_export <- function(
    sf_object,
    grouping_vars,
    fun,
    field = NA,
    norm = NULL,
    grid,
    aoi,
    dataset_name,
    output_path) {
  # Ensure grouping variables exist in the sf_object
  grouping_vars <- as.character(grouping_vars)
  missing_vars <- grouping_vars[!grouping_vars %in% names(sf_object)]
  if (length(missing_vars) > 0) {
    stop("The following grouping variables are missing in the sf_object: ", paste(missing_vars, collapse = ", "))
  }

  # Convert to terra SpatVector
  # sf_terra <- terra::vect(sf_object)

  # Group the sf object by the grouping variables
  sf_groups <- sf_object |>
    dplyr::group_by(!!!rlang::syms(grouping_vars))

  # Group labels
  group_labels <- dplyr::group_keys(sf_groups) |>
    dplyr::mutate(
      label = if (length(grouping_vars) > 1) {
        apply(dplyr::across(everything()), 1, paste, collapse = "_")
      } else {
        .data[[grouping_vars[1]]]
      }
    ) |>
    dplyr::pull(label)


  # Split groups for processing
  sf_groups <- dplyr::group_split(sf_groups)

  # Iterate through each group, rasterize, mask, and export
  purrr::walk2(sf_groups, group_labels, function(group, label) {
    # Convert group back to terra::SpatVector
    group_terra <- terra::vect(group)

    # Rasterize group
    density_raster <- terra::rasterize(
      x = group,
      y = grid,
      fun = fun,
      field = field
    )

    # Normalize
    if (!is.null(norm)) density_raster <- density_raster / norm

    # Mask with AOI
    masked_raster <- terra::mask(density_raster, aoi)

    # Construct output filename
    output_filename <- file.path(
      output_path,
      paste0(dataset_name, "_", label, ".tif")
    )

    # Export the raster
    terra::writeRaster(
      masked_raster,
      filename = output_filename,
      overwrite = TRUE,
      gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
    )
  })
  invisible(NULL)
}
