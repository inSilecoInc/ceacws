#' Export Rasters to Archive
#'
#' Creates a zip archive containing raster files and processing parameters.
#' Organizes rasters into 'original' and 'processed' folders with metadata.
#'
#' @param stored_rasters List of stored raster objects
#' @param processed_rasters List of processed raster objects
#' @param export_stored Logical, whether to include stored rasters
#' @param export_processed Logical, whether to include processed rasters
#' @param filename Character, name for the output archive (without extension)
#' @return Character path to the created zip file
#'
#' @examples
#' \dontrun{
#' # Export both stored and processed rasters
#' archive_path <- export_rasters_archive(
#'   stored_rasters = my_stored_rasters,
#'   processed_rasters = my_processed_rasters,
#'   export_stored = TRUE,
#'   export_processed = TRUE,
#'   filename = "threat_layers_export"
#' )
#' }
#'
#' @export
export_rasters_archive <- function(stored_rasters, processed_rasters,
                                   export_stored = TRUE, export_processed = TRUE,
                                   filename = "rasters_export") {
  # Validate inputs
  if (!export_stored && !export_processed) {
    stop("At least one of export_stored or export_processed must be TRUE")
  }

  # Create temporary directory for archive contents
  temp_dir <- tempfile()
  dir.create(temp_dir)

  # Create subdirectories
  if (export_stored) {
    dir.create(file.path(temp_dir, "original"))
  }
  if (export_processed) {
    dir.create(file.path(temp_dir, "processed"))
  }

  # Export stored rasters
  if (export_stored && length(stored_rasters) > 0) {
    for (raster_name in names(stored_rasters)) {
      layer <- stored_rasters[[raster_name]]
      layer_name <- paste0(layer$id, "_", layer$metadata$filters_applied$category)
      output_file <- file.path(temp_dir, "original", paste0(layer_name, ".tif"))

      tryCatch(
        {
          terra::writeRaster(layer$raster, output_file, overwrite = TRUE)
        },
        error = function(e) {
          warning("Failed to export stored raster ", layer_name, ": ", e$message)
        }
      )
    }
  }

  # Export processed rasters
  if (export_processed && length(processed_rasters) > 0) {
    for (raster_name in names(processed_rasters)) {
      layer <- processed_rasters[[raster_name]]
      layer_name <- paste0(layer$id, "_", layer$metadata$filters_applied$category)
      output_file <- file.path(temp_dir, "processed", paste0(layer_name, ".tif"))

      tryCatch(
        {
          terra::writeRaster(layer$raster, output_file, overwrite = TRUE)
        },
        error = function(e) {
          warning("Failed to export processed raster ", layer_name, ": ", e$message)
        }
      )
    }
  }

  # Create processing parameters file
  params_file <- file.path(temp_dir, "processing_parameters.txt")
  .write_processing_parameters(stored_rasters, processed_rasters, params_file)

  # Create zip archive
  archive_path <- file.path(tempdir(), paste0(filename, ".zip"))

  # Use archive package to create zip
  archive::archive_write_dir(
    archive = archive_path,
    dir = temp_dir
  )

  # Clean up temporary directory
  unlink(temp_dir, recursive = TRUE)

  return(archive_path)
}

#' Write Processing Parameters to File
#'
#' Creates a comprehensive text file documenting all layer metadata including
#' filtering, combination, and processing information.
#'
#' @param stored_rasters List of stored raster objects
#' @param processed_rasters List of processed raster objects
#' @param file_path Character path where to write the parameters file
#' @noRd
.write_processing_parameters <- function(stored_rasters, processed_rasters, file_path) {
  # Start with header
  lines <- c(
    "CEACWS Threat Layers - Export Metadata",
    "=" %R% 60,
    paste("Export Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste("Total Stored Layers:", length(stored_rasters)),
    paste("Total Processed Layers:", length(processed_rasters)),
    ""
  )

  # Helper function to format layer metadata
  format_layer_info <- function(layer, layer_name, layer_type = "stored") {
    layer_lines <- c(
      "",
      paste("Layer:", layer_name, layer$name),
      paste("Type:", stringr::str_to_title(layer_type)),
      "-" %R% 40
    )

    # Basic layer information
    layer_lines <- c(layer_lines, paste("Display Name:", layer$name %||% "N/A"))
    layer_lines <- c(layer_lines, paste("Layer ID:", layer$id %||% "N/A"))

    # Filtering information
    if (!is.null(layer$metadata$filters_applied)) {
      filters <- layer$metadata$filters_applied
      layer_lines <- c(layer_lines, "", "Applied Filters:")

      if (!is.null(filters$category)) {
        layer_lines <- c(layer_lines, paste("  Category:", filters$category))
      }

      if (!is.null(filters$subcategory) && filters$subcategory != "") {
        layer_lines <- c(layer_lines, paste("  Subcategory:", filters$subcategory))
      }

      if (!is.null(filters$type) && length(filters$type) > 0) {
        type_str <- if (length(filters$type) > 1) {
          paste(filters$type, collapse = ", ")
        } else {
          filters$type
        }
        layer_lines <- c(layer_lines, paste("  Type:", type_str))
      }

      if (!is.null(filters$year) && length(filters$year) > 0) {
        year_str <- if (length(filters$year) > 1) {
          paste(sort(filters$year), collapse = ", ")
        } else {
          filters$year
        }
        layer_lines <- c(layer_lines, paste("  Year:", year_str))
      }

      if (!is.null(filters$month) && length(filters$month) > 0) {
        if (length(filters$month) == 12) {
          month_str <- "All months"
        } else if (length(filters$month) == 1) {
          month_str <- month.name[as.numeric(filters$month)]
        } else {
          month_names <- month.name[as.numeric(filters$month)]
          month_str <- paste(month_names, collapse = ", ")
        }
        layer_lines <- c(layer_lines, paste("  Months:", month_str))
      }

      if (!is.null(filters$species) && length(filters$species) > 0) {
        species_str <- paste(filters$species, collapse = ", ")
        layer_lines <- c(layer_lines, paste("  Species:", species_str))
      }

      if (!is.null(filters$dataset) && length(filters$dataset) > 0) {
        dataset_str <- paste(filters$dataset, collapse = ", ")
        layer_lines <- c(layer_lines, paste("  Dataset:", dataset_str))
      }
    } else {
      layer_lines <- c(layer_lines, "", "Applied Filters: None")
    }

    # Combination method
    combination_method <- layer$metadata$combination_method %||%
      layer$metadata$filters_applied$combination_method %||% "N/A"
    layer_lines <- c(layer_lines, paste("Combination Method:", combination_method))

    # Processing information (for processed layers)
    if (layer_type == "processed" && !is.null(layer$processing_params)) {
      params <- layer$processing_params
      layer_lines <- c(layer_lines, "", "Spatial Processing Applied:")

      if (!is.null(params$extent)) {
        extent_str <- paste(round(params$extent, 4), collapse = ", ")
        layer_lines <- c(layer_lines, paste("  New Extent (xmin,xmax,ymin,ymax):", extent_str))
      }

      if (!is.null(params$resolution)) {
        resolution_str <- if (length(params$resolution) == 1) {
          params$resolution
        } else {
          paste(params$resolution, collapse = " x ")
        }
        layer_lines <- c(layer_lines, paste("  New Resolution:", resolution_str, "degrees"))
      }

      if (!is.null(params$method)) {
        layer_lines <- c(layer_lines, paste("  Resampling Method:", params$method))
      }
    } else if (layer_type == "processed") {
      layer_lines <- c(layer_lines, "", "Spatial Processing Applied: None")
    }

    # File information
    if (!is.null(layer$metadata$file_count)) {
      layer_lines <- c(layer_lines, paste("Source Files Count:", layer$metadata$file_count))
    }

    if (!is.null(layer$metadata$created_at)) {
      created_str <- format(layer$metadata$created_at, "%Y-%m-%d %H:%M:%S")
      layer_lines <- c(layer_lines, paste("Created:", created_str))
    }

    # Raster technical information
    if (!is.null(layer$raster)) {
      layer_lines <- c(layer_lines, "", "Raster Information:")
      dims <- dim(layer$raster)
      extent <- as.vector(terra::ext(layer$raster))
      layer_lines <- c(layer_lines, paste("  Dimensions (rows, cols, layers):", paste(dims, collapse = " x ")))
      layer_lines <- c(layer_lines, paste("  Extent:", paste(round(extent, 4), collapse = ", ")))
      layer_lines <- c(layer_lines, paste("  Resolution:", paste(round(terra::res(layer$raster), 4), collapse = " x ")))
      layer_lines <- c(layer_lines, paste("  CRS:", terra::crs(layer$raster)))
    }

    return(layer_lines)
  }

  # Add stored layers section
  if (length(stored_rasters) > 0) {
    lines <- c(lines, "", "STORED LAYERS", "=" %R% 60)

    for (raster_name in names(stored_rasters)) {
      layer <- stored_rasters[[raster_name]]
      layer_info <- format_layer_info(layer, raster_name, "stored")
      lines <- c(lines, layer_info)
    }
  }

  # Add processed layers section
  if (length(processed_rasters) > 0) {
    lines <- c(lines, "", "", "PROCESSED LAYERS", "=" %R% 60)

    for (raster_name in names(processed_rasters)) {
      layer <- processed_rasters[[raster_name]]
      layer_info <- format_layer_info(layer, raster_name, "processed")
      lines <- c(lines, layer_info)
    }
  }

  if (length(stored_rasters) == 0 && length(processed_rasters) == 0) {
    lines <- c(lines, "No layers available for export.")
  }

  # Add footer
  lines <- c(lines, "", "", "=" %R% 60, "End of Export Metadata Report")

  # Write to file
  writeLines(lines, file_path)
}

# Helper operator for string repetition
`%R%` <- function(str, n) {
  paste(rep(str, n), collapse = "")
}
