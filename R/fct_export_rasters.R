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
      output_file <- file.path(temp_dir, "original", paste0(raster_name, ".tif"))
      
      tryCatch({
        terra::writeRaster(layer$raster, output_file, overwrite = TRUE)
      }, error = function(e) {
        warning("Failed to export stored raster ", raster_name, ": ", e$message)
      })
    }
  }
  
  # Export processed rasters
  if (export_processed && length(processed_rasters) > 0) {
    for (raster_name in names(processed_rasters)) {
      layer <- processed_rasters[[raster_name]]
      output_file <- file.path(temp_dir, "processed", paste0(raster_name, ".tif"))
      
      tryCatch({
        terra::writeRaster(layer$raster, output_file, overwrite = TRUE)
      }, error = function(e) {
        warning("Failed to export processed raster ", raster_name, ": ", e$message)
      })
    }
  }
  
  # Create processing parameters file
  params_file <- file.path(temp_dir, "processing_parameters.txt")
  .write_processing_parameters(processed_rasters, params_file)
  
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
#' Creates a text file documenting the processing parameters applied to rasters.
#'
#' @param processed_rasters List of processed raster objects
#' @param file_path Character path where to write the parameters file
#' @noRd
.write_processing_parameters <- function(processed_rasters, file_path) {
  
  # Start with header
  lines <- c(
    "CEACWS Threat Layers - Processing Parameters",
    "=" %R% 50,
    paste("Export Date:", Sys.time()),
    paste("Total Processed Rasters:", length(processed_rasters)),
    "",
    "Processing Details:",
    "-" %R% 30
  )
  
  if (length(processed_rasters) == 0) {
    lines <- c(lines, "No processed rasters available.")
  } else {
    
    # Add details for each processed raster
    for (raster_name in names(processed_rasters)) {
      layer <- processed_rasters[[raster_name]]
      
      lines <- c(lines, "")
      lines <- c(lines, paste("Raster:", raster_name))
      lines <- c(lines, paste("Display Name:", layer$display_name %||% "N/A"))
      lines <- c(lines, paste("Original ID:", layer$id %||% "N/A"))
      lines <- c(lines, paste("Processed:", isTRUE(layer$processed)))
      
      if (!is.null(layer$processing_params)) {
        params <- layer$processing_params
        
        lines <- c(lines, "Processing Parameters:")
        
        if (!is.null(params$extent)) {
          extent_str <- paste(params$extent, collapse = ", ")
          lines <- c(lines, paste("  Extent (xmin,xmax,ymin,ymax):", extent_str))
        }
        
        if (!is.null(params$resolution)) {
          resolution_str <- if (length(params$resolution) == 1) {
            params$resolution
          } else {
            paste(params$resolution, collapse = ", ")
          }
          lines <- c(lines, paste("  Resolution:", resolution_str))
        }
        
        if (!is.null(params$method)) {
          lines <- c(lines, paste("  Resampling Method:", params$method))
        }
      } else {
        lines <- c(lines, "Processing Parameters: None")
      }
      
      # Add raster info
      if (!is.null(layer$raster)) {
        dims <- dim(layer$raster)
        extent <- as.vector(terra::ext(layer$raster))
        lines <- c(lines, paste("  Dimensions (rows, cols, layers):", paste(dims, collapse = ", ")))
        lines <- c(lines, paste("  Extent:", paste(round(extent, 4), collapse = ", ")))
        lines <- c(lines, paste("  CRS:", terra::crs(layer$raster)))
      }
    }
  }
  
  # Add footer
  lines <- c(lines, "", "=" %R% 50, "End of Report")
  
  # Write to file
  writeLines(lines, file_path)
}

# Helper operator for string repetition
`%R%` <- function(str, n) {
  paste(rep(str, n), collapse = "")
}