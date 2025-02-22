prc_viirs_nighttime_light <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/viirs_nighttime_light-1.0.0/processed/"
  # dir.create(output_path)
  # input_path <- "workspace/data/harvested/viirs_nighttime_light-1.0.0/raw/"
  # input_files <- input_path
  input_files <- unlist(input_files)

  # Data
  files <- dir(input_files, pattern = "*.tif.gz", full.names = TRUE)

  # Plan for parallel processing
  future::plan(future::multisession, workers = parallel::detectCores() - 3)

  # Process files in parallel
  furrr::future_map(
    files,
    ~ process_vnl_file(.x, output_path),
    .options = furrr::furrr_options(seed = TRUE)
  )
}

# Individual File Processing Function
process_vnl_file <- function(file, output_path) {
  # Decompress file
  tmp <- file.path(output_path, tools::file_path_sans_ext(basename(file)))
  out <- file.path(tmp, tools::file_path_sans_ext(basename(file)))
  R.utils::gunzip(file, destname = out, overwrite = FALSE, remove = FALSE)

  # Extract month and year from the file name for output naming
  year_month <- stringr::str_replace(basename(out), ".*_(\\d{4})(\\d{2})\\d{2}-.*", "\\1_\\2")

  # Output file
  output_file <- file.path(output_path, paste0("viirs_nighttime_light_", year_month, ".tif"))

  # Import, crop and export
  terra::terraOptions(progress = 0)
  suppressWarnings({
    terra::rast(out) |>
      terra::crop(terra::ext(-85, -35, 38, 75)) |>
      terra::writeRaster(
        filename = output_file,
        filetype = "COG",
        gdal = c("COMPRESS=LZW", "TILED=YES", "OVERVIEW_RESAMPLING=AVERAGE", "BIGTIFF=IF_SAFER"),
        overwrite = TRUE
      )
  })

  # Remove unzipped files
  fs::dir_delete(tmp)
}
