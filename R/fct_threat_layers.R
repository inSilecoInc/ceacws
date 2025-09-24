#' Scan Available Threat Layer Files
#'
#' Scans the workspace/data/analyzed/ directory and returns a simple data.frame
#' of all available threat layer files with parsed metadata.
#'
#' @param analyzed_path Path to analyzed data directory. Defaults to "workspace/data/analyzed"
#' @return A data.frame with columns: filepath, filename, category, subcategory, type, year, month, etc.
#' @export
#'
get_threat_layers <- function(analyzed_path = file.path("workspace", "data", "analyzed")) {
  # Check if directory exists
  if (!dir.exists(analyzed_path)) {
    warning("Analyzed data directory not found: ", analyzed_path)
    return(data.frame())
  }

  # Find all .tif files, excluding Atlantic scale layers
  tif_files <- dir(analyzed_path, recursive = TRUE, pattern = "*.tif", full.names = TRUE)

  # Filter out Atlantic scale layers
  tif_files <- tif_files[!stringr::str_detect(tif_files, "atlantic|fisheries")]

  if (length(tif_files) == 0) {
    return(data.frame())
  }

  # Parse all files and build data.frame
  parsed_data <- list()

  for (filepath in tif_files) {
    parsed <- parse_threat_layer_filename(filepath)

    if (!is.null(parsed)) {
      parsed_data[[length(parsed_data) + 1]] <- parsed
    }
  }

  if (length(parsed_data) == 0) {
    return(data.frame())
  }

  # Convert to data.frame
  result_df <- do.call(rbind.data.frame, c(parsed_data, stringsAsFactors = FALSE))

  # Ensure consistent column types
  result_df$filepath <- as.character(result_df$filepath)
  result_df$filename <- as.character(result_df$filename)
  result_df$category <- as.character(result_df$category)

  return(result_df)
}

#' Create Base Result Structure
#'
#' Creates a base list structure with all possible fields for threat layer parsing.
#'
#' @param filepath The full file path
#' @param filename The filename
#' @return A list with all fields initialized
#' @noRd
create_base_result <- function(filepath, filename) {
  list(
    filepath = filepath,
    filename = filename,
    category = NA_character_,
    subcategory = NA_character_,
    type = NA_character_,
    species = NA_character_,
    year = NA_integer_,
    year_range = NA_character_,
    month = NA_integer_,
    dataset = NA_character_,
    nighttime = FALSE
  )
}

#' Identify Threat Layer Type
#'
#' Determines the threat layer type based on filename patterns.
#'
#' @param filename The filename to analyze
#' @return Character string identifying the threat type or NULL if not recognized
#' @noRd
#'
identify_threat_type <- function(filename) {
  if (stringr::str_detect(filename, "^fisheries_effort_")) {
    return("fisheries")
  }
  if (stringr::str_detect(filename, "^night_light_")) {
    return("night_lights")
  }
  if (stringr::str_detect(filename, "^offshore_petroleum_activity_")) {
    return("offshore_petroleum_activity")
  }
  if (stringr::str_detect(filename, "^offshore_petroleum_platform_")) {
    return("offshore_petroleum_platform")
  }
  if (stringr::str_detect(filename, "^offshore_wind_farm_")) {
    return("offshore_wind_farm")
  }
  if (stringr::str_detect(filename, "^petroleum_pollution_incidents_")) {
    return("petroleum_pollution")
  }
  if (stringr::str_detect(filename, "^ship_light_detection_")) {
    return("ship_light_detection")
  }
  if (stringr::str_detect(filename, "^shipping_intensity_density_")) {
    return("shipping_daily")
  }
  if (stringr::str_detect(filename, "^shipping_night_light_intensity_density_")) {
    return("shipping_nighttime")
  }

  NULL
}

#' Parse Threat Layer Filename
#'
#' Extracts structured metadata from threat layer filenames using specific parsers.
#'
#' @param filename The filename to parse
#' @param filepath The full file path
#' @return A list with parsed attributes or NULL if not recognized
#' @noRd
parse_threat_layer_filename <- function(filepath) {
  filename <- fs::path_file(filepath)
  threat_type <- identify_threat_type(filename)
  switch(threat_type,
    "fisheries" = parse_fisheries_filename(filename, filepath),
    "night_lights" = parse_night_lights_filename(filename, filepath),
    "offshore_petroleum_activity" = parse_offshore_petroleum_activity_filename(filename, filepath),
    "offshore_petroleum_platform" = parse_offshore_petroleum_platform_filename(filename, filepath),
    "offshore_wind_farm" = parse_offshore_wind_farm_filename(filename, filepath),
    "petroleum_pollution" = parse_petroleum_pollution_filename(filename, filepath),
    "ship_light_detection" = parse_ship_light_detection_filename(filename, filepath),
    "shipping_daily" = parse_shipping_daily_filename(filename, filepath),
    "shipping_nighttime" = parse_shipping_nighttime_filename(filename, filepath),
    NULL
  )
}

#' Parse Fisheries Filename
#' @noRd
parse_fisheries_filename <- function(filename, filepath) {
  result <- create_base_result(filepath, filename)

  match <- stringr::str_match(filename, "^fisheries_effort_(.+)_(\\d{4}-\\d{4})_ ?(\\d+)\\.tif$")
  if (is.na(match[1])) {
    return(NULL)
  }

  type_species <- match[2]
  result$year_range <- match[3]
  result$month <- as.integer(match[4])
  result$category <- "fisheries"

  # Check if there's a species (more than one underscore in type_species)
  parts <- strsplit(type_species, "_")[[1]]
  if (length(parts) > 1) {
    result$subcategory <- "species_specific"
    result$type <- parts[1]
    result$species <- paste(parts[2:length(parts)], collapse = "_")
  } else {
    result$subcategory <- "gear_only"
    result$type <- type_species
  }

  result
}

#' Parse Night Lights Filename
#' @noRd
parse_night_lights_filename <- function(filename, filepath) {
  result <- create_base_result(filepath, filename)

  match <- stringr::str_match(filename, "^night_light_(\\d{4})-(\\d{2})-01\\.tif$")
  if (is.na(match[1])) {
    return(NULL)
  }

  result$category <- "night_lights"
  result$year <- as.integer(match[2])
  result$month <- as.integer(match[3])
  result$nighttime <- TRUE

  result
}

#' Parse Offshore Petroleum Activity Filename
#' @noRd
parse_offshore_petroleum_activity_filename <- function(filename, filepath) {
  result <- create_base_result(filepath, filename)

  match <- stringr::str_match(filename, "^offshore_petroleum_activity_(.+)\\.tif$")
  if (is.na(match[1])) {
    return(NULL)
  }

  result$category <- "offshore_petroleum"
  result$subcategory <- "activity"
  result$type <- match[2]

  result
}

#' Parse Offshore Petroleum Platform Filename
#' @noRd
parse_offshore_petroleum_platform_filename <- function(filename, filepath) {
  result <- create_base_result(filepath, filename)
  result$category <- "offshore_petroleum"

  # Annual pattern: offshore_petroleum_platform_{type}_{year}.tif
  if (stringr::str_detect(filename, "^offshore_petroleum_platform_.+_\\d{4}\\.tif$")) {
    match <- stringr::str_match(filename, "^offshore_petroleum_platform_(.+)_(\\d{4})\\.tif$")
    if (!is.na(match[1])) {
      result$subcategory <- "platform_annual"
      result$type <- match[2]
      result$year <- as.integer(match[3])
      return(result)
    }
  }

  # Monthly pattern: offshore_petroleum_platform_{type}_{year}-{month}-01.tif
  if (stringr::str_detect(filename, "^offshore_petroleum_platform_.+-\\d{2}-01\\.tif$")) {
    match <- stringr::str_match(filename, "^offshore_petroleum_platform_(.+)_(\\d{4})-(\\d{2})-01\\.tif$")
    if (!is.na(match[1])) {
      result$subcategory <- "platform_monthly"
      result$type <- match[2]
      result$year <- as.integer(match[3])
      result$month <- as.integer(match[4])
      return(result)
    }
  }

  NULL
}

#' Parse Offshore Wind Farm Filename
#' @noRd
parse_offshore_wind_farm_filename <- function(filename, filepath) {
  result <- create_base_result(filepath, filename)

  match <- stringr::str_match(filename, "^offshore_wind_farm_(.+)\\.tif$")
  if (is.na(match[1])) {
    return(NULL)
  }

  result$category <- "offshore_wind_farm"
  result$type <- match[2]

  result
}

#' Parse Petroleum Pollution Filename
#' @noRd
parse_petroleum_pollution_filename <- function(filename, filepath) {
  result <- create_base_result(filepath, filename)
  result$category <- "petroleum_pollution"

  # Try specific type pattern first
  match <- stringr::str_match(filename, "^petroleum_pollution_incidents_(.+)_ ?(\\d+)_(.+)\\.tif$")
  if (!is.na(match[1])) {
    result$subcategory <- "specific_type"
    result$dataset <- match[2]
    result$month <- as.integer(match[3])
    result$type <- match[4]
    return(result)
  }

  # Try all types pattern
  match <- stringr::str_match(filename, "^petroleum_pollution_incidents_(.+)_ ?(\\d+)\\.tif$")
  if (!is.na(match[1]) && match[2] != "all") {
    result$subcategory <- "all_types"
    result$dataset <- match[2]
    result$month <- as.integer(match[3])
    return(result)
  }

  NULL
}

#' Parse Ship Light Detection Filename
#' @noRd
parse_ship_light_detection_filename <- function(filename, filepath) {
  result <- create_base_result(filepath, filename)

  match <- stringr::str_match(filename, "^ship_light_detection_(\\d+)\\.tif$")
  if (is.na(match[1])) {
    return(NULL)
  }

  result$category <- "ship_light_detection"
  result$month <- as.integer(match[2])
  result$nighttime <- TRUE

  result
}

#' Parse Shipping Daily Filename
#' @noRd
parse_shipping_daily_filename <- function(filename, filepath) {
  result <- create_base_result(filepath, filename)

  match <- stringr::str_match(filename, "^shipping_intensity_density_(\\d{4})_(\\d+)_(.+)\\.tif$")
  if (is.na(match[1])) {
    return(NULL)
  }

  result$category <- "shipping"
  result$subcategory <- "daily"
  result$year <- as.integer(match[2])
  result$month <- as.integer(match[3])
  result$type <- match[4]

  result
}

#' Parse Shipping Nighttime Filename
#' @noRd
parse_shipping_nighttime_filename <- function(filename, filepath) {
  result <- create_base_result(filepath, filename)

  match <- stringr::str_match(filename, "^shipping_night_light_intensity_density_(\\d{4})_(\\d+)_(.+)\\.tif$")
  if (is.na(match[1])) {
    return(NULL)
  }

  result$category <- "shipping"
  result$subcategory <- "nighttime"
  result$year <- as.integer(match[2])
  result$month <- as.integer(match[3])
  result$type <- match[4]
  result$nighttime <- TRUE

  result
}

#' Prepare UI Filter Data
#'
#' Transforms flat threat layer data.frame into a nested structure optimized
#' for UI filter dropdown population.
#'
#' @return Nested list structure with categories and filter options
#' @export
prepare_ui_threat_layers_data <- function() {
  threat_data <- get_threat_layers()
  if (nrow(threat_data) == 0) {
    return(list())
  }

  # Get unique categories
  categories <- unique(threat_data$category)
  categories <- categories[!is.na(categories)]

  result <- list()

  for (cat in categories) {
    cat_data <- threat_data[threat_data$category == cat & !is.na(threat_data$category), ]

    # Check if this category has subcategories
    subcategories <- unique(cat_data$subcategory)
    subcategories <- subcategories[!is.na(subcategories)]

    if (length(subcategories) > 0) {
      # Category with subcategories
      result[[cat]] <- list(subcategories = subcategories)

      # Add filter options for each subcategory
      for (subcat in subcategories) {
        subcat_data <- cat_data[cat_data$subcategory == subcat & !is.na(cat_data$subcategory), ]
        result[[cat]][[subcat]] <- extract_filter_options(subcat_data)
      }
    } else {
      # Category without subcategories - direct filter options
      result[[cat]] <- extract_filter_options(cat_data)
    }
  }

  result
}

#' Extract Filter Options
#'
#' Extracts unique filter values from a subset of threat layer data.
#'
#' @param data data.frame subset for a specific category/subcategory
#' @return List with filter options (types, species, years, months, etc.)
#' @noRd
extract_filter_options <- function(data) {
  # Extract unique non-NA values for each filter field
  list(
    types = get_unique_values(data$type),
    species = get_unique_values(data$species),
    years = get_unique_values(data$year),
    year_ranges = get_unique_values(data$year_range),
    months = sort(get_unique_values(data$month)),
    datasets = get_unique_values(data$dataset),
    nighttime = any(data$nighttime, na.rm = TRUE),
    data = data # Keep the actual data for filtering
  )
}

#' Get Unique Non-NA Values
#'
#' Helper to extract unique non-NA values from a vector.
#'
#' @param x Vector to extract unique values from
#' @return Vector of unique non-NA values, or NULL if none exist
#' @noRd
get_unique_values <- function(x) {
  unique_vals <- unique(x[!is.na(x)])
  if (length(unique_vals) == 0) {
    return(NULL)
  }
  unique_vals
}
