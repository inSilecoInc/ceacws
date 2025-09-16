#' Get Available Threat Layers
#'
#' Scans the workspace/data/analyzed/ directory and returns a structured list
#' of available threat layers, filtering out Atlantic scale layers.
#'
#' @return A structured list optimized for UI filtering with types, years, months, nighttime, and layers components
#' @export
#'
#' @importFrom fs dir_ls path_file path_dir
#' @importFrom stringr str_extract str_detect str_replace_all str_match
#' @importFrom dplyr bind_rows

# Helper operator for NULL coalescing
`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}
get_threat_layers <- function() {
  analyzed_path <- file.path("workspace", "data", "analyzed")

  # Check if directory exists
  if (!dir.exists(analyzed_path)) {
    warning("Analyzed data directory not found: ", analyzed_path)
    return(list())
  }

  # Find all .tif files, excluding Atlantic scale layers
  tif_files <- fs::dir_ls(analyzed_path,
    recurse = TRUE,
    glob = "*.tif"
  ) |>
    # Filter out Atlantic scale layers
    {
      \(x) x[!stringr::str_detect(x, "atlantic")]
    }()

  if (length(tif_files) == 0) {
    return(list())
  }

  # Parse all files first
  all_parsed <- list()
  for (file in tif_files) {
    filename <- fs::path_file(file)
    parsed <- parse_filename(filename, file)
    if (!is.null(parsed)) {
      all_parsed[[length(all_parsed) + 1]] <- parsed
    }
  }

  if (length(all_parsed) == 0) {
    return(list())
  }

  # Duplicate NEEC petroleum pollution entries for both all_types and specific_type subcategories
  neec_entries <- list()
  for (i in seq_along(all_parsed)) {
    entry <- all_parsed[[i]]
    if (entry$category == "petroleum_pollution" && 
        entry$subcategory == "specific_type" && 
        entry$dataset == "neec") {
      # Create duplicate entry for all_types subcategory
      all_types_entry <- entry
      all_types_entry$subcategory <- "all_types"
      all_types_entry$type <- NULL  # Remove type field for all_types
      neec_entries[[length(neec_entries) + 1]] <- all_types_entry
    }
  }
  
  # Add the duplicated entries to the main list
  if (length(neec_entries) > 0) {
    all_parsed <- c(all_parsed, neec_entries)
  }

  # Group by categories and create UI-optimized structure
  categories <- unique(sapply(all_parsed, function(x) x$category))
  result <- list()

  for (cat in categories) {
    cat_data <- all_parsed[sapply(all_parsed, function(x) x$category == cat)]

    # Handle offshore petroleum split
    if (cat == "offshore_petroleum") {
      # Split into activity and platforms
      activity_data <- cat_data[sapply(cat_data, function(x) x$subcategory == "activity")]
      platform_data <- cat_data[sapply(cat_data, function(x) x$subcategory %in% c("platform_annual", "platform_monthly"))]

      if (length(activity_data) > 0) {
        result[["offshore_petroleum_activity"]] <- create_category_structure("offshore_petroleum_activity", activity_data)
      }
      if (length(platform_data) > 0) {
        result[["offshore_petroleum_platforms"]] <- create_category_structure("offshore_petroleum_platforms", platform_data)
      }
    } else {
      result[[cat]] <- create_category_structure(cat, cat_data)
    }
  }

  return(result)
}

#' Parse filename according to threat layer patterns
#' @param filename The filename to parse
#' @param filepath The full file path
#' @return A list with parsed attributes or NULL if not recognized
parse_filename <- function(filename, filepath) {
  # Fisheries: fisheries_effort_{type}_{year start-year end}_{month number}.tif
  # or fisheries_effort_{type}_{species}_{year start-year end}_{month number}.tif
  if (stringr::str_detect(filename, "^fisheries_effort_")) {
    # Pattern: fisheries_effort_{type}_{year-year}_{month}.tif or
    #          fisheries_effort_{type}_{species}_{year-year}_{month}.tif
    match <- stringr::str_match(filename, "^fisheries_effort_(.+)_(\\d{4}-\\d{4})_ ?(\\d+)\\.tif$")
    if (!is.na(match[1])) {
      type_species <- match[2]
      year_range <- match[3]
      month <- as.integer(match[4])

      # Check if there's a species (more than one underscore in type_species)
      parts <- strsplit(type_species, "_")[[1]]
      if (length(parts) > 1) {
        return(list(
          category = "fisheries",
          subcategory = "species_specific",
          type = parts[1],
          species = paste(parts[2:length(parts)], collapse = "_"),
          year_range = year_range,
          month = month,
          filepath = filepath,
          nighttime = FALSE
        ))
      } else {
        return(list(
          category = "fisheries",
          subcategory = "gear_only",
          type = type_species,
          species = NULL,
          year_range = year_range,
          month = month,
          filepath = filepath,
          nighttime = FALSE
        ))
      }
    }
  }

  # Night lights: night_light_{year}-{month}-01.tif
  if (stringr::str_detect(filename, "^night_light_")) {
    match <- stringr::str_match(filename, "^night_light_(\\d{4})-(\\d{2})-01\\.tif$")
    if (!is.na(match[1])) {
      return(list(
        category = "night_lights",
        year = as.integer(match[2]),
        month = as.integer(match[3]),
        filepath = filepath,
        nighttime = TRUE
      ))
    }
  }

  # Offshore petroleum activity: offshore_petroleum_activity_{type}.tif
  if (stringr::str_detect(filename, "^offshore_petroleum_activity_")) {
    match <- stringr::str_match(filename, "^offshore_petroleum_activity_(.+)\\.tif$")
    if (!is.na(match[1])) {
      return(list(
        category = "offshore_petroleum",
        subcategory = "activity",
        type = match[2],
        filepath = filepath,
        nighttime = FALSE
      ))
    }
  }

  # Offshore petroleum platform annual: offshore_petroleum_platform_{type}_{year}.tif
  if (stringr::str_detect(filename, "^offshore_petroleum_platform_.+_\\d{4}\\.tif$")) {
    match <- stringr::str_match(filename, "^offshore_petroleum_platform_(.+)_(\\d{4})\\.tif$")
    if (!is.na(match[1])) {
      return(list(
        category = "offshore_petroleum",
        subcategory = "platform_annual",
        type = match[2],
        year = as.integer(match[3]),
        filepath = filepath,
        nighttime = FALSE
      ))
    }
  }

  # Offshore petroleum platform monthly: offshore_petroleum_platform_{type}_{year}-{month}-01.tif
  if (stringr::str_detect(filename, "^offshore_petroleum_platform_.+-\\d{2}-01\\.tif$")) {
    match <- stringr::str_match(filename, "^offshore_petroleum_platform_(.+)_(\\d{4})-(\\d{2})-01\\.tif$")
    if (!is.na(match[1])) {
      return(list(
        category = "offshore_petroleum",
        subcategory = "platform_monthly",
        type = match[2],
        year = as.integer(match[3]),
        month = as.integer(match[4]),
        filepath = filepath,
        nighttime = FALSE
      ))
    }
  }

  # Offshore wind farm: offshore_wind_farm_{type}.tif
  if (stringr::str_detect(filename, "^offshore_wind_farm_")) {
    match <- stringr::str_match(filename, "^offshore_wind_farm_(.+)\\.tif$")
    if (!is.na(match[1])) {
      return(list(
        category = "offshore_wind_farm",
        type = match[2],
        filepath = filepath,
        nighttime = FALSE
      ))
    }
  }

  # Petroleum pollution incidents: petroleum_pollution_incidents_{dataset}_{month}.tif
  # or petroleum_pollution_incidents_{dataset}_{month}_{type}.tif
  if (stringr::str_detect(filename, "^petroleum_pollution_incidents_")) {
    # Try specific type pattern first
    match <- stringr::str_match(filename, "^petroleum_pollution_incidents_(.+)_ ?(\\d+)_(.+)\\.tif$")
    if (!is.na(match[1])) {
      return(list(
        category = "petroleum_pollution",
        subcategory = "specific_type",
        dataset = match[2],
        month = as.integer(match[3]),
        type = match[4],
        filepath = filepath,
        nighttime = FALSE
      ))
    }

    # Try all types pattern
    match <- stringr::str_match(filename, "^petroleum_pollution_incidents_(.+)_ ?(\\d+)(?:_(.+))?\\.tif$")
    if (!is.na(match[1]) && match[1, 2] != "all") {
      return(list(
        category = "petroleum_pollution",
        subcategory = "all_types",
        dataset = match[2],
        month = as.integer(match[3]),
        filepath = filepath,
        nighttime = FALSE
      ))
    }
  }

  # Ship light detection: ship_light_detection_{month}.tif
  if (stringr::str_detect(filename, "^ship_light_detection_")) {
    match <- stringr::str_match(filename, "^ship_light_detection_(\\d+)\\.tif$")
    if (!is.na(match[1])) {
      return(list(
        category = "ship_light_detection",
        month = as.integer(match[2]),
        filepath = filepath,
        nighttime = TRUE
      ))
    }
  }

  # Shipping daily: shipping_intensity_density_{year}_{month}_{type}.tif
  if (stringr::str_detect(filename, "^shipping_intensity_density_")) {
    match <- stringr::str_match(filename, "^shipping_intensity_density_(\\d{4})_(\\d+)_(.+)\\.tif$")
    if (!is.na(match[1])) {
      return(list(
        category = "shipping",
        subcategory = "daily",
        year = as.integer(match[2]),
        month = as.integer(match[3]),
        type = match[4],
        filepath = filepath,
        nighttime = FALSE
      ))
    }
  }

  # Shipping nighttime: shipping_night_light_intensity_density_{year}_{month}_{type}.tif
  if (stringr::str_detect(filename, "^shipping_night_light_intensity_density_")) {
    match <- stringr::str_match(filename, "^shipping_night_light_intensity_density_(\\d{4})_(\\d+)_(.+)\\.tif$")
    if (!is.na(match[1])) {
      return(list(
        category = "shipping",
        subcategory = "nighttime",
        year = as.integer(match[2]),
        month = as.integer(match[3]),
        type = match[4],
        filepath = filepath,
        nighttime = TRUE
      ))
    }
  }

  return(NULL)
}

#' Create UI-optimized category structure
#' @param category The category name
#' @param cat_data List of parsed data for this category
#' @return List with types, years, months, nighttime, and layers components
create_category_structure <- function(category, cat_data) {
  # Convert to data.frame for easier manipulation
  layers_df <- data.frame(
    type = sapply(cat_data, function(x) x$type %||% NA_character_),
    year = sapply(cat_data, function(x) x$year %||% NA_integer_),
    month = sapply(cat_data, function(x) x$month %||% NA_integer_),
    year_range = sapply(cat_data, function(x) x$year_range %||% NA_character_),
    dataset = sapply(cat_data, function(x) x$dataset %||% NA_character_),
    species = sapply(cat_data, function(x) x$species %||% NA_character_),
    subcategory = sapply(cat_data, function(x) x$subcategory %||% NA_character_),
    filepath = sapply(cat_data, function(x) x$filepath),
    layer_id = sapply(cat_data, function(x) {
      fs::path_file(x$filepath) |>
        stringr::str_replace("\\.tif$", "")
    }),
    nighttime = sapply(cat_data, function(x) x$nighttime %||% FALSE),
    stringsAsFactors = FALSE
  )

  # Handle special cases for complex categories
  if (category == "fisheries") {
    # Separate gear-only vs species-specific
    gear_only <- layers_df[is.na(layers_df$species), ]
    species_specific <- layers_df[!is.na(layers_df$species), ]

    result <- list(
      gear_only = create_simple_structure(gear_only),
      species_specific = create_simple_structure(species_specific)
    )
  } else if (category == "offshore_petroleum_platforms") {
    # Separate monthly vs annual
    monthly <- layers_df[layers_df$subcategory == "platform_monthly" & !is.na(layers_df$subcategory), ]
    annual <- layers_df[layers_df$subcategory == "platform_annual" & !is.na(layers_df$subcategory), ]

    result <- list(
      monthly = create_simple_structure(monthly),
      annual = create_simple_structure(annual)
    )
  } else if (category == "petroleum_pollution") {
    # Normalize dataset as type - combine dataset and type into a single type field
    layers_df$combined_type <- ifelse(
      !is.na(layers_df$dataset) & !is.na(layers_df$type),
      paste(layers_df$dataset, layers_df$type, sep = "_"),
      ifelse(!is.na(layers_df$dataset), layers_df$dataset, layers_df$type)
    )
    layers_df$type <- layers_df$combined_type

    # Separate by subcategory but treat as simple structure with normalized types
    all_types <- layers_df[layers_df$subcategory == "all_types" & !is.na(layers_df$subcategory), ]
    specific_type <- layers_df[layers_df$subcategory == "specific_type" & !is.na(layers_df$subcategory), ]

    result <- list(
      all_types = create_simple_structure(all_types),
      specific_type = create_simple_structure(specific_type)
    )
  } else if (category == "shipping") {
    # Separate daily vs nighttime
    daily <- layers_df[layers_df$subcategory == "daily" & !is.na(layers_df$subcategory), ]
    nighttime <- layers_df[layers_df$subcategory == "nighttime" & !is.na(layers_df$subcategory), ]

    result <- list(
      daily = create_simple_structure(daily),
      nighttime = create_simple_structure(nighttime)
    )
  } else {
    # Simple categories (including offshore_petroleum_activity)
    result <- create_simple_structure(layers_df)
  }

  return(result)
}

#' Create simple structure for a category
#' @param df Data frame with layer information
#' @return List with types, years, months, nighttime, layers
create_simple_structure <- function(df) {
  if (nrow(df) == 0) {
    return(list(
      types = character(0),
      years = integer(0),
      months = integer(0),
      year_ranges = character(0),
      datasets = character(0),
      nighttime = FALSE,
      layers = data.frame()
    ))
  }

  # Extract unique values, removing NAs
  types <- unique(df$type[!is.na(df$type)])
  years <- sort(unique(df$year[!is.na(df$year)]))
  months <- sort(unique(df$month[!is.na(df$month)]))
  year_ranges <- unique(df$year_range[!is.na(df$year_range)])
  datasets <- unique(df$dataset[!is.na(df$dataset)])

  # Check if any layer is nighttime
  has_nighttime <- any(df$nighttime, na.rm = TRUE)

  list(
    types = types,
    years = years,
    months = months,
    year_ranges = year_ranges,
    datasets = datasets,
    nighttime = has_nighttime,
    layers = df
  )
}
