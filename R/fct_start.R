#' Start function
#'
#' @export

fct_start <- function() {
    # create /docs for reports output
    dir.create(file.path(app_sys("app", "www"), "docs"))

    # create disclaimer.html
    disc_path_md <<- app_sys("app", "www", "static_docs", "disclaimer.md")
    disc_frontm <<- rmarkdown::yaml_front_matter(disc_path_md)

    # Render md document
    path_docs <<- app_sys("app", "www", "static_docs")
    list.files(
        path_docs,
        pattern = ".md$",
        full.names = TRUE
    ) |> lapply(
        \(x) {
            markdown::mark(x, x |> fs::path_ext_set("html"))
        }
    )

    map_bbox <<- list(lng1 = -75, lat1 = 35, lng2 = -35, lat2 = 67)

    # Initialize UI-ready threat layers data
    threat_layers_list <<- prepare_ui_threat_layers_data()

    # Validate that threat layers were found
    if (length(threat_layers_list) == 0) {
        stop("No threat layers found. Check that workspace/data/analyzed/ contains valid .tif files.")
    }

    # Pre-compute category choices for UI
    category_choices <- names(threat_layers_list)
    category_labels <- gsub("_", " ", category_choices)
    names(category_choices) <- tools::toTitleCase(category_labels)
    threat_layer_categories <<- c("Select category..." = "", category_choices)
    
    # Initialize layer units metadata based on category, subcategory, and type
    threat_layer_units <<- list(
      # Night lights
      "night_lights" = list(
        "default" = "nW/cm²/sr/month"
      ),
      
      # Offshore petroleum 
      "offshore_petroleum" = list(
        "activity" = list(
          "default" = "Presence / absence"
        ),
        "platform_annual" = list(
          "total_heat" = "Total heat in Kelvin",
          "detection_frequency" = "Sum of percent detection frequency"
        ),
        "platform_monthly" = list(
          "default" = "Total heat in Kelvin"
        )
      ),
      
      # Offshore wind farm
      "offshore_wind_farm" = list(
        "default" = "Presence / absence"
      ),
      
      # Petroleum pollution
      "petroleum_pollution" = list(
        "all_types" = list(
          "default" = "Number of incidents / month"
        ),
        "specific_type" = list(
          "default" = "Number of incidents / month"
        )
      ),
      
      # Ship light detection
      "ship_light_detection" = list(
        "default" = "nW/cm²/sr/month"
      ),
      
      # Shipping
      "shipping" = list(
        "daily" = list(
          "default" = "Number of ships per day"
        ),
        "nighttime" = list(
          "default" = "Number of vessels per night per day"
        )
      ),
      
      # Fallback
      "default" = "Intensity"
    )

    onStop(clean_up_app)
}

clean_up_app <- function() {
    cli::cli_alert_info("Application stopped -- cleaning up")
    unlink(file.path(app_sys("app", "www"), "docs"), recursive = TRUE)
    unlink(disc_path_md |> fs::path_ext_set("html"))
}
