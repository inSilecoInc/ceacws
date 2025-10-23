#' threat_layers_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_threat_layers_processing_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        bslib::card(
          bslib::accordion(
            id = ns("processing"),
            bslib::accordion_panel(
              title = "Threat layers",
              uiOutput(ns("selected_rasters"))
            ),
            bslib::accordion_panel(
              title = "Spatial processing",
              uiOutput(ns("spatial_processing_ui"))
            ),
            bslib::accordion_panel(
              title = "Export",
              uiOutput(ns("export_ui"))
            )
          )
        )
      ),
      column(
        width = 8,
        bslib::card(
          leaflet::leafletOutput(ns("mapId"), height = 800)
        )
      )
    )
  )
}

#' threat_layers_processing Server Functions
#'
#' @param stored_rasters Reactive containing the stored_rasters list
#' @param r ReactiveValues object containing map and other state
#'
#' @noRd
mod_threat_layers_processing_server <- function(id, stored_rasters, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize with base_map()
    output$mapId <- leaflet::renderLeaflet({
      base_map()
    })

    # Update UI when stored_rasters changes
    observe({
      rasters <- stored_rasters()

      output$selected_rasters <- renderUI({
        if (length(rasters) == 0) {
          return(div(
            p("No threat layers available", style = "color: #999;"),
          ))
        }

        # Create checkbox for available rasters
        choices <- setNames(
          names(rasters),
          vapply(rasters, function(x) {
            stringr::str_to_sentence(
              paste0(x$id, ": ", x$name)
            )
          }, character(1))
        )
        checkboxGroupInput(
          inputId = ns("selected_rasters"),
          label = "Select layers:",
          choices = choices,
          selected = NULL
        )
      })

      # Spatial processing UI
      output$spatial_processing_ui <- renderUI({
        if (length(rasters) == 0) {
          return(div(
            p("No threat layers available", style = "color: #999;"),
          ))
        }
        mod_spatial_resampling_ui(ns("resampling"))
      })

      # Export UI
      output$export_ui <- renderUI({
        if (length(rasters) == 0) {
          return(div(
            p("No threat layers available", style = "color: #999;"),
          ))
        }

        div(
          h5("Export Rasters"),

          # Summary section
          div(
            class = "mb-3",
            h6("Summary"),
            verbatimTextOutput(ns("export_summary"))
          ),

          # Export options
          div(
            class = "mb-3",
            h6("Export Options"),
            checkboxInput(
              inputId = ns("export_stored"),
              label = "Export stored rasters (original)",
              value = TRUE
            ),
            checkboxInput(
              inputId = ns("export_processed"),
              label = "Export processed rasters",
              value = FALSE
            )
          ),

          # Download button
          div(
            class = "mt-3",
            downloadButton(
              outputId = ns("download_rasters"),
              label = "Download Archive",
              class = "btn btn-success"
            )
          )
        )
      })
    })

    # Track previous selection
    prev_selected <- reactiveVal(character())

    observeEvent(input$selected_rasters,
      {
        rasts <- stored_rasters()
        old <- prev_selected()
        new <- input$selected_rasters
        added <- setdiff(new, old)
        removed <- setdiff(old, new)

        # proxy to existing map
        map_proxy <- leaflet::leafletProxy(ns("mapId"))

        # add new rasters
        shinycssloaders::showPageSpinner(
          background = "#cccccccc",
          color = "#333333",
          caption = "Rendering",
          image = "www/img/insil.gif",
          image.width = "200",
          image.height = "200"
        )
        if (length(input$selected_rasters) == 0) {
          map_proxy <- map_proxy |>
            leaflet::clearImages() |>
            leaflet::clearControls() # Clear all legends when no layers
        } else {
          for (id in added) {
            layer <- rasts[[id]]

            # Extract layer metadata for appropriate styling and units
            layer_category <- layer$metadata$filters_applied$category %||% "default"
            layer_subcategory <- layer$metadata$filters_applied$subcategory %||% NULL
            layer_type <- layer$metadata$filters_applied$type %||% NULL

            # Generate color palette for this layer
            color_palette <- generate_raster_palette(layer$raster, layer_category)

            # Create legend title with proper units
            legend_title <- create_legend_title(layer$name, layer_category, layer_subcategory, layer_type)

            # Add raster with color palette
            map_proxy <- map_proxy |>
              leaflet::addRasterImage(
                layer$raster,
                colors = color_palette,
                layerId = id,
                group = layer$name,
                opacity = 0.7,
                project = TRUE
              ) |>
              leaflet::addLegend(
                position = "bottomright",
                pal = color_palette,
                values = terra::values(layer$raster, na.rm = TRUE),
                title = legend_title,
                layerId = paste0("legend_", id),
                group = layer$name,
                opacity = 1,
                className = "custom-legend"
              )
          }
        }

        # remove deselected rasters and their legends
        for (id in removed) {
          map_proxy <- map_proxy |>
            leaflet::removeImage(layerId = id) |>
            leaflet::removeControl(layerId = paste0("legend_", id))
        }

        shinycssloaders::hidePageSpinner()

        prev_selected(new)
      },
      ignoreNULL = FALSE
    )

    # Start module for spatial processing
    processed_rasters <- mod_spatial_resampling_server("resampling", stored_rasters)

    # Export summary output
    output$export_summary <- renderText({
      stored_count <- length(stored_rasters())
      processed_count <- length(processed_rasters())

      paste0(
        "Stored rasters: ", stored_count, "\n",
        "Processed rasters: ", processed_count
      )
    })

    # Download handler for raster export
    output$download_rasters <- downloadHandler(
      filename = function() {
        paste0("ceacws_threat_layers_", Sys.Date(), ".zip")
      },
      content = function(file) {
        # Validate export selections
        export_stored <- isTRUE(input$export_stored)
        export_processed <- isTRUE(input$export_processed)

        if (!export_stored && !export_processed) {
          showNotification("No export options selected", type = "warning")
          return()
        }

        # Check if there are rasters to export
        stored_count <- length(stored_rasters())
        processed_count <- length(processed_rasters())

        if (export_stored && stored_count == 0) {
          showNotification("No stored rasters available for export", type = "warning")
          return()
        }

        if (export_processed && processed_count == 0) {
          showNotification("No processed rasters available for export", type = "warning")
          return()
        }

        if ((!export_stored || stored_count == 0) &&
          (!export_processed || processed_count == 0)) {
          showNotification("No rasters available for export", type = "warning")
          return()
        }

        # Show progress
        showNotification("Preparing archive...", type = "message")

        tryCatch(
          {
            # Create archive using helper function
            archive_path <- export_rasters_archive(
              stored_rasters = stored_rasters(),
              processed_rasters = processed_rasters(),
              export_stored = export_stored,
              export_processed = export_processed,
              filename = "ceacws_threat_layers"
            )

            # Copy to download location
            file.copy(archive_path, file)

            showNotification("Archive created successfully!", type = "message")
          },
          error = function(e) {
            showNotification(paste("Export failed:", e$message), type = "error")
          }
        )
      }
    )
  })
}

#' Generate Color Palette for Raster Data
#'
#' Creates appropriate color palettes for threat layer visualization
#'
#' @param raster_data Terra SpatRaster object
#' @param layer_category Character string of layer category for palette selection
#' @return leaflet colorNumeric palette function
#' @noRd
generate_raster_palette <- function(raster_data, layer_category = NULL) {
  # Extract non-NA values for domain
  values <- terra::values(raster_data, na.rm = TRUE)

  if (length(values) == 0) {
    # Return a default palette if no values
    return(leaflet::colorNumeric("viridis", domain = c(0, 1), na.color = "transparent"))
  }

  # Choose color palette based on layer category
  palette_name <- switch(layer_category,
    "offshore_wind_farm" = "Blues",
    "offshore_petroleum_platform" = "Oranges",
    "offshore_petroleum_activity" = "Reds",
    "shipping_night_light_intensity_density" = "plasma",
    "shipping_ais" = "viridis",
    "petroleum_pollution_incidents" = "Reds",
    "fisheries" = "Greens",
    "viridis" # default
  )

  leaflet::colorNumeric(
    palette = palette_name,
    domain = range(values, na.rm = TRUE),
    na.color = "transparent"
  )
}

#' Get Layer Units from Metadata
#'
#' Extracts appropriate units for a threat layer based on category
#'
#' @param layer_category Character string of layer category
#' @return Character string of units
#' @noRd
get_layer_units <- function(layer_category, layer_subcategory = NULL, layer_type = NULL) {
  # Use global threat_layer_units metadata
  if (!exists("threat_layer_units")) {
    return("Intensity")
  }

  # Navigate the nested structure: category -> subcategory -> type
  category_data <- threat_layer_units[[layer_category]]

  if (is.null(category_data)) {
    return(threat_layer_units[["default"]] %||% "Intensity")
  }

  # If category_data is a string, return it directly
  if (is.character(category_data)) {
    return(category_data)
  }

  # If no subcategory specified, try "default"
  if (is.null(layer_subcategory) || layer_subcategory == "") {
    subcategory_data <- category_data[["default"]]
  } else {
    subcategory_data <- category_data[[layer_subcategory]]
  }

  if (is.null(subcategory_data)) {
    # Try default subcategory if specific one not found
    subcategory_data <- category_data[["default"]]
  }

  if (is.null(subcategory_data)) {
    return(threat_layer_units[["default"]] %||% "Intensity")
  }

  # If subcategory_data is a string, return it directly
  if (is.character(subcategory_data)) {
    return(subcategory_data)
  }

  # Navigate to type level
  if (is.null(layer_type) || layer_type == "") {
    type_data <- subcategory_data[["default"]]
  } else {
    type_data <- subcategory_data[[layer_type]]
  }

  if (is.null(type_data)) {
    # Try default type if specific one not found
    type_data <- subcategory_data[["default"]]
  }

  if (is.null(type_data)) {
    return(threat_layer_units[["default"]] %||% "Intensity")
  }

  return(type_data)
}

#' Create Legend Title for Layer
#'
#' Generates descriptive legend titles combining layer name and units
#'
#' @param layer_name Character string of layer name
#' @param layer_category Character string of layer category
#' @return Character string for legend title
#' @noRd
create_legend_title <- function(layer_name, layer_category, layer_subcategory = NULL, layer_type = NULL) {
  units <- get_layer_units(layer_category, layer_subcategory, layer_type)
  clean_name <- tools::toTitleCase(gsub("_", " ", layer_name))
  # Use HTML formatting for multi-line legend with italicized units
  paste0(clean_name, "<br><i>", units, "</i>")
}
