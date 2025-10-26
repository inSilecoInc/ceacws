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
      # # To add a camera icon to export map as PNG, to implement in the future
      #       leaflet::addControl(
      #         html = '
      #   <a id="snapshot-btn"
      #      title="Download Map Snapshot"
      #      style="
      #        background: #ffffff00;
      #        border: 1px solid #fffff00;
      #        border-radius: 4px;
      #        width: 20px;
      #        height: 20px;
      #        display: flex;
      #        align-items: center;
      #        justify-content: center;
      #        text-decoration: none;
      #        padding-bottom: 4px;
      #        cursor: pointer;">
      #     <i class="fa fa-camera" style="font-size: 22px; color: #000;"></i>
      #   </a>
      # ',
      #         position = "topright"
      #       )
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

        # Map display options (toggles)
        div(
          # Display options toggles
          div(
            class = "row mb-3",
            div(
              class = "col-6",
              shinyWidgets::prettySwitch(
                inputId = ns("log_transform"),
                label = "Log transform values",
                value = FALSE,
                status = "primary",
                inline = TRUE
              )
            ),
            div(
              class = "col-6",
              # This toggle will be conditionally shown when processed rasters are available
              uiOutput(ns("processed_layers_toggle"))
            )
          ),

          # Layer selection cards
          tags$label("Select layers:", class = "form-label mb-3"),
          div(
            class = "row g-2",
            lapply(names(rasters), function(layer_id) {
              layer <- rasters[[layer_id]]
              div(
                class = "col-6",
                create_toggle_layer_card(ns, layer)
              )
            })
          )
        )
      })

      # Conditional UI for processed layers toggle
      output$processed_layers_toggle <- renderUI({
        # Only show toggle if processed rasters are available
        if (length(processed_rasters()) > 0) {
          shinyWidgets::prettySwitch(
            inputId = ns("show_processed"),
            label = "Show processed layers",
            value = FALSE,
            status = "success",
            inline = TRUE
          )
        }
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

    # Create reactive for log-transformed rasters
    transformed_rasters <- reactive({
      rasters <- stored_rasters()
      if (length(rasters) == 0) {
        return(list())
      }

      # Check if log transform is enabled
      log_enabled <- isTRUE(input$log_transform)

      if (!log_enabled) {
        return(rasters) # Return original rasters
      }

      # Apply log transformation to each raster
      lapply(rasters, function(layer) {
        tryCatch(
          {
            transformed_layer <- log_transform_layer(layer)
            return(transformed_layer)
          },
          error = function(e) {
            # If transformation fails, return original layer
            warning(paste("Log transformation failed for layer:", layer$name))
            return(layer)
          }
        )
      })
    })

    # Create a reactive to collect all switch states
    selected_layers <- reactive({
      rasts <- transformed_rasters() # Use transformed rasters for selection
      if (length(rasts) == 0) {
        return(character())
      }

      selected <- character()
      for (layer_id in names(rasts)) {
        switch_input <- input[[paste0("layer_", layer_id)]]
        if (isTRUE(switch_input)) {
          selected <- c(selected, layer_id)
        }
      }
      selected
    })

    observeEvent(
      {
        list(selected_layers(), input$log_transform)
      },
      {
        rasts <- transformed_rasters() # Use transformed rasters for map rendering
        old <- prev_selected()
        new <- selected_layers()

        # When log transform changes, we need to re-render all selected layers
        # So we treat all current selections as "added" and remove existing versions
        added <- new
        removed <- setdiff(old, new)

        # If layers are currently selected, remove them first to re-render with new transform
        if (length(new) > 0) {
          for (id in new) {
            map_proxy <- leaflet::leafletProxy(ns("mapId")) |>
              leaflet::removeImage(layerId = id) |>
              leaflet::removeControl(layerId = paste0("legend_", id))
          }
        }

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
        if (length(new) == 0) {
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

            # Create legend title with proper units (adjust for log transformation)
            base_title <- create_legend_title(layer$name, layer_category, layer_subcategory, layer_type)
            legend_title <- if (isTRUE(layer$metadata$log_transformed)) {
              # Add "Log " prefix to units for transformed data
              gsub("<br><i>", "<br><i>Log ", base_title)
            } else {
              base_title
            }

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
  if (is.null(layer_type) || all(layer_type == "") || all(names(subcategory_data) == "default")) {
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

#' Create Toggle Layer Card
#'
#' @description Creates a card for layer selection with toggle switch
#'
#' @param ns Namespace function
#' @param layer Layer object with metadata
#' @noRd
create_toggle_layer_card <- function(ns, layer) {
  # Extract all metadata for display
  filters_applied <- layer$metadata$filters_applied
  combination_method <- layer$metadata$filters_applied$combination_method %||% "None"

  # Build detailed filter information
  filter_details <- list()

  if (!is.null(filters_applied)) {
    # Subcategory
    if (!is.null(filters_applied$subcategory) && length(filters_applied$subcategory) > 0 && all(filters_applied$subcategory != "")) {
      filter_details$Subcategory <- paste(filters_applied$subcategory, collapse = ", ")
    }

    # Type
    if (!is.null(filters_applied$type) && length(filters_applied$type) > 0 && all(filters_applied$type != "")) {
      filter_details$Type <- paste(filters_applied$type, collapse = ", ")
    }

    # Year
    if (!is.null(filters_applied$year) && length(filters_applied$year) > 0) {
      filter_details$Year <- paste(sort(filters_applied$year), collapse = ", ")
    }

    # Months
    if (!is.null(filters_applied$month) && length(filters_applied$month) > 0) {
      if (length(filters_applied$month) == 12) {
        filter_details$Months <- "All months"
      } else if (length(filters_applied$month) == 1) {
        month_name <- month.name[as.numeric(filters_applied$month)]
        filter_details$Months <- month_name
      } else {
        month_names <- month.name[as.numeric(filters_applied$month)]
        filter_details$Months <- paste(month_names, collapse = ", ")
      }
    }
  }

  div(
    class = "card mb-3 h-100",
    div(
      class = "card-header d-flex justify-content-between align-items-center",
      h6(class = "mb-1", layer$name),
      shinyWidgets::prettySwitch(
        inputId = ns(paste0("layer_", layer$id)),
        label = NULL,
        value = FALSE,
        status = "primary",
        inline = TRUE
      )
    ),
    div(
      class = "card-body",
      tags$small(
        class = "text-muted",
        # Display all filter details
        lapply(names(filter_details), function(detail_name) {
          tags$div(
            tags$strong(paste0(detail_name, ": ")),
            filter_details[[detail_name]]
          )
        }),
        # Show combination method if applicable
        if (combination_method != "None") {
          tags$div(
            tags$strong("Combination: "), combination_method
          )
        }
      )
    )
  )
}



log_transform_layer <- function(layer) {
  # Create a copy of the layer
  transformed_layer <- layer

  # Apply log transformation to the raster data
  raster_values <- terra::values(layer$raster)

  # Handle zeros and negative values (add small constant if needed)
  raster_values[raster_values <= 0] <- NA

  # Apply log transformation
  log_values <- log(raster_values + 1)

  # Update the raster with transformed values
  terra::values(transformed_layer$raster) <- log_values

  # Update metadata to indicate transformation
  transformed_layer$metadata$log_transformed <- TRUE
  transformed_layer$name <- paste0(layer$name, " (Log+1)")
  return(transformed_layer)
}
