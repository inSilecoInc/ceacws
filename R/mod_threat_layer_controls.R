#' Threat Layer Controls Module UI
#'
#' @description A shiny Module for controlling threat layer visibility on map
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList div checkboxInput actionButton icon p strong h4
#' @importFrom shiny moduleServer observe observeEvent reactive renderUI reactiveValues reactiveValuesToList uiOutput
mod_threat_layer_controls_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # h4("Threat Layers"),
    uiOutput(ns("layer_controls"))
  )
}

#' Threat Layer Controls Module Server
#'
#' @description Server logic for threat layer controls
#'
#' @param id Internal parameter for {shiny}
#' @param threat_layers_data Reactive containing processed raster data
#' @param r ReactiveValues object containing map and other state
#'
#' @noRd
mod_threat_layer_controls_server <- function(id, threat_layers_data, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Internal state for layer visibility (isolated from parent)
    layer_visibility <- reactiveValues()

    # Internal state for layer expansion (collapsed by default)
    layer_expanded <- reactiveValues()

    # Render layer controls
    output$layer_controls <- renderUI({
      layers <- threat_layers_data()

      if (length(layers) == 0) {
        return(p("No threat layers loaded.", style = "color: #999;"))
      }

      layer_cards <- lapply(names(layers), function(layer_name) {
        layer <- layers[[layer_name]]

        # Initialize visibility state if not set
        if (is.null(layer_visibility[[layer_name]])) {
          layer_visibility[[layer_name]] <- FALSE
        }

        # Initialize expanded state if not set (collapsed by default)
        if (is.null(layer_expanded[[layer_name]])) {
          layer_expanded[[layer_name]] <- FALSE
        }

        # Truncate display name if too long
        display_name <- layer$display_name
        truncated_name <- if (nchar(display_name) > 35) {
          paste0(substr(display_name, 1, 32), "...")
        } else {
          display_name
        }

        div(
          class = "card mb-2",
          div(
            class = "card-body p-2",
            div(
              class = "d-flex align-items-center",
              checkboxInput(
                ns(paste0("layer_", layer_name)),
                "",
                value = layer_visibility[[layer_name]],
                width = "auto"
              ),
              div(
                class = "flex-grow-1",
                style = "margin-left: 10px;",
                strong(
                  truncated_name,
                  title = display_name
                )
              ),
              actionButton(
                ns(paste0("expand_", layer_name)),
                icon(if (layer_expanded[[layer_name]]) "chevron-down" else "chevron-right"),
                class = "btn-sm btn-outline-secondary",
                style = "padding: 2px 6px; margin-left: 5px;"
              )
            ),

            # Layer details (conditionally visible)
            if (layer_expanded[[layer_name]]) {
              div(
                class = "mt-2 pt-2",
                style = "border-top: 1px solid #dee2e6; font-size: 0.875em; color: #6c757d;",
                p(
                  class = "mb-1",
                  paste(layer$file_count, "files combined")
                ),
                if (!is.null(layer$combination_method)) {
                  p(
                    class = "mb-1",
                    paste("Method:", tools::toTitleCase(layer$combination_method))
                  )
                },
                p(
                  class = "mb-0",
                  paste("Category:", tools::toTitleCase(gsub("_", " ", layer$criteria$category)))
                )
              )
            }
          )
        )
      })

      do.call(tagList, layer_cards)
    })

    # Handle checkbox changes for layer visibility
    observe({
      layers <- threat_layers_data()

      for (layer_name in names(layers)) {
        layer_data <- layers[[layer_name]]

        # Create individual observer for each checkbox
        local({
          current_layer_name <- layer_name
          current_layer_data <- layer_data
          checkbox_id <- paste0("layer_", current_layer_name)

          observeEvent(input[[checkbox_id]],
            {
              is_visible <- input[[checkbox_id]]

              if (!is.null(is_visible) && !is.null(r$map)) {
                if (is_visible) {
                  # Add layer to map
                  r$map <- r$map |>
                    leaflet::addRasterImage(
                      current_layer_data$raster,
                      layerId = current_layer_name,
                      group = current_layer_data$display_name,
                      opacity = 0.7,
                      project = TRUE
                    )
                  layer_visibility[[current_layer_name]] <- TRUE
                } else {
                  # Remove layer from map
                  r$map <- r$map |>
                    leaflet::removeImage(layerId = current_layer_name)
                  layer_visibility[[current_layer_name]] <- FALSE
                }
              }
            },
            ignoreInit = TRUE
          )
        })
      }
    })

    # Handle expand/collapse button clicks
    observe({
      layers <- threat_layers_data()

      for (layer_name in names(layers)) {
        # Create individual observer for each expand button
        local({
          current_layer_name <- layer_name
          expand_button_id <- paste0("expand_", current_layer_name)

          observeEvent(input[[expand_button_id]],
            {
              # Toggle expanded state
              layer_expanded[[current_layer_name]] <- !layer_expanded[[current_layer_name]]
            },
            ignoreInit = TRUE
          )
        })
      }
    })

    # Return layer visibility states for external use
    return(reactive(reactiveValuesToList(layer_visibility)))
  })
}
