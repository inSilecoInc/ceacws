#' Threat Layers Module UI
#'
#' @description A shiny Module for threat layer selection and configuration
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fluidRow column h3 div conditionalPanel
#' @importFrom shiny actionButton icon br
#' @importFrom shinyjs show hide
mod_threat_layers_selection_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Title row with visualize button on the right
    fluidRow(
      column(2),
      column(6, h3("Threat layers")),
      column(
        2,
        div(
          style = "text-align: right; margin-top: 10px;",
          conditionalPanel(
            condition = paste0("output['", ns("show_visualize"), "']"),
            actionButton(ns("visualize_map"), "Visualize layers on map", class = "btn-success")
          )
        )
      ),
      column(2)
    ),

    # Add new layer button underneath title
    fluidRow(
      column(2),
      column(
        8,
        div(
          style = "margin-bottom: 15px;",
          actionButton(ns("add"), "Add a new layer", icon = icon("circle-plus")),
          actionButton(ns("cancel"), "Cancel",
            class = "btn-secondary",
            style = "margin-left: 10px; display: none;"
          )
        )
      ),
      column(2)
    ),

    # Conditional panel for layer filtering (shown when add button is clicked)
    conditionalPanel(
      condition = paste0("output['", ns("show_filters_panel"), "']"),
      fluidRow(
        column(2),
        column(
          8,
          mod_layer_filters_ui(ns("filters")),

          # Add Layer button - hidden by default, shown when valid selection exists
          br(),
          actionButton(
            ns("add_layer_btn"),
            "Add Layer",
            class = "btn-primary",
            style = "display: none;" # Hidden by default
          )
        ),
        column(2)
      )
    ),


    # Added layers display area
    fluidRow(
      column(2),
      column(
        8,
        br(),
        h4("Stored rasters"),
        uiOutput(ns("layer_cards"))
      ),
      column(2)
    )
  )
}

#' Threat Layers Module Server
#'
#' @param id Internal parameter for {shiny}
#'
#' @noRd
#' @importFrom shiny moduleServer reactive reactiveVal outputOptions observe observeEvent
#' @importFrom shiny showNotification removeNotification renderUI div h5 h6 p actionButton
#' @importFrom shiny icon strong br tags tagList
#' @importFrom shinyjs show hide
mod_threat_layers_selection_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive value to control filtering panel visibility
    show_filters <- reactiveVal(FALSE)

    # Layer storage management
    stored_layers <- reactiveVal(list())
    layer_counter <- reactiveVal(0)

    # Initialize the filtering module
    filter_results <- mod_layer_filters_server("filters")

    # Render layer cards directly
    output$layer_cards <- renderUI({
      layers <- stored_layers()

      if (length(layers) == 0) {
        div(
          class = "text-center text-muted",
          style = "padding: 2rem;",
          h5("No layers added"),
          p("Use the filters above to select and add threat layers")
        )
      } else {
        # Create cards for each layer
        cards <- lapply(names(layers), function(layer_id) {
          layer <- layers[[layer_id]]
          create_layer_card(ns, layer)
        })

        div(
          class = "layer-cards-container",
          do.call(tagList, cards)
        )
      }
    })

    # Handle remove layers button clicks
    observe({
      layers <- stored_layers()
      layer_ids <- names(layers)

      for (layer_id in layer_ids) {
        button_name <- paste0("remove_", layer_id)
        if (!is.null(input[[button_name]]) && input[[button_name]] > 0) {
          # Get layer name for notification
          layer_name <- layers[[layer_id]]$name

          # Remove the layer
          updated_layers <- remove_layer_from_collection(layer_id, stored_layers())
          stored_layers(updated_layers)

          # Show notification
          showNotification(
            paste("Removed layer:", layer_name),
            type = "message",
            duration = 3
          )
        }
      }
    })

    # Output for conditional panel visibility
    output$show_filters_panel <- reactive({
      show_filters()
    })
    outputOptions(output, "show_filters_panel", suspendWhenHidden = FALSE)

    # Handle Add new layer button click
    observeEvent(input$add, {
      show_filters(TRUE)
      shinyjs::show("cancel")
    })

    # Handle Cancel button click
    observeEvent(input$cancel, {
      show_filters(FALSE)
      shinyjs::hide("cancel")

      # Reset all filtering state
      filter_results$reset()
    })


    # Monitor filter results and show Add Layer button when valid selection exists
    observe({
      selection <- filter_results$selection()

      if (!is.null(selection) && !is.null(selection$count) && selection$count > 0) {
        # Show "Add Layer" button when valid selection exists
        shinyjs::show("add_layer_btn")
      } else {
        # Hide "Add Layer" button when no valid selection
        shinyjs::hide("add_layer_btn")
      }
    })

    # Handle Add Layer button click
    observeEvent(input$add_layer_btn, {
      selection <- filter_results$selection()

      tryCatch(
        {
          # Show processing notification
          processing_id <- showNotification(
            paste("Processing", selection$count, "files..."),
            type = "message",
            duration = NULL
          )

          # Process raster files
          combined_raster <- combine_rasters_gdalcubes(
            selection$files$filepath,
            selection$criteria$combination_method
          )

          # Add to layer collection
          result <- add_layer_to_collection(
            combined_raster,
            selection,
            stored_layers(),
            layer_counter()
          )

          # Update reactive values
          stored_layers(result)
          layer_counter(layer_counter() + 1)

          # Remove processing notification and show success
          removeNotification(processing_id)
          showNotification(
            "Layer added successfully",
            type = "message",
            duration = 3
          )
        },
        error = function(e) {
          # Handle errors gracefully
          removeNotification(processing_id)
          showNotification(
            paste("Error processing layer:", e$message),
            type = "error",
            duration = 5
          )
        }
      )

      # Reset all filtering state
      show_filters(FALSE)
      shinyjs::hide("cancel")
      filter_results$reset()
    })

    # Return for parent module
    list(
      stored_rasters = reactive({
        stored_layers()
      })
    )
  })
}

# ============================================================================
# Layer Storage Management Helper Functions
# ============================================================================

#' Generate unique layer ID
generate_layer_id <- function(counter) {
  paste0("layer_", sprintf("%03d", counter + 1))
}

#' Generate user-friendly layer name from filters
generate_layer_name <- function(selection) {
  filters <- selection$criteria
  parts <- c()

  # Add category
  if (!is.null(filters$category)) {
    parts <- c(parts, gsub("_", " ", filters$category))
  }

  # Add subcategory
  if (!is.null(filters$subcategory)) {
    parts <- c(parts, gsub("_", " ", filters$subcategory))
  }

  # Add type if available
  if (!is.null(filters$type) && length(filters$type) > 0) {
    if (length(filters$type) <= 2) {
      parts <- c(parts, paste(filters$type, collapse = ", "))
    } else {
      parts <- c(parts, paste0(length(filters$type), " types"))
    }
  }

  # Add year
  if (!is.null(filters$year) && length(filters$year) > 0) {
    if (length(filters$year) == 1) {
      parts <- c(parts, as.character(filters$year))
    } else {
      parts <- c(parts, paste0(min(filters$year), "-", max(filters$year)))
    }
  }

  # Fallback name
  if (length(parts) == 0) {
    parts <- "Threat Layer"
  }

  paste(parts, collapse = " ")
}

#' Add layer to collection
add_layer_to_collection <- function(raster_object, selection, stored_layers, layer_counter) {
  layer_id <- generate_layer_id(layer_counter)
  layer_name <- generate_layer_name(selection)

  # Create layer object
  layer <- list(
    id = layer_id,
    name = layer_name,
    raster = raster_object,
    metadata = list(
      file_count = selection$count,
      source_files = selection$files$filepath,
      filters_applied = selection$criteria,
      created_at = Sys.time(),
      extent = sf::st_bbox(raster_object),
      resolution = attr(raster_object, "dimensions")$delta
    )
  )

  # Add to storage
  current_layers <- stored_layers
  current_layers[[layer_id]] <- layer

  current_layers
}

#' Remove layer from collection
remove_layer_from_collection <- function(layer_id, stored_layers) {
  stored_layers[[layer_id]] <- NULL
  stored_layers
}

#' Create individual layer card
#' @noRd
create_layer_card <- function(ns, layer) {
  div(
    class = "card mb-3",
    div(
      class = "card-header d-flex justify-content-between align-items-center",
      h6(class = "mb-0", layer$name),
      div(
        class = "btn-group btn-group-sm",
        actionButton(
          ns(paste0("remove_", layer$id)),
          "",
          icon = icon("trash"),
          class = "btn btn-outline-danger btn-sm",
          title = "Remove layer"
        )
      )
    ),
    div(
      class = "card-body",
      div(
        class = "row",
        div(
          class = "col-6",
          tags$small(
            class = "text-muted",
            strong("Files: "), layer$metadata$file_count, br(),
            strong("Created: "),
            format(layer$metadata$created_at, "%Y-%m-%d %H:%M")
          )
        ),
        div(
          class = "col-6",
          tags$small(
            class = "text-muted",
            strong("Method: "),
            layer$metadata$filters_applied$combination_method %||% "single", br(),
            strong("Resolution: "),
            if (!is.null(layer$metadata$resolution)) {
              paste(round(layer$metadata$resolution, 4), collapse = " x ")
            } else {
              "Unknown"
            }
          )
        )
      ),
      # Show applied filters if available
      if (!is.null(layer$metadata$filters_applied)) {
        div(
          class = "col-6",
          tags$small(
            class = "text-muted",
            strong("Filters: "),
            render_filter_summary(layer$metadata$filters_applied)
          )
        )
      }
    )
  )
}

#' Render filter summary for display
#' @noRd
render_filter_summary <- function(filters) {
  parts <- c()

  if (!is.null(filters$category)) {
    parts <- c(parts, paste("Category:", filters$category))
  }

  if (!is.null(filters$subcategory)) {
    parts <- c(parts, paste("Subcategory:", filters$subcategory))
  }

  if (!is.null(filters$year) && length(filters$year) > 0) {
    if (length(filters$year) == 1) {
      parts <- c(parts, paste("Year:", filters$year))
    } else {
      parts <- c(parts, paste(
        "Years:", min(filters$year), "-",
        max(filters$year)
      ))
    }
  }

  if (!is.null(filters$type) && length(filters$type) > 0) {
    if (length(filters$type) <= 2) {
      parts <- c(parts, paste("Type:", paste(filters$type, collapse = ", ")))
    } else {
      parts <- c(parts, paste("Types:", length(filters$type), "selected"))
    }
  }

  if (length(parts) == 0) {
    return("No filters applied")
  }

  paste(parts, collapse = "; ")
}

#' Null-coalescing operator
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
