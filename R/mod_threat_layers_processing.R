#' threat_layers_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fluidRow column verbatimTextOutput uiOutput
#' @importFrom bslib card accordion accordion_panel
mod_threat_layers_processing_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        card(
          accordion(
            id = ns("processing"),
            accordion_panel(
              title = "Threat layers",
              uiOutput(ns("layer_checkboxes"))
            ),
            accordion_panel(
              title = "Processing",
              # Processing options content will go here
            ),
            accordion_panel(
              title = "Export",
              # Export options content will go here
            )
          )
        )
      ),
      column(
        width = 8,
        card(
          mapedit::editModUI("map-select")
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
#' @importFrom shiny moduleServer renderText renderUI checkboxInput observe observeEvent reactiveValues reactive
#' @importFrom leaflet addRasterImage removeImage
mod_threat_layers_processing_server <- function(id, stored_rasters, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update UI when stored_rasters changes
    observeEvent(stored_rasters(), {
      rasters <- stored_rasters()

      output$layer_checkboxes <- renderUI({
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
    })


    # # keep track of previous selection
    # prev_selected <- reactiveVal(character())

    # observeEvent(input$selected_rasters, {
    #   req(r$map)
    #   rasters <- stored_rasters()

    #   old <- prev_selected()
    #   new <- input$selected_rasters

    #   # find differences
    #   added <- setdiff(new, old)
    #   removed <- setdiff(old, new)

    #   # add new rasters
    #   for (id in added) {
    #     layer <- rasters[[id]]

    #     r$map <- r$map |>
    #       leaflet::addRasterImage(
    #         layer$raster,
    #         layerId = id,
    #         group   = layer$name,
    #         opacity = 0.7,
    #         project = TRUE
    #       )
    #   }

    #   # remove deselected rasters
    #   for (id in removed) {
    #     r$map <- r$map |>
    #       leaflet::removeImage(layerId = id)
    #   }

    #   # update state
    #   prev_selected(new)
    # })


    # Initial map — render only once
    # output$mapId <- leaflet::renderLeaflet({
    #   leaflet() |>
    #     addTiles() |>
    #     setView(lng = -75, lat = 45, zoom = 4)
    # })

    # Track previous selection
    prev_selected <- reactiveVal(character())

    observeEvent(input$selected_rasters, {
      rasters <- stored_rasters()

      old <- prev_selected()
      new <- input$selected_rasters

      added <- setdiff(new, old)
      removed <- setdiff(old, new)

      # Work with leafletProxy instead of r$map
      map_proxy <- leaflet::leafletProxy("mapId")

      # Add newly selected rasters
      for (id in added) {
        layer <- rasters[[id]]

        map_proxy <- map_proxy |>
          leaflet::addRasterImage(
            layer$raster, # should be SpatRaster from terra
            layerId = id,
            group   = layer$name,
            opacity = 0.7,
            project = TRUE
          )
      }

      # Remove deselected rasters
      for (id in removed) {
        map_proxy <- map_proxy |>
          leaflet::removeImage(layerId = id)
      }

      prev_selected(new)
    })
  })
}
