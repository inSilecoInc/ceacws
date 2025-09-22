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
              uiOutput(ns("selected_rasters"))
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
#' @importFrom shiny moduleServer renderText renderUI checkboxInput observe observeEvent reactiveValues reactive
#' @importFrom leaflet addRasterImage removeImage
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
        if (length(input$selected_rasters) == 0) {
          map_proxy <- map_proxy |> leaflet::clearImages()
        } else {
          for (id in added) {
            layer <- rasts[[id]]
            map_proxy <- map_proxy |>
              leaflet::addRasterImage(
                layer$raster, # terra::SpatRaster
                layerId = id,
                group   = layer$name,
                opacity = 0.7,
                project = TRUE
              )
          }
        }

        # remove deselected rasters
        for (id in removed) {
          map_proxy <- map_proxy |>
            leaflet::removeImage(layerId = id)
        }

        prev_selected(new)
      },
      ignoreNULL = FALSE
    )
  })
}
