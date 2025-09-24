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
        if (length(input$selected_rasters) == 0) {
          map_proxy <- map_proxy |> leaflet::clearImages()
        } else {
          for (id in added) {
            layer <- rasts[[id]]
            map_proxy <- map_proxy |>
              leaflet::addRasterImage(
                layer$raster,
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
