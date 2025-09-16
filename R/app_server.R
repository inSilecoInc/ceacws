#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @noRd
app_server <- function(input, output, session) {
    #
    r <- reactiveValues(
        map = base_map(),
        geom_slc = NULL,
        disclaimer_agreed = FALSE
    )

    # disclaimer
    mod_dialog_disclaimers_server("show_dialog", r)

    # timeout
    mod_timeout_client_server("session_timeout", r)
    
    # threat layers module
    threat_layers_data <- mod_threat_layers_server("threat_layers", r)

    # Handle tab switching when visualize is clicked
    observeEvent(threat_layers_data$visualize_triggered(), {
      # Switch to map tab
      updateTabsetPanel(session, "main_tabs", selected = "map")
      
      # Store threat layers data for map module
      r$threat_layers <- threat_layers_data$processed_rasters()
    })

    # initiate map
    mod_select_map_server("map-setting", r)

    # generate map
    observeEvent(r$map, {
        r$geom_slc <- callModule(
            mapedit::editMod,
            leafmap = r$map,
            id = "map-select"
        )
    })
    
    # Add threat layers to map when available
    observeEvent(r$threat_layers, {
      req(r$threat_layers)
      
      # Add each threat layer to the map
      for (layer_name in names(r$threat_layers)) {
        layer_data <- r$threat_layers[[layer_name]]
        raster_obj <- layer_data$raster
        
        # Add raster layer to map
        r$map <- r$map |>
          leaflet::addRasterImage(
            raster_obj,
            group = layer_data$display_name,
            opacity = 0.7,
            project = TRUE
          )
      }
      
      # Update layer control to include new threat layers
      layer_names <- sapply(r$threat_layers, function(x) x$display_name)
      
      r$map <- r$map |>
        leaflet::addLayersControl(
          baseGroups = c("OpenStreetMap", "Ocean Basemap"),
          overlayGroups = layer_names,
          position = "bottomleft",
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )
    })

    # reporting
    mod_render_doc_server("report", r)
    observeEvent(r$report_html, {
        output$preview_report <- renderUI({
            # looks like it has to be a relative path starting with www/
            pth <- fs::path_rel(r$report_html, app_sys("app"))
            tags$iframe(
                # looks like path must be relative
                id = "iframe_report", src = pth, width = "100%"
            )
        })
    })

    onSessionEnded(function() {
        cli::cli_alert_info("Session ended -- cleaning up")
        # do what needs to be done!
    })
}