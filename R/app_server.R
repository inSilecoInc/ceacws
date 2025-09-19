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

  # Dynamic tabs based on threat layers availability
  output$dynamic_tabs <- renderUI({
    has_layers <- length(threat_layers_data$stored_rasters()) > 0

    tabs <- list(
      # Tab 1: Threat layers (always available)
      tabPanel(
        title = tagList(icon("layer-group"), "Threat layers"),
        value = "threat_layers",
        mod_threat_layers_ui("threat_layers")
      )
    )

    # Add Map and Report tabs only when threat layers exist
    if (has_layers) {
      tabs <- append(tabs, list(
        # Tab 2: Map
        tabPanel(
          title = tagList(icon("map"), "Map"),
          value = "map",
          sidebarLayout(
            sidebarPanel(
              # Threat Layers Card
              div(
                class = "card mb-3",
                div(
                  class = "card-header",
                  style = "cursor: pointer;",
                  `data-bs-toggle` = "collapse",
                  `data-bs-target` = "#threat-layers-card",
                  div(
                    class = "d-flex justify-content-between align-items-center",
                    h5(class = "mb-0", "Threat layers"),
                    icon("chevron-down")
                  )
                ),
                div(
                  id = "threat-layers-card",
                  class = "collapse show",
                  div(
                    class = "card-body",
                    mod_threat_layer_controls_ui("threat_controls")
                  )
                )
              ),

              # Map Controls Card (includes search, explore, and resampling)
              div(
                class = "card mb-3",
                div(
                  class = "card-header",
                  style = "cursor: pointer;",
                  `data-bs-toggle` = "collapse",
                  `data-bs-target` = "#map-controls-card",
                  div(
                    class = "d-flex justify-content-between align-items-center",
                    h5(class = "mb-0", "Map controls"),
                    icon("chevron-down")
                  )
                ),
                div(
                  id = "map-controls-card",
                  class = "collapse",
                  div(
                    class = "card-body",
                    mod_select_map_ui("map-setting")
                  )
                )
              )
            ),
            mainPanel(
              mapedit::editModUI("map-select")
            )
          )
        ),
        # Tab 3: Report
        tabPanel(
          title = tagList(icon("file"), "Report"),
          value = "report",
          sidebarLayout(
            sidebarPanel(
              mod_render_doc_ui("report")
            ),
            mainPanel(
              htmlOutput("preview_report")
            )
          )
        )
      ))
    }

    do.call(tabsetPanel, c(list(id = "main_tabs", selected = "threat_layers"), tabs))
  })

  # Handle tab switching when visualize is clicked
  observeEvent(threat_layers_data$visualize_triggered(), {
    # Switch to map tab
    updateTabsetPanel(session, "main_tabs", selected = "map")

    # Store threat layers data for map module
    r$threat_layers_for_map <- threat_layers_data$stored_rasters()
  })

  # initiate map
  map_data <- mod_select_map_server("map-setting", r)

  # initiate threat layer controls
  mod_threat_layer_controls_server(
    "threat_controls",
    reactive(r$threat_layers_for_map),
    r
  )

  # generate map
  observeEvent(r$map, {
    r$geom_slc <- callModule(
      mapedit::editMod,
      leafmap = r$map,
      id = "map-select"
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