#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @noRd
app_server <- function(input, output, session) {
  # Active threat layers selection module
  stored_rasters <- mod_threat_layers_selection_server("threat_layers")

  r <- reactiveValues(
    map = base_map(),
    geom_slc = NULL,
    disclaimer_agreed = FALSE
  )

  # Activate threat layers processing module
  mod_threat_layers_processing_server("threat_processing", stored_rasters$stored_rasters, r)

  # disclaimer
  # mod_dialog_disclaimers_server("show_dialog", r)

  # timeout
  mod_timeout_client_server("session_timeout", r)

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


  onSessionEnded(function() {
    cli::cli_alert_info("Session ended -- cleaning up")
    # do what needs to be done!
  })
}
