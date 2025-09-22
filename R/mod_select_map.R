#' select_map UI Function
#'
#' @description A shiny Module to create spatial objects.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_select_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    myhelptxt("Search and explore map."),
    br(),
    div(
      style = "display: inline-block;",
      textInput(
        ns("location"),
        label = "Explore by location",
        value = NULL
      )
    ),
    actionButton(ns("search_loc"), "Search", icon = icon("search")),
    actionButton(ns("reset_view"), "Reset view", icon = icon("arrow-right")),
    # DT::DTOutput(ns("geom_selected"))
  )
}

#' select_map Server Functions
#'
#' @noRd
mod_select_map_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$search_loc, {
      cli::cli_alert_info("Map - Resetting map view using location")
      r$map <- set_view_to_city(input$location, r$map)
    })

    observeEvent(input$reset_view, {
      cli::cli_alert_info("Map - Resetting map view to default")
      r$map <- reset_view(r$map)
    })

    observeEvent(r$geom_slc, {
      output$geom_selected <- DT::renderDT({
        cli::cli_alert_info("Updating geom info")
        # r$geom_slc() return an error if null, thus tryCatch()
        dfout <- tryCatch(
          {
            if (nrow(r$geom_slc()$all) > 0) {
              r$geom_slc()$all |> sf::st_drop_geometry()
            } else {
              data.frame()
            }
          },
          error = function(e) data.frame()
        )
      })
    })
  })
}
