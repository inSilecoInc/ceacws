#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @import shiny
run_app <- function(onStart = NULL,
                    options = list(),
                    enableBookmarking = "url",
                    uiPattern = "/",
                    ...) {
  golem::with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = fct_start,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}


utils::globalVariables(c(
  "disc_path_md",
  "path_docs",
  "map_bbox",
  "threat_layers_list",
  "threat_layer_categories"
))
