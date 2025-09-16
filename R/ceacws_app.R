#' Launch CEACWS Threat Analysis Application
#'
#' This function launches the CEACWS (Canadian Environmental Assessment
#' Canadian Wildlife Service) Shiny application for analyzing threats to
#' seabirds in the Northwest Atlantic.
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#'
#' @return A Shiny application object
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch the CEACWS application
#' ceacws_app()
#' }
ceacws_app <- function(...) {
  run_app(...)
}
