#' helptext
#'
#' @description add help text.
#' 
#' @param x help message (see [shiny::helpText(]).
#'
#' @return a shiny.tag object.
#'
#' @noRd

myhelptxt <- function(x) {
    shiny::helpText(
        shiny::HTML(
            glue::glue(
                '<i class="fas fa-info-circle" aria-hidden="true"></i> {x}'
            )
        )
    )
}

utils::globalVariables(
    c("map_bbox", "disc_frontm", "disc_path_md")
)
