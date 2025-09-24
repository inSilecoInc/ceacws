#' The application User-Interface
#'
#' @param request Internal parameter for {shiny}.
#'
#' @noRd
app_ui <- function(request) {
    tagList(
        # External resources (CSS, JS, etc.)
        golem_add_external_resources(),

        # Session timeout module (kept outside main layout so it doesn't affect spacing)
        mod_timeout_client_ui("session_timeout"),

        # Main application layout
        fluidPage(
            theme = app_theme(),
            shinyjs::useShinyjs(),

            # Tab layout
            tabsetPanel(
                id = "main_tabs",
                type = "tabs",

                # Threat layers tab
                tabPanel(
                    title = tagList(icon("layer-group"), "Threat layers"),
                    value = "threat_layers_tab",
                    div(
                        class = "p-3",
                        mod_threat_layers_selection_ui("threat_layers")
                    )
                ),

                # Processing panel
                tabPanel(
                    title = tagList(icon("map"), "Map"),
                    value = "processing_tab",
                    div(
                        class = "p-3",
                        mod_threat_layers_processing_ui("threat_processing")
                    )
                )
            )
        )
    )
}
#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @noRd
golem_add_external_resources <- function() {
    golem::add_resource_path(
        "www",
        app_sys("app/www")
    )

    tags$head(
        golem::favicon(),
        golem::bundle_resources(
            path = app_sys("app/www"),
            app_title = "ceacws"
        )
    )
}


#' Application theme helper
#'
#' Define a central bs_theme for reuse and easier maintenance.
#'
#' @return A {bslib} theme object
#' @noRd
app_theme <- function() {
    bslib::bs_theme(
        version = 5,
        "table-striped-bg" = "#e6f6ec",
        "table-hover-color" = "#fff",
        "table-hover-bg" = "#03662a",
        "table-active-bg" = "#03662a",
        "pagination-color" = "#03662a",
        "pagination-hover-bg" = "#e6f6ec",
        "pagination-focus-color" = "#03662a",
        "accordion-button-focus-border-color" = "#e6f6ec",
        "focus-ring-color" = "#e6f6ec"
    )
}
