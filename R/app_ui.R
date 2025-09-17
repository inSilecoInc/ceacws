#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'
#' @import shiny
#' @importFrom shinyjs useShinyjs hide show
#' @noRd

app_ui <- function(request) {
    tagList(
        # Leave this function for adding external resources
        golem_add_external_resources(),
        # Your application UI logic
        fluidPage(
            theme = bslib::bs_theme(
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
            ),
            useShinyjs(),
            mod_timeout_client_ui("session_timeout"),

            # # Application title
            # fluidRow(
            #     column(
            #         12,
            #         h2("CEACWS Shiny Template", style = "margin-bottom: 20px;")
            #     )
            # ),

            # Dynamic tabs based on threat layers availability
            uiOutput("dynamic_tabs")
        )
    )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
    add_resource_path(
        "www",
        app_sys("app/www")
    )

    tags$head(
        favicon(),
        bundle_resources(
            path = app_sys("app/www"),
            app_title = "ceacws"
        )
    )
}
