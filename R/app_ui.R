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

            # Application title
            fluidRow(
                column(
                    12,
                    h1("CEACWS Shiny Template", style = "margin-bottom: 20px;")
                )
            ),

            # Main tabs
            tabsetPanel(
                id = "main_tabs",
                selected = "threat_layers",

                # Tab 1: Threat layers (landing tab)
                tabPanel(
                    title = tagList(icon("layer-group"), "Threat layers"),
                    value = "threat_layers",
                    mod_threat_layers_ui("threat_layers")
                ),

                # Tab 2: Map (existing template functionality)
                tabPanel(
                    title = tagList(icon("map"), "Map"),
                    value = "map",
                    sidebarLayout(
                        sidebarPanel(
                            mod_select_map_ui("map-setting"),
                            br(),
                            fluidRow(
                                column(
                                    12,
                                    bookmarkButton(label = "Share this application view")
                                )
                            )
                        ),
                        mainPanel(
                            mapedit::editModUI("map-select")
                        )
                    )
                ),

                # Tab 3: Report (existing template functionality)
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
            )
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
