#' render_doc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd

mod_render_doc_ui <- function(id) {
    ns <- NS(id)
    tagList(
        selectInput(ns("tpl_list"),
            label = "Template list",
            choices = names(list_templates())
        ),
        textInput(ns("title"), label = "Title", value = "title"),
        fluidRow(
            column(
                6,
                textInput(ns("author"), label = "Author", value = "John Doe")
            ),
            column(
                6,
                dateInput(ns("date"), label = "Date"),
            )
        ),
        actionButton(
            ns("render"),
            title = "Generate the report.",
            label = "Render",
            icon = icon("gear")
        ),
        downloadButton(
            ns("save_pdf"),
            title = "Download a pdf version",
            label = "Download",
            icon = icon("file-pdf")
        )
    )
}

#' render_doc Server Functions
#'
#' @noRd
mod_render_doc_server <- function(id, r) {
    moduleServer(id, function(input, output, session) {
        hide("save_pdf")
        ns <- session$ns
        observeEvent(
            input$render,
            {
                list.files(
                    app_sys("app", "www", "docs"),
                    pattern = "\\.html$",
                    full.names = TRUE
                ) |>
                    unlink()
                ntf_id <- showNotification(
                    "Rendering HTML preview...",
                    duration = NULL,
                    closeButton = TRUE
                )
                flh <- custom_render(
                    input$tpl_list,
                    data = list(
                        title = input$title,
                        author = input$author,
                        date = input$date
                    ),
                    envir = list(geom = r$geom_slc()$all),
                    output_dir = app_sys("app", "www", "docs")
                )
                r$report_html <- flh
                show("save_pdf")
                removeNotification(ntf_id)
            }
        )

        # see https://mastering-shiny.org/action-transfer.html
        output$save_pdf <- downloadHandler(
            filename = function() {
                fs::path_ext_set(basename(r$report_html), "pdf")
            },
            content = function(file) {
                id <- showNotification(
                    "Rendering PDF file...",
                    duration = NULL,
                    closeButton = TRUE
                )
                on.exit(removeNotification(id), add = TRUE)
                fs::file_copy(pagedown::chrome_print(r$report_html), file)
            }
        )
    })
}
