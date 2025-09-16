mod_dialog_disclaimers_server <- function(id, r) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        query_modal <- modalDialog(
            title = disc_frontm$title,
            includeHTML(disc_path_md |> fs::path_ext_set("html")),
            easyClose = FALSE,
            size = "xl",
            footer = tagList(
                checkboxInput(ns("agreed"), disc_frontm$agree,
                    value = FALSE,
                    width = "90%"
                ),
                actionButton(ns("dismiss"), "OK")
            )
        )

        # Show the model on start up ...
        showModal(query_modal)

        observeEvent(r$show_dialog, {
            if (r$show_dialog) {
                showModal(query_modal)
            }
        })

        # ... or when user wants to change query
        observeEvent(input$dismiss, {
            if (input$agreed) {
                removeModal()
                r$show_dialog <- FALSE
                r$disclaimer_agreed <- TRUE
            } else {
                showNotification("You must check the box!", type = "warning")
            }
        })
    })
}
