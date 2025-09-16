#' timeout_client UI Function
#'
#' @description This Shiny module is intended to manage and close user sessions
#' that have been inactive for a specified period. It utilizes two Golem
#' configuration variables:
#' Golem config variables
#' - timeout_session_time: Time of inactivity (in secs) before the user is notified.
#' - timeout_closing_time: Time (in secs) after notification before the session is automatically closed.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_timeout_client_ui <- function(id) {
    ns <- NS(id)
    timeoutSeconds <- get_golem_config("timeout_session_time")

    inactivity <- sprintf("
    function idleTimer() {
      let t = setTimeout(logout, %s);

      window.onmousemove = resetTimer; // catches mouse movements
      window.onmousedown = resetTimer; // catches mouse movements
      window.onclick = resetTimer;     // catches mouse clicks
      window.onscroll = resetTimer;    // catches scrolling
      window.onkeypress = resetTimer;  //catches keyboard actions

      function logout() {
        Shiny.setInputValue('%s', Date.now());
      }

      function resetTimer() {
        clearTimeout(t);
        t = setTimeout(logout, %s);  // time is in milliseconds (1000 is 1 second)
      }
    }
    idleTimer();
  ", timeoutSeconds * 1000, ns("timeOut"), timeoutSeconds * 1000)

    tagList(
        tags$script(inactivity)
    )
}

#' timeout_client Server Functions
#'
#' @noRd
mod_timeout_client_server <- function(id, r) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        timer <- reactiveValues(
            eventTime = NULL,
            left = NULL,
            on = FALSE
        )

        observe({
            if (timer$on) {
                invalidateLater(1000, session)
                timer$left <- round(difftime(timer$eventTime, Sys.time(), units = "secs"))
                if (timer$left < 0) {
                    timer$on <- FALSE
                    cli::cli_inform("Timeout - Closing user session {session$token}")
                    removeModal()
                    showModal(modalDialog(
                        title = "Session closed",
                        p("Session closed after", get_golem_config("timeout_closing_time"), "seconds of inactivity"),
                        footer = NULL
                    ))
                    session$close()
                }
            }
        })

        output$eventTimeRemaining <- renderText({
            timer$left
        })

        observeEvent(input$timeOut, {
            cli::cli_inform("Timeout - showing modal")

            timer$eventTime <- Sys.time() + get_golem_config("timeout_closing_time")
            timer$on <- TRUE

            showModal(modalDialog(
                title = "Are you still there?",
                p("Session closing in", textOutput(ns("eventTimeRemaining"), inline = TRUE), "seconds due to inactivity"),
                footer = actionButton(ns("stillActive"), "Yes")
            ))
        })

        observeEvent(input$stillActive, {
            removeModal()
            timer$on <- FALSE
            if (!r$disclaimer_agreed) {
                r$show_dialog <- TRUE
            }
        })
    })
}
