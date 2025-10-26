#' Spatial Resampling Module UI
#'
#' @description A shiny Module for spatial resampling (resolution and extent)
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_spatial_resampling_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      # Resolution section
      div(
        class = "mb-3",
        h6("Resolution"),
        div(
          class = "form-check",
          checkboxInput(
            inputId = ns("enable_resolution"),
            label = "Resample resolution",
            value = FALSE
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("enable_resolution"), "']"),
          numericInput(
            inputId = ns("new_resolution"),
            label = "New resolution (degrees)",
            value = 0.01,
            min = 0.001,
            max = 1,
            step = 0.001
          ) # ,
          # selectInput(
          #   inputId = ns("resolution_method"),
          #   label = "Resampling method",
          #   choices = list(
          #     "Bilinear (continuous data)" = "bilinear",
          #     "Nearest neighbor (categorical data)" = "near",
          #     "Cubic convolution" = "cubic",
          #     "Lanczos" = "lanczos"
          #   ),
          #   selected = "bilinear"
          # )
        )
      ),

      # Extent section
      div(
        class = "mb-3",
        h6("Extent"),
        div(
          class = "form-check",
          checkboxInput(
            inputId = ns("enable_extent"),
            label = "Crop to bounding box",
            value = FALSE
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("enable_extent"), "']"),
          div(
            class = "row",
            div(
              class = "col-6",
              numericInput(
                inputId = ns("xmin"),
                label = "West (xmin)",
                value = -75,
                step = 0.1
              )
            ),
            div(
              class = "col-6",
              numericInput(
                inputId = ns("xmax"),
                label = "East (xmax)",
                value = -65,
                step = 0.1
              )
            )
          ),
          div(
            class = "row",
            div(
              class = "col-6",
              numericInput(
                inputId = ns("ymin"),
                label = "South (ymin)",
                value = 40,
                step = 0.1
              )
            ),
            div(
              class = "col-6",
              numericInput(
                inputId = ns("ymax"),
                label = "North (ymax)",
                value = 50,
                step = 0.1
              )
            )
          )
        )
      ),

      # Apply button
      div(
        class = "mt-3",
        actionButton(
          inputId = ns("apply_processing"),
          label = "Apply Processing",
          class = "btn btn-primary"
        )
      )
    )
  )
}

#' Spatial Resampling Module Server
#'
#' @description Server logic for spatial resampling
#'
#' @param id Internal parameter for {shiny}
#' @param stored_rasters Reactive containing the stored_rasters list
#'
#' @noRd
mod_spatial_resampling_server <- function(id, stored_rasters, processed_available = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Store processed rasters separately - only populated after processing
    processed_rasters <- reactiveVal(list())


    observeEvent(input$apply_processing, {
      if (!isTRUE(input$enable_resolution) && !isTRUE(input$enable_extent)) {
        showNotification("No processing options selected", type = "warning")
        return()
      }

      rasters <- stored_rasters()
      if (length(rasters) == 0) {
        showNotification("No rasters available", type = "warning")
        return(NULL)
      }

      showNotification("Processing rasters...", type = "message")

      new_extent <- if (isTRUE(input$enable_extent)) {
        c(input$xmin, input$xmax, input$ymin, input$ymax)
      } else {
        NULL
      }

      new_resolution <- if (isTRUE(input$enable_resolution)) {
        input$new_resolution
      } else {
        NULL
      }

      method <- input$resolution_method %||% "bilinear"

      processed <- list()
      errors <- character(0)

      for (nm in names(rasters)) {
        tryCatch(
          {
            layer <- rasters[[nm]]

            processed_raster <- resample_raster(
              raster = layer$raster,
              new_extent = new_extent,
              new_resolution = new_resolution,
              method = method
            )

            processed_layer <- layer
            processed_layer$raster <- processed_raster
            processed_layer$processed <- TRUE
            processed_layer$processing_params <- list(
              extent = new_extent,
              resolution = new_resolution,
              method = method
            )

            processed[[nm]] <- processed_layer
          },
          error = function(e) {
            errors <<- c(errors, paste(nm, ":", e$message))
            processed[[nm]] <<- rasters[[nm]]
          }
        )
      }

      if (length(errors) > 0) {
        showNotification(paste("Errors:", paste(errors, collapse = "; ")), type = "error")
      } else {
        showNotification("Processing completed successfully!", type = "message")
      }

      # assign back to the reactiveValues
      processed_rasters(processed)

      # Update availability flag if provided
      if (!is.null(processed_available)) {
        processed_available(length(processed) > 0)
      }
    })

    # Return processed rasters
    return(reactive(processed_rasters()))
  })
}
