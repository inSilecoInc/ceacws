#' Threat Layers Module UI
#'
#' @description A shiny Module for threat layer selection and configuration
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom terra rast
mod_threat_layers_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Title row with visualize button on the right
    fluidRow(
      column(8, h3("Threat layers")),
      column(
        4,
        div(
          style = "text-align: right; margin-top: 10px;",
          conditionalPanel(
            condition = paste0("output['", ns("show_visualize"), "']"),
            actionButton(ns("visualize_map"), "Visualize layers on map", class = "btn-success")
          )
        )
      )
    ),

    # Add new layer button underneath title
    fluidRow(
      column(
        12,
        div(
          style = "margin-bottom: 15px;",
          actionButton(ns("add"), "Add a new layer", icon = icon("circle-plus"))
        )
      )
    ),

    # Conditional panel for threat layer selection
    conditionalPanel(
      condition = paste0("input['", ns("add"), "'] > 0 && !output['", ns("hide_panel"), "']"),
      fluidRow(
        column(
          12,
          wellPanel(
            h4("Add Threat Layer"),

            # Main category selection
            selectInput(
              ns("category"),
              "Select threat layer category:",
              choices = NULL,
              selected = NULL
            ),

            # Subcategory (daily/nighttime, gear/species, etc.)
            conditionalPanel(
              condition = paste0("output['", ns("show_subcategory"), "']"),
              selectInput(
                ns("subcategory"),
                "Subcategory:",
                choices = NULL,
                selected = NULL
              )
            ),

            # Type selection (allow multiple)
            conditionalPanel(
              condition = paste0("output['", ns("show_type"), "']"),
              selectInput(
                ns("type"),
                "Type:",
                choices = NULL,
                selected = NULL,
                multiple = TRUE
              )
            ),

            # Species selection (allow multiple)
            conditionalPanel(
              condition = paste0("output['", ns("show_species"), "']"),
              selectInput(
                ns("species"),
                "Species:",
                choices = NULL,
                selected = NULL,
                multiple = TRUE
              )
            ),

            # Year selection (allow multiple)
            conditionalPanel(
              condition = paste0("output['", ns("show_year"), "']"),
              selectInput(
                ns("year"),
                "Year:",
                choices = NULL,
                selected = NULL,
                multiple = TRUE
              )
            ),

            # Month selection (allow multiple)
            conditionalPanel(
              condition = paste0("output['", ns("show_month"), "']"),
              selectInput(
                ns("month"),
                "Month:",
                choices = NULL,
                selected = NULL,
                multiple = TRUE
              )
            ),

            # Filter button
            fluidRow(
              column(
                6,
                actionButton(ns("filter_layers"), "Filter", class = "btn-info", style = "margin-bottom: 15px;")
              ),
              column(
                6,
                actionButton(ns("clear_filter"), "Clear Filter", class = "btn-warning", style = "margin-bottom: 15px;")
              )
            ),

            # Filter summary (show after filtering)
            conditionalPanel(
              condition = paste0("output['", ns("show_filter_summary"), "']"),
              div(
                class = "alert alert-info",
                h5("Filter Results"),
                uiOutput(ns("filter_summary"))
              )
            ),

            # Warning message (show when no files match)
            conditionalPanel(
              condition = paste0("output['", ns("show_warning"), "']"),
              div(
                class = "alert alert-warning",
                icon("exclamation-triangle"),
                " No layers are available with those criteria."
              )
            ),

            # Combination method (show when multiple files after filtering)
            conditionalPanel(
              condition = paste0("output['", ns("show_combination"), "']"),
              selectInput(
                ns("combination_method"),
                "Combination method:",
                choices = c(
                  "Sum" = "sum",
                  "Average" = "average",
                  "Maximum" = "maximum",
                  "Minimum" = "minimum",
                  "Overlay" = "overlay"
                ),
                selected = "sum"
              )
            ),

            # Action buttons (only show after successful filtering)
            conditionalPanel(
              condition = paste0("output['", ns("show_add_buttons"), "']"),
              fluidRow(
                column(
                  6,
                  actionButton(ns("confirm_add"), "Add Layer", class = "btn-primary")
                ),
                column(
                  6,
                  actionButton(ns("cancel_add"), "Cancel", class = "btn-secondary")
                )
              )
            )
          )
        )
      )
    ),

    # Added layers list
    fluidRow(
      column(
        12,
        uiOutput(ns("layers"))
      )
    )
  )
}

#' Threat Layers Module Server
#'
#' @param id Internal parameter for {shiny}
#' @param r Reactive values object
#'
#' @noRd
mod_threat_layers_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize selected layers list
    selected_layers <- reactiveVal(list())

    # Initialize filtered layers (result of Filter button)
    filtered_layers <- reactiveVal(NULL)

    # Initialize processed rasters storage
    processed_rasters <- reactiveVal(list())
    
    # Control panel visibility
    hide_panel <- reactiveVal(FALSE)

    # Helper function to convert month numbers to names
    month_names <- c(
      "January", "February", "March", "April", "May", "June",
      "July", "August", "September", "October", "November", "December"
    )

    # Control panel hiding
    output$hide_panel <- reactive({
      hide_panel()
    })
    outputOptions(output, "hide_panel", suspendWhenHidden = FALSE)
    
    # Reset panel visibility when add button is clicked
    observeEvent(input$add, {
      hide_panel(FALSE)
    })

    # Initialize category choices on startup
    observe({
      req(exists("available_threat_layers"))

      category_choices <- names(available_threat_layers)
      category_labels <- gsub("_", " ", category_choices)
      names(category_choices) <- tools::toTitleCase(category_labels)

      updateSelectInput(session, "category",
        choices = c("Select category..." = "", category_choices)
      )
    })

    # Control visibility of subcategory dropdown
    output$show_subcategory <- reactive({
      req(input$category != "")
      req(exists("available_threat_layers"))

      cat_data <- available_threat_layers[[input$category]]
      # Check if category has subcategories (complex categories)
      !"types" %in% names(cat_data)
    })
    outputOptions(output, "show_subcategory", suspendWhenHidden = FALSE)

    # Update subcategory choices based on category selection
    observeEvent(input$category, {
      req(input$category != "")
      req(exists("available_threat_layers"))

      cat_data <- available_threat_layers[[input$category]]

      # Check if category has subcategories
      if (!"types" %in% names(cat_data)) {
        subcat_choices <- names(cat_data)
        subcat_labels <- gsub("_", " ", subcat_choices)
        names(subcat_choices) <- tools::toTitleCase(subcat_labels)

        updateSelectInput(session, "subcategory",
          choices = c("Select subcategory..." = "", subcat_choices)
        )
      }
    })

    # Get current data based on selections
    current_data <- reactive({
      req(input$category != "")
      req(exists("available_threat_layers"))

      cat_data <- available_threat_layers[[input$category]]

      # Check if this is a complex category with subcategories
      if (!"types" %in% names(cat_data)) {
        req(input$subcategory != "")
        return(cat_data[[input$subcategory]])
      } else {
        return(cat_data)
      }
    })

    # Control visibility of type dropdown
    output$show_type <- reactive({
      req(current_data())
      data <- current_data()
      length(data$types) > 0
    })
    outputOptions(output, "show_type", suspendWhenHidden = FALSE)

    # Control visibility of species dropdown
    output$show_species <- reactive({
      req(current_data())
      req(input$category == "fisheries")
      req(input$subcategory == "species_specific")
      TRUE
    })
    outputOptions(output, "show_species", suspendWhenHidden = FALSE)

    # Control visibility of year dropdown
    output$show_year <- reactive({
      req(current_data())
      data <- current_data()
      length(data$years) > 0 || length(data$year_ranges) > 0
    })
    outputOptions(output, "show_year", suspendWhenHidden = FALSE)

    # Control visibility of month dropdown
    output$show_month <- reactive({
      req(current_data())
      data <- current_data()
      length(data$months) > 0
    })
    outputOptions(output, "show_month", suspendWhenHidden = FALSE)

    # Handle Filter button click
    observeEvent(input$filter_layers, {
      req(current_data())
      data <- current_data()
      layers <- data$layers

      # Apply filters based on selections
      if (!is.null(input$type) && length(input$type) > 0 && "type" %in% names(layers)) {
        layers <- layers[layers$type %in% input$type, ]
      }
      if (!is.null(input$species) && length(input$species) > 0 && "species" %in% names(layers)) {
        layers <- layers[layers$species %in% input$species, ]
      }
      if (!is.null(input$year) && length(input$year) > 0) {
        if ("year" %in% names(layers)) {
          layers <- layers[layers$year %in% input$year, ]
        } else if ("year_range" %in% names(layers)) {
          layers <- layers[layers$year_range %in% input$year, ]
        }
      }
      if (!is.null(input$month) && length(input$month) > 0 && "month" %in% names(layers)) {
        layers <- layers[layers$month %in% as.numeric(input$month), ]
      }

      # Store filtered results
      filtered_layers(layers)
    })

    # Handle Clear Filter button click
    observeEvent(input$clear_filter, {
      # Reset all selections
      updateSelectInput(session, "category", selected = "")
      updateSelectInput(session, "subcategory", selected = "")
      updateSelectInput(session, "type", selected = NULL)
      updateSelectInput(session, "species", selected = NULL)
      updateSelectInput(session, "year", selected = NULL)
      updateSelectInput(session, "month", selected = NULL)
      updateSelectInput(session, "combination_method", selected = "sum")

      # Clear filtered results
      filtered_layers(NULL)

      showNotification("Filter cleared.", type = "message")
    })

    # Control visibility of filter summary
    output$show_filter_summary <- reactive({
      !is.null(filtered_layers()) && nrow(filtered_layers()) > 0
    })
    outputOptions(output, "show_filter_summary", suspendWhenHidden = FALSE)

    # Render filter summary
    output$filter_summary <- renderUI({
      req(filtered_layers())
      layers <- filtered_layers()

      # Build summary of applied filters
      summary_parts <- c()

      if (!is.null(input$category) && input$category != "") {
        cat_display <- tools::toTitleCase(gsub("_", " ", input$category))
        summary_parts <- c(summary_parts, paste("Category:", cat_display))
      }

      if (!is.null(input$subcategory) && input$subcategory != "") {
        subcat_display <- tools::toTitleCase(gsub("_", " ", input$subcategory))
        summary_parts <- c(summary_parts, paste("Subcategory:", subcat_display))
      }

      if (!is.null(input$type) && length(input$type) > 0) {
        if (length(input$type) == 1) {
          summary_parts <- c(summary_parts, paste("Type:", input$type))
        } else {
          summary_parts <- c(summary_parts, paste("Types:", paste(input$type, collapse = ", ")))
        }
      }

      if (!is.null(input$species) && length(input$species) > 0) {
        if (length(input$species) == 1) {
          summary_parts <- c(summary_parts, paste("Species:", input$species))
        } else {
          summary_parts <- c(summary_parts, paste("Species:", paste(input$species, collapse = ", ")))
        }
      }

      if (!is.null(input$year) && length(input$year) > 0) {
        if (length(input$year) == 1) {
          summary_parts <- c(summary_parts, paste("Year:", input$year))
        } else {
          summary_parts <- c(summary_parts, paste("Years:", paste(input$year, collapse = ", ")))
        }
      }

      if (!is.null(input$month) && length(input$month) > 0) {
        month_names_selected <- month_names[as.numeric(input$month)]
        if (length(input$month) == 1) {
          summary_parts <- c(summary_parts, paste("Month:", month_names_selected))
        } else {
          summary_parts <- c(summary_parts, paste("Months:", paste(month_names_selected, collapse = ", ")))
        }
      }

      # Create summary display
      filter_summary <- if (length(summary_parts) > 0) {
        paste(summary_parts, collapse = " | ")
      } else {
        "All layers (no filters applied)"
      }

      tagList(
        p(strong("Applied filters:"), br(), filter_summary),
        p(strong("Result:"), paste(nrow(layers), "layer(s) found"))
      )
    })

    # Control visibility of warning message
    output$show_warning <- reactive({
      req(filtered_layers())
      nrow(filtered_layers()) == 0
    })
    outputOptions(output, "show_warning", suspendWhenHidden = FALSE)

    # Control visibility of combination method dropdown
    output$show_combination <- reactive({
      req(filtered_layers())
      nrow(filtered_layers()) > 1
    })
    outputOptions(output, "show_combination", suspendWhenHidden = FALSE)

    # Control visibility of Add/Cancel buttons
    output$show_add_buttons <- reactive({
      req(filtered_layers())
      nrow(filtered_layers()) > 0
    })
    outputOptions(output, "show_add_buttons", suspendWhenHidden = FALSE)

    # Update dropdown choices based on current data
    observe({
      req(current_data())
      data <- current_data()

      # Update type choices
      if (length(data$types) > 0) {
        updateSelectInput(session, "type",
          choices = data$types,
          selected = NULL
        )
      }

      # Update species choices (only for fisheries species_specific)
      if (input$category == "fisheries" && input$subcategory == "species_specific") {
        # Extract unique species from layers
        species_available <- unique(data$layers$species[!is.na(data$layers$species)])
        if (length(species_available) > 0) {
          updateSelectInput(session, "species",
            choices = species_available,
            selected = NULL
          )
        }
      }

      # Update year choices (regular years or year ranges)
      if (length(data$years) > 0) {
        updateSelectInput(session, "year",
          choices = data$years,
          selected = NULL
        )
      } else if (length(data$year_ranges) > 0) {
        updateSelectInput(session, "year",
          choices = data$year_ranges,
          selected = NULL
        )
      }

      # Update month choices (convert numbers to names)
      if (length(data$months) > 0) {
        month_choices <- data$months
        names(month_choices) <- month_names[data$months]
        updateSelectInput(session, "month",
          choices = month_choices,
          selected = NULL
        )
      }
    })

    # Handle layer addition
    observeEvent(input$confirm_add, {
      # Validate required selections
      req(input$category != "")
      req(filtered_layers())

      layers <- filtered_layers()

      if (nrow(layers) == 0) {
        showNotification("No matching layers found. Please adjust your selections.", type = "error")
        return()
      }

      # Build criteria for display
      criteria <- list(
        category = input$category,
        subcategory = input$subcategory,
        type = if (!is.null(input$type) && length(input$type) > 0) input$type else NULL,
        species = if (!is.null(input$species) && length(input$species) > 0) input$species else NULL,
        year = if (!is.null(input$year) && length(input$year) > 0) input$year else NULL,
        month = if (!is.null(input$month) && length(input$month) > 0) input$month else NULL,
        combination_method = if (nrow(layers) > 1) input$combination_method else NULL
      )

      # Create display name
      display_parts <- c()
      display_parts <- c(display_parts, tools::toTitleCase(gsub("_", " ", input$category)))

      if (!is.null(criteria$subcategory)) {
        display_parts <- c(display_parts, paste("(", tools::toTitleCase(gsub("_", " ", criteria$subcategory)), ")"))
      }

      if (!is.null(criteria$type)) {
        type_text <- if (length(criteria$type) > 1) {
          paste0(length(criteria$type), " types")
        } else {
          criteria$type
        }
        display_parts <- c(display_parts, type_text)
      }

      if (!is.null(criteria$year)) {
        year_text <- if (length(criteria$year) > 1) {
          paste0(length(criteria$year), " years")
        } else {
          criteria$year
        }
        display_parts <- c(display_parts, year_text)
      }

      if (!is.null(criteria$month)) {
        month_text <- if (length(criteria$month) > 1) {
          paste0(length(criteria$month), " months")
        } else {
          month_names[as.numeric(criteria$month)]
        }
        display_parts <- c(display_parts, month_text)
      }

      if (!is.null(criteria$combination_method)) {
        display_parts <- c(display_parts, paste0("(", tools::toTitleCase(criteria$combination_method), ")"))
      }

      display_name <- paste(display_parts, collapse = " - ")

      # Process raster files
      tryCatch(
        {
          # Import raster layers
          raster_list <- lapply(layers$filepath, function(filepath) {
            terra::rast(filepath)
          })

          # Combine rasters if multiple files
          combined_raster <- if (length(raster_list) == 1) {
            raster_list[[1]]
          } else {
            # Apply combination method
            switch(criteria$combination_method,
              "sum" = Reduce("+", raster_list),
              "average" = Reduce("+", raster_list) / length(raster_list),
              "maximum" = do.call(max, raster_list),
              "minimum" = do.call(min, raster_list),
              "overlay" = {
                # For overlay, create a stack
                do.call(c, raster_list)
              }
            )
          }

          # Create unique name for this raster
          raster_name <- paste0(
            "layer_", length(processed_rasters()) + 1, "_",
            gsub("[^A-Za-z0-9_]", "_", display_name)
          )

          # Store processed raster
          current_rasters <- processed_rasters()
          current_rasters[[raster_name]] <- list(
            raster = combined_raster,
            display_name = display_name,
            criteria = criteria,
            file_count = nrow(layers),
            combination_method = criteria$combination_method
          )
          processed_rasters(current_rasters)

          # Add to selected layers for display
          current_selected <- selected_layers()
          new_layer <- list(
            criteria = criteria,
            layers = layers,
            display_name = display_name,
            file_count = nrow(layers),
            raster_name = raster_name,
            raster_info = capture.output(print(combined_raster))
          )
          current_selected[[length(current_selected) + 1]] <- new_layer
          selected_layers(current_selected)

          # Reset form and hide panel
          updateSelectInput(session, "category", selected = "")
          filtered_layers(NULL)
          hide_panel(TRUE)
          showNotification(paste("Layer processed successfully!", nrow(layers), "files combined."), type = "message")
        },
        error = function(e) {
          showNotification(paste("Error processing raster layers:", e$message), type = "error")
        }
      )
    })

    # Handle cancellation
    observeEvent(input$cancel_add, {
      updateSelectInput(session, "category", selected = "")
      filtered_layers(NULL)
      hide_panel(TRUE)
    })

    # Control visualize button visibility
    output$show_visualize <- reactive({
      length(selected_layers()) > 0
    })
    outputOptions(output, "show_visualize", suspendWhenHidden = FALSE)

    # Render selected layers list
    output$layers <- renderUI({
      layers <- selected_layers()

      if (length(layers) == 0) {
        return(p("No layers selected yet.", style = "color: #999;"))
      }

      layer_items <- lapply(seq_along(layers), function(i) {
        layer <- layers[[i]]
        div(
          class = "alert alert-info",
          fluidRow(
            column(
              10,
              strong(layer$display_name),
              br(),
              span(paste(layer$file_count, "files"), style = "font-size: 0.875em; color: #6c757d;")
            ),
            column(
              2,
              actionButton(
                ns(paste0("remove_", i)),
                icon("trash"),
                class = "btn-sm btn-danger",
                style = "float: right;"
              )
            )
          )
        )
      })

      do.call(tagList, layer_items)
    })

    # Handle layer removal (dynamic buttons)
    observe({
      layers <- selected_layers()
      if (length(layers) > 0) {
        for (i in seq_along(layers)) {
          local({
            layer_index <- i
            observeEvent(input[[paste0("remove_", layer_index)]], {
              current_selected <- selected_layers()

              # Remove from processed rasters if it exists
              if (!is.null(current_selected[[layer_index]]$raster_name)) {
                current_rasters <- processed_rasters()
                current_rasters[[current_selected[[layer_index]]$raster_name]] <- NULL
                processed_rasters(current_rasters)
              }

              # Remove from selected layers
              current_selected[[layer_index]] <- NULL
              selected_layers(current_selected)
              showNotification("Layer removed.", type = "warning")
            })
          })
        }
      }
    })
    
    # Handle visualize button click
    visualize_triggered <- reactiveVal(0)
    
    observeEvent(input$visualize_map, {
      # Increment counter to trigger reactive
      visualize_triggered(visualize_triggered() + 1)
      
      # Store current processed rasters for visualization
      r$threat_layers_for_map <- processed_rasters()
      
      # Trigger map tab switch (this will be handled by parent module)
      showNotification("Layers ready for visualization on map!", type = "message")
    })

    # Return both selected layers and processed rasters for use by parent module
    return(list(
      selected_layers = reactive(selected_layers()),
      processed_rasters = reactive(processed_rasters()),
      visualize_triggered = reactive(visualize_triggered())
    ))
  })
}
