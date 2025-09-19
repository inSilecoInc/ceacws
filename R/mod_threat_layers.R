#' Create systematic layer name from criteria
#'
#' @param criteria List of filter criteria
#' @param layer_count Number of files being combined
#' @return Character string with systematic layer name
#' @noRd
create_layer_name <- function(criteria, layer_count) {
  # Helper functions
  format_multiple <- function(items, singular, plural = NULL) {
    if (is.null(items)) return(NULL)
    if (length(items) == 1) {
      return(items[1])
    } else if (length(items) <= 3) {
      return(paste(items, collapse = "_"))
    } else {
      return(if (!is.null(plural)) plural else paste0("Multi_", singular))
    }
  }
  
  format_months <- function(month_nums) {
    if (is.null(month_nums)) return(NULL)
    month_abbrev <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    months <- month_abbrev[as.numeric(month_nums)]
    format_multiple(months, "Months")
  }
  
  # Build name parts
  parts <- c()
  
  # 1. Category (required)
  category_clean <- tools::toTitleCase(gsub("_", " ", criteria$category))
  category_clean <- gsub(" ", "_", category_clean)
  parts <- c(parts, category_clean)
  
  # 2. Subcategory (if present)
  if (!is.null(criteria$subcategory)) {
    subcat_clean <- tools::toTitleCase(gsub("_", " ", criteria$subcategory))
    subcat_clean <- gsub(" ", "_", subcat_clean)
    parts <- c(parts, subcat_clean)
  }
  
  # 3. Type filter
  if (!is.null(criteria$type)) {
    type_clean <- gsub(" ", "_", format_multiple(criteria$type, "Types"))
    parts <- c(parts, type_clean)
  }
  
  # 4. Species filter (for fisheries)
  if (!is.null(criteria$species)) {
    species_clean <- gsub(" ", "_", format_multiple(criteria$species, "Species"))
    parts <- c(parts, species_clean)
  }
  
  # 5. Year filter
  if (!is.null(criteria$year)) {
    year_clean <- format_multiple(criteria$year, "Years")
    parts <- c(parts, year_clean)
  }
  
  # 6. Month filter
  if (!is.null(criteria$month)) {
    month_clean <- format_months(criteria$month)
    parts <- c(parts, month_clean)
  }
  
  # 7. Combination method (only if multiple files)
  if (!is.null(criteria$combination_method) && layer_count > 1) {
    method_clean <- tools::toTitleCase(criteria$combination_method)
    parts <- c(parts, method_clean)
  }
  
  # Combine parts
  layer_name <- paste(parts, collapse = "_")
  
  # Return display name (with spaces for UI)
  display_name <- gsub("_", " ", layer_name)
  
  return(display_name)
}

#' Get file-safe version of layer name
#'
#' @param criteria List of filter criteria
#' @param layer_count Number of files being combined
#' @return Character string with file-safe layer name (underscores, no spaces)
#' @noRd
get_file_safe_name <- function(criteria, layer_count) {
  # Use same logic as create_layer_name but keep underscores
  display_name <- create_layer_name(criteria, layer_count)
  file_safe_name <- gsub(" ", "_", display_name)
  return(file_safe_name)
}

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
      column(2),
      column(6, h3("Threat layers")),
      column(
        2,
        div(
          style = "text-align: right; margin-top: 10px;",
          conditionalPanel(
            condition = paste0("output['", ns("show_visualize"), "']"),
            actionButton(ns("visualize_map"), "Visualize layers on map", class = "btn-success")
          )
        )
      ),
      column(2)
    ),

    # Add new layer button underneath title
    fluidRow(
      column(2),
      column(
        8,
        div(
          style = "margin-bottom: 15px;",
          actionButton(ns("add"), "Add a new layer", icon = icon("circle-plus"))
        )
      ),
      column(2)
    ),

    # Conditional panel for threat layer selection
    conditionalPanel(
      condition = paste0("input['", ns("add"), "'] > 0 && !output['", ns("hide_panel"), "']"),
      fluidRow(
        column(2),
        column(
          8,
          wellPanel(
            h4("Add Threat Layer"),
            fluidRow(
              column(
                6,
                # Main category selection
                selectInput(
                  ns("category"),
                  "Select threat layer category:",
                  choices = NULL,
                  selected = NULL
                )
              ),

              # Subcategory (daily/nighttime, gear/species, etc.)
              column(
                6,
                conditionalPanel(
                  condition = paste0("output['", ns("show_subcategory"), "']"),
                  selectInput(
                    ns("subcategory"),
                    "Subcategory:",
                    choices = NULL,
                    selected = NULL
                  )
                )
              )
            ),
            fluidRow(
              # Type selection (allow multiple)
              column(
                3,
                conditionalPanel(
                  condition = paste0("output['", ns("show_type"), "']"),
                  selectInput(
                    ns("type"),
                    "Type:",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE
                  )
                )
              ),
              column(
                3,
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
                )
              ),
              column(
                3,
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
                )
              ),
              column(
                3,
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
                )
              ),
            ),

            # Filter button
            fluidRow(
              column(
                6,
                actionButton(ns("filter_layers"), "Filter", class = "btn-info", style = "margin-bottom: 15px;"),
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
                  "Minimum" = "minimum"
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
                  actionButton(ns("confirm_add"), "Add Layer", class = "btn-primary"),
                  actionButton(ns("cancel_add"), "Cancel", class = "btn-secondary")
                )
              )
            )
          )
        ),
        column(2)
      )
    ),

    # Added layers list
    fluidRow(
      column(2),
      column(
        8,
        uiOutput(ns("layers"))
      ),
      column(2)
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

    # Initialize stored rasters list (combines display info and raster objects)
    stored_rasters <- reactiveVal(list())

    # Initialize filtered layers (result of Filter button)
    filtered_layers <- reactiveVal(NULL)

    # Control panel visibility
    hide_panel <- reactiveVal(FALSE)
    
    # Flag to prevent observers during clear operations
    clearing_filters <- reactiveVal(FALSE)

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
      # Set flag to prevent observers from interfering
      clearing_filters(TRUE)
      
      # Reset all selections and choices to initial state
      req(exists("available_threat_layers"))
      
      # Clear filtered results first
      filtered_layers(NULL)
      
      # Reset category to initial choices
      category_choices <- names(available_threat_layers)
      category_labels <- gsub("_", " ", category_choices)
      names(category_choices) <- tools::toTitleCase(category_labels)
      
      updateSelectInput(session, "category", 
                       choices = c("Select category..." = "", category_choices),
                       selected = "")
      
      # Clear all other dropdowns completely
      updateSelectInput(session, "subcategory", 
                       choices = c("Select subcategory..." = ""),
                       selected = "")
      updateSelectInput(session, "type", 
                       choices = NULL,
                       selected = NULL)
      updateSelectInput(session, "species", 
                       choices = NULL,
                       selected = NULL)
      updateSelectInput(session, "year", 
                       choices = NULL,
                       selected = NULL)
      updateSelectInput(session, "month", 
                       choices = NULL,
                       selected = NULL)
      updateSelectInput(session, "combination_method", 
                       choices = c(
                         "Sum" = "sum",
                         "Average" = "average", 
                         "Maximum" = "maximum",
                         "Minimum" = "minimum"
                       ),
                       selected = "sum")

      # Allow observers to run again
      clearing_filters(FALSE)

      showNotification("Filters cleared successfully.", type = "message")
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
      req(!clearing_filters())  # Don't run during clear operations
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
      # Show loading notification and disable button
      showNotification(
        "Processing raster layers... This may take a few moments.",
        duration = NULL,  # Keep until manually removed
        closeButton = FALSE,
        id = "processing_layers",
        type = "message"
      )
      
      # Disable the button to prevent multiple clicks
      shinyjs::disable("confirm_add")
      
      # Validate required selections
      req(input$category != "")
      req(filtered_layers())

      layers <- filtered_layers()

      if (nrow(layers) == 0) {
        removeNotification("processing_layers")
        shinyjs::enable("confirm_add")
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

      # Create systematic layer name
      display_name <- create_layer_name(criteria, nrow(layers))

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
              "sum" = terra::app(terra::rast(raster_list), fun = function(x) {
                s <- sum(x, na.rm = TRUE)
                ifelse(s == 0, NA, s)
              }),
              "average" = terra::app(terra::rast(raster_list), fun = function(x) {
                s <- mean(x, na.rm = TRUE)
                ifelse(s == 0, NA, s)
              }),
              "maximum" = terra::app(terra::rast(raster_list), fun = function(x) {
                s <- max(x, na.rm = TRUE)
                ifelse(s == 0, NA, s)
              }),
              "minimum" = terra::app(terra::rast(raster_list), fun = function(x) {
                s <- min(x, na.rm = TRUE)
                ifelse(s == 0, NA, s)
              })
            )
          }

          # Create unique name for this raster using file-safe naming
          file_safe_name <- get_file_safe_name(criteria, nrow(layers))
          raster_name <- paste0(
            "layer_", length(stored_rasters()) + 1, "_",
            file_safe_name
          )

          # Store raster with all metadata in single structure
          current_stored <- stored_rasters()
          new_layer <- list(
            raster = combined_raster,
            display_name = display_name,
            file_safe_name = file_safe_name,
            criteria = criteria,
            layers = layers,
            file_count = nrow(layers),
            raster_name = raster_name,
            combination_method = criteria$combination_method,
            raster_info = capture.output(print(combined_raster))
          )
          current_stored[[raster_name]] <- new_layer
          stored_rasters(current_stored)

          # Remove loading notification and show success
          removeNotification("processing_layers")
          shinyjs::enable("confirm_add")
          
          # Reset form and hide panel
          updateSelectInput(session, "category", selected = "")
          filtered_layers(NULL)
          hide_panel(TRUE)
          showNotification(paste("Layer processed successfully!", nrow(layers), "files combined."), type = "message")
        },
        error = function(e) {
          # Remove loading notification and show error
          removeNotification("processing_layers")
          shinyjs::enable("confirm_add")
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
      length(stored_rasters()) > 0
    })
    outputOptions(output, "show_visualize", suspendWhenHidden = FALSE)

    # Render stored layers list
    output$layers <- renderUI({
      layers <- stored_rasters()

      if (length(layers) == 0) {
        return(p("No layers selected yet.", style = "color: #999;"))
      }

      layer_items <- lapply(names(layers), function(raster_name) {
        layer <- layers[[raster_name]]
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
                ns(paste0("remove_", raster_name)),
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
      # Get current layers
      layers <- stored_rasters()
      
      # Only create observers if there are layers
      if (length(layers) > 0) {
        # Watch for any remove button clicks
        lapply(names(layers), function(raster_name) {
          observeEvent(input[[paste0("remove_", raster_name)]], {
            # Remove from stored rasters
            current_stored <- stored_rasters()
            current_stored[[raster_name]] <- NULL
            stored_rasters(current_stored)

            showNotification("Layer removed.", type = "warning")
          }, ignoreInit = TRUE, once = TRUE)
        })
      }
    })

    # Handle visualize button click
    visualize_triggered <- reactiveVal(0)

    observeEvent(input$visualize_map, {
      # Increment counter to trigger reactive
      visualize_triggered(visualize_triggered() + 1)

      # Store current stored rasters for visualization
      r$threat_layers_for_map <- stored_rasters()

      # Trigger map tab switch (this will be handled by parent module)
      showNotification("Layers ready for visualization on map!",
                       type = "message")
    })

    # Return stored rasters and visualization trigger for use by parent module
    list(
      stored_rasters = reactive(stored_rasters()),
      visualize_triggered = reactive(visualize_triggered())
    )
  })
}
