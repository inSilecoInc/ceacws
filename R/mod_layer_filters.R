#' Layer Filters Module UI
#'
#' @description A shiny module for filtering threat layers based on various criteria
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_layer_filters_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Filter form
    wellPanel(
      h4("Layer Filters"),
      fluidRow(
        column(
          6,
          # Category selection
          selectInput(
            ns("category"),
            "Select threat layer category:",
            choices = threat_layer_categories,
            selected = ""
          )
        ),
        column(
          6,
          # Subcategory UI - shown when category has subcategories
          uiOutput(ns("subcategory_ui")),
        )
      ),


      # Dynamic filter UI - populated by server based on selection
      uiOutput(ns("dynamic_filters")),

      # Action buttons row
      fluidRow(
        column(
          12,
          div(
            style = "margin-top: 15px;",
            actionButton(ns("filter_layers"), "Filter", class = "btn-info"),
            actionButton(ns("clear_filter"), "Clear Filter", class = "btn-warning", style = "margin-left: 10px;")
          )
        )
      ),

      # Results summary - populated by server
      uiOutput(ns("results_summary"))
    )
  )
}

#' Layer Filters Module Server
#'
#' @param id Internal parameter for {shiny}
#'
#' @noRd
mod_layer_filters_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Filtered results storage
    filtered_layers_info <- reactiveVal(NULL)

    # Reset filters if category changes
    observeEvent(input$category,
      {
        reset_dynamic_inputs(session)
        filtered_layers_info(NULL)
      },
      ignoreInit = TRUE
    )

    # Subcategory UI - only shown when category has subcategories
    output$subcategory_ui <- renderUI({
      req(input$category != "")

      cat_data <- threat_layers_list[[input$category]]

      # Check if category has subcategories
      if (!is.null(cat_data$subcategories)) {
        subcat_choices <- cat_data$subcategories
        subcat_labels <- gsub("_", " ", subcat_choices)
        names(subcat_choices) <- tools::toTitleCase(subcat_labels)

        selectInput(
          ns("subcategory"),
          "Subcategory:",
          choices = c("Select subcategory..." = "", subcat_choices),
          selected = ""
        )
      } else {
        # No subcategories needed
        NULL
      }
    })

    # Dynamic filter UI - populated based on category/subcategory selection
    output$dynamic_filters <- renderUI({
      req(input$category != "")

      cat_data <- threat_layers_list[[input$category]]

      # Determine which data to use for filters
      if (!is.null(cat_data$subcategories)) {
        # Category has subcategories - need subcategory selection
        req(input$subcategory != "")
        current_data <- cat_data[[input$subcategory]]
      } else {
        # No subcategories - use category data directly
        current_data <- cat_data
      }

      # Create filter dropdowns
      filter_rows <- create_filter_dropdowns(current_data, ns)
      do.call(tagList, filter_rows)
    })


    # Handle filter button click
    observeEvent(input$filter_layers, {
      req(input$category != "")

      # Get current data context
      cat_data <- threat_layers_list[[input$category]]

      if (!is.null(cat_data$subcategories)) {
        req(input$subcategory != "")
        current_data <- cat_data[[input$subcategory]]
      } else {
        current_data <- cat_data
      }

      # Apply filters to the data
      filtered_layers <- apply_filters(current_data$data, input)

      # Store filtered results
      filtered_layers_info(list(
        files = filtered_layers,
        criteria = list(
          category = input$category,
          subcategory = input$subcategory,
          type = input$type,
          species = input$species,
          year = input$year,
          year_range = input$year_range,
          month = input$month,
          dataset = input$dataset,
          combination_method = if (nrow(filtered_layers) > 1) "sum" else NULL
        ),
        count = nrow(filtered_layers)
      ))
    })


    # Handle clear button click
    observeEvent(input$clear_filter, {
      updateSelectInput(session, "category", selected = "")
      reset_dynamic_inputs(session)
      filtered_layers_info(NULL)
      showNotification("Filters cleared", type = "message")
    })

    # Results summary
    output$results_summary <- renderUI({
      req(filtered_layers_info())

      result <- filtered_layers_info()

      if (result$count == 0) {
        div(
          class = "alert alert-warning",
          style = "margin-top: 15px;",
          h5("No Results"),
          p("No layers match the selected criteria.")
        )
      } else {
        ui_elements <- list(
          div(
            class = "alert alert-success",
            style = "margin-top: 15px;",
            h5("Filter Results"),
            p(paste("Found", result$count, "matching layer(s)"))
          )
        )

        # Add combination method dropdown if multiple files
        if (result$count > 1) {
          ui_elements[[length(ui_elements) + 1]] <- fluidRow(
            column(
              6,
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
            )
          )
        }

        do.call(tagList, ui_elements)
      }
    })

    # Update combination method as required
    observeEvent(input$combination_method, {
      req(filtered_layers_info())

      current_info <- filtered_layers_info()
      current_info$criteria$combination_method <- input$combination_method
      filtered_layers_info(current_info)
    })

    # Export for parent module
    list(
      selection = reactive({
        filtered_layers_info()
      }),
      reset = function() {
        # Clear the filtered results
        filtered_layers_info(NULL)

        # Reset category (which will cascade to clear subcategory and dynamic filters)
        updateSelectInput(session, "category", selected = "")

        # Reset all dynamic inputs
        reset_dynamic_inputs(session)
      }
    )
  })
}

#' Create Filter Dropdowns
#'
#' Creates appropriate filter dropdowns based on available data fields.
#'
#' @param data_options List containing filter options (types, years, months, etc.)
#' @param ns Namespace function
#' @return List of UI elements
#' @noRd
create_filter_dropdowns <- function(data_options, ns) {
  dropdowns <- list()

  # Create columns for dropdowns - adjust number based on available filters
  col_width <- 3 # Default to 4 columns

  # Type dropdown
  if (!is.null(data_options$types) && length(data_options$types) > 0) {
    dropdowns[[length(dropdowns) + 1]] <- column(
      col_width,
      selectInput(
        ns("type"),
        "Type:",
        choices = data_options$types,
        selected = NULL,
        multiple = TRUE
      )
    )
  }

  # Species dropdown (for fisheries)
  if (!is.null(data_options$species) && length(data_options$species) > 0) {
    dropdowns[[length(dropdowns) + 1]] <- column(
      col_width,
      selectInput(
        ns("species"),
        "Species:",
        choices = data_options$species,
        selected = NULL,
        multiple = TRUE
      )
    )
  }

  # Dataset dropdown (for petroleum pollution)
  if (!is.null(data_options$datasets) && length(data_options$datasets) > 0) {
    dropdowns[[length(dropdowns) + 1]] <- column(
      col_width,
      selectInput(
        ns("dataset"),
        "Dataset:",
        choices = data_options$datasets,
        selected = NULL,
        multiple = TRUE
      )
    )
  }

  # Year dropdown (regular years or year ranges)
  if (!is.null(data_options$years) && length(data_options$years) > 0) {
    dropdowns[[length(dropdowns) + 1]] <- column(
      col_width,
      selectInput(
        ns("year"),
        "Year:",
        choices = data_options$years,
        selected = NULL,
        multiple = TRUE
      )
    )
  } else if (!is.null(data_options$year_ranges) && length(data_options$year_ranges) > 0) {
    dropdowns[[length(dropdowns) + 1]] <- column(
      col_width,
      selectInput(
        ns("year_range"),
        "Year Range:",
        choices = data_options$year_ranges,
        selected = NULL,
        multiple = TRUE
      )
    )
  }

  # Month dropdown
  if (!is.null(data_options$months) && length(data_options$months) > 0) {
    # Convert month numbers to names
    month_names <- c(
      "January", "February", "March", "April", "May", "June",
      "July", "August", "September", "October", "November", "December"
    )
    month_choices <- data_options$months
    names(month_choices) <- month_names[data_options$months]

    dropdowns[[length(dropdowns) + 1]] <- column(
      col_width,
      selectInput(
        ns("month"),
        "Month:",
        choices = month_choices,
        selected = NULL,
        multiple = TRUE
      )
    )
  }

  # Wrap dropdowns in fluidRow(s)
  if (length(dropdowns) > 0) {
    # Group dropdowns into rows of 4
    rows <- list()
    for (i in seq(1, length(dropdowns), by = 4)) {
      end_idx <- min(i + 3, length(dropdowns))
      rows[[length(rows) + 1]] <- do.call(fluidRow, dropdowns[i:end_idx])
    }
    return(rows)
  }

  list()
}

#' Apply Filters to Data
#'
#' Filters the data based on user selections.
#'
#' @param data data.frame to filter
#' @param input Shiny input object with filter selections
#' @return Filtered data.frame
#' @noRd
apply_filters <- function(data, input) {
  # Apply type filter
  if (!is.null(input$type) && length(input$type) > 0) {
    data <- data[data$type %in% input$type, ]
  }

  # Apply species filter
  if (!is.null(input$species) && length(input$species) > 0) {
    data <- data[data$species %in% input$species, ]
  }

  # Apply dataset filter
  if (!is.null(input$dataset) && length(input$dataset) > 0) {
    data <- data[data$dataset %in% input$dataset, ]
  }

  # Apply year filter
  if (!is.null(input$year) && length(input$year) > 0) {
    data <- data[data$year %in% input$year, ]
  }

  # Apply year range filter
  if (!is.null(input$year_range) && length(input$year_range) > 0) {
    data <- data[data$year_range %in% input$year_range, ]
  }

  # Apply month filter
  if (!is.null(input$month) && length(input$month) > 0) {
    data <- data[data$month %in% as.numeric(input$month), ]
  }

  filtered_data <- data
}


# Helper function to reset all dynamic inputs
reset_dynamic_inputs <- function(session) {
  updateSelectInput(session, "subcategory", selected = "")
  updateSelectInput(session, "type", selected = NULL)
  updateSelectInput(session, "species", selected = NULL)
  updateSelectInput(session, "year", selected = NULL)
  updateSelectInput(session, "year_range", selected = NULL)
  updateSelectInput(session, "month", selected = NULL)
  updateSelectInput(session, "dataset", selected = NULL)
  updateSelectInput(session, "combination_method", selected = "sum")
}
