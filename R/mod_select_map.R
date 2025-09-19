#' select_map UI Function
#'
#' @description A shiny Module to create spatial objects.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_select_map_ui <- function(id) {
    ns <- NS(id)
    tagList(
        # Search and explore section
        myhelptxt("Search and explore map."),
        br(),
        div(
            style = "display: inline-block;",
            textInput(
                ns("location"),
                label = "Explore by location",
                value = NULL
            )
        ),
        actionButton(ns("search_loc"), "Search", icon = icon("search")),
        actionButton(ns("reset_view"), "Reset view", icon = icon("arrow-right")),
        
        hr(),
        
        # Resampling section
        h6("Resampling", class = "mb-3"),
        
        h6("Extent Options", class = "mb-3"),
        
        # Extent method selection
        radioButtons(
            ns("resample_extent_method"),
            "Select extent method:",
            choices = list(
                "Draw on map" = "draw",
                "Bounding box (EPSG:4326)" = "bbox",
                "Upload polygon file" = "upload"
            ),
            selected = "draw"
        ),
        
        # Draw on map option
        conditionalPanel(
            condition = paste0("input['", ns("resample_extent_method"), "'] == 'draw'"),
            div(
                class = "d-flex align-items-center mb-2",
                actionButton(
                    ns("info_drawing"),
                    icon("info-circle"),
                    class = "btn-info btn-sm me-2",
                    style = "padding: 4px 8px;"
                ),
                span("Use the drawing tools on the map to create polygons", 
                     class = "small text-muted")
            ),
            
            # List of drawn polygons
            h6("Drawn Polygons", class = "mt-3 mb-2"),
            uiOutput(ns("polygon_list")),
            
            # Clear all button
            actionButton(
                ns("clear_all_polygons"),
                "Clear All",
                icon = icon("trash"),
                class = "btn-outline-danger btn-sm mt-2"
            )
        ),
        
        # Bounding box option
        conditionalPanel(
            condition = paste0("input['", ns("resample_extent_method"), "'] == 'bbox'"),
            fluidRow(
                column(
                    6,
                    numericInput(
                        ns("bbox_min_lon"),
                        "Min Longitude:",
                        value = -70,
                        min = -180,
                        max = 180,
                        step = 0.001
                    )
                ),
                column(
                    6,
                    numericInput(
                        ns("bbox_max_lon"),
                        "Max Longitude:",
                        value = -40,
                        min = -180,
                        max = 180,
                        step = 0.001
                    )
                )
            ),
            fluidRow(
                column(
                    6,
                    numericInput(
                        ns("bbox_min_lat"),
                        "Min Latitude:",
                        value = 40,
                        min = -90,
                        max = 90,
                        step = 0.001
                    )
                ),
                column(
                    6,
                    numericInput(
                        ns("bbox_max_lat"),
                        "Max Latitude:",
                        value = 60,
                        min = -90,
                        max = 90,
                        step = 0.001
                    )
                )
            ),
            actionButton(
                ns("preview_bbox"),
                "Preview on map",
                icon = icon("eye"),
                class = "btn-outline-primary btn-sm"
            )
        ),
        
        # File upload option
        conditionalPanel(
            condition = paste0("input['", ns("resample_extent_method"), "'] == 'upload'"),
            fileInput(
                ns("extent_file"),
                "Choose polygon file:",
                accept = c(".shp", ".geojson", ".gpkg", ".kml"),
                multiple = FALSE
            ),
            p("Supported formats: .shp, .geojson, .gpkg, .kml",
              class = "text-muted small")
        ),
        
        hr(),
        
        # Resolution options
        h6("Resolution", class = "mb-3"),
        numericInput(
            ns("resample_resolution"),
            "New resolution (degrees):",
            value = 0.01,
            min = 0.001,
            max = 1,
            step = 0.001
        ),
        
        hr(),
        
        # Action buttons
        div(
            class = "d-grid gap-2",
            actionButton(
                ns("apply_resampling"),
                "Apply Resampling",
                icon = icon("crop"),
                class = "btn-success",
                style = "margin-bottom: 10px;"
            ),
            actionButton(
                ns("reset_extent"),
                "Reset to Original",
                icon = icon("undo"),
                class = "btn-outline-secondary"
            )
        ),
        
        hr(),
        
        # Geometry info table
        DT::DTOutput(ns("geom_selected"))
    )
}

#' select_map Server Functions
#'
#' @noRd
mod_select_map_server <- function(id, r) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Polygon management for resampling
        polygon_list <- reactiveVal(list())
        selected_polygon <- reactiveVal(NULL)

        # Search and map navigation
        observeEvent(input$search_loc, {
            cli::cli_alert_info("Map - Resetting map view using location")
            r$map <- set_view_to_city(input$location, r$map)
        })

        observeEvent(input$reset_view, {
            cli::cli_alert_info("Map - Resetting map view to default")
            r$map <- reset_view(r$map)
        })

        # Info button modal for drawing instructions
        observeEvent(input$info_drawing, {
            showModal(modalDialog(
                title = "Drawing Instructions",
                tags$div(
                    tags$h6("How to create polygons:"),
                    tags$ul(
                        tags$li("Click the polygon tool in the map toolbar"),
                        tags$li("Click on the map to start drawing"),
                        tags$li("Click to add each point of your polygon"),
                        tags$li("Double-click or click the first point to finish")
                    )
                ),
                easyClose = TRUE,
                footer = modalButton("Close")
            ))
        })

        # Monitor for new drawings and add to list
        observeEvent(r$geom_slc, {
            # Update geometry info table
            output$geom_selected <- DT::renderDT({
                cli::cli_alert_info("Updating geom info")
                dfout <- tryCatch(
                    {
                        if (!is.null(r$geom_slc) && !is.null(r$geom_slc$all) && nrow(r$geom_slc$all) > 0) {
                            r$geom_slc$all |> sf::st_drop_geometry()
                        } else {
                            data.frame()
                        }
                    },
                    error = function(e) data.frame()
                )
                dfout
            })

            # Update polygon list for resampling
            tryCatch({
                if (!is.null(r$geom_slc) && !is.null(r$geom_slc$all) && 
                    inherits(r$geom_slc$all, "sf") && nrow(r$geom_slc$all) > 0) {
                    
                    geom_data <- r$geom_slc$all
                    
                    # Clear existing list and rebuild
                    new_polygons <- list()
                    
                    # Add all valid polygons from the current map state
                    for (i in seq_len(nrow(geom_data))) {
                        geom_type <- sf::st_geometry_type(geom_data[i, ])[1]
                        
                        if (geom_type %in% c("POLYGON", "MULTIPOLYGON")) {
                            polygon_id <- paste0("polygon_", i)
                            
                            new_polygons[[polygon_id]] <- list(
                                id = polygon_id,
                                geometry = geom_data[i, ],
                                name = paste("Polygon", i)
                            )
                        }
                    }
                    
                    # Update polygon list
                    polygon_list(new_polygons)
                    
                    # Clear selection if it no longer exists
                    if (!is.null(selected_polygon()) && !selected_polygon() %in% names(new_polygons)) {
                        selected_polygon(NULL)
                    }
                } else {
                    # No valid geometries, clear the list
                    polygon_list(list())
                    selected_polygon(NULL)
                }
            }, error = function(e) {
                cli::cli_alert_warning("Error processing geometries: {e$message}")
                polygon_list(list())
                selected_polygon(NULL)
            })
        })

        # Render polygon list with radio buttons
        output$polygon_list <- renderUI({
            polygons <- polygon_list()
            
            if (length(polygons) == 0) {
                return(p("No polygons drawn yet.", class = "text-muted small"))
            }
            
            # Create radio button choices
            choices <- setNames(names(polygons), sapply(polygons, function(x) x$name))
            
            tagList(
                radioButtons(
                    ns("selected_polygon_id"),
                    "Select polygon for resampling:",
                    choices = choices,
                    selected = selected_polygon()
                ),
                # Remove buttons for each polygon
                lapply(names(polygons), function(polygon_id) {
                    polygon <- polygons[[polygon_id]]
                    div(
                        class = "d-flex justify-content-between align-items-center mb-1",
                        span(polygon$name, class = "small"),
                        actionButton(
                            ns(paste0("remove_", polygon_id)),
                            icon("times"),
                            class = "btn-outline-danger btn-xs",
                            style = "padding: 2px 4px; font-size: 10px;"
                        )
                    )
                })
            )
        })

        # Handle polygon selection
        observeEvent(input$selected_polygon_id, {
            selected_polygon(input$selected_polygon_id)
        })

        # Handle individual polygon removal
        observe({
            polygons <- polygon_list()
            for (polygon_id in names(polygons)) {
                local({
                    current_id <- polygon_id
                    observeEvent(input[[paste0("remove_", current_id)]], {
                        current_polygons <- polygon_list()
                        current_polygons[[current_id]] <- NULL
                        polygon_list(current_polygons)
                        
                        # Clear selection if removed polygon was selected
                        if (selected_polygon() == current_id) {
                            selected_polygon(NULL)
                        }
                    }, ignoreInit = TRUE, once = TRUE)
                })
            }
        })

        # Handle clear all polygons
        observeEvent(input$clear_all_polygons, {
            polygon_list(list())
            selected_polygon(NULL)
            
            # Clear mapedit geometries properly
            r$geom_slc <- list(all = sf::st_sf(data.frame(), geometry = sf::st_sfc()))
            
            shiny::showNotification("All polygons cleared.", type = "warning")
        })

        # Handle bounding box preview
        observeEvent(input$preview_bbox, {
          tryCatch({
            # Create bbox from input values
            bbox_coords <- c(
              xmin = input$bbox_min_lon,
              ymin = input$bbox_min_lat,
              xmax = input$bbox_max_lon,
              ymax = input$bbox_max_lat
            )

            # Validate bbox
            if (bbox_coords["xmin"] >= bbox_coords["xmax"] ||
                bbox_coords["ymin"] >= bbox_coords["ymax"]) {
              shiny::showNotification(
                "Invalid bounding box: minimum values must be less than maximum values",
                type = "error"
              )
              return()
            }

            # Create bbox polygon
            bbox_poly <- sf::st_polygon(list(matrix(c(
              bbox_coords["xmin"], bbox_coords["ymin"],
              bbox_coords["xmax"], bbox_coords["ymin"],
              bbox_coords["xmax"], bbox_coords["ymax"],
              bbox_coords["xmin"], bbox_coords["ymax"],
              bbox_coords["xmin"], bbox_coords["ymin"]
            ), ncol = 2, byrow = TRUE)))

            bbox_sf <- sf::st_sf(geometry = sf::st_sfc(bbox_poly, crs = 4326))

            # Add to map as preview
            r$map <- r$map |>
              leaflet::clearGroup("bbox_preview") |>
              leaflet::addPolygons(
                data = bbox_sf,
                group = "bbox_preview",
                color = "red",
                weight = 2,
                opacity = 0.8,
                fillOpacity = 0.2,
                popup = "Bounding Box Preview"
              )

            shiny::showNotification(
              "Bounding box preview added to map",
              type = "message"
            )
          }, error = function(e) {
            shiny::showNotification(
              paste("Error creating bounding box:", e$message),
              type = "error"
            )
          })
        })

        # Handle file upload
        observeEvent(input$extent_file, {
          tryCatch({
            if (!is.null(input$extent_file)) {
              file_path <- input$extent_file$datapath
              file_ext <- tools::file_ext(input$extent_file$name)

              # Read spatial file based on extension
              if (file_ext %in% c("shp", "geojson", "gpkg", "kml")) {
                uploaded_sf <- sf::st_read(file_path, quiet = TRUE)

                # Transform to WGS84 if needed
                if (sf::st_crs(uploaded_sf) != sf::st_crs(4326)) {
                  uploaded_sf <- sf::st_transform(uploaded_sf, 4326)
                }

                # Add to map
                r$map <- r$map |>
                  leaflet::clearGroup("uploaded_extent") |>
                  leaflet::addPolygons(
                    data = uploaded_sf,
                    group = "uploaded_extent",
                    color = "blue",
                    weight = 2,
                    opacity = 0.8,
                    fillOpacity = 0.2,
                    popup = "Uploaded Extent"
                  )

                shiny::showNotification(
                  "File uploaded and added to map",
                  type = "message"
                )
              } else {
                shiny::showNotification("Unsupported file format", type = "error")
              }
            }
          }, error = function(e) {
            shiny::showNotification(
              paste("Error uploading file:", e$message),
              type = "error"
            )
          })
        })

        # Handle resampling application
        observeEvent(input$apply_resampling, {
          tryCatch({
            extent_method <- input$resample_extent_method
            resolution <- input$resample_resolution

            # Get extent based on method
            extent_geom <- NULL

            if (extent_method == "draw") {
              if (!is.null(selected_polygon()) &&
                  selected_polygon() %in% names(polygon_list())) {
                extent_geom <- polygon_list()[[selected_polygon()]]$geometry
              } else {
                shiny::showNotification(
                  "Please select a drawn polygon for resampling",
                  type = "error"
                )
                return()
              }
            } else if (extent_method == "bbox") {
              # Create bbox geometry from inputs
              bbox_coords <- c(
                xmin = input$bbox_min_lon,
                ymin = input$bbox_min_lat,
                xmax = input$bbox_max_lon,
                ymax = input$bbox_max_lat
              )

              if (bbox_coords["xmin"] >= bbox_coords["xmax"] ||
                  bbox_coords["ymin"] >= bbox_coords["ymax"]) {
                shiny::showNotification(
                  "Invalid bounding box: minimum values must be less than maximum values",
                  type = "error"
                )
                return()
              }

              bbox_poly <- sf::st_polygon(list(matrix(c(
                bbox_coords["xmin"], bbox_coords["ymin"],
                bbox_coords["xmax"], bbox_coords["ymin"],
                bbox_coords["xmax"], bbox_coords["ymax"],
                bbox_coords["xmin"], bbox_coords["ymax"],
                bbox_coords["xmin"], bbox_coords["ymin"]
              ), ncol = 2, byrow = TRUE)))

              extent_geom <- sf::st_sf(geometry = sf::st_sfc(bbox_poly, crs = 4326))
            } else if (extent_method == "upload") {
              if (!is.null(input$extent_file)) {
                file_path <- input$extent_file$datapath
                extent_geom <- sf::st_read(file_path, quiet = TRUE)

                if (sf::st_crs(extent_geom) != sf::st_crs(4326)) {
                  extent_geom <- sf::st_transform(extent_geom, 4326)
                }
              } else {
                shiny::showNotification(
                  "Please upload a file for resampling",
                  type = "error"
                )
                return()
              }
            }

            if (!is.null(extent_geom)) {
              # Store resampling parameters for use by other modules
              r$resampling_extent <- extent_geom
              r$resampling_resolution <- resolution
              r$resampling_applied <- Sys.time()

              shiny::showNotification(
                "Resampling parameters applied",
                type = "success"
              )
            } else {
              shiny::showNotification(
                "Unable to determine extent for resampling",
                type = "error"
              )
            }

          }, error = function(e) {
            shiny::showNotification(
              paste("Error applying resampling:", e$message),
              type = "error"
            )
          })
        })

        # Handle reset to original
        observeEvent(input$reset_extent, {
          r$resampling_extent <- NULL
          r$resampling_resolution <- NULL
          r$resampling_applied <- NULL

          # Clear preview layers
          r$map <- r$map |>
            leaflet::clearGroup("bbox_preview") |>
            leaflet::clearGroup("uploaded_extent")

          shiny::showNotification(
            "Reset to original extent and resolution",
            type = "message"
          )
        })

        # Return polygon data for use by parent modules
        list(
          polygon_list = reactive(polygon_list()),
          selected_polygon = reactive(selected_polygon()),
          selected_polygon_geometry = reactive({
            if (!is.null(selected_polygon()) &&
                selected_polygon() %in% names(polygon_list())) {
              polygon_list()[[selected_polygon()]]$geometry
            } else {
              NULL
            }
          })
        )
      })
    }
