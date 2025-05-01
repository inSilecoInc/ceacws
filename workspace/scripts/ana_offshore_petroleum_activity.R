ana_offshore_petroleum_activity <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/offshore_petroleum_activity-1.0.0/"
  # # dir.create(output_path)
  # input_files <- c(
  #   "workspace/data/harvested/offshore_petroleum_nfl-1.0.0/processed/offshore_petroleum_nfl.gpkg",
  #   "workspace/data/harvested/offshore_petroleum_ns-1.0.0/processed/offshore_petroleum_ns.gpkg",
  #   "workspace/data/harvested/aoi-1.0.0/processed/aoi.gpkg",
  #   "workspace/data/harvested/aoi-1.0.0/processed/grid.tif"
  # )
  input_files <- unlist(input_files)

  grid <- terra::rast(input_files[basename(input_files) == "grid.tif"])
  aoi <- sf::st_read(input_files[basename(input_files) == "aoi.gpkg"], quiet = TRUE)
  input_files <- input_files[!basename(input_files) %in% c("grid.tif", "aoi.gpkg")]

  # Data

  # Current Production* - Active license polygons, active development wells, Active installations
  # Current Discovery - Active significant discovery license polygons, active delineation wells
  # Current Exploration - Active exploration license polygons, active exploration wells
  # (Could sum to make a single ‘Current Activity’ layer)

  # Future Scope - Recent calls for bids polygons (NS: 2022; NL: 2023, 2024), active sector polygons. Not sure if we should include closed sectors to incorporate Labrador South sector NL-01LS (see notes in Polygon Layers below)

  # Past Production* - InActive wells, inActive license polygons (if available)
  # Past Discovery - Inactive delineation wells, inactive significant discovery license polygons (if available)
  # Past Exploration - Inactive exploration wells, inactive exploration license polygons (if available)
  # (Could sum to make a single ‘Past Activity’ layer)

  dat <- lapply(input_files, sf::st_read, quiet = TRUE) |>
    dplyr::bind_rows() |>
    sf::st_transform(32198) |>
    dplyr::mutate(geom_type = sf::st_geometry_type(geom)) |>
    dplyr::mutate(
      categories = dplyr::case_when(
        # Future scope
        classification %in% c("call for bids", "sector") ~ "Future Scope",

        # Production - Oil
        classification %in% c("production", "development") &
          status %in% c("Active", "Drilling", "Disposal") &
          subtype %in% c(
            "Production Operations", "Oil Producer",
            "Oil and Gas Well", "Oil Well", "Oil Well"
          ) ~ "Current Oil Production",
        classification %in% c("production", "development") &
          status %in% c("Abandoned", "Suspended", "Closed", "Off Station") &
          subtype %in% c(
            "Production Operations", "Oil Producer",
            "Oil and Gas Well", "Oil Well", "Oil Well"
          ) ~ "Past Oil Production",

        # Production - Non-Oil
        classification %in% c("production", "development") &
          status %in% c("Active", "Drilling", "Disposal") &
          !subtype %in% c(
            "Production Operations", "Oil Producer",
            "Oil and Gas Well", "Oil Well", "Oil Well"
          ) ~ "Current Non-Oil Production",
        classification %in% c("production", "development") &
          status %in% c("Abandoned", "Suspended", "Closed", "Off Station") &
          !subtype %in% c(
            "Production Operations", "Oil Producer",
            "Oil and Gas Well", "Oil Well", "Oil Well"
          ) ~ "Past Non-Oil Production",

        # Production Other
        classification %in% c("production", "development") & is.na(status) ~ "Other Production",

        # Discovery
        classification %in% c("significant discovery", "delineation") &
          status %in% c("Active", "Drilling", "Disposal") ~ "Current Discovery",
        classification %in% c("significant discovery", "delineation") &
          status %in% c("Abandoned", "Suspended", "Closed", "Off Station") ~ "Past Discovery",
        classification %in% c("significant discovery", "delineation") & is.na(status) ~ "Other Discovery",


        # Exploration
        classification %in% c("exploration") &
          status %in% c("Active", "Drilling", "Disposal") ~ "Current Exploration",
        classification %in% c("exploration") &
          status %in% c("Abandoned", "Suspended", "Closed", "Off Station") ~ "Past Exploration",
        classification %in% c("exploration") & is.na(status) ~ "Other Exploration",


        # Licenses
        classification %in% c("exploration licenses") &
          status %in% c("Active") ~ "Current Exploration Licenses",
        classification %in% c("significant discovery licenses") &
          status %in% c("Active") ~ "Current Significant Discovery Licenses",
        classification %in% c("production licenses") &
          status %in% c("Active") ~ "Current Production Licenses",
        classification %in% c("exploration licenses") &
          status %in% c("Suspended") ~ "Past Exploration Licenses",
        classification %in% c("significant discovery licenses") &
          status %in% c("Suspended") ~ "Past Significant Discovery Licenses",
        classification %in% c("production licenses") &
          status %in% c("Suspended") ~ "Past Production Licenses",

        # Default
        .default = NA_character_
      )
    )

  # Spatial processing
  uid <- dat$geom_type == "POINT"
  pts <- dat[uid, ]
  poly <- dat[!uid, ]

  # Buffered points
  pts <- sf::st_buffer(pts, dist = 500)

  # Combine and transform back to CRS 4326
  dat <- dplyr::bind_rows(pts, poly) |>
    sf::st_transform(4326) |>
    dplyr::select(-geom_type)

  # Intersect aoi & grid
  dat <- sf::st_intersection(dat, sf::st_geometry(aoi)) |>
    dplyr::group_by(categories)
  nm <- dplyr::group_keys(dat)
  dat <- dplyr::group_split(dat)
  for (i in 1:length(dat)) {
    dat[[i]] <- sf::st_union(dat[[i]]) |>
      sf::st_as_sf()
    uid <- sf::st_geometry_type(dat[[i]]) == "MULTILINESTRING"
    dat[[i]] <- dat[[i]][!uid, ]
  }
  dat <- dplyr::bind_rows(dat) |>
    cbind(nm)

  # Rasterize and export
  group_rasterize_export(
    dat,
    grouping_vars = "categories",
    fun = "count",
    field = NA,
    grid = grid,
    aoi = aoi,
    dataset_name = "offshore_petroleum_activity",
    output_path = output_path
  )
}
