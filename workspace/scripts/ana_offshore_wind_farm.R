ana_offshore_wind_farm <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/offshore_wind_farm-1.0.0/"
  # # dir.create(output_path)
  # input_files <- c(
  #   "workspace/data/harvested/offshore_wind_can-1.0.0/processed/offshore_wind_can.gpkg",
  #   "workspace/data/harvested/offshore_wind_nfl-1.0.0/processed/offshore_wind_nfl.gpkg",
  #   "workspace/data/harvested/offshore_wind_ns-1.0.0/processed/offshore_wind_ns.gpkg",
  #   "workspace/data/harvested/offshore_wind_usa-1.0.0/processed/offshore_wind_usa.gpkg",
  #   "workspace/data/harvested/aoi-1.0.0/processed/aoi.gpkg",
  #   "workspace/data/harvested/aoi-1.0.0/processed/grid.tif"
  # )
  input_files <- unlist(input_files)

  grid <- terra::rast(input_files[basename(input_files) == "grid.tif"])
  aoi <- sf::st_read(input_files[basename(input_files) == "aoi.gpkg"], quiet = TRUE)
  input_files <- input_files[!basename(input_files) %in% c("grid.tif", "aoi.gpkg")]
  input_files <- input_files[basename(input_files) %in% c("offshore_wind_can.gpkg", "offshore_wind_usa.gpkg")]

  # Data
  dat <- lapply(input_files, sf::st_read, quiet = TRUE) |>
    dplyr::bind_rows() |>
    dplyr::mutate(categories = dplyr::case_when(
      # Current wind farms
      classification %in% c("turbine locations", "substations") ~ "Current wind farms",

      # Transmission infrastructure
      classification %in% c(
        "cable interconnections", "export cable corridors",
        "cable landings", "project inter-array cables"
      ) ~ "Transmission infrastructure",

      # Future Development areas
      classification %in% c(
        "lease", "planning area", "mhk leases and planning areas",
        "project phase areas proposed"
      ) ~ "Future development areas",

      # Environmental monitoring
      classification %in% c("ocean observing devices") ~ "Environmental monitoring",

      # Detauft
      .default = NA_character_
    ))

  # Spatial processing
  geom_type <- sf::st_geometry_type(dat)
  uid <- geom_type %in% c("POINT", "LINESTRING", "MULTILINESTRING")
  pts_lns <- dat[uid, ]
  poly <- dat[!uid, ]

  # Buffered points
  pts_lns <- sf::st_buffer(pts_lns, dist = 500)

  # Combine and transform back to CRS 4326
  dat <- dplyr::bind_rows(pts_lns, poly) |>
    sf::st_transform(4326)

  # Intersect aoi & grid
  dat <- sf::st_intersection(dat, sf::st_geometry(aoi)) |>
    sf::st_make_valid() |>
    dplyr::group_by(categories)
  nm <- dplyr::group_keys(dat)
  dat <- dplyr::group_split(dat)
  for (i in 1:length(dat)) {
    dat[[i]] <- sf::st_union(dat[[i]]) |>
      sf::st_as_sf()
    uid <- sf::st_geometry_type(dat[[i]]) %in% c("MULTILINESTRING", "LINESTRING", "GEOMETRYCOLLECTION")
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
    dataset_name = "offshore_wind_farm",
    output_path = output_path
  )
}
