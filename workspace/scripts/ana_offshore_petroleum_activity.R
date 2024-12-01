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
  dat <- lapply(input_files, sf::st_read, quiet = TRUE) |>
    dplyr::bind_rows() |>
    sf::st_transform(32198) |>
    dplyr::mutate(geom_type = sf::st_geometry_type(geom))
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
    dplyr::group_by(classification, status)
  nm <- dplyr::group_keys(dat)
  dat <- dplyr::group_split(dat)
  for (i in 1:length(dat)) {
    dat[[i]] <- sf::st_union(dat[[i]]) |>
      sf::st_as_sf()
  }
  dat <- dplyr::bind_rows(dat) |>
    cbind(nm)

  # Rasterize and export
  group_rasterize_export(
    dat,
    grouping_vars = c("classification", "status"),
    fun = "count",
    field = NA,
    grid = grid,
    aoi = aoi,
    dataset_name = "ofshore_petroleum_activity",
    output_path = output_path
  )
}
