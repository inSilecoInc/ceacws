#' Start function
#'
#' @export

fct_start <- function() {
    # create /docs for reports output
    dir.create(file.path(app_sys("app", "www"), "docs"))

    # create disclaimer.html
    disc_path_md <<- app_sys("app", "www", "static_docs", "disclaimer.md")
    disc_frontm <<- rmarkdown::yaml_front_matter(disc_path_md)

    # Render md document
    path_docs <<- app_sys("app", "www", "static_docs")
    list.files(
        path_docs,
        pattern = ".md$",
        full.names = TRUE
    ) |> lapply(
        \(x) {
            markdown::mark(x, x |> fs::path_ext_set("html"))
        }
    )

    map_bbox <<- list(lng1 = -65.6, lat1 = 45.5, lng2 = -61.6, lat2 = 51.5)
    
    # Initialize threat layers list
    available_threat_layers <<- get_threat_layers()

    onStop(clean_up_app)
}

clean_up_app <- function() {
    cli::cli_alert_info("Application stopped -- cleaning up")
    unlink(file.path(app_sys("app", "www"), "docs"), recursive = TRUE)
    unlink(disc_path_md |> fs::path_ext_set("html"))
}
