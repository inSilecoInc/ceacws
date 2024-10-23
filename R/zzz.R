# Internals
#' @importFrom exactextractr exact_extract
#' @importFrom fs path path_package
#' @importFrom glue glue glue_sql
#' @importFrom units set_units
#' @importFrom whisker whisker.render
#' @importFrom yaml yaml.load_file write_yaml read_yaml
NULL


# ------------------------------------------------------------------------------
.onLoad <- function(lib, pkg) {
  rlang::run_on_load()
}
# Path to configuration file
yaml_file <- here::here("project-data", "config", "config.yml")
rlang::on_load({
  # Load configuration file
  if (file.exists(yaml_file)) {
    config <- yaml::read_yaml(yaml_file)

    # Store it in an internal environment for use across the package
    # .configEnv$config <- config
  } else {
    warning("YAML configuration file not found!")
  }
})
