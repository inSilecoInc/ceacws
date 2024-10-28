# Internals
#' @importFrom exactextractr exact_extract
#' @importFrom fs path path_package
#' @importFrom glue glue glue_sql
#' @importFrom units set_units
#' @importFrom whisker whisker.render
#' @importFrom yaml yaml.load_file write_yaml read_yaml
NULL


# ------------------------------------------------------------------------------
# Path to configuration file
yaml_file <- "workspace/config/config.yaml"

# Function to load configuration file
load_config <- function(config_path = yaml_file) {
  if (file.exists(config_path)) {
    config <- yaml::read_yaml(config_path)
    return(config)
  } else {
    stop("Configuration file not found: ", config_path)
  }
}

# Option to load on package load
.onLoad <- function(lib, pkg) {
  rlang::run_on_load()
}

rlang::on_load({
  # Load configuration file
  if (file.exists(yaml_file)) {
    config <- load_config()

    # Store in an internal environment for use across the package
    # .configEnv$config <- config
  }
})
