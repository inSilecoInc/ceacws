#' Custom report rendering
#'
#' @description A function to render R Makdown templates found in `inst/
#' templates`.
#'
#' @param template a template. Use "?" to list available template.
#' @param data data passed to [whisker::whisker.render()].
#' @param ... further arguments passed to [rmarkdown::render()].
#'
#' @return The path of the output files or a vector of template names,
#' invisibly.
#'
#' @examples
#' \dontrun{
#' custom_render("report1",
#'   data = list(title = "templex"),
#'   envir = list(geom = 1)
#' )
#' }
custom_render <- function(template = "?", data = NULL, ...) {
  args <- list(...)

  lst <- list_templates()

  if (template == "?") {
    cli::cli_ul()
    for (i in lst) {
      cli::cli_li("{i}")
    }
    cli::cli_end()
    return(invisible(lst))
  }

  stopifnot(exprs = {
    template %in% names(lst)
    length(template) == 1
  })

  fl <- lst[names(lst) == template]

  cli::cli_alert_info(
    "Whisker rendering {fl}"
  )

  tfl <- file.path(tempdir(), fs::path_file(fl))
  readLines(fl) |>
    whisker::whisker.render(data = data) |>
    writeLines(con = tfl)

  if (!"output_dir" %in% names(args)) {
    args <- c(output_dir = ".", args)
  }

  cli::cli_alert_info(
    "Rmarkdown rendering {tfl}"
  )

  out <- do.call(rmarkdown::render, c(input = tfl, args))

  invisible(out)
}


list_templates <- function() {
  out <- app_sys("templates") |>
    list.files(full.names = TRUE)
  names(out) <- unlist(out) |>
    fs::path_file() |>
    fs::path_ext_remove()
  out
}
