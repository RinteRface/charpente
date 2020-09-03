#' Create Directory
#' 
#' @param path Path to directory to create.
#' @param ... Addition arguments to pass to [fs::dir_create()]
#' @noRd 
#' @keywords internal
directory_create <- function(path, ...){

  exists <- fs::dir_exists(path)

  if(exists)
    cli::cli_alert_warning("Directory {.file {path}} already exists")

  fs::dir_create(path, ...)
  cli::cli_alert_success("Directory {.file {path}} successfully created")
}