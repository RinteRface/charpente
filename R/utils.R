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



#' Create file
#'
#' @param path Path to file.
#' @param ... Elements to add.
#' @noRd
#' @keywords internal
file_create <- function(path, ...){

  exists <- fs::file_exists(paste(path, ..., sep ="/"))
  if(exists) {
    cli::cli_alert_warning("Editing file at {.file {path}}")
    write(..., file = path, append = TRUE)
  } else {
    fs::file_create(path)
    write(..., file = path)
    cli::cli_alert_success("File {.file {path}} successfully created")
  }
}