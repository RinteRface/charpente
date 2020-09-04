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

#' Retrieves Package Name
#' 
#' @return Name of package.
get_pkg_name <- function(){
  desc <- readLines("DESCRIPTION")
  pkg <- desc[grepl("^Package:", desc)]
  pkg <- gsub("^Package: ", "", pkg)
  trimws(pkg)
}

#' Create file
#'
#' @param path Path to file.
#' @noRd
#' @keywords internal
file_create <- function(path){
  exists <- fs::file_exists(path)
  if(exists) {
    cli::cli_alert_warning("Editing file at {.file {path}}")
  } else {
    fs::file_create(path)
    cli::cli_alert_success("File {.file {path}} successfully created")
  }
}
