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
  # If there is already a file (might be the case if update a dependency),
  # we remove the existing file (avoid to append to an existing file)
  if (exists) {
    fs::file_delete(path)
    cli::cli_alert_warning("Remove existing file {.file {path}}")
  } else {
    fs::file_create(path)
    cli::cli_alert_success("File {.file {path}} successfully created")
  }
}



#' Golem's default custom templates
#'
#' These functions do not aim at being called as is by users, but to be
#' passed as an argument to the \link{create_custom_handler} function.
#'
#' @param path The path to the JS script where this template will be written.
#' @param name Shiny's custom handler name.
#' @param code JavaScript code to be written in the function.
#'
#' @keywords internal
#' @export
js_handler_template <- function (path, name, code = " ")
{
  write_there <- function(...) {
    write(..., file = path, append = TRUE)
  }
  write_there("$(function() {")
  write_there(sprintf("  Shiny.addCustomMessageHandler('%s', function(message) {",
                      name))
  write_there(code)
  write_there("  });")
  write_there("});")
}
