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



#' Return the full path for a default file template
#'
#' @keywords internal
#' @param file Name of the file including the file extension.
#' @param where Where to find the template.
get_template <- function(file, where) {
  paste0(where, "/", file)
}


#' Insert provided parameters into template
#'
#' @keywords internal
#' @param template Template obtained with \link{get_template}.
#' @param ... List of parameters to insert in the template.
#' @param where Where to find the template.
#' @importFrom glue glue
#' @importFrom readr read_file
process_template <- function(template, ..., where = system.file("utils", package = "charpente")) {
  pars <- list(...)
  temp <- glue::glue(
    readr::read_file(get_template(template, where)),
    name = pars$name,
    split_version = paste(head(strsplit(pars$version, ".", fixed = TRUE)[[1]], -1), collapse = "."),
    version = pars$version,
    entry_point = "main.js",
    license = pars$license,
    .open = "<<",
    .close = ">>"
  )

  write_there <- function(...) {
    write(..., file = "./package.json", append = FALSE)
  }
  write_there(temp)
}
