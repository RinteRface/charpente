#' Imports External Dependencies
#'
#' Download and create depdendency function.
#'
#' @param name Name of library.
#' @param scripts,stylesheets Vector of JavaScript and CSS files to use
#' as depdendencies.
#'
#' @export
create_dependency <- function(name, scripts = NULL, stylesheets = NULL){

  # assert is in package
  # should further check valid name, e.g.: no spaces or punctuation

  # checks
  if(missing(name)) stop("Missing `name`")
  if(is.null(scripts) && is.null(stylesheets))
    stop("Must have at least one of `scripts` or `stylesheets`")

  # path to dependency
  path <- sprintf("inst/%s", name)

  # create directories
  if(!is.null(scripts)) directory_create_asset(path, "js")
  if(!is.null(stylesheets)) directory_create_asset(path, "css")

  # download dependencies
  dependencies_download(path, scripts)
  dependencies_download(path, stylesheets)
}

#' Creates Assets Directories
#'
#' @param path Path to store depdendencies `inst/libraryName`.
#' @param asset Asset path to create, depends on file type.
#'
#' @noRd
#' @keywords internal
directory_create_asset <- function(path, asset = c("js", "css")){
  asset <- match.arg(asset)
  path <- sprintf("%s/%s", path, asset)
  directory_create(path)
}

#' Download Dependencies
#'
#' @inheritParams directory_create_asset
#' @param files Files to download.
#'
#' @noRd
#' @keywords internal
dependencies_download <- function(path, files){
  if(is.null(files)) return()

  # file info
  extension <- tools::file_ext(files)[1]
  file_names <- gsub(".*\\/", "", files)

  # path to store
  path <- sprintf("%s/%s", path, extension)

  # download files
  purrr::map2(files, file_names, function(file, name, base_path){
    path <- sprintf("%s/%s", base_path, name)
    utils::download.file(file, path, quiet = TRUE)
  }, base_path = path)

  invisible()
}


options("CDN" = "https://api.cdnjs.com/libraries/")
#' Get all version for the current dependency
#'
#' @param dep Library name.
#' @param cdn CDN url. Default to \url{https://api.cdnjs.com}.
#' @param latest Whether to get the last version. Default to FALSE.
#'
#' @return A vector containing all versions
#' @export
#'
#' @examples
#' get_dependency_versions("framework7")
#' get_dependency_versions("bootstrap")
#' get_dependency_versions("react", latest = TRUE)
get_dependency_versions <- function(dep, cdn = NULL, latest = FALSE) {
  # default to cdnjs
  cdn <- if (is.null(cdn)) getOption("CDN")
  tryCatch({
    cli::cli_alert_info("Trying with cdnjs")
    res <- jsonlite::fromJSON(sprintf("%s%s", cdn, dep))$versions
    if (latest) {
      cli::cli_alert_success("Success!")
      cli::cli_rule()
      res[length(res)]
    } else {
      cli::cli_alert_success("Success!")
      cli::cli_rule()
      res
    }
  }, error = function(e) {
    cli::cli_alert_danger("Failed with cdnjs")
    cli::cli_alert_info("Trying with jsdelivr")
    tryCatch({
      cli::cli_alert_success("Success!")
      cli::cli_rule()
      temp <- jsonlite::fromJSON(sprintf("https://data.jsdelivr.com/v1/package/npm/%s", dep))
      if (latest) temp$tags$latest else temp$versions
    }, error = function(e) {
      cli::cli_alert_danger(sprintf("Failed: we tried from cdnjs and jsdelivr but %s", e))
    })
  })
}


#https://api.cdnjs.com/libraries/framework7/5.7.12?fields=files,sri
