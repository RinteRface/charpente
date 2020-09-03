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
