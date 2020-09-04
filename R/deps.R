#' Imports External Dependencies
#'
#' Download and create dependency function.
#'
#' @param name Name of library.
#' @param pkg Package where to create the dependency.
#' @param tag Library version. Default to NULL. If NULL, takes the latest version.
#' @param options See \link{charpente_options}.
#'
#' @export
#' @examples
#' \dontrun{
#'  create_dependency("tabler", pkg = "tablerDash")
#'  # Use CDNs
#'  create_dependency(
#'   "framework7",
#'   pkg = "shinyMobile",
#'   options = charpente_options(local = FALSE)
#'  )
#' }
create_dependency <- function(name, pkg, tag = NULL, options = charpente_options()){

  # assert is in package
  # should further check valid name, e.g.: no spaces or punctuation

  # checks
  if (missing(name)) stop("Missing `name`")
  if (missing(pkg)) stop("Missing `pkg`")

  # discover assets and stop if not
  if (is.null(tag)) tag <- get_dependency_versions(name, TRUE)
  assets <- get_dependency_assets(name, tag)
  if(nrow(assets$files) == 0) stop(sprintf("No assets found for %s", name))


  # path to dependency
  path <- sprintf("inst/%s-%s", name, tag)

  # below we select step by step. It possible that a repo does not have bundle of minified
  # files. We inspect the content each time, if it's NULL, we change options.
  scripts <- select_asset(assets$files, "js", name, options)
  if (is.null(scripts)) {
    scripts <- select_asset(assets$files, "js", name, options = charpente_options(bundle = FALSE))
  }
  if (is.null(scripts)) {
    scripts <- select_asset(assets$files, "js", name, options = charpente_options(bundle = FALSE, minified = FALSE))
  }
  # CSS files are not in bundles
  stylesheets <- select_asset(assets$files, "css", name, options = charpente_options(bundle = FALSE))
  if (is.null(stylesheets)) {
    stylesheets <- select_asset(assets$files, "css", name, options = charpente_options(bundle = FALSE, minified = FALSE))
  }


  # if local download files and create directories
  if (options$local) {
    # create directories only when necessary and download files
    if(!is.null(scripts)) {
      directory_create_asset(path, "js")
      dependencies_download(path, paste0(assets$url, scripts))
    }
    if(!is.null(stylesheets)) {
      directory_create_asset(path, "css")
      dependencies_download(path, paste0(assets$url, stylesheets))
    }
  }


  # TO DO ... create html_dependencies.R. Only if it does not exist ...
  if (!is.null(scripts) || !is.null(stylesheets)) {

    # need to overwrite path which was used before
    path <- sprintf("R/%s-dependencies.R", pkg)

    file_create(path)

    # taken from golem ;)
    write_there <- function(...){
      write(..., file = path, append = TRUE)
    }

    # write in the file
    tag <- strsplit(strsplit(assets$url, "@")[[1]][2], "/")[[1]][1]
    # attach function
    write_there(sprintf("add_%s_deps <- function(tag) {", name))

    # htmlDependency content
    write_there(sprintf(" %s_deps <- htmltools::htmlDependency(", name))
    write_there(sprintf('  name = "%s",', name))
    write_there(sprintf('  version = utils::packageVersion("%s"),', pkg))
    if (options$local) {
      write_there(sprintf('  src = c(file = "%s"),', tag))
    } else {
      write_there(sprintf('  src = c(href = "%s"),', assets$url))
    }
    if (!is.null(scripts)) {
      write_there(sprintf('  script = c("%s"),', scripts))
    }
    if (!is.null(stylesheets)) {
      write_there("  stylesheet = c(")
      lapply(seq_along(stylesheets), function (i) {
        if (i == length(stylesheets)) {
          write_there(sprintf('   "%s"', stylesheets[[i]]))
        } else {
          write_there(sprintf('   "%s",', stylesheets[[i]]))
        }
      })
      write_there("  ),")
    }
    if (options$local) {
      write_there(sprintf('  package = "%s",', pkg))
    }
    # end deps
    write_there(" )")

    # attach deps
    write_there(sprintf(" htmltools::tagList(tag, %s_deps)", name))
    # end function
    write_there("}")
    write_there("    ")
  } else {
    cli::cli_alert_danger("Unable to create the dependency")
  }
}




#' Configure charpente
#'
#' @param local Whether to download files locally or to point to a CDN. Default to TRUE.
#' @param minified Whether to download minified files. Default to TRUE. Set to FALSE should you need
#' all files.
#' @param bundle Some libraries like Bootstrap provide bundles containing all components.
#' If bundle is TRUE, the download will target only those files.
#'
#' @return A list of options.
#' @export
charpente_options <- function(local = TRUE, minified = TRUE, bundle = TRUE) {
  list(
    local = local,
    minified = minified,
    bundle = bundle
  )
}



#' Select a specific type of asset
#'
#' @param name Library name.
#' @param assets Dataframe of assets.
#' @param type Type to extract. Could be js, css, ...
#' @param options See \link{charpente_options}.
#'
#' @return A vector
#' @noRd
#' @keywords internal
select_asset <- function(assets, type, name, options) {
  # TO DO: fine tune the regex -> maybe people only want minified files, maybe they
  # want everything, ...
  regex <- if (options$bundle) {
    sprintf("^.+\\.bundle\\.min\\.%s$", type)
  } else {
    if (options$minified) {
      sprintf("^.+\\.min\\.%s$", type)
    } else {
      sprintf("^.+\\.%s$", type)
    }
  }
  selected_assets <- assets[grep(regex, assets$name), ]$name
  # this will prevent to create a directory for nothing
  if (length(selected_assets) == 0) return(NULL)
  paste(type, selected_assets, sep = "/")
}


#' Creates Assets Directories
#'
#' @param path Path to store dependencies `inst/libraryName`.
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
#' @param files Files to download
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

    # don't download if empty folder
    if (extension != "") {
      utils::download.file(file, path, quiet = TRUE)
    } else {
      cli::cli_alert_danger("No file to download")
    }
  }, base_path = path)

  invisible()
}


# dont change !!! CDNs have different API structure
options("DEFAULT_CDN" = "https://data.jsdelivr.com/v1/package/npm/")


#' Get all version for the current dependency
#'
#' Query from \url{https://data.jsdelivr.com/v1/package/npm/} under the hood.
#'
#' @param dep Library name.
#' @param latest Whether to get the last version. Default to FALSE.
#'
#' @return A vector containing all versions
#' @export
#'
#' @examples
#' \dontrun{
#'  get_dependency_versions("framework7")
#'  get_dependency_versions("bootstrap")
#'  get_dependency_versions("react", latest = TRUE)
#' }
get_dependency_versions <- function(dep, latest = FALSE) {
  # default to cdnjs
  cdn <- getOption("DEFAULT_CDN")
  tryCatch({
    cli::cli_alert_info(sprintf("Trying with %s%s", cdn, dep))
    temp <- jsonlite::fromJSON(sprintf("%s%s", cdn, dep))
    if (latest) {
      cli::cli_alert_success("Success!")
      cli::cli_rule()
      temp$tags$latest
    } else {
      cli::cli_alert_success("Success!")
      cli::cli_rule()
      temp$versions
    }
  }, error = function(e) {
    cli::cli_alert_danger(sprintf("Failed: we tried but %s", e))
  })
}



#' Get all links to dependencies
#'
#' Query from \url{https://data.jsdelivr.com/v1/package/npm/} under the hood.
#'
#' @param dep Library name.
#' @param tag Library version. Default to latest.
#'
#' @return A list of url containing links to CSS and JS dependencies for the given library.
#' @export
#'
#' @examples
#' \dontrun{
#'  get_dependency_assets("bootstrap")
#'  get_dependency_assets("framework7", tag = "5.5.5")
#' }
get_dependency_assets <- function(dep, tag = "latest") {
  # get all js and css assets from jsdelivr
  cdn <- getOption("DEFAULT_CDN")
  if (tag == "latest") tag <- get_dependency_versions(dep, TRUE)
  temp <- jsonlite::fromJSON(sprintf("%s%s@%s", cdn, dep, tag))$files

  download_endpoint <- "https://cdn.jsdelivr.net/npm/"
  url <- sprintf("%s%s@%s/", download_endpoint, dep, tag)

  # bootstrap has a dist folder
  if ("dist" %in% temp$name) {
    temp <- temp[temp$name %in% c("dist"), "files"][[1]]
    temp <- do.call(rbind, temp$files)
    list(url = paste0(url, "dist/"), files = temp[, c("name", "hash")])
  } else {
    temp <- temp[temp$name %in% c("css", "js"), "files"]
    temp <- do.call(rbind, temp)
    list(url = url, files = temp[, c("name", "hash")])
  }
}



#fromJSON("https://data.jsdelivr.com/v1/package/npm/framework7@5.7.12") %>%
#  tibble::as_tibble() %>%
#  dplyr::filter(files$name %in% c("css", "js")) %>%
#  dplyr::mutate(
#    files = files$files,
#    name = files$name
#  ) %>%
#  dplyr::select(files) %>%
#  tidyr::unnest(c(files)) %>%
#  dplyr::select(name, hash)




