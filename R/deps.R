#' Imports External Dependencies
#'
#' Download and create dependency function.
#'
#' @param name Name of library.
#' @param tag Library version. Default to NULL. If NULL, takes the latest version.
#' @param options See \link{charpente_options}.
#' @param open Whether to allow rstudioapi to open the newly created script. Default to TRUE.
#'
#' @export
#' @examples
#' \dontrun{
#'  create_dependency("tabler")
#'  # Use CDNs
#'  create_dependency(
#'   "framework7",
#'   options = charpente_options(local = FALSE)
#'  )
#' }
create_dependency <- function(name, tag = NULL, open = interactive(), options = charpente_options()){

  # assert is in package
  # should further check valid name, e.g.: no spaces or punctuation

  # checks
  if (missing(name)) stop("Missing `name`")

  pkg <- get_pkg_name()

  # discover assets and stop if not
  if (is.null(tag)) tag <- get_dependency_versions(name, TRUE)
  assets <- get_dependency_assets(name, tag)

  if (inherits(assets$files, "list")) {
    if (length(assets$files) == 0) {
      stop(sprintf("No assets found for %s", name))
    }
  }

  # delete other versions and install new ones
  if (length(find_dep_file(name)) > 0) {
    fs::file_delete(paste0("inst/", find_dep_file(name)))
  }

  # path to dependency
  path <- sprintf("inst/%s-%s", name, tag)

  # select assets
  scripts <- assets$files$js$file
  stylesheets <- assets$files$css$file

  # Handle case like @vizuaalog/bulmajs

  if (length(grep("/", name)) > 0) {
    if (grep("/", name)) {
      name <- strsplit(name, "/")[[1]][2]
    }
  }

  # if local download files and create directories
  if (options$local) {
    # sanitize path name
    path <- sprintf("inst/%s-%s", name, tag)
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
    path <- sprintf("R/%s-dependencies.R", name)
    file_create(path)

    # taken from golem ;)
    write_there <- function(...){
      write(..., file = path, append = TRUE)
    }

    insert_multiple_lines <- function(what) {
      lapply(seq_along(what), function (i) {
        if (i == length(what)) {
          write_there(sprintf('   "%s"', what[[i]]))
        } else {
          write_there(sprintf('   "%s",', what[[i]]))
        }
      })
    }

    # handle missing js/ and css/ in file path (the CDN location
    # may be different from the local file location), only for
    # local option!
    if (options$local) {

      if (!is.null(scripts)) {
        # remove /dist for bootstrap
        scripts <- utils::tail(strsplit(scripts, "/")[[1]], n = 1)
        if (length(grep("js/", scripts)) == 0) scripts <- file.path("js", scripts)
      }
      if (!is.null(stylesheets)) {
        # remove /dist for bootstrap
        stylesheets <- utils::tail(strsplit(stylesheets, "/")[[1]], n = 1)
        if (length(grep("css/", stylesheets)) == 0) stylesheets <- file.path("css", stylesheets)
      }
    }

    # write in the file
    tag <- strsplit(utils::tail(strsplit(assets$url, "@")[[1]], n = 1), "/")[[1]][1]

    # Replace any - in name by _ since - is not handled in R function names
    func_name <- gsub("-", "_", name)

    # roxygen export tag
    write_there(sprintf("#' %s dependencies utils", name))
    write_there("#'")
    write_there(sprintf("#' @description This function attaches %s. dependencies to the given tag", name))
    write_there("#'")
    write_there("#' @param tag Element to attach the dependencies.")
    write_there("#'")
    write_there("#' @importFrom htmltools tagList htmlDependency")
    write_there("#' @export")
    # attach function
    write_there(sprintf("add_%s_deps <- function(tag) {", func_name))

    # htmlDependency content
    write_there(sprintf(" %s_deps <- htmlDependency(", func_name))
    write_there(sprintf('  name = "%s",', name))
    write_there(sprintf('  version = "%s",', tag))
    if (options$local) {
      write_there(sprintf('  src = c(file = "%s-%s"),', name, tag))
    } else {
      write_there(sprintf('  src = c(href = "%s"),', assets$url))
    }
    if (!is.null(scripts)) {
      if (length(scripts) > 1) {
        write_there("  script = c(")
        insert_multiple_lines(scripts)
        write_there("  ),")
      } else {
        write_there(sprintf('  script = "%s",', scripts))
      }
    }
    if (!is.null(stylesheets)) {
      if (length(stylesheets) > 1) {
        write_there("  stylesheet = c(")
        insert_multiple_lines(stylesheets)
        write_there("  ),")
      } else {
        write_there(sprintf('  stylesheet = "%s",', stylesheets))
      }
    }
    if (options$local) {
      write_there(sprintf('  package = "%s",', pkg))
    }
    # end deps
    write_there(" )")

    # attach deps
    write_there(sprintf(" tagList(tag, %s_deps)", func_name))
    # end function
    write_there("}")
    write_there("    ")

    if (open && rstudioapi::isAvailable()) rstudioapi::navigateToFile(path)

  } else {
    cli::cli_alert_danger("Unable to create the dependency")
  }
}





#' Imports Internal Dependencies
#'
#' Wrap internal scripts and stylesheets in one \link[htmltools]{htmlDependency}.
#'
#' @param name Package name.
#' @param version Package version.
#' @param entry_point Entry points to create dependency for.
#' @param open Whether to allow rstudioapi to open the newly created script. Default to TRUE.
#' @param mode Internal. Don't use.
#' @keywords Internal
create_custom_dependency <- function(name, version, entry_point, open = interactive(), mode) {

  # need to overwrite path which was used before
  path <- sprintf("R/%s-dependencies.R", name)
  file_create(path)

  # taken from golem ;)
  write_there <- function(...){
    write(..., file = path, append = TRUE)
  }

  if (length(entry_point) > 1) {
    # remove everything before last / and remove .js
    entry_point_names <- gsub(".*/|.js", "", entry_point)
  } else {
    entry_point_names <- name
  }

  # write in file for each entry point
  lapply(entry_point_names, function(dep) {

    stylesheet <- sprintf("%s%s.css", dep, mode)
    script <- sprintf("%s%s.js", dep, mode)

    # roxygen export
    write_there(sprintf("#' %s dependencies utils", dep))
    write_there("#'")
    write_there(sprintf("#' @description This function attaches %s dependencies to the given tag", dep))
    write_there("#'")
    write_there("#' @param tag Element to attach the dependencies.")
    write_there("#'")
    write_there("#' @importFrom utils packageVersion")
    write_there("#' @importFrom htmltools tagList htmlDependency")
    write_there("#' @export")
    # attach function
    write_there(sprintf("add_%s_deps <- function(tag) {", dep))

    # htmlDependency content
    write_there(sprintf(" %s_deps <- htmlDependency(", dep))
    write_there(sprintf('  name = "%s",', dep))
    write_there(sprintf('  version = "%s",', version))
    write_there(sprintf('  src = c(file = "%s-%s"),', name, version))
    write_there(sprintf('  script = "dist/%s",', script))
    write_there(sprintf('  stylesheet = "dist/%s",', stylesheet))
    write_there(sprintf('  package = "%s",', name))
    # end deps
    write_there(" )")

    # attach deps
    write_there(sprintf(" tagList(tag, %s_deps)", dep))
    # end function
    write_there("}")
    write_there("    ")
  })

  # path to dependency
  if (!file.exists(sprintf("R/%s-dependencies.R", name))) {
    ui_oops("Failed to create dependencies file!")
  } else {
    ui_done("Dependencies successfully updated!")
  }

  if (open && rstudioapi::isAvailable()) rstudioapi::navigateToFile(path)
}



#' Update the given dependency to a specific version or latest
#'
#' @param name Library name.
#' @param version_target Targeted version. Default to latest.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  update_dependency("framework7")
#' }
update_dependency <- function(name, version_target = "latest") {
  # get the current version
  current <- get_installed_dependency(name)
  versions <- get_dependency_versions(name)

  # Little trick: get_dependency_versions with latest = TRUE returns, the latest
  # stable dep, which is not necessarily the latest version. There might be alpha/beta
  # releases. We assume people will want to test those non official versions. Latest
  # is then defined as the first element of versions (versions contains all version
  # sorted by newer to older).
  latest <- versions[1]
  if (version_target == "latest") version_target <- latest


  # stop if current is version_target
  if (current == version_target) stop("Versions are identical")

  # get the latest version
  cli::cli_alert_info(
    sprintf(
      "current version: %s ||
      target version: %s ||
      latest version: %s",
      current,
      version_target,
      latest
    )
  )

  # if the version target is latest and current is latest, there is nothing to to
  # and we exit the function. TO DO: get_dependency_versions has low perfs!!
  current_rank <- match(current, versions)
  target_rank <- match(version_target, versions)

  if (current_rank < target_rank) {
    # downgrade
    cli::cli_alert_warning(sprintf("Downgrading %s to %s", name, version_target))
  } else{
    # current is outdated, let's upgrade
    cli::cli_alert_warning(sprintf("Upgrading %s to %s", name, version_target))
  }

  # delete all deps and install new ones
  fs::file_delete(paste0("inst/", find_dep_file(name)))
  create_dependency(name, tag = version_target)

}



#' Get the current version of the given dependency
#'
#' Used by \link{get_installed_dependency}.
#'
#' @param name Library name.
#' @noRd
#' @keywords internal
extract_dependency_version <- function(name) {
  temp_split <- strsplit(name, "-")[[1]]
  stringr::str_c(strsplit(name, "-")[[1]][2: length(temp_split)], collapse = "-")
}



#' Get the version of the current installed dependency
#'
#' Used by \link{update_dependency}.
#'
#' @param name Library name
#'
#' @return A character containing the version number
#' @export
get_installed_dependency <- function(name) {
  # TO DO: need to handle dependencies with CDN which don't have any folder.
  # A yaml config file would be good!
  file_name <- find_dep_file(name)
  if (purrr::is_empty(file_name)) stop("Dependency not found!")
  extract_dependency_version(file_name)
}



#' Find the folder corresponding to the given dependency
#'
#' Used by \link{get_installed_dependency} and \link{update_dependency}.
#'
#' @param name Library name.
#' @noRd
#' @keywords internal
find_dep_file <- function(name) {
  list.files(path = "inst/", pattern = sprintf("%s-", name))
}


#' Configure charpente
#'
#' @param local Whether to download files locally or to point to a CDN. Default to TRUE.
#'
#' @return A list of options.
#' @export
charpente_options <- function(local = TRUE) {
  list(local = local)
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
  cdn <- "https://data.jsdelivr.com/v1/package/npm/"
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
  cdn <- "https://data.jsdelivr.com/v1/package/npm/"
  if (tag == "latest") tag <- get_dependency_versions(dep, latest = TRUE)
  entrypoints <- jsonlite::fromJSON(sprintf("%s%s@%s/entrypoints", cdn, dep, tag))

  download_endpoint <- "https://cdn.jsdelivr.net/npm/"
  url <- sprintf("%s%s@%s", download_endpoint, dep, tag)

  list(url = url, files = entrypoints)
}
