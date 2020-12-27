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
  if(nrow(assets$files) == 0) stop(sprintf("No assets found for %s", name))


  # path to dependency
  path <- sprintf("inst/%s-%s", name, tag)

  # below we select step by step. It possible that a repo does not have bundle of minified
  # files. We inspect the content each time, if it's NULL, we change options.
  scripts <- select_asset(assets$files, "js", name, nested = assets$hasSubfolders, options)
  if (is.null(scripts)) {
    scripts <- select_asset(assets$files, "js", name, nested = assets$hasSubfolders, options = charpente_options(bundle = FALSE))
  }
  if (is.null(scripts)) {
    scripts <- select_asset(assets$files, "js", name, nested = assets$hasSubfolders, options = charpente_options(bundle = FALSE, minified = FALSE))
  }
  # CSS files are not in bundles
  stylesheets <- select_asset(assets$files, "css", name, nested = assets$hasSubfolders, options = charpente_options(bundle = FALSE))
  if (is.null(stylesheets)) {
    stylesheets <- select_asset(assets$files, "css", name, nested = assets$hasSubfolders, options = charpente_options(bundle = FALSE, minified = FALSE))
  }

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

    # write in the file
    tag <- strsplit(utils::tail(strsplit(assets$url, "@")[[1]], n = 1), "/")[[1]][1]
    # attach function
    write_there(sprintf("add_%s_deps <- function(tag) {", name))

    # htmlDependency content
    write_there(sprintf(" %s_deps <- htmltools::htmlDependency(", name))
    write_there(sprintf('  name = "%s",', name))
    write_there(sprintf('  version = "%s",', tag))
    if (options$local) {
      write_there(sprintf('  src = c(file = "%s-%s"),', name, tag))
    } else {
      write_there(sprintf('  src = c(href = "%s"),', assets$url))
    }
    if (!is.null(scripts)) {
      write_there("  script = c(")
      insert_multiple_lines(scripts)
      write_there("  ),")
    }
    if (!is.null(stylesheets)) {
      write_there("  stylesheet = c(")
      insert_multiple_lines(stylesheets)
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

    if (open && rstudioapi::isAvailable()) rstudioapi::navigateToFile(path)

  } else {
    cli::cli_alert_danger("Unable to create the dependency")
  }
}





#' Imports Internal Dependencies
#'
#' Wrap internal scripts and stylesheets in one \link[htmltools]{htmlDependency}.
#'
#' @param name Name of library.
#' @param script List of scripts to include. Assumes scripts are located at the root of the inst folder.
#' @param stylesheet List of stylesheets to include. Assumes stylesheets are located at the root of the inst folder.
#' @param open Whether to allow rstudioapi to open the newly created script. Default to TRUE.
#' @export
#'
#' @examples
#' \dontrun{
#'  create_custom_dependency("custom", script = c("hello.js", "blabla.js"))
#' }
create_custom_dependency <- function(name, script = NULL, stylesheet = NULL,
                                     open = interactive()) {
  # checks
  if (missing(name)) stop("Missing `name`")

  pkg <- get_pkg_name()
  pkg_version <- utils::packageVersion(pkg)

  # TO DO: find a way to better handle CSS (like JS)
  if (!is.null(stylesheet)) {
    dir_path <- paste0("inst/", name, "-", pkg_version, "/css")
    fs::dir_create(dir_path)
    old_path <- system.file(paste0("inst/", stylesheet), package = pkg)
    new_path <- system.file(dir_path, package = pkg)
    fs::file_move(old_path, new_path)
  }


  if (!is.null(script) || !is.null(stylesheet)) {

    # need to overwrite path which was used before
    path <- sprintf("R/%s-dependencies.R", name)
    file_create(path)

    # taken from golem ;)
    write_there <- function(...){
      write(..., file = path, append = TRUE)
    }

    # if we have multiple scripts/stylesheets
    insert_multiple_lines <- function(what) {
      lapply(seq_along(what), function (i) {
        if (i == length(what)) {
          write_there(sprintf('   "%s"', what[[i]]))
        } else {
          write_there(sprintf('   "%s",', what[[i]]))
        }
      })
    }


    # attach function
    write_there(sprintf("add_%s_deps <- function(tag) {", name))

    # htmlDependency content
    write_there(sprintf(" %s_deps <- htmltools::htmlDependency(", name))
    write_there(sprintf('  name = "%s",', name))
    write_there(sprintf('  version = utils::packageVersion("%s"),', pkg))
    write_there(sprintf('  src = c(file = "%s-%s"),', name, pkg_version))
    if (!is.null(script)) {
      script <- sprintf("js/%s.min.js", name)
      write_there(sprintf('  script = "%s",', script))
    }
    if (!is.null(stylesheet)) {
      stylesheet <- paste0("css/", stylesheet)
      write_there("  stylesheet = c(")
      insert_multiple_lines(stylesheet)
      write_there("  ),")
    }
    write_there(sprintf('  package = "%s",', pkg))
    # end deps
    write_there(" )")

    # attach deps
    write_there(sprintf(" htmltools::tagList(tag, %s_deps)", name))
    # end function
    write_there("}")
    write_there("    ")
  }


  # path to dependency
  if (open && rstudioapi::isAvailable()) rstudioapi::navigateToFile(path)
}




#' Attach all created dependencies in the ./R directory to the tag
#'
#' This function only works if there are existing dependencies. Otherwise,
#' an error is raised.
#'
#' @param tag Tag to attach the dependencies
#' @param deps Dependencies to add. Expect a vector of names. If NULL, all dependencies
#' are added.
#' @export
#'
#' @examples
#' \dontrun{
#'  library(htmltools)
#'  findDependencies(add_dependencies(div()))
#'  findDependencies(add_dependencies(div(), deps = "bulma"))
#' }
add_dependencies <- function(tag, deps = NULL) {
  if (is.null(deps)) {
    temp_names <- list.files("./R", pattern = "dependencies.R$")
    deps <- unlist(lapply(temp_names, stringr::str_split_n, pattern = "-", n = 1))
  }

  if (length(deps) == 0) stop("No dependencies found.")

  deps <- lapply(deps, function(x) {
    temp <- eval(
      parse(
        text = sprintf("htmltools::findDependencies(add_%s_deps(htmltools::div()))", x)
      )
    )
    # this assumes all add_*_deps function only add 1 dependency
    temp[[1]]
  })

  htmltools::tagList(tag, deps)
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
#' @param minified Whether to download minified files. Default to TRUE. Set to FALSE should you need
#' all files.
#' @param bundle Some libraries like Bootstrap provide bundles containing all components.
#' If bundle is TRUE, the download will target only those files. Not compatible with lite.
#' @param lite Some libraries like framework7 propose lite version of the scripts.
#' Default to FALSE. Not compatible with bundle.
#'
#' @return A list of options.
#' @export
charpente_options <- function(local = TRUE, minified = TRUE, bundle = TRUE, lite = FALSE) {
  if (bundle && lite) stop("Cannot choose bundle and lite options!")
  list(
    local = local,
    minified = minified,
    bundle = bundle
  )
}



#' Select a specific type of asset
#'
#' @param assets Dataframe of assets.
#' @param type Type to extract. Could be js, css, ...
#' @param name Library name.
#' @param nested Whether assets are in a subfolder.
#' @param options See \link{charpente_options}.
#'
#' @return A vector
#' @noRd
#' @keywords internal
select_asset <- function(assets, type, name, nested, options) {
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
  if (nested) {
    paste(type, selected_assets, sep = "/")
  } else {
    paste(selected_assets, sep = "/")
  }

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
  if (tag == "latest") tag <- get_dependency_versions(dep, latest = TRUE)
  temp <- jsonlite::fromJSON(sprintf("%s%s@%s", cdn, dep, tag))$files

  download_endpoint <- "https://cdn.jsdelivr.net/npm/"
  url <- sprintf("%s%s@%s/", download_endpoint, dep, tag)

  # bootstrap has a dist folder
  if ("dist" %in% temp$name) {
    temp <- temp[temp$name %in% c("dist"), "files"][[1]]
    hasSubfolders <- inherits(temp$files, "list")
    if (inherits(temp$files, "list")) temp <- do.call(rbind, temp$files)
    list(url = paste0(url, "dist/"), files = temp[, c("name", "hash")], hasSubfolders = hasSubfolders)
  } else {
    temp <- temp[temp$name %in% c("css", "js"), "files"][[1]]
    if (inherits(temp$files, "list")) temp <- do.call(rbind, temp)
    list(url = url, files = temp[, c("name", "hash")], hasSubfolders = TRUE)
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




