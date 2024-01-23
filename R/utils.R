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
    split_version = if (!is.null(version)) {
      paste(
        head(strsplit(pars$version, ".", fixed = TRUE)[[1]], -1),
        collapse = "."
      )
    },
    version = pars$version,
    entry_point = paste0(shQuote(pars$entry_point), collapse = ", "),
    entry_name = pars$entry_name,
    license = pars$license,
    .open = "<<",
    .close = ">>"
  )

  write_there <- function(...) {
    write(..., file = template, append = FALSE)
  }
  write_there(temp)
}



#' Add script.js to main.js entry point.
#'
#' Needed to keep the entry point up to date
#'
#' @param name Script name.
#' @keywords internal
reference_script <- function(name) {
  write(
    sprintf("import './%s.js';", name),
    file = "./srcjs/main.js",
    append = TRUE
  )
  ui_done("Script successfuly added to JS entry point!")
}

#' Insert import into main SCSS file
#'
#' Useful to reference scss files into the main one.
#'
#' @param path Could be file.name or folder/file.name
#'
#' @keywords internal
reference_style <- function(path) {
  write(
    sprintf("@import \"%s\";", path),
    file = "./styles/main.scss",
    append = TRUE
  )
  ui_done("Style file successfuly added to Sass entry point!")
}


#' Setup esbuild
#'
#' Installs esbuild for the local project
#'
#' @param light Used to only install Sass plugins. This is
#' to workaround a breaking change in charpente where styles does
#' not exist in old versions.
#'
#' @return Installs esbuild in node_modules (dev scope),
#' if not existing, creates srcjs + srcjs/main.js andstyles + styles/main.scss,
#' and sets relevant files and folders in .gitignore. and .Rbuildignore.
#' @export
set_esbuild <- function(light = FALSE) {

  pkg_desc <- desc::description$
    new("./DESCRIPTION")$
    get(c("Package", "Version", "License"))

  # Pull package.json
  process_template(
    "package.json",
    name = pkg_desc[1],
    version = pkg_desc[2], # node does not support 0.1.0.9000
    license = pkg_desc[3],
    entry_point = "main.js"
  )

  npm::npm_install(
    c(
      # Don't re-install esbuild. This will install
      # only missing Sass dependencies.
      if (!light) "esbuild",
      "esbuild-sass-plugin",
      "postcss",
      "autoprefixer"
    ),
    scope = "dev"
  )
  # If light, we don't want to recreate srcjs folder
  # which has always been in charpente since the first release.
  if (!light) {

    if (!dir.exists("srcjs")) {
      dir.create("srcjs")
    }

    write("import \"../styles/main.scss\";",
          "./srcjs/main.js",
          append = file.exists("srcjs/main.js"))

  }

  if (!dir.exists("styles")) {
    dir.create("styles")
  }

  if (!file.exists("styles/main.scss")) {
    file.create("styles/main.scss")
  }

  # Ignore files/folders: srcjs, node_modules, ...
  use_build_ignore(
    c(
      "srcjs",
      "node_modules",
      "package.json",
      "package-lock.json",
      "styles",
      "esbuild.dev.json",
      "esbuild.prod.json"
    )
  )

  use_git_ignore("node_modules")
}


#' Setup mocha
#'
#' Installs mocha for the local project
#'
#' @return Installs mocha in node_modules (dev scope), creates srcjs/test folder,
#' write basic test im test_basic.js
#' @export
set_mocha <- function() {
  npm::npm_install("mocha", scope = "dev")

  if (!dir.exists("srcjs/test")) {
    dir.create("srcjs/test")
  }

  if (!file.exists("srcjs/test/test_basic.js")) {
    file.create("srcjs/test/test_basic.js")
    writeLines(
      "describe('Basic test', () => {
      it('should not fail', (done) => {
        done();
      });
    });
    ",
      "srcjs/test/test_basic.js"
    )
  }
}




#' Setup version control
#'
#' Sets git and optionally link to github for the current project
#'
#' @inheritParams create_charpente
#' @keywords internal
set_version_control <- function(remote, private) {
  use_git()
  if (!is.null(remote)) {
    # handle new GH tokens
    if (nchar(gh::gh_token()) > 0) {
      repo_status <- if (private) "private" else "public"
      #ui_info("Creating {ui_value(repo_status)} remote repository at {ui_value(remote)}")
      use_github(remote, private, protocol = "ssh")
      use_github_action_check_standard()
      use_github_action(name = "pkgdown.yaml")
      use_github_actions_badge()
    } else {
      ui_warn("Please create a valid Github token (see usethis::gh_token_help()) and restart.")
      return()
    }
  }
}



#' Copy charpente utils function
#'
#' @inheritParams create_charpente
#' @return Copy all charpente utils function in the ./R/PKG_NAM-utils.R script
#' @keywords internal
copy_charpente_utils <- function(pkg_name) {
  fs::file_copy(
    system.file("utils/charpente-utils.R", package = "charpente"),
    sprintf("./R/%s-utils.R", pkg_name)
  )
}



#' Run esbuild
#'
#' Apply esbuild to the srcjs folder.
#'
#' @inheritParams build_js
#' @param outputDir Output directory
#' @param entry_point Entry points to be used
#' @keywords internal
run_esbuild <- function(mode, outputDir, entry_point) {
  # styles did not exist in previous {charpent} versions
  if (!dir.exists("styles")) {
    # Only add missing pieces ...
    set_esbuild(light = TRUE)
  }

  # Get pkg info
  pkg_desc <- desc::description$
    new("./DESCRIPTION")$
    get(c("Package", "Version"))

  # Recreate esbuild.prod/dev.js each time
  process_template(
    sprintf("esbuild.%s.js", mode),
    name = pkg_desc[[1]],
    entry_name = if (length(entry_point) == 1) pkg_desc[[1]] else "[name]",
    version = pkg_desc[[2]],
    entry_point = entry_point
  )

  npm::npm_run(sprintf("run build-%s", mode))
  ui_warn(sprintf("%s folder created ...", outputDir))
  ui_done("JavaScript successfully processed!")
}
