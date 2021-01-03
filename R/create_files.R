#' Create a shiny custom input binding boilerplate
#'
#' @inheritParams golem::add_js_input_binding
#' @export
#' @rdname create_file
create_input_binding <- purrr::partial(
  golem::add_js_input_binding,
  pkg = ".",
  dir = "srcjs"
)

#' Create a shiny output binding boilerplate
#'
#' @inheritParams golem::add_js_output_binding
#' @export
#' @rdname create_file
create_output_binding <- purrr::partial(
  golem::add_js_output_binding,
  pkg = ".",
  dir = "srcjs"
)

#' Create a shiny custom handler boilerplate
#'
#' Creates a script in inst and the R part in ./R
#'
#' @inheritParams golem::add_js_handler
#' @export
#' @rdname create_file
create_custom_handler <- function(
  name,
  pkg = ".",
  dir = "srcjs",
  open = TRUE,
  dir_create = TRUE
) {

  # create the JS part
  golem::add_js_handler(
    name,
    pkg,
    dir,
    open,
    dir_create,
    template = charpente::js_handler_template(
      path = sprintf(paste0(dir, "/%s.js"), name),
      name = name
    )
  )

  # create the R part
  path <- sprintf("R/%s-handler.R", name)
  file_create(path)

  write_there <- function(...){
    write(..., file = path, append = TRUE)
  }

  # TO DO: add dropNulls
  write_there(sprintf("send_%s_message <- function(id = NULL, options = NULL, session = shiny::getDefaultReactiveDomain()) {", name))
  write_there(" message <- list(")
  write_there("  # your logic")
  write_there(" )")
  write_there(" ")
  write_there(sprintf(" session$sendCustomMessage(type = \"%s\", message)", name))
  write_there("}")

  if (open && rstudioapi::isAvailable()) rstudioapi::navigateToFile(path)
}


#' Create a JavaScript file
#'
#' @inheritParams golem::add_module
#' @inheritParams golem::add_js_file
#' @export
#' @rdname create_file
create_js <- purrr::partial(
  golem::add_js_file,
  pkg = ".",
  dir = "srcjs"
)

#' Create a css file
#'
#' @inheritParams golem::add_css_file
#' @export
#' @rdname create_file
create_css <- partial(
  golem::add_css_file,
  pkg = ".",
  dir = "inst"
)




#' Compress and optimize all files in the current folder
#'
#' Generates a minified file under inst/pkg_name-pkg_version, if mode
#' is prod. If mode is dev, aggregates all js files without mangling
#' or compression.
#'
#' @param dir Default to srcjs.
#' @param files Use to specify custom order for files processing.
#' Default to NULL.
#' @param mode Production or development mode. Choose either "prod" or "dev".
#' @param source_maps Default to TRUE. Whether to include sourceMaps
#' for minified scripts. Default to TRUE.
#' @param ... Other options. See \url{https://github.com/terser/terser}.
#' @export
#' @importFrom utils tail packageVersion
build_js <- function(dir = "srcjs", files = NULL, mode = c("prod", "dev"),
                        source_maps = TRUE, ...) {

  mode <- match.arg(mode)

  # dev mode
  opts <- list(...)
  if (mode == "dev") opts$output$beautify <- TRUE
  if (mode == "dev" && is.null(opts$mangle)) opts$mangle <- FALSE
  if (mode == "dev" && is.null(opts$compress)) opts$compress <- FALSE

  customJS <- if (is.null(files)) {
    list.files(
      path = dir,
      recursive = TRUE,
      full.names = TRUE
    )
  } else {
    files
  }

  pkg_name <- get_pkg_name()
  pkg_version <- packageVersion(pkg_name)

  outputDir <- sprintf(
    "inst/%s-%s/js",
    pkg_name,
    pkg_version
  )

  dir.create(outputDir, recursive = TRUE)

  ui_warn(sprintf("%s folder created ...", outputDir))

  # Concat + Compress + source maps ----------------------------------------------------------------
  if (mode == "prod" && source_maps) {
    opts$sourceMap <- list(
      root = sprintf("../../%s-build", pkg_name),
      filename = sprintf("%s.min.js", pkg_name),
      url = sprintf("%s.min.js.map", pkg_name),
      includeSources = TRUE
    )
  }

  jstools::terser_file(
    input = customJS,
    # custom file name depends on mode type
    output = if (mode == "prod") {
      sprintf("%s/%s.min.js", outputDir, pkg_name)
    } else if (mode == "dev") {
      sprintf("%s/%s.js", outputDir, pkg_name)
    },
    options = do.call(jstools::terser_options, opts)
  )

  ui_done("JavaScript successfully compressed!")

  # create custom dependency
  file_mode <- if (mode == "dev") {
    ""
  } else if (mode == "prod") {
    ".min"
  }
  create_custom_dependency(pkg_name, script = customJS, open = FALSE, mode = file_mode)
  if (!file.exists(sprintf("R/%s-dependencies.R", pkg_name))) {
    ui_done("Dependency successfully created!")
  } else {
    ui_done("Dependency successfully updated!")
  }
}
