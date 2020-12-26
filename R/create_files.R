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
#' Generates a minified file under inst/pkg_name-pkg_version.
#'
#' @param dir Default to srcjs.
#' @param source_maps Default to TRUE.
#' @export
#' @importFrom utils tail packageVersion
compress_js <- function(dir = "srcjs", source_maps = TRUE) {
  customJS <- list.files(
    path = dir,
    recursive = TRUE,
    full.names = TRUE
  )

  pkg_name <- tail(strsplit(getwd(), "/")[[1]], 1)
  pkg_version <- packageVersion(pkg_name)

  outputDir <- sprintf(
    "inst/%s-%s",
    pkg_name,
    pkg_version
  )

  dir.create(outputDir)

  # Concat + Compress + source maps ----------------------------------------------------------------
  sourceMap <- NULL
  sourceMap <- if (source_maps) {
    list(
      root = sprintf("../../%s-build", pkg_name),
      filename = sprintf("%s.min.js", pkg_name),
      url = sprintf("%s.min.js.map", pkg_name),
      includeSources = TRUE
    )
  }

  jstools::terser_file(
    input = customJS,
    output = sprintf("%s/%s.min.js", outputDir, pkg_name),
    options = jstools::terser_options(sourceMap = sourceMap)
  )
}
