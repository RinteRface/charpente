#' Compress and optimize all files in the current folder
#'
#' Generates a minified file under inst/pkg_name-pkg_version, if mode
#' is prod. If mode is dev, aggregates all js files without mangling
#' or compression.
#'
#' @param dir Default to srcjs.
#' @param mode Production or development mode. Choose either "prod" or "dev".
#' "prod" bundles, aggregates and minifyies files. "dev" only bundles the code.
#' Modules follow the ES6 format (import/export).
#' @param entry_points Entry point(s) to use in esbuild configuration. In case of
#' a monolithic bundle, only one entrypoint is needed. This the default.
#' In case of component based bundles, a vector of entrypoints is needed.
#' The output files will match the entrypoints names.
#' @export
#' @importFrom utils tail packageVersion
build_js <- function(dir = "srcjs", mode = c("prod", "dev"), entry_points = "main.js") {

  mode <- match.arg(mode)
  pkg_desc <- desc::description$new("./DESCRIPTION")$get(c("Package", "Version", "License"))
  outputDir <- sprintf("inst/%s-%s/dist", pkg_desc[1], pkg_desc[2])

  # make sure entrypoints look like ./dir/file.js
  if (any(!grepl(sprintf("^\\./%s", dir), entry_points))) {
    entry_points <- paste0("./", dir, "/", entry_points)
  }

  # check if entry_points exists
  if (any(!file.exists(entry_points))) {
    missing_entry_points <- entry_points[!file.exists(entry_points)]
    ui_stop("The following entry points don't exist: {paste0(entry_points, collapse = ', ')}")
  }

  # run esbuild
  run_esbuild(mode, outputDir, entry_points)

  # create custom dependency
  create_custom_dependency(
    name = pkg_desc[1],
    version = pkg_desc[2],
    entry_points = entry_points,
    open = FALSE,
    mode = if (mode == "dev") "" else if (mode == "prod") ".min"
  )
}


#' Test JS code
#'
#' Test the entire srcjs/test folder
#'
#' @return A message showing the test results
#' @export
test_js <- function() {
  o <- npm::npm_run("run test")
  ui_done(o)
}
