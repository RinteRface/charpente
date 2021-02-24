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
#' @param entry_point Required internally to setup the esbuild config.
#' @export
#' @importFrom utils tail packageVersion
build_js <- function(dir = "srcjs", mode = c("prod", "dev"), entry_point = "main.js") {

  mode <- match.arg(mode)

  pkg_desc <- desc::description$new("./DESCRIPTION")$get(c("Package", "Version", "License"))

  outputDir <- sprintf(
    "inst/%s-%s/js",
    pkg_desc[1],
    pkg_desc[2]
  )

  # Configure package.json so that esbuild knows where to build the JS code
  process_template(
    "package.json",
    name = pkg_desc[1],
    version = pkg_desc[2], # node does not support 0.1.0.9000
    license = pkg_desc[3]
  )
  # run esbuild
  npm::npm_run(sprintf("run build-%s", mode))
  ui_warn(sprintf("%s folder created ...", outputDir))
  ui_done("JavaScript successfully processed!")

  # create custom dependency
  file_mode <- if (mode == "dev") {
    ""
  } else if (mode == "prod") {
    ".min"
  }

  create_custom_dependency(
    pkg_desc[1],
    pkg_desc[2],
    open = FALSE,
    mode = file_mode
  )

  if (!file.exists(sprintf("R/%s-dependencies.R", pkg_desc[1]))) {
    ui_done("Dependency successfully created!")
  } else {
    ui_done("Dependency successfully updated!")
  }
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
