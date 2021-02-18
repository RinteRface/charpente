#' Create a shiny custom input binding boilerplate
#'
#' @inheritParams golem::add_js_input_binding
#' @param add_reference Whether to add an import statement in main.js. Defaults to TRUE.
#' @export
#' @rdname create_file
create_input_binding <- function(name, pkg = ".", dir = "srcjs", open = TRUE,
                                 dir_create = TRUE, initialize = FALSE, dev = FALSE,
                                 events = list(name = "click", rate_policy = FALSE),
                                 add_reference = TRUE) {
  golem::add_js_input_binding(
    name,
    pkg,
    dir,
    open,
    dir_create = FALSE,
    initialize,
    dev,
    events
  )

  # Add entry to main.js
  if (add_reference) reference_script(name)
}

#' Create a shiny output binding boilerplate
#'
#' @inheritParams golem::add_js_output_binding
#' @param add_reference Whether to add an import statement in main.js. Defaults to TRUE.
#' @export
#' @rdname create_file
create_output_binding <- function(name, pkg = ".", dir = "srcjs", open = TRUE,
                                  dir_create = TRUE, add_reference = TRUE) {
  golem::add_js_output_binding(
    name,
    pkg,
    dir,
    open,
    dir_create = FALSE
  )

  # Add entry to main.js
  if (add_reference) reference_script(name)
}

#' Create a shiny custom handler boilerplate
#'
#' Creates a script in inst and the R part in ./R
#'
#' @inheritParams golem::add_js_handler
#' @param add_reference Whether to add an import statement in main.js. Defaults to TRUE.
#' @export
#' @rdname create_file
create_custom_handler <- function(
  name,
  pkg = ".",
  dir = "srcjs",
  open = TRUE,
  dir_create = TRUE,
  add_reference = TRUE
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

  # Add entry to main.js
  if (add_reference) reference_script(name)

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
#' @param add_reference Whether to add an import statement in main.js. Defaults to TRUE.
#' @export
#' @rdname create_file
create_js <- function(name, pkg = ".", dir = "srcjs", open = TRUE,
                      dir_create = TRUE, with_doc_ready = TRUE, template = golem::js_template,
                      ..., add_reference = TRUE) {
  # Create JS file
  golem::add_js_file(
    name,
    pkg,
    dir ,
    open,
    dir_create = FALSE,
    with_doc_ready,
    template,
    ...
  )

  # Add entry to main.js
  if (add_reference) reference_script(name)
}

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
#' "prod" bundles, aggregates and minifyies files. "dev" only bundles the code.
#' Modules follow the ES6 format (import/export).
#' @param entry_point Required internally to setup the esbuild config.
#' @export
#' @importFrom utils tail packageVersion
build_js <- function(dir = "srcjs", files = NULL, mode = c("prod", "dev"), entry_point = "main.js") {

  mode <- match.arg(mode)

  customJS <- if (is.null(files)) {
    list.files(
      path = dir,
      recursive = TRUE,
      full.names = TRUE
    )
  } else {
    files
  }

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
    script = customJS,
    open = FALSE,
    mode = file_mode
  )

  if (!file.exists(sprintf("R/%s-dependencies.R", pkg_desc[1]))) {
    ui_done("Dependency successfully created!")
  } else {
    ui_done("Dependency successfully updated!")
  }
}
