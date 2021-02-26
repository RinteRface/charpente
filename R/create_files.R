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
