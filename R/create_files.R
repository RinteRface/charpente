#' Create a shiny custom input binding boilerplate
#'
#' @inheritParams golem::add_js_input_binding
#' @export
#' @rdname create_file
create_input_binding <- purrr::partial(
  golem::add_js_input_binding,
  pkg = ".",
  dir = "inst"
)

#' Create a shiny output binding boilerplate
#'
#' @inheritParams golem::add_js_output_binding
#' @export
#' @rdname create_file
create_output_binding <- purrr::partial(
  golem::add_js_output_binding,
  pkg = ".",
  dir = "inst"
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
  dir = "inst",
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
    template = charpente::js_handler_template(path = sprintf(paste0(dir, "/%s.js"), name))
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
  dir = "inst"
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
