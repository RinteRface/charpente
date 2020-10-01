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
#' @inheritParams golem::add_js_handler
#' @export
#' @rdname create_file
create_custom_handler <- purrr::partial(
  golem::add_js_handler,
  pkg = ".",
  dir = "inst"
)

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
