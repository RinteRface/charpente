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
