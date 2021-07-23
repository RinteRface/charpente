#' Attach all created dependencies in the ./R directory to the provided tag
#'
#' This function only works if there are existing dependencies. Otherwise,
#' an error is raised.
#'
#' @param tag Tag to attach the dependencies.
#' @param deps Dependencies to add. Expect a vector of names.
#' If NULL, all dependencies are added.
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
    deps <- unlist(lapply(temp_names, strsplit, split = "-dependencies.R"))
  }

  if (length(deps) == 0) stop("No dependencies found.")

  deps <- lapply(deps, function(x) {
    dep <- eval(
      parse(
        text = sprintf("htmltools::findDependencies(add_%s_deps(htmltools::div()))", x)
      )
    )
    dep
  })

  # flatten list by one level
  deps <- unlist(deps, recursive = FALSE)

  htmltools::tagList(tag, deps)
}


# Remove NULL list elements
dropNulls <- function (x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

# Validate Bootstrap compatible element width parameter
validate_width <- function(width) {
  if (is.numeric(width)) {
    if (width < 1 || width > 12) {
      stop("width must belong to [1, 12], as per Bootstrap 4 grid documentation. See more at https://getbootstrap.com/docs/4.0/layout/grid/")
    }
  } else {
    stop("width must be numeric")
  }
}




# Return TRUE if a shiny.tag object has a CSS class, FALSE otherwise.
hasCssClass <- function(tag, class) {
  if (is.null(tag$attribs) || is.null(tag$attribs$class))
    return(FALSE)

  classes <- strsplit(tag$attribs$class, " +")[[1]]
  return(class %in% classes)
}


#' Check that a tag has specific properties
#'
#' Check for type and class. Raise an error if the conditions
#' are not fulfilled. This function is borrowed from shinydashboard.
#'
#' @param tag Tag to check.
#' @param type Expected type.
#' @param class Expected class.
#' @param allowUI ?
#'
#' @return An error if conditions are not met
#' @export
#'
#' @examples
#' \dontrun{
#'  library(shiny)
#'  myTag <- div(class = "bg-blue")
#'  tagAssert(myTag, type = "div")
#'  tagAssert(myTag, type = "li") # will fail
#'  tagAssert(myTag, class = "bg-blue")
#' }
tagAssert <- function(tag, type = NULL, class = NULL, allowUI = TRUE) {
  if (!inherits(tag, "shiny.tag")) {
    print(tag)
    stop("Expected an object with class 'shiny.tag'.")
  }

  # Skip dynamic output elements
  if (allowUI &&
      (hasCssClass(tag, "shiny-html-output") ||
       hasCssClass(tag, "shinydashboard-menu-output"))) {
    return()
  }

  if (!is.null(type) && tag$name != type) {
    stop("Expected tag to be of type ", type)
  }

  if (!is.null(class)) {
    if (is.null(tag$attribs$class)) {
      stop("Expected tag to have class '", class, "'")

    } else {
      tagClasses <- strsplit(tag$attribs$class, " ")[[1]]
      if (!(class %in% tagClasses)) {
        stop("Expected tag to have class '", class, "'")
      }
    }
  }
}




# This is like a==b, except that if a or b is NULL or an empty vector, it won't
# return logical(0). If a AND b are NULL/length-0, this will return TRUE; if
# just one of them is NULL/length-0, this will FALSE. This is for use in
# conditionals where `if(logical(0))` would cause an error. Similar to using
# identical(a,b), but less stringent about types: `equals(1, 1L)` is TRUE, but
# `identical(1, 1L)` is FALSE.
equals <- function(a, b) {
  alen <- length(a)
  blen <- length(b)
  if (alen==0 && blen==0) {
    return(TRUE)
  }
  if (alen > 1 || blen > 1) {
    stop("Can only compare objects of length 0 or 1")
  }
  if (alen==0 || blen==0) {
    return(FALSE)
  }

  a == b
}



#' Check that tag has specific properties
#'
#' Return TRUE if a tag object matches a specific id, and/or tag name, and/or
#  class, and or other arbitrary tag attributes. This function is borrowed from shinydashboard.
#'
#' @param item Tag to validate.
#' @param ... Any attribute to check (must be named).
#' @param id Expected id.
#' @param name Expected name.
#' @param class Expected class.
#'
#' @return TRUE or FALSE, depending on the test result.
#' @export
#'
#' @examples
#' \dontrun{
#'  library(shiny)
#'  myTag <- div(class = "bg-blue")
#'  tagMatches(myTag, id = "d")
#'  tagMatches(myTag, class = "bg-blue")
#' }
tagMatches <- function(item, ..., id = NULL, name = NULL, class = NULL) {
  dots <- list(...)
  if (!inherits(item, "shiny.tag")) {
    return(FALSE)
  }
  if (!is.null(id) && !equals(item$attribs$id, id)) {
    return(FALSE)
  }
  if (!is.null(name) && !equals(item$name, name)) {
    return(FALSE)
  }
  if (!is.null(class)) {
    if (is.null(item$attribs$class)) {
      return(FALSE)
    }
    classes <- strsplit(item$attribs$class, " ")[[1]]
    if (! class %in% classes) {
      return(FALSE)
    }
  }

  for (i in seq_along(dots)) {
    arg     <- dots[[i]]
    argName <- names(dots)[[i]]
    if (!equals(item$attribs[[argName]], arg)) {
      return(FALSE)
    }
  }

  TRUE
}



#' Find the child of the targeted element that has a specific attribute and value.
#'
#' This function takes a DOM element/tag object and reccurs within it until
#  it finds a child which has an attribute called `attr` and with value `val`
#  (and returns TRUE). If it finds an element with an attribute called `attr`
#  whose value is NOT `val`, it returns FALSE. If it exhausts all children
#  and it doesn't find an element with an attribute called `attr`, it also
#  returns FALSE. This function is borrowed from shinydashboard.
#'
#' @param x Parent tag.
#' @param attr Atttribute like id, class, data-toggle, ...
#' @param val Attribute value.
#'
#' @return TRUE or FALSE, depending on the search result
#' @export
findAttribute <- function(x, attr, val) {
  if (is.atomic(x)) return(FALSE) # exhausted this branch of the tree

  if (!is.null(x$attribs[[attr]])) { # found attribute called `attr`
    if (identical(x$attribs[[attr]], val)) return(TRUE)
    else return(FALSE)
  }

  if (length(x$children) > 0) { # recursion
    return(any(unlist(lapply(x$children, findAttribute, attr, val))))
  }

  return(FALSE) # found no attribute called `attr`
}
