globalVariables(".", package = "charpente")

#' @importFrom XML xmlAttrs
#' @importFrom stringr str_detect
make_attrs <- function(node) {
  attrs <- xmlAttrs(node)
  names(attrs) %>%
    Map(function (name) {
      val <- attrs[[name]]
      if (str_detect(string = name, pattern = "-")) {
        name <- paste0("`", name, "`")
      }
      paste0(name, ' = ', if (val == "") "NA" else paste0('"', val, '"'))
    }, .)
}



Keep <- function(fun, xs) Map(fun, xs) %>% Filter(Negate(is.null), .)



#' @importFrom XML xmlName xmlValue xmlChildren
#' @importFrom stringr str_pad
#' @importFrom purrr partial
render_node <- function(node, indent = 0, prefix = FALSE) {
  if (xmlName(node) == "text") {
    txt <- xmlValue(node)
    if (nchar(trimws(txt)) > 0) {
      paste0('"', trimws(txt), '"')
    }
  } else {
    tagName <- if (prefix) paste0("tags$", xmlName(node)) else xmlName(node)
    newIndent <- indent + length(tagName) + 1
    xmlChildren(node) %>%
      Keep(partial(render_node, indent = newIndent, prefix = prefix), .) %>%
      append(make_attrs(node), .) %>%
      paste(collapse = str_pad(",\n", width = newIndent, side = c("right"))) %>%
      trimws(which = c("left")) %>%
      paste0(tagName, "(", ., ")")
  }
}


#' Convert HTML content to R Shiny tags
#'
#' @param html HTML string
#' @param prefix Whether to prefix elements by tag$...
#'
#' @return A list of R Shiny tags
#' @export
#' @author Alan Dipert, RStudio
#' @importFrom XML htmlParse getNodeSet
#' @import cli
#' @importFrom styler style_text
#' @examples
#' if (interactive()) {
#'  library(charpente)
#'  bs4_card <- '<div class="card" style="width: 18rem;">
#'    <img class="card-img-top" src="..." alt="Card image cap">
#'      <div class="card-body">
#'      <h5 class="card-title">Card title</h5>
#'      <p class="card-text">Some quick example text.</p>
#'        <a href="#" class="btn btn-primary">Go somewhere</a>
#'      </div>
#'    </div>'
#'  html_2_R(bs4_card)
#'
#'  ## With non standard attributes
#'  tag <- "<div data-toggle='accordion'></div>"
#'  html_2_R(tag)
#' }
html_2_R <- function(html, prefix = FALSE) {
  cli_h2("Converting code ...")
  r_output <- html %>%
    htmlParse %>%
    getNodeSet("/html/body/*") %>%
    `[[`(1) %>%
    render_node(prefix = prefix)

  if (exists("r_output")) {
    cli_alert_success("Code converted with success.")
    cli_alert_info("Copy and paste the following R code")
    cli_rule()
    cli_code(style_text(r_output))
  } else {
    cli_alert_danger("Failed to convert code.")
  }
}
