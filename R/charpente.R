#' Create a package using charpente and usethis conventions
#'
#' A charpente powered package does not differ from other package structure.
#' \link{create_charpente} calls a series of function to help you quickly setting up the
#' package structure.
#'
#' @param path A path. If it exists, it is used. If it does not exist, it is created,
#' provided that the parent path exists.
#' @param remote Name of the remote Github organization to connect to.
#' @param private Whether the repository is private. Default to FALSE.
#' @param license Which license is your project under?
#'
#' @export
#' @import usethis
#' @importFrom utils tail
#'
#' @examples
#' \dontrun{
#'  create_charpente("bs4Dash")
#' }
create_charpente <- function(path, remote = NULL, private = FALSE, license) {

  pkg_name <- tail(strsplit(path, "/")[[1]], 1)
  # create package + project but don't open until all files are added
  create_package(
    path,
    # To add pkg imports, remotes, ...
    fields = list(
      Package = pkg_name,
      Imports = "shiny, htmltools, utils",
      Version = "0.0.0.9000"
    ),
    rstudio = rstudioapi::isAvailable(),
    roxygen = TRUE,
    check_name = TRUE,
    open = FALSE
  )

  ui_done("Package {ui_value(pkg_name)} successfuly created!")

  # set new wd
  setwd(path)

  # create inst + pkg_name subfolder (contain minified custom js, css ...)
  dir.create("inst")
  dir.create(sprintf("inst/%s-0.0.0.9000", pkg_name))

  # LICENSE, ...
  eval(parse(text = paste0("use_", license, "_license()")))
  use_cran_comments(FALSE)
  use_readme_md(FALSE)
  use_code_of_conduct(contact = "<YOUR_MAIL>")
  use_news_md(FALSE)

  # readme badges
  use_cran_badge()
  use_lifecycle_badge("experimental")

  # testthat
  use_testthat()
  use_test("dummy-test", open = FALSE)

  # Copy charpente-utils
  copy_charpente_utils(pkg_name)

  # Setup esbuild for JS code management
  set_esbuild()
  # Add mocha for tests
  set_mocha()

  # version control
  set_version_control(remote, private)

  # only open the project at the end
  ui_warn("Opening new project in a new session ...")
  ui_todo("Don't forget to complete the red bullet steps (see above)!")
  rstudioapi::openProject(path, newSession = TRUE)

}
