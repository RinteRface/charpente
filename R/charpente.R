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
#'
#' @examples
#' \dontrun{
#'  create_charpente("bs4Dash")
#' }
create_charpente <- function(path, remote = NULL, private = FALSE, license) {

  # create package + project but don't open until all files are added
  create_package(
    path,
    fields = list(),
    rstudio = rstudioapi::isAvailable(),
    roxygen = TRUE,
    check_name = TRUE,
    open = FALSE
  )

  setwd(path)
  eval(parse(text = paste0("use_", license, "_license")))

  use_cran_comments()
  use_readme_md()

  use_cran_badge()
  use_lifecycle_badge("experimental")

  use_testthat()
  use_pkgdown()


  cli::cli_alert_info("Setting up git and github ...")
  use_git()
  if (!is.null(remote)) {
    use_github(remote, private, protocol = "ssh", auth_token = github_token())
    use_github_action_check_full()
    use_github_action(url = "https://raw.githubusercontent.com/r-lib/actions/master/examples/pkgdown.yaml")
    use_github_actions_badge()
  }
  cli::cli_alert_success("Done")


  use_code_of_conduct()
  use_news_md()

  # only open the project at the end
  rstudioapi::openProject(path, newSession = TRUE)

}
