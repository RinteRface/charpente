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
  use_code_of_conduct()
  use_news_md(FALSE)

  # readme badges
  use_cran_badge()
  use_lifecycle_badge("experimental")

  # testthat
  use_testthat()
  use_test("dummy-test")

  # Copy charpente-utils
  fs::file_copy(
    system.file("utils/charpente-utils.R", package = "charpente"),
    sprintf("./R/%s-utils.R", pkg_name)
  )


  # Setup esbuild for JS code management
  npm::npm_install("esbuild", scope = "dev")
  dir.create("srcjs")
  file.create("./srcjs/main.js")

  # Add mocha for tests
  npm::npm_install("mocha", scope = "dev")
  dir.create("srcjs/test")
  file.create("srcjs/test/test_basic.js")
  writeLines(
    "describe('Basic test', () => {
      it('should not fail', (done) => {
        done();
      });
    });
    ",
    "srcjs/test/test_basic.js"
  )

  # Ignore files/folders: srcjs, node_modules, ...
  use_build_ignore("srcjs")
  use_build_ignore("node_modules")
  use_build_ignore("package.json")
  use_build_ignore("package-lock.json")

  # version control
  use_git()
  if (!is.null(remote)) {
    repo_status <- if (private) "private" else "public"
    #ui_info("Creating {ui_value(repo_status)} remote repository at {ui_value(remote)}")
    use_github(remote, private, protocol = "ssh", auth_token = github_token())
    use_github_action_check_full()
    use_github_action(url = "https://raw.githubusercontent.com/r-lib/actions/master/examples/pkgdown.yaml")
    use_github_actions_badge()
  }

  # only open the project at the end
  ui_warn("Opening new project in a new session ...")
  ui_todo("Don't forget to complete the red bullet steps (see above)!")
  rstudioapi::openProject(path, newSession = TRUE)

}
