#' Utils to set up a PWA compatible structure
#'
#' Creates a web manifest, service-worker.js, icons and set the
#' necessary dependencies. The app must be part of a package.
#' Must not be used from the package root but from the app root.
#'
#' @inheritParams create_manifest
#' @param register_service_worker Whether to register the service worker. Default to
#' TRUE. Don't change the file name of service-worker.js!!!
#' @param create_dependencies Default to TRUE. Relevant if used in a shinyMobile context.
#' If used outside, you must set it to FALSE and handle the dependencies
#' yourself.
#' @export
set_pwa <- function(path, name = "My Progressive Web App", shortName = "My App",
                    description = "What it does!", lang = "en-US",
                    startUrl = "/", display = c("standalone", "minimal-ui", "fullscreen", "browser"),
                    background_color = "#ffffff", theme_color = "#ffffff",
                    register_service_worker = TRUE, create_dependencies = TRUE) {

  # Create the manifest
  create_manifest(
    path = path,
    name = name,
    description = description,
    lang = lang,
    startUrl = startUrl,
    display = display,
    background_color = background_color,
    theme_color = theme_color
  )

  if (create_dependencies) {
    # download the Google pwacompat script and make it a dependency
    create_dependency("pwacompat", options = charpente_options(bundle = FALSE))

    # Link manifest + icons + ...
    create_pwa_dependency()
    # Provide user prescription (update add_dependencies deps)
    ui_todo("Don't forget to update the dependencies in add_dependencies!")
  }

  # copy service worker + offline templates + icons
  fs::file_copy(
    system.file("pwa-utils/js/service-worker.js", package = "charpente"),
    paste0(path, "/www"),
    overwrite = TRUE
  )
  fs::file_copy(
    system.file("pwa-utils/html/offline.html", package = "charpente"),
    paste0(path, "/www"),
    overwrite = TRUE
  )
  fs::dir_copy(
    system.file("pwa-utils/icons", package = "charpente"),
    paste0(path, "/www/icons"),
    overwrite = TRUE
  )
  ui_done("pwa-utils successfully copied to /www!")

  # Register service worker: this is custom user code
  # so must go under srcjs/
  if (register_service_worker) {
    create_js("sw-register")
    js_code <- "
      window.addEventListener('load', () => {
        if ('serviceWorker' in navigator) {
          var pathname = window.location.pathname;
          navigator.serviceWorker
            .register(pathname + 'service-worker.js', { scope: pathname})
            .then(function() { console.log('Service Worker Registered'); });
        };
      });
    "
    write(js_code, "srcjs/sw-register.js")
    reference_script("sw-register")
  }
}



#' Create a PWA dependency
#'
#' List all relevant pwa resources in a custom dependency. Includes
#' the web manifest + icons using the head parameter of htmltools::htmlDependency.
#'
#' @param open Whether to allow rstudioapi to open the newly created script. Default to TRUE.
#'
#' @export
create_pwa_dependency <- function(open = interactive()) {
  pkg <- get_pkg_name()
  pkg_version <- utils::packageVersion(pkg)

  # need to overwrite path which was used before
  path <- sprintf("R/pwa-dependencies.R")
  file_create(path)

  # taken from golem ;)
  write_there <- function(...){
    write(..., file = path, append = TRUE)
  }

  pwa_tags <- paste(
    "<link rel=\\\"manifest\\\" href=\\\"manifest.webmanifest\\\"  />",
    "<link rel=\\\"icon\\\" type=\\\"image/png\\\" href=\\\"icons/icon-144.png\\\" sizes=\\\"144x144\\\" />",
    sep = "\n"
  )

  # roxygen export
  write_there("#' PWA dependencies utils")
  write_there("#'")
  write_there("#' @description This function attaches PWA manifest and icons to the given tag")
  write_there("#'")
  write_there("#' @param tag Element to attach the dependencies.")
  write_there("#'")
  write_there("#' @importFrom utils packageVersion")
  write_there("#' @importFrom htmltools tagList htmlDependency")
  write_there("#' @export")
  # attach function
  write_there("add_pwa_deps <- function(tag) {")

  # htmlDependency content
  write_there(" pwa_deps <- htmlDependency(")
  write_there('  name = "pwa-utils",')
  write_there(sprintf('  version = packageVersion("%s"),', pkg))
  write_there(sprintf('  src = c(file = "%s-%s"),', pkg, pkg_version))
  write_there(sprintf('  head = "%s",', pwa_tags))
  write_there(sprintf('  package = "%s",', pkg))
  # end deps
  write_there(" )")

  # attach deps
  write_there(" tagList(tag, pwa_deps)")
  # end function
  write_there("}")
  write_there("    ")

  if (open && rstudioapi::isAvailable()) rstudioapi::navigateToFile(path)
}


#' Create a manifest for your shiny app
#'
#' This is a central piece if you want to have your app standalone for instance
#'
#' @param path App path.
#' @param name App name.
#' @param shortName App short name.
#' @param description App description
#' @param lang App language (en-US by default).
#' @param startUrl Page to open at start.
#' @param display Display mode. Choose among \code{c("minimal-ui", "standalone", "fullscreen", "browser")}.
#' In practice, you want the standalone mode so that the app looks like a native app.
#' @param background_color The background_color property is used on the splash screen when the application is first launched.
#' @param theme_color The theme_color sets the color of the tool bar, and may be reflected in the app's preview in task switchers.
#'
#' @return This function creates a www folder for your shiny app. Must specify the path.
#' It creates 1 folders to contain icons and the manifest.json file.
#'
#' @note See \url{https://developer.mozilla.org/en-US/docs/Web/Manifest} for more informations.
#' @export
#'
#' @examples
#' create_manifest(
#'   path = tempdir(),
#'   name = "My App",
#'   shortName = "My App",
#'   description = "What it does!",
#'   lang = "en-US",
#'   startUrl = "https://www.google.com/",
#'   display = "standalone",
#'   background_color = "#3367D6",
#'   theme_color = "#3367D6"
#' )
create_manifest <- function(path, name = "My Progressive Web App", shortName = "My App",
                            description = "What it does!", lang = "en-US",
                            startUrl = "/", display = c("standalone", "minimal-ui", "fullscreen", "browser"),
                            background_color = "#ffffff", theme_color = "#ffffff") {

  display <- match.arg(display)

  manifest <- list(
    name = name,
    short_name = shortName,
    description = description,
    lang = lang,
    start_url = startUrl,
    display = display,
    background_color = background_color,
    theme_color = theme_color,
    icons = list( # array
      # icon 1
      list(
        src = "icons/icon-144.png",
        sizes = "144x144"
      )
    ),
    shortcuts = list( # array
      # shortcut 1
      list(
        name = "Shortcut",
        short_name = "Shortcut",
        description =  "Do something",
        url =  "https://dgranjon.shinyapps.io/shinyMobileGolemTest/?foo=1",
        icons = list(
          list(
            src = "icons/shortcut.png",
            sizes = "192x192"
          )
        )
      )
    )
  )

  # create /www folder if does not exist yet
  if (!dir.exists(paste0(path, "/www"))) {
    dir.create(paste0(path, "/www/icons"), recursive = TRUE)
  }
  jsonlite::write_json(
    manifest,
    path = paste0(path, "/www/manifest.webmanifest"),
    pretty = TRUE,
    auto_unbox = TRUE
  )
  ui_done("Web manifest successfully created!")
}
