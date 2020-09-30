onAttach <- function(libname, pkgname) {
  # dont change !!! CDNs have different API structure
  options("DEFAULT_CDN" = "https://data.jsdelivr.com/v1/package/npm/")
}
