% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jstools.R
\name{build_js}
\alias{build_js}
\title{Compress and optimize all files in the current folder}
\usage{
build_js(dir = "srcjs", mode = c("prod", "dev"), entry_points = "main.js")
}
\arguments{
\item{dir}{Default to srcjs.}

\item{mode}{Production or development mode. Choose either "prod" or "dev".
"prod" bundles, aggregates and minifyies files. "dev" only bundles the code.
Modules follow the ES6 format (import/export).}

\item{entry_points}{Entry point(s) to use in esbuild configuration. In case of
a monolithic bundle, only one entrypoint is needed. This the default.
In case of component based bundles, a vector of entrypoints is needed.
The output files will match the entrypoints names.}
}
\description{
Generates a minified file under inst/pkg_name-pkg_version, if mode
is prod. If mode is dev, aggregates all js files without mangling
or compression.
}
