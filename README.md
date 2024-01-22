
# charpente <img src="https://rinterface.com/inst/images/charpente.png" width="200px" align="right"/>

<!-- badges: start -->
[![R-CMD-check](https://github.com/RinteRface/charpente/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RinteRface/charpente/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/charpente)](https://CRAN.R-project.org/package=charpente)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of `{charpente}` is to significantly reduce the complexity of creating new HTML templates for Shiny:

  - `{charpente}` creates a plug and play package structure.
  - `{charpente}` automatically import dependencies from [jsdelivr](https://www.jsdelivr.com/), so that you don't have to do it by hand! 
  - `{charpente}` eases the conversion from HTML to R.
  - `{charpente}` offers multiple R and JS boilerplate for `{shiny}` input bindings, `{shiny}` message handlers, ...
  - `{charpente}` enables seamless JavaScript code management (powered by [esbuild](https://esbuild.github.io/)): concat, compress, mangle, bundle, minify, ... for JS and
  Sass code. 


## Installation

You can install the development version of `{charpente}` from Github with:

``` r
# latest version
remotes::install_github("RinteRface/charpente")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(charpente)

path <- file.path(tempdir(), "mypkg")
create_charpente(path, license = "mit")

# Once the package is created and opened

# Look for all bulma flavors
get_dependency_versions("bulma")
# Get latest stable
get_dependency_versions("bulma", latest = TRUE)
# Inspect bulma
get_dependency_assets("bulma")

# Download bulma locally
create_dependency("bulma")
devtools::load_all()
# Test the newly added dependencies tools
findDependencies(add_bulma_deps(div()))

# Create JS handler
create_custom_handler("modal")

# Create input binding
create_input_binding("myinput")

# Create output binding
create_output_binding("myoutput")

# Compress JS and CSS (Sass) for production
build_js()
devtools::load_all()
```

## Using esbuild and mocha

If you want to use `esbuild` and `mocha` in an existing project, you can use the functions `set_esbuild()` and `set_mocha()`. A simple workflow looks as follows:

```r
# Setup esbuild for JS code management
set_esbuild()

# Add mocha for tests
set_mocha()
```

## Acknowledgment 
The author would like to warmly thank [Victor Perrier](https://twitter.com/_pvictorr?lang=fr), 
[John Coene](https://twitter.com/jdatap), [Colin Fay](https://twitter.com/_ColinFay), [Alan Dipert](https://twitter.com/alandipert), [Kenton Russel](https://twitter.com/timelyportfolio) for providing many building block and inspiration to this package. 

## Code of Conduct
  
  Please note that the `{charpente}` project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). 
  By contributing to this project, you agree to abide by its terms.
