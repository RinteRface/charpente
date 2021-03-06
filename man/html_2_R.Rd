% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/html2R.R
\name{html_2_R}
\alias{html_2_R}
\title{Convert HTML content to R Shiny tags}
\usage{
html_2_R(html, path = "/html/body/*", prefix = TRUE)
}
\arguments{
\item{html}{HTML string}

\item{path}{Path where to extract elements. Default to body content.}

\item{prefix}{Whether to prefix elements by tag$...}
}
\value{
A list of R Shiny tags
}
\description{
Convert HTML content to R Shiny tags
}
\examples{
if (interactive()) {
 library(charpente)
 bs4_card <- '<div class="card" style="width: 18rem;">
   <img class="card-img-top" src="..." alt="Card image cap">
     <div class="card-body">
     <h5 class="card-title">Card title</h5>
     <p class="card-text">Some quick example text.</p>
       <a href="#" class="btn btn-primary">Go somewhere</a>
     </div>
   </div>'
 html_2_R(bs4_card)

 ## With non standard attributes
 tag <- "<div data-toggle='accordion'></div>"
 html_2_R(tag)
}
}
\author{
Alan Dipert, RStudio
}
