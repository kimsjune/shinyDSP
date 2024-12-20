% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ShinyDSP.R
\name{shinyDSP}
\alias{shinyDSP}
\title{Creates the shiny app, ready to be loaded}
\usage{
shinyDSP()
}
\value{
A \code{\link[shiny:shinyApp]{shiny::shinyApp()}} object
}
\description{
Creates the shiny app, ready to be loaded
}
\examples{
library(shinyDSP)
app <- shinyDSP()
if (interactive()) {
    shiny::runApp(app)
}
}
\author{
Seung J. Kim
}
