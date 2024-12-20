#' Helper function that exposes .png assets to the Shiny package
#'
#' @param libname libname
#' @param pkgname pkgname
#'
#' @return [shiny::addResourcePath()]
#' @keywords internal
.onAttach <- function(libname, pkgname) {
    shiny::addResourcePath("www", system.file("www", package = "shinyDSP"))
    # shiny::addResourcePath("favicon.ico", system.file("favicon.ico",
    #     package = "shinyDSP"
    # ))
}
