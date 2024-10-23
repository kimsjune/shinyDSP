#' Create the "Table" nav panel
#'
#' @return [bslib::nav_panel()]
#' @keywords internal
#'
#' @author Seung J. Kim
.interfaceTableNavPanel <- function(output) {
    bslib::nav_panel(
        "Table",
        value = "Table",
        shiny::uiOutput("topTable") %>% shinycssloaders::withSpinner(type = 4)
    )
}
