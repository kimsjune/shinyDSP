#' Create the "Table" nav panel
#'
#' @return [bslib::nav_panel()]
#' @keywords internal
#'
#' @author Seung J. Kim
.interfaceTableNavPanel <- function() {
    bslib::nav_panel(
        "Table",
        value = "Table",
        bslib::layout_sidebar(
            shiny::uiOutput("topTable") %>% shinycssloaders::withSpinner(type = 4)
        )
    )
}
