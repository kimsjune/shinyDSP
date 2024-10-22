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
        bslib::layout_sidebar(
            sidebar = bslib::accordion(
                bslib::accordion_panel(
                    "",
                    shiny::actionButton(
                        inputId = "generateTable",
                        "Run"

                    )
                )

            ),
            shiny::uiOutput("topTable") %>% shinycssloaders::withSpinner(type = 4)
        )
    )
}
