#' Create the "QC" nav panel
#'
#' @return [bslib::nav_panel()]
#' @keywords internal
#'
#' @author Seung J. Kim
.interfaceQcNavPanel <- function() {
    bslib::nav_panel(
        "QC",
        value = "QC",
        bslib::layout_sidebar(
            sidebar = bslib::accordion(
                ## critical that this is open
                open = c("options","Apply QC cutoffs"),
                bslib::accordion_panel(
                    "",
                    value = "options",
                    shiny::actionButton(inputId = "generateQc", "Show QC plots"),
                    shiny::uiOutput("qcSelect"),
                    shiny::numericInput(
                        inputId = "nQcPlots", "# of plots",
                        value = 1,
                        min = 1
                    ),
                    shiny::uiOutput("qcColBy"),
                    shiny::uiOutput("qcColPal")
                ),
                bslib::accordion_panel(
                    "Apply QC cutoffs",
                    shiny::uiOutput("qcCutoff")
                )
            ),
            shiny::uiOutput("qcPlot") %>%
                shinycssloaders::withSpinner(type = 4)
        )
    )
}
