#' Create the "Volcano" nav panel
#'
#' @return [bslib::nav_panel()]
#' @keywords internal
#'
#' @author Seung J. Kim
.interfaceVolcanoNavPanel <- function() {
    bslib::nav_panel(
        "Volcano",
        value = "Volcano",
        bslib::layout_sidebar(
            sidebar = accordion(
                bslib::accordion_panel(
                    " ",
                    shiny::actionButton(
                        inputId = "generateVolcano",
                        label = "Show/update"
                    )
                ),
                bslib::accordion_panel(
                    "Volcano options",
                    htmltools::div(
                        shiny::numericInput(
                            inputId = "maxOverlap",
                            "Density of gene names to show",
                            min = 0, max = 50, value = 6
                        ),
                        shiny::numericInput(
                            inputId = "delabSize",
                            "Gene label size", value = 6
                        ),
                        shiny::numericInput(
                            inputId = "PvalCutoff",
                            "P value cutoff",
                            value = 0.05,
                            min = 0
                        ),
                        shiny::numericInput(
                            inputId = "logFCcutoff",
                            "logFC cutoff",
                            value = 1
                        ),
                        shiny::textInput(
                            inputId = "UpCol",
                            "Up gene colour",
                            value = "red2"
                        ),
                        shiny::textInput(
                            inputId = "DnCol",
                            "Down gene colour",
                            value = "blue2"
                        ),
                        shiny::textInput(
                            inputId = "notDEcol",
                            "Not DE colour",
                            value = "grey75"
                        ),
                        shiny::checkboxInput(
                            inputId = "toggleCustomRange",
                            "Customize x & y range"
                        ),
                        style = "display: inline-block;"
                    ),
                    htmltools::div(
                        id = "showCustomRange", style = "display: inline-block;",
                        shiny::uiOutput("customRange")
                    )
                )
            ),
            shiny::uiOutput("volcanoUI") %>%
              shinycssloaders::withSpinner(type = 4)
        )
    )
}
