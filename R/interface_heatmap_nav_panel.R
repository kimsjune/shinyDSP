.interfaceHeatmapNavPanel <- function() {
    bslib::nav_panel(
        "Heatmap",
        value = "Heatmap",
        bslib::layout_sidebar(
            sidebar =
                bslib::accordion(
                    bslib::accordion_panel(
                        " ",
                        shiny::actionButton(
                            inputId = "generateHeatmap",
                            label = "Show/update"
                        )
                    ),
                    bslib::accordion_panel(
                        "Heatmap options",
                        htmltools::div(
                            shiny::numericInput(
                                inputId = "topNgenes",
                                "Number of genes to show", value = 50
                            ),
                            # numericInput(inputId = "heatmap_fc",
                            #              "log2 FC to test", value = 1),
                            shiny::textInput("heatmapCol", "Heatmap colour scheme\n (see appendix for options)",
                                value = "Inferno"
                            ),
                            shiny::sliderInput("heatmapRange",
                                "Z score range",
                                value = c(-2, 2), min = -5, max = 5
                            ),
                            shiny::radioButtons("heatmapSize",
                                "Plot display size",
                                choices = c(
                                    "x-small" = 9,
                                    "small" = 12,
                                    "medium" = 16,
                                    "large" = 20
                                ),
                                selected = 12
                            ),
                            shiny::numericInput(
                                inputId = "heatmapFontSize",
                                "Gene name size", value = 12
                            )
                        )
                    )
                ),
            shiny::uiOutput("heatmapUI") %>% withSpinner(type = 4)
        )
    )
}
