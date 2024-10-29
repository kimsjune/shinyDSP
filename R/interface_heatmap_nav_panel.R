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
                            shiny::selectInput("heatmapCol",
                                "Heatmap colour scheme",
                                choices = c(
                                    "viridis", "magma", "plasma",
                                    "inferno", "cividis", "mako",
                                    "rocker", "turbo"
                                ),
                                selected = "inferno",
                                selectize = TRUE,
                                multiple = FALSE
                            ),
                            shiny::sliderInput("heatmapRange",
                                "Z score range",
                                value = c(-2, 2), min = -5, max = 5
                            ),
                            shiny::radioButtons("heatmapSize",
                                "Plot display size",
                                choices = c(
                                    "small" = 9,
                                    "medium" = 12,
                                    "large" = 16
                                ),
                                selected = 9
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
