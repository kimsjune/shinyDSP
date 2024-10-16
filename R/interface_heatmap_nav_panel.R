.interfaceHeatmapNavPanel <- function() {
    bslib::nav_panel(
        "Heatmap",
        bslib::layout_sidebar(
            sidebar =
                bslib::accordion(
                    bslib::accordion_panel(
                        " ",
                        shiny::actionButton(
                            inputId = "generateHeatmap",
                            label = "Update"
                        )
                    ),
                    bslib::accordion_panel(
                        "Heatmap options",
                        div(

                            numericInput(inputId = "top_n_genes",
                                         "Number of genes to show", value = 50),
                            # numericInput(inputId = "heatmap_fc",
                            #              "log2 FC to test", value = 1),
                            textInput("heatmap_col", "Heatmap colour scheme\n (see appendix for options)",
                                      value = "Inferno"),

                            sliderInput("heatmap_range",
                                        "Z score range", value = c(-2,2), min=-5, max=5),
                            radioButtons("heatmap_size",
                                         "Plot display size",
                                         choices = c(
                                             "x-small" = 9,
                                             "small" = 12,
                                             "medium" = 16,
                                             "large" = 20),
                                         selected = 12),
                            numericInput(inputId = "heatmap_fontsize",
                                         "Gene name size", value = 12)



                        )
                    )
                ),
            uiOutput("heatmapUI") %>% withSpinner(type=4),
            downloadButton("downloadHeatmap")
        )
    )

}
