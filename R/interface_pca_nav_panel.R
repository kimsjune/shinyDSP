.interfacePcaNavPanel <- function() {
    bslib::nav_panel(
        # "Viz.",
        # navset_card_tab(
        # nav_panel(
        "PCA",
        value = "PCA",
        bslib::page_fillable(
            bslib::layout_columns(
                fill = FALSE, # thank god this fixed UI elements from overlapping each other
                col_widths = c(
                    4, 4, 4,
                    4, 4, 4,
                    12,
                    12,
                    12,
                    12
                ),
                shiny::uiOutput("pcaPlotCpm",
                         style = "padding: 4px;"
                ) %>% shinycssloaders::withSpinner(type = 4, size = 0.5, proxy.height = 50),
                shiny::uiOutput("pcaPlotQ3",
                         style = "padding: 4px;"
                ) %>% shinycssloaders::withSpinner(type = 4, size = 0.5, proxy.height = 50),
                shiny::uiOutput("pcaPlotRuv",
                         style = "padding: 4px;"
                ) %>% shinycssloaders::withSpinner(type = 4, size = 0.5, proxy.height = 50),
                shiny::uiOutput("pcaPlotCpmBatch",
                         style = "padding: 4px;"
                ) %>% shinycssloaders::withSpinner(type = 4, size = 0.5, proxy.height = 50),
                shiny::uiOutput("pcaPlotQ3Batch",
                         style = "padding: 4px;"
                ) %>% shinycssloaders::withSpinner(type = 4, size = 0.5, proxy.height = 50),
                shiny::uiOutput("pcaPlotRuvBatch",
                         style = "padding: 4px;"
                ) %>% shinycssloaders::withSpinner(type = 4, size = 0.5, proxy.height = 50),
                shiny::downloadButton("downloadPCA", "Save PCA"),
                shiny::actionButton("togglePCAcustom", "Show/hide options",
                             style = "display: inline-block; padding: 4px;"
                ),

                htmltools::div(
                    id = "PCAcustom", style = "display: inline-block;",
                    shiny::uiOutput("customization")
                ),

                htmltools::div(
                    id = "PCAcustomBatch", style = "display: inline-block;",
                    shiny::uiOutput("customizationBatch")
                )
            )
        )
    )
}
