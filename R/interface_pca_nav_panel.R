#' Create the "PCA" nav panel
#'
#' @return [bslib::nav_panel()]
#' @keywords internal
#'
#' @author Seung J. Kim
.interfacePcaNavPanel <- function() {
    bslib::nav_panel(
        "PCA",
        value = "PCA",
        bslib::page_fillable(
            bslib::layout_sidebar(
                sidebar = accordion(
                    ## PCA customization panel must be OPEN for the inputs to
                    ## show up. Same goes for the actionButton...
                    open = c("runButton", "k", "Shapes and colours"),
                    bslib::accordion_panel(
                        "",
                        value = "runButton",
                        shiny::uiOutput("generatePcaButton")
                    ),
                    bslib::accordion_panel(
                        "Download",
                        htmltools::p("Main plots"),
                        shiny::fluidRow(
                            lapply(
                                c("png", "tiff", "pdf", "svg"),
                                function(ext) {
                                    shiny::column(
                                        6,
                                        shiny::downloadButton(
                                            paste0("pca_", ext),
                                            paste(toupper(ext))
                                        )
                                    )
                                }
                            )
                        ),
                        htmltools::p("Legends"),
                        shiny::fluidRow(
                            lapply(
                                c("png", "tiff", "pdf", "svg"),
                                function(ext) {
                                    shiny::column(
                                        6,
                                        shiny::downloadButton(
                                            paste0("pcaLegend_", ext),
                                            paste(toupper(ext))
                                        )
                                    )
                                }
                            )
                        )
                    ),
                    bslib::accordion_panel(
                        "",
                        value = "k",
                        shiny::uiOutput("selectYourK") %>%
                            shinycssloaders::withSpinner(
                                type = 4, size = 0.5, proxy.height = 75
                            )
                    ),
                    bslib::accordion_panel(
                        "Shapes and colours",
                        htmltools::div(
                            id = "PCAcustom", style = "display: inline-block;",
                            shiny::uiOutput("customization")
                        ),
                        htmltools::div(
                            id = "PCAcustomBatch",
                            style = "display: inline-block;",
                            shiny::uiOutput("customizationBatch")
                        )
                    )
                ),
                bslib::layout_column_wrap(
                    ## thank god this fixed UI elements from overlapping
                    ## each other
                    fill = FALSE, fillable = FALSE,
                    width = NULL, height = 900,
                    style = bslib::css(
                        grid_template_columns = "2fr 2fr 2fr 1fr"
                    ),
                    shiny::uiOutput("pcaPlotCpm",
                        style = "padding: 4px;"
                    ) %>% shinycssloaders::withSpinner(
                        type = 4, size = 0.5,
                        proxy.height = 50
                    ),
                    shiny::uiOutput("pcaPlotQ3",
                        style = "padding: 4px;"
                    ) %>% shinycssloaders::withSpinner(
                        type = 4, size = 0.5,
                        proxy.height = 50
                    ),
                    shiny::uiOutput("pcaPlotRuv",
                        style = "padding: 4px;"
                    ) %>% shinycssloaders::withSpinner(
                        type = 4, size = 0.5,
                        proxy.height = 50
                    ),
                    shiny::uiOutput("pcaPlotLegend",
                        style = "padding: 4px;"
                    ) %>% shinycssloaders::withSpinner(
                        type = 4, size = 0.5,
                        proxy.height = 50
                    ),
                    shiny::uiOutput("pcaPlotCpmBatch",
                        style = "padding: 4px;"
                    ) %>% shinycssloaders::withSpinner(
                        type = 4, size = 0.5,
                        proxy.height = 50
                    ),
                    shiny::uiOutput("pcaPlotQ3Batch",
                        style = "padding: 4px;"
                    ) %>% shinycssloaders::withSpinner(
                        type = 4, size = 0.5,
                        proxy.height = 50
                    ),
                    shiny::uiOutput("pcaPlotRuvBatch",
                        style = "padding: 4px;"
                    ) %>% shinycssloaders::withSpinner(
                        type = 4, size = 0.5,
                        proxy.height = 50
                    ),
                    shiny::uiOutput("pcaPlotLegendBatch",
                        style = "padding: 4px;"
                    ) %>% shinycssloaders::withSpinner(
                        type = 4, size = 0.5,
                        proxy.height = 50
                    )
                )
            )
        )
    )
}
