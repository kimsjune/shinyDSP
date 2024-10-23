.outputPcaNavPanel <- function(input, output, session, rv) {
    # nocov start
    # Opens a grouped widgets to pick shape and colour
    output$customization <- shiny::renderUI({
        shiny::req(input$selectedTypes)
        rv$pcaCustomization()
    })
    # nocov end

    # nocov start
    # Opens a grouped widgets to pick shape and colour
    output$customizationBatch <- shiny::renderUI({
        shiny::req(input$selectedTypes)
        rv$pcaCustomizationBatch()
    })
    # nocov end

    # nocov start
    output$pcaPlotCpm <- shiny::renderUI({
        shiny::validate(
            shiny::need(
                shiny::isTruthy(input$generatePCA) &
                  shiny::isTruthy(input$selectedTypes),
                "Hit 'run' to generate PCA plots."
            )
        )
        shiny::renderPlot(rv$pcaPlotCpm() +
            ggplot2::theme(legend.position = "none"))
    })
    # nocov end

    # nocov start
    output$pcaPlotQ3 <- shiny::renderUI({
        shiny::req(input$generatePCA, input$selectedTypes)
        shiny::renderPlot(rv$pcaPlotQ3() +
            ggplot2::theme(legend.position = "none"))
    })
    # nocov end

    # nocov start
    output$pcaPlotRuv <- shiny::renderUI({
        shiny::req(input$generatePCA, input$selectedTypes)
        shiny::renderPlot(rv$pcaPlotRuv() +
            ggplot2::theme(legend.position = "none"))
    })
    # nocov end

    # nocov start
    output$pcaPlotCpmBatch <- shiny::renderUI({
        shiny::req(input$generatePCA, input$selectedTypes)
        shiny::renderPlot(rv$pcaPlotCpmBatch() +
            ggplot2::theme(legend.position = "none"))
    })
    # nocov end

    # nocov start
    output$pcaPlotQ3Batch <- shiny::renderUI({
        shiny::req(input$generatePCA, input$selectedTypes)
        shiny::renderPlot(rv$pcaPlotQ3Batch() +
            ggplot2::theme(legend.position = "none"))
    })
    # nocov end

    # nocov start
    output$pcaPlotRuvBatch <- shiny::renderUI({
        shiny::req(input$generatePCA, input$selectedTypes)
        shiny::renderPlot(rv$pcaPlotRuvBatch() +
            ggplot2::theme(legend.position = "none"))
    })
    # nocov end


    # nocov start
    output$pcaPlotLegend <- shiny::renderUI({
        shiny::req(input$generatePCA, input$selectedTypes)
        legend <- ggpubr::as_ggplot(
            ggpubr::get_legend(
                rv$pcaPlotCpm()
            )
        )

        shiny::renderPlot(legend)
    })
    # nocov end

    # nocov start
    output$pcaPlotLegendBatch <- shiny::renderUI({
        shiny::req(input$generatePCA, input$selectedTypes)
        legend <- ggpubr::as_ggplot(
            ggpubr::get_legend(
                rv$pcaPlotCpmBatch()
            )
        )
        shiny::renderPlot(legend)
    })
    # nocov end

    # nocov start
    shiny::observe({
        lapply(c("png", "tiff", "pdf", "svg"), function(ext) {
            output[[paste0("pca_", ext)]] <- shiny::downloadHandler(
                filename = function() {
                    paste("plot", ext, sep = ".")
                },
                content = function(file) {
                    ggplot2::ggsave(file,
                        plot = cowplot::plot_grid(

                            rv$pcaPlotCpm() +
                              ggplot2::theme(legend.position = "none"),
                            rv$pcaPlotQ3() +
                              ggplot2::theme(legend.position = "none"),
                            rv$pcaPlotRuv() +
                              ggplot2::theme(legend.position = "none"),
                            rv$pcaPlotCpmBatch() +
                              ggplot2::theme(legend.position = "none"),
                            rv$pcaPlotQ3Batch() +
                              ggplot2::theme(legend.position = "none"),
                            rv$pcaPlotRuvBatch() +
                              ggplot2::theme(legend.position = "none"),
                            align = "hv", axis = "tblr", ncol = 3, nrow = 2
                        ),
                        height = 14, width = 14, units = c("in"),
                        device = ext
                    )
                }
            )
        })

        lapply(c("png", "tiff", "pdf", "svg"), function(ext) {
            output[[paste0("pcaLegend_", ext)]] <- shiny::downloadHandler(
                filename = function() {
                    paste("plot", ext, sep = ".")
                },
                content = function(file) {
                    ggplot2::ggsave(file,
                        plot = cowplot::plot_grid(
                            ggpubr::as_ggplot(
                                ggpubr::get_legend(
                                    rv$pcaPlotCpm()
                                )
                            ),
                            ggpubr::as_ggplot(
                                ggpubr::get_legend(
                                    rv$pcaPlotCpmBatch()
                                )
                            )
                        ),
                        height = 12, width = 12, units = c("in"),
                        device = ext
                    )
                }
            )
        })
    })
    # nocov end


    # nocov start
    shiny::observeEvent(input$togglePCAcustom, {
        shinyjs::toggle(id = "PCAcustom")
    })
    # nocov end

    # nocov start
    shiny::observeEvent(input$togglePCAcustom, {
        shinyjs::toggle(id = "PCAcustomBatch")
    })
    # nocov end
}
