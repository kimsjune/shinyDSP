.outputPcaNavPanel <- function() {
  # nocov start
  # Opens a grouped widgets to pick shape and colour
  output$customization <- shiny::renderUI({
    shiny::req(input$selectedTypes)
    PCAcustomization()
  })
  # nocov end

  # nocov start
  # Opens a grouped widgets to pick shape and colour
  output$customizationBatch <- shiny::renderUI({
    shiny::req(input$selectedTypes)
    PCAcustomizationBatch()
  })
  # nocov end

  # nocov start
  output$pcaPlotCpm <- shiny::renderUI({
    shiny::validate(
      shiny::need(
        shiny::isTruthy(input$generatePCA) & shiny::isTruthy(input$selectedTypes),
        "Hit 'run' to generate PCA plots."
      )
    )
    shiny::renderPlot(pcaPlotCpm()+
                        ggplot2::theme(legend.position = "none"))
  })
  # nocov end

  # nocov start
  output$pcaPlotQ3 <- shiny::renderUI({
    shiny::req(input$generatePCA, input$selectedTypes)
    shiny::renderPlot(pcaPlotQ3()+
                        ggplot2::theme(legend.position = "none"))
  })
  # nocov end

  # nocov start
  output$pcaPlotRuv <- shiny::renderUI({
    shiny::req(input$generatePCA, input$selectedTypes)
    shiny::renderPlot(pcaPlotRuv()+
                        ggplot2::theme(legend.position = "none"))
  })
  # nocov end

  # nocov start
  output$pcaPlotCpmBatch <- shiny::renderUI({
    shiny::req(input$generatePCA, input$selectedTypes)
    shiny::renderPlot(pcaPlotCpmBatch()+
                        ggplot2::theme(legend.position = "none"))
  })
  # nocov end

  # nocov start
  output$pcaPlotQ3Batch <- shiny::renderUI({
    shiny::req(input$generatePCA, input$selectedTypes)
    shiny::renderPlot(pcaPlotQ3Batch()+
                        ggplot2::theme(legend.position = "none"))
  })
  # nocov end

  # nocov start
  output$pcaPlotRuvBatch <- shiny::renderUI({
    shiny::req(input$generatePCA, input$selectedTypes)
    shiny::renderPlot(pcaPlotRuvBatch()+
                        ggplot2::theme(legend.position = "none"))
  })
  # nocov end


  # nocov start
  output$pcaPlotLegend <- shiny::renderUI({
    shiny::req(input$generatePCA, input$selectedTypes)
    legend <- ggpubr::as_ggplot(
      ggpubr::get_legend(
        pcaPlotCpm()
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
        pcaPlotCpmBatch()
      )
    )
    shiny::renderPlot(legend)
  })
  # nocov end

  # # nocov start
  # output$downloadPca <- shiny::downloadHandler(
  #   filename = function() {
  #     paste0("pca_", Sys.Date(), ".png")
  #   },
  #   content = function(file) {
  #     ggplot2::ggsave(file, device = "png",
  #                        cowplot::plot_grid(
  #
  #         pcaPlotCpm() + ggplot2::theme(legend.position = "none"),
  #         pcaPlotQ3() + ggplot2::theme(legend.position = "none"),
  #         pcaPlotRuv() + ggplot2::theme(legend.position = "none"),
  #         pcaPlotCpmBatch() + ggplot2::theme(legend.position = "none"),
  #         pcaPlotQ3Batch() + ggplot2::theme(legend.position = "none"),
  #         pcaPlotRuvBatch() + ggplot2::theme(legend.position = "none"),
  #         align = 'hv', axis = 'tblr', ncol = 3, nrow = 2
  #                        ),
  #         height = 12, width = 12, units = c("in")
  #
  #
  #     )
  #   },
  #   contentType = "image/png"
  # )
  # # nocov end
  #
  # # nocov start
  # output$downloadPcaLegend <- shiny::downloadHandler(
  #   filename = function() {
  #     paste0("pca_legend_", Sys.Date(), ".png")
  #   },
  #   content = function(file) {
  #     ggplot2::ggsave(file, device = "png",
  #                     cowplot::plot_grid(
  #                     ggpubr::as_ggplot(
  #                       ggpubr::get_legend(
  #                         pcaPlotCpm()
  #                       )
  #                     ),
  #                     ggpubr::as_ggplot(
  #                       ggpubr::get_legend(
  #                         pcaPlotCpmBatch()
  #                       )
  #                     )),
  #                     height = 12, width = 12, units = c("in")
  #
  #
  #     )
  #   },
  #   contentType = "image/png"
  # )
  # # nocov end



  lapply(c("png", "tiff", "pdf", "svg"), function(ext) {
    output[[paste0("pca_", ext)]] <- downloadHandler(
      filename = function() {
        paste("plot", ext, sep = ".")
      },
      content = function(file) {

        ggsave(file,
               plot = cowplot::plot_grid(

                 pcaPlotCpm() + ggplot2::theme(legend.position = "none"),
                 pcaPlotQ3() + ggplot2::theme(legend.position = "none"),
                 pcaPlotRuv() + ggplot2::theme(legend.position = "none"),
                 pcaPlotCpmBatch() + ggplot2::theme(legend.position = "none"),
                 pcaPlotQ3Batch() + ggplot2::theme(legend.position = "none"),
                 pcaPlotRuvBatch() + ggplot2::theme(legend.position = "none"),
                 align = 'hv', axis = 'tblr', ncol = 3, nrow = 2
               ),
               height = 14, width = 14, units = c("in"),
               device = ext)
      }
    )
  })

  lapply(c("png", "tiff", "pdf", "svg"), function(ext) {
    output[[paste0("pcaLegend_", ext)]] <- downloadHandler(
      filename = function() {
        paste("plot", ext, sep = ".")
      },
      content = function(file) {

        ggsave(file,
                                   plot = cowplot::plot_grid(
                                   ggpubr::as_ggplot(
                                     ggpubr::get_legend(
                                       pcaPlotCpm()
                                     )
                                   ),
                                   ggpubr::as_ggplot(
                                     ggpubr::get_legend(
                                       pcaPlotCpmBatch()
                                     )
                                   )),
               height = 12, width = 12, units = c("in"),
               device = ext)
      }
    )
  })
}
