.outputPcaNavPanel <- function() {
  # nocov start
  # Opens a grouped widgets to pick shape and colour
  output$customization <- shiny::renderUI({
    shiny::req(input$selectedTypes)
    .PCAcustomization()
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
        shiny::isTruthy(input$load) & shiny::isTruthy(input$selectedTypes),
        "'Load' data then select groups first!"
      )
    )
    shiny::renderPlot(pcaPlotCpm()+
                        ggplot2::theme(legend.position = "none"))
  })
  # nocov end

  # nocov start
  output$pcaPlotQ3 <- shiny::renderUI({
    shiny::req(input$load, input$selectedTypes)
    shiny::renderPlot(pcaPlotQ3()+
                        ggplot2::theme(legend.position = "none"))
  })
  # nocov end

  # nocov start
  output$pcaPlotRuv <- shiny::renderUI({
    shiny::req(input$load, input$selectedTypes)
    shiny::renderPlot(pcaPlotRuv()+
                        ggplot2::theme(legend.position = "none"))
  })
  # nocov end

  # nocov start
  output$pcaPlotCpmBatch <- shiny::renderUI({
    shiny::req(input$load, input$selectedTypes)
    shiny::renderPlot(pcaPlotCpmBatch()+
                        ggplot2::theme(legend.position = "none"))
  })
  # nocov end

  # nocov start
  output$pcaPlotQ3Batch <- shiny::renderUI({
    shiny::req(input$load, input$selectedTypes)
    shiny::renderPlot(pcaPlotQ3Batch()+
                        ggplot2::theme(legend.position = "none"))
  })
  # nocov end

  # nocov start
  output$pcaPlotRuvBatch <- shiny::renderUI({
    shiny::req(input$load, input$selectedTypes)
    shiny::renderPlot(pcaPlotRuvBatch()+
                        ggplot2::theme(legend.position = "none"))
  })
  # nocov end


  # nocov start
  output$pcaPlotLegend <- shiny::renderUI({
    shiny::req(input$load, input$selectedTypes)
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
    shiny::req(input$load, input$selectedTypes)
    legend <- ggpubr::as_ggplot(
      ggpubr::get_legend(
        pcaPlotCpmBatch()
      )
    )
    shiny::renderPlot(legend)
  })
  # nocov end

  # nocov start
  output$downloadPca <- shiny::downloadHandler(
    filename = function() {
      paste0("pca_", Sys.Date(), ".png")
    },
    content = function(file) {
      grDevices::png(file)
      cowplot::save_plot(file,

          pcaPlotCpm() + ggplot2::theme(legend.position = "none"),
          pcaPlotQ3() + ggplot2::theme(legend.position = "none"),
          pcaPlotRuv() + ggplot2::theme(legend.position = "none"),
          pcaPlotCpmBatch() + ggplot2::theme(legend.position = "none"),
          pcaPlotQ3Batch() + ggplot2::theme(legend.position = "none"),
          pcaPlotRuvBatch() + ggplot2::theme(legend.position = "none")

      ,
        ncol = 3, align = "v",
        axis = "lr"
      )
      dev.off()
    },
    contentType = "image/png"
  )
  # nocov end
}
