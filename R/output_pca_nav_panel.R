.outputPcaNavPanel <- function(input, output) {
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
    shiny::renderPlot(pcaPlotCpm())
  })
  # nocov end

  # nocov start
  output$pcaPlotQ3 <- shiny::renderUI({
    shiny::req(input$load, input$selectedTypes)
    shiny::renderPlot(pcaPlotQ3())
  })
  # nocov end

  # nocov start
  output$pcaPlotRuv <- shiny::renderUI({
    shiny::req(input$load, input$selectedTypes)
    shiny::renderPlot(pcaPlotRuv())
  })
  # nocov end

  # nocov start
  output$pcaPlotCpmBatch <- shiny::renderUI({
    shiny::req(input$load, input$selectedTypes)
    shiny::renderPlot(pcaPlotCpmBatch())
  })
  # nocov end

  # nocov start
  output$pcaPlotQ3Batch <- shiny::renderUI({
    shiny::req(input$load, input$selectedTypes)
    shiny::renderPlot(pcaPlotQ3Batch())
  })
  # nocov end

  # nocov start
  output$pcaPlotRuvBatch <- shiny::renderUI({
    shiny::req(input$load, input$selectedTypes)
    shiny::renderPlot(pcaPlotRuvBatch())
  })
  # nocov end

  # nocov start
  # output$downloadPCA <- shiny::downloadHandler(
  #   filename = function() {
  #     base::paste0("pca_", Sys.Date(), ".png")
  #   },
  #   content = function(file) {
  #     png(file)
  #     pcaPlot()
  #     dev.off()
  #   },
  #   contentType = "image/png"
  # )
  # nocov end
}
