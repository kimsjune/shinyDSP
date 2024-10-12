# nocov start
speCPM <- shiny::eventReactive(spe(), {
  speCPM <- scater::runPCA(spe())
  return(speCPM)
})
# nocov end

# nocov start
speCPM_compute <- shiny::eventReactive(speCPM(), {
  speCPM_compute <- SingleCellExperiment::reducedDim(speCPM(), "PCA")
  return(speCPM_compute)
})
# nocov end

# nocov start
pcaPlotCpm <- shiny::reactive({
  req(speCPM(), speCPM_compute())

  # Initialize
  ROIshapes <- list()
  ROIcolours <- list()

  ROIshapes <- lapply(seq_along(input$selectedTypes), function(i) {
    input[[paste0("shape_", input$selectedTypes[i])]]
  })
  ROIcolours <- lapply(seq_along(input$selectedTypes), function(i) {
    input[[paste0("colour_", input$selectedTypes[i])]]
  })

  pcaPlot <- .PCAFunction(
    speCPM(), speCPM_compute(), input$selectedExpVar,
    input$selectedTypes, ROIshapes, ROIcolours
  ) +
    ggplot2::ggtitle(paste0("CPM - by ", input$selectedExpVar))

  return(pcaPlot)
})
# nocov end

# nocov start
pcaPlotCpmBatch <- shiny::reactive({
  req(speCPM(), speCPM_compute())

  batchVars <- data()[[2]] |>
    dplyr::pull(input$selectedBatch) |>
    unique()

  # Initialize
  ROIshapes <- list()
  ROIcolours <- list()

  ROIshapes <- lapply(seq_along(batchVars), function(i) {
    input[[paste0("shape_", batchVars[i])]]
  })
  ROIcolours <- lapply(seq_along(batchVars), function(i) {
    input[[paste0("colour_", batchVars[i])]]
  })

  pcaPlot <- .PCAFunction(
    speCPM(), speCPM_compute(),
    input$selectedBatch, batchVars,
    ROIshapes, ROIcolours
  ) +
    ggplot2::ggtitle("CPM - by batch")

  return(pcaPlot)
})
# nocov end
