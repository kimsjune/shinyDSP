speQ3 <- shiny::eventReactive(spe(), {
    speQ3 <- standR::geomxNorm(spe(), method = "upperquartile")
    speQ3 <- scater::runPCA(speQ3)
    return(speQ3)
})

speQ3_compute <- shiny::eventReactive(speQ3(), {
    speQ3_compute <- SingleCellExperiment::reducedDim(speQ3(), "PCA")
    return(speQ3_compute)
})

pcaPlotQ3 <- shiny::reactive({
    req(speQ3(), speQ3_compute())

    # Initialize
    ROIshapes <- list()
    ROIcolours <- list()

    ROIshapes <- lapply(seq_along(input$selectedTypes), function(i) {input[[paste0("shape_",input$selectedTypes[i])]]})
    ROIcolours <- lapply(seq_along(input$selectedTypes), function(i) {input[[paste0("colour_",input$selectedTypes[i])]]})

    pcaPlot <- .PCAFunction(speQ3(), speQ3_compute(), "Type",
                                  input$selectedTypes, ROIshapes, ROIcolours) +
        ggplot2::ggtitle("Q3 - by type")
    return(pcaPlot)
})

pcaPlotQ3Batch <- shiny::reactive({
    req(speQ3(), speQ3_compute())

    batchVars <- data()[[2]] %>%
        dplyr::pull(input$selectedBatch) %>%
        unique()

    # Initialize
    ROIshapes <- list()
    ROIcolours <- list()

    ROIshapes <- lapply(seq_along(batchVars), function(i) {input[[paste0("shape_",batchVars[i])]]})
    ROIcolours <- lapply(seq_along(batchVars), function(i) {input[[paste0("colour_",batchVars[i])]]})

    pcaPlot <- .PCAFunction(speQ3(), speQ3_compute(), input$selectedBatch,
                                  batchVars, ROIshapes, ROIcolours) +
        ggplot2::ggtitle("Q3 - by batch")

    return(pcaPlot)
})
