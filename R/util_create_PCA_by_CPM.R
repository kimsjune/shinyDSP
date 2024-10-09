speCPM <- shiny::eventReactive(spe(), {
    speCPM <- scater::runPCA(spe())
    return(speCPM)
})

speCPM_compute <- shiny::eventReactive(speCPM(), {
    speCPM_compute <- SingleCellExperiment::reducedDim(speCPM(), "PCA")
    return(speCPM_compute)
})


pcaPlotCpm <- shiny::reactive({
    req(speCPM(), speCPM_compute())

    # Initialize
    ROIshapes <- list()
    ROIcolours <- list()

    ROIshapes <- lapply(seq_along(input$selectedTypes), function(i) {input[[paste0("shape_",input$selectedTypes[i])]]})
    ROIcolours <- lapply(seq_along(input$selectedTypes), function(i) {input[[paste0("colour_",input$selectedTypes[i])]]})



    pcaPlot <- .PCAFunction(speCPM(), speCPM_compute(), "Type",
                                  input$selectedTypes, ROIshapes, ROIcolours) +
        ggplot2::ggtitle("CPM - by type")

    return(pcaPlot)
})

pcaPlotCpmBatch <- shiny::reactive({
    req(speCPM(), speCPM_compute())

    batchVars <- data()[[2]] |>
        dplyr::pull(input$selectedBatch) |>
        unique()

    # Initialize
    ROIshapes <- list()
    ROIcolours <- list()

    ROIshapes <- lapply(seq_along(batchVars), function(i) {input[[paste0("shape_",batchVars[i])]]})
    ROIcolours <- lapply(seq_along(batchVars), function(i) {input[[paste0("colour_",batchVars[i])]]})






    pcaPlot <- .PCAFunction(speCPM(), speCPM_compute(),
                                  input$selectedBatch, batchVars,
                                  ROIshapes, ROIcolours) +
        ggplot2::ggtitle("CPM - by batch")

    return(pcaPlot)
})
