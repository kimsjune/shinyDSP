# nocov start
speRUV_NCGs <- shiny::eventReactive(spe(), {
    return(standR::findNCGs(spe(),
        batch_name = input$selectedBatch,
        top_n = 200
    ))
})
# nocov end

# nocov start
speRUVBatchCorrection <- shiny::eventReactive(list(speRUV_NCGs(), input$k), {
    ExpVar <- paste0(input$selectedExpVar, collapse = "_")
    return(standR::geomxBatchCorrection(speRUV_NCGs(),
        factors = ExpVar,
        NCGs = S4Vectors::metadata(speRUV_NCGs())$NCGs, k = input$k
    ))
})
# nocov end

# nocov start
speRUV <- shiny::eventReactive(speRUVBatchCorrection(), {
    speRUV <- scater::runPCA(speRUVBatchCorrection())
    return(speRUV)
})
# nocov end

# nocov start
speRUV_compute <- shiny::eventReactive(speRUV(), {
    speRUV_compute <- SingleCellExperiment::reducedDim(speRUV(), "PCA")
    return(speRUV_compute)
})
# nocov end

# nocov start
pcaPlotRuv <- shiny::reactive({
    ExpVar <- paste0(input$selectedExpVar, collapse = "_")

    # Initialize
    ROIshapes <- list()
    ROIcolours <- list()

    ROIshapes <- lapply(seq_along(input$selectedTypes), function(i) {
        input[[paste0("shape_", input$selectedTypes[i])]]
    })
    ROIcolours <- lapply(seq_along(input$selectedTypes), function(i) {
        input[[paste0("colour_", input$selectedTypes[i])]]
    })

    pca_plot <- .PCAFunction(
        speRUV(), speRUV_compute(), ExpVar,
        input$selectedTypes, ROIshapes, ROIcolours
    ) +
        ggplot2::ggtitle(paste0("RUV4 - by ", ExpVar))

    return(pca_plot)
})
# nocov end

# nocov start
pcaPlotRuvBatch <- shiny::reactive({
    req(speRUV(), speRUV_compute())

    batchVars <- data()[[2]] %>%
        dplyr::pull(input$selectedBatch) %>%
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
        speRUV(), speRUV_compute(),
        input$selectedBatch, batchVars,
        ROIshapes, ROIcolours
    ) +
        ggplot2::ggtitle("RUV4 - by batch")

    return(pcaPlot)
})
# nocov end
