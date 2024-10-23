.speQ3 <- function(input, output, session, rv) {
    # nocov start
    speQ3 <- shiny::eventReactive(rv$spe(), {
        speQ3 <- standR::geomxNorm(rv$spe(), method = "upperquartile")
        speQ3 <- scater::runPCA(speQ3)
        return(speQ3)
    })
    # nocov end
    return(speQ3)
}

.speQ3_compute <- function(input, output, session, rv) {
    # nocov start
    speQ3_compute <- shiny::eventReactive(rv$speQ3(), {
        speQ3_compute <- SingleCellExperiment::reducedDim(rv$speQ3(), "PCA")
        return(speQ3_compute)
    })
    # nocov end
    return(speQ3_compute)
}

.pcaPlotQ3 <- function(input, output, session, rv) {
    # nocov start
    pcaPlotQ3 <- shiny::reactive({
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

        pcaPlot <- .PCAFunction(
            rv$speQ3(), rv$speQ3_compute(), ExpVar,
            input$selectedTypes, ROIshapes, ROIcolours
        ) +
            ggplot2::ggtitle(paste0("Q3 - by ", ExpVar))

        return(pcaPlot)
    })
    # nocov end
    return(pcaPlotQ3)
}

.pcaPlotQ3Batch <- function(input, output, session, rv) {
    # nocov start
    pcaPlotQ3Batch <- shiny::reactive({
        batchVars <- rv$data()[[2]] %>%
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
            rv$speQ3(), rv$speQ3_compute(), input$selectedBatch,
            batchVars, ROIshapes, ROIcolours
        ) +
            ggplot2::ggtitle("Q3 - by batch")

        return(pcaPlot)
    })
    # nocov end
    return(pcaPlotQ3Batch)
}
