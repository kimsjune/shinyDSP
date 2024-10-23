.speCpm <- function(input, output, session, rv) {
    # nocov start
    speCpm <- shiny::eventReactive(rv$spe(), {
        speCpm <- scater::runPCA(rv$spe())
        return(speCpm)
    })
    # nocov end
    return(speCpm)
}

.speCpm_compute <- function(input, output, session, rv) {
    # nocov start
    speCpm_compute <- shiny::eventReactive(rv$speCpm(), {
        speCpm_compute <- SingleCellExperiment::reducedDim(rv$speCpm(), "PCA")
        return(speCpm_compute)
    })
    # nocov end
    return(speCpm_compute)
}

.pcaPlotCpm <- function(input, output, session, rv) {
    # nocov start
    pcaPlotCpm <- shiny::reactive({
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
            rv$speCpm(), rv$speCpm_compute(), ExpVar,
            input$selectedTypes, ROIshapes, ROIcolours
        ) +
            ggplot2::ggtitle(paste0("CPM - by ", ExpVar))

        return(pcaPlot)
    })
    # nocov end
    return(pcaPlotCpm)
}

.pcaPlotCpmBatch <- function(input, output, session, rv) {
    # nocov start
    pcaPlotCpmBatch <- shiny::reactive({
        batchVars <- rv$data()[[2]] |>
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
            rv$speCpm(), rv$speCpm_compute(),
            input$selectedBatch, batchVars,
            ROIshapes, ROIcolours
        ) +
            ggplot2::ggtitle("CPM - by batch")

        return(pcaPlot)
    })
    # nocov end
    return(pcaPlotCpmBatch)
}
