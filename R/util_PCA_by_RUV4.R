.speRuv_NCGs <- function(input, output, session, rv) {
    # nocov start
    speRuv_NCGs <- shiny::eventReactive(rv$spe(), {
        return(standR::findNCGs(rv$spe(),
            batch_name = input$selectedBatch,
            top_n = 200
        ))
    })
    # nocov end
    return(speRuv_NCGs)
}

.speRuvBatchCorrection <- function(input, output, session, rv) {
    # nocov start
    ## Need BOTH input$selectedExpVar and rv$speRuv_NCGs()
    speRuvBatchCorrection <- shiny::eventReactive(c(
        input$selectedExpVar,
        rv$speRuv_NCGs()
    ), {
        ExpVar <- paste0(input$selectedExpVar, collapse = "_")
        return(standR::geomxBatchCorrection(rv$speRuv_NCGs(),
            factors = ExpVar,
            NCGs = S4Vectors::metadata(rv$speRuv_NCGs())$NCGs, k = input$k
        ))
    })
    # nocov end
    return(speRuvBatchCorrection)
}

.speRuv <- function(input, output, session, rv) {
    # nocov start
    speRuv <- shiny::eventReactive(rv$speRuvBatchCorrection(), {
        speRuv <- scater::runPCA(rv$speRuvBatchCorrection())
        return(speRuv)
    })
    # nocov end
    return(speRuv)
}

.speRuv_compute <- function(input, output, session, rv) {
    # nocov start
    speRuv_compute <- shiny::eventReactive(rv$speRuv(), {
        speRuv_compute <- SingleCellExperiment::reducedDim(rv$speRuv(), "PCA")
        return(speRuv_compute)
    })
    # nocov end
    return(speRuv_compute)
}

.pcaPlotRuv <- function(input, output, session, rv) {
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
            rv$speRuv(), rv$speRuv_compute(), ExpVar,
            input$selectedTypes, ROIshapes, ROIcolours
        ) +
            ggplot2::ggtitle(paste0("Ruv4 - by ", ExpVar))

        return(pca_plot)
    })
    # nocov end
    return(pcaPlotRuv)
}


.pcaPlotRuvBatch <- function(input, output, session, rv) {
    # nocov start
    pcaPlotRuvBatch <- shiny::reactive({
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
            rv$speRuv(), rv$speRuv_compute(),
            input$selectedBatch, batchVars,
            ROIshapes, ROIcolours
        ) +
            ggplot2::ggtitle("Ruv4 - by batch")

        return(pcaPlot)
    })
    # nocov end
    return(pcaPlotRuvBatch)
}
