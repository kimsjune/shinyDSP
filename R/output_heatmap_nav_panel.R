.outputHeatmapNavPanel2 <- function(input, output, rv) {
    # nocov start
    output$heatmapUI <- shiny::renderUI({
        shiny::validate(
            shiny::need(
                shiny::isTruthy(input$generateHeatmap),
                "Hit 'Show/update'!"
            )
        )


        tabsets <- lapply(names(rv$heatmap()), function(name) {
            shiny::tabPanel(
                name,
                shiny::plotOutput(outputId = paste0("heatmap_", name)),
                shiny::fluidRow(
                    class = "justified-buttons",
                    lapply(c("png", "tiff", "pdf", "svg"), function(ext) {
                        shiny::column(3,
                            style = "text-align: center;",
                            shiny::downloadButton(
                                paste0("downloadHeatmap", name, ext),
                                paste(toupper(ext))
                            )
                        )
                    })
                )
            )
        })


        shiny::tabsetPanel(
            type = "tabs",
            !!!tabsets
        )
    })
    # nocov end

    # nocov start
    shiny::observeEvent(input$generateHeatmap, {
        lapply(names(rv$heatmap()), function(name) {
            output[[paste0("heatmap_", name)]] <- shiny::renderPlot(
                expr = rv$heatmap()[[name]],
                width = "auto",
                height = "auto"
            )
        })
    })
    # nocov end

    # nocov start
    shiny::observeEvent(input$generateHeatmap, {
        shiny::req(rv$lcpmSubScaleTopGenes())


        lapply(names(rv$heatmap()), function(name) {
            w <- as.numeric(input$heatmapSize) / 1.5 * dim(rv$lcpmSubScaleTopGenes()[[name]])[2]
            h <- as.numeric(input$heatmapSize) / 1.5 * dim(rv$lcpmSubScaleTopGenes()[[name]])[1]
            lapply(c("png", "tiff", "pdf", "svg"), function(ext) {
                output[[paste0("downloadHeatmap", name, ext)]] <- shiny::downloadHandler(
                    filename = function() {
                        paste("heatmap", name, ext, sep = ".")
                    },
                    content = function(file) {
                        ggplot2::ggsave(file,
                            grid::grid.grabExpr(ComplexHeatmap::draw(rv$heatmap()[[name]])),
                            height = as.numeric(h),
                            width = as.numeric(w),
                            units = c("mm"),
                            device = ext,
                            limitsize = FALSE
                        )
                    }
                )
            })
        })
    })
    # nocov end
}
