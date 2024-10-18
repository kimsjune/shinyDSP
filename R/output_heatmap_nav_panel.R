.outputHeatmapNavPanel <- function() {
    # nocov start
    output$heatmapUI <- shiny::renderUI({
        shiny::renderPlot(
            heatmap(),
            height = 1.5 * as.numeric(heatmap_size()) * dim(lcpm_subset_scale_topGenes())[1]
            # height = as.numeric(heatmap_size()) * dim(lcpm_subset_scale_topGenes())[1],
            # width = as.numeric(heatmap_size()) * dim(lcpm_subset_scale_topGenes())[2]
        )
    })
    #
    # output$downloadHeatmap <- shiny::downloadHandler(
    #     filename = function() {
    #         paste0("heatmap_", Sys.Date(), ".png")
    #     },
    #     content = function(file) {
    #         png(file, width = dim(lcpm_subset_scale_topGenes())[2] * 50, height = dim(lcpm_subset_scale_topGenes())[1] * 50, bg = "transparent")
    #         draw(heatmap())
    #         dev.off()
    #     },
    #     contentType = "image/png"
    # )
}
