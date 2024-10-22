.lcpmSubScaleTopGenes <- function(input, output, session, rv) {

    lcpmSubScaleTopGenes <- shiny::eventReactive(input$generateHeatmap, {
    shiny::req(input$selectedTypes)

    spe <- switch(input$selectedNorm,
                  "CPM" = rv$speCpm(),
                  "Q3" = rv$speQ3(),
                  "RUV4" = rv$speRuv())

    ExpVar <- paste0(input$selectedExpVar, collapse = "_")

    # lcpmSub <- lapply(seq_along(input$selectedTypes), function(i) {
    #     SummarizedExperiment::assay(spe, 2)[, SummarizedExperiment::colData(spe)
    #     %>% tibble::as_tibble()
    #         %>% dplyr::select(ExpVar)
    #     == input$selectedTypes[i]]
    # })

    lcpmSubScaleTopGenes <- lapply(names(rv$topTabDF()), function(name) {
        SummarizedExperiment::assay(spe,2)[
            rv$topTabDF()[[name]] %>% dplyr::slice_head(n = input$topNgenes) %>%
                dplyr::select(Gene) %>% unlist() %>% unname()
            ,
                                           SummarizedExperiment::colData(spe) %>%
                                               tibble::as_tibble() %>%
                                               dplyr::pull(ExpVar) %in%
                                               stringr::str_split_1(name, "_vs_")] %>%
            data.frame() %>% t() %>% scale() %>% t()

    })

    names(lcpmSubScaleTopGenes) <- names(rv$topTabDF())

#
#     lcpmSubScale <- t(scale(t(data.frame(lcpmSub))))
#
#     lcpmSubScaleTopGenes <- lcpmSubScale[rv$topTabDF() %>%
#                                                           dplyr::slice_head(n = input$topNgenes) %>%
#                                                           dplyr::select(Gene) %>%
#                                                           unlist() %>%
#                                                           unname(), ]

    return(lcpmSubScaleTopGenes)

})
    return(lcpmSubScaleTopGenes)

}

.columnSplit <- function(input, output, session, rv) {
    spe <- switch(input$selectedNorm,
                  "CPM" = rv$speCpm(),
                  "Q3" = rv$speQ3(),
                  "RUV4" = rv$speRuv())

    ExpVar <- paste0(input$selectedExpVar, collapse = "_")

    columnSplit <- shiny::eventReactive(rv$lcpmSubScaleTopGenes(),{

        columnSplit <- lapply(names(rv$topTabDF()), function(name) {


            SummarizedExperiment::colData(spe) %>% tibble::as_tibble() %>%
                dplyr::filter(!!dplyr::ensym(ExpVar) %in% stringr::str_split_1(name, "_vs_")) %>%
            dplyr::pull(ExpVar) %>% table()
        })

        names(columnSplit) <- names(rv$topTabDF())



        return(columnSplit)

    })

        return(columnSplit)


}




# heatmapColnames <- shiny::eventReactive(lcpm_subset_scale(), {
#
#         spe <- eval(parse(text = input$selectedNorm))
#     ExpVar <- paste0(input$selectedExpVar, collapse = "_")
#
#     mydata <- lapply(seq_along(input$selectedTypes), function(i) {
#         SummarizedExperiment::assay(spe, 2)[, SummarizedExperiment::colData(spe)$ExpVar == input$selectedTypes[i]]
#     })
#
#     return(do.call(cbind, mydata))
# })



.heatmap <- function(input, output, session, rv) {
heatmap <- shiny::eventReactive(rv$lcpmSubScaleTopGenes(), {
    spe <- switch(input$selectedNorm,
                  "CPM" = rv$speCpm(),
                  "Q3" = rv$speQ3(),
                  "RUV4" = rv$speRuv())
    ExpVar <- paste0(input$selectedExpVar, collapse = "_")

    colFunc <- circlize::colorRamp2(c(input$heatmapRange[1], 0, input$heatmapRange[2]), hcl_palette = input$heatmapCol)

    heatmap <- lapply(names(rv$lcpmSubScaleTopGenes()), function(name){

        ComplexHeatmap::Heatmap(rv$lcpmSubScaleTopGenes()[[name]],
                                cluster_columns = F, col = colFunc,
                                heatmap_legend_param = list(
                                    border = "black",
                                    title = "Z score",
                                    title_gp = grid::gpar(fontsize = input$heatmapFontSize, fontface = "plain", fontfamily = "sans"),
                                    labels_gp = grid::gpar(fontsize = input$heatmapFontSize, fontface = "plain", fontfamily = "sans"),
                                    legend_height = grid::unit(3 * as.numeric(input$heatmapSize), units = "mm")
                                ),
                                top_annotation = ComplexHeatmap::HeatmapAnnotation(
                                    foo = ComplexHeatmap::anno_block(
                                        gp = grid::gpar(lty = 0, fill = "transparent"),
                                        labels = rv$columnSplit()[[name]] %>% names(),
                                        labels_gp = grid::gpar(col = "black", fontsize = 14, fontfamily = "sans", fontface = "bold"),
                                        labels_rot = 0, labels_just = "center", labels_offset = grid::unit(4.5, "mm")
                                    )
                                ),
                                border_gp = grid::gpar(col = "black", lwd = 0.2),
                                row_names_gp = grid::gpar(fontfamily = "sans", fontface = "italic", fontsize = input$heatmapFontSize),
                                show_column_names = F,
                                column_split = rep(LETTERS[seq_len(rv$columnSplit()[[name]] %>% unlist() %>% unname() %>% length())],

                                                   rv$columnSplit()[[name]] %>% unlist() %>% unname())
                                )


    })

    names(heatmap) <- names(rv$lcpmSubScaleTopGenes())


    return(heatmap)
})
    return(heatmap)
}


