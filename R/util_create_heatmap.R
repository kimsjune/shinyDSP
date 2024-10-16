lcpm_subset_scale <- shiny::eventReactive(input$generateHeatmap,{
    mydata <- list()
    spe <- eval(parse(text = input$selectedNorm))
    ExpVar <- paste0(input$selectedExpVar, collapse = "_")

    mydata <- lapply(seq_along(input$selectedTypes), function(i) {
        SummarizedExperiment::assay(spe,2)[, SummarizedExperiment::colData(spe)
                                           %>% tibble::as_tibble()
                                           %>% dplyr::select(ExpVar)
                                           == input$selectedTypes[i]]
    })


    lcpm_subset_scale <- t(scale(t(data.frame(mydata))))

    return(lcpm_subset_scale)
})

colnames4heatmap <- shiny::eventReactive(lcpm_subset_scale(),{
    mydata <- list()
    spe <- eval(parse(text = input$selectedNorm))
    ExpVar <- paste0(input$selectedExpVar, collapse = "_")

    mydata <- lapply(seq_along(input$selectedTypes), function(i) {
        SummarizedExperiment::assay(spe,2)[, SummarizedExperiment::colData(spe)$ExpVar == input$selectedTypes[i]]
    })



    return(colnames(do.call(cbind, mydata)))
})




lcpm_subset_scale_topGenes <- shiny::eventReactive(lcpm_subset_scale(),{
    spe <- eval(parse(text = input$selectedNorm))
    ## BEWARE! top_n() reorders rows by some column value. Must use slice_head() to pick first n rows
    lcpm_subset_scale_topGenes <- lcpm_subset_scale()[topTabDF() %>%
                                                          dplyr::slice_head(n = top_n_genes()) %>%
                                                          dplyr::select(Gene) %>%
                                                          unlist() %>%
                                                          unname(), ]

    return(lcpm_subset_scale_topGenes)
})


heatmap <- shiny::eventReactive(lcpm_subset_scale_topGenes(),{
    col_fun <- circlize::colorRamp2(c(heatmap_range()[1], 0, heatmap_range()[2]), hcl_palette = heatmap_col())

    chm <- ComplexHeatmap::Heatmap(lcpm_subset_scale_topGenes(),
                   cluster_columns = F,
                   col = col_fun,
                   # width = grid::unit(dim(lcpm_subset_scale_topGenes())[2]*15, "mm"),
                   # height = grid::unit(dim(lcpm_subset_scale_topGenes())[1]*15, "mm"),
                   # width = grid::unit(as.numeric(heatmap_size()) / 2 * dim(lcpm_subset_scale_topGenes())[2], "mm"),
                   # height = grid::unit(as.numeric(heatmap_size()) / 2 * dim(lcpm_subset_scale_topGenes())[1], "mm"),
                   heatmap_legend_param = list(
                       border = "black",
                       title = "Z score",
                       title_gp = grid::gpar(fontsize = heatmap_fontsize(), fontface = "plain", fontfamily = "sans"),
                       labels_gp = grid::gpar(fontsize = heatmap_fontsize(), fontface = "plain", fontfamily = "sans"),
                       legend_height = grid::unit(3 * as.numeric(heatmap_size()), "mm")
                   ),
                   # top_annotation = ComplexHeatmap::HeatmapAnnotation(
                   #     foo = ComplexHeatmap::anno_block(
                   #         gp = grid::gpar(lty = 0, fill = "transparent"),
                   #         labels = unlist(input$selectedTypes),
                   #         labels_gp = grid::gpar(col = "black", fontsize = 14, fontfamily = "sans", fontface = "bold"),
                   #         labels_rot = 0, labels_just = "center", labels_offset = grid::unit(4.5, "mm")
                   #     )
                   # ),
                   border_gp = grid::gpar(col = "black", lwd = 0.2),
                   row_names_gp = grid::gpar(fontfamily = "sans", fontface = "italic", fontsize = heatmap_fontsize()),
                   show_column_names = F,

                   # top_annotation= HeatmapAnnotation(
                   #   foo = ComplexHeatmap::anno_block(
                   #     gp = grid::gpar(lty=0, fill="transparent"),
                   #     labels = unlist(input$selectedTypes),
                   #     labels_gp = grid::gpar(col="black", fontsize=7, fontfamily = "sans", fontface = "bold"),
                   #     labels_rot = 20, labels_just = "center", labels_offset = grid::unit(4,"mm")
                   #   )
                   # ),
                   # column_split = rep(LETTERS[seq_along(input$selectedTypes)],
                   #                    # times = as.numeric(unname(table(colData(spe)[colnames(lcpm_subset_scale()), "anno_type"])))
                   #                    times = as.numeric(unname(table(SummarizedExperiment::colData(spe)[colnames4heatmap(), ExpVar])))
                   # ),
                   column_title = NULL
    )

    return(chm)
})
