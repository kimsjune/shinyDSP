.lcpmSubScaleTopGenes <- function(input, output, session, rv) {
    # nocov start
    lcpmSubScaleTopGenes <- shiny::eventReactive(input$generateHeatmap, {
        shiny::req(input$selectedTypes)

        spe <- switch(input$selectedNorm,
            "CPM" = rv$speCpm(),
            "Q3" = rv$speQ3(),
            "RUV4" = rv$speRuv()
        )

        ExpVar <- paste0(input$selectedExpVar, collapse = "_")

        lcpmSubScaleTopGenes <- lapply(names(rv$topTabDF()), function(name) {
            columns <- stringr::str_split_1(name, "_vs_") %>%
                sapply(function(.) {
                    which(SummarizedExperiment::colData(spe) %>%
                        tibble::as_tibble() %>%
                        dplyr::pull(ExpVar) == .)
                }) %>%
                unlist()

            table <- SummarizedExperiment::assay(spe, 2)[
                rv$topTabDF()[[name]] %>%
                    dplyr::slice_head(n = input$topNgenes) %>%
                    dplyr::select(Gene) %>%
                    unlist() %>%
                    unname(),
                columns
            ] %>%
                data.frame() %>%
                t() %>%
                scale() %>%
                t()

            return(table)
        })

        names(lcpmSubScaleTopGenes) <- names(rv$topTabDF())


        return(lcpmSubScaleTopGenes)
    })
    # nocov end
    return(lcpmSubScaleTopGenes)
}

.columnSplit <- function(input, output, session, rv) {
    # nocov start
    spe <- switch(input$selectedNorm,
        "CPM" = rv$speCpm(),
        "Q3" = rv$speQ3(),
        "RUV4" = rv$speRuv()
    )

    ExpVar <- paste0(input$selectedExpVar, collapse = "_")

    columnSplit <- shiny::eventReactive(rv$lcpmSubScaleTopGenes(), {
        columnSplit <- lapply(names(rv$topTabDF()), function(name) {
            columnSplit <- stringr::str_split_1(name, "_vs_") %>%
                sapply(function(.) {
                    which(
                        SummarizedExperiment::colData(spe) %>%
                            tibble::as_tibble() %>% dplyr::select(ExpVar) == .
                    )
                }) %>%
                summary() %>%
                .[, "Length"]
        })

        names(columnSplit) <- names(rv$lcpmSubScaleTopGenes())



        return(columnSplit)
    })
    # nocov end
    return(columnSplit)
}


.heatmap <- function(input, output, session, rv) {
    # nocov start
    heatmap <- shiny::eventReactive(rv$lcpmSubScaleTopGenes(), {
        spe <- switch(input$selectedNorm,
            "CPM" = rv$speCpm(),
            "Q3" = rv$speQ3(),
            "RUV4" = rv$speRuv()
        )
        ExpVar <- paste0(input$selectedExpVar, collapse = "_")

        colFunc <- circlize::colorRamp2(
            c(
                input$heatmapRange[1], 0,
                input$heatmapRange[2]
            ),
            hcl_palette = input$heatmapCol
        )

        heatmap <- lapply(names(rv$lcpmSubScaleTopGenes()), function(name) {
            ComplexHeatmap::Heatmap(rv$lcpmSubScaleTopGenes()[[name]],
                cluster_columns = F, col = colFunc,
                heatmap_legend_param = list(
                    border = "black",
                    title = "Z score",
                    title_gp = grid::gpar(
                        fontsize = input$heatmapFontSize,
                        fontface = "plain",
                        fontfamily = "sans"
                    ),
                    labels_gp = grid::gpar(
                        fontsize = input$heatmapFontSize,
                        fontface = "plain",
                        fontfamily = "sans"
                    ),
                    legend_height = grid::unit(
                        3 * as.numeric(input$heatmapSize),
                        units = "mm"
                    )
                ),
                top_annotation = ComplexHeatmap::HeatmapAnnotation(
                    foo = ComplexHeatmap::anno_block(
                        gp = grid::gpar(lty = 0, fill = "transparent"),
                        labels = rv$columnSplit()[[name]] %>% names(),
                        labels_gp = grid::gpar(
                            col = "black", fontsize = 14,
                            fontfamily = "sans",
                            fontface = "bold"
                        ),
                        labels_rot = 0, labels_just = "center",
                        labels_offset = grid::unit(4.5, "mm")
                    )
                ),
                border_gp = grid::gpar(col = "black", lwd = 0.2),
                row_names_gp = grid::gpar(
                    fontfamily = "sans",
                    fontface = "italic",
                    fontsize = input$heatmapFontSize
                ),
                show_column_names = F,
                column_split = rep(
                    LETTERS[seq_len(rv$columnSplit()[[name]] %>% length())],
                    rv$columnSplit()[[name]] %>% unname() %>% as.numeric()
                )
            )
        })

        names(heatmap) <- names(rv$lcpmSubScaleTopGenes())


        return(heatmap)
    })
    # nocov end
    return(heatmap)
}
