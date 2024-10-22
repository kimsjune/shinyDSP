.volcano <- function(input, output, session, rv) {
# nocov start
volcano <- shiny::eventReactive(input$generateVolcano,{
    shiny::req(rv$efit(), rv$contrast())


    shiny::withProgress(message = "Plotting...", {

        volcanoDF <- lapply(seq_len(ncol(rv$contrast())), function(i) {
                limma::topTable(rv$efit(), coef = i, number = Inf) %>%
                    tibble::rownames_to_column(var = "Target.name") %>%
                    dplyr::select("Target.name", "logFC", "adj.P.Val") %>%
                    dplyr::mutate(de = ifelse(logFC >= input$logFCcutoff &
                        adj.P.Val < input$PvalCutoff, "UP",
                    ifelse(logFC <= -(input$logFCcutoff) &
                        adj.P.Val < input$PvalCutoff, "DN",
                    "NO"
                    )
                    )) %>%
                dplyr::mutate(logFC_threshold = quantile(abs(logFC), 0.99, na.rm = TRUE),
                                 pval_threshold = quantile(adj.P.Val, 0.01, na.rm = TRUE),
                              deLab  = ifelse(abs(logFC) > logFC_threshold & adj.P.Val < pval_threshold
                                              & abs(logFC) >= input$logFCcutoff & adj.P.Val < input$PvalCutoff, Target.name, NA)
                )

            })


        # If not using custom range, determine maximum absolute FC for each plot
        if (input$toggleCustomRange == FALSE) {
            plots <- list()

            plots <- lapply(seq_along(volcanoDF), function(i) {
                .volcanoFunction(
                    volcanoDF[[i]], input$delabSize, input$maxOverlap,
                    colnames(rv$contrast())[i],
                    input$logFCcutoff, input$PvalCutoff,
                    input$DnCol, input$notDEcol, input$UpCol
                ) +
                    ggplot2::xlim(
                        0 - max(abs(volcanoDF[[i]] %>% dplyr::select(logFC))),
                        max(abs(volcanoDF[[i]] %>% dplyr::select(logFC)))
                    )
            })
        } else {
            plots <- lapply(seq_along(volcanoDF), function(i) {
                .volcanoFunction(
                    volcanoDF[[i]], input$delabSize, input$maxOverlap,
                    colnames(rv$contrast())[i],
                    input$logFCcutoff, input$PvalCutoff,
                    input$DnCol, input$notDEcol, input$UpCol
                ) +
                    ggplot2::xlim(
                        input$customX[1],
                        input$customX[2]
                    ) +
                    ggplot2::ylim(
                        0, input$customY
                    )
            })
        }
    })

    names(plots) <- names(rv$topTabDF())

    return(plots)
})
# nocov end
    return(volcano)
}
