# nocov start
volcano <- shiny::eventReactive(c(
    input$selectedTypes, input$selectedNorm,
    input$generateVolcano
), {
    shiny::req(efit(), contrast(), input$logFCcutoff, input$PvalCutoff)

    shiny::withProgress(message = "Plotting...", {
        # make data frame
        # volcanoDF <- lapply(as.list(seq_len(ncol(contrast()))), function(i) {
        #     limma::topTable(efit(), coef = i, number = Inf) %>%
        #         tibble::rownames_to_column(var = "Target.name") %>%
        #         dplyr::select("Target.name", "logFC", "adj.P.Val") %>%
        #         dplyr::mutate(de = ifelse(logFC >= input$logFCcutoff &
        #             adj.P.Val < input$PvalCutoff, "UP",
        #         ifelse(logFC <= -(input$logFCcutoff) &
        #             adj.P.Val < input$PvalCutoff, "DN",
        #         "NO"
        #         )
        #         )) %>%
        #         dplyr::mutate(deLab = ifelse(
        #             quantile(abs(logFC), 0.999, na.rm = TRUE) < abs(logFC), Target.name,
        #             NA
        #         ))
        # })
        volcanoDF <- lapply(seq_len(ncol(contrast())), function(i) {
                limma::topTable(efit(), coef = i, number = Inf) %>%
                    tibble::rownames_to_column(var = "Target.name") %>%
                    dplyr::select("Target.name", "logFC", "adj.P.Val") %>%
                    dplyr::mutate(de = ifelse(logFC >= input$logFCcutoff &
                        adj.P.Val < input$PvalCutoff, "UP",
                    ifelse(logFC <= -(input$logFCcutoff) &
                        adj.P.Val < input$PvalCutoff, "DN",
                    "NO"
                    )
                    )) %>%
                    dplyr::mutate(deLab = dplyr::case_when(
                        logFC > quantile(logFC[logFC > 0], 0.9, na.rm = TRUE) ~ Target.name,  # Above 90th percentile of positive values
                        logFC < quantile(logFC[logFC < 0], 0.9, na.rm = TRUE) ~ Target.name,  # Below 90th percentile of negative values
                        TRUE ~ NA_character_                                             # Otherwise NA
                    )

                    )
            })


        # If not using custom range, determine maximum absolute FC for each plot
        if (input$toggleCustomRange == FALSE) {
            plots <- list()

            plots <- lapply(seq_along(volcanoDF), function(i) {
                .volcanoFunction(
                    volcanoDF[[i]], input$delabSize, input$maxOverlap,
                    colnames(contrast())[i],
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
                    colnames(contrast())[i],
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

    names(plots) <- names(topTabDF())

    return(plots)
})
# nocov end
