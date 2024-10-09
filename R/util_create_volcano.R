volcano <- shiny::eventReactive(c(input$selectedTypes, input$selectedNorm, input$generateVolcano),{
    shiny::req(efit(), contrast(), input$logFCcutoff, input$PvalCutoff)

    shiny::withProgress(message = "Plotting...", {


    # make data frame
    volcanoDF <- lapply(as.list(seq_len(ncol(contrast()))), function(i) {
        limma::topTable(efit(), coef = i, number = Inf) %>%
            tibble::rownames_to_column(var = "Target.name") %>%
            dplyr::select("Target.name", "logFC", "adj.P.Val") %>%
            dplyr::mutate(de = ifelse(logFC >= input$logFCcutoff & adj.P.Val < input$PvalCutoff, "UP", ifelse(logFC <= -(input$logFCcutoff) & adj.P.Val < input$PvalCutoff, "DN", "NO"))) %>%
            dplyr::mutate(deLab = ifelse(
                quantile(abs(logFC), 0.999, na.rm =TRUE ) < abs(logFC), Target.name, NA) )
    })


        # If not using custom range, determine maximum absolute FC for each plot
        if (input$toggleCustomRange == FALSE) {
            plots <- list()

            plots <- lapply(seq_along(volcanoDF), function(i) {
                    .volcanoFunction(volcanoDF[[i]], input$delabSize, input$maxOverlap, colnames(contrast())[i],
                                      input$logFCcutoff, input$PvalCutoff,
                                      input$DnCol, input$notDEcol, input$UpCol)+
                        xlim(
                            0- max(abs(volcanoDF[[i]] %>% dplyr::select(logFC))),
                            max(abs(volcanoDF[[i]] %>% dplyr::select(logFC)))
                        )
            })



        } else {

            plots <- lapply(seq_along(volcanoDF), function(i) {
                .volcanoFunction(volcanoDF[[i]], input$delabSize, input$maxOverlap, colnames(contrast())[i],
                                 input$logFCcutoff, input$PvalCutoff,
                                 input$DnCol, input$notDEcol, input$UpCol)+
                            xlim(
                                input$customX[1],
                                input$customX[2]
                            )+
                            ylim(
                                0, input$customY
                            )
            })

        }
    })


    return(plots)
})

