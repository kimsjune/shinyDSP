.design <- function(input, output, session, rv) {
    # nocov start
    design <- shiny::reactive({
        shiny::validate(
            shiny::need(
                shiny::isTruthy(input$selectedNorm) &
                    shiny::isTruthy(input$selectedExpVar) &
                    shiny::isTruthy(input$selectedTypes),
                "Select variables/groups of interest and normalization"
            )
        )

        spe <- switch(input$selectedNorm,
            "CPM" = rv$speCpm(),
            "Q3" = rv$speQ3(),
            "RUV4" = rv$speRuv()
        )




        ExpVar <- paste0(input$selectedExpVar, collapse = "_")


        preFormula <- list()


        preFormula <- lapply(seq_along(input$selectedConfounders), function(i) {
            input$selectedConfounders[i]
        })

        preFormula <- c("~0", ExpVar, preFormula)

        if (input$selectedNorm == "rv$speRuv()") {
            for (i in seq_along(input$k)) {
                preFormula[[i + length(preFormula)]] <- paste0("ruv_W", i)
            }
        }

        formula <- gsub(" ", " + ", paste(preFormula, collapse = " "))
        design <- stats::model.matrix(eval(parse(text = formula)),
            data = SummarizedExperiment::colData(spe)
        )

        if (!limma::is.fullrank(design)) {
            formula <- gsub(" ", " + ", paste(preFormula[
                c(1, 2, tail(preFormula, length(input$k)))
            ], collapse = " "))
            design <- stats::model.matrix(eval(parse(text = formula)),
                data = SummarizedExperiment::colData(spe)
            )
        }

        colnames(design) <- gsub(ExpVar, "", colnames(design))
        colnames(design) <- gsub(input$selectedBatch, "", colnames(design))
        colnames(design) <- gsub(" ", "_", colnames(design))

        return(design)
    })
    # nocov end
    return(design)
}

.dge <- function(input, output, session, rv) {
    # nocov start
    dge <- shiny::eventReactive(rv$design(), {
        shiny::withProgress(message = "Creating a DGEList object...", {
            spe <- switch(input$selectedNorm,
                "CPM" = rv$speCpm(),
                "Q3" = rv$speQ3(),
                "RUV4" = rv$speRuv()
            )

            dge <- edgeR::SE2DGEList(spe)

            shiny::incProgress(1 / 4)

            keep <- edgeR::filterByExpr(dge, rv$design())


            dge <- dge[keep, , keep.lib.sizes = FALSE]
            shiny::incProgress(2 / 4)


            dge <- edgeR::estimateDisp(dge, design = rv$design(), robust = TRUE)
            shiny::incProgress(3 / 4)
        })
        return(dge)
    })
    # nocov end
    return(dge)
}


.contrast <- function(input, output, session, rv) {
    # nocov start
    contrast <- shiny::eventReactive(rv$design(), {
        # replace space with _
        selectedTypes_underscore <- gsub(" ", "_", input$selectedTypes)

        comparisons <- list()

        comparisons <- lapply(
            seq_len(choose(length(selectedTypes_underscore), 2)),
            function(i) {
                noquote(
                    paste0(
                        utils::combn(selectedTypes_underscore, 2,
                            simplify = FALSE
                        )[[i]][1],
                        "-",
                        utils::combn(selectedTypes_underscore, 2,
                            simplify = FALSE
                        )[[i]][2]
                    )
                )
            }
        )

        con <- limma::makeContrasts(
            # Must use as.character()
            contrasts = as.character(unlist(comparisons)),
            levels = colnames(rv$design())
        )

        colnames(con) <- sub("-", "_vs_", colnames(con))

        return(con)
    })
    # nocov end
    return(contrast)
}

.efit <- function(input, output, session, rv) {
    # nocov start
    efit <- shiny::eventReactive(rv$dge(), {
        shiny::req(input$selectedBatch, rv$contrast())

        shiny::withProgress(
            message = "Performing differential gene expression analysis...",
            {
                spe <- switch(input$selectedNorm,
                    "CPM" = rv$speCpm(),
                    "Q3" = rv$speQ3(),
                    "RUV4" = rv$speRuv()
                )

                block_by <- SummarizedExperiment::colData(spe)[[
                    input$selectedBatch
                ]]

                v <- limma::voom(rv$dge(), rv$design())
                corfit <- limma::duplicateCorrelation(v, rv$design(),
                    block = block_by
                )

                shiny::incProgress(1 / 5)

                v2 <- limma::voom(rv$dge(), rv$design(),
                    block = block_by,
                    correlation = corfit$consensus
                )
                corfit2 <- limma::duplicateCorrelation(v, rv$design(),
                    block = block_by
                )

                shiny::incProgress(2 / 5)

                fit <- limma::lmFit(v, rv$design(),
                    block = block_by,
                    correlation = corfit2$consensus
                )

                shiny::incProgress(3 / 5)

                fit_contrast <- limma::contrasts.fit(fit,
                    contrasts = rv$contrast()
                )
                efit <- limma::eBayes(fit_contrast, robust = TRUE)

                shiny::incProgress(4 / 5)
            }
        )

        return(efit)
    })
    # nocov end
    return(efit)
}
