# nocov start
design <- shiny::reactive({
    shiny::req(input$selectedNorm)
    spe <- eval(parse(text = input$selectedNorm))
    ExpVar <- paste0(input$selectedExpVar, collapse = "_")

    # li <- list()
    # li[[1]] <- "~0"
    # li[[2]] <- ExpVar
    # li[[3]] <- input$selectedBatch

    preFormula <- list()


    preFormula <- lapply(seq_along(input$selectedConfounders), function(i) {
        input$selectedConfounders[i]
    })

    preFormula <- c("~0", ExpVar, preFormula)

    if (input$selectedNorm == "speRUV()") {
        for (i in seq_along(input$k)) {
            preFormula[[i + length(preFormula)]] <- paste0("ruv_W", i)
        }
    }

    formula <- gsub(" ", " + ", paste(preFormula, collapse = " "))
    design <- stats::model.matrix(eval(parse(text = formula)),
        data = SummarizedExperiment::colData(spe)
    )

    if (!limma::is.fullrank(design)) {
        formula <- gsub(" ", " + ", paste(preFormula[c(1, 2, tail(preFormula, length(input$k)))], collapse = " "))
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

# nocov start
dge <- shiny::eventReactive(design(), {
    shiny::withProgress(message = "Creating a DGEList object...", {
        spe <- eval(parse(text = input$selectedNorm))

        dge <- edgeR::SE2DGEList(spe)

        shiny::incProgress(1 / 4)

        keep <- edgeR::filterByExpr(dge, design())


        dge <- dge[keep, , keep.lib.sizes = FALSE]
        shiny::incProgress(2 / 4)


        dge <- edgeR::estimateDisp(dge, design = design(), robust = TRUE)
        shiny::incProgress(3 / 4)
    })
    return(dge)
})
# nocov end

# nocov start
efit <- shiny::eventReactive(dge(), {
    shiny::req(input$selectedBatch, contrast())

    shiny::withProgress(
        message = "Performing differential gene expression analysis...",
        {
            spe <- eval(parse(text = input$selectedNorm))

            block_by <- SummarizedExperiment::colData(spe)[[input$selectedBatch]]

            v <- limma::voom(dge(), design())
            corfit <- limma::duplicateCorrelation(v, design(), block = block_by)

            shiny::incProgress(1 / 5)

            v2 <- limma::voom(dge(), design(), block = block_by, correlation = corfit$consensus)
            corfit2 <- limma::duplicateCorrelation(v, design(), block = block_by)

            shiny::incProgress(2 / 5)

            fit <- limma::lmFit(v, design(), block = block_by, correlation = corfit2$consensus)

            shiny::incProgress(3 / 5)

            fit_contrast <- limma::contrasts.fit(fit, contrasts = contrast())
            efit <- limma::eBayes(fit_contrast, robust = TRUE)

            shiny::incProgress(4 / 5)
        }
    )

    return(efit)
})
# nocov end


# nocov start
contrast <- shiny::eventReactive(design(), {
    # replace space with _
    selectedTypes_underscore <- gsub(" ", "_", input$selectedTypes)

    comparisons <- list()

    comparisons <- lapply(
        seq_len(choose(length(selectedTypes_underscore), 2)),
        function(i) {
            noquote(
                paste0(
                    utils::combn(selectedTypes_underscore, 2, simplify = FALSE)[[i]][1],
                    "-",
                    utils::combn(selectedTypes_underscore, 2, simplify = FALSE)[[i]][2]
                )
            )
        }
    )

    con <- limma::makeContrasts(
        # Must use as.character()
        contrasts = as.character(unlist(comparisons)),
        levels = colnames(design())
    )

    colnames(con) <- sub("-", "_vs_", colnames(con))

    return(con)
})
# nocov end
