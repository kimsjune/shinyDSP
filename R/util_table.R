# nocov start
topTabDF <- shiny::reactive({
    # If there are more than two groups, must sort by F, not P...
    # but it might sort by default? could be redundant

    if (length(input$selectedTypes) > 2) {
        dt <- limma::topTable(efit(),
            coef = seq_len(ncol(contrast())), number = Inf,
            p.value = 0.05, sort.by = "F", adjust.method = "BH",
            lfc = input$lfc
        ) %>%
            tibble::rownames_to_column(var = "Gene") # %>%
        # dplyr::select(!c(
        #   "ProbeName", "GeneID", "HUGOSymbol", "ProbeDisplayName",
        #   "Accessions", "GenomeBuild", "AnalyteType", "CodeClass",
        #   "ProbePool", "TargetGroup"
        # ))
    } else {
        dt <- limma::topTable(efit(),
            coef = seq_len(ncol(contrast())), number = Inf,
            p.value = 0.05, sort.by = "P", adjust.method = "BH",
            lfc = input$lfc
        ) %>%
            tibble::rownames_to_column(var = "Gene") # %>%
        # dplyr::select(!c(
        #   "ProbeName", "GeneID", "HUGOSymbol", "ProbeDisplayName",
        #   "Accessions", "GenomeBuild", "AnalyteType", "CodeClass",
        #   "ProbePool", "TargetGroup"
        # ))
    }
    return(dt)
})
# nocov end
