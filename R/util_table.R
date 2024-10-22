.topTabDF <- function(input, output, session, rv) {
# nocov start
topTabDF <- shiny::eventReactive(rv$efit(),{
    shiny::req(rv$efit(), rv$contrast())
    # If there are more than two groups, must sort by F, not P...
    # but it might sort by default? could be redundant


    topTabDF <- lapply(seq_len(ncol(rv$contrast())), function(i) {
        limma::topTable(rv$efit(), coef = i, number = Inf, p.value  = 0.05,
                        adjust.method = "BH", lfc = input$lfc) %>%
            tibble::rownames_to_column(var = "Gene")
    })

    names(topTabDF) <- colnames(rv$contrast())

    # if (length(input$selectedTypes) > 2) {
    #     dt <- limma::topTable(efit(),
    #         coef = seq_len(ncol(contrast())), number = Inf,
    #         p.value = 0.05, sort.by = "F", adjust.method = "BH",
    #         lfc = input$lfc
    #     ) %>%
    #         tibble::rownames_to_column(var = "Gene") # %>%
    #     # dplyr::select(!c(
    #     #   "ProbeName", "GeneID", "HUGOSymbol", "ProbeDisplayName",
    #     #   "Accessions", "GenomeBuild", "AnalyteType", "CodeClass",
    #     #   "ProbePool", "TargetGroup"
    #     # ))
    # } else {
    #     dt <- limma::topTable(efit(),
    #         coef = seq_len(ncol(contrast())), number = Inf,
    #         p.value = 0.05, sort.by = "P", adjust.method = "BH",
    #         lfc = input$lfc
    #     ) %>%
    #         tibble::rownames_to_column(var = "Gene") # %>%
    #     # dplyr::select(!c(
    #     #   "ProbeName", "GeneID", "HUGOSymbol", "ProbeDisplayName",
    #     #   "Accessions", "GenomeBuild", "AnalyteType", "CodeClass",
    #     #   "ProbePool", "TargetGroup"
    #     # ))
    # }
    return(topTabDF)
})
# nocov end
    return(topTabDF)
}
