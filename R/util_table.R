.topTabDF <- function(input, output, session, rv) {
# nocov start
topTabDF <- shiny::reactive({
    shiny::req(rv$efit(), rv$contrast())
    # If there are more than two groups, must sort by F, not P...
    # but it might sort by default? could be redundant


    topTabDF <- lapply(seq_len(ncol(rv$contrast())), function(i) {
        limma::topTable(rv$efit(), coef = i, number = Inf, p.value  = 0.05,
                        adjust.method = "BH", lfc = input$lfc) %>%
            tibble::rownames_to_column(var = "Gene")
    })

    names(topTabDF) <- colnames(rv$contrast())

    return(topTabDF)
})
# nocov end
    return(topTabDF)
}
