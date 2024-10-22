.topTabDF <- function(input, output, session, rv) {
# nocov start
topTabDF <- shiny::reactive({
    shiny::req(rv$efit(), rv$contrast())
    # If there are more than two groups, must sort by F, not P...
    # but it might sort by default? could be redundant

    numeric_vector <- seq_len(ncol(rv$contrast()))
    new_list <- as.list(numeric_vector)
    new_list[[length(new_list)+1]] <- numeric_vector

    topTabDF <- lapply(new_list, function(i) {
    # topTabDF <- lapply(seq_len(ncol(rv$contrast())), function(i) {
        limma::topTable(rv$efit(), coef = i, number = Inf, p.value  = 0.05,
                        adjust.method = "BH", lfc = input$lfc) %>%
            tibble::rownames_to_column(var = "Gene")
    })

    names(topTabDF) <- c(colnames(rv$contrast()),
                         colnames(rv$contrast()) %>% stringr::str_split(., "_vs_") %>%
                                      unlist() %>% unique() %>% paste(., collapse = "_vs_")
                         )

    return(topTabDF)
})
# nocov end
    return(topTabDF)
}
