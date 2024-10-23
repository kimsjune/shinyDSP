.topTabDF <- function(input, output, session, rv) {
    # nocov start
    topTabDF <- shiny::reactive({
        shiny::req(rv$efit(), rv$contrast())

        numeric_vector <- seq_len(ncol(rv$contrast()))
        new_list <- as.list(numeric_vector)

        if (length(input$selectedTypes) > 2) {
            new_list[[length(new_list) + 1]] <- numeric_vector
        }

        topTabDF <- lapply(new_list, function(i) {
            limma::topTable(rv$efit(),
                coef = i, number = Inf, p.value = 0.05,
                adjust.method = "BH", lfc = input$lfc
            ) %>%
                tibble::rownames_to_column(var = "Gene")
        })


        if (length(input$selectedTypes) > 2) {
            names(topTabDF) <- c(
                colnames(rv$contrast()),
                colnames(rv$contrast()) %>% stringr::str_split(., "_vs_") %>%
                    unlist() %>% unique() %>% paste(., collapse = "_vs_")
            )
        } else {
            names(topTabDF) <- colnames(rv$contrast())
        }

        return(topTabDF)
    })
    # nocov end
    return(topTabDF)
}
