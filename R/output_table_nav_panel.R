.outputTableNavPanel2 <- function(input, output, rv) {
    # nocov start
    output$topTable <- shiny::renderUI({
        shiny::req(rv$topTabDF())



        lapply(names(rv$topTabDF()), function(name) {
            output[[paste0("table_", name)]] <- DT::renderDataTable({
                rv$topTabDF()[[name]] %>%
                    dplyr::mutate(dplyr::across(
                        dplyr::where(is.numeric),
                        ~ ifelse(abs(.) < 1,
                            ## Scientific notation for abs < 1
                            formatC(., format = "e", digits = 3),
                            formatC(., format = "f", digits = 3)
                        )
                    )) %>%
                    DT::datatable()
            })
            output[[paste0("downloadTable_", name)]] <- shiny::downloadHandler(
                filename = function() {
                    paste(name, "csv", sep = ".")
                },
                content = function(file) {
                    utils::write.csv(rv$topTabDF()[[name]], file,
                        row.names = FALSE
                    )
                }
            )
        })

        tabsets <- lapply(names(rv$topTabDF()), function(name) {
            shiny::tabPanel(
                name,
                DT::dataTableOutput(outputId = paste0("table_", name)),
                shiny::downloadButton(
                    outputId = paste0("downloadTable_", name),
                    label = "Download table"
                )
            )
        })

        shiny::tabsetPanel(
            type = "tabs",
            !!!tabsets
        )
    })
}
