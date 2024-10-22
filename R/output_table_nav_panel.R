.outputTableNavPanel <- function(input, output, session, rv) {
    # nocov start
    output$topTable <- shiny::renderUI({
        print("Rendering topTable UI")

        print(rv$topTabDF())
        # shiny::validate(
        #     shiny::need(
        #         shiny::isTruthy(input$load) &
        #             shiny::isTruthy(input$selectedTypes) &
        #             shiny::isTruthy(input$selectedNorm),
        #         "'Load' data, 'choose types', select 'normalization', then hit 'run'!"
        #     )
        # )
        # DT::renderDT(
        #     topTabDF() %>%
        #         DT::datatable() %>%
        #         DT::formatSignif(columns = c(3:ncol(topTabDF())), digits = 4)
        # )

        lapply(names(rv$topTabDF()), function(name) {
            output[[paste0("table_", name)]] <- DT::renderDataTable({
                rv$topTabDF()[[name]] %>%
                    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ ifelse(abs(.) < 1,
                                                                                   formatC(., format = "e", digits = 3),  # Scientific notation for abs < 1
                                                                                   formatC(., format = "f", digits = 3)))) %>%
                    DT::datatable()
            })
            output[[paste0("downloadTable_", name)]] <- shiny::downloadHandler(
                filename = function() {
                    paste(name, "csv", sep = ".")
                },
                content = function(file) {
                    utils::write.csv(rv$topTabDF()[[name]], file, row.names = FALSE)
                }
            )
        }
        )

        tabsets <- lapply(names(rv$topTabDF()), function(name){
            shiny::tabPanel(name,
                     DT::dataTableOutput(outputId = paste0("table_",name)),
                     shiny::downloadButton(outputId = paste0("downloadTable_", name), label = "Download table")


                     )
        })

        shiny::tabsetPanel(
            type = "tabs",
            !!!tabsets
        )
    })



}
