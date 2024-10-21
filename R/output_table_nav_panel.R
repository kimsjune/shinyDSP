.outputTableNavPanel <- function() {
    # nocov start
    output$topTable <- shiny::renderUI({
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

        tabsets <- lapply(names(topTabDF()), function(name){
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
    # nocov end

    # nocov start
    # output$downloadTable <- shiny::downloadHandler(
    #
    #     filename = "table.csv",
    #     content = function(file) {
    #         utils::write.table(topTabDF(), file, sep = ",", row.names = FALSE)
    #     }
    # )
    # nocov end
}
