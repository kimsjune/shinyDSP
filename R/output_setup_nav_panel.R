.outputSetupNavPanel <- function(input, output, rv) {
    # nocov start
    output$countFile <- shiny::renderUI({
        shiny::validate(
            shiny::need(
                shiny::isTruthy(input$useSampleData) ||
                    shiny::isTruthy(input$uploadedCountFile),
                "'Use demo data' or upload your own!"
            )
        )
        DT::renderDT(
            rv$data()$countFile %>%
                dplyr::slice_sample(n = 20) %>%
                DT::datatable()
        )
    })
    # nocov end



    # nocov start
    output$sampleAnnoFile <- shiny::renderUI({
        shiny::req(input$load, rv$data())

        DT::renderDT(
            rv$data()$sampleAnnoFile %>%
                dplyr::slice_sample(n = 20) %>%
                DT::datatable()
        )
    })
    # nocov end
}
