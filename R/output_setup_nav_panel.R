.outputSetupNavPanel <- function() {


    # nocov start
    output$countFile <- shiny::renderUI({
        shiny::validate(
            shiny::need(
                shiny::isTruthy(input$load) & shiny::isTruthy(data()),


                "'Use demo data or upload your own, and hit 'load'!"
            )
        )
        DT::renderDT(
            data()$countFile %>%
                dplyr::slice_sample(n = 20) %>%
                DT::datatable()

        )
    })
    # nocov end

    # nocov start
    output$sampleAnnoFile <- shiny::renderUI({
        shiny::req(input$load, data())

        DT::renderDT(
            data()$sampleAnnoFile %>%
                dplyr::slice_sample(n = 20) %>%
                DT::datatable()
        )
    })
    # nocov end
}
