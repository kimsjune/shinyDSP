.outputSetupNavPanel <- function(input, output, session, rv) {



    # nocov start
    output$countFile <- shiny::renderUI({
        shiny::validate(
            shiny::need(
                shiny::isTruthy(input$load) & shiny::isTruthy(rv$data()),


                "'Use demo data or upload your own, and hit 'load'!"
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
