.outputSidebar <- function(session, input, output) {

    shiny::observeEvent(input$load, {
        output$select_your_k <- shiny::renderUI({
            shiny::numericInput(
                inputId = "k",
                "k value for RUV4 norm.",
                value = 2,
                min = 1,
                max = 20
            )
        })
    })

}
