.observeEvent_sidebar <- function(input, output) {
    # nocov start
    observeEvent(input$useSampleData, {
        if (input$useSampleData == FALSE) {
            shinyjs::show("uploadedCountFile")
            shinyjs::show("uploadedSampleAnnoFile")
        } else {
            shinyjs::hide("uploadedCountFile")
            shinyjs::hide("uploadedSampleAnnoFile")
        }
    })
    # nocov end

    # # nocov start
    # shiny::observeEvent(input$load, {
    #     output$selectYourK <- shiny::renderUI({
    #         shiny::req(data())
    #         shiny::numericInput(
    #             inputId = "k",
    #             "k value for RUV4 norm.",
    #             value = 2,
    #             min = 1,
    #             max = 20
    #         )
    #     })
    # })
    # # nocov end
    #
    # # nocov start
    # shiny::observeEvent(input$run, {
    #     output$selectYourNorm <- shiny::renderUI({
    #         shiny::req(data())
    #         shinyWidgets::radioGroupButtons(
    #             inputId = "selectedNorm",
    #             choices = list(
    #                 "CPM" = "speCPM()",
    #                 "Q3" = "speQ3()",
    #                 "RUV4" = "speRUV()"
    #             ),
    #             size = "sm",
    #             justified = TRUE,
    #             label = bslib::tooltip(
    #               trigger = list(
    #                 "Normalization",
    #                 bsicons::bs_icon("info-circle")
    #               ),
    #               "Pick a method for differential gene expression analysis."
    #             ),
    #             selected = character(0)
    #         )
    #     })
    # })
    # # nocov end
}
