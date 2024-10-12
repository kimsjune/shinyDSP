.observeEvent_sidebar <- function(input, output) {
  # nocov start
  observeEvent(input$useSampleData, {
    if (input$useSampleData == FALSE) {
      shinyjs::show("uploadedFile")
    } else {
      shinyjs::hide("uploadedFile")
    }
  })
  # nocov end

  # nocov start
  shiny::observeEvent(input$load, {
    output$selectYourK <- shiny::renderUI({
      shiny::numericInput(
        inputId = "k",
        "k value for RUV4 norm.",
        value = 2,
        min = 1,
        max = 20
      )
    })
  })
  # nocov end

  # nocov start
  shiny::observeEvent(input$run, {
    output$selectYourNorm <- shiny::renderUI({
      shinyWidgets::radioGroupButtons(
        inputId = "selectedNorm",
        choices = list(
          "CPM" = "speCPM()",
          "Q3" = "speQ3()",
          "RUV4" = "speRUV()"
        ),
        size = "sm",
        justified = TRUE,
        selected = "RUV4",
        label = "Normalization"
      )
    })
  })
  # nocov end
}
