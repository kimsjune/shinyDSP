.outputTableNavPanel <- function(input, output) {
  # nocov start
  output$table <- shiny::renderUI({
    shiny::validate(
      shiny::need(
        shiny::isTruthy(input$load) &
          shiny::isTruthy(input$selectedTypes) &
          shiny::isTruthy(input$selectedNorm) &
          shiny::isTruthy(input$run),
        "'Load' data, 'choose types', select 'normalization', then hit 'run'!"
      )
    )
    DT::renderDT(
      topTabDF() %>%
        DT::datatable() %>%
        DT::formatSignif(columns = c(3:ncol(topTabDF())), digits = 4)
    )
  })
  # nocov end

  # nocov start
  output$downloadTable <- shiny::downloadHandler(
    filename = "output.csv",
    content = function(file) {
      utils::write.table(topTabDF(), file, sep = ",", row.names = FALSE)
    }
  )
  # nocov end
}
