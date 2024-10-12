.observeEvent_pca_nav_panel <- function(input) {
  # nocov start
  shiny::observeEvent(input$togglePCAcustom, {
    shinyjs::toggle(id = "PCAcustom")
  })
  # nocov end

  # nocov start
  shiny::observeEvent(input$togglePCAcustom, {
    shinyjs::toggle(id = "PCAcustomBatch")
  })
  # nocov end
}
