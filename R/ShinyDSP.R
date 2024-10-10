#' Opens the shiny app in a web browser
#'
#' @return A [shiny::shinyApp()] object
#' @export
#'
#' @author Seung J. Kim
#'
#' @examples
#' library(ShinyDSP)
#' app <- ShinyDSP()
#' if (interactive()) {
#'     shiny::runApp(app)
#' }
ShinyDSP <- function() {
  ui <- .build_ui()
  server <- .build_server()
  app <- shiny::shinyApp(ui, server)
}
