# app.R

#library(shiny)

# Source UI and server files
#source("R/ui.R")      # Define the UI
#source("R/server.R")  # Define the server logic






#' Opens the shiny app in a web browser
#'
#' @return A Shiny app object
#' @export
#'
#' @examples
#' app <- build_app()
#' if (interactive()) shiny::runApp(app)
build_app <- function() {
    ui <- .build_ui()         # Call the function to build UI
    server <- .build_server()  # Call the function to build server
    app <- shiny::shinyApp(ui = ui, server = server)  # Create the app
    return(app)
}
