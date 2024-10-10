#' Creates the "sidebar" UI element
#'
#' @return [bslib::sidebar()]
#' @keywords internal
#'
#' @author Seung J. Kim
.interfaceSidebar <- function() {
  bslib::sidebar(
    shinyWidgets::prettySwitch(
      inputId = "useSampleData",
      label = "Use demo data",
      value = FALSE,
      fill = TRUE
    ),
    shinyjs::hidden(
      shiny::fileInput(
        inputId = "uploadedFile",
        label = "Upload your file",
        accept = c(".xlsx")
      )
    ),
    shiny::actionButton(
      inputId = "load",
      "Load",
      style = "display: inline-block; padding: 4px"
    ),
    shiny::uiOutput("selectYourType") %>% shinycssloaders::withSpinner(
      type = 4, size = 0.5, proxy.height = 50
    ),
    shiny::uiOutput("selectYourBatch") %>% shinycssloaders::withSpinner(
      type = 4, size = 0.5, proxy.height = 50
    ),
    shiny::actionButton(
      inputId = "run",
      "Run",
      style = "display: inline-block; padding: 4px"
    ),
    shiny::tags$hr(),
    shiny::uiOutput("selectYourNorm"),
    shiny::uiOutput("selectYourK"),
    shiny::tags$hr(),
    shiny::numericInput(
      inputId = "lfc",
      label = "log2 fold change cutoff",
      value = 1
    )
  )
}
