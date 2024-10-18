#' Creates the "sidebar" UI element
#'
#' @return [bslib::sidebar()]
#' @keywords internal
#'
#' @author Seung J. Kim
.interfaceSidebar <- function() {
  bslib::sidebar(
    bslib::accordion(
      open = TRUE,
      multiple = TRUE,
      bslib::accordion_panel(
        "",

    # shinyWidgets::prettySwitch(
    #   inputId = "useSampleData",
    #   label = "Use demo data",
    #   value = FALSE,
    #   fill = TRUE
    # ),
    # shinyjs::hidden(
    #   shiny::fileInput(
    #     inputId = "uploadedFile",
    #     label = "Upload your file",
    #     accept = c(".xlsx")
    #   )
    # ),
    # shiny::actionButton(
    #   inputId = "load",
    #   "Load",
    #   style = "display: inline-block; padding: 4px"
    # ),
    shiny::uiOutput("selectYourExpVar") %>% shinycssloaders::withSpinner(
      type = 4, size = 0.5, proxy.height = 75
    ),
    shiny::uiOutput("selectYourType") %>% shinycssloaders::withSpinner(
      type = 4, size = 0.5, proxy.height = 75
    ),
    shiny::uiOutput("selectYourBatch") %>% shinycssloaders::withSpinner(
      type = 4, size = 0.5, proxy.height = 75
    ),
    shiny::uiOutput("selectYourConfounder") %>% shinycssloaders::withSpinner(
      type = 4, size = 0.5, proxy.height = 75
    ),
    # shiny::actionButton(
    #   inputId = "run",
    #   "Run",
    #   style = "display: inline-block; padding: 4px"
    # )
    ),
    bslib::accordion_panel(
      "",

    shiny::uiOutput("selectYourNorm") %>% shinycssloaders::withSpinner(
      type = 4, size = 0.5, proxy.height = 75
      ),
    shiny::uiOutput("selectYourK") %>% shinycssloaders::withSpinner(
      type = 4, size = 0.5, proxy.height = 75
    ),
    shiny::uiOutput("selectYourLFC")
    )


    )
  )
}
