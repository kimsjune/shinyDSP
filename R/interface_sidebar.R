#' Creates the "sidebar" UI element
#'
#' @return [bslib::sidebar()]
#' @keywords internal
#'
#' @author Seung J. Kim
.interfaceSidebar <- function(output) {
  bslib::sidebar(
    bslib::accordion(
      open = TRUE,
      multiple = TRUE,
      bslib::accordion_panel(
        "",

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
