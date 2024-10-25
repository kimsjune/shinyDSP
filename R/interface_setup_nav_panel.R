#' Create the "setup" nav panel
#'
#' @return [bslib::nav_panel()]
#' @keywords internal
#'
#' @author Seung J. Kim
.interfaceSetupNavPanel <- function() {
    bslib::nav_panel(
        "Setup",
        value = "Setup",
        bslib::layout_sidebar(
            sidebar = bslib::accordion(
                bslib::accordion_panel(
                    "",
                    shinyWidgets::prettySwitch(
                        inputId = "useSampleData",
                        label = "Use demo data",
                        value = FALSE,
                        fill = TRUE
                    ),
                    shiny::tags$div(
                        style = "text-align: center;",
                        shiny::tags$p("OR")
                    ),
                    shinyjs::hidden(
                        shiny::fileInput(
                            inputId = "uploadedCountFile",
                            label = "Upload count file",
                            buttonLabel = tags$img(
                                src = "www/upload.png", height = "24px",
                                width = "24px"
                            ),
                            accept = c(".csv", ".txt")
                        )
                    ),
                    shinyjs::hidden(
                        shiny::fileInput(
                            inputId = "uploadedSampleAnnoFile",
                            label = "Upload annotation",
                            buttonLabel = tags$img(
                                src = "www/upload.png", height = "24px",
                                width = "24px"
                            ),
                            accept = c(".csv", ".txt"),
                        )
                    ),
                    shiny::actionButton(
                        inputId = "load",
                        "Load data"
                    )
                )
            ),
            bslib::layout_columns(
                # thank god this fixed UI elements from overlapping each other
                fill = FALSE,
                col_widths = c(12, 12),
                shiny::tags$div(
                    shiny::tags$p(
                        "Upload or use demo data, then press 'Load data'.
                    "
                    )
                ),
                shiny::tags$div(
                    shiny::tags$p(
                        "Your count table looks like this:
                    "
                    )
                ),
                shiny::uiOutput("countFile") %>%
                    shinycssloaders::withSpinner(type = 4),
                shiny::tags$div(
                    shiny::tags$p(
                        "Your annotation table looks like this:
                    "
                    )
                ),
                shiny::uiOutput("sampleAnnoFile") %>%
                    shinycssloaders::withSpinner(type = 4)
            )
        )
    )
}
