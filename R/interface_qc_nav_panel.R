.interfaceQcNavPanel <- function(output) {
    bslib::nav_panel(
        "QC",
        value = "QC",
        bslib::layout_sidebar(
            sidebar = bslib::accordion(

                bslib::accordion_panel(
                "",
                shiny::actionButton(inputId = "generateQc","Show QC plots"),
                shiny::uiOutput("qcSelect"),
                shiny::numericInput(inputId = "nQcPlots", "# of plots",
                                    value = 1,
                                    min = 1),
                shiny::uiOutput("qcColBy"),
                shiny::uiOutput("qcColPal"),
                shiny::uiOutput("qcCutoffUI")
                )



            ),



                shiny::uiOutput("qcPlot") %>%
                    shinycssloaders::withSpinner(type = 4)



        )
    )

}
