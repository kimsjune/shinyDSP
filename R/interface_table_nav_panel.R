.interfaceTableNavPanel <- function() {
    bslib::nav_panel(
        "Table",
        bslib::layout_sidebar(



        shiny::uiOutput("table") %>% shinycssloaders::withSpinner(type = 4),
        shiny::downloadButton("downloadTable", "Save table")
    )
    )
}
