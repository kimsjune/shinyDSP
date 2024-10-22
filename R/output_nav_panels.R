.outputNavPanels <- function(input, output, session, rv) {
    shiny::observe({
        if (is.null(input$selectedTypes)){
            bslib::nav_hide("navpanel", "PCA")
        } else {
            bslib::nav_show("navpanel", "PCA")
        }
    }
    )
    shiny::observe({
        if (is.null(input$selectedNorm) |
            is.null(input$selectedExpVar) |
            is.null(input$selectedTypes)){
            bslib::nav_hide("navpanel", "Table")
            bslib::nav_hide("navpanel", "Volcano")
            bslib::nav_hide("navpanel", "Heatmap")
        } else {
            bslib::nav_show("navpanel", "Table")
            bslib::nav_show("navpanel", "Volcano")
            bslib::nav_show("navpanel", "Heatmap")
        }
    }
    )

}
