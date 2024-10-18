.outputVolcanoNavPanel <- function(){
# nocov start
output$customRange <- shiny::renderUI({
    htmltools::div(
        shiny::sliderInput(
            inputId = "customX",
            "logFC", min = -10, max = 10, dragRange = FALSE,
            value = c(-4, 4)
        ),
        shiny::numericInput(
            inputId = "customY",
            "-log P value", value = 10
        )
    )
})
# nocov end

# nocov start
output$volcanoUI <- shiny::renderUI({
    shiny::validate(
        shiny::need(
            shiny::isTruthy(input$load) &
                shiny::isTruthy(input$selectedTypes) &
                shiny::isTruthy(input$selectedNorm) &
                shiny::isTruthy(input$generateVolcano),
            "Press 'update'!"
        )
    )

    # Plots as a list can be grouped into a grid and output
    shiny::renderPlot(
        # grid.arrange(grobs = plots)
        cowplot::plot_grid(
            plotlist = volcano(), ncol = 1, align = "v",
            axis = "lr"
        ),
        height = shiny::reactive(350 * ncol(contrast()))
    )
})
# nocov end

# nocov start
# output$downloadVolcano <- shiny::downloadHandler(
#   filename = function() {
#     paste0("volcano_", Sys.Date(), ".png")
#   },
#   content = function(file) {
#     cowplot::save_plot(file, volcanoPlots())
#   },
#   contentType = "image/png"
# )
# no cov

}
