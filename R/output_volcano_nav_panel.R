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
            "Hit 'run'!"
        )
    )

    # # Plots as a list can be grouped into a grid and output
    # shiny::renderPlot(
    #     # grid.arrange(grobs = plots)
    #     cowplot::plot_grid(
    #         plotlist = volcano(), ncol = 1, align = "v",
    #         axis = "lr"
    #     ),
    #     height = shiny::reactive(350 * ncol(contrast()))
    # )

    tabsets <- lapply(names(volcano()), function(name){
        tabPanel(name,
                 shiny::plotOutput(outputId = paste0("volcano_", name))


        )
    })

    tabsetPanel(
        type = "tabs",
        !!!tabsets
    )
})

lapply(c("png", "tiff", "pdf", "svg"), function(ext) {
    output[[paste0("downloadVolcano", ext)]] <- downloadHandler(
        filename = function() {
            paste("volcano", ext, sep = ".")
        },
        content = function(file) {

            ggsave(file,
                   plot = cowplot::plot_grid(
                       plotlist = volcano(),


                       align = 'hv', axis = 'tblr', ncol = 3, nrow = 2
                   ),
                   height = 14, width = 14, units = c("in"),
                   device = ext)
        }
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
