.outputVolcanoNavPanel <- function(input, output, session, rv){
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
            "-log P value", value = 40
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

    tabsets <- lapply(names(rv$volcano()), function(name){
        shiny::tabPanel(name,
                 shiny::plotOutput(outputId = paste0("volcano_", name)),


                 shiny::fluidRow(
                 lapply(c("png","tiff","pdf","svg"), function(ext){

                shiny::column(3,
                     shiny::downloadButton(paste0("downloadVolcano", name, ext), paste(toupper(ext)))
                ) })))})


    shiny::tabsetPanel(
        type = "tabs",
        !!!tabsets
    )
})


shiny::observeEvent(input$generateVolcano,{
lapply(names(rv$volcano()), function(name) {



lapply(c("png", "tiff", "pdf", "svg"), function(ext) {
    output[[paste0("downloadVolcano", name, ext)]] <- downloadHandler(
        filename = function() {
            paste("volcano", name, ext, sep = ".")
        },
        content = function(file) {
            rv$
            ggsave(file,

                   rv$volcano()[[name]],



                   height = 4, width = 6, units = c("in"),
                   device = ext)
        }
    )
})
})

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
