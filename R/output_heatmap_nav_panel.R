.outputHeatmapNavPanel <- function(input, output, session, rv) {
    # nocov start
    output$heatmapUI <- shiny::renderUI({

        shiny::validate(
            shiny::need(shiny::isTruthy(input$generateHeatmap),
                        "Hit 'Show/update'!")
        )


        tabsets <- lapply(names(rv$heatmap()), function(name){
            shiny::tabPanel(name,
                            shiny::plotOutput(outputId = paste0("heatmap_", name)),


                            shiny::fluidRow(
                                lapply(c("png","tiff","pdf","svg"), function(ext){

                                    shiny::column(1, style = "text-align: center;",
                                                  shiny::downloadButton(paste0("downloadHeatmap", name, ext), paste(toupper(ext)))
                                    ) }))

            )})


        shiny::tabsetPanel(
            type = "tabs",
            !!!tabsets
        )
    })
    # nocov end

    # nocov start
    shiny::observeEvent(input$generateHeatmap,{
        lapply(names(rv$heatmap()), function(name) {
            output[[paste0("heatmap_", name)]] <- shiny::renderPlot({
                rv$heatmap()[[name]]
            })
        }
        )
    })
    # nocov end

    # nocov start
    shiny::observeEvent(input$generateHeatmap,{
        lapply(names(rv$heatmap()), function(name) {
    lapply(c("png", "tiff", "pdf", "svg"), function(ext) {
        output[[paste0("downloadHeatmap", name, ext)]] <- shiny::downloadHandler(
            filename = function() {
                paste("heatmap", name, ext, sep = ".")
            },
            content = function(file) {
                rv$
                    ggsave(file,

                           rv$heatmap()[[name]],



                           height = 8, width = 8, units = c("in"),
                           device = ext)
            }
        )
    })
        })
    })
    # nocov end


}

