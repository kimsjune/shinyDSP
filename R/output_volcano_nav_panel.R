.outputVolcanoNavPanel2 <- function(input, output, rv) {
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
                shiny::isTruthy(input$generateVolcano),
                "Hit 'Show/update'!"
            )
        )


        tabsets <- lapply(names(rv$volcano()), function(name) {
            shiny::tabPanel(
                name,
                shiny::plotOutput(outputId = paste0("volcano_", name)),
                shiny::fluidRow(
                    lapply(c("png", "tiff", "pdf", "svg"), function(ext) {
                        shiny::column(
                            2,
                            shiny::downloadButton(
                                paste0("downloadVolcano", name, ext),
                                paste(toupper(ext))
                            )
                        )
                    })
                )
            )
        })


        shiny::tabsetPanel(
            type = "tabs",
            !!!tabsets
        )
    })


    shiny::observeEvent(input$generateVolcano, {
        lapply(names(rv$volcano()), function(name) {
            lapply(c("png", "tiff", "pdf", "svg"), function(ext) {
                output[[paste0("downloadVolcano", name, ext)]] <-
                    shiny::downloadHandler(
                        filename = function() {
                            paste("volcano", name, ext, sep = ".")
                        },
                        content = function(file) {
                            rv$
                                ggsave(file,
                                rv$volcano()[[name]],
                                height = 4, width = 6, units = c("in"),
                                device = ext
                            )
                        }
                    )
            })
        })
    })

    # nocov start
    shiny::observeEvent(input$toggleCustomRange, {
        shinyjs::toggle("showCustomRange")
    })
    # nocov end

    # nocov start
    shiny::observeEvent(input$generateVolcano, {
        lapply(names(rv$volcano()), function(name) {
            output[[paste0("volcano_", name)]] <- shiny::renderPlot({
                rv$volcano()[[name]]
            })
        })
    })
    # nocov end
}
