.pcaCustomization <- function(input, output, session, rv) {
    pcaCustomization <- shiny::eventReactive(input$selectedTypes, {
        shapes_colours_pca <- list()
        shapes_colours_pca <- lapply(seq_along(input$selectedTypes), function(i) {
            htmltools::div(
                shinyWidgets::radioGroupButtons(
                    inputId = paste0("shape_", input$selectedTypes[i]),
                    label = paste0("Pick a shape for ", input$selectedTypes[i]),
                    choiceNames = list(
                        tags$img(
                            src = "www/circle.png", height = "24px",
                            width = "24px"
                        ),
                        tags$img(
                            src = "www/square.png", height = "24px",
                            width = "24px"
                        ),
                        tags$img(
                            src = "www/square.png", height = "20px",
                            width = "20px",
                            style = "rotate: 45deg;"
                        ),
                        tags$img(
                            src = "www/triangle.png", height = "24px",
                            width = "24px"
                        ),
                        tags$img(
                            src = "www/triangle.png", height = "24px",
                            width = "24px",
                            style = "rotate: 180deg;"
                        )
                    ),
                    size = "sm",
                    justified = TRUE,
                    choiceValues = list(
                        21,
                        22,
                        23,
                        24,
                        25
                    )
                ),
                shiny::textInput(
                    inputId = paste0("colour_", input$selectedTypes[i]),
                    label = paste0("Pick a colour for ", input$selectedTypes[i]),
                    value = sample(c(
                        "black", "blue", "pink3", "purple", "orange",
                        "darkgreen", "maroon", "turquoise3"
                    ), 1)
                ),
                style = "display: inline-block;"
            )
        })


        return(shapes_colours_pca) # Show the actual widget,
    })
    return(pcaCustomization)
}

.pcaCustomizationBatch <- function(input, output, session, rv) {
    # nocov start
    pcaCustomizationBatch <- shiny::eventReactive(input$selectedTypes, {
        ExpVar <- paste0(input$selectedExpVar, collapse = "_")

        selectedTypes <- lapply(seq_along(input$selectedTypes), function(i) {
            input$selectedTypes[i]
        })

        # must use dplyr for this to work...
        batchVars <- rv$new_sampleAnnoFile() %>%
            tibble::as_tibble() %>%
            # not sure when to use !! or as.name() OR both
            dplyr::filter(!!as.name(ExpVar) %in% !!selectedTypes) %>%
            dplyr::pull(input$selectedBatch) %>%
            sort() %>%
            unique()


        # Must be initialized first
        shapes_colours_pca_batch <- list()

        shapes_colours_pca_batch <- lapply(seq_along(batchVars), function(i) {
            htmltools::div(
                shinyWidgets::radioGroupButtons(
                    inputId = paste0("shape_", batchVars[i]),
                    # For each ROI, I need to address its shape and colour separately

                    label = paste0("Pick a shape for ", batchVars[i]),
                    choiceNames = list(
                        tags$img(
                            src = "www/circle.png", height = "24px",
                            width = "24px"
                        ),
                        tags$img(
                            src = "www/square.png", height = "24px",
                            width = "24px"
                        ),
                        tags$img(
                            src = "www/square.png", height = "20px",
                            width = "20px",
                            style = "rotate: 45deg;"
                        ),
                        tags$img(
                            src = "www/triangle.png", height = "24px",
                            width = "24px"
                        ),
                        tags$img(
                            src = "www/triangle.png", height = "24px",
                            width = "24px",
                            style = "rotate: 180deg;"
                        )
                    ),
                    size = "sm",
                    justified = TRUE,
                    choiceValues = list(
                        21,
                        22,
                        23,
                        24,
                        25
                    )
                ),
                shiny::textInput(
                    inputId = paste0("colour_", batchVars[i]),
                    label = paste0("Pick a colour for ", batchVars[i]),
                    value = sample(c(
                        "black", "blue", "pink3", "purple", "orange",
                        "darkgreen", "maroon", "turquoise3"
                    ), 1)
                ),
                style = "display: inline-block;"
            )
        })


        return(shapes_colours_pca_batch)
    })
    # nocov end
    return(pcaCustomizationBatch)
}
