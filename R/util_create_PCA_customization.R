# Opens a grouped widgets to pick shape and colour
.PCAcustomization <- function() {
    shapes_colours_pca <- list()

    # seq_along() counts the total # of elements
    # for (i in seq_along(unique(colData(speQC())$Type))) {
    shapes_colours_pca <- lapply(seq_along(input$selectedTypes), function (i) {

    # for (i in seq_along(input$selectedTypes)) {
    #     shapes_colours_pca[[i]] <-
            # wellPanel( # wellPanel() groups radio and text nicely
            # but it also breaks inline-block style
            htmltools::div(
                # radioButtons(inputId = paste0("shape_",unique(colData(speQC())$Type)[i]),
                shinyWidgets::radioGroupButtons(
                    inputId = paste0("shape_", input$selectedTypes[i]),
                    # For each ROI, I need to address its shape and colour separately

                    # label = paste0("Pick a shape for ",unique(colData(speQC())$Type)[i]),
                    label = paste0("Pick a shape for ", input$selectedTypes[i]),

                    # Each shape is matched to ggplot2 shapes

                    choiceNames = list(

                        tags$img(src = "www/circle.png", height = "24px", width = "24px"),
                        tags$img(src = "www/square.png", height = "24px", width = "24px"),
                        tags$img(src = "www/square.png", height = "20px", width = "20px", style = "rotate: 45deg;"),
                        tags$img(src = "www/triangle.png", height = "24px", width = "24px"),
                        tags$img(src = "www/triangle.png", height = "24px", width = "24px", style = "rotate: 180deg;")

                        ),
                    size = 'sm',
                    justified = TRUE,
                    choiceValues = list(
                        21,
                        22,
                        23,
                        24,
                        25
                    )
                ),
                # textInput(inputId = paste0("colour_",unique(colData(speQC())$Type)[i]),
                shiny::textInput(
                    inputId = paste0("colour_", input$selectedTypes[i]),
                    # label = paste0("Pick a colour for ",unique(colData(speQC())$Type)[i]),
                    label = paste0("Pick a colour for ", input$selectedTypes[i]),
                    value = sample(c(
                        "black", "blue", "pink3", "purple", "orange",
                        "darkgreen", "maroon", "turquoise3"
                    ), 1)
                ),
                style = "display: inline-block;"
            )
    })
        # )
   # }

    shapes_colours_pca # Show the actual widget,
}

# Opens a grouped widgets to pick shape and colour
PCAcustomizationBatch <- shiny::reactive({
    # must use dplyr for this to work...
    batchVars <- data()[[2]] %>%
        dplyr::pull(input$selectedBatch) %>%
        unique()
    # batchVars <- data()[[2]]$input$selectedBatch
    # data <- data()[[2]]
    # selectedBatch <- as.character(input$selectedBatch)
    # batchVars <- unique(data$selectedBatch)
    #  #selectedBatch <- as.character(input$selectedBatch)
    #
    # # batchVars <- c("hu_lymph_node_001", "hu_lymph_node_001b", "hu_lymph_node_002")
    #  batchVars <- data

    # Must be initialized first
    shapes_colours_pca_batch <- list()

    # seq_along() counts the total # of elements
    # for (i in seq_along(unique(colData(speQC())$Type))) {
    shapes_colours_pca_batch <- lapply(seq_along(batchVars), function (i) {

    # for (i in seq_along(batchVars)) {
    #     shapes_colours_pca_batch[[i]] <-
            # wellPanel( # wellPanel() groups radio and text nicely
            # but it also breaks inline-block style
            htmltools::div(
                # radioButtons(inputId = paste0("shape_",unique(colData(speQC())$Type)[i]),
                shinyWidgets::radioGroupButtons(
                    inputId = paste0("shape_", batchVars[i]),
                    # For each ROI, I need to address its shape and colour separately

                    # label = paste0("Pick a shape for ",unique(colData(speQC())$Type)[i]),
                    label = paste0("Pick a shape for ", batchVars[i]),

                    # Each shape is matched to ggplot2 shapes

                    choiceNames = list(
                        tags$img(src = "www/circle.png", height = "24px", width = "24px"),
                        tags$img(src = "www/square.png", height = "24px", width = "24px"),
                        tags$img(src = "www/square.png", height = "20px", width = "20px", style = "rotate: 45deg;"),
                        tags$img(src = "www/triangle.png", height = "24px", width = "24px"),
                        tags$img(src = "www/triangle.png", height = "24px", width = "24px", style = "rotate: 180deg;")


                        ),
                        size = 'sm',
                        justified = TRUE

                    ,
                    choiceValues = list(
                        21,
                        22,
                        23,
                        24,
                        25
                    )
                ),
                # textInput(inputId = paste0("colour_",unique(colData(speQC())$Type)[i]),
                shiny::textInput(
                    inputId = paste0("colour_", batchVars[i]),
                    # label = paste0("Pick a colour for ",unique(colData(speQC())$Type)[i]),
                    label = paste0("Pick a colour for ", batchVars[i]),
                    value = sample(c(
                        "black", "blue", "pink3", "purple", "orange",
                        "darkgreen", "maroon", "turquoise3"
                    ), 1)
                ),
                style = "display: inline-block;"
            )
    })
        # )
    #}

    return(shapes_colours_pca_batch)
})
