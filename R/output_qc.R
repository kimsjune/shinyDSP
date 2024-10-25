.outputQcNavPanel2 <- function(input, output, rv) {
    output$qcSelect <- renderUI({
        shiny::req(rv$new_sampleAnnoFile())

        shiny::selectInput(
            inputId = "selectedQc",
            "Select QC variables",
            choices = rv$new_sampleAnnoFile() %>% tibble::as_tibble() %>%
                dplyr::select_if(is.numeric) %>% colnames(),
            selected = NULL,
            multiple = TRUE,
            selectize = TRUE
        )
    })

    output$qcColBy <- renderUI({
        shiny::req(rv$new_sampleAnnoFile())

        shiny::selectInput(
            inputId = "selectedQcColBy",
            "Select group to colour by",
            choices = rv$new_sampleAnnoFile() %>% tibble::as_tibble() %>%
                dplyr::select_if(is.character) %>% colnames(),
            selected = "SlideName",
            multiple = FALSE,
            selectize = TRUE
        )
    })

    output$qcColPal <- renderUI({
        shiny::req(rv$new_sampleAnnoFile())

        shiny::selectInput(
            inputId = "selectedQcColPal",
            "Select colour palette",
            choices = c(
                "alphabet", "alphabet2", "cols25", "glasbey", "watlington"
            ),
            selected = "glasbey",
            multiple = FALSE,
            selectize = TRUE
        )
    })

    shiny::observe({
        output$qcCutoff <- renderUI({
            lapply(input$selectedQc, function(column) {
                shiny::numericInput(paste0("cutoff_", column),
                    label = paste("Cutoff for", column),
                    value = NA
                )
            })
        })
    })



    output$qcPlot <- renderUI({
        shiny::validate(
            shiny::need(
                shiny::isTruthy(input$generateQc) &
                    shiny::isTruthy(length(input$selectedQc) >= 2),
                "Pick QC variables (2 or more) and hit 'show'!"
            )
        )



        ## ggplot2 options
        uniqueCategories <- rv$new_sampleAnnoFile() %>%
            dplyr::pull(input$selectedQcColBy) %>%
            unique() %>%
            as.factor()
        numCategories <- length(uniqueCategories)
        colours <- switch(input$selectedQcColPal,
            "alphabet" = pals::alphabet() %>% sample(numCategories),
            "alphabet2" = pals::alphabet2() %>% sample(numCategories),
            "cols25" = pals::cols25() %>% sample(numCategories),
            "glasbey" = pals::glasbey() %>% sample(numCategories),
            "watlington" = pals::watlington() %>% sample(numCategories)
        )


        plotsAndButtons <- lapply(seq_len(input$nQcPlots), function(i) {
            x <- input$selectedQc[i %% length(input$selectedQc) + 1]
            y <- input$selectedQc[(i + 1) %% length(input$selectedQc) + 1]



            output[[paste0("qcPlot_", i)]] <- shiny::renderPlot({
                .qcFunction(
                    rv$new_sampleAnnoFile(),
                    x, y, input$selectedQcColBy, colours, uniqueCategories
                )
            })

            downloadButtons <- lapply(
                c("png", "tiff", "svg", "pdf"),
                function(ext) {
                    output[[paste0("downloadQc_", i, "_", ext)]] <-
                        shiny::downloadHandler(
                            filename = function() {
                                paste0("qc_", i, ".", ext)
                            },
                            content = function(file) {
                                p <- .qcFunction(
                                    rv$new_sampleAnnoFile(), x, y,
                                    input$selectedQcColBy, colours,
                                    uniqueCategories
                                )
                                ggplot2::ggsave(file, plot = p, device = ext)
                            }
                        )
                    shiny::downloadButton(paste0("downloadQc_", i, "_", ext),
                        label = paste(toupper(ext))
                    )
                }
            )


            shiny::tagList(
                shiny::plotOutput(paste0("qcPlot_", i)),
                do.call(shiny::tagList, downloadButtons)
            )

        })

        return(shiny::tagList(plotsAndButtons))
    })


    ## I want to put this function in a separate file but shinytest2 can't
    ## find it unless its right here. But .PCAFunction() works just fine.
    ## Maybe because that's being called in reactive() instead of renderUI()?
    .qcFunction <- function(data, x, y, selectedQcColBy, colours,
                            uniqueCategories){
        ggplot2::ggplot(
            data,
            ggplot2::aes(!!as.name(x), !!as.name(y),
                         fill = !!as.name(selectedQcColBy)
            )
        ) +
            ggplot2::geom_point(
                shape = 21, size = 3,
                stroke = 0.5
            ) +
            ggplot2::scale_x_continuous(labels = function(x) {
                ifelse(abs(x) > 9999, scales::scientific(x), x)
            }) +
            ggplot2::scale_y_continuous(labels = function(y) {
                ifelse(abs(y) > 9999, scales::scientific(y), y)
            }) +
            ggplot2::scale_fill_manual(
                values =
                    setNames(
                        colours,
                        uniqueCategories
                    )
            ) +
            ggplot2::theme(
                panel.grid.minor = ggplot2::element_blank(),
                panel.grid.major = ggplot2::element_blank(),
                axis.text = ggplot2::element_text(
                    color = "black", size = 16
                ),
                axis.line = ggplot2::element_blank(),
                axis.ticks = ggplot2::element_line(
                    colour = "black"
                ),
                axis.title = ggplot2::element_text(size = 16),
                legend.title = ggplot2::element_text(
                    size = 16, vjust = 0.5, hjust = 0.5,
                    face = "bold", family = "sans"
                ),
                legend.text = ggplot2::element_text(
                    size = 16, vjust = 0.5, hjust = 0,
                    face = "bold", family = "sans"
                ),
                plot.margin = grid::unit(c(1, 1, 1, 1), "mm"),
                plot.background = ggplot2::element_rect(
                    fill = NA,
                    colour = NA
                ),
                plot.title = ggplot2::element_text(
                    size = 16, hjust = 0.5, face = "bold",
                    family = "sans"
                ),
                panel.border = ggplot2::element_rect(
                    colour = "black",
                    ## Good lord if this fill is not set, it
                    ## puts a white box around the entire plot
                    ## area
                    fill = NA,
                    linewidth = 0.4
                ),
                panel.background = ggplot2::element_rect(
                    fill = NA,
                    colour = NA
                ),
                legend.background = ggplot2::element_rect(
                    fill = NA,
                    colour = NA
                ),
                legend.box.background = ggplot2::element_rect(
                    fill = NA,
                    colour = NA
                ),
                legend.key = ggplot2::element_rect(
                    fill = NA,
                    colour = NA
                ),
                aspect.ratio = 1
            )

}

}
