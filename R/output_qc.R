.outputQcNavPanel <- function(input, output, session, rv) {

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
            choices = c("alphabet","alphabet2","glasbey"),


            selected = "glasbey",
            multiple = FALSE,
            selectize = TRUE
        )
    })

    shiny::observeEvent(input$generateQc,{
        shiny::req(input$selectedQc, input$selectedQcColBy)

        output$qcCutoffUI <- renderUI({
            lapply(input$selectedQc, function(column){
                shiny::numericInput(paste0("cutoff_", column),
                                    label = paste("Cutoff for", column),
                                    value = 0)
            })
        })


    output$qcPlot <- renderUI({
        shiny::validate(
            shiny::need(
                shiny::isTruthy(input$selectedQc) &
                    shiny::isTruthy(input$generateQc),
                "Pick QC variables and hit 'show'!"
            )
        )



        ## ggplot2 options
        unique_categories <- rv$new_sampleAnnoFile() %>% dplyr::pull(input$selectedQcColBy) %>% unique() %>% as.factor()
        num_categories <- length(unique_categories)
        colors <- paste0("pals::",input$selectedQcColPal,"(",num_categories,")")

        print(unique_categories)
        print(num_categories)
        print(colors)


    plots <- lapply(1:input$nQcPlots, function(i){
        if (length(input$selectedQc) >= 2 ) {
            x <- input$selectedQc[i %% length(input$selectedQc) + 1]
            y <- input$selectedQc[(i + 1) %% length(input$selectedQc) + 1]

            plotOutputId <- paste0("qcPlot_", i)


            output[[plotOutputId]] <- shiny::renderPlot({
                ggplot2::ggplot(rv$new_sampleAnnoFile() ,
                                ggplot2::aes(!!as.name(x), !!as.name(y),
                                colour = !!as.name(input$selectedQcColBy))) +
                    ggplot2::geom_point() +
                    ggplot2::scale_colour_manual(values = pals::glasbey(7)) +
                    ggplot2::theme(
                        panel.grid.minor = ggplot2::element_blank(),
                        panel.grid.major = ggplot2::element_blank(),
                        axis.text = ggplot2::element_text(color = "black", size = 16),
                        axis.line = ggplot2::element_blank(),
                        axis.ticks = ggplot2::element_line(colour = "black"),
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
                            ## puts a white box around the entire plot area
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
                        # legend.position = "bottom",
                        aspect.ratio = 1
                    )
            })

            return(shiny::plotOutput(plotOutputId, height = 300))



        }
    })

    return(shiny::tagList(plots))

    })

    })

}
