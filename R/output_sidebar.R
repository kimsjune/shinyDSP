.outputSidebar <- function(input, output, session, rv) {
    # nocov start
    observeEvent(input$useSampleData, {
        if (input$useSampleData == FALSE) {
            shinyjs::show("uploadedCountFile")
            shinyjs::show("uploadedSampleAnnoFile")
        } else {
            shinyjs::hide("uploadedCountFile")
            shinyjs::hide("uploadedSampleAnnoFile")
        }
    })
    # nocov end


    # nocov start
    output$selectYourExpVar <- shiny::renderUI({
        shiny::req(rv$data())

        shiny::selectInput(
            inputId = "selectedExpVar",
            # label = "Choose the main variable",
            label = bslib::tooltip(
                trigger = list(
                    "Variable(s) of interest",
                    bsicons::bs_icon("info-circle")
                ),
                "Pick column(s) that contain biological variables such as genotype and/or
          treatment."
            ),
            choices = rv$data()$sampleAnnoFile %>% dplyr::select(dplyr::where(is.character))
                %>% colnames(),
            multiple = TRUE,
            selectize = TRUE,
            selected = NULL
        )
    })
    # nocov end

    # nocov start
    output$selectYourType <- shiny::renderUI({
        shiny::req(rv$new_sampleAnnoFile(), input$selectedExpVar)


        ExpVar <- paste0(input$selectedExpVar, collapse = "_")

        shiny::selectInput(
            inputId = "selectedTypes",
            # label = "Choose groups to analyze",
            label = bslib::tooltip(
                trigger = list(
                    "Groups of interest",
                    bsicons::bs_icon("info-circle")
                ),
                "Pick groups you want to compare such as 'WT' and 'Mutant'"
            ),
            # choices = data()$sampleAnnoFile %>% pull(input$selectedExpVar) %>% unique(),
            choices = rv$new_sampleAnnoFile() %>% dplyr::pull(!!ExpVar) %>% unique(),
            multiple = TRUE,
            selectize = TRUE,
            selected = NULL
        )
    })

    # nocov end

    # nocov start
    output$selectYourBatch <- shiny::renderUI({
        shiny::req(rv$data())

        shiny::selectInput(
            inputId = "selectedBatch",
            # label = "Choose sample batch",
            bslib::tooltip(
                trigger = list(
                    "A batch variable",
                    bsicons::bs_icon("info-circle")
                ),
                "For example, sample preparation date "
            ),
            choices = rv$data()$sampleAnnoFile %>% dplyr::select(dplyr::where(is.character))
                %>% colnames(),
            multiple = FALSE
        )
    })
    # nocov end

    # nocov start
    output$selectYourConfounder <- shiny::renderUI({
        shiny::req(rv$data())

        shiny::selectizeInput(
            inputId = "selectedConfounders",
            # label = "Choose sample batch",
            bslib::tooltip(
                trigger = list(
                    "Confounding variables",
                    bsicons::bs_icon("info-circle")
                ),
                "For example, sex or age "
            ),
            choices = rv$data()$sampleAnnoFile %>% dplyr::select(dplyr::where(is.character))
                %>% colnames(),
            multiple = TRUE,
            selected = NULL,
            options = list(
                placeholder = "",
                onInitialize = I('function() { this.setValue(""); }')
            )
        )
    })
    # nocov end

    # nocov start

        output$selectYourK <- shiny::renderUI({
            shiny::req(rv$data())
            shiny::numericInput(
                inputId = "k",
                "k value for RUV4 norm.",
                value = 2,
                min = 1,
                max = 20
            )
        })

    # nocov end

    # nocov start

        output$selectYourNorm <- shiny::renderUI({
            shiny::req(rv$data())
            shinyWidgets::radioGroupButtons(
                inputId = "selectedNorm",
                choices = list(
                    "CPM" = "CPM",
                    "Q3" = "Q3",
                    "RUV4" = "RUV4"
                ),
                size = "sm",
                justified = TRUE,
                label = bslib::tooltip(
                    trigger = list(
                        "Normalization",
                        bsicons::bs_icon("info-circle")
                    ),
                    "Pick a method for differential gene expression analysis."
                ),
                selected = character(0)
            )
        })

    # nocov end

    # nocov start
        output$selectYourLFC <- shiny::renderUI({
            shiny::req(rv$data())

            shiny::numericInput(
                inputId = "lfc",
                label = "log2 fold change cutoff",
                value = 1
            )
        })
        # nocov end

}
