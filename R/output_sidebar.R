.outputSidebar <- function(input, output) {
  # nocov start
  output$selectYourExpVar <- shiny::renderUI({
    req(data())

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
      choices = data()$sampleAnnoFile %>% dplyr::select(where(is.character))
        %>% colnames(),
      multiple = TRUE,
      selectize = TRUE,
      selected = NULL
    )
  })
  # nocov end

  # nocov start
  output$selectYourType <- shiny::renderUI({
    req(new_sampleAnnoFile(), input$selectedExpVar)


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
      choices = new_sampleAnnoFile() %>% dplyr::pull(!!ExpVar) %>% unique(),
      multiple = TRUE,
      selectize = TRUE,
      selected = NULL
    )
  })

  # nocov end

  # nocov start
  output$selectYourBatch <- shiny::renderUI({
    req(data())

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
      choices = data()$sampleAnnoFile %>% dplyr::select(where(is.character))
        %>% colnames(),
      multiple = FALSE
    )
  })
  # nocov end

  # nocov start
  output$selectYourConfounder <- shiny::renderUI({
    req(data())

    shiny::selectizeInput(
      inputId = "selectedConfounders",
      # label = "Choose sample batch",
      bslib::tooltip(
        trigger = list(
          "Any confounding variables",
          bsicons::bs_icon("info-circle")
        ),
        "For example, sex or age "
      ),
      choices = data()$sampleAnnoFile %>% dplyr::select(where(is.character))
        %>% colnames(),
      multiple = TRUE,
      options = list(
        placeholder = "",
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  })
  # nocov end
  invisible(NULL)
}
