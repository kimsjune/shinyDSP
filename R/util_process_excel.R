# nocov
data <- shiny::eventReactive(input$load, {
  shiny::validate(
    shiny::need(
      shiny::isTruthy(input$useSampleData) ||
        shiny::isTruthy(input$uploadedFile),
      "Use demo data OR upload your own!"
    )
  )

  data <- list()

  if (input$useSampleData == TRUE) {
    eh <- ExperimentHub()
    AnnotationHub::query(eh, "standR")
    countFilePath <- eh[["EH7364"]]
    sampleAnnoFilePath <- eh[["EH7365"]]
    #featureAnnoFilePath <- eh[["EH7366"]]

    # do NOT set row.names = 1 because 'readGeoMx() expects "TargetName" column
    # do NOT use read.delim() because sample names contain | which are not
    # converted well. These strings must match with SegmentDisplayName column
    # in sampleAnnoFile which retain | characters...
    countFile <- readr::read_delim(unname(countFilePath))
    sampleAnnoFile <- readr::read_delim(unname(sampleAnnoFilePath))
    #featureAnnoFile <- read.delim(unname(featureAnnoFilePath))


  } else {
    # segProp <- readxl::read_excel(input$uploadedFile$datapath,
    #   sheet = "SegmentProperties"
    # )
    # bioprob <- readxl::read_excel(input$uploadedFile$datapath,
    #   sheet = "BioProbeCountMatrix"
    # )
    countFile <- readxl::read_excel(input$countFile$datapath)
    sampleAnnoFile <- readxl::read_excel(input$sampleAnnoFile$datapath)
    #featureAnnoFile <- readxl::read_excel(input$featureAnnoFile$datapath)

  }

  # countFile <- as.data.frame(bioprob[, c(3, 13:(dim(bioprob)[2] - 2))])
  # sampleAnnoFile <- as.data.frame(segProp)
  # featureAnnoFile <- as.data.frame(bioprob[, seq_len(12)])

  data[[1]] <- as.data.frame(countFile)
  data[[2]] <- as.data.frame(sampleAnnoFile)

  names(data) <- c("countFile","sampleAnnoFile")
  #data[[3]] <- featureAnnoFile
  #data[[4]] <- sampleAnnoFile[,ncol(sampleAnnoFile)]
  #data[[5]] <- colnames(sampleAnnoFile)

  return(data)
})
# nocov end

new_sampleAnnoFile <- shiny::eventReactive(input$selectedExpVar,{
  req(input$selectedExpVar)

  ExpVar <- paste0(input$selectedExpVar, collapse = "_")


  if (length(input$selectedExpVar) > 1) {
    new_sampleAnnoFile <- data()$sampleAnnoFile %>%
      tidyr::unite(
        !!ExpVar,
        input$selectedExpVar,
        sep = "_"
      )
  } else {
    new_sampleAnnoFile <- data()$sampleAnnoFile
  }
  return(new_sampleAnnoFile)
})

# nocov start
spe <- shiny::eventReactive(c(input$run, input$selectedTypes, input$selectedExpVar), {
  spe <- standR::readGeoMx(
    data()[[1]],
   # data()[[2]]
   new_sampleAnnoFile()
  )
  selectedTypes <- input$selectedTypes
  selectedExpVar <- paste0(input$selectedExpVar, collapse = "_")

  test <- colData(spe) %>% tibble::as_tibble() %>% pull(!!selectedExpVar)

  spe <- spe[, grepl(paste(selectedTypes, collapse = "|"), test)]

  # if (input$enableQC) {
  # qc <- colData(spe)$AlignedReads/colData(spe)$RawReads >=0.9 & colData(spe)$SequencingSaturation >=90
  #
  # spe <- spe[,qc]
  # }

  return(spe)
})
# nocov end
