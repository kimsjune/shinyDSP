data <- shiny::eventReactive(input$load, {
    validate(
        need(isTruthy(input$useSampleData) || isTruthy(input$uploadedFile),
        "Use demo data OR upload your own!")
    )

    data <- list()

    if (input$useSampleData == TRUE) {
    segProp <- readxl::read_excel(
        "inst/extdata/Export1_InitialDataset.xlsx",
        sheet = "SegmentProperties")
    bioprob <- readxl::read_excel(
        "inst/extdata/Export1_InitialDataset.xlsx",
        sheet = "BioProbeCountMatrix")
    } else {

    segProp <- readxl::read_excel(input$uploadedFile$datapath, sheet = "SegmentProperties")
    bioprob <- readxl::read_excel(input$uploadedFile$datapath, sheet = "BioProbeCountMatrix")
    }

    countFile <- as.data.frame(bioprob[, c(3, 13:(dim(bioprob)[2] - 2))])
    sampleAnnoFile <- as.data.frame(segProp)
    featureAnnoFile <- as.data.frame(bioprob[, seq_along(12)])

    data[[1]] <- countFile
    data[[2]] <- sampleAnnoFile
    data[[3]] <- featureAnnoFile
    data[[4]] <- sampleAnnoFile$Type
    data[[5]] <- colnames(sampleAnnoFile)



    return(data)
})


spe <- shiny::eventReactive(c(input$run, input$selectedTypes), {
    spe <- standR::readGeoMx(
        data()[[1]],
        data()[[2]],
        data()[[3]]
    )




    spe <- spe[, grepl(paste(input$selectedTypes, collapse = "|"), spe$Type)]

    # qc <- colData(spe)$AlignedReads/colData(spe)$RawReads >=0.9 & colData(spe)$SequencingSaturation >=90
    #
    # speQC <- spe[,qc]


    return(spe)
})

