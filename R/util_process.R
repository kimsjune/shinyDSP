.data <- function(input, output, session, rv) {
    # nocov
    data <- shiny::eventReactive(input$load, {
        shiny::validate(
            shiny::need(
                shiny::isTruthy(input$useSampleData) ||
                    shiny::isTruthy(input$uploadedCountFile),
                "Use demo data OR upload your own!"
            )
        )

        data <- list()

        if (input$useSampleData == TRUE) {
            eh <- ExperimentHub()
            AnnotationHub::query(eh, "standR")
            countFilePath <- eh[["EH7364"]]
            sampleAnnoFilePath <- eh[["EH7365"]]

            ## do NOT set row.names = 1
            ## because 'readGeoMx() expects "TargetName" column
            ## do NOT use read.delim() because sample names contain |
            ## which are not
            ## converted well. These strings must match with SegmentDisplayName
            ## column
            ## in sampleAnnoFile which retain | characters...
            countFile <- readr::read_delim(unname(countFilePath),
                na = character()
            )
            sampleAnnoFile <- readr::read_delim(unname(sampleAnnoFilePath),
                na = character()
            )
        } else {
            countFile <- readr::read_delim(
                unname(input$uploadedCountFile$datapath)
            ) %>% replace(is.na(.), 0)
            sampleAnnoFile <- readr::read_delim(
                unname(input$uploadedSampleAnnoFile$datapath)
            )
        }

        data[[1]] <- as.data.frame(countFile)
        data[[2]] <- as.data.frame(sampleAnnoFile)

        names(data) <- c("countFile", "sampleAnnoFile")

        return(data)
    })
    return(data)
}
# nocov end

.new_sampleAnnoFile <- function(input, output, session, rv) {
    new_sampleAnnoFile <- shiny::eventReactive(input$selectedExpVar, {
        req(input$selectedExpVar)

        ExpVar <- paste0(input$selectedExpVar, collapse = "_")


        if (length(input$selectedExpVar) > 1) {
            new_sampleAnnoFile <- rv$data()$sampleAnnoFile %>%
                tidyr::unite(
                    !!ExpVar,
                    input$selectedExpVar,
                    sep = "_"
                )
        } else {
            new_sampleAnnoFile <- rv$data()$sampleAnnoFile
        }
        return(new_sampleAnnoFile)
    })
    return(new_sampleAnnoFile)
}

.spe <- function(input, output, session, rv) {
    # nocov start
    spe <- shiny::eventReactive(c(input$selectedTypes), {
        spe <- standR::readGeoMx(
            rv$data()[[1]],
            # data()[[2]]
            rv$new_sampleAnnoFile()
        )
        selectedTypes <- input$selectedTypes
        selectedExpVar <- paste0(input$selectedExpVar, collapse = "_")

        test <- colData(spe) %>%
            tibble::as_tibble() %>%
            pull(!!selectedExpVar)

        spe <- spe[, grepl(paste(selectedTypes, collapse = "|"), test)]

        # if (input$enableQC) {
        # qc <- colData(spe)$AlignedReads/colData(spe)$RawReads >=0.9 & colData(spe)$SequencingSaturation >=90
        #
        # spe <- spe[,qc]
        # }

        return(spe)
    })
    # nocov end
    return(spe)
}
