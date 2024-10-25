.data <- function(input, output, session, rv) {
    # nocov
    data <- shiny::eventReactive(input$load, {
        shiny::validate(
            shiny::need(
                shiny::isTruthy(input$useSampleData) ||
                    shiny::isTruthy(input$uploadedCountFile),
                ""
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
        shiny::req(input$selectedExpVar)

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
    spe <- shiny::reactive({
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


        ## filter
        # Create filtering conditions using lapply
        filters <- lapply(input$selectedQc, function(column) {
            cutoff_value <- input[[paste0("cutoff_", column)]]
            if (!is.na(cutoff_value)) {
                return(SummarizedExperiment::colData(spe)[[column]] >
                           cutoff_value)
            } else {
                return(NULL)
            }
        })


        # Remove NULLs and combine filters
        filters <- Filter(Negate(is.null), filters)
        if (length(filters) > 0) {
            combined_filter <- Reduce(`&`, filters)
            spe <- spe[, combined_filter]
        }




        return(spe)
    })
    # nocov end
    return(spe)
}
