test_that("Demo data can be read and processed into spe", {
    segProp <- readxl::read_excel(
        "../../inst/extdata/Export1_InitialDataset_subset.xlsx",
        sheet = "SegmentProperties"
    )
    bioprob <- readxl::read_excel(
        "../../inst/extdata/Export1_InitialDataset_subset.xlsx",
        sheet = "BioProbeCountMatrix"
    )

    data <- list()

    countFile <- as.data.frame(bioprob[, c(3, 13:(dim(bioprob)[2] - 2))])
    sampleAnnoFile <- as.data.frame(segProp)
    featureAnnoFile <- as.data.frame(bioprob[, seq_len(12)])

    data[[1]] <- countFile
    data[[2]] <- sampleAnnoFile
    data[[3]] <- featureAnnoFile
    data[[4]] <- sampleAnnoFile$Type
    data[[5]] <- colnames(sampleAnnoFile)

    spe <- standR::readGeoMx(
        data[[1]],
        data[[2]],
        data[[3]]
    )

    expect_equal(dim(data[[1]]), c(2260, 192))
    expect_equal(dim(data[[2]]), c(193, 31))
    expect_equal(dim(data[[3]]), c(2260, 12))
    expect_equal(length(data[[4]]), 193)
    expect_equal(length(data[[5]]), 31)
    expect_type(spe, "S4")

})
