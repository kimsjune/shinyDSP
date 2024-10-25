library(shinytest2)

test_that("testing shiyDSP all in one", {
    app <- AppDriver$new(shinyDSP(), seed = 0, load_timeout = 30000, timeout = 300000, variant = platform_variant())

    app$set_window_size(width = 1235, height = 730)
    app$set_inputs(useSampleData = TRUE)
    # Update output value
    app$set_window_size(width = 1235, height = 730)
    app$click("load")
    app$set_window_size(width = 1235, height = 730)
    # Update output value
    app$set_inputs(selectedNorm = character(0))
    # Update output value
    app$set_inputs(selectedConfounders = character(0))
    # Update output value
    app$set_window_size(width = 1235, height = 730)
    # Update unbound `input` value
    # Update output value
    app$set_window_size(width = 1235, height = 730)
    # Update unbound `input` value
    app$expect_screenshot()
    app$set_inputs(selectedExpVar = "disease_status")
    # Update output value
    app$set_inputs(selectedExpVar = c("disease_status", "region"))
    # Update output value
    app$set_inputs(selectedTypes = "DKD_glomerulus")
    app$set_inputs(selectedTypes = c("DKD_glomerulus", "DKD_tubule"))
    app$set_inputs(selectedTypes = c("DKD_glomerulus", "DKD_tubule", "normal_tubule"))
    app$set_inputs(selectedTypes = c("DKD_glomerulus", "DKD_tubule", "normal_tubule", "normal_glomerulus"))
    app$set_inputs(navpanel = "QC")
    # Update output value
    app$set_window_size(width = 1235, height = 730)
    app$expect_screenshot()
    app$set_inputs(selectedQc = "UMIQ30")
    # Update output value
    app$set_inputs(selectedQc = c("UMIQ30", "DeduplicatedReads"))
    # Update output value
    app$click("generateQc")
    # Update output value
    app$set_inputs(navpanel = "PCA")
    # Update output value
    app$set_window_size(width = 1235, height = 730)
    # Update output value
    app$expect_screenshot()
    app$set_inputs(selectedNorm = "CPM")
    app$set_inputs(navpanel = "Table")
    app$set_window_size(width = 779, height = 1785)
    # Update output value
    # Update unbound `input` value
    app$expect_screenshot()
    app$set_window_size(width = 779, height = 1785)
    app$set_inputs(navpanel = "Volcano")
    app$set_window_size(width = 779, height = 1785)
    app$click("generateVolcano")
    # Update output value
    app$set_window_size(width = 779, height = 1785)
    # Update output value
    app$expect_screenshot()
    app$set_inputs(navpanel = "Heatmap")
    app$set_window_size(width = 779, height = 1785)
    app$click("generateHeatmap")
    # Update output value
    app$set_window_size(width = 779, height = 1785)
    # Update output value
    app$expect_screenshot()
})
