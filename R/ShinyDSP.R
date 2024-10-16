#' Creates the shiny app, ready to be loaded
#'
#' @return A [shiny::shinyApp()] object
#' @export
#'
#' @author Seung J. Kim
#'
#' @examples
#' library(shinyDSP)
#' app <- shinyDSP()
#' if (interactive()) {
#'   shiny::runApp(app)
#' }
shinyDSP <- function() {
  ui <- bslib::page_navbar(
    htmltools::tags$head(htmltools::tags$link(
      rel = "shortcut icon",
      href = "favicon.ico/lung.png"
    )),
    htmltools::tags$style(
      "
    .introText {
    padding-left: 100px;
    padding-right: 100px;
    }
    "
    ),
    shinyjs::useShinyjs(),
    title = "shinyDSP",
    id = "main",
    fillable = TRUE,
    sidebar = .interfaceSidebar(),
    .interfaceIntroNavPanel(),
    .interfacePcaNavPanel(),
    .interfaceTableNavPanel(),
    .interfaceVolcanoNavPanel(),
    # .interfaceHeatmapNavPanel()







    # bslib::nav_panel(
    #   "Appendix",
    #   div(
    #     tags$p("These are all possible colours schmes for the heatmap. Enter the names on the top exactly without quotes."),
    #     tags$img(src = "images/hcl.svg", alt = "hcl_palette")
    #   )
    # )

    # bslib::nav_panel(
    #   "Credit",
    #   div(
    #     tags$p("Amin Manji and Meggie Vo helped with beta testing, and grammar, respectively."),
    #     tags$p("Funded by the AMOSO foundation and PSI),
    #     tags$img(src = "images/amoso-logo.png", alt = "amoso logo")
    #   )
    # )


    # nav_spacer(),
    #
    # nav_menu(
    #   title = "Links",
    #   nav_item(
    #     tags$a(
    #       shiny::icon("github"), "", href = "https://github.com/rstudio/shiny"))
    # )
  )

  server <- function(input, output, session) {
    options(shiny.maxRequestSize=50*1024^2)
    # # nocov
    # data <- shiny::eventReactive(input$load, {
    #   shiny::validate(
    #     shiny::need(
    #       shiny::isTruthy(input$useSampleData) ||
    #         shiny::isTruthy(input$uploadedFile),
    #       "Use demo data OR upload your own!"
    #     )
    #   )
    #
    #   data <- list()
    #
    #   if (input$useSampleData == TRUE) {
    #     segProp <- readxl::read_excel(
    #       "inst/extdata/Export1_InitialDataset_subset.xlsx",
    #       sheet = "SegmentProperties"
    #     )
    #     bioprob <- readxl::read_excel(
    #       "inst/extdata/Export1_InitialDataset_subset.xlsx",
    #       sheet = "BioProbeCountMatrix"
    #     )
    #   } else {
    #     segProp <- readxl::read_excel(input$uploadedFile$datapath,
    #                                   sheet = "SegmentProperties"
    #     )
    #     bioprob <- readxl::read_excel(input$uploadedFile$datapath,
    #                                   sheet = "BioProbeCountMatrix"
    #     )
    #   }
    #
    #   countFile <- as.data.frame(bioprob[, c(3, 13:(dim(bioprob)[2] - 2))])
    #   sampleAnnoFile <- as.data.frame(segProp)
    #   featureAnnoFile <- as.data.frame(bioprob[, seq_len(12)])
    #
    #   data[[1]] <- countFile
    #   data[[2]] <- sampleAnnoFile
    #   data[[3]] <- featureAnnoFile
    #   data[[4]] <- sampleAnnoFile$Type
    #   data[[5]] <- colnames(sampleAnnoFile)
    #
    #   return(data)
    # })
    # # nocov end
    #
    # # nocov start
    # spe <- shiny::eventReactive(c(input$run, input$selectedTypes), {
    #   spe <- standR::readGeoMx(
    #     data()[[1]],
    #     data()[[2]],
    #     data()[[3]]
    #   )
    #
    #   spe <- spe[, grepl(paste(input$selectedTypes, collapse = "|"), spe$Type)]
    #
    #   # if (input$enableQC) {
    #   # qc <- colData(spe)$AlignedReads/colData(spe)$RawReads >=0.9 & colData(spe)$SequencingSaturation >=90
    #   #
    #   # spe <- spe[,qc]
    #   # }
    #
    #   return(spe)
    # })
    # # nocov end

    source("R/util_process_excel.R", local = TRUE)$value



    source("R/util_PCA_function.R", local = TRUE)$value
    # # nocov start
    # speCPM <- shiny::eventReactive(spe(), {
    #   speCPM <- scater::runPCA(spe())
    #   return(speCPM)
    # })
    # # nocov end
    #
    # # nocov start
    # speCPM_compute <- shiny::eventReactive(speCPM(), {
    #   speCPM_compute <- SingleCellExperiment::reducedDim(speCPM(), "PCA")
    #   return(speCPM_compute)
    # })
    # # nocov end
    #
    # # nocov start
    # pcaPlotCpm <- shiny::reactive({
    #   req(speCPM(), speCPM_compute())
    #
    #   # Initialize
    #   ROIshapes <- list()
    #   ROIcolours <- list()
    #
    #   ROIshapes <- lapply(seq_along(input$selectedTypes), function(i) {
    #     input[[paste0("shape_", input$selectedTypes[i])]]
    #   })
    #   ROIcolours <- lapply(seq_along(input$selectedTypes), function(i) {
    #     input[[paste0("colour_", input$selectedTypes[i])]]
    #   })
    #
    #   pcaPlot <- .PCAFunction(
    #     speCPM(), speCPM_compute(), "Type",
    #     input$selectedTypes, ROIshapes, ROIcolours
    #   ) +
    #     ggplot2::ggtitle("CPM - by type")
    #
    #   return(pcaPlot)
    # })
    # # nocov end
    #
    # # nocov start
    # pcaPlotCpmBatch <- shiny::reactive({
    #   req(speCPM(), speCPM_compute())
    #
    #   batchVars <- data()[[2]] |>
    #     dplyr::pull(input$selectedBatch) |>
    #     unique()
    #
    #   # Initialize
    #   ROIshapes <- list()
    #   ROIcolours <- list()
    #
    #   ROIshapes <- lapply(seq_along(batchVars), function(i) {
    #     input[[paste0("shape_", batchVars[i])]]
    #   })
    #   ROIcolours <- lapply(seq_along(batchVars), function(i) {
    #     input[[paste0("colour_", batchVars[i])]]
    #   })
    #
    #   pcaPlot <- .PCAFunction(
    #     speCPM(), speCPM_compute(),
    #     input$selectedBatch, batchVars,
    #     ROIshapes, ROIcolours
    #   ) +
    #     ggplot2::ggtitle("CPM - by batch")
    #
    #   return(pcaPlot)
    # })
    #
    #
    # # nocov start
    # PCAcustomizationBatch <- shiny::reactive({
    #   # must use dplyr for this to work...
    #   batchVars <- data()[[2]] %>%
    #     dplyr::pull(input$selectedBatch) %>%
    #     unique()
    #
    #   # Must be initialized first
    #   shapes_colours_pca_batch <- list()
    #
    #   shapes_colours_pca_batch <- lapply(seq_along(batchVars), function(i) {
    #     htmltools::div(
    #       shinyWidgets::radioGroupButtons(
    #         inputId = paste0("shape_", batchVars[i]),
    #         # For each ROI, I need to address its shape and colour separately
    #
    #         label = paste0("Pick a shape for ", batchVars[i]),
    #         choiceNames = list(
    #           tags$img(src = "www/circle.png", height = "24px", width = "24px"),
    #           tags$img(src = "www/square.png", height = "24px", width = "24px"),
    #           tags$img(
    #             src = "www/square.png", height = "20px", width = "20px",
    #             style = "rotate: 45deg;"
    #           ),
    #           tags$img(src = "www/triangle.png", height = "24px", width = "24px"),
    #           tags$img(
    #             src = "www/triangle.png", height = "24px", width = "24px",
    #             style = "rotate: 180deg;"
    #           )
    #         ),
    #         size = "sm",
    #         justified = TRUE,
    #         choiceValues = list(
    #           21,
    #           22,
    #           23,
    #           24,
    #           25
    #         )
    #       ),
    #       shiny::textInput(
    #         inputId = paste0("colour_", batchVars[i]),
    #         label = paste0("Pick a colour for ", batchVars[i]),
    #         value = sample(c(
    #           "black", "blue", "pink3", "purple", "orange",
    #           "darkgreen", "maroon", "turquoise3"
    #         ), 1)
    #       ),
    #       style = "display: inline-block;"
    #     )
    #   })
    #
    #
    #   return(shapes_colours_pca_batch)
    # })
    # # nocov end

    source("R/util_create_PCA_customization.R", local = TRUE)$value

    # # nocov start
    # speCPM <- shiny::eventReactive(spe(), {
    #   speCPM <- scater::runPCA(spe())
    #   return(speCPM)
    # })
    # # nocov end
    #
    # # nocov start
    # speCPM_compute <- shiny::eventReactive(speCPM(), {
    #   speCPM_compute <- SingleCellExperiment::reducedDim(speCPM(), "PCA")
    #   return(speCPM_compute)
    # })
    # # nocov end
    #
    # # nocov start
    # pcaPlotCpm <- shiny::reactive({
    #   req(speCPM(), speCPM_compute())
    #
    #   # Initialize
    #   ROIshapes <- list()
    #   ROIcolours <- list()
    #
    #   ROIshapes <- lapply(seq_along(input$selectedTypes), function(i) {
    #     input[[paste0("shape_", input$selectedTypes[i])]]
    #   })
    #   ROIcolours <- lapply(seq_along(input$selectedTypes), function(i) {
    #     input[[paste0("colour_", input$selectedTypes[i])]]
    #   })
    #
    #   pcaPlot <- .PCAFunction(
    #     speCPM(), speCPM_compute(), "Type",
    #     input$selectedTypes, ROIshapes, ROIcolours
    #   ) +
    #     ggplot2::ggtitle("CPM - by type")
    #
    #   return(pcaPlot)
    # })
    # # nocov end
    #
    # # nocov start
    # pcaPlotCpmBatch <- shiny::reactive({
    #   req(speCPM(), speCPM_compute())
    #
    #   batchVars <- data()[[2]] |>
    #     dplyr::pull(input$selectedBatch) |>
    #     unique()
    #
    #   # Initialize
    #   ROIshapes <- list()
    #   ROIcolours <- list()
    #
    #   ROIshapes <- lapply(seq_along(batchVars), function(i) {
    #     input[[paste0("shape_", batchVars[i])]]
    #   })
    #   ROIcolours <- lapply(seq_along(batchVars), function(i) {
    #     input[[paste0("colour_", batchVars[i])]]
    #   })
    #
    #   pcaPlot <- .PCAFunction(
    #     speCPM(), speCPM_compute(),
    #     input$selectedBatch, batchVars,
    #     ROIshapes, ROIcolours
    #   ) +
    #     ggplot2::ggtitle("CPM - by batch")
    #
    #   return(pcaPlot)
    # })
    # # nocov end

   source("R/util_create_PCA_by_CPM.R", local = TRUE)$value
    # # nocov start
    # speQ3 <- shiny::eventReactive(spe(), {
    #   speQ3 <- standR::geomxNorm(spe(), method = "upperquartile")
    #   speQ3 <- scater::runPCA(speQ3)
    #   return(speQ3)
    # })
    # # nocov end
    #
    # # nocov start
    # speQ3_compute <- shiny::eventReactive(speQ3(), {
    #   speQ3_compute <- SingleCellExperiment::reducedDim(speQ3(), "PCA")
    #   return(speQ3_compute)
    # })
    # # nocov end
    #
    # # nocov start
    # pcaPlotQ3 <- shiny::reactive({
    #   req(speQ3(), speQ3_compute())
    #
    #   # Initialize
    #   ROIshapes <- list()
    #   ROIcolours <- list()
    #
    #   ROIshapes <- lapply(seq_along(input$selectedTypes), function(i) {
    #     input[[paste0("shape_", input$selectedTypes[i])]]
    #   })
    #   ROIcolours <- lapply(seq_along(input$selectedTypes), function(i) {
    #     input[[paste0("colour_", input$selectedTypes[i])]]
    #   })
    #
    #   pcaPlot <- .PCAFunction(
    #     speQ3(), speQ3_compute(), "Type",
    #     input$selectedTypes, ROIshapes, ROIcolours
    #   ) +
    #     ggplot2::ggtitle("Q3 - by type")
    #   return(pcaPlot)
    # })
    # # nocov end
    #
    # # nocov start
    # pcaPlotQ3Batch <- shiny::reactive({
    #   req(speQ3(), speQ3_compute())
    #
    #   batchVars <- data()[[2]] %>%
    #     dplyr::pull(input$selectedBatch) %>%
    #     unique()
    #
    #   # Initialize
    #   ROIshapes <- list()
    #   ROIcolours <- list()
    #
    #   ROIshapes <- lapply(seq_along(batchVars), function(i) {
    #     input[[paste0("shape_", batchVars[i])]]
    #   })
    #   ROIcolours <- lapply(seq_along(batchVars), function(i) {
    #     input[[paste0("colour_", batchVars[i])]]
    #   })
    #
    #   pcaPlot <- .PCAFunction(
    #     speQ3(), speQ3_compute(), input$selectedBatch,
    #     batchVars, ROIshapes, ROIcolours
    #   ) +
    #     ggplot2::ggtitle("Q3 - by batch")
    #
    #   return(pcaPlot)
    # })
    # # nocov end

   source("R/util_create_PCA_by_Q3.R", local = TRUE)$value
    # # nocov start
    # speRUV_NCGs <- shiny::eventReactive(spe(), {
    #   return(standR::findNCGs(spe(),
    #                           batch_name = input$selectedBatch,
    #                           top_n = 200
    #   ))
    # })
    # # nocov end
    #
    # # nocov start
    # speRUVBatchCorrection <- shiny::eventReactive(list(speRUV_NCGs(), input$k), {
    #   return(standR::geomxBatchCorrection(speRUV_NCGs(),
    #                                       factors = "Type",
    #                                       NCGs = S4Vectors::metadata(speRUV_NCGs())$NCGs, k = input$k
    #   ))
    # })
    # # nocov end
    #
    # # nocov start
    # speRUV <- shiny::eventReactive(speRUVBatchCorrection(), {
    #   speRUV <- scater::runPCA(speRUVBatchCorrection())
    #   return(speRUV)
    # })
    # # nocov end
    #
    # # nocov start
    # speRUV_compute <- shiny::eventReactive(speRUV(), {
    #   speRUV_compute <- SingleCellExperiment::reducedDim(speRUV(), "PCA")
    #   return(speRUV_compute)
    # })
    # # nocov end
    #
    # # nocov start
    # pcaPlotRuv <- shiny::reactive({
    #   # Initialize
    #   ROIshapes <- list()
    #   ROIcolours <- list()
    #
    #   ROIshapes <- lapply(seq_along(input$selectedTypes), function(i) {
    #     input[[paste0("shape_", input$selectedTypes[i])]]
    #   })
    #   ROIcolours <- lapply(seq_along(input$selectedTypes), function(i) {
    #     input[[paste0("colour_", input$selectedTypes[i])]]
    #   })
    #
    #   pca_plot <- .PCAFunction(
    #     speRUV(), speRUV_compute(), "Type",
    #     input$selectedTypes, ROIshapes, ROIcolours
    #   ) +
    #     ggplot2::ggtitle("RUV4 - by type")
    #   return(pca_plot)
    # })
    # # nocov end
    #
    # # nocov start
    # pcaPlotRuvBatch <- shiny::reactive({
    #   req(speRUV(), speRUV_compute())
    #
    #   batchVars <- data()[[2]] %>%
    #     dplyr::pull(input$selectedBatch) %>%
    #     unique()
    #
    #   # Initialize
    #   ROIshapes <- list()
    #   ROIcolours <- list()
    #
    #   ROIshapes <- lapply(seq_along(batchVars), function(i) {
    #     input[[paste0("shape_", batchVars[i])]]
    #   })
    #   ROIcolours <- lapply(seq_along(batchVars), function(i) {
    #     input[[paste0("colour_", batchVars[i])]]
    #   })
    #
    #   pcaPlot <- .PCAFunction(
    #     speRUV(), speRUV_compute(),
    #     input$selectedBatch, batchVars,
    #     ROIshapes, ROIcolours
    #   ) +
    #     ggplot2::ggtitle("RUV4 - by batch")
    #
    #   return(pcaPlot)
    # })
    # # nocov end

    source("R/util_create_PCA_by_RUV4.R", local = TRUE)$value
    # # nocov start
    # design <- shiny::reactive({
    #   shiny::req(input$selectedNorm)
    #   spe <- eval(parse(text = input$selectedNorm))
    #
    #   li <- list()
    #   li[[1]] <- "~0"
    #   li[[2]] <- "Type"
    #   li[[3]] <- input$selectedBatch
    #
    #   if (input$selectedNorm == "speRUV()") {
    #     for (i in seq_along(input$k)) {
    #       li[[i + 3]] <- paste0("ruv_W", i)
    #     }
    #   }
    #
    #   formula <- gsub(" ", " + ", paste(li, collapse = " "))
    #   design <- stats::model.matrix(eval(parse(text = formula)),
    #                                 data = SummarizedExperiment::colData(spe)
    #   )
    #   colnames(design) <- gsub("Type", "", colnames(design))
    #   colnames(design) <- gsub(input$selectedBatch, "", colnames(design))
    #   colnames(design) <- gsub(" ", "_", colnames(design))
    #
    #   return(design)
    # })
    # # nocov end
    #
    # # nocov start
    # dge <- shiny::eventReactive(design(), {
    #   shiny::withProgress(message = "Creating a DGEList object...", {
    #     spe <- eval(parse(text = input$selectedNorm))
    #
    #     dge <- edgeR::SE2DGEList(spe)
    #
    #     shiny::incProgress(1 / 4)
    #
    #     keep <- edgeR::filterByExpr(dge, design())
    #
    #
    #     dge <- dge[keep, , keep.lib.sizes = FALSE]
    #     shiny::incProgress(2 / 4)
    #
    #
    #     dge <- edgeR::estimateDisp(dge, design = design(), robust = TRUE)
    #     shiny::incProgress(3 / 4)
    #   })
    #   return(dge)
    # })
    # # nocov end
    #
    # # nocov start
    # efit <- shiny::eventReactive(dge(), {
    #   shiny::req(input$selectedBatch, contrast())
    #
    #   shiny::withProgress(
    #     message = "Performing differential gene expression analysis...",
    #     {
    #       spe <- eval(parse(text = input$selectedNorm))
    #
    #       block_by <- SummarizedExperiment::colData(spe)[[input$selectedBatch]]
    #
    #       v <- limma::voom(dge(), design())
    #       corfit <- limma::duplicateCorrelation(v, design(), block = block_by)
    #
    #       shiny::incProgress(1 / 5)
    #
    #       v2 <- limma::voom(dge(), design(), block = block_by, correlation = corfit$consensus)
    #       corfit2 <- limma::duplicateCorrelation(v, design(), block = block_by)
    #
    #       shiny::incProgress(2 / 5)
    #
    #       fit <- limma::lmFit(v, design(), block = block_by, correlation = corfit2$consensus)
    #
    #       shiny::incProgress(3 / 5)
    #
    #       fit_contrast <- limma::contrasts.fit(fit, contrasts = contrast())
    #       efit <- limma::eBayes(fit_contrast, robust = TRUE)
    #
    #       shiny::incProgress(4 / 5)
    #     }
    #   )
    #
    #   return(efit)
    # })
    # # nocov end
    #
    #
    # # nocov start
    # contrast <- shiny::eventReactive(design(), {
    #   # replace space with _
    #   selectedTypes_underscore <- gsub(" ", "_", input$selectedTypes)
    #
    #   comparisons <- list()
    #
    #   comparisons <- lapply(
    #     seq_len(choose(length(selectedTypes_underscore), 2)),
    #     function(i) {
    #       noquote(
    #         paste0(
    #           utils::combn(selectedTypes_underscore, 2, simplify = FALSE)[[i]][1],
    #           "-",
    #           utils::combn(selectedTypes_underscore, 2, simplify = FALSE)[[i]][2]
    #         )
    #       )
    #     }
    #   )
    #
    #   con <- limma::makeContrasts(
    #     # Must use as.character()
    #     contrasts = as.character(unlist(comparisons)),
    #     levels = colnames(design())
    #   )
    #
    #   colnames(con) <- sub("-", "_vs_", colnames(con))
    #
    #   return(con)
    # })
    # # nocov end

   source("R/util_differential_gene_exp.R", local = TRUE)$value
    # # nocov start
    # topTabDF <- shiny::reactive({
    #   # If there are more than two groups, must sort by F, not P...
    #   # but it might sort by default? could be redundant
    #
    #   if (length(input$selectedTypes) > 2) {
    #     dt <- limma::topTable(efit(),
    #                           coef = seq_len(ncol(contrast())), number = Inf,
    #                           p.value = 0.05, sort.by = "F", adjust.method = "BH",
    #                           lfc = input$lfc
    #     ) %>%
    #       tibble::rownames_to_column(var = "Gene") %>%
    #       dplyr::select(!c(
    #         "ProbeName", "GeneID", "HUGOSymbol", "ProbeDisplayName",
    #         "Accessions", "GenomeBuild", "AnalyteType", "CodeClass",
    #         "ProbePool", "TargetGroup"
    #       ))
    #   } else {
    #     dt <- limma::topTable(efit(),
    #                           coef = seq_len(ncol(contrast())), number = Inf,
    #                           p.value = 0.05, sort.by = "P", adjust.method = "BH",
    #                           lfc = input$lfc
    #     ) %>%
    #       tibble::rownames_to_column(var = "Gene") %>%
    #       dplyr::select(!c(
    #         "ProbeName", "GeneID", "HUGOSymbol", "ProbeDisplayName",
    #         "Accessions", "GenomeBuild", "AnalyteType", "CodeClass",
    #         "ProbePool", "TargetGroup"
    #       ))
    #   }
    #   return(dt)
    # })
    # # nocov end

   source("R/util_create_table.R", local = TRUE)$value
    # # nocov start
    # volcano <- shiny::eventReactive(c(
    #   input$selectedTypes, input$selectedNorm,
    #   input$generateVolcano
    # ), {
    #   shiny::req(efit(), contrast(), input$logFCcutoff, input$PvalCutoff)
    #
    #   shiny::withProgress(message = "Plotting...", {
    #     # make data frame
    #     volcanoDF <- lapply(as.list(seq_len(ncol(contrast()))), function(i) {
    #       limma::topTable(efit(), coef = i, number = Inf) %>%
    #         tibble::rownames_to_column(var = "Target.name") %>%
    #         dplyr::select("Target.name", "logFC", "adj.P.Val") %>%
    #         dplyr::mutate(de = ifelse(logFC >= input$logFCcutoff &
    #                                     adj.P.Val < input$PvalCutoff, "UP",
    #                                   ifelse(logFC <= -(input$logFCcutoff) &
    #                                            adj.P.Val < input$PvalCutoff, "DN",
    #                                          "NO"
    #                                   )
    #         )) %>%
    #         dplyr::mutate(deLab = ifelse(
    #           quantile(abs(logFC), 0.999, na.rm = TRUE) < abs(logFC), Target.name,
    #           NA
    #         ))
    #     })
    #
    #
    #     # If not using custom range, determine maximum absolute FC for each plot
    #     if (input$toggleCustomRange == FALSE) {
    #       plots <- list()
    #
    #       plots <- lapply(seq_along(volcanoDF), function(i) {
    #         .volcanoFunction(
    #           volcanoDF[[i]], input$delabSize, input$maxOverlap,
    #           colnames(contrast())[i],
    #           input$logFCcutoff, input$PvalCutoff,
    #           input$DnCol, input$notDEcol, input$UpCol
    #         ) +
    #           ggplot2::xlim(
    #             0 - max(abs(volcanoDF[[i]] %>% dplyr::select(logFC))),
    #             max(abs(volcanoDF[[i]] %>% dplyr::select(logFC)))
    #           )
    #       })
    #     } else {
    #       plots <- lapply(seq_along(volcanoDF), function(i) {
    #         .volcanoFunction(
    #           volcanoDF[[i]], input$delabSize, input$maxOverlap,
    #           colnames(contrast())[i],
    #           input$logFCcutoff, input$PvalCutoff,
    #           input$DnCol, input$notDEcol, input$UpCol
    #         ) +
    #           ggplot2::xlim(
    #             input$customX[1],
    #             input$customX[2]
    #           ) +
    #           ggplot2::ylim(
    #             0, input$customY
    #           )
    #       })
    #     }
    #   })
    #
    #   return(plots)
    # })
    # # nocov end

   source("R/util_create_volcano.R", local = TRUE)$value




    # shiny::observeEvent(input$run,{
    #   if (input$main == "introPage"){
    #     nav_select(id = "main", selected = "PCA")
    #   }
    # })
    ## --------------------Sidebar observe---------------------------------------
    .observeEvent_sidebar(input, output)

    ## --------------------Sidebar outputs---------------------------------------
    # nocov start
    output$selectYourExpVar <- shiny::renderUI({
      req(data())

      shiny::selectInput(
        inputId = "selectedExpVar",
        #label = "Choose the main variable",
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



    observe({
     # print(head(new_sampleAnnoFile())),
      print(design())
    }
    )





    # nocov start


    output$selectYourType <- shiny::renderUI({
      req(new_sampleAnnoFile(), input$selectedExpVar)


      ExpVar <- paste0(input$selectedExpVar, collapse = "_")

      shiny::selectInput(
        inputId = "selectedTypes",
        #label = "Choose groups to analyze",
        label = bslib::tooltip(
          trigger = list(
            "Groups of interest",
            bsicons::bs_icon("info-circle")
          ),
          "Pick groups you want to compare such as 'WT' and 'Mutant'"
        ),
        #choices = data()$sampleAnnoFile %>% pull(input$selectedExpVar) %>% unique(),
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
        #label = "Choose sample batch",
        bslib::tooltip(
          trigger = list(
            "A batch variable",
            bsicons::bs_icon("info-circle")
          ),
          "For example, sample preparation date "
        ),
        choices = data()$sampleAnnoFile %>% dplyr::select(where(is.character))
        %>%  colnames(),
        multiple = FALSE
      )
    })
    # nocov end

    # nocov start
    output$selectYourConfounder <- shiny::renderUI({
      req(data())

      shiny::selectizeInput(
        inputId = "selectedConfounders",
        #label = "Choose sample batch",
        bslib::tooltip(
          trigger = list(
            "A batch variable",
            bsicons::bs_icon("info-circle")
          ),
          "For example, sex or age "
        ),
        choices = data()$sampleAnnoFile %>% dplyr::select(where(is.character))
        %>%  colnames(),
        multiple = TRUE,
        options = list(
          placeholder = 'Please select an option below',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    })
    # nocov end



    ## --------------------PCA nav panel observe---------------------------------
    .observeEvent_pca_nav_panel(input)

    ## --------------------PCA nav panel outputs---------------------------------
    # nocov start
    # Opens a grouped widgets to pick shape and colour
    output$customization <- shiny::renderUI({
      shiny::req(input$selectedTypes)
      .PCAcustomization()
    })
    # nocov end

    # nocov start
    # Opens a grouped widgets to pick shape and colour
    output$customizationBatch <- shiny::renderUI({
      shiny::req(input$selectedTypes)
      PCAcustomizationBatch()
    })
    # nocov end

    # nocov start
    output$pcaPlotCpm <- shiny::renderUI({
      shiny::validate(
        shiny::need(
          shiny::isTruthy(input$load) & shiny::isTruthy(input$selectedTypes),
          "'Load' data then select groups first!"
        )
      )
      shiny::renderPlot(pcaPlotCpm())
    })
    # nocov end

    # nocov start
    output$pcaPlotQ3 <- shiny::renderUI({
      shiny::req(input$load, input$selectedTypes)
      shiny::renderPlot(pcaPlotQ3())
    })
    # nocov end

    # nocov start
    output$pcaPlotRuv <- shiny::renderUI({
      shiny::req(input$load, input$selectedTypes)
      shiny::renderPlot(pcaPlotRuv())
    })
    # nocov end

    # nocov start
    output$pcaPlotCpmBatch <- shiny::renderUI({
      shiny::req(input$load, input$selectedTypes)
      shiny::renderPlot(pcaPlotCpmBatch())
    })
    # nocov end

    # nocov start
    output$pcaPlotQ3Batch <- shiny::renderUI({
      shiny::req(input$load, input$selectedTypes)
      shiny::renderPlot(pcaPlotQ3Batch())
    })
    # nocov end

    # nocov start
    output$pcaPlotRuvBatch <- shiny::renderUI({
      shiny::req(input$load, input$selectedTypes)
      shiny::renderPlot(pcaPlotRuvBatch())
    })
    # nocov end

    # nocov start
    # output$downloadPCA <- shiny::downloadHandler(
    #   filename = function() {
    #     base::paste0("pca_", Sys.Date(), ".png")
    #   },
    #   content = function(file) {
    #     png(file)
    #     pcaPlot()
    #     dev.off()
    #   },
    #   contentType = "image/png"
    # )
    # nocov end

    ## --------------------Table nav panel output-------------------------------
    # nocov start
    output$table <- shiny::renderUI({
      shiny::validate(
        shiny::need(
          shiny::isTruthy(input$load) &
            shiny::isTruthy(input$selectedTypes) &
            shiny::isTruthy(input$selectedNorm) &
            shiny::isTruthy(input$run),
          "'Load' data, 'choose types', select 'normalization', then hit 'run'!"
        )
      )
      DT::renderDT(
        topTabDF() %>%
          DT::datatable() %>%
          DT::formatSignif(columns = c(3:ncol(topTabDF())), digits = 4)
      )
    })
    # nocov end

    # nocov start
    output$downloadTable <- shiny::downloadHandler(
      filename = "output.csv",
      content = function(file) {
        utils::write.table(topTabDF(), file, sep = ",", row.names = FALSE)
      }
    )
    # nocov end

    ## --------------------Volcano observe---------------------------------------
    # nocov start
    shiny::observeEvent(input$toggleCustomRange, {
      shinyjs::toggle("showCustomRange")
    })
    # nocov end

    ## --------------------Volcano outputs---------------------------------------
    # nocov start
    output$customRange <- shiny::renderUI({
      htmltools::div(
        shiny::sliderInput(
          inputId = "customX",
          "logFC", min = -10, max = 10, dragRange = FALSE, value = c(-4, 4)
        ),
        shiny::numericInput(
          inputId = "customY",
          "-log P value", value = 10
        )
      )
    })
    # nocov end

    # nocov start
    output$volcanoUI <- shiny::renderUI({
      shiny::validate(
        shiny::need(
          shiny::isTruthy(input$load) &
            shiny::isTruthy(input$selectedTypes) &
            shiny::isTruthy(input$selectedNorm) &
            shiny::isTruthy(input$run) &
            shiny::isTruthy(input$generateVolcano),
          "'Load' data, 'choose types', select 'normalization', hit 'run',
          then hit 'update'!"
        )
      )
      plotHeight <- shiny::reactive(350 * ncol(contrast()))

      # Plots as a list can be grouped into a grid and output
      shiny::renderPlot(
        # grid.arrange(grobs = plots)
        cowplot::plot_grid(
          plotlist = volcano(), ncol = 1, align = "v",
          axis = "lr"
        ),
        height = plotHeight()
      )
    })
    # nocov end

    # nocov start
    # output$downloadVolcano <- shiny::downloadHandler(
    #   filename = function() {
    #     paste0("volcano_", Sys.Date(), ".png")
    #   },
    #   content = function(file) {
    #     cowplot::save_plot(file, volcanoPlots())
    #   },
    #   contentType = "image/png"
    # )
    # no cov

    #### --------------------Heatmap outputs---------------------------------------



    # top_n_genes <- shiny::reactive({
    #     input$top_n_genes
    # })
    #
    # heatmap_col <- shiny::reactive({
    #     input$heatmap_col
    # })
    #
    # heatmap_range <- shiny::reactive({
    #     input$heatmap_range
    # })
    #
    # heatmap_size <- shiny::reactive({
    #     input$heatmap_size
    # })
    #
    # heatmap_fontsize <- shiny::reactive({
    #     input$heatmap_fontsize
    # })
    #
    #
    #
    #
    # lcpm_subset_scale <- shiny::reactive({
    #     mydata <- list()
    #     for (i in seq_along(shiny::reactiveRun())) {
    #         mydata[[i]] <- assay(spe_ruv_subset(), 2)[, colData(spe_ruv_subset())$anno_type == shiny::reactiveRun()[i]]
    #     }
    #
    #
    #     lcpm_subset_scale <- t(scale(t(data.frame(mydata))))
    #
    #     return(lcpm_subset_scale)
    # })
    #
    # colnames4heatmap <- shiny::reactive({
    #     mydata <- list()
    #     for (i in seq_along(shiny::reactiveRun())) {
    #         mydata[[i]] <- assay(spe_ruv_subset(), 2)[, colData(spe_ruv_subset())$anno_type == shiny::reactiveRun()[i]]
    #     }
    #
    #
    #     return(colnames(do.call(cbind, mydata)))
    # })
    #
    #
    #
    #
    # lcpm_subset_scale_topGenes <- shiny::reactive({
    #     ## BEWARE! top_n() reorders rows by some column value. Must use slice_head() to pick first n rows
    #     lcpm_subset_scale_topGenes <- lcpm_subset_scale()[topTabDF() %>%
    #                                                           slice_head(n = top_n_genes()) %>%
    #                                                           select(Gene) %>%
    #                                                           unlist() %>%
    #                                                           unname(), ]
    #
    #     return(lcpm_subset_scale_topGenes)
    # })
    #
    #
    # heatmap <- shiny::reactive({
    #     col_fun <- colorRamp2(c(heatmap_range()[1], 0, heatmap_range()[2]), hcl_palette = heatmap_col())
    #
    #     chm <- Heatmap(lcpm_subset_scale_topGenes(),
    #                    cluster_columns = F,
    #                    col = col_fun,
    #                    # width = unit(dim(lcpm_subset_scale_topGenes())[2]*15, "mm"),
    #                    # height = unit(dim(lcpm_subset_scale_topGenes())[1]*15, "mm"),
    #                    width = unit(as.numeric(heatmap_size()) / 2 * dim(lcpm_subset_scale_topGenes())[2], "mm"),
    #                    height = unit(as.numeric(heatmap_size()) / 2 * dim(lcpm_subset_scale_topGenes())[1], "mm"),
    #                    heatmap_legend_param = list(
    #                        border = "black",
    #                        title = "Z score",
    #                        title_gp = gpar(fontsize = heatmap_fontsize(), fontface = "plain", fontfamily = "sans"),
    #                        labels_gp = gpar(fontsize = heatmap_fontsize(), fontface = "plain", fontfamily = "sans"),
    #                        legend_height = unit(3 * as.numeric(heatmap_size()), "mm")
    #                    ),
    #                    top_annotation = HeatmapAnnotation(
    #                        foo = anno_block(
    #                            gp = gpar(lty = 0, fill = "transparent"),
    #                            labels = unlist(shiny::reactiveRun()),
    #                            labels_gp = gpar(col = "black", fontsize = 14, fontfamily = "sans", fontface = "bold"),
    #                            labels_rot = 0, labels_just = "center", labels_offset = unit(4.5, "mm")
    #                        )
    #                    ),
    #                    border_gp = gpar(col = "black", lwd = 0.2),
    #                    row_names_gp = gpar(fontfamily = "sans", fontface = "italic", fontsize = heatmap_fontsize()),
    #                    show_column_names = F,
    #
    #                    # top_annotation= HeatmapAnnotation(
    #                    #   foo = anno_block(
    #                    #     gp = gpar(lty=0, fill="transparent"),
    #                    #     labels = unlist(shiny::reactiveRun()),
    #                    #     labels_gp = gpar(col="black", fontsize=7, fontfamily = "sans", fontface = "bold"),
    #                    #     labels_rot = 20, labels_just = "center", labels_offset = unit(4,"mm")
    #                    #   )
    #                    # ),
    #                    column_split = rep(LETTERS[seq_along(shiny::reactiveRun())],
    #                                       # times = as.numeric(unname(table(colData(spe_ruv_subset())[colnames(lcpm_subset_scale()), "anno_type"])))
    #                                       times = as.numeric(unname(table(colData(spe_ruv_subset())[colnames4heatmap(), "anno_type"])))
    #                    ),
    #                    column_title = NULL
    #     )
    #
    #     return(chm)
    # })
    #
    # output$heatmapUI <- shiny::renderUI({
    #     shiny::renderPlot(
    #         heatmap(),
    #         height = 1.5 * as.numeric(heatmap_size()) * dim(lcpm_subset_scale_topGenes())[1]
    #         # height = as.numeric(heatmap_size()) * dim(lcpm_subset_scale_topGenes())[1],
    #         # width = as.numeric(heatmap_size()) * dim(lcpm_subset_scale_topGenes())[2]
    #     )
    # })
    #
    # output$downloadHeatmap <- shiny::downloadHandler(
    #     filename = function() {
    #         paste0("heatmap_", Sys.Date(), ".png")
    #     },
    #     content = function(file) {
    #         png(file, width = dim(lcpm_subset_scale_topGenes())[2] * 50, height = dim(lcpm_subset_scale_topGenes())[1] * 50, bg = "transparent")
    #         draw(heatmap())
    #         dev.off()
    #     },
    #     contentType = "image/png"
    # )
  }
  app <- shiny::shinyApp(ui = ui, server = server)
}
