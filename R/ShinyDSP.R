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
#'     shiny::runApp(app)
#' }
shinyDSP <- function() {


  ui <- htmltools::tagList(
      htmltools::tags$head(htmltools::tags$link(
        rel = "shortcut icon",
        href = "favicon.ico/lung.png"
      )),
      htmltools::tags$style(
      ),
      shinyjs::useShinyjs(),


    bslib::page_navbar(
    title = "shinyDSP",
    id = "navpanel",
    fillable = TRUE,
    sidebar = .interfaceSidebar(),
    .interfaceSetupNavPanel(),
    .interfaceQcNavPanel(),
    .interfacePcaNavPanel(),
    .interfaceTableNavPanel(),
    .interfaceVolcanoNavPanel(),
    .interfaceHeatmapNavPanel(),


    bslib::nav_spacer(),

    bslib::nav_menu(
      title = "Links",
      bslib::nav_item(
        htmltools::tags$a(
          shiny::icon("github"), "", href = "https://github.com/kimsjune/shinyDSP"))


    )
    )
  )

  server <- function(input, output, session) {
    options(shiny.maxRequestSize = 50 * 1024^2)


    rv <- shiny::reactiveValues()

    shiny::observe({
      ##------util_process.R----------------
      rv$data <- .data(input, output, session, rv)
      rv$new_sampleAnnoFile <- .new_sampleAnnoFile(input, output, session, rv)
      rv$spe <- .spe(input, output, session, rv)

      ##------output_PCA_customization.R-----------
      rv$pcaCustomization <- .pcaCustomization(input, output, session, rv)
      rv$pcaCustomizationBatch <- .pcaCustomizationBatch(input, output, session, rv)

      ##------util_PCA_by_CPM.R
      rv$speCpm <- .speCpm(input, output, session, rv)
      rv$speCpm_compute <- .speCpm_compute(input, output, session, rv)
      rv$pcaPlotCpm <- .pcaPlotCpm(input, output, session, rv)
      rv$pcaPlotCpmBatch <- .pcaPlotCpmBatch(input, output, session, rv)

      ##------util_PCA_by_Q3.R
      rv$speQ3 <- .speQ3(input, output, session, rv)
      rv$speQ3_compute <- .speQ3_compute(input, output, session, rv)
      rv$pcaPlotQ3 <- .pcaPlotQ3(input, output, session, rv)
      rv$pcaPlotQ3Batch <- .pcaPlotQ3Batch(input, output, session, rv)

      ##------util_PCA_by_RUV4.R
      rv$speRuv_NCGs <- .speRuv_NCGs(input, output, session, rv)
      rv$speRuvBatchCorrection <- .speRuvBatchCorrection(input, output, session, rv)
      rv$speRuv <- .speRuv(input, output, session, rv)
      rv$speRuv_compute <- .speRuv_compute(input, output, session, rv)
      rv$pcaPlotRuv <- .pcaPlotRuv(input, output, session, rv)
      rv$pcaPlotRuvBatch <- .pcaPlotRuvBatch(input, output, session, rv)
    })


    shiny::observeEvent(input$selectedNorm,{
      ##------util_differential_gene_exp.R
      rv$design <- .design(input, output, session, rv)
      rv$dge <- .dge(input, output, session, rv)
      rv$contrast <- .contrast(input, output, session, rv)
      rv$efit <- .efit(input, output,session, rv)

      ##------util_table.R
      rv$topTabDF <- .topTabDF(input, output, session, rv)

      ##------util_volcano.R
      rv$volcano <- .volcano(input, output, session, rv)

      ##------util_heatmap.R
      rv$lcpmSubScaleTopGenes <- .lcpmSubScaleTopGenes(input, output, session, rv)
      rv$columnSplit <- .columnSplit(input, output, session, rv)
      rv$heatmap <- .heatmap(input, output, session, rv)

    })









##---------------------nav panel behaviour----------------------------------
.outputNavPanels(input, output, rv)




## --------------------Sidebar outputs--------------------------------------
## I think there is a bug with acceptable function names. Error w/o '2' at the
## end
.outputSidebar2(input, output, rv)

## --------------------Setup nav panel output-------------------------------
.outputSetupNavPanel(input, output, rv)

## --------------------QC nav panel output----------------------------------
.outputQcNavPanel(input, output, rv)





## --------------------PCA nav panel output---------------------------------
.outputPcaNavPanel2(input, output, rv)


## --------------------Table nav panel output-------------------------------
.outputTableNavPanel2(input, output, rv)







## --------------------Volcano nav panel output-----------------------------
.outputVolcanoNavPanel2(input, output, rv)





## --------------------Heatmap nav panel output-----------------------------
.outputHeatmapNavPanel2(input, output, rv)






  }
  shiny::shinyApp(ui, server)
}
