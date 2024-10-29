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
        htmltools::tags$head(),
        # Add CSS to adjust the navigation panel position
        header = htmltools::tags$style(htmltools::HTML("
    .navbar-nav {
      margin-left: 75px;  /* Adjust the value to move the panels right */
    }
  ")),
        htmltools::tags$style(
            htmltools::HTML("
    .justified-buttons {
      display: flex;
      justify-content: space-between;
      margin-bottom: 20px; /* optional spacing below */
    }
  ")
        ),
        shinyjs::useShinyjs(),
        bslib::page_navbar(
            title = htmltools::tags$div(
                style = "display: inline; align-items: center; height: 50px",
                htmltools::span("shinyDSP"),
                htmltools::span(
                    paste0(
                        " v",
                        utils::packageDescription(
                            "shinyDSP"
                        )$Version
                    ),
                    style = "font-size: 14px; color: grey;"
                )
            ),
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
                        shiny::icon("github"), "",
                        href = "https://github.com/kimsjune/shinyDSP"
                    )
                )
            )
        )
    )

    server <- function(input, output, session) {
        options(shiny.maxRequestSize = 50 * 1024^2)



        # nocov start
        shiny::observe({
            iv <- shinyvalidate::InputValidator$new()

            lapply(input$selectedTypes, function(type) {
                colourInput <- input[[paste0("colour_", type)]]

                iv$add_rule(paste0("colour_", type), shinyvalidate::sv_required())

                iv$add_rule(paste0("colour_", type), shinyvalidate::sv_in_set(
                    grDevices::colors(), "Must be a valid colour in grDevices::colors()"
                ))
            })
            iv$add_rule("UpCol", shinyvalidate::sv_required())
            iv$add_rule("DnCol", shinyvalidate::sv_required())

            iv$add_rule("UpCol", shinyvalidate::sv_in_set(
                grDevices::colors(), "Must be a valid colour in grDevices::colors()"
            ))
            iv$add_rule("DnCol", shinyvalidate::sv_in_set(
                grDevices::colors(), "Must be a valid colour in grDevices::colors()"
            ))

            iv$enable()
        })
        # nocov end


        # nocov start
        rv <- shiny::reactiveValues()

        shiny::observe({
            ## ------util_process.R----------------
            rv$data <- .data(input, output, session, rv)
            rv$new_sampleAnnoFile <- .new_sampleAnnoFile(
                input, output, session, rv
            )
            rv$spe <- .spe(input, output, session, rv)

            ## ------output_PCA_customization.R-----------
            rv$pcaCustomization <- .pcaCustomization(input, output, session, rv)
            rv$pcaCustomizationBatch <- .pcaCustomizationBatch(
                input, output, session, rv
            )

            ## ------util_PCA_by_CPM.R
            rv$speCpm <- .speCpm(input, output, session, rv)
            rv$speCpm_compute <- .speCpm_compute(input, output, session, rv)
            rv$pcaPlotCpm <- .pcaPlotCpm(input, output, session, rv)
            rv$pcaPlotCpmBatch <- .pcaPlotCpmBatch(input, output, session, rv)

            ## ------util_PCA_by_Q3.R
            rv$speQ3 <- .speQ3(input, output, session, rv)
            rv$speQ3_compute <- .speQ3_compute(input, output, session, rv)
            rv$pcaPlotQ3 <- .pcaPlotQ3(input, output, session, rv)
            rv$pcaPlotQ3Batch <- .pcaPlotQ3Batch(input, output, session, rv)

            ## ------util_PCA_by_RUV4.R
            rv$speRuv_NCGs <- .speRuv_NCGs(input, output, session, rv)
            rv$speRuvBatchCorrection <- .speRuvBatchCorrection(
                input, output, session, rv
            )
            rv$speRuv <- .speRuv(input, output, session, rv)
            rv$speRuv_compute <- .speRuv_compute(input, output, session, rv)
            rv$pcaPlotRuv <- .pcaPlotRuv(input, output, session, rv)
            rv$pcaPlotRuvBatch <- .pcaPlotRuvBatch(input, output, session, rv)
        })
        # nocov end

        # nocov start
        shiny::observeEvent(input$selectedNorm, {
            ## ------util_differential_gene_exp.R
            rv$design <- .design(input, output, session, rv)
            rv$dge <- .dge(input, output, session, rv)
            rv$contrast <- .contrast(input, output, session, rv)
            rv$efit <- .efit(input, output, session, rv)

            ## ------util_table.R
            rv$topTabDF <- .topTabDF(input, output, session, rv)

            ## ------util_volcano.R
            rv$volcano <- .volcano(input, output, session, rv)

            ## ------util_heatmap.R
            rv$lcpmSubScaleTopGenes <- .lcpmSubScaleTopGenes(
                input, output, session, rv
            )
            rv$columnSplit <- .columnSplit(input, output, session, rv)
            rv$heatmap <- .heatmap(input, output, session, rv)
        })
        # nocov start

        ## ---------------------nav panel behaviour-----------------------------
        .outputNavPanels2(input, output, rv)

        ## --------------------Sidebar outputs----------------------------------
        ## I think there is a bug with acceptable function names. Error w/o '2'
        ## at the end
        .outputSidebar2(input, output, rv)

        ## --------------------Setup nav panel output---------------------------
        .outputSetupNavPanel2(input, output, rv)

        ## --------------------QC nav panel output-----------------------------
        .outputQcNavPanel2(input, output, rv)

        ## --------------------PCA nav panel output-----------------------------
        .outputPcaNavPanel2(input, output, rv)

        ## --------------------Table nav panel output---------------------------
        .outputTableNavPanel2(input, output, rv)

        ## --------------------Volcano nav panel output-------------------------
        .outputVolcanoNavPanel2(input, output, rv)

        ## --------------------Heatmap nav panel output-------------------------
        .outputHeatmapNavPanel2(input, output, rv)
    }
    shiny::shinyApp(ui, server)
}
