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
   ui <- bslib::page_navbar(
        htmltools::tags$head(htmltools::tags$link(
            rel = "shortcut icon",
            href = "favicon.ico/lung.png"
        )),
        htmltools::tags$style(

        ),
        shinyjs::useShinyjs(),
        title = "shinyDSP",
        id = "navpanel",
        fillable = TRUE,
        sidebar = .interfaceSidebar(output),
        .interfaceSetupNavPanel(output),
        .interfacePcaNavPanel(output),
        .interfaceTableNavPanel(output),
        .interfaceVolcanoNavPanel(),
        .interfaceHeatmapNavPanel()







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

      rv <- shiny::reactiveValues()

      observe({
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




      })

      shiny::eventReactive(input$generateTable,{
        print(rv$topTabDF())
      })




      source("R/util_heatmap.R", local = TRUE)$value


        options(shiny.maxRequestSize = 50 * 1024^2)
      shiny::observe({
        if (is.null(input$selectedTypes)){
          bslib::nav_hide("navpanel", "PCA")
        } else {
          bslib::nav_show("navpanel", "PCA")
        }
      }
      )

      shiny::observe({
        if (is.null(input$selectedNorm)){
          bslib::nav_hide("navpanel", "Table")
          bslib::nav_hide("navpanel", "Volcano")
          bslib::nav_hide("navpanel", "Heatmap")
        } else {
          bslib::nav_show("navpanel", "Table")
          bslib::nav_show("navpanel", "Volcano")
          bslib::nav_show("navpanel", "Heatmap")
        }
      }
      )









        # shiny::observeEvent(input$run,{
        #   if (input$main == "introPage"){
        #     nav_select(id = "main", selected = "PCA")
        #   }
        # })
        ## --------------------Sidebar observe----------------------------------
        .observeEvent_sidebar(input)

        ## --------------------Sidebar outputs----------------------------------
        # source("R/output_sidebar.R", local = TRUE)$value
        .outputSidebar(input, output, session, rv)

        ## -------
        # source("R/output_setup_nav_panel.R", local = TRUE)$value
        .outputSetupNavPanel(input, output, session, rv)


        ## --------------------PCA nav panel observe----------------------------
        .observeEvent_pca_nav_panel(input)


        ## --------------------PCA nav panel outputs----------------------------
        # source("R/output_pca_nav_panel.R", local = TRUE)$value
        .outputPcaNavPanel(input, output, session, rv)


        ## --------------------Table nav panel output---------------------------
        # source("R/output_table_nav_panel.R", local = TRUE)$value
        .outputTableNavPanel(input, output, session, rv)


        ## --------------------Table observe


        ## --------------------Volcano observe----------------------------------
        # nocov start
        shiny::observeEvent(input$toggleCustomRange, {
            shinyjs::toggle("showCustomRange")
        })
        # nocov end

        shiny::observeEvent(input$generateVolcano,{
          lapply(names(rv$volcano()), function(name) {
            output[[paste0("volcano_", name)]] <- shiny::renderPlot({
              rv$volcano()[[name]]

            })


          }
          )
        })


        ## --------------------Volcano outputs----------------------------------
        # source("R/output_volcano_nav_panel.R", local = TRUE)$value
        .outputVolcanoNavPanel(input, output, session, rv)





        #### --------------------Heatmap outputs--------------------------------
        # source("R/output_heatmap_nav_panel.R", local = TRUE)$value
        .outputHeatmapNavPanel(input, output, session, rv)



        top_n_genes <- shiny::reactive({
            input$top_n_genes
        })

        heatmap_col <- shiny::reactive({
            input$heatmap_col
        })

        heatmap_range <- shiny::reactive({
            input$heatmap_range
        })

        heatmap_size <- shiny::reactive({
            input$heatmap_size
        })

        heatmap_fontsize <- shiny::reactive({
            input$heatmap_fontsize
        })
        #
        #
        #
        #
        observe({


        })


    }
    shiny::shinyApp(ui = ui, server = server)
}
