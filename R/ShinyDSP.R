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
        sidebar = .interfaceSidebar(),
        .interfaceSetupNavPanel(),
        .interfacePcaNavPanel(),
        .interfaceTableNavPanel(),
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

      ## These files need to be sourced unlike interface_* and observeEvent_*
      source("R/util_process_excel.R", local = TRUE)$value
      source("R/util_PCA_function.R", local = TRUE)$value
      source("R/util_PCA_customization.R", local = TRUE)$value
      source("R/util_PCA_by_CPM.R", local = TRUE)$value
      source("R/util_PCA_by_Q3.R", local = TRUE)$value
      source("R/util_PCA_by_RUV4.R", local = TRUE)$value
      source("R/util_differential_gene_exp.R", local = TRUE)$value
      source("R/util_table.R", local = TRUE)$value
      source("R/util_volcano.R", local = TRUE)$value
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
        source("R/output_sidebar.R", local = TRUE)$value
        .outputSidebar()

        ## -------
        source("R/output_setup_nav_panel.R", local = TRUE)$value
        .outputSetupNavPanel()


        ## --------------------PCA nav panel observe----------------------------
        .observeEvent_pca_nav_panel(input)


        ## --------------------PCA nav panel outputs----------------------------
        source("R/output_pca_nav_panel.R", local = TRUE)$value
        .outputPcaNavPanel()


        ## --------------------Table nav panel output---------------------------
        source("R/output_table_nav_panel.R", local = TRUE)$value
        .outputTableNavPanel()


        ## --------------------Table observe
        shiny::observe({
          lapply(names(topTabDF()), function(name) {
            output[[paste0("table_", name)]] <- DT::renderDataTable({
              topTabDF()[[name]] %>%
                dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ ifelse(abs(.) < 1,
                                              formatC(., format = "e", digits = 3),  # Scientific notation for abs < 1
                                              formatC(., format = "f", digits = 3)))) %>%
                DT::datatable()
            })
            output[[paste0("downloadTable_", name)]] <- shiny::downloadHandler(
              filename = function() {
                paste(name, "csv", sep = ".")
              },
              content = function(file) {
                utils::write.csv(topTabDF()[[name]], file, row.names = FALSE)
              }
            )
          }
          )
        })

        ## --------------------Volcano observe----------------------------------
        # nocov start
        shiny::observeEvent(input$toggleCustomRange, {
            shinyjs::toggle("showCustomRange")
        })
        # nocov end

        shiny::observe({
          lapply(names(volcano()), function(name) {
            output[[paste0("volcano_", name)]] <- shiny::renderPlot({
              volcano()[[name]]

            })


          }
          )
        })


        ## --------------------Volcano outputs----------------------------------
        source("R/output_volcano_nav_panel.R", local = TRUE)$value
        .outputVolcanoNavPanel()





        #### --------------------Heatmap outputs--------------------------------
        source("R/output_heatmap_nav_panel.R", local = TRUE)$value
        .outputHeatmapNavPanel()



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
